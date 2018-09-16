use std::sync::{Arc, atomic::AtomicUsize, atomic::Ordering, RwLock, RwLockWriteGuard};
use std::{fmt, cmp, hash};

use fnv::{FnvHashSet, FnvHashMap};

use socrates_ast::parsed::{TypeRef, ActiveType};
use socrates_ast::{Spanning, Span};
use socrates_ast::parsed::{Formula, Term, IdentifierType, MaybeTypedVariable};
use socrates_errors::ErrorContext;
use types::TypeStorage;
use scope::Scope;
use printer::DisplayType;

type GeneratorScope<'i> = Scope<ScopeData<'i>>;

#[derive(Debug)]
struct ScopeData<'i> {
    generics_counter: Arc<AtomicUsize>,
    constraints: Arc<RwLock<ConstraintStorage<'i>>>,
    generics: Vec<ConstraintType>,
}

#[derive(Debug)]
struct ConstraintStorage<'i> {
    constraints: FnvHashSet<Constraint<'i>>,
}

#[derive(Clone)]
enum ConstraintType {
    Type(TypeRef, Vec<ConstraintType>, Span),
    GenericParam(usize),
}

#[derive(Clone)]
enum ConstraintLHS<'i> {
    Identifier(&'i str, IdentifierType, Span),
    Type(ConstraintType),
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct Constraint<'i> {
    lhs: ConstraintLHS<'i>,
    rhs: ConstraintType,
}

pub fn type_check<'i>(mut f: Spanning<Formula<'i>>, types: &TypeStorage<'i>, errors: &mut ErrorContext<'i>) -> Spanning<Formula<'i>> {
    let scope = Scope::new_toplevel(ScopeData::new());
    generate_formula_constraints(&f, types, &scope);

    let mut constraints = scope.inner.constraints.write()
        .expect("ICE: Multiple writers on to constraint storage");

    let unification_error =
        errors.block_exec(|errors| {
            unify_generic_constraints(
                &mut constraints,
                types,
                errors);
            Ok(())
        }).is_err();

    if unification_error {
        return f;
    }

    debug!(
        "Initial constraints: \n{}",
        (*constraints).constraints.iter()
            .map(|c| format!("    {}", DisplayType::new(c, types)))
            .collect::<Vec<_>>()
            .join("\n"));

    merge_variable_definitions(&mut constraints, types, errors);
    remove_redundant_generics(&mut constraints);

    debug!(
        "Reduced constraints: \n{}",
        (*constraints).constraints.iter()
            .map(|c| format!("    {}", DisplayType::new(c, types)))
            .collect::<Vec<_>>()
            .join("\n"));

    let variable_bounds = generate_variable_bounds(&constraints, types);

    debug!(
        "Variables bounds: \n{}",
        variable_bounds.iter()
            .map(|((s, v), t)| format!("    {}:{} <- {}", s, v, DisplayType::new(t, types)))
            .collect::<Vec<_>>()
            .join("\n"));

    assign_quantifier_variables(&mut f, &variable_bounds);

    f
}

fn generate_formula_constraints<'i>(
    f: &Spanning<Formula<'i>>,
    types: &TypeStorage<'i>,
    scope: &Arc<GeneratorScope<'i>>,
) {
    use socrates_ast::parsed::Formula::*;

    match &f.inner {
        Predicate(name, args) =>
            generate_predicate_constraints(name, args, f.pos, types, scope),
        BinaryRelation(_, lhs, rhs) => {
            let upper_bound = scope.inner.get_new_generic_param();
            generate_term_constraints(lhs, &upper_bound, types, scope);
            generate_term_constraints(rhs, &upper_bound, types, scope);
        }
        BinaryLogic(_, lhs, rhs) => {
            generate_formula_constraints(lhs, types, scope);
            generate_formula_constraints(rhs, types, scope);
        }
        UnaryLogic(_, f) =>
            generate_formula_constraints(f, types, scope),
        Quantified(_, _, v, f) =>
            generate_quantifier_constraints(&v, f, types, scope),
    }
}

fn generate_term_constraints<'i>(
    t: &Spanning<Term<'i>>,
    upper_bound: &ConstraintType,
    types: &TypeStorage<'i>,
    scope: &Arc<GeneratorScope<'i>>,
) {
    use socrates_ast::parsed::Term::*;

    match &t.inner {
        Identifier(name, ty) => 
            generate_identifier_constraints(name, *ty, t.pos, upper_bound, scope),
        Function(name, args) =>
            generate_function_constraints(name, &args.inner, t.pos, upper_bound, types, scope),
        BinaryNumeric(_, lhs, rhs) => {
            generate_term_constraints(lhs, upper_bound, types, scope);
            generate_term_constraints(rhs, upper_bound, types, scope);
        }
        Number(_) => (),
    }
}

fn generate_predicate_constraints<'i>(
    name: &'i str,
    args: &[Spanning<Term<'i>>],
    span: Span,
    types: &TypeStorage<'i>,
    scope: &Arc<GeneratorScope<'i>>,
) {
    let predicate = types.get_predicate(name).expect("ICE: Predicate not found");
    assert!(predicate.arg_types().len() == args.len(), "ICE: Predicate arg count mismatch");

    let subscope = make_subscope(scope, predicate.generics(), span, types);

    for (arg, arg_type) in args.iter().zip(predicate.arg_types().iter()) {
        let upper_bound = make_constraint_type_from_active(&subscope, arg_type, arg.pos);
        generate_term_constraints(arg, &upper_bound, types, &subscope);
    }
}

fn generate_quantifier_constraints<'i>(
    vars: &Spanning<Vec<MaybeTypedVariable<'i>>>,
    formula: &Spanning<Formula<'i>>,
    types: &TypeStorage<'i>,
    scope: &Arc<GeneratorScope<'i>>,
) {
    let subscope = make_subscope(&scope, &[], vars.pos, types);

    generate_formula_constraints(formula, types, &subscope);
}

fn generate_identifier_constraints<'i>(
    name: &'i str,
    ident_type: IdentifierType,
    span: Span,
    upper_bound: &ConstraintType,
    scope: &Arc<GeneratorScope<'i>>,
) {
    constraints_mut(scope).push(
        ConstraintLHS::Identifier(name, ident_type, span),
        upper_bound.clone(),
    );
    if let IdentifierType::Atom(tr) = ident_type {
        constraints_mut(scope).push(
            ConstraintLHS::Type(upper_bound.clone()),
            ConstraintType::Type(tr, vec![], span),
        );
    }
}

fn generate_function_constraints<'i>(
    name: &'i str,
    args: &[Spanning<Term<'i>>],
    span: Span,
    upper_bound: &ConstraintType,
    types: &TypeStorage<'i>,
    scope: &Arc<GeneratorScope<'i>>,
) {
    let function = types.get_function(name).expect("ICE: Function not found");
    assert!(function.arg_types().len() == args.len(), "ICE: Predicate arg count mismatch");

    let subscope = make_subscope(scope, function.generics(), span, types);

    for (arg, arg_type) in args.iter().zip(function.arg_types().iter()) {
        let upper_bound = make_constraint_type_from_active(&subscope, arg_type, arg.pos);
        generate_term_constraints(arg, &upper_bound, types, &subscope);
    }

    if let Some(return_type) = function.return_type() {
        let rhs = make_constraint_type_from_active(&subscope, return_type, span);
        constraints_mut(&subscope).push(
            ConstraintLHS::Type(upper_bound.clone()),
            rhs,
        );
    }
}

fn constraints_mut<'a, 'i>(scope: &'a GeneratorScope<'i>) -> RwLockWriteGuard<'a, ConstraintStorage<'i>> {
    scope.inner.constraints
        .write().expect("ICE: Multiple writers on to constraint storage")
}

fn make_subscope<'i>(
    scope: &Arc<GeneratorScope<'i>>,
    declared_generics: &[(&'i str, Option<TypeRef>)],
    span: Span,
    types: &TypeStorage<'i>,
) -> Arc<GeneratorScope<'i>>
{
    let counter = &scope.inner.generics_counter;
    let generics = declared_generics.iter()
        .map(|(_, upper_bound)| {
            let t = ConstraintType::GenericParam(counter.fetch_add(1, Ordering::Relaxed));
            if let Some(upper_bound) = upper_bound {
                constraints_mut(&scope)
                    .push(
                        ConstraintLHS::Type(t.clone()),
                        make_constraint_type_from_ref(*upper_bound, span, types));
            }
            t
        })
        .collect();
    Scope::make_subscope(scope, ScopeData {
        generics_counter: scope.inner.generics_counter.clone(),
        constraints: scope.inner.constraints.clone(),
        generics,
    })
}

fn make_constraint_type_from_ref<'i>(
    type_ref: TypeRef,
    span: Span,
    types: &TypeStorage<'i>,
) -> ConstraintType {
    ConstraintType::Type(
        type_ref,
        types.type_params(type_ref).iter()
            .map(|p| make_constraint_type_from_ref(*p, span, types))
            .collect(),
        span,
    )
}

fn make_constraint_type_from_active<'i>(
    scope: &GeneratorScope<'i>,
    active_type: &ActiveType,
    span: Span,
) -> ConstraintType {
    match active_type {
        ActiveType::GenericParam { index } =>
            scope.inner.generics.get(*index)
                .expect("ICE: Generics not registered")
                .clone(),
        ActiveType::Ref { to, params } => ConstraintType::Type(
            *to,
            params.iter().map(|p| make_constraint_type_from_active(scope, p, span)).collect(),
            span,
        ),
    }
}

fn unify_generic_constraints<'i>(
    constraints: &mut ConstraintStorage<'i>,
    types: &TypeStorage<'i>,
    errors: &mut ErrorContext<'i>,
) {
    let mut new_constraints = FnvHashSet::default();
    let mut to_remove = FnvHashSet::default();

    for constraint in &constraints.constraints {
        if constraint.unify_generics(&mut new_constraints, types, errors) {
            to_remove.insert(constraint.clone());
        }
    }

    constraints.constraints.extend(new_constraints);

    for c in to_remove {
        constraints.constraints.remove(&c);
    }
}

fn merge_variable_definitions<'i>(
    constraints: &mut ConstraintStorage<'i>,
    types: &TypeStorage<'i>,
    errors: &mut ErrorContext<'i>,
) {
    let cs = constraints;
    let mut constraints = cs.constraints.drain().collect::<Vec<_>>();
    while let Some((i1, i2)) = find_conflicting_identifier_constraints(&constraints) {
        let mut acc = FnvHashSet::default();
        let c1_has_generics = constraints[i1].rhs.contains_generics();
        let c2_has_generics = constraints[i2].rhs.contains_generics();

        {
            let (c1, c2) = index_two_mut(&mut constraints, i1, i2);
            trace!("Conflicts: {}, {}", DisplayType::new(c1, types), DisplayType::new(c2, types));
            unify_types(&c1.rhs, &c2.rhs, &mut acc, types, errors);
        }

        for result in acc {
            match result {
                UnificationResult::Error => break,
                UnificationResult::NewConstraint(constraint) => {
                    trace!("  => Adding constraint {}", DisplayType::new(&constraint, types));
                    constraints.push(constraint);
                }
                UnificationResult::SubstituteGenerics(from, to) => {
                    trace!("  => Substituting generics {} => {}", from, to);
                    substitute_generics(&mut constraints, from, to);
                }
            }
        }

        match (c1_has_generics, c2_has_generics) {
            (true, true) => (),
            (true, false) => {
                trace!("  => Removing constraint {}", DisplayType::new(&constraints[i2], types));
                constraints.remove(i2);
            }
            (false, true) => {
                trace!("  => Removing constraint {}", DisplayType::new(&constraints[i1], types));
                constraints.remove(i1);
            }
            (false, false) => {
                let c1_is_subtype = match (&constraints[i1].rhs, &constraints[i2].rhs) {
                    (ConstraintType::Type(tr1, _, _), ConstraintType::Type(tr2, _, _)) =>
                        types.is_in_hierarchy(*tr1, *tr2),
                    _ => false,
                };

                let remove_idx = if c1_is_subtype { i2 } else { i1 };

                trace!("  => Remove constraint {}", DisplayType::new(&constraints[remove_idx], types));
                constraints.remove(remove_idx);
            }
        }

        let mut dedups = constraints.drain(..).collect::<FnvHashSet<_>>();
        constraints = dedups.drain().collect();
        trace!(
            "New constraints: \n{}",
            constraints.iter()
                .map(|c| format!("    {}", DisplayType::new(c, types)))
                .collect::<Vec<_>>()
                .join("\n"));
    }

    cs.constraints = constraints.into_iter().collect();
}

fn substitute_generics<'i>(
    constraints: &mut Vec<Constraint<'i>>,
    from: usize,
    to: usize,
) {
    constraints.iter_mut().for_each(|c| c.substitute_generics(from, to));
    constraints.retain(|c| c.lhs != c.rhs);
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
enum UnificationResult<'i> {
    Error,
    SubstituteGenerics(usize, usize),
    NewConstraint(Constraint<'i>)
}

fn unify_types<'i>(
    lhs: &ConstraintType,
    rhs: &ConstraintType,
    acc: &mut FnvHashSet<UnificationResult>,
    types: &TypeStorage<'i>,
    errors: &mut ErrorContext<'i>,
)
{
    // println!("Unifying types {} and {}", DisplayType::new(lhs, types), DisplayType::new(rhs, types));
    match (lhs, rhs) {
        (ConstraintType::GenericParam(lhs), ConstraintType::GenericParam(rhs)) => {
            acc.insert(UnificationResult::SubstituteGenerics(*lhs, *rhs));
        }
        (lhs @ ConstraintType::GenericParam(_), rhs) => {
            acc.insert(UnificationResult::NewConstraint(Constraint::new(lhs.clone(), rhs.clone())));
        }
        (lhs, rhs @ ConstraintType::GenericParam(_)) => {
            acc.insert(UnificationResult::NewConstraint(Constraint::new(rhs.clone(), lhs.clone())));
        }
        (ConstraintType::Type(lhs, lhs_args, _), ConstraintType::Type(rhs, rhs_args, _)) => {
            if (types.is_integer_type(*lhs) && types.is_integer_type(*rhs))
                || (types.is_interpreted_type(*lhs) && types.is_interpreted_type(*rhs)
                    && (types.is_in_hierarchy(*lhs, *rhs) || types.is_in_hierarchy(*rhs, *lhs))) {
                return;
            }
            else if types.is_uninterpreted_type(*lhs) && types.is_uninterpreted_type(*rhs) {
                assert_eq!(lhs_args.len(), rhs_args.len());
                for (lhs, rhs) in lhs_args.iter().zip(rhs_args) {
                    unify_types(lhs, rhs, acc, types, errors);
                }
            }
            else {
                acc.insert(UnificationResult::Error);
            }
        }
    }
}

fn find_conflicting_identifier_constraints<'i>(
    constraints: &[Constraint<'i>],
) -> Option<(usize, usize)>
{
    let mut start_idx = 0;
    while constraints.len() >= 2 && start_idx <= constraints.len() - 2 {
        let (first_idx, first_var) = constraints[start_idx..]
            .iter()
            .enumerate()
            .filter_map(|(i, c)| match &c.lhs {
                ConstraintLHS::Identifier(_, v @ IdentifierType::Variable(_, _), _)
                    => Some((i, *v)),
                _ => None
            })
            .next()?;

        let first_idx = start_idx + first_idx;
        start_idx = first_idx + 1;

        println!("Conflict: first_idx = {}, constraints.len() = {}", first_idx, constraints.len());

        if first_idx >= constraints.len() - 2 {
            println!("  => fail");
            continue;
        }

        let second_idx =
            match constraints[first_idx+1..]
                .iter()
                .position(|c| match &c.lhs {
                    ConstraintLHS::Identifier(_, v, _) => *v == first_var,
                    _ => false,
                })
            {
                Some(second_idx) => second_idx,
                None => continue,
            };

        println!("  => second_idx = {}, first_idx + 1 + second_idx = {}", second_idx, first_idx + 1 + second_idx);

        return Some((first_idx, first_idx + 1 + second_idx));
    }

    None
}

fn index_two_mut<T>(l: &mut [T], i1: usize, i2: usize) -> (&mut T, &mut T) {
    assert!(i1 < i2, "index_two_mut unordered: {} < {} failed", i1, i2);
    let (first, rest) = l.split_at_mut(i1 + 1);
    (&mut first[i1], &mut rest[i2 - i1 - 1])
}

fn remove_redundant_generics<'i>(
    constraints: &mut ConstraintStorage<'i>
) {
    while let Some((from, to)) = find_redundant_generics_constraint(constraints) {
        trace!("Substituting redundant generics: {} => {}", from, to);
        let mut cs = constraints.constraints.drain().collect();
        substitute_generics(&mut cs, from, to);
        constraints.constraints = cs.drain(..).collect();
    }
}

fn find_redundant_generics_constraint<'i>(constraints: &ConstraintStorage<'i>) -> Option<(usize, usize)> {
    constraints.constraints.iter()
        .filter_map(|c| match (&c.lhs, &c.rhs) {
            (ConstraintLHS::Type(ConstraintType::GenericParam(lhs)), ConstraintType::GenericParam(rhs)) => Some((*rhs, *lhs)),
            _ => None
        })
        .next()
}

fn generate_variable_bounds<'i>(
    constraints: &ConstraintStorage<'i>,
    types: &TypeStorage<'i>
) -> FnvHashMap<(u32, u32), ActiveType>
{
    constraints
        .constraints_for_variables_iter()
        .map(|(v, rhs)| (v, find_lower_bound(rhs, constraints, types).make_active()))
        .collect()
}

fn find_lower_bound<'i>(
    ty: &ConstraintType,
    constraints: &ConstraintStorage,
    types: &TypeStorage<'i>,
) -> ConstraintType {
    match ty {
        ConstraintType::GenericParam(p) => {
            constraints
                .constraints_for_generic_param(*p)
                .fold(None as Option<&ConstraintType>, |lower_bound, rhs|
                    match (lower_bound, rhs) {
                        (None, rhs) => Some(rhs),
                        (Some(ConstraintType::Type(lhs_tr, _, _)), ConstraintType::Type(rhs_tr, _, _)) =>
                            if types.is_in_hierarchy(*rhs_tr, *lhs_tr) {
                                Some(rhs)
                            }
                            else {
                                lower_bound
                            }
                        _ => lower_bound,
                    })
                .expect("ICE: Could not find lower bound for generic param")
                .clone()
        }
        ConstraintType::Type(_, _, _) => ty.clone()
    }
}

fn assign_quantifier_variables(f: &mut Spanning<Formula>, bounds: &FnvHashMap<(u32, u32), ActiveType>) {
    match &mut f.inner {
        Formula::BinaryLogic(_, ref mut lhs, ref mut rhs) => {
            assign_quantifier_variables(lhs, bounds);
            assign_quantifier_variables(rhs, bounds);
        },
        Formula::UnaryLogic(_, ref mut v) => {
            assign_quantifier_variables(v, bounds);
        },
        Formula::Quantified(_, s, v, ref mut f) => {
            let s = s.expect("ICE: Quantifier has not been assigned a scope index");
            for (i, Spanning { inner: (_, t), .. }) in v.inner.iter_mut().enumerate() {
                *t = Some(bounds.get(&(s, i as u32)).expect("ICE: Variable bound not assigned").clone());
            }

            assign_quantifier_variables(f, bounds);
        },
        Formula::BinaryRelation { .. } | Formula::Predicate { .. } => ()
    }
}

impl<'i> ScopeData<'i> {
    fn new() -> Self {
        ScopeData {
            generics_counter: Arc::new(AtomicUsize::new(0)),
            constraints: Arc::new(RwLock::new(ConstraintStorage::new())),
            generics: vec![],
        }
    }

    fn get_new_generic_param(&self) -> ConstraintType {
        ConstraintType::GenericParam(
            self.generics_counter.fetch_add(1, Ordering::Relaxed)
        )
    }
}

impl<'i> ConstraintStorage<'i> {
    fn new() -> Self {
        ConstraintStorage {
            constraints: FnvHashSet::default(),
        }
    }

    fn push(&mut self, lhs: ConstraintLHS<'i>, rhs: ConstraintType) {
        if lhs != rhs {
            let c = Constraint { lhs, rhs };
            trace!("Adding constraint: {:?}", c);
            self.constraints.insert(c);
        }
        else {
            trace!("Ignoring constraint for identical sides: {:?}", lhs);
        }
    }

    fn constraints_for_variables_iter(&self) -> impl Iterator<Item=((u32, u32), &ConstraintType)> {
        self.constraints
            .iter()
            .filter_map(|c| match &c.lhs {
                ConstraintLHS::Identifier(_, IdentifierType::Variable(s, v), _) =>
                    Some(((*s, *v), &c.rhs)),
                _ => None,
            })
    }

    fn constraints_for_generic_param(&self, param: usize) -> impl Iterator<Item=&ConstraintType> {
        self.constraints
            .iter()
            .filter_map(move |c| match &c.lhs {
                ConstraintLHS::Type(ConstraintType::GenericParam(p)) if *p == param =>
                    Some(&c.rhs),
                _ => None,
            })
    }
}

impl<'i> Constraint<'i> {
    fn new<L: Into<ConstraintLHS<'i>>>(lhs: L, rhs: ConstraintType) -> Self {
        Constraint { lhs: lhs.into(), rhs }
    }

    fn unify_generics(&self, destination: &mut FnvHashSet<Constraint<'i>>, types: &TypeStorage<'i>, errors: &mut ErrorContext<'i>) -> bool {
        match (&self.lhs, &self.rhs) {
            (
                ConstraintLHS::Type(ConstraintType::Type(lhs_tr, lhs_params, lhs_span)),
                ConstraintType::Type(rhs_tr, rhs_params, _rhs_span),
            ) =>
                match (lhs_params.len(), rhs_params.len()) {
                    (0, 0) => false,
                    (x, y) if x != y || lhs_tr != rhs_tr => {
                        errors.push_error_message(
                            *lhs_span,
                            format!("Types {} and {} are not compatible",
                                DisplayType::new(&self.lhs, types),
                                DisplayType::new(&self.rhs, types)));
                        true
                    }
                    _ => {
                        for (lhs, rhs) in lhs_params.iter().zip(rhs_params) {
                            destination.insert(Constraint::new(lhs.clone(), rhs.clone()));
                        }
                        true
                    }
            }
            _ => false,
        }
    }

    fn substitute_generics(&mut self, from: usize, to: usize) {
        self.lhs.substitute_generics(from, to);
        self.rhs.substitute_generics(from, to);
    }
}

impl ConstraintType {
    fn substitute_generics(&mut self, from: usize, to: usize) {
        match self {
            ConstraintType::GenericParam(p) =>
                if *p == from {
                    *p = to
                },
            ConstraintType::Type(_, params, _) =>
                for param in params {
                    param.substitute_generics(from, to);
                },
        }
    }

    fn make_active(&self) -> ActiveType {
        match self {
            ConstraintType::GenericParam(p) => ActiveType::GenericParam { index: *p },
            ConstraintType::Type(tr, params, _) => ActiveType::Ref {
                to: *tr,
                params: params.iter().map(|ct| ct.make_active()).collect(),
            }
        }
    }

    fn contains_generics(&self) -> bool {
        match self {
            ConstraintType::GenericParam(_) => true,
            ConstraintType::Type(_, params, _) => params.iter().any(ConstraintType::contains_generics)
        }
    }
}

impl<'i> ConstraintLHS<'i> {
    fn substitute_generics(&mut self, from: usize, to: usize) {
        match self {
            ConstraintLHS::Identifier(_, _, _) => (),
            ConstraintLHS::Type(ct) => ct.substitute_generics(from, to),
        }
    }
}

impl<'i> Into<ConstraintLHS<'i>> for ConstraintType {
    fn into(self) -> ConstraintLHS<'i> {
        ConstraintLHS::Type(self)
    }
}

impl<'i> fmt::Display for DisplayType<'i, TypeRef> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.storage.type_name(*self.ty))
    }
}

impl<'i> fmt::Display for DisplayType<'i, ConstraintType> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ty {
            ConstraintType::GenericParam(i) => write!(f, "${}", i),
            ConstraintType::Type(tr, p, _) => 
                if p.is_empty() {
                    write!(f, "{}", self.display(tr))
                }
                else {
                    let params = p.iter()
                        .map(|p| format!("{}", self.display(p)))
                        .collect::<Vec<_>>();
                    write!(f, "{}[{}]", self.display(tr), params.join(", "))
                }
        }
    }
}

impl<'i> fmt::Display for DisplayType<'i, ConstraintLHS<'i>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ty {
            ConstraintLHS::Identifier(name, IdentifierType::Unresolved, _)
                => write!(f, "?{}", name),
            ConstraintLHS::Identifier(name, IdentifierType::Atom(tr), _)
                => write!(f, "{}({})", name, self.display(tr)),
            ConstraintLHS::Identifier(name, IdentifierType::Variable(scope, idx), _)
                => write!(f, "{}@{}:{}", name, scope, idx),
            ConstraintLHS::Type(ty) => self.display(ty).fmt(f),
        }
    }
}

impl<'i> fmt::Display for DisplayType<'i, Constraint<'i>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <= {}", self.display(&self.ty.lhs), self.display(&self.ty.rhs))
    }
}

impl fmt::Debug for ConstraintType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstraintType::GenericParam(i) => write!(f, "$_T{}", i),
            ConstraintType::Type(tr, p, _) => 
                if p.is_empty() {
                    write!(f, "{:?}", tr)
                }
                else {
                    let params = p.iter()
                        .map(|p| format!("{:?}", p))
                        .collect::<Vec<_>>();
                    write!(f, "{:?}({})", tr, params.join(", "))
                }
        }
    }
}

impl<'i> fmt::Debug for ConstraintLHS<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstraintLHS::Identifier(name, IdentifierType::Unresolved, _)
                => write!(f, "({:?}, ???)", name),
            ConstraintLHS::Identifier(name, IdentifierType::Atom(tr), _)
                => write!(f, "({:?}, atom {:?})", name, tr),
            ConstraintLHS::Identifier(name, IdentifierType::Variable(scope, idx), _)
                => write!(f, "({:?}, variable {}:{})", name, scope, idx),
            ConstraintLHS::Type(ty) => ty.fmt(f),
        }
    }
}

impl<'i> fmt::Debug for Constraint<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} <= {:?}", self.lhs, self.rhs)
    }
}

impl cmp::PartialEq for ConstraintType {
    fn eq(&self, other: &Self) -> bool {
        use self::ConstraintType::*;
        match (self, other) {
            (GenericParam(lhs), GenericParam(rhs)) => lhs == rhs,
            (Type(lhs_tr, lhs_params, _), Type(rhs_tr, rhs_params, _)) =>
                lhs_tr == rhs_tr && lhs_params == rhs_params,
            (GenericParam(_), _) | (_, GenericParam(_)) => false,
        }
    }
}

impl cmp::Eq for ConstraintType {}

impl<'i> cmp::PartialEq for ConstraintLHS<'i> {
    fn eq(&self, other: &Self) -> bool {
        use self::ConstraintLHS::*;
        match (self, other) {
            (Identifier(lhs_name, lhs_ty, _), Identifier(rhs_name, rhs_ty, _)) =>
                lhs_name == rhs_name && lhs_ty == rhs_ty,
            (Type(lhs_ct), Type(rhs_ct)) => lhs_ct == rhs_ct,
            (Identifier(_, _, _), _) | (_, Identifier(_, _, _)) => false,
        }
    }
}

impl<'i> cmp::PartialEq<ConstraintType> for ConstraintLHS<'i> {
    fn eq(&self, other: &ConstraintType) -> bool {
        use self::ConstraintLHS::*;
        match self {
            Identifier(_, _, _) => false,
            Type(ct) => ct == other,
        }
    }
}

impl<'i> cmp::Eq for ConstraintLHS<'i> {}

impl hash::Hash for ConstraintType {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        use self::ConstraintType::*;

        match self {
            GenericParam(p) => p.hash(state),
            Type(tr, params, _) => {
                tr.hash(state);
                params.hash(state);
            }
        }
    }
}

impl<'i> hash::Hash for ConstraintLHS<'i> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        use self::ConstraintLHS::*;

        match self {
            Identifier(name, ty, _) => {
                name.hash(state);
                ty.hash(state);
            }
            Type(ct) => ct.hash(state),
        }
    }
}