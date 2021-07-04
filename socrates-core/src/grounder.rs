use std::rc::Rc;

use itertools::Itertools;

use socrates_errors::eyre::Error;
use socrates_ast::parsed::ActiveType;
use socrates_ast::parsed::{BinaryRelationOperator, IdentifierType, Quantifier};
use socrates_ast::simple::{Formula, Term};

use crate::cnf::CNFFormula;
use crate::cnf_receiver::CNFReceiver;
use crate::gaf::{GAFStorage, GroundTerm, GAF};
use crate::types::TypeStorage;

pub trait Emitter<'i> {
    type FormulaResult;
    type ProblemResult;
    fn emit(
        &mut self,
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<Self::FormulaResult, Error>;
    fn finish(self) -> Result<Self::ProblemResult, Error>;
}

pub struct ToplevelCNFEmitter<'i, R: CNFReceiver> {
    gaf_storage: GAFStorage<'i>,
    receiver: R,
    current_scope: Option<Rc<Scope<'i>>>,
}

pub struct BufferingCNFEmitter<'i> {
    gaf_storage: GAFStorage<'i>,
    current_scope: Option<Rc<Scope<'i>>>,
}

struct Scope<'i> {
    index: u32,
    variable_values: Vec<GroundTerm<'i>>,
    parent: Option<Rc<Scope<'i>>>,
}

impl<'i, R: CNFReceiver> ToplevelCNFEmitter<'i, R> {
    pub fn new(gaf_storage: &GAFStorage<'i>, receiver: R) -> Self {
        ToplevelCNFEmitter {
            gaf_storage: gaf_storage.clone(),
            receiver,
            current_scope: None,
        }
    }

    fn with_pushed_scope<F>(&mut self, scope: Rc<Scope<'i>>, f: F) -> Result<(), Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Error>,
    {
        let old_scope = self.current_scope.clone();
        self.current_scope = Some(scope);
        let result = f(self);
        self.current_scope = old_scope;
        result
    }

    fn emit_universal_quantifier(
        &mut self,
        scope_index: u32,
        variables: &[(&'i str, ActiveType)],
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<(), Error> {
        for assignment in variable_assignment_iter(&variables, types) {
            let new_scope = Scope::make_subscope(&self.current_scope, scope_index, assignment);
            self.with_pushed_scope(new_scope, |emitter| emitter.emit(formula, types))?;
        }
        Ok(())
    }

    fn emit_predicate(
        &mut self,
        sign: bool,
        name: &'i str,
        args: &[Term<'i>],
        _types: &TypeStorage<'i>,
    ) -> Result<(), Error> {
        let gaf = self.gaf_storage.reify(GAF::new(
            name,
            args.iter()
                .map(|t| ground_term(t, &self.gaf_storage, &self.current_scope))
                .collect(),
        ));

        self.receiver.receive(&[(sign, gaf)])
    }

    fn emit_buffered(
        &mut self,
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<(), Error> {
        let cnf = BufferingCNFEmitter::emit(
            self.current_scope.clone(),
            formula,
            self.gaf_storage.clone(),
            types,
        )?;

        for clause in cnf {
            self.receiver.receive(&clause)?;
        }

        Ok(())
    }
}

impl<'i, R: CNFReceiver> Emitter<'i> for ToplevelCNFEmitter<'i, R> {
    type FormulaResult = ();
    type ProblemResult = R::ProblemResult;

    fn emit(&mut self, formula: &Formula<'i>, types: &TypeStorage<'i>) -> Result<(), Error> {
        use socrates_ast::simple::Formula::*;
        match formula {
            Quantified(Quantifier::Universal, s, vs, f) => {
                self.emit_universal_quantifier(*s, &vs, f.as_ref(), types)
            }
            s @ Quantified(Quantifier::Existential, _, _, _) => self.emit_buffered(s, types),
            Conjunction(lhs, rhs) => {
                self.emit(lhs, types)?;
                self.emit(rhs, types)?;
                Ok(())
            }
            s @ Disjunction(_, _) => self.emit_buffered(s, types),
            Predicate(sign, name, args) => self.emit_predicate(*sign, name, args, types),
            s @ Constraint(_, _, _) => self.emit_buffered(s, types),
            Contradiction => panic!("ICE: Contradiction in toplevel NNF"),
            Tautology => panic!("ICE: Tautology in toplevel NNF"),
        }
    }

    fn finish(self) -> Result<Self::ProblemResult, Error> {
        self.receiver.finish()
    }
}

impl<'i> BufferingCNFEmitter<'i> {
    fn emit(
        scope: Option<Rc<Scope<'i>>>,
        formula: &Formula<'i>,
        gaf_storage: GAFStorage<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        let mut emitter = BufferingCNFEmitter {
            current_scope: scope,
            gaf_storage,
        };

        emitter.emit(formula, types)
    }

    fn with_pushed_scope<F>(&mut self, scope: Rc<Scope<'i>>, f: F) -> Result<(), Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Error>,
    {
        let old_scope = self.current_scope.clone();
        self.current_scope = Some(scope);
        let result = f(self);
        self.current_scope = old_scope;
        result
    }

    fn construct_universal_quantifier(
        &mut self,
        scope_index: u32,
        variables: &[(&'i str, ActiveType)],
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        let mut clauses: CNFFormula = Default::default();
        for assignment in variable_assignment_iter(&variables, types) {
            let new_scope = Scope::make_subscope(&self.current_scope, scope_index, assignment);
            self.with_pushed_scope(new_scope, |emitter| {
                clauses.extend_conjunction(emitter.emit(formula, types)?);
                Ok(())
            })?;
        }
        Ok(clauses)
    }

    fn construct_existential_quantifier(
        &mut self,
        scope_index: u32,
        variables: &[(&'i str, ActiveType)],
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        let mut clauses: CNFFormula = Default::default();
        for assignment in variable_assignment_iter(&variables, types) {
            let new_scope = Scope::make_subscope(&self.current_scope, scope_index, assignment);
            self.with_pushed_scope(new_scope, |emitter| {
                clauses.extend_disjunction(&emitter.emit(formula, types)?);
                Ok(())
            })?;
        }
        Ok(clauses)
    }

    fn construct_predicate(
        &mut self,
        sign: bool,
        name: &'i str,
        args: &[Term<'i>],
        _types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        let gaf = self.gaf_storage.reify(GAF::new(
            name,
            args.iter()
                .map(|t| ground_term(t, &self.gaf_storage, &self.current_scope))
                .collect(),
        ));

        Ok(CNFFormula::new_unit((sign, gaf)))
    }

    fn construct_constraint(
        &mut self,
        op: BinaryRelationOperator,
        lhs: &Term<'i>,
        rhs: &Term<'i>,
        _types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        let lhs = ground_term(lhs, &self.gaf_storage, &self.current_scope);
        let rhs = ground_term(rhs, &self.gaf_storage, &self.current_scope);

        if let Some(result) = evaluate_constraint(op, &lhs, &rhs) {
            if result {
                Ok(CNFFormula::new_true())
            } else {
                Ok(CNFFormula::new_false())
            }
        } else {
            unimplemented!("Non-static constraints not implemented yet");
        }
    }
}

impl<'i> Emitter<'i> for BufferingCNFEmitter<'i> {
    type FormulaResult = CNFFormula;
    type ProblemResult = ();

    fn emit(
        &mut self,
        formula: &Formula<'i>,
        types: &TypeStorage<'i>,
    ) -> Result<CNFFormula, Error> {
        use socrates_ast::simple::Formula::*;
        match formula {
            Quantified(Quantifier::Universal, s, vs, f) => {
                self.construct_universal_quantifier(*s, vs, f.as_ref(), types)
            }
            Quantified(Quantifier::Existential, s, vs, f) => {
                self.construct_existential_quantifier(*s, vs, f.as_ref(), types)
            }
            Conjunction(lhs, rhs) => {
                let mut clauses = self.emit(lhs, types)?;
                clauses.extend_conjunction(self.emit(rhs, types)?);
                Ok(clauses)
            }
            Disjunction(lhs, rhs) => {
                let mut clauses = self.emit(lhs, types)?;
                clauses.extend_disjunction(&self.emit(rhs, types)?);
                Ok(clauses)
            }
            Predicate(sign, name, args) => self.construct_predicate(*sign, name, args, types),
            Constraint(op, lhs, rhs) => self.construct_constraint(*op, lhs, rhs, types),
            Contradiction => panic!("ICE: Contradiction in NNF formula"),
            Tautology => panic!("ICE: Tautology in NNF formula"),
        }
    }

    fn finish(self) -> Result<(), Error> {
        Ok(())
    }
}

fn ground_term<'i>(
    term: &Term<'i>,
    gafs: &GAFStorage<'i>,
    scope: &Option<Rc<Scope<'i>>>,
) -> GroundTerm<'i> {
    match term {
        Term::BinaryNumeric(_, _, _) => unimplemented!("BinaryNumeric grounding not implemented"),
        Term::Function(name, args) => GroundTerm::new_function(
            name,
            args.iter().map(|a| ground_term(a, gafs, scope)).collect(),
        ),
        Term::Identifier(name, IdentifierType::Atom(_)) => GroundTerm::new_ident(name),
        Term::Identifier(_, IdentifierType::Variable(s, v)) => scope
            .as_ref()
            .expect("ICE: No scope when encountering variable")
            .lookup(*s, *v)
            .clone(),
        Term::Identifier(_, IdentifierType::Unresolved) => {
            panic!("ICE: Unresolved identifier when grounding term")
        }
        Term::Number(n) => GroundTerm::Number(*n),
    }
}

fn evaluate_constraint(
    op: BinaryRelationOperator,
    lhs: &GroundTerm<'_>,
    rhs: &GroundTerm<'_>,
) -> Option<bool> {
    use socrates_ast::parsed::BinaryRelationOperator::*;
    match (lhs, rhs) {
        (GroundTerm::Number(lhs), GroundTerm::Number(rhs)) => Some(match op {
            Equality => *lhs == *rhs,
            Inequality => *lhs != *rhs,
            GreaterThan => *lhs > *rhs,
            GreaterThanEqual => *lhs >= *rhs,
            LessThan => *lhs < *rhs,
            LessThanEqual => *lhs <= *rhs,
        }),
        (GroundTerm::Named(lhs_name, lhs_args), GroundTerm::Named(rhs_name, rhs_args)) => {
            match op {
                Equality => Some(*lhs_name == *rhs_name && *lhs_args == *rhs_args),
                Inequality => Some(*lhs_name != *rhs_name || *lhs_args != *rhs_args),
                _ => None,
            }
        }
        _ => None,
    }
}

fn variable_assignment_iter<'i>(
    variables: &[(&'i str, ActiveType)],
    types: &TypeStorage<'i>,
) -> impl Iterator<Item = Vec<GroundTerm<'i>>> {
    variables
        .iter()
        .map(|(_, ty)| match ty {
            ActiveType::GenericParam { .. } => panic!("ICE: Variable is bound by generic type"),
            ActiveType::Ref { to, params } => {
                if params.is_empty() {
                    types.instance_iter(*to).into_iter()
                } else {
                    panic!("ICE: Variable is bound by uninterpreted type");
                }
            }
        })
        .multi_cartesian_product()
}

impl<'i> Scope<'i> {
    fn lookup(&self, scope: u32, variable: u32) -> &GroundTerm<'i> {
        if self.index == scope {
            &self.variable_values[variable as usize]
        } else if let Some(parent) = &self.parent {
            parent.lookup(scope, variable)
        } else {
            panic!("ICE: Variable {}:{} not found", scope, variable);
        }
    }

    fn make_subscope(
        parent: &Option<Rc<Scope<'i>>>,
        index: u32,
        variable_values: Vec<GroundTerm<'i>>,
    ) -> Rc<Self> {
        Rc::new(Scope {
            index,
            variable_values,
            parent: parent.clone(),
        })
    }
}
