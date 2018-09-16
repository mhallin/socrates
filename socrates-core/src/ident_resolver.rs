use std::sync::Arc;

use socrates_ast::parsed::{ActiveType, Formula, Term, IdentifierType, BinaryRelationOperator};
use socrates_ast::Spanning;
use types::TypeStorage;
use socrates_errors::ErrorContext;
use scope::Scope;
use util::plural;

type ResolverScope<'i> = Scope<Vec<Spanning<&'i str>>>;

pub fn resolve_identifiers<'i>(mut f: Spanning<Formula<'i>>, types: &TypeStorage<'i>, errors: &mut ErrorContext<'i>) -> Spanning<Formula<'i>> {
    assign_atoms(&mut f.inner, types, &Scope::new_toplevel(vec![]), errors);
    f
}

fn assign_atoms<'i>(
    f: &mut Formula<'i>,
    types: &TypeStorage<'i>,
    scope: &Arc<ResolverScope<'i>>,
    errors: &mut ErrorContext<'i>,
) {
    use socrates_ast::parsed::Formula::*;

    match f {
        Predicate(name, args) => {
            if let Some(predicate) = types.get_predicate(&name.inner) {
                let expected_count = predicate.arg_types().len();
                if expected_count != args.inner.len() {
                    errors.push_error_message(args.pos, format!(
                        "predicate `{}` expects {} argument{}",
                        name.inner,
                        expected_count,
                        plural(expected_count)));
                }
            }
            else {
                errors.push_error_message(name.pos, format!("predicate `{}` does not exist", name.inner));
            }
            for mut arg in &mut args.inner {
                assign_atoms_term(&mut arg, types, scope, errors, TermContext::Boolean);
            }
        }
        BinaryRelation(op, ref mut lhs, ref mut rhs) => {
            let term_context = match &op.inner {
                BinaryRelationOperator::Equality |
                BinaryRelationOperator::Inequality => TermContext::Unknown,

                BinaryRelationOperator::GreaterThan |
                BinaryRelationOperator::GreaterThanEqual |
                BinaryRelationOperator::LessThan |
                BinaryRelationOperator::LessThanEqual => TermContext::Numeric,
            };

            assign_atoms_term(lhs, types, scope, errors, term_context);
            assign_atoms_term(rhs, types, scope, errors, term_context);
        }
        BinaryLogic(_, lhs, rhs) => {
            assign_atoms(&mut lhs.inner, types, scope, errors);
            assign_atoms(&mut rhs.inner, types, scope, errors);
        }
        UnaryLogic(_, f) => assign_atoms(&mut f.inner, types, scope, errors),
        Quantified(_, s, v, f) => {
            for v in &v.inner {
                if let Some(type_ref) = types.get_instance(&v.inner.0) {
                    errors.push_warning_message(
                        v.pos,
                        format!("variable `{}` shadows instance of type `{}`", v.inner.0, types.type_name(type_ref)));
                }
            }

            let subscope = make_subscope(scope, &v.inner);
            *s = Some(subscope.self_index as u32);
            assign_atoms(&mut f.inner, types, &subscope, errors);
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TermContext {
    Numeric,
    Boolean,
    Unknown,
}

fn assign_atoms_term<'i>(t: &mut Spanning<Term<'i>>, types: &TypeStorage<'i>, scope: &ResolverScope, errors: &mut ErrorContext<'i>, term_context: TermContext) {
    use socrates_ast::parsed::Term::*;

    match &mut t.inner {
        Identifier(name, ty) if ty == &IdentifierType::Unresolved => {
            if let Some((scope, var)) = lookup(scope, name) {
                *ty = IdentifierType::Variable(scope, var);
            }
            else if let Some(type_ref) = types.get_instance(name) {
                *ty = IdentifierType::Atom(type_ref);
            }
            else {
                errors.push_error_message(t.pos, format!("instance `{}` does not exist", name));
            }
        }
        Function(name, args) => {
            if let Some(function) = types.get_function(&name.inner) {
                match term_context {
                    TermContext::Numeric if function.is_uninterpreted() =>
                        errors.push_error_message(t.pos, format!("function `{}` is an uninterpreted function, but is used here in a numeric context", name.inner)),
                    TermContext::Boolean if function.is_numeric() =>
                        errors.push_error_message(t.pos, format!("function `{}` is a numeric function, but is used here in a boolean context", name.inner)),
                    _ => (),
                }

                let expected_count = function.arg_types().len();
                if expected_count != args.inner.len() {
                    errors.push_error_message(args.pos, format!(
                        "function `{}` expects {} argument{}",
                        name.inner,
                        expected_count,
                        plural(expected_count)));
                }
            }
            else {
                errors.push_error_message(name.pos, format!("function `{}` does not exist", name.inner))
            }
            for mut arg in &mut args.inner {
                assign_atoms_term(&mut arg, types, scope, errors, TermContext::Boolean);
            }
        }
        BinaryNumeric(_, ref mut lhs, ref mut rhs) => {
            assign_atoms_term(lhs, types, scope, errors, TermContext::Numeric);
            assign_atoms_term(rhs, types, scope, errors, TermContext::Numeric);
        }
        Identifier(..) | Number(_) => (),
    }
}

fn make_subscope<'i>(
    scope: &Arc<ResolverScope<'i>>,
    variables: &[Spanning<(&'i str, Option<ActiveType>)>]
) -> Arc<ResolverScope<'i>>
{
    Scope::make_subscope(
        scope,
        variables.iter().map(|v| v.as_ref().map(|(n, _)| *n)).collect())
}

fn lookup<'i>(scope: &ResolverScope<'i>, name: &str) -> Option<(u32, u32)> {
    if let Some(idx) = scope.inner.iter().position(|n| n.inner == name) {
        Some((scope.self_index as u32, idx as u32))
    }
    else if let Some(parent) = scope.parent() {
        lookup(parent, name)
    }
    else {
        None
    }
}
