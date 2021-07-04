use socrates_ast::parsed::{ActiveType::*, BinaryRelationOperator, IdentifierType, Quantifier::*};
use socrates_ast::simple::{Formula, Formula::*, Term::*};

use super::Context;

fn new_simple_context<'i>() -> Context<'i> {
    let mut ctx = Context::new();
    ctx.exec_unit(
        r"
:type v;
:instances v u {a,b,c};

:predicate P();
:predicate Q();

:predicate R(v);
    ",
    )
    .expect("Could not create basic types");

    ctx
}

fn assert_formula_eq<'i>(ctx: &mut Context<'i>, source: &'i str, expected: &Formula<'i>) {
    let parsed = ctx.exec(source).expect("Could not parse test source");

    assert_eq!(expected, &parsed,);
}

#[test]
fn test_positive_atom() {
    let mut ctx = new_simple_context();
    assert_formula_eq(&mut ctx, "P();", &Predicate(true, "P", vec![]));
}

#[test]
fn test_negative_atom() {
    let mut ctx = new_simple_context();
    assert_formula_eq(&mut ctx, "~P();", &Predicate(false, "P", vec![]));
}

#[test]
fn test_demorgan_conjunction() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "~(P() & Q());",
        &Disjunction(
            Box::new(Predicate(false, "P", vec![])),
            Box::new(Predicate(false, "Q", vec![])),
        ),
    );
}

#[test]
fn test_demorgan_disjunction() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "~(P() | Q());",
        &Conjunction(
            Box::new(Predicate(false, "P", vec![])),
            Box::new(Predicate(false, "Q", vec![])),
        ),
    );
}

#[test]
fn test_implication() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "P() -> Q();",
        &Disjunction(
            Box::new(Predicate(false, "P", vec![])),
            Box::new(Predicate(true, "Q", vec![])),
        ),
    );
}

#[test]
fn test_negated_implication() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "~(P() -> Q());",
        &Conjunction(
            Box::new(Predicate(true, "P", vec![])),
            Box::new(Predicate(false, "Q", vec![])),
        ),
    );
}

#[test]
fn test_equivalence() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "P() <-> Q();",
        &Conjunction(
            Box::new(Disjunction(
                Box::new(Predicate(false, "P", vec![])),
                Box::new(Predicate(true, "Q", vec![])),
            )),
            Box::new(Disjunction(
                Box::new(Predicate(false, "Q", vec![])),
                Box::new(Predicate(true, "P", vec![])),
            )),
        ),
    );
}

#[test]
fn test_negated_equivalence() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "~(P() <-> Q());",
        &Disjunction(
            Box::new(Conjunction(
                Box::new(Predicate(true, "P", vec![])),
                Box::new(Predicate(false, "Q", vec![])),
            )),
            Box::new(Conjunction(
                Box::new(Predicate(true, "Q", vec![])),
                Box::new(Predicate(false, "P", vec![])),
            )),
        ),
    );
}

#[test]
fn test_universal_quantifier() {
    let mut ctx = new_simple_context();
    let vt = ctx.get_type("v");
    assert_formula_eq(
        &mut ctx,
        "forall x R(x);",
        &Quantified(
            Universal,
            0,
            vec![(
                "x",
                Ref {
                    to: vt,
                    params: vec![],
                },
            )],
            Box::new(Predicate(
                true,
                "R",
                vec![Identifier("x", IdentifierType::Variable(0, 0))],
            )),
        ),
    );
}

#[test]
fn test_negated_universal_quantifier() {
    let mut ctx = new_simple_context();
    let vt = ctx.get_type("v");
    assert_formula_eq(
        &mut ctx,
        "~(forall x R(x));",
        &Quantified(
            Existential,
            0,
            vec![(
                "x",
                Ref {
                    to: vt,
                    params: vec![],
                },
            )],
            Box::new(Predicate(
                false,
                "R",
                vec![Identifier("x", IdentifierType::Variable(0, 0))],
            )),
        ),
    );
}

#[test]
fn test_existential_quantifier() {
    let mut ctx = new_simple_context();
    let vt = ctx.get_type("v");
    assert_formula_eq(
        &mut ctx,
        "exists x R(x);",
        &Quantified(
            Existential,
            0,
            vec![(
                "x",
                Ref {
                    to: vt,
                    params: vec![],
                },
            )],
            Box::new(Predicate(
                true,
                "R",
                vec![Identifier("x", IdentifierType::Variable(0, 0))],
            )),
        ),
    );
}

#[test]
fn test_negated_existential_quantifier() {
    let mut ctx = new_simple_context();
    let vt = ctx.get_type("v");
    assert_formula_eq(
        &mut ctx,
        "~(exists x R(x));",
        &Quantified(
            Universal,
            0,
            vec![(
                "x",
                Ref {
                    to: vt,
                    params: vec![],
                },
            )],
            Box::new(Predicate(
                false,
                "R",
                vec![Identifier("x", IdentifierType::Variable(0, 0))],
            )),
        ),
    );
}

#[test]
fn test_double_negated_conjunction() {
    let mut ctx = new_simple_context();
    assert_formula_eq(
        &mut ctx,
        "~(~(P() & Q()));",
        &Conjunction(
            Box::new(Predicate(true, "P", vec![])),
            Box::new(Predicate(true, "Q", vec![])),
        ),
    );
}

#[test]
fn test_binary_relation() {
    let mut ctx = new_simple_context();
    let ut = ctx.get_type("u");
    assert_formula_eq(
        &mut ctx,
        "a == b;",
        &Constraint(
            BinaryRelationOperator::Equality,
            Identifier("a", IdentifierType::Atom(ut)),
            Identifier("b", IdentifierType::Atom(ut)),
        ),
    );
}
