use parsed::{
    self,
    Quantifier,
    BinaryRelationOperator,
    BinaryNumericOperator,
    IdentifierType,
    ActiveType,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Formula<'i> {
    Quantified(Quantifier, u32, Vec<(&'i str, ActiveType)>, Box<Formula<'i>>),
    Conjunction(Box<Formula<'i>>, Box<Formula<'i>>),
    Disjunction(Box<Formula<'i>>, Box<Formula<'i>>),
    Predicate(bool, &'i str, Vec<Term<'i>>),
    Constraint(BinaryRelationOperator, Term<'i>, Term<'i>),
    Contradiction,
    Tautology,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term<'i> {
    Identifier(&'i str, IdentifierType),
    Function(&'i str, Vec<Term<'i>>),
    Number(i64),
    BinaryNumeric(BinaryNumericOperator, Box<Term<'i>>, Box<Term<'i>>),
}

impl<'i> Into<Term<'i>> for parsed::Term<'i> {
    fn into(self) -> Term<'i> {
        use parsed::Term::*;

        match self {
            Identifier(i, tr) => Term::Identifier(i, tr),
            Function(name, args) => Term::Function(name.inner, args.inner.into_iter().map(|n| n.inner.into()).collect()),
            Number(i) => Term::Number(i),
            BinaryNumeric(op, lhs, rhs) => Term::BinaryNumeric(op.inner, Box::new(lhs.inner.into()), Box::new(rhs.inner.into())),
        }
    }
}

impl<'i> Into<Formula<'i>> for parsed::Formula<'i> {
    fn into(self) -> Formula<'i> {
        use parsed::Formula::*;
        use parsed::{UnaryLogicOperator, BinaryLogicOperator};
        use position::Spanning;

        simplify(match self {
            Quantified(q, scope_idx, vars, f) => Formula::Quantified(
                q.inner,
                scope_idx.expect("ICE: Scope index not assigned"),
                vars.inner.into_iter().map(|n| (n.inner.0, n.inner.1.expect("ICE: Types must be resolved before simplifying formulas"))).collect(),
                Box::new(simplify(f.inner.into()))),

            Predicate(name, args) => Formula::Predicate(
                true,
                name.inner,
                args.inner.into_iter().map(|a| a.inner.into()).collect()),

            BinaryRelation(op, lhs, rhs) => Formula::Constraint(
                op.inner,
                lhs.inner.into(),
                rhs.inner.into()),

            BinaryLogic(Spanning { inner: BinaryLogicOperator::Conjunction, .. }, lhs, rhs)
                => Formula::Conjunction(
                    Box::new(simplify(lhs.inner.into())),
                    Box::new(simplify(rhs.inner.into()))),

            BinaryLogic(Spanning { inner: BinaryLogicOperator::Disjunction, .. }, lhs, rhs)
                => Formula::Disjunction(
                    Box::new(simplify(lhs.inner.into())),
                    Box::new(simplify(rhs.inner.into()))),

            BinaryLogic(Spanning { inner: BinaryLogicOperator::Equivalence, pos }, lhs, rhs) => {
                let right = simplify(BinaryLogic(
                    Spanning { inner: BinaryLogicOperator::Implication, pos },
                    lhs.clone(),
                    rhs.clone(),
                ).into());
                let left = simplify(BinaryLogic(
                    Spanning { inner: BinaryLogicOperator::Implication, pos },
                    rhs,
                    lhs,
                ).into());

                Formula::Conjunction(Box::new(right), Box::new(left))
            }

            BinaryLogic(Spanning { inner: BinaryLogicOperator::Implication, pos }, lhs, rhs) => {
                let lhs = simplify(UnaryLogic(
                    Spanning { inner: UnaryLogicOperator::Negation, pos },
                    lhs,
                ).into());
                let rhs = simplify(rhs.inner.into());

                Formula::Disjunction(Box::new(lhs), Box::new(rhs))
            }

            UnaryLogic(Spanning { inner: UnaryLogicOperator::Negation, .. }, f) =>
                simplify(perform_negation(f.inner.into()))
        })
    }
}

fn simplify(f: Formula) -> Formula {
    constant_fold(f)
}

fn constant_fold(f: Formula) -> Formula {
    match f {
        Formula::Disjunction(lhs, rhs) => match (simplify(*lhs), simplify(*rhs)) {
            // p | T = T
            (Formula::Tautology, _) | (_, Formula::Tautology) => Formula::Tautology,

            // p | F = p
            (Formula::Contradiction, other) | (other, Formula::Contradiction) => other,

            // p | p = p
            (ref lhs, ref rhs) if lhs == rhs => lhs.clone(),

            // p | q = p | q
            (lhs, rhs) => Formula::Disjunction(Box::new(lhs), Box::new(rhs)),
        }
        Formula::Conjunction(lhs, rhs) => match (simplify(*lhs), simplify(*rhs)) {
            // p & F = F
            (Formula::Contradiction, _) | (_, Formula::Contradiction) => Formula::Contradiction,

            // p & T = p
            (Formula::Tautology, other) | (other, Formula::Tautology) => other,

            // p & p = p
            (ref lhs, ref rhs) if lhs == rhs => lhs.clone(),

            // p & q = p & q
            (lhs, rhs) => Formula::Conjunction(Box::new(lhs), Box::new(rhs)),
        }
        Formula::Quantified(q, s, v, f) => match simplify(*f) {
            // forall _ T = T, exists _ T = T
            Formula::Tautology => Formula::Tautology,

            // forall _ F = F, exists _ F = F
            Formula::Contradiction => Formula::Contradiction,

            // forall _ p = forall _ p, exists _ p = exists _ p
            f => Formula::Quantified(q, s, v, Box::new(f)),
        }
        f => f,
    }
}

fn perform_negation(f: Formula) -> Formula {
    match f {
        // ~(p & q) = ~p | ~q
        Formula::Conjunction(lhs, rhs)
            => Formula::Disjunction(Box::new(perform_negation(*lhs)), Box::new(perform_negation(*rhs))),

        // ~(p | q) = ~p & ~q
        Formula::Disjunction(lhs, rhs)
            => Formula::Conjunction(Box::new(perform_negation(*lhs)), Box::new(perform_negation(*rhs))),
        
        // ~exists _ p = forall _ ~p
        Formula::Quantified(Quantifier::Existential, s, v, f)
            => Formula::Quantified(Quantifier::Universal, s, v, Box::new(perform_negation(*f))),

        // ~forall _ p = exists _ ~p
        Formula::Quantified(Quantifier::Universal, s, v, f)
            => Formula::Quantified(Quantifier::Existential, s, v, Box::new(perform_negation(*f))),

        // ~p = ~p
        Formula::Predicate(sign, name, args)
            => Formula::Predicate(!sign, name, args),

        Formula::Constraint(op, lhs, rhs)
            => Formula::Constraint(op.negated(), lhs, rhs),

        // ~T = F
        Formula::Tautology => Formula::Contradiction,

        // ~F = T
        Formula::Contradiction => Formula::Tautology,
    }
}