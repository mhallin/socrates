use crate::position::Spanning;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TypeRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActiveType {
    Ref {
        to: TypeRef,
        params: Vec<ActiveType>,
    },
    GenericParam {
        index: usize,
    },
}

#[derive(Debug, Clone)]
pub enum InteractiveItem<'i> {
    Interactive(Spanning<InteractiveCommand<'i>>),
    Item(Box<Spanning<DocumentItem<'i>>>),
}

#[derive(Debug, Clone)]
pub enum InteractiveCommand<'i> {
    ShowModel,
    ProveFormula(Box<Spanning<Formula<'i>>>),
}

#[derive(Debug, Clone)]
pub enum DocumentItem<'i> {
    Definition(Spanning<Definition<'i>>),
    Formula(Spanning<Formula<'i>>),
    BucketedFormula(Spanning<&'i str>, Spanning<Formula<'i>>),
}

#[derive(Debug, Clone)]
pub struct TypeSpec<'i> {
    pub name: Spanning<&'i str>,
    pub args: Option<Spanning<Vec<Spanning<TypeSpec<'i>>>>>,
}

#[derive(Debug, Clone)]
pub enum Definition<'i> {
    Type(TypeSpec<'i>),
    Instances(
        TypeSpec<'i>,
        Spanning<&'i str>,
        Spanning<Vec<Spanning<&'i str>>>,
    ),
    UninterpretedFunction(
        TypeSpec<'i>,
        Spanning<&'i str>,
        Spanning<Vec<Spanning<TypeSpec<'i>>>>,
    ),
    NumericFunction(Spanning<&'i str>, Spanning<Vec<Spanning<TypeSpec<'i>>>>),
    Predicate(Spanning<&'i str>, Spanning<Vec<Spanning<TypeSpec<'i>>>>),
    IntegerType(Spanning<&'i str>, Spanning<i64>, Spanning<i64>),
    CompletionBucket(Spanning<&'i str>, Spanning<&'i str>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Quantifier {
    Universal,
    Existential,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinaryRelationOperator {
    Equality,
    Inequality,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinaryNumericOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinaryLogicOperator {
    Implication,
    Equivalence,
    Conjunction,
    Disjunction,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryLogicOperator {
    Negation,
}

pub type MaybeTypedVariable<'i> = Spanning<(&'i str, Option<ActiveType>)>;

#[derive(Debug, Clone)]
pub enum Formula<'i> {
    Predicate(Spanning<&'i str>, Spanning<Vec<Spanning<Term<'i>>>>),
    BinaryRelation(
        Spanning<BinaryRelationOperator>,
        Spanning<Term<'i>>,
        Spanning<Term<'i>>,
    ),
    BinaryLogic(
        Spanning<BinaryLogicOperator>,
        Box<Spanning<Formula<'i>>>,
        Box<Spanning<Formula<'i>>>,
    ),
    UnaryLogic(Spanning<UnaryLogicOperator>, Box<Spanning<Formula<'i>>>),
    Quantified(
        Spanning<Quantifier>,
        Option<u32>,
        Spanning<Vec<MaybeTypedVariable<'i>>>,
        Box<Spanning<Formula<'i>>>,
    ),
}

#[derive(Debug, Clone)]
pub enum Term<'i> {
    Identifier(&'i str, IdentifierType),
    Function(Spanning<&'i str>, Spanning<Vec<Spanning<Term<'i>>>>),
    Number(i64),
    BinaryNumeric(
        Spanning<BinaryNumericOperator>,
        Box<Spanning<Term<'i>>>,
        Box<Spanning<Term<'i>>>,
    ),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum IdentifierType {
    Unresolved,
    Atom(TypeRef),
    Variable(u32, u32),
}

impl BinaryRelationOperator {
    pub fn negated(self) -> Self {
        use self::BinaryRelationOperator::*;

        match self {
            Equality => Inequality,
            Inequality => Equality,
            GreaterThan => LessThanEqual,
            GreaterThanEqual => LessThan,
            LessThan => GreaterThanEqual,
            LessThanEqual => GreaterThan,
        }
    }
}
