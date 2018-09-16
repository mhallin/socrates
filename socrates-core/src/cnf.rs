use gaf::GAFIndex;

pub type Literal = (bool, GAFIndex);
pub type Clause = Vec<Literal>;

#[derive(Debug, Clone, Default)]
pub struct CNFFormula {
    clauses: Vec<Clause>,
}

impl CNFFormula {
    pub fn new_unit(lit: Literal) -> Self {
        CNFFormula {
            clauses: vec![
                vec![lit],
            ],
        }
    }

    pub fn new_true() -> Self {
        CNFFormula { clauses: vec![] }
    }

    pub fn new_false() -> Self {
        CNFFormula { clauses: vec![ vec![] ] }
    }

    pub fn extend_conjunction(&mut self, other: CNFFormula) {
        self.clauses.extend(other.clauses);
    }

    pub fn extend_disjunction(&mut self, other: &CNFFormula) {
        let mut new_clauses = vec![];
        for lhs_clause in &self.clauses {
            for rhs_clause in &other.clauses {
                let mut new_clause = lhs_clause.clone();
                new_clause.extend(rhs_clause.clone());
                new_clauses.push(new_clause);
            }
        }

        self.clauses = new_clauses;
    }
}

impl IntoIterator for CNFFormula {
    type Item = Clause;
    type IntoIter = ::std::vec::IntoIter<Clause>;

    fn into_iter(self) -> Self::IntoIter {
        self.clauses.into_iter()
    }
}