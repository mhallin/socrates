use std::{cell::RefCell, rc::Rc};

use minisat::{Bool, Lit, Solver};
use socrates_core::{cnf::Literal, CNFReceiver};
use socrates_errors::eyre;

pub struct MinisatReceiver {
    solver: Rc<RefCell<Solver>>,
    literals: RefCell<Vec<Lit>>,
}

impl MinisatReceiver {
    pub fn new(solver: Rc<RefCell<Solver>>) -> Self {
        MinisatReceiver {
            solver,
            literals: RefCell::new(vec![]),
        }
    }

    pub fn literal(&self, index: usize) -> Option<Lit> {
        let literals = self.literals.borrow();
        literals.get(index).copied()
    }
}

impl<'a> CNFReceiver for &MinisatReceiver {
    type ProblemResult = ();

    fn receive(&mut self, lits: &[Literal]) -> Result<(), eyre::Error> {
        let mut solver = self.solver.borrow_mut();
        let mut literals = self.literals.borrow_mut();

        for (_, idx) in lits {
            let idx = idx.0 as usize;
            while idx >= literals.len() {
                match solver.new_lit() {
                    Bool::Const(_) => {
                        panic!("New literal should not result in boolean constant")
                    }
                    Bool::Lit(lit) => literals.push(lit),
                }
            }
        }

        log::trace!("Adding clause {:?}", lits);

        solver.add_clause(lits.iter().map(|(sign, idx)| {
            if *sign {
                Bool::Lit(literals[idx.0 as usize])
            } else {
                !Bool::Lit(literals[idx.0 as usize])
            }
        }));

        Ok(())
    }

    fn finish(self) -> Result<Self::ProblemResult, eyre::Error> {
        Ok(())
    }
}
