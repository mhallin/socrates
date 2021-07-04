use std::fs::File;
use std::io::{Seek, SeekFrom, Write};

use failure::Error;

use crate::cnf::Literal;
use crate::gaf::GAFIndex;

pub trait CNFReceiver {
    type ProblemResult;
    fn receive(&mut self, lits: &[Literal]) -> Result<(), Error>;
    fn receive_unit(&mut self, l: Literal) -> Result<(), Error> {
        self.receive(&[l])
    }
    fn finish(self) -> Result<Self::ProblemResult, Error>;
}

pub struct DIMACSReceiver {
    target: File,
    max_literal: GAFIndex,
    clause_count: usize,
}

impl DIMACSReceiver {
    pub fn new(mut target: File) -> Result<Self, Error> {
        writeln!(target, "p cnf                              ")?;
        Ok(DIMACSReceiver {
            target,
            max_literal: GAFIndex(0),
            clause_count: 0,
        })
    }
}

impl CNFReceiver for DIMACSReceiver {
    type ProblemResult = ();

    fn receive(&mut self, lits: &[Literal]) -> Result<(), Error> {
        log::trace!("Writing clause to DIMACS: {:?}", lits);
        for (sign, idx) in lits {
            let sign = if *sign { 1i32 } else { -1 };
            write!(self.target, "{} ", sign * (idx.0 as i32 + 1))?;
            if *idx > self.max_literal {
                self.max_literal = *idx;
            }
        }

        writeln!(self.target, "0")?;
        self.clause_count += 1;

        Ok(())
    }

    fn finish(mut self) -> Result<(), Error> {
        self.target.seek(SeekFrom::Start(0))?;
        write!(
            self.target,
            "p cnf {} {}",
            self.max_literal.0 + 1,
            self.clause_count
        )?;
        Ok(())
    }
}
