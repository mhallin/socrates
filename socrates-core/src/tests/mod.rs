use std::io::stderr;

use fnv::FnvHashMap;

use socrates_ast::parsed::{ActiveType, TypeRef};
use socrates_ast::simple::Formula;
use socrates_errors::{
    eyre::{format_err, Error},
    ErrorContext,
};
use socrates_parser::{DocumentParser, WrappedLalrpopError};

use crate::grounder::Emitter;
use crate::handle_item;
use crate::predicate_completion::Bucket;
use crate::printer::DisplayType;
use crate::types::TypeStorage;

mod instance_iterator;
mod nnf;
mod type_inferencer;

pub struct Context<'i> {
    pub types: TypeStorage<'i>,
    pub buckets: FnvHashMap<&'i str, Bucket>,
    pub emitter: TestEmitter<'i>,
}

pub struct TestEmitter<'i> {
    pub formulas: Vec<Formula<'i>>,
}

impl<'i> Context<'i> {
    pub fn new() -> Self {
        Context {
            types: TypeStorage::new(),
            buckets: FnvHashMap::default(),
            emitter: TestEmitter { formulas: vec![] },
        }
    }

    pub fn exec_unit(&mut self, source: &'i str) -> Result<(), Error> {
        let mut errors = ErrorContext::new("<test>", source);

        errors
            .block_exec(|errors| {
                let document = match DocumentParser::new().parse(errors, &source) {
                    Ok(document) => document,
                    Err(e) => {
                        errors.push_error(WrappedLalrpopError(e));
                        vec![]
                    }
                };

                for item in document {
                    handle_item(
                        &mut self.emitter,
                        item,
                        &mut self.types,
                        errors,
                        &mut self.buckets,
                    );
                }

                Ok(())
            })
            .map_err(|e| {
                let _ = errors.write_user_friendly(&mut stderr());
                e
            })
    }

    pub fn exec(&mut self, source: &'i str) -> Result<Formula<'i>, Error> {
        let formulas_before = self.emitter.formulas.len();
        self.exec_unit(source)?;
        if self.emitter.formulas.len() > formulas_before {
            Ok(self.emitter.formulas[self.emitter.formulas.len() - 1].clone())
        } else {
            Err(format_err!("No formulas emitted"))
        }
    }

    pub fn get_type(&self, name: &str) -> TypeRef {
        self.types
            .get_type(name)
            .unwrap_or_else(|| panic!("Could not find type `{}`", name))
    }

    pub fn build_active_type(&self, name: &str, params: Vec<ActiveType>) -> ActiveType {
        ActiveType::Ref {
            to: self
                .types
                .get_type(name)
                .unwrap_or_else(|| panic!("Could not find type {}", name)),
            params,
        }
    }

    pub fn assert_variable_type(&self, f: &Formula<'_>, (s, v): (u32, u32), ty: &ActiveType) {
        fn inner_assert(
            types: &TypeStorage<'_>,
            f: &Formula<'_>,
            (s, v): (u32, u32),
            ty: &ActiveType,
        ) -> bool {
            use socrates_ast::simple::Formula::*;
            match f {
                Quantified(_, qs, qv, f) => {
                    if s == *qs {
                        assert_eq!(
                            ty,
                            &qv[v as usize].1,
                            "Expected variable `{}` (@{}:{}) to be of type `{}`, was resolved to `{}`",
                            qv[v as usize].0,
                            s, v,
                            DisplayType::new(ty, types),
                            DisplayType::new(&qv[v as usize].1, types));
                        true
                    } else {
                        inner_assert(types, f, (s, v), ty)
                    }
                }
                Conjunction(lhs, rhs) => {
                    inner_assert(types, lhs, (s, v), ty) || inner_assert(types, rhs, (s, v), ty)
                }
                Disjunction(lhs, rhs) => {
                    inner_assert(types, lhs, (s, v), ty) || inner_assert(types, rhs, (s, v), ty)
                }
                Predicate(_, _, _) | Constraint(_, _, _) | Contradiction | Tautology => false,
            }
        }

        assert!(
            inner_assert(&self.types, f, (s, v), ty),
            "Variable {}:{} not found",
            s,
            v
        );
    }
}

impl<'i> Emitter<'i> for TestEmitter<'i> {
    type FormulaResult = ();
    type ProblemResult = ();

    fn emit(&mut self, formula: &Formula<'i>, _: &TypeStorage<'i>) -> Result<(), Error> {
        self.formulas.push(formula.clone());
        Ok(())
    }

    fn finish(self) -> Result<(), Error> {
        Ok(())
    }
}

pub fn new_tal_context<'i>() -> Context<'i> {
    let mut ctx = Context::new();
    ctx.exec_unit(
        r"
:type value;
:type fluent[value];
:instances value location {l1};
:instances value uav {u1};
:instances value bool {false,true};

:function fluent[bool] is-open();
:function fluent[location] loc(uav);
:function fluent[T] foo(T);
:function fluent[T] foo3(T,S,S);

:integertype timepoint [0..16];

:predicate Holds(timepoint, fluent[T], T);
    ",
    )
    .expect("Could not create basic types");

    ctx
}
