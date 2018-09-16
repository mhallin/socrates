#[macro_use] extern crate lalrpop_util;

extern crate socrates_ast;
extern crate socrates_errors;

mod parse_error;

lalrpop_mod!(
    #[cfg_attr(feature = "cargo-clippy", allow(clippy))] parser
);

pub use parser::{DocumentParser, SingleDelimitedItemParser};
pub use parse_error::WrappedLalrpopError;
