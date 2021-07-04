use lalrpop_util::lalrpop_mod;

mod parse_error;

lalrpop_mod!(
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::all, unused_parens))]
    parser
);

pub use crate::parse_error::WrappedLalrpopError;
pub use crate::parser::{
    DocumentParser, InteractiveDocumentParser, SingleDelimitedItemParser,
    SingleInteractiveItemParser,
};
