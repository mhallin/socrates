#[macro_use] extern crate failure;
#[macro_use] extern crate log;
extern crate colored;

mod error;
mod context;

pub use error::{CompilerError, Error};
pub use context::ErrorContext;
