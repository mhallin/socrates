use std::error;
use std::fmt;

use lalrpop_util::ParseError;

use socrates_errors::CompilerError;

use parser;

#[derive(Debug)]
pub struct WrappedLalrpopError<'input>(pub ParseError<usize, parser::Token<'input>, &'static str>);

impl<'input> fmt::Display for WrappedLalrpopError<'input> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            ParseError::InvalidToken { .. } => write!(fmt, "Invalid token"),
            ParseError::UnrecognizedToken {
                token: Some((_, t, _)),
                ..
            } => write!(fmt, "Unexpected token `{}`", t),
            ParseError::UnrecognizedToken { token: None, .. } => write!(fmt, "Unexpected token"),
            ParseError::ExtraToken {
                token: (_, t, _), ..
            } => write!(fmt, "Unexpected token `{}`", t),
            ParseError::User { error } => write!(fmt, "{}", error),
        }
    }
}

impl<'input> error::Error for WrappedLalrpopError<'input> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.0.source()
    }
}

impl<'input> CompilerError for WrappedLalrpopError<'input> {
    fn location(&self) -> Option<(usize, usize)> {
        match &self.0 {
            ParseError::InvalidToken { location } => Some((*location, *location + 1)),
            ParseError::UnrecognizedToken {
                token: Some((start, _, end)),
                ..
            } => Some((*start, *end)),
            ParseError::UnrecognizedToken { token: None, .. } => None,
            ParseError::ExtraToken {
                token: (start, _, end),
            } => Some((*start, *end)),
            ParseError::User { .. } => None,
        }
    }

    fn note(&self) -> Option<String> {
        match &self.0 {
            ParseError::UnrecognizedToken { expected, .. } if !expected.is_empty() => {
                Some(format!("expected any of {}", expected.join(", ")))
            }
            _ => None,
        }
    }
}
