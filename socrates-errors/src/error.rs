use std::{error, fmt};

pub trait CompilerError: error::Error {
    fn location(&self) -> Option<(usize, usize)>;
    fn note(&self) -> Option<String> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pos: (usize, usize),
    message: String,
}

impl Error {
    pub fn new(pos: (usize, usize), message: String) -> Self {
        Error { pos, message }
    }

    pub fn pos(&self) -> (usize, usize) {
        self.pos
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl CompilerError for Error {
    fn location(&self) -> Option<(usize, usize)> {
        Some(self.pos)
    }
}
