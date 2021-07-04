use std::io;

use colored::Colorize;
use failure::format_err;

use crate::{CompilerError, Error};

#[derive(Debug)]
pub struct ErrorContext<'input> {
    errors: Vec<Box<dyn CompilerError + 'input>>,
    warnings: Vec<Box<dyn CompilerError + 'input>>,
    source: &'input str,
    line_starts: Vec<usize>,
    filename: &'input str,
}

impl<'input> ErrorContext<'input> {
    pub fn new(filename: &'input str, source: &'input str) -> Self {
        ErrorContext {
            errors: vec![],
            warnings: vec![],
            source,
            line_starts: find_line_starts(source),
            filename,
        }
    }

    pub fn block_exec<U, F: FnOnce(&mut Self) -> Result<U, failure::Error>>(
        &mut self,
        f: F,
    ) -> Result<U, failure::Error> {
        let error_count = self.errors.len();
        let value = f(self);

        if self.errors.len() == error_count {
            value
        } else {
            Err(format_err!("{}", self.errors[self.errors.len() - 1]))
        }
    }

    pub fn push_error<E: CompilerError + 'input>(&mut self, error: E) {
        self.errors.push(Box::new(error));
    }

    pub fn push_error_message<S: AsRef<str>>(&mut self, pos: (usize, usize), message: S) {
        let message = message.as_ref().to_owned();
        log::debug!("Pushing error: {}", message);
        self.errors.push(Box::new(Error::new(pos, message)));
    }

    pub fn push_warning<E: CompilerError + 'input>(&mut self, warning: E) {
        self.warnings.push(Box::new(warning));
    }

    pub fn push_warning_message<S: AsRef<str>>(&mut self, pos: (usize, usize), message: S) {
        let message = message.as_ref().to_owned();
        log::debug!("Pushing warning: {}", message);
        self.warnings.push(Box::new(Error::new(pos, message)));
    }

    pub fn write_user_friendly<W: io::Write>(&self, out: &mut W) -> Result<(), io::Error> {
        let mut issues = self
            .errors
            .iter()
            .map(|e| (true, e))
            .chain(self.warnings.iter().map(|e| (false, e)))
            .collect::<Vec<_>>();
        issues.sort_by_key(|(_, e)| e.location().unwrap_or((0, 0)));

        for (is_error, issue) in &issues {
            writeln!(
                out,
                "{}{}{}",
                if *is_error {
                    "error".bright_red()
                } else {
                    "warning".bright_yellow()
                }
                .bold(),
                ": ".bold(),
                format!("{}", issue).bold()
            )?;

            if let Some((start, end)) = issue.location() {
                let (line, start_col) = self.get_line_col(start);
                let line_source = self.get_line(line);
                let (end_line, end_col) = self.get_line_col(end);
                let end_col = if end_line != line {
                    line_source.len()
                } else {
                    end_col
                };
                let col_1 = format!("{} ", line + 1);

                let pipe = "|".bright_blue().bold();

                writeln!(
                    out,
                    "{0: <1$}{2} {3}:{4}:{5}",
                    "",
                    col_1.len() - 1,
                    "-->".bright_blue().bold(),
                    self.filename,
                    line + 1,
                    start_col + 1
                )?;
                writeln!(out, "{0: <1$}{2}", "", col_1.len(), pipe)?;
                writeln!(
                    out,
                    "{}{} {}",
                    col_1.bright_blue().bold(),
                    pipe,
                    line_source
                )?;

                let line = format!("{0:^<1$}", "", (end_col - start_col).max(1)).bold();
                writeln!(
                    out,
                    "{0: <1$}{2} {0: <3$}{4}",
                    "",
                    col_1.len(),
                    pipe,
                    start_col,
                    if *is_error {
                        line.bright_red()
                    } else {
                        line.bright_yellow()
                    }
                )?;

                if let Some(note) = issue.note() {
                    writeln!(
                        out,
                        "{0: <1$}{2} {3} {4}",
                        "",
                        col_1.len(),
                        "=".bright_blue().bold(),
                        "note:".bold(),
                        note
                    )?;
                }
            } else {
                writeln!(out, " --> {}", self.filename)?;
            }

            writeln!(out)?;
        }

        let error_count = self.errors.len();

        if error_count > 0 {
            writeln!(
                out,
                "{}{}",
                "error".bright_red().bold(),
                format!(
                    ": aborting due to {} error{}",
                    error_count,
                    if error_count != 1 { "s" } else { "" }
                )
                .bold()
            )?;
            writeln!(out)?;
        }

        Ok(())
    }

    fn get_line_col(&self, location: usize) -> (usize, usize) {
        let mut last_line_start = 0;
        for (line_index, line_start) in self.line_starts.iter().enumerate().skip(1) {
            if location < *line_start {
                return (line_index - 1, location - last_line_start);
            }
            last_line_start = *line_start;
        }

        let last_line_idx = self.line_starts.len() - 1;
        let last_line_start = self.line_starts[last_line_idx];
        (last_line_idx, location - last_line_start)
    }

    fn get_line(&self, line: usize) -> &str {
        let line_count = self.line_starts.len();
        let line_start = self.line_starts[line];
        let next_line_start = if line == line_count - 1 {
            self.source.len()
        } else {
            self.line_starts[line + 1] - 1
        };

        &self.source[line_start..next_line_start]
    }
}

fn find_line_starts(source: &str) -> Vec<usize> {
    [0usize]
        .iter()
        .copied()
        .chain(
            source
                .char_indices()
                .filter_map(|(index, ch)| if ch == '\n' { Some(index + 1) } else { None }),
        )
        .collect()
}
