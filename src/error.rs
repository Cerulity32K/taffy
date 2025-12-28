use std::{
    collections::HashMap,
    fmt::{self, Display, Write},
    io,
};

use tactical::{HasRecoverability, Recoverability, Span};
use thiserror::Error;

use crate::{
    // exec::ExecError,
    tok::{self, Token},
};

pub struct PadAdapter<'a> {
    buf: &'a mut (dyn fmt::Write + 'a),
    pub indent_size: usize,
    on_newline: bool,
}
impl<'a> PadAdapter<'a> {
    pub fn new(buf: &'a mut (dyn fmt::Write + 'a), indent_size: usize) -> Self {
        Self {
            buf,
            indent_size,
            on_newline: true,
        }
    }
}
impl fmt::Write for PadAdapter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for s in s.split_inclusive('\n') {
            if self.on_newline {
                for _ in 0..self.indent_size {
                    self.buf.write_char(' ')?;
                }
            }
            self.on_newline = s.ends_with('\n');
            self.buf.write_str(s)?;
        }
        Ok(())
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        if self.on_newline {
            for _ in 0..self.indent_size {
                self.buf.write_char(' ')?;
            }
        }
        self.on_newline = c == '\n';
        self.buf.write_char(c)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expected {
    Verbatim(Token),
    Identifier,
    Integer,
    String,
    StringCharacter,
    TokenCharacter,
    EscapeCharacter,
    MutabilitySpecifier,
    OpeningDelimiter(tok::DelimiterKind),
    ClosingDelimiter(tok::DelimiterKind),
}
impl Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Verbatim(token) => write!(f, "`{token}`"),
            Expected::Identifier => write!(f, "identifier"),
            Expected::Integer => write!(f, "integer"),
            Expected::String => write!(f, "string"),
            Expected::StringCharacter => write!(f, "string character"),
            Expected::TokenCharacter => write!(f, "token character"),
            Expected::EscapeCharacter => write!(f, "escape character"),
            Expected::MutabilitySpecifier => write!(f, "mutability specifier"),
            Expected::OpeningDelimiter(delimiter_kind) => {
                write!(f, "`{}`", delimiter_kind.opening())
            }
            Expected::ClosingDelimiter(delimiter_kind) => {
                write!(f, "`{}`", delimiter_kind.closing())
            }
        }
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Got {
    InvalidCharacter(char),
    InvalidToken(Token),
    EOF,
}
impl Display for Got {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Got::InvalidCharacter(character) => write!(f, "invalid character {character:?}"),
            Got::InvalidToken(token) => write!(f, "invalid token `{token}`"),
            Got::EOF => write!(f, "end of file"),
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
    Mismatch(Expected, Got),
    LeftoverToken(Token),
    AllBranchesFailed(HashMap<String, ParseError>),
    Unfused,
}
impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorKind::Mismatch(expected, got) => {
                write!(f, "expected {expected}, got {got}")
            }
            ParseErrorKind::LeftoverToken(token) => write!(f, "leftover token `{token}`"),
            ParseErrorKind::AllBranchesFailed(branches) => {
                write!(f, "tried {} branch", branches.len())?;
                if branches.len() != 1 {
                    write!(f, "es")?;
                }
                writeln!(f, ", all failed:")?;
                let mut adapter = PadAdapter::new(f, 2);
                for (item, error) in branches {
                    writeln!(adapter, "- `{item}`: {}", error.kind)?;
                }
                Ok(())
            }
            ParseErrorKind::Unfused => write!(f, "elements were not fused"),
        }
    }
}
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
    pub recoverability: Recoverability,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} error: `{}`", self.recoverability, self.kind)
    }
}
impl HasRecoverability for ParseError {
    fn recoverability(&self) -> Recoverability {
        self.recoverability
    }
    fn set_recoverability(&mut self, new_recoverability: Recoverability) {
        self.recoverability = new_recoverability;
    }
}
impl ParseError {
    pub fn recoverable(kind: ParseErrorKind, spanning: Span) -> Self {
        Self {
            kind,
            span: spanning,
            recoverability: Recoverability::Recoverable,
        }
    }
    pub fn unrecoverable(kind: ParseErrorKind, spanning: Span) -> Self {
        Self {
            kind,
            span: spanning,
            recoverability: Recoverability::Unrecoverable,
        }
    }
    pub fn branches_failed<'a>(
        errors: impl IntoIterator<Item = (&'a str, ParseError)>,
        first_span: Span,
    ) -> Self {
        Self::recoverable(
            ParseErrorKind::AllBranchesFailed(HashMap::from_iter(
                errors.into_iter().map(|(k, v)| (k.to_owned(), v)),
            )),
            first_span,
        )
    }
}

#[derive(Debug, Error)]
pub enum CompilerError {
    // TODO untuple this (source, error)
    Parse(String, ParseError),
    // Exec(#[from] ExecError),
    IO(#[from] io::Error),
}
impl From<(String, ParseError)> for CompilerError {
    fn from((source, error): (String, ParseError)) -> Self {
        Self::Parse(source, error)
    }
}
impl Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerError::Parse(source, ParseError { span, kind, .. }) => {
                if span.is_null() {
                    return write!(f, "Parse error at unknown location: {kind}");
                }
                let mut lines = source.lines().skip(span.start_row().saturating_sub(1));
                if let Some(line) = lines.next() {
                    writeln!(f, "{line}")?;
                    for _ in 1..span.start_col() {
                        write!(f, " ")?;
                    }
                    let end = if span.start_row() == span.end_row() {
                        span.end_col()
                    } else {
                        line.len()
                    };
                    for _ in span.start_col()..=end {
                        write!(f, "^")?;
                    }
                    writeln!(f)?;
                }
                for line in (&mut lines).take((span.end_row() - span.start_row()).saturating_sub(1))
                {
                    writeln!(f, "{line}")?;
                    for _ in 0..line.len() {
                        write!(f, "^")?;
                    }
                    writeln!(f)?;
                }
                if span.start_row() != span.end_row()
                    && let Some(line) = lines.next()
                {
                    writeln!(f, "{line}")?;
                    for _ in 1..span.end_col() {
                        write!(f, "^")?;
                    }
                }
                writeln!(f)?;
                write!(f, "Parse error at {span}: {kind}")
            }
            // CompilerError::Exec(err) => write!(f, "Execution error: {err}"),
            CompilerError::IO(error) => write!(f, "I/O error: {error}"),
        }
    }
}
