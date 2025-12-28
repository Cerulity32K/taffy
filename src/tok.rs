use std::fmt::{self, Debug, Display};

use num::BigInt;
use tactical::{ParseContext, Span, Syntax, cursor};

use crate::{
    error::{Expected, Got, ParseError, ParseErrorKind},
    lang::Result,
};

/// Declares a syntactical element that only has one representation.
#[macro_export]
macro_rules! verbatim {
    ($token:expr => $name:ident: $token_type:ty | $error_type:ty[$expected:expr]) => {
        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $name(pub $crate::Span);
        impl<__D> $crate::Syntax<$token_type, $error_type, __D> for $name {
            fn from_tokens(
                tokens: &mut $crate::cursor!($token_type),
                context: $crate::ParseContext<__D>,
            ) -> ::std::result::Result<Self, $error_type> {
                let mut try_tokens = tokens.clone();
                match try_tokens.next() {
                    Some((span, token)) if token == ($token) => {
                        *tokens = try_tokens;
                        Ok(Self(span))
                    },
                    Some((span, got)) => Err($crate::ParseError::recoverable(
                        $crate::error::ParseErrorKind::Mismatch(($expected), $crate::error::Got::InvalidToken(got)),
                        span,
                    )),
                    None => Err($crate::ParseError::recoverable(
                        $crate::error::ParseErrorKind::Mismatch(($expected), $crate::error::Got::EOF),
                        context.eof_span(),
                    )),
                }
            }
            fn to_tokens(&self) -> Vec<($crate::Span, $token_type)> {
                vec![(self.0, ($token))]
            }
            fn span(&self) -> $crate::Span {
                self.0
            }
        }
    };
    ($token:expr => $name:ident: $token_type:ty | $error_type:ty) => {
        $crate::verbatim!($token => $name: $token_type | $error_type[$crate::error::Expected::Verbatim($token)]);
    };
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Punct(char),
    IdentifierLike { repr: String, escaped: bool },
    IntegerLiteral(BigInt),
    StringLiteral(String),
    Group(DelimiterKind, Vec<(Span, Token)>),
}
impl Token {
    pub fn any_identifier() -> Self {
        Self::IdentifierLike {
            repr: String::new(),
            escaped: false,
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Punct(punct) => write!(f, "{punct}"),
            Token::IdentifierLike { repr, escaped } => {
                if *escaped {
                    write!(f, "`{repr:?}`")
                } else {
                    write!(f, "{repr}")
                }
            }
            Token::IntegerLiteral(i) => write!(f, "{i}"),
            Token::StringLiteral(str) => write!(f, "{str:?}"),
            Token::Group(delimiter, tokens) => {
                write!(f, "{} ", delimiter.opening())?;
                for (_, token) in tokens {
                    write!(f, "{token} ")?;
                }
                write!(f, "{}", delimiter.closing())
            }
        }
    }
}

pub mod punct {
    use crate::combo::Fused;

    macro_rules! punct {
        ($name:ident: $punct:literal) => {
            $crate::verbatim!($crate::tok::Token::Punct($punct) => $name: $crate::tok::Token | $crate::error::ParseError);
        };
    }

    punct![Tilde: '~'];
    punct![Exclamation: '!'];
    punct![At: '@'];
    punct![Dollar: '$'];
    punct![Percent: '%'];
    punct![Caret: '^'];
    punct![Ampersand: '&'];
    punct![Asterisk: '*'];
    pub type Asterisk2 = Fused<Asterisk, Asterisk>;
    punct![Hyphen: '-'];
    punct![Equal: '='];
    punct![Plus: '+'];
    punct![Semicolon: ';'];
    punct![Colon: ':'];
    punct![Backslash: '\\'];
    punct![Pipe: '|'];
    punct![Comma: ','];
    punct![Period: '.'];
    punct![OpeningAngle: '<'];
    punct![ClosingAngle: '>'];
    punct![Slash: '/'];
    punct![Question: '?'];
    pub type ThinArrow = Fused<Hyphen, ClosingAngle>;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub span: Span,
    pub repr: String,
    pub escaped: bool,
}
impl<D> Syntax<Token, ParseError, D> for Ident {
    fn from_tokens(tokens: &mut cursor!(Token), context: ParseContext<D>) -> Result<Self> {
        match tokens.next() {
            Some((span, Token::IdentifierLike { repr, escaped })) => Ok(Self {
                span,
                repr,
                escaped,
            }),
            Some((span, got)) => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::Identifier, Got::InvalidToken(got)),
                span,
            )),
            None => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::Identifier, Got::EOF),
                context.eof_span(),
            )),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        vec![(
            self.span,
            Token::IdentifierLike {
                repr: self.repr.clone(),
                escaped: self.escaped,
            },
        )]
    }
    fn span(&self) -> Span {
        self.span
    }
}
impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.escaped {
            write!(f, "`{}`", self.repr)
        } else {
            write!(f, "{}", self.repr)
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum DelimiterKind {
    Parenthesis,
    Bracket,
    Brace,
}
impl DelimiterKind {
    pub fn opening(&self) -> char {
        match self {
            DelimiterKind::Parenthesis => '(',
            DelimiterKind::Bracket => '[',
            DelimiterKind::Brace => '{',
        }
    }
    pub fn closing(&self) -> char {
        match self {
            DelimiterKind::Parenthesis => ')',
            DelimiterKind::Bracket => ']',
            DelimiterKind::Brace => '}',
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerLiteral(pub BigInt, pub Span);
impl Syntax<Token, ParseError, ()> for IntegerLiteral {
    fn from_tokens(tokens: &mut cursor!(Token), context: ParseContext<()>) -> Result<Self> {
        match tokens.clone().next() {
            Some((span, Token::IntegerLiteral(i))) => {
                tokens.next();
                Ok(Self(i, span))
            }
            Some((span, got)) => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::Integer, Got::InvalidToken(got)),
                span,
            )),
            None => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::Integer, Got::EOF),
                context.eof_span(),
            )),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        vec![(self.1, Token::IntegerLiteral(self.0.clone()))]
    }
    fn span(&self) -> Span {
        self.1
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StringLiteral(pub String, pub Span);
impl Syntax<Token, ParseError, ()> for StringLiteral {
    fn from_tokens(tokens: &mut cursor!(Token), context: ParseContext<()>) -> Result<Self> {
        match tokens.clone().next() {
            Some((span, Token::StringLiteral(s))) => {
                tokens.next();
                Ok(Self(s, span))
            }
            Some((span, got)) => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::String, Got::InvalidToken(got)),
                span,
            )),
            None => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(Expected::String, Got::EOF),
                context.eof_span(),
            )),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        vec![(self.1, Token::StringLiteral(self.0.clone()))]
    }
    fn span(&self) -> Span {
        self.1
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Kw<const NAME: &'static str>(pub Span);
impl<const NAME: &'static str> Debug for Kw<NAME> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(NAME).field(&self.0).finish()
    }
}
impl<const NAME: &'static str, D> Syntax<Token, ParseError, D> for Kw<NAME> {
    fn from_tokens(
        tokens: &mut cursor!(Token),
        context: ParseContext<D>,
    ) -> std::result::Result<Self, ParseError> {
        match tokens.clone().next() {
            Some((span, Token::IdentifierLike { repr, .. })) if repr == NAME => Ok(Self(span)),
            Some((span, got)) => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(
                    Expected::Verbatim(Token::IdentifierLike {
                        repr: NAME.to_owned(),
                        escaped: false,
                    }),
                    Got::InvalidToken(got),
                ),
                span,
            )),
            None => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(
                    Expected::Verbatim(Token::IdentifierLike {
                        repr: NAME.to_owned(),
                        escaped: false,
                    }),
                    Got::EOF,
                ),
                context.eof_span(),
            )),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        vec![(
            self.0,
            Token::IdentifierLike {
                repr: NAME.to_owned(),
                escaped: false,
            },
        )]
    }
    fn span(&self) -> Span {
        self.0
    }
}

pub fn get_string_contents(
    chars: &mut cursor!(char),
    mut first_span: Span,
    end: char,
) -> Result<(Span, String)> {
    let mut string = String::new();
    while let Some((span, ch)) = chars.next() {
        match ch {
            '\n' => {
                return Err(ParseError::unrecoverable(
                    ParseErrorKind::Mismatch(
                        Expected::StringCharacter,
                        Got::InvalidCharacter('\n'),
                    ),
                    span,
                ));
            }
            '\\' => match chars.next().ok_or(ParseError::unrecoverable(
                ParseErrorKind::Mismatch(Expected::EscapeCharacter, Got::EOF),
                span.next_column(),
            ))? {
                (span, ch @ ('"' | '`' | '\\')) => {
                    first_span += span;
                    string.push(ch);
                }
                (span, 'n') => {
                    first_span += span;
                    string.push('\n');
                }
                (span, ch) => {
                    return Err(ParseError::unrecoverable(
                        ParseErrorKind::Mismatch(
                            Expected::EscapeCharacter,
                            Got::InvalidCharacter(ch),
                        ),
                        span,
                    ));
                }
            },
            ch if ch == end => {
                first_span += span;
                return Ok((first_span, string));
            }
            ch => {
                first_span += span;
                string.push(ch);
            }
        }
    }
    Err(ParseError::unrecoverable(
        ParseErrorKind::Mismatch(Expected::StringCharacter, Got::EOF),
        first_span.next_column(),
    ))
}

macro_rules! pat_punct {
    () => {
        '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '-' | '=' | '+' | ';' | ':' | '\\' | '|' | ',' | '.' | '<' | '>' | '/' | '?'
    };
}

/// Tokenises source code. Inner kernel for recursion and iteration.
///
/// The additional span is used for delimited group parsing;
/// if `closing_delimiter` is `Some(_)`, then it is the span of the closing delimiter,
/// otherwise `None` is returned.
pub fn tokenise_inner(
    chars: &mut cursor!(char),
    closing_delimiter: Option<DelimiterKind>,
) -> Result<(Vec<(Span, Token)>, Option<Span>)> {
    let mut output = vec![];
    let mut eof_span = Span::null();
    while let Some((mut first_span, ch)) = chars.next() {
        eof_span = first_span;
        match ch {
            '#' => {
                while let Some((_, ch)) = chars.next()
                    && ch != '\n'
                {}
            }
            '0'..='9' => {
                let mut number = BigInt::from(ch as u8) - b'0';
                while let Some((span, ch @ '0'..='9')) = chars.clone().next() {
                    chars.next();
                    number *= 10;
                    number += ch as u8 - b'0';
                    first_span += span;
                }
                output.push((first_span, Token::IntegerLiteral(number)));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut repr = ch.to_string();
                while let Some((span, ch @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_'))) =
                    chars.clone().next()
                {
                    chars.next();
                    first_span += span;
                    repr.push(ch);
                }
                output.push((
                    first_span,
                    Token::IdentifierLike {
                        repr,
                        escaped: false,
                    },
                ))
            }
            '"' | '`' => {
                let (span, contents) = get_string_contents(chars, first_span, ch)?;
                output.push((
                    span,
                    if ch == '"' {
                        Token::StringLiteral(contents)
                    } else {
                        Token::IdentifierLike {
                            repr: contents,
                            escaped: true,
                        }
                    },
                ));
            }
            '(' | '[' | '{' => {
                let delimiter = match ch {
                    '(' => DelimiterKind::Parenthesis,
                    '[' => DelimiterKind::Bracket,
                    '{' => DelimiterKind::Brace,
                    _ => unreachable!(),
                };
                let (tokens, closing_span) = tokenise_inner(chars, Some(delimiter))?;
                output.push((
                    first_span + closing_span.unwrap(),
                    Token::Group(delimiter, tokens),
                ))
            }
            ')' | ']' | '}'
                if closing_delimiter.is_some() && closing_delimiter.unwrap().closing() == ch =>
            {
                return Ok((output, Some(first_span)));
            }
            pat_punct!() => {
                output.push((first_span, Token::Punct(ch)));
            }
            other if !other.is_whitespace() => {
                return Err(ParseError::recoverable(
                    ParseErrorKind::Mismatch(
                        Expected::StringCharacter,
                        Got::InvalidCharacter(other),
                    ),
                    first_span,
                ));
            }
            _ => {}
        }
    }
    if closing_delimiter.is_some() {
        eof_span.shift_forwards();
        return Err(ParseError::unrecoverable(
            ParseErrorKind::Mismatch(
                Expected::ClosingDelimiter(closing_delimiter.unwrap()),
                Got::EOF,
            ),
            eof_span,
        ));
    }
    Ok((output, None))
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanningChars<I> {
    iter: I,
    span: Span,
}
impl<I: Iterator<Item = char>> Iterator for SpanningChars<I> {
    type Item = (Span, char);
    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.iter.next()?;
        let span = self.span;
        if ch == '\n' {
            self.span.shift_down();
        } else {
            self.span.shift_forwards();
        }
        Some((span, ch))
    }
}

/// Tokenises source code.
pub fn tokenise(str: &str) -> Result<Vec<(Span, Token)>> {
    Ok(tokenise_inner(
        &mut SpanningChars {
            iter: str.chars(),
            span: Span::start(),
        },
        None,
    )?
    .0)
}
