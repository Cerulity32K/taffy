use std::marker::PhantomData;

use tactical::{ParseContext, Span, Syntax, cursor};

use crate::{
    error::{Expected, Got, ParseError, ParseErrorKind},
    lang::Result,
    tok::{DelimiterKind, Token},
};

pub trait Delimiter {
    const DELIMITER: DelimiterKind;
}
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Parenthesis;
impl Delimiter for Parenthesis {
    const DELIMITER: DelimiterKind = DelimiterKind::Parenthesis;
}
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bracket;
impl Delimiter for Bracket {
    const DELIMITER: DelimiterKind = DelimiterKind::Bracket;
}
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Brace;
impl Delimiter for Brace {
    const DELIMITER: DelimiterKind = DelimiterKind::Brace;
}
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Grouped<D: Delimiter, S: Syntax<Token, ParseError, ()>> {
    pub item: S,
    pub span: Span,
    _pd: PhantomData<D>,
}
impl<D: Delimiter, S: Syntax<Token, ParseError, ()>> Syntax<Token, ParseError, ()>
    for Grouped<D, S>
{
    fn from_tokens(tokens: &mut cursor!(Token), context: ParseContext<()>) -> Result<Self> {
        match tokens.next() {
            Some((span, Token::Group(delimiter, inner_tokens))) if D::DELIMITER == delimiter => {
                let mut iter = inner_tokens.into_iter();
                let item = S::from_tokens(&mut iter, context)?;
                if let Some((span, leftover)) = iter.next() {
                    return Err(ParseError::recoverable(
                        ParseErrorKind::LeftoverToken(leftover),
                        span,
                    ));
                }
                Ok(Self {
                    item,
                    span,
                    _pd: PhantomData,
                })
            }
            Some((span, other)) => {
                return Err(ParseError::recoverable(
                    ParseErrorKind::Mismatch(
                        Expected::OpeningDelimiter(D::DELIMITER),
                        Got::InvalidToken(other),
                    ),
                    span,
                ));
            }
            None => {
                return Err(ParseError::recoverable(
                    ParseErrorKind::Mismatch(Expected::OpeningDelimiter(D::DELIMITER), Got::EOF),
                    context.eof_span(),
                ));
            }
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        vec![(self.span, Token::Group(D::DELIMITER, self.item.to_tokens()))]
    }
    fn span(&self) -> Span {
        self.span + self.item.span()
    }
}

/// Matches two elements that are adjacent to each other.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fused<T, U>(pub T, pub U);
impl<Tok, D: Clone, T: Syntax<Tok, ParseError, D>, U: Syntax<Tok, ParseError, D>>
    Syntax<Tok, ParseError, D> for Fused<T, U>
{
    fn from_tokens(tokens: &mut cursor!(Tok), context: ParseContext<D>) -> Result<Self> {
        let left = T::from_tokens(tokens, context.clone())?;
        let right = U::from_tokens(tokens, context)?;
        if !left.span().is_left_adjacent_to(right.span()) {
            return Err(ParseError::recoverable(
                ParseErrorKind::Unfused,
                left.span() + right.span(),
            ));
        }
        return Ok(Self(left, right));
    }
    fn to_tokens(&self) -> Vec<(Span, Tok)> {
        let mut tokens = self.0.to_tokens();
        tokens.append(&mut self.1.to_tokens());
        tokens
    }
    fn span(&self) -> Span {
        self.0.span() + self.1.span()
    }
}
