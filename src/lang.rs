use std::fmt::Display;

use tactical::{
    DenyTrailing, Force, ParseContext, ParseResultExt, Punctuated, RequireOne, RequireTrailing,
    Span, Syntax, Try, cursor, syntax,
};

use crate::{
    combo::{Brace, Bracket, Fused, Grouped, Parenthesis},
    error::{Expected, Got, ParseError, ParseErrorKind},
    tok::{self, Kw, Token},
};

pub type Result<T> = std::result::Result<T, ParseError>;

type Intrinsic = (Tok![@], Name);
type Marking = (Tok![$], Name);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum TermLeaf: Token, () => !ParseError[ParseError::branches_failed] {
        "name" => Name(Name),
        "string" => String(tok::StringLiteral),
        "integer" => Integer(tok::IntegerLiteral),
        "intrinsic" => Intrinsic(Intrinsic),

        "structure" => Structure(Box<Structure>),
        "block" => Block(Box<Block>),

        "delimited" => Delimited(Box<Delimited>),
        "tuple" => Tuple(Box<Tuple>),

        "elementwise array" => ElementwiseArray(Box<ElementwiseArray>),
        "replicated array" => ReplicatedArray(Box<ReplicatedArray>),

        "union" => Union(Box<Union>),
    }
);

syntax!(
    [#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum OperatorPunct: Token, () => !ParseError[ParseError::branches_failed] {
        "tilde" => Tilde(Tok![~]),
        "exclamation" => Exclamation(Tok![!]),
        "dollar" => Dollar(Tok![$]),
        "percent" => Percent(Tok![%]),
        "caret" => Caret(Tok![^]),
        "ampersand" => Ampersand(Tok![&]),
        "asterisk" => Asterisk(Tok![*]),
        "hyphen" => Hyphen(Tok![-]),
        "equal" => Equal(Tok![=]),
        "plus" => Plus(Tok![+]),
        "backslash" => Backslash(Tok![b/]),
        "pipe" => Pipe(Tok![|]),
        "open angle" => OpenAngle(Tok![<]),
        "closed angle" => ClosedAngle(Tok![>]),
        "slash" => Slash(Tok![/]),
        "question" => Question(Tok![?]),
    }
);
impl Display for OperatorPunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Tilde(_) => '~',
                Self::Exclamation(_) => '!',
                Self::Dollar(_) => '$',
                Self::Percent(_) => '%',
                Self::Caret(_) => '^',
                Self::Ampersand(_) => '&',
                Self::Asterisk(_) => '*',
                Self::Hyphen(_) => '-',
                Self::Equal(_) => '=',
                Self::Plus(_) => '+',
                Self::Backslash(_) => '\\',
                Self::Pipe(_) => '|',
                Self::OpenAngle(_) => '<',
                Self::ClosedAngle(_) => '>',
                Self::Slash(_) => '/',
                Self::Question(_) => '?',
            }
        )
    }
}

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum OperatorPunctCons: Token, () => !ParseError[ParseError::branches_failed] {
        "cons" => Cons(Fused<OperatorPunct, Box<OperatorPunctCons>>),
        "last" => Last(OperatorPunct),
    }
);
impl Display for OperatorPunctCons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorPunctCons::Cons(Fused(op, ops)) => write!(f, "{op}{ops}"),
            OperatorPunctCons::Last(op) => write!(f, "{op}"),
        }
    }
}

pub type MethodCallOperator = (Tok![.], Name, Tuple);
pub type FieldAccessOperator = (Tok![.], Name);
pub type DereferenceOperator = (Tok![.], Tok![*]);
pub type ArrowOperator = (Tok![*], Box<ShapeOperator>);
pub type ReshapeOperator = (Tok![.], Term);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum ShapeOperator: Token, () => !ParseError[ParseError::branches_failed] {
        "method call" => MethodCall(MethodCallOperator),
        "field access" => FieldAccess(FieldAccessOperator),
        "dereference" => Dereference(DereferenceOperator),
        "arrow" => Arrow(ArrowOperator),
        "reshape" => Reshape(ReshapeOperator),
    }
);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum TightPostfixUnaryOperator: Token, () => !ParseError[ParseError::branches_failed] {
        "call" => Call(Tuple),
        "shape" => Shape(ShapeOperator)
    }
);

pub type CastOperator = (Tok![->], Force<Term>);
syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum LoosePostfixUnaryOperator: Token, () => !ParseError[ParseError::branches_failed] {
        "cast" => Cast(CastOperator),
    }
);

pub type UnaryOperation = (
    Vec<OperatorPunct>,
    TermLeaf,
    Vec<TightPostfixUnaryOperator>,
    Vec<LoosePostfixUnaryOperator>,
);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum Term: Token, () => !ParseError[ParseError::branches_failed] {
        "delimited" => Delimited(Delimited),
        "unary operation" => UnaryOperation(UnaryOperation),

        "declaration" => Declaration(DeclExpr),
    }
);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Mutability {
    pub span: Span,
    pub readable: bool,
    pub writeable: bool,
}
impl Syntax<Token, ParseError, ()> for Mutability {
    fn from_tokens(tokens: &mut cursor!(Token), context: ParseContext<()>) -> Result<Self> {
        let spec = tok::Ident::from_tokens(tokens, context)?;
        let span = spec.span;
        if spec.escaped {
            return Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(
                    Expected::MutabilitySpecifier,
                    Got::InvalidToken(Token::IdentifierLike {
                        repr: spec.repr,
                        escaped: spec.escaped,
                    }),
                ),
                spec.span,
            ));
        }
        match &spec.repr[..] {
            "r" => Ok(Self {
                span,
                readable: true,
                writeable: false,
            }),
            "w" => Ok(Self {
                span,
                readable: false,
                writeable: true,
            }),
            "rw" | "wr" => Ok(Self {
                span,
                readable: true,
                writeable: true,
            }),
            "nrw" | "nwr" => Ok(Self {
                span,
                readable: false,
                writeable: false,
            }),
            _ => Err(ParseError::recoverable(
                ParseErrorKind::Mismatch(
                    Expected::MutabilitySpecifier,
                    Got::InvalidToken(Token::IdentifierLike {
                        repr: spec.repr,
                        escaped: spec.escaped,
                    }),
                ),
                spec.span,
            )),
        }
    }
    fn to_tokens(&self) -> Vec<(Span, Token)> {
        let token = match self {
            Self {
                readable: true,
                writeable: false,
                ..
            } => Token::IdentifierLike {
                repr: "r".to_owned(),
                escaped: false,
            },
            Self {
                readable: false,
                writeable: true,
                ..
            } => Token::IdentifierLike {
                repr: "w".to_owned(),
                escaped: false,
            },
            Self {
                readable: true,
                writeable: true,
                ..
            } => Token::IdentifierLike {
                repr: "rw".to_owned(),
                escaped: false,
            },
            Self {
                readable: false,
                writeable: false,
                ..
            } => Token::IdentifierLike {
                repr: "nrw".to_owned(),
                escaped: false,
            },
        };
        vec![(self.span, token)]
    }
    fn span(&self) -> Span {
        self.span
    }
}

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum Name: Token, () => !ParseError[ParseError::branches_failed] {
        "raw" => Raw(tok::Ident),
        "from expression" => FromExpression((Kw<"ident">, Expr)),
    }
);

pub type BinaryExpr = Punctuated<Term, OperatorPunctCons, DenyTrailing, RequireOne>;
pub type AndExpr = Punctuated<BinaryExpr, Kw<"and">, DenyTrailing, RequireOne>;
pub type Expr = Punctuated<AndExpr, Kw<"or">, DenyTrailing, RequireOne>;

pub type Field = (Name, Force<Tok![:]>, Force<Expr>);

pub type Structure = Grouped<Brace, (Try<Field>, Option<Punctuated<Field, Tok![,]>>)>;

pub type Delimited = Grouped<Parenthesis, Expr>;
pub type Tuple = Grouped<Parenthesis, Punctuated<Expr, Tok![,]>>;

pub type ElementwiseArray = Grouped<Bracket, Punctuated<Expr, Tok![,]>>;
pub type ReplicatedArray = Grouped<Bracket, (Expr, Tok![**], Expr)>;

pub type Variant = (Name, Option<(Tok![:], Force<Expr>)>);
pub type Union = (Tok![<], Punctuated<Variant, Tok![,]>, Tok![>]);

pub type Block = Grouped<Brace, Punctuated<Expr, Tok![;]>>;

pub type DeclExpr = (Kw<"let">, Pattern, Tok![=], Expr);

pub type BinderPattern = Name;
pub type MutabilityPattern = (Mutability, BinderPattern);
pub type TuplePattern = Grouped<Parenthesis, Punctuated<Pattern, Tok![,]>>;
pub type MarkedPattern = (Kw<"marked">, Marking, Box<Pattern>);
syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum Pattern: Token, () => !ParseError[ParseError::branches_failed] {
        "binder" => Binder(BinderPattern),
        "mutability" => Mutability(MutabilityPattern),
        "tuple" => Tuple(TuplePattern),
        "marked" => Marked(MarkedPattern),
    }
);

pub type Param = (Pattern, Force<Tok![:]>, Force<Expr>);
pub type Params = Grouped<Parenthesis, Punctuated<Param, Tok![,], RequireTrailing>>;
pub type FunctionDecl = (Name, Option<Params>, Tok![=], Expr);
pub type MarkingDecl = (Kw<"marking">, Marking);

pub type ForallSpec = (Kw<"forall">, Params);
pub type EnsureConstraint = (Kw<"ensure">, Expr, Tok![->], Expr);
pub type RequireConstraint = (Kw<"require">, Expr);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum Constraint: Token, () => !ParseError[ParseError::branches_failed] {
        "ensure" => Ensure(EnsureConstraint),
        "require" => Require(RequireConstraint),
    }
);

syntax!(
    [#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]]
    pub enum Item: Token, () => !ParseError[ParseError::branches_failed] {
        "function" => Function(FunctionDecl),
        "marking" => Marking(MarkingDecl),
        "constraint" => Constraint((ForallSpec, Constraint))
    }
);

pub type Items = Punctuated<Item, Force<Tok![;]>>;
