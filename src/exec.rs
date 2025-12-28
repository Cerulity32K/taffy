use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

use num::BigInt;
use tactical::Punctuated;
use thiserror::Error;

use crate::{
    combo::Grouped,
    lang::{self, Expr, Item, ItemKind, Items, Name, Term},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeShape {
    /// Dynamic type.
    Dynamic,
    /// An integer with arbitrary precision.
    Integer,
    /// A structure type.
    Structure(HashMap<String, Type>),
    /// A tuple type.
    Tuple(Vec<Type>),
    /// An array type.
    Array(Box<Type>, usize),
    /// A union/sum type.
    Union(HashMap<String, Type>),
    // /// The type is an unevaluated expression.
    // ///
    // /// Used for recursive types, such as `Node(T: type) :: { data: T, next: *rw Node(T) }` to prevent infinite recursion.
    // Unevaluated(Expression),
}
impl TypeShape {
    pub const fn void() -> Self {
        Self::empty_tuple()
    }
    pub fn never() -> Self {
        Self::empty_union()
    }
    pub const fn top() -> Self {
        Self::void()
    }
    pub fn bottom() -> Self {
        Self::never()
    }
    pub fn empty_structure() -> Self {
        Self::Structure(HashMap::new())
    }
    pub const fn empty_tuple() -> Self {
        Self::Tuple(vec![])
    }
    pub fn empty_array(ty: Type) -> Self {
        Self::Array(Box::new(ty), 0)
    }
    pub fn empty_union() -> Self {
        Self::Union(HashMap::new())
    }
}
impl Display for TypeShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeShape::Dynamic => write!(f, "@dyn"),
            TypeShape::Integer => write!(f, "int"),
            TypeShape::Structure(fields) => {
                write!(f, "{{ ")?;
                for (name, field) in fields {
                    write!(f, "{name}: {field}, ")?;
                }
                write!(f, "}}")
            }
            TypeShape::Tuple(values) => {
                write!(f, "(")?;
                for value in values {
                    write!(f, "{value}, ")?;
                }
                write!(f, ")")
            }
            TypeShape::Array(ty, length) => {
                write!(f, "[{ty} ** {length}]")
            }
            TypeShape::Union(variants) => {
                write!(f, "<")?;
                for (name, ty) in variants {
                    write!(f, "{name}: {ty}, ")?;
                }
                write!(f, ">")
            }
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    shape: Box<TypeShape>,
    markings: HashSet<String>,
}
impl From<TypeShape> for Type {
    fn from(shape: TypeShape) -> Self {
        Self {
            shape: Box::new(shape),
            markings: HashSet::new(),
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.markings.is_empty() {
            return Ok(());
        }
        write!(f, "marked [")?;
        for marking in &self.markings {
            write!(f, "${marking}, ")?;
        }
        write!(f, "] {}", self.shape)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueShape {
    Type(Type),
    Integer(BigInt),
    String(String),
    Structure(HashMap<String, Value>),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Variant(String, Box<Value>),
    Identifier(String),
    Lazy(Expr),
    Function(FunctionValue),
}
impl ValueShape {
    pub const fn void() -> Self {
        Self::empty_tuple()
    }
    pub const fn top() -> Self {
        Self::void()
    }
    pub fn empty_structure() -> Self {
        Self::Structure(HashMap::new())
    }
    pub const fn empty_tuple() -> Self {
        Self::Tuple(vec![])
    }
    pub fn empty_array() -> Self {
        Self::Array(vec![])
    }
    pub fn to_type(&self, exec: &mut ExecContext) -> Option<Type> {
        match self {
            ValueShape::Type(ty) => Some(ty.clone()),
            ValueShape::Integer(big_int) => None,
            ValueShape::String(str) => None,
            ValueShape::Structure(fields) => {
                let mut out = HashMap::with_capacity(fields.len());
                for (name, ty) in fields
                    .iter()
                    .map(|(name, value)| (name, value.to_type(exec)))
                {
                    out.insert(name.clone(), ty?);
                }
                Some(TypeShape::Structure(out).into())
            }
            ValueShape::Tuple(values) => {
                let mut out = Vec::with_capacity(values.len());
                for ty in values.iter().map(|value| value.to_type(exec)) {
                    out.push(ty?);
                }
                Some(TypeShape::Tuple(out).into())
            }
            ValueShape::Array(values) => {
                if values.is_empty() {
                    return Some(TypeShape::Array(Box::new(TypeShape::Dynamic.into()), 0).into());
                }
                let first_type = values.first().unwrap().to_type(exec)?;
                for i in 1..values.len() {
                    if values[i].to_type(exec)? != first_type {
                        return Some(TypeShape::Dynamic.into());
                    }
                }
                Some(TypeShape::Array(Box::new(first_type), values.len()).into())
            }
            ValueShape::Variant(name, value) => Some(
                TypeShape::Union(HashMap::from_iter([(name.clone(), value.to_type(exec)?)])).into(),
            ),
            ValueShape::Identifier(ident) => exec.get(Some(ident))?.to_type(exec),
            ValueShape::Lazy(expr) => exec.evaluate(expr.clone(), false).unwrap().to_type(exec),
            ValueShape::Function(_) => None,
        }
    }
}
impl Display for ValueShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueShape::Type(ty) => write!(f, "{ty}"),
            ValueShape::Integer(big_int) => todo!(),
            ValueShape::String(_) => todo!(),
            ValueShape::Structure(hash_map) => todo!(),
            ValueShape::Tuple(values) => todo!(),
            ValueShape::Array(values) => todo!(),
            ValueShape::Variant(_, value) => todo!(),
            ValueShape::Identifier(_) => todo!(),
            ValueShape::Lazy(punctuated) => todo!(),
            ValueShape::Function(function) => write!(f, "{function}"),
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Value {
    shape: ValueShape,
    markings: HashSet<String>,
}
impl Value {
    pub fn to_type(&self, exec: &mut ExecContext) -> Option<Type> {
        Some(Type {
            shape: self.shape.to_type(exec)?.shape,
            markings: self.markings.clone(),
        })
    }
}
impl From<ValueShape> for Value {
    fn from(shape: ValueShape) -> Self {
        Self {
            shape,
            markings: HashSet::new(),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.markings.is_empty() {
            return Ok(());
        }
        write!(f, "marked [")?;
        for marking in &self.markings {
            write!(f, "${marking}, ")?;
        }
        write!(f, "] {}", self.shape)
    }
}

static NEXT_FUNCTION_ID: AtomicU64 = AtomicU64::new(0);
pub fn new_function_id() -> u64 {
    NEXT_FUNCTION_ID.fetch_add(1, Ordering::Relaxed)
}
pub struct FunctionID {
    id: u64,
    name: String,
    arg_types: Vec<Type>,
    return_type: Type,
}

#[derive(Clone)]
pub enum FunctionValue {
    FunctionID(Arc<FunctionID>),
    Intrinsic(Arc<dyn Fn(Vec<Value>) -> Value>),
}
impl Debug for FunctionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionValue::FunctionID(id) => {
                write!(f, "fn(")?;
                for ty in &id.arg_types {
                    write!(f, "{ty}, ")?;
                }
                write!(f, ") -> {}", id.return_type)
            }
            FunctionValue::Intrinsic(_) => write!(f, "fn(intrinsic)"),
        }
    }
}
impl Display for FunctionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
impl PartialEq for FunctionValue {
    fn eq(&self, other: &Self) -> bool {
        // TODO
        false
    }
}
impl Eq for FunctionValue {}

#[derive(Clone, Copy, Debug, Error, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecError {}

pub struct Scope {
    pub locals: HashMap<String, Value>,
    pub dotvar: Option<Value>,
}
impl Scope {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            dotvar: None,
        }
    }
    pub fn prelude() -> Self {
        Self {
            locals: HashMap::from_iter([(
                "int".to_owned(),
                ValueShape::Type(TypeShape::Integer.into()).into(),
            )]),
            dotvar: None,
        }
    }
}

pub struct ExecContext {
    scopes: Vec<Scope>,
}
impl ExecContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::prelude()],
        }
    }
    pub fn new_scope<T>(&mut self, callback: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(Scope::new());
        let output = callback(self);
        self.scopes.pop();
        output
    }
    // TODO no flags
    pub fn evaluate(&mut self, expr: Expr, keep_identifiers: bool) -> Result<Value, ExecError> {
        let mut or_terms = expr.items;
        if or_terms.len() > 1 {
            todo!("or combination");
        }
        let mut and_terms = or_terms.remove(0).0.items;
        if and_terms.len() > 1 {
            todo!("and combination");
        }
        let mut binary_terms = and_terms.remove(0).0.items;
        if binary_terms.len() > 1 {
            todo!("binary operators");
        }
        let (term, _) = binary_terms.remove(0);
        match term {
            Term::Delimited(grouped) => self.evaluate(grouped.item, keep_identifiers),
            Term::UnaryOperation((prefix, leaf, tight_postfix, loose_postfix)) => {
                if prefix.len() > 1 || tight_postfix.len() > 1 || loose_postfix.len() > 1 {
                    todo!("unary operations");
                }
                match leaf {
                    lang::TermLeaf::Name(name) => {
                        let name = self.evaluate_name(&name)?;
                        self.get(Some(&name)).ok_or_else(|| todo!("invalid name"))
                    }
                    lang::TermLeaf::String(str) => Ok(ValueShape::String(str.0).into()),
                    lang::TermLeaf::Integer(int) => Ok(ValueShape::Integer(int.0.into()).into()),
                    lang::TermLeaf::Intrinsic((_, intrinsic_name)) => {
                        let name = self.evaluate_name(&intrinsic_name)?;
                        self.evaluate_intrinsic(&name, None)
                            .ok_or_else(|| todo!("unable to evaluate intrinsic"))
                    }
                    lang::TermLeaf::Structure(structure) => todo!(),
                    lang::TermLeaf::Block(expressions) => {
                        self.new_scope(|ctx| {
                            expressions.item.items.iter().for_each(|expr| {
                                ctx.evaluate(expr.0.clone(), false).expect("dfsafsrrweqweq"); // TODO
                            });
                        });
                        Ok(ValueShape::void().into()) // TODO block values
                    }
                    lang::TermLeaf::Delimited(expr) => todo!(),
                    lang::TermLeaf::Tuple(items) => todo!(),
                    lang::TermLeaf::ElementwiseArray(elements) => todo!(),
                    lang::TermLeaf::ReplicatedArray(inner) => todo!(),
                    lang::TermLeaf::Union(_) => todo!(),
                }
            }
            Term::Declaration(_) => todo!(),
        }
    }
    pub fn evaluate_name(&mut self, name: &Name) -> Result<String, ExecError> {
        match name {
            Name::Raw(ident) => Ok(ident.repr.clone()),
            Name::FromExpression((_, expr)) => match self.evaluate(expr.clone(), true).unwrap() {
                Value {
                    shape: ValueShape::Identifier(ident),
                    ..
                } => Ok(ident),
                other => panic!("`{other}` is not an identifier"),
            },
        }
    }
    pub fn get(&self, name: Option<&str>) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(name) = name {
                if scope.locals.contains_key(name) {
                    return scope.locals.get(name).cloned();
                }
            } else {
                if scope.dotvar.is_some() {
                    return scope.dotvar.clone();
                }
            }
        }
        None
    }
    /// Evaluates an intrinsic.
    ///
    /// In forms of direct calls (`@<ident>(<args>)`), `args` is `Some`.
    /// Otherwise, **although an attempt might be made call the intrinsic indirectly**, `args` will be None.
    /// Some intrinsics require direct calling, as they may fudge with expression evaluation.
    pub fn evaluate_intrinsic(
        &mut self,
        name: &str,
        args: Option<Vec<Expr>>,
    ) -> Option<Value> {
        let make_fn: fn(fn(Vec<Value>) -> Value) -> Value =
            |f| ValueShape::Function(FunctionValue::Intrinsic(Arc::new(f))).into();
        let result: Value = match name {
            "dbg" => make_fn(|args| {
                args.iter().for_each(|arg| println!("{}", arg));
                ValueShape::void().into()
            }),
            "dyn" => ValueShape::Type(TypeShape::Dynamic.into()).into(),
            _ => panic!("unknown intrinsic `{name}`"),
        };
        if let Some(args) = args {
            if let ValueShape::Function(function) = &result.shape {
                match function {
                    FunctionValue::FunctionID(_) => todo!(),
                    FunctionValue::Intrinsic(intrinsic) => {
                        Some(intrinsic(
                            args.iter()
                                .map(|arg| self.evaluate(arg.clone(), false).unwrap())
                                .collect(),
                        ))
                    }
                }
            } else {
                todo!("intrinsic @{name} is not functional")
            }
        } else {
            Some(result)
        }
    }
}

pub fn execute_main(items: Items) -> Result<Value, ExecError> {
    let mut context = ExecContext::new();
    for (item, _) in items.items {
        match item {
            Item::Definition(kind) => match kind {
                ItemKind::Function((name, _, _, expr)) => {
                    if context.evaluate_name(&name)? == "main" {
                        return Ok(context.new_scope(|ctx| ctx.evaluate(expr, false))?);
                    }
                }
            },
            _ => {}
        }
    }
    todo!("no main")
}
