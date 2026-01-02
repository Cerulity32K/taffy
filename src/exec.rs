use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

use num::BigInt;
use thiserror::Error;

use crate::{
    error::CompilerError,
    lang::{self, Expr, Item, Items, Name, Term},
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
        if !self.markings.is_empty() {
            write!(f, "marked [")?;
            for marking in &self.markings {
                write!(f, "${marking}, ")?;
            }
            write!(f, "] ")?;
        }
        write!(f, "{}", self.shape)
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
            ValueShape::Integer(_) => None,
            ValueShape::String(_) => None,
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
            ValueShape::Lazy(expr) => exec.evaluate(expr.clone(), false).ok()?.to_type(exec),
            ValueShape::Function(_) => None,
        }
    }
}
impl Display for ValueShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueShape::Type(ty) => write!(f, "{ty}"),
            ValueShape::Integer(big_int) => write!(f, "{big_int}"),
            ValueShape::String(s) => write!(f, "{s:?}"),
            ValueShape::Structure(map) => {
                write!(f, "{{ ")?;
                for (k, v) in map {
                    write!(f, "{k}: {v}, ")?;
                }
                write!(f, "}}")
            }
            ValueShape::Tuple(values) => {
                write!(f, "(")?;
                for v in values {
                    write!(f, "{v}, ")?;
                }
                write!(f, ")")
            }
            ValueShape::Array(values) => {
                write!(f, "[")?;
                for v in values {
                    write!(f, "{v}, ")?;
                }
                write!(f, "]")
            }
            ValueShape::Variant(name, value) => write!(f, "<{name}: {value}>"),
            ValueShape::Identifier(ident) => write!(f, "{ident}"),
            ValueShape::Lazy(_) => write!(f, "<lazy>"),
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
        if !self.markings.is_empty() {
            write!(f, "marked [")?;
            for marking in &self.markings {
                write!(f, "${marking}, ")?;
            }
            write!(f, "] ")?;
        }
        write!(f, "{}", self.shape)
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
        write!(f, "<function>")
    }
}
impl PartialEq for FunctionValue {
    fn eq(&self, _other: &Self) -> bool {
        // TODO: proper equality
        false
    }
}
impl Eq for FunctionValue {}

#[derive(Clone, Copy, Debug, Error, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecError {
    #[error("Invalid name")]
    InvalidName,
    #[error("Unknown intrinsic")]
    UnknownIntrinsic,
    #[error("Not implemented")]
    NotImplemented,
    #[error("No main function found")]
    NoMain,
}

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
    
    pub fn evaluate(&mut self, expr: Expr, keep_identifiers: bool) -> Result<Value, ExecError> {
        let mut or_terms = expr.items;
        if or_terms.len() > 1 {
            return Err(ExecError::NotImplemented); // TODO: or combination
        }
        let mut and_terms = or_terms.remove(0).0.items;
        if and_terms.len() > 1 {
            return Err(ExecError::NotImplemented); // TODO: and combination
        }
        let mut binary_terms = and_terms.remove(0).0.items;
        if binary_terms.len() > 1 {
            return Err(ExecError::NotImplemented); // TODO: binary operators
        }
        let (term, _) = binary_terms.remove(0);
        match term {
            Term::Delimited(grouped) => self.evaluate(grouped.item, keep_identifiers),
            Term::UnaryOperation((prefix, leaf, tight_postfix, loose_postfix)) => {
                if !prefix.is_empty() || !loose_postfix.is_empty() {
                    return Err(ExecError::NotImplemented); // TODO: prefix and loose postfix operations
                }
                
                // First evaluate the leaf
                let mut value = match leaf {
                    lang::TermLeaf::Name(name) => {
                        let name = self.evaluate_name(&name)?;
                        if keep_identifiers {
                            ValueShape::Identifier(name.clone()).into()
                        } else {
                            self.get(Some(&name)).ok_or(ExecError::InvalidName)?
                        }
                    }
                    lang::TermLeaf::String(str) => ValueShape::String(str.0).into(),
                    lang::TermLeaf::Integer(int) => ValueShape::Integer(int.0.into()).into(),
                    lang::TermLeaf::Intrinsic((_, intrinsic_name)) => {
                        let name = self.evaluate_name(&intrinsic_name)?;
                        self.evaluate_intrinsic(&name, None)
                            .ok_or(ExecError::UnknownIntrinsic)?
                    }
                    lang::TermLeaf::Structure(_structure) => {
                        // TODO: Implement structure parsing properly
                        // For now, return an empty structure
                        ValueShape::Structure(HashMap::new()).into()
                    }
                    lang::TermLeaf::Block(expressions) => {
                        let mut last_value = ValueShape::void().into();
                        self.new_scope(|ctx| {
                            for (expr, _) in &expressions.item.items {
                                last_value = ctx.evaluate(expr.clone(), false)?;
                            }
                            Ok::<_, ExecError>(())
                        })?;
                        last_value
                    }
                    lang::TermLeaf::Delimited(expr) => self.evaluate(expr.item, keep_identifiers)?,
                    lang::TermLeaf::Tuple(items) => {
                        let mut values = Vec::new();
                        for (item, _) in &items.item.items {
                            values.push(self.evaluate(item.clone(), false)?);
                        }
                        ValueShape::Tuple(values).into()
                    }
                    lang::TermLeaf::ElementwiseArray(elements) => {
                        let mut values = Vec::new();
                        for (elem, _) in &elements.item.items {
                            values.push(self.evaluate(elem.clone(), false)?);
                        }
                        ValueShape::Array(values).into()
                    }
                    lang::TermLeaf::ReplicatedArray(inner) => {
                        let value = self.evaluate(inner.item.0.clone(), false)?;
                        let count = self.evaluate(inner.item.2.clone(), false)?;
                        if let ValueShape::Integer(n) = count.shape {
                            let count_usize = n.to_string().parse::<usize>().unwrap_or(0);
                            ValueShape::Array(vec![value; count_usize]).into()
                        } else {
                            return Err(ExecError::NotImplemented);
                        }
                    }
                    lang::TermLeaf::Union(_) => return Err(ExecError::NotImplemented),
                };
                
                // Now handle tight postfix operators (like function calls)
                for postfix in tight_postfix {
                    match postfix {
                        lang::TightPostfixUnaryOperator::Call(args) => {
                            // Evaluate the arguments
                            let mut arg_values = Vec::new();
                            for (arg, _) in &args.item.items {
                                arg_values.push(self.evaluate(arg.clone(), false)?);
                            }
                            
                            // Call the function
                            match &value.shape {
                                ValueShape::Function(FunctionValue::Intrinsic(intrinsic)) => {
                                    value = intrinsic(arg_values);
                                }
                                ValueShape::Function(FunctionValue::FunctionID(_)) => {
                                    return Err(ExecError::NotImplemented); // TODO: user functions
                                }
                                _ => return Err(ExecError::NotImplemented),
                            }
                        }
                        lang::TightPostfixUnaryOperator::Shape(_) => {
                            return Err(ExecError::NotImplemented); // TODO: shape operators
                        }
                    }
                }
                
                Ok(value)
            }
            Term::Declaration((_, pattern, _, expr)) => {
                let value = self.evaluate(expr, false)?;
                // For now, only handle simple name bindings
                match pattern {
                    lang::Pattern::Binder(name) => {
                        let var_name = self.evaluate_name(&name)?;
                        self.scopes.last_mut().unwrap().locals.insert(var_name, value.clone());
                        Ok(value)
                    }
                    _ => Err(ExecError::NotImplemented),
                }
            }
        }
    }
    
    pub fn evaluate_name(&mut self, name: &Name) -> Result<String, ExecError> {
        match name {
            Name::Raw(ident) => Ok(ident.repr.clone()),
            Name::FromExpression((_, expr)) => {
                match self.evaluate(expr.clone(), true)? {
                    Value {
                        shape: ValueShape::Identifier(ident),
                        ..
                    } => Ok(ident),
                    Value {
                        shape: ValueShape::String(s),
                        ..
                    } => Ok(s),
                    _ => Err(ExecError::InvalidName),
                }
            }
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
    
    pub fn evaluate_intrinsic(
        &mut self,
        name: &str,
        args: Option<Vec<Expr>>,
    ) -> Option<Value> {
        let make_fn: fn(fn(Vec<Value>) -> Value) -> Value =
            |f| ValueShape::Function(FunctionValue::Intrinsic(Arc::new(f))).into();
        
        let result: Value = match name {
            "dbg" => make_fn(|args| {
                for arg in args {
                    println!("{}", arg);
                }
                ValueShape::void().into()
            }),
            "dyn" => ValueShape::Type(TypeShape::Dynamic.into()).into(),
            "concat" => make_fn(|args| {
                let mut result = String::new();
                for arg in args {
                    if let ValueShape::String(s) = arg.shape {
                        result.push_str(&s);
                    }
                }
                ValueShape::String(result).into()
            }),
            _ => return None,
        };
        
        if let Some(args) = args {
            if let ValueShape::Function(function) = &result.shape {
                match function {
                    FunctionValue::FunctionID(_) => None, // TODO
                    FunctionValue::Intrinsic(intrinsic) => {
                        Some(intrinsic(
                            args.iter()
                                .map(|arg| self.evaluate(arg.clone(), false).unwrap())
                                .collect(),
                        ))
                    }
                }
            } else {
                None
            }
        } else {
            Some(result)
        }
    }
}

pub fn execute_main(items: Items) -> Result<Value, CompilerError> {
    let mut context = ExecContext::new();
    for (item, _) in items.items {
        match item {
            Item::Function(func_decl) => {
                let (name, params, _, expr) = func_decl;
                let func_name = context.evaluate_name(&name)
                    .map_err(|_| CompilerError::IO(std::io::Error::new(
                        std::io::ErrorKind::Other, 
                        "Failed to evaluate function name"
                    )))?;
                
                if func_name == "main" {
                    if params.is_none() || params.as_ref().unwrap().item.items.is_empty() {
                        return context.new_scope(|ctx| ctx.evaluate(expr, false))
                            .map_err(|e| CompilerError::IO(std::io::Error::new(
                                std::io::ErrorKind::Other,
                                format!("Execution error: {:?}", e)
                            )));
                    }
                }
            }
            Item::Marking(_) => {
                // Markings are processed but don't execute
            }
            Item::Constraint(_) => {
                // Constraints are for type checking
            }
        }
    }
    Err(CompilerError::IO(std::io::Error::new(
        std::io::ErrorKind::Other,
        "No main function found"
    )))
}