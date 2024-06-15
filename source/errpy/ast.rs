/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)]

use ocamlrep::FromOcamlRep;
use ocamlrep::ToOcamlRep;
use serde::Deserialize;
use serde::Serialize;

#[derive(ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
pub struct RecoverableErrorWithLocation {
    pub error: String,
    pub lineno: usize,
    pub col_offset: usize,
    pub end_lineno: usize,
    pub end_col_offset: usize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum Num {
    Int(isize),
    Float(f64),
    Complex(f64),
    /// pyast uses Py.Object.t for Big_int, but this is presumably not what we
    /// want (if it's an opaque wrapper for a CPython object). Just have the
    /// source text representing the integer digits for now instead.
    #[rust_to_ocaml(name = "Big_int")]
    BigInt(String),
}

#[rust_to_ocaml(attr = "deriving show")]
pub type Object_ = Num;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ConstantDesc {
    Ellipsis,
    Bool(bool),
    Num(Num),
    Str(String),
    ByteStr(String),
}

#[rust_to_ocaml(attr = "deriving show")]
pub type Constant = Option<ConstantDesc>;

#[rust_to_ocaml(attr = "deriving show")]
pub type Singleton = Option<bool>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[repr(C)]
pub struct Withitem {
    pub context_expr: Expr,
    pub optional_vars: Option<Expr>,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(u8)]
pub enum Unaryop {
    Invert,
    Not,
    UAdd,
    USub,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum TypeIgnore {
    TypeIgnore { lineno: isize, tag: String },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Stmt {
    pub desc: StmtDesc,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum StmtDesc {
    FunctionDef {
        name: String,
        args: Arguments,
        body: Vec<Stmt>,
        decorator_list: Vec<Expr>,
        type_params: Vec<Expr>,
        returns: Option<Expr>,
        type_comment: Option<String>,
    },
    AsyncFunctionDef {
        name: String,
        args: Arguments,
        body: Vec<Stmt>,
        decorator_list: Vec<Expr>,
        type_params: Vec<Expr>,
        returns: Option<Expr>,
        type_comment: Option<String>,
    },
    ClassDef {
        name: String,
        bases: Vec<Expr>,
        keywords: Vec<Keyword>,
        body: Vec<Stmt>,
        decorator_list: Vec<Expr>,
        type_params: Vec<Expr>,
    },
    Return(Option<Expr>),
    Delete(Vec<Expr>),
    Assign {
        targets: Vec<Expr>,
        value: Expr,
        type_comment: Option<String>,
    },
    AugAssign {
        target: Expr,
        op: Operator,
        value: Expr,
    },
    AnnAssign {
        target: Expr,
        annotation: Expr,
        value: Option<Expr>,
        simple: isize,
    },
    For {
        target: Expr,
        iter: Expr,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
        type_comment: Option<String>,
    },
    AsyncFor {
        target: Expr,
        iter: Expr,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
        type_comment: Option<String>,
    },
    While {
        test: Expr,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    If {
        test: Expr,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    With {
        items: Vec<Withitem>,
        body: Vec<Stmt>,
        type_comment: Option<String>,
    },
    AsyncWith {
        items: Vec<Withitem>,
        body: Vec<Stmt>,
        type_comment: Option<String>,
    },
    Match {
        subject: Expr,
        cases: Vec<MatchCase>,
    },
    Raise {
        exc: Option<Expr>,
        cause: Option<Expr>,
    },
    Try {
        body: Vec<Stmt>,
        handlers: Vec<Excepthandler>,
        orelse: Vec<Stmt>,
        finalbody: Vec<Stmt>,
    },
    TryStar {
        body: Vec<Stmt>,
        handlers: Vec<Excepthandler>,
        orelse: Vec<Stmt>,
        finalbody: Vec<Stmt>,
    },
    Assert {
        test: Expr,
        msg: Option<Expr>,
    },
    Import(Vec<Alias>),
    ImportFrom {
        module__: Option<String>,
        names: Vec<Alias>,
        level: Option<isize>,
    },
    Global(Vec<String>),
    Nonlocal(Vec<String>),
    Expr(Expr),
    Pass,
    Break,
    Continue,
}

#[rust_to_ocaml(and)]
pub type Slice = Expr;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Pattern {
    pub desc: Box<PatternDesc>,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: isize,
    pub end_col_offset: isize,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum PatternDesc {
    MatchValue(Expr),
    MatchSingleton(Constant),
    MatchSequence(Vec<Pattern>),
    MatchMapping {
        keys: Vec<Expr>,
        patterns: Vec<Pattern>,
        rest: Option<String>,
    },
    MatchClass {
        cls: Expr,
        patterns: Vec<Pattern>,
        kwd_attrs: Vec<String>,
        kwd_patterns: Vec<Pattern>,
    },
    MatchStar(Option<String>),
    MatchAs {
        pattern: Option<Pattern>,
        name: Option<String>,
    },
    MatchOr(Vec<Pattern>),
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(u8)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    MatMult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
    FloorDiv,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum Mod_ {
    Module {
        body: Vec<Stmt>,
        type_ignores: Vec<TypeIgnore>,
    },
    Interactive(Vec<Stmt>),
    Expression(Expr),
    FunctionType {
        argtypes: Vec<Expr>,
        returns: Expr,
    },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Keyword {
    pub arg: Option<String>,
    pub value: Expr,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(u8)]
pub enum ExprContext {
    Load,
    Store,
    Del,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Expr {
    pub desc: Box<ExprDesc>,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum ExprDesc {
    BoolOp {
        op: Boolop,
        values: Vec<Expr>,
    },
    NamedExpr {
        target: Expr,
        value: Expr,
    },
    BinOp {
        left: Expr,
        op: Operator,
        right: Expr,
    },
    UnaryOp {
        op: Unaryop,
        operand: Expr,
    },
    Lambda {
        args: Arguments,
        body: Expr,
    },
    IfExp {
        test: Expr,
        body: Expr,
        orelse: Expr,
    },
    Dict {
        // keys is of Optional Expr type because dictionary splats `{**arg1} are
        // represented by a value and a corresponding None key
        // normally of course we expect a non None key and value pair e.g.
        // for `{'a': 12}`
        keys: Vec<Option<Expr>>,
        values: Vec<Expr>,
    },
    Set(Vec<Expr>),
    ListComp {
        elt: Expr,
        generators: Vec<Comprehension>,
    },
    SetComp {
        elt: Expr,
        generators: Vec<Comprehension>,
    },
    DictComp {
        key: Expr,
        value: Expr,
        generators: Vec<Comprehension>,
    },
    GeneratorExp {
        elt: Expr,
        generators: Vec<Comprehension>,
    },
    Await(Expr),
    Yield(Option<Expr>),
    YieldFrom(Expr),
    Compare {
        left: Expr,
        ops: Vec<Cmpop>,
        comparators: Vec<Expr>,
    },
    Call {
        func: Expr,
        args: Vec<Expr>,
        keywords: Vec<Keyword>,
    },
    FormattedValue {
        value: Expr,
        conversion: Option<isize>,
        format_spec: Option<Expr>,
    },
    JoinedStr(Vec<Expr>),
    Constant {
        value: Constant,
        kind: Option<String>,
    },
    Attribute {
        value: Expr,
        attr: String,
        ctx: ExprContext,
    },
    Subscript {
        value: Expr,
        slice: Expr,
        ctx: ExprContext,
    },
    Starred {
        value: Expr,
        ctx: ExprContext,
    },
    Name {
        id: String,
        ctx: ExprContext,
    },
    List {
        elts: Vec<Expr>,
        ctx: ExprContext,
    },
    Tuple {
        elts: Vec<Expr>,
        ctx: ExprContext,
    },
    Slice {
        lower: Option<Expr>,
        upper: Option<Expr>,
        step: Option<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Excepthandler {
    pub desc: ExcepthandlerDesc,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C, u8)]
pub enum ExcepthandlerDesc {
    ExceptHandler {
        type__: Option<Expr>,
        name: Option<String>,
        body: Vec<Stmt>,
        star: bool,
    },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Comprehension {
    pub target: Expr,
    pub iter: Expr,
    pub ifs: Vec<Expr>,
    pub is_async: bool,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(u8)]
pub enum Cmpop {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(u8)]
pub enum Boolop {
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Arguments {
    pub posonlyargs: Vec<Arg>,
    pub args: Vec<Arg>,
    pub vararg: Option<Arg>,
    pub kwonlyargs: Vec<Arg>,
    pub kw_defaults: Vec<Option<Expr>>,
    pub kwarg: Option<Arg>,
    pub defaults: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and)]
#[repr(C)]
pub struct Arg {
    pub arg: String,
    pub annotation: Option<Expr>,
    pub type_comment: Option<String>,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(and, attr = "deriving show")]
#[repr(C)]
pub struct Alias {
    pub name: String,
    pub asname: Option<String>,
    pub lineno: isize,
    pub col_offset: isize,
    pub end_lineno: Option<isize>,
    pub end_col_offset: Option<isize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ArgumentsArgs {
    #[rust_to_ocaml(name = "Expr_list")]
    ExprList(Vec<Expr>),
    #[rust_to_ocaml(name = "Arg_list")]
    ArgList(Vec<Arg>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ArgumentsKwarg {
    #[rust_to_ocaml(name = "Identifier_opt")]
    IdentifierOpt(Option<String>),
    #[rust_to_ocaml(name = "Arg_opt")]
    ArgOpt(Option<Arg>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ArgumentsVararg {
    #[rust_to_ocaml(name = "Identifier_opt")]
    IdentifierOpt(Option<String>),
    #[rust_to_ocaml(name = "Arg_opt")]
    ArgOpt(Option<Arg>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ExcepthandlerExcepthandlerName {
    #[rust_to_ocaml(name = "Identifier_opt")]
    IdentifierOpt(Option<String>),
    #[rust_to_ocaml(name = "Expr_opt")]
    ExprOpt(Option<Expr>),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ExprBytesS {
    String(String),
    Bytes(String),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ExprSubscriptSlice {
    Slice(Slice),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum ExprYieldfromValue {
    #[rust_to_ocaml(name = "Expr_opt")]
    ExprOpt(Option<Expr>),
    Expr(Expr),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum KeywordArg {
    #[rust_to_ocaml(name = "Identifier_opt")]
    IdentifierOpt(Option<String>),
    Identifier(String),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize, ToOcamlRep, FromOcamlRep)]
#[rust_to_ocaml(attr = "deriving show")]
#[repr(C, u8)]
pub enum StmtImportfromModule {
    #[rust_to_ocaml(name = "Identifier_opt")]
    IdentifierOpt(Option<String>),
    Identifier(String),
}
