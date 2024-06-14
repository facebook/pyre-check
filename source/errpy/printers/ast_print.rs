/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use ast::Alias;
use ast::Arg;
use ast::Arguments;
use ast::Boolop;
use ast::Cmpop;
use ast::Comprehension;
use ast::ConstantDesc;
use ast::Excepthandler;
use ast::ExcepthandlerDesc;
use ast::Expr;
use ast::ExprContext;
use ast::ExprDesc;
use ast::Keyword;
use ast::MatchCase;
use ast::Mod_;
use ast::Num;
use ast::Operator;
use ast::Pattern;
use ast::PatternDesc;
use ast::Stmt;
use ast::StmtDesc;
use ast::Unaryop;
use ast::Withitem;

use crate::ast;
use crate::cst_to_ast::ASTAndMetaData;

pub const UNKOWN_NODE_MOD: &str = "~~?AST Mod_ Node Missing formatting?~~";
pub const UNKOWN_NODE_EXPR: &str = "~~?AST ExprDesc Node Missing formatting?~~";

impl fmt::Display for ASTAndMetaData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ast.as_ref().unwrap())
    }
}

impl fmt::Display for Mod_ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mod_::Module {
                body,
                type_ignores: _,
            } => {
                write!(f, "Module(body=[{}], type_ignores=[])", format_vec(body))
            }
            _ => write!(f, "{}", UNKOWN_NODE_MOD), // ignore
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.desc,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap()
        )
    }
}

impl fmt::Display for StmtDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtDesc::Assign {
                targets,
                value,
                type_comment: _,
            } => {
                write!(
                    f,
                    "Assign(targets=[{}], value={}, ",
                    format_vec(targets),
                    value
                )
            }
            StmtDesc::AnnAssign {
                target,
                annotation,
                value,
                simple,
            } => {
                let fmt_value = if let Some(v) = value {
                    format!(", value={}", v)
                } else {
                    String::from("")
                };

                write!(
                    f,
                    "AnnAssign(target={}, annotation={}{}, simple={}, ",
                    target, annotation, fmt_value, simple,
                )
            }
            StmtDesc::AugAssign { target, op, value } => {
                write!(
                    f,
                    "AugAssign(target={}, op={}, value={}, ",
                    target, op, value
                )
            }
            StmtDesc::Expr(expr) => write!(f, "Expr(value={}, ", expr),
            StmtDesc::Delete(exprs) => {
                write!(f, "Delete(targets=[{}], ", format_vec(exprs))
            }
            StmtDesc::With {
                items,
                body,
                type_comment: _,
            } => {
                write!(
                    f,
                    "With(items=[{}], body=[{}], ",
                    format_vec(items),
                    format_vec(body),
                )
            }
            StmtDesc::AsyncWith {
                items,
                body,
                type_comment: _,
            } => {
                write!(
                    f,
                    "AsyncWith(items=[{}], body=[{}], ",
                    format_vec(items),
                    format_vec(body),
                )
            }
            StmtDesc::For {
                target,
                iter,
                body,
                orelse,
                type_comment: _,
            } => {
                write!(
                    f,
                    "For(target={}, iter={}, body=[{}], orelse=[{}], ",
                    target,
                    iter,
                    format_vec(body),
                    format_vec(orelse)
                )
            }
            StmtDesc::AsyncFor {
                target,
                iter,
                body,
                orelse,
                type_comment: _,
            } => {
                write!(
                    f,
                    "AsyncFor(target={}, iter={}, body=[{}], orelse=[{}], ",
                    target,
                    iter,
                    format_vec(body),
                    format_vec(orelse)
                )
            }
            StmtDesc::Try {
                body,
                handlers,
                orelse,
                finalbody,
            } => {
                write!(
                    f,
                    "Try(body=[{}], handlers=[{}], orelse=[{}], finalbody=[{}], ",
                    format_vec(body),
                    format_vec(handlers),
                    format_vec(orelse),
                    format_vec(finalbody)
                )
            }
            StmtDesc::While { test, body, orelse } => {
                write!(
                    f,
                    "While(test={}, body=[{}], orelse=[{}], ",
                    test,
                    format_vec(body),
                    format_vec(orelse)
                )
            }
            StmtDesc::Pass => write!(f, "Pass("),
            StmtDesc::Break => write!(f, "Break("),
            StmtDesc::Continue => write!(f, "Continue("),
            StmtDesc::Raise { exc, cause } => match exc {
                Some(ex) => match cause {
                    Some(cuz) => write!(f, "Raise(exc={}, cause={}, ", ex, cuz),
                    _ => write!(f, "Raise(exc={}, ", ex),
                },
                _ => write!(f, "Raise("),
            },
            StmtDesc::Assert { test, msg } => match msg {
                Some(msg) => write!(f, "Assert(test={}, msg={}, ", test, msg),
                _ => write!(f, "Assert(test={}, ", test),
            },
            StmtDesc::Return(value) => match value {
                Some(valu) => write!(f, "Return(value={}, ", valu),
                _ => write!(f, "Return("),
            },
            StmtDesc::Global(names) => {
                write!(f, "Global(names=[{}], ", format_vec_names(names))
            }
            StmtDesc::Import(aliases) => {
                write!(f, "Import(names=[{}], ", format_vec(aliases))
            }
            StmtDesc::ImportFrom {
                module__,
                names,
                level,
            } => {
                let modstr = match module__ {
                    Some(mmstr) => format!("module='{}', ", mmstr),
                    _ => String::from(""),
                };
                let levelstr = match level {
                    Some(ll) => format!("level={}, ", ll),
                    _ => String::from(""),
                };
                write!(
                    f,
                    "ImportFrom({}names=[{}], {}",
                    modstr,
                    format_vec(names),
                    levelstr
                )
            }
            StmtDesc::Nonlocal(names) => {
                write!(f, "Nonlocal(names=[{}], ", format_vec_names(names))
            }
            StmtDesc::If { test, body, orelse } => {
                write!(
                    f,
                    "If(test={}, body=[{}], orelse=[{}], ",
                    test,
                    format_vec(body),
                    format_vec(orelse)
                )
            }
            StmtDesc::FunctionDef {
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment: _,
            } => {
                let rets = match returns {
                    Some(r) => format!(", returns={}", r),
                    _ => "".to_string(),
                };

                write!(
                    f,
                    "FunctionDef(name='{}', args={}, body=[{}], decorator_list=[{}]{}, type_params=[{}], ",
                    name,
                    args,
                    format_vec(body),
                    format_vec(decorator_list),
                    rets,
                    format_vec(type_params)
                )
            }
            StmtDesc::AsyncFunctionDef {
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment: _,
            } => {
                let rets = match returns {
                    Some(r) => format!(", returns={}", r),
                    _ => "".to_string(),
                };

                write!(
                    f,
                    "AsyncFunctionDef(name='{}', args={}, body=[{}], decorator_list=[{}]{}, type_params=[{}], ",
                    name,
                    args,
                    format_vec(body),
                    format_vec(decorator_list),
                    rets,
                    format_vec(type_params)
                )
            }
            StmtDesc::ClassDef {
                name,
                bases,
                keywords,
                body,
                decorator_list,
                type_params,
            } => {
                write!(
                    f,
                    "ClassDef(name='{}', bases=[{}], keywords=[{}], body=[{}], decorator_list=[{}], type_params=[{}], ",
                    name,
                    format_vec(bases),
                    format_vec(keywords),
                    format_vec(body),
                    format_vec(decorator_list),
                    format_vec(type_params),
                )
            }
            StmtDesc::Match { subject, cases } => {
                write!(
                    f,
                    "Match(subject={}, cases=[{}], ",
                    subject,
                    format_vec(cases),
                )
            }
        }
    }
}

impl fmt::Display for Alias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let asname = match &self.asname {
            Some(the_name) => format!("asname='{}', ", the_name),
            _ => "".to_string(),
        };

        write!(
            f,
            "alias(name='{}', {}lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.name,
            asname,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap()
        )
    }
}

fn format_vec<T: std::fmt::Display>(exprs: &[T]) -> String {
    exprs
        .iter()
        .map(|targ_elm| format!("{}", targ_elm))
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_vec_names<T: std::fmt::Display>(exprs: &[T]) -> String {
    exprs
        .iter()
        .map(|targ_elm| format!("'{}'", targ_elm))
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_gens(generators: &[Comprehension]) -> String {
    generators
        .iter()
        .map(|targ_elm| format!("{}", targ_elm))
        .collect::<Vec<_>>()
        .join(", ")
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.desc,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap()
        )
    }
}

impl fmt::Display for MatchCase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let guard_str = match &self.guard {
            Some(guard) => format!("guard={}, ", guard),
            _ => String::from(""),
        };

        write!(
            f,
            "match_case(pattern={}, {}body=[{}])",
            self.pattern,
            guard_str,
            format_vec(&self.body),
        )
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.desc, self.lineno, self.col_offset, self.end_lineno, self.end_col_offset
        )
    }
}

impl fmt::Display for PatternDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatternDesc::MatchOr(or_choices) => {
                write!(f, "MatchOr(patterns=[{}], ", format_vec(or_choices))
            }
            PatternDesc::MatchSequence(choices) => {
                write!(f, "MatchSequence(patterns=[{}], ", format_vec(choices))
            }
            PatternDesc::MatchClass {
                cls,
                patterns,
                kwd_attrs,
                kwd_patterns,
            } => {
                write!(
                    f,
                    "MatchClass(cls={}, patterns=[{}], kwd_attrs=[{}], kwd_patterns=[{}], ",
                    cls,
                    format_vec(patterns),
                    format_vec_names(kwd_attrs),
                    format_vec(kwd_patterns)
                )
            }
            PatternDesc::MatchStar(name) => match name {
                Some(name) => write!(f, "MatchStar(name='{}', ", name),
                None => write!(f, "MatchStar("),
            },
            PatternDesc::MatchSingleton(constant) => match constant {
                Some(constant) => write!(f, "MatchSingleton(value={}, ", constant),
                None => write!(f, "MatchSingleton(value=None, "),
            },
            PatternDesc::MatchValue(expr) => write!(f, "MatchValue(value={}, ", expr),
            PatternDesc::MatchMapping {
                keys,
                patterns,
                rest,
            } => {
                let rest_format = match rest {
                    Some(rest) => format!("rest='{}', ", rest),
                    _ => "".to_string(),
                };

                write!(
                    f,
                    "MatchMapping(keys=[{}], patterns=[{}], {}",
                    format_vec(keys),
                    format_vec(patterns),
                    rest_format,
                )
            }
            PatternDesc::MatchAs { pattern, name } => {
                let pattern_str = match pattern {
                    Some(pattern) => format!("pattern={}, ", pattern),
                    _ => String::from(""),
                };

                let name_str = match name {
                    Some(name) => format!("name='{}', ", name),
                    _ => String::from(""),
                };

                write!(f, "MatchAs({}{}", pattern_str, name_str)
            }
        }
    }
}

impl fmt::Display for ExprDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprDesc::Name { id, ctx } => write!(f, "Name(id='{}', ctx={}, ", id, ctx),
            ExprDesc::Constant { value, kind: _ } => match value {
                Some(val) => write!(f, "Constant(value={}, ", val),
                _ => write!(f, "Constant(value=None, "),
            },
            ExprDesc::List { elts, ctx } => {
                write!(f, "List(elts=[{}], ctx={}, ", format_vec(elts), ctx)
            }
            ExprDesc::Tuple { elts, ctx } => {
                write!(f, "Tuple(elts=[{}], ctx={}, ", format_vec(elts), ctx)
            }
            ExprDesc::Set(elts) => {
                write!(f, "Set(elts=[{}], ", format_vec(elts))
            }
            ExprDesc::Dict { keys, values } => {
                write!(
                    f,
                    "Dict(keys=[{}], values=[{}], ",
                    keys.iter()
                        .map(|targ_elm| match targ_elm {
                            Some(value) => format!("{}", value),
                            _ => "None".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                    format_vec(values)
                )
            }
            ExprDesc::Lambda { args, body } => {
                write!(f, "Lambda(args={}, body={}, ", args, body)
            }
            ExprDesc::UnaryOp { op, operand } => {
                write!(f, "UnaryOp(op={}, operand={}, ", op, operand)
            }
            ExprDesc::BinOp { left, op, right } => {
                write!(f, "BinOp(left={}, op={}, right={}, ", left, op, right)
            }
            ExprDesc::BoolOp { op, values } => {
                write!(f, "BoolOp(op={}, values=[{}], ", op, format_vec(values))
            }
            ExprDesc::Compare {
                left,
                ops,
                comparators,
            } => {
                write!(
                    f,
                    "Compare(left={}, ops=[{}], comparators=[{}], ",
                    left,
                    format_vec(ops),
                    format_vec(comparators)
                )
            }
            ExprDesc::Attribute { value, attr, ctx } => {
                write!(
                    f,
                    "Attribute(value={}, attr='{}', ctx={}, ",
                    value, attr, ctx
                )
            }
            ExprDesc::Subscript { value, slice, ctx } => {
                write!(
                    f,
                    "Subscript(value={}, slice={}, ctx={}, ",
                    value, slice, ctx
                )
            }
            ExprDesc::Slice { lower, upper, step } => {
                let lower_str = match lower {
                    Some(sth) => format!("lower={}, ", sth),
                    _ => String::from(""),
                };

                let upper_str = match upper {
                    Some(sth) => format!("upper={}, ", sth),
                    _ => String::from(""),
                };

                let step_str = match step {
                    Some(sth) => format!("step={}, ", sth),
                    _ => String::from(""),
                };

                write!(f, "Slice({}{}{}", lower_str, upper_str, step_str)
            }
            ExprDesc::Call {
                func,
                args,
                keywords,
            } => {
                write!(
                    f,
                    "Call(func={}, args=[{}], keywords=[{}], ",
                    func,
                    format_vec(args),
                    format_vec(keywords)
                )
            }
            ExprDesc::Starred { value, ctx } => {
                write!(f, "Starred(value={}, ctx={}, ", value, ctx)
            }
            ExprDesc::IfExp { test, body, orelse } => {
                write!(f, "IfExp(test={}, body={}, orelse={}, ", test, body, orelse)
            }
            ExprDesc::ListComp { elt, generators } => {
                write!(
                    f,
                    "ListComp(elt={}, generators=[{}], ",
                    elt,
                    format_gens(generators)
                )
            }
            ExprDesc::SetComp { elt, generators } => {
                write!(
                    f,
                    "SetComp(elt={}, generators=[{}], ",
                    elt,
                    format_gens(generators)
                )
            }
            ExprDesc::GeneratorExp { elt, generators } => {
                write!(
                    f,
                    "GeneratorExp(elt={}, generators=[{}], ",
                    elt,
                    format_gens(generators)
                )
            }
            ExprDesc::DictComp {
                key,
                value,
                generators,
            } => {
                write!(
                    f,
                    "DictComp(key={}, value={}, generators=[{}], ",
                    key,
                    value,
                    format_gens(generators)
                )
            }
            ExprDesc::NamedExpr { target, value } => {
                write!(f, "NamedExpr(target={}, value={}, ", target, value)
            }
            ExprDesc::JoinedStr(exprs) => {
                write!(f, "JoinedStr(values=[{}], ", format_vec(exprs))
            }
            ExprDesc::FormattedValue {
                value,
                conversion,
                format_spec,
            } => {
                let conversion_str = match conversion {
                    Some(sth) => format!(", conversion={}", sth),
                    _ => String::from(""),
                };
                let format_spec_str = match format_spec {
                    Some(sth) => format!(", format_spec={}", sth),
                    _ => String::from(""),
                };
                write!(
                    f,
                    "FormattedValue(value={}{}{}, ",
                    value, conversion_str, format_spec_str
                )
            }
            ExprDesc::Await(arg) => {
                write!(f, "Await(value={}, ", arg)
            }
            ExprDesc::Yield(value) => match value {
                Some(val) => write!(f, "Yield(value={}, ", val),
                _ => write!(f, "Yield("),
            },
            ExprDesc::YieldFrom(value) => {
                write!(f, "YieldFrom(value={}, ", value)
            }
        }
    }
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let posonlyargsx = self
            .posonlyargs
            .iter()
            .map(|targ_elm| format!("{}", targ_elm))
            .collect::<Vec<_>>()
            .join(", ");

        let argsx = self
            .args
            .iter()
            .map(|targ_elm| format!("{}", targ_elm))
            .collect::<Vec<_>>()
            .join(", ");

        let varargx = match &self.vararg {
            Some(varg) => format!(", vararg={}", varg),
            _ => "".to_string(),
        };

        let kwonlyargsx = self
            .kwonlyargs
            .iter()
            .map(|targ_elm| format!("{}", targ_elm))
            .collect::<Vec<_>>()
            .join(", ");

        let kw_defaultsx = self
            .kw_defaults
            .iter()
            .map(|targ_elm| match targ_elm {
                Some(x) => format!("{}", x),
                _ => "None".to_string(),
            })
            .collect::<Vec<_>>()
            .join(", ");

        let kwargx = match &self.kwarg {
            Some(kwarg) => format!(", kwarg={}", kwarg),
            _ => "".to_string(),
        };

        let defaultsx = self
            .defaults
            .iter()
            .map(|targ_elm| format!("{}", targ_elm))
            .collect::<Vec<_>>()
            .join(", ");

        write!(
            f,
            "arguments(posonlyargs=[{}], args=[{}]{}, kwonlyargs=[{}], kw_defaults=[{}]{}, defaults=[{}])",
            posonlyargsx, argsx, varargx, kwonlyargsx, kw_defaultsx, kwargx, defaultsx
        )
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let annotationx = match &self.annotation {
            Some(annot) => format!(", annotation={}", annot),
            _ => "".to_string(),
        };

        let type_commentx = match &self.type_comment {
            Some(tc) => format!(", type_comment={}", tc),
            _ => "".to_string(),
        };

        write!(
            f,
            "arg(arg='{}'{}{}, lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.arg,
            annotationx,
            type_commentx,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap() // Weird, we expect this to be self.end_col_offset.unwrap() but it's this... Could be a bug?
        )
    }
}

impl fmt::Display for Comprehension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ifsz = self
            .ifs
            .iter()
            .map(|targ_elm| format!("{}", targ_elm))
            .collect::<Vec<_>>()
            .join(", ");

        write!(
            f,
            "comprehension(target={}, iter={}, ifs=[{}], is_async={})",
            self.target,
            self.iter,
            ifsz,
            if self.is_async { "1" } else { "0" }
        )
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let contents = match &self.arg {
            Some(arg) => format!("arg='{}', value={}", arg, self.value),
            _ => format!("value={}", self.value),
        };

        write!(
            f,
            "keyword({}, lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            contents,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap()
        )
    }
}

impl fmt::Display for ExprContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}()", self)
    }
}

impl fmt::Display for Unaryop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            Unaryop::Invert => "Invert()",
            Unaryop::Not => "Not()",
            Unaryop::UAdd => "UAdd()",
            Unaryop::USub => "USub()",
        };

        write!(f, "{}", res)
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            Operator::Add => "Add()",
            Operator::Sub => "Sub()",
            Operator::Mult => "Mult()",
            Operator::MatMult => "MatMult()",
            Operator::Div => "Div()",
            Operator::Mod => "Mod()",
            Operator::Pow => "Pow()",
            Operator::LShift => "LShift()",
            Operator::RShift => "RShift()",
            Operator::BitOr => "BitOr()",
            Operator::BitXor => "BitXor()",
            Operator::BitAnd => "BitAnd()",
            Operator::FloorDiv => "FloorDiv()",
        };

        write!(f, "{}", res)
    }
}

impl fmt::Display for Cmpop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            Cmpop::Eq => "Eq()",
            Cmpop::NotEq => "NotEq()",
            Cmpop::Lt => "Lt()",
            Cmpop::LtE => "LtE()",
            Cmpop::Gt => "Gt()",
            Cmpop::GtE => "GtE()",
            Cmpop::Is => "Is()",
            Cmpop::IsNot => "IsNot()",
            Cmpop::In => "In()",
            Cmpop::NotIn => "NotIn()",
        };

        write!(f, "{}", res)
    }
}

///
/// Attempt to replicate PyOS_double_to_string function used by float_repr
/// in floatobject.c for representing floats as strings
/// https://github.com/python/cpython/blob/df81d2892eed3a256eb61ce59304f2173fb0c945/Python/pystrtod.c
pub fn cpython_float_to_string(value: &f64, is_complex: bool) -> String {
    if value.log10().ceil() >= 17. {
        // 17 digits or more requires exponent representation with + after the e
        format!("{:e}", value).replace('e', "e+")
    } else if value < &0.0001 && value != &0.0 {
        // <= 0.00001 requires exponent representation, except for exactly
        // zero (0.0) which we don't format with scientific notation
        let mut little_formatted = format!("{:e}", value);
        let exp = little_formatted.split_off(little_formatted.find('e').unwrap());
        little_formatted.push_str(&format!("e-{:0>pad$}", &exp[2..], pad = 2));
        little_formatted
    } else {
        // normal f64 in normal way...
        if !is_complex && value.fract() == 0.0 {
            // except we must ensure thats
            // .0 is appended to floats which could be confused for integers
            // i.e. those with no mantissa
            // but this does not apply to imaginary component of complex numbers
            format!("{:.1}", value)
        } else {
            format!("{}", value)
        }
    }
}

impl fmt::Display for ConstantDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstantDesc::Num(Num::Int(vala)) => {
                write!(f, "{}", vala)
            }
            ConstantDesc::Num(Num::Float(vala)) => {
                write!(f, "{}", cpython_float_to_string(vala, false))
            }
            ConstantDesc::Num(Num::Complex(vala)) => {
                write!(f, "{}j", cpython_float_to_string(vala, true))
            }
            ConstantDesc::ByteStr(vala) => {
                write!(f, "b{}", vala)
            }
            rest => {
                let res = match rest {
                    ConstantDesc::Str(vala) => vala,
                    ConstantDesc::Bool(true) => "True",
                    ConstantDesc::Bool(false) => "False",
                    ConstantDesc::Num(Num::BigInt(stra)) => stra,
                    ConstantDesc::Ellipsis => "Ellipsis",
                    _ => "",
                };
                write!(f, "{}", res)
            }
        }
    }
}

impl fmt::Display for Boolop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            Boolop::And => "And()",
            Boolop::Or => "Or()",
        };
        write!(f, "{}", res)
    }
}

impl fmt::Display for Withitem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opt_fmt = match &self.optional_vars {
            Some(opt) => format!(", optional_vars={}", opt),
            _ => String::from(""),
        };
        write!(f, "withitem(context_expr={}{})", self.context_expr, opt_fmt)
    }
}

impl fmt::Display for ExcepthandlerDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExcepthandlerDesc::ExceptHandler { type__, name, body } => {
                let type_fmt = match &type__ {
                    Some(ttype) => format!("type={}, ", ttype),
                    _ => String::from(""),
                };

                let name_fmt = match &name {
                    Some(nname) => format!("name='{}', ", nname),
                    _ => String::from(""),
                };

                write!(
                    f,
                    "ExceptHandler({}{}body=[{}], ",
                    type_fmt,
                    name_fmt,
                    format_vec(body)
                )
            }
        }
    }
}

impl fmt::Display for Excepthandler {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}lineno={}, col_offset={}, end_lineno={}, end_col_offset={})",
            self.desc,
            self.lineno,
            self.col_offset,
            self.end_lineno.unwrap(),
            self.end_col_offset.unwrap()
        )
    }
}
