// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree
use std::convert::TryFrom;

use ast::Alias;
use ast::Arg;
use ast::Arguments;
use ast::Boolop;
use ast::Cmpop;
use ast::Comprehension;
use ast::ConstantDesc;
use ast::ExcepthandlerDesc;
use ast::Expr;
use ast::ExprDesc;
use ast::Keyword;
use ast::MatchCase;
use ast::Mod_;
use ast::Num;
use ast::Operator;
use ast::PatternDesc;
use ast::Stmt;
use ast::StmtDesc;
use ast::Unaryop;
use ast::Withitem;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use printers::ast_pretty_print_helper::PrintHelper;
use printers::ast_print::cpython_float_to_string;
use printers::ast_print::UNKOWN_NODE_MOD;

use crate::ast;
use crate::ast::Pattern;
use crate::cst_to_ast::ASTAndMetaData;
use crate::printers;

/// The output of this pretty preinter is required to match the output of
/// the `ast.unparse` function in CPython 3.10 onwards. The spec for this
/// (including rare edge cases) can be found here:
/// https://github.com/python/cpython/blob/main/Python/ast_unparse.c

#[repr(u8)]
#[derive(PartialEq, PartialOrd)]
#[derive(Copy, Clone)]
#[derive(IntoPrimitive, TryFromPrimitive)]
enum PriorityLevel {
    NamedExpr,         // :=
    Tuple,             // ()
    Test,              // 'if'-'else', 'lambda'
    Or,                // 'or'
    And,               // 'and'
    Not,               // 'not'
    Compare,           // '<', '>', '==', '>=', '<=', '!=' 'in', 'not in', 'is', 'is not'
    ExpressionOrBitOr, // expression or '|' - these share the same priority level
    BitXOr,            // '^'
    BitAnd,            // '&'
    Shift,             // '<<', '>>'
    PlusMinus,         // '+', '-'
    Term,              // '*', '@', '/', '%', '//'
    Factor,            // unary '+', '-', '~'
    Power,             // '**'
    Await,             // 'await'
    Atom,              // Constants
}

impl PriorityLevel {
    fn next(&self) -> PriorityLevel {
        match PriorityLevel::try_from(Into::<u8>::into(*self) + 1) {
            Ok(next_level) => next_level,
            Err(..) => PriorityLevel::NamedExpr,
        }
    }
}

fn bool_operator_to_priority_level(operator: &Boolop) -> PriorityLevel {
    match operator {
        Boolop::And => PriorityLevel::And,
        Boolop::Or => PriorityLevel::Or,
    }
}
fn unary_operator_to_priority_level(operator: &Unaryop) -> PriorityLevel {
    match operator {
        Unaryop::Invert => PriorityLevel::Factor,
        Unaryop::Not => PriorityLevel::Not,
        Unaryop::UAdd => PriorityLevel::Factor,
        Unaryop::USub => PriorityLevel::Factor,
    }
}

fn operator_to_priority_level(operator: &Operator) -> PriorityLevel {
    match operator {
        Operator::Add => PriorityLevel::PlusMinus,
        Operator::Sub => PriorityLevel::PlusMinus,
        Operator::Mult => PriorityLevel::Term,
        Operator::MatMult => PriorityLevel::Term,
        Operator::Div => PriorityLevel::Term,
        Operator::Mod => PriorityLevel::Term,
        Operator::Pow => PriorityLevel::Power,
        Operator::LShift => PriorityLevel::Shift,
        Operator::RShift => PriorityLevel::Shift,
        Operator::BitOr => PriorityLevel::ExpressionOrBitOr,
        Operator::BitXor => PriorityLevel::BitXOr,
        Operator::BitAnd => PriorityLevel::BitAnd,
        Operator::FloorDiv => PriorityLevel::Term,
    }
}

fn apply_parenthesis(
    pprint_output: &mut PrintHelper,
    current_priority_level: PriorityLevel,
    priority_level: PriorityLevel,
    parenthesis: &str,
) {
    if current_priority_level > priority_level {
        pprint_output.push_str(parenthesis);
    }
}
fn left_parenthesis(
    pprint_output: &mut PrintHelper,
    current_priority_level: PriorityLevel,
    priority_level: PriorityLevel,
) {
    apply_parenthesis(pprint_output, current_priority_level, priority_level, "(");
}
fn right_parenthesis(
    pprint_output: &mut PrintHelper,
    current_priority_level: PriorityLevel,
    priority_level: PriorityLevel,
) {
    apply_parenthesis(pprint_output, current_priority_level, priority_level, ")");
}

impl ASTAndMetaData {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        self.ast.as_ref().unwrap().pprint(pprint_output);
    }
}

impl Mod_ {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        match self {
            Mod_::Module {
                body,
                type_ignores: _,
            } => process_module_class_functiondef_block(body, true, pprint_output),
            _ => pprint_output.push_str(UNKOWN_NODE_MOD), // ignore
        }
    }
}

fn process_module_class_functiondef_block(
    body: &[Stmt],
    at_module_level: bool,
    pprint_output: &mut PrintHelper,
) {
    let mut is_first = true;
    for targ_elm in body.iter() {
        match is_first {
            true => {
                // for first statement in a block
                if !at_module_level {
                    match targ_elm.desc {
                        StmtDesc::ClassDef { .. }
                        | StmtDesc::FunctionDef { .. }
                        | StmtDesc::AsyncFunctionDef { .. } => {
                            pprint_output.push_str("\n");
                        }
                        _ => (),
                    }
                }

                // if the first line is a string, we turn it into a docstring
                if let StmtDesc::Expr(expr) = &targ_elm.desc {
                    if let ExprDesc::Constant {
                        value: Some(ConstantDesc::Str(astring)),
                        kind: _,
                    } = &*expr.desc
                    {
                        let mut res = astring
                            .to_string()
                            .replace("\\n", "\n")
                            .replace("\\t", "\t");
                        res = res[1..res.len() - 1].to_string();
                        pprint_output.push_ident();
                        pprint_output.push_str(&format!("\"\"\"{}\"\"\"", res).to_string());
                        pprint_output.push_str("\n");
                        continue;
                    }
                }
            }
            false => {
                // for everything but first statement in a block...
                pprint_output.strip_all_but_one_trailing_newline();

                match targ_elm.desc {
                    StmtDesc::ClassDef { .. }
                    | StmtDesc::FunctionDef { .. }
                    | StmtDesc::AsyncFunctionDef { .. } => {
                        pprint_output.push_str("\n");
                    }
                    _ => (),
                }
            }
        }

        targ_elm.pprint(pprint_output);
        pprint_output.push_str("\n");
        is_first = false;
    }
}

fn format_vec_str(names: &[String], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for name in names.iter() {
        at_least_one = true;
        pprint_output.push_str(name);
        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }
}

fn format_vec_expr(exprs: &[Expr], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for expr in exprs.iter() {
        at_least_one = true;
        (*expr.desc).pprint(pprint_output);
        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }
}

fn format_vec_alias(aliases: &[Alias], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for alias in aliases.iter() {
        at_least_one = true;
        pprint_output.push_str(&alias.name);
        match &alias.asname {
            Some(asname_text) => {
                pprint_output.push_str(" as ");
                pprint_output.push_str(asname_text);
            }
            _ => (),
        }
        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }
}

fn format_vec_keywords(keywords: &[Keyword], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for keyword in keywords.iter() {
        at_least_one = true;
        keyword.pprint(pprint_output);
        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }
}

fn format_vec_pattern_for_or(or_choices: &[Pattern], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for or_choice in or_choices.iter() {
        at_least_one = true;

        if let PatternDesc::MatchOr(_) = *or_choice.desc {
            pprint_output.push_str("(");
            or_choice.desc.pprint(pprint_output);
            pprint_output.push_str(")");
        } else {
            or_choice.desc.pprint(pprint_output);
        }

        pprint_output.push_str(" | ");
    }
    if at_least_one {
        pprint_output.pop_many(3);
    }
}

fn format_vec_pattern(choices: &[Pattern], pprint_output: &mut PrintHelper) {
    let mut at_least_one = false;
    for or_choice in choices.iter() {
        at_least_one = true;

        if let PatternDesc::MatchOr(_) = *or_choice.desc {
            or_choice.desc.pprint(pprint_output);
        } else {
            or_choice.desc.pprint(pprint_output);
        }

        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }
}

///
/// Function will format output without a trailing newline
/// the caller is to add newline after the block if appropriate
fn format_block(body: &[Stmt], pprint_output: &mut PrintHelper) {
    pprint_output.inc_ident();
    for item in body.iter() {
        item.pprint(pprint_output);
        pprint_output.push_str("\n");
    }
    // remove last newline, we only want newlines between block statements
    pprint_output.pop();
    pprint_output.dec_ident();
}

impl Stmt {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        pprint_output.push_ident();
        match &self.desc {
            StmtDesc::Assign {
                targets,
                value,
                type_comment: _,
            } => {
                for targ_elm in targets.iter() {
                    (*targ_elm.desc)
                        .pprint_with_priority_level(pprint_output, PriorityLevel::NamedExpr);
                    pprint_output.push_str(" = ");
                }
                pprint_output.pop_many(3);

                pprint_output.push_str(" = ");
                handle_assign_rhs_yield(&value.desc, pprint_output);
            }
            StmtDesc::AnnAssign {
                target,
                annotation,
                value,
                simple,
            } => {
                let lhs = &*target.desc;
                if *simple == 0 {
                    // not simple and lhs is a Name, then wrap the name in ()'s
                    match lhs {
                        ExprDesc::Name { id, ctx: _ } => {
                            pprint_output.push_str("(");
                            pprint_output.push_str(id);
                            pprint_output.push_str(")");
                        }
                        _ => lhs.pprint(pprint_output),
                    }
                } else {
                    lhs.pprint(pprint_output);
                }

                pprint_output.push_str(": ");
                (*annotation.desc).pprint(pprint_output);

                if let Some(v) = value {
                    pprint_output.push_str(" = ");
                    (*v.desc).pprint(pprint_output);
                }
            }
            StmtDesc::AugAssign { target, op, value } => {
                (*target.desc).pprint(pprint_output);
                pprint_output.push_str(" ");
                op.pprint(pprint_output);
                pprint_output.push_str("= ");
                handle_assign_rhs_yield(&value.desc, pprint_output);
            }
            StmtDesc::Expr(expr) => (*expr.desc).pprint(pprint_output),
            StmtDesc::Delete(exprs) => {
                pprint_output.push_str("del ");
                format_vec_expr(exprs, pprint_output);
            }
            StmtDesc::Import(aliases) => {
                pprint_output.push_str("import ");
                format_vec_alias(aliases, pprint_output);
            }
            StmtDesc::ImportFrom {
                module__,
                names,
                level,
            } => {
                pprint_output.push_str("from ");

                match level {
                    Some(llevel) => {
                        let compl = *llevel;
                        if compl > 0 {
                            let mut ll = 0;
                            while ll < compl {
                                pprint_output.push_str(".");
                                ll += 1;
                            }
                        }
                    }
                    _ => (),
                }

                match module__ {
                    Some(mm) => pprint_output.push_str(mm),
                    _ => (),
                }

                pprint_output.push_str(" import ");
                format_vec_alias(names, pprint_output);
            }
            StmtDesc::Pass => pprint_output.push_str("pass"),
            StmtDesc::Break => pprint_output.push_str("break"),
            StmtDesc::Continue => pprint_output.push_str("continue"),
            StmtDesc::Raise { exc, cause } => {
                pprint_output.push_str("raise");

                match &exc {
                    Some(ex) => {
                        pprint_output.push_str(" ");
                        (*ex.desc).pprint(pprint_output);
                        match &cause {
                            Some(ca) => {
                                pprint_output.push_str(" from ");
                                (*ca.desc).pprint(pprint_output);
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                };
            }
            StmtDesc::Assert { test, msg } => {
                pprint_output.push_str("assert ");
                (*test.desc).pprint(pprint_output);

                match &msg {
                    Some(msg) => {
                        pprint_output.push_str(", ");
                        (*msg.desc).pprint(pprint_output);
                    }
                    _ => (),
                };
            }
            StmtDesc::Return(value) => {
                pprint_output.push_str("return");

                match &value {
                    Some(val) => {
                        pprint_output.push_str(" ");
                        (*val.desc).pprint(pprint_output);
                    }
                    _ => (),
                };
            }
            StmtDesc::Global(names) => {
                pprint_output.push_str("global ");
                format_vec_str(names, pprint_output);
            }
            StmtDesc::Nonlocal(names) => {
                pprint_output.push_str("nonlocal ");
                format_vec_str(names, pprint_output);
            }
            StmtDesc::If { test, body, orelse } => {
                format_if_stmt(test, body, orelse, pprint_output)
            }
            StmtDesc::For {
                target,
                iter,
                body,
                orelse,
                type_comment: _,
            } => format_for_stmt(target, iter, body, orelse, false, pprint_output),
            StmtDesc::AsyncFor {
                target,
                iter,
                body,
                orelse,
                type_comment: _,
            } => format_for_stmt(target, iter, body, orelse, true, pprint_output),
            StmtDesc::While { test, body, orelse } => {
                pprint_output.push_str("while ");
                (*test.desc).pprint(pprint_output);
                pprint_output.push_str(":\n");

                format_block(body, pprint_output);

                if !orelse.is_empty() {
                    pprint_output.push_str("\nelse:\n");
                    format_block(orelse, pprint_output);
                }
            }
            StmtDesc::AsyncFunctionDef {
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment,
            } => format_funcdef(
                true,
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment,
                pprint_output,
            ),
            StmtDesc::FunctionDef {
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment,
            } => format_funcdef(
                false,
                name,
                args,
                body,
                decorator_list,
                type_params,
                returns,
                type_comment,
                pprint_output,
            ),
            StmtDesc::With {
                items,
                body,
                type_comment,
            } => format_with(false, items, body, type_comment, pprint_output),
            StmtDesc::AsyncWith {
                items,
                body,
                type_comment,
            } => format_with(true, items, body, type_comment, pprint_output),
            StmtDesc::Try {
                body,
                handlers,
                orelse,
                finalbody,
            } => {
                pprint_output.push_str("try:\n");
                format_block(body, pprint_output);

                for handle in handlers {
                    handle.desc.pprint(pprint_output);
                }

                if !orelse.is_empty() {
                    pprint_output.push_str("\n");
                    pprint_output.push_ident();
                    pprint_output.push_str("else:\n");
                    format_block(orelse, pprint_output);
                }

                if !finalbody.is_empty() {
                    pprint_output.push_str("\n");
                    pprint_output.push_ident();
                    pprint_output.push_str("finally:\n");
                    format_block(finalbody, pprint_output);
                }
            }
            StmtDesc::ClassDef {
                name,
                bases,
                keywords,
                body,
                decorator_list,
                type_params: _,
            } => format_class_def(name, bases, keywords, body, decorator_list, pprint_output),
            StmtDesc::Match { subject, cases } => {
                pprint_output.push_str("match ");
                subject.desc.pprint(pprint_output);
                pprint_output.push_str(":\n");
                pprint_output.inc_ident();

                for case in cases {
                    pprint_output.push_ident();
                    case.pprint(pprint_output);
                }

                pprint_output.dec_ident();
            }
        }
    }
}

impl MatchCase {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        pprint_output.push_str("case ");
        self.pattern.desc.pprint(pprint_output);

        match &self.guard {
            Some(if_clause) => {
                pprint_output.push_str(" if ");
                (*if_clause.desc).pprint(pprint_output);
            }
            None => (),
        }

        pprint_output.push_str(":\n");

        format_block(&self.body, pprint_output);
        pprint_output.push_str("\n");
    }
}

impl PatternDesc {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        match self {
            PatternDesc::MatchOr(or_choices) => {
                format_vec_pattern_for_or(or_choices, pprint_output)
            }
            PatternDesc::MatchSequence(choices) => {
                pprint_output.push_str("[");
                format_vec_pattern(choices, pprint_output);
                pprint_output.push_str("]");
            }
            PatternDesc::MatchClass {
                cls,
                patterns,
                kwd_attrs,
                kwd_patterns,
            } => {
                cls.desc.pprint(pprint_output);
                pprint_output.push_str("(");
                let mut at_least_one_instance = false;

                for pattern in patterns {
                    at_least_one_instance = true;
                    (*pattern.desc).pprint(pprint_output);
                    pprint_output.push_str(", ");
                }

                for instance in kwd_attrs.iter().zip(kwd_patterns.iter()) {
                    at_least_one_instance = true;

                    let (name, pattern) = instance;
                    pprint_output.push_str(format!("{}=", name).as_str());
                    (*pattern.desc).pprint(pprint_output);
                    pprint_output.push_str(", ");
                }

                if at_least_one_instance {
                    pprint_output.pop_many(2);
                }

                pprint_output.push_str(")");
            }
            PatternDesc::MatchStar(name) => match name {
                Some(name) => pprint_output.push_str(format!("*{}", name).as_str()),
                None => pprint_output.push_str("*_"),
            },
            PatternDesc::MatchValue(expr) => expr.desc.pprint(pprint_output),
            PatternDesc::MatchSingleton(constant) => match constant {
                Some(constant) => constant.pprint(pprint_output),
                None => pprint_output.push_str("None"),
            },

            PatternDesc::MatchMapping {
                keys,
                patterns,
                rest,
            } => {
                pprint_output.push_str("{");

                let mut at_least_one_instance = false;

                for instance in keys.iter().zip(patterns.iter()) {
                    at_least_one_instance = true;

                    let (expression, pattern) = instance;
                    (*expression.desc).pprint(pprint_output);
                    pprint_output.push_str(": ");
                    (*pattern.desc).pprint(pprint_output);
                    pprint_output.push_str(", ");
                }

                if let Some(rest) = rest {
                    pprint_output.push_str(format!("**{}", rest).as_str());
                } else if at_least_one_instance {
                    pprint_output.pop_many(2);
                }

                pprint_output.push_str("}");
            }

            PatternDesc::MatchAs { pattern, name } => {
                if pattern.is_none() && name.is_none() {
                    pprint_output.push_str("_");
                } else if pattern.is_some() {
                    pattern.as_ref().unwrap().desc.pprint(pprint_output);

                    if name.is_some() {
                        pprint_output.push_str(" as ");
                        pprint_output.push_str(name.clone().unwrap().as_str());
                    }
                } else if name.is_some() {
                    pprint_output.push_str(name.clone().unwrap().as_str());
                }
            }
        }
    }
}

fn format_class_def(
    name: &str,
    bases: &[Expr],
    keywords: &[Keyword],
    body: &[Stmt],
    decorator_list: &[Expr],
    pprint_output: &mut PrintHelper,
) {
    for item in decorator_list.iter() {
        pprint_output.push_str("@");
        (*item.desc).pprint(pprint_output);
        pprint_output.push_str("\n");
    }
    pprint_output.push_str(format!("class {}", name).as_str());

    if !bases.is_empty() || !keywords.is_empty() {
        //superclass_items
        pprint_output.push_str("(");
        format_vec_expr(bases, pprint_output);
        if !bases.is_empty() && !keywords.is_empty() {
            pprint_output.push_str(", ");
        }

        if !keywords.is_empty() {
            format_vec_keywords(keywords, pprint_output)
        }
        pprint_output.push_str(")");
    }

    pprint_output.push_str(":\n");

    if !body.is_empty() {
        pprint_output.inc_ident();
        process_module_class_functiondef_block(body, false, pprint_output);
        pprint_output.dec_ident();

        match &body.last().unwrap().desc {
            StmtDesc::FunctionDef { .. } | StmtDesc::AsyncFunctionDef { .. } => (),
            _ => {
                pprint_output.push_str("\n");
            }
        }
    }
}

pub fn format_with(
    is_async: bool,
    items: &[Withitem],
    body: &[Stmt],
    _type_comment: &Option<String>,
    pprint_output: &mut PrintHelper,
) {
    if is_async {
        pprint_output.push_str("async ");
    }
    pprint_output.push_str("with ");

    let mut at_least_one = false;
    for withitem in items.iter() {
        at_least_one = true;
        withitem.pprint(pprint_output);
        pprint_output.push_str(", ");
    }
    if at_least_one {
        pprint_output.pop_many(2);
    }

    pprint_output.push_str(":\n");
    format_block(body, pprint_output);
}

pub fn format_funcdef(
    is_async: bool,
    name: &str,
    args: &Arguments,
    body: &[Stmt],
    decorator_list: &[Expr],
    _type_params: &[Expr],
    returns: &Option<Expr>,
    _type_comment: &Option<String>,
    pprint_output: &mut PrintHelper,
) {
    for item in decorator_list.iter() {
        pprint_output.push_str("@");
        (*item.desc).pprint(pprint_output);
        pprint_output.push_str("\n");
        pprint_output.push_ident();
    }
    if is_async {
        pprint_output.push_str("async def ");
    } else {
        pprint_output.push_str("def ");
    }
    pprint_output.push_str(name);
    // args are quite involved
    {
        let mut at_least_one_arg = false;
        pprint_output.push_str("(");

        {
            let mut positional_and_args = vec![];
            for it in args.posonlyargs.iter() {
                positional_and_args.push(it);
            }

            let mut pos_slash_index: i32 = positional_and_args.len() as i32;

            for it in args.args.iter() {
                positional_and_args.push(it);
            }

            // normal args - may have default values...
            let mut args_w_detaults = vec![]; // these come second
            for it in positional_and_args
                .iter()
                .rev()
                .zip(args.defaults.iter().rev())
            {
                args_w_detaults.push(it);
            }

            // printing
            // print those without defaults
            let mut w_default_cnt = positional_and_args.len() - args_w_detaults.len();
            if w_default_cnt > 0 {
                for arg_no_default in positional_and_args.iter() {
                    at_least_one_arg = true;
                    arg_no_default.pprint(pprint_output);
                    pprint_output.push_str(", ");
                    pos_slash_index -= 1;
                    if pos_slash_index == 0 {
                        pprint_output.push_str("/, ");
                    }
                    w_default_cnt -= 1;
                    if w_default_cnt == 0 {
                        break;
                    }
                }
            }
            // now for those with defaults:
            for it in args_w_detaults.iter().rev() {
                at_least_one_arg = true;
                let (arg, defaultx) = it;
                arg.pprint(pprint_output);
                pprint_output.push_str("=");
                (*defaultx.desc).pprint(pprint_output);
                pprint_output.push_str(", ");
                pos_slash_index -= 1;
                if pos_slash_index == 0 {
                    pprint_output.push_str("/, ");
                }
            }
        }

        //vararg
        match &args.vararg {
            Some(vararg) => {
                at_least_one_arg = true;
                pprint_output.push_str("*");
                vararg.pprint(pprint_output);
                pprint_output.push_str(", ");
            }
            _ => {
                if !args.kwonlyargs.is_empty() {
                    pprint_output.push_str("*, ");
                }
            }
        }

        //kwonlyargs
        for it in args.kwonlyargs.iter().zip(args.kw_defaults.iter()) {
            // normal args
            at_least_one_arg = true;
            let (kwonlyarg, kwonlyarg_default) = it;

            kwonlyarg.pprint(pprint_output);

            match kwonlyarg_default {
                Some(x) => {
                    pprint_output.push_str("=");
                    (*x.desc).pprint(pprint_output);
                }
                _ => (),
            }

            pprint_output.push_str(", ");
        }

        //...

        //kwarg
        match &args.kwarg {
            Some(kwarg) => {
                at_least_one_arg = true;
                pprint_output.push_str("**");
                kwarg.pprint(pprint_output);
                pprint_output.push_str(", ");
            }
            _ => (),
        }

        if at_least_one_arg {
            pprint_output.pop_many(2);
        }

        pprint_output.push_str(")");
    }
    // args

    match &returns {
        Some(ret) => {
            pprint_output.push_str(" -> ");
            (*ret.desc).pprint(pprint_output);
        }
        _ => (),
    }

    pprint_output.push_str(":\n");
    pprint_output.inc_ident();
    process_module_class_functiondef_block(body, false, pprint_output);
    pprint_output.dec_ident();
}

pub fn format_for_stmt(
    target: &Expr,
    iter: &Expr,
    body: &[Stmt],
    orelse: &[Stmt],
    is_async: bool,
    pprint_output: &mut PrintHelper,
) {
    if is_async {
        pprint_output.push_str("async ");
    }
    pprint_output.push_str("for ");
    (*target.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Tuple);
    pprint_output.push_str(" in ");
    (*iter.desc).pprint(pprint_output);
    pprint_output.push_str(":\n");

    format_block(body, pprint_output);

    if !orelse.is_empty() {
        pprint_output.push_str("\nelse:\n");

        format_block(orelse, pprint_output);
    }
}

pub fn format_if_stmt(
    test: &Expr,
    body: &[Stmt],
    orelse: &[Stmt],
    pprint_output: &mut PrintHelper,
) {
    pprint_output.push_str("if ");
    (*test.desc).pprint(pprint_output);
    pprint_output.push_str(":\n");

    format_block(body, pprint_output);

    let mut isfirst = true;
    let orelse_len = orelse.len();

    for orelse_statement in orelse.iter() {
        // Python has an interesting way to represent elif instances. "elif
        // clauses don’t have a special representation in the AST, but rather
        // appear as extra If nodes within the orelse section of the previous
        // one." - https://docs.python.org/3/library/ast.html
        // Hence if an orelse field of an If node contains a single if
        // statement then the parent else of the If node gets turned into an
        // elif block with the contents of the if. However, if there is more
        // than one item in the orelse field block (even if the first item is
        // an if) then this logic does not apply and the else of the If node
        //stays as an else.
        if orelse_len == 1 {
            match &orelse_statement.desc {
                StmtDesc::If { test, body, orelse } => {
                    if isfirst {
                        pprint_output.push_str("\n");
                        pprint_output.push_ident();
                        pprint_output.push_str("el");
                    }
                    isfirst = false;
                    format_if_stmt(test, body, orelse, pprint_output);

                    pprint_output.push_str("\n");
                    continue;
                }
                _ => (),
            }
        }

        if isfirst {
            pprint_output.push_str("\n");
            pprint_output.push_ident();
            pprint_output.push_str("else:\n");
        }
        isfirst = false;
        pprint_output.inc_ident();
        orelse_statement.pprint(pprint_output);
        pprint_output.dec_ident();

        pprint_output.push_str("\n");
    }
    if !isfirst {
        // remove last newline if at least one orelse block
        pprint_output.pop();
    }
}

impl ExprDesc {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        // PriorityLevel::Test is the lowest level of priority besides tuple,
        // tuple is not used as the default to avoid supressing parenthesis
        // being added around tuple expressions of simple expressions.
        self.pprint_with_priority_level(pprint_output, PriorityLevel::Test);
    }

    fn pprint_with_priority_level(
        &self,
        pprint_output: &mut PrintHelper,
        current_priority_level: PriorityLevel,
    ) {
        match self {
            ExprDesc::Name { id, ctx: _ } => pprint_output.push_str(id),
            ExprDesc::Constant { value, kind: _ } => match value {
                Some(val) => val.pprint(pprint_output),
                _ => pprint_output.push_str("None"),
            },
            ExprDesc::List { elts, ctx: _ } => {
                pprint_output.push_str("[");
                format_vec_expr(elts, pprint_output);
                pprint_output.push_str("]");
            }
            ExprDesc::Tuple { elts, ctx: _ } => {
                let priority_level = PriorityLevel::Tuple;

                left_parenthesis(pprint_output, current_priority_level, priority_level);

                let mut count = 0;
                for expr in elts.iter() {
                    count += 1;
                    // PriorityLevel::Test, not Tuple
                    (*expr.desc).pprint(pprint_output);
                    pprint_output.push_str(", ");
                }

                if count > 0 {
                    pprint_output.pop();
                    if count > 1 {
                        // leave the last comma if there is only one item in the tuple
                        pprint_output.pop();
                    }
                }

                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Set(ss) => {
                pprint_output.push_str("{");
                format_vec_expr(ss, pprint_output);
                pprint_output.push_str("}");
            }
            ExprDesc::Await(arg) => {
                let priority_level = PriorityLevel::Await;
                left_parenthesis(pprint_output, current_priority_level, priority_level);
                pprint_output.push_str("await ");

                match *arg.desc {
                    ExprDesc::Yield(_) | ExprDesc::YieldFrom(_) => {
                        pprint_output.push_str("(");
                        (*arg.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Atom);
                        pprint_output.push_str(")");
                    }
                    _ => (*arg.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Atom),
                }

                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Dict { keys, values } => {
                pprint_output.push_str("{");
                let mut atleastone = false;

                for (k, v) in keys.iter().zip(values.iter()) {
                    match k {
                        Some(value) => {
                            (*value.desc).pprint(pprint_output);
                            pprint_output.push_str(": ");
                            (*v.desc).pprint(pprint_output);
                        }
                        _ => {
                            pprint_output.push_str("**");
                            (*v.desc).pprint_with_priority_level(
                                pprint_output,
                                PriorityLevel::ExpressionOrBitOr,
                            );
                        }
                    }
                    pprint_output.push_str(", ");
                    atleastone = true;
                }

                if atleastone {
                    pprint_output.pop_many(2);
                }

                pprint_output.push_str("}");
            }
            ExprDesc::Lambda { args, body } => {
                let priority_level = PriorityLevel::Test;
                left_parenthesis(pprint_output, current_priority_level, priority_level);
                pprint_output.push_str("lambda ");

                let mut atleastone = false;
                for arg in args.args.iter() {
                    atleastone = true;
                    arg.pprint(pprint_output);
                    pprint_output.push_str(", ");
                }

                if atleastone {
                    pprint_output.pop_many(2);
                } else {
                    // remove the space after the lambda
                    pprint_output.pop();
                }

                pprint_output.push_str(": ");
                (*body.desc).pprint_with_priority_level(pprint_output, priority_level);
                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::UnaryOp { op, operand } => {
                let priority_level = unary_operator_to_priority_level(op);
                left_parenthesis(pprint_output, current_priority_level, priority_level);
                op.pprint(pprint_output);
                (*operand.desc).pprint_with_priority_level(pprint_output, priority_level);
                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Subscript {
                value,
                slice,
                ctx: _,
            } => {
                (*value.desc).pprint(pprint_output);
                pprint_output.push_str("[");

                match &(*slice.desc) {
                    ExprDesc::Tuple { elts, ctx: _ } => {
                        let mut count = 0;
                        for expr in elts.iter() {
                            count += 1;
                            (*expr.desc).pprint(pprint_output);
                            pprint_output.push_str(", ");
                        }

                        pprint_output.pop(); // always remove the space
                        if count > 1 {
                            // when there is only one item, then leave the , at the end
                            pprint_output.pop();
                        }
                    }
                    _ => (*slice.desc).pprint(pprint_output),
                }

                pprint_output.push_str("]");
            }
            ExprDesc::Slice { lower, upper, step } => {
                if lower.is_none() && upper.is_none() && step.is_none() {
                    pprint_output.push_str(":");
                } else {
                    match lower {
                        Some(thing) => {
                            (*thing.desc).pprint(pprint_output);
                            pprint_output.push_str(":");
                        }
                        _ => (),
                    }

                    match upper {
                        Some(thing) => {
                            match lower {
                                Some(_) => (),
                                _ => pprint_output.push_str(":"),
                            }
                            (*thing.desc).pprint(pprint_output);
                        }
                        _ => (),
                    }

                    match step {
                        Some(thing) => {
                            match lower {
                                Some(_) => (),
                                _ => pprint_output.push_str(":"),
                            }

                            pprint_output.push_str(":");

                            (*thing.desc).pprint(pprint_output);
                        }
                        _ => (),
                    }
                }
            }
            ExprDesc::BinOp { left, op, right } => {
                let priority_level = operator_to_priority_level(op);
                // default is left associative
                let mut left_priority_level = priority_level;
                let mut right_priority_level = priority_level.next();

                if op == &Operator::Pow {
                    // right associative
                    left_priority_level = priority_level.next();
                    right_priority_level = priority_level;
                }

                left_parenthesis(pprint_output, current_priority_level, priority_level);
                (left.desc).pprint_with_priority_level(pprint_output, left_priority_level);
                pprint_output.push_str(" ");
                op.pprint(pprint_output);
                pprint_output.push_str(" ");
                (right.desc).pprint_with_priority_level(pprint_output, right_priority_level);
                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::BoolOp { op, values } => {
                let priority_level = bool_operator_to_priority_level(op);

                left_parenthesis(pprint_output, current_priority_level, priority_level);

                let mut is_first_elem = true;
                for value in values.iter() {
                    let value_description = &*value.desc;

                    if let ExprDesc::BoolOp {
                        op: child_operator, ..
                    } = value_description
                    {
                        let child_priority_level = bool_operator_to_priority_level(op);
                        // insert params for nested non collapsed boolean
                        // operators
                        if is_first_elem && op == &Boolop::Or && child_operator == &Boolop::And {
                            // except for case of: or(and(_, _), _)
                            // e.g. `a and b or c` stays as `a and b or c`
                            // with no params needed
                            value_description
                                .pprint_with_priority_level(pprint_output, child_priority_level);
                        } else {
                            value_description
                                .pprint_with_priority_level(pprint_output, PriorityLevel::Atom);
                        }
                    } else if let ExprDesc::UnaryOp {
                        op: child_operator, ..
                    } = value_description
                    {
                        // Circumvent operator precedence here and print always parantheses around
                        // `not` operator if it is not the first element in a binary expression.
                        // Necessary to comply with CPythons AST pretty printing.
                        if !is_first_elem && child_operator == &Unaryop::Not {
                            pprint_output.push_str("(");
                            value_description.pprint(pprint_output);
                            pprint_output.push_str(")");
                        } else {
                            value_description.pprint(pprint_output);
                        }
                    } else {
                        value_description.pprint(pprint_output);
                    }
                    pprint_output.push_str(" ");
                    op.pprint(pprint_output);
                    pprint_output.push_str(" ");
                    is_first_elem = false;
                }

                pprint_output.pop_many(2);
                match op {
                    Boolop::And => {
                        pprint_output.pop_many(3);
                    }
                    Boolop::Or => {
                        pprint_output.pop_many(2);
                    }
                };

                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Compare {
                left,
                ops,
                comparators,
            } => {
                let priority_level = PriorityLevel::Compare;
                left_parenthesis(pprint_output, current_priority_level, priority_level);

                (*left.desc).pprint_with_priority_level(pprint_output, priority_level.next());
                pprint_output.push_str(" ");

                for it in ops.iter().zip(comparators.iter()) {
                    let (op, comp) = it;
                    op.pprint(pprint_output);
                    pprint_output.push_str(" ");
                    (*comp.desc).pprint_with_priority_level(pprint_output, priority_level.next());
                    pprint_output.push_str(" ");
                }
                pprint_output.pop();

                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Attribute {
                value,
                attr,
                ctx: _,
            } => {
                (*value.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Atom);

                match *value.desc {
                    ExprDesc::Constant {
                        value:
                            Some(ConstantDesc::Num(Num::Int(_)))
                            | Some(ConstantDesc::Num(Num::BigInt(_))),
                        ..
                    } => pprint_output.push_str(" ."),
                    _ => pprint_output.push_str("."),
                }

                pprint_output.push_str(attr);
            }
            ExprDesc::Call {
                func,
                args,
                keywords,
            } => {
                (*func.desc).pprint(pprint_output);
                pprint_output.push_str("(");

                let mut atleastone = false;
                for keyword_or_arg in args.iter() {
                    atleastone = true;
                    (*keyword_or_arg.desc).pprint(pprint_output);
                    pprint_output.push_str(", ");
                }
                for keyword_or_arg in keywords.iter() {
                    atleastone = true;
                    keyword_or_arg.pprint(pprint_output);
                    pprint_output.push_str(", ");
                }
                if atleastone {
                    pprint_output.pop_many(2);
                }

                pprint_output.push_str(")");
            }
            ExprDesc::Starred { value, ctx: _ } => {
                pprint_output.push_str("*");
                (*value.desc)
                    .pprint_with_priority_level(pprint_output, PriorityLevel::ExpressionOrBitOr);
            }
            ExprDesc::IfExp { test, body, orelse } => {
                let priority_level = PriorityLevel::Test;
                left_parenthesis(pprint_output, current_priority_level, priority_level);

                (*body.desc).pprint_with_priority_level(pprint_output, priority_level.next());
                pprint_output.push_str(" if ");
                (*test.desc).pprint_with_priority_level(pprint_output, priority_level.next());
                pprint_output.push_str(" else ");
                (*orelse.desc).pprint_with_priority_level(pprint_output, priority_level);

                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::ListComp { elt, generators } => {
                format_list_comp("[", "]", &elt.desc, generators, pprint_output);
            }
            ExprDesc::SetComp { elt, generators } => {
                format_list_comp("{", "}", &elt.desc, generators, pprint_output);
            }
            ExprDesc::GeneratorExp { elt, generators } => {
                format_list_comp("(", ")", &elt.desc, generators, pprint_output);
            }
            ExprDesc::DictComp {
                key,
                value,
                generators,
            } => {
                pprint_output.push_str("{");
                (*key.desc).pprint(pprint_output);
                pprint_output.push_str(": ");
                (*value.desc).pprint(pprint_output);
                pprint_output.push_str(" ");

                for comp in generators.iter() {
                    comp.pprint(pprint_output);
                    pprint_output.push_str(" ");
                }
                pprint_output.pop();

                pprint_output.push_str("}");
            }
            ExprDesc::NamedExpr { target, value } => {
                let priority_level = PriorityLevel::NamedExpr;
                left_parenthesis(pprint_output, current_priority_level, priority_level);
                (*target.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Atom);
                pprint_output.push_str(" := ");
                (*value.desc).pprint_with_priority_level(pprint_output, PriorityLevel::Atom);
                right_parenthesis(pprint_output, current_priority_level, priority_level);
            }
            ExprDesc::Yield(callx) => {
                pprint_output.push_str("yield");

                match &callx {
                    Some(val) => {
                        pprint_output.push_str(" ");

                        match *val.desc {
                            ExprDesc::Await(_) => {
                                pprint_output.push_str("(");
                                (*val.desc).pprint(pprint_output);
                                pprint_output.push_str(")");
                            }
                            _ => (*val.desc).pprint(pprint_output),
                        }
                    }
                    _ => (),
                }
            }
            ExprDesc::YieldFrom(val) => {
                pprint_output.push_str("yield from ");

                match *val.desc {
                    ExprDesc::Await(_) => {
                        pprint_output.push_str("(");
                        (*val.desc).pprint(pprint_output);
                        pprint_output.push_str(")");
                    }
                    _ => (*val.desc).pprint(pprint_output),
                }
            }
            ExprDesc::JoinedStr(exprs) => {
                pprint_output.push_str("f");

                if exprs.len() == 1 {
                    // special logic to deal with case where f-string consists
                    // of a single string and no iterpolation nodes...
                    if let ExprDesc::Constant {
                        value: Some(ConstantDesc::Str(const_string)),
                        kind: _,
                    } = &*exprs[0].desc
                    {
                        let formatted_output = const_string.replace('{', "{{").replace('}', "}}");
                        pprint_output.push_str(&formatted_output);
                        return;
                    }
                }

                // if a constant part of the f-string contains single quotes
                // we surround the string with double quotes
                let mut needs_double_quote = false;
                let f_string_output_start = pprint_output.current_length();
                for expr in exprs.iter() {
                    pprint_output.ignore_next_n_chars(1);

                    if let ExprDesc::Constant {
                        value: Some(ConstantDesc::Str(const_string)),
                        kind: _,
                    } = &*expr.desc
                    {
                        let formatted_output = const_string.replace('{', "{{").replace('}', "}}");
                        needs_double_quote |=
                            const_string[1..const_string.len() - 1].contains('\'');
                        pprint_output.push_str(&formatted_output);
                    } else {
                        (*expr.desc).pprint(pprint_output);
                    }

                    pprint_output.pop();
                }

                let quote_type = if needs_double_quote { "\"" } else { "'" };
                pprint_output.insert_at(f_string_output_start, quote_type);
                pprint_output.push_str(quote_type);
            }
            ExprDesc::FormattedValue {
                value,
                conversion,
                format_spec,
            } => {
                // The f-string's consists of a series of formattedValues and
                // string constant nodes. Strings are always wrapped in either
                // ' or " (depending on if they themselves contain a ') and we
                // always ignore the leading and trailing char (the ' or ")
                // via the `ignore_next_n_chars` and `pop` calls. For
                // consistency therefore, we have to add a leading and
                // trailing '
                pprint_output.push_str("'{");
                (*value.desc).pprint(pprint_output);

                if let Some(conversion) = conversion {
                    // the following magic numbers are defined in the Python
                    // language spec:
                    // https://docs.python.org/3.10/library/ast.html#ast.FormattedValue
                    pprint_output.push_str(match conversion {
                        115 => "!s",
                        114 => "!r",
                        97 => "!a",
                        _ => "",
                    });
                }

                if let Some(format_spec) = format_spec {
                    pprint_output.push_str(":");
                    pprint_output.ignore_next_n_chars(2);
                    (*format_spec.desc).pprint(pprint_output);
                    pprint_output.pop();
                }
                pprint_output.push_str("}'");
            }
        }
    }
}

fn format_list_comp(
    prefix: &str,
    postfix: &str,
    exprdesc: &ExprDesc,
    generators: &[Comprehension],
    pprint_output: &mut PrintHelper,
) {
    pprint_output.push_str(prefix);
    exprdesc.pprint(pprint_output);
    pprint_output.push_str(" ");

    for comp in generators.iter() {
        comp.pprint(pprint_output);
        pprint_output.push_str(" ");
    }
    pprint_output.pop();

    pprint_output.push_str(postfix);
}

impl Arg {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        pprint_output.push_str(&self.arg);
        match &self.annotation {
            Some(annot) => {
                pprint_output.push_str(": ");
                (*annot.desc).pprint(pprint_output);
            }
            _ => (),
        }
    }
}

fn handle_assign_rhs_yield(value: &ExprDesc, pprint_output: &mut PrintHelper) {
    // Bit of a hack but it seems that yeild when used on the rhs of an expression requires wrapping in params: ()
    match &value {
        ExprDesc::Yield(yldarg) => {
            pprint_output.push_str("(yield");
            match yldarg {
                Some(arg) => {
                    pprint_output.push_str(" ");
                    (*arg.desc).pprint(pprint_output);
                }
                _ => (),
            }
            pprint_output.push_str(")");
        }
        ExprDesc::YieldFrom(yfarg) => {
            pprint_output.push_str("(yield from ");
            (*yfarg.desc).pprint(pprint_output);
            pprint_output.push_str(")");
        }
        _ => {
            value.pprint(pprint_output);
        }
    };
}

impl Comprehension {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        pprint_output.push_str(if self.is_async { "async for " } else { "for " });
        self.target
            .desc
            .pprint_with_priority_level(pprint_output, PriorityLevel::Tuple);
        pprint_output.push_str(" in ");
        self.iter.desc.pprint(pprint_output);
        pprint_output.push_str(" ");
        for guard in self.ifs.iter() {
            pprint_output.push_str("if ");
            guard.desc.pprint(pprint_output);
            pprint_output.push_str(" ");
        }

        pprint_output.pop();
    }
}

impl Keyword {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        match &self.arg {
            Some(arg) => {
                pprint_output.push_str(arg);
                pprint_output.push_str("=");
                (*self.value.desc).pprint(pprint_output);
            }
            _ => {
                pprint_output.push_str("**");
                (*self.value.desc).pprint(pprint_output);
            }
        }
    }
}

impl Unaryop {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        let res = match self {
            Unaryop::Invert => "~",
            Unaryop::Not => "not ",
            Unaryop::UAdd => "+",
            Unaryop::USub => "-",
        };

        pprint_output.push_str(res)
    }
}

impl Operator {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        let res = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mult => "*",
            Operator::MatMult => "@",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::Pow => "**",
            Operator::LShift => "<<",
            Operator::RShift => ">>",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::BitAnd => "&",
            Operator::FloorDiv => "//",
        };

        pprint_output.push_str(res)
    }
}

impl Cmpop {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        let res = match self {
            Cmpop::Eq => "==",
            Cmpop::NotEq => "!=",
            Cmpop::Lt => "<",
            Cmpop::LtE => "<=",
            Cmpop::Gt => ">",
            Cmpop::GtE => ">=",
            Cmpop::Is => "is",
            Cmpop::IsNot => "is not",
            Cmpop::In => "in",
            Cmpop::NotIn => "not in",
        };

        pprint_output.push_str(res)
    }
}

impl Boolop {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        let res = match self {
            Boolop::And => "and",
            Boolop::Or => "or",
        };

        pprint_output.push_str(res)
    }
}

impl ConstantDesc {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        match self {
            ConstantDesc::Num(Num::Int(vala)) => {
                pprint_output.push_str(&vala.to_string());
            }
            ConstantDesc::Num(Num::Float(vala)) => {
                pprint_output.push_str(&cpython_float_to_string(vala, false));
            }
            ConstantDesc::Num(Num::Complex(vala)) => {
                pprint_output.push_str(&cpython_float_to_string(vala, true));
                pprint_output.push_str("j");
            }
            rest => {
                let res = match rest {
                    ConstantDesc::Str(vala) => vala,
                    ConstantDesc::ByteStr(vala) => {
                        pprint_output.push_str("b");
                        vala
                    }
                    ConstantDesc::Num(Num::BigInt(vala)) => vala,
                    ConstantDesc::Bool(true) => "True",
                    ConstantDesc::Bool(false) => "False",
                    ConstantDesc::Ellipsis => "...",
                    _ => "",
                };
                pprint_output.push_str(res)
            }
        };
    }
}

impl Withitem {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        self.context_expr.desc.pprint(pprint_output);

        if let Some(opt) = &self.optional_vars {
            pprint_output.push_str(" as ");
            opt.desc.pprint(pprint_output);
        }
    }
}

impl ExcepthandlerDesc {
    pub fn pprint(&self, pprint_output: &mut PrintHelper) {
        match self {
            ExcepthandlerDesc::ExceptHandler { type__, name, body } => {
                pprint_output.push_str("\n");
                pprint_output.push_ident();
                pprint_output.push_str("except");

                if let Some(tt) = type__ {
                    pprint_output.push_str(" ");
                    (*tt.desc).pprint(pprint_output);
                }

                if let Some(nname) = name {
                    pprint_output.push_str(" as ");
                    pprint_output.push_str(nname);
                }

                pprint_output.push_str(":\n");

                format_block(body, pprint_output);
            }
        }
    }
}
