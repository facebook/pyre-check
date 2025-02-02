/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::str::FromStr;

use itertools::Itertools;
use regex::Match;
use regex::Regex;
use ruff_python_ast::BoolOp;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprNumberLiteral;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtIf;

use crate::ast::Ast;
use crate::util::prelude::SliceExt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PythonVersion {
    major: u32,
    minor: u32,
    micro: u32,
}

impl Default for PythonVersion {
    fn default() -> Self {
        Self {
            major: 3,
            minor: 12,
            micro: 0,
        }
    }
}

impl FromStr for PythonVersion {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let version_pattern = Regex::new(r"(\d+)(\.(\d+))?(\.(\d+))?").unwrap();
        let captures = version_pattern
            .captures(s)
            .ok_or_else(|| anyhow::anyhow!("Invalid version string: {s}."))?;

        fn extract_number(capture: Option<Match>, default: u32) -> anyhow::Result<u32> {
            capture.map_or(Ok(default), |capture| {
                let capture_str = capture.as_str();
                let number = capture_str
                    .parse::<u32>()
                    .map_err(|_| anyhow::anyhow!("Invalid version number {capture_str}"))?;
                Ok(number)
            })
        }

        let major = extract_number(captures.get(1), 3)?;
        let minor = extract_number(captures.get(3), 0)?;
        let micro = extract_number(captures.get(5), 0)?;
        Ok(Self {
            major,
            minor,
            micro,
        })
    }
}

impl PythonVersion {
    #[allow(dead_code)] // Only used in tests so far
    pub fn new(major: u32, minor: u32, micro: u32) -> Self {
        Self {
            major,
            minor,
            micro,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Config {
    version: PythonVersion,
    platform: String,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: PythonVersion::default(),
            platform: "linux".to_owned(),
        }
    }
}

impl Config {
    pub fn new(version: PythonVersion, platform: String) -> Self {
        Self { version, platform }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Value {
    Tuple(Vec<Value>),
    String(String),
    Int(i64),
    Bool(bool),
}

impl Value {
    fn to_bool(&self) -> bool {
        match self {
            Value::Bool(x) => *x,
            Value::Int(x) => *x != 0,
            Value::String(x) => !x.is_empty(),
            Value::Tuple(x) => !x.is_empty(),
        }
    }

    fn same_type(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Tuple(_), Value::Tuple(_)) => true,
            (Value::String(_), Value::String(_)) => true,
            (Value::Int(_), Value::Int(_)) => true,
            (Value::Bool(_), Value::Bool(_)) => true,
            _ => false,
        }
    }

    fn compare(&self, op: CmpOp, other: &Value) -> Option<bool> {
        if !self.same_type(other) {
            return None; // Someone got confused
        }
        Some(match op {
            CmpOp::Eq => self == other,
            CmpOp::NotEq => self != other,
            CmpOp::Lt => self < other,
            CmpOp::LtE => self <= other,
            CmpOp::Gt => self > other,
            CmpOp::GtE => self >= other,
            _ => return None,
        })
    }
}

impl Config {
    /// Return true/false if we can statically evaluate it, and None if we can't.
    pub fn evaluate_bool(&self, x: &Expr) -> Option<bool> {
        Some(self.evaluate(x)?.to_bool())
    }

    /// Version of `evaluate_bool` where `None` means no test (thus always statically true).
    pub fn evaluate_bool_opt(&self, x: Option<&Expr>) -> Option<bool> {
        match x {
            None => Some(true),
            Some(x) => self.evaluate_bool(x),
        }
    }

    fn evaluate(&self, x: &Expr) -> Option<Value> {
        match x {
            Expr::Compare(x) if x.ops.len() == 1 && x.comparators.len() == 1 => Some(Value::Bool(
                self.evaluate(&x.left)?
                    .compare(x.ops[0], &self.evaluate(&x.comparators[0])?)?,
            )),
            Expr::Attribute(ExprAttribute {
                value: box Expr::Name(name),
                attr,
                ..
            }) if &name.id == "sys" => match attr.as_str() {
                "platform" => Some(Value::String(self.platform.clone())),
                "version_info" => Some(Value::Tuple(vec![
                    Value::Int(self.version.major as i64),
                    Value::Int(self.version.minor as i64),
                ])),
                _ => None,
            },
            Expr::Tuple(x) => Some(Value::Tuple(
                x.elts.try_map(|x| self.evaluate(x).ok_or(())).ok()?,
            )),
            Expr::NumberLiteral(ExprNumberLiteral { value: i, .. }) => {
                Some(Value::Int(i.as_int()?.as_i64()?))
            }
            Expr::StringLiteral(x) => Some(Value::String(x.value.to_str().to_owned())),
            Expr::BoolOp(x) => match x.op {
                BoolOp::And => {
                    let mut last = Value::Bool(true);
                    for x in &x.values {
                        last = self.evaluate(x)?;
                        if !last.to_bool() {
                            break;
                        }
                    }
                    Some(last)
                }
                BoolOp::Or => {
                    let mut last = Value::Bool(false);
                    for x in &x.values {
                        last = self.evaluate(x)?;
                        if last.to_bool() {
                            break;
                        }
                    }
                    Some(last)
                }
            },
            _ => None,
        }
    }

    /// Like `Ast::if_branches`, but skips branch that statically evaluate to `false`,
    /// and stops if any branch evalutes to `true`.
    pub fn pruned_if_branches<'a, 'b: 'a>(
        &'a self,
        x: &'b StmtIf,
    ) -> impl Iterator<Item = (Option<&'b Expr>, &'b [Stmt])> + 'a {
        Ast::if_branches(x)
            .map(|(test, body)| {
                let b = self.evaluate_bool_opt(test);
                (b, if b == Some(true) { None } else { test }, body)
            })
            .filter(|x| x.0 != Some(false))
            .map(|x| (x.1, x.2))
            .take_while_inclusive(|x| x.0.is_some())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_py_version() {
        assert_eq!(
            PythonVersion::from_str("3").unwrap(),
            PythonVersion::new(3, 0, 0)
        );
        assert_eq!(
            PythonVersion::from_str("3.8").unwrap(),
            PythonVersion::new(3, 8, 0)
        );
        assert_eq!(
            PythonVersion::from_str("3.8.6").unwrap(),
            PythonVersion::new(3, 8, 6)
        );
        assert_eq!(
            PythonVersion::from_str("3.10.2").unwrap(),
            PythonVersion::new(3, 10, 2)
        );
        assert_eq!(
            PythonVersion::from_str("python3.10").unwrap(),
            PythonVersion::new(3, 10, 0)
        );
        assert_eq!(
            PythonVersion::from_str("cinder.3.8").unwrap(),
            PythonVersion::new(3, 8, 0)
        );
        assert_eq!(
            PythonVersion::from_str("3.10.cinder").unwrap(),
            PythonVersion::new(3, 10, 0)
        );
        assert!(PythonVersion::from_str("").is_err());
        assert!(PythonVersion::from_str("abc").is_err());
    }

    #[test]
    fn test_tuple_lexicographical_compare() {
        fn assert_compare(op: CmpOp, x: &[i64], y: &[i64]) {
            let lhs = Value::Tuple(x.map(|x| Value::Int(*x)));
            let rhs = Value::Tuple(y.map(|x| Value::Int(*x)));
            assert_eq!(lhs.compare(op, &rhs), Some(true));
        }

        assert_compare(CmpOp::Eq, &[], &[]);
        assert_compare(CmpOp::Lt, &[], &[1]);
        assert_compare(CmpOp::Eq, &[1], &[1]);
        assert_compare(CmpOp::Lt, &[1], &[1, 2]);
        assert_compare(CmpOp::Lt, &[1], &[2]);
        assert_compare(CmpOp::Lt, &[1], &[2, 3]);
        assert_compare(CmpOp::Lt, &[1, 2], &[1, 2, 3]);
        assert_compare(CmpOp::Gt, &[1, 3], &[1, 2, 3]);
    }
}
