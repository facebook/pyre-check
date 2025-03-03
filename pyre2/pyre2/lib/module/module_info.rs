/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::ModModule;
use ruff_source_file::LineIndex;
use ruff_source_file::OneIndexed;
use ruff_source_file::SourceLocation;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::ast::Ast;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::module::ignore::Ignore;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;

#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq, Hash, Default)]
pub struct SourceRange {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.row == self.end.row {
            if self.start.column == self.end.column {
                write!(f, "{}:{}", self.start.row, self.start.column)
            } else {
                write!(
                    f,
                    "{}:{}-{}",
                    self.start.row, self.start.column, self.end.column
                )
            }
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.start.row, self.start.column, self.end.row, self.end.column
            )
        }
    }
}

/// Information about a module, notably its name, path, and contents.
#[derive(Debug, Clone, Dupe)]
pub struct ModuleInfo(Arc<ModuleInfoInner>);

#[derive(Debug, Clone)]
struct ModuleInfoInner {
    name: ModuleName,
    path: ModulePath,
    index: LineIndex,
    ignore: Ignore,
    contents: Arc<String>,
}

impl ModuleInfo {
    /// Create a new ModuleInfo. Will NOT read the `path`, but use the value from `contents` instead.
    pub fn new(name: ModuleName, path: ModulePath, contents: Arc<String>) -> Self {
        let index = LineIndex::from_source_text(&contents);
        let ignore = Ignore::new(&contents);
        Self(Arc::new(ModuleInfoInner {
            name,
            path,
            index,
            ignore,
            contents,
        }))
    }

    pub fn len(&self) -> usize {
        self.0.contents.len()
    }

    pub fn source_range(&self, range: TextRange) -> SourceRange {
        SourceRange {
            start: self.source_location(range.start()),
            end: self.source_location(range.end()),
        }
    }

    pub fn source_location(&self, offset: TextSize) -> SourceLocation {
        assert!(
            offset.to_usize() <= self.len(),
            "Module {}({}): offset out of range, expected {} <= {}",
            self.0.name,
            self.0.path,
            offset.to_usize(),
            self.len()
        );
        self.0.index.source_location(offset, &self.0.contents)
    }

    pub fn code_at(&self, range: TextRange) -> &str {
        &self.0.contents[range]
    }

    pub fn path(&self) -> &ModulePath {
        &self.0.path
    }

    pub fn contents(&self) -> &Arc<String> {
        &self.0.contents
    }

    pub fn parse(&self, errors: &ErrorCollector) -> ModModule {
        let (module, parse_errors) = Ast::parse(self.contents());
        for err in parse_errors {
            errors.add(
                err.location,
                format!("Parse error: {err}"),
                ErrorKind::ParseError,
            );
        }
        module
    }

    pub fn name(&self) -> ModuleName {
        self.0.name
    }

    pub fn to_text_size(&self, line: u32, column: u32) -> TextSize {
        self.0.index.offset(
            OneIndexed::from_zero_indexed(line as usize),
            OneIndexed::from_zero_indexed(column as usize),
            &self.0.contents,
        )
    }

    pub fn is_ignored(&self, source_range: &SourceRange, msg: &str) -> bool {
        self.0.ignore.is_ignored(source_range, msg)
    }
}

#[derive(Debug, Clone)]
pub struct TextRangeWithModuleInfo {
    pub module_info: ModuleInfo,
    pub range: TextRange,
}

impl TextRangeWithModuleInfo {
    pub fn new(module_info: ModuleInfo, range: TextRange) -> Self {
        Self { module_info, range }
    }
}
