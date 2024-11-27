/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::fmt;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
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
use crate::module::ignore::Ignore;
use crate::module::module_name::ModuleName;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
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

#[derive(Debug, Clone, Dupe)]
pub struct ModuleInfo(Arc<ModuleInfoInner>);

#[derive(Debug, Clone)]
struct ModuleInfoInner {
    name: ModuleName,
    path: PathBuf,
    index: LineIndex,
    ignore: Ignore,
    contents: String,
    should_type_check: bool,
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, Hash)]
pub enum ModuleStyle {
    /// .py - executable code.
    Executable,
    /// .pyi - just types that form an interface.
    Interface,
}

impl ModuleInfo {
    /// Create a new ModuleInfo. Will NOT read the `path`, but use the value from `contents` instead.
    pub fn new(name: ModuleName, path: PathBuf, contents: String, should_type_check: bool) -> Self {
        let index = LineIndex::from_source_text(&contents);
        let ignore = Ignore::new(&contents);
        Self(Arc::new(ModuleInfoInner {
            name,
            path,
            index,
            ignore,
            contents,
            should_type_check,
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
            self.0.path.display(),
            offset.to_usize(),
            self.len()
        );
        self.0.index.source_location(offset, &self.0.contents)
    }

    pub fn code_at(&self, range: TextRange) -> &str {
        &self.0.contents[range]
    }

    /// Whether things imported by this module are reexported.
    pub fn style(&self) -> ModuleStyle {
        if self.0.path.extension() == Some(OsStr::new("pyi")) {
            ModuleStyle::Interface
        } else {
            ModuleStyle::Executable
        }
    }

    pub fn is_interface(&self) -> bool {
        self.style() == ModuleStyle::Interface
    }

    pub fn path(&self) -> &Path {
        &self.0.path
    }

    pub fn contents(&self) -> &str {
        &self.0.contents
    }

    pub fn parse(&self, errors: &ErrorCollector) -> ModModule {
        let (module, parse_errors) = Ast::parse(self.contents());
        for err in parse_errors {
            errors.add(self, err.location, format!("Parse error: {err}"));
        }
        module
    }

    pub fn is_init(&self) -> bool {
        self.0.path.file_stem() == Some(OsStr::new("__init__"))
    }

    pub fn should_type_check(&self) -> bool {
        self.0.should_type_check
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

    pub fn is_ignored(&self, source_range: SourceRange, msg: &str) -> bool {
        self.0.ignore.is_ignored(source_range, msg)
    }
}
