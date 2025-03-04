/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A qualified name - name plus its location.

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::util::lock::RwLock;

/// A name, plus where it is defined.
pub struct QName {
    /// The `name` and `range` must be consistent.
    /// They always come from a single `Identifier`.
    name: Name,
    range: TextRange,
    module_name: ModuleName,
    module_info: RwLock<ModuleInfo>,
}

impl Debug for QName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("QName")
            .field("name", &self.name)
            // The full details of ModuleInfo are pretty boring in most cases,
            // and we only cache it so we can defer expanding the range.
            // Therefore, shorten the Debug output, as ModuleInfo is pretty big.
            .field("module", &self.module_name)
            .finish()
    }
}

impl PartialEq for QName {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for QName {}

impl PartialOrd for QName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
    }
}

impl Display for QName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_location(f)
    }
}

impl QName {
    fn key(&self) -> (&Name, TextSize, TextSize, ModuleName) {
        (
            &self.name,
            self.range.start(),
            self.range.end(),
            self.module_name,
        )
    }

    pub fn new(name: Identifier, module: ModuleInfo) -> Self {
        Self {
            name: name.id,
            range: name.range,
            module_name: module.name(),
            module_info: RwLock::new(module),
        }
    }

    pub fn id(&self) -> &Name {
        &self.name
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn short_identifier(&self) -> ShortIdentifier {
        ShortIdentifier::new_from_decomposed_identifier(self.range)
    }

    pub fn module_info(&self) -> ModuleInfo {
        self.module_info.read().dupe()
    }

    pub fn module_name(&self) -> ModuleName {
        self.module_name
    }

    pub fn fmt_name(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }

    pub fn fmt_with_module(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.module_name(), self.name)
    }

    pub fn fmt_with_location(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}@{}",
            self.module_name,
            self.name,
            self.module_info.read().source_range(self.range)
        )
    }

    pub fn immutable_eq(&self, other: &QName) -> bool {
        self.name == other.name
            && self.range == other.range
            && self.module_name == other.module_name
    }

    pub fn immutable_hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.range.hash(state);
        self.module_name.hash(state);
    }

    pub fn mutate(&self, x: &QName) {
        *self.module_info.write() = x.module_info().dupe();
    }
}
