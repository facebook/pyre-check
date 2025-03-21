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
use pyrefly_derive::TypeEq;
use ruff_python_ast::name::Name;
use starlark_map::ordered_set::OrderedSet;

use crate::module::module_name::ModuleName;
use crate::types::types::Type;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

/// In Python if you do `import foo.bar` and `import foo.baz` then what you are really
/// doing is importing a single symbol `foo` that contains the two modules accessible from it.
///
/// To represent that, we have a set of modules and a `path` of how far down we are.
/// Any module that does not start with a prefix of the `path` is no longer accessible,
/// but we keep them around (under an `Arc`) since it's more efficient not to recreate
/// the `SmallMap` on each access.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Module {
    path: Box<[Name]>,
    /// Use an OrderedMap so we have a table Hash/Ord instance.
    modules: Arc<OrderedSet<ModuleName>>,
}

impl Visit<Type> for Module {
    const RECURSE_CONTAINS: bool = false;
    fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a Type)) {}
}

impl VisitMut<Type> for Module {
    const RECURSE_CONTAINS: bool = false;
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {}
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.path.join("."))
    }
}

impl Module {
    /// Created from an import, e.g. `import foo.bar.baz`
    pub fn new(name: Name, modules: OrderedSet<ModuleName>) -> Module {
        assert!(
            modules.iter().all(|x| x.first_component() == name),
            "{name} {modules:?}"
        );
        Self {
            path: Box::new([name]),
            modules: Arc::new(modules),
        }
    }

    /// Created from an alias, e.g. `import foo.bar.baz as bar`
    pub fn new_as(name: ModuleName) -> Module {
        Self {
            path: name.as_str().split('.').map(Name::new).collect(),
            modules: Arc::new(OrderedSet::from_iter([name])),
        }
    }

    pub fn to_type(self) -> Type {
        Type::Module(self)
    }

    pub fn push_path(&self, component: Name) -> Self {
        let mut path = Vec::with_capacity(self.path.len() + 1);
        path.extend(self.path.iter().cloned());
        path.push(component);
        Module {
            path: path.into_boxed_slice(),
            modules: self.modules.dupe(),
        }
    }

    pub fn path(&self) -> &[Name] {
        &self.path
    }

    pub fn add_module(&self, m: ModuleName) -> Self {
        let mut modules = (*self.modules).clone();
        modules.insert(m);
        Self {
            path: self.path.clone(),
            modules: Arc::new(modules),
        }
    }

    pub fn merge(&mut self, m: &Module) {
        assert_eq!(self.path, m.path);
        let mut modules = (*self.modules).clone();
        modules.extend(m.modules.iter().copied());
        self.modules = Arc::new(modules);
    }

    pub fn is_submodules_imported_directly(&self) -> bool {
        let prefix = self.path();
        self.modules
            .iter()
            .any(|name| name.components().starts_with(prefix))
    }
}
