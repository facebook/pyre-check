/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;

use parse_display::Display;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::config::Config;
use crate::export::definitions::DefinitionStyle;
use crate::export::definitions::Definitions;
use crate::export::exports::LookupExport;
use crate::graph::index::Idx;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;

/// Many names may map to the same TextRange (e.g. from foo import *).
/// But no other static will point at the same TextRange.
#[derive(Default, Clone, Debug)]
pub struct Static(pub SmallMap<Name, StaticInfo>);

#[derive(Clone, Debug)]
pub struct StaticInfo {
    pub loc: TextRange,
    /// How many times this will be redefined
    pub count: usize,
    /// True if this is going to appear as a `Key::Import``.
    /// A little fiddly to keep syncronised with the other field.
    pub uses_key_import: bool,
}

impl Static {
    fn add_with_count(&mut self, name: Name, loc: TextRange, count: usize) -> &mut StaticInfo {
        // Use whichever one we see first
        let res = self.0.entry(name).or_insert(StaticInfo {
            loc,
            count: 0,
            uses_key_import: false,
        });
        res.count += count;
        res
    }

    pub fn add(&mut self, name: Name, range: TextRange) {
        self.add_with_count(name, range, 1);
    }

    pub fn stmts(
        &mut self,
        x: &[Stmt],
        module_info: &ModuleInfo,
        top_level: bool,
        lookup: &dyn LookupExport,
        config: &Config,
    ) {
        let mut d = Definitions::new(x, module_info.name(), module_info.path().is_init(), config);
        if top_level && module_info.name() != ModuleName::builtins() {
            d.inject_builtins();
        }
        for (name, def) in d.definitions {
            self.add_with_count(name, def.range, def.count)
                .uses_key_import = def.style == DefinitionStyle::ImportModule;
        }
        for (m, range) in d.import_all {
            if let Ok(exports) = lookup.get(m) {
                for name in exports.wildcard(lookup).iter() {
                    self.add_with_count(name.clone(), range, 1).uses_key_import = true;
                }
            }
        }
    }

    pub fn expr_lvalue(&mut self, x: &Expr) {
        let mut add = |name: &ExprName| self.add(name.id.clone(), name.range);
        Ast::expr_lvalue(x, &mut add);
    }
}

/// The current value of the name, plus optionally the current value of the annotation.
#[derive(Default, Clone, Debug)]
pub struct Flow {
    pub info: SmallMap<Name, FlowInfo>,
    // Should this flow be merged into the next? Flow merging occurs after constructs like branches and loops.
    pub no_next: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlowStyle {
    /// The annotation associated with this key, if any.
    /// If there is one, all subsequent bindings must obey this annotation.
    /// Also store am I initialized, or am I the result of `x: int`?
    Annotated {
        ann: Idx<KeyAnnotation>,
        is_initialized: bool,
    },
    /// Am I the result of an import (which needs merging).
    /// E.g. `import foo.bar` and `import foo.baz` need merging.
    /// The `ModuleName` will be the most recent entry.
    MergeableImport(ModuleName),
    /// Was I imported from somewhere (and if so, where)
    /// E.g. `from foo import bar` would get `foo` here.
    Import(ModuleName),
    /// Am I an alias for a module import, `import foo.bar as baz`
    /// would get `foo.bar` here.
    ImportAs(ModuleName),
}

impl FlowStyle {
    pub fn ann(&self) -> Option<Idx<KeyAnnotation>> {
        match self {
            FlowStyle::Annotated { ann, .. } => Some(*ann),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FlowInfo {
    pub key: Idx<Key>,
    pub style: Option<FlowStyle>,
}

impl FlowInfo {
    fn new(key: Idx<Key>, style: Option<FlowStyle>) -> Self {
        Self { key, style }
    }

    pub fn new_with_ann(key: Idx<Key>, ann: Option<Idx<KeyAnnotation>>) -> Self {
        Self::new(
            key,
            ann.map(|x| FlowStyle::Annotated {
                ann: x,
                is_initialized: true,
            }),
        )
    }

    pub fn ann(&self) -> Option<Idx<KeyAnnotation>> {
        self.style.as_ref()?.ann()
    }

    pub fn is_initialized(&self) -> bool {
        match self.style.as_ref() {
            Some(FlowStyle::Annotated { is_initialized, .. }) => *is_initialized,
            _ => true,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ClassBodyInner {
    name: Identifier,
    pub instance_attributes_by_method: SmallMap<Name, SmallMap<Name, InstanceAttribute>>,
}

impl ClassBodyInner {
    pub fn as_self_type_key(&self) -> Key {
        Key::SelfType(ShortIdentifier::new(&self.name))
    }
}

/// Information about an instance attribute coming from a `self` assignment
/// in a method.
#[derive(Clone, Debug)]
pub struct InstanceAttribute(pub Binding, pub Option<Idx<KeyAnnotation>>, pub TextRange);

#[derive(Clone, Debug)]
pub struct MethodInner {
    pub name: Identifier,
    pub self_name: Option<Identifier>,
    pub instance_attributes: SmallMap<Name, InstanceAttribute>,
}

#[derive(Clone, Debug)]
pub enum ScopeKind {
    Annotation,
    ClassBody(ClassBodyInner),
    Comprehension,
    Function,
    Method(MethodInner),
    Module,
}

#[derive(Clone, Debug, Display)]
pub enum LoopExit {
    NeverRan,
    #[display("break")]
    Break,
    #[display("continue")]
    Continue,
}

/// Flow snapshots for all possible exitpoints from a loop.
#[derive(Clone, Debug)]
pub struct Loop(pub Vec<(LoopExit, Flow)>);

#[derive(Clone, Debug)]
pub struct Scope {
    pub stat: Static,
    pub flow: Flow,
    /// Are Flow types above this unreachable.
    /// Set when we enter something like a function, and can't guarantee what flow values are in scope.
    pub barrier: bool,
    pub kind: ScopeKind,
    /// Stack of for/while loops we're in. Does not include comprehensions.
    pub loops: Vec<Loop>,
}

impl Scope {
    fn new(barrier: bool, kind: ScopeKind) -> Self {
        Self {
            stat: Default::default(),
            flow: Default::default(),
            barrier,
            kind,
            loops: Default::default(),
        }
    }

    pub fn annotation() -> Self {
        Self::new(false, ScopeKind::Annotation)
    }

    pub fn class_body(name: Identifier) -> Self {
        Self::new(
            false,
            ScopeKind::ClassBody(ClassBodyInner {
                name,
                instance_attributes_by_method: SmallMap::new(),
            }),
        )
    }

    pub fn comprehension() -> Self {
        Self::new(false, ScopeKind::Comprehension)
    }

    pub fn function() -> Self {
        Self::new(true, ScopeKind::Function)
    }

    pub fn method(name: Identifier) -> Self {
        Self::new(
            true,
            ScopeKind::Method(MethodInner {
                name,
                self_name: None,
                instance_attributes: SmallMap::new(),
            }),
        )
    }

    pub fn module() -> Self {
        Self::new(false, ScopeKind::Module)
    }
}
