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
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use vec1::Vec1;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::ClassFieldInitialValue;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyFunction;
use crate::config::Config;
use crate::export::definitions::DefinitionStyle;
use crate::export::definitions::Definitions;
use crate::export::exports::LookupExport;
use crate::export::special::SpecialExport;
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
    /// The location of the first annotated name for this binding, if any.
    pub annot: Option<Idx<KeyAnnotation>>,
    /// How many times this will be redefined
    pub count: usize,
    /// True if this is going to appear as a `Key::Import``.
    /// A little fiddly to keep syncronised with the other field.
    pub uses_key_import: bool,
}

impl Static {
    fn add_with_count(
        &mut self,
        name: Name,
        loc: TextRange,
        annot: Option<Idx<KeyAnnotation>>,
        count: usize,
    ) -> &mut StaticInfo {
        // Use whichever one we see first
        let res = self.0.entry(name).or_insert(StaticInfo {
            loc,
            annot,
            count: 0,
            uses_key_import: false,
        });
        res.count += count;
        res
    }

    pub fn add(&mut self, name: Name, range: TextRange, annot: Option<Idx<KeyAnnotation>>) {
        self.add_with_count(name, range, annot, 1);
    }

    pub fn stmts(
        &mut self,
        x: &[Stmt],
        module_info: &ModuleInfo,
        top_level: bool,
        lookup: &dyn LookupExport,
        config: &Config,
        mut get_annotation_idx: impl FnMut(ShortIdentifier) -> Idx<KeyAnnotation>,
    ) {
        let mut d = Definitions::new(x, module_info.name(), module_info.path().is_init(), config);
        if top_level && module_info.name() != ModuleName::builtins() {
            d.inject_builtins();
        }
        for (name, def) in d.definitions {
            let annot = def.annot.map(&mut get_annotation_idx);
            self.add_with_count(name, def.range, annot, def.count)
                .uses_key_import = def.style == DefinitionStyle::ImportModule;
        }
        for (m, range) in d.import_all {
            if let Ok(exports) = lookup.get(m) {
                for name in exports.wildcard(lookup).iter() {
                    // TODO: semantics of import * and global var with same name
                    self.add_with_count(name.clone(), range, None, 1)
                        .uses_key_import = true;
                }
            }
        }
    }

    pub fn expr_lvalue(&mut self, x: &Expr) {
        let mut add = |name: &ExprName| self.add(name.id.clone(), name.range, None);
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

#[derive(Debug, Clone, PartialEq)]
pub enum FlowStyle {
    /// Am I initialized, or am I the result of `x: int`?
    Annotated { is_initialized: bool },
    /// Am I a type-annotated assignment in a class body?
    AnnotatedClassField { initial_value: Option<Expr> },
    /// Am I the result of an import (which needs merging).
    /// E.g. `import foo.bar` and `import foo.baz` need merging.
    /// The `ModuleName` will be the most recent entry.
    MergeableImport(ModuleName),
    /// Was I imported from somewhere (and if so, where)
    /// E.g. Both `from foo import bar` and
    /// `from foo import bar as baz` would get `(foo, bar)`.
    Import(ModuleName, Name),
    /// Am I an alias for a module import, `import foo.bar as baz`
    /// would get `foo.bar` here.
    ImportAs(ModuleName),
    /// Am I a function definition? Used to chain overload definitions.
    FunctionDef(Idx<KeyFunction>),
}

#[derive(Debug, Clone)]
pub struct FlowInfo {
    pub key: Idx<Key>,
    pub style: Option<FlowStyle>,
}

impl FlowInfo {
    pub fn as_initial_value(&self) -> ClassFieldInitialValue {
        match self.style.as_ref() {
            Some(FlowStyle::AnnotatedClassField {
                initial_value: Some(e),
            }) => ClassFieldInitialValue::Class(Some(e.clone())),
            Some(FlowStyle::AnnotatedClassField {
                initial_value: None,
            }) => ClassFieldInitialValue::Instance,
            // All other styles (e.g. function def, import) indicate we do have
            // a value, but it is not coming from a simple style.
            _ => ClassFieldInitialValue::Class(None),
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

    pub fn as_class_metadata_key(&self) -> KeyClassMetadata {
        KeyClassMetadata(ShortIdentifier::new(&self.name))
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

    fn module() -> Self {
        Self::new(false, ScopeKind::Module)
    }
}

#[derive(Clone, Debug)]
pub struct Scopes(Vec1<Scope>);

impl Scopes {
    pub fn module() -> Self {
        Self(Vec1::new(Scope::module()))
    }

    pub fn current(&self) -> &Scope {
        self.0.last()
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.0.last_mut()
    }

    /// There is only one scope remaining, return it.
    pub fn finish(self) -> Scope {
        assert_eq!(self.0.len(), 1);
        self.0.to_vec().pop().unwrap()
    }

    pub fn push(&mut self, scope: Scope) {
        self.0.push(scope);
    }

    pub fn pop(&mut self) -> Scope {
        self.0.pop().unwrap()
    }

    pub fn iter_rev(&self) -> impl Iterator<Item = &Scope> {
        self.0.iter().rev()
    }

    pub fn iter_rev_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.0.iter_mut().rev()
    }

    pub fn update_flow_info(&mut self, name: &Name, key: Idx<Key>, style: Option<FlowStyle>) {
        self.current_mut()
            .flow
            .info
            .insert(name.clone(), FlowInfo { key, style });
    }

    fn get_flow_info(&self, name: &Name) -> Option<&FlowInfo> {
        for scope in self.iter_rev() {
            if let Some(flow) = scope.flow.info.get(name) {
                return Some(flow);
            }
        }
        None
    }

    pub fn as_special_export(&self, e: &Expr, current_module: ModuleName) -> Option<SpecialExport> {
        // Only works for things with `Foo`, or `source.Foo`, or `F` where `from module import Foo as F`.
        // Does not work for things with nested modules - but no SpecialExport's have that.
        match e {
            Expr::Name(name) => {
                let name = &name.id;
                let flow = self.get_flow_info(name)?;
                match &flow.style {
                    Some(FlowStyle::Import(m, name2)) => {
                        let special = SpecialExport::new(name2)?;
                        if special.defined_in(*m) {
                            Some(special)
                        } else {
                            None
                        }
                    }
                    _ => {
                        let special = SpecialExport::new(name)?;
                        if special.defined_in(current_module) {
                            Some(special)
                        } else {
                            None
                        }
                    }
                }
            }
            Expr::Attribute(ExprAttribute {
                value: box Expr::Name(module),
                attr: name,
                ..
            }) => {
                let special = SpecialExport::new(&name.id)?;
                let flow = self.get_flow_info(&module.id)?;
                match flow.style {
                    Some(FlowStyle::MergeableImport(m)) if special.defined_in(m) => Some(special),
                    Some(FlowStyle::ImportAs(m)) if special.defined_in(m) => Some(special),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
