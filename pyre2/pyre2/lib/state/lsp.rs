/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExport;
use crate::module::module_info::TextRangeWithModuleInfo;
use crate::module::short_identifier::ShortIdentifier;
use crate::state::handle::Handle;
use crate::state::state::State;
use crate::types::types::Type;
use crate::util::prelude::VecExt;
use crate::visitors::Visitors;

impl State {
    pub fn get_type(&self, handle: &Handle, key: &Key) -> Option<Arc<Type>> {
        let idx = self.get_bindings(handle)?.key_to_idx(key);
        self.get_answers(handle)?.get_idx(idx)
    }

    fn get_type_from_trace(&self, handle: &Handle, range: TextRange) -> Option<Arc<Type>> {
        self.get_answers(handle)?.get_type_trace(range)
    }

    fn identifier_at(&self, handle: &Handle, position: TextSize) -> Option<Identifier> {
        let mod_module = self.get_ast(handle)?;
        fn f(x: &Expr, find: TextSize, res: &mut Option<Identifier>) {
            if let Expr::Name(x) = x
                && x.range.contains_inclusive(find)
            {
                *res = Some(Ast::expr_name_identifier(x.clone()));
            } else {
                Visitors::visit_expr(x, |x| f(x, find, res));
            }
        }
        let mut res = None;
        Visitors::visit_mod_expr(&mod_module, |x| f(x, position, &mut res));
        res
    }

    fn attribute_at(&self, handle: &Handle, position: TextSize) -> Option<ExprAttribute> {
        let mod_module = self.get_ast(handle)?;
        fn f(x: &Expr, find: TextSize, res: &mut Option<ExprAttribute>) {
            if let Expr::Attribute(x) = x
                && x.attr.range.contains_inclusive(find)
            {
                *res = Some(x.clone());
            } else {
                Visitors::visit_expr(x, |x| f(x, find, res));
            }
        }
        let mut res = None;
        Visitors::visit_mod_expr(&mod_module, |x| f(x, position, &mut res));
        res
    }

    pub fn hover(&self, handle: &Handle, position: TextSize) -> Option<Arc<Type>> {
        if let Some(id) = self.identifier_at(handle, position) {
            return self.get_type(handle, &Key::Usage(ShortIdentifier::new(&id)));
        }
        let attribute = self.attribute_at(handle, position)?;
        self.get_type_from_trace(handle, attribute.range)
    }

    fn key_to_definition(
        &self,
        handle: &Handle,
        key: &Key,
        gas: isize,
    ) -> Option<(Handle, TextRange)> {
        if let Key::Definition(x) = key {
            return Some((handle.dupe(), x.range()));
        }
        let bindings = self.get_bindings(handle)?;
        let idx = bindings.key_to_idx(key);
        let res = self.binding_to_definition(handle, bindings.get(idx), gas);
        if res.is_none()
            && let Key::Anywhere(_, range) = key
        {
            return Some((handle.dupe(), *range));
        }
        res
    }

    fn binding_to_definition(
        &self,
        handle: &Handle,
        binding: &Binding,
        gas: isize,
    ) -> Option<(Handle, TextRange)> {
        if gas <= 0 {
            return None;
        }
        let bindings = self.get_bindings(handle)?;
        match binding {
            Binding::Forward(k) => self.key_to_definition(handle, bindings.idx_to_key(*k), gas - 1),
            Binding::Phi(ks) if !ks.is_empty() => self.key_to_definition(
                handle,
                bindings.idx_to_key(*ks.iter().next().unwrap()),
                gas - 1,
            ),
            Binding::Import(m, name) => {
                let handle = self.import_handle(handle, *m).ok()?;
                let bindings = self.get_bindings(&handle)?;
                let b = bindings.get(bindings.key_to_idx(&KeyExport(name.clone())));
                self.binding_to_definition(&handle, b, gas - 1)
            }
            Binding::Module(name, _, _) => Some((
                self.import_handle(handle, *name).ok()?,
                TextRange::default(),
            )),
            Binding::CheckLegacyTypeParam(k, _) => {
                let binding = bindings.get(*k);
                self.key_to_definition(handle, bindings.idx_to_key(binding.0), gas - 1)
            }
            _ => None,
        }
    }

    pub fn goto_definition(
        &self,
        handle: &Handle,
        position: TextSize,
    ) -> Option<TextRangeWithModuleInfo> {
        if let Some(id) = self.identifier_at(handle, position) {
            let (handle, range) =
                self.key_to_definition(handle, &Key::Usage(ShortIdentifier::new(&id)), 20)?;
            return Some(TextRangeWithModuleInfo::new(
                self.get_module_info(&handle)?,
                range,
            ));
        }
        let attribute = self.attribute_at(handle, position)?;
        let base_type = self
            .get_answers(handle)?
            .get_type_trace(attribute.value.range())?;
        self.ad_hoc_solve(handle, |solver| {
            let mut def_opt = None;
            solver.distribute_over_union(&base_type, |obj| {
                if def_opt.is_none() {
                    def_opt = solver.lookup_attr_def_range(obj.clone(), attribute.attr.id());
                }
                obj.clone()
            });
            def_opt
        })
        .flatten()
    }

    pub fn completion(&self, handle: &Handle, position: TextSize) -> Vec<Name> {
        self.completion_opt(handle, position).unwrap_or_default()
    }

    fn completion_opt(&self, handle: &Handle, position: TextSize) -> Option<Vec<Name>> {
        if self.identifier_at(handle, position).is_some() {
            let bindings = self.get_bindings(handle)?;
            let module_info = self.get_module_info(handle)?;
            let names = bindings
                .available_definitions(position)
                .into_iter()
                .filter_map(|idx| {
                    if let Key::Definition(id) = bindings.idx_to_key(idx) {
                        Some(Name::new(module_info.code_at(id.range())))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            return Some(names);
        }
        let attribute = self.attribute_at(handle, position)?;
        let base_type = self
            .get_answers(handle)?
            .get_type_trace(attribute.value.range())?;
        self.ad_hoc_solve(handle, |solver| {
            solver
                .completions(base_type.arc_clone())
                .into_map(|x| x.name)
        })
    }

    pub fn inlay_hints(&self, handle: &Handle) -> Option<Vec<(TextSize, String)>> {
        let is_interesting_type = |x: &Type| x != &Type::any_error();
        let is_interesting_expr = |x: &Expr| !Ast::is_literal(x);

        let bindings = self.get_bindings(handle)?;
        let mut res = Vec::new();
        for idx in bindings.keys::<Key>() {
            match bindings.idx_to_key(idx) {
                key @ Key::ReturnType(id) => {
                    match bindings.get(bindings.key_to_idx(&Key::Definition(id.clone()))) {
                        Binding::Function(x, _pred, _class_meta)
                            if !matches!(bindings.get(idx), &Binding::AnnotatedType(..)) =>
                        {
                            if let Some(ty) = self.get_type(handle, key)
                                && is_interesting_type(&ty)
                            {
                                let fun = bindings.get(*x);
                                res.push((fun.def.parameters.range.end(), format!(" -> {ty}")));
                            }
                        }
                        _ => {}
                    }
                }
                key @ Key::Definition(_) if let Some(ty) = self.get_type(handle, key) => {
                    let e = match bindings.get(idx) {
                        Binding::NameAssign(_, None, e) => Some(&**e),
                        Binding::Expr(None, e) => Some(e),
                        _ => None,
                    };
                    if let Some(e) = e
                        && is_interesting_expr(e)
                        && is_interesting_type(&ty)
                    {
                        let ty = format!(": {}", ty);
                        res.push((key.range().end(), ty));
                    }
                }
                _ => {}
            }
        }
        Some(res)
    }
}
