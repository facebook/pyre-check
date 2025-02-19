/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExport;
use crate::module::short_identifier::ShortIdentifier;
use crate::state::handle::Handle;
use crate::state::state::State;
use crate::types::types::Type;
use crate::visitors::Visitors;

impl State {
    pub fn get_type(&self, handle: &Handle, key: &Key) -> Option<Arc<Type>> {
        let idx = self.get_bindings(handle)?.key_to_idx(key);
        self.get_answers(handle)?.get_idx(idx)
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

    pub fn hover(&self, handle: &Handle, position: TextSize) -> Option<Arc<Type>> {
        let id = self.identifier_at(handle, position)?;
        self.get_type(handle, &Key::Usage(ShortIdentifier::new(&id)))
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
    ) -> Option<(Handle, TextRange)> {
        let id = self.identifier_at(handle, position)?;
        self.key_to_definition(handle, &Key::Usage(ShortIdentifier::new(&id)), 20)
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
