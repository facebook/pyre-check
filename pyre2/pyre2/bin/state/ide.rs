/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExport;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::state::state::State;
use crate::types::types::Type;
use crate::visitors::Visitors;

impl State {
    pub fn get_type(&self, module: ModuleName, key: &Key) -> Option<Type> {
        self.get_solutions(module)?.types.get(key).cloned()
    }

    fn identifier_at(&self, module: ModuleName, position: TextSize) -> Option<Identifier> {
        let mod_module = self.get_ast(module)?;
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

    pub fn hover(&self, module: ModuleName, position: TextSize) -> Option<Type> {
        let id = self.identifier_at(module, position)?;
        self.get_type(module, &Key::Usage(ShortIdentifier::new(&id)))
    }

    fn key_to_definition(
        &self,
        module: ModuleName,
        key: &Key,
        gas: isize,
    ) -> Option<(ModuleName, TextRange)> {
        if let Key::Definition(x) = key {
            return Some((module, x.range()));
        }
        let bindings = self.get_bindings(module)?;
        let idx = bindings.key_to_idx(key);
        let res = self.binding_to_definition(module, bindings.get(idx), gas);
        if res.is_none()
            && let Key::Anywhere(_, range) = key
        {
            return Some((module, *range));
        }
        res
    }

    fn binding_to_definition(
        &self,
        module: ModuleName,
        binding: &Binding,
        gas: isize,
    ) -> Option<(ModuleName, TextRange)> {
        if gas <= 0 {
            return None;
        }
        let bindings = self.get_bindings(module)?;
        match binding {
            Binding::Forward(k) => self.key_to_definition(module, bindings.idx_to_key(*k), gas - 1),
            Binding::Phi(ks) if !ks.is_empty() => self.key_to_definition(
                module,
                bindings.idx_to_key(*ks.iter().next().unwrap()),
                gas - 1,
            ),
            Binding::Import(m, name) => {
                let bindings = self.get_bindings(*m)?;
                let b = bindings.get(bindings.key_to_idx(&KeyExport(name.clone())));
                self.binding_to_definition(*m, b, gas - 1)
            }
            Binding::Module(name, _, _) => Some((*name, TextRange::default())),
            Binding::CheckLegacyTypeParam(k, _) => {
                let binding = bindings.get(*k);
                self.key_to_definition(module, bindings.idx_to_key(binding.0), gas - 1)
            }
            _ => None,
        }
    }

    pub fn goto_definition(
        &self,
        module: ModuleName,
        position: TextSize,
    ) -> Option<(ModuleName, TextRange)> {
        let id = self.identifier_at(module, position)?;
        self.key_to_definition(module, &Key::Usage(ShortIdentifier::new(&id)), 20)
    }

    pub fn inlay_hints(&self, module: ModuleName) -> Option<Vec<(TextSize, String)>> {
        let is_interesting_type = |x: &Type| x != &Type::any_error();
        let is_interesting_expr = |x: &Expr| !Ast::is_literal(x);

        let bindings = self.get_bindings(module)?;
        let mut res = Vec::new();
        for idx in bindings.keys::<Key>() {
            match bindings.idx_to_key(idx) {
                // TODO: Binding::Function corresponds to the last definition in a sequence of
                // functions. This means we will not find a Key::Definition for other defines in
                // the sequence.
                //
                // This is a minor issue, since overload definitions require annotations. It's
                // possible to have non-overloads, but those defines are immediately shadowed by
                // the last define in the sequence, so the inlay hint is not very interesting.
                key @ Key::ReturnType(id) => {
                    match bindings.get(bindings.key_to_idx(&Key::Definition(id.clone()))) {
                        Binding::Function(x)
                            if !matches!(bindings.get(idx), &Binding::AnnotatedType(..)) =>
                        {
                            if let Some(ty) = self.get_type(module, key)
                                && is_interesting_type(&ty)
                            {
                                res.push((
                                    x.last().def.parameters.range.end(),
                                    format!(" -> {ty}"),
                                ));
                            }
                        }
                        _ => {}
                    }
                }
                key @ Key::Definition(_) if let Some(ty) = self.get_type(module, key) => {
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
