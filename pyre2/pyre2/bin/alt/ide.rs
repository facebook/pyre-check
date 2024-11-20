use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::alt::binding::Binding;
use crate::alt::binding::Key;
use crate::alt::driver::Driver;
use crate::ast::Ast;
use crate::module::module_name::ModuleName;
use crate::types::types::Type;
use crate::visitors::Visitors;

impl Driver {
    fn identifier_at(&self, module: ModuleName, position: TextSize) -> Option<Identifier> {
        let mod_module = self.get_mod_module(module)?;
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
        Visitors::visit_mod_expr(mod_module, |x| f(x, position, &mut res));
        res
    }

    pub fn hover(&self, module: ModuleName, position: TextSize) -> Option<Type> {
        let id = self.identifier_at(module, position)?;
        self.get_type_for_idx(
            module,
            self.get_bindings(module)?.key_to_idx(&Key::Usage(id)),
        )
        .cloned()
    }

    fn key_to_definition(
        &self,
        module: ModuleName,
        key: &Key,
        gas: isize,
    ) -> Option<(ModuleName, TextRange)> {
        eprintln!("Definition {gas} = {key:?}");
        if gas <= 0 {
            return None;
        }
        if let Key::Definition(x) = key {
            return Some((module, x.range));
        }
        let bindings = self.get_bindings(module)?;
        let idx = bindings.key_to_idx(key);
        let res = match bindings.get(idx) {
            Binding::Forward(k) => self.key_to_definition(module, k, gas - 1),
            Binding::Phi(ks) if !ks.is_empty() => {
                self.key_to_definition(module, ks.iter().next().unwrap(), gas - 1)
            }
            Binding::Import(m, name) => {
                self.key_to_definition(*m, &Key::Export(name.clone()), gas - 1)
            }
            Binding::Module(name, _, _) => Some((*name, TextRange::default())),
            Binding::CheckLegacyTypeParam(k, _) => {
                let binding = bindings.get(bindings.key_to_idx(k));
                self.key_to_definition(module, &binding.0, gas - 1)
            }
            b => {
                eprintln!("No definition for {key:?} => {b:?}");
                None
            }
        };
        if res.is_none()
            && let Key::Anywhere(_, range) = key
        {
            return Some((module, *range));
        }
        res
    }

    pub fn goto_definition(
        &self,
        module: ModuleName,
        position: TextSize,
    ) -> Option<(ModuleName, TextRange)> {
        let id = self.identifier_at(module, position)?;
        self.key_to_definition(module, &Key::Usage(id), 20)
    }

    pub fn inlay_hints(&self, module: ModuleName) -> Option<Vec<(TextSize, String)>> {
        let is_interesting_type = |x: &Type| x != &Type::any_error();
        let is_interesting_expr = |x: &Expr| !Ast::is_literal(x);

        let bindings = self.get_bindings(module)?;
        let mut res = Vec::new();
        for idx in bindings.keys::<Key>() {
            match bindings.idx_to_key(idx) {
                Key::ReturnType(id) => {
                    match bindings.get(bindings.key_to_idx(&Key::Definition(id.clone()))) {
                        Binding::Function(x, _)
                            if !matches!(bindings.get(idx), &Binding::AnnotatedType(..)) =>
                        {
                            if let Some(ty) = self.get_type_for_idx(module, idx)
                                && is_interesting_type(ty)
                            {
                                res.push((x.parameters.range.end(), format!(" -> {ty}")));
                            }
                        }
                        _ => {}
                    }
                }
                Key::Anywhere(name, range) => {
                    let key = Key::Definition(Identifier {
                        id: name.clone(),
                        range: *range,
                    });
                    if bindings.contains_key(&key)
                        && let idx = bindings.key_to_idx(&key)
                        && let Some(ty) = self.get_type_for_idx(module, idx)
                    {
                        let idx_binding = match bindings.get(idx) {
                            Binding::NameAssign(_, _, b, _) => b,
                            b => b,
                        };
                        if let Binding::Expr(None, e) = idx_binding
                            && is_interesting_expr(e)
                            && is_interesting_type(ty)
                        {
                            let ty = format!(": {}", ty);
                            res.push((range.end(), ty));
                        }
                    }
                }
                _ => {}
            }
        }
        Some(res)
    }
}
