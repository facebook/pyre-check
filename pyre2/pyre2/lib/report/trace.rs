/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::Expr;
use ruff_text_size::Ranged;
use serde::Serialize;
use starlark_map::small_map::SmallMap;

use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::state::State;
use crate::util::visit::Visit;

#[derive(Serialize)]
struct Output {
    modules: Vec<ModuleOutput>,
}

#[derive(Serialize)]
struct ModuleOutput {
    name: ModuleName,
    path: ModulePath,
    types: SmallMap<String, String>,
    definitions: SmallMap<String, (String, String)>,
}

fn trace_module(state: &State, handle: &Handle) -> Option<ModuleOutput> {
    let info = state.get_module_info(handle)?;
    let ast = state.get_ast(handle)?;

    let mut types = SmallMap::new();
    let mut definitions = SmallMap::new();
    ast.visit(&mut |x| {
        let loc = match x {
            Expr::Name(x) => x.range,
            Expr::Attribute(x) => x.attr.range,
            _ => return,
        };
        if let Some(ty) = state.hover(handle, loc.start()) {
            types.insert(info.source_range(x.range()).to_string(), ty.to_string());
        }
        if let Some(def) = state.goto_definition(handle, loc.start()) {
            definitions.insert(
                info.source_range(x.range()).to_string(),
                (
                    def.module_info.path().to_string(),
                    def.module_info.source_range(def.range).to_string(),
                ),
            );
        }
    });
    Some(ModuleOutput {
        name: handle.module(),
        path: handle.path().dupe(),
        types,
        definitions,
    })
}

/// Report on how many there are of each binding, and how much memory they take up, per module.
pub fn trace(state: &State) -> String {
    let mut modules = Vec::new();
    for h in state.handles() {
        if let Some(module) = trace_module(state, &h) {
            modules.push(module);
        }
    }
    let output = Output { modules };
    serde_json::to_string_pretty(&output).unwrap()
}
