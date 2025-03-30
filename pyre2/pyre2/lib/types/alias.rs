/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Things defined as `Dict = _Alias()` which should really be defined as `Dict = dict`.
//! But there are a few nuances where it is a bit different, e.g. `Dict()` doesn't create a dictionary.
//! See <https://github.com/python/typeshed/pull/13589> for details.

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;

use crate::module::module_name::ModuleName;

/// Does it match `_Alias()`
fn is_alias_call(x: &Expr) -> bool {
    if let Expr::Call(x) = x
        && x.arguments.is_empty()
        && let Expr::Name(x) = &*x.func
        && x.id == "_Alias"
    {
        return true;
    }
    false
}

/// Must be defined in `typing`.
pub fn resolve_typeshed_alias(
    module: ModuleName,
    lhs: &Name,
    rhs: &Expr,
) -> Option<(ModuleName, Name)> {
    if module != ModuleName::typing() && module != ModuleName::typing_extensions() {
        return None;
    }
    if !is_alias_call(rhs) {
        return None;
    }
    match lhs.as_str() {
        "List" => Some((ModuleName::builtins(), Name::new_static("list"))),
        "Dict" => Some((ModuleName::builtins(), Name::new_static("dict"))),
        "Set" => Some((ModuleName::builtins(), Name::new_static("set"))),
        "FrozenSet" => Some((ModuleName::builtins(), Name::new_static("frozenset"))),
        "DefaultDict" => Some((ModuleName::collections(), Name::new_static("defaultdict"))),
        "Counter" => Some((ModuleName::collections(), Name::new_static("Counter"))),
        "Deque" => Some((ModuleName::collections(), Name::new_static("deque"))),
        "ChainMap" => Some((ModuleName::collections(), Name::new_static("ChainMap"))),
        "OrderedDict" => Some((ModuleName::collections(), Name::new_static("OrderedDict"))),
        _ => None,
    }
}
