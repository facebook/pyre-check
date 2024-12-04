/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::any::type_name_of_val;

use serde::Deserialize;
use serde::Serialize;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::error::collector::ErrorCollector;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::table_for_each;
use crate::util::display::DisplayWith;
use crate::util::prelude::SliceExt;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Info {
    modules: SmallMap<ModuleName, Module>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Module {
    bindings: Vec<Binding>,
    errors: Vec<Error>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Binding {
    kind: String,
    key: String,
    location: String,
    binding: String,
    result: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Error {
    location: String,
    message: String,
}

impl Info {
    pub fn new(modules: &[(&ModuleInfo, &ErrorCollector, &Bindings, &Solutions)]) -> Self {
        fn f<K: Keyed>(
            t: &SolutionsEntry<K>,
            module_info: &ModuleInfo,
            bindings: &Bindings,
            res: &mut Vec<Binding>,
        ) where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            for (key, val) in t.iter() {
                let idx = bindings.key_to_idx(key);
                res.push(Binding {
                    kind: type_name_of_val(key).rsplit_once(':').unwrap().1.to_owned(),
                    key: module_info.display(key).to_string(),
                    location: module_info.source_range(key.range()).to_string(),
                    binding: bindings.get(idx).display_with(bindings).to_string(),
                    result: val.to_string(),
                })
            }
        }

        Self {
            modules: modules
                .iter()
                .map(|(module_info, errors, bindings, solutions)| {
                    let mut res = Vec::new();
                    table_for_each!(solutions, |t| f(t, module_info, bindings, &mut res));
                    let errors = errors.collect().map(|e| Error {
                        location: e.source_range().to_string(),
                        message: e.msg.clone(),
                    });
                    (
                        module_info.name(),
                        Module {
                            bindings: res,
                            errors,
                        },
                    )
                })
                .collect(),
        }
    }
}
