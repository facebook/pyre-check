/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::any;
use std::fmt::Debug;
use std::fmt::Write;
use std::marker::PhantomData;
use std::mem;

use starlark_map::small_map::SmallMap;

use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::bindings::Bindings;
use crate::binding::table::TableKeyed;
use crate::module::module_name::ModuleName;
use crate::state::state::State;
use crate::table;
use crate::table_for_each;

table!(
    #[derive(Default)]
    pub struct PhantomTable(PhantomData)
);

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct ReportKey {
    module: ModuleName,
    type_name: &'static str,
    ctor: String,
    size: usize,
}

impl ReportKey {
    fn new<T: Debug>(module: ModuleName, v: &T) -> Self {
        let mut ctor = format!("{v:?}");
        if let Some(i) = ctor.find('(') {
            ctor.truncate(i);
        }
        Self {
            module,
            type_name: any::type_name_of_val(v),
            ctor,
            size: mem::size_of_val(v),
        }
    }
}

/// Report on how many there are of each binding, and how much memory they take up, per module.
pub fn binding_memory(state: &State) -> String {
    #[allow(clippy::trivially_copy_pass_by_ref)] // required to match the macro signature
    fn f<K: Keyed>(
        _: &PhantomData<K>,
        module: ModuleName,
        entry: &Bindings,
        report: &mut SmallMap<ReportKey, usize>,
    ) where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        for idx in entry.keys::<K>() {
            let key = entry.idx_to_key(idx);
            let val = entry.get(idx);
            *report.entry(ReportKey::new(module, key)).or_default() += 1;
            *report.entry(ReportKey::new(module, val)).or_default() += 1;
        }
    }

    let mut report = SmallMap::new();
    let phantom_table = PhantomTable::default();
    for module in state.modules() {
        let bindings = state.get_bindings(module).unwrap();
        table_for_each!(&phantom_table, |v| f(v, module, &bindings, &mut report));
    }

    let mut entries = report.into_iter().collect::<Vec<_>>();
    entries.sort_by_key(|(k, v)| k.size * v);
    entries.reverse();
    let mut res = String::new();
    writeln!(res, "Module,Type,Ctor,Count,Size").unwrap();
    for (k, v) in entries {
        writeln!(
            res,
            "{},{},{},{},{}",
            k.module,
            k.type_name,
            k.ctor,
            v,
            k.size * v
        )
        .unwrap();
    }
    res
}
