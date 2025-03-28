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

use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::bindings::Bindings;
use crate::binding::table::TableKeyed;
use crate::module::module_name::ModuleName;
use crate::state::state::ReadableState;
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
        if let Some(mut i) = ctor.find(['{', '(']) {
            if ctor.as_bytes().get(i - 1) == Some(&b' ') {
                i -= 1;
            }
            ctor.truncate(i);
        }
        let mut type_name = any::type_name_of_val(v);
        if let Some((_, x)) = type_name.rsplit_once(':') {
            type_name = x;
        }
        Self {
            module,
            type_name,
            ctor,
            size: mem::size_of_val(v),
        }
    }
}

/// Report on how many there are of each binding, and how much memory they take up, per module.
pub fn binding_memory(state: &ReadableState) -> String {
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
    for handle in state.handles() {
        let bindings = state.get_bindings(&handle).unwrap();
        table_for_each!(&phantom_table, |v| f(
            v,
            handle.module(),
            &bindings,
            &mut report
        ));
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

#[cfg(test)]
mod tests {
    use ruff_python_ast::Identifier;
    use ruff_text_size::TextRange;

    use super::*;
    use crate::binding::binding::BindingClass;
    use crate::binding::binding::BindingClassMetadata;
    use crate::binding::binding::Key;
    use crate::graph::index::Idx;
    use crate::module::short_identifier::ShortIdentifier;
    use crate::types::class::ClassIndex;

    #[test]
    fn test_binding_memory() {
        let module = ModuleName::from_str("my_module");

        let v = Key::Usage(ShortIdentifier::new(&Identifier::new(
            "my_usage",
            TextRange::default(),
        )));
        assert_eq!(
            ReportKey::new(module, &v),
            ReportKey {
                module,
                type_name: "Key",
                ctor: "Usage".to_owned(),
                size: mem::size_of_val(&v),
            }
        );

        let v = BindingClass::FunctionalClassDef(
            ClassIndex(0),
            Identifier::new("my_class", TextRange::default()),
            SmallMap::new(),
        );
        assert_eq!(
            ReportKey::new(module, &v),
            ReportKey {
                module,
                type_name: "BindingClass",
                ctor: "FunctionalClassDef".to_owned(),
                size: mem::size_of_val(&v),
            }
        );

        let v = BindingClassMetadata {
            def: Idx::new(42),
            bases: Default::default(),
            keywords: Default::default(),
            decorators: Default::default(),
            is_new_type: false,
            special_base: None,
        };
        assert_eq!(
            ReportKey::new(module, &v),
            ReportKey {
                module,
                type_name: "BindingClassMetadata",
                ctor: "BindingClassMetadata".to_owned(),
                size: mem::size_of_val(&v),
            }
        );
    }
}
