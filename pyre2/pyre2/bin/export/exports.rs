/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Stmt;
use starlark_map::small_set::SmallSet;

use crate::config::Config;
use crate::export::definitions::Definitions;
use crate::export::definitions::DunderAllEntry;
use crate::graph::calculation::Calculation;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;

pub trait LookupExport {
    fn get(&self, module: ModuleName) -> Result<Exports, Arc<String>>;
}

#[derive(Debug, Default, Clone, Dupe)]
pub struct Exports(Arc<ExportsInner>);

#[derive(Debug, Default)]
struct ExportsInner {
    /// The underlying definitions.
    definitions: Definitions,
    /// Names that are available via `from <this_module> import *`
    wildcard: Calculation<Arc<SmallSet<Name>>>,
    /// Names that are available via `from <this_module> import <name>`.
    exports: Calculation<Arc<SmallSet<Name>>>,
}

impl Display for Exports {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for x in self.0.definitions.dunder_all.iter() {
            match x {
                DunderAllEntry::Name(_, x) => writeln!(f, "export {x}")?,
                DunderAllEntry::Module(_, x) => writeln!(f, "from {x} import *")?,
                DunderAllEntry::Remove(_, x) => writeln!(f, "unexport {x}")?,
            }
        }
        Ok(())
    }
}

impl Exports {
    pub fn new(x: &[Stmt], module_info: &ModuleInfo, config: &Config) -> Self {
        let mut definitions =
            Definitions::new(x, module_info.name(), module_info.path().is_init(), config);
        definitions.ensure_dunder_all(module_info.path().style());
        Self(Arc::new(ExportsInner {
            definitions,
            wildcard: Calculation::new(),
            exports: Calculation::new(),
        }))
    }

    /// What symbols will I get if I do `from <this_module> import *`?
    pub fn wildcard(&self, lookup: &dyn LookupExport) -> Arc<SmallSet<Name>> {
        let f = || {
            let mut result = SmallSet::new();
            for x in &self.0.definitions.dunder_all {
                match x {
                    DunderAllEntry::Name(_, x) => {
                        result.insert(x.clone());
                    }
                    DunderAllEntry::Module(_, x) => {
                        // They did `__all__.extend(foo.__all__)``, but didn't import `foo`.
                        if let Ok(import) = lookup.get(*x) {
                            result.extend(import.wildcard(lookup).iter().cloned());
                        }
                    }
                    DunderAllEntry::Remove(_, x) => {
                        result.shift_remove(x);
                    }
                }
            }
            Arc::new(result)
        };
        self.0.wildcard.calculate(f).unwrap_or_default()
    }

    fn exports(&self, lookup: &dyn LookupExport) -> Arc<SmallSet<Name>> {
        let f = || {
            let mut result = SmallSet::new();
            result.extend(self.0.definitions.definitions.keys().cloned());
            for x in self.0.definitions.import_all.keys() {
                if let Ok(exports) = lookup.get(*x) {
                    result.extend(exports.wildcard(lookup).iter().cloned());
                }
            }
            Arc::new(result)
        };
        self.0.exports.calculate(f).unwrap_or_default()
    }

    pub fn contains(&self, name: &Name, lookup: &dyn LookupExport) -> bool {
        self.exports(lookup).contains(name)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use starlark_map::small_map::SmallMap;
    use starlark_map::smallmap;

    use super::*;
    use crate::ast::Ast;
    use crate::module::module_path::ModulePath;
    use crate::module::module_path::ModuleStyle;

    impl LookupExport for SmallMap<ModuleName, Exports> {
        fn get(&self, module: ModuleName) -> Result<Exports, Arc<String>> {
            match self.get(&module) {
                Some(x) => Ok(x.dupe()),
                None => Err(Arc::new("Error".to_owned())),
            }
        }
    }

    fn mk_exports(contents: &str, style: ModuleStyle) -> Exports {
        let ast = Ast::parse(contents).0;
        let path = ModulePath::filesystem(PathBuf::from(if style == ModuleStyle::Interface {
            "foo.pyi"
        } else {
            "foo.py"
        }));
        let module_info = ModuleInfo::new(
            ModuleName::from_str("foo"),
            path,
            Arc::new(contents.to_owned()),
        );
        Exports::new(&ast.body, &module_info, &Config::default())
    }

    fn eq_wildcards(exports: &Exports, lookup: &dyn LookupExport, all: &[&str]) {
        assert_eq!(
            exports
                .wildcard(lookup)
                .iter()
                .map(|x| x.as_str())
                .collect::<Vec<_>>(),
            all
        );
    }

    #[must_use]
    fn contains(exports: &Exports, lookup: &dyn LookupExport, name: &str) -> bool {
        exports.contains(&Name::new(name), lookup)
    }

    #[test]
    fn test_exports() {
        let simple = mk_exports("simple_val = 1\n_simple_val = 2", ModuleStyle::Executable);
        eq_wildcards(&simple, &SmallMap::new(), &["simple_val"]);

        let imports = smallmap! {ModuleName::from_str("simple") => simple};
        let contents = r#"
from simple import *
from bar import X, Y as Z, Q as Q
import baz
import test as test

x = 1
_x = 2
"#;

        let executable = mk_exports(contents, ModuleStyle::Executable);
        let interface = mk_exports(contents, ModuleStyle::Interface);

        eq_wildcards(
            &executable,
            &imports,
            &["simple_val", "X", "Z", "Q", "baz", "test", "x"],
        );
        eq_wildcards(&interface, &imports, &["Q", "test", "x"]);

        for x in [&executable, &interface] {
            assert!(contains(x, &imports, "Z"));
            assert!(contains(x, &imports, "baz"));
            assert!(!contains(x, &imports, "magic"));
        }
        assert!(contains(&executable, &imports, "simple_val"));
    }

    #[test]
    fn test_reexport() {
        // `a` is not in the `import *` of `b`, but it can be used as `b.a`
        let a = mk_exports("a = 1", ModuleStyle::Interface);
        let b = mk_exports("from a import *", ModuleStyle::Interface);
        let imports = smallmap! {ModuleName::from_str("a") => a};
        assert!(contains(&b, &imports, "a"));
        eq_wildcards(&b, &imports, &[]);
    }

    #[test]
    fn test_cyclic() {
        let a = mk_exports("from b import *", ModuleStyle::Interface);
        let b = mk_exports("from a import *\nx = 1", ModuleStyle::Interface);
        let imports = smallmap! {
                ModuleName::from_str("a") => a.dupe(),
                ModuleName::from_str("b") => b.dupe(),
        };
        eq_wildcards(&a, &imports, &[]);
        eq_wildcards(&b, &imports, &["x"]);
        assert!(contains(&b, &imports, "x"));
        assert!(!contains(&b, &imports, "y"));
    }

    #[test]
    fn over_export() {
        let a = mk_exports("from b import *", ModuleStyle::Executable);
        let b = mk_exports("from a import magic\n__all__ = []", ModuleStyle::Executable);
        let imports = smallmap! {
                ModuleName::from_str("a") => a.dupe(),
                ModuleName::from_str("b") => b.dupe(),
        };
        eq_wildcards(&a, &imports, &[]);
        eq_wildcards(&b, &imports, &[]);
        assert!(!contains(&a, &imports, "magic"));
        assert!(contains(&b, &imports, "magic"));
    }
}
