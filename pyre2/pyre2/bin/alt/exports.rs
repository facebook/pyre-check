use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Stmt;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use super::definitions::DunderAllEntry;
use crate::alt::definitions::Definitions;
use crate::config::Config;
use crate::graph::calculation::Calculation;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;

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
                DunderAllEntry::Name(x) => writeln!(f, "export {x}")?,
                DunderAllEntry::Module(x) => writeln!(f, "from {x} import *")?,
                DunderAllEntry::Remove(x) => writeln!(f, "unexport {x}")?,
            }
        }
        Ok(())
    }
}

impl Exports {
    pub fn new(x: &[Stmt], module_info: &ModuleInfo, config: &Config) -> Self {
        let mut definitions =
            Definitions::new(x, module_info.name(), module_info.is_init(), config);
        definitions.ensure_dunder_all(module_info.style());
        Self(Arc::new(ExportsInner {
            definitions,
            wildcard: Calculation::new(),
            exports: Calculation::new(),
        }))
    }

    /// What symbols will I get if I do `from <this_module> import *`?
    pub fn wildcard(&self, modules: &SmallMap<ModuleName, Exports>) -> Arc<SmallSet<Name>> {
        let f = || {
            let mut result = SmallSet::new();
            for x in &self.0.definitions.dunder_all {
                match x {
                    DunderAllEntry::Name(x) => {
                        result.insert(x.clone());
                    }
                    DunderAllEntry::Module(x) => {
                        // They did `__all__.extend(foo.__all__)``, but didn't import `foo`.
                        // Let's just ignore, and there will be an error when we check `foo.__all__`.
                        if let Some(import) = modules.get(x) {
                            result.extend(import.wildcard(modules).iter().cloned());
                        }
                    }
                    DunderAllEntry::Remove(x) => {
                        result.shift_remove(x);
                    }
                }
            }
            Arc::new(result)
        };
        self.0.wildcard.calculate(f).unwrap_or_default()
    }

    fn exports(&self, modules: &SmallMap<ModuleName, Exports>) -> Arc<SmallSet<Name>> {
        let f = || {
            let mut result = SmallSet::new();
            result.extend(self.0.definitions.definitions.keys().cloned());
            for x in self.0.definitions.import_all.keys() {
                result.extend(modules.get(x).unwrap().wildcard(modules).iter().cloned());
            }
            Arc::new(result)
        };
        self.0.exports.calculate(f).unwrap_or_default()
    }

    pub fn contains(&self, name: &Name, imports: &SmallMap<ModuleName, Exports>) -> bool {
        self.exports(imports).contains(name)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use starlark_map::smallmap;

    use super::*;
    use crate::ast::Ast;
    use crate::module::module_info::ModuleStyle;

    fn mk_exports(contents: &str, style: ModuleStyle) -> Exports {
        let ast = Ast::parse(contents).0;
        let path = PathBuf::from(if style == ModuleStyle::Interface {
            "foo.pyi"
        } else {
            "foo.py"
        });
        let module_info = ModuleInfo::new(
            ModuleName::from_str("foo"),
            path,
            contents.to_owned(),
            false,
        );
        Exports::new(&ast.body, &module_info, &Config::default())
    }

    fn check(exports: &Exports, imports: &SmallMap<ModuleName, Exports>, all: &[&str]) {
        assert_eq!(
            exports
                .wildcard(imports)
                .iter()
                .map(|x| x.as_str())
                .collect::<Vec<_>>(),
            all
        );
    }

    #[test]
    fn test_exports() {
        let simple = mk_exports("simple_val = 1\n_simple_val = 2", ModuleStyle::Executable);
        check(&simple, &SmallMap::new(), &["simple_val"]);

        let imports = smallmap!(ModuleName::from_str("simple") => simple);
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

        check(
            &executable,
            &imports,
            &["simple_val", "X", "Z", "Q", "baz", "test", "x"],
        );
        check(&interface, &imports, &["Q", "test", "x"]);

        for x in [&executable, &interface] {
            assert!(x.contains(&Name::new("Z"), &imports));
            assert!(x.contains(&Name::new("baz"), &imports));
            assert!(!x.contains(&Name::new("magic"), &imports));
        }
        assert!(executable.contains(&Name::new("simple_val"), &imports));
    }

    #[test]
    fn test_reexport() {
        // `a` is not in the `import *` of `b`, but it can be used as `b.a`
        let a = mk_exports("a = 1", ModuleStyle::Interface);
        let b = mk_exports("from a import *", ModuleStyle::Interface);
        let imports = smallmap!(ModuleName::from_str("a") => a);
        assert!(b.contains(&Name::new("a"), &imports));
        check(&b, &imports, &[]);
    }

    #[test]
    fn test_cyclic() {
        let a = mk_exports("from b import *", ModuleStyle::Interface);
        let b = mk_exports("from a import *\nx = 1", ModuleStyle::Interface);
        let imports = smallmap!(
            ModuleName::from_str("a") => a.dupe(),
            ModuleName::from_str("b") => b.dupe(),
        );
        check(&a, &imports, &[]);
        check(&b, &imports, &["x"]);
        assert!(b.contains(&Name::new("x"), &imports));
        assert!(!b.contains(&Name::new("y"), &imports));
    }

    #[test]
    fn over_export() {
        let a = mk_exports("from b import *", ModuleStyle::Executable);
        let b = mk_exports("from a import magic\n__all__ = []", ModuleStyle::Executable);
        let imports = smallmap!(
            ModuleName::from_str("a") => a.dupe(),
            ModuleName::from_str("b") => b.dupe(),
        );
        check(&a, &imports, &[]);
        check(&b, &imports, &[]);
        assert!(!a.contains(&Name::new("magic"), &imports));
        assert!(b.contains(&Name::new("magic"), &imports));
    }
}
