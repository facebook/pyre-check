use std::env::current_dir;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use tracing::info;

use crate::module::module_name::ModuleName;

pub fn default_include() -> anyhow::Result<Vec<PathBuf>> {
    // run the command hg root and get the output
    let result = Command::new("hg").arg("root").output()?;
    let root = String::from_utf8(result.stdout)?;
    let root = root.trim();
    let stdlib = PathBuf::from(format!(
        "{root}/fbcode/tools/pyre/stubs/typeshed/typeshed/stdlib"
    ));
    let stdlib = pathdiff::diff_paths(&stdlib, current_dir()?).unwrap_or(stdlib);
    Ok(vec![stdlib])
}

pub fn module_from_path(path: &Path) -> ModuleName {
    ModuleName::from_str(path.file_stem().unwrap().to_str().unwrap())
}

pub fn find_module(name: ModuleName, include: &[PathBuf]) -> anyhow::Result<PathBuf> {
    let parts = name.components();
    let possibilities = vec![
        parts.join("/") + ".pyi",
        parts.join("/") + ".py",
        parts.join("/") + "/__init__.pyi",
        parts.join("/") + "/__init__.py",
    ];

    for include in include {
        for suffix in &possibilities {
            let path = include.join(suffix);
            if path.exists() {
                info!("Found {name} at {}", path.display());
                return Ok(path);
            }
        }
    }
    Err(anyhow::anyhow!("Couldn't find path for `{name}`"))
}
