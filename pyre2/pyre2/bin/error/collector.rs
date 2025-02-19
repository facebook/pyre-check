/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use dupe::Dupe;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use tracing::error;

use crate::error::error::Error;
use crate::error::style::ErrorStyle;
use crate::module::module_info::ModuleInfo;
use crate::util::lock::Mutex;

#[derive(Debug, Default, Clone)]
struct ModuleErrors {
    /// Set to `true` when we have no duplicates and are sorted.
    clean: bool,
    items: Vec<Error>,
}

impl ModuleErrors {
    fn push(&mut self, err: Error) {
        self.clean = false;
        self.items.push(err);
    }

    fn cleanup(&mut self) {
        if self.clean {
            return;
        }
        self.clean = true;
        self.items.sort();
        self.items.dedup();
    }

    fn is_empty(&self) -> bool {
        // No need to do cleanup if it's empty.
        self.items.is_empty()
    }

    fn len(&mut self) -> usize {
        self.cleanup();
        self.items.len()
    }

    fn iter(&mut self) -> impl Iterator<Item = &Error> {
        self.cleanup();
        self.items.iter()
    }
}

/// Collects the user errors (e.g. type errors) associated with a module.
// Deliberately don't implement Clone,
#[derive(Debug)]
pub struct ErrorCollector {
    style: ErrorStyle,
    errors: Mutex<ModuleErrors>,
}

impl Display for ErrorCollector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in self.errors.lock().iter() {
            writeln!(f, "ERROR: {err}")?;
        }
        Ok(())
    }
}

impl ErrorCollector {
    pub fn new(style: ErrorStyle) -> Self {
        Self {
            style,
            errors: Mutex::new(Default::default()),
        }
    }

    pub fn add_error(&self, err: Error) {
        if err.is_ignored() {
            // Should we record these anyway? Not clear.
            return;
        }
        if self.style != ErrorStyle::Never {
            self.errors.lock().push(err);
        }
    }

    pub fn add(&self, module_info: &ModuleInfo, range: TextRange, msg: String) {
        let source_range = module_info.source_range(range);
        let is_ignored = module_info.is_ignored(&source_range, &msg);
        self.add_error(Error::new(
            module_info.path().dupe(),
            source_range,
            msg,
            is_ignored,
        ));
    }

    pub fn style(&self) -> ErrorStyle {
        self.style
    }

    pub fn is_empty(&self) -> bool {
        self.errors.lock().is_empty()
    }

    pub fn len(&self) -> usize {
        self.errors.lock().len()
    }

    pub fn collect(&self) -> Vec<Error> {
        self.errors.lock().iter().cloned().collect()
    }

    pub fn summarise<'a>(xs: impl Iterator<Item = &'a ErrorCollector>) -> Vec<(String, usize)> {
        let mut map = SmallMap::new();
        for x in xs {
            for err in x.errors.lock().iter() {
                // Lots of error messages have names in them, e.g. "Can't find module `foo`".
                // We want to summarise those together, so replace bits of text inside `...` with `...`.
                let clean_msg = err
                    .msg()
                    .split('`')
                    .enumerate()
                    .map(|(i, x)| if i % 2 == 0 { x } else { "..." })
                    .collect::<Vec<_>>()
                    .join("`");
                *map.entry(clean_msg).or_default() += 1;
            }
        }
        let mut res = map.into_iter().collect::<Vec<_>>();
        res.sort_by_key(|x| x.1);
        res
    }

    pub fn todo(&self, module_info: &ModuleInfo, msg: &str, v: impl Ranged + Debug) {
        let s = format!("{v:?}");
        if s == format!("{:?}", v.range()) {
            // The v is just a range, so don't add the constructor
            self.add(module_info, v.range(), format!("TODO: {msg}"));
        } else {
            let prefix = s.split_once(' ').map_or(s.as_str(), |x| x.0);
            self.add(module_info, v.range(), format!("TODO: {prefix} - {msg}"));
        }
    }

    pub fn print(&self) {
        for err in self.errors.lock().iter() {
            error!("{err}");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::sync::Arc;

    use ruff_python_ast::name::Name;
    use ruff_text_size::TextSize;

    use super::*;
    use crate::module::module_name::ModuleName;
    use crate::module::module_path::ModulePath;
    use crate::util::prelude::SliceExt;

    #[test]
    fn test_error_collector() {
        let errors = ErrorCollector::new(ErrorStyle::Delayed);
        let mi = ModuleInfo::new(
            ModuleName::from_name(&Name::new("main")),
            ModulePath::filesystem(Path::new("main.py").to_owned()),
            Arc::new("contents".to_owned()),
        );
        errors.add(
            &mi,
            TextRange::new(TextSize::new(1), TextSize::new(3)),
            "b".to_owned(),
        );
        errors.add(
            &mi,
            TextRange::new(TextSize::new(1), TextSize::new(3)),
            "a".to_owned(),
        );
        errors.add(
            &mi,
            TextRange::new(TextSize::new(1), TextSize::new(3)),
            "a".to_owned(),
        );
        errors.add(
            &mi,
            TextRange::new(TextSize::new(2), TextSize::new(3)),
            "a".to_owned(),
        );
        errors.add(
            &mi,
            TextRange::new(TextSize::new(1), TextSize::new(3)),
            "b".to_owned(),
        );
        assert_eq!(errors.collect().map(|x| x.msg()), vec!["a", "b", "a"]);
    }
}
