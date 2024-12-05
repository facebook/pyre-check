/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Mutex;

use dupe::Dupe;
use parse_display::Display;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::SmallMap;
use tracing::error;

use crate::error::error::Error;
use crate::module::module_info::ModuleInfo;

/// Wrapped to provide custom Ord/Eq etc.
/// Do not use ModuleInfo in the comparisons.
#[derive(Debug, Clone, Display)]
struct OneError(Error);

impl PartialEq for OneError {
    fn eq(&self, other: &Self) -> bool {
        self.0.range == other.0.range && self.0.msg == other.0.msg
    }
}

impl Eq for OneError {}

impl Hash for OneError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.range.hash(state);
        self.0.msg.hash(state);
    }
}

impl PartialOrd for OneError {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OneError {
    fn cmp(&self, other: &Self) -> Ordering {
        fn f(x: &Error) -> (TextSize, TextSize, &str) {
            (x.range.start(), x.range.end(), &x.msg)
        }
        f(&self.0).cmp(&f(&other.0))
    }
}

#[derive(Debug, Default, Clone)]
struct ModuleErrors {
    /// Set to `true` when we have no duplicates and are sorted.
    clean: bool,
    items: Vec<OneError>,
}

impl ModuleErrors {
    fn push(&mut self, err: Error) {
        self.clean = false;
        self.items.push(OneError(err));
    }

    fn cleanup(&mut self) {
        if self.clean {
            return;
        }
        self.clean = true;
        self.items.sort();
        self.items.dedup();
    }

    fn len(&mut self) -> usize {
        self.cleanup();
        self.items.len()
    }

    fn iter(&mut self) -> impl Iterator<Item = &OneError> {
        self.cleanup();
        self.items.iter()
    }
}

// Deliberately don't implement Clone,
#[derive(Debug, Default)]
pub struct ErrorCollector {
    quiet: bool,
    errors: Mutex<ModuleErrors>,
}

impl Display for ErrorCollector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in self.errors.lock().unwrap().iter() {
            writeln!(f, "ERROR: {err}")?;
        }
        Ok(())
    }
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self {
            quiet: false,
            errors: Mutex::new(Default::default()),
        }
    }

    #[expect(dead_code)] // Gone missing in the meantime
    pub fn new_quiet() -> Self {
        Self {
            quiet: true,
            errors: Mutex::new(Default::default()),
        }
    }

    pub fn add_error(&self, err: Error) {
        if !self.quiet {
            error!("{err}");
        }
        if err.is_ignored() {
            // We might want to do something with this later, but for now just ignore it.
            return;
        }
        self.errors.lock().unwrap().push(err);
    }

    pub fn add(&self, module_info: &ModuleInfo, range: TextRange, msg: String) {
        self.add_error(Error::new(module_info.dupe(), range, msg));
    }

    pub fn len(&self) -> usize {
        self.errors.lock().unwrap().len()
    }

    pub fn collect(&self) -> Vec<Error> {
        self.errors
            .lock()
            .unwrap()
            .iter()
            .map(|x| x.0.clone())
            .collect()
    }

    pub fn summarise<'a>(xs: impl Iterator<Item = &'a ErrorCollector>) -> Vec<(String, usize)> {
        let mut map = SmallMap::new();
        for x in xs {
            for err in x.errors.lock().unwrap().iter() {
                // Lots of error messages have names in them, e.g. "Can't find module `foo`".
                // We want to summarise those together, so replace bits of text inside `...` with `...`.
                let clean_msg = err
                    .0
                    .msg
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
        for err in self.errors.lock().unwrap().iter() {
            error!("{err}");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use ruff_python_ast::name::Name;

    use super::*;
    use crate::module::module_name::ModuleName;

    #[test]
    fn test_error_collector() {
        let errors = ErrorCollector::new();
        let mi = ModuleInfo::new(
            ModuleName::from_name(&Name::new("main")),
            Path::new("main.py").to_owned(),
            "contents".to_owned(),
            true,
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
        assert_eq!(
            errors.collect().iter().map(|x| &x.msg).collect::<Vec<_>>(),
            vec!["a", "b", "a"]
        );
    }
}
