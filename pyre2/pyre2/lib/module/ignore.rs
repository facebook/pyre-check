/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_source_file::OneIndexed;
use starlark_map::small_map::SmallMap;

use crate::module::module_info::SourceRange;

#[derive(PartialEq, Debug, Clone, Hash, Eq, Dupe, Copy)]
enum SuppressionKind {
    Ignore,
    Pyre,
    Pyrefly,
}

/// Record the position of `# type: ignore[valid-type]` statements.
/// For now we don't record the content of the ignore, but we could.
#[derive(Debug, Clone, Default)]
pub struct Ignore {
    ignores: SmallMap<OneIndexed, Vec<SuppressionKind>>,
}

impl Ignore {
    pub fn new(code: &str) -> Self {
        let mut ignores = SmallMap::new();
        for (line, line_str) in code.lines().enumerate() {
            if let Some(kind) = Self::get_suppression_kind(line_str) {
                ignores.insert(OneIndexed::from_zero_indexed(line), [kind].to_vec());
            }
        }
        Self { ignores }
    }

    fn get_suppression_kind(line: &str) -> Option<SuppressionKind> {
        for l in line.split("# ").skip(1) {
            if l.starts_with("type: ignore") {
                return Some(SuppressionKind::Ignore);
            } else if l.starts_with("pyrefly: ignore") {
                return Some(SuppressionKind::Pyrefly);
            } else if l.starts_with("pyre-ignore") || l.starts_with("pyre-fixme") {
                return Some(SuppressionKind::Pyre);
            }
        }
        None
    }

    pub fn is_ignored(&self, range: &SourceRange, msg: &str) -> bool {
        // for now, we ignore the msg
        let _unused = msg;
        // We allow an ignore the line before the range, or on any line within the range.
        // We convert to/from zero-indexed because OneIndexed does not implement Step.
        (range.start.row.to_zero_indexed().saturating_sub(1)..=range.end.row.to_zero_indexed())
            .any(|x| self.ignores.contains_key(&OneIndexed::from_zero_indexed(x)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_suppression_kind() {
        assert!(Ignore::get_suppression_kind("stuff # type: ignore # and then stuff").is_some());
        assert!(Ignore::get_suppression_kind("more # stuff # type: ignore[valid-type]").is_some());
        assert!(Ignore::get_suppression_kind("# ignore: pyrefly").is_none());
        assert!(Ignore::get_suppression_kind(" pyrefly: ignore").is_none());
        assert!(Ignore::get_suppression_kind("normal line").is_none());
    }
}
