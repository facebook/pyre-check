/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_source_file::OneIndexed;
use starlark_map::small_set::SmallSet;

use crate::module::module_info::SourceRange;

/// Record the position of `# type: ignore[valid-type]` statements.
/// For now we don't record the content of the ignore, but we could.
#[derive(Debug, Clone, Default)]
pub struct Ignore {
    ignores: SmallSet<OneIndexed>,
}

impl Ignore {
    pub fn new(code: &str) -> Self {
        let mut ignores = SmallSet::new();
        for (line, line_str) in code.lines().enumerate() {
            if Self::is_ignore_directive(line_str) {
                ignores.insert(OneIndexed::from_zero_indexed(line));
            }
        }
        Self { ignores }
    }

    fn is_ignore_directive(line: &str) -> bool {
        line.split("# ").skip(1).any(|part| {
            part.starts_with("type: ignore")
                || part.starts_with("pyrefly: ignore")
                || part.starts_with("pyre-ignore")
                || part.starts_with("pyre-fixme")
        })
    }

    pub fn is_ignored(&self, range: &SourceRange, msg: &str) -> bool {
        // for now, we ignore the msg
        let _unused = msg;
        // We allow an ignore the line before the range, or on any line within the range.
        // We convert to/from zero-indexed because OneIndexed does not implement Step.
        (range.start.row.to_zero_indexed().saturating_sub(1)..=range.end.row.to_zero_indexed())
            .any(|x| self.ignores.contains(&OneIndexed::from_zero_indexed(x)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_ignore_directive() {
        assert!(Ignore::is_ignore_directive(
            "stuff # type: ignore # and then stuff"
        ));
        assert!(Ignore::is_ignore_directive(
            "more # stuff # type: ignore[valid-type]"
        ));
        assert!(!Ignore::is_ignore_directive("# ignore: pyrefly"));
        assert!(!Ignore::is_ignore_directive(" pyrefly: ignore"));
        assert!(!Ignore::is_ignore_directive("normal line"));
    }
}
