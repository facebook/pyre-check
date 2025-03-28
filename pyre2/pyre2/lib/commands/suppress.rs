/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use starlark_map::small_map::SmallMap;

use crate::error::error::Error;
use crate::util::fs_anyhow;

/// Combines all errors that affect one line into a single entry.
// The current format is: `# type: ignore  # error1, error2, ...`, because pyrefly does not currently support pyre-fixme.
fn dedup_errors(errors: &[Error]) -> SmallMap<usize, String> {
    let mut deduped_errors = SmallMap::new();
    for error in errors {
        let e: &mut String = deduped_errors
            .entry(error.source_range().start.row.to_zero_indexed())
            .or_default();
        if e.is_empty() {
            e.push_str("# pyrefly: ignore  # ");
        } else {
            e.push_str(", ");
        }
        e.push_str(error.error_kind().to_name());
    }
    deduped_errors
}

/// Adds error suppressions for the given errors in the given files.
/// Returns a list of files that failed to be be patched, and a list of files that were patched.
/// The list of failures includes the error that occurred, which may be a read or write error.
fn add_suppressions(
    path_errors: &SmallMap<PathBuf, Vec<Error>>,
) -> (Vec<(&PathBuf, anyhow::Error)>, Vec<&PathBuf>) {
    let mut failures = vec![];
    let mut successes = vec![];
    for (path, errors) in path_errors {
        let file = match fs_anyhow::read_to_string(path) {
            Ok(f) => f,
            Err(e) => {
                failures.push((path, e));
                continue;
            }
        };
        let deduped_errors = dedup_errors(errors);
        let mut buf = String::new();
        for (idx, line) in file.lines().enumerate() {
            if let Some(error_comment) = deduped_errors.get(&idx) {
                // As a simple formatting step, indent the error comment to match the line below it.
                if let Some(first_char) = line.find(|c: char| !c.is_whitespace()) {
                    buf.push_str(&line[..first_char]);
                }
                buf.push_str(error_comment);
                buf.push('\n');
            }
            buf.push_str(line);
            buf.push('\n');
        }
        if let Err(e) = fs_anyhow::write(path, buf.as_bytes()) {
            failures.push((path, e));
        } else {
            successes.push(path);
        }
    }
    (failures, successes)
}

pub fn suppress_errors(path_errors: &SmallMap<PathBuf, Vec<Error>>) {
    eprintln!("Inserting error suppressions...");
    if path_errors.is_empty() {
        eprintln!("No errors to suppress!");
        return;
    }
    let (failures, successes) = add_suppressions(path_errors);
    eprintln!(
        "Finished suppressing errors in {}/{} files",
        successes.len(),
        path_errors.len()
    );
    if !failures.is_empty() {
        eprintln!("Failed to suppress errors in {} files:", failures.len());
        for (path, e) in failures {
            eprintln!("  {path:#?}: {e}");
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_str_eq;
    use ruff_source_file::OneIndexed;
    use ruff_source_file::SourceLocation;
    use tempfile;
    use vec1::Vec1;

    use super::*;
    use crate::error::kind::ErrorKind;
    use crate::module::module_info::SourceRange;
    use crate::module::module_path::ModulePath;

    fn sourcerange(row: usize, column: usize) -> SourceRange {
        let row = OneIndexed::new(row).unwrap();
        let column = OneIndexed::new(column).unwrap();
        SourceRange {
            start: SourceLocation { row, column },
            end: SourceLocation { row, column },
        }
    }

    fn error(path: PathBuf, row: usize, column: usize, error_kind: ErrorKind) -> Error {
        Error::new(
            ModulePath::filesystem(path),
            sourcerange(row, column),
            Vec1::new("test message".to_owned()),
            false,
            error_kind,
        )
    }

    #[test]
    fn test_add_suppressions() {
        let tdir = tempfile::tempdir().unwrap();
        let path = tdir.path().join("test.py");
        fs_anyhow::write(
            &path,
            br#"x: str = 1


def f(y: int) -> None:
    """Doc comment"""
    x = "one" + y
    return x


f(x)

"#,
        )
        .unwrap();
        // These errors were determined by manually running pyrefly on the snippet above.
        // As such, they may not be 100% accurate with what pyrefly produces now.
        let errors = {
            let mut e = SmallMap::new();
            e.insert(
                path.clone(),
                vec![
                    error(path.clone(), 1, 10, ErrorKind::BadAssignment),
                    error(path.clone(), 6, 7, ErrorKind::BadArgumentType),
                    error(path.clone(), 7, 10, ErrorKind::BadReturn),
                    error(path.clone(), 10, 3, ErrorKind::BadArgumentType),
                ],
            );
            e
        };

        let (got_failures, got_successes) = add_suppressions(&errors);
        assert!(got_failures.is_empty());
        assert_eq!(vec![&path], got_successes);

        let want_file = r#"# pyrefly: ignore  # bad-assignment
x: str = 1


def f(y: int) -> None:
    """Doc comment"""
    # pyrefly: ignore  # bad-argument-type
    x = "one" + y
    # pyrefly: ignore  # bad-return
    return x


# pyrefly: ignore  # bad-argument-type
f(x)

"#;
        let got_file = fs_anyhow::read_to_string(&path).unwrap();
        assert_str_eq!(want_file, got_file);
    }
}
