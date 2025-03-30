/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::error::error::Error;
use crate::error::kind::ErrorKind;
use crate::module::module_path::ModulePath;
use crate::util::display;

type ErrorCounts = SmallMap<ErrorKind, usize>;

/// Returns a Vec of (T, error_counts) where the Vec is sorted by the total
/// error count, descending, and the error_counts are also sorted by the error
/// count, descending.
fn collect_and_sort<I, T>(errors: I) -> Vec<(T, Vec<(ErrorKind, usize)>)>
where
    I: IntoIterator<Item = (T, ErrorCounts)>,
{
    let mut errors = errors
        .into_iter()
        .map(|(p, error_counts)| {
            let mut error_kind_counts = error_counts.into_iter().collect::<Vec<_>>();
            error_kind_counts.sort_by_key(|(_, c)| -(*c as isize));
            (p, error_kind_counts)
        })
        .collect::<Vec<_>>();
    errors.sort_by_key(|(_, m)| -(m.iter().map(|x| x.1).sum::<usize>() as isize));
    errors
}

/// Groups the errors by the path_index'th component of the path,
/// and then collects and sorts the errors as in collect_and_sort.
fn get_top_error_dirs(
    path_errors: &SmallMap<ModulePath, ErrorCounts>,
    path_index: usize,
) -> Vec<(PathBuf, Vec<(ErrorKind, usize)>)> {
    let mut dirs: SmallMap<_, ErrorCounts> = SmallMap::new();
    for (path, errors) in path_errors {
        let dir = PathBuf::from(path.to_string())
            .components()
            .take(path_index + 1)
            .collect::<PathBuf>();
        let error_counts = dirs.entry(dir).or_default();
        for (kind, count) in errors {
            *error_counts.entry(*kind).or_default() += count;
        }
    }
    collect_and_sort(dirs)
}

fn get_errors_per_file(errors: &[Error]) -> SmallMap<ModulePath, SmallMap<ErrorKind, usize>> {
    let mut map: SmallMap<ModulePath, SmallMap<ErrorKind, usize>> = SmallMap::new();
    for err in errors {
        *map.entry(err.path().dupe())
            .or_default()
            .entry(err.error_kind())
            .or_default() += 1;
    }
    map
}

/// Prints a summary of errors found in the input.
/// The summary shows the top directories by error count, top files by error count,
/// and the top errors by count.
/// path_index controls how directories are grouped. For example, for the directory /alpha/beta/gamma/...,
/// path_index = 0 groups by /alpha, path_index = 1 groups by /alpha/beta,
/// and path_index = 2 groups by alpha/beta/gamma.
/// If the path_index is larger than the number of components in the path, then the entire path is used.
pub fn print_error_summary(errors: &[Error], path_index: usize) {
    // TODO: Sort errors by count and then name. More consistent and human-readable.
    // TODO: Consider output formatting.
    let path_errors = get_errors_per_file(errors);
    eprintln!("=== Error Summary ===");
    if path_errors.is_empty() {
        eprintln!("No errors found!");
        return;
    }
    eprintln!("Top 30 Directories by Error Count:");
    let top_dirs = get_top_error_dirs(&path_errors, path_index);
    for (dir, error_kind_counts) in top_dirs.into_iter().take(30) {
        let total_error_count = error_kind_counts.iter().fold(0, |count, (_, c)| count + *c);
        eprintln!(
            "{}: {}",
            dir.display(),
            display::count(total_error_count, "error")
        );
        for (kind, count) in error_kind_counts {
            eprintln!("  {}: {}", kind.to_name(), display::number_thousands(count));
        }
    }

    eprintln!("\nTop 30 Files by Error Count:");
    let top_files = collect_and_sort(path_errors.clone());
    for (path, error_kind_counts) in top_files.into_iter().take(30) {
        let total_error_count = error_kind_counts.iter().fold(0, |count, (_, c)| count + *c);
        eprintln!("{path}: {}", display::count(total_error_count, "error"));
        for (kind, count) in error_kind_counts {
            eprintln!("  {}: {}", kind.to_name(), display::number_thousands(count));
        }
    }

    eprintln!("\nTop Errors by Count:");
    let error_counts = {
        let mut ec = path_errors
            .values()
            .fold(SmallMap::<ErrorKind, usize>::new(), |mut acc, m| {
                for (kind, count) in m {
                    *acc.entry(*kind).or_default() += count;
                }
                acc
            })
            .into_iter()
            .collect::<Vec<_>>();
        ec.sort_by_key(|(_, c)| -(*c as isize));
        ec
    };
    for (kind, count) in error_counts {
        eprintln!("{}: {}", kind.to_name(), display::count(count, "instance"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collect_and_sort() {
        let errors: Vec<(usize, SmallMap<ErrorKind, usize>)> = vec![
            (3, SmallMap::from_iter([(ErrorKind::AsyncError, 1)])),
            (
                2,
                SmallMap::from_iter([(ErrorKind::ReadOnly, 1), (ErrorKind::AsyncError, 2)]),
            ),
            (
                1,
                SmallMap::from_iter([
                    (ErrorKind::ReadOnly, 2),
                    (ErrorKind::AsyncError, 1),
                    (ErrorKind::ParseError, 3),
                ]),
            ),
        ];
        let want = vec![
            (
                1,
                vec![
                    (ErrorKind::ParseError, 3),
                    (ErrorKind::ReadOnly, 2),
                    (ErrorKind::AsyncError, 1),
                ],
            ),
            (
                2,
                vec![(ErrorKind::AsyncError, 2), (ErrorKind::ReadOnly, 1)],
            ),
            (3, vec![(ErrorKind::AsyncError, 1)]),
        ];
        let got = collect_and_sort(errors);
        assert_eq!(want, got);
    }

    #[test]
    fn test_get_top_error_dirs() {
        fn mpfs(s: &str) -> ModulePath {
            ModulePath::filesystem(PathBuf::from(s))
        }
        fn pb(s: &str) -> PathBuf {
            PathBuf::from(s)
        }
        let errors = SmallMap::from_iter(vec![
            (
                mpfs("base/proj/sub/a.py"),
                SmallMap::from_iter([(ErrorKind::ReadOnly, 2)]),
            ),
            (
                mpfs("base/proj/sub/b.py"),
                SmallMap::from_iter([(ErrorKind::AsyncError, 3)]),
            ),
            (
                mpfs("base/proj/dub/z.py"),
                SmallMap::from_iter([(ErrorKind::ReadOnly, 4)]),
            ),
            (
                mpfs("base/short.py"),
                SmallMap::from_iter([(ErrorKind::AnnotationMismatch, 10)]),
            ),
        ]);

        let want = vec![(
            pb("base"),
            vec![
                (ErrorKind::AnnotationMismatch, 10),
                (ErrorKind::ReadOnly, 6),
                (ErrorKind::AsyncError, 3),
            ],
        )];
        assert_eq!(want, get_top_error_dirs(&errors, 0));

        let want = vec![
            (
                pb("base/short.py"),
                vec![(ErrorKind::AnnotationMismatch, 10)],
            ),
            (
                pb("base/proj"),
                vec![(ErrorKind::ReadOnly, 6), (ErrorKind::AsyncError, 3)],
            ),
        ];
        assert_eq!(want, get_top_error_dirs(&errors, 1));

        let want = vec![
            (
                pb("base/short.py"),
                vec![(ErrorKind::AnnotationMismatch, 10)],
            ),
            (
                pb("base/proj/sub"),
                vec![(ErrorKind::AsyncError, 3), (ErrorKind::ReadOnly, 2)],
            ),
            (pb("base/proj/dub"), vec![(ErrorKind::ReadOnly, 4)]),
        ];
        assert_eq!(want, get_top_error_dirs(&errors, 2));

        let want = vec![
            (
                pb("base/short.py"),
                vec![(ErrorKind::AnnotationMismatch, 10)],
            ),
            (pb("base/proj/dub/z.py"), vec![(ErrorKind::ReadOnly, 4)]),
            (pb("base/proj/sub/b.py"), vec![(ErrorKind::AsyncError, 3)]),
            (pb("base/proj/sub/a.py"), vec![(ErrorKind::ReadOnly, 2)]),
        ];
        assert_eq!(want, get_top_error_dirs(&errors, 3));
        assert_eq!(want, get_top_error_dirs(&errors, 30000));
    }
}
