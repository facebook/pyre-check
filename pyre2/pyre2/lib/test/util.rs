/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Once;
use std::thread::sleep;
use std::time::Duration;
use std::time::Instant;

use anyhow::anyhow;
use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_source_file::LineIndex;
use ruff_source_file::OneIndexed;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::KeyExport;
use crate::error::style::ErrorStyle;
use crate::metadata::RuntimeMetadata;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::state::subscriber::TestSubscriber;
use crate::test::stdlib::lookup_test_stdlib;
use crate::types::class::Class;
use crate::types::types::Type;
use crate::util::trace::init_tracing;

#[macro_export]
macro_rules! testcase {
    ($name:ident, $imports:expr, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!())
        }
    };
    ($name:ident, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro(
                $crate::test::util::TestEnv::new(),
                $contents,
                file!(),
                line!(),
            )
        }
    };
}

#[macro_export]
macro_rules! testcase_with_bug {
    ($explanation:literal, $name:ident, $imports:expr, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!() + 1)
        }
    };
    ($explanation:literal, $name:ident, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro(
                $crate::test::util::TestEnv::new(),
                $contents,
                file!(),
                line!() + 1,
            )
        }
    };
}

enum TestPathStyle {
    Source,
    Stub,
}

fn default_path(module: ModuleName, style: TestPathStyle) -> PathBuf {
    let ext = match style {
        TestPathStyle::Source => "py",
        TestPathStyle::Stub => "pyi",
    };
    PathBuf::from(format!("{}.{}", module.as_str().replace('.', "/"), ext))
}

#[derive(Debug, Default, Clone)]
pub struct TestEnv(SmallMap<ModuleName, (ModulePath, Option<String>)>);

impl TestEnv {
    pub fn new() -> Self {
        // We aim to init the tracing before now, but if not, better now than never
        test_init_tracing();
        Self::default()
    }

    pub fn add_with_path(&mut self, name: &str, code: &str, path: &str) {
        self.0.insert(
            ModuleName::from_str(name),
            (
                ModulePath::memory(PathBuf::from(path)),
                Some(code.to_owned()),
            ),
        );
    }

    pub fn add(&mut self, name: &str, code: &str) {
        let module_name = ModuleName::from_str(name);
        let relative_path = ModulePath::memory(default_path(module_name, TestPathStyle::Source));
        self.0
            .insert(module_name, (relative_path, Some(code.to_owned())));
    }

    pub fn one(name: &str, code: &str) -> Self {
        let mut res = Self::new();
        res.add(name, code);
        res
    }

    pub fn one_with_path(name: &str, code: &str, path: &str) -> Self {
        let mut res = Self::new();
        res.add_with_path(name, code, path);
        res
    }

    pub fn add_real_path(&mut self, name: &str, path: PathBuf) {
        let module_name = ModuleName::from_str(name);
        self.0
            .insert(module_name, (ModulePath::filesystem(path), None));
    }

    pub fn config() -> RuntimeMetadata {
        RuntimeMetadata::default()
    }

    pub fn to_state(self) -> (State, impl Fn(&str) -> Handle) {
        let config = Self::config();
        let loader = LoaderId::new(self.clone());
        let handles = self
            .0
            .into_iter()
            .map(|(x, (path, _))| Handle::new(x, path, config.dupe(), loader.dupe()))
            .collect::<Vec<_>>();
        let mut state = State::new(true);
        let subscriber = TestSubscriber::new();
        state.run(&handles, Some(Box::new(subscriber.dupe())));
        subscriber.finish();
        state.print_errors();
        (state, move |module| {
            let name = ModuleName::from_str(module);
            Handle::new(
                name,
                loader.find(name).unwrap().0,
                Self::config(),
                loader.dupe(),
            )
        })
    }
}

pub fn code_frame_of_source_at_range(source: &str, range: TextRange) -> String {
    let index = LineIndex::from_source_text(source);
    let start_loc = index.source_location(range.start(), source);
    let end_loc = index.source_location(range.end(), source);
    if (range.start().checked_add(TextSize::from(1))) == Some(range.end()) {
        let full_line = source.lines().nth(start_loc.row.to_zero_indexed()).unwrap();
        format!(
            "{} | {}\n{}   {}^",
            start_loc.row,
            full_line,
            " ".repeat(start_loc.row.to_string().len()),
            " ".repeat(start_loc.column.to_zero_indexed())
        )
    } else if start_loc.row == end_loc.row {
        let full_line = source.lines().nth(start_loc.row.to_zero_indexed()).unwrap();
        format!(
            "{} | {}\n{}   {}{}",
            start_loc.row,
            full_line,
            " ".repeat(start_loc.row.to_string().len()),
            " ".repeat(start_loc.column.to_zero_indexed()),
            "^".repeat(end_loc.column.to_zero_indexed() - start_loc.column.to_zero_indexed())
        )
    } else {
        panic!("Computing multi-line code frame is unsupported for now.")
    }
}

pub fn code_frame_of_source_at_position(source: &str, position: TextSize) -> String {
    code_frame_of_source_at_range(
        source,
        TextRange::new(position, position.checked_add(TextSize::new(1)).unwrap()),
    )
}

/// Given `source`, this function will find all the positions pointed by the special `# ^` comments.
///
/// e.g. for
/// ```
/// Line 1: x = 42
/// Line 2: #    ^
/// ```
///
/// The position will be the position of `2` in Line 1.
pub fn extract_cursors_for_test(source: &str) -> Vec<TextSize> {
    let mut ranges = Vec::new();
    let mut prev_line = "";
    let index = LineIndex::from_source_text(source);
    for (line_index, line_str) in source.lines().enumerate() {
        for (row_index, _) in line_str.match_indices('^') {
            if prev_line.len() < row_index {
                panic!("Invald cursor at {}:{}", line_index, row_index);
            }
            let position = index.offset(
                OneIndexed::from_zero_indexed(line_index - 1),
                OneIndexed::from_zero_indexed(row_index),
                source,
            );
            ranges.push(position);
        }
        prev_line = line_str;
    }
    ranges
}

pub fn mk_multi_file_state(
    files: &[(&'static str, &str)],
    assert_zero_errors: bool,
) -> (HashMap<&'static str, Handle>, State) {
    let mut test_env = TestEnv::new();
    for (name, code) in files {
        test_env.add(name, code);
    }
    let (state, handle) = test_env.to_state();
    if assert_zero_errors {
        assert_eq!(state.count_errors(), 0);
    }
    let mut handles = HashMap::new();
    for (name, _) in files {
        handles.insert(*name, handle(name));
    }
    (handles, state)
}

pub fn mk_multi_file_state_assert_no_errors(
    files: &[(&'static str, &str)],
) -> (HashMap<&'static str, Handle>, State) {
    mk_multi_file_state(files, true)
}

fn get_batched_lsp_operations_report_helper(
    files: &[(&'static str, &str)],
    assert_zero_errors: bool,
    get_report: impl Fn(&State, &Handle, TextSize) -> String,
) -> String {
    let (handles, state) = mk_multi_file_state(files, assert_zero_errors);
    let mut report = String::new();
    for (name, code) in files {
        report.push_str("# ");
        report.push_str(name);
        report.push_str(".py\n");
        let handle = handles.get(name).unwrap();
        for position in extract_cursors_for_test(code) {
            report.push_str(&code_frame_of_source_at_position(code, position));
            report.push('\n');
            report.push_str(&get_report(&state, handle, position));
            report.push_str("\n\n");
        }
        report.push('\n');
    }

    report
}

/// Given a list of `files`, extract the location pointed by the special `#   ^` comments
/// (See `extract_cursors_for_test`), and perform the operation defined by `get_report`.
/// A human-readable report of the results of all specified operations will be returned.
pub fn get_batched_lsp_operations_report(
    files: &[(&'static str, &str)],
    get_report: impl Fn(&State, &Handle, TextSize) -> String,
) -> String {
    get_batched_lsp_operations_report_helper(files, true, get_report)
}

pub fn get_batched_lsp_operations_report_allow_error(
    files: &[(&'static str, &str)],
    get_report: impl Fn(&State, &Handle, TextSize) -> String,
) -> String {
    get_batched_lsp_operations_report_helper(files, false, get_report)
}

impl Loader for TestEnv {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        let style = ErrorStyle::Delayed;
        if let Some((path, _)) = self.0.get(&module) {
            Ok((path.dupe(), style))
        } else if lookup_test_stdlib(module).is_some() {
            Ok((
                ModulePath::memory(default_path(module, TestPathStyle::Stub)),
                style,
            ))
        } else {
            Err(FindError::new(anyhow!("Module not given in test suite")))
        }
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        // This function involves scanning all paths to find what matches.
        // Not super efficient, but fine for tests, and we don't have many modules.
        let memory_path = ModulePath::memory(path.to_owned());
        for (p, contents) in self.0.values() {
            if p == &memory_path
                && let Some(c) = contents
            {
                return Some(Arc::new(c.clone()));
            }
        }
        Some(Arc::new(
            lookup_test_stdlib(ModuleName::from_str(path.file_stem()?.to_str()?))?.to_owned(),
        ))
    }
}

static INIT_TRACING_ONCE: Once = Once::new();

pub fn test_init_tracing() {
    INIT_TRACING_ONCE.call_once(|| init_tracing(true, true));
}

/// Should only be used from the `testcase!` macro.
pub fn testcase_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
) -> anyhow::Result<()> {
    test_init_tracing();
    let mut start_line = line as usize + 1;
    if !env.0.is_empty() {
        start_line += 1;
    }
    env.add_with_path(
        "main",
        &format!("{}{}", "\n".repeat(start_line), contents),
        file,
    );
    // If any given test regularly takes > 10s, that's probably a bug.
    // Currently all are less than 3s in debug, even when running in parallel.
    let limit = 10;
    for _ in 0..3 {
        let start = Instant::now();
        env.clone().to_state().0.check_against_expectations()?;
        if start.elapsed().as_secs() <= limit {
            return Ok(());
        }
        // Give a bit of a buffer if the machine is very busy
        sleep(Duration::from_secs(limit / 2));
    }
    Err(anyhow!("Test took too long (> {limit}s)"))
}

pub fn mk_state(code: &str) -> (Handle, State) {
    let (state, handle) = TestEnv::one("main", code).to_state();
    (handle("main"), state)
}

pub fn get_class(name: &str, handle: &Handle, state: &State) -> Option<Class> {
    let solutions = state.get_solutions(handle).unwrap();

    match solutions.get(&KeyExport(Name::new(name))).map(|x| &**x) {
        Some(Type::ClassDef(cls)) => Some(cls.dupe()),
        _ => None,
    }
}
