/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
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
use crate::config::ErrorConfigs;
use crate::error::error::print_errors;
use crate::metadata::RuntimeMetadata;
use crate::module::bundled::typeshed;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::require::Require;
use crate::state::state::State;
use crate::state::subscriber::TestSubscriber;
use crate::types::class::Class;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;
use crate::util::thread_pool::init_thread_pool;
use crate::util::thread_pool::ThreadCount;
use crate::util::trace::init_tracing;
use crate::PythonVersion;

#[macro_export]
macro_rules! testcase {
    (bug = $explanation:literal, $name:ident, $imports:expr, $contents:literal,) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            $crate::test::util::testcase_for_macro($imports, $contents, file!(), line!() + 1)
        }
    };
    (bug = $explanation:literal, $name:ident, $contents:literal,) => {
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

fn default_path(module: ModuleName) -> PathBuf {
    PathBuf::from(format!("{}.py", module.as_str().replace('.', "/")))
}

#[derive(Debug, Default, Clone)]
pub struct TestEnv {
    modules: SmallMap<ModuleName, (ModulePath, Option<String>)>,
    version: PythonVersion,
}

impl TestEnv {
    pub fn new() -> Self {
        // We aim to init the tracing before now, but if not, better now than never
        init_test();
        Self::default()
    }

    pub fn new_with_version(version: PythonVersion) -> Self {
        let mut res = Self::new();
        res.version = version;
        res
    }

    pub fn add_with_path(&mut self, name: &str, code: &str, path: &str) {
        self.modules.insert(
            ModuleName::from_str(name),
            (
                ModulePath::memory(PathBuf::from(path)),
                Some(code.to_owned()),
            ),
        );
    }

    pub fn add(&mut self, name: &str, code: &str) {
        let module_name = ModuleName::from_str(name);
        let relative_path = ModulePath::memory(default_path(module_name));
        self.modules
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
        self.modules
            .insert(module_name, (ModulePath::filesystem(path), None));
    }

    pub fn metadata(&self) -> RuntimeMetadata {
        RuntimeMetadata::new(self.version, "linux".to_owned())
    }

    pub fn to_state(self) -> (State, impl Fn(&str) -> Handle) {
        let config = self.metadata();
        let loader = LoaderId::new(self.clone());
        let handles = self
            .modules
            .into_iter()
            // Reverse so we start at the last file, which is likely to be what the user
            // would have opened, so make it most faithful.
            .rev()
            .map(|(x, (path, _))| Handle::new(x, path, config.dupe(), loader.dupe()))
            .collect::<Vec<_>>();
        let state = State::new();
        let subscriber = TestSubscriber::new();
        state.run(
            &handles.map(|x| (x.dupe(), Require::Everything)),
            Require::Exports,
            Some(Box::new(subscriber.dupe())),
        );
        subscriber.finish();
        print_errors(
            &state
                .transaction()
                .readable()
                .get_loads(handles.iter())
                .collect_errors(&ErrorConfigs::default())
                .shown,
        );
        (state, move |module| {
            let name = ModuleName::from_str(module);
            Handle::new(
                name,
                loader.find_import(name).unwrap(),
                config.dupe(),
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
            "^".repeat(std::cmp::max(
                end_loc.column.to_zero_indexed() - start_loc.column.to_zero_indexed(),
                1
            ))
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
                panic!("Invalid cursor at {}:{}", line_index, row_index);
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
    let mut handles = HashMap::new();
    for (name, _) in files {
        handles.insert(*name, handle(name));
    }
    if assert_zero_errors {
        assert_eq!(
            state
                .transaction()
                .readable()
                .get_loads(handles.values())
                .collect_errors(&ErrorConfigs::default())
                .shown
                .len(),
            0
        );
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
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        if let Some((path, _)) = self.modules.get(&module) {
            Ok(path.dupe())
        } else if let Some(path) = typeshed().map_err(FindError::not_found)?.find(module) {
            Ok(path)
        } else {
            Err(FindError::not_found(anyhow!(
                "Module not given in test suite"
            )))
        }
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        // This function involves scanning all paths to find what matches.
        // Not super efficient, but fine for tests, and we don't have many modules.
        let memory_path = ModulePath::memory(path.to_owned());
        for (p, contents) in self.modules.values() {
            if p == &memory_path
                && let Some(c) = contents
            {
                return Some(Arc::new(c.clone()));
            }
        }
        None
    }
}

pub fn init_test() {
    init_tracing(true, true);
    // Enough threads to see parallelism bugs, but not too many to debug through.
    init_thread_pool(ThreadCount::NumThreads(NonZeroUsize::new(3).unwrap()));
}

/// Should only be used from the `testcase!` macro.
pub fn testcase_for_macro(
    mut env: TestEnv,
    contents: &str,
    file: &str,
    line: u32,
) -> anyhow::Result<()> {
    init_test();
    let mut start_line = line as usize + 1;
    if !env.modules.is_empty() {
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
        let (state, handle) = env.clone().to_state();
        state
            .transaction()
            .readable()
            .get_loads([&handle("main")])
            .check_against_expectations(&ErrorConfigs::default())?;
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
    let solutions = state
        .transaction()
        .readable()
        .get_solutions(handle)
        .unwrap();

    match solutions.get(&KeyExport(Name::new(name))).map(|x| &**x) {
        Some(Type::ClassDef(cls)) => Some(cls.dupe()),
        _ => None,
    }
}
