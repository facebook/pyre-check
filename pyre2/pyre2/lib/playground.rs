/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use anyhow::anyhow;
use dupe::Dupe;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use ruff_source_file::SourceLocation;
use serde::Serialize;
use starlark_map::small_map::SmallMap;

use crate::config::ErrorConfigs;
use crate::metadata::RuntimeMetadata;
use crate::module::bundled::typeshed;
use crate::module::bundled::BundledTypeshed;
use crate::module::module_info::SourceRange;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::require::Require;
use crate::state::state::State;
use crate::util::prelude::VecExt;

#[derive(Serialize)]
pub struct Position {
    #[serde(rename(serialize = "column"))]
    pub column: i32,
    #[serde(rename(serialize = "lineNumber"))]
    pub line: i32,
}

impl Position {
    fn new(position: SourceLocation) -> Self {
        Self {
            line: position.row.to_zero_indexed() as i32 + 1,
            column: position.column.to_zero_indexed() as i32 + 1,
        }
    }
}

#[derive(Serialize)]
pub struct Range {
    #[serde(rename(serialize = "startLineNumber"))]
    pub start_line: i32,
    #[serde(rename(serialize = "startColumn"))]
    pub start_col: i32,
    #[serde(rename(serialize = "endLineNumber"))]
    pub end_line: i32,
    #[serde(rename(serialize = "endColumn"))]
    pub end_col: i32,
}

impl Range {
    fn new(range: SourceRange) -> Self {
        Self {
            start_line: range.start.row.to_zero_indexed() as i32 + 1,
            start_col: range.start.column.to_zero_indexed() as i32 + 1,
            end_line: range.end.row.to_zero_indexed() as i32 + 1,
            end_col: range.end.column.to_zero_indexed() as i32 + 1,
        }
    }
}

#[derive(Serialize)]
pub struct Diagnostic {
    #[serde(rename(serialize = "startLineNumber"))]
    pub start_line: i32,
    #[serde(rename(serialize = "startColumn"))]
    pub start_col: i32,
    #[serde(rename(serialize = "endLineNumber"))]
    pub end_line: i32,
    #[serde(rename(serialize = "endColumn"))]
    pub end_col: i32,
    pub message: String,
    pub kind: String,
    pub severity: i32,
}

#[derive(Serialize)]
pub struct TypeQueryContent {
    language: String,
    value: String,
}

#[derive(Serialize)]
pub struct TypeQueryResult {
    contents: Vec<TypeQueryContent>,
}

#[derive(Serialize)]
pub struct AutoCompletionItem {
    label: String,
    detail: Option<String>,
    kind: Option<CompletionItemKind>,
    #[serde(rename(serialize = "sortText"))]
    sort_text: Option<String>,
}

#[derive(Serialize)]
pub struct InlayHint {
    label: String,
    position: Position,
}

#[derive(Debug, Clone)]
struct DemoEnv(
    SmallMap<ModuleName, (ModulePath, Option<String>)>,
    BundledTypeshed,
);

impl DemoEnv {
    pub fn new() -> Self {
        Self(SmallMap::new(), typeshed().unwrap().clone())
    }

    pub fn add(&mut self, name: &str, code: String) {
        let module_name = ModuleName::from_str(name);
        let relative_path = ModulePath::memory(PathBuf::from("test.py"));
        self.0.insert(module_name, (relative_path, Some(code)));
    }

    pub fn config() -> RuntimeMetadata {
        RuntimeMetadata::default()
    }

    pub fn typeshed(&self) -> &BundledTypeshed {
        &self.1
    }
}

impl Loader for DemoEnv {
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        if let Some((path, _)) = self.0.get(&module) {
            Ok(path.dupe())
        } else if self.typeshed().find(module).is_some() {
            Ok(ModulePath::memory(PathBuf::from(format!(
                "{}.pyi",
                module.as_str().replace('.', "/")
            ))))
        } else {
            Err(FindError::not_found(anyhow!(
                "module is not available in sandbox"
            )))
        }
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        // This function involves scanning all paths to find what matches.
        // Not super efficient, but fine for tests, and we don't have many modules.
        let memory_path = ModulePath::memory(path.to_owned());
        for (p, contents) in self.0.values() {
            if p == &memory_path {
                if let Some(c) = contents {
                    return Some(Arc::new(c.clone()));
                }
            }
        }
        let module_name = ModuleName::from_str(path.file_stem()?.to_str()?);
        Some(Arc::new(
            self.typeshed()
                .find_and_load(module_name)?
                .to_string()
                .to_owned(),
        ))
    }
}

#[derive(Debug)]
struct Load(Arc<Mutex<DemoEnv>>);

impl Loader for Load {
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        self.0.lock().unwrap().find_import(module)
    }

    fn load_from_memory(&self, path: &Path) -> Option<Arc<String>> {
        self.0.lock().unwrap().load_from_memory(path)
    }
}

pub struct LanguageServiceState {
    state: State,
    demo_env: Arc<Mutex<DemoEnv>>,
    loader: LoaderId,
    handle: Handle,
}

impl Default for LanguageServiceState {
    fn default() -> Self {
        let demo_env = {
            let mut demo_env = DemoEnv::new();
            demo_env.add("test", "".to_owned());
            Arc::new(Mutex::new(demo_env))
        };
        let load = Load(demo_env.dupe());
        let loader = LoaderId::new(load);
        let state = State::new();
        let handle = Handle::new(
            ModuleName::from_str("test"),
            ModulePath::memory(PathBuf::from("test.py")),
            DemoEnv::config(),
            loader.dupe(),
        );
        state.run(
            &[(handle.dupe(), Require::Everything)],
            Require::Exports,
            None,
        );
        Self {
            state,
            demo_env,
            loader,
            handle,
        }
    }
}

impl LanguageServiceState {
    pub fn update_source(&mut self, source: String) {
        self.demo_env.lock().unwrap().add("test", source);
        let mut transaction = self
            .state
            .new_committable_transaction(Require::Exports, None);
        transaction
            .as_mut()
            .invalidate_memory(self.loader.dupe(), &[PathBuf::from("test.py")]);
        self.state.run_with_committing_transaction(
            transaction,
            &[(self.handle.dupe(), Require::Everything)],
        );
    }

    pub fn get_errors(&self) -> Vec<Diagnostic> {
        self.state
            .transaction()
            .readable()
            .get_loads([&self.handle])
            .collect_errors(&ErrorConfigs::default())
            .shown
            .into_iter()
            .map(|e| {
                let range = e.source_range();
                Diagnostic {
                    start_line: range.start.row.to_zero_indexed() as i32 + 1,
                    start_col: range.start.column.to_zero_indexed() as i32 + 1,
                    end_line: range.end.row.to_zero_indexed() as i32 + 1,
                    end_col: range.end.column.to_zero_indexed() as i32 + 1,
                    message: e.msg().to_owned(),
                    kind: e.error_kind().to_name().to_owned(),
                    severity: 8,
                }
            })
            .collect()
    }

    pub fn query_type(&mut self, line: i32, column: i32) -> Option<TypeQueryResult> {
        let handle = self.handle.dupe();
        self.state
            .transaction()
            .readable()
            .get_module_info(&handle)
            .map(|info| info.to_text_size((line - 1) as u32, (column - 1) as u32))
            .and_then(|position| self.state.transaction().hover(&handle, position))
            .map(|t| t.to_string())
            .map(|result| TypeQueryResult {
                contents: vec![TypeQueryContent {
                    language: "python".to_owned(),
                    value: result,
                }],
            })
    }

    pub fn goto_definition(&mut self, line: i32, column: i32) -> Option<Range> {
        let handle = self.handle.dupe();
        self.state
            .transaction()
            .readable()
            .get_module_info(&handle)
            .map(|info| info.to_text_size((line - 1) as u32, (column - 1) as u32))
            .and_then(|position| self.state.transaction().goto_definition(&handle, position))
            .map(|range_with_mod_info| {
                Range::new(
                    range_with_mod_info
                        .module_info
                        .source_range(range_with_mod_info.range),
                )
            })
    }

    pub fn autocomplete(&mut self, line: i32, column: i32) -> Vec<AutoCompletionItem> {
        let handle = self.handle.dupe();
        self.state
            .transaction()
            .readable()
            .get_module_info(&handle)
            .map(|info| info.to_text_size((line - 1) as u32, (column - 1) as u32))
            .map_or(Vec::new(), |position| {
                self.state.transaction().completion(&handle, position)
            })
            .into_iter()
            .map(
                |CompletionItem {
                     label,
                     detail,
                     sort_text,
                     kind,
                     ..
                 }| AutoCompletionItem {
                    label,
                    detail,
                    kind,
                    sort_text,
                },
            )
            .collect::<Vec<_>>()
    }

    pub fn inlay_hint(&mut self) -> Vec<InlayHint> {
        let handle = self.handle.dupe();
        let transaction = self.state.transaction();
        transaction
            .readable()
            .get_module_info(&handle)
            .zip(transaction.inlay_hints(&handle))
            .map(|(info, hints)| {
                hints.into_map(|(position, label)| {
                    let position = Position::new(info.source_location(position));
                    InlayHint { label, position }
                })
            })
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::kind::ErrorKind;

    #[test]
    fn test_regular_import() {
        let mut state = LanguageServiceState::default();
        let expected_errors: Vec<String> = Vec::new();

        state.update_source("from typing import *".to_owned());

        assert_eq!(
            state
                .get_errors()
                .into_iter()
                .map(|x| x.message)
                .collect::<Vec<_>>(),
            expected_errors,
        );
    }

    #[test]
    fn test_invalid_import() {
        let mut state = LanguageServiceState::default();
        state.update_source("from t".to_owned());
        let expected_errors: Vec<&str> = vec![
            "Could not find import of `t`, module is not available in sandbox",
            "Parse error: Expected 'import', found newline",
        ];
        let expected_error_kinds: Vec<ErrorKind> =
            vec![ErrorKind::MissingModuleAttribute, ErrorKind::ParseError];

        assert_eq!(
            state
                .get_errors()
                .into_iter()
                .map(|x| x.message)
                .collect::<Vec<_>>(),
            expected_errors,
        );

        assert_eq!(
            state
                .get_errors()
                .into_iter()
                .map(|x| x.kind)
                .collect::<Vec<_>>(),
            expected_error_kinds
                .iter()
                .map(|k| k.to_name())
                .collect::<Vec<_>>(),
        );
    }
}
