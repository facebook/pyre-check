/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod format;
pub mod index;
pub mod issue;
pub mod model;
pub mod types;

#[cfg(test)]
mod integration_tests {
    use std::path::PathBuf;

    use crate::index;
    use crate::issue::IssueOptions;
    use crate::model::ModelOptions;
    use crate::types;

    fn test_data_dir() -> PathBuf {
        PathBuf::from(std::env::var("TEST_DATA_PATH").expect("TEST_DATA_PATH not set"))
    }

    #[test]
    fn test_indexing() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();
        // Verify we found models, issues, and call graphs.
        assert!(!index.callables.is_empty());
        let model_count = index
            .callables
            .values()
            .filter(|c| c.model.is_some())
            .count();
        assert!(model_count > 0);
        let issue_count: usize = index.callables.values().map(|c| c.issues.len()).sum();
        assert!(issue_count > 0);
        let call_graph_count = index
            .callables
            .values()
            .filter(|c| c.higher_order_call_graph.is_some())
            .count();
        assert!(call_graph_count > 0);
    }

    #[test]
    fn test_index_persistence() {
        let dir = test_data_dir();
        // Create a temporary directory to avoid writing .pysa-index.db into test_data.
        let temp_dir = tempfile::tempdir().unwrap();
        for entry in std::fs::read_dir(&dir).unwrap() {
            let entry = entry.unwrap();
            let dest = temp_dir.path().join(entry.file_name());
            std::fs::copy(entry.path(), &dest).unwrap();
        }

        // First call builds the index.
        let db1 = index::open_or_build_index(temp_dir.path()).unwrap();
        assert!(temp_dir.path().join("model-explorer-index.db").exists());

        // Verify data is accessible via the database.
        let callables1 = db1
            .search_callables(&regex::Regex::new(".*").unwrap())
            .unwrap();
        assert!(!callables1.is_empty());

        // Drop the first connection so the file is not held open.
        drop(db1);

        // Second call loads from cache.
        let db2 = index::open_or_build_index(temp_dir.path()).unwrap();
        let callables2 = db2
            .search_callables(&regex::Regex::new(".*").unwrap())
            .unwrap();
        assert_eq!(callables1.len(), callables2.len());
    }

    #[test]
    fn test_get_model() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        let callable = "integration_test.taint.source";
        let entry = index.callables.get(callable).expect("callable not found");
        let position = entry.model.as_ref().expect("no model");
        let data = index::read_entry_from_files(&index.files, position).unwrap();
        let model: types::Model = serde_json::from_value(data).unwrap();

        assert_eq!(model.callable, callable);
    }

    #[test]
    fn test_get_model_with_filtering() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        let callable = "integration_test.taint.sink";
        let entry = index.callables.get(callable).expect("callable not found");
        let position = entry.model.as_ref().expect("no model");
        let data = index::read_entry_from_files(&index.files, position).unwrap();
        let mut model: types::Model = serde_json::from_value(data).unwrap();

        // Filter to show only sinks, and strip all metadata.
        let options = ModelOptions {
            show_sources: false,
            show_sinks: true,
            show_tito: false,
            show_features: false,
            show_tito_positions: false,
            show_class_intervals: false,
            show_leaf_names: false,
            ..Default::default()
        };
        crate::model::filter_and_strip_model(&mut model, &options);

        assert!(model.sources.is_none());
        assert!(model.tito.is_none());
    }

    #[test]
    fn test_get_issues() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        let callable = "integration_test.assignments_to_sinks.test_direct";
        let entry = index.callables.get(callable).expect("callable not found");

        let mut issues: Vec<types::Issue> = Vec::new();
        for position in &entry.issues {
            let data = index::read_entry_from_files(&index.files, position).unwrap();
            let issue: types::Issue = serde_json::from_value(data).unwrap();
            issues.push(issue);
        }

        assert!(!issues.is_empty());
        for issue in &issues {
            assert_eq!(issue.callable, callable);
        }
    }

    #[test]
    fn test_get_issues_filter_by_code() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        let callable = "integration_test.assignments_to_sinks.test_direct";
        let entry = index.callables.get(callable).expect("callable not found");

        let mut issues: Vec<types::Issue> = Vec::new();
        for position in &entry.issues {
            let data = index::read_entry_from_files(&index.files, position).unwrap();
            let issue: types::Issue = serde_json::from_value(data).unwrap();
            issues.push(issue);
        }

        let options = IssueOptions {
            code: Some(5001),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        crate::issue::filter_issues(&mut issues, &options);

        for issue in &issues {
            assert_eq!(issue.code, 5001);
        }
    }

    #[test]
    fn test_get_issues_filter_by_nonexistent_code() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        let callable = "integration_test.assignments_to_sinks.test_direct";
        let entry = index.callables.get(callable).expect("callable not found");

        let mut issues: Vec<types::Issue> = Vec::new();
        for position in &entry.issues {
            let data = index::read_entry_from_files(&index.files, position).unwrap();
            let issue: types::Issue = serde_json::from_value(data).unwrap();
            issues.push(issue);
        }

        let options = IssueOptions {
            code: Some(99999),
            ..Default::default()
        };
        crate::issue::filter_issues(&mut issues, &options);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_get_call_graph() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();

        // Find a callable with a higher-order call graph.
        let callable = index
            .callables
            .iter()
            .find(|(_, v)| v.higher_order_call_graph.is_some())
            .map(|(k, _)| k.clone())
            .expect("no callable with higher-order call graph");

        let entry = index.callables.get(&callable).unwrap();
        let position = entry.higher_order_call_graph.as_ref().unwrap();
        let data = index::read_entry_from_files(&index.files, position).unwrap();
        let call_graph: types::CallGraph = serde_json::from_value(data).unwrap();

        assert_eq!(call_graph.callable, callable);
    }

    #[test]
    fn test_search() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();
        let regex = regex::Regex::new(r"integration_test\.taint\.").unwrap();

        let matches: Vec<&str> = index
            .callables
            .keys()
            .filter(|name| regex.is_match(name))
            .map(String::as_str)
            .collect();

        assert!(!matches.is_empty());
        for name in &matches {
            assert!(name.contains("integration_test.taint."));
        }
    }

    #[test]
    fn test_search_no_match() {
        let dir = test_data_dir();
        let index = index::build_index(&dir).unwrap();
        let regex = regex::Regex::new(r"^this_callable_does_not_exist_anywhere$").unwrap();

        let matches: Vec<&str> = index
            .callables
            .keys()
            .filter(|name| regex.is_match(name))
            .map(String::as_str)
            .collect();

        assert!(matches.is_empty());
    }
}
