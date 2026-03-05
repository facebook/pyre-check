/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::Issue;

/// Options controlling which issues to include and what metadata to show.
#[derive(Debug, Clone, Default)]
pub struct IssueOptions {
    /// If set, only include issues with this code.
    pub code: Option<i64>,
    /// If set, only include issues whose master_handle contains this substring.
    pub handle: Option<String>,
    /// Include feature annotations in output.
    pub show_features: bool,
    /// Include TITO position information in output.
    pub show_tito_positions: bool,
    /// Include class interval information in output.
    pub show_class_intervals: bool,
    /// Include leaf name information in output.
    pub show_leaf_names: bool,
}

/// Strip metadata fields from a single issue's traces based on the given options.
fn strip_issue(issue: &mut Issue, options: &IssueOptions) {
    if !options.show_features {
        issue.features = None;
    }

    for trace in issue.traces.iter_mut() {
        for root in trace.roots.iter_mut() {
            if !options.show_tito_positions {
                root.tito_positions = None;
            }

            if !options.show_class_intervals {
                root.receiver_interval = None;
                root.caller_interval = None;
                root.is_self_call = None;
            }

            if !options.show_features {
                root.local_features = None;
            }

            for frame in root.kinds.iter_mut() {
                if !options.show_features {
                    frame.features = None;
                }
                if !options.show_leaf_names {
                    frame.leaves = None;
                }
            }
        }
    }
}

/// Filter a list of issues according to the given options.
///
/// Removes issues that do not match the filter criteria and strips metadata
/// fields that the caller has not requested, modifying `issues` in place.
pub fn filter_issues(issues: &mut Vec<Issue>, options: &IssueOptions) {
    if let Some(code) = options.code {
        issues.retain(|issue| issue.code == code);
    }

    if let Some(ref handle) = options.handle {
        issues.retain(|issue| issue.master_handle.contains(handle.as_str()));
    }

    for issue in issues.iter_mut() {
        strip_issue(issue, options);
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn sample_issue() -> Issue {
        serde_json::from_value(json!({
            "callable": "example.flow",
            "callable_line": 7,
            "code": 5002,
            "line": 9,
            "start": 15,
            "end": 16,
            "filename": "example.py",
            "message": "Data from [Test] source(s) may reach [Test] sink(s)",
            "traces": [
                {
                    "name": "forward",
                    "roots": [
                        {
                            "kinds": [
                                {
                                    "features": [{"always-via": "special_source"}],
                                    "leaves": [{"name": "pysa._test_source", "port": "leaf:return"}],
                                    "length": 1,
                                    "kind": "Test"
                                }
                            ],
                            "local_features": [{"has": "first-index"}],
                            "tito_positions": [{"line": 8, "start": 4, "end": 20}],
                            "receiver_interval": [{"lower": 1, "upper": 2}],
                            "caller_interval": [{"lower": 0, "upper": 10}],
                            "is_self_call": false,
                            "call": {
                                "position": {"line": 8, "start": 8, "end": 20},
                                "resolves_to": ["example.get_source"],
                                "port": "result"
                            }
                        }
                    ]
                },
                {
                    "name": "backward",
                    "roots": [
                        {
                            "kinds": [
                                {
                                    "features": [{"always-via": "special_sink"}],
                                    "leaves": [{"name": "pysa._test_sink", "port": "leaf:arg"}],
                                    "kind": "Test"
                                }
                            ],
                            "local_features": [{"always-via": "tito"}],
                            "origin": {"line": 9, "start": 15, "end": 16}
                        }
                    ]
                }
            ],
            "features": [
                {"always-via": "special_source"},
                {"always-via": "special_sink"}
            ],
            "sink_handle": {
                "kind": "Call",
                "callee": "pysa._test_sink",
                "index": 0,
                "parameter": "formal(arg)"
            },
            "master_handle": "example.flow:5002:0:Call|pysa._test_sink|0|formal(arg):abcdef"
        }))
        .unwrap()
    }

    fn sample_issue_other_code() -> Issue {
        serde_json::from_value(json!({
            "callable": "example.other",
            "code": 6001,
            "line": 1,
            "start": 1,
            "end": 1,
            "filename": "other.py",
            "message": "other issue",
            "traces": [],
            "features": [],
            "master_handle": "example.other:6001:0:Return:deadbeef"
        }))
        .unwrap()
    }

    #[test]
    fn test_filter_by_code() {
        let mut issues = vec![sample_issue(), sample_issue_other_code()];
        let options = IssueOptions {
            code: Some(5002),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].code, 5002);
    }

    #[test]
    fn test_filter_by_code_no_match() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            code: Some(9999),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_filter_by_handle() {
        let mut issues = vec![sample_issue(), sample_issue_other_code()];
        let options = IssueOptions {
            handle: Some("_test_sink".to_string()),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].callable, "example.flow");
    }

    #[test]
    fn test_filter_by_handle_no_match() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            handle: Some("nonexistent".to_string()),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        assert!(issues.is_empty());
    }

    #[test]
    fn test_strip_features() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: false,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        let issue = &issues[0];

        // Top-level features removed.
        assert!(issue.features.is_none());

        // local_features removed from roots.
        let forward_root = &issue.traces[0].roots[0];
        assert!(forward_root.local_features.is_none());

        // features removed from frames.
        let frame = &forward_root.kinds[0];
        assert!(frame.features.is_none());

        // leaves should still be present.
        assert!(frame.leaves.is_some());
    }

    #[test]
    fn test_strip_tito_positions() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: true,
            show_tito_positions: false,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        let forward_root = &issues[0].traces[0].roots[0];
        assert!(forward_root.tito_positions.is_none());
        // Other fields remain.
        assert!(forward_root.local_features.is_some());
    }

    #[test]
    fn test_strip_class_intervals() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: false,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        let forward_root = &issues[0].traces[0].roots[0];
        assert!(forward_root.receiver_interval.is_none());
        assert!(forward_root.caller_interval.is_none());
        assert!(forward_root.is_self_call.is_none());
        // Other fields remain.
        assert!(forward_root.local_features.is_some());
    }

    #[test]
    fn test_strip_leaf_names() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: false,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        let frame = &issues[0].traces[0].roots[0].kinds[0];
        assert!(frame.leaves.is_none());
        // features should still be present.
        assert!(frame.features.is_some());
    }

    #[test]
    fn test_strip_all_metadata() {
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: false,
            show_tito_positions: false,
            show_class_intervals: false,
            show_leaf_names: false,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        let issue = &issues[0];

        assert!(issue.features.is_none());

        let forward_root = &issue.traces[0].roots[0];
        assert!(forward_root.local_features.is_none());
        assert!(forward_root.tito_positions.is_none());
        assert!(forward_root.receiver_interval.is_none());
        assert!(forward_root.caller_interval.is_none());
        assert!(forward_root.is_self_call.is_none());

        let frame = &forward_root.kinds[0];
        assert!(frame.features.is_none());
        assert!(frame.leaves.is_none());

        // Core fields remain intact.
        assert_eq!(frame.kind, "Test");
        assert_eq!(issue.code, 5002);
        assert_eq!(issue.callable, "example.flow");
    }

    #[test]
    fn test_filter_code_and_handle_combined() {
        let mut issues = vec![sample_issue(), sample_issue_other_code()];
        let options = IssueOptions {
            code: Some(5002),
            handle: Some("_test_sink".to_string()),
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
        };
        filter_issues(&mut issues, &options);
        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].code, 5002);
    }

    #[test]
    fn test_no_filtering_preserves_all() {
        let original = sample_issue();
        let mut issues = vec![sample_issue()];
        let options = IssueOptions {
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        // Compare the serialized JSON to ensure equality.
        assert_eq!(
            serde_json::to_value(&issues[0]).unwrap(),
            serde_json::to_value(&original).unwrap()
        );
    }

    #[test]
    fn test_empty_issues_vec() {
        let mut issues: Vec<Issue> = vec![];
        let options = IssueOptions {
            code: Some(5002),
            ..Default::default()
        };
        filter_issues(&mut issues, &options);
        assert!(issues.is_empty());
    }
}
