/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::CallCallees;
use crate::types::CallGraph;
use crate::types::CallTarget;
use crate::types::ExpressionCallees;

/// Options controlling which fields to show in call graph target entries.
#[derive(Debug, Clone, Default)]
pub struct CallGraphOptions {
    /// Show the `index` field in call graph entries.
    pub show_index: bool,
    /// Show the `return_type` field in call graph entries.
    pub show_return_type: bool,
    /// Show the `receiver_class` field in call graph entries.
    pub show_receiver_type: bool,
    /// Only show call graph edges starting at or after this line.
    pub start_line: Option<i64>,
    /// Only show call graph edges starting at or before this line.
    pub end_line: Option<i64>,
}

/// Parsed line range from a call graph location key.
struct LocationLineRange {
    line_start: i64,
    line_end: i64,
}

/// Parse the line range from a call graph location key.
///
/// The key format is `line_start:col_start-line_end:col_end`, optionally
/// followed by a `|` and a suffix that should be ignored.
/// For example: `"26:8-26:15"` or `"30:9-30:27|artificial-call|subscript-get-item"`.
fn parse_location_key(key: &str) -> Option<LocationLineRange> {
    // Strip any suffix after the first `|`.
    let location_part = key.split('|').next().unwrap_or(key);

    // Split into start and end parts on `-`.
    let (start_part, end_part) = location_part.split_once('-')?;

    // Parse line_start from `line_start:col_start`.
    let line_start: i64 = start_part.split(':').next()?.parse().ok()?;

    // Parse line_end from `line_end:col_end`.
    let line_end: i64 = end_part.split(':').next()?.parse().ok()?;

    Some(LocationLineRange {
        line_start,
        line_end,
    })
}

/// Check whether a location key falls within the given line range.
///
/// An edge is within the range if its line span overlaps with
/// `[start_line, end_line]`. If the key cannot be parsed, the edge is kept.
fn is_location_in_line_range(key: &str, start_line: Option<i64>, end_line: Option<i64>) -> bool {
    if start_line.is_none() && end_line.is_none() {
        return true;
    }

    let range = match parse_location_key(key) {
        Some(r) => r,
        None => return true, // Keep entries we cannot parse.
    };

    if let Some(start) = start_line {
        if range.line_end < start {
            return false;
        }
    }
    if let Some(end) = end_line {
        if range.line_start > end {
            return false;
        }
    }

    true
}

/// Strip unwanted fields from a call graph according to the given options.
pub fn strip_call_graph(call_graph: &mut CallGraph, options: &CallGraphOptions) {
    if let Some(returned) = &mut call_graph.returned_callables {
        strip_call_targets(returned, options);
    }

    if let Some(calls) = &mut call_graph.calls {
        // Filter by line range before stripping fields.
        if options.start_line.is_some() || options.end_line.is_some() {
            calls.retain(|key, _| {
                is_location_in_line_range(key, options.start_line, options.end_line)
            });
        }

        for (_location, callees) in calls.iter_mut() {
            strip_expression_callees(callees, options);
        }
    }
}

fn strip_call_target(target: &mut CallTarget, options: &CallGraphOptions) {
    if !options.show_index {
        target.index = None;
    }
    if !options.show_return_type {
        target.return_type = None;
    }
    if !options.show_receiver_type {
        target.receiver_class = None;
    }
}

fn strip_call_targets(targets: &mut [CallTarget], options: &CallGraphOptions) {
    for target in targets.iter_mut() {
        strip_call_target(target, options);
    }
}

fn strip_expression_callees(callees: &mut ExpressionCallees, options: &CallGraphOptions) {
    match callees {
        ExpressionCallees::Call(call) => strip_call_callees(call, options),
        ExpressionCallees::AttributeAccess(aa) => {
            if let Some(props) = &mut aa.properties {
                strip_call_targets(props, options);
            }
            if let Some(globals) = &mut aa.globals {
                strip_call_targets(globals, options);
            }
            if let Some(if_called) = &mut aa.if_called {
                strip_call_callees(if_called, options);
            }
        }
        ExpressionCallees::Identifier(id) => {
            if let Some(globals) = &mut id.globals {
                strip_call_targets(globals, options);
            }
            if let Some(if_called) = &mut id.if_called {
                strip_call_callees(if_called, options);
            }
        }
        ExpressionCallees::Define(def) => {
            if let Some(targets) = &mut def.define_targets {
                strip_call_targets(targets, options);
            }
            if let Some(targets) = &mut def.decorated_targets {
                strip_call_targets(targets, options);
            }
        }
        ExpressionCallees::FormatStringArtificial(targets) => {
            strip_call_targets(targets, options);
        }
        ExpressionCallees::FormatStringStringify(targets) => {
            strip_call_targets(targets, options);
        }
        ExpressionCallees::Return(_) => {}
    }
}

fn strip_call_callees(call: &mut CallCallees, options: &CallGraphOptions) {
    if let Some(targets) = &mut call.calls {
        strip_call_targets(targets, options);
    }
    if let Some(targets) = &mut call.new_calls {
        strip_call_targets(targets, options);
    }
    if let Some(targets) = &mut call.init_calls {
        strip_call_targets(targets, options);
    }
    if let Some(targets) = &mut call.decorated_targets {
        strip_call_targets(targets, options);
    }
    if let Some(params) = &mut call.higher_order_parameters {
        for param in params.iter_mut() {
            if let Some(targets) = &mut param.calls {
                strip_call_targets(targets, options);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn sample_call_graph() -> CallGraph {
        serde_json::from_value(json!({
            "callable": "test.callable",
            "returned_callables": [
                {"target": "foo", "index": 0, "return_type": ["float"]},
                {"target": "bar", "index": 1, "return_type": [], "receiver_class": "MyClass"}
            ],
            "calls": {
                "10:5:20": {
                    "call": {
                        "calls": [
                            {"target": "baz", "index": 0, "return_type": ["int"], "implicit_receiver": true, "receiver_class": "dict"}
                        ]
                    }
                },
                "15:3:10": {
                    "define": {
                        "define_targets": [
                            {"target": "qux", "index": 2, "return_type": []}
                        ]
                    }
                }
            }
        }))
        .unwrap()
    }

    #[test]
    fn test_strip_all_by_default() {
        let mut cg = sample_call_graph();
        let options = CallGraphOptions::default();
        strip_call_graph(&mut cg, &options);

        let returned = cg.returned_callables.as_ref().unwrap();
        for entry in returned {
            assert!(entry.index.is_none());
            assert!(entry.return_type.is_none());
            assert!(entry.receiver_class.is_none());
        }

        let calls = cg.calls.as_ref().unwrap();
        let call_entry = &calls["10:5:20"];
        if let ExpressionCallees::Call(call) = call_entry {
            let targets = call.calls.as_ref().unwrap();
            for target in targets {
                assert!(target.index.is_none());
                assert!(target.return_type.is_none());
                assert!(target.receiver_class.is_none());
            }
        } else {
            panic!("Expected Call variant");
        }
    }

    #[test]
    fn test_show_all_fields() {
        let mut cg = sample_call_graph();
        let options = CallGraphOptions {
            show_index: true,
            show_return_type: true,
            show_receiver_type: true,
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let returned = cg.returned_callables.as_ref().unwrap();
        let first = &returned[0];
        assert!(first.index.is_some());
        assert!(first.return_type.is_some());

        let second = &returned[1];
        assert!(second.receiver_class.is_some());
    }

    #[test]
    fn test_show_index_only() {
        let mut cg = sample_call_graph();
        let options = CallGraphOptions {
            show_index: true,
            show_return_type: false,
            show_receiver_type: false,
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let returned = cg.returned_callables.as_ref().unwrap();
        for entry in returned {
            assert!(entry.index.is_some());
            assert!(entry.return_type.is_none());
            assert!(entry.receiver_class.is_none());
        }
    }

    #[test]
    fn test_show_return_type_only() {
        let mut cg = sample_call_graph();
        let options = CallGraphOptions {
            show_index: false,
            show_return_type: true,
            show_receiver_type: false,
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let returned = cg.returned_callables.as_ref().unwrap();
        for entry in returned {
            assert!(entry.index.is_none());
            assert!(entry.return_type.is_some());
            assert!(entry.receiver_class.is_none());
        }
    }

    #[test]
    fn test_show_receiver_type_only() {
        let mut cg = sample_call_graph();
        let options = CallGraphOptions {
            show_index: false,
            show_return_type: false,
            show_receiver_type: true,
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let returned = cg.returned_callables.as_ref().unwrap();
        let second = &returned[1];
        assert!(second.index.is_none());
        assert!(second.return_type.is_none());
        assert!(second.receiver_class.is_some());
    }

    #[test]
    fn test_strip_with_no_returned_callables() {
        let mut cg: CallGraph = serde_json::from_value(json!({
            "callable": "test.no_returned",
            "calls": {
                "1:1:1": {
                    "call": {
                        "calls": [
                            {"target": "x", "index": 0, "return_type": []}
                        ]
                    }
                }
            }
        }))
        .unwrap();

        let options = CallGraphOptions::default();
        strip_call_graph(&mut cg, &options);

        assert!(cg.returned_callables.is_none());
        let calls = cg.calls.as_ref().unwrap();
        if let ExpressionCallees::Call(call) = &calls["1:1:1"] {
            let target = &call.calls.as_ref().unwrap()[0];
            assert!(target.index.is_none());
        } else {
            panic!("Expected Call variant");
        }
    }

    #[test]
    fn test_strip_with_no_calls() {
        let mut cg: CallGraph = serde_json::from_value(json!({
            "callable": "test.no_calls",
            "returned_callables": [
                {"target": "y", "index": 5, "return_type": ["str"]}
            ]
        }))
        .unwrap();

        let options = CallGraphOptions::default();
        strip_call_graph(&mut cg, &options);

        assert!(cg.calls.is_none());
        let returned = cg.returned_callables.as_ref().unwrap();
        let target = &returned[0];
        assert!(target.index.is_none());
        assert!(target.return_type.is_none());
    }

    fn sample_call_graph_with_lines() -> CallGraph {
        serde_json::from_value(json!({
            "callable": "test.callable",
            "calls": {
                "5:1-5:10": {
                    "call": {
                        "calls": [
                            {"target": "a", "index": 0, "return_type": []}
                        ]
                    }
                },
                "10:1-10:20": {
                    "call": {
                        "calls": [
                            {"target": "b", "index": 0, "return_type": []}
                        ]
                    }
                },
                "12:4-14:15": {
                    "define": {
                        "define_targets": [
                            {"target": "c", "index": 0, "return_type": []}
                        ]
                    }
                },
                "25:1-25:10": {
                    "call": {
                        "calls": [
                            {"target": "d", "index": 0, "return_type": []}
                        ]
                    }
                }
            }
        }))
        .unwrap()
    }

    #[test]
    fn test_parse_location_key_simple() {
        let range = parse_location_key("26:8-26:15").unwrap();
        assert_eq!(range.line_start, 26);
        assert_eq!(range.line_end, 26);
    }

    #[test]
    fn test_parse_location_key_multiline() {
        let range = parse_location_key("12:4-14:15").unwrap();
        assert_eq!(range.line_start, 12);
        assert_eq!(range.line_end, 14);
    }

    #[test]
    fn test_parse_location_key_with_suffix() {
        let range = parse_location_key("30:9-30:27|artificial-call|subscript-get-item").unwrap();
        assert_eq!(range.line_start, 30);
        assert_eq!(range.line_end, 30);
    }

    #[test]
    fn test_parse_location_key_invalid() {
        assert!(parse_location_key("invalid").is_none());
        assert!(parse_location_key("").is_none());
    }

    #[test]
    fn test_filter_by_line_range() {
        let mut cg = sample_call_graph_with_lines();
        let options = CallGraphOptions {
            start_line: Some(10),
            end_line: Some(20),
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 2);
        assert!(calls.contains_key("10:1-10:20"));
        assert!(calls.contains_key("12:4-14:15"));
        assert!(!calls.contains_key("5:1-5:10"));
        assert!(!calls.contains_key("25:1-25:10"));
    }

    #[test]
    fn test_filter_by_start_line_only() {
        let mut cg = sample_call_graph_with_lines();
        let options = CallGraphOptions {
            start_line: Some(12),
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 2);
        assert!(calls.contains_key("12:4-14:15"));
        assert!(calls.contains_key("25:1-25:10"));
    }

    #[test]
    fn test_filter_by_end_line_only() {
        let mut cg = sample_call_graph_with_lines();
        let options = CallGraphOptions {
            end_line: Some(11),
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 2);
        assert!(calls.contains_key("5:1-5:10"));
        assert!(calls.contains_key("10:1-10:20"));
    }

    #[test]
    fn test_filter_with_suffix_in_key() {
        let mut cg: CallGraph = serde_json::from_value(json!({
            "callable": "test.callable",
            "calls": {
                "10:1-10:20": {
                    "call": {
                        "calls": [
                            {"target": "a", "index": 0, "return_type": []}
                        ]
                    }
                },
                "20:5-20:15|artificial-call|subscript-get-item": {
                    "call": {
                        "calls": [
                            {"target": "b", "index": 0, "return_type": []}
                        ]
                    }
                },
                "30:1-30:10": {
                    "call": {
                        "calls": [
                            {"target": "c", "index": 0, "return_type": []}
                        ]
                    }
                }
            }
        }))
        .unwrap();

        let options = CallGraphOptions {
            start_line: Some(15),
            end_line: Some(25),
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 1);
        assert!(calls.contains_key("20:5-20:15|artificial-call|subscript-get-item"));
    }

    #[test]
    fn test_no_line_filter() {
        let mut cg = sample_call_graph_with_lines();
        let options = CallGraphOptions::default();
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 4);
    }

    #[test]
    fn test_filter_multiline_span_overlapping_range() {
        // Edge spans lines 12-14, range is 13-13. Should be kept (overlap).
        let mut cg = sample_call_graph_with_lines();
        let options = CallGraphOptions {
            start_line: Some(13),
            end_line: Some(13),
            ..Default::default()
        };
        strip_call_graph(&mut cg, &options);

        let calls = cg.calls.as_ref().unwrap();
        assert_eq!(calls.len(), 1);
        assert!(calls.contains_key("12:4-14:15"));
    }
}
