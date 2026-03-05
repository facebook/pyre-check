/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use anyhow::Result;

use crate::types::CallGraph;
use crate::types::Condition;
use crate::types::Frame;
use crate::types::Issue;
use crate::types::Leaf;
use crate::types::LocalTaint;
use crate::types::Model;
use crate::types::Position;

/// Format a feature value as a string.
///
/// If the feature is a plain string, return it as-is.
/// If it is an object with a single key-value pair, return "key:value".
fn format_feature(feature: &serde_json::Value) -> String {
    match feature {
        serde_json::Value::String(s) => s.clone(),
        serde_json::Value::Object(map) if map.len() == 1 => {
            let (key, value) = map.iter().next().unwrap();
            match value {
                serde_json::Value::String(v) => format!("{key}:{v}"),
                _ => format!("{key}:{value}"),
            }
        }
        _ => format!("{feature}"),
    }
}

/// Format a leaf value as a string.
///
/// Returns the leaf name, optionally followed by `:port`.
fn format_leaf(leaf: &Leaf) -> String {
    match &leaf.port {
        Some(port) => format!("{}:{port}", leaf.name),
        None => leaf.name.clone(),
    }
}

/// Format a position as "filename:line:start", "path:line:start", or "line:start".
fn format_position(position: &Position) -> String {
    let filename = position.filename.as_deref();
    let path = position.path.as_deref();
    let line = position.line;
    let start = position.start;

    match (filename, path) {
        (Some(f), _) if f != "*" => format!("{f}:{line}:{start}"),
        (_, Some(p)) => format!("{p}:{line}:{start}"),
        _ => format!("{line}:{start}"),
    }
}

/// Format a tito position as "line:start:end".
fn format_tito_position(position: &Position) -> String {
    let line = position.line;
    let start = position.start;
    let end = position.end.unwrap_or(0);
    format!("{line}:{start}:{end}")
}

/// Format an issue's location as "filename:line:start".
fn format_issue_location(issue: &Issue) -> String {
    let filename = &issue.filename;
    let line = issue.line;
    let start = issue.start;

    if filename == "*" {
        format!("{line}:{start}")
    } else {
        format!("{filename}:{line}:{start}")
    }
}

/// Print call/origin/declaration/tito info from a local_taint entry.
fn print_call_info(local_taint: &LocalTaint, indent: &str) {
    if let Some(call) = &local_taint.call {
        println!("{indent}CalleePort: {}", call.port);
        for resolve_to in &call.resolves_to {
            println!("{indent}Callee: {resolve_to}");
        }
        let loc = format_position(&call.position);
        println!("{indent}Location: {loc}");
    } else if let Some(origin) = &local_taint.origin {
        let line = origin.line;
        let start = origin.start;
        println!("{indent}Origin: Location: {line}:{start}");
    } else if local_taint.declaration.is_some() {
        println!("{indent}Declaration:");
    } else if local_taint.tito.is_some() {
        println!("{indent}Tito:");
    }
}

/// Print local taint metadata: receiver_interval, caller_interval, is_self_call,
/// tito_positions, and local_features.
fn print_local_taint_metadata(local_taint: &LocalTaint, indent: &str) {
    if let Some(receiver_interval) = &local_taint.receiver_interval {
        println!("{indent}ReceiverInterval: {receiver_interval}");
    }
    if let Some(caller_interval) = &local_taint.caller_interval {
        println!("{indent}CallerInterval: {caller_interval}");
    }
    if let Some(is_self_call) = local_taint.is_self_call {
        println!("{indent}IsSelfCall: {is_self_call}");
    }
    if let Some(tito_positions) = &local_taint.tito_positions {
        let positions: Vec<String> = tito_positions.iter().map(format_tito_position).collect();
        println!("{indent}TitoPositions: {}", positions.join(", "));
    }
    if let Some(local_features) = &local_taint.local_features {
        let features: Vec<String> = local_features.iter().map(format_feature).collect();
        println!("{indent}LocalFeatures: {}", features.join(", "));
    }
}

/// Print a single taint kind frame: kind, distance, return_paths, features, and leaves.
fn print_frame(frame: &Frame, indent: &str) {
    let kind = &frame.kind;
    let distance = frame.length.unwrap_or(0);

    if let Some(return_paths) = &frame.return_paths {
        for (path, collapse_depth) in return_paths {
            println!(
                "{indent}{kind}: ReturnPath {path} CollapseDepth {collapse_depth} Distance {distance}"
            );
        }
    } else {
        println!("{indent}{kind}: Distance {distance}");
    }

    if let Some(features) = &frame.features {
        let feature_strs: Vec<String> = features.iter().map(format_feature).collect();
        println!("{indent}  Features: {}", feature_strs.join(", "));
    }

    if let Some(leaves) = &frame.leaves {
        let leaf_strs: Vec<String> = leaves.iter().map(format_leaf).collect();
        println!("{indent}  Leaves: {}", leaf_strs.join(", "));
    }
}

/// Print taint conditions (sources, sinks, or tito).
fn print_taint_conditions(conditions: &[Condition], is_tito: bool) {
    for condition in conditions {
        let label = if is_tito {
            "ParameterPath"
        } else {
            "CallerPort"
        };
        let port = &condition.port;
        println!("  {label}: {port}");

        for local_taint in &condition.taint {
            print_call_info(local_taint, "    ");
            print_local_taint_metadata(local_taint, "    ");

            for frame in &local_taint.kinds {
                print_frame(frame, "      ");
            }
        }
    }
}

/// Print a taint model as pretty-printed JSON.
pub fn print_model_json(model: &Model) -> Result<()> {
    println!("{}", serde_json::to_string_pretty(model)?);
    Ok(())
}

/// Print a taint model as human-readable text.
pub fn print_model_text(model: &Model) -> Result<()> {
    println!("Model for {}", model.callable);

    // Location: filename or path
    let filename = model.filename.as_deref().unwrap_or("");
    let path = model.path.as_deref();
    if filename == "*" {
        if let Some(p) = path {
            println!("Location: {p}");
        } else {
            println!("Location: {filename}");
        }
    } else {
        println!("Location: {filename}");
    }

    if let Some(sources) = &model.sources {
        println!("Sources:");
        print_taint_conditions(sources, false);
    }

    if let Some(sinks) = &model.sinks {
        println!("Sinks:");
        print_taint_conditions(sinks, false);
    }

    if let Some(tito) = &model.tito {
        println!("Tito:");
        print_taint_conditions(tito, true);
    }

    if let Some(global_sanitizer) = &model.global_sanitizer {
        println!("GlobalSanitizers: {global_sanitizer}");
    }
    if let Some(parameters_sanitizer) = &model.parameters_sanitizer {
        println!("ParametersSanitizer: {parameters_sanitizer}");
    }
    if let Some(sanitizers) = &model.sanitizers {
        println!("Sanitizers: {sanitizers}");
    }
    if let Some(modes) = &model.modes {
        println!("Modes: {}", modes.join(", "));
    }

    Ok(())
}

/// Print a list of issues as pretty-printed JSON.
pub fn print_issues_json(issues: &[Issue]) -> Result<()> {
    println!("{}", serde_json::to_string_pretty(&issues)?);
    Ok(())
}

/// Print a list of issues as human-readable text.
pub fn print_issues_text(issues: &[Issue]) -> Result<()> {
    for issue in issues {
        println!("Issue:");
        println!("  Code: {}", issue.code);

        let location = format_issue_location(issue);
        println!("  Location: {location}");

        println!("  Message: {}", issue.message);
        println!("  Handle: {}", issue.master_handle);

        for trace in &issue.traces {
            let capitalized = capitalize(&trace.name);
            println!("  {capitalized}:");

            for local_taint in &trace.roots {
                print_call_info(local_taint, "    ");
                print_local_taint_metadata(local_taint, "    ");

                for frame in &local_taint.kinds {
                    print_frame(frame, "      ");
                }
            }
        }
    }

    Ok(())
}

/// Print a call graph entry as pretty-printed JSON.
pub fn print_call_graph_json(call_graph: &CallGraph) -> Result<()> {
    println!("{}", serde_json::to_string_pretty(call_graph)?);
    Ok(())
}

/// Print a call graph entry as human-readable text.
pub fn print_call_graph_text(call_graph: &CallGraph) -> Result<()> {
    println!("Call graph for {}", call_graph.callable);

    if let Some(returned) = &call_graph.returned_callables {
        let returned_strs: Vec<String> = returned
            .iter()
            .map(|v| {
                v.as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("{v}"))
            })
            .collect();
        println!("Returned callables: {}", returned_strs.join(", "));
    }

    println!("Calls:");
    if let Some(calls) = &call_graph.calls {
        for (location, details) in calls {
            println!("  {location}:");

            if let Some(targets) = details.get("targets").and_then(|v| v.as_array()) {
                for target in targets {
                    if let Some(s) = target.as_str() {
                        println!("    {s}");
                    } else {
                        println!("    {target}");
                    }
                }
            }
            // The call graph format uses nested objects (call/define/attribute_access),
            // so just print the JSON for each entry if it doesn't have simple targets.
            if details.get("targets").is_none() {
                println!("    {details}");
            }
        }
    }

    Ok(())
}

/// Capitalize the first letter of a string.
fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => {
            let upper: String = first.to_uppercase().collect();
            upper + chars.as_str()
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_format_feature_string() {
        let feature = json!("always-via-special_source");
        assert_eq!(format_feature(&feature), "always-via-special_source");
    }

    #[test]
    fn test_format_feature_object() {
        let feature = json!({"via": "tito"});
        assert_eq!(format_feature(&feature), "via:tito");
    }

    #[test]
    fn test_format_leaf_name_only() {
        let leaf = Leaf {
            name: "UserControlled".to_string(),
            port: None,
        };
        assert_eq!(format_leaf(&leaf), "UserControlled");
    }

    #[test]
    fn test_format_leaf_with_port() {
        let leaf = Leaf {
            name: "UserControlled".to_string(),
            port: Some("formal(x)".to_string()),
        };
        assert_eq!(format_leaf(&leaf), "UserControlled:formal(x)");
    }

    #[test]
    fn test_format_position_with_filename() {
        let pos = Position {
            filename: Some("foo.py".to_string()),
            path: None,
            line: 10,
            start: 5,
            end: None,
        };
        assert_eq!(format_position(&pos), "foo.py:10:5");
    }

    #[test]
    fn test_format_position_star_with_path() {
        let pos = Position {
            filename: Some("*".to_string()),
            path: Some("/src/foo.py".to_string()),
            line: 3,
            start: 1,
            end: None,
        };
        assert_eq!(format_position(&pos), "/src/foo.py:3:1");
    }

    #[test]
    fn test_format_position_no_filename() {
        let pos = Position {
            filename: None,
            path: None,
            line: 42,
            start: 7,
            end: None,
        };
        assert_eq!(format_position(&pos), "42:7");
    }

    #[test]
    fn test_capitalize() {
        assert_eq!(capitalize("source"), "Source");
        assert_eq!(capitalize("sink"), "Sink");
        assert_eq!(capitalize(""), "");
    }
}
