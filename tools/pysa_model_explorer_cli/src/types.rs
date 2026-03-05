/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;

/// A position within source code: line, start column, and optional end column.
///
/// Also used for locations that may include a filename or path (e.g., call positions,
/// issue locations). When filename/path are absent, only line/start/end are meaningful.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub filename: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub path: Option<String>,

    pub line: i64,
    pub start: i64,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub end: Option<i64>,
}

/// A leaf in a taint trace (source or sink).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Leaf {
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub port: Option<String>,
}

/// A single taint kind frame within a local taint entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frame {
    pub kind: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub length: Option<i64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub features: Option<Vec<serde_json::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub leaves: Option<Vec<Leaf>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub return_paths: Option<serde_json::Map<String, serde_json::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub via_features: Option<Vec<serde_json::Value>>,
}

/// Information about a call site within a taint trace.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallInfo {
    pub position: Position,
    pub resolves_to: Vec<String>,
    pub port: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub call_site: Option<String>,
}

/// A local taint entry within a condition or trace root.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalTaint {
    pub kinds: Vec<Frame>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub local_features: Option<Vec<serde_json::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub tito_positions: Option<Vec<Position>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub receiver_interval: Option<serde_json::Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub caller_interval: Option<serde_json::Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub is_self_call: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub call: Option<CallInfo>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub origin: Option<Position>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub declaration: Option<serde_json::Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub tito: Option<serde_json::Value>,
}

/// A taint condition: a port with associated local taint entries.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    pub port: String,
    pub taint: Vec<LocalTaint>,
}

/// A taint model for a callable.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Model {
    pub callable: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub filename: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub callable_line: Option<i64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub path: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub sources: Option<Vec<Condition>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub sinks: Option<Vec<Condition>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub tito: Option<Vec<Condition>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub modes: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub global_sanitizer: Option<serde_json::Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub parameters_sanitizer: Option<serde_json::Value>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub sanitizers: Option<serde_json::Value>,
}

/// A trace within an issue (e.g., "forward" or "backward").
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trace {
    pub name: String,
    pub roots: Vec<LocalTaint>,
}

/// An issue found by Pysa.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Issue {
    pub callable: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub callable_line: Option<i64>,

    pub code: i64,
    pub line: i64,
    pub start: i64,
    pub end: i64,
    pub filename: String,
    pub message: String,
    pub traces: Vec<Trace>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub features: Option<Vec<serde_json::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub sink_handle: Option<serde_json::Value>,

    pub master_handle: String,
}

/// A higher-order call graph entry for a callable.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallGraph {
    pub callable: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub filename: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub returned_callables: Option<Vec<serde_json::Value>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub calls: Option<serde_json::Map<String, serde_json::Value>>,
}
