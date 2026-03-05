/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::Model;

/// Options controlling which parts of a taint model to include.
#[derive(Debug, Clone, Default)]
pub struct ModelOptions {
    /// Show source taint (default: true when `sections` is empty).
    pub show_sources: bool,
    /// Show sink taint (default: true when `sections` is empty).
    pub show_sinks: bool,
    /// Show taint-in-taint-out (default: true when `sections` is empty).
    pub show_tito: bool,
    /// If set, only include taint entries matching this kind.
    pub kind: Option<String>,
    /// If set, only include taint entries matching this caller port.
    pub caller_port: Option<String>,
    /// Include feature annotations in output.
    pub show_features: bool,
    /// Include TITO position information in output.
    pub show_tito_positions: bool,
    /// Include class interval information in output.
    pub show_class_intervals: bool,
    /// Include leaf name information in output.
    pub show_leaf_names: bool,
}

/// Filter and strip a taint model according to the given options.
///
/// Removes sections, taint kinds, and metadata fields that the caller has not
/// requested, modifying `model` in place.
pub fn filter_and_strip_model(model: &mut Model, options: &ModelOptions) {
    // Step 1: Category filtering — remove entire sections the caller doesn't want.
    if !options.show_sources {
        model.sources = None;
    }
    if !options.show_sinks {
        model.sinks = None;
    }
    if !options.show_tito {
        model.tito = None;
    }

    // Steps 2–4: Process each remaining taint section.
    for conditions in [&mut model.sources, &mut model.sinks, &mut model.tito] {
        if let Some(conditions) = conditions {
            filter_conditions(conditions, options);
            strip_conditions(conditions, options);
        }
    }
}

/// Filter conditions by kind and caller port, removing empty entries up the chain.
fn filter_conditions(conditions: &mut Vec<crate::types::Condition>, options: &ModelOptions) {
    // Filter by kind within each condition's taint entries.
    if let Some(target_kind) = &options.kind {
        for condition in conditions.iter_mut() {
            filter_condition_by_kind(condition, target_kind);
        }
    }

    // Filter by caller port — keep only conditions whose port matches.
    if let Some(target_port) = &options.caller_port {
        conditions.retain(|condition| condition.port == *target_port);
    }

    // Remove conditions left with an empty taint array after kind filtering.
    if options.kind.is_some() {
        conditions.retain(|condition| !condition.taint.is_empty());
    }
}

/// Remove frames from a single condition that do not match `target_kind`.
///
/// Walks condition → taint[] → kinds[] and retains only frames whose `kind`
/// field equals `target_kind`. Removes local_taint entries whose kinds array
/// becomes empty.
fn filter_condition_by_kind(condition: &mut crate::types::Condition, target_kind: &str) {
    for local_taint in condition.taint.iter_mut() {
        local_taint.kinds.retain(|frame| frame.kind == target_kind);
    }

    // Remove local_taint entries whose kinds array is now empty.
    condition
        .taint
        .retain(|local_taint| !local_taint.kinds.is_empty());
}

/// Strip metadata fields from all taint conditions based on the display options.
fn strip_conditions(conditions: &mut [crate::types::Condition], options: &ModelOptions) {
    for condition in conditions.iter_mut() {
        for local_taint in condition.taint.iter_mut() {
            strip_local_taint(local_taint, options);
        }
    }
}

/// Strip metadata fields from a single local_taint object and its child frames.
fn strip_local_taint(local_taint: &mut crate::types::LocalTaint, options: &ModelOptions) {
    if !options.show_features {
        local_taint.local_features = None;
    }

    if !options.show_tito_positions {
        local_taint.tito_positions = None;
    }

    if !options.show_class_intervals {
        local_taint.receiver_interval = None;
        local_taint.caller_interval = None;
        local_taint.is_self_call = None;
    }

    // Strip fields from each frame in the kinds array.
    for frame in local_taint.kinds.iter_mut() {
        strip_frame(frame, options);
    }
}

/// Strip metadata fields from a single frame (element of a `kinds` array).
fn strip_frame(frame: &mut crate::types::Frame, options: &ModelOptions) {
    if !options.show_features {
        frame.features = None;
    }

    if !options.show_leaf_names {
        frame.leaves = None;
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn sample_model() -> Model {
        serde_json::from_value(json!({
            "callable": "test.callable",
            "sources": [
                {
                    "port": "formal(x)",
                    "taint": [
                        {
                            "kinds": [
                                {
                                    "kind": "UserControlled",
                                    "features": ["f1"],
                                    "leaves": [{"name": "leaf1"}]
                                },
                                {
                                    "kind": "Header",
                                    "features": ["f2"],
                                    "leaves": [{"name": "leaf2"}]
                                }
                            ],
                            "local_features": ["lf1"],
                            "tito_positions": [{"line": 1, "start": 0}],
                            "receiver_interval": {"lower": 0, "upper": 10},
                            "caller_interval": {"lower": 0, "upper": 5},
                            "is_self_call": false
                        }
                    ]
                },
                {
                    "port": "formal(y)",
                    "taint": [
                        {
                            "kinds": [
                                {
                                    "kind": "Header",
                                    "features": [],
                                    "leaves": []
                                }
                            ],
                            "local_features": [],
                            "tito_positions": []
                        }
                    ]
                }
            ],
            "sinks": [
                {
                    "port": "formal(x)",
                    "taint": [
                        {
                            "kinds": [
                                {
                                    "kind": "SQL",
                                    "features": ["sql_f"],
                                    "leaves": [{"name": "sql_leaf"}]
                                }
                            ],
                            "local_features": ["sql_lf"]
                        }
                    ]
                }
            ],
            "tito": [
                {
                    "port": "formal(x)",
                    "taint": [
                        {
                            "kinds": [
                                {
                                    "kind": "LocalReturn",
                                    "features": [],
                                    "leaves": []
                                }
                            ],
                            "local_features": [],
                            "tito_positions": [{"line": 5, "start": 0}]
                        }
                    ]
                }
            ]
        }))
        .unwrap()
    }

    #[test]
    fn test_category_filtering_hides_sources() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: false,
            show_sinks: true,
            show_tito: true,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);
        assert!(model.sources.is_none());
        assert!(model.sinks.is_some());
        assert!(model.tito.is_some());
    }

    #[test]
    fn test_category_filtering_hides_all() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: false,
            show_sinks: false,
            show_tito: false,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);
        assert!(model.sources.is_none());
        assert!(model.sinks.is_none());
        assert!(model.tito.is_none());
    }

    #[test]
    fn test_kind_filtering() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: true,
            show_tito: true,
            kind: Some("UserControlled".to_string()),
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        // Sources: only the "formal(x)" condition should remain (it had UserControlled).
        // The "formal(y)" condition only had "Header", so it should be removed.
        let sources = model.sources.as_ref().unwrap();
        assert_eq!(sources.len(), 1);
        assert_eq!(sources[0].port, "formal(x)");

        let kinds = &sources[0].taint[0].kinds;
        assert_eq!(kinds.len(), 1);
        assert_eq!(kinds[0].kind, "UserControlled");

        // Sinks had only "SQL" — should be removed entirely.
        let sinks = model.sinks.as_ref().unwrap();
        assert!(sinks.is_empty());

        // Tito had only "LocalReturn" — should be removed entirely.
        let tito = model.tito.as_ref().unwrap();
        assert!(tito.is_empty());
    }

    #[test]
    fn test_caller_port_filtering() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: true,
            show_tito: true,
            caller_port: Some("formal(y)".to_string()),
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        assert_eq!(sources.len(), 1);
        assert_eq!(sources[0].port, "formal(y)");

        // Sinks and tito only had formal(x), so they should be empty.
        assert!(model.sinks.as_ref().unwrap().is_empty());
        assert!(model.tito.as_ref().unwrap().is_empty());
    }

    #[test]
    fn test_strip_features() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: false,
            show_tito: false,
            show_features: false,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        let local_taint = &sources[0].taint[0];
        // local_features should be removed from local_taint.
        assert!(local_taint.local_features.is_none());
        // features should be removed from each frame.
        let frame = &local_taint.kinds[0];
        assert!(frame.features.is_none());
    }

    #[test]
    fn test_strip_tito_positions() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: false,
            show_tito: false,
            show_tito_positions: false,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        let local_taint = &sources[0].taint[0];
        assert!(local_taint.tito_positions.is_none());
    }

    #[test]
    fn test_strip_class_intervals() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: false,
            show_tito: false,
            show_class_intervals: false,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        let local_taint = &sources[0].taint[0];
        assert!(local_taint.receiver_interval.is_none());
        assert!(local_taint.caller_interval.is_none());
        assert!(local_taint.is_self_call.is_none());
    }

    #[test]
    fn test_strip_leaf_names() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: false,
            show_tito: false,
            show_leaf_names: false,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        let frame = &sources[0].taint[0].kinds[0];
        assert!(frame.leaves.is_none());
    }

    #[test]
    fn test_show_all_metadata() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: true,
            show_tito: true,
            show_features: true,
            show_tito_positions: true,
            show_class_intervals: true,
            show_leaf_names: true,
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        let local_taint = &sources[0].taint[0];
        assert!(local_taint.local_features.is_some());
        assert!(local_taint.tito_positions.is_some());
        assert!(local_taint.receiver_interval.is_some());
        assert!(local_taint.caller_interval.is_some());
        assert!(local_taint.is_self_call.is_some());

        let frame = &local_taint.kinds[0];
        assert!(frame.features.is_some());
        assert!(frame.leaves.is_some());
    }

    #[test]
    fn test_combined_kind_and_port_filter() {
        let mut model = sample_model();
        let options = ModelOptions {
            show_sources: true,
            show_sinks: true,
            show_tito: true,
            kind: Some("Header".to_string()),
            caller_port: Some("formal(y)".to_string()),
            ..Default::default()
        };
        filter_and_strip_model(&mut model, &options);

        let sources = model.sources.as_ref().unwrap();
        assert_eq!(sources.len(), 1);
        assert_eq!(sources[0].port, "formal(y)");
        let kinds = &sources[0].taint[0].kinds;
        assert_eq!(kinds.len(), 1);
        assert_eq!(kinds[0].kind, "Header");
    }
}
