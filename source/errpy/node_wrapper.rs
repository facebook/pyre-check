// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use std::iter;
use std::str;

use tree_sitter::Node as TSNode;
use tree_sitter::Point;
use tree_sitter::TreeCursor;

/// Tree-sitter can emit `comment` and `error` nodes anywhere in its output CST's.
/// This means that in theory our code needs to handle the possibility of a comment
/// or error node being present anywhere in the CST! This is not really practical
/// to implement in `cst_to_ast.rs` so instead here we add `node_wrapper` to filter
/// out ERROR and COMMENT notes in a new `Node` struct which wraps around
/// the Tree sitter `TSNode`.
///
/// We make use of an arena based tree allocator within `FilteredCST` for ease of
/// understanding and future maintenance.
///
/// The methods exposed here are a subset of those on the `TSNode` API visible at:
/// https://docs.rs/tree-sitter/latest/src/tree_sitter/lib.rs.html

#[derive(Debug, Default)]
pub struct FilteredCST<'tree> {
    nodes: Vec<Node<'tree>>,
}

pub struct Node<'a> {
    ts_node: TSNode<'a>,
    parent_idx: Option<usize>,
    children: Vec<usize>,
    named_children: Vec<usize>,
    children_for_field_name: HashMap<&'a str, Vec<usize>>,
}

impl<'a> fmt::Debug for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.ts_node)
    }
}

impl<'tree> FilteredCST<'tree> {
    /// Place node into FilteredCST tree and return numerial index
    fn allocate(&mut self, to_push: Node<'tree>) -> usize {
        self.nodes.push(to_push);
        self.nodes.len() - 1
    }

    pub fn get_root(&self) -> &Node<'tree> {
        &self.nodes[0]
    }
}

impl<'a> PartialEq for Node<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ts_node.id() == other.ts_node.id()
    }
}

/// iterative method to create a node tree from root TS Node
pub fn build_node_tree(root_ts_node: TSNode) -> FilteredCST {
    let mut arena = FilteredCST { nodes: vec![] };

    let mut to_process = VecDeque::new();
    to_process.push_back((None, None, root_ts_node));

    while !to_process.is_empty() {
        if let Some((parent_position, field_name, ts_node)) = to_process.pop_front() {
            let mut current_node = Node::new(ts_node);
            current_node.parent_idx = parent_position;
            let current_node_idx: usize = arena.allocate(current_node);

            if let Some(parent_position) = parent_position {
                arena.nodes[parent_position].children.push(current_node_idx);
                if ts_node.is_named() {
                    arena.nodes[parent_position]
                        .named_children
                        .push(current_node_idx);
                }

                if let Some(field_name) = field_name {
                    let children_for_field_name =
                        &mut arena.nodes[parent_position].children_for_field_name;

                    if !children_for_field_name.contains_key(field_name) {
                        children_for_field_name.insert(field_name, vec![]);
                    }
                    children_for_field_name
                        .get_mut(&field_name)
                        .unwrap()
                        .push(current_node_idx);
                }
            }

            let field_names: Vec<Option<&str>> = extract_child_field_names(ts_node);

            let walker = &mut ts_node.walk();
            for (child, child_field_name) in ts_node.children(walker).zip(field_names.iter()) {
                let kind = child.kind();
                if kind == "ERROR" || kind == "comment" || kind == "line_continuation" {
                    continue;
                }

                to_process.push_back((Some(current_node_idx), *child_field_name, child));
            }
        }
    }

    arena
}

/// Extract all child field names from a TSNode
fn extract_child_field_names<'a>(ts_node: TSNode<'a>) -> Vec<Option<&str>> {
    let cursor = &mut ts_node.walk();
    cursor.reset(ts_node);
    cursor.goto_first_child();
    (0..ts_node.child_count())
        .map(move |_| {
            let result = cursor.field_name();
            cursor.goto_next_sibling();
            result
        })
        .collect()
}

impl<'tree> Node<'tree> {
    pub fn new(ts_node: TSNode<'tree>) -> Self {
        Self {
            ts_node,
            parent_idx: None,
            children: vec![],
            named_children: vec![],
            children_for_field_name: HashMap::new(),
        }
    }

    /// Get a numeric id for this node that is unique.
    pub fn id(&self) -> usize {
        self.ts_node.id()
    }

    /// Get this node's start position in terms of rows and columns.
    pub fn start_position(&self) -> Point {
        self.ts_node.start_position()
    }

    /// Get this node's end position in terms of rows and columns.
    pub fn end_position(&self) -> Point {
        self.ts_node.end_position()
    }

    /// Get the node's child at the given index, where zero represents the first child.
    pub fn child<'a>(&'a self, filtered_cst: &'a FilteredCST<'tree>, idx: usize) -> Option<&Self> {
        self.children
            .get(idx)
            .map(|child_index| &filtered_cst.nodes[*child_index])
    }

    /// Get this node's number of children.
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Get this node's utf8 text as a string.
    pub fn utf8_text<'a>(&self, source: &'a [u8]) -> Result<&'a str, str::Utf8Error> {
        self.ts_node.utf8_text(source)
    }

    /// Get the byte offsets where this node starts.
    pub fn start_byte(&self) -> usize {
        self.ts_node.start_byte()
    }

    /// Get the byte offsets where this node end.
    pub fn end_byte(&self) -> usize {
        self.ts_node.end_byte()
    }

    /// Create a new [TreeCursor] starting from this node.
    pub fn walk(&self) -> TreeCursor<'tree> {
        self.ts_node.walk()
    }

    /// Get this node's type as a string.
    pub fn kind(&self) -> &'static str {
        self.ts_node.kind()
    }

    /// Get this node's immediate parent.
    pub fn parent<'a>(&'a self, filtered_cst: &'a FilteredCST<'tree>) -> Option<&Self> {
        self.parent_idx
            .map(|parent_idx| &filtered_cst.nodes[parent_idx])
    }

    /// Check if this node is *named*.
    ///
    /// Named nodes correspond to named rules in the grammar, whereas *anonymous* nodes
    /// correspond to string literals in the grammar.
    pub fn is_named(&self) -> bool {
        self.ts_node.is_named()
    }

    /// Get the first child with the given field name.
    ///
    /// If multiple children may have the same field name, access them using
    /// [children_by_field_name](Node::children_by_field_name)
    pub fn child_by_field_name<'a>(
        &'a self,
        filtered_cst: &'a FilteredCST<'tree>,
        field_name: &'a str,
    ) -> Option<&'a Node<'a>> {
        self.children_by_field_name(filtered_cst, field_name).next()
    }

    /// Iterate over this node's children with a given field name.
    ///
    /// See also [Node::children].
    pub fn children_by_field_name<'a>(
        &'a self,
        filtered_cst: &'a FilteredCST<'tree>,
        field_name: &str,
    ) -> Box<dyn Iterator<Item = &Node<'a>> + 'a> {
        match self.children_for_field_name.get(field_name) {
            Some(children) => Box::new(
                children
                    .iter()
                    .map(|child_index| &filtered_cst.nodes[*child_index]),
            ),
            None => Box::new(iter::empty()),
        }
    }

    /// Iterate over this node's named children.
    ///
    /// See also [Node::children].
    pub fn named_children<'a>(
        &'a self,
        filtered_cst: &'a FilteredCST<'tree>,
    ) -> impl ExactSizeIterator<Item = &Node<'a>> + 'a {
        self.named_children
            .iter()
            .map(|child_index| &filtered_cst.nodes[*child_index])
    }

    /// Iterate over this node's children.
    pub fn children<'a>(
        &'a self,
        filtered_cst: &'a FilteredCST<'tree>,
    ) -> impl ExactSizeIterator<Item = &Node<'tree>> + 'a {
        self.children
            .iter()
            .map(|child_index| &filtered_cst.nodes[*child_index])
    }
}
