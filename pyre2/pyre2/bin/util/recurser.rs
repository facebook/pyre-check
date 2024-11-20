/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utility to break cycles when recursing.

use std::cell::RefCell;
use std::collections::HashSet;
use std::hash::Hash;

#[derive(Debug)]
pub struct Recurser<T> {
    seen: RefCell<HashSet<T>>,
}

impl<T> Default for Recurser<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Guard<'a, T>
where
    T: Hash + Eq,
{
    recurser: &'a Recurser<T>,
    value: T,
}

impl<T: Hash + Eq> Drop for Guard<'_, T> {
    fn drop(&mut self) {
        self.recurser.seen.borrow_mut().remove(&self.value);
    }
}

impl<T> Recurser<T> {
    pub fn new() -> Self {
        Recurser {
            seen: RefCell::new(HashSet::new()),
        }
    }
}

impl<T: Hash + Eq + Copy> Recurser<T> {
    pub fn recurse(&self, x: T) -> Option<Guard<'_, T>> {
        if self.seen.borrow().contains(&x) {
            None
        } else {
            self.seen.borrow_mut().insert(x);
            Some(Guard {
                recurser: self,
                value: x,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Tree {
        Node(Vec<Tree>),
        Leaf(i32, Box<Tree>),
    }

    fn tree(t: &Tree, r: &Recurser<i32>, res: &mut Vec<i32>) {
        match t {
            Tree::Node(ts) => {
                for t in ts {
                    tree(t, r, res);
                }
            }
            Tree::Leaf(i, n) if let Some(_guard) = r.recurse(*i) => {
                res.push(*i);
                tree(n, r, res);
            }
            Tree::Leaf(_, _) => {}
        }
    }

    fn leaf(i: i32, n: Tree) -> Tree {
        Tree::Leaf(i, Box::new(n))
    }

    fn leaf_(i: i32) -> Tree {
        leaf(i, node(vec![]))
    }

    fn node(ts: Vec<Tree>) -> Tree {
        Tree::Node(ts)
    }

    #[test]
    fn test_recurser_demo() {
        let r = Recurser::new();

        let mut res = Vec::new();
        tree(&leaf(1, leaf_(1)), &r, &mut res);
        assert_eq!(res, vec![1]);

        res.clear();
        tree(&leaf(1, leaf_(2)), &r, &mut res);
        assert_eq!(res, vec![1, 2]);

        res.clear();
        tree(&node(vec![leaf_(1), leaf_(1)]), &r, &mut res);
        assert_eq!(res, vec![1, 1]);
    }
}
