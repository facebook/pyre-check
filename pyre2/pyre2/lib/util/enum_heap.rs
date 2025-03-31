/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A max-heap for values represented by an enum.

use std::cmp::Ordering;
use std::collections::BinaryHeap;

struct Element<K, V> {
    key: (K, isize),
    value: V,
}

impl<K: PartialEq, V> PartialEq for Element<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl<K: Eq, V> Eq for Element<K, V> {}

impl<K: PartialOrd, V> PartialOrd for Element<K, V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.key.partial_cmp(&other.key)
    }
}

impl<K: Ord, V> Ord for Element<K, V> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key.cmp(&other.key)
    }
}

pub struct EnumHeap<K, V> {
    index: isize,
    items: BinaryHeap<Element<K, V>>,
}

impl<K: Ord, V> Default for EnumHeap<K, V> {
    fn default() -> Self {
        Self {
            index: 0,
            items: BinaryHeap::new(),
        }
    }
}

impl<K: Ord, V> EnumHeap<K, V> {
    fn new() -> Self {
        Self::default()
    }

    pub fn pop(&mut self) -> Option<(K, V)> {
        self.items.pop().map(|e| (e.key.0, e.value))
    }

    /// LIFO = Last In First Out
    pub fn push_lifo(&mut self, k: K, v: V) {
        self.items.push(Element {
            key: (k, self.index),
            value: v,
        });
        self.index += 1;
    }

    /// FIFO = First In First Out
    pub fn push_fifo(&mut self, k: K, v: V) {
        self.items.push(Element {
            key: (k, -self.index),
            value: v,
        });
        self.index += 1;
    }
}

impl<K: Ord, V> FromIterator<(K, V)> for EnumHeap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut res = Self::new();
        for (k, v) in iter {
            res.push_fifo(k, v);
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Eq, PartialEq, Debug, Ord, PartialOrd)]
    enum Colors {
        Red,
        Green,
        Blue,
    }

    #[test]
    fn test_heap() {
        let mut heap = EnumHeap::new();
        heap.push_fifo(Colors::Blue, 1);
        heap.push_fifo(Colors::Red, 4);
        heap.push_fifo(Colors::Blue, 2);
        heap.push_lifo(Colors::Blue, 0);
        heap.push_fifo(Colors::Green, 3);
        assert_eq!(heap.pop(), Some((Colors::Blue, 0)));
        assert_eq!(heap.pop(), Some((Colors::Blue, 1)));
        assert_eq!(heap.pop(), Some((Colors::Blue, 2)));
        assert_eq!(heap.pop(), Some((Colors::Green, 3)));
        assert_eq!(heap.pop(), Some((Colors::Red, 4)));
        assert_eq!(heap.pop(), None);
    }
}
