/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

#[derive(Debug)]
pub struct AtomicTextRange(AtomicU64);

impl AtomicTextRange {
    pub fn new(range: TextRange) -> Self {
        Self(AtomicU64::new(Self::to_u64(range)))
    }

    pub fn get(&self) -> TextRange {
        Self::from_u64(self.0.load(Ordering::Relaxed))
    }

    pub fn set(&self, range: TextRange) {
        self.0.store(Self::to_u64(range), Ordering::Relaxed);
    }

    fn to_u64(range: TextRange) -> u64 {
        let low = range.start().to_u32();
        let high = range.end().to_u32();
        ((high as u64) << 32) | (low as u64)
    }

    fn from_u64(packed: u64) -> TextRange {
        let high = (packed >> 32) as u32;
        let low = packed as u32;
        TextRange::new(TextSize::new(low), TextSize::new(high))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_text_range() {
        let range1 = TextRange::new(TextSize::new(0), TextSize::new(1));
        let range2 = TextRange::new(TextSize::new(100), TextSize::new(800));
        let atomic = AtomicTextRange::new(range1);
        assert_eq!(range1, atomic.get());
        atomic.set(range2);
        assert_eq!(range2, atomic.get());
    }
}
