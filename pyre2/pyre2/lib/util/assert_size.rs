/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[macro_export]
macro_rules! assert_words {
    ($t:ty, $e:expr) => {
        // We care what the type is on 64-bit platforms,
        // but not on WASM (32 bit) it will be smaller, don't care by how much.
        #[cfg(target_pointer_width = "64")]
        static_assertions::assert_eq_size!($t, [usize; $e]);
    };
}

#[macro_export]
macro_rules! assert_bytes {
    ($t:ty, $e:expr) => {
        #[cfg(target_pointer_width = "64")]
        static_assertions::assert_eq_size!($t, [u8; $e]);
    };
}

#[cfg(test)]
mod tests {
    assert_words!(String, 3);
}
