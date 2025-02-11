/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The unused dependencies are to check things compile with WASM enabled.
#![allow(unused_crate_dependencies)]

use std::mem::ManuallyDrop;
use std::slice;

#[no_mangle]
pub extern "C" fn allocation(n: usize) -> *mut u8 {
    ManuallyDrop::new(Vec::with_capacity(n)).as_mut_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn evaluate(s: *const u8) -> *mut u8 {
    let length = u32::from_le_bytes(*(s as *const [u8; 4])) as usize;
    let input = slice::from_raw_parts(s.offset(4), length);
    let output = evaluate_buffers(input);
    ManuallyDrop::new(output).as_mut_ptr()
}

fn evaluate_buffers(input: &[u8]) -> Vec<u8> {
    input.to_owned()
}
