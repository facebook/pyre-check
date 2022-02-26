(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let files = ref SMap.empty

let get x = SMap.find_unsafe x !files
let set x y = files := SMap.add x y !files
