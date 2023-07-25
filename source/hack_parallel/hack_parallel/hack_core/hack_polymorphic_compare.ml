(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

external compare   : 'a -> 'a -> int = "%compare"
external ascending : 'a -> 'a -> int = "%compare"
let descending x y = compare y x

let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (=) = (=)
let (<>) = (<>)
let equal = (=)
let min = min
let max = max
