(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

let test_forward _ = ()

let () = "livenessCheck" >::: ["forward" >:: test_forward] |> Test.run
