(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

let spf = Printf.sprintf
let print_endlinef fmt = Printf.ksprintf print_endline fmt
let prerr_endlinef fmt = Printf.ksprintf prerr_endline fmt

(* Since OCaml usually runs w/o backtraces enabled, the note makes errors
 * easier to debug. *)
let unsafe_opt_note note = function
  | None -> raise (Invalid_argument note)
  | Some x -> x

let unsafe_opt x = unsafe_opt_note "unsafe_opt got None" x

let with_context ~enter ~exit ~do_ =
  enter ();
  let result = try do_ () with e ->
    exit ();
    raise e in
  exit ();
  result
