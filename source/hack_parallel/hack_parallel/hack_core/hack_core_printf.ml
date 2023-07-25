(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Printf

(** failwith, invalid_arg, and exit accepting printf's format. *)

let failwithf    fmt = ksprintf (fun s () -> failwith s)                        fmt
let invalid_argf fmt = ksprintf (fun s () -> invalid_arg s)                     fmt
let exitf        fmt = ksprintf (fun s () -> Printf.eprintf "%s\n%!" s; exit 1) fmt
