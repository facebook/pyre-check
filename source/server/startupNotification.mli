(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* APIs provided in this module is to facilitate showing a customizable notification message when
   the server stops silently in the background. The message is intended to be displayed on the next
   pyre server cli invocation. *)

(* Write to a file under `log_path` that can be later read by `consume`. *)
(* If this API is invoked multiple times for the same log path, later invocations would overwrite
   the contents for earlier invocations. *)
val produce : log_path:PyrePath.t -> string -> unit

(* If there's a file under `log_path` that's been written by `produce` before, return the content
   and remove the file. *)
val consume : log_path:PyrePath.t -> unit -> string option
