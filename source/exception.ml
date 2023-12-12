(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Exception handling logic for Pyre internals. This was originally copied from Hack and Flow
 * because stack traces are mutable global state.
 *
 * In ocaml, backtraces (the path that the exception bubbled up after being thrown) are stored as
 * global state and NOT with the exception itself. This means the only safe place to ever read the
 * backtrace is immediately after the exception is caught in the `with` block of a `try...with`.
 *
 * Proper use of this module is something like
 *
 *  try
 *    ...
 *  with exn ->
 *    let e = Exception.wrap exn in (* DO THIS FIRST!!! *)
 *    my_fun e; (* If this code throws internally it will overwrite the global backtrace *)
 *    Exception.reraise e
 *
 * NOTE: if more exception handling functionality is required, check
 * [flow/src/hack_forked/utils/core/exception.ml](https://www.internalfb.com/code/fbsource/[2b0db5caa803]/fbcode/flow/src/hack_forked/utils/core/exception.ml)
 * for functionality that may have already been implemented.
 *)

type t = {
  exn: exn;
  backtrace: Printexc.raw_backtrace;
}

let wrap exn =
  let backtrace = Printexc.get_raw_backtrace () in
  { exn; backtrace }


(* The inverse of `wrap`, returns the wrapped `exn`. You might use this to pattern match on the raw
   exception or print it, but should not reraise it since it will not include the correct backtrace;
   use `reraise` or `to_exn` instead. *)
let unwrap { exn; backtrace = _ } = exn

(* Raise the wrapped exception with its *original* backtrace. *)
let reraise { exn; backtrace } = Printexc.raise_with_backtrace exn backtrace

(** [raise_with_backtrace exn t] raises [exn] with the backtrace from [t]. This could be useful for
    reraising an exception with a new message, without changing the backtrace. *)
let raise_with_backtrace exn { backtrace; _ } = reraise { exn; backtrace }

(* Like `to_string`, but don't add the backtrace to the output message. *)
let get_exn_string { exn; backtrace = _ } = Printexc.to_string exn

(* Like `to_string`, but only include the backtrace in the output. *)
let get_backtrace_string { exn = _; backtrace } = Printexc.raw_backtrace_to_string backtrace

(* Get a string representing the wrapped exception, including its backtrace. *)
let to_string t =
  let ctor = get_exn_string t in
  let bt = get_backtrace_string t in
  if String.equal bt "" then
    ctor
  else
    ctor ^ "\n" ^ bt


(* Like `to_string`, but for an exception where the wrapped value isn't needed. *)
let exn_to_string e = wrap e |> to_string

(* Returns a string of the current backtrace, default n = 100. *)
let get_current_callstack_string n = Printexc.get_callstack n |> Printexc.raw_backtrace_to_string

let record_backtrace = Printexc.record_backtrace

let protect ~f ~finally =
  let res =
    try f () with
    | e ->
        let e = wrap e in
        finally ();
        reraise e
  in
  finally ();
  res
