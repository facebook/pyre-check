(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Convenient wrappers around `Lwt_process`. *)

module Completed : sig
  type t = {
    stdout: string;
    stderr: string;
    status: Unix.process_status;
  }
end

val run
  :  ?consume_stdout:(Lwt_io.input_channel -> string Lwt.t) ->
  ?consume_stderr:(Lwt_io.input_channel -> string Lwt.t) ->
  arguments:string list ->
  string ->
  Completed.t Lwt.t
(** [run ~arguments command] spawns a subprocess which shells out to `command` with the given
    [arguments]. The returned promise will resolve upon completion of the subprocess.

    If the subprocess writes to `stdout` and `stderr`, their outputs will be captured from the
    current process by two input channels, from which the contents can be read. By default, contents
    will be read with [Lwt_io.read], meaning that the `stdout` and `stderr` content will be
    available in their entirety after the subprocess terminates. If output needs to be consumed in a
    more fine-grained manner (which is often the case for long-running subprocesses that dump their
    progress to `stdout` or `stderr`), clients of this API can explicitly specify custom reading
    logic with the [consume_stdout] and [consume_stderr] arguments. The returned [Complete.t] of
    this API will always have its [stdout] and [stderr] field set to the string returned by
    [consume_stdout] and [consume_stderr] callback, respectively. *)
