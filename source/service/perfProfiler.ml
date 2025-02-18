(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PerfProfiler: start and stop the `perf` profiler tool. *)

type t =
  | Running of { pid: int }
  | CommandNotFound

let start ~filename () =
  let pid = Unix.getpid () in
  let arguments =
    ["perf"; "record"; "--call-graph"; "dwarf"; "--pid"; string_of_int pid; "--output"; filename]
  in
  Log.dump "Running command: %s" (String.concat " " arguments);
  try
    let pid =
      Unix.create_process "perf" (Array.of_list arguments) Unix.stdin Unix.stderr Unix.stderr
    in
    (* Wait to give time for perf to start. *)
    let () = Unix.sleepf 0.1 in
    Running { pid }
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Log.warning "perf: command not found. profiling is disabled.";
      CommandNotFound


let stop = function
  | CommandNotFound -> ()
  | Running { pid } -> (
      (* Check if perf is still running (it should). *)
      let wait_pid, wait_result = Unix.waitpid [Unix.WNOHANG] pid in
      if wait_pid > 0 then
        match wait_result with
        | Unix.WEXITED returncode ->
            Log.warning "`perf` unexpectedly exited with return code %d" returncode
        | Unix.WSIGNALED signal -> Log.warning "`perf` was unexpectedly killed by signal %d" signal
        | Unix.WSTOPPED signal -> Log.warning "`perf` unexpectedly stopped with signal %d" signal
      else
        let () = Log.dump "Stopping perf profiler" in
        let () = Unix.kill pid Sys.sigterm in
        let wait_pid, wait_result = Unix.waitpid [] pid in
        if wait_pid = 0 then
          Log.warning "Failed to waitpid() on `perf`"
        else
          match wait_result with
          | Unix.WSIGNALED signal when signal = Sys.sigterm -> ()
          | Unix.WEXITED returncode ->
              Log.warning "`perf` unexpectedly exited with return code %d" returncode
          | Unix.WSIGNALED signal ->
              Log.warning "`perf` was unexpectedly killed by signal %d" signal
          | Unix.WSTOPPED signal -> Log.warning "`perf` unexpectedly stopped with signal %d" signal)
