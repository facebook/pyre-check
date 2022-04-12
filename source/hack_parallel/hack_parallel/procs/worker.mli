(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


(*****************************************************************************)
(* Module building workers.
 * A worker is a subprocess executing an arbitrary function.
 * You should first create a fixed amount of workers and then use those
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml).
*)
(*****************************************************************************)

exception Worker_exited_abnormally of int * Unix.process_status

(** Worker exited with the given exception, as a string (because marshal-ing on
 * exceptions is not safe). *)
exception Worker_exception of string * Printexc.raw_backtrace

(** Worker killed by Out Of Memory. *)
exception Worker_oomed

(** Raise this exception when sending work to a worker that is already busy.
 * We should never be doing that, and this is an assertion error. *)
exception Worker_busy

(** Raise this exception when sending work to a worker that is already killed.
 * We should never be doing that, and this is an assertion error. *)
exception Worker_killed

type send_job_failure =
  | Worker_already_exited of Unix.process_status
  | Other_send_job_failure of exn

exception Worker_failed_to_send_job of send_job_failure

(* The type of a worker visible to the outside world *)
type t


(*****************************************************************************)
(* The handle is what we get back when we start a job. It's a "future"
 * (sometimes called a "promise"). The scheduler uses the handle to retrieve
 * the result of the job when the task is done (cf multiWorker.ml).
*)
(*****************************************************************************)
type 'a handle

type 'a entry
val register_entry_point:
  restore:('a -> unit) -> 'a entry

(** Creates a pool of workers. *)
val make:
  saved_state : 'a ->
  entry       : 'a entry ->
  nbr_procs   : int ->
  gc_control  : Gc.control ->
  heap_handle : SharedMemory.handle ->
  t list

(* Call in a sub-process (CAREFUL, GLOBALS ARE COPIED) *)
val call: t -> ('a -> 'b) -> 'a -> 'b handle

(* Retrieves the result (once the worker is done) hangs otherwise *)
val get_result: 'a handle -> 'a

(* Selects among multiple handles those which are ready. *)
type 'a selected = {
  readys: 'a handle list;
  waiters: 'a handle list;
}
val select: 'a handle list -> 'a selected

(* Returns the worker which produces this handle *)
val get_worker: 'a handle -> t

(* Kill a worker *)
val kill: t -> unit

(* Return the id of the worker to which the current process belong. 0 means the master process *)
val current_worker_id: unit -> int

val exception_backtrace: exn -> Printexc.raw_backtrace
