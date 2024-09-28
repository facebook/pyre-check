(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)


module Exit_status = Hack_utils.Exit_status
module Measure = Hack_utils.Measure
module Chan = Domainslib.Chan
open Hack_heap

(*****************************************************************************
 * Module building workers
 *
 * A worker is a subprocess executing an arbitrary function
 *
 * You should first create a fixed amount of workers and then use those
 * because the amount of workers is limited and to make the load-balancing
 * of tasks better (cf multiWorker.ml)
 *
 * On Unix, we "spawn" workers when initializing Hack. Then, this
 * worker, "fork" an ephemeral worker for each incoming request. The forked
 * wephemeral worker will die after processing a single request. (We use this
 * two-layer architecture because, *if* the long-lived worker was created when the
 * original process was still very small, those forks run much faster than forking
 * off the original process which will eventually have a large heap).
 *
 * A worker never handle more than one request at a time.
 *
 *****************************************************************************)

exception Worker_exited_abnormally of int * Unix.process_status
exception Worker_exception of string * Printexc.raw_backtrace
exception Worker_busy
exception Worker_killed

type send_job_failure =
  | Worker_already_exited of Unix.process_status
  | Other_send_job_failure of exn

exception Worker_failed_to_send_job of send_job_failure

(* The maximum amount of workers *)
let max_workers = 1000


let () =
  let print_status () = function
    | Unix.WEXITED code -> Printf.sprintf "exited with return code %d" code
    | Unix.WSIGNALED signal -> Printf.sprintf "was killed with signal %d" signal
    | Unix.WSTOPPED signal -> Printf.sprintf "was stopped with signal %d" signal
  in
  Printexc.register_printer (function
    | Worker_exited_abnormally (pid, status) ->
      Some (Printf.sprintf "Worker exited abnormally. Process %d %a" pid print_status status)
    | _ -> None)

(*****************************************************************************
 * The job executed by the worker.
 *
 * The 'serializer' is the job continuation: it is a function that must
 * be called at the end of the request in order to send back the result
 * to the master (this is "internal business", this is not visible outside
 * this module). The ephemeral worker will provide the expected function.
 * cf 'send_result' in 'worker_job_main.
 *
 *****************************************************************************)

type request = Request of (serializer -> unit)
and serializer = { send: 'a. 'a -> unit }

module Response = struct
  type 'a t =
    | Success of { result: 'a; stats: Measure.record_data }
    | Failure of { exn: string; backtrace: Printexc.raw_backtrace }

  type any = Any : 'a t -> any

  let unpack (w : 'a t) : 'a =
    match w with
    | Success { result; stats } ->
      Measure.merge ~from:(Measure.deserialize stats) ();
      result
    | Failure { exn; backtrace } ->
      raise (Worker_exception (exn, backtrace))

end


(*****************************************************************************
 * Everything we need to know about a worker.
 *
 *****************************************************************************)

type t = {
  (* Sanity check: is the worker still available ? *)
  killed: bool Atomic.t;

  (* Sanity check: is the worker currently busy ? *)
  busy: bool Atomic.t;

  domain : unit Domain.t;

  ic: Response.any Chan.t;
  oc: request Chan.t;

  (* Setting [done_] to true signals the worker not to thread additional tasks. *)
  done_ : bool Atomic.t;

  (* File descriptor corresponding to ic, used for `select`. *)
  (*infd: Unix.file_descr;*)
}

type 'a handle = t

(*****************************************************************************
 * Entry point for spawned worker.
 *
 *****************************************************************************)

let rec waitpid_no_eintr flags pid =
  try Unix.waitpid flags pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_no_eintr flags pid


let worker_job_main ic oc =
  let start_user_time = ref 0.0 in
  let start_system_time = ref 0.0 in
  let send_response response =
    Chan.send oc response
  in
  let send_result result =
    let tm = Unix.times () in
    let end_user_time = tm.Unix.tms_utime +. tm.Unix.tms_cutime in
    let end_system_time = tm.Unix.tms_stime +. tm.Unix.tms_cstime in
    Measure.sample "worker_user_time" (end_user_time -. !start_user_time);
    Measure.sample "worker_system_time" (end_system_time -. !start_system_time);

    let stats = Measure.serialize (Measure.pop_global ()) in
    send_response Response.(Any (Success { result; stats }))
  in
  try
    Measure.push_global ();
    let Request do_process = Chan.recv ic in
    let tm = Unix.times () in
    start_user_time := tm.Unix.tms_utime +. tm.Unix.tms_cutime;
    start_system_time := tm.Unix.tms_stime +. tm.Unix.tms_cstime;
    do_process { send = send_result };
  with
  | End_of_file ->
      exit 1
  | SharedMemory.Out_of_shared_memory ->
      Exit_status.(exit Out_of_shared_memory)
  | SharedMemory.Hash_table_full ->
      Exit_status.(exit Hash_table_full)
  | SharedMemory.Heap_full ->
      Exit_status.(exit Heap_full)
  | SharedMemory.Sql_assertion_failure err_num ->
      let exit_code = match err_num with
        | 11 -> Exit_status.Sql_corrupt
        | 14 -> Exit_status.Sql_cantopen
        | 21 -> Exit_status.Sql_misuse
        | _ -> Exit_status.Sql_assertion_failure
      in
      Exit_status.exit exit_code
  | exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      send_response Response.(Any (Failure { exn = Base.Exn.to_string exn; backtrace }))

let fork_handler ic oc =
  (* We fork an ephemeral worker for every incoming request.
     And let it die after one request. This is the quickest GC. *)
  let d = Domain.spawn (fun () -> worker_job_main ic oc) in
  Domain.join d

let worker_loop done_ handler ic oc =
  try
    while not (Atomic.get done_) do
      handler ic oc
    done
  with End_of_file -> ()

let worker_main restore state handler done_ infd outfd =
  restore state;
  worker_loop done_ handler infd outfd;
  exit 0

(**************************************************************************
 * Creates a pool of workers.
 *
 **************************************************************************)


let current_worker_id = ref 0

(** Make a few workers. *)
let make ~nbr_procs ~gc_control ~long_lived_workers =
  if nbr_procs <= 0 then invalid_arg "Number of workers must be positive";
  if nbr_procs >= max_workers then failwith "Too many workers";
  let restore worker_id =
    current_worker_id := worker_id;
    SharedMemory.connect ();
    Gc.set gc_control
  in
  let handler =
    if long_lived_workers then
      worker_job_main
    else
      fork_handler
  in
  let fork worker_id =
    let child_in = Chan.make_bounded 1 in
    let child_out = Chan.make_bounded 1 in
    let done_ = Atomic.make false in
    let domain =
      Domain.spawn
        (fun () -> worker_main restore worker_id handler done_ child_in child_out)
    in
    { busy = Atomic.make false;
      killed = Atomic.make false;
      ic = child_out;
      oc = child_in;
      domain;
      done_;
    }
  in
  let rec loop acc n =
    if n = 0 then acc
    else
      let worker = fork n in
      loop (worker :: acc) (n - 1)
  in
  (* Flush any buffers before forking workers, to avoid double output. *)
  flush_all ();
  loop [] nbr_procs

let current_worker_id () = !current_worker_id

(**************************************************************************
 * Send a job to a worker
 *
 **************************************************************************)

let call w (type a) (type b) (f : a -> b) (x : a) : b handle =
  if Atomic.get w.killed then raise Worker_killed;
  if Atomic.get w.busy then raise Worker_busy;
  (* Mark the worker as busy. *)
  Atomic.set w.busy true;
  let request = Request (fun { send } -> send (f x)) in
  (* Send the job to the ephemeral worker. *)
  Chan.send w.oc request;
  (* And returned the 'handle'. *)
  w


(**************************************************************************
 * Read results from a handle.
 * This might block if the worker hasn't finished yet.
 *
 **************************************************************************)

let get_result (w : 'a handle) : 'a =
  Atomic.set w.busy false;
  match Chan.recv w.ic with
  | Response.(Any (Success { result; stats })) ->
    assert (Atomic.get w.done_);
    Measure.merge ~from:(Measure.deserialize stats) ();
    Atomic.set w.done_ false;
    (Obj.magic result : 'a)
  | Response.(Any (Failure { exn; backtrace })) ->
    assert (Atomic.get w.done_);
    Atomic.set w.done_ false;
    raise (Worker_exception (exn, backtrace))

(*****************************************************************************
 * Our polling primitive on workers
 * Given a list of handle, returns the ones that are ready.
 *
 *****************************************************************************)

type 'a selected = {
  readys: ('a Response.t * t) list;
  waiters: 'a handle list;
}

let select = fun (type a) (ws : a handle list) ->
  let rec loop readys waiters = function
    | [] -> { readys; waiters }
    | w :: ws -> (
      match Chan.recv_poll w.ic with
      | Some Response.(Any (result : _ Response.t)) ->
        (* Very unsafe, but so was the previous implem *)
        loop (((Obj.magic result : a Response.t), w) :: readys) waiters ws
      | None ->
        loop readys (w :: waiters) ws
      )
  in
  loop [] [] ws

let get_worker w = w

(**************************************************************************
 * Worker termination
 **************************************************************************)

let kill w =
  if not (Atomic.get w.killed) then begin
    (* TODO this does not actually kill the task but waits for its completion,
       we may want to actually kill it *)
    Atomic.set w.done_ true;
    Atomic.set w.killed true;
    Domain.join w.domain;
  end

let exception_backtrace = function
  | Worker_exception (_, backtrace) -> backtrace
  | _ -> Printexc.get_raw_backtrace ()
