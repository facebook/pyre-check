(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

(* Core shadows/deprecates the stdlib Unix module. *)
module CamlUnix = Unix
open Core
module Gc = Stdlib.Gc
module Set = Stdlib.Set
module SharedMemory = Hack_parallel.Std.SharedMemory

module type KeyType = SharedMemory.KeyType

module type ValueType = SharedMemory.ValueType

module NoCache = SharedMemory.NoCache
module WithCache = SharedMemory.WithCache
module FirstClass = SharedMemory.FirstClass

type bytes = int

let initialized : bool ref = ref false

(* Defined in `Gc` module. *)
let best_fit_allocation_policy = 2

let worker_garbage_control =
  (* GC for the worker process. *)
  {
    (Gc.get ()) with
    Gc.minor_heap_size = 256 * 1024;
    allocation_policy = best_fit_allocation_policy;
    space_overhead = 200;
  }


let reset_shared_memory_callbacks = ref []

let initialize ~on_already_initialized ~heap_size ~dep_table_pow ~hash_table_pow ~log_level () =
  let do_initialize =
    if !initialized then
      match on_already_initialized with
      | `Skip -> false
      | `Error -> failwith "shared memory already initialized"
      | `Reset -> true
    else
      true
  in
  if do_initialize then (
    let timer = Timer.start () in
    (* 4 MB *)
    let minor_heap_size = 4 * 1024 * 1024 in
    (* GC for the master process. *)
    Gc.set
      {
        (Gc.get ()) with
        Gc.minor_heap_size;
        allocation_policy = best_fit_allocation_policy;
        space_overhead = 120;
      };
    let shared_mem_config = { SharedMemory.heap_size; dep_table_pow; hash_table_pow; log_level } in
    SharedMemory.init shared_mem_config;
    let () =
      if !initialized then (
        SharedMemory.invalidate_caches ();
        List.iter !reset_shared_memory_callbacks ~f:(fun callback -> callback ()))
    in
    Statistics.performance
      ~name:"Initialized shared memory"
      ~phase_name:"Initializing shared memory"
      ~integers:
        ["heap size", heap_size; "dep table pow", dep_table_pow; "hash table pow", hash_table_pow]
      ~timer
      ();
    initialized := true)


external pyre_reset : unit -> unit = "pyre_reset"

let reset_shared_memory () =
  let () =
    if not !initialized then
      failwith "cannot call reset_shared_memory() before initializing shared memory"
  in
  SharedMemory.invalidate_caches ();
  pyre_reset ();
  List.iter !reset_shared_memory_callbacks ~f:(fun callback -> callback ())


let add_reset_shared_memory_callback callback =
  reset_shared_memory_callbacks := callback :: !reset_shared_memory_callbacks


let initialize_for_tests () =
  let heap_size =
    (* 1 GB *)
    1024 * 1024 * 1024
  in
  let dep_table_pow = 18 in
  let hash_table_pow = 20 in
  let log_level = 0 in
  initialize ~on_already_initialized:`Reset ~heap_size ~dep_table_pow ~hash_table_pow ~log_level ()


let initialize
    {
      Configuration.Analysis.debug;
      shared_memory = { heap_size; dependency_table_power; hash_table_power };
      _;
    }
  =
  let log_level =
    if debug then
      1
    else
      0
  in
  initialize
    ~on_already_initialized:`Skip
    ~heap_size
    ~dep_table_pow:dependency_table_power
    ~hash_table_pow:hash_table_power
    ~log_level
    ()


let heap_size () =
  SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float


let report_statistics () =
  Hack_parallel.Std.Measure.print_stats ();
  Hack_parallel.Std.Measure.print_distributions ()


let is_initialized () = !initialized

exception TarError of string

type tar_structure = {
  directory: PyrePath.t;
  table_path: PyrePath.t;
  dependencies_path: PyrePath.t;
}

let prepare_saved_state_directory { Configuration.Analysis.log_directory; _ } =
  let root = PyrePath.create_relative ~root:log_directory ~relative:"saved_state" in
  let table_path = PyrePath.create_relative ~root ~relative:"table" in
  let dependencies_path = PyrePath.create_relative ~root ~relative:"deps" in
  let () =
    try CamlUnix.mkdir (PyrePath.absolute root) 0o777 with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
    | CamlUnix.Unix_error ((EEXIST | EISDIR), _, _) ->
        PyrePath.unlink_if_exists table_path;
        PyrePath.unlink_if_exists dependencies_path
    | e -> raise e
  in
  { directory = root; table_path; dependencies_path }


let rec waitpid_no_eintr flags pid =
  try CamlUnix.waitpid flags pid with
  | CamlUnix.Unix_error (EINTR, _, _) -> waitpid_no_eintr flags pid


let run_tar arguments =
  let open CamlUnix in
  let in_read, in_write = pipe ~cloexec:true () in
  let out_read, out_write = pipe ~cloexec:true () in
  let err_read, err_write = pipe ~cloexec:true () in
  let pid = create_process "tar" (Array.of_list ("tar" :: arguments)) in_read out_write err_write in
  List.iter ~f:close [in_read; out_write; err_write];
  let _, process_status = waitpid_no_eintr [] pid in
  List.iter ~f:close [in_write; out_read; err_read];
  match process_status with
  | WEXITED 0 -> ()
  | _ ->
      raise
        (TarError
           (Format.sprintf "unable to run tar command %s " (List.to_string ~f:Fn.id arguments)))


exception SavedStateLoadingFailure of string

let save_shared_memory ~path ~configuration =
  SharedMemory.collect `aggressive;
  let { directory; table_path; dependencies_path } = prepare_saved_state_directory configuration in
  SharedMemory.save_table (PyrePath.absolute table_path);
  let _edges_count : bytes =
    SharedMemory.save_dep_table_sqlite (PyrePath.absolute dependencies_path) "0.0.0"
  in
  run_tar ["cf"; path; "-C"; PyrePath.absolute directory; "."]


let load_shared_memory ~path ~configuration =
  let { directory; table_path; dependencies_path } = prepare_saved_state_directory configuration in
  run_tar ["xf"; path; "-C"; PyrePath.absolute directory];
  try
    SharedMemory.load_table (PyrePath.absolute table_path);
    let _edges_count : bytes =
      SharedMemory.load_dep_table_sqlite (PyrePath.absolute dependencies_path) true
    in
    ()
  with
  | SharedMemory.C_assertion_failure message ->
      let message =
        Format.sprintf
          "Assertion failure in shared memory loading: %s. This is likely due to a mismatch \
           between the saved state and the binary version."
          message
      in
      raise (SavedStateLoadingFailure message)


module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare
end

module SingletonKey = struct
  include IntKey

  let key = 0
end

let equal_from_compare compare value0 value1 = Int.equal 0 (compare value0 value1)

module type ValueTypeWithEquivalence = sig
  include ValueType

  val equal : t -> t -> bool
end

module type SerializableValueType = sig
  type t

  module Serialized : ValueType

  val serialize : t -> Serialized.t

  val deserialize : Serialized.t -> t
end

module Serializer (Value : SerializableValueType) = struct
  module Table = NoCache.Make (SingletonKey) (Value.Serialized)

  let store table =
    let data = Value.serialize table in
    Table.add SingletonKey.key data


  let load () =
    let table = Table.get_exn SingletonKey.key |> Value.deserialize in
    Table.remove_batch (Table.KeySet.singleton SingletonKey.key);
    table
end

module type InternerValueType = sig
  include ValueType

  val to_string : t -> string
end

(* Provide a unique integer for a given value. *)
module Interner (Value : InternerValueType) = struct
  module Table = SharedMemory.WithCache.Make (IntKey) (Value)

  type t = int

  let intern value =
    (* The shared memory implementation uses the first 8 bytes of the md5 as a
     * key to the hashtable. Since we already assume that there won't be
     * collisions there, let's use the same strategy here.
     *)
    let id =
      value
      |> Value.to_string
      |> Md5.digest_string
      |> Md5_lib.to_binary
      |> Stdlib.Bytes.of_string
      |> fun md5 -> Stdlib.Bytes.get_int64_ne md5 0 |> Int64.to_int_trunc
    in
    Table.write_around id value;
    id


  let unintern id =
    match Table.get id with
    | Some value -> value
    | None -> Format.asprintf "Invalid intern key %d" id |> failwith


  let compare = Int.compare
end
