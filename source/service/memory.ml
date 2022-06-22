(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Gc = Caml.Gc
module Set = Caml.Set

module type KeyType = SharedMemory.KeyType

module type ValueType = SharedMemory.ValueType

module NoCache = SharedMemory.NoCache
module WithCache = SharedMemory.WithCache
module FirstClass = SharedMemory.FirstClass

type bytes = int

type configuration = {
  heap_handle: SharedMemory.handle;
  minor_heap_size: bytes;
}

let configuration : configuration option ref = ref None

(* Defined in `Gc` module. *)
let best_fit_allocation_policy = 2

let worker_garbage_control =
  (* GC for the worker process. *)
  {
    (Gc.get ()) with
    Gc.minor_heap_size = 256 * 1024;
    allocation_policy = best_fit_allocation_policy;
    space_overhead = 120;
  }


let initialize ~heap_size ~dep_table_pow ~hash_table_pow ~log_level () =
  match !configuration with
  | None ->
      (* 4 MB *)
      let minor_heap_size = 4 * 1024 * 1024 in
      (* GC for the master process. *)
      Gc.set
        {
          (Gc.get ()) with
          Gc.minor_heap_size;
          allocation_policy = best_fit_allocation_policy;
          space_overhead = 100;
        };
      let shared_mem_config =
        {
          SharedMemory.global_size = 0;
          heap_size;
          dep_table_pow;
          hash_table_pow;
          shm_dirs = ["/dev/shm"; "/pyre"];
          shm_min_avail = 1024 * 1024 * 512;
          (* 512 MB *)
          log_level;
        }
      in
      Log.info
        "Initializing shared memory [heap_size=%d, dep_table_pow=%d, hash_table_pow=%d]"
        heap_size
        dep_table_pow
        hash_table_pow;
      let heap_handle = SharedMemory.init shared_mem_config in
      configuration := Some { heap_handle; minor_heap_size };
      { heap_handle; minor_heap_size }
  | Some configuration -> configuration


let initialize_for_tests () =
  let heap_size =
    (* 1 GB *)
    1024 * 1024 * 1024
  in
  let dep_table_pow = 18 in
  let hash_table_pow = 18 in
  let log_level = 0 in
  let _ = initialize ~heap_size ~dep_table_pow ~hash_table_pow ~log_level () in
  ()


let get_heap_handle
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
  let { heap_handle; _ } =
    initialize
      ~heap_size
      ~dep_table_pow:dependency_table_power
      ~hash_table_pow:hash_table_power
      ~log_level
      ()
  in
  heap_handle


let heap_size () =
  SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float


let report_statistics () =
  Measure.print_stats ();
  Measure.print_distributions ()


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
    try Core.Unix.mkdir (PyrePath.absolute root) with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
    | Core.Unix.Unix_error ((EEXIST | EISDIR), _, _) ->
        PyrePath.remove_if_exists table_path;
        PyrePath.remove_if_exists dependencies_path
    | e -> raise e
  in
  { directory = root; table_path; dependencies_path }


let run_tar arguments =
  let { Unix.Process_info.pid; _ } = Unix.create_process ~prog:"tar" ~args:arguments in
  if Result.is_error (Unix.waitpid pid) then
    raise
      (TarError (Format.sprintf "unable to run tar command %s " (List.to_string ~f:Fn.id arguments)))
  else
    ()


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


external pyre_reset : unit -> unit = "pyre_reset"

let reset_shared_memory () =
  SharedMemory.invalidate_caches ();
  pyre_reset ()


module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  let from_string = Int.of_string
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
      |> Digest.string
      |> Md5_lib.to_binary
      |> Caml.Bytes.of_string
      |> fun md5 -> Caml.Bytes.get_int64_ne md5 0 |> Int64.to_int_trunc
    in
    Table.write_around id value;
    id


  let unintern id =
    match Table.get id with
    | Some value -> value
    | None -> Format.asprintf "Invalid intern key %d" id |> failwith


  let compare = Int.compare
end

module SharedMemory = Hack_parallel.Std.SharedMemory
