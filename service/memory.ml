(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Hashtbl = Caml.Hashtbl
module Gc = Caml.Gc
module Digest = Caml.Digest
module Set = Caml.Set
module SharedMemory = Hack_parallel.Std.SharedMem

let unsafe_little_endian_representation ~key =
  (* Ensure that key is a well-formed digest. *)
  Digest.to_hex key
  |> Digest.from_hex
  |> fun digest ->
  assert (Digest.equal digest key);

  (* Mimic what hack_parallel does, which is cast a key to a uint64_t pointer and dereference. This
     code is not portable by any means. *)
  let rec compute_little_endian accumulator index =
    let accumulator =
      Caml.Int64.mul accumulator (Int64.of_int 256)
      |> Caml.Int64.add (Int64.of_int (Char.to_int key.[index]))
    in
    if index = 0 then
      accumulator
    else
      compute_little_endian accumulator (index - 1)
  in
  (* Take the first 8 bytes in reverse order. *)
  compute_little_endian Int64.zero 7


type decodable = ..

type decoding_error =
  [ `Malformed_key
  | `Unknown_type
  | `Decoder_failure of exn
  ]

let registry = Hashtbl.create 13

let register prefix decoder =
  let prefix = Prefix.make_key prefix "" in
  assert (not (Hashtbl.mem registry prefix));
  Hashtbl.add registry prefix decoder


let decode ~key ~value =
  match String.index_exn key '$' with
  | exception Not_found_s _ -> Result.Error `Malformed_key
  | dollar -> (
      let prefix_size = dollar + 1 in
      let prefix = String.sub key ~pos:0 ~len:prefix_size in
      match Hashtbl.find registry prefix with
      | exception Not_found -> Result.Error `Unknown_type
      | decoder -> (
          let key = String.sub key ~pos:prefix_size ~len:(String.length key - prefix_size) in
          match decoder key value with
          | result -> Result.Ok result
          | exception exn -> Result.Error (`Decoder_failure exn) ) )


module type KeyType = sig
  include SharedMem.UserKeyType

  type out

  val from_string : string -> out
end

module type ValueType = sig
  include Value.Type

  val unmarshall : string -> t
end

module Register (Key : KeyType) (Value : ValueType) () : sig
  type decodable += Decoded of Key.out * Value.t option

  val serialize_key : Key.t -> string

  val hash_of_key : Key.t -> string

  val compute_hashes_to_keys : keys:Key.t list -> string String.Map.t
end = struct
  (* Register decoder *)
  type decodable += Decoded of Key.out * Value.t option

  let () =
    let decode key value =
      let value =
        try Some (Value.unmarshall value) with
        | _ -> None
      in
      Decoded (Key.from_string key, value)
    in
    register Value.prefix decode


  let serialize_key key = Key.to_string key |> Prefix.make_key Value.prefix |> Base64.encode_exn

  let hash_of_key key =
    key
    |> Key.to_string
    |> Prefix.make_key Value.prefix
    |> Digest.string
    |> (fun key -> unsafe_little_endian_representation ~key)
    |> Int64.to_string


  let compute_hashes_to_keys ~keys =
    let add map key = Map.set map ~key:(hash_of_key key) ~data:(serialize_key key) in
    List.fold keys ~init:String.Map.empty ~f:add
end

module NoCache = struct
  module type S = sig
    include SharedMemory.NoCache

    type key_out

    type decodable += Decoded of key_out * t option

    val serialize_key : key -> string

    val hash_of_key : key -> string

    val compute_hashes_to_keys : keys:key list -> string String.Map.t
  end

  module Make (Key : KeyType) (Value : ValueType) : sig
    include
      S
        with type t = Value.t
         and type key = Key.t
         and type key_out = Key.out
         and module KeySet = Set.Make(Key)
         and module KeyMap = MyMap.Make(Key)
  end = struct
    type key_out = Key.out

    include Register (Key) (Value) ()

    include SharedMemory.NoCache (Key) (Value)
  end
end

module WithCache = struct
  module type S = sig
    include SharedMemory.WithCache

    type key_out

    type decodable += Decoded of key_out * t option

    val serialize_key : key -> string

    val hash_of_key : key -> string

    val compute_hashes_to_keys : keys:key list -> string String.Map.t
  end

  module Make (Key : KeyType) (Value : ValueType) : sig
    include
      S
        with type t = Value.t
         and type key = Key.t
         and type key_out = Key.out
         and module KeySet = Set.Make(Key)
         and module KeyMap = MyMap.Make(Key)
  end = struct
    type key_out = Key.out

    include Register (Key) (Value) ()

    include SharedMemory.WithCache (Key) (Value)
  end
end

type bytes = int

type configuration = {
  heap_handle: Hack_parallel.Std.SharedMem.handle;
  minor_heap_size: bytes;
}

let configuration : configuration option ref = ref None

let worker_garbage_control =
  { (Gc.get ()) with Gc.minor_heap_size = 256 * 1024; (* 256 KB *)
                                                      space_overhead = 100 }


let initialize ~heap_size ~dep_table_pow ~hash_table_pow ~log_level () =
  match !configuration with
  | None ->
      let minor_heap_size = 4 * 1024 * 1024 in
      (* 4 MB *)
      let space_overhead = 50 in
      (* Only sets the GC for the master process - the parallel workers use GC settings with less
         overhead. *)
      Gc.set { (Gc.get ()) with Gc.minor_heap_size; space_overhead };
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


let get_heap_handle { Configuration.Analysis.debug; _ } =
  let log_level =
    if debug then
      1
    else
      0
  in
  let heap_size =
    (* 8 GB *)
    8192 * 1024 * 1024
  in
  let dep_table_pow = 27 in
  let hash_table_pow = 24 in
  let { heap_handle; _ } = initialize ~heap_size ~dep_table_pow ~hash_table_pow ~log_level () in
  heap_handle


let heap_size () =
  SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float


let report_statistics () =
  Measure.print_stats ();
  Measure.print_distributions ()


exception TarError of string

type tar_structure = {
  directory: Pyre.Path.t;
  table_path: Pyre.Path.t;
  dependencies_path: Pyre.Path.t;
}

let prepare_saved_state_directory { Configuration.Analysis.log_directory; _ } =
  let open Pyre in
  let root = Path.create_relative ~root:log_directory ~relative:"saved_state" in
  let table_path = Path.create_relative ~root ~relative:"table" in
  let dependencies_path = Path.create_relative ~root ~relative:"deps" in
  let () =
    try Core.Unix.mkdir (Path.absolute root) with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already exists. *)
    | Core.Unix.Unix_error ((EEXIST | EISDIR), _, _) ->
        let remove_if_exists path =
          match Sys.file_exists path with
          | `Yes -> Core.Unix.remove path
          | `No
          | `Unknown ->
              ()
        in
        remove_if_exists (Path.absolute table_path);
        remove_if_exists (Path.absolute dependencies_path)
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


let save_shared_memory ~path ~configuration =
  let open Pyre in
  SharedMemory.collect `aggressive;
  let { directory; table_path; dependencies_path } = prepare_saved_state_directory configuration in
  SharedMem.save_table (Path.absolute table_path);
  let _edges_count : bytes =
    SharedMem.save_dep_table_sqlite (Path.absolute dependencies_path) "0.0.0"
  in
  run_tar ["cf"; path; "-C"; Path.absolute directory; "."]


let load_shared_memory ~path ~configuration =
  let open Pyre in
  let { directory; table_path; dependencies_path } = prepare_saved_state_directory configuration in
  run_tar ["xf"; path; "-C"; Path.absolute directory];
  SharedMem.load_table (Path.absolute table_path);
  let _edges_count : bytes =
    SharedMem.load_dep_table_sqlite (Path.absolute dependencies_path) true
  in
  ()


external pyre_reset : unit -> unit = "pyre_reset"

let reset_shared_memory () =
  SharedMem.invalidate_caches ();
  pyre_reset ()


module SingletonKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Int.of_string

  let key = 0
end

module type ComparableValueType = sig
  include ValueType

  val compare : t -> t -> int
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
    let table = Table.find_unsafe SingletonKey.key |> Value.deserialize in
    Table.remove_batch (Table.KeySet.singleton SingletonKey.key);
    table
end
