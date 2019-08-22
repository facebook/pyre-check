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
  | exception Not_found -> Result.Error `Malformed_key
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

let initial_heap_size = 4096 * 1024 * 1024 (* 4 GB *)

let worker_garbage_control =
  { (Gc.get ()) with Gc.minor_heap_size = 256 * 1024; (* 256 KB *)
                                                      space_overhead = 100 }


let initialize log_level =
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
          SharedMemory.global_size = initial_heap_size;
          heap_size = initial_heap_size;
          dep_table_pow = 24;
          hash_table_pow = 22;
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


let get_heap_handle { Configuration.Analysis.debug; _ } =
  let log_level =
    if debug then
      1
    else
      0
  in
  let { heap_handle; _ } = initialize log_level in
  heap_handle


let heap_size () =
  SharedMemory.heap_size () |> Float.of_int |> (fun size -> size /. 1.0e6) |> Int.of_float


let report_statistics () =
  Measure.print_stats ();
  Measure.print_distributions ()


let save_shared_memory ~path =
  SharedMemory.collect `aggressive;
  SharedMem.save_table path


let load_shared_memory ~path = SharedMem.load_table path

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

module Dependency = struct
  type t = int

  let compare = compare_int

  let sexp_of_t = sexp_of_int

  let t_of_sexp = int_of_sexp

  let make v =
    let mask = (1 lsl 31) - 1 in
    Hashtbl.hash v land mask
end

module DependencySet = Core.Set.Make (Dependency)

module DependencyGraph = struct
  external hh_add_dep : int -> unit = "hh_add_dep"

  external hh_get_dep : int -> int list = "hh_get_dep"

  external hh_get_dep_sqlite : int -> int list = "hh_get_dep_sqlite"

  external hh_allow_dependency_table_reads : bool -> bool = "hh_allow_dependency_table_reads"

  external hh_assert_allow_dependency_table_reads
    :  unit ->
    unit
    = "hh_assert_allow_dependency_table_reads"

  let hh_add_dep x = WorkerCancel.with_worker_exit (fun () -> hh_add_dep x)

  let hh_get_dep x = WorkerCancel.with_worker_exit (fun () -> hh_get_dep x)

  let add x y = hh_add_dep ((x lsl 31) lor y)

  let get x =
    hh_assert_allow_dependency_table_reads ();
    let deps = DependencySet.empty in
    let deps = List.fold_left ~init:deps ~f:DependencySet.add (hh_get_dep x) in
    let deps = List.fold_left ~init:deps ~f:DependencySet.add (hh_get_dep_sqlite x) in
    deps
end

(* This is not currently used, but I'd like to keep it in the module for
   documentation/discoverability purposes *)
let _ = DependencyGraph.hh_allow_dependency_table_reads

type 'keyset transaction_element = {
  before: unit -> unit;
  after: unit -> 'keyset;
}

module DependencyKey = struct
  module type S = sig
    include KeyType

    module KeySet : Set.S with type elt = t

    val encode : t -> int

    val decode : int -> t

    module Transaction : sig
      type t

      val empty : t

      val add : t -> KeySet.t transaction_element -> t

      val execute : t -> update:(unit -> 'a) -> 'a * KeySet.t
    end
  end

  module Make (Key : KeyType) = struct
    include Key
    module KeySet = Set.Make (Key)

    module IntegerKey = struct
      type t = int

      let to_string = Int.to_string

      let compare = Int.compare

      type out = int

      let from_string = Int.of_string
    end

    module DependencyValue = struct
      type t = Key.t

      let prefix = Prefix.make ()

      let description = "Dependency Decoder"

      let unmarshall value = Marshal.from_string value 0
    end

    module IntegerValue = struct
      type t = int

      let prefix = Prefix.make ()

      let description = "Dependency Encoder"

      let unmarshall value = Marshal.from_string value 0
    end

    module DecodeTable = WithCache.Make (IntegerKey) (DependencyValue)
    module EncodeTable = WithCache.Make (Key) (IntegerValue)

    let encode dependency =
      match EncodeTable.get dependency with
      | Some encoded -> encoded
      | None ->
          let rec claim_free_id current =
            DecodeTable.write_through current dependency;
            match DecodeTable.get_no_cache current with
            (* Successfully claimed the id *)
            | Some decoded when Int.equal 0 (Key.compare decoded dependency) -> current
            (* Someone else claimed the id first *)
            | Some _non_equal_decoded_dependency -> claim_free_id (current + 1)
            | None -> failwith "read-your-own-write consistency was violated"
          in
          let encoded = claim_free_id (Dependency.make dependency) in
          (* Theoretically someone else racing this should be benign since having a different
             dependency <-> id pair in the tables is perfectly fine, as long as it's a unique id,
             which is ensured by claim_free_id. However, this should not matter, since we should
             only be working on a dependency in a single worker *)
          EncodeTable.add dependency encoded;
          encoded


    let decode encoded = DecodeTable.find_unsafe encoded

    module Transaction = struct
      type t = KeySet.t transaction_element list

      let empty = []

      let add existing element = element :: existing

      let execute (elements : t) ~update =
        List.iter elements ~f:(fun { before; _ } -> before ());
        let update_result = update () in
        let f sofar { after; _ } = KeySet.union sofar (after ()) in
        update_result, List.fold elements ~init:KeySet.empty ~f
    end
  end
end

module DependencyTracking = struct
  module type TableType = sig
    include NoCache.S

    module Value : ComparableValueType with type t = t
  end

  module Make (DependencyKey : DependencyKey.S) (Table : TableType) = struct
    let add_dependency key value =
      DependencyKey.encode value |> DependencyGraph.add (Dependency.make (Table.Value.prefix, key))


    let get_dependents key =
      DependencyGraph.get (Dependency.make (Table.Value.prefix, key))
      |> DependencySet.fold ~init:DependencyKey.KeySet.empty ~f:(fun sofar encoded ->
             let decoded = DependencyKey.decode encoded in
             DependencyKey.KeySet.add decoded sofar)


    let get_all_dependents keys =
      Table.KeySet.elements keys
      |> List.map ~f:get_dependents
      |> List.fold ~init:DependencyKey.KeySet.empty ~f:DependencyKey.KeySet.union


    let get ?dependency key =
      Option.iter dependency ~f:(add_dependency key);
      Table.get key


    let deprecate_keys = Table.oldify_batch

    let dependencies_since_last_deprecate keys =
      let old_key_map = Table.get_old_batch keys in
      let new_key_map = Table.get_batch keys in
      Table.remove_old_batch keys;

      let add_dependency key sofar =
        let value_has_changed key =
          match Table.KeyMap.find key old_key_map, Table.KeyMap.find key new_key_map with
          | None, None -> false
          | Some old_value, Some new_value ->
              not (Int.equal 0 (Table.Value.compare old_value new_value))
          | _ -> true
        in
        if value_has_changed key then
          let dependencies = get_dependents key in
          DependencyKey.KeySet.union dependencies sofar
        else
          sofar
      in
      Table.KeySet.fold add_dependency keys DependencyKey.KeySet.empty


    let add_to_transaction (transaction : DependencyKey.Transaction.t) ~keys =
      DependencyKey.Transaction.add
        transaction
        {
          before = (fun () -> deprecate_keys keys);
          after = (fun () -> dependencies_since_last_deprecate keys);
        }
  end
end

module DependencyTrackedTableWithCache
    (Key : KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : ComparableValueType) =
struct
  module Table = WithCache.Make (Key) (Value)
  include Table

  include DependencyTracking.Make
            (DependencyKey)
            (struct
              include Table
              module Value = Value
            end)
end

module DependencyTrackedTableNoCache
    (Key : KeyType)
    (DependencyKey : DependencyKey.S)
    (Value : ComparableValueType) =
struct
  module Table = NoCache.Make (Key) (Value)
  include Table

  include DependencyTracking.Make
            (DependencyKey)
            (struct
              include Table
              module Value = Value
            end)
end
