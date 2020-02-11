(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Hashtbl = Caml.Hashtbl
module Set = Caml.Set
open Memory

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

    val decode : int -> t option

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


    let decode encoded = DecodeTable.get encoded

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

module DependencyKind = struct
  type t =
    | Get
    | Mem
end

module DependencyTracking = struct
  module type TableType = sig
    include NoCache.S

    module Value : ComparableValueType with type t = t
  end

  module Make (DependencyKey : DependencyKey.S) (Table : TableType) = struct
    let add_dependency ~(kind : DependencyKind.t) (key : Table.key) (value : DependencyKey.t) =
      DependencyKey.encode value
      |> DependencyGraph.add (Dependency.make (Table.Value.prefix, key, kind))


    let get_dependents ~(kind : DependencyKind.t) (key : Table.key) =
      DependencyGraph.get (Dependency.make (Table.Value.prefix, key, kind))
      |> DependencySet.fold ~init:DependencyKey.KeySet.empty ~f:(fun sofar encoded ->
             match DependencyKey.decode encoded with
             | Some decoded -> DependencyKey.KeySet.add decoded sofar
             | None -> sofar)


    let get_all_dependents keys =
      let keys = Table.KeySet.elements keys in
      let init =
        List.map keys ~f:(get_dependents ~kind:Get)
        |> List.fold ~init:DependencyKey.KeySet.empty ~f:DependencyKey.KeySet.union
      in
      List.map keys ~f:(get_dependents ~kind:Mem) |> List.fold ~init ~f:DependencyKey.KeySet.union


    let get ?dependency key =
      Option.iter dependency ~f:(add_dependency key ~kind:Get);
      Table.get key


    let mem ?dependency key =
      Option.iter dependency ~f:(add_dependency key ~kind:Mem);
      Table.mem key


    let deprecate_keys = Table.oldify_batch

    let dependencies_since_last_deprecate keys =
      let old_key_map = Table.get_old_batch keys in
      let new_key_map = Table.get_batch keys in
      Table.remove_old_batch keys;

      let add_dependency key sofar =
        let value_has_changed, presence_has_changed =
          match Table.KeyMap.find key old_key_map, Table.KeyMap.find key new_key_map with
          | None, None -> false, false
          | Some old_value, Some new_value ->
              not (Int.equal 0 (Table.Value.compare old_value new_value)), false
          | None, Some _
          | Some _, None ->
              true, true
        in
        let sofar =
          if value_has_changed then
            get_dependents ~kind:Get key |> DependencyKey.KeySet.union sofar
          else
            sofar
        in
        if presence_has_changed then
          get_dependents ~kind:Mem key |> DependencyKey.KeySet.union sofar
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


    let add_pessimistic_transaction (transaction : DependencyKey.Transaction.t) ~keys =
      DependencyKey.Transaction.add
        transaction
        {
          before = (fun () -> Table.remove_batch keys);
          after = (fun () -> get_all_dependents keys);
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
