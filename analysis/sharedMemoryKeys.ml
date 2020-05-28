(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module TraditionalRegistry = struct
  module type KeyType = sig
    type t [@@deriving hash]

    include Memory.KeyType with type t := t
  end

  module Make (Key : KeyType) = struct
    module EncodedDependencyKey = struct
      type t = DependencyTrackedMemory.EncodedDependency.t

      let to_string = DependencyTrackedMemory.EncodedDependency.to_string

      let compare = DependencyTrackedMemory.EncodedDependency.compare

      type out = DependencyTrackedMemory.EncodedDependency.t

      let from_string = DependencyTrackedMemory.EncodedDependency.of_string
    end

    module DependencyValue = struct
      type t = Key.t

      let prefix = Prefix.make ()

      let description = "Dependency Decoder"

      let unmarshall value = Marshal.from_string value 0
    end

    module EncodedDependencyValue = struct
      type t = DependencyTrackedMemory.EncodedDependency.t

      let prefix = Prefix.make ()

      let description = "Dependency Encoder"

      let unmarshall value = Marshal.from_string value 0
    end

    module DecodeTable = Memory.WithCache.Make (EncodedDependencyKey) (DependencyValue)
    module EncodeTable = Memory.WithCache.Make (Key) (EncodedDependencyValue)

    module EncodedDependency = struct
      type t = DependencyTrackedMemory.EncodedDependency.t

      let prefix = Prefix.make ()

      let description = "Dependency Encoder"

      let unmarshall value = Marshal.from_string value 0
    end

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
            | Some _non_equal_decoded_dependency ->
                claim_free_id (DependencyTrackedMemory.EncodedDependency.increment current)
            | None -> failwith "read-your-own-write consistency was violated"
          in
          let encoded =
            claim_free_id (DependencyTrackedMemory.EncodedDependency.make ~hash:Key.hash dependency)
          in
          (* Theoretically someone else racing this should be benign since having a different
             dependency <-> id pair in the tables is perfectly fine, as long as it's a unique id,
             which is ensured by claim_free_id. However, this should not matter, since we should
             only be working on a dependency in a single worker *)
          EncodeTable.add dependency encoded;
          encoded


    let decode encoded = DecodeTable.get encoded >>| fun decoded -> [decoded]
  end
end

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare

  type out = int

  let from_string = Core.Int.of_string
end

module StringKey = struct
  type t = string

  let to_string = Fn.id

  let compare = String.compare

  type out = string

  let from_string x = x
end

module ReferenceKey = struct
  type t = Reference.t [@@deriving compare, sexp]

  let to_string = Reference.show

  type out = Reference.t

  let from_string name = Reference.create name
end

module AttributeTableKey = struct
  module T = struct
    type t = {
      include_generated_attributes: bool;
      accessed_via_metaclass: bool;
      name: Type.Primitive.t;
    }
    [@@deriving compare, sexp, hash, show]
  end

  include T
  module Set = Set.Make (T)
  include Hashable.Make (T)

  let to_string key = sexp_of_t key |> Sexp.to_string

  type out = t

  let from_string sexp = Sexp.of_string sexp |> t_of_sexp
end

module ParseAnnotationKey = struct
  type type_validation_policy =
    | NoValidation
    | ValidatePrimitives
    | ValidatePrimitivesAndTypeParameters
  [@@deriving compare, sexp, hash, show]

  module T = struct
    type t = {
      validation: type_validation_policy;
      expression: Expression.t;
    }
    [@@deriving compare, sexp, hash, show]
  end

  include T
  module Set = Set.Make (T)
  include Hashable.Make (T)

  let to_string key = sexp_of_t key |> Sexp.to_string

  type out = t

  let from_string sexp = Sexp.of_string sexp |> t_of_sexp
end

module ReferenceSet = Caml.Set.Make (Reference)

module ReferenceDependencyKey = DependencyTrackedMemory.DependencyKey.Make (struct
  type key = Reference.t [@@deriving compare, sexp]

  module KeySet = ReferenceSet

  type registered = Reference.t [@@deriving compare, sexp]

  module RegisteredSet = ReferenceSet

  module Registry = TraditionalRegistry.Make (struct
    type t = Reference.t [@@deriving hash]

    let to_string = Reference.show

    let compare = Reference.compare

    type out = Reference.t

    let from_string name = Reference.create name
  end)
end)

type dependency =
  | TypeCheckDefine of Reference.t
  | AliasRegister of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
  | AnnotateGlobal of Reference.t
  | AnnotateGlobalLocation of Reference.t
  | FromEmptyStub of Reference.t
  | AttributeTable of AttributeTableKey.t
  | ParseAnnotation of ParseAnnotationKey.t
  | Metaclass of Type.Primitive.t
[@@deriving show, compare, sexp, hash]

module In = struct
  type key = dependency [@@deriving compare, sexp]

  module KeySet = Caml.Set.Make (struct
    type t = dependency [@@deriving compare, sexp]
  end)

  type registered = {
    hash: DependencyTrackedMemory.EncodedDependency.t;
    key: key;
  }
  [@@deriving compare, sexp]

  module RegisteredSet = Caml.Set.Make (struct
    type t = registered [@@deriving compare, sexp]
  end)

  let get_key { key; _ } = key

  module Table = DependencyTrackedMemory.EncodedDependency.Table

  module Registry = struct
    type table = key list Table.t

    let add_to_table table ~hash ~key =
      let append dependency = function
        | Some existing ->
            let equal left right = Int.equal (compare_key left right) 0 in
            if List.mem existing dependency ~equal then
              existing
            else
              dependency :: existing
        | None -> [dependency]
      in
      Table.update table hash ~f:(append key)


    type t =
      | Master of table
      | Worker of key list

    module Serializable = struct
      module Serialized = struct
        type nonrec t = key list

        let unmarshall value = Marshal.from_string value 0

        let prefix = Prefix.make ()

        let description = "Decoder storage"
      end

      type t = table

      let serialize table = Table.data table |> List.concat_no_order

      let deserialize keys =
        let table = Table.create ~size:(List.length keys) () in
        let add key =
          add_to_table
            table
            ~hash:(DependencyTrackedMemory.EncodedDependency.make ~hash:hash_dependency key)
            ~key
        in
        List.iter keys ~f:add;
        table
    end

    module Storage = Memory.Serializer (Serializable)

    let global : t ref = ref (Master (Table.create ()))

    let store () =
      match !global with
      | Master table -> Storage.store table
      | Worker _ -> failwith "trying to store from worker"


    let load () = global := Master (Storage.load ())

    let register key =
      let hash = DependencyTrackedMemory.EncodedDependency.make ~hash:hash_dependency key in
      let () =
        match !global with
        | Master table -> add_to_table table ~key ~hash
        | Worker keys -> global := Worker (key :: keys)
      in
      { hash; key }


    type serialized = Serializable.Serialized.t

    let encode { hash; _ } = hash

    let decode hash =
      match !global with
      | Master table -> Table.find table hash >>| List.map ~f:(fun key -> { hash; key })
      | Worker _ -> failwith "Can only decode from master"


    let collected_map_reduce scheduler ~policy ~configuration ~initial ~map ~reduce ~inputs () =
      let map sofar inputs =
        if Scheduler.is_master () then
          map sofar inputs, []
        else (
          global := Worker [];
          let payload = map sofar inputs in
          match !global with
          | Worker keys -> payload, keys
          | Master _ -> failwith "can't set worker back to master" )
      in
      let reduce (payload, keys) sofar =
        let register key =
          let (_ : registered) = register key in
          ()
        in
        List.iter keys ~f:register;
        reduce payload sofar
      in
      Scheduler.map_reduce scheduler ~policy ~configuration ~initial ~map ~reduce ~inputs ()
  end
end

module DependencyKey = struct
  include In
  include DependencyTrackedMemory.DependencyKey.Make (In)
end

module LocationKey = struct
  type t = Location.t

  let to_string = Location.show

  let compare = Location.compare

  type out = string

  let from_string = Fn.id
end
