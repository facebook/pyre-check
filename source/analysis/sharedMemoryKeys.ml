(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Pyre

module IntKey = struct
  type t = int

  let to_string = Int.to_string

  let compare = Int.compare
end

module StringKey = struct
  type t = string

  let to_string = Fn.id

  let compare = String.compare
end

module ReferenceKey = struct
  type t = Reference.t [@@deriving compare, sexp]

  let to_string = Reference.show
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
end

type dependency =
  | CreateModuleErrors of Reference.t
  | TypeCheckDefine of Reference.t
  | AliasRegister of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
  | AnnotateGlobal of Reference.t
  | AnnotateGlobalLocation of Reference.t
  | FromEmptyStub of Reference.t
  | AttributeTable of AttributeTableKey.t
  | Metaclass of Type.Primitive.t
  | ComputeModuleComponents of Reference.t
  | FunctionDefinitions of Reference.t
[@@deriving show, compare, sexp, hash]

module In = struct
  type key = dependency [@@deriving compare, sexp]

  module KeySet = Stdlib.Set.Make (struct
    type t = dependency [@@deriving compare, sexp]
  end)

  type registered = {
    hash: DependencyTrackedMemory.EncodedDependency.t;
    key: key;
  }
  [@@deriving compare, sexp]

  module RegisteredSet = Stdlib.Set.Make (struct
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
      Hashtbl.update table hash ~f:(append key)


    type t =
      | Master of table
      | Worker of key list

    module Serializable = struct
      module Serialized = struct
        type nonrec t = key list

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "Decoder storage"
      end

      type t = table

      let serialize table = Hashtbl.data table |> List.concat_no_order

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
      | Master table -> Hashtbl.find table hash >>| List.map ~f:(fun key -> { hash; key })
      | Worker _ -> failwith "Can only decode from master"


    let collected_map_reduce scheduler ~policy ~initial ~map ~reduce ~inputs () =
      let map inputs =
        if Scheduler.is_master () then
          map inputs, []
        else (
          global := Worker [];
          let payload = map inputs in
          match !global with
          | Worker keys -> payload, keys
          | Master _ -> failwith "can't set worker back to master")
      in
      let reduce (payload, keys) sofar =
        let register key =
          let (_ : registered) = register key in
          ()
        in
        List.iter keys ~f:register;
        reduce payload sofar
      in
      Scheduler.map_reduce scheduler ~policy ~initial ~map ~reduce ~inputs ()


    let collected_iter scheduler ~policy ~f ~inputs =
      collected_map_reduce scheduler ~policy ~initial:() ~map:f ~reduce:(fun _ _ -> ()) ~inputs ()
  end
end

module DependencyKey = struct
  include In
  include DependencyTrackedMemory.DependencyKey.Make (In)
end

module LocationKey = struct
  type t = Location.t

  let to_string key = Location.sexp_of_t key |> Sexp.to_string

  let compare = Location.compare
end
