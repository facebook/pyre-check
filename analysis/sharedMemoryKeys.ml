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
      in_test: bool;
      accessed_via_metaclass: bool;
      name: Type.Primitive.t;
      assumptions: Assumptions.t;
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
      assumptions: Assumptions.t;
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
[@@deriving show, compare, sexp, hash]

module DependencySet = Caml.Set.Make (struct
  type t = dependency [@@deriving compare, sexp]
end)

module DependencyKey = struct
  module Registry = struct
    let register = Fn.id

    include TraditionalRegistry.Make (struct
      type t = dependency [@@deriving compare, sexp, hash]

      let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

      let compare = compare_dependency

      type out = dependency

      let from_string string = Sexp.of_string string |> dependency_of_sexp
    end)
  end

  let get_key = Fn.id

  include DependencyTrackedMemory.DependencyKey.Make (struct
    type key = dependency [@@deriving compare, sexp]

    module KeySet = DependencySet

    type registered = dependency [@@deriving compare, sexp]

    module RegisteredSet = DependencySet
    module Registry = Registry
  end)
end

module LocationKey = struct
  type t = Location.t

  let to_string = Location.show

  let compare = Location.compare

  type out = string

  let from_string = Fn.id
end
