(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Target: represents a global symbol that might have information attached to it.
 *
 * This is mostly used to represent callables in the interprocedural framework,
 * using `Function` or `Method`.
 * `Override f` represents the set of methods overriding the method `f`.
 * `Object` represents global variables or class attributes.
 *)

open Core
open Ast
open Statement
open Pyre
module PyrePysaApi = Analysis.PyrePysaApi

type kind =
  | Normal
  | PropertySetter
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

type function_name = {
  name: string;
  kind: kind;
}
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

type method_name = {
  class_name: string;
  method_name: string;
  kind: kind;
}
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

module T = struct
  type t =
    | Function of function_name
    | Method of method_name
    | Override of method_name
    (* Represents a global variable or field of a class that we want to model,
     * e.g os.environ or HttpRequest.GET *)
    | Object of string
  [@@deriving show { with_path = false }, sexp, compare, hash, eq]
end

include T

module Map = struct
  include Data_structures.SerializableMap.Make (T)

  module Tree = Map.Make_tree (struct
    include T
    include Comparator.Make (T)
  end)
end

(* Lower priority appears earlier in comparison. *)
let priority = function
  | Function _ -> 0
  | Method _ -> 1
  | Override _ -> 2
  | Object _ -> 3


let compare left right =
  let priority_comparison = Int.compare (priority left) (priority right) in
  if priority_comparison <> 0 then
    priority_comparison
  else
    match left, right with
    | Function first, Function second -> compare_function_name first second
    | Method first, Method second -> compare_method_name first second
    | Override first, Override second -> compare_method_name first second
    | Object first, Object second -> String.compare first second
    | _ -> failwith "The compared targets must belong to the same variant."


let pp_internal = pp

let show_internal = Format.asprintf "%a" pp_internal

let pp_kind formatter = function
  | Normal -> ()
  | PropertySetter -> Format.fprintf formatter "@setter"


let pp_pretty formatter = function
  | Function { name; kind } -> Format.fprintf formatter "%s%a" name pp_kind kind
  | Method { class_name; method_name; kind } ->
      Format.fprintf formatter "%s.%s%a" class_name method_name pp_kind kind
  | Override { class_name; method_name; kind } ->
      Format.fprintf formatter "Overrides{%s.%s%a}" class_name method_name pp_kind kind
  | Object name -> Format.fprintf formatter "Object{%s}" name


let show_pretty = Format.asprintf "%a" pp_pretty

let pp_pretty_with_kind formatter = function
  | Function { name; kind } -> Format.fprintf formatter "%s%a (fun)" name pp_kind kind
  | Method { class_name; method_name; kind } ->
      Format.fprintf formatter "%s.%s%a (method)" class_name method_name pp_kind kind
  | Override { class_name; method_name; kind } ->
      Format.fprintf formatter "%s.%s%a (override)" class_name method_name pp_kind kind
  | Object name -> Format.fprintf formatter "%s (object)" name


let show_pretty_with_kind = Format.asprintf "%a" pp_pretty_with_kind

let pp_external formatter = function
  | Function { name; kind } ->
      Format.fprintf
        formatter
        "%a%a"
        Reference.pp
        (name |> Reference.create |> Reference.delocalize)
        pp_kind
        kind
  | Method { class_name; method_name; kind } ->
      Format.fprintf formatter "%s.%s%a" class_name method_name pp_kind kind
  | Override { class_name; method_name; kind } ->
      Format.fprintf formatter "Overrides{%s.%s%a}" class_name method_name pp_kind kind
  | Object name -> Format.fprintf formatter "Obj{%s}" name


(* Equivalent to pp_internal. Required by @@deriving. *)
let pp = pp_internal

let external_name = Format.asprintf "%a" pp_external

let create_function_name ?(kind = Normal) reference = { name = Reference.show reference; kind }

let create_method_name ?(kind = Normal) reference =
  {
    class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
    method_name = Reference.last reference;
    kind;
  }


let create_function ?kind reference = Function (create_function_name ?kind reference)

let create_method ?kind reference = Method (create_method_name ?kind reference)

let create_property_setter reference = Method (create_method_name ~kind:PropertySetter reference)

let create_override ?kind reference = Override (create_method_name ?kind reference)

let create_property_setter_override reference =
  Override (create_method_name ~kind:PropertySetter reference)


let create define_name define =
  let open Define in
  let kind = if Define.is_property_setter define then PropertySetter else Normal in
  match define.signature.legacy_parent with
  | Some _ -> create_method ~kind define_name
  | None -> create_function ~kind define_name


let create_object reference = Object (Reference.show reference)

let create_derived_override override ~at_type =
  match override with
  | Override { method_name; kind; _ } ->
      Override { class_name = Reference.show at_type; method_name; kind }
  | _ -> failwith "unexpected"


let get_corresponding_method = function
  | Override method_name -> Method method_name
  | _ -> failwith "not an override target"


let get_corresponding_override = function
  | Method method_name -> Override method_name
  | _ -> failwith "unexpected"


let class_name = function
  | Method { class_name; _ } -> Some class_name
  | Override { class_name; _ } -> Some class_name
  | Function _
  | Object _ ->
      None


let method_name = function
  | Method { method_name; _ } -> Some method_name
  | Override { method_name; _ } -> Some method_name
  | Function _
  | Object _ ->
      None


let is_function_or_method = function
  | Function _
  | Method _ ->
      true
  | Override _
  | Object _ ->
      false


let is_method = function
  | Method _ -> true
  | _ -> false


let is_method_or_override = function
  | Method _
  | Override _ ->
      true
  | Function _
  | Object _ ->
      false


let override_to_method = function
  | Function target -> Function target
  | Method target
  | Override target ->
      Method target
  | Object name -> Object name


(** Return the define name of a Function or Method target. Note that multiple targets can match to
    the same define name (e.g, property getters and setters). Hence, use this at your own risk. *)
let define_name = function
  | Function { name; _ } -> Reference.create name
  | Method { class_name; method_name; _ } ->
      Reference.create ~prefix:(Reference.create class_name) method_name
  | Override _
  | Object _ ->
      failwith "unexpected"


let object_name = function
  | Object name -> Reference.create name
  | _ -> failwith "unexpected"


module Set = Stdlib.Set.Make (T)
module Hashable = Core.Hashable.Make (T)
module HashMap = Hashable.Table
module HashSet = Hashable.Hash_set

type definitions_result = {
  qualifier: Reference.t;
  (* Mapping from a target to its selected definition. *)
  callables: Define.t Node.t Map.t;
  (* True if there was multiple non-stub definitions. *)
  has_multiple_definitions: bool;
}

(** This is the source of truth for the mapping of callables to definitions. All parts of the
    analysis should use this (or `get_module_and_definition`) rather than walking over source files. *)
let get_definitions ~pyre_api define_name =
  PyrePysaApi.ReadOnly.get_function_definition pyre_api define_name
  >>| fun ({ Analysis.FunctionDefinition.qualifier; _ } as definitions) ->
  let bodies = Analysis.FunctionDefinition.all_bodies definitions in
  (* Ignore defines for type overloads. *)
  let bodies =
    List.filter ~f:(fun { Node.value; _ } -> not (Define.is_overloaded_function value)) bodies
  in
  let fold ({ callables; _ } as result) define =
    let target = create define_name (Node.value define) in
    match Map.find_opt target callables with
    | None -> { result with callables = Map.add target define callables }
    | Some previous_define -> (
        (* Prefer the non-stub definition, so we can analyze its body. *)
        match Define.is_stub (Node.value previous_define), Define.is_stub (Node.value define) with
        | true, true -> result
        | false, true -> result
        | true, false -> { result with callables = Map.add target define callables }
        | false, false -> { result with has_multiple_definitions = true })
  in
  List.fold
    ~init:{ qualifier; callables = Map.empty; has_multiple_definitions = false }
    ~f:fold
    bodies


let get_module_and_definition ~pyre_api callable =
  define_name callable
  |> get_definitions ~pyre_api
  >>= fun { qualifier; callables; _ } ->
  Map.find_opt callable callables >>| fun define -> qualifier, define


let get_callable_location ~pyre_api callable =
  get_module_and_definition ~pyre_api callable
  >>| fun (module_reference, { Node.location; _ }) ->
  Location.with_module ~module_reference location


let resolve_method ~pyre_api ~class_type ~method_name =
  let callable_implementation =
    Type.split class_type
    |> fst
    |> Type.primitive_name
    >>= PyrePysaApi.ReadOnly.attribute_from_class_name
          pyre_api
          ~transitive:true
          ~name:method_name
          ~type_for_lookup:class_type
  in
  match callable_implementation with
  | Some callable when Analysis.AnnotatedAttribute.defined callable ->
      Analysis.AnnotatedAttribute.annotation callable
      |> Analysis.TypeInfo.Unit.annotation
      |> Type.callable_name
      >>| create_method
  | _ -> None


module ArtificialTargets = struct
  let format_string = Object "<format-string>"

  let str_add = Object "<str.__add__>"

  let str_mod = Object "<str.__mod__>"

  let str_format = Object "<str.format>"

  let str_literal = Object "<literal-string>"

  let condition = Object "<condition>"
end

module SharedMemoryKey = struct
  include T

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp_string = Sexp.of_string sexp_string |> t_of_sexp
end

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory = struct
  type target = T.t

  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (SharedMemoryKey)
      (struct
        type t = unit

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "A set of targets"
      end)

  type t = T.t

  let cleanup = T.cleanup

  let from_heap targets = targets |> List.map ~f:(fun target -> target, ()) |> T.of_alist

  module ReadOnly = T.ReadOnly

  let read_only = T.read_only
end
