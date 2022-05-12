(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Analysis
open Pyre

type method_name = {
  class_name: string;
  method_name: string;
}
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

module T = struct
  type t =
    | Function of string
    | Method of method_name
    | Override of method_name
    (* Represents a global variable or field of a class that we want to model,
     * e.g os.environ or HttpRequest.GET *)
    | Object of string
  [@@deriving show { with_path = false }, sexp, compare, hash, eq]
end

include T

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
    | Function first, Function second -> String.compare first second
    | Method first, Method second -> compare_method_name first second
    | Override first, Override second -> compare_method_name first second
    | Object first, Object second -> String.compare first second
    | _ -> failwith "The compared targets must belong to the same variant."


let pp_internal = pp

let show_internal = Format.asprintf "%a" pp_internal

let pp_pretty formatter = function
  | Function name -> Format.fprintf formatter "%s" name
  | Method { class_name; method_name } -> Format.fprintf formatter "%s::%s" class_name method_name
  | Override { class_name; method_name } ->
      Format.fprintf formatter "Override{%s::%s}" class_name method_name
  | Object name -> Format.fprintf formatter "Object{%s}" name


let show_pretty = Format.asprintf "%a" pp_pretty

let pp_pretty_with_kind formatter = function
  | Function target -> Format.fprintf formatter "%s (fun)" target
  | Method { class_name; method_name } ->
      Format.fprintf formatter "%s::%s (method)" class_name method_name
  | Override { class_name; method_name } ->
      Format.fprintf formatter "%s::%s (override)" class_name method_name
  | Object name -> Format.fprintf formatter "%s (object)" name


let show_pretty_with_kind = Format.asprintf "%a" pp_pretty_with_kind

let pp_external formatter = function
  | Function target ->
      Format.fprintf formatter "%a" Reference.pp (target |> Reference.create |> Reference.delocalize)
  | Method { class_name; method_name } -> Format.fprintf formatter "%s.%s" class_name method_name
  | Override { class_name; method_name } ->
      Format.fprintf formatter "Ovr{%s::%s}" class_name method_name
  | Object name -> Format.fprintf formatter "Obj{%s}" name


(* Equivalent to pp_internal. Required by @@deriving. *)
let pp = pp_internal

let external_name = Format.asprintf "%a" pp_external

let create_function reference = Function (Reference.show reference)

let create_method_name ?(suffix = "") reference =
  {
    class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
    method_name = Reference.last reference ^ suffix;
  }


let create_function_name ?(suffix = "") reference = Reference.show reference ^ suffix

let property_setter_suffix = "@setter"

let create_method reference = Method (create_method_name reference)

let create_property_setter reference =
  Method (create_method_name ~suffix:property_setter_suffix reference)


let create_override reference = Override (create_method_name reference)

let create_property_setter_override reference =
  Override (create_method_name ~suffix:property_setter_suffix reference)


let create { Node.value = define; _ } =
  let open Define in
  let name = define.signature.name in
  match define.signature.parent with
  | Some _ ->
      (* Property setters can be defined within classes. *)
      if Define.is_property_setter define then
        create_property_setter name
      else
        create_method name
  | None ->
      (* Property setters can be defined within functions. *)
      if Define.is_property_setter define then
        Function (create_function_name ~suffix:property_setter_suffix name)
      else
        create_function name


let create_object reference = Object (Reference.show reference)

let create_derived_override override ~at_type =
  match override with
  | Override { method_name; _ } -> Override { class_name = Reference.show at_type; method_name }
  | _ -> failwith "unexpected"


let get_override_reference = function
  | Override { class_name; method_name } ->
      Reference.combine (Reference.create class_name) (Reference.create method_name)
  | _ -> failwith "not an override target"


let get_method_reference = function
  | Method { class_name; method_name } ->
      Reference.combine (Reference.create class_name) (Reference.create method_name)
  | _ -> failwith "not a method target"


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


let get_short_name = function
  | Function target -> target
  | Method { method_name; _ }
  | Override { method_name; _ } ->
      method_name
  | Object name -> name


let override_to_method = function
  | Function target -> Function target
  | Method target
  | Override target ->
      Method target
  | Object name -> Object name


let get_module_and_definition ~resolution callable =
  let drop_setter_suffix name =
    if String.is_suffix name ~suffix:property_setter_suffix then
      String.drop_suffix name (String.length property_setter_suffix), true
    else
      name, false
  in
  let find_defines name =
    GlobalResolution.function_definition resolution name
    >>| fun ({ FunctionDefinition.qualifier; _ } as definitions) ->
    FunctionDefinition.all_bodies definitions, qualifier
  in
  (* If the callable is a property setter, then find the definition that is decorated with
     "*.setter". Otherwise, return one of the definitions of the callable. *)
  match callable with
  | Function name -> (
      let function_name, is_setter = drop_setter_suffix name in
      match find_defines (Reference.create function_name) with
      | Some (bodies, qualifier) ->
          let find_body { Node.value = { Define.signature; _ } as value; _ } =
            (not (Define.is_overloaded_function value))
            && ((not is_setter) || Define.Signature.is_property_setter signature)
          in
          List.find bodies ~f:find_body >>| fun body -> qualifier, body
      | None -> None)
  | Method { class_name; method_name } -> (
      let method_name, is_setter = drop_setter_suffix method_name in
      let define_name =
        Reference.combine (Reference.create class_name) (Reference.create method_name)
      in
      match find_defines define_name with
      | Some (bodies, qualifier) ->
          let find_body { Node.value = { Define.signature; _ }; _ } =
            (not is_setter) || Define.Signature.is_property_setter signature
          in
          List.find bodies ~f:find_body >>| fun body -> qualifier, body
      | None -> None)
  | _ -> failwith "expected a function or method"


let resolve_method ~resolution ~class_type ~method_name =
  let callable_implementation =
    Type.split class_type
    |> fst
    |> Type.primitive_name
    >>= GlobalResolution.attribute_from_class_name
          ~transitive:true
          ~resolution
          ~name:method_name
          ~instantiated:class_type
  in
  match callable_implementation with
  | Some callable when Annotated.Attribute.defined callable ->
      Annotated.Attribute.annotation callable
      |> Annotation.annotation
      |> Type.callable_name
      >>| create_method
  | _ -> None


module Map = Core.Map.Make (T)
module Set = Caml.Set.Make (T)
module Hashable = Core.Hashable.Make (T)
module HashMap = Hashable.Table
module HashSet = Hashable.Hash_set

module SharedMemoryKey = struct
  include T

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp_string = Sexp.of_string sexp_string |> t_of_sexp
end
