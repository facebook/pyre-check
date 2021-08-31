(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
[@@deriving show, sexp, compare, hash, eq]

type function_t = [ `Function of string ] [@@deriving show, sexp, compare, hash, eq]

type method_t = [ `Method of method_name ] [@@deriving show, sexp, compare, hash, eq]

type callable_t =
  [ function_t
  | method_t
  ]
[@@deriving show, sexp, compare, hash, eq]

type override_t = [ `OverrideTarget of method_name ] [@@deriving show, sexp, compare, hash, eq]

(* Technically not a callable, but we store models of some fields/globals, E.g. os.environ, or
   HttpRequest.GET *)
type object_t = [ `Object of string ] [@@deriving show, sexp, compare, hash, eq]

type non_override_t =
  [ callable_t
  | object_t
  ]
[@@deriving show, sexp, compare, hash, eq]

type t =
  [ non_override_t
  | override_t
  ]
[@@deriving show, sexp, hash, eq]

(* Lower priority appears earlier in comparison. *)
let priority = function
  | `Function _ -> 0
  | `Method _ -> 1
  | `OverrideTarget _ -> 2
  | `Object _ -> 3


let compare left right =
  let priority_comparison = Int.compare (priority left) (priority right) in
  if priority_comparison <> 0 then
    priority_comparison
  else (* left and right must have the same variant. *)
    match left, right with
    | `Function first, `Function second -> String.compare first second
    | `Method first, `Method second -> compare_method_name first second
    | `OverrideTarget first, `OverrideTarget second -> compare_method_name first second
    | `Object first, `Object second -> String.compare first second
    | _ -> failwith "The compared targets must belong to the same variant."


(* pp forces type to be equal to t, but we want [<t] *)
let pretty_print formatter callable =
  let callable = (callable :> t) in
  match callable with
  | `Function name -> String.pp formatter name
  | `Method { class_name; method_name } ->
      String.pp formatter (Format.sprintf "%s::%s" class_name method_name)
  | `OverrideTarget { class_name; method_name } ->
      String.pp formatter (Format.sprintf "Override{%s::%s}" class_name method_name)
  | `Object name -> String.pp formatter (Format.sprintf "Object{%s}" name)


type t_with_result = callable_t

let create_function reference = `Function (Reference.show reference)

let create_method_name ?(suffix = "") reference =
  {
    class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
    method_name = Reference.last reference ^ suffix;
  }


let create_method reference = `Method (create_method_name reference)

let create_property_setter reference = `Method (create_method_name ~suffix:"$setter" reference)

let create_override reference = `OverrideTarget (create_method_name reference)

let create_property_setter_override reference =
  `OverrideTarget (create_method_name ~suffix:"$setter" reference)


let create { Node.value = define; _ } =
  let open Define in
  let name = Node.value define.signature.name in
  match define.signature.parent with
  | Some _ ->
      if Define.is_property_setter define then
        create_property_setter name
      else
        create_method name
  | None -> create_function name


let create_derived_override override ~at_type =
  match override with
  | `OverrideTarget { method_name; _ } ->
      `OverrideTarget { class_name = Reference.show at_type; method_name }


let create_object reference = `Object (Reference.show reference)

module Key = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp

  let hash = hash

  let hash_fold_t = hash_fold_t

  let to_string = show

  let compare = compare

  type out = string

  let from_string = ident
end

module CallableKey = struct
  type t = callable_t [@@deriving sexp, hash]

  let to_string = show_callable_t

  let compare = compare_callable_t

  type out = string

  let from_string = ident
end

module OverrideKey = struct
  type t = override_t [@@deriving sexp, hash]

  let to_string = show_override_t

  let compare = compare_override_t

  type out = string

  let from_string = ident
end

module Set = Caml.Set.Make (Key)
module CallableSet = Caml.Set.Make (CallableKey)
module OverrideSet = Caml.Set.Make (OverrideKey)
module HashSet = Hash_set.Make (Key)
module CallableHashSet = Hash_set.Make (CallableKey)

let get_module_and_definition ~resolution callable =
  let get_bodies { class_name; method_name } =
    let method_name, is_setter =
      if String.is_suffix method_name ~suffix:"$setter" then
        String.drop_suffix method_name (String.length "$setter"), true
      else
        method_name, false
    in
    let define_name =
      Reference.combine (Reference.create class_name) (Reference.create method_name)
    in
    GlobalResolution.function_definition resolution define_name
    >>| fun ({ FunctionDefinition.qualifier; _ } as definitions) ->
    FunctionDefinition.all_bodies definitions |> fun bodies -> qualifier, bodies, is_setter
  in
  match callable with
  | `Function name ->
      Reference.create name
      |> GlobalResolution.function_definition resolution
      >>= fun ({ FunctionDefinition.qualifier; _ } as definitions) ->
      FunctionDefinition.all_bodies definitions
      |> List.find ~f:(fun { Node.value; _ } -> not (Define.is_overloaded_function value))
      >>= fun body -> Some (qualifier, body)
  | `Method method_name -> (
      match get_bodies method_name with
      | Some (qualifier, bodies, is_setter) ->
          if is_setter then
            let is_setter { Node.value = { Define.signature; _ }; _ } =
              Define.Signature.is_property_setter signature
            in
            List.find bodies ~f:is_setter >>| fun body -> qualifier, body
          else
            List.hd bodies >>| fun body -> qualifier, body
      | None -> None)


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


let get_override_reference = function
  | `OverrideTarget { class_name; method_name } ->
      Reference.combine (Reference.create class_name) (Reference.create method_name)


let get_method_reference = function
  | `Method { class_name; method_name } ->
      Reference.combine (Reference.create class_name) (Reference.create method_name)


let get_corresponding_method = function
  | `OverrideTarget method_name -> `Method method_name


let get_corresponding_override = function
  | `Method method_name -> `OverrideTarget method_name


let get_callable_t = function
  | `Function name -> Some (`Function name)
  | `Method method_name -> Some (`Method method_name)
  | `OverrideTarget method_name -> Some (`Method method_name)
  | `Object _ -> None


let show = function
  | `Function target -> Format.sprintf "%s (fun)" target
  | `Method { class_name; method_name } -> Format.sprintf "%s::%s (method)" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "%s::%s (override)" class_name method_name
  | `Object name -> Format.sprintf "%s (object)" name


let external_target_name = function
  | `Function target -> target |> Reference.create |> Reference.delocalize |> Reference.show
  | `Method { class_name; method_name } -> Format.sprintf "%s.%s" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "Ovr{%s::%s}" class_name method_name
  | `Object name -> Format.sprintf "Obj{%s}" name


let class_name = function
  | `Method { class_name; _ } -> Some class_name
  | `Function _
  | `OverrideTarget _
  | `Object _ ->
      None


let get_short_name = function
  | `Function target -> target
  | `Method { method_name; _ }
  | `OverrideTarget { method_name; _ } ->
      method_name
  | `Object name -> name


let compare target1 target2 =
  let target1 = (target1 :> t) in
  let target2 = (target2 :> t) in
  compare target1 target2


module CallableMap = Map.Make (CallableKey)
module Map = Map.Make (Key)
module Hashable = Hashable.Make (Key)
