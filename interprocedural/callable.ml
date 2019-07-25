(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

type function_target = [ `Function of string ] [@@deriving show, sexp, compare, hash, eq]

type method_target = [ `Method of method_name ] [@@deriving show, sexp, compare, hash, eq]

type real_target =
  [ function_target
  | method_target
  ]
[@@deriving show, sexp, compare, hash, eq]

type override_target = [ `OverrideTarget of method_name ]
[@@deriving show, sexp, compare, hash, eq]

(* Technically not a callable, but we store models of some fields/globals, E.g. os.environ, or
   HttpRequest.GET *)
type object_target = [ `Object of string ] [@@deriving show, sexp, compare, hash, eq]

type non_override_target =
  [ real_target
  | object_target
  ]
[@@deriving show, sexp, compare, hash, eq]

type t =
  [ non_override_target
  | override_target
  ]
[@@deriving show, sexp, compare, hash, eq]

(* pp forces type to be equal to t, but we want [<t] *)
let pretty_print formatter callable =
  let callable = (callable :> t) in
  pp formatter callable


type target_with_result = real_target

let create_function reference = `Function (Reference.show reference)

let create_method reference =
  `Method
    {
      class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
      method_name = Reference.last reference;
    }


let create_override reference =
  `OverrideTarget
    {
      class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
      method_name = Reference.last reference;
    }


let create { Node.value = define; _ } =
  match define.Define.signature.parent with
  | Some _ -> create_method define.signature.name
  | None -> create_function define.signature.name


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

module RealKey = struct
  type t = real_target [@@deriving sexp, hash]

  let to_string = show_real_target

  let compare = compare_real_target

  type out = string

  let from_string = ident
end

module OverrideKey = struct
  type t = override_target [@@deriving sexp, hash]

  let to_string = show_override_target

  let compare = compare_override_target

  type out = string

  let from_string = ident
end

module Set = Caml.Set.Make (Key)
module OverrideSet = Caml.Set.Make (OverrideKey)

let get_definition ~resolution = function
  | `Function name when String.is_suffix name ~suffix:".$toplevel" ->
      String.drop_suffix name 10
      |> Reference.create
      |> SharedMemory.Sources.get
      >>| Source.top_level_define_node
  | `Function name ->
      Reference.create name
      |> GlobalResolution.function_definitions resolution
      >>= List.find ~f:(fun { Node.value; _ } -> not (Define.is_overloaded_method value))
  | `Method { class_name; method_name } when String.equal method_name "$class_toplevel" -> (
      Type.Primitive class_name
      |> (fun annotation -> GlobalResolution.class_definition resolution annotation)
      |> function
      | Some { Node.location; value = { Class.name; body; _ }; _ } ->
          Define.create_class_toplevel ~parent:name ~statements:body
          |> Node.create ~location
          |> Option.some
      | None -> None )
  | `Method { class_name; method_name } ->
      Type.Primitive class_name
      |> GlobalResolution.class_definition resolution
      >>| Node.value
      >>= Class.find_define ~method_name


let get_method_implementation ~resolution ~class_type ~method_name =
  let callable_implementation =
    let last = Reference.last method_name in
    GlobalResolution.class_definition resolution class_type
    >>| Annotated.Class.create
    >>| fun definition ->
    Annotated.Class.attribute
      ~transitive:true
      definition
      ~resolution
      ~name:last
      ~instantiated:class_type
  in
  match callable_implementation with
  | Some callable when Annotated.Attribute.defined callable -> (
      Annotated.Attribute.annotation callable
      |> Annotation.annotation
      |> function
      | Type.Callable { Type.Callable.kind = Named name; _ } -> Some (create_method name)
      | _ -> None )
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


let show = function
  | `Function target -> Format.sprintf "%s (fun)" target
  | `Method { class_name; method_name } -> Format.sprintf "%s::%s (method)" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "%s::%s (override)" class_name method_name
  | `Object name -> Format.sprintf "%s (object)" name


let external_target_name = function
  | `Function target -> target
  | `Method { class_name; method_name } -> Format.sprintf "%s.%s" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "Ovr{%s::%s}" class_name method_name
  | `Object name -> Format.sprintf "Obj{%s}" name


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


module RealMap = Map.Make (RealKey)
module Map = Map.Make (Key)
module Hashable = Hashable.Make (Key)
