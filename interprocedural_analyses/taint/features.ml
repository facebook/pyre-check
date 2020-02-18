(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre

module Breadcrumb = struct
  type first_kind =
    | FirstField
    | FirstIndex
  [@@deriving show, compare]

  type t =
    (* Used to determine 'foo' from request.foo and request.GET['foo'] *)
    | First of {
        kind: first_kind;
        name: string;
      }
    | FormatString (* Via f"{something}" *)
    | HasFirst of first_kind
    | Obscure
    | SimpleVia of string (* Declared breadcrumbs *)
    | ViaValue of string (* Via inferred from ViaValueOf. *)
    | Tito
    | Type of string (* Type constraint *)
  [@@deriving show, compare]

  let to_json ~on_all_paths breadcrumb =
    let prefix = if on_all_paths then "always-" else "" in
    match breadcrumb with
    | First { name; kind = FirstField } -> `Assoc [prefix ^ "first-field", `String name]
    | First { name; kind = FirstIndex } -> `Assoc [prefix ^ "first-index", `String name]
    | FormatString -> `Assoc [prefix ^ "via", `String "format-string"]
    | HasFirst FirstField -> `Assoc [prefix ^ "has", `String "first-field"]
    | HasFirst FirstIndex -> `Assoc [prefix ^ "has", `String "first-index"]
    | Obscure -> `Assoc [prefix ^ "via", `String "obscure"]
    | SimpleVia name -> `Assoc [prefix ^ "via", `String name]
    | ViaValue name -> `Assoc [prefix ^ "via-value", `String name]
    | Tito -> `Assoc [prefix ^ "via", `String "tito"]
    | Type name -> `Assoc [prefix ^ "type", `String name]


  let simple_via ~allowed name =
    if List.mem allowed name ~equal:String.equal then
      SimpleVia name
    else
      Format.sprintf "Unrecognized Via annotation `%s`" name |> failwith
end

(* Simple set of features that are unrelated, thus cheap to maintain *)
module Simple = struct
  let name = "simple features"

  type t =
    | LeafName of string
    | TitoPosition of Location.WithModule.t
    | Breadcrumb of Breadcrumb.t
    | ViaValueOf of { position: int }
  [@@deriving show, compare]

  let via_value_of_breadcrumb ~argument:{ Expression.Call.Argument.value; _ } =
    Interprocedural.CallResolution.extract_constant_name value
    >>| fun feature -> Breadcrumb (Breadcrumb.ViaValue feature)
end

module SimpleSet = Abstract.OverUnderSetDomain.Make (Simple)

let strip_simple_feature_for_callsite features =
  let strip feature =
    match feature.Abstract.OverUnderSetDomain.element with
    | Simple.TitoPosition _ -> None
    | _ -> Some feature
  in
  List.filter_map ~f:strip features


(* Set of complex features, where element can be abstracted and joins are expensive. Should only be
   used for elements that need this kind of joining. *)
module Complex = struct
  let name = "complex features"

  type t = ReturnAccessPath of Abstract.TreeDomain.Label.path [@@deriving show, compare]

  let less_or_equal ~left ~right =
    match left, right with
    | ReturnAccessPath left_path, ReturnAccessPath right_path ->
        Abstract.TreeDomain.Label.is_prefix ~prefix:right_path left_path


  let widen set =
    let truncate = function
      | ReturnAccessPath p when List.length p > 3 -> ReturnAccessPath (List.take p 3)
      | x -> x
    in
    if List.length set > 3 then
      [ReturnAccessPath []]
    else
      List.map ~f:truncate set
end

module ComplexSet = Abstract.ElementSetDomain.Make (Complex)

let obscure = Simple.Breadcrumb Breadcrumb.Obscure

let add_type_breadcrumb ~resolution annotation =
  let rec matches_modulo_optional_and_awaitable ~f annotation =
    match annotation with
    | None
    | Some Type.Any
    | Some Type.Bottom ->
        false
    | Some (Type.Optional annotation)
    | Some (Type.Parametric { name = "typing.Awaitable"; parameters = [Single annotation] }) ->
        matches_modulo_optional_and_awaitable ~f (Some annotation)
    | Some annotation -> f annotation
  in
  let is_scalar =
    let scalar_predicate return_type =
      GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.number
      || GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.enumeration
    in
    matches_modulo_optional_and_awaitable annotation ~f:scalar_predicate
  in
  let is_boolean =
    matches_modulo_optional_and_awaitable annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.bool)
  in
  let add feature_set =
    let add_if cond type_name feature_set =
      if cond then
        SimpleSet.inject (Simple.Breadcrumb (Breadcrumb.Type type_name)) :: feature_set
      else
        feature_set
    in
    feature_set |> add_if (is_scalar || is_boolean) "scalar" |> add_if is_boolean "bool"
  in
  add


let simple_via ~allowed name = Simple.Breadcrumb (Breadcrumb.simple_via ~allowed name)

let gather_breadcrumbs feature breadcrumbs =
  match feature.Abstract.OverUnderSetDomain.element with
  | Simple.Breadcrumb _ -> feature :: breadcrumbs
  | _ -> breadcrumbs


let is_breadcrumb = function
  | { Abstract.OverUnderSetDomain.element = Simple.Breadcrumb _; _ } -> true
  | _ -> false
