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
  [@@deriving show, sexp, compare]

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
  [@@deriving show, sexp, compare]

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
  type t =
    | LeafName of string
    | TitoPosition of Location.Reference.t
    | Breadcrumb of Breadcrumb.t
    | ViaValueOf of { position: int }
  [@@deriving show, sexp, compare]

  let via_value_of_breadcrumb ~argument:{ Expression.Call.Argument.value; _ } =
    Interprocedural.CallResolution.extract_constant_name value
    >>| fun feature -> Breadcrumb (Breadcrumb.ViaValue feature)
end

module SimpleSet = AbstractOverUnderSetDomain.Make (Simple)

let strip_simple_feature_for_callsite features =
  let strip feature =
    match feature.SimpleSet.element with
    | TitoPosition _ -> None
    | _ -> Some feature
  in
  List.filter_map ~f:strip features


(* Set of complex features, where element can be abstracted and joins are expensive. Should only be
   used for elements that need this kind of joining. *)
module Complex = struct
  type t = ReturnAccessPath of AbstractTreeDomain.Label.path [@@deriving show, sexp, compare]

  let less_or_equal ~left ~right =
    match left, right with
    | ReturnAccessPath left_path, ReturnAccessPath right_path ->
        AbstractTreeDomain.Label.is_prefix ~prefix:right_path left_path


  let widen set =
    if List.length set > 3 then
      [ReturnAccessPath []]
    else
      set
end

module ComplexSet = AbstractElementSetDomain.Make (Complex)

let add_obscure set = SimpleSet.element (Simple.Breadcrumb Breadcrumb.Obscure) :: set

let add_type_breadcrumb ~resolution annotation =
  let is_scalar =
    match annotation with
    | None -> false
    | Some return_type ->
        GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.number
        || GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.bool
        || GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.enumeration
  in
  let add feature_set =
    if not is_scalar then
      feature_set
    else
      SimpleSet.element (Simple.Breadcrumb (Breadcrumb.Type "scalar")) :: feature_set
  in
  add


let simple_via ~allowed name = Simple.Breadcrumb (Breadcrumb.simple_via ~allowed name)

let gather_breadcrumbs breadcrumbs feature =
  match feature.SimpleSet.element with
  | Simple.Breadcrumb _ -> feature :: breadcrumbs
  | _ -> breadcrumbs
