(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis

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
    | Tito
    | Type of string (* Type constraint *)
  [@@deriving show, sexp, compare]

  let to_json = function
    | First { name; kind = FirstField } -> `Assoc ["first-field", `String name]
    | First { name; kind = FirstIndex } -> `Assoc ["first-index", `String name]
    | FormatString -> `Assoc ["via", `String "format-string"]
    | HasFirst FirstField -> `Assoc ["has", `String "first-field"]
    | HasFirst FirstIndex -> `Assoc ["has", `String "first-index"]
    | Obscure -> `Assoc ["via", `String "obscure"]
    | SimpleVia name -> `Assoc ["via", `String name]
    | Tito -> `Assoc ["via", `String "tito"]
    | Type name -> `Assoc ["type", `String name]


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
  [@@deriving show, sexp, compare]
end

module SimpleSet = AbstractSetDomain.Make (Simple)

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

let add_obscure set = Simple.Breadcrumb Breadcrumb.Obscure :: set

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
      Simple.Breadcrumb (Breadcrumb.Type "scalar") :: feature_set
  in
  add


let simple_via ~allowed name = Simple.Breadcrumb (Breadcrumb.simple_via ~allowed name)
