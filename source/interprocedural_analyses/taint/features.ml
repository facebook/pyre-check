(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

module First (Kind : sig
  val kind : string
end) =
struct
  type t = string [@@deriving show]

  let name = "first-" ^ Kind.kind

  let compare = String.compare

  let to_json firsts =
    match firsts with
    | [] -> []
    | _ :: _ ->
        let first_name = "first-" ^ Kind.kind in
        let element_to_json element = `Assoc [first_name, `String element] in
        `Assoc ["has", `String first_name] :: List.map firsts ~f:element_to_json
end

module FirstIndex = First (struct
  let kind = "index"
end)

module FirstField = First (struct
  let kind = "field"
end)

module FirstIndexSet = Abstract.SetDomain.Make (FirstIndex)
module FirstFieldSet = Abstract.SetDomain.Make (FirstField)

module TitoPosition = struct
  let name = "tito positions"

  type t = Location.t [@@deriving show, compare]

  let max_count () = TaintConfiguration.maximum_tito_positions
end

module TitoPositionSet = Abstract.ToppedSetDomain.Make (TitoPosition)

module LeafName = struct
  let name = "leaf names"

  type t = {
    leaf: string;
    port: string option;
  }
  [@@deriving compare]

  let pp formatter { leaf; port } =
    match port with
    | None -> Format.fprintf formatter "LeafName(%s)" leaf
    | Some port -> Format.fprintf formatter "LeafName(%s, port=%s)" leaf port


  let show = Format.asprintf "%a" pp

  let to_json ~kind_json { leaf; port } =
    let port_assoc =
      match port with
      | Some port -> ["port", `String port]
      | None -> []
    in
    `Assoc (port_assoc @ ["kind", kind_json; "name", `String leaf])
end

module LeafNameSet = Abstract.SetDomain.Make (LeafName)

module Breadcrumb = struct
  let name = "breadcrumbs"

  type t =
    | FormatString (* Via f"{something}" *)
    | Obscure
    | Lambda
    | SimpleVia of string (* Declared breadcrumbs *)
    | ViaValue of {
        tag: string option;
        value: string;
      }
    (* Via inferred from ViaValueOf. *)
    | ViaType of {
        tag: string option;
        value: string;
      }
    (* Via inferred from ViaTypeOf. *)
    | Tito
    | Type of string (* Type constraint *)
    | Broadening (* Taint tree was collapsed for various reasons *)
    | WidenBroadening (* Taint tree was collapsed during widening *)
    | TitoBroadening (* Taint tree was collapsed when applying tito *)
    | IssueBroadening (* Taint tree was collapsed when matching sources and sinks *)
  [@@deriving compare]

  let pp formatter breadcrumb =
    let pp_via_value_or_type header tag value =
      match tag with
      | None -> Format.fprintf formatter "%s[%s]" header value
      | Some tag -> Format.fprintf formatter "%s[%s, tag=%s]" header value tag
    in
    match breadcrumb with
    | FormatString -> Format.fprintf formatter "FormatString"
    | Obscure -> Format.fprintf formatter "Obscure"
    | Lambda -> Format.fprintf formatter "Lambda"
    | SimpleVia name -> Format.fprintf formatter "SimpleVia[%s]" name
    | ViaValue { tag; value } -> pp_via_value_or_type "ViaValue" tag value
    | ViaType { tag; value } -> pp_via_value_or_type "ViaType" tag value
    | Tito -> Format.fprintf formatter "Tito"
    | Type name -> Format.fprintf formatter "Type[%s]" name
    | Broadening -> Format.fprintf formatter "Broadening"
    | WidenBroadening -> Format.fprintf formatter "WidenBroadening"
    | TitoBroadening -> Format.fprintf formatter "TitoBroadening"
    | IssueBroadening -> Format.fprintf formatter "IssueBroadening"


  let show = Format.asprintf "%a" pp

  let to_json ~on_all_paths breadcrumb =
    let prefix = if on_all_paths then "always-" else "" in
    let via_value_or_type_annotation ~via_kind ~tag ~value =
      match tag with
      | None -> `Assoc [Format.sprintf "%svia-%s" prefix via_kind, `String value]
      | Some tag -> `Assoc [Format.sprintf "%svia-%s-%s" prefix tag via_kind, `String value]
    in
    match breadcrumb with
    | FormatString -> `Assoc [prefix ^ "via", `String "format-string"]
    | Obscure -> `Assoc [prefix ^ "via", `String "obscure"]
    | Lambda -> `Assoc [prefix ^ "via", `String "lambda"]
    | SimpleVia name -> `Assoc [prefix ^ "via", `String name]
    | ViaValue { tag; value } -> via_value_or_type_annotation ~via_kind:"value" ~tag ~value
    | ViaType { tag; value } -> via_value_or_type_annotation ~via_kind:"type" ~tag ~value
    | Tito -> `Assoc [prefix ^ "via", `String "tito"]
    | Type name -> `Assoc [prefix ^ "type", `String name]
    | Broadening -> `Assoc [prefix ^ "via", `String "broadening"]
    | WidenBroadening -> `Assoc [prefix ^ "via", `String "widen-broadening"]
    | TitoBroadening -> `Assoc [prefix ^ "via", `String "tito-broadening"]
    | IssueBroadening -> `Assoc [prefix ^ "via", `String "issue-broadening"]


  let simple_via ~allowed name =
    if List.mem allowed name ~equal:String.equal then
      Ok (SimpleVia name)
    else
      Error (Format.sprintf "Unrecognized Via annotation `%s`" name)
end

module BreadcrumbSet = Abstract.OverUnderSetDomain.Make (Breadcrumb)

module ViaFeature = struct
  let name = "via features"

  type t =
    | ViaValueOf of {
        parameter: AccessPath.Root.t;
        tag: string option;
      }
    | ViaTypeOf of {
        parameter: AccessPath.Root.t;
        tag: string option;
      }
  [@@deriving compare]

  let pp formatter simple =
    let pp_via_value_or_type header parameter tag =
      match tag with
      | None -> Format.fprintf formatter "%s[%a]" header AccessPath.Root.pp parameter
      | Some tag ->
          Format.fprintf formatter "%s[%a, tag=%s]" header AccessPath.Root.pp parameter tag
    in
    match simple with
    | ViaValueOf { parameter; tag } -> pp_via_value_or_type "ViaValueOf" parameter tag
    | ViaTypeOf { parameter; tag } -> pp_via_value_or_type "ViaTypeOf" parameter tag


  let show = Format.asprintf "%a" pp

  let via_value_of_breadcrumb ?tag ~argument =
    let feature =
      match argument with
      | None -> "<missing>"
      | Some argument ->
          Interprocedural.CallResolution.extract_constant_name argument
          |> Option.value ~default:"<unknown>"
    in
    Breadcrumb.ViaValue { value = feature; tag }


  let via_type_of_breadcrumb ?tag ~resolution ~argument =
    let feature =
      argument
      >>| Resolution.resolve_expression resolution
      >>| snd
      >>| Type.weaken_literals
      |> Option.value ~default:Type.Top
      |> Type.show
    in
    Breadcrumb.ViaType { value = feature; tag }
end

module ViaFeatureSet = Abstract.SetDomain.Make (ViaFeature)

module ReturnAccessPath = struct
  let name = "return access paths"

  type t = Abstract.TreeDomain.Label.path [@@deriving show { with_path = false }, compare]

  let less_or_equal ~left ~right = Abstract.TreeDomain.Label.is_prefix ~prefix:right left

  let common_prefix = function
    | head :: tail -> List.fold ~init:head ~f:Abstract.TreeDomain.Label.common_prefix tail
    | [] -> []


  let widen set =
    if List.length set > TaintConfiguration.maximum_return_access_path_width then
      [common_prefix set]
    else
      let truncate = function
        | p when List.length p > TaintConfiguration.maximum_return_access_path_depth ->
            List.take p TaintConfiguration.maximum_return_access_path_depth
        | x -> x
      in
      List.map ~f:truncate set
end

module ReturnAccessPathSet = struct
  module T = Abstract.ElementSetDomain.Make (ReturnAccessPath)
  include T

  let join left right =
    let set = T.join left right in
    if T.count set > TaintConfiguration.maximum_return_access_path_width then
      set |> T.elements |> ReturnAccessPath.common_prefix |> T.singleton
    else
      set
end

let obscure = Breadcrumb.Obscure

let lambda = Breadcrumb.Lambda

let tito = Breadcrumb.Tito

let format_string = Breadcrumb.FormatString

let widen_broadening =
  BreadcrumbSet.create
    [
      Part (BreadcrumbSet.Element, Breadcrumb.Broadening);
      Part (BreadcrumbSet.Element, Breadcrumb.WidenBroadening);
    ]


let tito_broadening =
  BreadcrumbSet.create
    [
      Part (BreadcrumbSet.Element, Breadcrumb.Broadening);
      Part (BreadcrumbSet.Element, Breadcrumb.TitoBroadening);
    ]


let issue_broadening =
  BreadcrumbSet.create
    [
      Part (BreadcrumbSet.Element, Breadcrumb.Broadening);
      Part (BreadcrumbSet.Element, Breadcrumb.IssueBroadening);
    ]


let type_bool =
  BreadcrumbSet.create
    [
      Part (BreadcrumbSet.Element, Breadcrumb.Type "scalar");
      Part (BreadcrumbSet.Element, Breadcrumb.Type "bool");
    ]


let type_breadcrumbs ~resolution annotation =
  let matches_at_leaves ~f annotation =
    let rec matches_at_leaves ~f annotation =
      match annotation with
      | Type.Any
      | Type.Bottom ->
          false
      | Type.Union [Type.NoneType; annotation]
      | Type.Union [annotation; Type.NoneType]
      | Type.Parametric { name = "typing.Awaitable"; parameters = [Single annotation] } ->
          matches_at_leaves ~f annotation
      | Type.Tuple (Concatenation concatenation) ->
          Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
          >>| (fun element -> matches_at_leaves ~f element)
          |> Option.value ~default:(f annotation)
      | Type.Tuple (Type.OrderedTypes.Concrete annotations) ->
          List.for_all annotations ~f:(matches_at_leaves ~f)
      | annotation -> f annotation
    in
    annotation >>| matches_at_leaves ~f |> Option.value ~default:false
  in
  let is_boolean =
    matches_at_leaves annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.bool)
  in
  let is_integer =
    matches_at_leaves annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.integer)
  in
  let is_float =
    matches_at_leaves annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.float)
  in
  let is_enumeration =
    matches_at_leaves annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.enumeration)
  in
  let is_scalar = is_boolean || is_integer || is_float || is_enumeration in
  let add_if cond type_name features =
    if cond then
      BreadcrumbSet.add (Breadcrumb.Type type_name) features
    else
      features
  in
  BreadcrumbSet.bottom
  |> add_if is_scalar "scalar"
  |> add_if is_boolean "bool"
  |> add_if is_integer "integer"
  |> add_if is_enumeration "enumeration"


let number_regexp = Str.regexp "[0-9]+"

let is_numeric name = Str.string_match number_regexp name 0

let to_first_name label =
  match label with
  | Abstract.TreeDomain.Label.Index name when is_numeric name -> Some "<numeric>"
  | Index name -> Some name
  | Field _ -> None
  | AnyIndex -> Some "<unknown>"
