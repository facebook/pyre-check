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

  let to_json ~leaf_kind_json { leaf; port } =
    let port_assoc =
      match port with
      | Some port -> ["port", `String port]
      | None -> []
    in
    `Assoc (port_assoc @ ["kind", leaf_kind_json; "name", `String leaf])
end

module LeafNameSet = Abstract.SetDomain.Make (LeafName)

module Breadcrumb = struct
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
  [@@deriving show { with_path = false }, compare]

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

(* Simple set of features that are unrelated, thus cheap to maintain *)
module Simple = struct
  let name = "simple features"

  type t =
    | Breadcrumb of Breadcrumb.t
    | ViaValueOf of {
        parameter: AccessPath.Root.t;
        tag: string option;
      }
    | ViaTypeOf of {
        parameter: AccessPath.Root.t;
        tag: string option;
      }
  [@@deriving show { with_path = false }, compare]

  let pp formatter = function
    | Breadcrumb breadcrumb -> Format.fprintf formatter "%a" Breadcrumb.pp breadcrumb
    | simple -> pp formatter simple


  let show simple = Format.asprintf "%a" pp simple

  let via_value_of_breadcrumb ?tag ~argument =
    let feature =
      match argument with
      | None -> "<missing>"
      | Some argument ->
          Interprocedural.CallResolution.extract_constant_name argument
          |> Option.value ~default:"<unknown>"
    in
    Breadcrumb (Breadcrumb.ViaValue { value = feature; tag })


  let via_type_of_breadcrumb ?tag ~resolution ~argument =
    let feature =
      argument
      >>| Resolution.resolve_expression resolution
      >>| snd
      >>| Type.weaken_literals
      |> Option.value ~default:Type.Top
      |> Type.show
    in
    Breadcrumb (Breadcrumb.ViaType { value = feature; tag })
end

module SimpleSet = Abstract.OverUnderSetDomain.Make (Simple)

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

let obscure = Simple.Breadcrumb Breadcrumb.Obscure

let lambda = Simple.Breadcrumb Breadcrumb.Lambda

let tito = Simple.Breadcrumb Breadcrumb.Tito

let format_string = Simple.Breadcrumb Breadcrumb.FormatString

let widen_broadening =
  SimpleSet.create
    [
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.Broadening);
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.WidenBroadening);
    ]


let tito_broadening =
  SimpleSet.create
    [
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.Broadening);
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.TitoBroadening);
    ]


let issue_broadening =
  SimpleSet.create
    [
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.Broadening);
      Part (SimpleSet.Element, Simple.Breadcrumb Breadcrumb.IssueBroadening);
    ]


let type_bool =
  SimpleSet.create
    [
      Part (SimpleSet.Element, Simple.Breadcrumb (Breadcrumb.Type "scalar"));
      Part (SimpleSet.Element, Simple.Breadcrumb (Breadcrumb.Type "bool"));
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
  let is_scalar =
    let scalar_predicate return_type =
      GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.number
      || GlobalResolution.less_or_equal resolution ~left:return_type ~right:Type.enumeration
    in
    matches_at_leaves annotation ~f:scalar_predicate
  in
  let is_boolean =
    matches_at_leaves annotation ~f:(fun left ->
        GlobalResolution.less_or_equal resolution ~left ~right:Type.bool)
  in
  let add_if cond type_name features =
    if cond then
      SimpleSet.add (Simple.Breadcrumb (Breadcrumb.Type type_name)) features
    else
      features
  in
  SimpleSet.bottom |> add_if (is_scalar || is_boolean) "scalar" |> add_if is_boolean "bool"


let simple_via ~allowed name =
  let open Core.Result in
  Breadcrumb.simple_via ~allowed name >>| fun breadcrumb -> Simple.Breadcrumb breadcrumb


let gather_breadcrumbs features breadcrumbs =
  let to_add =
    SimpleSet.transform
      SimpleSet.Element
      Abstract.Domain.Filter
      ~f:(function
        | Simple.Breadcrumb _ -> true
        | _ -> false)
      features
  in
  SimpleSet.add_set breadcrumbs ~to_add


let is_breadcrumb = function
  | { Abstract.OverUnderSetDomain.element = Simple.Breadcrumb _; _ } -> true
  | _ -> false


let number_regexp = Str.regexp "[0-9]+"

let is_numeric name = Str.string_match number_regexp name 0

let to_first_name label =
  match label with
  | Abstract.TreeDomain.Label.Index name when is_numeric name -> Some "<numeric>"
  | Index name -> Some name
  | Field _ -> None
  | AnyIndex -> Some "<unknown>"
