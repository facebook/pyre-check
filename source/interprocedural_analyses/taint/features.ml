(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

module MakeInterner (T : sig
  type t

  val name : string

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end) =
struct
  module Value = struct
    include T

    let to_string = T.show

    let prefix = Prefix.make ()

    let description = T.name
  end

  include Memory.Interner (Value)

  let name = T.name

  let pp formatter id = id |> unintern |> T.pp formatter

  let show id = id |> unintern |> T.show
end

module MakeAbstractSetFromInterner (Interner : sig
  val name : string

  val show : int -> string
end) =
struct
  include Abstract.SetDomain.MakeWithSet (struct
    include Data_structures.PatriciaTreeSet.PatriciaTreeIntSet

    let show_element = Interner.show

    let element_name = Interner.name
  end)
end

module MakeAbstractSetFromInternerAndStats (Interner : sig
  val name : string

  val show : int -> string
end) (Stats : sig
  val common_integers : int array
end) =
struct
  include Abstract.SetDomain.MakeWithSet (struct
    include Data_structures.BitSetPatriciaTreeIntSet.Make (struct
      let common_integers = Stats.common_integers
    end)

    let show_element = Interner.show

    let element_name = Interner.name
  end)
end

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

module FirstIndexInterned = MakeInterner (FirstIndex)

module FirstIndexSet = struct
  include
    MakeAbstractSetFromInternerAndStats
      (FirstIndexInterned)
      (struct
        let common_integers = TaintAnalysisFeatureStats.common_first_indices
      end)

  let number_regexp = Str.regexp "[0-9]+"

  let add_first index indices =
    let is_numeric name = Str.string_match number_regexp name 0 in
    let to_first_name = function
      | Abstract.TreeDomain.Label.Index name when is_numeric name -> Some "<numeric>"
      | Index name -> Some name
      | Field _ -> None
      | AnyIndex -> Some "<unknown>"
    in
    if is_bottom indices then
      to_first_name index
      >>| FirstIndexInterned.intern
      >>| singleton
      |> Option.value ~default:bottom
    else
      indices


  let sequence_join new_indices existing_indices =
    if is_bottom existing_indices then
      new_indices
    else
      existing_indices
end

module FirstField = First (struct
  let kind = "field"
end)

module FirstFieldInterned = MakeInterner (FirstField)

module FirstFieldSet = struct
  include
    MakeAbstractSetFromInternerAndStats
      (FirstFieldInterned)
      (struct
        let common_integers = TaintAnalysisFeatureStats.common_first_fields
      end)

  let add_first field fields =
    if is_bottom fields then
      field |> FirstFieldInterned.intern |> singleton
    else
      fields


  let sequence_join new_fields existing_fields =
    if is_bottom existing_fields then
      new_fields
    else
      existing_fields
end

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

  let to_json { leaf; port } =
    let port_assoc =
      match port with
      | Some port -> ["port", `String port]
      | None -> []
    in
    `Assoc (port_assoc @ ["name", `String leaf])
end

module LeafNameInterned = MakeInterner (LeafName)
module LeafNameSet = MakeAbstractSetFromInterner (LeafNameInterned)

module Breadcrumb = struct
  let name = "breadcrumbs"

  type t =
    | FormatString (* Via f"{something}" *)
    | ObscureModel
    | ObscureUnknownCallee
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
    | Tito (* Taint In Taint Out *)
    | Type of string (* Type constraint *)
    | Broadening (* Taint tree was collapsed for various reasons *)
    | WidenBroadening (* Taint tree was collapsed during widening *)
    | TitoBroadening (* Taint tree was collapsed when applying tito *)
    | IssueBroadening (* Taint tree was collapsed when matching sources and sinks *)
    | Crtex (* Taint comes from the Cross Repository Taint EXchange *)
  [@@deriving compare]

  let pp formatter breadcrumb =
    let pp_via_value_or_type header tag value =
      match tag with
      | None -> Format.fprintf formatter "%s[%s]" header value
      | Some tag -> Format.fprintf formatter "%s[%s, tag=%s]" header value tag
    in
    match breadcrumb with
    | FormatString -> Format.fprintf formatter "FormatString"
    | ObscureModel -> Format.fprintf formatter "ObscureModel"
    | ObscureUnknownCallee -> Format.fprintf formatter "ObscureUnknownCallee"
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
    | Crtex -> Format.fprintf formatter "Crtex"


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
    | ObscureModel -> `Assoc [prefix ^ "via", `String "obscure:model"]
    | ObscureUnknownCallee -> `Assoc [prefix ^ "via", `String "obscure:unknown-callee"]
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
    | Crtex -> `Assoc [prefix ^ "via", `String "crtex"]


  let simple_via ~allowed name =
    if List.mem allowed name ~equal:String.equal then
      Ok (SimpleVia name)
    else
      Error (Format.sprintf "Unrecognized Via annotation `%s`" name)
end

module BreadcrumbInterned = MakeInterner (Breadcrumb)

module BreadcrumbSet = Abstract.OverUnderSetDomain.MakeWithSet (struct
  include Data_structures.BitSetPatriciaTreeIntSet.Make (struct
    let common_integers = TaintAnalysisFeatureStats.common_breadcrumbs
  end)

  let show_element = BreadcrumbInterned.show

  let element_name = BreadcrumbInterned.name
end)

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

  let via_value_of_breadcrumb ?tag ~arguments =
    let open Ast.Expression.Call.Argument in
    let extract_constant_value arguments =
      List.find_map
        ~f:(fun argument -> Interprocedural.CallResolution.extract_constant_name argument.value)
        arguments
    in
    let argument_kind = function
      | { value = { Node.value = Starred (Once _); _ }; _ } -> "args"
      | { value = { Node.value = Starred (Twice _); _ }; _ } -> "kwargs"
      | { value = _; name = Some _ } -> "named"
      | { value = _; name = None } -> "positional"
    in
    let generate_kind arguments =
      List.map ~f:argument_kind arguments
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:"_or_"
    in
    let feature =
      match arguments with
      | [] -> "<missing>"
      | arguments -> (
          match extract_constant_value arguments with
          | Some value -> value
          | None -> Format.asprintf "<unknown:%s>" (generate_kind arguments))
    in
    Breadcrumb.ViaValue { value = feature; tag } |> BreadcrumbInterned.intern


  let via_type_of_breadcrumb ?tag ~resolution ~argument =
    let feature =
      argument
      >>| Interprocedural.CallResolution.resolve_ignoring_untracked ~resolution
      >>| Type.weaken_literals
      |> Option.value ~default:Type.Top
      |> Type.show
    in
    Breadcrumb.ViaType { value = feature; tag } |> BreadcrumbInterned.intern


  let via_type_of_breadcrumb_for_object ?tag ~resolution ~object_target =
    let feature =
      object_target
      |> Reference.create
      |> Resolution.resolve_reference resolution
      |> Type.weaken_literals
      |> Type.show
    in
    Breadcrumb.ViaType { value = feature; tag } |> BreadcrumbInterned.intern


  let to_json via =
    let to_json_via_value_or_type kind parameter tag =
      let json = ["kind", `String kind; "parameter", `String (AccessPath.Root.show parameter)] in
      let json =
        match tag with
        | Some tag -> ("tag", `String tag) :: json
        | None -> json
      in
      `Assoc json
    in
    match via with
    | ViaValueOf { parameter; tag } -> to_json_via_value_or_type "ViaValueOf" parameter tag
    | ViaTypeOf { parameter; tag } -> to_json_via_value_or_type "ViaTypeOf" parameter tag
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


  let to_json path = `String (Abstract.TreeDomain.Label.show_path path)
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

(* We need to make all breadcrumb creation lazy because the shared memory might
 * not be initialized yet. *)

let memoize closure () = Lazy.force closure

let memoize_breadcrumb_interned breadcrumb =
  memoize (lazy (breadcrumb |> BreadcrumbInterned.intern))


let obscure_model = memoize_breadcrumb_interned Breadcrumb.ObscureModel

let obscure_unknown_callee = memoize_breadcrumb_interned Breadcrumb.ObscureUnknownCallee

let lambda = memoize_breadcrumb_interned Breadcrumb.Lambda

let tito = memoize_breadcrumb_interned Breadcrumb.Tito

let format_string = memoize_breadcrumb_interned Breadcrumb.FormatString

let type_scalar = memoize_breadcrumb_interned (Breadcrumb.Type "scalar")

let type_bool = memoize_breadcrumb_interned (Breadcrumb.Type "bool")

let type_integer = memoize_breadcrumb_interned (Breadcrumb.Type "integer")

let type_enumeration = memoize_breadcrumb_interned (Breadcrumb.Type "enumeration")

let broadening = memoize_breadcrumb_interned Breadcrumb.Broadening

let issue_broadening = memoize_breadcrumb_interned Breadcrumb.IssueBroadening

let string_concat_left_hand_side =
  memoize_breadcrumb_interned (Breadcrumb.SimpleVia "string_concat_lhs")


let string_concat_right_hand_side =
  memoize_breadcrumb_interned (Breadcrumb.SimpleVia "string_concat_rhs")


let memoize_breadcrumb_set breadcrumbs =
  memoize
    (lazy
      (breadcrumbs
      |> List.map ~f:(fun breadcrumb ->
             Abstract.Domain.Part (BreadcrumbSet.Element, BreadcrumbInterned.intern breadcrumb))
      |> BreadcrumbSet.create))


let widen_broadening_set =
  memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.WidenBroadening]


let tito_broadening_set = memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.TitoBroadening]

let issue_broadening_set =
  memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.IssueBroadening]


let type_bool_scalar_set = memoize_breadcrumb_set [Breadcrumb.Type "scalar"; Breadcrumb.Type "bool"]

let type_breadcrumbs
    { Interprocedural.CallGraph.ReturnType.is_boolean; is_integer; is_float; is_enumeration }
  =
  let is_scalar = is_boolean || is_integer || is_float || is_enumeration in
  let add_if condition breadcrumb features =
    if condition then
      BreadcrumbSet.add (breadcrumb ()) features
    else
      features
  in
  BreadcrumbSet.bottom
  |> add_if is_scalar type_scalar
  |> add_if is_boolean type_bool
  |> add_if is_integer type_integer
  |> add_if is_enumeration type_enumeration


let expand_via_features ~resolution ~callees ~arguments via_features =
  let expand_via_feature via_feature breadcrumbs =
    let match_all_arguments_to_parameter parameter =
      AccessPath.match_actuals_to_formals arguments [parameter]
      |> List.filter_map ~f:(fun (argument, matches) ->
             if not (List.is_empty matches) then
               Some argument
             else
               None)
    in
    let match_argument_to_parameter parameter =
      match match_all_arguments_to_parameter parameter with
      | [] -> None
      | argument :: _ -> Some argument.value
    in
    match via_feature with
    | ViaFeature.ViaValueOf { parameter; tag } ->
        let arguments = match_all_arguments_to_parameter parameter in
        BreadcrumbSet.add (ViaFeature.via_value_of_breadcrumb ?tag ~arguments) breadcrumbs
    | ViaFeature.ViaTypeOf { parameter; tag } ->
        let breadcrumb =
          match callees with
          | [Interprocedural.Target.Object object_target] ->
              ViaFeature.via_type_of_breadcrumb_for_object ?tag ~resolution ~object_target
          | _ ->
              ViaFeature.via_type_of_breadcrumb
                ?tag
                ~resolution
                ~argument:(match_argument_to_parameter parameter)
        in
        BreadcrumbSet.add breadcrumb breadcrumbs
  in
  ViaFeatureSet.fold
    ViaFeatureSet.Element
    ~f:expand_via_feature
    ~init:BreadcrumbSet.empty
    via_features
