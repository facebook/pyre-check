(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Features: implements features, which are bits of informations carried on a
 * taint. Those are essential to help users triage issues.
 * These features are used to build the taint representation in `Domains`.
 *)

open Core
open Ast
open Pyre
module PyrePysaApi = Interprocedural.PyrePysaApi
module AccessPath = Interprocedural.AccessPath

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

    let prefix = Hack_parallel.Std.Prefix.make ()

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

module LocalFirstIndexSet = Abstract.WrapperDomain.Make (struct
  include FirstIndexSet

  let name = "local indexes"
end)

module PropagatedFirstIndexSet = Abstract.WrapperDomain.Make (struct
  include FirstIndexSet

  let name = "propagated indexes"
end)

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

module LocalFirstFieldSet = Abstract.WrapperDomain.Make (struct
  include FirstFieldSet

  let name = "local fields"
end)

module PropagatedFirstFieldSet = Abstract.WrapperDomain.Make (struct
  include FirstFieldSet

  let name = "propagated fields"
end)

module TitoPosition = struct
  let name = "tito positions"

  type t = Location.t [@@deriving show, compare, equal]

  let max_count =
    let on_first_call =
      lazy
        (TaintConfiguration.SharedMemory.get_global ()
        |> Option.value ~default:TaintConfiguration.Heap.default
        |> TaintConfiguration.maximum_tito_positions)
    in
    fun () -> Lazy.force on_first_call
end

module TitoPositionSet = Abstract.ToppedSetDomain.Make (TitoPosition)

module LeafPort = struct
  type t =
    | Leaf of {
        root: string;
        path: AccessPath.Path.t;
      }
    | Producer of {
        id: int;
        port: string;
      }
    | Anchor of { port: string }
  [@@deriving equal]

  let pp formatter = function
    | Leaf { root; path } -> Format.fprintf formatter "Leaf(%s%a)" root AccessPath.Path.pp path
    | Producer { id; port } -> Format.fprintf formatter "Producer(%d, %s)" id port
    | Anchor { port } -> Format.fprintf formatter "Anchor(%s)" port


  let show = Format.asprintf "%a" pp

  let pp_external formatter = function
    | Leaf { root; path } -> Format.fprintf formatter "leaf:%s%a" root AccessPath.Path.pp path
    | Producer { id; port } -> Format.fprintf formatter "producer:%d:%s" id port
    | Anchor { port } -> Format.fprintf formatter "anchor:%s" port


  let show_external = Format.asprintf "%a" pp_external

  let from_access_path ~root ~path =
    let root =
      match root with
      | AccessPath.Root.LocalResult -> "return"
      | AccessPath.Root.PositionalParameter { name; _ }
      | AccessPath.Root.NamedParameter { name }
      | AccessPath.Root.CapturedVariable { name; _ } ->
          name
      | AccessPath.Root.StarParameter _ -> "*"
      | AccessPath.Root.StarStarParameter _ -> "**"
      | AccessPath.Root.Variable _ -> failwith "unexpected port in apply_call"
    in
    Leaf { root; path }
end

module LeafName = struct
  let name = "leaf names"

  type t = {
    leaf: string;
    port: LeafPort.t;
  }
  [@@deriving equal]

  let pp formatter { leaf; port } =
    Format.fprintf formatter "LeafName(%s, port=%a)" leaf LeafPort.pp port


  let show = Format.asprintf "%a" pp

  let to_json { leaf; port } =
    `Assoc ["name", `String leaf; "port", `String (LeafPort.show_external port)]
end

module LeafNameInterned = MakeInterner (LeafName)
module LeafNameSet = MakeAbstractSetFromInterner (LeafNameInterned)

module Breadcrumb = struct
  let name = "breadcrumbs"

  type t =
    | FormatString (* Via f"{something}" *)
    | ObscureModel
    | ObscureUnknownCallee
    | HigherOrderParameter
    | CapturedVariable
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
    | ViaAttributeName of {
        tag: string option;
        value: string;
      }
    (* Via inferred from ViaAttributeName. *)
    | Tito (* Taint In Taint Out *)
    | Type of string (* Type constraint *)
    | Broadening (* Taint tree was collapsed for various reasons *)
    | WidenBroadening (* Taint tree was collapsed during widening *)
    | TitoBroadening (* Taint tree was collapsed when applying tito *)
    | ModelBroadening (* Taint tree was collapsed during model broadening *)
    | ModelSourceBroadening (* Source tree was collapsed during model broadening *)
    | ModelSinkBroadening (* Sink tree was collapsed during model broadening *)
    | ModelTitoBroadening (* Tito tree was collapsed during model broadening *)
    | ModelShaping (* Taint tree was collapsed during model shaping *)
    | ModelSourceShaping (* Source tree was collapsed during model shaping *)
    | ModelSinkShaping (* Sink tree was collapsed during model shaping *)
    | ModelTitoShaping (* Tito tree was collapsed during model shaping *)
    | IssueBroadening (* Taint tree was collapsed when matching sources and sinks *)
    | ShimBroadening (* Taint tree was collapsed when applying shims *)
    | Crtex (* Taint comes from the Cross Repository Taint EXchange *)
    | TransformTitoDepth of int
    | PropagatedReturnSink
      (* Sink taint that originate from the return statements AND is propagated up *)
  [@@deriving equal, compare]

  let pp formatter breadcrumb =
    let pp_via header tag value =
      match tag with
      | None -> Format.fprintf formatter "%s[%s]" header value
      | Some tag -> Format.fprintf formatter "%s[%s, tag=%s]" header value tag
    in
    match breadcrumb with
    | FormatString -> Format.fprintf formatter "FormatString"
    | ObscureModel -> Format.fprintf formatter "ObscureModel"
    | ObscureUnknownCallee -> Format.fprintf formatter "ObscureUnknownCallee"
    | HigherOrderParameter -> Format.fprintf formatter "HigherOrderParameter"
    | CapturedVariable -> Format.fprintf formatter "CapturedVariable"
    | SimpleVia name -> Format.fprintf formatter "SimpleVia[%s]" name
    | ViaValue { tag; value } -> pp_via "ViaValue" tag value
    | ViaType { tag; value } -> pp_via "ViaType" tag value
    | ViaAttributeName { tag; value } -> pp_via "ViaAttributeName" tag value
    | Tito -> Format.fprintf formatter "Tito"
    | Type name -> Format.fprintf formatter "Type[%s]" name
    | Broadening -> Format.fprintf formatter "Broadening"
    | WidenBroadening -> Format.fprintf formatter "WidenBroadening"
    | TitoBroadening -> Format.fprintf formatter "TitoBroadening"
    | ModelBroadening -> Format.fprintf formatter "ModelBroadening"
    | ModelSourceBroadening -> Format.fprintf formatter "ModelSourceBroadening"
    | ModelSinkBroadening -> Format.fprintf formatter "ModelSinkBroadening"
    | ModelTitoBroadening -> Format.fprintf formatter "ModelTitoBroadening"
    | ModelShaping -> Format.fprintf formatter "ModelShaping"
    | ModelSourceShaping -> Format.fprintf formatter "ModelSourceShaping"
    | ModelSinkShaping -> Format.fprintf formatter "ModelSinkShaping"
    | ModelTitoShaping -> Format.fprintf formatter "ModelTitoShaping"
    | IssueBroadening -> Format.fprintf formatter "IssueBroadening"
    | ShimBroadening -> Format.fprintf formatter "ShimBroadening"
    | Crtex -> Format.fprintf formatter "Crtex"
    | TransformTitoDepth depth -> Format.fprintf formatter "TransformTitoDepth(%d)" depth
    | PropagatedReturnSink -> Format.fprintf formatter "PropagatedReturnSink"


  let show = Format.asprintf "%a" pp

  let to_json ~on_all_paths breadcrumb =
    let prefix = if on_all_paths then "always-" else "" in
    let via_to_json ~via_kind ~tag ~value =
      match tag with
      | None -> `Assoc [Format.sprintf "%svia-%s" prefix via_kind, `String value]
      | Some tag -> `Assoc [Format.sprintf "%svia-%s-%s" prefix tag via_kind, `String value]
    in
    match breadcrumb with
    | FormatString -> `Assoc [prefix ^ "via", `String "format-string"]
    | ObscureModel -> `Assoc [prefix ^ "via", `String "obscure:model"]
    | ObscureUnknownCallee -> `Assoc [prefix ^ "via", `String "obscure:unknown-callee"]
    | HigherOrderParameter -> `Assoc [prefix ^ "via", `String "higher-order-parameter"]
    | CapturedVariable -> `Assoc [prefix ^ "via", `String "captured-variable"]
    | SimpleVia name -> `Assoc [prefix ^ "via", `String name]
    | ViaValue { tag; value } -> via_to_json ~via_kind:"value" ~tag ~value
    | ViaType { tag; value } -> via_to_json ~via_kind:"type" ~tag ~value
    | ViaAttributeName { tag; value } -> via_to_json ~via_kind:"attribute" ~tag ~value
    | Tito -> `Assoc [prefix ^ "via", `String "tito"]
    | Type name -> `Assoc [prefix ^ "type", `String name]
    | Broadening -> `Assoc [prefix ^ "via", `String "broadening"]
    | WidenBroadening -> `Assoc [prefix ^ "via", `String "widen-broadening"]
    | TitoBroadening -> `Assoc [prefix ^ "via", `String "tito-broadening"]
    | ModelBroadening -> `Assoc [prefix ^ "via", `String "model-broadening"]
    | ModelSourceBroadening -> `Assoc [prefix ^ "via", `String "model-source-broadening"]
    | ModelSinkBroadening -> `Assoc [prefix ^ "via", `String "model-sink-broadening"]
    | ModelTitoBroadening -> `Assoc [prefix ^ "via", `String "model-tito-broadening"]
    | ModelShaping -> `Assoc [prefix ^ "via", `String "model-shaping"]
    | ModelSourceShaping -> `Assoc [prefix ^ "via", `String "model-source-shaping"]
    | ModelSinkShaping -> `Assoc [prefix ^ "via", `String "model-sink-shaping"]
    | ModelTitoShaping -> `Assoc [prefix ^ "via", `String "model-tito-shaping"]
    | IssueBroadening -> `Assoc [prefix ^ "via", `String "issue-broadening"]
    | ShimBroadening -> `Assoc [prefix ^ "via", `String "shim-broadening"]
    | Crtex -> `Assoc [prefix ^ "via", `String "crtex"]
    | TransformTitoDepth depth ->
        `Assoc [prefix ^ "via", `String (Format.sprintf "transform-tito-depth:%d" depth)]
    | PropagatedReturnSink -> `Assoc [prefix ^ "via", `String "propagated-return-sink"]


  let simple_via ~allowed name =
    if List.mem allowed name ~equal:String.equal then
      Ok (SimpleVia name)
    else
      Error (Format.sprintf "Unrecognized Via annotation `%s`" name)
end

module BreadcrumbInterned = MakeInterner (Breadcrumb)

module BreadcrumbSet = struct
  include Data_structures.BitSetPatriciaTreeIntSet.Make (struct
    let common_integers = TaintAnalysisFeatureStats.common_breadcrumbs
  end)

  let show_element = BreadcrumbInterned.show

  let element_name = BreadcrumbInterned.name
end

module BreadcrumbMayAlwaysSet = Abstract.OverUnderSetDomain.MakeWithSet (BreadcrumbSet)

(* Stores local breadcrumbs (also called trace breadcrumbs) inferred during the analysis of the
   current function. *)
module LocalBreadcrumbSet = Abstract.WrapperDomain.Make (struct
  include BreadcrumbMayAlwaysSet

  let name = "local breadcrumbs"
end)

(* Stores local breadcrumbs that are kind-specific, unlike `LocalBreadcrumbSet`. For instance,
   `via:transform-tito-depth:N` should only be emitted on transform kinds. *)
module LocalKindSpecificBreadcrumbSet = Abstract.WrapperDomain.Make (struct
  include BreadcrumbMayAlwaysSet

  let name = "local kind-specific breadcrumbs"
end)

(* Stores breadcrumbs propagated from the callee. *)
module PropagatedBreadcrumbSet = Abstract.WrapperDomain.Make (struct
  include BreadcrumbMayAlwaysSet

  let name = "propagated breadcrumbs"
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
    | ViaAttributeName of { tag: string option }
  [@@deriving compare, equal]

  let pp formatter simple =
    let pp_via header parameter tag =
      match parameter, tag with
      | None, None -> Format.fprintf formatter "%s" header
      | None, Some tag -> Format.fprintf formatter "%s[tag=%s]" header tag
      | Some parameter, None ->
          Format.fprintf formatter "%s[%a]" header AccessPath.Root.pp_for_via_breadcrumb parameter
      | Some parameter, Some tag ->
          Format.fprintf
            formatter
            "%s[%a, tag=%s]"
            header
            AccessPath.Root.pp_for_via_breadcrumb
            parameter
            tag
    in
    match simple with
    | ViaValueOf { parameter; tag } -> pp_via "ViaValueOf" (Some parameter) tag
    | ViaTypeOf { parameter; tag } -> pp_via "ViaTypeOf" (Some parameter) tag
    | ViaAttributeName { tag } -> pp_via "ViaAttributeName" None tag


  let show = Format.asprintf "%a" pp

  let via_value_of_breadcrumb ?tag ~arguments () =
    let open Ast.Expression.Call.Argument in
    let extract_constant_value arguments =
      List.find_map ~f:(fun argument -> AccessPath.extract_constant_name argument.value) arguments
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


  let via_type_of_breadcrumb
      ?tag
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~caller
      ~argument
      ()
    =
    let feature =
      argument
      >>| Interprocedural.TypeOfExpressionSharedMemory.compute_or_retrieve_type
            type_of_expression_shared_memory
            ~pyre_in_context
            ~callable:caller
      >>| Type.weaken_literals
      |> Option.value ~default:Type.Top
      |> Type.show
    in
    Breadcrumb.ViaType { value = feature; tag } |> BreadcrumbInterned.intern


  let via_type_of_breadcrumb_for_object ?tag ~pyre_in_context ~object_target () =
    let feature =
      object_target
      |> Reference.create
      |> PyrePysaApi.InContext.resolve_reference pyre_in_context
      |> Type.weaken_literals
      |> Type.show
    in
    Breadcrumb.ViaType { value = feature; tag } |> BreadcrumbInterned.intern


  let via_attribute_name_breadcrumb_for_object ?tag ~object_target () =
    Breadcrumb.ViaAttributeName { value = object_target |> Reference.create |> Reference.last; tag }
    |> BreadcrumbInterned.intern


  let to_json via =
    let via_to_json kind parameter tag =
      let json = ["kind", `String kind] in
      let json =
        match parameter with
        | Some parameter ->
            ("parameter", `String (AccessPath.Root.show_for_via_breadcrumb parameter)) :: json
        | None -> json
      in
      let json =
        match tag with
        | Some tag -> ("tag", `String tag) :: json
        | None -> json
      in
      `Assoc (List.rev json)
    in
    match via with
    | ViaValueOf { parameter; tag } -> via_to_json "ViaValueOf" (Some parameter) tag
    | ViaTypeOf { parameter; tag } -> via_to_json "ViaTypeOf" (Some parameter) tag
    | ViaAttributeName { tag } -> via_to_json "ViaAttributeName" None tag
end

module ViaFeatureSet = Abstract.SetDomain.Make (ViaFeature)

module MakeScalarDomain (Name : sig
  val name : string
end) =
Abstract.SimpleDomain.Make (struct
  type t = int

  let name = Name.name

  let equal = Int.equal

  let join = Int.min

  let meet = Int.max

  let less_or_equal ~left ~right = left >= right

  let bottom = Int.max_value

  let pp formatter length =
    if Int.equal length Int.min_value then
      Format.fprintf formatter "<bottom>"
    else
      Format.fprintf formatter "%d" length


  let show = Format.asprintf "%a" pp
end)

module CollapseDepth = struct
  include MakeScalarDomain (struct
    let name = "collapse depth"
  end)

  (* A special value that disables collapsing, which is also the maximum depth. *)
  let no_collapse = 999999

  let should_collapse depth = depth < no_collapse

  let approximate _ = 0

  let transform_on_widening_collapse _ = 0

  let transform_on_sink = function
    | 0 -> 0
    | _ -> bottom


  let transform_on_hoist collapse_depth =
    if is_bottom collapse_depth then
      bottom
    else
      0
end

module ReturnAccessPath = struct
  type t = AccessPath.Path.t

  let show = AccessPath.Path.show
end

module ReturnAccessPathTree = struct
  let maximum_depth =
    let cache_first_call =
      lazy
        (TaintConfiguration.SharedMemory.get_global ()
        |> Option.value ~default:TaintConfiguration.Heap.default
        |> TaintConfiguration.maximum_return_access_path_depth_after_widening)
    in
    fun () -> Lazy.force cache_first_call


  let maximum_width =
    let cache_first_call =
      lazy
        (TaintConfiguration.SharedMemory.get_global ()
        |> Option.value ~default:TaintConfiguration.Heap.default
        |> TaintConfiguration.maximum_return_access_path_width)
    in
    fun () -> Lazy.force cache_first_call


  module Tree =
    Abstract.TreeDomain.Make
      (struct
        let max_tree_depth_after_widening = maximum_depth

        let check_invariants = TaintConfiguration.runtime_check_invariants ()
      end)
      (CollapseDepth)
      ()

  include Tree

  let limit_width tree =
    Tree.limit_to ~width:(maximum_width ()) ~transform:CollapseDepth.approximate tree


  let limit_depth tree =
    Tree.collapse_to ~depth:(maximum_depth ()) ~transform:CollapseDepth.approximate tree


  let widen ~iteration ~prev ~next = Tree.widen ~iteration ~prev ~next |> limit_width

  let to_json tree =
    let path_to_json (path, collapse_depth) json_list =
      (ReturnAccessPath.show path, `Int collapse_depth) :: json_list
    in
    Tree.fold Tree.Path ~f:path_to_json tree ~init:[]
end

(* We need to make all breadcrumb creation lazy because the shared memory might
 * not be initialized yet. *)

let memoize closure () = Lazy.force closure

let memoize_breadcrumb_interned breadcrumb =
  memoize (lazy (breadcrumb |> BreadcrumbInterned.intern))


let obscure_model = memoize_breadcrumb_interned Breadcrumb.ObscureModel

let obscure_unknown_callee = memoize_breadcrumb_interned Breadcrumb.ObscureUnknownCallee

let higher_order_parameter = memoize_breadcrumb_interned Breadcrumb.HigherOrderParameter

let captured_variable = memoize_breadcrumb_interned Breadcrumb.CapturedVariable

let tito = memoize_breadcrumb_interned Breadcrumb.Tito

let format_string = memoize_breadcrumb_interned Breadcrumb.FormatString

let type_scalar = memoize_breadcrumb_interned (Breadcrumb.Type "scalar")

let type_bool = memoize_breadcrumb_interned (Breadcrumb.Type "bool")

let type_integer = memoize_breadcrumb_interned (Breadcrumb.Type "integer")

let type_enumeration = memoize_breadcrumb_interned (Breadcrumb.Type "enumeration")

let broadening = memoize_breadcrumb_interned Breadcrumb.Broadening

let widen_broadening = memoize_breadcrumb_interned Breadcrumb.WidenBroadening

let issue_broadening = memoize_breadcrumb_interned Breadcrumb.IssueBroadening

let propagated_return_sink = memoize_breadcrumb_interned Breadcrumb.PropagatedReturnSink

let string_concat_left_hand_side =
  memoize_breadcrumb_interned (Breadcrumb.SimpleVia "string_concat_lhs")


let string_concat_right_hand_side =
  memoize_breadcrumb_interned (Breadcrumb.SimpleVia "string_concat_rhs")


let memoize_breadcrumb_set breadcrumbs =
  memoize
    (lazy (breadcrumbs |> List.map ~f:BreadcrumbInterned.intern |> BreadcrumbMayAlwaysSet.of_list))


let widen_broadening_set =
  memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.WidenBroadening]


let tito_broadening_set = memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.TitoBroadening]

let model_broadening_set =
  memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.ModelBroadening]


let model_source_broadening_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelBroadening; Breadcrumb.ModelSourceBroadening]


let model_sink_broadening_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelBroadening; Breadcrumb.ModelSinkBroadening]


let model_tito_broadening_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelBroadening; Breadcrumb.ModelTitoBroadening]


let model_source_shaping_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelShaping; Breadcrumb.ModelSourceShaping]


let model_sink_shaping_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelShaping; Breadcrumb.ModelSinkShaping]


let model_tito_shaping_set =
  memoize_breadcrumb_set
    [Breadcrumb.Broadening; Breadcrumb.ModelShaping; Breadcrumb.ModelTitoShaping]


let issue_broadening_set =
  memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.IssueBroadening]


let shim_broadening_set = memoize_breadcrumb_set [Breadcrumb.Broadening; Breadcrumb.ShimBroadening]

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
  BreadcrumbSet.empty
  |> add_if is_scalar type_scalar
  |> add_if is_boolean type_bool
  |> add_if is_integer type_integer
  |> add_if is_enumeration type_enumeration


let type_breadcrumbs_from_annotation ~pyre_api type_ =
  let open Interprocedural.CallGraph in
  type_
  >>| ReturnType.from_annotation ~pyre_api
  |> Option.value ~default:ReturnType.none
  |> type_breadcrumbs


let expand_via_features
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~caller
    ~callee
    ~arguments
    via_features
  =
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
        BreadcrumbSet.add (ViaFeature.via_value_of_breadcrumb ?tag ~arguments ()) breadcrumbs
    | ViaFeature.ViaTypeOf { parameter; tag } ->
        let breadcrumb =
          match Interprocedural.Target.get_regular callee with
          | Interprocedural.Target.Regular.Object object_target ->
              ViaFeature.via_type_of_breadcrumb_for_object ?tag ~pyre_in_context ~object_target ()
          | _ ->
              ViaFeature.via_type_of_breadcrumb
                ?tag
                ~pyre_in_context
                ~argument:(match_argument_to_parameter parameter)
                ~type_of_expression_shared_memory
                ~caller
                ()
        in
        BreadcrumbSet.add breadcrumb breadcrumbs
    | ViaFeature.ViaAttributeName { tag } -> (
        match Interprocedural.Target.get_regular callee with
        | Interprocedural.Target.Regular.Object object_target ->
            let breadcrumb =
              ViaFeature.via_attribute_name_breadcrumb_for_object ?tag ~object_target ()
            in
            BreadcrumbSet.add breadcrumb breadcrumbs
        | _ -> breadcrumbs)
  in
  ViaFeatureSet.fold
    ViaFeatureSet.Element
    ~f:expand_via_feature
    ~init:BreadcrumbSet.empty
    via_features
