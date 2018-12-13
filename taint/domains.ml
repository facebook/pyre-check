(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis


module type TAINT_SET = sig
  include AbstractDomain.S

  type element
  [@@deriving compare]

  val element: element AbstractDomain.part

  val add: t -> element -> t
  val of_list: element list -> t
  val to_json: t -> Yojson.Safe.json list
  val singleton: element -> t
end


module type SET_ARG = sig
  include Set.Elt

  val show: t -> string
end


module Set(Element: SET_ARG) : TAINT_SET with type element = Element.t = struct

  module Set = Analysis.AbstractSetDomain.Make(Element)
  include Set

  let element = Set.Element

  type element = Element.t
  [@@deriving compare]

  let show set =
    elements set
    |> List.map ~f:Element.show
    |> String.concat ~sep:", "
    |> Format.sprintf "{%s}"

  let to_json set =
    let element_to_json element =
      let kind = `String (Element.show element) in
      `Assoc ["kind", kind]
    in
    elements set
    |> List.map ~f:element_to_json
end


let location_to_json ?(include_filename = true) location : Yojson.Safe.json =
  let line = Location.line location in
  let column = Location.column location in
  let end_column =
    Location.stop_column location  (* Note: not correct for multiple line span *)
  in
  let optionally_add_filename fields =
    if include_filename then
      let path = Location.path location in
      ("filename", `String path) :: fields
    else
      fields
  in
  let fields = [
    "line", `Int line;
    "start", `Int column;
    "end", `Int end_column;
  ] |> optionally_add_filename
  in
  `Assoc fields


module TraceInfo = struct
  type t =
    | Declaration
    | Origin of Location.t
    | CallSite of {
        port: AccessPath.Root.t;
        path: AbstractTreeDomain.Label.path;
        location: Location.t;
        callees: Interprocedural.Callable.t list;
        trace_length: int;
      }
  [@@deriving compare, sexp, show]

  let show = function
    | Declaration -> "declaration"
    | Origin location ->
        let instantiated =
          Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
        in
        Format.sprintf
          "@%s"
          (Location.Instantiated.show instantiated)
    | CallSite { location; callees; _ } ->
        let instantiated =
          Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
        in
        Format.sprintf
          "via call@%s[%s]"
          (Location.Instantiated.show instantiated)
          (String.concat
             ~sep:" "
             (List.map ~f:Interprocedural.Callable.external_target_name callees))

  (* Breaks recursion among trace info and overall taint domain. *)
  let has_significant_summary =
    ref (
      fun
        (_: AccessPath.Root.t)
        (_: AbstractTreeDomain.Label.path)
        (_: Interprocedural.Callable.non_override_target)
        -> true
    )

  (* Returns the (dictionary key * json) to emit *)
  let to_json trace : (string * Yojson.Safe.json) option =
    match trace with
    | Declaration ->
        Some ("decl", `Null)
    | Origin location ->
        let location_json =
          Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
          |> location_to_json
        in
        Some ("root", location_json)
    | CallSite { location; callees; port; path; trace_length; } ->
        let callee_json =
          Interprocedural.DependencyGraph.expand_callees callees
          |> List.filter ~f:(!has_significant_summary port path)
          |> List.map
            ~f:(fun callable -> `String (Interprocedural.Callable.external_target_name callable))
        in
        if callee_json <> [] then
          let location_json =
            Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
            |> location_to_json
          in
          let port_json = AccessPath.create port path |> AccessPath.to_json in
          let call_json =
            `Assoc [
              "position", location_json;
              "resolves_to", `List callee_json;
              "port", port_json;
              "length", `Int trace_length;
            ]
          in
          Some ("call", call_json)
        else
          None

  let less_or_equal ~left ~right =
    match left, right with
    | CallSite {
        path = path_left;
        location = location_left;
        port = port_left;
        callees = callees_left;
        trace_length = trace_length_left
      },
      CallSite {
        path = path_right;
        location = location_right;
        port = port_right;
        callees = callees_right;
        trace_length = trace_length_right;
      } ->
        port_left = port_right
        && Location.compare location_left location_right = 0
        && callees_left = callees_right
        && trace_length_right <= trace_length_left
        && AbstractTreeDomain.Label.is_prefix ~prefix:path_right path_left
    | _ ->
        left = right

  let widen set = set
end
module TraceInfoSet = AbstractElementSetDomain.Make(TraceInfo)


(* Simple set of features that are unrelated, thus cheap to maintain *)
module SimpleFeatures = struct
  type t =
    | LeafName of string
    | TitoPosition of Location.Reference.t
  [@@deriving show, sexp, compare]
end
module SimpleFeatureSet = AbstractSetDomain.Make(SimpleFeatures)


(* Set of complex features, where element can be abstracted and joins are
   expensive. Should only be used for elements that need this kind of
   joining.
*)
module ComplexFeatures = struct
  type t =
    | ReturnAccessPath of AbstractTreeDomain.Label.path
  [@@deriving show, sexp, compare]

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
module ComplexFeatureSet = AbstractElementSetDomain.Make(ComplexFeatures)


module FlowDetails = struct
  module Slots = struct
    type 'a slot =
      | TraceInfo: TraceInfoSet.t slot
      | SimpleFeature: SimpleFeatureSet.t slot
      | ComplexFeature: ComplexFeatureSet.t slot

    let slot_name (type a) (slot: a slot) =
      match slot with
      | TraceInfo -> "TraceInfo"
      | SimpleFeature -> "SimpleFeature"
      | ComplexFeature -> "ComplexFeature"

    let slot_domain (type a) (slot: a slot) =
      match slot with
      | TraceInfo -> (module TraceInfoSet : AbstractDomain.S with type t = a)
      | SimpleFeature -> (module SimpleFeatureSet : AbstractDomain.S with type t = a)
      | ComplexFeature -> (module ComplexFeatureSet : AbstractDomain.S with type t = a)
  end

  include AbstractProductDomain.Make(Slots)

  let initial = product [
      (Element (Slots.TraceInfo, TraceInfoSet.singleton TraceInfo.Declaration));
    ]

  let trace_info = ProductSlot (Slots.TraceInfo, TraceInfoSet.Element)
  let simple_feature = ProductSlot (Slots.SimpleFeature, SimpleFeatureSet.Element)
  let simple_feature_set = ProductSlot (Slots.SimpleFeature, SimpleFeatureSet.Set)

  let gather_leaf_names accumulator = function
    | SimpleFeatures.LeafName name ->
        name :: accumulator
    | TitoPosition _ ->
        accumulator

  let complex_feature = ProductSlot (Slots.ComplexFeature, ComplexFeatureSet.Element)
  let complex_feature_set = ProductSlot (Slots.ComplexFeature, ComplexFeatureSet.Set)

end


module type TAINT_DOMAIN = sig
  include AbstractDomain.S

  type leaf
  val leaf: leaf AbstractDomain.part
  val trace_info: TraceInfo.t AbstractDomain.part
  val simple_feature: SimpleFeatures.t AbstractDomain.part
  val simple_feature_set: SimpleFeatures.t list AbstractDomain.part
  val complex_feature: ComplexFeatures.t AbstractDomain.part
  val complex_feature_set: ComplexFeatures.t list AbstractDomain.part

  (* Add trace info at call-site *)
  val apply_call:
    Location.t
    -> callees: Interprocedural.Callable.t list
    -> port: AccessPath.Root.t
    -> path: AbstractTreeDomain.Label.path
    -> element: t
    -> t

  val to_json: t -> Yojson.Safe.json

end


module MakeTaint(Leaf: SET_ARG) : sig
  include TAINT_DOMAIN with type leaf = Leaf.t
  val leaves: t -> leaf list
  val singleton: leaf -> t
  val of_list: leaf list -> t
end = struct
  module Key = struct
    include Leaf

    let absence_implicitly_maps_to_bottom = true
  end

  module Map = AbstractMapDomain.Make(Key)(FlowDetails)
  include Map

  type leaf = Leaf.t
  [@@deriving compare]

  let add map leaf =
    Map.set map ~key:leaf ~data:FlowDetails.initial

  let singleton leaf =
    add Map.bottom leaf

  let of_list leaves =
    List.fold leaves ~init:Map.bottom ~f:add

  let leaf = Map.Key
  let trace_info = FlowDetails.trace_info
  let simple_feature = FlowDetails.simple_feature
  let complex_feature = FlowDetails.complex_feature
  let simple_feature_set = FlowDetails.simple_feature_set
  let complex_feature_set = FlowDetails.complex_feature_set

  let leaves map =
    Map.fold leaf ~init:[] ~f:(Fn.flip List.cons) map

  let to_json taint =
    let element_to_json (leaf, features) =
      let trace_info =
        FlowDetails.(fold trace_info ~f:(Fn.flip List.cons) ~init:[] features)
        |> List.dedup_and_sort ~compare:TraceInfo.compare
      in
      let leaf_kind_json = `String (Leaf.show leaf) in
      let tito_positions, leaf_json =
        let gather_json (tito, leaves) = function
          | SimpleFeatures.LeafName name ->
              (tito, `Assoc ["kind", leaf_kind_json; "name", `String name] :: leaves)
          | TitoPosition location ->
              let tito_location_json =
                Location.instantiate ~lookup:(fun _ -> Some "") location
                |> location_to_json ~include_filename:false
              in
              (tito_location_json :: tito, leaves)
        in
        let gather_return_access_path leaves = function
          | ComplexFeatures.ReturnAccessPath path ->
              let path_name = AbstractTreeDomain.Label.show_path path in
              `Assoc ["kind", leaf_kind_json; "name", `String path_name] :: leaves
        in
        let tito_positions, leaves =
          FlowDetails.(fold simple_feature ~f:gather_json ~init:([], []) features)
        in
        tito_positions,
        FlowDetails.(fold complex_feature ~f:gather_return_access_path ~init:leaves features)
      in
      let trace_json = List.filter_map ~f:TraceInfo.to_json trace_info in
      let leaf_json =
        if leaf_json = [] then
          [`Assoc ["kind", leaf_kind_json]]
        else
          leaf_json
      in
      if List.is_empty trace_json then
        [`Assoc [
            "decl", `String "MISSING";
            "tito", `List tito_positions;
            "leaves", `List leaf_json;
          ]
        ]
      else
        List.map
          trace_json
          ~f:(fun trace_pair ->
              `Assoc [
                trace_pair;
                "tito", `List tito_positions;
                "leaves", `List leaf_json;
              ])
    in
    let elements =
      Map.to_alist taint
      |> List.concat_map ~f:element_to_json
    in
    `List elements

  let apply_call location ~callees ~port ~path ~element:taint =
    let open TraceInfo in
    let needs_leaf_name =
      let is_declaration is_declaration = function
        | Declaration -> true
        | _ -> is_declaration
      in
      Map.fold FlowDetails.trace_info ~init:false ~f:is_declaration taint
    in
    let call_trace = CallSite { location; callees; port; path; trace_length = 1 } in
    let translate = function
      | Origin _ ->
          call_trace
      | CallSite { trace_length; _ } ->
          CallSite { location; callees; port; path; trace_length = trace_length + 1 }
      | Declaration ->
          Origin location
    in
    let taint = Map.transform FlowDetails.trace_info ~f:translate taint in
    if needs_leaf_name then
      let add_leaf_names info_set =
        let add_leaf_name info_set callee =
          SimpleFeatures.LeafName (Interprocedural.Callable.external_target_name callee) :: info_set
        in
        List.fold callees ~f:add_leaf_name ~init:info_set
      in
      Map.transform FlowDetails.simple_feature_set ~f:add_leaf_names taint
    else
      taint

end


module ForwardTaint = MakeTaint(Sources)
module BackwardTaint = MakeTaint(Sinks)


module MakeTaintTree(Taint : TAINT_DOMAIN)() = struct
  include AbstractTreeDomain.Make(struct
      let max_tree_depth_after_widening = 4
      let check_invariants = true
    end)
      (Taint)
      ()

  let apply_call location ~callees ~port taint_tree =
    let transform_path {path; ancestors=_; tip} =
      {
        path;
        ancestors = Taint.bottom;
        tip = Taint.apply_call location ~callees ~port ~path ~element:tip;
      }
    in
    transform RawPath ~f:transform_path taint_tree

  let empty = bottom
  let is_empty = is_bottom

  (* Keep only non-essential structure. *)
  let essential tree =
    let essential_trace_info = function
      | TraceInfo.CallSite callsite ->
          TraceInfo.CallSite { callsite with trace_length = 100; }
      | default -> default
    in
    let essential_complex_features set =
      let simplify_feature = function
        | ComplexFeatures.ReturnAccessPath _ -> None
      in
      List.filter_map ~f:simplify_feature set
    in
    transform Taint.trace_info ~f:essential_trace_info tree
    |> transform Taint.complex_feature_set ~f:essential_complex_features
end


module MakeTaintEnvironment(Taint : TAINT_DOMAIN)() = struct
  module Tree = MakeTaintTree(Taint)()

  include AbstractMapDomain.Make(
    struct
      include AccessPath.Root
      let absence_implicitly_maps_to_bottom = true
    end)(Tree)

  let to_json environment =
    let element_to_json json_list (root, tree) =
      let path_to_json json_list { Tree.path; ancestors; tip } =
        let tip =
          let ancestor_leaf_names =
            Taint.fold
              FlowDetails.simple_feature
              ancestors
              ~f:FlowDetails.gather_leaf_names
              ~init:[]
            |> List.map ~f:(fun name -> SimpleFeatures.LeafName name)
          in
          let join_ancestor_leaf_names leaves =
            leaves @ ancestor_leaf_names
          in
          Taint.transform FlowDetails.simple_feature_set tip ~f:join_ancestor_leaf_names
        in
        let port =
          AccessPath.create root path
          |> AccessPath.to_json
        in
        (`Assoc [
            "port", port;
            "taint", Taint.to_json tip;
          ]
        ) :: json_list
      in
      Tree.fold Tree.RawPath ~f:path_to_json tree ~init:json_list
    in
    let paths =
      to_alist environment
      |> List.fold ~f:element_to_json ~init:[]
    in
    `List paths

  let assign ?(weak=false) ~root ~path subtree environment =
    let assign_tree = function
      | None ->
          Tree.assign ~weak ~tree:Tree.bottom path ~subtree
      | Some tree ->
          Tree.assign ~weak ~tree path ~subtree
    in
    update environment root ~f:assign_tree

  let read ?(transform_non_leaves = fun _ e -> e) ~root ~path environment =
    match find environment root with
    | None ->
        Tree.bottom
    | Some tree ->
        Tree.read ~transform_non_leaves path tree

  let empty = bottom
  let is_empty = is_bottom

  let roots environment =
    fold Key ~f:(Fn.flip List.cons) ~init:[] environment

end


(** Used to infer which sources reach the exit points of a function. *)
module ForwardState = MakeTaintEnvironment(ForwardTaint)()


(** Used to infer which sinks are reached from parameters, as well as the
    taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState = MakeTaintEnvironment(BackwardTaint)()


(* Special sink as it needs the return access path *)
let local_return_taint =
  BackwardTaint.create [
    Part (BackwardTaint.leaf, Sinks.LocalReturn);
    Part (BackwardTaint.trace_info, TraceInfo.Declaration);
    Part (BackwardTaint.complex_feature, ComplexFeatures.ReturnAccessPath [])
  ]
