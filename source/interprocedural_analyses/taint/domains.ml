(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

let location_to_json
    {
      Location.start = { line = start_line; column = start_column };
      stop = { line = end_line; column = end_column };
    }
    : Yojson.Safe.json
  =
  (* If the location spans multiple lines, we only return the position of the first character. *)
  `Assoc
    [
      "line", `Int start_line;
      "start", `Int start_column;
      "end", `Int (if start_line = end_line then end_column else start_column);
    ]


let location_with_module_to_json ~filename_lookup location_with_module : Yojson.Safe.json =
  let optionally_add_filename fields =
    match filename_lookup with
    | Some lookup ->
        let { Location.WithPath.path; _ } =
          Location.WithModule.instantiate ~lookup location_with_module
        in
        ("filename", `String path) :: fields
    | None -> fields
  in
  match location_to_json (Location.strip_module location_with_module) with
  | `Assoc fields -> `Assoc (optionally_add_filename fields)
  | _ -> failwith "unreachable"


module TraceInfo = struct
  let name = "trace"

  type t =
    (* User-specified taint on a model. *)
    | Declaration of { leaf_name_provided: bool }
    (* Leaf taint at the callsite of a tainted model, i.e the start or end of the trace. *)
    | Origin of Location.WithModule.t
    (* Taint propagated from a call. *)
    | CallSite of {
        port: AccessPath.Root.t;
        path: Abstract.TreeDomain.Label.path;
        location: Location.WithModule.t;
        callees: Interprocedural.Target.t list;
      }
  [@@deriving compare]

  let pp formatter = function
    | Declaration _ -> Format.fprintf formatter "Declaration"
    | Origin location -> Format.fprintf formatter "Origin(%a)" Location.WithModule.pp location
    | CallSite { location; callees; port; path } ->
        let port = AccessPath.create port path |> AccessPath.show in
        Format.fprintf
          formatter
          "CallSite(callees=[%s], location=%a, port=%s)"
          (String.concat
             ~sep:", "
             (List.map ~f:Interprocedural.Target.external_target_name callees))
          Location.WithModule.pp
          location
          port


  let show = Format.asprintf "%a" pp

  (* Breaks recursion among trace info and overall taint domain. *)
  (* See implementation in TaintResult. *)
  let has_significant_summary =
    ref
      (fun
        (_ : AccessPath.Root.t)
        (_ : Abstract.TreeDomain.Label.path)
        (_ : Interprocedural.Target.non_override_t)
      -> true)


  (* Only called when emitting models before we compute the json so we can dedup *)
  let expand_call_site trace =
    match trace with
    | CallSite { location; callees; port; path } ->
        let callees =
          Interprocedural.DependencyGraph.expand_callees callees
          |> List.filter ~f:(!has_significant_summary port path)
        in
        CallSite { location; callees = (callees :> Interprocedural.Target.t list); port; path }
    | _ -> trace


  let create_json ~filename_lookup ~trace_length trace : string * Yojson.Safe.json =
    match trace with
    | Declaration _ -> "decl", `Null
    | Origin location ->
        let location_json = location_with_module_to_json ~filename_lookup location in
        "root", location_json
    | CallSite { location; callees; port; path } ->
        let callee_json =
          callees
          |> List.map ~f:(fun callable ->
                 `String (Interprocedural.Target.external_target_name callable))
        in
        let location_json = location_with_module_to_json ~filename_lookup location in
        let port_json = AccessPath.create port path |> AccessPath.to_json in
        let call_json =
          `Assoc
            [
              "position", location_json;
              "resolves_to", `List callee_json;
              "port", port_json;
              "length", `Int trace_length;
            ]
        in
        "call", call_json


  (* Returns the (dictionary key * json) to emit *)
  let to_json = create_json ~filename_lookup:None

  let to_external_json ~filename_lookup = create_json ~filename_lookup:(Some filename_lookup)

  let less_or_equal ~left ~right =
    match left, right with
    | ( CallSite
          { path = path_left; location = location_left; port = port_left; callees = callees_left },
        CallSite
          {
            path = path_right;
            location = location_right;
            port = port_right;
            callees = callees_right;
          } ) ->
        AccessPath.Root.equal port_left port_right
        && Location.WithModule.compare location_left location_right = 0
        && [%compare.equal: Interprocedural.Target.t list] callees_left callees_right
        && Abstract.TreeDomain.Label.compare_path path_right path_left = 0
    | _ -> [%compare.equal: t] left right


  let widen set = set

  let strip_for_callsite = function
    | Origin _ -> Origin Location.WithModule.any
    | CallSite { port; path; location = _; callees } ->
        CallSite { port; path; location = Location.WithModule.any; callees }
    | Declaration _ -> Declaration { leaf_name_provided = false }
end

module TraceLength = Abstract.SimpleDomain.Make (struct
  type t = int

  let name = "trace length"

  let join = min

  let meet = max

  let less_or_equal ~left ~right = left >= right

  let bottom = max_int

  let show length = if Int.equal length max_int then "<bottom>" else string_of_int length
end)

module FlowDetails = struct
  module Slots = struct
    let name = "flow details"

    type 'a slot =
      | Breadcrumb : Features.BreadcrumbSet.t slot
      | ViaFeature : Features.ViaFeatureSet.t slot
      | ReturnAccessPath : Features.ReturnAccessPathSet.t slot
      | TraceLength : TraceLength.t slot
      | TitoPosition : Features.TitoPositionSet.t slot
      | LeafName : Features.LeafNameSet.t slot
      | FirstIndex : Features.FirstIndexSet.t slot
      | FirstField : Features.FirstFieldSet.t slot

    (* Must be consistent with above variants *)
    let slots = 8

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Breadcrumb -> "Breadcrumb"
      | ViaFeature -> "ViaFeature"
      | ReturnAccessPath -> "ReturnAccessPath"
      | TraceLength -> "TraceLength"
      | TitoPosition -> "TitoPosition"
      | LeafName -> "LeafName"
      | FirstIndex -> "FirstIndex"
      | FirstField -> "FirstField"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Breadcrumb -> (module Features.BreadcrumbSet : Abstract.Domain.S with type t = a)
      | ViaFeature -> (module Features.ViaFeatureSet : Abstract.Domain.S with type t = a)
      | ReturnAccessPath ->
          (module Features.ReturnAccessPathSet : Abstract.Domain.S with type t = a)
      | TraceLength -> (module TraceLength : Abstract.Domain.S with type t = a)
      | TitoPosition -> (module Features.TitoPositionSet : Abstract.Domain.S with type t = a)
      | LeafName -> (module Features.LeafNameSet : Abstract.Domain.S with type t = a)
      | FirstIndex -> (module Features.FirstIndexSet : Abstract.Domain.S with type t = a)
      | FirstField -> (module Features.FirstFieldSet : Abstract.Domain.S with type t = a)


    let strict _ = false
  end

  include Abstract.ProductDomain.Make (Slots)

  let initial =
    create
      [Part (Features.BreadcrumbSet.Self, Features.BreadcrumbSet.empty); Part (TraceLength.Self, 0)]


  let strip_tito_positions =
    transform Features.TitoPositionSet.Self Map ~f:(fun _ -> Features.TitoPositionSet.bottom)


  let add_tito_position position = transform Features.TitoPositionSet.Element Add ~f:position

  let add_breadcrumb breadcrumb = transform Features.BreadcrumbSet.Element Add ~f:breadcrumb

  let add_breadcrumbs breadcrumbs = transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs

  let product_pp = pp (* shadow *)

  let pp formatter = Format.fprintf formatter "FlowDetails(%a)" product_pp

  let show = Format.asprintf "%a" pp

  let subtract to_remove ~from =
    (* Do not partially subtract slots, since this is unsound. *)
    if to_remove == from then
      bottom
    else if is_bottom to_remove then
      from
    else if less_or_equal ~left:from ~right:to_remove then
      bottom
    else
      from
end

module type TAINT_DOMAIN = sig
  include Abstract.Domain.S

  type kind [@@deriving eq]

  val kind : kind Abstract.Domain.part

  val ignore_kind_at_call : kind -> bool

  val trace_info : TraceInfo.t Abstract.Domain.part

  val add_breadcrumb : Features.Breadcrumb.t -> t -> t

  val add_breadcrumbs : Features.BreadcrumbSet.t -> t -> t

  val breadcrumbs : t -> Features.BreadcrumbSet.t

  val via_features : t -> Features.ViaFeatureSet.t

  val transform_on_widening_collapse : t -> t

  val prune_maximum_length : TraceLength.t -> t -> t

  (* Add trace info at call-site *)
  val apply_call
    :  Location.WithModule.t ->
    callees:Interprocedural.Target.t list ->
    port:AccessPath.Root.t ->
    path:Abstract.TreeDomain.Label.path ->
    element:t ->
    t

  val to_json : t -> Yojson.Safe.json

  val to_external_json : filename_lookup:(Reference.t -> string option) -> t -> Yojson.Safe.json
end

module type KIND_ARG = sig
  include Abstract.SetDomain.ELEMENT

  val equal : t -> t -> bool

  val show : t -> string

  val ignore_kind_at_call : t -> bool
end

module KindTaint (Kind : KIND_ARG) = struct
  module Key = struct
    include Kind

    let absence_implicitly_maps_to_bottom = false
  end

  module Map = Abstract.MapDomain.Make (Key) (FlowDetails)
  include Map

  let singleton kind = Map.set Map.bottom ~key:kind ~data:FlowDetails.initial
end

module MakeTaint (Kind : KIND_ARG) : sig
  include TAINT_DOMAIN with type kind = Kind.t

  val kinds : t -> kind list

  val singleton : ?location:Location.WithModule.t -> kind -> t

  val of_list : ?location:Location.WithModule.t -> kind list -> t
end = struct
  module Key = struct
    include TraceInfo

    let absence_implicitly_maps_to_bottom = true
  end

  module KindTaintDomain = KindTaint (Kind)
  module Map = Abstract.MapDomain.Make (Key) (KindTaintDomain)
  include Map

  type kind = Kind.t [@@deriving compare]

  let equal_kind = Kind.equal

  let add ?location map kind =
    let trace =
      match location with
      | None -> TraceInfo.Declaration { leaf_name_provided = false }
      | Some location -> TraceInfo.Origin location
    in
    let kind_taint = KindTaintDomain.singleton kind in
    Map.update map trace ~f:(function
        | None -> kind_taint
        | Some existing -> KindTaintDomain.join kind_taint existing)


  let singleton ?location kind = add ?location Map.bottom kind

  let of_list ?location kinds = List.fold kinds ~init:Map.bottom ~f:(add ?location)

  let kind = KindTaintDomain.Key

  let ignore_kind_at_call = Kind.ignore_kind_at_call

  let trace_info = Map.Key

  let kinds map =
    Map.fold kind ~init:[] ~f:List.cons map |> List.dedup_and_sort ~compare:Kind.compare


  let create_json ~trace_info_to_json taint =
    let leaf_to_json trace_info (kind, features) =
      let trace_length = FlowDetails.fold TraceLength.Self features ~f:min ~init:55555 in
      let kind_json = `String (Kind.show kind) in
      let breadcrumbs, leaf_json =
        let gather_json { Abstract.OverUnderSetDomain.element; in_under } breadcrumbs =
          let breadcrumb_json = Features.Breadcrumb.to_json element ~on_all_paths:in_under in
          breadcrumb_json :: breadcrumbs
        in
        let gather_return_access_path path leaves =
          let path_name = Abstract.TreeDomain.Label.show_path path in
          `Assoc ["kind", kind_json; "name", `String path_name; "depth", `Int trace_length]
          :: leaves
        in
        let breadcrumbs =
          FlowDetails.fold Features.BreadcrumbSet.ElementAndUnder ~f:gather_json ~init:[] features
        in
        let leaves =
          FlowDetails.get FlowDetails.Slots.LeafName features
          |> Features.LeafNameSet.elements
          |> List.map ~f:(Features.LeafName.to_json ~kind_json)
        in
        let first_index_breadcrumbs =
          FlowDetails.get FlowDetails.Slots.FirstIndex features
          |> Features.FirstIndexSet.elements
          |> Features.FirstIndex.to_json
        in
        let first_field_breadcrumbs =
          FlowDetails.get FlowDetails.Slots.FirstField features
          |> Features.FirstFieldSet.elements
          |> Features.FirstField.to_json
        in
        ( List.concat [first_index_breadcrumbs; first_field_breadcrumbs; breadcrumbs],
          FlowDetails.fold
            Features.ReturnAccessPathSet.Element
            ~f:gather_return_access_path
            ~init:leaves
            features )
      in
      let tito_positions =
        FlowDetails.get FlowDetails.Slots.TitoPosition features
        |> Features.TitoPositionSet.elements
        |> List.map ~f:location_to_json
      in
      let trace_json = trace_info_to_json ~trace_length trace_info in
      let leaf_json =
        if List.is_empty leaf_json then
          [`Assoc ["kind", kind_json]]
        else
          leaf_json
      in
      let association =
        let cons_if_non_empty key list assoc =
          if List.is_empty list then
            assoc
          else
            (key, `List list) :: assoc
        in
        []
        |> cons_if_non_empty "features" breadcrumbs
        |> cons_if_non_empty "leaves" leaf_json
        |> cons_if_non_empty "tito" tito_positions
      in
      `Assoc (trace_json :: association)
    in
    let trace_to_json (trace_info, kind_taint) =
      KindTaintDomain.Map.to_alist kind_taint |> List.map ~f:(leaf_to_json trace_info)
    in
    (* expand now do dedup possibly abstract targets that resolve to the same concrete ones *)
    let taint = Map.transform Key Map ~f:TraceInfo.expand_call_site taint in
    let elements = Map.to_alist taint |> List.concat_map ~f:trace_to_json in
    `List elements


  let to_json = create_json ~trace_info_to_json:TraceInfo.to_json

  let to_external_json ~filename_lookup =
    create_json ~trace_info_to_json:(TraceInfo.to_external_json ~filename_lookup)


  let add_breadcrumb breadcrumb = transform Features.BreadcrumbSet.Element Add ~f:breadcrumb

  let add_breadcrumbs breadcrumbs taint =
    if Features.BreadcrumbSet.is_bottom breadcrumbs || Features.BreadcrumbSet.is_empty breadcrumbs
    then
      taint
    else
      transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs taint


  let breadcrumbs taint =
    let gather_breadcrumbs to_add breadcrumbs =
      Features.BreadcrumbSet.add_set breadcrumbs ~to_add
    in
    fold Features.BreadcrumbSet.Self ~f:gather_breadcrumbs ~init:Features.BreadcrumbSet.bottom taint


  let via_features taint =
    fold
      Features.ViaFeatureSet.Self
      ~f:Features.ViaFeatureSet.join
      ~init:Features.ViaFeatureSet.bottom
      taint


  let transform_on_widening_collapse =
    (* using an always-feature here would break the widening invariant: a <= a widen b *)
    let open Features in
    let broadening =
      BreadcrumbSet.of_approximation
        [
          { element = Breadcrumb.Broadening; in_under = false };
          { element = Breadcrumb.IssueBroadening; in_under = false };
        ]
    in
    add_breadcrumbs broadening


  let prune_maximum_length maximum_length =
    let filter_flow (_, flow_details) =
      let length = FlowDetails.get FlowDetails.Slots.TraceLength flow_details in
      TraceLength.is_bottom length || TraceLength.less_or_equal ~left:maximum_length ~right:length
    in
    transform KindTaintDomain.KeyValue Filter ~f:filter_flow


  let apply_call location ~callees ~port ~path ~element:taint =
    let apply (trace_info, kind_taint) =
      let open TraceInfo in
      let apply_flow_details flow_details =
        flow_details
        |> FlowDetails.transform Features.TitoPositionSet.Self Map ~f:(fun _ ->
               Features.TitoPositionSet.bottom)
        |> FlowDetails.transform Features.ViaFeatureSet.Self Map ~f:(fun _ ->
               Features.ViaFeatureSet.bottom)
      in
      let kind_taint =
        KindTaintDomain.transform FlowDetails.Self Map ~f:apply_flow_details kind_taint
      in
      match trace_info with
      | Origin _
      | CallSite _ ->
          let increase_length n = if n < max_int then n + 1 else n in
          let trace_info = CallSite { location; callees; port; path } in
          let kind_taint =
            kind_taint |> KindTaintDomain.transform TraceLength.Self Map ~f:increase_length
          in
          trace_info, kind_taint
      | Declaration { leaf_name_provided } ->
          let trace_info = Origin location in
          let new_leaf_names =
            if leaf_name_provided then
              Features.LeafNameSet.bottom
            else
              let open Features in
              let make_leaf_name callee =
                LeafName.{ leaf = Interprocedural.Target.external_target_name callee; port = None }
              in
              List.map ~f:make_leaf_name callees |> Features.LeafNameSet.of_list
          in
          let kind_taint =
            KindTaintDomain.transform Features.LeafNameSet.Self Add ~f:new_leaf_names kind_taint
          in
          trace_info, kind_taint
    in
    Map.transform Map.KeyValue Map ~f:apply taint
end

module ForwardTaint = MakeTaint (Sources)
module BackwardTaint = MakeTaint (Sinks)

module MakeTaintTree (Taint : TAINT_DOMAIN) () = struct
  include
    Abstract.TreeDomain.Make
      (struct
        let max_tree_depth_after_widening () = TaintConfiguration.maximum_tree_depth_after_widening

        let check_invariants = true
      end)
      (Taint)
      ()

  let apply_call location ~callees ~port taint_tree =
    let transform_path (path, _) tip =
      let tip =
        tip
        |> Taint.transform Taint.kind Filter ~f:(fun kind -> not (Taint.ignore_kind_at_call kind))
        |> fun taint -> Taint.apply_call location ~callees ~port ~path ~element:taint
      in
      tip
    in
    transform Taint.Self (Context (Path, Map)) ~f:transform_path taint_tree


  let empty = bottom

  let is_empty = is_bottom

  let compute_essential_features ~essential_return_access_paths tree =
    let essential_trace_info = function
      | _ -> TraceInfo.Declaration { leaf_name_provided = false }
    in
    let essential_breadcrumbs _ = Features.BreadcrumbSet.bottom in
    let essential_via_features _ = Features.ViaFeatureSet.bottom in
    let essential_tito_positions _ = Features.TitoPositionSet.bottom in
    let essential_leaf_names _ = Features.LeafNameSet.bottom in
    transform Taint.trace_info Map ~f:essential_trace_info tree
    |> transform Features.ReturnAccessPathSet.Self Map ~f:essential_return_access_paths
    |> transform Features.BreadcrumbSet.Self Map ~f:essential_breadcrumbs
    |> transform Features.ViaFeatureSet.Self Map ~f:essential_via_features
    |> transform Features.TitoPositionSet.Self Map ~f:essential_tito_positions
    |> transform Features.LeafNameSet.Self Map ~f:essential_leaf_names


  (* Keep only non-essential structure. *)
  let essential tree =
    let essential_return_access_paths _ = Features.ReturnAccessPathSet.bottom in
    compute_essential_features ~essential_return_access_paths tree


  let essential_for_constructor tree =
    let essential_return_access_paths set = set in
    compute_essential_features ~essential_return_access_paths tree


  let approximate_return_access_paths ~maximum_return_access_path_length tree =
    let cut_off paths =
      if Features.ReturnAccessPathSet.count paths > maximum_return_access_path_length then
        Features.ReturnAccessPathSet.elements paths
        |> Features.ReturnAccessPath.common_prefix
        |> Features.ReturnAccessPathSet.singleton
      else
        paths
    in
    transform Features.ReturnAccessPathSet.Self Map ~f:cut_off tree


  let prune_maximum_length maximum_length =
    transform Taint.Self Map ~f:(Taint.prune_maximum_length maximum_length)


  let filter_by_kind ~kind taint_tree =
    taint_tree
    |> transform Taint.kind Filter ~f:(Taint.equal_kind kind)
    |> collapse ~transform:Fn.id


  let breadcrumbs taint_tree =
    let gather_breadcrumbs to_add breadcrumbs =
      Features.BreadcrumbSet.add_set breadcrumbs ~to_add
    in
    fold
      Features.BreadcrumbSet.Self
      ~f:gather_breadcrumbs
      ~init:Features.BreadcrumbSet.bottom
      taint_tree
end

module MakeTaintEnvironment (Taint : TAINT_DOMAIN) () = struct
  module Tree = struct
    include MakeTaintTree (Taint) ()

    let add_breadcrumb breadcrumb = transform Features.BreadcrumbSet.Element Add ~f:breadcrumb

    let add_breadcrumbs breadcrumbs taint_tree =
      if Features.BreadcrumbSet.is_bottom breadcrumbs || Features.BreadcrumbSet.is_empty breadcrumbs
      then
        taint_tree
      else
        transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs taint_tree


    let breadcrumbs taint_tree =
      let gather_breadcrumbs to_add breadcrumbs =
        Features.BreadcrumbSet.add_set breadcrumbs ~to_add
      in
      fold
        Features.BreadcrumbSet.Self
        ~f:gather_breadcrumbs
        ~init:Features.BreadcrumbSet.bottom
        taint_tree


    let add_via_features via_features taint_tree =
      if Features.ViaFeatureSet.is_bottom via_features then
        taint_tree
      else
        transform Features.ViaFeatureSet.Self Add ~f:via_features taint_tree
  end

  include
    Abstract.MapDomain.Make
      (struct
        let name = "env"

        include AccessPath.Root

        let absence_implicitly_maps_to_bottom = true
      end)
      (Tree)

  let create_json ~taint_to_json environment =
    let element_to_json json_list (root, tree) =
      let path_to_json (path, tip) json_list =
        let port = AccessPath.create root path |> AccessPath.to_json in
        (path, ["port", port; "taint", taint_to_json tip]) :: json_list
      in
      let ports =
        Tree.fold Tree.Path ~f:path_to_json tree ~init:[]
        |> List.dedup_and_sort ~compare:(fun (p1, _) (p2, _) ->
               Abstract.TreeDomain.Label.compare_path p1 p2)
        |> List.rev_map ~f:(fun (_, fields) -> `Assoc fields)
      in
      List.rev_append ports json_list
    in
    let paths = to_alist environment |> List.fold ~f:element_to_json ~init:[] in
    `List paths


  let to_json = create_json ~taint_to_json:Taint.to_json

  let to_external_json ~filename_lookup =
    create_json ~taint_to_json:(Taint.to_external_json ~filename_lookup)


  let assign ?(weak = false) ~root ~path subtree environment =
    let assign_tree = function
      | None -> Tree.assign ~weak ~tree:Tree.bottom path ~subtree
      | Some tree -> Tree.assign ~weak ~tree path ~subtree
    in
    update environment root ~f:assign_tree


  let read_tree_raw
      ?(transform_non_leaves = fun _ e -> e)
      ?(use_precise_labels = false)
      ~root
      ~path
      environment
    =
    match get_opt root environment with
    | None -> Taint.bottom, Tree.bottom
    | Some tree -> Tree.read_raw ~transform_non_leaves ~use_precise_labels path tree


  let read ?(transform_non_leaves = fun _ e -> e) ~root ~path environment =
    match get_opt root environment with
    | None -> Tree.bottom
    | Some tree -> Tree.read ~transform_non_leaves path tree


  let empty = bottom

  let is_empty = is_bottom

  let roots environment = fold Key ~f:List.cons ~init:[] environment

  let add_breadcrumb breadcrumb = transform Features.BreadcrumbSet.Element Add ~f:breadcrumb

  let add_breadcrumbs breadcrumbs taint_tree =
    if Features.BreadcrumbSet.is_bottom breadcrumbs || Features.BreadcrumbSet.is_empty breadcrumbs
    then
      taint_tree
    else
      transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs taint_tree


  let add_via_features via_features taint_tree =
    if Features.ViaFeatureSet.is_bottom via_features then
      taint_tree
    else
      transform Features.ViaFeatureSet.Self Add ~f:via_features taint_tree


  let extract_features_to_attach ~root ~attach_to_kind taint =
    let taint =
      read ~root ~path:[] taint
      |> Tree.transform Taint.kind Filter ~f:(Taint.equal_kind attach_to_kind)
      |> Tree.collapse ~transform:Fn.id
    in
    Taint.breadcrumbs taint, Taint.via_features taint
end

module ForwardState = MakeTaintEnvironment (ForwardTaint) ()
(** Used to infer which sources reach the exit points of a function. *)

module BackwardState = MakeTaintEnvironment (BackwardTaint) ()
(** Used to infer which sinks are reached from parameters, as well as the taint-in-taint-out (TITO)
    using the special LocalReturn sink. *)

(* Special sink as it needs the return access path *)
let local_return_taint =
  BackwardTaint.create
    [
      Part (BackwardTaint.trace_info, TraceInfo.Declaration { leaf_name_provided = false });
      Part (BackwardTaint.kind, Sinks.LocalReturn);
      Part (TraceLength.Self, 0);
      Part (Features.ReturnAccessPathSet.Element, []);
      Part (Features.BreadcrumbSet.Self, Features.BreadcrumbSet.empty);
    ]


module Sanitize = struct
  type sanitize_sources =
    | AllSources
    | SpecificSources of Sources.Set.t
  [@@deriving show, eq]

  type sanitize_sinks =
    | AllSinks
    | SpecificSinks of Sinks.Set.t
  [@@deriving show, eq]

  type sanitize_tito =
    | AllTito
    | SpecificTito of {
        sanitized_tito_sources: Sources.Set.t;
        sanitized_tito_sinks: Sinks.Set.t;
      }
  [@@deriving show, eq]

  type sanitize = {
    sources: sanitize_sources option;
    sinks: sanitize_sinks option;
    tito: sanitize_tito option;
  }
  [@@deriving show, eq]

  include Abstract.SimpleDomain.Make (struct
    type t = sanitize

    let name = "sanitize"

    let bottom = { sources = None; sinks = None; tito = None }

    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        (match left.sources, right.sources with
        | None, _ -> true
        | Some _, None -> false
        | Some AllSources, Some AllSources -> true
        | Some AllSources, Some (SpecificSources _) -> false
        | Some (SpecificSources _), Some AllSources -> true
        | Some (SpecificSources left), Some (SpecificSources right) -> Sources.Set.subset left right)
        && (match left.sinks, right.sinks with
           | None, _ -> true
           | Some _, None -> false
           | Some AllSinks, Some AllSinks -> true
           | Some AllSinks, Some (SpecificSinks _) -> false
           | Some (SpecificSinks _), Some AllSinks -> true
           | Some (SpecificSinks left), Some (SpecificSinks right) -> Sinks.Set.subset left right)
        &&
        match left.tito, right.tito with
        | None, _ -> true
        | Some _, None -> false
        | Some AllTito, Some AllTito -> true
        | Some AllTito, Some (SpecificTito _) -> false
        | Some (SpecificTito _), Some AllTito -> true
        | Some (SpecificTito left), Some (SpecificTito right) ->
            Sources.Set.subset left.sanitized_tito_sources right.sanitized_tito_sources
            && Sinks.Set.subset left.sanitized_tito_sinks right.sanitized_tito_sinks


    let join left right =
      if phys_equal left right then
        left
      else
        let sources =
          match left.sources, right.sources with
          | None, Some _ -> right.sources
          | Some _, None -> left.sources
          | Some AllSources, _
          | _, Some AllSources ->
              Some AllSources
          | Some (SpecificSources left_sources), Some (SpecificSources right_sources) ->
              Some (SpecificSources (Sources.Set.union left_sources right_sources))
          | None, None -> None
        in
        let sinks =
          match left.sinks, right.sinks with
          | None, Some _ -> right.sinks
          | Some _, None -> left.sinks
          | Some AllSinks, _
          | _, Some AllSinks ->
              Some AllSinks
          | Some (SpecificSinks left_sinks), Some (SpecificSinks right_sinks) ->
              Some (SpecificSinks (Sinks.Set.union left_sinks right_sinks))
          | None, None -> None
        in
        let tito =
          match left.tito, right.tito with
          | None, Some tito
          | Some tito, None ->
              Some tito
          | Some AllTito, _
          | _, Some AllTito ->
              Some AllTito
          | Some (SpecificTito left), Some (SpecificTito right) ->
              Some
                (SpecificTito
                   {
                     sanitized_tito_sources =
                       Sources.Set.union left.sanitized_tito_sources right.sanitized_tito_sources;
                     sanitized_tito_sinks =
                       Sinks.Set.union left.sanitized_tito_sinks right.sanitized_tito_sinks;
                   })
          | None, None -> None
        in
        { sources; sinks; tito }


    let meet a b = if less_or_equal ~left:b ~right:a then b else a

    let show = show_sanitize
  end)

  let empty = bottom

  let is_empty = is_bottom

  let equal = equal_sanitize

  let to_json { sources; sinks; tito } =
    let to_string name = `String name in
    let sources_to_json sources =
      `List (sources |> Sources.Set.elements |> List.map ~f:Sources.show |> List.map ~f:to_string)
    in
    let sinks_to_json sinks =
      `List (sinks |> Sinks.Set.elements |> List.map ~f:Sinks.show |> List.map ~f:to_string)
    in
    let sources_json =
      match sources with
      | Some AllSources -> ["sources", `String "All"]
      | Some (SpecificSources sources) -> ["sources", sources_to_json sources]
      | None -> []
    in
    let sinks_json =
      match sinks with
      | Some AllSinks -> ["sinks", `String "All"]
      | Some (SpecificSinks sinks) -> ["sinks", sinks_to_json sinks]
      | None -> []
    in
    let tito_json =
      match tito with
      | Some AllTito -> ["tito", `String "All"]
      | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
          [
            "tito_sources", sources_to_json sanitized_tito_sources;
            "tito_sinks", sinks_to_json sanitized_tito_sinks;
          ]
      | None -> []
    in
    `Assoc (sources_json @ sinks_json @ tito_json)
end

(** Map from parameters or return value to a sanitizer. *)
module SanitizeRootMap = struct
  include
    Abstract.MapDomain.Make
      (struct
        let name = "sanitize"

        include AccessPath.Root

        let absence_implicitly_maps_to_bottom = true
      end)
      (Sanitize)

  let roots map = fold Key ~f:List.cons ~init:[] map

  let to_json map =
    map
    |> to_alist
    |> List.map ~f:(fun (root, sanitize) ->
           let (`Assoc fields) = Sanitize.to_json sanitize in
           let port = AccessPath.create root [] |> AccessPath.to_json in
           `Assoc (("port", port) :: fields))
    |> fun elements -> `List elements
end
