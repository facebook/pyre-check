(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module type TAINT_SET = sig
  include Abstract.Domain.S

  type element [@@deriving compare]

  val element : element Abstract.Domain.part

  val add : element -> t -> t

  val of_list : element list -> t

  val to_json : t -> Yojson.Safe.json list

  val singleton : element -> t
end

module type SET_ARG = sig
  include Abstract.SetDomain.ELEMENT

  val equal : t -> t -> bool

  val show : t -> string

  val ignore_leaf_at_call : t -> bool
end

module Set (Element : SET_ARG) : TAINT_SET with type element = Element.t = struct
  module Set = Abstract.SetDomain.Make (Element)
  include Set

  let element = Set.Element

  type element = Element.t [@@deriving compare]

  let show set =
    elements set |> List.map ~f:Element.show |> String.concat ~sep:", " |> Format.sprintf "{%s}"


  let to_json set =
    let element_to_json element =
      let kind = `String (Element.show element) in
      `Assoc ["kind", kind]
    in
    elements set |> List.map ~f:element_to_json
end

let location_to_json
    ?filename_lookup
    ( { Location.WithModule.start = { line; column }; stop = { column = end_column; _ }; _ } as
    location_with_module )
    : Yojson.Safe.json
  =
  let optionally_add_filename fields =
    match filename_lookup with
    | Some lookup ->
        let { Location.WithPath.path; _ } =
          Location.WithModule.instantiate ~lookup location_with_module
        in
        ("filename", `String path) :: fields
    | None -> fields
  in
  let fields =
    (* Note: not correct for multiple line span *)
    ["line", `Int line; "start", `Int column; "end", `Int end_column] |> optionally_add_filename
  in
  `Assoc fields


module TraceInfo = struct
  let name = "trace"

  type t =
    | Declaration
    | Origin of Location.WithModule.t
    | CallSite of {
        port: AccessPath.Root.t;
        path: Abstract.TreeDomain.Label.path;
        location: Location.WithModule.t;
        callees: Interprocedural.Callable.t list;
      }
  [@@deriving compare, show]

  let _ = show (* shadowed below *)

  let show = function
    | Declaration -> "declaration"
    | Origin location -> Format.asprintf "@%a" Location.WithModule.pp location
    | CallSite { location; callees; port; path } ->
        let port = AccessPath.create port path |> AccessPath.show in
        Format.asprintf
          "via call@%a[%s][%s]"
          Location.WithModule.pp
          location
          (String.concat
             ~sep:" "
             (List.map ~f:Interprocedural.Callable.external_target_name callees))
          port


  (* Breaks recursion among trace info and overall taint domain. *)
  let has_significant_summary =
    ref
      (fun (_ : AccessPath.Root.t)
           (_ : Abstract.TreeDomain.Label.path)
           (_ : Interprocedural.Callable.non_override_target)
           -> true)


  (* Only called when emitting models before we compute the json so we can dedup *)
  let expand_call_site trace =
    match trace with
    | CallSite { location; callees; port; path } ->
        let callees =
          Interprocedural.DependencyGraph.expand_callees callees
          |> List.filter ~f:(!has_significant_summary port path)
        in
        CallSite { location; callees = (callees :> Interprocedural.Callable.t list); port; path }
    | _ -> trace


  let create_json ~location_to_json ~trace_length trace : string * Yojson.Safe.json =
    match trace with
    | Declaration -> "decl", `Null
    | Origin location ->
        let location_json = location_to_json location in
        "root", location_json
    | CallSite { location; callees; port; path } ->
        let callee_json =
          callees
          |> List.map ~f:(fun callable ->
                 `String (Interprocedural.Callable.external_target_name callable))
        in
        let location_json = location_to_json location in
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
  let to_json = create_json ~location_to_json

  let to_external_json ~filename_lookup =
    create_json ~location_to_json:(location_to_json ~filename_lookup)


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
        port_left = port_right
        && Location.WithModule.compare location_left location_right = 0
        && callees_left = callees_right
        && Abstract.TreeDomain.Label.compare_path path_right path_left = 0
    | _ -> left = right


  let widen set = set

  let strip_for_callsite = function
    | Origin _ -> Origin Location.WithModule.any
    | CallSite { port; path; location = _; callees } ->
        CallSite { port; path; location = Location.WithModule.any; callees }
    | Declaration -> Declaration
end

module TraceLength = Abstract.SimpleDomain.Make (struct
  type t = int [@@deriving show]

  let name = "trace length"

  let join = min

  let less_or_equal ~left ~right = left >= right

  let bottom = max_int
end)

module FlowDetails = struct
  module Slots = struct
    let name = "flow details"

    type 'a slot =
      | SimpleFeature : Features.SimpleSet.t slot
      | ComplexFeature : Features.ComplexSet.t slot
      | TraceLength : TraceLength.t slot

    (* Must be consistent with above variants *)
    let slots = 3

    let slot_name (type a) (slot : a slot) =
      match slot with
      | SimpleFeature -> "SimpleFeature"
      | ComplexFeature -> "ComplexFeature"
      | TraceLength -> "TraceLength"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | SimpleFeature -> (module Features.SimpleSet : Abstract.Domain.S with type t = a)
      | ComplexFeature -> (module Features.ComplexSet : Abstract.Domain.S with type t = a)
      | TraceLength -> (module TraceLength : Abstract.Domain.S with type t = a)


    let strict _ = false
  end

  include Abstract.ProductDomain.Make (Slots)

  let initial =
    create [Part (Features.SimpleSet.Self, Features.SimpleSet.empty); Part (TraceLength.Self, 0)]


  let simple_feature = Features.SimpleSet.Element

  let simple_feature_element = Features.SimpleSet.ElementAndUnder

  let simple_feature_set = Features.SimpleSet.SetAndUnder

  let complex_feature = Features.ComplexSet.Element

  let complex_feature_set = Features.ComplexSet.Set
end

module type TAINT_DOMAIN = sig
  include Abstract.Domain.S

  type leaf [@@deriving eq]

  val leaf : leaf Abstract.Domain.part

  val ignore_leaf_at_call : leaf -> bool

  val trace_info : TraceInfo.t Abstract.Domain.part

  val simple_feature : Features.Simple.t Abstract.Domain.part

  val simple_feature_element
    : Features.Simple.t Abstract.OverUnderSetDomain.approximation Abstract.Domain.part

  val simple_feature_set
    : Features.Simple.t Abstract.OverUnderSetDomain.approximation list Abstract.Domain.part

  val complex_feature : Features.Complex.t Abstract.Domain.part

  val complex_feature_set : Features.Complex.t list Abstract.Domain.part

  (* Add trace info at call-site *)
  val apply_call
    :  Location.WithModule.t ->
    callees:Interprocedural.Callable.t list ->
    port:AccessPath.Root.t ->
    path:Abstract.TreeDomain.Label.path ->
    element:t ->
    t

  val to_json : t -> Yojson.Safe.json

  val to_external_json : filename_lookup:(Reference.t -> string option) -> t -> Yojson.Safe.json
end

module LeafTaint (Leaf : SET_ARG) = struct
  module Key = struct
    include Leaf

    let absence_implicitly_maps_to_bottom = false
  end

  module Map = Abstract.MapDomain.Make (Key) (FlowDetails)
  include Map

  let singleton leaf = Map.set Map.bottom ~key:leaf ~data:FlowDetails.initial
end

module MakeTaint (Leaf : SET_ARG) : sig
  include TAINT_DOMAIN with type leaf = Leaf.t

  val leaves : t -> leaf list

  val singleton : ?location:Location.WithModule.t -> leaf -> t

  val of_list : ?location:Location.WithModule.t -> leaf list -> t
end = struct
  module Key = struct
    include TraceInfo

    let absence_implicitly_maps_to_bottom = true
  end

  module LeafDomain = LeafTaint (Leaf)
  module Map = Abstract.MapDomain.Make (Key) (LeafDomain)
  include Map

  type leaf = Leaf.t [@@deriving compare]

  let equal_leaf = Leaf.equal

  let add ?location map leaf =
    let trace =
      match location with
      | None -> TraceInfo.Declaration
      | Some location -> TraceInfo.Origin location
    in
    let leaf_taint = LeafDomain.singleton leaf in
    Map.update map trace ~f:(function
        | None -> leaf_taint
        | Some existing -> LeafDomain.join leaf_taint existing)


  let singleton ?location leaf = add ?location Map.bottom leaf

  let of_list ?location leaves = List.fold leaves ~init:Map.bottom ~f:(add ?location)

  let leaf = LeafDomain.Key

  let ignore_leaf_at_call = Leaf.ignore_leaf_at_call

  let trace_info = Map.Key

  let simple_feature = FlowDetails.simple_feature

  let simple_feature_element = FlowDetails.simple_feature_element

  let complex_feature = FlowDetails.complex_feature

  let simple_feature_set = FlowDetails.simple_feature_set

  let complex_feature_set = FlowDetails.complex_feature_set

  let leaves map =
    Map.fold leaf ~init:[] ~f:List.cons map |> List.dedup_and_sort ~compare:Leaf.compare


  let create_json ~trace_info_to_json taint =
    let leaf_to_json trace_info (leaf, features) =
      let trace_length = FlowDetails.fold TraceLength.Self features ~f:min ~init:55555 in
      let leaf_kind_json = `String (Leaf.show leaf) in
      let breadcrumbs, tito_positions, leaf_json =
        let gather_json { Abstract.OverUnderSetDomain.element; in_under } (breadcrumbs, tito, leaves)
          =
          match element with
          | Features.Simple.LeafName name ->
              ( breadcrumbs,
                tito,
                `Assoc
                  ["kind", leaf_kind_json; "name", `String name; "on_all_flows", `Bool in_under]
                :: leaves )
          | TitoPosition location ->
              let tito_location_json = location_to_json location in
              breadcrumbs, tito_location_json :: tito, leaves
          | ViaValueOf _ ->
              (* The taint analysis creates breadcrumbs for ViaValueOf features dynamically.*)
              breadcrumbs, tito, leaves
          | Breadcrumb breadcrumb ->
              let breadcrumb_json = Features.Breadcrumb.to_json breadcrumb ~on_all_paths:in_under in
              breadcrumb_json :: breadcrumbs, tito, leaves
        in
        let gather_return_access_path feature leaves =
          match feature with
          | Features.Complex.ReturnAccessPath path ->
              let path_name = Abstract.TreeDomain.Label.show_path path in
              `Assoc ["kind", leaf_kind_json; "name", `String path_name] :: leaves
        in
        let breadcrumbs, tito_positions, leaves =
          FlowDetails.(fold simple_feature_element ~f:gather_json ~init:([], [], []) features)
        in
        ( breadcrumbs,
          tito_positions,
          FlowDetails.(fold complex_feature ~f:gather_return_access_path ~init:leaves features) )
      in
      let trace_json = trace_info_to_json ~trace_length trace_info in
      let leaf_json =
        if leaf_json = [] then
          [`Assoc ["kind", leaf_kind_json]]
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
    let trace_to_json (traceinfo, leaftaint) =
      LeafDomain.Map.to_alist leaftaint |> List.map ~f:(leaf_to_json traceinfo)
    in
    (* expand now do dedup possibly abstract targets that resolve to the same concrete ones *)
    let taint = Map.transform Key (Abstract.Domain.Map TraceInfo.expand_call_site) taint in
    let elements = Map.to_alist taint |> List.concat_map ~f:trace_to_json in
    `List elements


  let to_json = create_json ~trace_info_to_json:TraceInfo.to_json

  let to_external_json ~filename_lookup =
    create_json ~trace_info_to_json:(TraceInfo.to_external_json ~filename_lookup)


  let apply_call location ~callees ~port ~path ~element:taint =
    let apply (trace_info, leaf_taint) =
      let open TraceInfo in
      let strip_tito_positions features =
        List.filter
          ~f:(fun { Abstract.OverUnderSetDomain.element; _ } ->
            match element with
            | Features.Simple.TitoPosition _ -> false
            | _ -> true)
          features
      in
      let leaf_taint =
        LeafDomain.transform
          FlowDetails.simple_feature_set
          (Abstract.Domain.Map strip_tito_positions)
          leaf_taint
      in
      match trace_info with
      | Origin _
      | CallSite _ ->
          let increase_length n = if n < max_int then n + 1 else n in
          let trace_info = CallSite { location; callees; port; path } in
          let leaf_taint =
            leaf_taint
            |> LeafDomain.transform TraceLength.Self (Abstract.Domain.Map increase_length)
          in
          trace_info, leaf_taint
      | Declaration ->
          let trace_info = Origin location in
          let add_leaf_names info_set =
            let open Features in
            let add_leaf_name info_set callee =
              Abstract.OverUnderSetDomain.
                {
                  element = Simple.LeafName (Interprocedural.Callable.external_target_name callee);
                  in_under = true;
                }
              :: info_set
            in
            List.fold callees ~f:add_leaf_name ~init:info_set
          in
          let leaf_taint =
            LeafDomain.transform
              FlowDetails.simple_feature_set
              (Abstract.Domain.Map add_leaf_names)
              leaf_taint
          in
          trace_info, leaf_taint
    in
    Map.transform Map.KeyValue (Abstract.Domain.Map apply) taint
end

module ForwardTaint = MakeTaint (Sources)
module BackwardTaint = MakeTaint (Sinks)

module MakeTaintTree (Taint : TAINT_DOMAIN) () = struct
  include Abstract.TreeDomain.Make
            (struct
              let max_tree_depth_after_widening = 4

              let check_invariants = true
            end)
            (Taint)
            ()

  let apply_call location ~callees ~port taint_tree =
    let transform_path { path; ancestors = _; tip } =
      let tip =
        Taint.partition
          Taint.leaf
          ~f:(fun leaf -> if Taint.ignore_leaf_at_call leaf then None else Some false)
          tip
        |> (fun map -> Map.Poly.find map false)
        |> function
        | None -> Taint.bottom
        | Some taint -> Taint.apply_call location ~callees ~port ~path ~element:taint
      in
      { path; ancestors = Taint.bottom; tip }
    in
    transform RawPath (Abstract.Domain.Map transform_path) taint_tree


  let empty = bottom

  let is_empty = is_bottom

  let compute_essential_features ~essential_complex_features tree =
    let essential_trace_info = function
      | _ -> TraceInfo.Declaration
    in
    let essential_simple_features _ = Features.SimpleSet.bottom in
    transform Taint.trace_info Abstract.Domain.(Map essential_trace_info) tree
    |> transform Features.ComplexSet.Self Abstract.Domain.(Map essential_complex_features)
    |> transform Features.SimpleSet.Self Abstract.Domain.(Map essential_simple_features)


  (* Keep only non-essential structure. *)
  let essential tree =
    let essential_complex_features _ = Features.ComplexSet.bottom in
    compute_essential_features ~essential_complex_features tree


  let essential_for_constructor tree =
    (* We special case access paths of length 1 to support some precision without blowing the
       analysis up. *)
    let essential_complex_features set = set in
    compute_essential_features ~essential_complex_features tree


  let filter_by_leaf ~leaf taint_tree =
    collapse taint_tree
    |> Taint.partition Taint.leaf ~f:(fun candidate ->
           if Taint.equal_leaf leaf candidate then Some true else None)
    |> (fun map -> Map.Poly.find map true)
    |> Option.value ~default:Taint.bottom


  let get_all_breadcrumbs taint_tree =
    let gather_features feature features =
      let open Features in
      match feature.Abstract.OverUnderSetDomain.element with
      | Simple.Breadcrumb _ -> feature :: features
      (* The ViaValueOf models will be converted to breadcrumbs at the call site via
         `get_callsite_model`. *)
      | Simple.ViaValueOf _ -> feature :: features
      | _ -> features
    in
    fold FlowDetails.simple_feature_element ~f:gather_features ~init:[] taint_tree
end

module MakeTaintEnvironment (Taint : TAINT_DOMAIN) () = struct
  module Tree = MakeTaintTree (Taint) ()

  include Abstract.MapDomain.Make
            (struct
              let name = "env"

              include AccessPath.Root

              let absence_implicitly_maps_to_bottom = true
            end)
            (Tree)

  let create_json ~taint_to_json environment =
    let element_to_json json_list (root, tree) =
      let path_to_json { Tree.path; ancestors = _; tip } json_list =
        let port = AccessPath.create root path |> AccessPath.to_json in
        (path, ["port", port; "taint", taint_to_json tip]) :: json_list
      in
      let ports =
        Tree.fold Tree.RawPath ~f:path_to_json tree ~init:[]
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


  let read ?(transform_non_leaves = fun _ e -> e) ~root ~path environment =
    match get root environment with
    | None -> Tree.bottom
    | Some tree -> Tree.read ~transform_non_leaves path tree


  let empty = bottom

  let is_empty = is_bottom

  let roots environment = fold Key ~f:List.cons ~init:[] environment
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
      Part (BackwardTaint.trace_info, TraceInfo.Declaration);
      Part (BackwardTaint.leaf, Sinks.LocalReturn);
      Part (BackwardTaint.complex_feature, Features.Complex.ReturnAccessPath []);
      Part (Features.SimpleSet.Self, Features.SimpleSet.empty);
    ]


let format_string_feature = Features.Simple.Breadcrumb Features.Breadcrumb.FormatString
