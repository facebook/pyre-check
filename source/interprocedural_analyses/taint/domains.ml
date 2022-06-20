(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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


(* Represents the link between frames. *)
module CallInfo = struct
  let name = "call info"

  type t =
    (* User-specified taint on a model. *)
    | Declaration of {
        (* If not provided, the leaf name set is set as the callee when taint is propagated. *)
        leaf_name_provided: bool;
      }
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
          (String.concat ~sep:", " (List.map ~f:Interprocedural.Target.external_name callees))
          Location.WithModule.pp
          location
          port


  let show = Format.asprintf "%a" pp

  (* Only called when emitting models before we compute the json so we can dedup *)
  let expand_overrides ~override_graph ~is_valid_callee trace =
    match trace with
    | CallSite { location; callees; port; path } ->
        let callees =
          Interprocedural.OverrideGraph.SharedMemory.expand_override_targets override_graph callees
          |> List.filter ~f:(fun callee -> is_valid_callee ~port ~path ~callee)
        in
        CallSite { location; callees; port; path }
    | _ -> trace


  (* Returns the (dictionary key * json) to emit *)
  let to_json ~filename_lookup trace : string * Yojson.Safe.json =
    match trace with
    | Declaration _ -> "decl", `Null
    | Origin location ->
        let location_json = location_with_module_to_json ~filename_lookup location in
        "root", location_json
    | CallSite { location; callees; port; path } ->
        let callee_json =
          callees
          |> List.map ~f:(fun callable -> `String (Interprocedural.Target.external_name callable))
        in
        let location_json = location_with_module_to_json ~filename_lookup location in
        let port_json = AccessPath.create port path |> AccessPath.to_json in
        let call_json =
          `Assoc ["position", location_json; "resolves_to", `List callee_json; "port", port_json]
        in
        "call", call_json


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
        [%compare.equal: AccessPath.Root.t] port_left port_right
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

(* This should be associated with every call site *)
module CallInfoIntervals = struct
  type call_info_intervals = {
    (* The interval of the class that literally contains this call site *)
    caller_interval: Interprocedural.ClassIntervalSet.t;
    (* The interval of the receiver object for this call site *)
    receiver_interval: Interprocedural.ClassIntervalSet.t;
    (* Whether this call site is a call on `self` *)
    is_self_call: bool;
  }

  (* If we are not sure if a call is on `self`, then we should treat it as a call not on `self`,
     such that SAPP will not intersect class intervals. *)
  let top =
    {
      caller_interval = Interprocedural.ClassIntervalSet.top;
      receiver_interval = Interprocedural.ClassIntervalSet.top;
      is_self_call = false;
    }


  let is_top { caller_interval; receiver_interval; is_self_call } =
    Interprocedural.ClassIntervalSet.is_top caller_interval
    && Interprocedural.ClassIntervalSet.is_top receiver_interval
    && is_self_call == false


  let to_json { caller_interval; receiver_interval; is_self_call } =
    let list = ["is_self_call", `Bool is_self_call] in
    let intervals_to_list intervals =
      let intervals = Interprocedural.ClassIntervalSet.to_list intervals in
      List.map intervals ~f:(fun interval ->
          `Assoc
            [
              "lower", `Int (Interprocedural.ClassInterval.lower_bound_exn interval);
              "upper", `Int (Interprocedural.ClassInterval.upper_bound_exn interval);
            ])
    in
    let list =
      if Interprocedural.ClassIntervalSet.is_top receiver_interval then
        list
      else (* Output for SAPP to use *)
        ("receiver_interval", `List (intervals_to_list receiver_interval)) :: list
    in
    let list =
      if
        Interprocedural.ClassIntervalSet.is_empty caller_interval
        || Interprocedural.ClassIntervalSet.is_top caller_interval
      then
        list
      else (* Output for debug purposes *)
        ("caller_interval", `List (intervals_to_list caller_interval)) :: list
    in
    list


  include Abstract.SimpleDomain.Make (struct
    let name = "intervals at call sites"

    type t = call_info_intervals

    let bottom =
      {
        caller_interval = Interprocedural.ClassIntervalSet.bottom;
        receiver_interval = Interprocedural.ClassIntervalSet.bottom;
        is_self_call = true;
      }


    let pp formatter { caller_interval; receiver_interval; is_self_call } =
      Format.fprintf
        formatter
        "@[caller_interval: [%a] receiver_interval: [%a] is_self_call: [%b]@]"
        Interprocedural.ClassIntervalSet.pp
        caller_interval
        Interprocedural.ClassIntervalSet.pp
        receiver_interval
        is_self_call


    let show = Format.asprintf "%a" pp

    let less_or_equal
        ~left:
          {
            caller_interval = caller_interval_left;
            receiver_interval = receiver_interval_left;
            is_self_call = is_self_call_left;
          }
        ~right:
          {
            caller_interval = caller_interval_right;
            receiver_interval = receiver_interval_right;
            is_self_call = is_self_call_right;
          }
      =
      Interprocedural.ClassIntervalSet.less_or_equal
        ~left:caller_interval_left
        ~right:caller_interval_right
      && Interprocedural.ClassIntervalSet.less_or_equal
           ~left:receiver_interval_left
           ~right:receiver_interval_right
      && not ((not is_self_call_left) && is_self_call_right)


    let join
        {
          caller_interval = caller_interval_left;
          receiver_interval = receiver_interval_left;
          is_self_call = is_self_call_left;
        }
        {
          caller_interval = caller_interval_right;
          receiver_interval = receiver_interval_right;
          is_self_call = is_self_call_right;
        }
      =
      {
        caller_interval =
          Interprocedural.ClassIntervalSet.join caller_interval_left caller_interval_right;
        receiver_interval =
          Interprocedural.ClassIntervalSet.join receiver_interval_left receiver_interval_right;
        (* The result of joining two calls is a call on `self` iff. both calls are on `self`. *)
        is_self_call = is_self_call_left && is_self_call_right;
      }


    let meet
        {
          caller_interval = caller_interval_left;
          receiver_interval = receiver_interval_left;
          is_self_call = is_self_call_left;
        }
        {
          caller_interval = caller_interval_right;
          receiver_interval = receiver_interval_right;
          is_self_call = is_self_call_right;
        }
      =
      {
        caller_interval =
          Interprocedural.ClassIntervalSet.meet caller_interval_left caller_interval_right;
        receiver_interval =
          Interprocedural.ClassIntervalSet.meet receiver_interval_left receiver_interval_right;
        (* The result of meeting two calls is a call on `self` iff. one of the calls is on `self`. *)
        is_self_call = is_self_call_left || is_self_call_right;
      }
  end)
end

(* Represents a frame, i.e a single hop between functions. *)
module Frame = struct
  module Slots = struct
    let name = "frame"

    type 'a slot =
      | Breadcrumb : Features.BreadcrumbSet.t slot
      | ViaFeature : Features.ViaFeatureSet.t slot
      | ReturnAccessPath : Features.ReturnAccessPathSet.t slot
      | TraceLength : TraceLength.t slot
      | LeafName : Features.LeafNameSet.t slot
      | FirstIndex : Features.FirstIndexSet.t slot
      | FirstField : Features.FirstFieldSet.t slot

    (* Must be consistent with above variants *)
    let slots = 7

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Breadcrumb -> "Breadcrumb"
      | ViaFeature -> "ViaFeature"
      | ReturnAccessPath -> "ReturnAccessPath"
      | TraceLength -> "TraceLength"
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
      | LeafName -> (module Features.LeafNameSet : Abstract.Domain.S with type t = a)
      | FirstIndex -> (module Features.FirstIndexSet : Abstract.Domain.S with type t = a)
      | FirstField -> (module Features.FirstFieldSet : Abstract.Domain.S with type t = a)


    let strict _ = false
  end

  include Abstract.ProductDomain.Make (Slots)

  let initial =
    create
      [Part (Features.BreadcrumbSet.Self, Features.BreadcrumbSet.empty); Part (TraceLength.Self, 0)]


  let add_propagated_breadcrumb breadcrumb =
    transform Features.BreadcrumbSet.Element Add ~f:breadcrumb


  let add_propagated_breadcrumbs breadcrumbs =
    transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs


  let product_pp = pp (* shadow *)

  let pp formatter = Format.fprintf formatter "Frame(%a)" product_pp

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

  val call_info : CallInfo.t Abstract.Domain.part

  val add_local_breadcrumb : Features.BreadcrumbInterned.t -> t -> t

  val add_local_breadcrumbs : Features.BreadcrumbSet.t -> t -> t

  val add_local_first_index : Abstract.TreeDomain.Label.t -> t -> t

  val add_local_first_field : string -> t -> t

  (* All breadcrumbs from all flows, accumulated with an `add`.
   * The over-under approximation is lost when accumulating. *)
  val accumulated_breadcrumbs : t -> Features.BreadcrumbSet.t

  (* All breadcrumbs from all flows, accumulated with a `join`.
   * The over-under approximation is properly preserved. *)
  val joined_breadcrumbs : t -> Features.BreadcrumbSet.t

  val first_indices : t -> Features.FirstIndexSet.t

  val first_fields : t -> Features.FirstFieldSet.t

  val via_features : t -> Features.ViaFeatureSet.t

  val transform_on_widening_collapse : t -> t

  val prune_maximum_length : TraceLength.t -> t -> t

  type kind_set

  val is_empty_kind_set : kind_set -> bool

  val sanitize : kind_set -> t -> t

  val apply_sanitize_transforms : SanitizeTransformSet.t -> t -> t

  val apply_transforms : TaintTransforms.t -> TaintTransforms.Order.t -> t -> t

  (* Add trace info at call-site *)
  val apply_call
    :  resolution:Analysis.Resolution.t ->
    location:Location.WithModule.t ->
    callee:Interprocedural.Target.t option ->
    arguments:Ast.Expression.Call.Argument.t list ->
    port:AccessPath.Root.t ->
    path:Abstract.TreeDomain.Label.path ->
    element:t ->
    is_self_call:bool ->
    caller_class_interval:Interprocedural.ClassIntervalSet.t ->
    receiver_class_interval:Interprocedural.ClassIntervalSet.t ->
    t

  (* Return the taint with only essential elements. *)
  val essential
    :  return_access_paths:(Features.ReturnAccessPathSet.t -> Features.ReturnAccessPathSet.t) ->
    t ->
    t

  val to_json
    :  expand_overrides:Interprocedural.OverrideGraph.SharedMemory.t option ->
    is_valid_callee:
      (port:AccessPath.Root.t ->
      path:Abstract.TreeDomain.Label.path ->
      callee:Interprocedural.Target.t ->
      bool) ->
    filename_lookup:(Reference.t -> string option) option ->
    t ->
    Yojson.Safe.json

  (* For every frame, convert the may breadcrumbs into must breadcrumbs. *)
  val may_breadcrumbs_to_must : t -> t

  (* Within every local taint, join every frame with the frame in the same local taint that has the
     specified kind. *)
  val join_every_frame_with : frame_kind:kind -> t -> t
end

module type KIND_ARG = sig
  include Abstract.SetDomain.ELEMENT

  val equal : t -> t -> bool

  val show : t -> string

  val ignore_kind_at_call : t -> bool

  val apply_call : t -> t

  val discard_subkind : t -> t

  val discard_sanitize_transforms : t -> t

  val apply_sanitize_transforms : SanitizeTransformSet.t -> t -> t option

  val apply_transforms : TaintTransforms.t -> TaintTransforms.Order.t -> t -> t option

  module Set : sig
    include Stdlib.Set.S with type elt = t

    val pp : Format.formatter -> t -> unit

    val show : t -> string
  end
end

(* Represents a map from a taint kind (`Sources.t` or `Sinks.t`) to a frame. *)
module MakeKindTaint (Kind : KIND_ARG) = struct
  module Key = struct
    include Kind

    let absence_implicitly_maps_to_bottom = false
  end

  include Abstract.MapDomain.Make (Key) (Frame)

  let singleton kind frame = set bottom ~key:kind ~data:frame
end

(* Represents taint originating from a specific call. *)
module MakeLocalTaint (Kind : KIND_ARG) = struct
  module KindTaintDomain = MakeKindTaint (Kind)

  module Slots = struct
    let name = "local taint"

    type 'a slot =
      | Kinds : KindTaintDomain.t slot
      | TitoPosition : Features.TitoPositionSet.t slot
      | Breadcrumb : Features.BreadcrumbSet.t slot
      | FirstIndex : Features.FirstIndexSet.t slot
      | FirstField : Features.FirstFieldSet.t slot
      | CallInfoIntervals : CallInfoIntervals.t slot

    (* Must be consistent with above variants *)
    let slots = 6

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Kinds -> "Kinds"
      | TitoPosition -> "TitoPosition"
      | Breadcrumb -> "Breadcrumb"
      | FirstIndex -> "FirstIndex"
      | FirstField -> "FirstField"
      | CallInfoIntervals -> "CallInfoIntervals"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Kinds -> (module KindTaintDomain : Abstract.Domain.S with type t = a)
      | TitoPosition -> (module Features.TitoPositionSet : Abstract.Domain.S with type t = a)
      | Breadcrumb -> (module Features.BreadcrumbSet : Abstract.Domain.S with type t = a)
      | FirstIndex -> (module Features.FirstIndexSet : Abstract.Domain.S with type t = a)
      | FirstField -> (module Features.FirstFieldSet : Abstract.Domain.S with type t = a)
      | CallInfoIntervals -> (module CallInfoIntervals : Abstract.Domain.S with type t = a)


    let strict (type a) (slot : a slot) =
      match slot with
      | Kinds
      | CallInfoIntervals ->
          true
      | _ -> false
  end

  include Abstract.ProductDomain.Make (Slots)

  (* Warning: do NOT use `BreadcrumbSet`, `FirstFieldSet` and `FirstFieldSet` abstract parts
   * (e.g,`BreadcrumbSet.Self`, `BreadcrumbSet.Element`, etc.) since these are ambiguous.
   * They can refer to the sets in `Frame` or in `LocalTaint`. *)

  let singleton kind frame =
    (* Initialize strict slots first *)
    create
      [
        Abstract.Domain.Part (KindTaintDomain.KeyValue, (kind, frame));
        Abstract.Domain.Part (CallInfoIntervals.Self, CallInfoIntervals.top);
      ]
    |> update Slots.Breadcrumb Features.BreadcrumbSet.empty


  let product_pp = pp (* shadow *)

  let pp formatter = Format.fprintf formatter "LocalTaint(%a)" product_pp

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

module MakeTaint (Kind : KIND_ARG) : sig
  include TAINT_DOMAIN with type kind = Kind.t and type kind_set = Kind.Set.t

  val kinds : t -> kind list

  val singleton : ?location:Location.WithModule.t -> Kind.t -> Frame.t -> t

  val of_list : ?location:Location.WithModule.t -> Kind.t list -> t
end = struct
  type kind = Kind.t [@@deriving compare, eq]

  module CallInfoKey = struct
    include CallInfo

    let absence_implicitly_maps_to_bottom = true
  end

  module LocalTaintDomain = MakeLocalTaint (Kind)
  module KindTaintDomain = LocalTaintDomain.KindTaintDomain
  module Map = Abstract.MapDomain.Make (CallInfoKey) (LocalTaintDomain)
  include Map

  let add ?location map kind frame =
    let call_info =
      match location with
      | None -> CallInfo.Declaration { leaf_name_provided = false }
      | Some location -> CallInfo.Origin location
    in
    let local_taint = LocalTaintDomain.singleton kind frame in
    Map.update map call_info ~f:(function
        | None -> local_taint
        | Some existing -> LocalTaintDomain.join local_taint existing)


  let singleton ?location kind frame = add ?location Map.bottom kind frame

  let of_list ?location kinds =
    List.fold kinds ~init:Map.bottom ~f:(fun taint kind -> add ?location taint kind Frame.initial)


  let kind = KindTaintDomain.Key

  let call_info = Map.Key

  let kinds map =
    Map.fold kind ~init:[] ~f:List.cons map |> List.dedup_and_sort ~compare:Kind.compare


  let to_json ~expand_overrides ~is_valid_callee ~filename_lookup taint =
    let cons_if_non_empty key list assoc =
      if List.is_empty list then
        assoc
      else
        (key, `List list) :: assoc
    in
    let open Features in
    let breadcrumbs_to_json ~breadcrumbs ~first_indices ~first_fields =
      let breadcrumb_to_json { Abstract.OverUnderSetDomain.element; in_under } breadcrumbs =
        let element = BreadcrumbInterned.unintern element in
        let json = Breadcrumb.to_json element ~on_all_paths:in_under in
        json :: breadcrumbs
      in
      let breadcrumbs =
        BreadcrumbSet.fold BreadcrumbSet.ElementAndUnder ~f:breadcrumb_to_json ~init:[] breadcrumbs
      in
      let first_index_breadcrumbs =
        first_indices
        |> FirstIndexSet.elements
        |> List.map ~f:FirstIndexInterned.unintern
        |> FirstIndex.to_json
      in
      let first_field_breadcrumbs =
        first_fields
        |> FirstFieldSet.elements
        |> List.map ~f:FirstFieldInterned.unintern
        |> FirstField.to_json
      in
      List.concat [first_index_breadcrumbs; first_field_breadcrumbs; breadcrumbs]
    in

    let trace_to_json (trace_info, local_taint) =
      let json = [CallInfo.to_json ~filename_lookup trace_info] in

      let tito_positions =
        LocalTaintDomain.get LocalTaintDomain.Slots.TitoPosition local_taint
        |> TitoPositionSet.elements
        |> List.map ~f:location_to_json
      in
      let json = cons_if_non_empty "tito" tito_positions json in

      let local_breadcrumbs =
        breadcrumbs_to_json
          ~breadcrumbs:(LocalTaintDomain.get LocalTaintDomain.Slots.Breadcrumb local_taint)
          ~first_indices:(LocalTaintDomain.get LocalTaintDomain.Slots.FirstIndex local_taint)
          ~first_fields:(LocalTaintDomain.get LocalTaintDomain.Slots.FirstField local_taint)
      in
      let json = cons_if_non_empty "local_features" local_breadcrumbs json in

      let add_kind (kind, frame) =
        let json = ["kind", `String (Kind.show kind)] in

        let trace_length = Frame.get Frame.Slots.TraceLength frame in
        let json =
          if trace_length = 0 then
            json
          else
            ("length", `Int trace_length) :: json
        in

        let leaves =
          Frame.get Frame.Slots.LeafName frame
          |> LeafNameSet.elements
          |> List.map ~f:LeafNameInterned.unintern
          |> List.map ~f:LeafName.to_json
        in
        let json = cons_if_non_empty "leaves" leaves json in

        let return_paths =
          Frame.get Frame.Slots.ReturnAccessPath frame
          |> ReturnAccessPathSet.elements
          |> List.map ~f:ReturnAccessPath.to_json
        in
        let json = cons_if_non_empty "return_paths" return_paths json in

        let via_features =
          Frame.get Frame.Slots.ViaFeature frame
          |> ViaFeatureSet.elements
          |> List.map ~f:ViaFeature.to_json
        in
        let json = cons_if_non_empty "via_features" via_features json in

        let breadcrumbs =
          breadcrumbs_to_json
            ~breadcrumbs:(Frame.get Frame.Slots.Breadcrumb frame)
            ~first_indices:(Frame.get Frame.Slots.FirstIndex frame)
            ~first_fields:(Frame.get Frame.Slots.FirstField frame)
        in
        let json = cons_if_non_empty "features" breadcrumbs json in

        `Assoc json
      in
      let kinds =
        LocalTaintDomain.get LocalTaintDomain.Slots.Kinds local_taint
        |> KindTaintDomain.to_alist
        |> List.map ~f:add_kind
      in
      let json = cons_if_non_empty "kinds" kinds json in
      let json =
        let call_info_intervals =
          LocalTaintDomain.get LocalTaintDomain.Slots.CallInfoIntervals local_taint
        in
        if CallInfoIntervals.is_top call_info_intervals then
          json
        else
          let call_info_intervals = CallInfoIntervals.to_json call_info_intervals in
          List.append call_info_intervals json
      in
      `Assoc json
    in
    let taint =
      match expand_overrides with
      | Some override_graph ->
          Map.transform
            Key
            Map
            ~f:(CallInfo.expand_overrides ~override_graph ~is_valid_callee)
            taint
      | None -> taint
    in
    let elements = Map.to_alist taint |> List.map ~f:trace_to_json in
    `List elements


  let add_local_breadcrumbs breadcrumbs taint =
    let apply_local_taint local_taint =
      let breadcrumbs =
        LocalTaintDomain.get LocalTaintDomain.Slots.Breadcrumb local_taint
        |> Features.BreadcrumbSet.add_set ~to_add:breadcrumbs
      in
      LocalTaintDomain.update LocalTaintDomain.Slots.Breadcrumb breadcrumbs local_taint
    in
    transform LocalTaintDomain.Self Map ~f:apply_local_taint taint


  let add_local_breadcrumb breadcrumb taint =
    add_local_breadcrumbs (Features.BreadcrumbSet.singleton breadcrumb) taint


  let add_local_first_index index taint =
    let apply_local_taint local_taint =
      let first_indices =
        LocalTaintDomain.get LocalTaintDomain.Slots.FirstIndex local_taint
        |> Features.FirstIndexSet.add_first index
      in
      LocalTaintDomain.update LocalTaintDomain.Slots.FirstIndex first_indices local_taint
    in
    transform LocalTaintDomain.Self Map ~f:apply_local_taint taint


  let add_local_first_field attribute taint =
    let apply_local_taint local_taint =
      let first_fields =
        LocalTaintDomain.get LocalTaintDomain.Slots.FirstField local_taint
        |> Features.FirstFieldSet.add_first attribute
      in
      LocalTaintDomain.update LocalTaintDomain.Slots.FirstField first_fields local_taint
    in
    transform LocalTaintDomain.Self Map ~f:apply_local_taint taint


  let get_features ~frame_slot ~local_slot ~bottom ~join ~sequence_join taint =
    let local_taint_features local_taint sofar =
      let frame_features frame sofar = Frame.get frame_slot frame |> join sofar in
      let features = LocalTaintDomain.fold Frame.Self local_taint ~init:bottom ~f:frame_features in
      let features = LocalTaintDomain.get local_slot local_taint |> sequence_join features in
      join sofar features
    in
    fold LocalTaintDomain.Self ~f:local_taint_features ~init:bottom taint


  let accumulated_breadcrumbs taint =
    get_features
      ~frame_slot:Frame.Slots.Breadcrumb
      ~local_slot:LocalTaintDomain.Slots.Breadcrumb
      ~bottom:Features.BreadcrumbSet.bottom
      ~join:Features.BreadcrumbSet.sequence_join
      ~sequence_join:Features.BreadcrumbSet.sequence_join
      taint


  let joined_breadcrumbs taint =
    get_features
      ~frame_slot:Frame.Slots.Breadcrumb
      ~local_slot:LocalTaintDomain.Slots.Breadcrumb
      ~bottom:Features.BreadcrumbSet.bottom
      ~join:Features.BreadcrumbSet.join
      ~sequence_join:Features.BreadcrumbSet.sequence_join
      taint


  let get_first ~frame_slot ~local_slot ~bottom ~join ~sequence_join taint =
    let local_taint_first local_taint sofar =
      let local_first = LocalTaintDomain.get local_slot local_taint in
      let frame_first frame sofar =
        Frame.get frame_slot frame |> sequence_join local_first |> join sofar
      in
      LocalTaintDomain.fold Frame.Self local_taint ~init:sofar ~f:frame_first
    in
    fold LocalTaintDomain.Self ~f:local_taint_first ~init:bottom taint


  let first_indices taint =
    get_first
      ~frame_slot:Frame.Slots.FirstIndex
      ~local_slot:LocalTaintDomain.Slots.FirstIndex
      ~bottom:Features.FirstIndexSet.bottom
      ~join:Features.FirstIndexSet.join
      ~sequence_join:Features.FirstIndexSet.sequence_join
      taint


  let first_fields taint =
    get_first
      ~frame_slot:Frame.Slots.FirstField
      ~local_slot:LocalTaintDomain.Slots.FirstField
      ~bottom:Features.FirstFieldSet.bottom
      ~join:Features.FirstFieldSet.join
      ~sequence_join:Features.FirstFieldSet.sequence_join
      taint


  let via_features taint =
    fold
      Features.ViaFeatureSet.Self
      ~f:Features.ViaFeatureSet.join
      ~init:Features.ViaFeatureSet.bottom
      taint


  let transform_on_widening_collapse taint =
    (* using an always-feature here would break the widening invariant: a <= a widen b *)
    let open Features in
    let broadening =
      BreadcrumbSet.of_approximation
        [
          { element = Features.broadening (); in_under = false };
          { element = Features.issue_broadening (); in_under = false };
        ]
    in
    add_local_breadcrumbs broadening taint


  let prune_maximum_length maximum_length =
    let filter_flow (_, frame) =
      let length = Frame.get Frame.Slots.TraceLength frame in
      TraceLength.is_bottom length || TraceLength.less_or_equal ~left:maximum_length ~right:length
    in
    transform KindTaintDomain.KeyValue Filter ~f:filter_flow


  type kind_set = Kind.Set.t

  let is_empty_kind_set = Kind.Set.is_empty

  let sanitize sanitized_kinds taint =
    if Kind.Set.is_empty sanitized_kinds then
      taint
    else
      transform
        KindTaintDomain.Key
        Filter
        ~f:(fun kind ->
          let kind = kind |> Kind.discard_sanitize_transforms |> Kind.discard_subkind in
          not (Kind.Set.mem kind sanitized_kinds))
        taint


  let apply_sanitize_transforms transforms taint =
    if SanitizeTransformSet.is_empty transforms then
      taint
    else
      transform KindTaintDomain.Key FilterMap ~f:(Kind.apply_sanitize_transforms transforms) taint


  let apply_transforms transforms order taint =
    if TaintTransforms.is_empty transforms then
      taint
    else
      transform KindTaintDomain.Key FilterMap ~f:(Kind.apply_transforms transforms order) taint


  let apply_call
      ~resolution
      ~location
      ~callee
      ~arguments
      ~port
      ~path
      ~element:taint
      ~is_self_call
      ~caller_class_interval
      ~receiver_class_interval
    =
    let callees =
      match callee with
      | Some callee -> [callee]
      | None -> []
    in
    let apply (call_info, local_taint) =
      let local_taint =
        local_taint
        |> LocalTaintDomain.transform KindTaintDomain.Key Filter ~f:(fun kind ->
               not (Kind.ignore_kind_at_call kind))
        |> LocalTaintDomain.transform KindTaintDomain.Key Map ~f:Kind.apply_call
      in
      let via_features_breadcrumbs =
        LocalTaintDomain.fold
          Features.ViaFeatureSet.Element
          ~f:Features.ViaFeatureSet.add
          ~init:Features.ViaFeatureSet.bottom
          local_taint
        |> Features.expand_via_features ~resolution ~callees ~arguments
      in
      let local_breadcrumbs = LocalTaintDomain.get LocalTaintDomain.Slots.Breadcrumb local_taint in
      let local_first_indices =
        LocalTaintDomain.get LocalTaintDomain.Slots.FirstIndex local_taint
      in
      let local_first_fields = LocalTaintDomain.get LocalTaintDomain.Slots.FirstField local_taint in
      let local_taint =
        local_taint
        |> LocalTaintDomain.update
             LocalTaintDomain.Slots.TitoPosition
             Features.TitoPositionSet.bottom
        |> LocalTaintDomain.update LocalTaintDomain.Slots.Breadcrumb via_features_breadcrumbs
        |> LocalTaintDomain.update LocalTaintDomain.Slots.FirstIndex Features.FirstIndexSet.bottom
        |> LocalTaintDomain.update LocalTaintDomain.Slots.FirstField Features.FirstFieldSet.bottom
      in
      let apply_frame frame =
        frame
        |> Frame.update Frame.Slots.ViaFeature Features.ViaFeatureSet.bottom
        |> Frame.transform
             Features.BreadcrumbSet.Self
             Map
             ~f:(Features.BreadcrumbSet.sequence_join local_breadcrumbs)
        |> Frame.transform
             Features.FirstIndexSet.Self
             Map
             ~f:(Features.FirstIndexSet.sequence_join local_first_indices)
        |> Frame.transform
             Features.FirstFieldSet.Self
             Map
             ~f:(Features.FirstFieldSet.sequence_join local_first_fields)
      in
      let local_taint = LocalTaintDomain.transform Frame.Self Map ~f:apply_frame local_taint in
      let local_taint =
        match callee with
        | None
        | Some (Interprocedural.Target.Object _)
        | Some (Interprocedural.Target.Function _) ->
            LocalTaintDomain.update
              LocalTaintDomain.Slots.CallInfoIntervals
              {
                CallInfoIntervals.caller_interval = caller_class_interval;
                receiver_interval = receiver_class_interval;
                is_self_call;
              }
              local_taint
        | Some (Interprocedural.Target.Method _)
        | Some (Interprocedural.Target.Override _) ->
            let open Interprocedural in
            let { CallInfoIntervals.caller_interval = callee_class_interval; _ } =
              LocalTaintDomain.get LocalTaintDomain.Slots.CallInfoIntervals local_taint
            in
            let intersect left right =
              let new_interval = ClassIntervalSet.meet left right in
              let should_propagate =
                (* Propagate if the intersection is not empty, and there exists a descendant
                   relation between left and right *)
                (not (ClassIntervalSet.is_empty new_interval))
                && (ClassIntervalSet.equal left new_interval
                   || ClassIntervalSet.equal right new_interval)
              in
              new_interval, should_propagate
            in
            if is_self_call then
              let new_interval, should_propagate =
                intersect callee_class_interval caller_class_interval
              in
              if not should_propagate then
                LocalTaintDomain.bottom
              else
                LocalTaintDomain.update
                  LocalTaintDomain.Slots.CallInfoIntervals
                  {
                    CallInfoIntervals.caller_interval = new_interval;
                    receiver_interval = receiver_class_interval;
                    is_self_call;
                  }
                  local_taint
            else
              let _, should_propagate = intersect callee_class_interval receiver_class_interval in
              if not should_propagate then
                LocalTaintDomain.bottom
              else
                LocalTaintDomain.update
                  LocalTaintDomain.Slots.CallInfoIntervals
                  {
                    CallInfoIntervals.caller_interval = caller_class_interval;
                    receiver_interval = receiver_class_interval;
                    is_self_call;
                  }
                  local_taint
      in
      match call_info with
      | CallInfo.Origin _
      | CallInfo.CallSite _ ->
          let increase_length n = if n < max_int then n + 1 else n in
          let call_info = CallInfo.CallSite { location; callees; port; path } in
          let local_taint =
            local_taint |> LocalTaintDomain.transform TraceLength.Self Map ~f:increase_length
          in
          call_info, local_taint
      | CallInfo.Declaration { leaf_name_provided } ->
          let call_info = CallInfo.Origin location in
          let new_leaf_names =
            if leaf_name_provided then
              Features.LeafNameSet.bottom
            else
              let open Features in
              let make_leaf_name callee =
                LeafName.{ leaf = Interprocedural.Target.external_name callee; port = None }
                |> LeafNameInterned.intern
              in
              List.map ~f:make_leaf_name callees |> Features.LeafNameSet.of_list
          in
          let local_taint =
            LocalTaintDomain.transform Features.LeafNameSet.Self Add ~f:new_leaf_names local_taint
          in
          call_info, local_taint
    in
    Map.transform Map.KeyValue Map ~f:apply taint


  let may_breadcrumbs_to_must taint =
    let apply_frame frame =
      Frame.transform Features.BreadcrumbSet.Self Map ~f:Features.BreadcrumbSet.over_to_under frame
    in
    Map.transform Frame.Self Map ~f:apply_frame taint


  let join_every_frame_with ~frame_kind taint =
    let apply_local_taint local_taint =
      let frame_to_join =
        let kinds = LocalTaintDomain.get LocalTaintDomain.Slots.Kinds local_taint in
        KindTaintDomain.get frame_kind kinds
      in
      if not (Frame.is_bottom frame_to_join) then
        LocalTaintDomain.transform Frame.Self Map ~f:(Frame.join frame_to_join) local_taint
      else
        local_taint
    in
    Map.transform LocalTaintDomain.Self Map ~f:apply_local_taint taint


  let essential ~return_access_paths taint =
    let apply (_, local_taint) =
      let call_info = CallInfo.Declaration { leaf_name_provided = false } in
      let local_taint =
        local_taint
        |> LocalTaintDomain.update
             LocalTaintDomain.Slots.TitoPosition
             Features.TitoPositionSet.bottom
        |> LocalTaintDomain.update LocalTaintDomain.Slots.Breadcrumb Features.BreadcrumbSet.empty
        |> LocalTaintDomain.update LocalTaintDomain.Slots.FirstIndex Features.FirstIndexSet.bottom
        |> LocalTaintDomain.update LocalTaintDomain.Slots.FirstField Features.FirstFieldSet.bottom
      in
      let apply_frame frame =
        frame
        |> Frame.update Frame.Slots.ViaFeature Features.ViaFeatureSet.bottom
        |> Frame.update Frame.Slots.Breadcrumb Features.BreadcrumbSet.bottom
        |> Frame.update Frame.Slots.FirstIndex Features.FirstIndexSet.bottom
        |> Frame.update Frame.Slots.FirstField Features.FirstFieldSet.bottom
        |> Frame.update Frame.Slots.LeafName Features.LeafNameSet.bottom
        |> Frame.transform Features.ReturnAccessPathSet.Self Map ~f:return_access_paths
      in
      let local_taint = LocalTaintDomain.transform Frame.Self Map ~f:apply_frame local_taint in
      call_info, local_taint
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

  let apply_call
      ~resolution
      ~location
      ~callee
      ~arguments
      ~port
      ~is_self_call
      ~caller_class_interval
      ~receiver_class_interval
      taint_tree
    =
    let transform_path (path, tip) =
      ( path,
        Taint.apply_call
          ~resolution
          ~location
          ~callee
          ~arguments
          ~port
          ~path
          ~element:tip
          ~is_self_call
          ~caller_class_interval
          ~receiver_class_interval )
    in
    transform Path Map ~f:transform_path taint_tree


  let may_breadcrumbs_to_must tree = transform Taint.Self Map ~f:Taint.may_breadcrumbs_to_must tree

  let empty = bottom

  let is_empty = is_bottom

  (* Return the taint tree with only the essential structure. *)
  let essential tree =
    let return_access_paths _ = Features.ReturnAccessPathSet.bottom in
    transform Taint.Self Map ~f:(Taint.essential ~return_access_paths) tree


  let essential_for_constructor tree =
    transform Taint.Self Map ~f:(Taint.essential ~return_access_paths:Fn.id) tree


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


  let add_local_breadcrumb breadcrumb =
    transform Taint.Self Map ~f:(Taint.add_local_breadcrumb breadcrumb)


  let add_local_breadcrumbs breadcrumbs taint_tree =
    if Features.BreadcrumbSet.is_bottom breadcrumbs || Features.BreadcrumbSet.is_empty breadcrumbs
    then
      taint_tree
    else
      transform Taint.Self Map ~f:(Taint.add_local_breadcrumbs breadcrumbs) taint_tree


  let add_local_first_index index = transform Taint.Self Map ~f:(Taint.add_local_first_index index)

  let add_local_first_field attribute =
    transform Taint.Self Map ~f:(Taint.add_local_first_field attribute)


  let accumulated_breadcrumbs taint_tree =
    let gather_breadcrumbs taint sofar =
      Taint.accumulated_breadcrumbs taint |> Features.BreadcrumbSet.add_set ~to_add:sofar
    in
    fold Taint.Self ~f:gather_breadcrumbs ~init:Features.BreadcrumbSet.bottom taint_tree


  let joined_breadcrumbs taint_tree =
    let gather_breadcrumbs taint sofar =
      Taint.accumulated_breadcrumbs taint |> Features.BreadcrumbSet.join sofar
    in
    fold Taint.Self ~f:gather_breadcrumbs ~init:Features.BreadcrumbSet.bottom taint_tree


  let add_via_features via_features taint_tree =
    if Features.ViaFeatureSet.is_bottom via_features then
      taint_tree
    else
      transform Features.ViaFeatureSet.Self Add ~f:via_features taint_tree


  let sanitize sanitized_kinds taint =
    if Taint.is_empty_kind_set sanitized_kinds then
      taint
    else
      transform Taint.Self Map ~f:(Taint.sanitize sanitized_kinds) taint


  let apply_sanitize_transforms transforms taint =
    if SanitizeTransformSet.is_empty transforms then
      taint
    else
      transform Taint.Self Map ~f:(Taint.apply_sanitize_transforms transforms) taint


  let apply_transforms transforms order taint =
    if TaintTransforms.is_empty transforms then
      taint
    else
      transform Taint.Self Map ~f:(Taint.apply_transforms transforms order) taint
end

module MakeTaintEnvironment (Taint : TAINT_DOMAIN) () = struct
  module Tree = MakeTaintTree (Taint) ()

  include
    Abstract.MapDomain.Make
      (struct
        let name = "env"

        include AccessPath.Root

        let absence_implicitly_maps_to_bottom = true
      end)
      (Tree)

  let to_json ~expand_overrides ~is_valid_callee ~filename_lookup environment =
    let element_to_json json_list (root, tree) =
      let path_to_json (path, tip) json_list =
        let port = AccessPath.create root path |> AccessPath.to_json in
        ( path,
          [
            "port", port;
            "taint", Taint.to_json ~expand_overrides ~is_valid_callee ~filename_lookup tip;
          ] )
        :: json_list
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

  let may_breadcrumbs_to_must = transform Taint.Self Map ~f:Taint.may_breadcrumbs_to_must

  let join_every_frame_with ~frame_kind =
    transform Taint.Self Map ~f:(Taint.join_every_frame_with ~frame_kind)


  let add_local_breadcrumb breadcrumb =
    transform Taint.Self Map ~f:(Taint.add_local_breadcrumb breadcrumb)


  let add_local_breadcrumbs breadcrumbs taint_tree =
    if Features.BreadcrumbSet.is_bottom breadcrumbs || Features.BreadcrumbSet.is_empty breadcrumbs
    then
      taint_tree
    else
      transform Taint.Self Map ~f:(Taint.add_local_breadcrumbs breadcrumbs) taint_tree


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
    Taint.accumulated_breadcrumbs taint, Taint.via_features taint


  let sanitize sanitized_kinds taint =
    if Taint.is_empty_kind_set sanitized_kinds then
      taint
    else
      transform Taint.Self Map ~f:(Taint.sanitize sanitized_kinds) taint


  let apply_sanitize_transforms transforms taint =
    if SanitizeTransformSet.is_empty transforms then
      taint
    else
      transform Taint.Self Map ~f:(Taint.apply_sanitize_transforms transforms) taint
end

module ForwardState = MakeTaintEnvironment (ForwardTaint) ()
(** Used to infer which sources reach the exit points of a function. *)

module BackwardState = MakeTaintEnvironment (BackwardTaint) ()
(** Used to infer which sinks are reached from parameters, as well as the taint-in-taint-out (TITO)
    using the special LocalReturn sink. *)

let local_return_frame =
  Frame.create
    [
      Part (TraceLength.Self, 0);
      Part (Features.ReturnAccessPathSet.Element, []);
      Part (Features.BreadcrumbSet.Self, Features.BreadcrumbSet.empty);
    ]


(* Special sink as it needs the return access path *)
let local_return_taint = BackwardTaint.singleton Sinks.LocalReturn local_return_frame

module MakeSanitizeKinds (Kind : KIND_ARG) = struct
  type set =
    | All
    | Specific of Kind.Set.t
  [@@deriving show, eq]

  include Abstract.SimpleDomain.Make (struct
    type t = set option [@@deriving show]

    let name = Format.sprintf "sanitize %ss" Kind.name

    let bottom = None

    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        match left, right with
        | None, _ -> true
        | Some _, None -> false
        | Some All, Some All -> true
        | Some All, Some (Specific _) -> false
        | Some (Specific _), Some All -> true
        | Some (Specific left), Some (Specific right) -> Kind.Set.subset left right


    let join left right =
      if phys_equal left right then
        left
      else
        match left, right with
        | None, Some _ -> right
        | Some _, None -> left
        | Some All, _
        | _, Some All ->
            Some All
        | Some (Specific left_sources), Some (Specific right_sources) ->
            Some (Specific (Kind.Set.union left_sources right_sources))
        | None, None -> None


    let meet a b = if less_or_equal ~left:b ~right:a then b else a
  end)

  let all = Some All

  let equal = [%equal: set option]

  let to_json set =
    let label = Format.sprintf "%ss" Kind.name in
    match set with
    | Some All -> [label, `String "All"]
    | Some (Specific set) ->
        [
          ( label,
            let to_string name = `String name in
            `List (set |> Kind.Set.elements |> List.map ~f:Kind.show |> List.map ~f:to_string) );
        ]
    | None -> []
end

module SanitizeSources = MakeSanitizeKinds (Sources)
module SanitizeSinks = MakeSanitizeKinds (Sinks)

module SanitizeTito = struct
  type set =
    | All
    | Specific of {
        sanitized_tito_sources: Sources.Set.t;
        sanitized_tito_sinks: Sinks.Set.t;
      }
  [@@deriving show, eq]

  include Abstract.SimpleDomain.Make (struct
    type t = set option [@@deriving show]

    let name = "sanitize tito"

    let bottom = None

    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        match left, right with
        | None, _ -> true
        | Some _, None -> false
        | Some All, Some All -> true
        | Some All, Some (Specific _) -> false
        | Some (Specific _), Some All -> true
        | Some (Specific left), Some (Specific right) ->
            Sources.Set.subset left.sanitized_tito_sources right.sanitized_tito_sources
            && Sinks.Set.subset left.sanitized_tito_sinks right.sanitized_tito_sinks


    let join left right =
      if phys_equal left right then
        left
      else
        match left, right with
        | None, Some tito
        | Some tito, None ->
            Some tito
        | Some All, _
        | _, Some All ->
            Some All
        | Some (Specific left), Some (Specific right) ->
            Some
              (Specific
                 {
                   sanitized_tito_sources =
                     Sources.Set.union left.sanitized_tito_sources right.sanitized_tito_sources;
                   sanitized_tito_sinks =
                     Sinks.Set.union left.sanitized_tito_sinks right.sanitized_tito_sinks;
                 })
        | None, None -> None


    let meet a b = if less_or_equal ~left:b ~right:a then b else a
  end)

  let all = Some All

  let equal = [%equal: set option]

  let to_json set =
    let to_string name = `String name in
    let sources_to_json sources =
      `List (sources |> Sources.Set.elements |> List.map ~f:Sources.show |> List.map ~f:to_string)
    in
    let sinks_to_json sinks =
      `List (sinks |> Sinks.Set.elements |> List.map ~f:Sinks.show |> List.map ~f:to_string)
    in
    match set with
    | Some All -> ["tito", `String "All"]
    | Some (Specific { sanitized_tito_sources; sanitized_tito_sinks }) ->
        [
          "tito_sources", sources_to_json sanitized_tito_sources;
          "tito_sinks", sinks_to_json sanitized_tito_sinks;
        ]
    | None -> []
end

module Sanitize = struct
  type sanitize = {
    sources: SanitizeSources.t;
    sinks: SanitizeSinks.t;
    tito: SanitizeTito.t;
  }
  [@@deriving show, eq]

  include Abstract.SimpleDomain.Make (struct
    type t = sanitize

    let name = "sanitize"

    let bottom =
      { sources = SanitizeSources.bottom; sinks = SanitizeSinks.bottom; tito = SanitizeTito.bottom }


    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        SanitizeSources.less_or_equal ~left:left.sources ~right:right.sources
        && SanitizeSinks.less_or_equal ~left:left.sinks ~right:right.sinks
        && SanitizeTito.less_or_equal ~left:left.tito ~right:right.tito


    let join left right =
      if phys_equal left right then
        left
      else
        let sources = SanitizeSources.join left.sources right.sources in
        let sinks = SanitizeSinks.join left.sinks right.sinks in
        let tito = SanitizeTito.join left.tito right.tito in
        { sources; sinks; tito }


    let meet a b = if less_or_equal ~left:b ~right:a then b else a

    let show = show_sanitize
  end)

  let all = { sources = SanitizeSources.all; sinks = SanitizeSinks.all; tito = SanitizeTito.all }

  let empty = bottom

  let is_empty = is_bottom

  let equal = equal_sanitize

  let to_json { sources; sinks; tito } =
    let sources_json = SanitizeSources.to_json sources in
    let sinks_json = SanitizeSinks.to_json sinks in
    let tito_json = SanitizeTito.to_json tito in
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
