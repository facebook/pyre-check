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


module TraceInfo = struct
  type t =
    | Declaration
    | Origin of Location.t
    | CallSite of {
        port: AccessPath.Root.t;
        path: AccessPathTree.Label.path;
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

  let location_to_json location : Yojson.Safe.json =
    let path = Location.path location in
    let line = Location.line location in
    let column = Location.column location in
    let end_column =
      Location.stop_column location  (* Note: not correct for multiple line span *)
    in
    `Assoc [
      "filename", `String path;
      "line", `Int line;
      "start", `Int column;
      "end", `Int end_column;
    ]

  (* Returns the (dictionary key * json) to emit *)
  let to_json trace : string * Yojson.Safe.json =
    match trace with
    | Declaration ->
        "decl", `Null
    | Origin location ->
        let location_json =
          Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
          |> location_to_json
        in
        "root", location_json
    | CallSite { location; callees; port; path; trace_length; } ->
        let location_json =
          Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
          |> location_to_json
        in
        let callee_json =
          List.map
            ~f:(fun callable -> `String (Interprocedural.Callable.external_target_name callable))
            callees
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
        "call", call_json

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
        && AccessPathTree.Label.is_prefix ~prefix:path_right path_left
    | _ ->
        left = right

end


module TraceInfoSet = struct
  include AbstractElementSetDomain.Make(TraceInfo)

  let initial = singleton (TraceInfo.Declaration)

  let trace_info = Element
end


module type TAINT_DOMAIN = sig
  include AbstractDomain.S

  type leaf
  val trace: TraceInfo.t AbstractDomain.part
  val leaf: leaf AbstractDomain.part

  (* Add trace info at call-site *)
  val apply_call:
    Location.t
    -> callees: Interprocedural.Callable.t list
    -> port: AccessPath.Root.t
    -> path: AccessPathTree.Label.path
    -> path_element: t
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

    let to_json leaf =
      let element_to_json leaf =
        let kind = `String (Leaf.show leaf) in
        `Assoc ["kind", kind]
      in
      [element_to_json leaf]
  end

  module Map = AbstractMapDomain.Make(Key)(TraceInfoSet)
  include Map

  type leaf = Leaf.t
  [@@derviving compare]

  let add map leaf =
    Map.set map ~key:leaf ~data:TraceInfoSet.initial

  let singleton leaf =
    add Map.bottom leaf

  let of_list leaves =
    List.fold leaves ~init:Map.bottom ~f:add

  let leaves map =
    Map.to_alist map |> List.map ~f:fst

  let to_json taint =
    let element_to_json (leaf, trace_info_set) =
      let trace_info =
        TraceInfoSet.(fold trace_info ~init: [] ~f:(Fn.flip List.cons) trace_info_set)
        |> List.dedup_and_sort ~compare:TraceInfo.compare
      in
      let trace_json = List.map ~f:TraceInfo.to_json trace_info in
      let leaf_json = Key.to_json leaf in
      List.map
        trace_json
        ~f:(fun trace_pair ->
            `Assoc [
              trace_pair;
              "leaves", `List leaf_json;
            ])
    in
    let elements =
      Map.to_alist taint
      |> List.concat_map ~f:element_to_json
    in
    `List elements

  type _ AbstractDomain.part +=
    | TraceInfo: TraceInfo.t AbstractDomain.part

  let trace = TraceInfo
  let leaf = Map.Key

  let apply_call location ~callees ~port ~path ~path_element:_ ~element:taint =
    let open TraceInfo in
    let call_trace = CallSite { location; callees; port; path; trace_length = 1 } in
    let translate feature =
      match feature with
      | Origin _ ->
          call_trace
      | CallSite { trace_length; _} ->
          CallSite { location; callees; port; path; trace_length = trace_length + 1 }
      | Declaration ->
          Origin location
    in
    Map.transform TraceInfoSet.trace_info ~f:translate taint

end


module ForwardTaint = MakeTaint(Sources)
module BackwardTaint = MakeTaint(Sinks)


module MakeTaintTree(Taint : TAINT_DOMAIN) = struct
  include AccessPathTree.Make
      (AccessPathTree.WithChecks)
      (AccessPath.Root)
      (Taint)

  let apply_call location ~callees ~port taint_tree =
    filter_map_tree_paths ~f:(Taint.apply_call location ~callees ~port) taint_tree

  let to_json taint =
    let element_to_json ~root ~path ~path_element:_ ~element json_list =
      let port =
        AccessPath.create root path
        |> AccessPath.to_json
      in
      (`Assoc [
          "port", port;
          "taint", Taint.to_json element;
        ]
      ) :: json_list
    in
    let paths = fold_paths ~f:element_to_json ~init:[] taint in
    `List paths
end


(** Used to infer which sources reach the exit points of a function. *)
module ForwardState = MakeTaintTree(ForwardTaint)


(** Used to infer which sinks are reached from parameters, as well as the
    taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState = MakeTaintTree(BackwardTaint)
