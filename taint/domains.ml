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
      }
  [@@deriving compare, sexp]

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
    | CallSite { location; callees; port; path; } ->
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
          ]
        in
        "call", call_json

  let absence_implicitly_maps_to_bottom = true
end


module SourceSet = Set(Sources)
module SinkSet = Set(Sinks)


module type TAINT_DOMAIN = sig
  include AbstractDomain.S

  val trace_info: TraceInfo.t AbstractDomain.part

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


module MakeTaint(TaintSet : TAINT_SET) : sig
  include TAINT_DOMAIN
  type leaf = TaintSet.element
  val leaves: t -> leaf list
  val singleton: leaf -> t
  val of_list: leaf list -> t

  val leaf: leaf AbstractDomain.part
end = struct
  module Map = AbstractMapDomain.Make(TraceInfo)(TaintSet)
  include Map

  type leaf = TaintSet.element
  [@@derviving compare]

  let trace_info = Map.Key
  let leaf = TaintSet.element

  let add taint leaf =
    let key = TraceInfo.Declaration in
    match Map.find taint key with
    | None ->
        Map.set taint ~key ~data:(TaintSet.singleton leaf)
    | Some elements ->
        Map.set taint ~key ~data:(TaintSet.add elements leaf)

  let of_list leaves =
    if List.length leaves = 0 then
      Map.bottom
    else
      TaintSet.of_list leaves
      |> (fun elements -> Map.set Map.bottom ~key:TraceInfo.Declaration ~data:elements)

  let apply_call location ~callees ~port ~path ~path_element:_ ~element:taint =
    let open TraceInfo in
    let call_trace = CallSite { location; callees; port; path; } in
    let translate trace_key =
      match trace_key with
      | CallSite _ | Origin _ -> call_trace
      | Declaration -> Origin location
    in
    Map.transform Map.Key ~f:translate taint

  let show taint =
    let show_pair (trace, elements) =
      Format.sprintf "%s -> %s" (TraceInfo.show trace) (TaintSet.show elements)
    in
    let pairs =
      Map.to_alist taint
      |> List.map ~f:show_pair
    in
    String.concat ~sep:"\n" pairs

  let to_json taint =
    let element_to_json (trace, elements) =
      let trace_pair = TraceInfo.to_json trace in
      let leaves = TaintSet.to_json elements in
      `Assoc [
        trace_pair;
        "leaves", `List leaves;
      ]
    in
    let elements =
      to_alist taint
      |> List.map ~f:element_to_json
    in
    `List elements

  let leaves map =
    Map.fold TaintSet.element ~init:[] ~f:(fun tail head -> head :: tail) map
    |> List.dedup_and_sort ~compare:TaintSet.compare_element

  let singleton element =
    of_list [element]
end


module ForwardTaint = MakeTaint(SourceSet)
module BackwardTaint = MakeTaint(SinkSet)


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
