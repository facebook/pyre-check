(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis


module type LEAF = sig
  type t
  [@@deriving compare]
end


module type S = sig
  include AbstractDomain.S

  module Leaf : LEAF

  val leaves: t -> Leaf.t list
  val partition_tf: t -> f: (Leaf.t -> bool) -> t * t
end


module type TAINT_SET = sig
  include S

  val singleton: Leaf.t -> t
  val add: t -> Leaf.t -> t
  val of_list: Leaf.t list -> t

end


module type SET_ARG = sig
  include Set.Elt

  val show: t -> string
end


module Set(Element: SET_ARG) : TAINT_SET with type Leaf.t = Element.t = struct

  include Analysis.AbstractSetDomain.Make(Element)

  module Leaf = struct
    type t = Element.t
    let compare = Element.compare
  end

  let leaves set =
    elements set
    |> List.sort ~compare:Element.compare

  let partition_tf set =
    partition_tf set

  let of_list leaves =
    List.fold leaves ~init:bottom ~f:add

  let show set =
    leaves set
    |> List.map ~f:Element.show
    |> String.concat ~sep:", "
    |> Format.sprintf "{%s}"
end


module Map(Key: Map.Key)(Element: TAINT_SET) : sig
  include S with type Leaf.t = Element.Leaf.t
  val set: t -> key: Key.t -> data: Element.t -> t
  val find: t -> Key.t -> Element.t option
  val fold : t -> init:'b -> f:(key:Key.t -> data:Element.t -> 'b -> 'b) -> 'b
  val to_alist: ?key_order:[`Increasing | `Decreasing] -> t -> (Key.t * Element.t) list
end = struct

  include Analysis.AbstractMapDomain.Make(Key)(Element)

  module Leaf = Element.Leaf

  let leaves map =
    fold
      map
      ~init:[]
      ~f:(fun ~key:_ ~data leaves ->
          Element.leaves data
          |> List.merge ~compare:Leaf.compare leaves)

  let partition_tf map ~f =
    fold
      map
      ~init:(empty, empty)
      ~f:(fun ~key ~data (true_result, false_result) ->
          let true_elements, false_elements = Element.partition_tf data ~f in
          set true_result ~key ~data:true_elements, set false_result ~key ~data:false_elements)

end


module TraceInfo = struct
  type t =
    | Declaration
    | Origin of Location.t
    | CallSite of {
        location: Location.t;
        callees: Interprocedural.Callable.t list;
      }
  [@@deriving compare, sexp]

  let show = function
    | Declaration -> "declaration"
    | Origin location ->
        let instantiated =
          Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
        in
        Format.sprintf "@%s" (Location.Instantiated.show instantiated)
    | CallSite { location; callees; } ->
        let instantiated =
          Ast.Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
        in
        Format.sprintf
          "via call@%s[%s]"
          (Location.Instantiated.show instantiated)
          (String.concat ~sep:" " (List.map ~f:Interprocedural.Callable.show callees))
end


module SourceSet = Set(Sources)
module SinkSet = Set(Sinks)


module type TAINT_DOMAIN = sig
  include TAINT_SET

  (* Add trace info at call-site *)
  val apply_call: Location.t -> callees: Interprocedural.Callable.t list -> t -> t
end


module MakeTaint(TaintSet : TAINT_SET) :
  TAINT_DOMAIN with type Leaf.t = TaintSet.Leaf.t = struct
  module Map = Map(TraceInfo)(TaintSet)
  include Map

  let add taint leaf =
    let key = TraceInfo.Declaration in
    match Map.find taint key with
    | None ->
        Map.set taint ~key ~data:(TaintSet.singleton leaf)
    | Some elements ->
        Map.set taint ~key ~data:(TaintSet.add elements leaf)

  let singleton leaf =
    Map.set Map.bottom ~key:TraceInfo.Declaration ~data:(TaintSet.singleton leaf)

  let of_list leaves =
    TaintSet.of_list leaves
    |> (fun elements -> Map.set Map.bottom ~key:TraceInfo.Declaration ~data:elements)

  let apply_call location ~callees taint =
    let open TraceInfo in
    let call_trace = CallSite { location; callees; } in
    let translate ~key:trace_key ~data taint =
      let new_trace =
        match trace_key with
        | CallSite _ | Origin _ -> call_trace
        | Declaration -> Origin location
      in
      let singleton_map = Map.set Map.bottom ~key:new_trace ~data in
      Map.join singleton_map taint
    in
    Map.fold taint ~init:Map.bottom ~f:translate

  let show t =
    let show_pair (trace, elements) =
      Format.sprintf "%s -> %s" (TraceInfo.show trace) (TaintSet.show elements)
    in
    let pairs =
      Map.to_alist t
      |> List.map ~f:show_pair
    in
    String.concat ~sep:"\n" pairs

end


module ForwardTaint = MakeTaint(SourceSet)
module BackwardTaint = MakeTaint(SinkSet)


module MakeTaintTree(Taint : TAINT_DOMAIN) = struct
  include AccessPathTree.Make
      (AccessPathTree.WithChecks)
      (AccessPath.Root)
      (Taint)

  let apply_call location ~callees taint_tree =
    filter_map_tree ~f:(Taint.apply_call location ~callees) taint_tree
end


(** Used to infer which sources reach the exit points of a function. *)
module ForwardState = MakeTaintTree(ForwardTaint)


(** Used to infer which sinks are reached from parameters, as well as the
    taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState = MakeTaintTree(BackwardTaint)
