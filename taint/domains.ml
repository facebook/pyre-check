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
    | Origin
    | CallSite of {
        location: Location.t;
        callees: Interprocedural.Callable.t list;
      }
  [@@deriving compare, sexp]

  let show = function
    | Origin -> "origin"
    | CallSite { location; callees; } ->
        let instantiated =
          Ast.Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
        in
        Format.sprintf "via call@%s[%s]"
          (Location.Instantiated.show instantiated)
          (String.concat ~sep:" " (List.map ~f:Interprocedural.Callable.show callees))
end


module SourceSet = Set(Sources)
module SinkSet = Set(Sinks)


module type TAINT_DOMAIN = sig
  include TAINT_SET

  (* Add trace info at call-site *)
  val apply_call: TraceInfo.t -> t -> t
end


module MakeTaint(TaintSet : TAINT_SET) :
  TAINT_DOMAIN with type Leaf.t = TaintSet.Leaf.t = struct
  include Map(TraceInfo)(TaintSet)

  let add taint leaf =
    let key = TraceInfo.Origin in
    match find taint key with
    | None ->
        set taint ~key ~data:(TaintSet.singleton leaf)
    | Some elements ->
        set taint ~key ~data:(TaintSet.add elements leaf)

  let singleton leaf =
    set bottom ~key:TraceInfo.Origin ~data:(TaintSet.singleton leaf)

  let of_list leaves =
    TaintSet.of_list leaves
    |> (fun elements -> set bottom ~key:TraceInfo.Origin ~data:elements)

  let apply_call trace taint =
    let summary =
      fold
        taint
        ~init:TaintSet.bottom
        ~f:(fun ~key:_ ~data taint -> TaintSet.join data taint)
    in
    set bottom ~key:trace ~data:summary

  let show t =
    let show_pair (trace, elements) =
      Format.sprintf "%s -> %s" (TraceInfo.show trace) (TaintSet.show elements)
    in
    let pairs =
      to_alist t
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

  let apply_call location callees taint_tree =
    let trace = TraceInfo.CallSite { location; callees; } in
    filter_map_tree ~f:(Taint.apply_call trace) taint_tree
end


(** Used to infer which sources reach the exit points of a function. *)
module ForwardState = MakeTaintTree(ForwardTaint)


(** Used to infer which sinks are reached from parameters, as well as the
    taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState = MakeTaintTree(BackwardTaint)
