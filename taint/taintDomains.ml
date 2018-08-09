(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis


module type S = sig
  include AbstractDomain.S

  module Leaf : sig
    type t
    [@@deriving compare]
  end

  val leaves: t -> Leaf.t list
  val partition_tf: t -> f: (Leaf.t -> bool) -> t * t
end


module Set(Element: Set.Elt) : sig

  include S with type Leaf.t = Element.t

  val singleton: Leaf.t -> t
  val add: t -> Leaf.t -> t
  val of_list: Leaf.t list -> t

end = struct

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
end


module Map(Key: Map.Key)(Element: S) : sig

  include S with type Leaf.t = Element.Leaf.t

  val set: t -> key: Key.t -> data: Element.t -> t

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
    | Root
    | CallSite of {
        location: Location.t;
        callees: Interprocedural.Callable.t list;
      }
  [@@deriving compare, sexp]
end


module SourceSet = Set(TaintSources)
module SinkSet = Set(TaintSinks)


module ForwardTaint = struct
  include Map(TraceInfo)(SourceSet)

  let singleton leaf =
    set bottom ~key:TraceInfo.Root ~data:(SourceSet.singleton leaf)

  let of_list leaves =
    SourceSet.of_list leaves
    |> (fun elements -> set bottom ~key:TraceInfo.Root ~data:elements)
end


module BackwardTaint = struct
  include Map(TraceInfo)(SinkSet)

  let singleton leaf =
    set bottom ~key:TraceInfo.Root ~data:(SinkSet.singleton leaf)

  let of_list leaves =
    SinkSet.of_list leaves
    |> (fun elements -> set bottom ~key:TraceInfo.Root ~data:elements)
end


(** Used to infer which sources reach the exit points of a function. *)
module ForwardState =
  TaintAccessPathTree.Make
    (TaintAccessPathTree.WithChecks)
    (TaintAccessPath.Root)
    (ForwardTaint)


(** Used to infer which sinks are reached from parameters, as well as the
    taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState =
  TaintAccessPathTree.Make
    (TaintAccessPathTree.WithChecks)
    (TaintAccessPath.Root)
    (BackwardTaint)
