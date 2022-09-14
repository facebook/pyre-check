(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type S = sig
  type elt

  type t [@@deriving compare, eq, hash, sexp, show]

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val union : t -> t -> t

  val diff : t -> t -> t

  val subset : t -> t -> bool

  val singleton : elt -> t

  val all : t

  val is_all : t -> bool

  val of_list : elt list -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module type TAINT_KIND = sig
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  val name : string

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end

module Source : sig
  include TAINT_KIND
end = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let name = "Source"

  let pp formatter (Named taint_kind) = Format.fprintf formatter "Not%s[%s]" name taint_kind

  let show = Format.asprintf "%a" pp
end

module Sink : sig
  include TAINT_KIND
end = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let name = "Sink"

  let pp formatter (Named taint_kind) = Format.fprintf formatter "Not%s[%s]" name taint_kind

  let show = Format.asprintf "%a" pp
end

module MakeSet (Kind : TAINT_KIND) = struct
  module Set = Data_structures.SerializableSet.Make (Kind)

  type t =
    (* Represent all taint kinds to sanitize. We should not create a source or sink kind that
       contains `All`. *)
    | All
    (* Represent a set of taint kinds to sanitize. *)
    | Specific of Set.t
  [@@deriving compare, eq, hash, sexp]

  type elt = Kind.t

  let empty = Specific Set.empty

  let is_empty = function
    | All -> false
    | Specific set -> Set.is_empty set


  let is_all = function
    | All -> true
    | Specific _ -> false


  let mem element set =
    match set with
    | All -> true
    | Specific set -> Set.mem element set


  let union left right =
    match left, right with
    | All, _ -> All
    | _, All -> All
    | Specific left, Specific right -> Specific (Set.union left right)


  let diff left right =
    match left, right with
    | All, Specific _ ->
        (* Use an over-approximation because this cannot be precisely computed. *) All
    | All, All
    | Specific _, All ->
        Specific Set.empty
    | Specific left, Specific right -> Specific (Set.diff left right)


  let subset left right =
    match left, right with
    | All, Specific _ -> false
    | All, All -> true
    | Specific _, All -> true
    | Specific left, Specific right -> Set.subset left right


  let singleton element = Specific (Set.singleton element)

  let all = All

  let of_list list = Specific (Set.of_list list)

  let fold f set init =
    match set with
    | All -> failwith "Unexpected to fold over all elements in `All`"
    | Specific set -> Set.fold f set init


  let pp formatter = function
    | All -> Format.fprintf formatter "SanitizeAll%s" Kind.name
    | Specific set ->
        if not (Set.is_empty set) then
          Format.fprintf
            formatter
            "%s"
            (Set.elements set |> List.map ~f:Kind.show |> String.concat ~sep:":")


  let show = Format.asprintf "%a" pp
end

module SourceSet = MakeSet (Source)
module SinkSet = MakeSet (Sink)

type t =
  | Source of Source.t
  | Sink of Sink.t
