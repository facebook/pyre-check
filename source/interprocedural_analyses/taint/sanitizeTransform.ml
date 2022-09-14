(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type S = sig
  type elt

  type set [@@deriving compare, eq, hash, sexp, show]

  include Abstract.Domain.S with type t = set

  type t = set [@@deriving compare, eq, hash, sexp, show]

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val diff : t -> t -> t

  val singleton : elt -> t

  val all : t

  val is_all : t -> bool

  val of_list : elt list -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val to_json : t -> Yojson.Safe.t option
end

module type TAINT_KIND = sig
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  val name : string

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val show_kind : t -> string
end

module Source : sig
  include TAINT_KIND
end = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let name = "Source"

  let pp formatter (Named taint_kind) = Format.fprintf formatter "Not%s[%s]" name taint_kind

  let show = Format.asprintf "%a" pp

  let show_kind (Named taint_kind) = Format.sprintf "%s" taint_kind
end

module Sink : sig
  include TAINT_KIND
end = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let name = "Sink"

  let pp formatter (Named taint_kind) = Format.fprintf formatter "Not%s[%s]" name taint_kind

  let show = Format.asprintf "%a" pp

  let show_kind (Named taint_kind) = Format.sprintf "%s" taint_kind
end

module MakeSet (Kind : TAINT_KIND) = struct
  module Set = Data_structures.SerializableSet.Make (Kind)

  type set =
    (* Represent all taint kinds to sanitize. We should not create a source or sink kind that
       contains `All`. *)
    | All
    (* Represent a set of taint kinds to sanitize. *)
    | Specific of Set.t
  [@@deriving compare, eq, hash, sexp, show]

  type elt = Kind.t

  include Abstract.SimpleDomain.Make (struct
    type t = set [@@deriving show]

    let name = Format.sprintf "sanitize %ss" Kind.name

    let bottom = Specific Set.empty

    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        match left, right with
        | All, All -> true
        | All, Specific _ -> false
        | Specific _, All -> true
        | Specific left, Specific right -> Set.subset left right


    let join left right =
      if phys_equal left right then
        left
      else
        match left, right with
        | All, _
        | _, All ->
            All
        | Specific left, Specific right -> Specific (Set.union left right)


    let meet a b = if less_or_equal ~left:b ~right:a then b else a
  end)

  type t = set [@@deriving compare, eq, hash, sexp]

  let empty = bottom

  let is_empty = is_bottom

  let is_all = function
    | All -> true
    | Specific _ -> false


  let mem element set =
    match set with
    | All -> true
    | Specific set -> Set.mem element set


  let diff left right =
    match left, right with
    | All, Specific _ ->
        (* Use an over-approximation because this cannot be precisely computed. *) All
    | All, All
    | Specific _, All ->
        Specific Set.empty
    | Specific left, Specific right -> Specific (Set.diff left right)


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

  let to_json set =
    match set with
    | All -> Some (`String "All")
    | Specific set ->
        if Set.is_empty set then
          None
        else
          Some
            (let to_string name = `String name in
             `List (set |> Set.elements |> List.map ~f:Kind.show_kind |> List.map ~f:to_string))
end

module SourceSet = MakeSet (Source)
module SinkSet = MakeSet (Sink)

type t =
  | Source of Source.t
  | Sink of Sink.t
