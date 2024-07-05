(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore
module List = Core.List
module Either = Core.Either

module type INNER = sig
  include S

  val name : string
end

module Make (Inner : INNER) = struct
  module rec Base : (BASE with type t := Inner.t) = MakeBase (Domain)

  and Domain : (S with type t = Inner.t) = struct
    include Inner

    type _ part += Self : t part

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f d ->
      match part, op with
      | Self, _ -> Base.transform part op ~f d
      | _, Context (Self, _) -> Base.transform part op ~f d
      | _ -> Inner.transform part op ~f d


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init d ->
      match part, op with
      | Self, _ -> Base.reduce part ~using:op ~f ~init d
      | _, Context (Self, _) -> Base.reduce part ~using:op ~f ~init d
      | _ -> Inner.reduce part ~using:op ~f ~init d


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core.Map.Poly.t
      =
     fun part op ~f d ->
      match part, op with
      | Self, _ -> Base.partition part op ~f d
      | _, Context (Self, _) -> Base.partition part op ~f d
      | _ -> Inner.partition part op ~f d


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          Inner.introspect op
      | Structure ->
          let range = Inner.introspect op in
          Format.sprintf "Wrapper(%s):" Inner.name :: List.map ~f:(fun s -> "  " ^ s) range
      | Name Self -> Format.sprintf "Wrapper(%s).Self" Inner.name
      | Name _ -> Inner.introspect op


    let create parts =
      let partition_by_part = function
        | Part (Self, value) -> Either.First (value : Inner.t)
        | part -> Either.Second part
      in
      let values, parts = List.partition_map ~f:partition_by_part parts in
      List.fold ~init:(Inner.create parts) ~f:Inner.join values


    let fold = Base.fold

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
