(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type BUCKETED_ELEMENT = sig
  include AbstractElementSetDomain.ELEMENT

  type bucket

  val bucket : t -> bucket

  val pp_bucket : Format.formatter -> bucket -> unit

  val compare_bucket : bucket -> bucket -> int
end

(** A set of abstract elements where elements can be related **)
module Make (Element : BUCKETED_ELEMENT) = struct
  module Set = AbstractElementSetDomain.Make (Element)

  module Map =
    AbstractMapDomain.Make
      (struct
        type t = Element.bucket

        let compare = Element.compare_bucket

        let pp = Element.pp_bucket

        let absence_implicitly_maps_to_bottom = true

        let name = "bucket"
      end)
      (Set)

  let add to_add buckets =
    let update = function
      | None -> Set.singleton to_add
      | Some existing -> Set.add to_add existing
    in
    let bucket = Element.bucket to_add in
    Map.update buckets bucket ~f:update


  let elements buckets = Map.fold Set.Element ~f:List.cons ~init:[] buckets

  let of_list elements =
    ListLabels.fold_left ~f:(fun set elt -> add elt set) elements ~init:Map.bottom


  (* Note: we alias all the parts to the underlying set, but we handle the Set part here so we can
     provider a non-bucketed view *)
  type _ part += Element = Set.Element

  module rec Base : (BASE with type t := Map.t) = MakeBase (Domain)

  and Domain : (S with type t = Map.t) = struct
    type t = Map.t

    let bottom = Map.bottom

    let is_bottom = Map.is_bottom

    type _ part += Self : t part

    let join = Map.join

    let widen ~iteration ~prev ~next =
      Map.widen ~iteration ~prev ~next |> elements |> Element.widen |> of_list


    let less_or_equal = Map.less_or_equal

    let subtract = Map.subtract

    let show buckets =
      elements buckets
      |> ListLabels.map ~f:Element.show
      |> String.concat ", "
      |> Format.sprintf "[%s]"


    let pp formatter map = Format.fprintf formatter "%s" (show map)

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f buckets ->
      match part with
      | Self -> Base.transform part op ~f buckets
      | _ -> Map.transform part op ~f buckets


    let reduce
        : type a b f. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init buckets ->
      match part with
      | Self -> Base.reduce part ~using:op ~f ~init buckets
      | _ -> Map.reduce part ~using:op ~f ~init buckets


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f buckets ->
      match part with
      | Self -> Base.partition part op ~f buckets
      | _ -> Map.partition part op ~f buckets


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
          Map.introspect op
      | Structure -> Map.introspect op
      | Name part -> (
          match part with
          | Element -> Format.sprintf "BucketedSet(%s).Element" Element.name
          | Self -> Format.sprintf "BucketedSet(%s).Self" Element.name
          | _ -> Base.introspect op)


    let create parts =
      let create_part so_far (Part (part, value)) =
        match part with
        | Element ->
            join
              so_far
              (Map.create [Part (Map.KeyValue, (Element.bucket value, Set.singleton value))])
        | _ -> Base.create part value so_far
      in
      ListLabels.fold_left ~f:create_part parts ~init:bottom


    let meet = Base.meet

    let fold = Base.fold

    let apply = Base.apply
  end

  let singleton element =
    Map.set Map.bottom ~key:(Element.bucket element) ~data:(Set.singleton element)


  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
