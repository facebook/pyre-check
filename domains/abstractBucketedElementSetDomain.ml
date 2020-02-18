(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open AbstractDomainCore

module type BUCKETED_ELEMENT = sig
  include AbstractElementSetDomain.ELEMENT

  type bucket

  val bucket : t -> bucket

  val show_bucket : bucket -> string

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

        let show = Element.show_bucket

        let absence_implicitly_maps_to_bottom = true

        let name = "bucket"
      end)
      (Set)

  type t = Map.t

  let bottom = Map.bottom

  let is_bottom = Map.is_bottom

  let add to_add buckets =
    let update = function
      | None -> Set.singleton to_add
      | Some existing -> Set.add to_add existing
    in
    let bucket = Element.bucket to_add in
    Map.update buckets bucket ~f:update


  let join = Map.join

  let elements buckets = Map.fold Set.Element ~f:List.cons ~init:[] buckets

  let of_list elements = ListLabels.fold_left ~f:(fun set elt -> add elt set) elements ~init:bottom

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

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self | Element = Set.Element | Set : Element.t list part

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (buckets : t) : b =
    match part with
    | Set ->
        (* Present the flattened set *)
        f (elements buckets) init
    | C.Self -> C.fold part ~f ~init buckets
    | _ -> Map.fold part ~f ~init buckets


  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t buckets ->
    match part, t with
    | Set, Map f ->
        (* Present the flattened set *)
        f (elements buckets) |> of_list
    | Set, Add l -> ListLabels.fold_left l ~f:(fun result e -> add e result) ~init:buckets
    | Set, Filter f ->
        if f (elements buckets) then
          buckets
        else
          bottom
    | Set, _ -> C.transform transformer part t buckets
    | C.Self, _ -> C.transform transformer part t buckets
    | _ -> Map.transform part t buckets


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let partition (type a b) (part : a part) ~(f : a -> b option) (buckets : t) =
    match part with
    | Set -> (
        match f (elements buckets) with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key buckets )
    | C.Self -> C.partition part ~f buckets
    | _ -> Map.partition part ~f buckets


  let create parts =
    let create_part so_far (Part (part, value)) =
      match part with
      | Set -> join so_far (of_list value)
      | Set.Element ->
          join
            so_far
            (Map.create [Part (Map.KeyValue, (Element.bucket value, Set.singleton value))])
      | _ -> C.create part value so_far
    in
    ListLabels.fold_left ~f:create_part parts ~init:bottom


  let singleton element =
    Map.set Map.bottom ~key:(Element.bucket element) ~data:(Set.singleton element)


  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Element;
        f#report Set;
        Map.introspect op
    | Structure -> Map.introspect op
    | Name part -> (
        match part with
        | Element -> Format.sprintf "BucketedSet(%s).Element" Element.name
        | Set -> Format.sprintf "BucketedSet(%s).Set" Element.name
        | Self -> Format.sprintf "BucketedSet(%s).Self" Element.name
        | _ -> C.introspect op )
end
