(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open AbstractDomain


module type KEY = sig
  include Map.Key

  val absence_implicitly_maps_to_bottom: bool
end


module Make(Key: KEY)(Element : AbstractDomain.S) = struct
  module Map = struct
    module ElementMap = Map.Make(Key)
    include ElementMap.Tree
  end

  type t = Element.t Map.t
  [@@deriving sexp]

  let bottom = Map.empty

  let is_bottom d =
    Map.is_empty d

  let set map ~key ~data =
    if Key.absence_implicitly_maps_to_bottom && Element.is_bottom data then
      Map.remove map key
    else
      Map.set map ~key ~data

  let singleton key data =
    set bottom ~key ~data

  let to_alist = Map.to_alist
  let find = Map.find

  let join x y =
    let merge ~key:_ = function
      | `Both (a, b) -> Some (Element.join a b)
      | `Left e | `Right e -> Some e
    in
    Map.merge ~f:merge x y

  let widen ~iteration ~previous ~next =
    let merge ~key:_ = function
      | `Both (previous, next) -> Some (Element.widen ~iteration ~previous ~next)
      | `Left e | `Right e -> Some e
    in
    Map.merge ~f:merge previous next

  let less_or_equal ~left ~right =
    let find_witness ~key:_ ~data =
      match data with
      | `Both (left, right) ->
          if not (Element.less_or_equal ~left ~right) then raise Exit
      | `Left _ ->
          (* If absence_implicitly_maps_to_bottom, then left is not bottom, so
             in either case, the relation does not hold. *)
          raise Exit
      | `Right _ ->
          (* An absent key is less than a present key in either case. *)
          ()
    in
    try
      Map.iter2 ~f:find_witness left right;
      true
    with
    | Exit -> false

  let show map =
    Sexp.to_string [%message (map: Element.t Map.t)]

  type _ part +=
    | Key: Key.t part

  let transform (type a) (part: a part) ~(f: a -> a) (map: t) : t =
    match part with
    | Key ->
        Map.fold
          map
          ~f:(fun ~key ~data result -> set ~key:(f key) ~data result)
          ~init:Map.empty
    | _ ->
        if Key.absence_implicitly_maps_to_bottom then
          Map.fold
            map
            ~f:(fun ~key ~data result -> set ~key ~data:(Element.transform part ~f data) result)
            ~init:Map.empty
        else
          Map.map ~f:(Element.transform part ~f) map

  let fold (type a b) (part: a part) ~(f: b -> a -> b) ~(init: b) (map: t) : b =
    match part with
    | Key ->
        Map.fold ~f:(fun ~key ~data:_ result -> f result key) ~init map
    | _ ->
        Map.fold
          map
          ~f:(fun ~key:_ ~data result -> Element.fold part ~f ~init:result data)
          ~init

  let partition (type a b) (part: a part) ~(f: a -> b) (map: t)
    : (b, t) Core.Map.Poly.t =
    let update ~key ~data = function
      | None ->
          Map.singleton key data
      | Some existing ->
          set existing ~key ~data
    in
    match part with
    | Key ->
        let partition_by_key ~key ~data partition =
          let partition_key = f key in
          Core.Map.Poly.update partition partition_key ~f:(update ~key ~data)
        in
        Map.fold map ~init:Core.Map.Poly.empty ~f:partition_by_key
    | _ ->
        let partition_by_elements ~key ~data partition =
          let element_partition = Element.partition part ~f data in
          Core.Map.Poly.fold
            element_partition
            ~init:partition
            ~f:(fun ~key:partition_key ~data partition ->
                Core.Map.Poly.update partition partition_key ~f:(update ~key ~data))
        in
        Map.fold map ~init:Core.Map.Poly.empty ~f:partition_by_elements
end
