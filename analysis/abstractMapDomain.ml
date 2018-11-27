(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open AbstractDomain


module type KEY = sig
  include Map.Key
  val show: t -> string
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

  let update map ~key ~data =
    let update = function
      | None -> data
      | Some existing -> Element.join existing data
    in
    if Key.absence_implicitly_maps_to_bottom && Element.is_bottom data then
      map
    else
      Map.update map key ~f:update

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

  let subtract to_remove ~from =
    let subtract_element ~key ~data result =
      match data with
      | `Both (to_remove, from) ->
          let difference = Element.subtract to_remove ~from in
          set result ~key ~data:difference
      | `Left _ ->
          result
      | `Right keep ->
          set result ~key ~data:keep
    in
    Map.fold2 to_remove from ~f:subtract_element ~init:bottom

  let show map =
    let show_pair (key, value) =
      Format.sprintf "%s -> %s" (Key.show key) (Element.show value)
    in
    Map.to_alist map
    |> List.map ~f:show_pair
    |> String.concat ~sep:"\n"

  let pp formatter map =
    Format.fprintf
      formatter
      "%s"
      (show map)

  type _ part +=
    | Key: Key.t part
    | KeyValue: (Key.t * Element.t) part

  let transform (type a) (part: a part) ~(f: a -> a) (map: t) : t =
    match part with
    | Key ->
        Map.fold
          map
          ~f:(fun ~key ~data result -> set ~key:(f key) ~data result)
          ~init:Map.empty
    | KeyValue ->
        Map.fold
          map
          ~f:(fun ~key ~data result ->
              let key, data = f (key, data) in
              set ~key ~data result)
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
    | KeyValue ->
        Map.fold ~f:(fun ~key ~data result -> f result (key, data)) ~init map
    | _ ->
        Map.fold
          map
          ~f:(fun ~key:_ ~data result -> Element.fold part ~f ~init:result data)
          ~init

  let partition (type a b) (part: a part) ~(f: a -> b) (map: t)
    : (b, t) Core.Map.Poly.t =
    let update ~key ~data = function
      | None ->
          singleton key data
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
    | KeyValue ->
        let partition_by_key ~key ~data partition =
          let partition_key = f (key, data) in
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

  let rec create parts =
    let rec split_into_alist parts =
      match parts with
      | [] ->
          [], []
      | Part (Key, value) :: rest ->
          let elements, pairs = split_into_alist rest in
          let element_value = Element.create (List.rev elements) in
          [], ((value : Key.t), element_value) :: pairs
      | Part (KeyValue, value) :: rest ->
          let elements, pairs = split_into_alist rest in
          elements, ((fst value : Key.t), (snd value : Element.t)) :: pairs
      | element_part :: rest ->
          let elements, pairs = split_into_alist rest in
          element_part :: elements, pairs
    in
    let common_elements, key_values = split_into_alist parts in
    let () =
      if key_values = [] && common_elements <> [] then failwith "No keys specified"
    in
    let common = Element.create (List.rev common_elements) in
    List.map key_values ~f:(fun (key, value) -> (key, Element.join common value))
    |> List.fold ~init:bottom ~f:(fun map (key, data) -> update map ~key ~data)

  let update map key ~f =
    let data =
      Map.find map key
      |> f
    in
    set map ~key ~data

end
