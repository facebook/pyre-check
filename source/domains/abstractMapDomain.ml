(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore
module List = Core_kernel.List
module String = Core_kernel.String
module MapPoly = Core_kernel.Map.Poly

module type KEY = sig
  include Map.OrderedType

  val name : string

  val show : t -> string

  val absence_implicitly_maps_to_bottom : bool
end

module Make (Key : KEY) (Element : AbstractDomainCore.S) = struct
  module Map = struct
    module Map = Map.Make (Key)
    include Map

    let set map ~key ~data = Map.add key data map

    let remove map key = Map.remove key map

    let update map key ~f = set map ~key ~data:(f (Map.find_opt key map))

    let to_alist = Map.bindings

    let merge left right ~f =
      let f key left right =
        match left, right with
        | Some left, None -> f ~key (`Left left)
        | None, Some right -> f ~key (`Right right)
        | Some left, Some right -> f ~key (`Both (left, right))
        | None, None -> None
      in
      Map.merge f left right


    let fold ~f map ~init = Map.fold (fun key data acc -> f ~key ~data acc) map init

    let fold2 ~f ~init left right =
      let combine _key left right =
        match left, right with
        | Some left, None -> Some (`Left left)
        | None, Some right -> Some (`Right right)
        | Some left, Some right -> Some (`Both (left, right))
        | None, None -> None
      in
      Map.merge combine left right |> fold ~f ~init


    let iter2 ~f left right =
      ignore (fold2 left right ~f:(fun ~key ~data () -> f ~key ~data) ~init:())


    let map ~f map = Map.map f map

    let find map key = Map.find_opt key map
  end

  type t = Element.t Map.t

  let bottom = Map.empty

  let is_bottom d = Map.is_empty d

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


  let singleton key data = set bottom ~key ~data

  let to_alist = Map.to_alist

  let join x y =
    let merge ~key:_ = function
      | `Both (a, b) -> Some (Element.join a b)
      | `Left e
      | `Right e ->
          Some e
    in
    if x == y then
      x
    else
      Map.merge ~f:merge x y


  let widen ~iteration ~prev ~next =
    let merge ~key:_ = function
      | `Both (prev, next) -> Some (Element.widen ~iteration ~prev ~next)
      | `Left e
      | `Right e ->
          Some e
    in
    if prev == next then
      prev
    else
      Map.merge ~f:merge prev next


  let less_or_equal ~left ~right =
    if left == right || is_bottom left then
      true
    else if is_bottom right then
      false
    else
      let find_witness ~key:_ ~data =
        match data with
        | `Both (left, right) -> if not (Element.less_or_equal ~left ~right) then raise Exit
        | `Left _ ->
            (* If absence_implicitly_maps_to_bottom, then left is not bottom, so in either case, the
               relation does not hold. *)
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
          if Element.is_bottom difference then
            Map.remove result key
          else
            Map.set result ~key ~data:difference
      | `Left _ -> result
      | `Right keep -> set result ~key ~data:keep
    in
    if from == to_remove then
      bottom
    else if is_bottom from then
      bottom
    else if is_bottom to_remove then
      from
    else
      Map.fold2 to_remove from ~f:subtract_element ~init:bottom


  let show map =
    let show_pair (key, value) = Format.sprintf "%s -> %s" (Key.show key) (Element.show value) in
    Map.to_alist map |> List.map ~f:show_pair |> String.concat ~sep:"\n"


  let pp formatter map = Format.fprintf formatter "%s" (show map)

  module CommonArg = struct
    type nonrec t = t

    let bottom = bottom

    let join = join

    let less_or_equal = less_or_equal
  end

  module C = Common (CommonArg)

  type _ part += Self = C.Self | Key : Key.t part | KeyValue : (Key.t * Element.t) part

  let rec transform : type a. a part -> a transform -> t -> t =
   fun part t map ->
    match part, t with
    | Key, Map f ->
        Map.fold map ~f:(fun ~key ~data result -> update ~key:(f key) ~data result) ~init:Map.empty
    | Key, Add key -> update map ~key ~data:Element.bottom
    | Key, Filter f -> Map.filter (fun key _ -> f key) map
    | Key, Expand f ->
        Map.fold
          map
          ~f:(fun ~key ~data result ->
            List.fold (f key) ~init:result ~f:(fun result key -> update result ~key ~data))
          ~init:Map.empty
    | KeyValue, Map f ->
        Map.fold
          map
          ~f:(fun ~key ~data result ->
            let key, data = f (key, data) in
            update ~key ~data result)
          ~init:Map.empty
    | KeyValue, Add (key, data) -> update map ~key ~data
    | KeyValue, Filter f -> Map.filter (fun key data -> f (key, data)) map
    | KeyValue, Expand f ->
        Map.fold
          map
          ~f:(fun ~key ~data result ->
            List.fold
              (f (key, data))
              ~init:result
              ~f:(fun result (key, data) -> update result ~key ~data))
          ~init:Map.empty
    | Key, _ -> C.transform transformer part t map
    | KeyValue, _ -> C.transform transformer part t map
    | C.Self, _ -> C.transform transformer part t map
    | _ ->
        if Key.absence_implicitly_maps_to_bottom then
          Map.fold
            map
            ~f:(fun ~key ~data result -> set ~key ~data:(Element.transform part t data) result)
            ~init:Map.empty
        else
          Map.map ~f:(Element.transform part t) map


  and transformer (T (part, t)) (d : t) : t = transform part t d

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (map : t) : b =
    match part with
    | Key -> Map.fold ~f:(fun ~key ~data:_ result -> f key result) ~init map
    | KeyValue -> Map.fold ~f:(fun ~key ~data result -> f (key, data) result) ~init map
    | C.Self -> C.fold part ~f ~init map
    | _ -> Map.fold map ~f:(fun ~key:_ ~data result -> Element.fold part ~f ~init:result data) ~init


  let partition (type a b) (part : a part) ~(f : a -> b option) (map : t) : (b, t) MapPoly.t =
    let update ~key ~data = function
      | None -> singleton key data
      | Some existing -> set existing ~key ~data
    in
    match part with
    | Key ->
        let partition_by_key ~key ~data partition =
          match f key with
          | None -> partition
          | Some partition_key -> MapPoly.update partition partition_key ~f:(update ~key ~data)
        in
        Map.fold map ~init:MapPoly.empty ~f:partition_by_key
    | KeyValue ->
        let partition_by_key ~key ~data partition =
          match f (key, data) with
          | None -> partition
          | Some partition_key -> MapPoly.update partition partition_key ~f:(update ~key ~data)
        in
        Map.fold map ~init:MapPoly.empty ~f:partition_by_key
    | C.Self -> C.partition part ~f map
    | _ ->
        let partition_by_elements ~key ~data partition =
          let element_partition = Element.partition part ~f data in
          MapPoly.fold
            element_partition
            ~init:partition
            ~f:(fun ~key:partition_key ~data partition ->
              MapPoly.update partition partition_key ~f:(update ~key ~data))
        in
        Map.fold map ~init:MapPoly.empty ~f:partition_by_elements


  let create parts =
    let rec split_into_alist parts =
      match parts with
      | [] -> [], []
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
      if key_values = [] && common_elements <> [] then
        failwith "No keys specified"
    in
    let common = Element.create (List.rev common_elements) in
    List.map key_values ~f:(fun (key, value) -> key, Element.join common value)
    |> List.fold ~init:bottom ~f:(fun map (key, data) -> update map ~key ~data)


  let update map key ~f =
    let data = Map.find map key |> f in
    set map ~key ~data


  let get = Map.find_opt

  let introspect (type a) (op : a introspect) : a =
    match op with
    | GetParts f ->
        f#report C.Self;
        f#report Key;
        f#report KeyValue;
        Element.introspect op
    | Structure ->
        let range = Element.introspect op in
        let strict =
          if Key.absence_implicitly_maps_to_bottom then
            "(strict)"
          else
            ""
        in
        Format.sprintf "%s -> %s" Key.name strict :: List.map ~f:(fun s -> "  " ^ s) range
    | Name part -> (
        match part with
        | Key -> Format.sprintf "Map(%s).Key" Key.name
        | KeyValue -> Format.sprintf "Map(%s).KeyValue" Key.name
        | Self -> Format.sprintf "Map(%s).Self" Key.name
        | _ -> Element.introspect op )


  let meet = C.meet
end
