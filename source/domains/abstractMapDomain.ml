(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore
module List = Core_kernel.List
module MapPoly = Core_kernel.Map.Poly

module type KEY = sig
  include Map.OrderedType

  val name : string

  val pp : Format.formatter -> t -> unit

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


  type _ part += Key : Key.t part | KeyValue : (Key.t * Element.t) part

  module rec Base : (BASE with type t := Element.t Map.t) = MakeBase (Domain)

  and Domain : (S with type t = Element.t Map.t) = struct
    type t = Element.t Map.t

    let pp formatter map =
      match Map.to_alist map with
      | [] -> Format.fprintf formatter "{}"
      | [(key, value)] -> Format.fprintf formatter "{%a -> %a}" Key.pp key Element.pp value
      | pairs ->
          let pp_pair formatter (key, value) =
            Format.fprintf formatter "@,%a -> %a" Key.pp key Element.pp value
          in
          let pp_pairs formatter = List.iter ~f:(pp_pair formatter) in
          Format.fprintf formatter "{@[<v 2>%a@]@,}" pp_pairs pairs


    let show = Format.asprintf "%a" pp

    let bottom = Map.empty

    let is_bottom d = Map.is_empty d

    type _ part += Self : t part

    let join x y =
      let merge ~key:_ = function
        | `Both (a, b) -> Some (Element.join a b)
        | `Left e
        | `Right e ->
            Some e
      in
      if x == y || is_bottom y then
        x
      else
        Map.merge ~f:merge x y


    let less_or_equal ~left ~right =
      if left == right || is_bottom left then
        true
      else if is_bottom right then
        false
      else
        let find_witness ~key:_ ~data =
          match data with
          | `Both (left, right) ->
              if not (Element.less_or_equal ~left ~right) then raise_notrace Exit
          | `Left _ ->
              (* If absence_implicitly_maps_to_bottom, then left is not bottom, so in either case,
                 the relation does not hold. *)
              raise_notrace Exit
          | `Right _ ->
              (* An absent key is less than a present key in either case. *)
              ()
        in
        try
          Map.iter2 ~f:find_witness left right;
          true
        with
        | Exit -> false


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


    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f map ->
      match part, op with
      | Key, Map ->
          Map.fold
            map
            ~f:(fun ~key ~data result -> update ~key:(f key) ~data result)
            ~init:Map.empty
      | Key, Add -> update map ~key:f ~data:Element.bottom
      | Key, Filter -> Map.filter (fun key _ -> f key) map
      | Key, FilterMap ->
          Map.fold
            map
            ~f:(fun ~key ~data result ->
              match f key with
              | Some key -> update ~key ~data result
              | None -> result)
            ~init:Map.empty
      | Key, Expand ->
          Map.fold
            map
            ~f:(fun ~key ~data result ->
              List.fold (f key) ~init:result ~f:(fun result key -> update result ~key ~data))
            ~init:Map.empty
      | KeyValue, Map ->
          Map.fold
            map
            ~f:(fun ~key ~data result ->
              let key, data = f (key, data) in
              update ~key ~data result)
            ~init:Map.empty
      | KeyValue, Add ->
          let key, data = f in
          update map ~key ~data
      | KeyValue, Filter -> Map.filter (fun key data -> f (key, data)) map
      | KeyValue, FilterMap ->
          Map.fold
            map
            ~f:(fun ~key ~data result ->
              match f (key, data) with
              | Some (key, data) -> update ~key ~data result
              | None -> result)
            ~init:Map.empty
      | KeyValue, Expand ->
          Map.fold
            map
            ~f:(fun ~key ~data result ->
              List.fold
                (f (key, data))
                ~init:result
                ~f:(fun result (key, data) -> update result ~key ~data))
            ~init:Map.empty
      | _, Context (Key, op) ->
          if Key.absence_implicitly_maps_to_bottom then
            Map.fold
              map
              ~f:(fun ~key ~data result ->
                set ~key ~data:(Element.transform part op ~f:(f key) data) result)
              ~init:Map.empty
          else
            Map.mapi (fun key data -> Element.transform part op ~f:(f key) data) map
      | _, Context (KeyValue, op) ->
          if Key.absence_implicitly_maps_to_bottom then
            Map.fold
              map
              ~f:(fun ~key ~data result ->
                set ~key ~data:(Element.transform part op ~f:(f (key, data)) data) result)
              ~init:Map.empty
          else
            Map.mapi (fun key data -> Element.transform part op ~f:(f (key, data)) data) map
      | (Self | Key | KeyValue), _ -> Base.transform part op ~f map
      | _, op ->
          if Key.absence_implicitly_maps_to_bottom then
            Map.fold
              map
              ~f:(fun ~key ~data result ->
                set ~key ~data:(Element.transform part op ~f data) result)
              ~init:Map.empty
          else
            Map.map ~f:(Element.transform part op ~f) map


    let reduce
        : type a b f. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init map ->
      match part, op with
      | Key, Acc -> Map.fold ~f:(fun ~key ~data:_ result -> f key result) ~init map
      | Key, Exists -> init || Map.exists (fun key _ -> f key) map
      | KeyValue, Acc -> Map.fold ~f:(fun ~key ~data result -> f (key, data) result) ~init map
      | KeyValue, Exists -> init || Map.exists (fun key data -> f (key, data)) map
      | part, Context (Key, op) ->
          Map.fold
            ~f:(fun ~key ~data init -> Element.reduce part ~using:op ~f:(f key) ~init data)
            ~init
            map
      | part, Context (KeyValue, op) ->
          Map.fold
            ~f:(fun ~key ~data init -> Element.reduce part ~using:op ~f:(f (key, data)) ~init data)
            ~init
            map
      | (Self | Key | KeyValue), _ -> Base.reduce part ~using:op ~f map ~init
      | _, op ->
          Map.fold
            ~f:(fun ~key:_ ~data result -> Element.reduce part ~using:op ~f ~init:result data)
            ~init
            map


    let singleton key data = set bottom ~key ~data

    let update_partition ~key ~data = function
      | None -> singleton key data
      | Some existing -> update existing ~key ~data


    let partition
        : type a b f. a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) MapPoly.t
      =
     fun part op ~f map ->
      match part, op with
      | Key, By ->
          let partition_by_key ~key ~data partition =
            let partition_key = f key in
            MapPoly.update partition partition_key ~f:(update_partition ~key ~data)
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_key
      | KeyValue, By ->
          let partition_by_key ~key ~data partition =
            let partition_key = f (key, data) in
            MapPoly.update partition partition_key ~f:(update_partition ~key ~data)
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_key
      | Key, ByFilter ->
          let partition_by_key ~key ~data partition =
            match f key with
            | None -> partition
            | Some partition_key ->
                MapPoly.update partition partition_key ~f:(update_partition ~key ~data)
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_key
      | KeyValue, ByFilter ->
          let partition_by_key ~key ~data partition =
            match f (key, data) with
            | None -> partition
            | Some partition_key ->
                MapPoly.update partition partition_key ~f:(update_partition ~key ~data)
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_key
      | part, Context (Key, op) ->
          let partition_by_elements ~key ~data partition =
            let element_partition = Element.partition part op ~f:(f key) data in
            MapPoly.fold
              element_partition
              ~init:partition
              ~f:(fun ~key:partition_key ~data partition ->
                MapPoly.update partition partition_key ~f:(update_partition ~key ~data))
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_elements
      | part, Context (KeyValue, op) ->
          let partition_by_elements ~key ~data partition =
            let element_partition = Element.partition part op ~f:(f (key, data)) data in
            MapPoly.fold
              element_partition
              ~init:partition
              ~f:(fun ~key:partition_key ~data partition ->
                MapPoly.update partition partition_key ~f:(update_partition ~key ~data))
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_elements
      | (Key | KeyValue | Self), _ -> Base.partition part op ~f map
      | _, op ->
          let partition_by_elements ~key ~data partition =
            let element_partition = Element.partition part op ~f data in
            MapPoly.fold
              element_partition
              ~init:partition
              ~f:(fun ~key:partition_key ~data partition ->
                MapPoly.update partition partition_key ~f:(update_partition ~key ~data))
          in
          Map.fold map ~init:MapPoly.empty ~f:partition_by_elements


    let introspect (type a) (op : a introspect) : a =
      match op with
      | GetParts f ->
          f#report Self;
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
          | _ -> Element.introspect op)


    let subtract to_remove ~from =
      let subtract_element ~key ~data result =
        match data with
        | `Both (to_remove, from) ->
            let difference = Element.subtract to_remove ~from in
            if Element.is_bottom difference then
              result
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


    let meet = Base.meet

    let fold = Base.fold

    let apply = Base.apply
  end

  let to_alist = Map.to_alist

  let of_list l = List.fold_left l ~f:(fun acc (key, data) -> set ~key ~data acc) ~init:Map.empty

  let update map key ~f =
    let data = Map.find map key |> f in
    set map ~key ~data


  let get_opt = Map.find_opt

  let get key map =
    match Map.find_opt key map with
    | Some d -> d
    | None -> Element.bottom


  let remove k m = Map.remove m k

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
