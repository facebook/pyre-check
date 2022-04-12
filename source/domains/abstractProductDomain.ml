(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open AbstractDomainCore

module type PRODUCT_CONFIG = sig
  type 'a slot

  (* Cardinality of type 'a slot, i.e., distinct constants *)
  val slots : int

  (* Name of the product slot, e.g., "Left", "Right". Must be distinct for each slot. *)
  val slot_name : 'a slot -> string

  (* The abstract domain of values in a given product slot. E.g., let slot_domain (type a) (a slot)
     = | Left -> (module LeftDomain : AbstractDomainCore.S with type t = a) ... *)
  val slot_domain : 'a slot -> 'a abstract_domain

  (* If a slot is strict, then the entire product is bottom when that slot is bottom. *)
  val strict : 'a slot -> bool
end

module Make (Config : PRODUCT_CONFIG) = struct
  type element = Element : 'a -> element [@@unbox]

  type product = element array

  module IntMap = Map.Make (Int)

  type abstract_slot = Slot : 'a Config.slot -> abstract_slot [@@unbox]

  let slots =
    let rec get_slots i sofar =
      if i < Config.slots then
        let slot = Slot (Obj.magic i : 'a Config.slot) in
        get_slots (i + 1) (slot :: sofar)
      else
        List.rev sofar
    in
    get_slots 0 [] |> Array.of_list


  let strict_slots =
    let filter_strict (Slot slot) = Config.strict slot in
    Array.to_list slots |> ListLabels.filter ~f:filter_strict |> Array.of_list


  (* The route map indicates for each part under a product element which slot the element is in *)
  let route_map : int list IntMap.t =
    let map = ref IntMap.empty in
    let gather (route : int) (type a) (part : a part) =
      let add_route = function
        | None -> Some [route]
        | Some routes -> Some (route :: routes)
      in
      map := IntMap.update (part_id part) add_route !map
    in
    Array.iteri
      (fun route (Slot slot) ->
        let module D = (val Config.slot_domain slot) in
        D.introspect
          (GetParts
             (object
                method report : 'a. 'a part -> unit = gather route
             end)))
      slots;
    !map


  let get_route (type a) (part : a part) =
    match IntMap.find_opt (part_id part) route_map with
    | Some [slot_index] -> slot_index
    | Some slot_indices ->
        let name_from_slot_index index =
          let (Slot slot) = slots.(index) in
          Config.slot_name slot
        in
        Format.sprintf
          "Part %s is present in multiple slots of a product: %s"
          (part_name part)
          (slot_indices |> List.map name_from_slot_index |> String.concat ", ")
        |> failwith
    | None -> Format.sprintf "No route to part %s" (part_name part) |> failwith


  let bottom =
    let get_bottom (Slot slot) =
      let module D = (val Config.slot_domain slot) in
      Element D.bottom
    in
    Array.map get_bottom slots


  let slot_number (type a) (slot : a Config.slot) : int =
    if Obj.repr slot |> Obj.is_int then (
      let i = Obj.magic slot in
      assert (i >= 0 && i < Array.length slots);
      i)
    else
      failwith "slots must be a datatype with 0-ary constructors"


  let get (type a) (slot : a Config.slot) (product : product) : a =
    let i = slot_number slot in
    match product.(i) with
    | Element value -> Obj.magic value


  exception Strict

  let make_strict result =
    let check_strict (Slot slot) =
      let module D = (val Config.slot_domain slot) in
      let value = get slot result in
      if D.is_bottom value then raise_notrace Strict
    in
    try
      Array.iter check_strict strict_slots;
      result
    with
    | Strict -> bottom


  let update (type a) (slot : a Config.slot) (value : a) (product : product) =
    let i = slot_number slot in
    match product.(i) with
    | Element old_value ->
        if old_value == Obj.magic value then
          product
        else
          let module D = (val Config.slot_domain slot) in
          if Config.strict slot && D.is_bottom value then
            bottom
          else
            let result = Array.copy product in
            result.(i) <- Element value;
            (* Check existing strict slots are not bottom *)
            make_strict result


  module rec Base : (BASE with type t := product) = MakeBase (Domain)

  and Domain : (S with type t = product) = struct
    type t = product

    type _ part += Self : t part

    let bottom = bottom

    let is_bottom product =
      let is_bottom_slot (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        let v = get slot product in
        if not (D.is_bottom v) then raise_notrace Exit
      in
      if product == bottom then
        true
      else
        try
          Array.iter is_bottom_slot slots;
          true
        with
        | Exit -> false


    let join left right =
      let merge (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        let left = get slot left in
        let right = get slot right in
        Element (D.join left right)
      in
      if left == right || is_bottom right then
        left
      else
        Array.map merge slots


    let widen ~iteration ~prev ~next =
      let merge (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        let prev = get slot prev in
        let next = get slot next in
        Element (D.widen ~iteration ~prev ~next)
      in
      if prev == next then
        prev
      else
        Array.map merge slots


    let less_or_equal ~left ~right =
      let less_or_equal_slot (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        let left = get slot left in
        let right = get slot right in
        if not (D.less_or_equal ~left ~right) then raise_notrace Exit
      in
      try
        Array.iter less_or_equal_slot slots;
        true
      with
      | Exit -> false


    let meet left right =
      let merge (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        let left = get slot left in
        let right = get slot right in
        let result = D.meet left right in
        if Config.strict slot && D.is_bottom result then
          raise_notrace Strict
        else
          Element result
      in
      if left == right then
        left
      else
        try Array.map merge slots with
        | Strict -> bottom


    type strict_subtract =
      | SingleStrict : 'a Config.slot * 'a -> strict_subtract
      | AllBottom

    let subtract to_remove ~from =
      if to_remove == from then
        bottom
      else if is_bottom to_remove then
        from
      else if less_or_equal ~left:from ~right:to_remove then
        bottom
      else if
        (* Handle cases depending on strictness of slots. If all slots are non-strict, then
           subtraction is pointwise. *)
        Array.length strict_slots = 0
      then (* point-wise *)
        let nonbottom_slots = ref (Array.length from) in
        let sub (Slot slot) =
          let module D = (val Config.slot_domain slot) in
          let to_remove = get slot to_remove in
          let from = get slot from in
          let result = D.subtract to_remove ~from in
          if D.is_bottom result then decr nonbottom_slots;
          Element result
        in
        let result = Array.map sub slots in
        if !nonbottom_slots = 0 then
          bottom
        else
          result
      else
        (* If pointwise subtraction results in bottom in all but one strict slot, then we can keep
           that strict slot's subtraction and leave all other slots unchanged. *)
        let find_single_strict_slot so_far (Slot slot) =
          let module D = (val Config.slot_domain slot) in
          let to_remove = get slot to_remove in
          let from = get slot from in
          let result = D.subtract to_remove ~from in
          if D.is_bottom result then
            so_far
          else if Config.strict slot then
            match so_far with
            | AllBottom -> SingleStrict (slot, result)
            | _ ->
                (* multiple slots are non-bottom *)
                raise_notrace Exit
          else (* non-strict slot non-bottom *)
            raise_notrace Exit
        in
        try
          match Array.fold_left find_single_strict_slot AllBottom slots with
          | SingleStrict (slot, new_value) -> update slot new_value from
          | AllBottom -> bottom
        with
        | Exit -> from


    let show product =
      let show_element (Slot slot) =
        let slot_name = Config.slot_name slot in
        let module D = (val Config.slot_domain slot) in
        let value = get slot product in
        if D.is_bottom value then
          None
        else
          Some (Format.sprintf "%s: %s" slot_name (D.show value))
      in
      if is_bottom product then
        "<bottom>"
      else
        Array.map show_element slots
        |> Array.to_list
        |> List.filter_map Core_kernel.Fn.id
        |> String.concat ", "


    let pp formatter map = Format.fprintf formatter "%s" (show map)

    let transform : type a f. a part -> ([ `Transform ], a, f, _) operation -> f:f -> t -> t =
     fun part op ~f product ->
      match part, op with
      | Self, _ -> Base.transform part op ~f product
      | _, Context (Self, _) -> Base.transform part op ~f product
      | _ ->
          let transform (Slot slot) =
            let value = get slot product in
            let module D = (val Config.slot_domain slot) in
            let new_value = D.transform part op ~f value in
            update slot new_value product
          in
          let route = get_route part in
          transform slots.(route)


    let reduce
        : type a f b. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> t -> b
      =
     fun part ~using:op ~f ~init product ->
      match part, op with
      | Self, _ -> Base.reduce part ~using:op ~f ~init product
      | _, Context (Self, _) -> Base.reduce part ~using:op ~f ~init product
      | _ ->
          let fold (Slot slot) =
            let value = get slot product in
            let module D = (val Config.slot_domain slot) in
            D.reduce part ~using:op ~f ~init value
          in
          let route = get_route part in
          fold slots.(route)


    let partition
        : type a f b.
          a part -> ([ `Partition ], a, f, b) operation -> f:f -> t -> (b, t) Core_kernel.Map.Poly.t
      =
     fun part op ~f product ->
      match part, op with
      | Self, _ -> Base.partition part op ~f product
      | _, Context (Self, _) -> Base.partition part op ~f product
      | _ ->
          let partition (Slot slot) : (b, t) Core_kernel.Map.Poly.t =
            let value = get slot product in
            let module D = (val Config.slot_domain slot) in
            D.partition part op ~f value
            |> Core_kernel.Map.Poly.map ~f:(fun value -> update slot value product)
          in
          let route = get_route part in
          partition slots.(route)


    let indent prefix range = ListLabels.map ~f:(fun s -> prefix ^ s) range

    let islot_name (i : int) =
      let slot = (Obj.magic i : 'a Config.slot) in
      let strict = Config.strict slot in
      let name = Config.slot_name slot in
      if strict then
        Format.sprintf "%s (strict)" name
      else
        name


    let introspect (type a) (op : a introspect) : a =
      let introspect_slot (op : a introspect) (Slot slot) : a =
        let module D = (val Config.slot_domain slot) in
        D.introspect op
      in
      match op with
      | GetParts f ->
          f#report Self;
          Array.iter (introspect_slot op) slots
      | Structure ->
          let tuples =
            Array.map (introspect_slot op) slots
            |> Array.to_list
            |> ListLabels.mapi ~f:(fun i sl -> islot_name i :: indent "  " sl)
            |> List.concat
          in
          "Product [" :: indent "  " tuples @ ["]"]
      | Name part -> (
          match part with
          | Self ->
              let slot_name (Slot slot) = Config.slot_name slot in
              let slot_names = Array.to_list slots |> ListLabels.map ~f:slot_name in
              Format.sprintf "Product(%s).Self" (String.concat "," slot_names)
          | _ ->
              let introspect (Slot slot) =
                let module D = (val Config.slot_domain slot) in
                D.introspect op
              in
              let route = get_route part in
              introspect slots.(route))


    let create parts =
      let update part = function
        | Some parts -> part :: parts
        | None -> [part]
      in
      let partition_by_slot partition (Part (part, _) as pv) =
        let key = get_route part in
        Core_kernel.Map.Poly.update partition key ~f:(update pv)
      in
      let partition =
        ListLabels.fold_left parts ~f:partition_by_slot ~init:Core_kernel.Map.Poly.empty
      in
      let create_slot i (Slot slot) =
        let module D = (val Config.slot_domain slot) in
        match Core_kernel.Map.Poly.find partition i with
        | Some parts -> Element (List.rev parts |> D.create)
        | _ -> Element D.bottom
      in
      Array.mapi create_slot slots |> make_strict


    let fold = Base.fold

    let apply = Base.apply
  end

  let _ = Base.fold (* unused module warning work-around *)

  include Domain
end
