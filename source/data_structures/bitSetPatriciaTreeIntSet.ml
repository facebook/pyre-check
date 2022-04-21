(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type CONFIG = sig
  val common_integers : int array
end

(*
 * This implements a set as a union of:
 * - A bitset (using a single `int`, i.e 63 bits) storing the most common elements.
 * Each bit of the integer represents the presence of an element from `Config.common_integers`.
 * - A patricia tree set for all other integers.
 *
 * This provides a very compact representation for most common integers, with a
 * very fast union (bitwise or) and intersection (bitwise and).
 *)
module Make (Config : CONFIG) = struct
  module PatriciaTreeIntSet = PatriciaTreeSet.PatriciaTreeIntSet

  (* `bit_to_integer.(bit) = value` means that `value` is stored at the position `bit`. *)
  (* We use an array (instead of a list) for better cache locality. *)
  let bit_to_integer =
    let mapping =
      Array.sub
        Config.common_integers
        ~pos:0
        ~len:(Int.min Sys.int_size (Array.length Config.common_integers))
    in
    let () = Array.sort ~compare:Int.compare mapping in
    mapping


  module IntHashTable = Hashtbl.Make (Int)

  let integer_to_bit =
    let table = IntHashTable.create ~size:(Array.length bit_to_integer) () in
    Array.iteri bit_to_integer ~f:(fun bit integer ->
        IntHashTable.add table ~key:integer ~data:bit |> ignore);
    table


  type element = int

  type t = {
    bitset: int;
    tree: PatriciaTreeSet.PatriciaTreeIntSet.t;
  }

  let empty = { bitset = 0; tree = PatriciaTreeIntSet.empty }

  let is_empty { bitset; tree } = bitset == 0 && PatriciaTreeIntSet.is_empty tree

  let get_integer_bit integer = IntHashTable.find integer_to_bit integer

  let add element { bitset; tree } =
    match get_integer_bit element with
    | Some bit -> { bitset = bitset lor Int.shift_left 1 bit; tree }
    | None -> { bitset; tree = PatriciaTreeIntSet.add element tree }


  let mem element { bitset; tree } =
    match get_integer_bit element with
    | Some bit ->
        let pattern = Int.shift_left 1 bit in
        bitset land pattern == pattern
    | None -> PatriciaTreeIntSet.mem element tree


  let singleton element = add element empty

  let remove element { bitset; tree } =
    match get_integer_bit element with
    | Some bit -> { bitset = bitset land lnot (Int.shift_left 1 bit); tree }
    | None -> { bitset; tree = PatriciaTreeIntSet.remove element tree }


  let union { bitset = bitset_left; tree = tree_left } { bitset = bitset_right; tree = tree_right } =
    { bitset = bitset_left lor bitset_right; tree = PatriciaTreeIntSet.union tree_left tree_right }


  let inter { bitset = bitset_left; tree = tree_left } { bitset = bitset_right; tree = tree_right } =
    { bitset = bitset_left land bitset_right; tree = PatriciaTreeIntSet.inter tree_left tree_right }


  let diff { bitset = bitset_left; tree = tree_left } { bitset = bitset_right; tree = tree_right } =
    {
      bitset = bitset_left land lnot bitset_right;
      tree = PatriciaTreeIntSet.diff tree_left tree_right;
    }


  let equal { bitset = bitset_left; tree = tree_left } { bitset = bitset_right; tree = tree_right } =
    bitset_left == bitset_right && PatriciaTreeIntSet.equal tree_left tree_right


  let subset { bitset = bitset_left; tree = tree_left } { bitset = bitset_right; tree = tree_right }
    =
    bitset_left land bitset_right == bitset_left && PatriciaTreeIntSet.subset tree_left tree_right


  let fold_bitset f bitset init =
    let rec fold bitset index accumulator =
      if bitset == 0 then
        accumulator
      else
        let accumulator = if bitset land 1 == 1 then f ~index ~accumulator else accumulator in
        fold (Int.shift_right_logical bitset 1) (index + 1) accumulator
    in
    fold bitset 0 init


  let fold f { bitset; tree } init =
    init
    |> fold_bitset (fun ~index ~accumulator -> f bit_to_integer.(index) accumulator) bitset
    |> PatriciaTreeIntSet.fold f tree


  let elements set = fold (fun element results -> element :: results) set []

  let iter f { bitset; tree } =
    fold_bitset (fun ~index ~accumulator:_ -> f bit_to_integer.(index)) bitset ();
    PatriciaTreeIntSet.iter f tree


  let map f set = fold (fun element set -> add (f element) set) set empty

  let for_all predicate { bitset; tree } =
    PatriciaTreeIntSet.for_all predicate tree
    && fold_bitset
         (fun ~index ~accumulator -> accumulator && predicate bit_to_integer.(index))
         bitset
         true


  let exists predicate { bitset; tree } =
    PatriciaTreeIntSet.exists predicate tree
    || fold_bitset
         (fun ~index ~accumulator -> accumulator || predicate bit_to_integer.(index))
         bitset
         false


  let filter predicate { bitset; tree } =
    {
      bitset =
        fold_bitset
          (fun ~index ~accumulator ->
            if predicate bit_to_integer.(index) then
              accumulator lor Int.shift_left 1 index
            else
              accumulator)
          bitset
          0;
      tree = PatriciaTreeIntSet.filter predicate tree;
    }


  (* This is in O(n), same as `PatriciaTreeIntSet.cardinal`. *)
  let cardinal set = fold (fun _ count -> count + 1) set 0

  let of_list elements =
    List.fold elements ~init:empty ~f:(fun accumulator element -> add element accumulator)
end
