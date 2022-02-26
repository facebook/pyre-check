(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Data_structures.PatriciaTreeSet.PatriciaTreeIntSet
module IntSet = Set.Make (Int)

let is_even x = x mod 2 = 0

let test_add _ =
  let set = empty in
  assert_equal (is_empty set) true;
  assert_equal (cardinal set) 0;
  assert_equal (mem 0 set) false;
  assert_equal (elements set) [];

  let set = add 0 empty in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 1;
  assert_equal (mem 0 set) true;
  assert_equal (mem 1 set) false;
  assert_equal (elements set) [0];

  let set = add 10 (add 0 empty) in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 2;
  assert_equal (mem 0 set) true;
  assert_equal (mem 1 set) false;
  assert_equal (mem 10 set) true;
  assert_equal (elements set) [10; 0];

  let set = add 10 (add 10 (add 0 empty)) in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 2;
  assert_equal (mem 0 set) true;
  assert_equal (mem 1 set) false;
  assert_equal (mem 10 set) true;
  assert_equal (elements set) [10; 0];

  let set = add 1 (add 10 (add 0 empty)) in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 3;
  assert_equal (mem 0 set) true;
  assert_equal (mem 1 set) true;
  assert_equal (mem 10 set) true;
  assert_equal (elements set) [1; 10; 0]


let test_singleton _ =
  let set = singleton 10 in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 1;
  assert_equal (mem 0 set) false;
  assert_equal (mem 10 set) true;
  assert_equal (elements set) [10]


let test_remove _ =
  let set = remove 1 empty in
  assert_equal (is_empty set) true;
  assert_equal (cardinal set) 0;
  assert_equal (mem 1 set) false;
  assert_equal (elements set) [];

  let set = remove 1 (add 1 empty) in
  assert_equal (is_empty set) true;
  assert_equal (cardinal set) 0;
  assert_equal (mem 1 set) false;
  assert_equal (elements set) [];

  let set = remove 2 (add 1 empty) in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 1;
  assert_equal (mem 1 set) true;
  assert_equal (mem 2 set) false;
  assert_equal (elements set) [1];

  let set = remove 0 (add 2 (add 0 empty)) in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 1;
  assert_equal (mem 0 set) false;
  assert_equal (mem 2 set) true;
  assert_equal (elements set) [2]


let test_of_list _ =
  let set = of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert_equal (is_empty set) false;
  assert_equal (cardinal set) 10;
  assert_equal (mem 5 set) true;
  assert_equal (mem 20 set) false;
  assert_equal (elements set) [7; 3; 5; 9; 1; 6; 2; 4; 8; 0]


let test_union _ =
  let a = of_list [0; 5; 10; 15] in
  let b = of_list [0; 1; 10; 12] in
  let set = union a b in
  assert_equal (cardinal set) 6;
  assert_equal
    (elements set |> IntSet.of_list)
    (IntSet.of_list [0; 1; 5; 10; 12; 15])
    ~cmp:IntSet.equal


let test_inter _ =
  let a = of_list [0; 5; 10; 15] in
  let b = of_list [0; 1; 10; 12] in
  let set = inter a b in
  assert_equal (cardinal set) 2;
  assert_equal (elements set |> IntSet.of_list) (IntSet.of_list [0; 10]) ~cmp:IntSet.equal


let test_diff _ =
  let a = of_list [0; 5; 10; 15] in
  let b = of_list [0; 1; 10; 12] in
  let set = diff a b in
  assert_equal (cardinal set) 2;
  assert_equal (elements set |> IntSet.of_list) (IntSet.of_list [5; 15]) ~cmp:IntSet.equal


let test_equal _ =
  assert_equal (of_list [1; 2; 3]) (of_list [3; 2; 1]) ~cmp:equal;
  assert_bool "not equal" (not (equal (of_list [1; 3]) (of_list [2; 3])))


let test_subset _ =
  assert_bool "is subset" (subset (of_list [2]) (of_list [1; 2; 3]));
  assert_bool "not subset" (not (subset (of_list [1; 4]) (of_list [1; 2; 3])));
  assert_bool "not subset" (not (subset (of_list [4; 5]) (of_list [1; 2; 3])));
  assert_bool "is subset" (subset (of_list [1; 3]) (of_list [1; 2; 3]))


let test_map _ = assert_equal (map (fun x -> 2 * x) (of_list [1; 2; 3])) (of_list [2; 4; 6])

let test_fold _ = assert_equal (fold ( + ) (of_list [1; 2; 3]) 0) 6

let test_for_all _ =
  assert_bool "for_all" (for_all is_even (of_list [2; 4; 6]));
  assert_bool "for_all" (not (for_all is_even (of_list [2; 3; 6])))


let test_exists _ =
  assert_bool "exists" (exists is_even (of_list [1; 4; 5]));
  assert_bool "exists" (not (exists is_even (of_list [1; 3; 5])))


let test_filter _ = assert_equal (filter is_even (of_list [1; 2; 3; 4])) (of_list [2; 4]) ~cmp:equal

let test_fuzz _ =
  Random.self_init ();
  let generate_random_int () =
    if Random.bool () then
      Random.bits ()
    else
      -Random.bits ()
  in
  let generate_random_list max_size () =
    let rec loop sofar = function
      | 0 -> sofar
      | n -> loop (generate_random_int () :: sofar) (n - 1)
    in
    loop [] (Random.int max_size)
  in
  let rec loop = function
    | 0 -> ()
    | n ->
        let list_a = generate_random_list 1000 () in
        let list_b = generate_random_list 1000 () in
        let set_a = IntSet.of_list list_a in
        let set_b = IntSet.of_list list_b in
        let patricia_set_a = of_list list_a in
        let patricia_set_b = of_list list_b in
        assert_equal (cardinal patricia_set_a) (IntSet.cardinal set_a);
        assert_equal (cardinal patricia_set_b) (IntSet.cardinal set_b);
        assert_equal (elements patricia_set_a |> IntSet.of_list) set_a ~cmp:IntSet.equal;
        assert_equal (elements patricia_set_b |> IntSet.of_list) set_b ~cmp:IntSet.equal;

        List.iter
          (fun x -> assert_equal ~msg:"mem" (IntSet.mem x set_a) (mem x patricia_set_a))
          list_b;

        let set_a_remove_b = List.fold_left (fun set x -> IntSet.remove x set) set_a list_b in
        let patricia_set_a_remove_b =
          List.fold_left (fun set x -> remove x set) patricia_set_a list_b
        in
        assert_equal
          ~msg:"remove"
          (elements patricia_set_a_remove_b |> IntSet.of_list)
          set_a_remove_b
          ~cmp:IntSet.equal;

        assert_equal
          ~msg:"subset"
          (IntSet.subset set_a set_b)
          (subset patricia_set_a patricia_set_b);

        let set_a_even = IntSet.filter is_even set_a in
        let patricia_set_a_even = filter is_even patricia_set_a in
        assert_equal
          ~msg:"filter"
          (elements patricia_set_a_even |> IntSet.of_list)
          set_a_even
          ~cmp:IntSet.equal;

        let test_binary_operator name set_operator patricia_set_operator () =
          let set = set_operator set_a set_b in
          let patricia_set = patricia_set_operator patricia_set_a patricia_set_b in
          assert_equal ~msg:name (cardinal patricia_set) (IntSet.cardinal set);
          assert_equal ~msg:name (elements patricia_set |> IntSet.of_list) set ~cmp:IntSet.equal
        in

        test_binary_operator "union" IntSet.union union ();
        test_binary_operator "inter" IntSet.inter inter ();
        test_binary_operator "diff" IntSet.diff diff ();
        loop (n - 1)
  in
  loop 1000


let () =
  "patriciaTreeSetTest"
  >::: [
         "add" >:: test_add;
         "singleton" >:: test_singleton;
         "remove" >:: test_remove;
         "of_list" >:: test_of_list;
         "union" >:: test_union;
         "inter" >:: test_inter;
         "diff" >:: test_diff;
         "equal" >:: test_equal;
         "subset" >:: test_subset;
         "map" >:: test_map;
         "fold" >:: test_fold;
         "for_all" >:: test_for_all;
         "exists" >:: test_exists;
         "filter" >:: test_filter;
         "fuzz" >:: test_fuzz;
       ]
  |> run_test_tt_main
