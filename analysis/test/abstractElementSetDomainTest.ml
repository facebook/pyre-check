(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Test


module Element = struct
  type t =
    | A
    | B
    | C of int
  [@@deriving compare, sexp, show]

  type group = t
  [@@deriving compare, sexp]

  let group = function
    | C _ -> C 0
    | a -> a

  let join a b =
    match a, b with
    | C x, C y -> C (Int.max x y)
    | _ -> a

  let less_or_equal ~left ~right =
    match left, right with
    | C x, C y -> x <= y
    | _ -> true
end


module Set = AbstractElementSetDomain.Make(Element)


let a = Set.singleton Element.A
let b = Set.singleton Element.B
let c5 = Set.singleton (Element.C 5)
let c6 = Set.singleton (Element.C 6)


let test_leq _ =
  assert_true (Set.less_or_equal ~left:a ~right:a);
  assert_true (Set.less_or_equal ~left:b ~right:b);
  assert_false (Set.less_or_equal ~left:a ~right:b);
  assert_false (Set.less_or_equal ~left:b ~right:a);
  assert_true (Set.less_or_equal ~left:c5 ~right:c5);
  assert_true (Set.less_or_equal ~left:c6 ~right:c6);
  assert_true (Set.less_or_equal ~left:c5 ~right:c6);
  assert_false (Set.less_or_equal ~left:c6 ~right:c5);
  ()


let test_join _ =
  let ab = Set.join a b in
  let c = Set.join c5 c6 in
  assert_true (Set.less_or_equal ~left:a ~right:ab);
  assert_true (Set.less_or_equal ~left:b ~right:ab);
  assert_true (Set.less_or_equal ~left:c5 ~right:c);
  assert_true (Set.less_or_equal ~left:c6 ~right:c);
  assert_true (Set.less_or_equal ~left:c ~right:c6);
  assert_false (Set.less_or_equal ~left:c ~right:c5);
  ()


let test_transform _ =
  let set = Set.join a (Set.join b c5) in
  let increment = function
    | Element.C n -> Element.C (n + 1)
    | other -> other
  in
  let transformed = Set.transform Set.Element ~f:increment set in
  let un_transformed = Set.transform Set.Element ~f:Fn.id set in
  assert_true (Set.less_or_equal ~left:set ~right:un_transformed);
  assert_true (Set.less_or_equal ~left:un_transformed ~right:set);
  assert_false (Set.less_or_equal ~left:c6 ~right:set);
  assert_true (Set.less_or_equal ~left:c6 ~right:transformed);
  ()


let test_fold _ =
  let set = Set.join a (Set.join b c6) in
  let leaves = Set.fold Set.Element set ~f:(fun result element -> element :: result) ~init:[] in
  assert_equal leaves [Element.C 6; Element.B; Element.A];
  ()


let test_partition _ =
  let ab = Set.join a b in
  let set = Set.join ab c6 in
  let partition = Set.partition Set.Element set ~f:(function Element.C _ -> true | _ -> false) in
  let without_c = Core.Map.Poly.find_exn partition false in
  let with_c = Core.Map.Poly.find_exn partition true in
  assert_true (Set.less_or_equal ~left:ab ~right:without_c);
  assert_true (Set.less_or_equal ~left:without_c ~right:ab);
  assert_true (Set.less_or_equal ~left:c6 ~right:with_c);
  assert_true (Set.less_or_equal ~left:with_c ~right:c6);
  ()


let () =
  "abstractElementSetDomainTest">:::[
    "test_join">::test_join;
    "test_leq">::test_leq;
    "test_transform">::test_transform;
    "test_fold">::test_fold;
    "test_partition">::test_partition;
  ]
  |> Test.run
