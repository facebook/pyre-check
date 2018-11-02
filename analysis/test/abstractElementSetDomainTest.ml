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


let test_leq _ =
  let a = Set.singleton Element.A in
  let b = Set.singleton Element.B in
  let c5 = Set.singleton (Element.C 5) in
  let c6 = Set.singleton (Element.C 6) in
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
  let a = Set.singleton Element.A in
  let b = Set.singleton Element.B in
  let c5 = Set.singleton (Element.C 5) in
  let c6 = Set.singleton (Element.C 6) in
  let ab = Set.join a b in
  let c = Set.join c5 c6 in
  assert_true (Set.less_or_equal ~left:a ~right:ab);
  assert_true (Set.less_or_equal ~left:b ~right:ab);
  assert_true (Set.less_or_equal ~left:c5 ~right:c);
  assert_true (Set.less_or_equal ~left:c6 ~right:c);
  assert_true (Set.less_or_equal ~left:c ~right:c6);
  assert_false (Set.less_or_equal ~left:c ~right:c5);
  ()


let () =
  "abstractElementSetDomainTest">:::[
    "test_join">::test_join;
    "test_leq">::test_leq;
  ]
  |> Test.run
