(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis


module type AbstractDomainUnderTest = sig
  include AbstractDomain.S
  val bottom : t
  (* A non-comparable list of values. *)
  val unrelated : t list
  (* More values that may be comparable with each other and unrelated *)
  val values : t list
end

let build_cartesian_prod l1 l2 =
  List.fold_right
    ~f:(fun v1 acc ->
        List.fold_right
          ~f:(fun v2 acc -> (v1, v2)::acc)
          ~init:acc l2)
    ~init:[] l1

(* General functor that tests abstract domains. *)
module TestAbstractDomain(D : AbstractDomainUnderTest) = struct

  let create_test v ~f =
    let v_s = D.show v in
    v_s >:: (f v)

  let values =
    (D.bottom :: D.unrelated)
    |> List.rev_append D.values

  let test_basic v _ =
    assert_bool "Bottom not <=" (D.less_or_equal ~left:D.bottom ~right:v);
    assert_bool "non symmetric" (D.less_or_equal ~left:v ~right:v)

  let test_bot_top _ =
    assert_bool "Bottom not bottom" (D.is_bottom D.bottom)

  let test_basic_values =
    List.map ~f:(create_test ~f:test_basic) values

  let test_cartesian ~title ~f values =
    List.mapi
      ~f:(fun index (v1, v2) ->
          let v1_s = D.show v1 in
          let name = Printf.sprintf "%s@%d: %s with %s" title index
                       v1_s
                       (D.show v2)
          in
          (name >:: (fun _ -> f v1 v2)))
      (build_cartesian_prod values values)

  (* Checks that the provided example D.unrelated are unrelated. *)
  let test_diff_unrelated =
    test_cartesian ~title:"unrelated"
      ~f:(fun v1 v2 ->
          if not (phys_equal v1 v2) then begin
            assert_bool "v1 <= v2" (not (D.less_or_equal ~left:v1 ~right:v2));
            assert_bool "v2 <= v1" (not (D.less_or_equal ~left:v2 ~right:v1))
          end)
      D.unrelated

  let test_join_conformance v1 v2 =
    let join = D.join v1 v2 in
    let join_s = D.show join in
    assert_bool "join >= v1" (D.less_or_equal ~left:v1 ~right:join);
    assert_bool "join >= v2" (D.less_or_equal ~left:v2 ~right:join);
    assert_bool (Printf.sprintf "join reflexivity: %s" join_s) (D.less_or_equal ~left:join ~right:join);
    let r_join = D.join v2 v1 in
    let r_join_s = D.show r_join in
    assert_bool (Printf.sprintf "join symmetry: %s, %s" join_s r_join_s)
      (D.less_or_equal ~left:join ~right:r_join && D.less_or_equal ~left:r_join ~right:join)

  let test_widen_conformance ~iteration v1 v2 =
    let join = D.join v1 v2 in
    let join_s = D.show join in
    let widen = D.widen ~iteration ~previous:v1 ~next:v2 in
    let widen_s = D.show widen in
    let msg = Printf.sprintf "iteration=%d:" iteration in
    assert_bool (Printf.sprintf "%s: widen >= v1" msg) (D.less_or_equal ~left:v1 ~right:widen);
    assert_bool (Printf.sprintf "%s: widen >= v2" msg) (D.less_or_equal ~left:v2 ~right:widen);
    assert_bool (Printf.sprintf "%s: widen symmetry: %s" msg widen_s) (D.less_or_equal ~left:widen ~right:widen);
    assert_bool (Printf.sprintf "%s: join <= widen, %s <= %s" msg join_s widen_s) (D.less_or_equal ~left:join ~right:widen)

  let test_joins =
    test_cartesian ~title:"join"
      ~f:test_join_conformance
      values

  let test_widens ~iteration =
    test_cartesian ~title:(Printf.sprintf "widen(%d)" iteration)
      ~f:(test_widen_conformance ~iteration)
      values

  (* The test suite created by this functor. *)
  let suite =
    test_basic_values
    |> List.rev_append [ "test_bot_top" >:: test_bot_top]
    |> List.rev_append test_diff_unrelated
    |> List.rev_append test_joins
    |> List.rev_append (test_widens ~iteration:0)
    |> List.rev_append (test_widens ~iteration:1)
    |> List.rev_append (test_widens ~iteration:2)
    |> List.rev_append (test_widens ~iteration:100)

end

(* Build up abstract domains to test. *)
module StringSet : AbstractDomainUnderTest = struct
  include AbstractSetDomain.Make(String)
  let top = None
  let unrelated = List.map ~f:singleton ["a"; "b"; "c"]
  let values =
    List.map ~f:(fun (a, b) -> union a b)
      (build_cartesian_prod unrelated unrelated)
  let values =
    List.map ~f:(fun (a, b) -> union a b)
      (build_cartesian_prod values values)
    |> List.dedup_and_sort ~compare
end

module TestStringSet = TestAbstractDomain(StringSet)

module IntToStringSet = struct
  include AbstractMapDomain.Make(Int)(StringSet)

  let top = None
  (* Test that maps are strict in bottom elements. *)
  let bottom = set empty ~key:1 ~data:StringSet.bottom

  (* Builds maps from i -> string sets, where all unrelated string elements are in the range
     except for one, and keys are from [0,n), n being the number of unrelated string elements.
     So each map value differs in the key to range mapping.
     [ 1 -> { "b" }, 2 -> { "c" } ]
     [ 1 -> { "a" }, 0 -> { "c" } ]
     [ 2 -> { "a" }, 0 -> { "b" } ]
  *)
  let rotation =
    let len = List.length TestStringSet.values in
    let populate offset =
      let result = ref empty in
      List.iteri ~f:(fun i v ->
          if i = offset then () (* skip *)
          else
            result := set (!result) ~key:((i + offset) mod len) ~data:v)
        StringSet.unrelated;
      !result
    in
    let rec build i acc =
      if i < len then
        populate i :: build (i + 1) acc
      else
        acc
    in
    build 0 []

  let unrelated = rotation
  let values = []
end

module TestIntToStringSet = TestAbstractDomain(IntToStringSet)

module PairStringMapIntToString = struct
  include AbstractPairDomain.Make(StringSet)(IntToStringSet)

  let unrelated =
    List.fold_left
      ~f:(fun l v1 ->
          List.fold_left ~f:(fun l v2 -> make v1 v2 :: l) ~init:l
            IntToStringSet.unrelated
        )
      ~init:[] StringSet.unrelated

  let values = []
end

module TestPair = TestAbstractDomain(PairStringMapIntToString)

module IntSet = struct
  include AbstractSetDomain.Make(Int)

  let top = None

  let unrelated = [singleton 1; singleton 2; singleton 3]
  let values =
    List.map ~f:(fun (a, b) -> union a b)
      (build_cartesian_prod unrelated unrelated)
  let values =
    List.map ~f:(fun (a, b) -> union a b)
      (build_cartesian_prod values values)
    |> List.dedup_and_sort ~compare
end

module TestIntSet = TestAbstractDomain(IntSet)

(* Do the testing. *)
let _ = run_test_tt_main ("flat string" >::: TestStringSet.suite)
let _ = run_test_tt_main ("int -> flat string" >::: TestIntToStringSet.suite)
let _ = run_test_tt_main ("string x (int -> flat string)" >::: TestPair.suite)
let _ = run_test_tt_main ("int set" >::: TestIntSet.suite)
