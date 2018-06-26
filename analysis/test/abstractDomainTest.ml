(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis


module type AbstractDomainUnderTest = sig
  include AbstractDomain.S

  val bottom: t

  (* A non-comparable list of values. *)
  val unrelated: t list

  (* More values that may be comparable with each other and unrelated *)
  val values: t list
end


(* General functor that tests abstract domains. *)
module TestAbstractDomain(Domain: AbstractDomainUnderTest) = struct

  let create_test value ~f =
    (Domain.show value) >:: (f value)

  let values =
    (Domain.bottom :: Domain.unrelated)
    |> List.rev_append Domain.values

  let test_basic value _ =
    assert_bool "Bottom not <=" (Domain.less_or_equal ~left:Domain.bottom ~right:value);
    assert_bool "non symmetric" (Domain.less_or_equal ~left:value ~right:value)

  let test_bottom_top _ =
    assert_bool "Bottom not bottom" (Domain.is_bottom Domain.bottom)

  let test_basic_values =
    List.map ~f:(create_test ~f:test_basic) values

  let test_cartesian ~title ~f values =
    let test_values index (v1, v2) =
      let name =
        Format.sprintf
          "%s@%d: %s with %s"
          title
          index
          (Domain.show v1)
          (Domain.show v2)
      in
      (name >:: (fun _ -> f v1 v2))
    in
    List.mapi
      (List.cartesian_product values values)
      ~f:test_values

  (* Checks that the provided example Domain.unrelated are unrelated. *)
  let test_diff_unrelated =
    let test_values v1 v2 =
      if not (phys_equal v1 v2) then
        begin
          assert_bool "v1 <= v2" (not (Domain.less_or_equal ~left:v1 ~right:v2));
          assert_bool "v2 <= v1" (not (Domain.less_or_equal ~left:v2 ~right:v1))
        end
    in
    test_cartesian ~title:"unrelated" ~f:test_values Domain.unrelated

  let test_join_conformance v1 v2 =
    let join = Domain.join v1 v2 in
    let join_s = Domain.show join in
    assert_bool "join >= v1" (Domain.less_or_equal ~left:v1 ~right:join);
    assert_bool "join >= v2" (Domain.less_or_equal ~left:v2 ~right:join);
    assert_bool
      (Format.sprintf "join reflexivity: %s" join_s)
      (Domain.less_or_equal ~left:join ~right:join);
    let right_join = Domain.join v2 v1 in
    let right_join_s = Domain.show right_join in
    assert_bool
      (Format.sprintf "join symmetry: %s, %s" join_s right_join_s)
      (Domain.less_or_equal ~left:join ~right:right_join
       && Domain.less_or_equal ~left:right_join ~right:join)

  let test_widen_conformance ~iteration v1 v2 =
    let join = Domain.join v1 v2 in
    let join_s = Domain.show join in
    let widen = Domain.widen ~iteration ~previous:v1 ~next:v2 in
    let widen_s = Domain.show widen in
    let message = Format.sprintf "iteration=%d:" iteration in
    assert_bool
      (Format.sprintf "%s: widen >= v1" message)
      (Domain.less_or_equal ~left:v1 ~right:widen);
    assert_bool
      (Format.sprintf "%s: widen >= v2" message)
      (Domain.less_or_equal ~left:v2 ~right:widen);
    assert_bool
      (Format.sprintf "%s: widen symmetry: %s" message widen_s)
      (Domain.less_or_equal ~left:widen ~right:widen);
    assert_bool
      (Format.sprintf "%s: join <= widen, %s <= %s" message join_s widen_s)
      (Domain.less_or_equal ~left:join ~right:widen)

  let test_joins =
    test_cartesian ~title:"join" ~f:test_join_conformance values

  let test_widens ~iteration =
    test_cartesian
      ~title:(Format.sprintf "widen(%d)" iteration)
      ~f:(test_widen_conformance ~iteration)
      values

  (* The test suite created by this functor. *)
  let suite =
    test_basic_values
    |> List.rev_append [ "test_bottom_top" >:: test_bottom_top]
    |> List.rev_append test_diff_unrelated
    |> List.rev_append test_joins
    |> List.rev_append (test_widens ~iteration:0)
    |> List.rev_append (test_widens ~iteration:1)
    |> List.rev_append (test_widens ~iteration:2)
    |> List.rev_append (test_widens ~iteration:100)

end

(* Build up abstract domains to test. *)
module StringSet: AbstractDomainUnderTest = struct
  include AbstractSetDomain.Make(String)

  let top =
    None

  let unrelated =
    List.map ~f:singleton ["a"; "b"; "c"]

  let values =
    List.cartesian_product unrelated unrelated
    |> List.map ~f:(Tuple2.uncurry union)

  let values =
    List.cartesian_product values values
    |> List.map ~f:(Tuple2.uncurry union)
    |> List.dedup_and_sort ~compare
end

module TestStringSet = TestAbstractDomain(StringSet)

module IntToStringSet = struct
  include AbstractMapDomain.Make(Int)(StringSet)

  let top =
    None

  (* Test that maps are strict in bottom elements. *)
  let bottom =
    set empty ~key:1 ~data:StringSet.bottom

  (* Builds maps from i -> string sets, where all unrelated string elements are in the range
     except for one, and keys are from [0,n), n being the number of unrelated string elements.
     So each map value differs in the key to range mapping.
     [ 1 -> { "b" }, 2 -> { "c" } ]
     [ 1 -> { "a" }, 0 -> { "c" } ]
     [ 2 -> { "a" }, 0 -> { "b" } ]
  *)
  let rotation =
    let length = List.length TestStringSet.values in
    let populate offset =
      let result = ref empty in
      let build_set index value =
        if index = offset then
          () (* skip *)
        else
          result := set !result ~key:((index + offset) mod length) ~data:value
      in
      List.iteri StringSet.unrelated ~f:build_set;
      !result
    in
    let rec build index accumulator =
      if index < length then
        populate index :: build (index + 1) accumulator
      else
        accumulator
    in
    build 0 []

  let unrelated =
    rotation

  let values =
    []
end

module TestIntToStringSet = TestAbstractDomain(IntToStringSet)

module PairStringMapIntToString = struct
  include AbstractPairDomain.Make(StringSet)(IntToStringSet)

  let unrelated =
    let fold_sets accumulator v1 =
      List.fold
        IntToStringSet.unrelated
        ~init:accumulator
        ~f:(fun accumulator v2 -> make v1 v2 :: accumulator)
    in
    List.fold StringSet.unrelated ~init:[] ~f:fold_sets

  let values =
    []
end

module TestPair = TestAbstractDomain(PairStringMapIntToString)

module IntSet = struct
  include AbstractSetDomain.Make(Int)

  let top =
    None

  let unrelated =
    [singleton 1; singleton 2; singleton 3]

  let values =
    List.map
      ~f:(Tuple2.uncurry union)
      (List.cartesian_product unrelated unrelated)

  let values =
    List.map
      ~f:(Tuple2.uncurry union)
      (List.cartesian_product values values)
    |> List.dedup_and_sort ~compare
end

module TestIntSet = TestAbstractDomain(IntSet)


let () =
  "abstractDomainTest">:::[
    "flat_string">:::TestStringSet.suite;
    "map_int_to_flat_string">:::TestIntToStringSet.suite;
    "string_x_maps_int_to_flat_string">:::TestPair.suite;
    "int_set">:::TestIntSet.suite;
  ]
  |> run_test_tt_main
