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

  val test_fold: test_ctxt -> unit
  val test_transform: test_ctxt -> unit
  val test_partition: test_ctxt -> unit
  val test_additional: test_ctxt -> unit
end


(* General functor that tests abstract domains. *)
module TestAbstractDomain(Domain: AbstractDomainUnderTest) = struct

  let values =
    (Domain.bottom :: Domain.unrelated)
    |> List.rev_append Domain.values

  let test_basic value _ =
    assert_bool "Bottom not <=" (Domain.less_or_equal ~left:Domain.bottom ~right:value);
    assert_bool "non symmetric" (Domain.less_or_equal ~left:value ~right:value)

  let test_bottom_top _ =
    assert_bool "Bottom not bottom" (Domain.is_bottom Domain.bottom)

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

  let test_widens ~iteration =
    test_cartesian
      ~title:(Format.sprintf "widen(%d)" iteration)
      ~f:(test_widen_conformance ~iteration)
      values

  (* The test suite created by this functor. *)
  let suite () =
    let create_test value ~f = (Domain.show value) >:: (f value) in
    let test_basic_values = List.map values ~f:(create_test ~f:test_basic) in
    let test_joins = test_cartesian ~title:"join" ~f:test_join_conformance values in

    test_basic_values
    |> List.rev_append [ "test_bottom_top" >:: test_bottom_top]
    |> List.rev_append test_diff_unrelated
    |> List.rev_append test_joins
    |> List.rev_append (test_widens ~iteration:0)
    |> List.rev_append (test_widens ~iteration:1)
    |> List.rev_append (test_widens ~iteration:100)
    |> List.rev_append [
      "test_fold" >:: Domain.test_fold;
      "test_transform" >:: Domain.test_transform;
      "test_partition" >:: Domain.test_partition;
      "test_additional" >:: Domain.test_additional;
    ]
end

(* Build up abstract domains to test. *)
module StringSet = struct
  include AbstractSetDomain.Make(String)

  let top =
    None

  let singletons = ["a"; "b"; "c"]

  let unrelated =
    List.map singletons ~f:singleton

  let values =
    List.cartesian_product unrelated unrelated
    |> List.map ~f:(Tuple2.uncurry join)
    |> List.dedup_and_sort ~compare

  let () = assert_equal 9 (List.length values)

  let test_fold _ =
    let test expected =
      let element_set = of_list expected in
      let actual =
        fold Element ~init:[] ~f:(Fn.flip List.cons) element_set
        |> List.sort ~compare:String.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)])
    in
    test ["a"];
    test ["a"; "b"];
    test ["a"; "b"; "c"]

  let test_transform _ =
    let test ~initial ~by ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform by ~f element_set
        |> fold Element ~init:[] ~f:(Fn.flip List.cons)
        |> List.sort ~compare:String.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)])
    in
    test ~initial:["a"; "b"] ~by:Element ~f:(fun x -> "t." ^ x) ~expected:["t.a"; "t.b"];
    test ~initial:["a"; "b"] ~by:Set ~f:(fun set -> "c" :: set) ~expected:["a"; "b"; "c"]

  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_list initial in
      let actual =
        partition Element ~f element_set
        |> Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result ->
              let elements =
                fold Element ~init:[] ~f:(Fn.flip List.cons) data
                |> List.sort ~compare:String.compare
              in
              (key, elements) :: result
            )
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (int * string list) list)]
          )
    in
    test
      ~initial:["abc"; "bef"; "g"]
      ~f:(fun x -> String.length x)
      ~expected:[1, ["g"]; 3, ["abc"; "bef"]]

  let test_additional _ =
    ()
end

module TestStringSet = TestAbstractDomain(StringSet)

module IntToStringSet = struct
  module Map = AbstractMapDomain.Make(struct
      include Int
      let show = Int.to_string
      let absence_implicitly_maps_to_bottom = true
    end)(StringSet)

  include Map

  let top =
    None

  (* Test that maps are strict in bottom elements. *)
  let bottom =
    set bottom ~key:1 ~data:StringSet.bottom

  let () =
    assert_equal
      Map.bottom
      bottom
      ~msg:"strict in bottom"
      ~printer:Map.show

  (* Builds maps from i -> string sets, where all unrelated string elements are in the range
     except for one, and keys are from [0,n), n being the number of unrelated string elements.
     So each map value differs in the key to range mapping.
     [ 1 -> { "b" }, 2 -> { "c" } ]
     [ 1 -> { "a" }, 0 -> { "c" } ]
     [ 2 -> { "a" }, 0 -> { "b" } ]
  *)
  let rotation =
    let length = List.length StringSet.unrelated in
    let populate offset =
      let result = ref bottom in
      let build_set index value =
        if index = offset then
          () (* skip *)
        else
          let key = (index + offset) mod length in
          result := set !result ~key ~data:value
      in
      List.iteri StringSet.unrelated ~f:build_set;
      !result
    in
    let rec build index accumulator =
      if index < length then
        build (index + 1) (populate index :: accumulator)
      else
        List.rev accumulator
    in
    build 0 []

  let unrelated =
    rotation

  let values =
    []

  let build_map elements =
    let add_elements map (key, elements) =
      set map ~key ~data:(StringSet.of_list elements)
    in
    List.fold ~f:add_elements ~init:bottom elements

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build_map initial in
      let result = fold part map ~f:(fun list element -> (f element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected:["d"; "c"; "c"; "b"; "b"; "a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:Int.to_string
      ~expected:["2"; "1"; "0"]

  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build_map initial in
      let result =
        transform part map ~f
        |> fold part ~f:(fun list element -> (to_result element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:(fun x -> "t." ^ x)
      ~to_result:Fn.id
      ~expected:["t.d"; "t.c"; "t.c"; "t.b"; "t.b"; "t.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:(fun key -> key + 1)
      ~to_result:Int.to_string
      ~expected:["3"; "2"; "1"]

  let test_partition _ =
    let test ~initial ~by:part ~f ~expected_keys =
      let map = build_map initial in
      let partition =
        let extract_keys ~key ~data result =
          let elements =
            fold Key ~init:[] ~f:(Fn.flip List.cons) data
            |> List.sort ~compare:Int.compare
          in
          (key, elements) :: result
        in
        partition part map ~f
        |> Core.Map.Poly.fold ~init:[] ~f:extract_keys
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected_keys
        partition
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (string * int list) list)]
          )
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected_keys:["a", [0]; "b", [0; 1]; "c", [1; 2]; "d", [2]];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:(fun key -> Bool.to_string (key <= 1))
      ~expected_keys:["false", [2]; "true", [0; 1]]

  let test_additional _ =
    ()
end

module TestIntToStringSet = TestAbstractDomain(IntToStringSet)

module StrictIntToStringSet = struct
  module Map = AbstractMapDomain.Make(struct
      include Int
      let show = Int.to_string
      let absence_implicitly_maps_to_bottom = false
    end)(StringSet)

  include Map

  let top =
    None

  let build_map elements =
    let add_elements map (key, elements) =
      set map ~key ~data:(StringSet.of_list elements)
    in
    List.fold ~f:add_elements ~init:bottom elements

  let unrelated = [
    build_map [ 3, [] ];
    build_map [ 4, [] ];
    build_map [ 5, [] ];
    build_map [ 0, ["a"] ];
    build_map [ 1, ["a"] ];
    build_map [ 2, ["a"] ];
    build_map [ 0, ["x"; "y"] ];
    build_map [ 1, ["foo"; "bar"] ];
  ]

  let values =
    []

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build_map initial in
      let result = fold part map ~f:(fun list element -> (f element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected:["d"; "c"; "c"; "b"; "b"; "a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:Int.to_string
      ~expected:["3"; "2"; "1"; "0"]

  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build_map initial in
      let result =
        transform part map ~f
        |> fold part ~f:(fun list element -> (to_result element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:(fun x -> "t." ^ x)
      ~to_result:Fn.id
      ~expected:["t.d"; "t.c"; "t.c"; "t.b"; "t.b"; "t.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:(fun key -> key + 1)
      ~to_result:Int.to_string
      ~expected:["4"; "3"; "2"; "1"]

  let test_partition _ =
    let test ~initial ~by:part ~f ~expected_keys =
      let map = build_map initial in
      let partition =
        partition part map ~f
        |> Core.Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result ->
              let elements =
                fold Key ~init:[] ~f:(Fn.flip List.cons) data
                |> List.sort ~compare:Int.compare
              in
              (key, elements) :: result
            )
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected_keys
        partition
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (string * int list) list)]
          )
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected_keys:["a", [0]; "b", [0; 1]; "c", [1; 2]; "d", [2]];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.Key
      ~f:(fun key -> Bool.to_string (key <= 1))
      ~expected_keys:["false", [2; 3]; "true", [0; 1]]

  let test_additional _ =
    ()
end

module TestStrictIntToStringSet = TestAbstractDomain(StrictIntToStringSet)

module PairStringMapIntToString = struct
  module LeftStringSet = AbstractSetDomain.Make(String)
  module Slots = struct
    type 'a slot =
      | Left: LeftStringSet.t slot
      | Right: IntToStringSet.t slot

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Left -> "left"
      | Right -> "right"

    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Left -> (module LeftStringSet : AbstractDomain.S with type t = a)
      | Right -> (module IntToStringSet : AbstractDomain.S with type t = a)
  end
  include AbstractProductDomain.Make(Slots)

  let build left right =
    product [Element (Slots.Left, left); Element (Slots.Right, right)]

  let unrelated =
    let fold_sets accumulator v1 =
      List.fold
        IntToStringSet.unrelated
        ~init:accumulator
        ~f:(fun accumulator v2 -> (build v1 v2) :: accumulator)
    in
    List.map
      ~f:LeftStringSet.of_list
      [
        ["a"; "b"; "c"];
        ["c"; "d"];
        ["d"; "e"];
      ]
    |> List.fold ~f:fold_sets ~init:[]

  let values =
    []

  let build elements =
    let add_elements pair (key, elements) =
      let set = get Slots.Left pair in
      let map = get Slots.Right pair in
      let left =
        List.map ~f:(fun x -> "left." ^ x) elements
        |> LeftStringSet.of_list
        |> LeftStringSet.join set
      in
      let right = IntToStringSet.set map ~key ~data:(StringSet.of_list elements) in
      build left right
    in
    List.fold ~f:add_elements ~init:bottom elements

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build initial in
      let result = fold part map ~f:(fun list element -> (f element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Left, LeftStringSet.Element))
      ~f:Fn.id
      ~expected:["left.d"; "left.c"; "left.b"; "left.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Right, StringSet.Element))
      ~f:Fn.id
      ~expected:["d"; "c"; "c"; "b"; "b"; "a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Right, IntToStringSet.Key))
      ~f:Int.to_string
      ~expected:["2"; "1"; "0"]

  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build initial in
      let result =
        transform part map ~f
        |> fold part ~f:(fun list element -> (to_result element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Left, LeftStringSet.Element))
      ~f:(fun x -> "left." ^ x)
      ~to_result:Fn.id
      ~expected:["left.left.d"; "left.left.c"; "left.left.b"; "left.left.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Right, StringSet.Element))
      ~f:(fun x -> "right." ^ x)
      ~to_result:Fn.id
      ~expected:["right.d"; "right.c"; "right.c"; "right.b"; "right.b"; "right.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:(ProductSlot (Slots.Right, IntToStringSet.Key))
      ~f:(fun x -> x + 1)
      ~to_result:Int.to_string
      ~expected:["3"; "2"; "1"]

  let test_partition _ =
    let test ~initial ~by:part ~f ~expected_keys =
      let map = build initial in
      let partition =
        partition part map ~f
        |> Core.Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result ->
              let elements =
                let part = ProductSlot (Slots.Right, IntToStringSet.Key) in
                fold part ~init:[] ~f:(Fn.flip List.cons) data
                |> List.sort ~compare:Int.compare
              in
              (key, elements) :: result
            )
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected_keys
        partition
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (string * int list) list)]
          )
    in
    test
      ~initial:[
        0, ["a"; "b"];
        1, ["b"; "c"];
        2, ["c"; "d"];
      ]
      ~by:(ProductSlot (Slots.Left, LeftStringSet.Element))
      ~f:Fn.id
      (* Pair right side distributed over left partition *)
      ~expected_keys:[
        "left.a", [0; 1; 2];
        "left.b", [0; 1; 2];
        "left.c", [0; 1; 2];
        "left.d", [0; 1; 2]
      ];
    test
      ~initial:[
        0, ["a"; "b"];
        1, ["b"; "c"];
        2, ["c"; "d"];
      ]
      ~by:(ProductSlot (Slots.Right, StringSet.Element))
      ~f:Fn.id
      ~expected_keys:[
        "a", [0];
        "b", [0; 1];
        "c", [1; 2];
        "d", [2];
      ];
    test
      ~initial:[
        0, ["a"; "b"];
        1, ["b"; "c"];
        2, ["c"; "d"];
      ]
      ~by:(ProductSlot (Slots.Right, IntToStringSet.Key))
      ~f:(fun key -> Bool.to_string (key <= 1))
      ~expected_keys:[
        "false", [2];
        "true", [0; 1];
      ]

  let test_additional _ =
    ()
end

module TestPair = TestAbstractDomain(PairStringMapIntToString)

module AbstractElement = struct
  type t =
    | A
    | B
    | C of string * int
    | CSuper  (* Above all Cs *)
  [@@deriving eq, compare, show, sexp]

  let less_or_equal ~left ~right =
    left = right
    ||
    match left, right with
    | C _, CSuper -> true
    | C (left, x), C (right, y) -> left = right && x <= y
    | _ -> false

  let show_short = function
    | A -> "A"
    | B -> "B"
    | C (s, n) -> Format.sprintf "C(%s,%d)" s n
    | CSuper -> "CSuper"
end

module AbstractElementSet = struct
  include AbstractElementSetDomain.Make(AbstractElement)

  let top =
    None

  let unrelated =
    let open AbstractElement in
    [singleton A; singleton B; singleton (C ("x", 5)); singleton (C ("y", 5))]

  let values =
    let open AbstractElement in
    [
      singleton (C ("x", 0));
      singleton (C ("x", 6));
      singleton (C ("y", 0));
      singleton (C ("y", 6));
    ]

  let accumulate_elements_as_strings list element =
    AbstractElement.show_short element :: list

  let test_fold _ =
    let test ~initial ~expected =
      let element_set = of_list initial in
      let actual =
        fold Element ~init:[] ~f:accumulate_elements_as_strings element_set
        |> List.sort ~compare:String.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)])
    in
    test
      ~initial:[A; B; C ("x", 5); C ("y", 5)]
      ~expected:["A"; "B"; "C(x,5)"; "C(y,5)"]

  let test_transform _ =
    let test ~initial ~over ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform over ~f element_set
        |> fold Element ~init:[] ~f:accumulate_elements_as_strings
        |> List.sort ~compare:String.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)])
    in
    test
      ~initial:[
        A;
        C ("x", 5);
        C ("y", 5);
      ]
      ~over:Element
      ~f:(function A -> B | B -> A | C (s, n) -> C (s, n + 1) | CSuper -> CSuper)
      ~expected:[
        "B";
        "C(x,6)";
        "C(y,6)";
      ];
    test
      ~initial:[
        A;
        C ("x", 5);
        C ("y", 5);
      ]
      ~over:Element
      ~f:(function A -> B | _ -> CSuper)
      ~expected:[
        "B";
        "CSuper";
      ];
    test
      ~initial:[
        A;
        C ("x", 5);
        C ("y", 5);
      ]
      ~over:Set
      ~f:(function existing -> CSuper :: existing)
      ~expected:[
        "A";
        "CSuper";
      ]

  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_list initial in
      let actual =
        partition Element ~f element_set
        |> Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result ->
              let elements =
                fold Element ~init:[] ~f:accumulate_elements_as_strings data
                |> List.sort ~compare:String.compare
              in
              (key, elements) :: result
            )
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected
        actual
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (int * string list) list)]
          )
    in
    test
      ~initial:[A; B; C ("x", 5); C("y", 5)]
      ~f:(function C (_, i) -> i | _ -> -1)
      ~expected:[-1, ["A"; "B"]; 5, ["C(x,5)"; "C(y,5)"]]

  let test_additional _ =
    let cmp a b =
      List.equal (elements a) (elements b) ~equal:AbstractElement.equal
    in
    let set = of_list [B; C ("x", 5); C ("y", 5)] in
    assert_equal
      set
      (add set (C ("x", 4)))
      ~msg:"add subsumed"
      ~cmp
      ~printer:show;
    assert_equal
      (of_list [B; C ("x", 6); C ("y", 5)])
      (add set (C ("x", 6)))
      ~msg:"add subsuming"
      ~cmp
      ~printer:show;
    assert_equal
      set
      (add set (C ("x", 5)))
      ~msg:"add existing"
      ~cmp
      ~printer:show;
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (add set A)
      ~msg:"add A"
      ~cmp
      ~printer:show;
    assert_equal
      (of_list [B; CSuper])
      (add set CSuper)
      ~msg:"add CSuper"
      ~cmp
      ~printer:show

end

module TestAbstractElement = TestAbstractDomain(AbstractElementSet)

module PairStringString = struct
  module Slots = struct
    type 'a slot =
      | Left: StringSet.t slot
      | Right: StringSet.t slot

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Left -> "left"
      | Right -> "right"

     let slot_domain (type a) (slot : a slot) =
      match slot with
      | Left -> (module StringSet : AbstractDomain.S with type t = a)
      | Right -> (module StringSet : AbstractDomain.S with type t = a)
  end
  include AbstractProductDomain.Make(Slots)

  let build left right =
    product [
      Element (Slots.Left, StringSet.of_list left);
      Element (Slots.Right, StringSet.of_list right);
    ]

  let unrelated = [
    build ["a"; "b"] ["c"; "d"];
    build ["c"; "d"] [];
    build [] ["e"];
  ]

  let values = [
  ]

  let test_fold _ =
    let test ~initial:(left, right) ~by:part ~f ~expected =
      let map = build left right in
      let result = fold part map ~f:(fun list element -> (f element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:(AllSlots StringSet.Element)
      ~f:Fn.id
      ~expected:["c"; "b"; "b"; "a"]

  let test_transform _ =
    let test ~initial:(left, right) ~by:part ~f ~expected ~to_result =
      let map = build left right in
      let result =
        transform part map ~f
        |> fold part ~f:(fun list element -> (to_result element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:(AllSlots StringSet.Element)
      ~f:(fun x -> "both." ^ x)
      ~to_result:Fn.id
      ~expected:["both.c"; "both.b"; "both.b"; "both.a"]

  let test_partition _ =
    let test ~initial:(left, right) ~by:part ~f ~expected =
      let map = build left right in
      let partition =
        partition part map ~f
        |> Core.Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected
        partition
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (string * string) list)]
          )
    in
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:(AllSlots StringSet.Element)
      ~f:Fn.id
      ~expected:[
        "a", "left: (set(a))";
        "b", "left: (set(b)), right: (set(b))";
        "c", "right: (set(c))";
      ]

  let test_additional _ =
    ()
end

module TestPairStringString = TestAbstractDomain(PairStringString)


module ProductDomain = struct
  module CitySet = AbstractSetDomain.Make(String)
  module YearSet = AbstractSetDomain.Make(Int)
  module RiverSet = AbstractSetDomain.Make(String)

  module Slots = struct

    type 'a slot =
      | Cities: CitySet.t slot
      | Years: YearSet.t slot
      | Rivers: RiverSet.t slot

    let slot_name (type a) (slot: a slot) =
      match slot with
      | Cities -> "Cities"
      | Years -> "Years"
      | Rivers -> "Rivers"

    let slot_domain (type a) (slot: a slot) =
      match slot with
      | Cities -> (module CitySet : AbstractDomain.S with type t = a)
      | Years -> (module YearSet : AbstractDomain.S with type t = a)
      | Rivers -> (module RiverSet : AbstractDomain.S with type t = a)

  end

  include AbstractProductDomain.Make(Slots)

  let build (cities, years, rivers) =
    product [
      Element (Cities, CitySet.of_list cities);
      Element (Years, YearSet.of_list years);
      Element (Rivers, RiverSet.of_list rivers);
    ]

  let unrelated = [
    build (["Bern"], [1191], ["Aare"]);
    build (["Neuchatel"], [1214], ["Thielle"]);
    build (["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"]);
  ]

  let values = [
  ]

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build initial in
      let result = fold part map ~f:(fun list element -> (f element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Cities, CitySet.Element))
      ~f:Fn.id
      ~expected:["Lausanne"; "Fribourg"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Years, YearSet.Element))
      ~f:Int.to_string
      ~expected:["1157"; "280"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Rivers, RiverSet.Element))
      ~f:Fn.id
      ~expected:["Sarine"; "Louve"; "Flon"]


  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build initial in
      let result =
        transform part map ~f
        |> fold part ~f:(fun list element -> (to_result element) :: list) ~init:[] in
      assert_equal
        expected
        result
        ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)]);
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Cities, CitySet.Element))
      ~f:(fun x -> x ^ " in Switzerland")
      ~to_result:Fn.id
      ~expected:["Lausanne in Switzerland"; "Fribourg in Switzerland"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Years, YearSet.Element))
      ~f:(fun x -> x + 1)
      ~to_result:Int.to_string
      ~expected:["1158"; "281"]

  let test_partition _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build initial in
      let partition =
        partition part map ~f
        |> Core.Map.Poly.fold
          ~init:[]
          ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Pervasives.compare
      in
      assert_equal
        expected
        partition
        ~printer:(fun elements ->
            Format.asprintf "%a" Sexp.pp [%message (elements: (string * string) list)]
          )
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Cities, CitySet.Element))
      ~f:Fn.id
      ~expected:[
        "Fribourg",
        "Cities: (set(Fribourg)), Rivers: (set(Flon Louve Sarine)), Years: (set(280 1157))";
        "Lausanne",
        "Cities: (set(Lausanne)), Rivers: (set(Flon Louve Sarine)), Years: (set(280 1157))";
      ];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Years, YearSet.Element))
      ~f:Int.to_string
      ~expected:[
        "1157",
        "Cities: (set(Fribourg Lausanne)), Rivers: (set(Flon Louve Sarine)), Years: (set(1157))";
        "280",
        "Cities: (set(Fribourg Lausanne)), Rivers: (set(Flon Louve Sarine)), Years: (set(280))";
      ];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:(ProductSlot (Rivers, RiverSet.Element))
      ~f:Fn.id
      ~expected:[
        "Flon",
        "Cities: (set(Fribourg Lausanne)), Rivers: (set(Flon)), Years: (set(280 1157))";
        "Louve",
        "Cities: (set(Fribourg Lausanne)), Rivers: (set(Louve)), Years: (set(280 1157))";
        "Sarine",
        "Cities: (set(Fribourg Lausanne)), Rivers: (set(Sarine)), Years: (set(280 1157))";
      ]

  let test_additional _ =
    let materialized_slot =
      transform (ProductSlot (Years, YearSet.Set)) bottom ~f:(function _ -> [0;1;2;3])
    in
    let actual =
      fold (ProductSlot (Years, YearSet.Element)) ~f:(Fn.flip List.cons) ~init:[] materialized_slot
    in
    assert_equal
      [3;2;1;0]
      actual
      ~printer:(fun elements ->
          Format.asprintf "%a" Sexp.pp [%message (elements: int list)]
        )

end

module TestProductDomain = TestAbstractDomain(ProductDomain)

let () =
  "abstractDomainTest">:::[
    "string_set">:::(TestStringSet.suite ());
    "map_int_to_string_set">:::(TestIntToStringSet.suite ());
    "strict_int_to_string_set">:::(TestStrictIntToStringSet.suite ());
    "string_x_maps_int_to_string_set">:::(TestPair.suite ());
    "dual_string">:::(TestPairStringString.suite ());
    "abstract_element">:::(TestAbstractElement.suite ());
    "product">:::(TestProductDomain.suite ());
  ]
  |> Test.run
