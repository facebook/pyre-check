(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel
open OUnit2
open AbstractDomain
module MapPoly = Map.Poly
module Fn = Core_kernel.Fn

let ( = ) = Caml.( = )

let string_list_printer elements = String.concat ~sep:"," elements

let int_list_printer elements = ListLabels.map ~f:Int.to_string elements |> String.concat ~sep:","

let string_pair_list_printer elements =
  List.map elements ~f:(fun (s1, s2) -> Format.sprintf "(%s,%s)" s1 s2) |> String.concat ~sep:", "


let int_string_pair_list_printer elements =
  List.map elements ~f:(fun (s1, s2) -> Format.sprintf "(%d,%s)" s1 s2) |> String.concat ~sep:", "


let int_string_list_list_printer elements =
  List.map elements ~f:(fun (key, elements) ->
      Format.sprintf "%d:[%s]" key (String.concat ~sep:"," elements))
  |> String.concat ~sep:"\n"


let string_int_list_list_printer elements =
  List.map elements ~f:(fun (key, elements) ->
      Format.sprintf "%s:[%s]" key (List.map ~f:Int.to_string elements |> String.concat ~sep:","))
  |> String.concat ~sep:"\n"


(* List.equal is incompatible in different core_kernel versions *)
let rec list_equal a b ~equal =
  match a, b with
  | [], [] -> true
  | ahead :: arest, bhead :: brest ->
      if equal ahead bhead then
        list_equal arest brest ~equal
      else
        false
  | _ -> false


module type AbstractDomainUnderTest = sig
  include AbstractDomain.S

  val bottom : t

  (* A non-comparable list of values. *)
  val unrelated : t list

  (* More values that may be comparable with each other and unrelated *)
  val values : t list

  val test_fold : test_ctxt -> unit

  val test_transform : test_ctxt -> unit

  val test_partition : test_ctxt -> unit

  val test_create : test_ctxt -> unit

  val test_additional : test_ctxt -> unit

  val test_context : test_ctxt -> unit
end

(* General functor that tests abstract domains. *)
module TestAbstractDomain (Domain : AbstractDomainUnderTest) = struct
  let values = Domain.bottom :: Domain.unrelated |> List.rev_append Domain.values

  let test_basic value _ =
    assert_bool "Bottom not <=" (Domain.less_or_equal ~left:Domain.bottom ~right:value);
    assert_bool "non symmetric" (Domain.less_or_equal ~left:value ~right:value)


  let test_bottom_top _ = assert_bool "Bottom not bottom" (Domain.is_bottom Domain.bottom)

  let test_cartesian ~title ~f values =
    let test_values index (v1, v2) =
      let name = Format.sprintf "%s@%d: %s with %s" title index (Domain.show v1) (Domain.show v2) in
      name >:: fun _ -> f v1 v2
    in
    List.mapi (List.cartesian_product values values) ~f:test_values


  (* Checks that the provided example Domain.unrelated are unrelated. *)
  let test_diff_unrelated =
    let test_values v1 v2 =
      if not (phys_equal v1 v2) then (
        assert_bool "v1 <= v2" (not (Domain.less_or_equal ~left:v1 ~right:v2));
        assert_bool "v2 <= v1" (not (Domain.less_or_equal ~left:v2 ~right:v1)))
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
    let widen = Domain.widen ~iteration ~prev:v1 ~next:v2 in
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


  let test_subtract_conformance v1 v2 =
    let check_difference ~title value ~from ~removed =
      let reconstituted = Domain.join value removed in
      assert_bool
        (Format.sprintf "%s: difference is less or equal to original" title)
        (Domain.less_or_equal ~left:value ~right:from);
      assert_bool
        (Format.sprintf "%s: original is less or equal to reconstitued" title)
        (Domain.less_or_equal ~left:from ~right:reconstituted)
    in
    let v1_minus_v2 = Domain.subtract v2 ~from:v1 in
    check_difference ~title:"v1_minus_v2" v1_minus_v2 ~from:v1 ~removed:v2;
    let v2_minus_v1 = Domain.subtract v1 ~from:v2 in
    check_difference ~title:"v2_minus_v1" v2_minus_v1 ~from:v2 ~removed:v1;
    let joined = Domain.join v1 v2 in
    let joined_minus_v1 = Domain.subtract v1 ~from:joined in
    check_difference ~title:"joined_minus_v1" joined_minus_v1 ~from:joined ~removed:v1;
    let joined_minus_v2 = Domain.subtract v2 ~from:joined in
    check_difference ~title:"joined_minus_v2" joined_minus_v2 ~from:joined ~removed:v2


  let test_self v _ =
    let transform x =
      assert_bool "argument not itself" (phys_equal x v);
      x
    in
    let assert_equivalent _what a b =
      if not (Domain.less_or_equal ~left:a ~right:b && Domain.less_or_equal ~left:b ~right:a) then
        assert_equal a b ~printer:Domain.show
    in
    let ident = Domain.transform Domain.Self Map ~f:transform v in
    let added = Domain.transform Domain.Self Add ~f:v Domain.bottom in
    let filtered = Domain.transform Domain.Self Filter ~f:(fun _ -> false) v in
    let kept = Domain.transform Domain.Self Filter ~f:(fun _ -> true) v in
    assert_bool "v <> ident" (phys_equal v ident);
    assert_equivalent "v <> bottom |> add v" v added;
    assert_equivalent "bottom <> filtered v" Domain.bottom filtered;
    assert_equivalent "v <> kept v" v kept


  let test_self_context v _ =
    let check_id context value =
      assert_bool "self context not equal to self" (phys_equal context value);
      assert_bool "self context not equal to value" (phys_equal context v);
      value
    in
    let r = Domain.transform Domain.Self (Context (Domain.Self, Map)) ~f:check_id v in
    assert_bool "identity transform not physically equal" (phys_equal r v)


  (* The test suite created by this functor. *)
  let suite () =
    let create_test value ~f = Domain.show value >:: f value in
    let test_basic_values = List.map values ~f:(create_test ~f:test_basic) in
    let test_self_values = List.map values ~f:(create_test ~f:test_self) in
    let test_self_contexts = List.map values ~f:(create_test ~f:test_self_context) in
    let test_joins = test_cartesian ~title:"join" ~f:test_join_conformance values in
    let test_subtract = test_cartesian ~title:"subtract" ~f:test_subtract_conformance values in
    test_basic_values
    |> List.rev_append test_self_values
    |> List.rev_append test_self_contexts
    |> List.rev_append ["test_bottom_top" >:: test_bottom_top]
    |> List.rev_append test_diff_unrelated
    |> List.rev_append test_joins
    |> List.rev_append test_subtract
    |> List.rev_append (test_widens ~iteration:0)
    |> List.rev_append (test_widens ~iteration:1)
    |> List.rev_append (test_widens ~iteration:100)
    |> List.rev_append
         [
           "test_fold" >:: Domain.test_fold;
           "test_transform" >:: Domain.test_transform;
           "test_partition" >:: Domain.test_partition;
           "test_create" >:: Domain.test_create;
           "test_additional" >:: Domain.test_additional;
           "test_context" >:: Domain.test_context;
         ]
end

(* Build up abstract domains to test. *)

module String = struct
  include String

  let name = "strings"

  let show x = x
end

module Int = struct
  include Int

  let name = "ints"

  let show = to_string
end

module StringSet = struct
  module T = AbstractSetDomain.Make (String)
  include T

  let singletons = ["a"; "b"; "c"]

  let unrelated = List.map singletons ~f:singleton

  let values =
    List.cartesian_product unrelated unrelated
    |> List.map ~f:(Tuple2.uncurry join)
    |> List.dedup_and_sort ~compare:Poly.compare


  let () = assert_equal 9 (List.length values)

  let test_fold _ =
    let test expected =
      let element_set = of_list expected in
      let actual =
        fold Element ~init:[] ~f:List.cons element_set |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ["a"];
    test ["a"; "b"];
    test ["a"; "b"; "c"]


  let test_transform _ =
    let test ~initial ~by ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform by Map ~f element_set
        |> fold Element ~init:[] ~f:List.cons
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:["a"; "b"] ~by:Element ~f:(fun x -> "t." ^ x) ~expected:["t.a"; "t.b"];
    test ~initial:["a"; "b"] ~by:Self ~f:(fun set -> add "c" set) ~expected:["a"; "b"; "c"]


  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_list initial in
      let actual =
        partition Element By ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Element ~init:[] ~f:List.cons data |> List.sort ~compare:String.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test
      ~initial:["abc"; "bef"; "g"]
      ~f:(fun x -> String.length x)
      ~expected:[1, ["g"]; 3, ["abc"; "bef"]]


  let test_create _ =
    assert_equal
      (of_list ["a"; "b"; "c"])
      (create [Part (Self, of_list ["a"; "b"; "c"])])
      ~printer:show;
    assert_equal
      (of_list ["a"; "b"; "c"])
      (create [Part (Element, "a"); Part (Element, "b"); Part (Element, "c")])
      ~printer:show


  let test_additional _ =
    let () =
      assert_equal "Set(strings)" (introspect Structure |> String.concat ~sep:"\n") ~printer:Fn.id
    in
    ()


  let test_context _ = ()
end

module TestStringSet = TestAbstractDomain (StringSet)

module InvertedStringSet = struct
  include AbstractInvertedSetDomain.Make (String)

  let singletons = ["a"; "b"; "c"]

  let unrelated = List.map singletons ~f:singleton

  let values =
    List.cartesian_product singletons singletons
    |> List.map ~f:(fun (a, b) -> singleton a |> add b)
    |> List.dedup_and_sort ~compare:Poly.compare


  let () = assert_equal 9 (List.length values)

  let of_elements elements = of_elements { is_universe = false; elements }

  let test_fold _ =
    let test expected =
      let element_set = of_elements expected in
      let actual =
        fold Element ~init:[] ~f:List.cons element_set |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ["a"];
    test ["a"; "b"];
    test ["a"; "b"; "c"]


  let test_transform _ =
    let test ~initial ~by ~f ~expected =
      let element_set = of_elements initial in
      let actual =
        transform by Map ~f element_set
        |> fold Element ~init:[] ~f:List.cons
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:["a"; "b"] ~by:Element ~f:(fun x -> "t." ^ x) ~expected:["t.a"; "t.b"];
    test ~initial:["a"; "b"] ~by:Self ~f:(fun set -> add "c" set) ~expected:["a"; "b"; "c"]


  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_elements initial in
      let actual =
        partition Element By ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Element ~init:[] ~f:List.cons data |> List.sort ~compare:String.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test
      ~initial:["abc"; "bef"; "g"]
      ~f:(fun x -> String.length x)
      ~expected:[1, ["g"]; 3, ["abc"; "bef"]]


  let test_create _ =
    assert_equal
      (of_elements ["a"; "b"; "c"])
      (create [Part (Self, of_elements ["a"; "b"; "c"])])
      ~printer:show;
    assert_equal
      (of_elements ["a"; "b"; "c"])
      (create [Part (Element, "a"); Part (Element, "b"); Part (Element, "c")])
      ~printer:show


  let assert_equivalent a b =
    if not (less_or_equal ~left:a ~right:b && less_or_equal ~left:b ~right:a) then
      assert_equal a b ~printer:show


  let test_additional _ =
    let () =
      assert_equal
        "InvertedSet(strings)"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let a = of_elements ["a"; "b"; "c"] in
    let b = of_elements ["b"; "c"; "d"] in
    let ajoinb = join a b in
    assert_equivalent (of_elements ["b"; "c"]) ajoinb


  let test_context _ = ()
end

module TestInvertedStringSet = TestAbstractDomain (InvertedStringSet)

module ToppedStringSet = struct
  module Config = struct
    include String

    let max_count () = 2
  end

  include AbstractToppedSetDomain.Make (Config)

  let singletons = ["a"; "b"; "c"]

  let unrelated = List.map singletons ~f:singleton

  let values =
    List.cartesian_product unrelated unrelated
    |> List.map ~f:(Tuple2.uncurry join)
    |> List.dedup_and_sort ~compare:Poly.compare


  let () = assert_equal 9 (List.length values)

  let test_fold _ =
    let test ?(top = false) expected =
      let element_set = of_list expected in
      let actual =
        fold Element ~init:[] ~f:List.cons element_set |> List.sort ~compare:String.compare
      in
      if top then
        assert_equal [] actual ~printer:string_list_printer
      else
        assert_equal expected actual ~printer:string_list_printer
    in
    test ["a"];
    test ["a"; "b"];
    test ["a"; "b"; "c"] ~top:true


  let test_transform _ =
    let test ~initial ~by ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform by Map ~f element_set
        |> fold Element ~init:[] ~f:List.cons
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:["a"; "b"] ~by:Element ~f:(fun x -> "t." ^ x) ~expected:["t.a"; "t.b"];
    test
      ~initial:["a"]
      ~by:Self
      ~f:(function
        | existing -> add "b" existing)
      ~expected:["a"; "b"];
    test
      ~initial:["a"; "b"]
      ~by:Self
      ~f:(function
        | existing -> add "c" existing)
      ~expected:[]


  (* top *)

  let test_partition _ = ()

  let test_create _ =
    assert_equal
      (of_list ["a"; "b"; "c"])
      (create [Part (Self, of_list ["a"; "b"; "c"])])
      ~printer:show;
    assert_equal
      (of_list ["a"; "b"; "c"])
      (create [Part (Element, "a"); Part (Element, "b"); Part (Element, "c")])
      ~printer:show;
    assert_equal (of_list ["a"; "b"]) (create [Part (Self, of_list ["a"; "b"])]) ~printer:show;
    assert_equal
      (of_list ["a"; "b"])
      (create [Part (Element, "a"); Part (Element, "b")])
      ~printer:show


  let test_additional _ =
    let () =
      assert_equal
        "ToppedSet(strings)"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    ()


  let test_context _ = ()
end

module TestToppedStringSet = TestAbstractDomain (ToppedStringSet)

module IntToStringSet = struct
  module Map =
    AbstractMapDomain.Make
      (struct
        include Int

        let absence_implicitly_maps_to_bottom = true
      end)
      (StringSet)

  include Map

  (* Test that maps are strict in bottom elements. *)
  let bottom = set bottom ~key:1 ~data:StringSet.bottom

  let () = assert_equal Map.bottom bottom ~msg:"strict in bottom" ~printer:Map.show

  (* Builds maps from i -> string sets, where all unrelated string elements are in the range
     except for one, and keys are from [0,n), n being the number of unrelated string elements.
     So each map value differs in the key to range mapping.

   * [ 1 -> { "b" }, 2 -> { "c" } ]
   * [ 1 -> { "a" }, 0 -> { "c" } ]
   * [ 2 -> { "a" }, 0 -> { "b" } ]
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


  let unrelated = rotation

  let values = []

  let build_map elements =
    let add_elements map (key, elements) = set map ~key ~data:(StringSet.of_list elements) in
    List.fold ~f:add_elements ~init:bottom elements


  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build_map initial in
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
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
      ~expected:["2"; "1"; "0"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.KeyValue
      ~f:(fun (key, set) -> Format.sprintf "%d:%d" key (StringSet.elements set |> List.length))
      ~expected:["2:2"; "1:2"; "0:2"]


  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build_map initial in
      let result =
        transform part Map ~f map
        |> fold part ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
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
      ~expected:["3"; "2"; "1"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.KeyValue
      ~f:(fun (key, set) -> key + 1, StringSet.add "new" set)
      ~to_result:(fun (key, set) -> Format.sprintf "%d: %s" key (StringSet.show set))
      ~expected:["3: [c, d, new]"; "2: [b, c, new]"; "1: [a, b, new]"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.KeyValue
      ~f:(fun (_key, set) -> 1, set)
      ~to_result:(fun (key, set) -> Format.sprintf "%d: %s" key (StringSet.show set))
      ~expected:["1: [a, b, c, d]"];
    test
      ~initial:[0, ["a"]; 1, ["b"]; 2, ["c"]; 3, ["d"]]
      ~by:Map.KeyValue
      ~f:(fun (_key, set) -> 1, set)
      ~to_result:(fun (key, set) -> Format.sprintf "%d: %s" key (StringSet.show set))
      ~expected:["1: [a, b, c, d]"]


  let test_partition _ =
    let test ~initial ~by:part ~f ~expected_keys =
      let map = build_map initial in
      let partition =
        let extract_keys ~key ~data result =
          let elements = fold Key ~init:[] ~f:List.cons data |> List.sort ~compare:Int.compare in
          (key, elements) :: result
        in
        partition part By ~f map
        |> MapPoly.fold ~init:[] ~f:extract_keys
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected_keys partition ~printer:string_int_list_list_printer
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
      ~expected_keys:["false", [2]; "true", [0; 1]];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"; "d"]; 2, ["c"; "d"]; 3, []]
      ~by:Map.KeyValue
      ~f:(fun (key, set) ->
        Format.sprintf "%b:%d" (key <= 1) (StringSet.elements set |> List.length))
      ~expected_keys:["false:2", [2]; "true:2", [0]; "true:3", [1]]


  let test_create _ =
    assert_equal
      (build_map [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []])
      (create
         [
           Part (Key, 0);
           Part (StringSet.Self, StringSet.of_list ["a"; "b"]);
           Part (Key, 1);
           Part (StringSet.Self, StringSet.of_list ["b"; "c"]);
           Part (Key, 2);
           Part (StringSet.Self, StringSet.of_list ["c"; "d"]);
           Part (Key, 3);
           Part (StringSet.Self, StringSet.of_list []);
         ])
      ~printer:show


  let assert_equivalent a b =
    if not (less_or_equal ~left:a ~right:b && less_or_equal ~left:b ~right:a) then
      assert_equal a b ~printer:show


  let test_additional _ =
    let () =
      assert_equal
        "ints -> (strict)\n  Set(strings)"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let v = build_map [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []] in
    let v1 = transform Key Filter ~f:(fun i -> i <> 1) v in
    let () =
      assert_equivalent
        v1
        (create
           [
             Part (Key, 0);
             Part (StringSet.Self, StringSet.of_list ["a"; "b"]);
             Part (Key, 2);
             Part (StringSet.Self, StringSet.of_list ["c"; "d"]);
             Part (Key, 3);
             Part (StringSet.Self, StringSet.of_list []);
           ])
    in
    let v2 = transform KeyValue Add ~f:(0, StringSet.of_list ["x"; "y"]) v1 in
    let () =
      assert_equivalent
        v2
        (create
           [
             Part (Key, 0);
             Part (StringSet.Self, StringSet.of_list ["a"; "b"; "x"; "y"]);
             Part (Key, 2);
             Part (StringSet.Self, StringSet.of_list ["c"; "d"]);
             Part (Key, 3);
             Part (StringSet.Self, StringSet.of_list []);
           ])
    in
    let v3 = transform Key FilterMap ~f:(fun i -> if i % 2 = 0 then Some (i / 2) else None) v in
    let () =
      assert_equivalent
        v3
        (create
           [
             Part (Key, 0);
             Part (StringSet.Self, StringSet.of_list ["a"; "b"]);
             Part (Key, 1);
             Part (StringSet.Self, StringSet.of_list ["c"; "d"]);
           ])
    in
    let assert_show ~expected map = assert_equal ~printer:Fn.id expected (show map) in
    assert_show ~expected:"{}" (build_map []);
    assert_show ~expected:"{0 -> [a]}" (build_map [0, ["a"]]);
    assert_show ~expected:"{\n   0 -> [a]\n   1 -> [b]\n}" (build_map [0, ["a"]; 1, ["b"]]);
    assert_show
      ~expected:"{\n   0 -> [a]\n   1 -> [b]\n   2 -> [c]\n}"
      (build_map [0, ["a"]; 1, ["b"]; 2, ["c"]]);
    assert_bool
      "of_list mapping to bottom removed in strict map"
      (of_list [0, StringSet.bottom] = bottom)


  let assert_equivalent a b =
    if not (less_or_equal ~left:a ~right:b && less_or_equal ~left:b ~right:a) then
      assert_equal a b ~printer:show


  let test_context _ =
    let v = build_map [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []] in
    let key_values key value sofar = (key, value) :: sofar in
    assert_equal
      ~printer:int_string_pair_list_printer
      (List.rev [0, "a"; 0, "b"; 1, "b"; 1, "c"; 2, "c"; 2, "d"])
      (reduce StringSet.Element ~using:(Context (Key, Acc)) ~f:key_values ~init:[] v);
    let filter key value = key = 1 && value = "b" in
    let concat key value = Format.sprintf "%d:%s" key value in
    assert_equivalent
      (build_map [1, ["b"]])
      (transform StringSet.Element (Context (Key, Filter)) ~f:(fun key -> filter key) v);
    assert_equivalent
      (build_map [1, ["1:b"]])
      (transform
         StringSet.Element
         (Context (Key, Seq (Filter, Map)))
         ~f:(fun key -> filter key, concat key)
         v);
    (* same, but inefficient as we traverse map twice *)
    assert_equivalent
      (build_map [1, ["1:b"]])
      (transform
         Self
         (Seq
            ( Context (Key, Nest (StringSet.Element, Filter)),
              Context (Key, Nest (StringSet.Element, Map)) ))
         ~f:(filter, concat)
         v);
    (* test multiple folds over different parts *)
    let acc_ints i acc = Format.sprintf "%d %s" i acc in
    let acc_strings s acc = Format.sprintf "%s %s" s acc in
    assert_equal
      ~printer:Fn.id
      "d c c b b a 2 1 0 "
      (reduce
         Self
         ~using:(Seq (Nest (Key, Acc), Nest (StringSet.Element, Acc)))
         ~f:(acc_ints, acc_strings)
         ~init:""
         v)
end

module TestIntToStringSet = TestAbstractDomain (IntToStringSet)

module NonStrictIntToStringSet = struct
  module Map =
    AbstractMapDomain.Make
      (struct
        include Int

        let absence_implicitly_maps_to_bottom = false
      end)
      (StringSet)

  include Map

  let build_map elements =
    let add_elements map (key, elements) = set map ~key ~data:(StringSet.of_list elements) in
    List.fold ~f:add_elements ~init:bottom elements


  let unrelated =
    [
      build_map [3, []];
      build_map [4, []];
      build_map [5, []];
      build_map [0, ["a"]];
      build_map [1, ["a"]];
      build_map [2, ["a"]];
      build_map [0, ["x"; "y"]];
      build_map [1, ["foo"; "bar"]];
    ]


  let values = []

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build_map initial in
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
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
        transform part Map ~f map
        |> fold part ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
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
        partition part By ~f map
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Key ~init:[] ~f:List.cons data |> List.sort ~compare:Int.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected_keys partition ~printer:string_int_list_list_printer
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


  let test_create _ =
    assert_equal
      (build_map [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []])
      (create
         [
           Part (Key, 0);
           Part (StringSet.Self, StringSet.of_list ["a"; "b"]);
           Part (Key, 1);
           Part (StringSet.Self, StringSet.of_list ["b"; "c"]);
           Part (Key, 2);
           Part (StringSet.Self, StringSet.of_list ["c"; "d"]);
           Part (Key, 3);
           Part (StringSet.Self, StringSet.of_list []);
         ])
      ~printer:show


  let assert_equivalent a b =
    if not (less_or_equal ~left:a ~right:b && less_or_equal ~left:b ~right:a) then
      assert_equal a b ~printer:show


  let test_additional _ =
    assert_equal
      "ints -> \n  Set(strings)"
      (introspect Structure |> String.concat ~sep:"\n")
      ~printer:Fn.id;
    let v = build_map [0, ["a"; "b"]; 1, ["b"; "c"; "d"]; 2, ["d"]; 3, []] in
    assert_equivalent
      (build_map [0, ["2a"; "2b"]; 1, ["3b"; "3c"; "3d"]; 2, ["1d"]; 3, []])
      (transform
         StringSet.Self
         (ReduceTransform ((Nest (StringSet.Element, Acc), 0), Nest (StringSet.Element, Map)))
         ~f:((fun _ acc -> acc + 1), fun r e -> Format.sprintf "%d%s" r e)
         v);
    assert_equal 18 (reduce Key ~using:(TransformReduce (Map, Acc)) ~f:(( * ) 3, ( + )) ~init:0 v)


  let test_context _ =
    let v = build_map [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]; 3, []] in
    let key_values key value acc = (key, value) :: acc in
    assert_equal
      ~printer:int_string_pair_list_printer
      (List.rev [0, "a"; 0, "b"; 1, "b"; 1, "c"; 2, "c"; 2, "d"])
      (reduce StringSet.Element ~using:(Context (Key, Acc)) ~f:key_values ~init:[] v)
end

module TestNonStrictIntToStringSet = TestAbstractDomain (NonStrictIntToStringSet)

module PairStringMapIntToString = struct
  module LeftStringSet = AbstractSetDomain.Make (String)

  module Slots = struct
    type 'a slot =
      | Left : LeftStringSet.t slot
      | Right : IntToStringSet.t slot

    let slots = 2

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Left -> "left"
      | Right -> "right"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Left -> (module LeftStringSet : AbstractDomain.S with type t = a)
      | Right -> (module IntToStringSet : AbstractDomain.S with type t = a)


    let strict (type a) (_slot : a slot) = false
  end

  include AbstractProductDomain.Make (Slots)

  let build left right = bottom |> update Slots.Left left |> update Slots.Right right

  let left_element = LeftStringSet.Element

  let left_set = LeftStringSet.Self

  let right_key = IntToStringSet.Key

  let right_element = StringSet.Element

  let right_set = StringSet.Self

  let unrelated =
    let fold_sets accumulator v1 =
      List.fold IntToStringSet.unrelated ~init:accumulator ~f:(fun accumulator v2 ->
          build v1 v2 :: accumulator)
    in
    List.map ~f:LeftStringSet.of_list [["a"; "b"; "c"]; ["c"; "d"]; ["d"; "e"]]
    |> List.fold ~f:fold_sets ~init:[]


  let values = []

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
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:left_element
      ~f:Fn.id
      ~expected:["left.d"; "left.c"; "left.b"; "left.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_element
      ~f:Fn.id
      ~expected:["d"; "c"; "c"; "b"; "b"; "a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_key
      ~f:Int.to_string
      ~expected:["2"; "1"; "0"]


  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build initial in
      let result =
        transform part Map ~f map
        |> fold part ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:left_element
      ~f:(fun x -> "left." ^ x)
      ~to_result:Fn.id
      ~expected:["left.left.d"; "left.left.c"; "left.left.b"; "left.left.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_element
      ~f:(fun x -> "right." ^ x)
      ~to_result:Fn.id
      ~expected:["right.d"; "right.c"; "right.c"; "right.b"; "right.b"; "right.a"];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_key
      ~f:(fun x -> x + 1)
      ~to_result:Int.to_string
      ~expected:["3"; "2"; "1"]


  let test_partition _ =
    let test ~initial ~by:part ~f ~expected_keys =
      let map = build initial in
      let partition =
        partition part By ~f map
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold right_key ~init:[] ~f:List.cons data |> List.sort ~compare:Int.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected_keys partition ~printer:string_int_list_list_printer
    in
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:left_element
      ~f:Fn.id (* Pair right side distributed over left partition *)
      ~expected_keys:
        ["left.a", [0; 1; 2]; "left.b", [0; 1; 2]; "left.c", [0; 1; 2]; "left.d", [0; 1; 2]];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_element
      ~f:Fn.id
      ~expected_keys:["a", [0]; "b", [0; 1]; "c", [1; 2]; "d", [2]];
    test
      ~initial:[0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]]
      ~by:right_key
      ~f:(fun key -> Bool.to_string (key <= 1))
      ~expected_keys:["false", [2]; "true", [0; 1]]


  let cmp a b = less_or_equal ~left:a ~right:b && less_or_equal ~left:b ~right:a

  let test_create _ =
    assert_equal
      (build [0, ["a"; "b"]; 1, ["b"; "c"]; 2, ["c"; "d"]])
      (create
         [
           Part (left_set, LeftStringSet.of_list ["left.a"; "left.b"; "left.c"; "left.d"]);
           Part (right_key, 0);
           Part (right_set, StringSet.of_list ["a"; "b"]);
           Part (right_key, 1);
           Part (right_set, StringSet.of_list ["b"; "c"]);
           Part (right_key, 2);
           Part (right_set, StringSet.of_list ["c"; "d"]);
         ])
      ~cmp
      ~printer:show


  let test_additional _ =
    let () =
      assert_equal
        "Product [\n  left\n    Set(strings)\n  right\n    ints -> (strict)\n      Set(strings)\n]"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let () =
      let parts : string list ref = ref [] in
      let gather (type a) (part : a part) = parts := AbstractDomain.part_name part :: !parts in
      introspect
        (GetParts
           (object
              method report : 'a. 'a part -> unit = gather
           end));
      assert_equal 8 (List.length !parts) ~printer:Int.to_string
    in
    let a =
      create
        [
          Part (left_set, LeftStringSet.of_list ["A"; "B"]);
          Part (right_key, 0);
          Part (right_set, StringSet.of_list ["X"]);
          Part (right_key, 1);
          Part (right_set, StringSet.of_list ["Y"]);
        ]
    in
    let b =
      create
        [
          Part (left_set, LeftStringSet.of_list ["A"]);
          Part (right_key, 0);
          Part (right_set, StringSet.of_list ["X"]);
        ]
    in
    let () =
      assert_equal
        (create
           [
             Part (left_set, LeftStringSet.of_list ["B"]);
             Part (right_key, 1);
             Part (right_set, StringSet.of_list ["Y"]);
           ])
        (subtract b ~from:a)
        ~cmp
        ~printer:show
    in
    ()


  let test_context _ = ()
end

module TestPair = TestAbstractDomain (PairStringMapIntToString)

module AbstractElement = struct
  let name = "A or B or C or CSuper"

  type t =
    | A
    | B
    | C of string * int
    | CSuper (* Above all Cs *)
  [@@deriving show]

  let equal = Poly.equal

  let compare = Poly.compare

  let less_or_equal ~left ~right =
    equal left right
    ||
    match left, right with
    | C _, CSuper -> true
    | C (left, x), C (right, y) -> String.equal left right && x <= y
    | _ -> false


  let show_short = function
    | A -> "A"
    | B -> "B"
    | C (s, n) -> Format.sprintf "C(%s,%d)" s n
    | CSuper -> "CSuper"


  let widen set =
    if List.length set > 5 then
      ListLabels.filter set ~f:(function
          | A
          | B ->
              true
          | _ -> false)
      |> List.cons CSuper
    else
      set
end

module AbstractElementSet = struct
  open AbstractElement
  include AbstractElementSetDomain.Make (AbstractElement)

  let unrelated = [singleton A; singleton B; singleton (C ("x", 5)); singleton (C ("y", 5))]

  let values =
    [singleton (C ("x", 0)); singleton (C ("x", 6)); singleton (C ("y", 0)); singleton (C ("y", 6))]


  let accumulate_elements_as_strings element list = AbstractElement.show_short element :: list

  let test_fold _ =
    let test ~initial ~expected =
      let element_set = of_list initial in
      let actual =
        fold Element ~init:[] ~f:accumulate_elements_as_strings element_set
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:[A; B; C ("x", 5); C ("y", 5)] ~expected:["A"; "B"; "C(x,5)"; "C(y,5)"]


  let test_transform _ =
    let test ~initial ~over ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform over Map ~f element_set
        |> fold Element ~init:[] ~f:accumulate_elements_as_strings
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Element
      ~f:(function
        | A -> B
        | B -> A
        | C (s, n) -> C (s, n + 1)
        | CSuper -> CSuper)
      ~expected:["B"; "C(x,6)"; "C(y,6)"];
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Element
      ~f:(function
        | A -> B
        | _ -> CSuper)
      ~expected:["B"; "CSuper"];
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Self
      ~f:(function
        | existing -> add CSuper existing)
      ~expected:["A"; "CSuper"]


  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_list initial in
      let actual =
        partition Element By ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Element ~init:[] ~f:accumulate_elements_as_strings data
                 |> List.sort ~compare:String.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test
      ~initial:[A; B; C ("x", 5); C ("y", 5)]
      ~f:(function
        | C (_, i) -> i
        | _ -> -1)
      ~expected:[-1, ["A"; "B"]; 5, ["C(x,5)"; "C(y,5)"]]


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (create [Part (Self, of_list [A; B; C ("x", 5); C ("y", 5)])])
      ~printer:show
      ~cmp:compare;
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (create
         [
           Part (Element, A);
           Part (Element, B);
           Part (Element, C ("x", 5));
           Part (Element, C ("y", 5));
         ])
      ~printer:show
      ~cmp:compare


  let test_additional _ =
    let () =
      assert_equal
        "AbstractElementSet(A or B or C or CSuper)"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let cmp a b = list_equal (elements a) (elements b) ~equal:AbstractElement.equal in
    let set = of_list [B; C ("x", 5); C ("y", 5)] in
    assert_equal set (add (C ("x", 4)) set) ~msg:"add subsumed" ~cmp ~printer:show;
    assert_equal
      (of_list [B; C ("x", 6); C ("y", 5)])
      (add (C ("x", 6)) set)
      ~msg:"add subsuming"
      ~cmp
      ~printer:show;
    assert_equal set (add (C ("x", 5)) set) ~msg:"add existing" ~cmp ~printer:show;
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (add A set)
      ~msg:"add A"
      ~cmp
      ~printer:show;
    assert_equal (of_list [B; CSuper]) (add CSuper set) ~msg:"add CSuper" ~cmp ~printer:show;
    let big_set = of_list [A; B; C ("x", 5); C ("y", 5); C ("z", 5); C ("q", 5)] in
    assert_equal
      (of_list [A; B; CSuper])
      (widen ~iteration:0 ~prev:big_set ~next:bottom)
      ~msg:"widen"
      ~cmp
      ~printer:show


  let test_context _ = ()
end

module TestAbstractElement = TestAbstractDomain (AbstractElementSet)

module AbstractBucketedElement = struct
  include AbstractElement

  type bucket =
    | CBucket
    | Rest

  let bucket = function
    | C _
    | CSuper ->
        CBucket
    | _ -> Rest


  let pp_bucket formatter = function
    | CBucket -> Format.fprintf formatter "CBucket"
    | Rest -> Format.fprintf formatter "Rest"


  let compare_bucket = Poly.compare
end

module AbstractBucketedElementSet = struct
  open AbstractBucketedElement
  include AbstractBucketedElementSetDomain.Make (AbstractBucketedElement)

  let unrelated = [singleton A; singleton B; singleton (C ("x", 5)); singleton (C ("y", 5))]

  let values =
    [singleton (C ("x", 0)); singleton (C ("x", 6)); singleton (C ("y", 0)); singleton (C ("y", 6))]


  let accumulate_elements_as_strings element list =
    AbstractBucketedElement.show_short element :: list


  let test_fold _ =
    let test ~initial ~expected =
      let element_set = of_list initial in
      let actual =
        fold Element ~init:[] ~f:accumulate_elements_as_strings element_set
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:[A; B; C ("x", 5); C ("y", 5)] ~expected:["A"; "B"; "C(x,5)"; "C(y,5)"]


  let test_transform _ =
    let test ~initial ~over ~f ~expected =
      let element_set = of_list initial in
      let actual =
        transform over Map ~f element_set
        |> fold Element ~init:[] ~f:accumulate_elements_as_strings
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Element
      ~f:(function
        | A -> B
        | B -> A
        | C (s, n) -> C (s, n + 1)
        | CSuper -> CSuper)
      ~expected:["B"; "C(x,6)"; "C(y,6)"];
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Element
      ~f:(function
        | A -> B
        | _ -> CSuper)
      ~expected:["B"; "CSuper"];
    test
      ~initial:[A; C ("x", 5); C ("y", 5)]
      ~over:Self
      ~f:(function
        | existing -> add CSuper existing)
      ~expected:["A"; "CSuper"]


  let test_partition _ =
    let test ~initial ~f ~expected =
      let element_set = of_list initial in
      let actual =
        partition Element ByFilter ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Element ~init:[] ~f:accumulate_elements_as_strings data
                 |> List.sort ~compare:String.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test
      ~initial:[A; B; C ("x", 5); C ("y", 5); C ("z", 6)]
      ~f:(function
        | C (_, 5) -> Some 5
        | C _ -> None
        | _ -> Some (-1))
      ~expected:[-1, ["A"; "B"]; 5, ["C(x,5)"; "C(y,5)"]]


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (create [Part (Self, of_list [A; B; C ("x", 5); C ("y", 5)])])
      ~printer:show
      ~cmp:compare;
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (create
         [
           Part (Element, A);
           Part (Element, B);
           Part (Element, C ("x", 5));
           Part (Element, C ("y", 5));
         ])
      ~printer:show
      ~cmp:compare


  let test_additional _ =
    let () =
      assert_equal
        "bucket -> (strict)\n  AbstractElementSet(A or B or C or CSuper)"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let cmp a b = list_equal (elements a) (elements b) ~equal:AbstractBucketedElement.equal in
    let set = of_list [B; C ("x", 5); C ("y", 5)] in
    assert_equal set (add (C ("x", 4)) set) ~msg:"add subsumed" ~cmp ~printer:show;
    assert_equal
      (of_list [B; C ("x", 6); C ("y", 5)])
      (add (C ("x", 6)) set)
      ~msg:"add subsuming"
      ~cmp
      ~printer:show;
    assert_equal set (add (C ("x", 5)) set) ~msg:"add existing" ~cmp ~printer:show;
    assert_equal
      (of_list [A; B; C ("x", 5); C ("y", 5)])
      (add A set)
      ~msg:"add A"
      ~cmp
      ~printer:show;
    assert_equal (of_list [B; CSuper]) (add CSuper set) ~msg:"add CSuper" ~cmp ~printer:show;
    let big_set = of_list [A; B; C ("x", 5); C ("y", 5); C ("z", 5); C ("q", 5)] in
    assert_equal
      (of_list [A; B; CSuper])
      (widen ~iteration:0 ~prev:big_set ~next:bottom)
      ~msg:"widen"
      ~cmp
      ~printer:show


  let test_context _ = ()
end

module TestAbstractBucketedElement = TestAbstractDomain (AbstractBucketedElementSet)

module PairStringString = struct
  module LeftStringSet = AbstractSetDomain.Make (String)
  module RightStringSet = AbstractSetDomain.Make (String)

  module Slots = struct
    type 'a slot =
      | Left : LeftStringSet.t slot
      | Right : RightStringSet.t slot

    let slots = 2

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Left -> "left"
      | Right -> "right"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Left -> (module LeftStringSet : AbstractDomain.S with type t = a)
      | Right -> (module RightStringSet : AbstractDomain.S with type t = a)


    let strict (type a) (slot : a slot) =
      match slot with
      | Left -> true
      | Right -> true
  end

  include AbstractProductDomain.Make (Slots)

  let build left right =
    create
      [
        Part (LeftStringSet.Self, LeftStringSet.of_list left);
        Part (RightStringSet.Self, RightStringSet.of_list right);
      ]


  let unrelated = [build ["a"; "b"] ["c"; "d"]; build ["c"; "d"] ["e"]; build ["a"] ["e"]]

  let values = [build [] []]

  let test_fold _ =
    let test ~initial:(left, right) ~by:part ~f ~expected =
      let map = build left right in
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
    in
    test ~initial:(["a"; "b"], ["b"; "c"]) ~by:LeftStringSet.Element ~f:Fn.id ~expected:["b"; "a"];
    test ~initial:(["a"; "b"], ["b"; "c"]) ~by:RightStringSet.Element ~f:Fn.id ~expected:["c"; "b"]


  let test_transform _ =
    let test ~initial:(left, right) ~by:part ~f ~expected ~to_result =
      let map = build left right in
      let result =
        transform part Map ~f map
        |> fold part ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:LeftStringSet.Element
      ~f:(fun x -> "left." ^ x)
      ~to_result:Fn.id
      ~expected:["left.b"; "left.a"];
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:RightStringSet.Element
      ~f:(fun x -> "right." ^ x)
      ~to_result:Fn.id
      ~expected:["right.c"; "right.b"]


  let test_partition _ =
    let test ~initial:(left, right) ~by:part ~f ~expected =
      let map = build left right in
      let partition =
        partition part By ~f map
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected partition ~printer:string_pair_list_printer
    in
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:LeftStringSet.Element
      ~f:Fn.id
      ~expected:["a", "left: [a], right: [b, c]"; "b", "left: [b], right: [b, c]"];
    test
      ~initial:(["a"; "b"], ["b"; "c"])
      ~by:RightStringSet.Element
      ~f:Fn.id
      ~expected:["b", "left: [a, b], right: [b]"; "c", "left: [a, b], right: [c]"]


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal
      (build ["a"; "b"] ["a"; "b"])
      (create
         [
           Part (LeftStringSet.Self, LeftStringSet.of_list ["a"; "b"]);
           Part (RightStringSet.Self, RightStringSet.of_list ["a"; "b"]);
         ])
      ~printer:show
      ~cmp:compare;

    (* Check strictness *)
    assert_equal bottom (build ["a"; "b"] []) ~printer:show ~cmp:compare;
    assert_equal bottom (build [] ["a"; "b"]) ~printer:show ~cmp:compare;

    (* Check update strictness *)
    assert_equal
      bottom
      (update Slots.Left (LeftStringSet.singleton "a") bottom)
      ~printer:show
      ~cmp:compare;
    assert_equal
      bottom
      (update Slots.Right (RightStringSet.singleton "a") bottom)
      ~printer:show
      ~cmp:compare


  let test_additional _ =
    assert_bool "product of bottoms is bottom" (is_bottom (build [] []));
    assert_bool
      "product of bottoms is less equal to bottom"
      (less_or_equal ~left:(build [] []) ~right:bottom);
    let a = build ["a"] ["b"; "c"] in
    let b = build ["a"] ["b"; "c"; "d"] in
    let () =
      assert_bool
        "subtraction of all slots leads to bottom"
        (less_or_equal ~left:(subtract b ~from:a) ~right:bottom)
    in
    let a = build ["a"; "b"] ["x"; "y"] in
    let b = build ["a"] ["x"] in
    let c = build ["a"] ["x"; "y"] in
    let d = build ["a"; "b"] ["x"] in
    let e = build ["b"] ["x"; "y"] in
    let f = build ["a"; "b"] ["y"] in
    let () = assert_equal a (subtract b ~from:a) ~cmp:compare ~printer:show in
    let () = assert_equal e (subtract c ~from:a) ~cmp:compare ~printer:show in
    let () = assert_equal f (subtract d ~from:a) ~cmp:compare ~printer:show in
    ()


  let test_context _ = ()
end

module TestPairStringString = TestAbstractDomain (PairStringString)

module ProductDomain = struct
  module CitySet = AbstractSetDomain.Make (String)
  module YearSet = AbstractSetDomain.Make (Int)
  module RiverSet = AbstractSetDomain.Make (String)

  module Slots = struct
    type 'a slot =
      | Cities : CitySet.t slot
      | Years : YearSet.t slot
      | Rivers : RiverSet.t slot

    let slots = 3

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Cities -> "Cities"
      | Years -> "Years"
      | Rivers -> "Rivers"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Cities -> (module CitySet : AbstractDomain.S with type t = a)
      | Years -> (module YearSet : AbstractDomain.S with type t = a)
      | Rivers -> (module RiverSet : AbstractDomain.S with type t = a)


    let strict (type a) (slot : a slot) =
      match slot with
      | Cities -> true
      | _ -> false
  end

  open Slots
  include AbstractProductDomain.Make (Slots)

  let build (cities, years, rivers) =
    bottom
    |> update Cities (CitySet.of_list cities)
    |> update Years (YearSet.of_list years)
    |> update Rivers (RiverSet.of_list rivers)


  let unrelated =
    [
      build (["Bern"], [1191], ["Aare"]);
      build (["Neuchatel"], [1214], ["Thielle"]);
      build (["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"]);
    ]


  let values = []

  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build initial in
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:CitySet.Element
      ~f:Fn.id
      ~expected:["Lausanne"; "Fribourg"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:YearSet.Element
      ~f:Int.to_string
      ~expected:["1157"; "280"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:RiverSet.Element
      ~f:Fn.id
      ~expected:["Sarine"; "Louve"; "Flon"]


  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = build initial in
      let result =
        transform part Map ~f map
        |> fold part ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:CitySet.Element
      ~f:(fun x -> x ^ " in Switzerland")
      ~to_result:Fn.id
      ~expected:["Lausanne in Switzerland"; "Fribourg in Switzerland"];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:YearSet.Element
      ~f:(fun x -> x + 1)
      ~to_result:Int.to_string
      ~expected:["1158"; "281"]


  let test_partition _ =
    let test ~initial ~by:part ~f ~expected =
      let map = build initial in
      let partition =
        partition part By ~f map
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected partition ~printer:string_pair_list_printer
    in
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:CitySet.Element
      ~f:Fn.id
      ~expected:
        [
          "Fribourg", "Cities: [Fribourg], Years: [280, 1157], Rivers: [Flon, Louve, Sarine]";
          "Lausanne", "Cities: [Lausanne], Years: [280, 1157], Rivers: [Flon, Louve, Sarine]";
        ];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:YearSet.Element
      ~f:(fun x -> Int.to_string x)
      ~expected:
        [
          "1157", "Cities: [Fribourg, Lausanne], Years: [1157], Rivers: [Flon, Louve, Sarine]";
          "280", "Cities: [Fribourg, Lausanne], Years: [280], Rivers: [Flon, Louve, Sarine]";
        ];
    test
      ~initial:(["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"])
      ~by:RiverSet.Element
      ~f:Fn.id
      ~expected:
        [
          "Flon", "Cities: [Fribourg, Lausanne], Years: [280, 1157], Rivers: [Flon]";
          "Louve", "Cities: [Fribourg, Lausanne], Years: [280, 1157], Rivers: [Louve]";
          "Sarine", "Cities: [Fribourg, Lausanne], Years: [280, 1157], Rivers: [Sarine]";
        ]


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal
      (build (["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"]))
      (create
         [
           Part (CitySet.Self, CitySet.of_list ["Lausanne"; "Fribourg"]);
           Part (YearSet.Self, YearSet.of_list [280; 1157]);
           Part (RiverSet.Self, RiverSet.of_list ["Flon"; "Louve"; "Sarine"]);
         ])
      ~printer:show
      ~cmp:compare


  let test_additional _ =
    let () =
      assert_equal
        "Product [\n\
        \  Cities (strict)\n\
        \    Set(strings)\n\
        \  Years\n\
        \    Set(ints)\n\
        \  Rivers\n\
        \    Set(strings)\n\
         ]"
        (introspect Structure |> String.concat ~sep:"\n")
        ~printer:Fn.id
    in
    let () =
      let materialized_slot =
        transform
          CitySet.Self
          Map
          ~f:(function
            | _ -> CitySet.of_list ["A"; "B"; "C"])
          bottom
      in
      let actual = fold CitySet.Element ~f:List.cons ~init:[] materialized_slot in
      assert_equal ["C"; "B"; "A"] actual ~printer:string_list_printer
    in
    let () =
      let unmaterialized_slot =
        (* due to strictness *)
        transform
          YearSet.Self
          Map
          ~f:(function
            | _ -> YearSet.of_list [0; 1; 2; 3])
          bottom
      in
      let actual = fold YearSet.Element ~f:List.cons ~init:[] unmaterialized_slot in
      assert_equal [] actual ~printer:int_list_printer
    in
    let a = build (["Lausanne"; "Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"]) in
    let b = build (["Fribourg"], [280; 1157], ["Flon"; "Louve"; "Sarine"]) in
    let c = build ([], [280; 1157], []) in
    let d = build (["Lausanne"], [280; 1157], ["Flon"]) in
    let () =
      assert_equal
        (build (["Lausanne"], [280; 1157], ["Flon"; "Louve"; "Sarine"]))
        (subtract b ~from:a)
        ~printer:show
        ~cmp:compare
    in
    let () = assert_equal a (subtract c ~from:a) ~printer:show ~cmp:compare in
    let () = assert_equal a (subtract d ~from:a) ~printer:show ~cmp:compare in
    ()


  let test_context _ = ()
end

module TestProductDomain = TestAbstractDomain (ProductDomain)

module PathDomain = struct
  exception Length of int

  let common_prefix a b =
    let size = min (String.length a) (String.length b) in
    try
      for i = 0 to size - 1 do
        if not (Char.equal a.[i] b.[i]) then raise (Length i)
      done;
      String.prefix a size
    with
    | Length n -> String.prefix a n


  module Element = struct
    type t =
      | Bottom
      | Path of string

    let name = "path"

    let show = function
      | Bottom -> "<bottom>"
      | Path p -> p


    let less_or_equal ~(left : t) ~(right : t) =
      match left, right with
      | Bottom, _ -> true
      | _, Bottom -> false
      | Path left, Path right -> String.is_prefix ~prefix:right left


    let join left right =
      match left, right with
      | Bottom, _ -> right
      | _, Bottom -> left
      | Path p1, Path p2 ->
          let p = common_prefix p1 p2 in
          if String.equal p p1 then
            left
          else if String.equal p p2 then
            right
          else
            Path p


    let meet left right =
      match left, right with
      | Bottom, _ -> left
      | _, Bottom -> right
      | Path p1, Path p2 ->
          let p = common_prefix p1 p2 in
          if String.equal p p1 then
            right
          else if String.equal p p2 then
            left
          else
            Bottom


    let bottom = Bottom
  end

  include AbstractSimpleDomain.Make (Element)

  let build path = create [Part (Self, Element.Path path)]

  let unrelated = [build "abcde"; build "efgh"; build "abcdf"]

  let values = []

  let test_fold _ =
    let test ~initial ~expected =
      let product = build initial in
      let result = fold Self product ~init:"" ~f:(fun value sofar -> Element.show value ^ sofar) in
      assert_equal expected result ~printer:Fn.id
    in
    test ~initial:"abc" ~expected:"abc"


  let test_transform _ =
    let test ~initial ~f ~expected =
      let product = build initial in
      let result = transform Self Map ~f product |> Element.show in
      assert_equal expected result ~printer:Fn.id
    in
    test
      ~initial:"abc"
      ~f:(function
        | Element.Bottom -> Element.Path "z"
        | Element.Path x -> Element.Path (x ^ "z"))
      ~expected:"abcz"


  let test_partition _ =
    let test ~initial ~f ~expected =
      let product = build initial in
      let partition =
        partition Self ByFilter product ~f
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected partition ~printer:string_pair_list_printer
    in
    test
      ~initial:"abc"
      ~f:(function
        | Element.Bottom -> None
        | Element.Path x -> Some x)
      ~expected:["abc", "abc"]


  let equal left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal (build "foo") (create [Part (Self, Element.Path "foo")]) ~printer:show ~cmp:equal


  let test_additional _ =
    let () =
      assert_equal "Simple(path)" (introspect Structure |> String.concat ~sep:"\n") ~printer:Fn.id
    in
    let test ~initial ~expected =
      let product = build initial in
      let result = Element.show product in
      assert_equal expected result ~printer:Fn.id
    in
    let test_meet a b ~expected =
      let meet = Element.meet a b in
      assert_equal expected meet ~printer:Element.show
    in
    let a = build "abcdefg" in
    let b = build "abxyz" in
    let c = join a b in
    assert_equal (build "ab") c ~printer:show ~cmp:equal;
    test ~initial:"abc" ~expected:"abc";
    test_meet a bottom ~expected:bottom;
    test_meet bottom a ~expected:bottom;
    test_meet a c ~expected:a;
    test_meet c a ~expected:a;
    test_meet b c ~expected:b;
    test_meet c b ~expected:b


  let test_context _ = ()
end

module TestSimpleDomain = TestAbstractDomain (PathDomain)

module TreeOfStringSets = struct
  include
    AbstractTreeDomain.Make
      (struct
        let max_tree_depth_after_widening () = 3

        let check_invariants = true
      end)
      (struct
        include StringSet

        let transform_on_widening_collapse = Fn.id
      end)
      ()

  let parse_path path =
    let parse_element element =
      if String.sub element ~pos:0 ~len:1 = "$" then
        AbstractTreeDomain.Label.Field element
      else
        AbstractTreeDomain.Label.create_name_index element
    in
    String.split path ~on:'.'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:parse_element


  let parse_tree components =
    let parse_component (path, elements) =
      AbstractDomain.Part (Path, (parse_path path, StringSet.of_list elements))
    in
    List.map components ~f:parse_component |> create


  let unrelated =
    [
      create [Part (Path, (parse_path "a", StringSet.of_list ["aa"]))];
      create [Part (Path, (parse_path "b", StringSet.of_list ["bb"]))];
      create
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["ab"]));
          Part (Path, (parse_path "c.d", StringSet.of_list ["cd"]));
        ];
      create [Part (Path, (parse_path "$a", StringSet.of_list ["aa"]))];
      create [Part (Path, (parse_path "$b", StringSet.of_list ["bb"]))];
      create
        [
          Part (Path, (parse_path "$a.$b", StringSet.of_list ["ab"]));
          Part (Path, (parse_path "$c.$d", StringSet.of_list ["cd"]));
        ];
    ]


  let values =
    [
      create [Part (Path, (parse_path "a.b", StringSet.of_list ["aa"]))];
      create [Part (Path, (parse_path "b.c", StringSet.of_list ["bb"]))];
      create [Part (Path, (parse_path "$a.$b", StringSet.of_list ["aa"]))];
      create [Part (Path, (parse_path "$b.$c", StringSet.of_list ["bb"]))];
      create [Part (Path, ([], StringSet.of_list ["ab"]))];
      create [Part (Path, ([], StringSet.of_list ["cd"]))];
    ]


  let show_path_element (path, tip) =
    Format.sprintf "path:%s; tip:%s" (AbstractTreeDomain.Label.show_path path) (StringSet.show tip)


  let test_fold _ =
    let test ~initial ~by:part ~f ~expected =
      let map = create initial in
      let result = fold part map ~f:(fun element list -> f element :: list) ~init:[] in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:[Part (Path, (parse_path "a.b", StringSet.of_list ["aa"]))]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected:["aa"];
    test
      ~initial:[Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]))]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected:["bb"; "aa"];
    test
      ~initial:
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]));
          Part (Path, (parse_path "a", StringSet.of_list ["a"]));
        ]
      ~by:Path
      ~f:show_path_element
      ~expected:["path:[a][b]; tip:[aa, bb]"; "path:[a]; tip:[a]"]


  let test_transform _ =
    let test ~initial ~by:part ~f ~expected ~to_result =
      let map = create initial in
      let result =
        transform part Map ~f map
        |> fold Path ~f:(fun element list -> to_result element :: list) ~init:[]
      in
      assert_equal expected result ~printer:string_list_printer
    in
    test
      ~initial:
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]));
          Part (Path, (parse_path "a", StringSet.of_list ["a"]));
        ]
      ~by:StringSet.Element
      ~f:(fun x -> "t." ^ x)
      ~to_result:show_path_element
      ~expected:["path:[a][b]; tip:[t.aa, t.bb]"; "path:[a]; tip:[t.a]"];
    test
      ~initial:
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]));
          Part (Path, (parse_path "a", StringSet.of_list ["a"]));
        ]
      ~by:Path
      ~f:(fun (path, element) -> AbstractTreeDomain.Label.Index "prefix" :: path, element)
      ~to_result:show_path_element
      ~expected:["path:[prefix][a][b]; tip:[aa, bb]"; "path:[prefix][a]; tip:[a]"]


  let test_partition _ =
    let test ~initial ~by:part ~f ~expected =
      let map = create initial in
      let partition =
        partition part By map ~f
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result -> (key, show data) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected partition ~printer:string_pair_list_printer
    in
    test
      ~initial:
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]));
          Part (Path, (parse_path "a", StringSet.of_list ["a"]));
        ]
      ~by:StringSet.Element
      ~f:Fn.id
      ~expected:
        [
          "a", "{\n   [a] -> [a]\n}";
          "aa", "{\n   [a][b] -> [aa]\n}";
          "bb", "{\n   [a][b] -> [bb]\n}";
        ];
    test
      ~initial:
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["aa"; "bb"]));
          Part (Path, (parse_path "a", StringSet.of_list ["a"]));
        ]
      ~by:Path
      ~f:(fun (path, _) -> List.take path 1 |> AbstractTreeDomain.Label.show_path)
      ~expected:["[a]", "{\n   [a] -> [a]\n   [a][b] -> [aa, bb]\n}"]


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_create _ =
    assert_equal
      (create [Part (Path, ([], StringSet.of_list ["a"; "b"]))])
      (create [Part (StringSet.Element, "a"); Part (StringSet.Element, "b")])
      ~printer:show
      ~cmp:compare


  let test_additional _ =
    let deep_element = create [Part (Path, (parse_path "a.b.c.d", StringSet.of_list ["x"; "y"]))] in
    assert_equal
      (widen ~iteration:0 ~prev:deep_element ~next:deep_element)
      (create [Part (Path, (parse_path "a.b.c", StringSet.of_list ["x"; "y"]))])
      ~printer:show
      ~cmp:compare;
    let () =
      let open AbstractTreeDomain.Label in
      let path1 = [create_name_index "foo"; create_name_index "bar"] in
      let path2 = [create_name_index "foo"; create_name_index "baz"] in
      let common = [create_name_index "foo"] in
      let path3 = common_prefix path1 path2 in
      let path4 = common_prefix path1 path3 in
      let path5 = common_prefix path3 path2 in
      assert_equal common path3;
      assert_equal path3 path4;
      assert_equal path4 path5
    in
    let () =
      let tree =
        create
          [
            Part (Path, (parse_path "a.b", StringSet.of_list ["x"; "y"]));
            Part (Path, (parse_path "c", StringSet.of_list ["z"]));
            Part (Path, (parse_path "d", StringSet.of_list ["q"]));
          ]
      in
      let mold =
        create
          [
            Part (Path, (parse_path "a", StringSet.of_list ["x"; "z"]));
            Part (Path, (parse_path "c", StringSet.of_list ["p"]));
          ]
      in
      let expected =
        create
          [
            Part (Path, (parse_path "a", StringSet.of_list ["x"; "y"]));
            Part (Path, (parse_path "c", StringSet.of_list ["z"]));
            Part (Path, ([], StringSet.of_list ["q"]));
          ]
      in
      assert_equal
        expected
        (shape ~transform:Fn.id tree ~mold)
        ~msg:"molded tree"
        ~printer:show
        ~cmp:compare
    in
    (* Test less or equal. *)
    assert_equal
      true
      (less_or_equal ~left:(parse_tree ["$keys", ["a"]]) ~right:(parse_tree ["$keys", ["a"]]));
    assert_equal
      true
      (less_or_equal ~left:(parse_tree ["$keys", ["a"]]) ~right:(parse_tree ["$keys", ["a"; "b"]]));
    assert_equal
      false
      (less_or_equal ~left:(parse_tree ["$keys", ["a"]]) ~right:(parse_tree ["f", ["b"]]));
    assert_equal
      true
      (less_or_equal
         ~left:(parse_tree ["a", ["v"]])
         ~right:(parse_tree ["a", ["v"]; "$keys", ["w"]]));
    assert_equal
      false
      (less_or_equal ~left:(parse_tree ["$keys", ["a"; "b"]]) ~right:(parse_tree ["$keys", ["a"]]));
    assert_equal
      false
      (less_or_equal ~left:(parse_tree ["$keys", ["a"; "b"]]) ~right:(parse_tree []));
    (* Even though the key taint is not LEQ the any taint in the [*] of the right tree, we view the
       tree as <= the other one since the key can never be accessed as a value. *)
    assert_equal
      false
      (less_or_equal
         ~left:(parse_tree ["$keys", ["a"]])
         ~right:
           (create
              [Part (Path, ([AbstractTreeDomain.Label.AnyIndex], StringSet.of_list ["distinct"]))]));
    assert_equal
      true
      (less_or_equal ~left:(parse_tree ["a.$keys", ["val"]]) ~right:(parse_tree ["a", ["val"]]));
    assert_equal
      false
      (less_or_equal ~left:(parse_tree ["a.$keys", ["val"]]) ~right:(parse_tree ["a", ["other"]]));

    let assert_join ~expected left right =
      assert_equal ~cmp:compare ~printer:show expected (join left right)
    in
    assert_join
      (parse_tree ["$keys", ["a"]; "a", ["v"]])
      (parse_tree ["$keys", ["b"]; "b", ["other"]])
      ~expected:(parse_tree ["$keys", ["a"; "b"]; "a", ["v"]; "b", ["other"]]);
    assert_join
      (parse_tree ["$keys", ["a"]])
      (create [Part (Path, ([], StringSet.of_list ["a"]))])
      ~expected:(create [Part (Path, ([], StringSet.of_list ["a"]))]);
    assert_join
      (create [Part (Path, ([], StringSet.of_list ["a"]))])
      (parse_tree ["$keys", ["a"]])
      ~expected:(create [Part (Path, ([], StringSet.of_list ["a"]))]);
    ();

    let assert_show ~expected tree = assert_equal ~printer:Fn.id expected (show tree) in
    assert_show ~expected:"[]" (parse_tree []);
    assert_show ~expected:"[a]" (parse_tree ["", ["a"]]);
    assert_show ~expected:"[a, b]" (parse_tree ["", ["a"; "b"]]);
    assert_show ~expected:"{\n   [a] -> [a]\n}" (parse_tree ["a", ["a"]]);
    assert_show ~expected:"{\n   [a] -> [a]\n   [b] -> [b]\n}" (parse_tree ["a", ["a"]; "b", ["b"]]);
    assert_show
      ~expected:"{\n   [a]\n   [b] -> [b]\n   [c][d] -> [c]\n}"
      (parse_tree ["", ["a"]; "b", ["b"]; "c.d", ["c"]]);

    (* limit_to width. *)
    let assert_limit_to ~expected ~width tree =
      assert_equal ~cmp:compare ~printer:show expected (limit_to ~transform:Fn.id ~width tree)
    in
    assert_limit_to
      ~expected:(parse_tree ["a", ["a"]; "b", ["b"]])
      ~width:2
      (parse_tree ["a", ["a"]; "b", ["b"]]);
    assert_limit_to
      ~expected:(parse_tree ["a", ["a"]; "b", ["b"]])
      ~width:3
      (parse_tree ["a", ["a"]; "b", ["b"]]);
    assert_limit_to
      ~expected:(create [Part (Path, ([], StringSet.of_list ["a"; "b"]))])
      ~width:1
      (parse_tree ["a", ["a"]; "b", ["b"]]);
    (* Weak assignment with bottom. *)
    assert_equal
      (parse_tree ["a", ["item"]])
      (assign
         ~weak:true
         ~tree:(parse_tree ["a", ["item"]])
         [AbstractTreeDomain.Label.Index "b"]
         ~subtree:bottom);
    (* collapse *)
    let assert_collapse ~transform ~expected tree =
      let actual =
        collapse ~transform tree
        |> StringSet.fold StringSet.Element ~init:[] ~f:List.cons
        |> List.sort ~compare:String.compare
      in
      assert_equal ~printer:string_list_printer expected actual
    in
    let add_suffix = StringSet.transform StringSet.Element Map ~f:(fun s -> s ^ "#") in
    assert_collapse ~transform:Fn.id ~expected:["a"; "b"] (parse_tree ["a", ["a"]; "b", ["b"]]);
    assert_collapse
      ~transform:add_suffix
      ~expected:["a"; "b#"; "c#"; "de#"]
      (parse_tree ["", ["a"]; "b", ["b"; "c"]; "d.e", ["de"]]);
    (* labels *)
    assert_equal
      ["[b]"; "[a]"]
      (labels (parse_tree ["a", ["x"]; "b", ["y"]]) |> List.map ~f:AbstractTreeDomain.Label.show)
      ~printer:string_list_printer;
    (* transform_non_leaves *)
    let counter = ref 0 in
    let _ =
      parse_tree ["a", ["x"]; "b", ["y"]]
      |> read
           ~transform_non_leaves:(fun _ t ->
             let () = counter := !counter + 1 in
             t)
           [AbstractTreeDomain.Label.Index "a"; AbstractTreeDomain.Label.Index "a"]
    in
    assert_equal 1 !counter


  let test_context _ =
    let tree =
      create
        [
          Part (Path, (parse_path "a.b", StringSet.of_list ["x"; "y"]));
          Part (Path, (parse_path "c", StringSet.of_list ["z"]));
          Part (Path, (parse_path "d", StringSet.of_list ["q"]));
        ]
    in
    let path_value (path, _) value sofar =
      (AbstractTreeDomain.Label.show_path path, value) :: sofar
    in
    assert_equal
      ~printer:string_pair_list_printer
      (List.rev ["[a][b]", "x"; "[a][b]", "y"; "[c]", "z"; "[d]", "q"])
      (reduce StringSet.Element ~using:(Context (Path, Acc)) ~f:path_value ~init:[] tree)
end

module TestTreeDomain = TestAbstractDomain (TreeOfStringSets)

module OverUnderStringSet = struct
  include AbstractOverUnderSetDomain.Make (String)
  open AbstractOverUnderSetDomain

  let unrelated =
    [
      of_approximation [{ element = "a"; in_under = true }; { element = "b"; in_under = true }];
      of_approximation [{ element = "a"; in_under = true }; { element = "c"; in_under = true }];
      of_approximation [{ element = "b"; in_under = true }; { element = "c"; in_under = true }];
    ]


  let values = List.cartesian_product unrelated unrelated |> List.map ~f:(Tuple2.uncurry join)

  let show_element { element; in_under } =
    if in_under then
      element
    else
      element ^ "-"


  let gather_result_elements set =
    fold ElementAndUnder ~init:[] ~f:(fun element result -> show_element element :: result) set
    |> List.sort ~compare:String.compare


  let gather_results set =
    fold Element ~init:[] ~f:(fun element result -> element :: result) set
    |> List.sort ~compare:String.compare


  let add set e = add e set

  let test_fold _ =
    let test_elements expected =
      let element_set = List.fold ~init:bottom ~f:add expected in
      let actual = gather_result_elements element_set in
      assert_equal expected actual ~printer:string_list_printer
    in
    let test expected =
      let element_set = List.fold ~init:bottom ~f:add expected in
      let actual = gather_results element_set in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ["a"];
    test ["a"; "b"];
    test ["a"; "b"; "c"];
    test_elements ["a"];
    test_elements ["a"; "b"];
    test_elements ["a"; "b"; "c"]


  let test_transform _ =
    let test_elements ~initial ~by ~f ~expected =
      let element_set = List.fold ~init:bottom ~f:add initial in
      let actual = transform by Map ~f element_set |> gather_result_elements in
      assert_equal expected actual ~printer:string_list_printer
    in
    let test ~initial ~by ~op ~f ~expected =
      let element_set = List.fold ~init:bottom ~f:add initial in
      let actual = transform by op ~f element_set |> gather_result_elements in
      assert_equal expected actual ~printer:string_list_printer
    in
    test
      ~initial:["a"; "b"]
      ~by:Element
      ~op:Map
      ~f:(fun element -> "t." ^ element)
      ~expected:["t.a"; "t.b"];
    test ~initial:["a"; "b"] ~by:Element ~op:Add ~f:"c" ~expected:["a"; "b"; "c"];
    test ~initial:[] ~by:Self ~op:Map ~f:(fun set -> add set "c") ~expected:["c"];
    test_elements
      ~initial:["a"; "b"]
      ~by:ElementAndUnder
      ~f:(fun { element; in_under } -> { element = "t." ^ element; in_under })
      ~expected:["t.a"; "t.b"];
    test_elements
      ~initial:["a"; "b"]
      ~by:Self
      ~f:(fun set -> add_set ~to_add:(of_approximation [{ element = "c"; in_under = false }]) set)
      ~expected:["a"; "b"; "c-"];
    test_elements
      ~initial:[]
      ~by:Self
      ~f:(fun set -> add_set ~to_add:(of_approximation [{ element = "c"; in_under = false }]) set)
      ~expected:["c-"]


  let test_partition _ =
    let test_elements ~initial ~f ~expected =
      let element_set = List.fold ~init:bottom ~f:add initial in
      let actual =
        partition ElementAndUnder By ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements = gather_result_elements data in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    let test ~initial ~f ~expected =
      let element_set = List.fold ~init:bottom ~f:add initial in
      let actual =
        partition Element By ~f element_set
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements = gather_result_elements data in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test
      ~initial:["abc"; "bef"; "g"]
      ~f:(fun element -> String.length element)
      ~expected:[1, ["g"]; 3, ["abc"; "bef"]];
    test_elements
      ~initial:["abc"; "bef"; "g"]
      ~f:(fun { element; _ } -> String.length element)
      ~expected:[1, ["g"]; 3, ["abc"; "bef"]]


  let from elements = List.fold ~init:bottom ~f:add elements

  let test_create _ =
    assert_equal
      (of_approximation
         [
           { element = "a"; in_under = true };
           { element = "b"; in_under = true };
           { element = "c"; in_under = false };
         ])
      (create
         [
           Part
             ( Self,
               of_approximation
                 [
                   { element = "a"; in_under = true };
                   { element = "b"; in_under = true };
                   { element = "c"; in_under = false };
                 ] );
         ])
      ~printer:show;
    assert_equal
      (of_approximation
         [
           { element = "a"; in_under = true };
           { element = "b"; in_under = true };
           { element = "c"; in_under = false };
         ])
      (create
         [
           Part (ElementAndUnder, { element = "a"; in_under = true });
           Part (ElementAndUnder, { element = "b"; in_under = true });
           Part (ElementAndUnder, { element = "c"; in_under = false });
         ])
      ~printer:show;
    assert_equal
      (from ["a"; "b"; "c"])
      (create [Part (Self, of_list ["a"; "b"; "c"])])
      ~printer:show;
    assert_equal
      (from ["a"; "b"; "c"])
      (create [Part (Element, "a"); Part (Element, "b"); Part (Element, "c")])
      ~printer:show


  let compare left right = less_or_equal ~left ~right && less_or_equal ~left:right ~right:left

  let test_additional _ =
    let set_a =
      of_approximation
        [
          { element = "a"; in_under = true };
          { element = "b"; in_under = true };
          { element = "c"; in_under = true };
        ]
    in
    let set_a_over =
      of_approximation
        [
          { element = "a"; in_under = false };
          { element = "b"; in_under = false };
          { element = "c"; in_under = false };
        ]
    in
    let set_b =
      of_approximation [{ element = "a"; in_under = true }; { element = "b"; in_under = true }]
    in
    let set_c = join set_a set_b in
    let set_d = of_approximation [{ element = "c"; in_under = true }] in
    assert_equal
      (of_approximation
         [
           { element = "a"; in_under = true };
           { element = "b"; in_under = true };
           { element = "c"; in_under = false };
         ])
      set_c
      ~printer:show
      ~cmp:compare;
    assert_equal set_a (join set_a bottom) ~printer:show ~cmp:compare;
    assert_equal set_a (join bottom set_a) ~printer:show ~cmp:compare;
    assert_equal set_a_over (join set_a empty) ~printer:show ~cmp:compare;
    assert_equal set_a_over (join empty set_a) ~printer:show ~cmp:compare;

    (* Test that adding does not use join *)
    assert_equal set_a (transform Element Add ~f:"c" set_c) ~printer:show ~cmp:compare;
    assert_equal set_a (transform Self Add ~f:set_d set_c) ~printer:show ~cmp:compare;
    assert_equal set_c (transform Self Add ~f:set_c set_c) ~printer:show ~cmp:compare;
    assert_equal empty (transform Element Map ~f:Fn.id empty) ~printer:show ~cmp:compare;
    assert_equal
      (of_approximation [{ element = "a"; in_under = true }])
      (transform Element Map ~f:(fun _ -> "a") set_c)
      ~printer:show
      ~cmp:compare;

    (* Test subtract *)
    assert_equal bottom (subtract set_a ~from:set_a) ~printer:show ~cmp:compare;
    assert_equal set_a (subtract set_a_over ~from:set_a) ~printer:show ~cmp:compare;
    assert_equal empty (subtract set_a ~from:set_a_over) ~printer:show ~cmp:compare;
    assert_equal set_b (subtract set_a ~from:set_b) ~printer:show ~cmp:compare;
    assert_equal set_a (subtract set_b ~from:set_a) ~printer:show ~cmp:compare;
    assert_equal set_b (subtract set_a_over ~from:set_b) ~printer:show ~cmp:compare;
    assert_equal
      (of_approximation [{ element = "c"; in_under = false }])
      (subtract set_b ~from:set_a_over)
      ~printer:show
      ~cmp:compare;

    (* Test empty *)
    assert_bool "bottom is empty" (is_empty bottom);
    assert_bool "empty is empty" (is_empty empty);
    assert_bool "subtraction is empty" (is_empty (subtract set_a ~from:set_a_over));

    (* Test expand *)
    assert_equal
      set_a
      (transform Element Expand ~f:(fun e -> [e; "c"]) set_b)
      ~printer:show
      ~cmp:compare


  let test_context _ = ()
end

module TestOverUnderStringSet = TestAbstractDomain (OverUnderStringSet)

module FlatString = struct
  include AbstractFlatDomain.Make (String)

  let unrelated = [make "a"; make "b"; make "c"; make "d"]

  let values = unrelated

  let test_fold _ =
    let test value expected =
      let actual = fold Element ~init:[] ~f:List.cons value |> List.sort ~compare:String.compare in
      assert_equal expected actual ~printer:string_list_printer
    in
    test (make "a") ["a"];
    test bottom [];
    test top []


  let test_transform _ =
    let test ~initial ~by ~f ~expected =
      let element = make initial in
      let actual =
        transform by Map ~f element
        |> fold Element ~init:[] ~f:List.cons
        |> List.sort ~compare:String.compare
      in
      assert_equal expected actual ~printer:string_list_printer
    in
    test ~initial:"a" ~by:Element ~f:(fun x -> "t." ^ x) ~expected:["t.a"]


  let test_partition _ =
    let test ~initial ~f ~expected =
      let element = make initial in
      let actual =
        partition Element By ~f element
        |> MapPoly.fold ~init:[] ~f:(fun ~key ~data result ->
               let elements =
                 fold Element ~init:[] ~f:List.cons data |> List.sort ~compare:String.compare
               in
               (key, elements) :: result)
        |> List.sort ~compare:Poly.compare
      in
      assert_equal expected actual ~printer:int_string_list_list_printer
    in
    test ~initial:"abc" ~f:(fun x -> String.length x) ~expected:[3, ["abc"]]


  let test_create _ = assert_equal (make "a") (create [Part (Element, "a")]) ~printer:show

  let test_additional _ =
    let () =
      assert_equal "Flat(strings)" (introspect Structure |> String.concat ~sep:"\n") ~printer:Fn.id
    in
    ()


  let test_context _ = ()
end

module TestFlatString = TestAbstractDomain (FlatString)

module TestProductAmbiguousPart = struct
  module StringSet = AbstractSetDomain.Make (String)

  module Slots = struct
    type 'a slot =
      | Left : StringSet.t slot
      | Right : StringSet.t slot

    let slots = 2

    let slot_name (type a) (slot : a slot) =
      match slot with
      | Left -> "left"
      | Right -> "right"


    let slot_domain (type a) (slot : a slot) =
      match slot with
      | Left -> (module StringSet : AbstractDomain.S with type t = a)
      | Right -> (module StringSet : AbstractDomain.S with type t = a)


    let strict (type a) (_slot : a slot) = false
  end

  include AbstractProductDomain.Make (Slots)

  let suite () =
    let assert_raises_any f =
      try
        let _ = f () in
        assert_failure "Expected an exception to be raised"
      with
      | _ -> ()
    in
    let test_create _ =
      assert_raises_any (fun () -> create [Part (StringSet.Self, StringSet.bottom)])
    in
    let test_transform _ =
      assert_raises_any (fun () -> transform StringSet.Self Map ~f:(StringSet.add "x") bottom)
    in
    let test_update_get _ =
      let x = StringSet.singleton "x" in
      assert_equal (bottom |> update Slots.Left x |> get Slots.Left) x;
      assert_equal (bottom |> update Slots.Right x |> get Slots.Right) x;
      assert_equal (bottom |> update Slots.Left x |> get Slots.Right) StringSet.bottom;
      assert_equal (bottom |> update Slots.Right x |> get Slots.Left) StringSet.bottom
    in
    [
      "test_create" >:: test_create;
      "test_transform" >:: test_transform;
      "test_update_get" >:: test_update_get;
    ]
end

let () =
  "abstractDomainTest"
  >::: [
         "string_set" >::: TestStringSet.suite ();
         "topped_string_set" >::: TestToppedStringSet.suite ();
         "inverted_string_set" >::: TestInvertedStringSet.suite ();
         "map_int_to_string_set" >::: TestIntToStringSet.suite ();
         "nonstrict_int_to_string_set" >::: TestNonStrictIntToStringSet.suite ();
         "string_x_maps_int_to_string_set" >::: TestPair.suite ();
         "dual_string" >::: TestPairStringString.suite ();
         "abstract_element" >::: TestAbstractElement.suite ();
         "abstract_bucketed_element" >::: TestAbstractBucketedElement.suite ();
         "product" >::: TestProductDomain.suite ();
         "simple" >::: TestSimpleDomain.suite ();
         "tree" >::: TestTreeDomain.suite ();
         "string_biset" >::: TestOverUnderStringSet.suite ();
         "flat_string" >::: TestFlatString.suite ();
         "product_ambiguous_part" >::: TestProductAmbiguousPart.suite ();
       ]
  |> run_test_tt_main
