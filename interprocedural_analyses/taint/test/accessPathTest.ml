(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core
open Taint
open Test

let test_of_expression context =
  let ( !+ ) expression = Test.parse_single_expression expression in
  let assert_of_expression ~resolution expression expected =
    assert_equal
      ~cmp:(Option.equal AccessPath.equal)
      ~printer:(function
        | None -> "None"
        | Some access_path -> AccessPath.show access_path)
      expected
      (AccessPath.of_expression ~resolution expression)
  in
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  assert_of_expression
    ~resolution
    !+"a"
    (Some { AccessPath.root = AccessPath.Root.Variable "a"; path = [] });
  assert_of_expression
    ~resolution
    !+"a.b"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "a";
         path = [Abstract.TreeDomain.Label.Field "b"];
       });
  assert_of_expression
    ~resolution
    !+"a.b.c"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "a";
         path = [Abstract.TreeDomain.Label.Field "b"; Abstract.TreeDomain.Label.Field "c"];
       });
  assert_of_expression ~resolution !+"a.b.call()" None;

  let resolution =
    ScratchProject.setup ~context ["qualifier.py", "unannotated = unknown_value()"]
    |> ScratchProject.build_resolution
  in
  assert_of_expression
    ~resolution
    !"$local_qualifier$unannotated"
    (Some
       {
         AccessPath.root = AccessPath.Root.Variable "qualifier";
         path = [Abstract.TreeDomain.Label.Field "unannotated"];
       });
  assert_of_expression
    ~resolution
    !"$local_qualifier$missing"
    (Some { AccessPath.root = AccessPath.Root.Variable "$local_qualifier$missing"; path = [] })


let test_match_actuals_to_formals _ =
  let open Ast.Statement in
  let open Ast.Expression in
  let positional ?(actual_path = []) ?(positional_only = false) (position, name) =
    {
      AccessPath.root = AccessPath.Root.PositionalParameter { position; name; positional_only };
      actual_path;
      formal_path = [];
    }
  in

  let starred ~position ~formal_path =
    {
      AccessPath.root = AccessPath.Root.StarParameter { position };
      actual_path = [];
      formal_path = [Abstract.TreeDomain.Label.Field (Int.to_string formal_path)];
    }
  in
  let double_starred ?(excluded = []) formal_path =
    {
      AccessPath.root = AccessPath.Root.StarStarParameter { excluded };
      actual_path = [];
      formal_path = [Abstract.TreeDomain.Label.Field formal_path];
    }
  in
  let named ?(actual_path = []) name =
    { AccessPath.root = AccessPath.Root.NamedParameter { name }; actual_path; formal_path = [] }
  in
  let assert_match ~signature ~call ~expected =
    let actuals = Test.parse_single_call call |> fun { Call.arguments; _ } -> arguments in
    let formals =
      Test.parse_single_define signature
      |> (fun { Define.signature = { Define.Signature.parameters; _ }; _ } -> parameters)
      |> AccessPath.Root.normalize_parameters
      |> List.map ~f:(fun (normalized, _, _) -> normalized)
    in
    let sort =
      let compare (left_expression, left_matches) (right_expression, right_matches) =
        match String.compare left_expression right_expression with
        | 0 -> List.compare AccessPath.compare_argument_match left_matches right_matches
        | comparison -> comparison
      in
      List.sort ~compare
    in
    let actual =
      AccessPath.match_actuals_to_formals actuals formals
      |> List.map ~f:(fun (expression, matches) -> Expression.show expression, matches)
    in
    let printer items =
      List.map items ~f:(fun (expression, matches) ->
          expression
          ^ ": "
          ^ (List.map ~f:AccessPath.show_argument_match matches |> String.concat ~sep:", "))
      |> String.concat ~sep:"\n"
    in
    assert_equal ~printer (sort expected) (sort actual)
  in
  assert_match ~signature:"def foo(x): ..." ~call:"foo(1)" ~expected:["1", [positional (0, "x")]];
  assert_match ~signature:"def foo(x): ..." ~call:"foo(x=1)" ~expected:["1", [positional (0, "x")]];
  assert_match
    ~signature:"def foo(*args): ..."
    ~call:"foo(1)"
    ~expected:["1", [starred ~position:0 ~formal_path:0]];
  assert_match ~signature:"def foo(*args): ..." ~call:"foo(x=1)" ~expected:["1", []];
  assert_match
    ~signature:"def foo(*args): ..."
    ~call:"foo(1, foo)"
    ~expected:
      ["1", [starred ~position:0 ~formal_path:0]; "foo", [starred ~position:0 ~formal_path:1]];
  assert_match
    ~signature:"def foo(x, *args): ..."
    ~call:"foo(1, 2, 3)"
    ~expected:
      [
        "1", [positional (0, "x")];
        "2", [starred ~position:1 ~formal_path:0];
        "3", [starred ~position:1 ~formal_path:1];
      ];
  assert_match
    ~signature:"def foo(x, y): ..."
    ~call:"foo(*[1, 2, 3, 4])"
    ~expected:
      [
        ( "*[1, 2, 3, 4]",
          [
            positional ~actual_path:[Abstract.TreeDomain.Label.Field "1"] (1, "y");
            positional ~actual_path:[Abstract.TreeDomain.Label.Field "0"] (0, "x");
          ] );
      ];
  assert_match
    ~signature:"def foo(**kwargs): ..."
    ~call:"foo(x=1)"
    ~expected:["1", [double_starred "x"]];
  assert_match
    ~signature:"def foo(a, b, *rest, c, d, **kw): ..."
    ~call:"foo(1, 2, 3, *[4], 5, c = 6, q = 7, r = 8, **{9:9})"
    ~expected:
      [
        "1", [positional (0, "a")];
        "2", [positional (1, "b")];
        "3", [starred ~position:2 ~formal_path:0];
        ( "*[4]",
          [
            {
              AccessPath.root = AccessPath.Root.StarParameter { position = 2 };
              actual_path = [];
              formal_path = [];
            };
          ] );
        ( "5",
          [
            {
              AccessPath.root = AccessPath.Root.StarParameter { position = 2 };
              actual_path = [];
              formal_path = [Abstract.TreeDomain.Label.Any];
            };
          ] );
        "6", [named "c"];
        "7", [double_starred ~excluded:["d"; "c"; "b"; "a"] "q"];
        "8", [double_starred ~excluded:["d"; "c"; "b"; "a"] "r"];
        ( "**{ 9:9 }",
          [
            {
              AccessPath.root = AccessPath.Root.StarStarParameter { excluded = ["d"; "c"; "b"; "a"] };
              actual_path = [];
              formal_path = [];
            };
            named ~actual_path:[Abstract.TreeDomain.Label.Field "d"] "d";
          ] );
      ];
  assert_match
    ~signature:"def foo(x): ..."
    ~call:"foo(**{'x': 1})"
    ~expected:
      [{|**{ "x":1 }|}, [positional ~actual_path:[Abstract.TreeDomain.Label.Field "x"] (0, "x")]];
  assert_match
    ~signature:"def foo(x, /): ..."
    ~call:"foo(1)"
    ~expected:["1", [positional ~positional_only:true (0, "x")]];
  ()


let () =
  "accessPath"
  >::: [
         "of_expression" >:: test_of_expression;
         "match_actuals_to_formals" >:: test_match_actuals_to_formals;
       ]
  |> Test.run
