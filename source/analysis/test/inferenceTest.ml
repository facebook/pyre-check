(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Inference
open Test

let configuration = Configuration.Analysis.create ~infer:true ~source_path:[] ()

let assert_backward ~resolution precondition statement postcondition =
  let module State = State (struct
    let qualifier = Reference.empty

    let configuration = configuration

    let define = +mock_define
  end)
  in
  let create annotations =
    let resolution =
      let annotation_store =
        let annotify (name, annotation) =
          let annotation =
            let create annotation = RefinementUnit.create ~base:(Annotation.create annotation) () in
            create annotation
          in
          !&name, annotation
        in
        List.map annotations ~f:annotify |> Reference.Map.of_alist_exn
      in
      Resolution.with_annotation_store resolution ~annotation_store
    in
    State.create ~resolution ()
  in
  let assert_state_equal =
    assert_equal
      ~cmp:State.equal
      ~printer:(Format.asprintf "%a" State.pp)
      ~pp_diff:(diff ~print:State.pp)
  in
  let parsed =
    parse statement
    |> function
    | { Source.statements; _ } -> statements
  in
  assert_state_equal
    (create postcondition)
    (List.fold_right
       ~f:(fun statement state -> State.backward ~key:Cfg.exit_index state ~statement)
       ~init:(create precondition)
       parsed)


let test_backward context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let assert_backward = assert_backward ~resolution in
  assert_backward ["y", Type.integer] "pass" ["y", Type.integer];

  (* Assignments. *)
  assert_backward ["x", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_backward ["y", Type.integer] "x = z" ["y", Type.integer];
  assert_backward ["x", Type.integer] "x += 1" ["x", Type.integer];
  assert_backward ["x", Type.integer] "x = y = z" ["x", Type.integer; "z", Type.integer];
  assert_backward
    ["x", Type.Primitive "B"; "y", Type.Primitive "C"]
    "x = y = z"
    ["x", Type.Primitive "B"; "y", Type.Primitive "C"; "z", Type.Primitive "B"];
  assert_backward ["a", Type.integer] "a, b = c, d" ["a", Type.integer; "c", Type.integer];
  assert_backward ["a", Type.Top; "b", Type.integer] "a = b" ["a", Type.Top; "b", Type.integer];

  (* Tuples *)
  assert_backward
    ["x", Type.integer; "y", Type.string]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_backward
    ["x", Type.tuple [Type.integer; Type.string]]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.string]; "y", Type.integer; "z", Type.string];

  (* Literals. *)
  assert_backward [] "x = 1.0" [];
  assert_backward [] "x = 'string'" [];
  assert_backward ["x", Type.Primitive "Foo"] "x = 'string'" ["x", Type.Primitive "Foo"];
  assert_backward ["x", Type.Primitive "Foo"] "x = 'string'" ["x", Type.Primitive "Foo"];

  (* Calls *)
  assert_backward [] "int_to_str(x)" ["x", Type.integer];
  assert_backward [] "str_float_to_int(x, y)" ["x", Type.string; "y", Type.float];
  assert_backward [] "str_float_tuple_to_int(t)" ["t", Type.tuple [Type.string; Type.float]];
  assert_backward ["x", Type.string] "unknown_to_int(x)" ["x", Type.string];
  assert_backward ["x", Type.float] "x = int_to_str(x)" ["x", Type.integer];
  assert_backward ["y", Type.float] "y = int_to_str(x)" ["y", Type.float; "x", Type.integer];
  assert_backward ["y", Type.integer] "y = int_to_str(x)" ["y", Type.integer; "x", Type.integer];
  assert_backward [] "str_float_to_int(x)" ["x", Type.string];
  assert_backward [] "str_float_to_int(x, 1.0)" ["x", Type.string];
  assert_backward [] "'a'.substr(x)" ["x", Type.integer];
  assert_backward
    ["y", Type.float]
    "y = obj.static_int_to_str(x)"
    ["y", Type.float; "x", Type.integer];
  assert_backward [] "str_float_tuple_to_int((x, y))" ["x", Type.string; "y", Type.float];
  assert_backward
    []
    "nested_tuple_to_int(((x, y), z))"
    ["x", Type.string; "y", Type.float; "z", Type.float];
  assert_backward
    [
      ( "cb",
        Type.Callable.create
          ~parameters:(Defined [Named { name = "arg"; annotation = Type.integer; default = false }])
          ~annotation:Type.none
          () );
    ]
    "cb(x)"
    [
      ( "cb",
        Type.Callable.create
          ~parameters:(Defined [Named { name = "arg"; annotation = Type.integer; default = false }])
          ~annotation:Type.none
          () );
      "x", Type.integer;
    ];
  assert_backward
    [
      ( "cb",
        Type.parametric
          "BoundMethod"
          [
            Single
              (Type.Callable.create
                 ~parameters:
                   (Defined
                      [
                        Named { name = "self"; annotation = Type.string; default = false };
                        Named { name = "arg"; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.none
                 ());
            Single Type.string;
          ] );
    ]
    "cb(x)"
    [
      ( "cb",
        Type.parametric
          "BoundMethod"
          [
            Single
              (Type.Callable.create
                 ~parameters:
                   (Defined
                      [
                        Named { name = "self"; annotation = Type.string; default = false };
                        Named { name = "arg"; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.none
                 ());
            Single Type.string;
          ] );
      "x", Type.integer;
    ];

  (* TODO(T84365830): Extend implementation to pass starred and unstarred tests *)
  assert_backward [] "str_float_to_int(*(x, y))" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "str_float_to_int(**{'s': x, 'f': y})" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "star_int_to_int(*[], y)" []


let () = "inference" >::: ["backward" >:: test_backward] |> Test.run
