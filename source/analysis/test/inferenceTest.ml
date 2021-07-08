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


let get_inference_errors ~context source =
  let source, configuration, ast_environment, global_resolution =
    let project = ScratchProject.setup ~context ["test.py", source] in
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.build_global_environment project
    in
    let configuration = ScratchProject.configuration_of project in
    let ast_environment =
      AnnotatedGlobalEnvironment.ast_environment global_environment |> AstEnvironment.read_only
    in
    let source =
      AstEnvironment.ReadOnly.get_processed_source ast_environment (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    ( source,
      { configuration with infer = true },
      ast_environment,
      AnnotatedGlobalEnvironment.read_only global_environment |> GlobalResolution.create )
  in
  Inference.run ~configuration ~global_resolution ~source
  |> List.map
       ~f:
         (AnalysisError.instantiate
            ~show_error_traces:false
            ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))


let test_infer_error_fields context =
  let assert_infer ?(fields = ["description"]) source errors =
    let fields_of_error error =
      let field_of_error field =
        let access_field body field =
          match body with
          | `Assoc list ->
              List.Assoc.find ~equal:String.equal list field |> Option.value ~default:`Null
          | _ -> `String "TEST FAIL: ERROR ACCESSING FIELD IN ERROR JSON"
        in
        List.fold
          ~init:(AnalysisError.Instantiated.to_yojson error)
          ~f:access_field
          (String.split ~on:'.' field)
      in
      List.map fields ~f:field_of_error
    in
    let to_string json = Yojson.Safe.sort json |> Yojson.Safe.to_string in
    let infer_errors = get_inference_errors ~context source in
    Memory.reset_shared_memory ();
    assert_equal
      ~cmp:(List.equal String.equal)
      ~printer:(fun errors -> Format.asprintf "%a" Sexp.pp [%message (errors : string list)])
      ~pp_diff:
        (diff ~print:(fun format errors ->
             Format.fprintf format "%a" Sexp.pp [%message (errors : string list)]))
      (List.map ~f:(fun string -> Yojson.Safe.from_string string |> to_string) errors)
      (List.map ~f:fields_of_error infer_errors |> List.concat |> List.map ~f:to_string)
  in
  assert_infer
    ~fields:["inference.parent"]
    {|
      class Test(object):
          def ret_int(self):
              return 5
    |}
    [{|"test.Test"|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def with_params(x: int, y):
          return 5
    |}
    [{|[{"name":"x","type":"int","value":null},{"name":"y","type":null,"value":null}]|}];
  assert_infer
    ~fields:["inference.decorators"]
    {|
      @staticmethod
      def returns_int():
          return 5
    |}
    [{|["staticmethod"]|}];
  assert_infer
    ~fields:["inference.annotation"; "inference.parameters"]
    {|
      def with_params(x = 5) -> int:
          return x
    |}
    [{|"int"|}; {|[{"name":"x","type":"int","value":"5"}]|}];
  assert_infer
    ~fields:["inference.annotation"]
    {|
      def ret_none(x):
          pass
    |}
    [{|"None"|}];
  assert_infer
    ~fields:["inference.async"]
    {|
      async def async_test ():
          return 5
    |}
    [{|true|}];
  (* TODO(T84365830): Should be A.C. Fix dequalify implementation. *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      import A
      from A import C
      from B import C
      def test_bad_import(x: A.C):
          return 5
    |}
    [{|[{"name":"x","type":"C","value":null}]|}];
  assert_infer
    ~fields:["inference.decorators"]
    {|
      @click.argument("config-path", type=click.Path(exists=True, readable=True))
      def foo(x: bool):
          return ""
    |}
    [{|["click.argument(\"config-path\", type = click.Path(exists = True, readable = True))"]|}]


let () =
  "inference"
  >::: ["backward" >:: test_backward; "infer_fields" >:: test_infer_error_fields]
  |> Test.run
