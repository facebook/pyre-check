(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement
open Inference

open Test


let configuration = Configuration.Analysis.create ~infer:true ()


let create
    ?(define = Test.mock_define)
    ?(expected_return = Type.Top)
    ?(immutables = [])
    annotations =
  let resolution =
    let annotations =
      let immutables = String.Map.of_alist_exn immutables in
      let annotify (name, annotation) =
        let annotation =
          let create annotation =
            match Map.find immutables name with
            | Some global -> Annotation.create_immutable ~global annotation
            | _ -> Annotation.create annotation
          in
          create annotation
        in
        Access.create name, annotation
      in
      List.map annotations ~f:annotify
      |> Access.Map.of_alist_exn
    in
    Resolution.with_annotations (Test.resolution ()) ~annotations
  in
  let define =
    +{
      define with
      Define.return_annotation = Some (Type.expression expected_return);
    }
  in
  State.create ~resolution ~define ()


let assert_backward precondition statement postcondition =
  let assert_state_equal =
    assert_equal
      ~cmp:State.equal
      ~printer:(Format.asprintf "%a" State.pp)
      ~pp_diff:(diff ~print:State.pp)
  in
  let parsed =
    (parse statement)
    |> function
    | { Source.statements = statements; _ } -> statements
  in
  assert_state_equal
    (create postcondition)
    (List.fold_right
       ~f:(fun statement state -> State.backward state ~statement)
       ~init:(create precondition)
       parsed)


let test_backward _ =
  assert_backward ["y", Type.integer] "pass" ["y", Type.integer];

  (* Assignments. *)
  assert_backward ["x", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_backward ["y", Type.integer] "x = z" ["y", Type.integer];

  assert_backward ["x", Type.integer] "x += 1" ["x", Type.integer];

  assert_backward ["x", Type.integer] "x = y = z" ["x", Type.integer; "z", Type.integer];
  assert_backward
    ["x", Type.Primitive ~~"B"; "y", Type.Primitive ~~"C"]
    "x = y = z"
    [
      "x", Type.Primitive ~~"B";
      "y", Type.Primitive ~~"C";
      "z", Type.Primitive ~~"B";
    ];

  assert_backward
    ["a", Type.integer]
    "a, b = c, d"
    ["a", Type.integer; "c", Type.integer];
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
  assert_backward ["x", Type.Primitive ~~"Foo"] "x = 'string'" ["x", Type.Primitive ~~"Foo"];
  assert_backward ["x", Type.Primitive ~~"Foo"] "x = 'string'" ["x", Type.Primitive ~~"Foo"];

  (* Calls *)
  assert_backward [] "int_to_str(x)" ["x", Type.integer];
  assert_backward [] "str_float_to_int(x, y)" ["x", Type.string; "y", Type.float];
  assert_backward
    []
    "str_float_tuple_to_int(t)"
    ["t", Type.tuple [Type.string; Type.float]];
  assert_backward ["x", Type.string] "unknown_to_int(x)" ["x", Type.string];
  assert_backward ["x", Type.float] "x = int_to_str(x)" ["x", Type.integer];
  assert_backward ["y", Type.float] "y = int_to_str(x)" ["y", Type.float; "x", Type.integer];
  assert_backward ["y", Type.integer] "y = int_to_str(x)" ["y", Type.integer; "x", Type.integer];
  assert_backward [] "str_float_to_int(x)" [];
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
  (* TODO(T25072735): Extend implementation to pass starred and unstarred tests *)
  assert_backward [] "str_float_to_int(*(x, y))" []; (* "x", Type.string; "y", Type.float *)
  assert_backward
    []
    "str_float_to_int(**{'s': x, 'f': y})"
    []; (* "x", Type.string; "y", Type.float *)
  assert_backward
    []
    "star_int_to_int(*[], y)"
    [] (* We don't yet infer y, which comes after starred param *)


let fixpoint_parse source =
  parse source
  |> Preprocessing.preprocess
  |> Preprocessing.defines
  |> List.hd_exn


let test_fixpoint_backward _ =
  let assert_fixpoint_backward source expected =
    let { Node.value = define; _ } as define_node = fixpoint_parse source in
    assert_equal
      ~cmp:(Fixpoint.equal ~f:State.equal)
      ~printer:(fun fixpoint -> Format.asprintf "%a" Fixpoint.pp fixpoint)
      ~pp_diff:(diff ~print:Fixpoint.pp)
      expected
      (Inference.backward_fixpoint
         (Cfg.create define)
         ~initial_forward:
           (State.initial
              ~resolution:(Test.resolution ())
              define_node)
         ~initialize_backward:(Inference.State.initial_backward define_node))
  in
  assert_fixpoint_backward
    {| def foo(): pass |}
    (Int.Table.of_alist_exn [
        0, create ["$return", Type.Top]; (* Entry *)
        1, create ["$return", Type.Top]; (* Normal *)
        2, create ["$return", Type.Top]; (* Error *)
        3, create ["$return", Type.Top]; (* Exit *)
        5, create ["$return", Type.Top]; (* Pass *)
      ]);
  assert_fixpoint_backward
    {| def foo() -> int: pass |}
    (Int.Table.of_alist_exn [
        0, create ~expected_return:Type.integer ["$return", Type.integer];
        1, create ~expected_return:Type.integer ["$return", Type.integer];
        2, create ~expected_return:Type.integer ["$return", Type.integer];
        3, create ~expected_return:Type.integer ["$return", Type.integer];
        5, create ~expected_return:Type.integer ["$return", Type.integer];
      ]);
  assert_fixpoint_backward
    {|
     def foo() -> int:
       x = y
       return x
    |}
    (Int.Table.of_alist_exn [
        0,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer; "$local_foo$x", Type.integer; "y", Type.integer];
        1,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        2,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        3,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        5,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
      ]);
  assert_fixpoint_backward
    {|
     def foo() -> int:
       z = y
       x = y
       return y
    |}
    (Int.Table.of_alist_exn [
        0,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer; "y", Type.integer];
        1,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        2,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        3,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        5,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
      ]);
  assert_fixpoint_backward
    {|
       def foo() -> int:
         x = y
         x = z
         return x
      |}
    (Int.Table.of_alist_exn [
        0,
        create
          ~expected_return:Type.integer
          [
            "$return", Type.integer;
            "$local_foo$x", Type.integer;
            "y", Type.integer;
            "z", Type.integer;
          ];
        1,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        2,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        3,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        5,
        create
          ~expected_return:Type.integer
          ["$return", Type.integer];
      ])


let test_check_missing_parameter _ =
  let assert_inference_errors =
    let check ~configuration ~environment ~source =
      let { TypeCheck.Result.errors; _ } =
        Inference.infer
          ~configuration
          ~environment
          ~source
      in
      errors
    in
    assert_errors ~infer:true ~check
  in

  assert_inference_errors
    {|
      def foo(x = 5) -> int:
        return x
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];

  assert_inference_errors
    {|
      def foo(x: typing.Any) -> None:
        x = 5
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified."];

  assert_inference_errors
    {|
      def foo(x: typing.Any = 5) -> None:
        pass
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified."]


let assert_infer
    ?(debug = false)
    ?(infer = true)
    ?(show_error_traces = false)
    ?(recursive_infer = false)
    ?(fields = ["description"])
    source
    errors =
  let check_errors configuration environment source =
    let { TypeCheck.Result.errors; _ } =
      Inference.infer
        ~configuration
        ~environment
        ~source
    in
    errors
  in
  let fields_of_error error =
    let field_of_error field =
      let access_field body field =
        match body with
        | `Assoc list ->
            List.Assoc.find ~equal:String.equal list field
            |> Option.value ~default:`Null
        | _ -> `String "TEST FAIL: ERROR ACCESSING FIELD IN ERROR JSON"
      in
      List.fold
        ~init:(Error.to_json ~detailed:show_error_traces error)
        ~f:access_field (String.split ~on:'.' field)
    in
    List.map fields ~f:field_of_error
  in
  let source =
    parse source
    |> Preprocessing.preprocess in
  let configuration = Configuration.Analysis.create ~debug ~infer ~recursive_infer () in
  let environment = Test.environment () in
  Service.Environment.populate ~configuration environment [source];
  let to_string json =
    Yojson.Safe.sort json
    |> Yojson.Safe.to_string
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(fun errors -> Format.asprintf "%a" Sexp.pp [%message (errors: string list)])
    ~pp_diff:
      (diff
         ~print:(fun format errors ->
             Format.fprintf format "%a" Sexp.pp [%message (errors: string list)]))
    (List.map ~f:(fun string -> Yojson.Safe.from_string string |> to_string) errors)
    (List.map
       ~f:fields_of_error
       (check_errors configuration  environment source)
     |> List.concat
     |> List.map ~f:to_string)


let test_infer _ =
  (* TODO(T37338460): Unbreak inference of self parameter when it is returned. *)
  assert_infer
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    [];

  assert_infer ~fields:["inference.parent"]
    {|
      class Test(object):
          def ret_int(self):
              return 5
    |}
    [{|"Test"|}];

  assert_infer
    {|
      def returns_int ():
          return 5
    |}
    ["\"Missing return annotation [3]: Returning `int` but no return type is specified.\""];

  assert_infer
    {|
      def returns_dict ():
          return {}
    |}
    [
      "\"Missing return annotation [3]: Returning `typing.Dict[]` " ^
      "but no return type is specified.\""
    ];

  assert_infer ~fields:["inference.parameters"]
    {|
      def with_params (x: int, y):
          return 5
    |}
    [{|[{"name":"x","type":"int","value":null},{"name":"y","type":null,"value":null}]|}];

  assert_infer
    {|
      def return_string ():
          return "Hello"
    |}
    ["\"Missing return annotation [3]: Returning `str` but no return type is specified.\""];

  assert_infer
    {|
      def return_bool ():
          return False
    |}
    ["\"Missing return annotation [3]: Returning `bool` but no return type is specified.\""];

  assert_infer
    {|
      def return_float ():
          return 1.0
    |}
    ["\"Missing return annotation [3]: Returning `float` but no return type is specified.\""];

  assert_infer
    {|
      def return_both ():
          if condition():
              return 5
          else:
              return "Hello"
    |}
    [
      "\"Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is " ^
      "specified.\""
    ];
  assert_infer
    {|
      def return_none ():
          pass
    |}
    ["\"Missing return annotation [3]: Returning `None` but no return type is specified.\""];
  assert_infer
    {|
      def return_none ():
          return
    |}
    ["\"Missing return annotation [3]: Returning `None` but no return type is specified.\""];
  assert_infer
    {|
      def return_none ():
          return None
    |}
    ["\"Missing return annotation [3]: Returning `None` but no return type is specified.\""];
  assert_infer
    {|
      def return_none () -> None:
          return None
    |}
    [];

  assert_infer ~fields:["inference.decorators"]
    {|
      @staticmethod
      def returns_int ():
          return 5
    |}
    [{|["staticmethod"]|}];

  assert_infer ~fields:["inference.parameters"]
    {|
      def with_params (x: int = 5,y):
          return 5
    |}
    [{|[{"name":"x","type":"int","value":"5"},
        {"name":"y","type":null,"value":null}]|}];

  assert_infer ~fields:["inference.parameters"]
    {|
      def testing_assert_infer_fragility (x: int = 5):
          return 5
    |}
    [{|[{"type":"int","name":"x","value":"5"}]|}];

  assert_infer ~fields:["inference.annotation"; "inference.parameters"]
    {|
      def with_params (x = 5) -> int:
          return x
    |}
    [
      {|"int"|};{|[{"name":"x","type":"int","value":"5"}]|};
    ];

  assert_infer ~fields:["inference.annotation"]
    {|
      def ret_none(x):
          pass
    |}
    [
      {|"None"|};
    ];

  assert_infer ~fields:["inference.parameters"]
    {|
      from typing import Optional
      def test_optional(x: Optional[str]):
          return 5
    |}
    [{|[{"name":"x","type":"Optional[str]","value":null}]|}];

  assert_infer ~fields:["inference.annotation"; "inference.parameters"]
    {|
      from typing import Optional
      def test_optional(x) -> Optional[str]:
          return x
    |}
    [
      {|"Optional[str]"|};{|[{"name":"x","type":"Optional[str]","value":null}]|}
    ];

  assert_infer ~fields:["inference.parameters"]
    {|
      def ret_int(x: typing.List[int]):
          return 5
    |}
    [{|[{"name":"x","type":"typing.List[int]","value":null}]|}];

  assert_infer ~fields:["inference.parameters"]
    {|
      def ret_list(x) -> typing.List[int]:
        return x
    |}
    [{|[{"name":"x","type":"typing.List[int]","value":null}]|}];

  assert_infer ~fields:["inference.async"]
    {|
      async def async_test ():
          return 5
    |}
    [{|true|}];

  (* Forward-backward iteration *)
  assert_infer ~fields:["inference.parameters"]
    {|
      def foo(z) -> int:
          z = y
          y = x
          return x
    |}
    [];

  (* TODO(T37338460): We should be inferring tuples. *)
  assert_infer ~fields:["inference.parameters"]
    {|
      def foo(y) -> typing.Tuple[int, float]:
          x = y
          z = y
          return (x, z)
    |}
    [];

  (* TODO(T37338460): We should be inferring tuples. *)
  assert_infer ~fields:["inference.parameters"]
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    [];

  assert_infer ~fields:["inference.parameters"]
    {|
      import A
      from A import C
      from B import C
      def test_bad_import(x: A.C):
          return 5
    |}
    [{|[{"name":"x","type":"C","value":null}]|}]; (* Should be A.C *)

  (* The next illustrates where we mess up with current simple dequalify implementation *)
  assert_infer ~fields:["inference.parameters"]
    {|
      def test_optional_bad(x: Optional[str]):
          return 5
      from typing import Optional
    |}
    [{|[{"name":"x","type":"Optional[str]","value":null}]|}]


let test_infer_backward _ =
  assert_infer ~fields:["inference.parameters"]
    {|
      def infer_param(y) -> int:
          x = y
          return x
    |}
    [
      {|[{"name":"y","type":"int","value":null}]|};
    ];

  assert_infer ~fields:["inference.parameters"]
    {|
      def infer_param(x) -> int:
          y = 5
          x = y
          return x
    |}
    [
      {|[{"name":"x","type":"int","value":null}]|};
    ];

  assert_infer ~fields:["inference.annotation";"inference.parameters"]
    {|
      def infer_param(y) -> int:
          z = y
          x = y
          return x
    |}
    [
      {|"int"|};{|[{"name":"y","type":"int","value":null}]|};
    ];

  assert_infer ~fields:["inference.parameters"]
    {|
      def infer_param(x, y) -> int:
          b = 5
          a, b = x, y
          a += b
          return a
    |}
    [
      {|[{"name":"x","type":"int","value":null},{"name":"y","type":null,"value":null}]|};
      {|[{"name":"x","type":null,"value":null},{"name":"y","type":"int","value":null}]|};
    ];
  ()


let test_recursive_infer _ =
  assert_infer ~recursive_infer:false ~fields:["inference.annotation"]
    {|
      def bar():
        return a
      def foo() -> None:
        global a
        a = 1
    |}
    [{|"int"|}];

  assert_infer ~recursive_infer:true ~fields:["inference.annotation"]
    {|
      def foo():
        return 1
      def bar():
        return foo()
    |}
    [{|"int"|}];

  assert_infer ~recursive_infer:true ~fields:["inference.annotation";"inference.parameters"]
    {|
      def foo():
        return 1
      def bar(a):
        a = foo()
        return a
    |}
    [
      {|"int"|};{|[]|};
    ];

  assert_infer ~recursive_infer:true ~fields:["inference.annotation";"inference.parameters"]
    {|
      def foo(a):
        a: int
        return a
      def bar():
        b = foo(a)
        return b
      def baz():
        return bar()
    |}
    [
      {|"int"|};{|[{"name":"a","type":null,"value":null}]|};
    ]


let () =
  "inference">:::[
    "backward">::test_backward;
    "fixpoint_backward">::test_fixpoint_backward;
    "missing_parameter">::test_check_missing_parameter;
    "infer">::test_infer;
    "infer_backward">::test_infer_backward;
    "recursive_infer">::test_recursive_infer;
  ]
  |> Test.run
