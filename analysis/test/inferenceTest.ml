(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Inference
open Test

let configuration = Configuration.Analysis.create ~infer:true ()

let assert_backward ~resolution precondition statement postcondition =
  let module State = State (struct
    let configuration = configuration

    let define = +mock_define
  end)
  in
  let create ?(immutables = []) annotations =
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
          !&name, annotation
        in
        List.map annotations ~f:annotify |> Reference.Map.of_alist_exn
      in
      Resolution.with_annotations resolution ~annotations
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
       ~f:(fun statement state -> State.backward state ~statement)
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

  (* TODO(T25072735): Extend implementation to pass starred and unstarred tests *)
  assert_backward [] "str_float_to_int(*(x, y))" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "str_float_to_int(**{'s': x, 'f': y})" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "star_int_to_int(*[], y)" []


(* We don't yet infer y, which comes after starred param *)

let fixpoint_parse source =
  parse source |> Preprocessing.preprocess |> Preprocessing.defines |> List.hd_exn


let test_check_missing_parameter context =
  let assert_inference_errors =
    assert_errors ~context ~debug:false ~infer:true ~check:Inference.run
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
    ?(fields = ["description"])
    ~context
    source
    errors
  =
  let check_errors configuration global_resolution source =
    let ast_environment = GlobalResolution.ast_environment global_resolution in
    Inference.run ~configuration ~global_resolution ~source
    |> List.map
         ~f:(Error.instantiate ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment))
  in
  let fields_of_error error =
    let field_of_error field =
      let access_field body field =
        match body with
        | `Assoc list ->
            List.Assoc.find ~equal:String.equal list field |> Option.value ~default:`Null
        | _ -> `String "TEST FAIL: ERROR ACCESSING FIELD IN ERROR JSON"
      in
      List.fold
        ~init:(Error.Instantiated.to_json ~show_error_traces error)
        ~f:access_field
        (String.split ~on:'.' field)
    in
    List.map fields ~f:field_of_error
  in
  let configuration, source, environment =
    let project = ScratchProject.setup ~context ["test.py", source] in
    let sources, _, environment = ScratchProject.build_environment project in
    let configuration = ScratchProject.configuration_of project in
    { configuration with debug; infer }, List.hd_exn sources, environment
  in
  let global_resolution = Environment.resolution environment () in
  let to_string json = Yojson.Safe.sort json |> Yojson.Safe.to_string in
  assert_equal
    ~cmp:(List.equal String.equal)
    ~printer:(fun errors -> Format.asprintf "%a" Sexp.pp [%message (errors : string list)])
    ~pp_diff:
      (diff ~print:(fun format errors ->
           Format.fprintf format "%a" Sexp.pp [%message (errors : string list)]))
    (List.map ~f:(fun string -> Yojson.Safe.from_string string |> to_string) errors)
    ( List.map ~f:fields_of_error (check_errors configuration global_resolution source)
    |> List.concat
    |> List.map ~f:to_string )


let test_infer context =
  let assert_infer = assert_infer ~context in
  (* TODO(T37338460): Unbreak inference of self parameter when it is returned. *)
  assert_infer
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    [];
  assert_infer
    ~fields:["inference.parent"]
    {|
      class Test(object):
          def ret_int(self):
              return 5
    |}
    [{|"test.Test"|}];
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
    ["\"Missing return annotation [3]: Return type is not specified.\""];
  assert_infer
    ~fields:["inference.parameters"]
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
    [ "\"Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is "
      ^ "specified.\"" ];
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
  assert_infer {|
      def return_none () -> None:
          return None
    |} [];
  assert_infer
    ~fields:["inference.decorators"]
    {|
      @staticmethod
      def returns_int ():
          return 5
    |}
    [{|["staticmethod"]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def with_params (x: int = 5,y):
          return 5
    |}
    [{|[{"name":"x","type":"int","value":"5"},
        {"name":"y","type":null,"value":null}]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def testing_assert_infer_fragility (x: int = 5):
          return 5
    |}
    [{|[{"type":"int","name":"x","value":"5"}]|}];
  assert_infer
    ~fields:["inference.annotation"; "inference.parameters"]
    {|
      def with_params (x = 5) -> int:
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
    ~fields:["inference.parameters"]
    {|
      from typing import Optional
      def test_optional(x: Optional[str]):
          return 5
    |}
    [{|[{"name":"x","type":"Optional[str]","value":null}]|}];
  assert_infer
    ~fields:["inference.annotation"; "inference.parameters"]
    {|
      from typing import Optional
      def test_optional(x) -> Optional[str]:
          return x
    |}
    [{|"Optional[str]"|}; {|[{"name":"x","type":"Optional[str]","value":null}]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def ret_int(x: typing.List[int]):
          return 5
    |}
    [{|[{"name":"x","type":"typing.List[int]","value":null}]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def ret_list(x) -> typing.List[int]:
        return x
    |}
    [{|[{"name":"x","type":"typing.List[int]","value":null}]|}];
  assert_infer
    ~fields:["inference.async"]
    {|
      async def async_test ():
          return 5
    |}
    [{|true|}];

  (* Don't infer Undeclared *)
  assert_infer
    ~fields:["inference.annotation"]
    {|
    def foo():
      if 1 > 2:
        x = 2
      else:
        assert not True
      return x
    |}
    [];
  assert_infer
    ~fields:["inference.annotation"]
    {|
      def foo():
        return {}
    |}
    ["\"typing.Dict[typing.Any, typing.Any]\""];

  (* Forward-backward iteration *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def foo(z) -> int:
          z = y
          y = x
          return x
    |}
    [];

  (* TODO(T37338460): We should be inferring tuples. *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def foo(y) -> typing.Tuple[int, float]:
          x = y
          z = y
          return (x, z)
    |}
    [];

  (* TODO(T37338460): We should be inferring tuples. *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    [];
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

  (* Should be A.C *)

  (* The next illustrates where we mess up with current simple dequalify implementation *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def test_optional_bad(x: Optional[str]):
          return 5
      from typing import Optional
    |}
    [{|[{"name":"x","type":"Optional[str]","value":null}]|}];
  assert_infer
    ~fields:["inference.annotation"]
    {|
      def foo() -> int:
        return 1234
      class A:
        x = foo()
    |}
    [{|"int"|}];
  assert_infer
    ~fields:["inference.annotation"]
    {|
      def foo() -> int:
        return 1234
      x = foo()
    |}
    [{|"int"|}];

  (* Don't add annotations containing unknown *)
  assert_infer
    ~fields:["inference.annotation"]
    {|
      def foo(a):
          x, _, z = a.b(':')
          return z, x
      |}
    [];

  (* Remove undeclared from parameter annotations *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def foo(a, x = 15):
        b = a.c()
        b = int(b)
        if b > x:
            x = b
    |}
    [ {|[{"name":"a","type":null,"value":null},{"name":"x","type":null,"value":"15"}]|};
      {|[{"name":"a","type":null,"value":null},{"name":"x","type":"int","value":"15"}]|} ];

  assert_infer
    ~fields:["inference.parameters"]
    {|
      def foo(x=None):
          if x:
              x = ""

    |}
    [ {|[{"name":"x","type":null,"value":"None"}]|};
      {|[{"name":"x","type":"typing.Optional[str]","value":"None"}]|} ];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      from typing import Optional
      def bar(x: Optional[str]):
        return x

      def foo(x = None):
        bar(x)
    |}
    [ {|[{"name":"x","type":null,"value":"None"}]|};
      {|[{"name":"x","type":"Optional[str]","value":"None"}]|} ];

  (* Don't infer default of None to be None *)
  assert_infer
    ~fields:["inference.parameters"]
    {|
        def foo(x = None):
          pass
    |}
    [{|[{"name":"x","type":null,"value":"None"}]|}; {|[{"name":"x","type":null,"value":"None"}]|}];

  assert_infer
    ~fields:["inference"]
    {|
      class A:
          @abstractmethod
          def foo():
              pass
    |}
    [{|{}|}]


let test_infer_backward context =
  let assert_infer = assert_infer ~context in
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def infer_param(y) -> int:
          x = y
          return x
    |}
    [{|[{"name":"y","type":"int","value":null}]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def infer_param(x) -> int:
          y = 5
          x = y
          return x
    |}
    [{|[{"name":"x","type":"int","value":null}]|}];
  assert_infer
    ~fields:["inference.annotation"; "inference.parameters"]
    {|
      def infer_param(y) -> int:
          z = y
          x = y
          return x
    |}
    [{|"int"|}; {|[{"name":"y","type":"int","value":null}]|}];
  assert_infer
    ~fields:["inference.parameters"]
    {|
      def infer_param(x, y) -> int:
          b = 5
          a, b = x, y
          a += b
          return a
    |}
    [ {|[{"name":"x","type":"int","value":null},{"name":"y","type":null,"value":null}]|};
      {|[{"name":"x","type":null,"value":null},{"name":"y","type":"int","value":null}]|} ];
  ()


let () =
  "inference"
  >::: [ "backward" >:: test_backward;
         "missing_parameter" >:: test_check_missing_parameter;
         "infer" >:: test_infer;
         "infer_backward" >:: test_infer_backward ]
  |> Test.run
