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


let assert_inference_errors ~context ~expected source =
  let errors = get_inference_errors ~context source in
  let actual = List.map errors ~f:AnalysisError.Instantiated.description in
  assert_equal ~cmp:(List.equal String.equal) ~printer:(String.concat ~sep:"\n") expected actual;
  Memory.reset_shared_memory ()


let test_check_missing_parameter context =
  let assert_inference_errors = assert_inference_errors ~context in
  assert_inference_errors
    {|
      def foo(x = 5) -> int:
        return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def foo(x: typing.Any) -> None:
        x = 5
    |}
    ~expected:
      [
        "Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified.";
      ];
  assert_inference_errors
    {|
      def foo(x: typing.Any = 5) -> None:
        pass
    |}
    ~expected:
      [
        "Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified.";
      ];
  assert_inference_errors
    {|
      def foo(x) -> int:
        return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def foo(x) -> None:
        y = 1
        x = y
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def foo(x) -> int:
        y = x
        return y
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def foo(x) -> int:
          y = 5
          x = y
          return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def infer_param(y) -> int:
          z = y
          x = y
          return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `y` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def infer_param(x, y) -> int:
          b = 5
          a, b = x, y
          a += b
          return a
    |}
    ~expected:
      [
        "Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified.";
        "Missing parameter annotation [2]: Parameter `y` has type `int` but no type is specified.";
      ];
  (* TODO(T84365830): Support inference on addition. *)
  assert_inference_errors {|
      def foo(x) -> None:
          x += 1
    |} ~expected:[];
  (* Analysis doesn't crash when __iadd__ is called with non-simple names. *)
  assert_inference_errors {|
      def foo(x) -> None:
        x[0] += y[3]
    |} ~expected:[];

  assert_inference_errors
    {|
      from typing import Optional
      def test_optional(x) -> Optional[str]:
          return x
    |}
    ~expected:
      [
        "Missing parameter annotation [2]: Parameter `x` has type `Optional[str]` but no type is \
         specified.";
      ];
  (* TODO(T84365830): Implement support for partial annotations. *)
  assert_inference_errors
    {|
      from typing import Optional
      def test_optional(x) -> List[Any]:
          return x
    |}
    ~expected:[];
  (* TODO(T84365830): Forward-backward iteration *)
  assert_inference_errors
    {|
      def foo(z) -> int:
          z = y
          y = x
          return x
    |}
    ~expected:[];
  assert_inference_errors
    {|
      def foo(y) -> typing.Tuple[int, float]:
          x = y
          z = y
          return (x, z)
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `y` has type `int` but no type is specified."];
  assert_inference_errors
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  (* TODO(T84365830): Handle nested tuples. *)
  assert_inference_errors
    {|
    def foo(x, y, z) -> typing.Tuple[typing.Tuple[str, int], bool]:
        return ((x, y), z)

    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `z` has type `bool` but no type is specified."];
  assert_inference_errors
    {|
      def foo(a, x = 15):
        b = a.c()
        b = int(b)
        if b > x:
            x = b
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `None` but no return type is specified.";
        "Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified.";
      ];
  assert_inference_errors
    {|
      def foo(x = None) -> None:
          if x:
              x = ""
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `str` but no type is specified."];
  assert_inference_errors
    {|
      from typing import Optional
      def bar(x: Optional[str]):
        return x

      def foo(x = None) -> None:
        bar(x)
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `Optional[str]` but no return type is specified.";
        "Missing parameter annotation [2]: Parameter `x` has type `Optional[str]` but no type is \
         specified.";
      ];
  (* TODO(T84365830): Be more intelligent about inferring None type. *)
  assert_inference_errors
    {|
        def foo(x = None):
          pass
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `None` but no return type is specified.";
        "Missing parameter annotation [2]: Parameter `x` has type `None` but no type is specified.";
      ];
  assert_inference_errors
    {|
        class A:
          def foo(self, x: int) -> int: ...
        class B(A):
          def foo(self, x):
            return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
        class A:
          def foo(self, x: int, y: str) -> int: ...
        class B(A):
          def foo(self, x, y: str):
            return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_inference_errors
    {|
        from typing import Any
        class A:
          def foo(self, x: Any) -> int: ...
        class B(A):
          def foo(self, x):
            return x
    |}
    ~expected:[];
  assert_inference_errors
    {|
        from typing import TypeVar
        T = TypeVar("T")
        class A:
          def foo(self, x: T) -> T: ...
        class B(A):
          def foo(self, x):
            return x
    |}
    ~expected:[];
  assert_inference_errors
    {|
        class A:
          def foo(self, x: "A") -> "A": ...
        class B(A):
          def foo(self, x):
            return x
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `A` but no type is specified."];
  assert_inference_errors
    {|
        class A:
          def foo(self: "A") -> str: ...
        class B(A):
          def foo(self):
            return x
    |}
    ~expected:[];
  (* We don't generate errors if there are already (possibly inconsistent) annotations. *)
  assert_inference_errors
    {|
        class A:
          def foo(self, x: int) -> int: ...
        class B(A):
          def foo(self, x: str) -> str:
            return x
    |}
    ~expected:[];
  assert_inference_errors
    {|
        class A:
          def foo(self, *args: str, **kwargs: float) -> int: ...
        class B(A):
          def foo(self, *args, **kwargs):
            return x
    |}
    ~expected:
      [
        "Missing parameter annotation [2]: Parameter `*args` has type `str` but no type is \
         specified.";
        "Missing parameter annotation [2]: Parameter `**kwargs` has type `float` but no type is \
         specified.";
      ];
  (* We only add annotations on direct overrides for now. *)
  assert_inference_errors
    {|
        class A:
          def foo(self, x: int) -> int: ...
        class B(A):
          def foo(self, x):
            return x
        class C(B):
          def foo(self, x):
            return x + 1
    |}
    ~expected:
      ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."]


let test_check_missing_return context =
  let assert_inference_errors = assert_inference_errors ~context in
  assert_inference_errors
    {|
      def foo():
        pass
    |}
    ~expected:["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_inference_errors
    {|
      def foo(x: int):
        return x
    |}
    ~expected:["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_inference_errors {|
      def foo() -> int:
        pass
    |} ~expected:[];
  assert_inference_errors
    {|
      def foo():
        x = 1
        return x
    |}
    ~expected:["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_inference_errors
    {|
      def foo():
        return
    |}
    ~expected:["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_inference_errors
    {|
      def foo():
        return None
    |}
    ~expected:["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_inference_errors
    {|
      def foo(b: bool):
        if b:
          return "hello"
        else:
          return 0
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is \
         specified.";
      ];
  assert_inference_errors
    {|
      def foo() -> int:
        return 1

      def bar():
        x = "string"
        x = foo()
        return x
    |}
    ~expected:["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_inference_errors
    {|
      def foo():
        x = undefined
    |}
    ~expected:["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_inference_errors
    {|
      def foo(a):
          x, _, z = a.b(':')
          return z, x
      |}
    ~expected:[];
  assert_inference_errors
    {|
    def foo():
      if 1 > 2:
        x = 2
      else:
        assert not True
      return x
    |}
    ~expected:["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_inference_errors
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    ~expected:["Missing return annotation [3]: Returning `Test` but no return type is specified."];
  assert_inference_errors
    {|
      def foo():
        return [1]
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.List[int]` but no return type is \
         specified.";
      ];
  (* TODO(T84365830): Implement support for empty containers. *)
  assert_inference_errors
    {|
      from typing import Any
      def foo(x: Any):
        return {"": x}
    |}
    ~expected:["Missing return annotation [3]: Return type is not specified."];
  assert_inference_errors {|
      def foo():
        return []
    |} ~expected:[];
  assert_inference_errors {|
      def foo():
        return {}
    |} ~expected:[];
  assert_inference_errors
    {|
      def foo(y: bool):
          x = {}
          if y:
              x["a"] = 1
          return x
      |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Dict[str, int]` but no return type is \
         specified.";
      ];
  assert_inference_errors
    {|
      def a():
          y = {}
          list = [1, 2, 3]
          for num in list:
              y["a"] = num
          return y
      |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Dict[str, int]` but no return type is \
         specified.";
      ];
  assert_inference_errors
    {|
        def a():
            x = []
            x.append("")
            x.append(1)
            return x
      |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.List[typing.Union[int, str]]` but no \
         return type is specified.";
      ];
  assert_inference_errors
    {|
      class A:
          @abstractmethod
          def foo():
              pass
    |}
    ~expected:["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_inference_errors
    {|
    def foo():
        return ("", "", "", "", "", "")
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Tuple[str, str, str, str, str, str]` but \
         no return type is specified.";
      ];
  assert_inference_errors
    {|
    def foo():
        return ("", "", "", 2)
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Tuple[str, str, str, int]` but no return \
         type is specified.";
      ];
  assert_inference_errors
    {|
        def foo():
          def bar(x: int) -> str:
            return ""
          return bar
      |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.Callable[[Named(x, int)], str]` but no \
         return type is specified.";
      ];
  assert_inference_errors
    {|
      def bar():
        def foo(x: int, y: str) -> bool:
            pass
        return [foo]
    |}
    ~expected:
      [
        "Missing return annotation [3]: Returning `typing.List[typing.Callable[[Named(x, int), \
         Named(y, str)], bool]]` but no return type is specified.";
      ]


let test_check_missing_global context =
  let assert_inference_errors = assert_inference_errors ~context in
  assert_inference_errors
    {|
      def foo() -> int:
        return 1234
      x = foo()
    |}
    ~expected:
      [
        "Missing global annotation [5]: Globally accessible variable `x` has type `int` but no \
         type is specified.";
      ];
  assert_inference_errors
    {|
      x = 1 + 1
    |}
    ~expected:
      [
        "Missing global annotation [5]: Globally accessible variable `x` has type `int` but no \
         type is specified.";
      ];
  (* TODO(T84365830): Implement support for global inference due to local usage. *)
  assert_inference_errors
    {|
      x = unknown
      def foo() -> None:
        global x
        x = 1
    |}
    ~expected:[];
  assert_inference_errors
    {|
      x = unknown
      def foo() -> int:
        return x
    |}
    ~expected:[];
  (* TODO(T84365830): Be more intelligent about inferring None type. *)
  assert_inference_errors
    {|
      foo = None
    |}
    ~expected:
      [
        "Missing global annotation [5]: Globally accessible variable `foo` has type `None` but no \
         type is specified.";
      ]


let test_check_missing_attribute context =
  let assert_inference_errors = assert_inference_errors ~context in
  assert_inference_errors
    {|
      def foo() -> int:
        return 1
      class Foo:
        x = foo()
    |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but no type \
         is specified.";
      ];
  assert_inference_errors
    {|
      class Foo:
        x = 1 + 1
    |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but no type \
         is specified.";
      ];
  assert_inference_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = 1 + 1
    |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but no type \
         is specified.";
      ];
  assert_inference_errors
    {|
      class Foo:
        x = None
        def __init__(self) -> None:
          self.x = 1 + 1
    |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type \
         `typing.Optional[int]` but no type is specified.";
      ];
  assert_inference_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = self.foo()

        def foo(self) -> int:
          return 1
    |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but no type \
         is specified.";
      ];
  (* TODO(T84365830): Be more intelligent about inferring None type. *)
  assert_inference_errors
    {|
    class Foo:
      foo = None
  |}
    ~expected:
      [
        "Missing attribute annotation [4]: Attribute `foo` of class `Foo` has type `None` but no \
         type is specified.";
      ]


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
  >::: [
         "backward" >:: test_backward;
         "missing_parameter" >:: test_check_missing_parameter;
         "missing_return" >:: test_check_missing_return;
         "missing_global" >:: test_check_missing_global;
         "missing_attribute" >:: test_check_missing_attribute;
         "infer_fields" >:: test_infer_error_fields;
       ]
  |> Test.run
