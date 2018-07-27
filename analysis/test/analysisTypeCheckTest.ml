(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open Statement
open TypeCheck


open Test

module TestSetup = AnalysisTestSetup


let assert_type_errors = TestSetup.assert_type_errors


let resolution = TestSetup.resolution ()


let create
    ?(define = TestSetup.empty_define)
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
      List.map ~f:annotify annotations
      |> Access.Map.of_alist_exn
    in
    Resolution.with_annotations resolution ~annotations
  in
  let define =
    +{
      define with
      Define.return_annotation = Some (Type.expression expected_return);
    }
  in
  State.create ~resolution ~define ()


let assert_state_equal =
  assert_equal
    ~cmp:State.equal
    ~printer:(Format.asprintf "%a" State.pp)
    ~pp_diff:(diff ~print:State.pp)


let assert_initial
    ~parameters
    ?parent
    ?return_annotation
    ?(decorators = [])
    ?(initial = (fun resolution define ->
        State.initial ~resolution define))
    expected =
  let define = {
    Define.name = Access.create "foo";
    parameters = List.map ~f:(~+) parameters;
    body = [];
    decorators;
    docstring = None;
    return_annotation;
    async = false;
    generated = false;
    parent = parent >>| Access.create;
  }
  in
  assert_state_equal
    expected
    (initial resolution (+define))


let test_initial _ =
  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = Some (+Float 1.0);
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = Some (+Float 1.0);
        annotation = None;
      }
    ]
    (create ["x", Type.float]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    ~return_annotation:!"int"
    (create ~immutables:["x", false] ~expected_return:Type.integer ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.float);
      };
      {
        Parameter.name = ~~"y";
        value = None;
        annotation = Some (Type.expression Type.string)
      };
    ]
    (create ~immutables:["x", false; "y", false] ["x", Type.float; "y", Type.string]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = None;
      };
    ]
    (create ["x", Type.Bottom]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"self";
        value = None;
        annotation = None;
      };
    ]
    ~parent:"Foo"
    (create ["self", Type.Primitive ~~"Foo"]);
  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"a";
        value = None;
        annotation = None;
      };
    ]
    ~decorators:[!"staticmethod"]
    ~parent:"Foo"
    (create ["a", Type.Bottom])


let test_less_or_equal _ =
  (* <= *)
  assert_true (State.less_or_equal ~left:(create []) ~right:(create []));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.integer]));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.Top]));
  assert_true
    (State.less_or_equal
       ~left:(create ["x", Type.integer])
       ~right:(create ["x", Type.integer; "y", Type.integer]));

  (* > *)
  assert_false (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create []));
  assert_false (State.less_or_equal ~left:(create ["x", Type.Top]) ~right:(create []));

  (* partial order *)
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["x", Type.string]));
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["y", Type.integer]))


let test_join _ =
  (* <= *)
  assert_state_equal (State.join (create []) (create [])) (create []);
  assert_state_equal
    (State.join (create []) (create ["x", Type.integer]))
    (create ["x", Type.Top]);
  assert_state_equal (State.join (create []) (create ["x", Type.Top])) (create ["x", Type.Top]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.integer; "y", Type.integer]))
    (create ["x", Type.integer; "y", Type.Top]);

  (* > *)
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create []))
    (create ["x", Type.Top]);
  assert_state_equal
    (State.join (create ["x", Type.Top]) (create []))
    (create ["x", Type.Top]);

  (* partial order *)
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.string]))
    (create ["x", Type.union [Type.string; Type.integer]]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["y", Type.integer]))
    (create
       ["x", Type.Top; "y", Type.Top])


let test_widen _ =
  let widening_threshold = 10 in
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:0)
    (create ["x", Type.union [Type.integer; Type.string]]);
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:(widening_threshold + 1))
    (create ["x", Type.Top])


let test_forward_expression _ =
  let assert_forward precondition statement postcondition =
    let parsed =
      parse statement
      |> function
      | { Source.statements = statement::rest; _ } -> statement::rest
      | _ -> failwith "unable to parse test"
    in
    (* TODO(T30448045): Call `forward_expression`. *)
    assert_state_equal
      (create postcondition)
      (List.fold
         ~f:(fun state statement -> State.forward ~key:0 state ~statement)
         ~init:(create precondition)
         parsed)
  in

  (* Access. *)
  assert_forward
    ["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    "x.add_key(1)"
    ["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom];
  assert_forward
    ["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    "x.add_value(1)"
    ["x", Type.dictionary ~key:Type.Bottom ~value:Type.integer];
  assert_forward
    ["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    "x.add_both(1, 'string')"
    ["x", Type.dictionary ~key:Type.integer ~value:Type.string];
  assert_forward
    ["x", Type.dictionary ~key:Type.string ~value:Type.Bottom]
    "x.add_key(1)"
    ["x", Type.dictionary ~key:Type.string ~value:Type.Bottom]; (* We're not yet joining. *)

  (* Literals. *)
  assert_forward [] "x = 1.0" ["x", Type.float];
  assert_forward [] "x = 'string'" ["x", Type.string];
  assert_forward [] "x = b'string'" ["x", Type.bytes];
  assert_forward [] "x = True" ["x", Type.bool];
  assert_forward [] "x = False" ["x", Type.bool];
  assert_forward [] "x = {}" ["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom];

  (* Calls. *)
  assert_forward [] "x = unknown()" ["x", Type.Top];
  assert_forward ["x", Type.string] "x = repr(x)" ["x", Type.float];
  assert_forward [] "x = int_to_str(1)" ["x", Type.string];

  (* Binary operators. *)
  assert_forward [] "x = 1.0 + 1.0" ["x", Type.float];
  assert_forward
    ["x", Type.integer; "y", Type.integer]
    "z = x + y"
    ["x", Type.integer; "y", Type.integer; "z", Type.integer];
  assert_forward
    ["x", Type.string; "y", Type.string]
    "z = x + y"
    ["x", Type.string; "y", Type.string; "z", Type.string];

  (* Boolean operators. *)
  assert_forward [] "x = 1.0 and 1.0" ["x", Type.float];
  assert_forward
    ["x", Type.integer; "y", Type.integer]
    "z = x and y"
    ["x", Type.integer; "y", Type.integer; "z", Type.integer];
  assert_forward
    ["x", Type.integer; "y", Type.string]
    "z = x and y"
    ["x", Type.integer; "y", Type.string; "z", Type.union [Type.integer; Type.string]];
  assert_forward
    [] "x = None or 1" ["x", Type.integer];
  assert_forward
    [] "x = None or None or 1" ["x", Type.integer];
  assert_forward
    [] "x = 'hi' or 1" ["x", Type.union [Type.string; Type.integer]];

  (* Comparison operator. *)
  assert_forward [] "x = 'a' < 1" ["x", Type.float];
  assert_forward [] "x = 'a' != 1" ["x", Type.integer];
  assert_forward [] "x = 1 < 1" ["x", Type.integer];
  assert_forward [] "x = 'a' < 1 < 3" ["x", Type.integer];
  assert_forward [] "x = 'a' < 1 != 3" ["x", Type.Bottom]; (* Contradiction. *)

  (* Unary operator. *)
  assert_forward [] "x = -1.0" ["x", Type.float];
  assert_forward [] "x = not 1.0" ["x", Type.bool];
  assert_forward [] "x = +'asdf'" ["x", Type.float];

  (* List. *)
  assert_forward [] "x = []" ["x", Type.list Type.Bottom];
  assert_forward [] "x = [1.0]" ["x", Type.list Type.float];
  assert_forward [] "x = [1.0, 'string']" ["x", Type.list (Type.union [Type.float; Type.string])];
  assert_forward [] "x = [] + [1]" ["x", Type.list Type.integer];
  assert_forward
    ["x", Type.list Type.float]
    "y = x[0]"
    ["x", Type.list Type.float; "y", Type.float];
  assert_forward
    ["x", Type.list Type.float]
    "y = x[a:b]"
    ["x", Type.list Type.float; "y", Type.list Type.float];
  assert_forward
    ["x", Type.list Type.integer]
    "y = [a for a in x]"
    ["a", Type.integer; "x", Type.list Type.integer; "y", Type.list Type.integer];

  (* Dictionary. *)
  assert_forward
    []
    "x = { 1.0: 'string' }"
    ["x", Type.dictionary ~key:Type.float ~value:Type.string];
  assert_forward
    []
    "x = { 1.0: 'string', 'string': 'string' }"
    ["x", Type.dictionary ~key:(Type.union [Type.float; Type.string]) ~value:Type.string];
  assert_forward
    []
    "x = { 1.0: 'string', 1.0: 1.0 }"
    ["x", Type.dictionary ~key:Type.float ~value:(Type.Union [Type.float; Type.string])];
  assert_forward
    []
    "x = { key: value for target in iterator }"
    ["target", Type.Top; "x", Type.dictionary ~key:Type.Object ~value:Type.Object];
  assert_forward
    ["iterator", Type.list Type.integer]
    "x = { target: target for target in iterator }"
    [
      "iterator", Type.list Type.integer;
      "target", Type.integer;
      "x", Type.dictionary ~key:Type.integer ~value:Type.integer
    ];
  assert_forward
    ["iterator", Type.list (Type.tuple [Type.string; Type.integer])]
    "x = { k: v for k, v in iterator }"
    [
      "iterator", Type.list (Type.tuple [Type.string; Type.integer]);
      "k", Type.string;
      "v", Type.integer;
      "x", Type.dictionary ~key:Type.string ~value:Type.integer
    ];
  assert_forward
    ["x", Type.dictionary ~key:Type.string ~value:Type.integer]
    "y = x['derp']"
    ["x", Type.dictionary ~key:Type.string ~value:Type.integer; "y", Type.integer];

  (* Generator. *)
  assert_forward
    []
    "x = (element for target in iterator)"
    ["target", Type.Top; "x", Type.generator Type.Object];

  (* Lambda. *)
  assert_forward
    []
    "x = lambda: 1.0"
    ["x", Type.lambda ~parameters:[] ~return_annotation:Type.float];

  assert_forward
    []
    "x = lambda y: 1"
    [
      "x", Type.lambda ~parameters:[Type.Object] ~return_annotation:Type.integer;
      "y", Type.Object;
    ];

  (* Set. *)
  assert_forward [] "x = { 1.0, }" ["x", Type.set Type.float];
  assert_forward [] "x = { 1.0, 'foo' }" ["x", Type.set (Type.union [Type.float; Type.string])];
  assert_forward
    ["x", Type.list Type.integer]
    "y = { a for a in x }"
    ["a", Type.integer; "x", Type.list Type.integer; "y", Type.set Type.integer];

  (* Starred. *)
  assert_forward [] "x = *1.0" ["x", Type.Object];

  (* Await. *)
  assert_forward
    ["x", Type.awaitable Type.integer]
    "y = await x"
    ["x", Type.awaitable Type.integer; "y", Type.integer];
  assert_forward
    ["x", Type.primitive "IsAwaitable"]
    "y = await x"
    ["x", Type.primitive "IsAwaitable"; "y", Type.integer];

  (* Redirects. *)
  assert_forward
    ["y", Type.union [Type.integer; Type.string]]
    "x = str(y)"
    ["x", Type.bool; "y", Type.union [Type.integer; Type.string]];
  assert_forward [] "x = str(1)" ["x", Type.bool];

  (* Unresolved type variables. *)
  assert_forward
    ["x", Type.set Type.Bottom]
    "y, z = x.__iter__(), x.__iter__().__next__()"
    ["x", Type.set Type.Bottom; "y", Type.iterator Type.Bottom; "z", Type.Bottom];

  assert_forward
    ["x", Type.Bottom]
    "y = identity(x)"
    ["x", Type.Bottom; "y", Type.Bottom]; (* Limitation: We're losing y's constraints here. *)

  (* TODO(T30448045): gradually migrate existing tests to check resolved type. *)
  let assert_forward ?(precondition = []) ?(postcondition = []) ?errors expression annotation =
    let expression = parse_single_expression expression in
    let { State.state = forwarded; resolved } =
      State.forward_expression
        ~state:(create precondition)
        ~expression
    in
    assert_equal ~cmp:Type.equal ~printer:Type.show annotation resolved;
    assert_state_equal (create postcondition) forwarded;
    match errors with
    | Some errors ->
        assert_equal
          ~cmp:(List.equal ~equal:String.equal)
          ~printer:(String.concat ~sep:"\n")
          errors
          (State.errors forwarded |> List.map ~f:(Error.description ~detailed:false))
    | _ ->
        ()
  in

  assert_forward "1j" Type.complex;
  assert_forward "1" Type.integer;
  assert_forward "1.0" Type.float;

  assert_forward "True" Type.bool;
  assert_forward "False" Type.bool


let test_forward_statement _ =
  let assert_forward
      ?(precondition_immutables = [])
      ?(postcondition_immutables = [])
      ?expected_return
      ?errors
      precondition
      statement
      postcondition =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement::rest; _ } -> statement::rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward_statement ~state ~statement)
        ~init:(create ?expected_return ~immutables:precondition_immutables precondition)
        parsed
    in
    assert_state_equal (create ~immutables:postcondition_immutables postcondition) forwarded;
    match errors with
    | Some errors ->
        assert_equal
          ~cmp:(List.equal ~equal:String.equal)
          ~printer:(String.concat ~sep:"\n")
          errors
          (State.errors forwarded |> List.map ~f:(Error.description ~detailed:false))
    | _ ->
        ()
  in

  (* Assignments. *)
  assert_forward ["y", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_forward ["y", Type.integer] "x = z" ["x", Type.Top; "y", Type.integer];
  assert_forward ["x", Type.integer] "x += 1" ["x", Type.integer];

  assert_forward
    ["z", Type.integer]
    "x = y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.integer];

  assert_forward
    ["c", Type.integer; "d", Type.Top]
    "a, b = c, d"
    ["a", Type.integer; "b", Type.Top; "c", Type.integer; "d", Type.Top];

  (* Here be dragons... *)
  assert_forward ["z", Type.integer] "x, y = z" ["x", Type.Top; "y", Type.Top; "z", Type.integer];

  (* Assignments with tuples. *)
  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.Top]; "y", Type.integer; "z", Type.Top];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string]]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_forward
    ["z", Type.Tuple (Type.Unbounded Type.integer)]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.Tuple (Type.Unbounded Type.integer)];
  assert_forward
    []
    "(x, y), z = 1"
    ["x", Type.Top; "y", Type.Top; "z", Type.Top];
  assert_forward
    ["z", Type.list Type.integer]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.list Type.integer];
  assert_forward
    []
    "x, y = return_tuple()"
    ["x", Type.integer; "y", Type.integer;];
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Unbounded Type.Object)];

  (* Assignments with list. *)
  assert_forward
    ["x", Type.list Type.integer]
    "[a, b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.integer];

  (* Assignments with immutables. *)
  assert_forward ~postcondition_immutables:["x", true] [] "global x" ["x", Type.Top];
  assert_forward ~postcondition_immutables:["y", false] [] "y: int" ["y", Type.integer];
  assert_forward ~postcondition_immutables:["y", false] [] "y: int = x" ["y", Type.integer];
  assert_forward ~postcondition_immutables:["y", false] [] "y: int = 'string'" ["y", Type.integer];
  assert_forward
    ~precondition_immutables:["y", false]
    ~postcondition_immutables:["y", false]
    ["y", Type.Top]
    "y = x"
    ["y", Type.Top];
  assert_forward
    ~postcondition_immutables:["y", false]
    ["x", Type.string]
    "y: int = x"
    ["x", Type.string; "y", Type.integer];
  assert_forward
    ~precondition_immutables:["y", false]
    ~postcondition_immutables:["y", false]
    ["y", Type.string]
    "y: int"
    ["y", Type.integer];

  (* Assert. *)
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x"
    ["x", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert y"
    ["x", Type.optional Type.integer];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is not None"
    ["x", Type.integer];

  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x and y"
    ["x", Type.integer; "y", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float; "z", Type.optional Type.float]
    "assert x and (y and z)"
    ["x", Type.integer; "y", Type.float; "z", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x or y"
    ["x", Type.optional Type.integer; "y", Type.optional Type.float];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is None"
    ["x", Type.optional Type.Bottom];

  assert_forward ["x", Type.Object] "assert isinstance(x, int)" ["x", Type.integer];
  assert_forward
    ["x", Type.Object]
    "assert isinstance(y, str)"
    ["x", Type.Object; "y", Type.string];

  assert_forward
    ["x", Type.Object]
    "assert isinstance(x, (int, str))"
    ["x", Type.union [Type.integer; Type.string]];
  assert_forward
    ["x", Type.integer]
    "assert isinstance(x, (int, str))"
    ["x", Type.integer];

  (* Raise. *)
  assert_forward ~errors:[] [] "raise 1" [];
  assert_forward
    ~errors:["Undefined name [18]: Global name `undefined` is undefined."]
    []
    "raise undefined"
    [];
  assert_forward ~errors:[] [] "raise" [];

  (* Return. *)
  assert_forward
    ~errors:["Missing return annotation [3]: Returning `int` but no return type is specified."]
    []
    "return 1"
    [];
  assert_forward ~expected_return:Type.integer ~errors:[] [] "return 1" [];
  assert_forward
    ~expected_return:Type.string
    ~errors:["Incompatible return type [7]: Expected `str` but got `int`."]
    []
    "return 1"
    [];

  (* Pass. *)
  assert_forward ["y", Type.integer] "pass" ["y", Type.integer]


let test_forward _ =
  let assert_forward precondition statement postcondition =
    let parsed =
      parse statement
      |> function
      | { Source.statements = statement::rest; _ } -> statement::rest
      | _ -> failwith "unable to parse test"
    in
    assert_state_equal
      (create postcondition)
      (List.fold
         ~f:(fun state statement -> State.forward ~key:0 state ~statement)
         ~init:(create precondition)
         parsed)
  in
  let _ = assert_forward in
  ()  (* TODO(T30448045): integration tests. *)


let test_show_error_traces _ =
  assert_type_errors ~show_error_traces:true
    "def foo() -> int: return 1.0"
    [
      "Incompatible return type [7]: Expected `int` but got `float`. Type `int` expected on line " ^
      "1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: Expected `str` but got `None`. " ^
      "Type `str` expected on line 1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> typing.List[str]: return 1"
    [
      "Incompatible return type [7]: Expected `typing.List[str]` but got `int`. Type " ^
      "`typing.List[str]` expected on line 1, specified on line 1.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [];

  assert_type_errors ~show_error_traces:true
    "def foo(): pass"
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 1, return type should be specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 3, return type should be specified on line 2.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`. Attribute `attribute` declared on line 3, incorrectly " ^
      "used on line 5.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as " ^
      "type `str`. constant incorrectly used on line 5.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        a = 1
        b = 2
        reveal_type(a + b)
    |}
    [
      "Undefined name [18]: Global name `reveal_type` is undefined.";
      "Revealed type [-1]: Revealed type for `a.__add__.(...)` is `int`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized. Attribute `attribute` is declared on " ^
      "line 3, never initialized and therefore must be `typing.Optional[int]`.";
    ];
  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute = x
      class Bar:
        def bar(self) -> str:
          foo = Foo()
          foo.attribute = 'string'
          return foo.attribute
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `str` " ^
      "but no type is specified. Attribute `attribute` declared on line 3, type `str` deduced " ^
      "from test.py:7:4.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    constant = x
    def foo() -> None:
      global constant
      constant = 1
    |}
    [
      "Undefined name [18]: Global name `x` is undefined.";
      (* TODO(T29421309): unbreak missing global annotation errors. *)
    ];

  assert_type_errors ~show_error_traces:true
    {|
    constant = x
    def foo() -> None:
      global constant
      constant = "hi"
      constant = 1
    |}
    [
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Other():
        attribute = x
        def foo(self) -> None:
          self.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Other` has type " ^
      "`int` but no type is specified. Attribute `attribute` declared on line 3, " ^
      "type `int` deduced from test.py:5:4.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];


  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        global x
        x = 5
      def bar() -> None:
        global x
        x = "str"
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `x` " ^
      "declared on line 4, type `typing.Union[int, str]` deduced from test.py:4:2, " ^
      "test.py:7:2.";
      "Missing global annotation [5]: Globally accessible variable `x` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `x` " ^
      "declared on line 7, type `typing.Union[int, str]` deduced from test.py:4:2, " ^
      "test.py:7:2."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def f()->int:
        x : int = ""
        return 0
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used " ^
      "as type `str`. x incorrectly used on line 3.";
    ]


let test_check_with_qualification _ =
  assert_type_errors
    {|
      x: int = 1
      def foo(x: str) -> str:
        return x
    |}
    [];

  assert_type_errors
    {|
      x: int = 1
      def foo(y: str) -> str:
        return x
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      list: typing.List[int] = [1]
      def hello() -> int:
        for i in list:
          return i
        return -1
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123

      def duh(global_number: str) -> int:
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def nonglobal_inner_access() -> int:
              return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def wut_inner_global() -> int:
              global global_number
              return global_number

    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      global_number: int = 123
      def rly() -> int:
          def rly_inner(global_number: str) -> None:
              pass
          return global_number
      def len(s: str) -> int:
          return 1
      def assign() -> int:
          global_number="a" # type: str
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def len(s: str) -> int:
        return 1
      def assign_outer() -> None:
          global_number="a" # type: str
          def assign_inner_access() -> int:
              return len(global_number)
          def assign_inner_global() -> int:
              global global_number
              return global_number
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      global_number: int = 1
      def derp() -> int:
          def derp_inner() -> None:
              global_number="a" # type: str
              pass
          return global_number
    |}
    [];

  assert_type_errors
    {|
      def access_side_effect(global_number: str) -> int:
          side_effect=global_number
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def access_side_effect_2() -> int:
          side_effect=global_number
          return global_number
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def pure_sideffect() -> None:
          side_effect=global_number
          def pure_side_effect_inner() -> int:
              return global_number
    |}
    [];


  assert_type_errors
    {|
      global_number: int = 1
      def access_transitive() -> int:
          transitive=global_number
          return transitive
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def assign_transitive() -> None:
          another=global_number
          # TODO(T27001301): uncomment next two lines when nested scopes will work
          #def out_of_ideas_3() -> int:
          #    return another
      def assign_transitive_2() -> int:
          transitive=global_number
          def assign_transitive_inner() -> None:
              global_number="a"
          return transitive
    |}
    []


let test_check_imports _ =
  assert_type_errors
    {|
      import durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      import typing
    |}
    [];
  assert_type_errors
    {|
      import typing, durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      from typing import durp
    |}
    [];
  assert_type_errors
    {|
      from durp import typing
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    ["Undefined name [18]: Global name `durp` is undefined."];
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."]


let test_reveal_type _ =
  assert_type_errors
    {|
      def foo(x: str) -> None:
        reveal_type(x)
    |}
    [
      "Undefined name [18]: Global name `reveal_type` is undefined.";
      "Revealed type [-1]: Revealed type for `x` is `str`.";
    ];

  assert_type_errors
    {|
      def foo(x) -> None:
        reveal_type(x)
    |}
    [
      "Undefined name [18]: Global name `reveal_type` is undefined.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Unbound`.";
    ];
  assert_type_errors
    {|
      def foo(x: int, y: int) -> None:
        reveal_type(x + y)
    |}
    [
      "Undefined name [18]: Global name `reveal_type` is undefined.";
      "Revealed type [-1]: Revealed type for `x.__add__.(...)` is `int`.";
    ];
  assert_type_errors
    {|
      def foo(x: int) -> None:

        reveal_type(int_to_str(x))
    |}
    [
      "Undefined name [18]: Global name `reveal_type` is undefined.";
      "Revealed type [-1]: Revealed type for `int_to_str.(...)` is `str`.";
    ]


let test_coverage _ =
  let assert_coverage source expected =
    let { Result.coverage; _ } =
      let environment = TestSetup.environment () in
      Analysis.TypeCheck.check
        TestSetup.configuration
        environment
        (parse source)
    in
    assert_equal ~printer:Coverage.show expected coverage
  in
  assert_coverage
    {| def foo(): pass |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 0 };
  assert_coverage
    {|
     def foo(y: int):
       if True:
         x = y
       else:
         x = z
    |}
    { Coverage.full = 1; partial = 0; untyped = 1; ignore = 0; crashes = 0 };
  assert_coverage
    {|
     def foo(y: asdf):
      if True:
        x = y
      else:
        x = 1
    |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 1 };

  assert_coverage
    {|
      def foo(y) -> int:
        x = returns_undefined()
        return x
    |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 1 }


let test_check _ =
  assert_type_errors
    "def foo() -> None: pass"
    [];

  assert_type_errors
    "def foo() -> None: return"
    [];

  assert_type_errors
    "def foo() -> float: return 1.0"
    [];

  assert_type_errors
    "def foo() -> float: return 1"
    [];

  assert_type_errors
    "def foo() -> int: return 1.0"
    ["Incompatible return type [7]: Expected `int` but got `float`."];

  assert_type_errors
    "def foo() -> str: return 1.0"
    ["Incompatible return type [7]: Expected `str` but got `float`."];

  assert_type_errors
    "def foo() -> str: return"
    ["Incompatible return type [7]: Expected `str` but got `None`."];

  assert_type_errors
    "def foo() -> typing.List[str]: return 1"
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `int`."];

  assert_type_errors
    ~debug:false
    "def foo() -> int: return"
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors
    "def foo() -> typing.List[str]: return []"
    [];

  assert_type_errors
    "def foo() -> typing.Dict[str, int]: return {}"
    [];

  assert_type_errors
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [];

  assert_type_errors
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any]: return f()
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[typing.Any]` but got " ^
      "`typing.Dict[typing.Any, typing.Any]`."
    ];

  assert_type_errors
    {|
      x: typing.Type
      def foo() -> typing.Type[typing.Any]:
        return x
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> list:
        return x
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        @overload
        def derp(self, x: str) -> str:
          pass

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];

  assert_type_errors
    {|
      def f(d: typing.Dict[int, int], x) -> None:
        d.update({ 1: x })
    |}
    [];

  assert_type_errors
    {|
      def f(d: typing.Dict[int, str], x) -> str:
        return d.get(x, "")
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        a = {"key": set()}
        b = a.get("key", set())
    |}
    [];

  assert_type_errors
    {|
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo() -> int:
        for x in [1,2,3]:
          if x > 0:
            return x
          x = 15
        return 0
    |}
    [];

  assert_type_errors
    {|
      x: str
      def foo() -> str:
        return x.__getitem__(0)
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> int:
        return x.__getitem__(0)
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[str]:
        a, *b = x
        return b
    |}
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `typing.List[int]`."];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x.__getitem__(slice(0, 1, None))
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> int:
        return x[0]
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x[0:1]
    |}
    [];

  assert_type_errors
    {|
      def f(x) -> int:
        class Stub:
          ...
        class Actual:
          def f() -> int:
            return 0
        if isinstance(x, Stub):
          return -1
        elif isinstance(x, Actual):
          return 0
        else:
          return 1
    |}
    [];

  assert_type_errors
    {|
      x: typing.Generator[int, int, int]
      def foo() -> typing.Generator:
        return x
    |}
    [];

  assert_type_errors
    {|
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];

  assert_type_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [
      "Incompatible return type [7]: Expected `str` but got `float`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return A()"
    [];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: Expected `A` but got `int`."];

  assert_type_errors
    "def bar() -> str: return ''\ndef foo() -> str: return bar()"
    [];

  assert_type_errors
    "def foo() -> str: return not_annotated()"
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];

  assert_type_errors
    {|
      class other(): pass
      def foo() -> other:
        result = 0
        if True:
          result = not_annotated()
        return result
    |}
    ["Incompatible return type [7]: Expected `other` but got `unknown`."];

  assert_type_errors
    {|
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
    [];

  assert_type_errors
    {|
      def derp() -> int:
        a, b = [1,2,3]
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo() -> int:
        (x, y), z = 0
        return x + y + z
    |}
    [
      (* There should be no name errors here. *)
      "Incompatible variable type [9]: Unable to unpack `int`, expected a `Tuple`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      @abstractmethod
      def abstract()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      @abc.abstractproperty
      def abstract()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      @typing.overload
      def overloaded()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def overloaded()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      class Derp:
        @property
        async def get_int(self) -> int:
          return 5

        def test(self) -> int:
          x = await self.get_int
          return x
    |}
    [];

  assert_type_errors
    {|
      def x()->int:
        return None
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `None`."
    ];

  assert_type_errors
    {|
      def x() -> int:
        if condition:
          return 1
    |}
    [
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
      "Undefined name [18]: Global name `condition` is undefined.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        if condition:
          return 1
        else:
          x = 1
    |}
    [
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
      "Undefined name [18]: Global name `condition` is undefined.";
    ];

  assert_type_errors
    {|
      def f(x: int) -> None:
        x: str = int_to_str(x)
    |}
    [];

  assert_type_errors
    {|
      def derp()->typing.Union[str, None]:
          return None
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[int])->typing.Generator[int, None, None]:
        return (x for x in l)
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[int, None, None]:
        return (x for x in l if x)
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[str, None, None]:
        return (x for x in l if x is not None)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[str, None, None]` " ^
      "but got `typing.Generator[int, None, None]`.";
    ];

  assert_type_errors
    {|
      def foo(l: typing.Iterable[typing.Any])->typing.Generator[typing.Any, None, None]:
        return (x for x in l)
    |}
    [];

  assert_type_errors
    {|
     def derp()->typing.Set[int]:
      return {1}
    |}
    [];

  assert_type_errors
    {|
     def derp()->typing.Set[int]:
      return {""}
    |}
    [
      "Incompatible return type [7]: Expected `typing.Set[int]` but got `typing.Set[str]`."
    ];

  assert_type_errors
    {|
     def foo() -> str:
      if True:
        return 1
      else:
        return 2
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];

  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> str:
          return ''

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)
    |}
    [];

  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> int:
          return 5

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)

    |}
    ["Incompatible parameter type [6]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(1,2)
    |}
    ["Too many arguments [19]: Call `foo` expects 1 positional argument, 2 were provided."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(y=4)
    |}
    [];

  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f()
    |}
    ["Missing argument [20]: Call `C.f` expects argument `x`."];

  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f("", "")
    |}
    ["Too many arguments [19]: Call `C.f` expects 2 positional arguments, 3 were provided."];

  assert_type_errors
    {|
      def foo(x: int, y: str) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];

  assert_type_errors
    {|
      def foo() -> str:
        return ""
      def f() -> None:
        a = foo(1,2,3,4)
    |}
    ["Too many arguments [19]: Call `foo` expects 0 positional arguments, 4 were provided."];

  assert_type_errors
    {|
      class C:
        def __init__(self, x: int) -> None:
          self.a = x
        def a(self) -> int:
          return self.a
    |}
    [];

  assert_type_errors
    {|
      def identity(x: int) -> int:
        return x
      class C:
        def __init__(self, x: int) -> None:
          self.a = identity(x)
        def a(self) -> int:
          return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a`" ^
      " of class `C` has type `int` but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`."
    ];

  assert_type_errors
    {|
      def foo( *args: int) -> typing.Iterable[str]:
        return args
    |}
    [
      "Incompatible return type [7]: Expected `typing.Iterable[str]` but " ^
      "got `typing.Sequence[int]`.";
    ];

  assert_type_errors
    {|
      def foo( **kwargs: int) -> None:
        return kwargs
    |}
    ["Incompatible return type [7]: Expected `None` but got `typing.Dict[str, int]`."];

  assert_type_errors
    {|
      def f( *args: int) -> None:
       pass
      def g( *args: int) -> None:
        return f( *args)
    |}
    [];
  ();

  assert_type_errors
    {|
      def f() -> str:
        return __name__
      def g() -> str:
        return __file__
      def h() -> str:
        return typing.__name__
      def i() -> str:
        return ...
    |}
    [];

  assert_type_errors
    {|
      def f() -> int:
        return builtins.__name__
    |}
    ["Incompatible return type [7]: Expected `int` but got `unknown`."]


let test_check_assign _ =
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x = 'string'  # Reassignment is okay.
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        x = 'string'
    |}
    ["Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."];

  assert_type_errors
    {|
      def foo(input: typing.Tuple[int, str]) -> None:
        x = input
    |}
    [];
  assert_type_errors
    {|
      def foo(input: int) -> None:
        x, y = input
    |}
    ["Incompatible variable type [9]: Unable to unpack `int`, expected a `Tuple`."];

  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x += 'asdf'
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."]


let test_check_coverage _ =
  let preprocess source =
    trim_extra_indentation source
    |> String.lstrip
    |> String.split ~on:'\n'
    |> List.map ~f:(fun line -> "    " ^ line)
    |> String.concat ~sep:"\n"
    |> String.substr_replace_all ~pattern:"ERROR" ~with_:"a.undefined"
    |> Format.asprintf "def foo(a: A) -> None:\n%s\n"
  in
  let assert_covered ?(additional_errors = []) source =
    assert_type_errors
      (preprocess source)
      (additional_errors @ ["Undefined attribute [16]: `A` has no attribute `undefined`."])
  in
  let assert_not_covered ?(additional_errors = []) source =
    assert_type_errors (preprocess source) additional_errors
  in

  (* Return statement. *)
  assert_covered
    ~additional_errors:["Incompatible return type [7]: Expected `None` but got `unknown`."]
    "return ERROR";

  (* Assignment. *)
  assert_covered "b = ERROR";

  (* Assertion. *)
  assert_covered "assert ERROR";

  (* Nested definitions. *)
  assert_covered "class B: ERROR";
  assert_covered
    ~additional_errors:[]
    "def nested() -> None: ERROR";

  (* Expressions. *)
  assert_covered "ERROR";

  (* Yield. *)
  assert_covered
    ~additional_errors:[
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Generator[unknown, None, None]`.";
    ]
    "yield ERROR";
  assert_not_covered
    ~additional_errors:[
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Generator[unknown, None, None]`.";
    ]
    "yield from ERROR";

  (* Control statements. *)
  assert_covered
    "for i in ERROR: pass";
  assert_covered "while ERROR: pass";
  assert_covered "if ERROR: pass";

  (* Raise. *)
  assert_covered "raise ERROR";

  assert_covered
    {|
      try:
        pass
      except ERROR:
        pass
    |};

  assert_covered
    {|
      with ERROR:
        pass
    |};
  assert_covered
    {|
      with ERROR as derp:
        pass
    |};

  (* Await. *)
  assert_covered
    ~additional_errors:[
      "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
    ]
    "await ERROR";

  (* Binary operator. *)
  assert_covered "ERROR | 1";
  assert_covered "1 % ERROR";

  (* Boolean operator. *)
  assert_covered "ERROR or False";
  assert_covered "True or ERROR";
  assert_covered "ERROR and False";
  assert_covered "True and ERROR";

  (* Comparison operator. *)
  assert_covered "1 == ERROR";
  assert_covered "ERROR < 1";

  (* Dictionaries. *)
  assert_covered "{ ERROR: 1 }";
  assert_covered "{ 1: ERROR }";
  assert_covered "{ ERROR: i for i in dict() }";
  assert_covered "{ i: ERROR for i in dict() }";
  assert_covered "{ i: 1 for i in ERROR }";

  (* Format string. *)
  assert_covered {|f"format{ERROR}"|};

  (* Generator. *)
  assert_covered "(ERROR for i in list())";

  (* Lambdas. *)
  assert_covered "lambda x: ERROR";

  (* Lists. *)
  assert_covered "[1, ERROR]";
  assert_covered "[ERROR for i in list()]";
  assert_covered "[i for i in ERROR]";

  (* Sets. *)
  assert_covered "{1, ERROR}";
  assert_covered "{ERROR for i in list()}";
  assert_covered "{i for i in ERROR}";

  (* Starred. *)
  assert_covered "*ERROR";
  assert_covered "**ERROR";

  (* Ternary. *)
  assert_covered "ERROR if True else 1";
  assert_covered "True if ERROR else 1";
  assert_covered "True if True else ERROR";

  (* Tuples. *)
  assert_covered "ERROR, True";
  assert_covered "True, ERROR";

  (* Unary operators. *)
  assert_covered "not ERROR";
  assert_covered "-ERROR"


let test_check_comprehensions _ =
  assert_type_errors
    {|
    def foo(input: typing.List[str]) -> typing.List[str]:
      return [a for a in input]
    |}
    [];

  assert_type_errors
    {|
    def foo(input: typing.List[str]) -> typing.List[str]:
      return [a for a in input if len(a) < 5]
    |}
    [];

  assert_type_errors
    {|
    def foo(input: str) -> typing.List[int]:
      return [a for a in input]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];

  assert_type_errors
    {|
      def foo() -> typing.List[str]:
        return [x for x in [4,3,None, 1] if x]
    |}
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `typing.List[int]`."];

  assert_type_errors
    {|
      def foo(l: typing.List[int]) -> None:
        a = [x > 0 and x < 0 for x in l]
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.Dict[int, str]) -> None:
        [x if y else 0 for x, y in l.items()]
    |}
    [
    ];

  assert_type_errors
    {|
      def foo() -> typing.Dict[int, int]:
        return { 0: x for x in [4,3,2] }
    |}
    [];

  assert_type_errors
    {|
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[int, str]` but got " ^
      "`typing.Dict[typing.Any, typing.Any]`.";
      "Incompatible variable type [9]: Unable to unpack `int`, expected a `Tuple`.";
    ];

  assert_type_errors
    {|
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d.items() }
    |}
    [];

  assert_type_errors
    {|
    def foo(input: typing.List[str]) -> typing.List[str]:
      return [a.lower() for a in input]
    |}
    [];
  assert_type_errors
    {|
    def foo(input: typing.List[str]) -> typing.List[int]:
      return [str_to_int(a) for a in input]
    |}
    [];
  assert_type_errors
    {|
    def foo(input: typing.Set[str]) -> typing.Set[str]:
      return {a for a in input}
    |}
    [];
  assert_type_errors
    {|
    def foo(input: typing.Set[str]) -> typing.Set[str]:
      return {a.lower() for a in input}
    |}
    [];
  assert_type_errors
    {|
    def foo(a: typing.List[str], b: typing.List[str]) -> int:
      return {x + y for x in a for y in b}
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Set[str]`."];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.List[int]:
        return [x for x in a]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];

  assert_type_errors
    {|
      def f() -> int:
          x = {
              "a": [k[0] for x in {}] for k in []
          }
          return 0
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, str]:
        return { x:x for x, y in a.items() }
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, int]:
        return { y:x for x, y in a.items() }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got " ^
      "`typing.Dict[int, str]`.";
    ];

  assert_type_errors
    {|
      def f() -> None:
        l = lambda y: y
        l(1)
        lambda *y: y
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() if y }
    |}
    [];
  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got" ^
      " `typing.Dict[str, typing.Optional[int]]`.";
    ];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: int_to_int(y) for (x, y) in a.items() if y }
    |}
    [];

  assert_type_errors
    {|
      def foo(d: typing.Dict[str, int]) -> None:
        { k: v for k, v in d }
    |}
    ["Incompatible variable type [9]: Unable to unpack `str`, expected a `Tuple`."]


let test_check_optional _ =
  assert_type_errors
    "def foo() -> str: return None"
    ["Incompatible return type [7]: Expected `str` but got `None`."];

  assert_type_errors
    "def foo() -> typing.Optional[str]: return None"
    [];

  assert_type_errors
    "def foo() -> typing.Optional[int]: return 1"
    [];

  assert_type_errors
    {|
      def foo(flag: bool) -> typing.Optional[float]:
          a = 1.0
          if flag:
            a = None
          return a
    |}
    [];

  assert_type_errors
    "def foo() -> typing.Optional[int]: return 1.0"
    ["Incompatible return type [7]: Expected `typing.Optional[int]` but got `float`."];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> int:
          if optional:
            return optional
          else:
            return -1
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Any:
        if True:
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but " ^
      "type `Any` is specified."
    ];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional or int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[bool, int]` " ^
      "but type `Any` is specified.";
      "Incompatible parameter type [6]: Expected `int` but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional and int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[bool, int]` but " ^
      "type `Any` is specified."
    ]


let test_check_function_parameters _ =
  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1)
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `float`.";
    ];
  assert_type_errors
    {|
      def preprocessed($renamed_i: str) -> None:
        pass
      def foo() -> None:
        preprocessed(1.0)
    |}
    [
      "Incompatible parameter type [6]: Expected `str` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        return int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo(i) -> None:
        int_to_int(i)
    |}
    [];

  assert_type_errors
    {|
      class A:
        def foo(self) -> None:
          int_to_int(self.attribute)
    |}
    ["Undefined attribute [16]: `A` has no attribute `attribute`."];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, None]) -> None: pass
      foo(None)
    |}
    [];

  assert_type_errors
    {|
     def foo(a: typing.Optional[int]) -> int:
      return to_int(a and int_to_str(a))
    |}
    [];

  assert_type_errors
    {|
     def foo(a: typing.Optional[int]) -> int:
      return to_int(a or int_to_str(a))
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(a: int) -> int:
        return a
      x: typing.Optional[int]
      foo(x if x else 1)
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x is not None else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.int_attribute
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Union[OtherAttributes, Attributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      def foo(x) -> None:
        takes_iterable(x)
    |}
    [];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: Expected `typing.Optional[int]` but got `str`."];
  assert_type_errors
    {|
      def foo(a):
        # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: Expected `typing.Optional[int]` but got `str`."];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, "hello")
    |}
    [];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, 1)
    |}
    ["Incompatible parameter type [6]: Expected `str` but got `int`."]


let test_check_function_parameters_with_backups _ =
  assert_type_errors "(1).__add__(1)" [];
  assert_type_errors "(1).__add__(1j)" [];
  assert_type_errors "(1).__add__(1.0)" []


let test_check_function_parameter_errors _ =
  assert_type_errors
    {|
      class Foo:
        attribute: str
      def foo(input: Foo) -> None:
        str_float_to_int(input.attribute, input.undefined)
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `undefined`."];
  assert_type_errors
    {|
      class Foo:
        attribute: str
      def foo(input: Foo) -> None:
        str_float_to_int(input.undefined, input.undefined)
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.attribute)
    |}
    ["Incompatible parameter type [6]: Expected `typing.Optional[str]` but got `unknown`."];
  assert_type_errors
    {|
      class Foo:
        attribute: int
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.undefined)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing.Optional[str]` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ]


let test_check_function_redirects _ =
  assert_type_errors
    {|
      def foo(a: float) -> float:
        return abs(a)
    |}
    []


let test_check_variable_arguments _ =
  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> str:
        return foo ( *b )
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible parameter type [6]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> int:
        return foo ( *b )
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `unknown`."];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo ( *b )
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo('asdf', *b)
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 'asdf' )  # assuming b = []
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 1, 'asdf' )
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        foo ( *b, 'asdf' )  # assuming b = [_]
    |}
    [];

  assert_type_errors
    {|
      def durp(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        durp ( *b, 1.0 )  # assuming b = [_]
    |}
    [
      "Incompatible parameter type [6]: Expected `str` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo('asdf', *b)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ]


let test_check_method_returns _ =
  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."]


let test_check_method_parameters _ =
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr(1)
    |}
    [];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: Expected `str` but got `int`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(input: typing.Any) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Foo:
        def bar(self) -> None:
          def baz(x: int) -> int:
            return x
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def bar(x: int) -> int:
          return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `Foo`."]


let test_check_method_resolution _ =
  assert_type_errors
    {|
      def foo() -> None:
        bar().baz()
    |}
    ["Undefined name [18]: Global name `bar` is undefined."];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.lower()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def derp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def herp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Bar` has no attribute `derp`.";
    ]


let test_check_self _ =
  (* Self parameter is typed. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return 1
        def bar(self) -> str:
          return self.foo()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Other:
          pass

      class Some:
          def one(self) -> None:
              self.two()

          def two(self: Other) -> None:
              pass
    |}
    ["Incompatible parameter type [6]: Expected `Other` but got `Some`."]


let test_check_static _ =
  (* No self parameter in static method. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar(input: str) -> str:
          return input.lower()

      class Bar:
        @classmethod
        def bar(cls, input: str) -> str:
          return input.lower()

        def baz(self) -> None:
          self.bar("")
    |}
    [];

  (* Static method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

        def bar(self) -> None:
          self.foo('asdf')

    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Class method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls, input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls) -> typing.Type[Foo]:
          return cls
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.classmethod('1234')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def staticmethod(i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.staticmethod('1234')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        def instancemethod(self, i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.instancemethod(Foo(), '1234')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."]


let test_check_init _ =
  assert_type_errors
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(renamed_self) -> None:
          renamed_self.attribute = 0
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      def __init__(self) -> None:
        self.attribute: bool = False
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      attribute: int = 1
      def __init__(self) -> None:
        pass
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      attribute: int
      attribute_two: str
      def __init__(self) -> None:
        pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `attribute_two` is declared in class `Foo` to " ^
      "have non-optional type `str` but is never initialized.";
    ];

  assert_type_errors
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        self.attribute = 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0 if True else 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if True:
            self.attribute = 0
          else:
            self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if False:
            return None
          self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if True:
            raise
          self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = unknown if True else unknown2
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` " ^
      "has type `int` but is used as type `unknown`.";
      "Undefined name [18]: Global name `unknown` is undefined.";
      "Undefined name [18]: Global name `unknown2` is undefined.";
    ];

  (* No need to initialize properties. *)
  assert_type_errors
    {|
    class Foo:
      def __init__(sefl) -> None:
        pass
      @property
      def foo() -> str:
        return "asdf"
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 0
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: typing.Optional[int]
        def __init__(self) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      attribute: int
      def __init__(self) -> None:
        self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
    class Foo:
      def __init__(self, x:int) -> None:
        pass
    a = Foo("")
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `a` has type `Foo` " ^
      "but no type is specified.";
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ]


let test_check_attributes _ =
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return self.bar
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo(Bar):
        bar: typing.Optional[int] = None
        def foo(self) -> typing.Optional[int]:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        bar: int
      class Foo(Bar):
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        bar = ... # type: int
        def foo(self) -> int:
          return self.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.bar: int = None
        def f(self) -> str:
            return self.bar
    |}
    [
      "Incompatible attribute type [8]: " ^
      "Attribute `bar` declared in class `Foo` has type `int` but is used as type `None`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];


  assert_type_errors
    ~debug:true
    ~strict:true
    {|
      class Bar:
        def bar() -> None:
          pass
      class Foo:
        bar: typing.Optional[Bar] = None
        def foo(self) -> None:
          self.bar.bar()
    |}
    ["Undefined attribute [16]: Optional type has no attribute `bar`."];

  assert_type_errors
    ~strict:true
    {|
      a = str
      b = 1
    |}
    [
      "Missing global annotation [5]: " ^
      "Globally accessible variable `b` has type `int` but no type is specified.";
    ];

  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo(self) -> int:
        return Bar.bar
    |}
    [];

  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo(self) -> int:
        x = Bar()
        return x.baz
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`. Type `int` expected on " ^
      "line 6, specified on line 4.";
      "Undefined attribute [16]: `Bar` has no attribute `baz`."
    ];

  assert_type_errors
    {|
      class Bar:
        bar: int
      class Foo:
        def foo(self, other: Bar) -> int:
          return other.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but no " ^
      "type is specified.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Any
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but type " ^
      "`Any` " ^ "is specified.";
      "Incompatible return type [7]: Expected `int` but got `str`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int
      def foo(param: Foo) -> int:
        param.bar = 'foo'
        return param.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      bar: int
      def foo() -> int:
        bar = 'foo'
        return bar
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but no " ^
      "type is specified.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = "foo"
        return foo_obj.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int
      class Bar(Foo):
        def foo(self) -> int:
          self.bar = "foo"
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.a = 3
        def foo(self) -> str:
            return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified.";
      "Incompatible return type [7]: Expected `str` but got `int`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a and a.bar:
          return a.bar
        return 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a.bar and a:
          return a.bar
        return 0
    |}
    [
      "Undefined attribute [16]: Optional type has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar, baz = 1, 2
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Undefined error [1]: Problem with analysis.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar, baz = list(range(2))
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Undefined error [1]: Problem with analysis.";
    ];

  assert_type_errors
    {|
      class Foo:
          def foo(self, bar: typing.Optional[int]) -> int:
              self.baz = bar
              if self.baz is None:
                  self.baz = 5
              return self.baz
    |}
    [
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` has type " ^
      "`typing.Optional[int]` but no type is specified.";
      (* TODO(T24330702): we should only report this once. *)
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
    ];

  (* TODO(T25072735): support attribute tests for: class variables, generic annotations *)
  assert_type_errors
    {|
      class Foo:
        bar: typing.ClassVar[int]
      def foo() -> int:
        Foo.bar = "foo"
        return Foo.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Generic[_T]
        def foo(self) -> int:
          self.bar = 0
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type " ^
      "`typing.Generic[Variable[_T]]` but is used as type `int`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Generic[Variable[_T]]`.";
    ];

  (* Static attributes are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        attribute: typing.ClassVar[int] = 1

      def foo() -> str:
        return Foo.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Foo:
        class Bar:
          attribute: int = 1

      def foo() -> str:
        return Foo.Bar().attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  (* Attributes defined in constructor. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
    ];
  assert_type_errors
    {|
      class unittest.TestCase: ...
      class Foo(unittest.TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: int = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [];

  (* Class implements `__getattr__`. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __getattr__(self, attribute) -> str: ...
        def foo(self) -> int:
          return self.undefined
        def bar(self) -> int:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  (* Attributes of other classes are properly resolved. *)
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo:
        def foo(self, bar: Bar) -> int:
          return bar.bar
      def foo(bar: Bar) -> int:
        return bar.bar
    |}
    [];

  (* Any has all attributes. *)
  assert_type_errors
    ~debug:false
    {|
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    [];

  (* We allow instance attributes to be accessed via class objects. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      Foo.attribute
    |}
    [];

  (* Check attribute type propagation. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def foo(self) -> None:
          self.attribute = not_annotated()
          a = self.attribute.something
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `unknown`.";
    ];

  (* Check attribute type variable resolution. *)
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      def bar(wrapper: Wrapper[int]) -> int:
        return wrapper.value
    |}
    [];
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      class WrapperSubclass(Wrapper[int]):
        pass

      def bar(wrapper: WrapperSubclass) -> int:
        return wrapper.value
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Class:
        @property
        def property(self: _T) -> typing.Sequence[_T]: ...
      def foo(c: Class) -> typing.Sequence[Class]:
        return c.property
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Class(typing.Generic[_T]):
        @property
        def property(self) -> _T: ...
      def foo(c: Class[int]) -> int:
        return c.property
    |}
    [];

  (* Do not resolve optional attributes to the optional type. *)
  assert_type_errors
    {|
      class Foo:
        debug: int = 1
      def foo(f: typing.Optional[Foo]) -> int:
        return f.debug
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Optional type has no attribute `debug`.";
    ];

  (* Attributes defined with property decorators. *)
  assert_type_errors
    {|
      class Foo:
        @property
        def prop(self) -> int: ...
      def foo() -> str:
        return Foo().prop
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  (* Attributes defined with getters and setters. *)
  assert_type_errors
    {|
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: int) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
      x: Optional[int]
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` " ^
      "has type `typing.Optional[int]` but is used as type `str`.";
    ];

  assert_type_errors
    {|
      __property__: typing.Any = ...
      x: Optional[int]
      class Foo:
        @__property__
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
        @$local_file$__property__
        def y(self) -> int: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> int:
        foo = Foo()
        return foo.y
    |}
    []


let test_check_globals _ =
  assert_type_errors
    {|
      constant: int = 1
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      nasty_global = foo()
      def foo() -> int:
        a = nasty_global
        return 0
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `nasty_global` " ^
      "has type `int` but no type is specified.";
    ]


let test_check_immutables _ =
  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as " ^
      "type `str`.";
    ];

  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      constant = "hi"
    |}
    [];

  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant: str
      constant = "hi"
    |}
    [];

  assert_type_errors
    {|
    constant: typing.Union[int, str]
    def foo() -> None:
      global constant
      constant = 1
    |}
    [];

  assert_type_errors
    {|
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    [];

  assert_type_errors
    {|
      constant: typing.Optional[str]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
    constant: typing.Optional[int]
    def foo() -> int:
      if constant is not None:
        return 0
      return constant
  |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors
    {|
    constant
    def foo() -> None:
      global constant
      constant = 1
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
    constant: typing.Any
    def foo() -> None:
      global constant
      constant = 1
    |}
    [];

  assert_type_errors
    {|
    constant
    def foo() -> int:
      global constant
      constant = 1
      return constant
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
    constant: int
    def foo(x) -> str:
      if x > 10:
        global constant
        constant: str
      return constant
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `typing.Union[int, str]`."
    ];

  assert_type_errors
    {|
    def foo(x: int) -> None:
      x = "hi"
    |}
    ["Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."];

  assert_type_errors
    {|
    def foo(x: typing.Optional[int]) -> None:
      x = 1
    |}
    [];

  assert_type_errors
    {|
    def foo(x: int) -> None:
      x: str
      x = "hi"
    |}
    [];

  assert_type_errors
    {|
    def foo() -> None:
      x: int = "hi"
    |}
    ["Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."];

  assert_type_errors
    {|
    def foo() -> None:
      x = 1
      y: str
      y = x
      x = y
    |}
    ["Incompatible variable type [9]: y is declared to have type `str` but is used as type `int`."];

  assert_type_errors
    {|
    def foo(x) -> None:
      if x > 10:
        y: int
      else:
        y: str

      y = "hi"
    |}
    [];

  assert_type_errors
    {|
    def foo(x) -> None:
      if x > 10:
        y: int
      else:
        y: str
      y = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo():
        attribute
      def bar() -> None:
        foo = Foo()
        foo.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Undefined name [18]: Global name `attribute` is undefined.";
      "Undefined attribute [16]: `Foo` has no attribute `attribute`.";
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
        x = { 'a': 'b' }
    |}
    [];

  assert_type_errors
    ~debug:false
    {|
      constant = 1
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];

  assert_type_errors
    ~debug:false
    ~infer:true
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified."
    ];

  (* TODO(T25072735): error on typing.Any (incompatible usage) rather than suggest it *)
  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Union[int, str]` but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Union[int, str]` but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = None
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Optional[int]` but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Optional[int]` but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = 1.0
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `float` " ^
      "but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `float` " ^
      "but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = A()
      def bar() -> None:
        global constant
        constant = B()
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `A` but " ^
      "no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `A` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      constant
      class Foo():
        constant
      def foo() -> None:
        foo = Foo()
        foo.constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing attribute annotation [4]: Attribute `constant` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Undefined name [18]: Global name `constant` is undefined.";
      "Undefined attribute [16]: `Foo` has no attribute `constant`.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `str` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `unknown`.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name', 'attribute']
        def foo(self) -> str:
          return self.name + self.attribute + self.constant
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `constant`.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
        def __init__(self) -> None:
          self.name: int = 1
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ]


let test_check_named_arguments _ =
  assert_type_errors
    {|
      def bar()->int:
        return str_float_to_int(i="",f=2.0) + str_float_to_int(f=1.0,i="bar")
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int): ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int = 10): ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      def bar()->int:
        return str_float_to_int(i="")
    |}
    ["Missing argument [20]: Call `str_float_to_int` expects argument `f`."];
  assert_type_errors
    {|
      def bar()->int:
        return 1 + str_float_to_int(i=2.0,f=1)
      def foo()->int:
        return str_float_to_int(f="No",i="Hi")
    |}
    [
      "Incompatible parameter type [6]: Expected `str` but got `float`.";
      "Incompatible parameter type [6]: Expected `float` but got `str`.";
    ]


let test_check_missing_return _ =
  assert_type_errors
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but no return type is specified."];

  assert_type_errors
    {|
      def foo() -> typing.Any:
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but type `Any` is specified."];

  assert_type_errors
    {|
      def foo() -> None:
        return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."];

  assert_type_errors
    {|
      def foo():
        return
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];

  assert_type_errors
    {|
      def foo():
        return None
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified.";];

  assert_type_errors
    {|
      def foo() -> None:
        return None
    |}
    [];

  assert_type_errors
    {|
      def foo(a):
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but no return type is " ^
      "specified."
    ];

  assert_type_errors
    {|
      def foo() -> None:
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Undefined name [18]: Global name `a` is undefined.";
      "Incompatible return type [7]: Expected `None` but got `int`."
    ];

  (* Don't report in non-debug mode. *)
  assert_type_errors
    ~debug:false
    {|
      def foo():
        return 1
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      def foo():
        pass
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      1 + 'asdf'  # report in top-level function
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."]


let test_check_yield _ =
  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1
    |}
    [];

  assert_type_errors
    {|
      def foo(i: int) -> typing.Iterable[int]:
        if i > 2:
          return
        else:
          yield i
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1.0
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` " ^
      "but got `typing.Generator[float, None, None]`.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield from [1]
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield from [""]
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` " ^
      "but got `typing.Generator[str, None, None]`."
    ];

  assert_type_errors
    {|
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Iterable[None]:
        yield
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Any:
        yield
    |}
    [
      "Missing return annotation [3]: Returning `typing.Generator[None, None, None]` " ^
      "but type `Any` is specified."
    ];

  assert_type_errors
    {|
      def foo():
        yield
    |}
    [
      "Missing return annotation [3]: Returning `typing.Generator[None, None, None]` " ^
      "but no return type is specified."
    ];

  assert_type_errors
    {|
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];

  assert_type_errors
    {|
      def foo(flag: bool) -> typing.Generator[int, None, None]:
        if flag:
          return
        yield 1
    |}
    [];
  assert_type_errors
    {|
      async def foo(flag: bool) -> typing.AsyncGenerator[int, None]:
        if flag:
          return
        yield 1
    |}
    []


let test_check_ternary _ =
  assert_type_errors
    {|
      def foo() -> int:
        x: typing.Optional[int]
        y: int
        z = x if x else y
        return z
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        return y if y else 5
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> int:
        if x > 10:
          y = None
        else:
          y = 5
        y = y if y else 0
        return y
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        x: int
        return y if x else 5
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
          int_to_int(x) if x else 0
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
          return int_to_int(x if x is not None else 1)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        a, b = ("hi", int_to_int(x) if x is not None else 1)
        return b
    |}
    [];
  assert_type_errors
    {|
      def f(s: str) -> None:
        pass

      def pick_alternative(s: typing.Optional[str]) -> None:
        f("foo" if s is None else s)

      def pick_alternative2(s: typing.Optional[str]) -> None:
        f("foo" if not s else s)

      def pick_alternative3(s: typing.Optional[str]) -> None:
        x = "foo" if s is None else s
        f(x)

      def pick_target(s: typing.Optional[str]) -> None:
        f(s if s is not None else "foo")

      def pick_target2(s: typing.Optional[str]) -> None:
        f(s if s else "foo")

      def pick_target3(s: typing.Optional[str]) -> None:
        x = s if s is not None else "foo"
        f(x)
    |}
    []


let test_check_union _ =
  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `typing.Union[int, str]` but got `float`."];

  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        if True:
          return 1
        else:
          return 'foo'
    |}
    [];

  assert_type_errors
    {|
      def takes_int(a: int) -> None: ...
      def takes_str(a: str) -> None: ...

      def foo(a: typing.Union[str, int]) -> None:
        if isinstance(a, str):
          takes_str(a)
        else:
          takes_int(a)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, int, float]) -> int:
        if isinstance(a, int):
          return a
        else:
          return a
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `typing.Union[float, str]`."
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    [
      "Incompatible return type [7]: Expected `float` but got `typing.Union[int, str]`."
    ];

  assert_type_errors
    {|
      variable: typing.Union[typing.Optional[int], typing.Optional[str]] = None
      def ret_opt_int() -> typing.Optional[int]:
          return None
      variable = ret_opt_int()
    |}
    []


let test_check_return_joining _ =
  assert_type_errors
    {|
      def foo():
        if True:
          return 1
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is " ^
      "specified."
    ];
  assert_type_errors
    {|
      def foo():
        if True:
          return 1
        else:
          return 2.0
    |}
    ["Missing return annotation [3]: Returning `float` but no return type is specified."];
  assert_type_errors
    {|
      def foo():
        if True:
          return None
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[str]` but no return type is " ^
      "specified."
    ];
  assert_type_errors
    {|
      def foo():
        if True:
          return A()
        else:
          return B()
    |}
    ["Missing return annotation [3]: Returning `A` but no return type is specified."]


let test_check_nested _ =
  assert_type_errors
    {|
      def foo() -> None:
        def nested() -> None:
          int_to_int(1.0)
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `float`.";
      "Incompatible parameter type [6]: Expected `int` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> None:
        def g() -> None:
          return
        a = g()
    |}
    [];

  (* Nesting behaves differently for the toplevel function. *)
  assert_type_errors
    ~qualifier:(Access.create "shadowing")
    {|
      def shadowing(i: int) -> None: ...
      shadowing('asdf')  # `shadowing` is not replaced with a dummy entry in the globals map.
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."]


let test_check_invalid_constructor _ =
  assert_type_errors
    {|
      class C:
        def __init__(self) -> None:
          return
    |}
    [];

  assert_type_errors
    {|
      class C:
        def __init__(self) -> int:
          return 0
    |}
    [
      "Incompatible constructor annotation [17]: `__init__` is annotated as " ^
      "returning `int`, but it should return `None`.";
    ]


let test_check_variable_restrictions _ =
  assert_type_errors
    {|
       def f(x: str) -> int:
         return variable_restricted_identity(x)
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
       def f(x: str) -> str:
         return variable_restricted_identity(x)
    |}
    [];
  assert_type_errors
    {|
       def f(x: float) -> str:
         return variable_restricted_identity(x)
    |}
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];

  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(t: T) -> None: ...
      def bar(t: T) -> None:
        foo(t)
    |}
    []


let test_check_variable_bindings _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        str_to_int(t)
    |}
    ["Incompatible parameter type [6]: Expected `str` but got `Variable[T (bound to int)]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo() -> T:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `Variable[T (bound to int)]` but got `float`."]


let test_check_refinement _ =
  assert_type_errors
    {|
    def takes_int(a: int) -> None: pass
    def foo() -> None:
      x: float
      x = 1
      takes_int(x)
      x = 1.0
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[typing.Any] = []
        l = [1]
        l.append('asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = []
        l.append('a')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = None
        l.append('a')
    |}
    [
      "Incompatible variable type [9]: l is declared to have type `typing.List[int]` but is " ^
      "used as type `None`.";
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          y = x
        return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, str]) -> int:
        if isinstance(x, str):
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          raise
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          x = 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          continue
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[float]) -> typing.Optional[int]:
        if x is not None:
          return int(x)
        return x
    |}
    [];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def foo(self) -> None:
              if self.a is None:
                  self.a = 5
    |}
    [];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def bar(self) -> int:
              if self.a is not None:
                  return self.a
              else:
                  return 1
    |}
    [];
  assert_type_errors
    {|
      def bar(x: typing.Optional[int]) -> None:
          if x and int_to_int(x) < 0:
              y = 1
    |}
    [];
  assert_type_errors
    {|
      def bar(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
          if not input:
            input = set()
          return input
    |}
    [];

  assert_type_errors
    {|
      def bar(input: typing.Optional[int]) -> int:
          if not input:
            input = not_annotated()
          return input
    |}
    [
      "Incompatible variable type [9]: input is declared to have type `typing.Optional[int]` " ^
      "but is used as type `unknown`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ]


let test_check_toplevel _ =
  assert_type_errors
    "int_to_int(1.0)"
    ["Incompatible parameter type [6]: Expected `int` but got `float`."];

  assert_type_errors
    {|
      a: int = None
      def foobar() -> None:
          b: int = None
    |}
    [
      "Incompatible variable type [9]: a is declared to have type `int` " ^
      "but is used as type `None`.";
      "Incompatible variable type [9]: b is declared to have type `int` but is used as type `None`."
    ]


let test_check_tuple _ =
  assert_type_errors
    {|
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `float`."];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, 2, 3)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, string]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, string]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo()-> typing.Tuple[int, ...]:
        return tuple([1,2,3])
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return tuple([""])
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[str, ...]`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Tuple[int, int, str]) -> typing.Tuple[str, int]:
        a, *b = x
        return b
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[str, int]` but " ^
      "got `typing.Tuple[int, str]`.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1,)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1, "")
    |}
    [];

  assert_type_errors
    {|
      def foo()-> typing.Tuple:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo()-> typing.Tuple:
        return (1, 2)
    |}
    [];
  assert_type_errors
    {|
      def bar(z: typing.Optional[int]) -> typing.Tuple[int, typing.Optional[int]]:
          return 1, int_to_int(z) if z is not None else None
    |}
    [];

  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.d
    |}
    [
      "Undefined attribute [16]: `typing.Any` has no attribute `__add__`.";
      "Undefined attribute [16]: `T` has no attribute `d`.";
    ];
  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y = t
        x, y, z = t
        x, y, z, other = t
    |}
    [];
  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
    [

    ]


let test_check_meta _ =
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[float], input)
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[float]`."];
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[unknown], input)
    |}
    ["Undefined type [11]: Type `unknown` is not defined."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      S = typing.TypeVar('S')
      class C(typing.Generic[T]): pass
      def foo(input) -> None:
        typing.cast(C[int], input)
      class D(typing.Generic[T, S]): pass
      def foo(input) -> None:
        typing.cast(D[int, float], input)
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> C:
        return C.__construct__()
      def boo() -> Subclass:
        return Subclass.__construct__()
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> C:
        return Subclass.__construct__()
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> Subclass:
        return C.__construct__()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> typing.Type[Foo]:
          return type(self)
        def bar(self) -> typing.Type[int]:
          return type(1)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        ATTRIBUTE: typing.ClassVar[int] = 1
        def foo(self) -> int:
          return type(self).ATTRIBUTE
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(t: T) -> str:
        return type(t).__name__
    |}
    [];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return type(x).__name__
    |}
    []


let test_check_redundant_cast _ =
  assert_type_errors
    {|
      def foo(x: int) -> None:
        typing.cast(int, x)
    |}
    ["Redundant cast [22]: The value being cast is already of type `int`."];
  assert_type_errors
    {|
      def foo(x: str) -> None:
        typing.cast(int, x)
    |}
    [];
  assert_type_errors
    {|
      def foo(x) -> None:
        typing.cast(int, x)
    |}
    []

let test_check_assert _ =
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional or len(optional) > 0:
          pass
    |}
    ["Incompatible parameter type [6]: Expected `typing.Sized` but got `typing.Optional[str]`."];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional is None or len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional and len(optional) > 0:
          pass
    |}
    []


let test_check_excepts _ =
  assert_type_errors
    {|
      class Exception: pass
      def takes_exception(e: Exception) -> None: pass
      def foo() -> None:
        try:
          pass
        except Exception as e:
          takes_exception(e)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Optional[int]:
        try:
          x = 1
        except:
          return None
        else:
          return x
    |}
    []


let test_check_async _ =
  assert_type_errors
    {|
      async def foo() -> int: return 1
      def bar() -> None:
        await foo()
    |}
    [];
  assert_type_errors
    {|
      def bar(a: typing.Awaitable[int]) -> int:
        return await a
    |}
    [];
  assert_type_errors
    {|
      def bar(a: IsAwaitable) -> int:
        await a
        return 0
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C(typing.Awaitable[T]): ...

      def foo(c: C) -> int:
        return (await c)
    |}
    ["Incompatible return type [7]: Expected `int` but got `unknown`."];

  assert_type_errors
    {|
      def bar(a: IsAwaitable) -> int:
        return (await a)
    |}
    [];

  assert_type_errors
    {|
      def bar(a: int) -> None:
        await a
    |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `int`."];
  assert_type_errors
    ~debug:false
    {|
      def bar(a: typing.Any) -> None:
        await a
    |}
    [];
  assert_type_errors
    {|
      @asyncio.coroutines.coroutine
      def get() -> typing.Generator[typing.Any, None, int]: ...
      async def foo() -> int:
        awaited = await get()
        return awaited
    |}
    [];
  assert_type_errors
    {|
      async def foo() -> typing.AsyncGenerator[int, None]:
        yield 1
    |}
    []


let test_check_behavioral_subtyping _ =
  (* Strengthened postcondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo() -> float: return 1.0
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Returned type `float` is not a subtype of the overridden return `int`."
    ];

  assert_type_errors
    {|
      class Foo():
        def foo() -> float: ...
      class Bar(Foo):
        def foo() -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo() -> None: pass
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Returned type `None` is not a subtype of the overridden return `int`."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo(): pass
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Returned type `unknown` is not a subtype of the overridden return `int`.";
      "Missing return annotation [3]: Returning `None` but no return type is specified."
    ];

  assert_type_errors
    {|
      T = typing.TypeVar("T", bound=int)
      class Foo():
        def foo(self, x: T) -> str:
          return ""
      class Bar(Foo[str]):
        def foo(self, x: str) -> str:
          return x
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Parameter of type `str` is not a supertype of the overridden parameter " ^
      "`Variable[T (bound to int)]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class Foo(typing.Generic[T]):
        def foo(self) -> T:
          return ""
      class Bar(Foo[int]):
        def foo(self) -> int:
          return 1
      class Bar(Foo[None]):
        def foo(self) -> None:
          pass
    |}
    ["Undefined type [11]: Type `Variable[T]` is not defined."];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo():
        def bar(x: int) -> int:
          return 1
      class Bar(Foo):
        def bar(x: int) -> typing.Union[str, int]:
          return 1
    |}
    [
      "Inconsistent override [15]: `Bar.bar` overloads method defined in `Foo` " ^
      "inconsistently. Returned type `typing.Union[int, str]` is not a subtype " ^
      "of the overridden return `int`."
    ];

  (* Weakened precondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo(a: float) -> None: ...
      class Bar(Foo):
        def foo(a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Parameter of type `int` is not a supertype of the overridden parameter `float`."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(a) -> None: ...
      class Bar(Foo):
        def foo() -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(a: int) -> None: ...
      class Bar(Foo):
        def foo(a) -> None: pass  # TODO(T23629633): we should warn on this too.
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo() -> None: ...
      class Bar(Foo):
        def foo(a) -> None: pass
    |}
    [];
  assert_type_errors
    {|
    class Foo():
      def foo(a) -> None: ...
    class Bar(Foo):
      def foo(a: int) -> None: pass
    |}
    [];
  assert_type_errors
    {|
    class Foo():
      def foo(a: int) -> None: pass
    class Bar(Foo):
      def foo(b: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overloads method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature."
    ];
  assert_type_errors
    {|
    class Foo():
      def foo(a: int) -> None: pass
    class Bar(Foo):
      def foo(_a: int) -> None: pass
    |}
    [];
  assert_type_errors ~show_error_traces:true
    {|
      class Foo():
        def bar(x: typing.Union[str, int]) -> None:
          pass
      class Bar(Foo):
        def bar(x: int) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `Bar.bar` overloads method defined in `Foo` " ^
      "inconsistently. Parameter of type `int` is not a " ^
      "supertype of the overridden parameter `typing.Union[int, str]`."
    ];

  (* Don't warn on constructors or class methods. *)
  assert_type_errors
    {|
      class Foo():
        def __init__(self, a: float) -> None: ...
      class Bar(Foo):
        def __init__(self, a: int) -> None: pass
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        @classmethod
        def foo(cls, a: float) -> None: ...
      class Bar(Foo):
        @classmethod
        def foo(cls, a: int) -> None: pass
    |}
    [];

  (* Don't warn on dunder methods. *)
  assert_type_errors
    {|
      class Foo():
        def __dunder__(self, a: float) -> None: ...
      class Bar(Foo):
        def __dunder__(self, a: int) -> None: pass
    |}
    [];

  (* Dunder methods must end with dunder. *)
  assert_type_errors
    {|
      class Foo():
        def __f(self, a: float) -> None: ...
      class Bar(Foo):
        def __f(self, a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.__f` overloads method defined in `Foo` inconsistently. " ^
      "Parameter of type `int` is not a supertype of the overridden parameter `float`.";
    ];

  (* Weakening of object precondition is not possible. *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, o: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other) -> bool: ...
    |}
    [];

  (* Ensure that our preprocessing doesn't clobber starred argument names. *)
  assert_type_errors
    {|
      class Foo():
        def foo( **kwargs) -> int: ...
      class Bar(Foo):
        def foo( **kwargs) -> int: ...
    |}
    [];

  (* Ignore anything involving `Any`. *)
  assert_type_errors
    ~debug:false
    {|
      class Foo():
        def __eq__(self, o: typing.Any) -> typing.Any: ...
      class Bar(Foo):
        def __eq__(self, o: int) -> int: pass
    |}
    [];

  (* Overrides when both *args and **kwargs exist are not inconsistent. *)
  assert_type_errors
    {|
      class Foo():
        def f(self, a: float) -> None: ...
      class Bar(Foo):
        def f(self, *args) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.f` overloads method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature.";
    ];
  assert_type_errors
    {|
      class Foo():
        def f(self, b: int) -> None: ...
      class Bar(Foo):
        def f(self, **kwargs) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.f` overloads method defined in `Foo` inconsistently. " ^
      "Could not find parameter `b` in overriding signature.";
    ];
  assert_type_errors
    {|
      class Foo():
        def f(self, c: str) -> None: ...
      class Bar(Foo):
        def f(self, *args, **kwargs) -> None: pass
    |}
    []


let test_check_collections _ =
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.List[int]]) -> typing.List[int]:
        return input or []
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
        return input or set()
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.Dict[int, str]]) -> typing.Dict[int, str]:
        return input or {}
    |}
    []


let test_check_constructors _ =
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          pass
      def foo() -> Foo:
        return Foo()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo(10)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo('asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int, s: typing.Optional[str] = None) -> None:
          pass
      def foo() -> None:
        Foo('asdf')
        Foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` but got `str`.";
      "Incompatible parameter type [6]: Expected `typing.Optional[str]` but got `int`.";
    ];

  (* Explicit call. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
        def foo(self) -> None:
          Foo.__init__(self, 'asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Super calls. *)
  assert_type_errors
    {|
      class Super:
        def foo(self, i: int) -> None:
          pass
      class Foo(Super):
        def foo(self, i: int) -> None:
          super().foo('asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      class Super:
        def __init__(self, i: int) -> None:
          pass
      class Foo(Super):
        def __init__(self, i: int) -> None:
          super().__init__('asdf')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      class Subclass(A, B):
        def foo(self)->A:
          return super()
        def wrong(self)->B:
          return super()
    |}
    ["Incompatible return type [7]: Expected `B` but got `A`."];

  (* Overloaded constructors. *)
  assert_type_errors
    {|
      class Class:
        @overload
        def __init__(self, i: int) -> None: ...
        def __init__(self, s: str) -> None: ...
      def construct() -> None:
        Class(1)
        Class('asdf')
    |}
    []


let test_check_explicit_method_call _ =
  assert_type_errors
    {|
      class Class:
        def method(self, i: int) -> None:
          pass
      Class.method(object(), 1)
    |}
    []


let test_check_meta_annotations _ =
  assert_type_errors
    {|
      class Class:
        pass
      def foo() -> typing.Type[Class]:
        return Class
    |}
    []


let test_check_unbound_variables _ =
  assert_type_errors
    {|
      def foo(flag) -> int:
        if flag:
          result = 1
        else:
          other = 1
        return result
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `typing.Union[int, typing.Undeclared]`."
    ];
  assert_type_errors
    {|
      def foo() -> int:
        assert unknown is None or 1
        return unknown
    |}
    [
      "Undefined name [18]: Global name `unknown` is undefined.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: bool
        def foo(self) -> int:
          if not self.attribute:
            self.attribute = True
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `bool`."]


let test_check_noreturn _ =
  assert_type_errors
    {|
      def no_return() -> typing.NoReturn:
        return 0
    |}
    ["Incompatible return type [7]: Expected `typing.NoReturn` but got `int`."];
  assert_type_errors
    {|
      def no_return() -> typing.NoReturn:
        # We implicitly return None, so have to accept this.
        return None
    |}
    [];

  assert_type_errors
    {|
      def no_return(input: typing.Optional[int]) -> int:
        if input is None:
          sys.exit(-1)
        return input
    |}
    [];

  assert_type_errors
    {|
      def no_return() -> str:
        sys.exit(0)  # once control flow terminates, we know input won't be returned.
    |}
    [];

  assert_type_errors
    {|
      def may_not_return() -> str:
        if True:
          sys.exit(0)
        else:
          return ""
    |}
    []


let test_check_contextmanager _ =
  assert_type_errors
    {|
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->int:
        with f() as number:
          return number
    |}
    [];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->str:
        with f() as number:
          return number
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f() -> typing.Iterable[int]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [
      (* TODO(T27138096): Iterable should have attribute `__enter__`. *)
      "Undefined attribute [16]: `typing.Iterable[typing.Any]` has no attribute `__enter__`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f() -> typing.Generator[int, None, None]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [];

  assert_type_errors
    {|
      class C:
        @contextlib.contextmanager
        def f(self) -> typing.Iterator[int]:
          yield 1
      def foo(c: C) -> str:
        with c.f() as manager:
          return manager
        return ""
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]


let test_check_callables _ =
  (* Objects with a `__call__` method are callables. *)
  assert_type_errors
    {|
      class Call:
        def __call__(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [];

  assert_type_errors
    {|
      class patch:
        def __call__(self) -> int: ...

      unittest.mock.patch: patch = ...

      def foo() -> None:
        unittest.mock.patch()
        unittest.mock.patch()  # subequent calls should not modify annotation map
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Callable(Foo.bar)[[Named(self, unknown), Named(x, int)], str]`.";
    ];

  assert_type_errors
    {|
      class Call:
        def __call__(self, x: int) -> int: ...
      def foo(call: Call) -> int:
        return call("")
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Callable parameter checks. *)
  assert_type_errors
    {|
      def foo(callable: typing.Callable[[str], None]) -> None:
        callable(1)
    |}
    ["Incompatible parameter type [6]: Expected `str` but got `int`."]


let test_check_assert_functions _ =
  assert_type_errors
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    ~qualifier:(Access.create "foo")
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    []


let test_check_undefined_type _ =
  (* Ensure other errors are not missed when undefined type is thrown. *)
  assert_type_errors
    {|
        class Bar:
            async def undefined(self, x: Derp) -> Derp:
                return x
        class Foo(Bar):
            def error(self) -> int:
                return None
            async def undefined(self, x: Herp) -> Herp:
                return x
      |}
    [
      "Incompatible return type [7]: Expected `int` but got `None`.";
      "Undefined type [11]: Type `Herp` is not defined."
    ]


let test_environment _ =
  (* Type aliases in signatures are resolved. *)
  assert_type_errors
    "hashlib.md5(1.0)"
    ["Incompatible parameter type [6]: Expected `typing.Union[int, str]` but got `float`."];

  (* Type aliases in the class hierarchy are resolved. I.e. we follow the conditional `Collection`
     indirection in typeshed. *)
  assert_type_errors
    {|
      def foo(input: typing.Sequence[int]) -> typing.Iterable[int]:
        return input
    |}
    []


let test_scheduling _ =
  (* Top-level is scheduled. *)
  assert_type_errors
    "'string' + 1"
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Functions are scheduled. *)
  assert_type_errors
    {|
      def bar() -> None: ...
      def foo() -> None:
        'string' + 1
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def bar() -> None:
        def foo() -> None:
          'string' + 1
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Class bodies are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        'string' + 1
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Methods are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> None:
          'string' + 1
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];

  (* Entry states are propagated. *)
  assert_type_errors
    {|
      variable = 1
      def foo() -> int:
        return variable
      def bar() -> str:
        return variable

      variable = 'asdf'
      def bar() -> str:
        return variable
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `variable` has type `int` but " ^
      "no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ]


let test_format_string _ =
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1}'
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1 + "x"}'
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + "x"}'
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + 2}'
    |}
    [];
  assert_type_errors
    {|
      def boo() -> int:
        return 1

      def foo() -> None:
        f'{boo() + "x"}'
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."]


let test_check_data_class _ =
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo('a')
    |}
    ["Incompatible parameter type [6]: Expected `int` but got `str`."];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    ["Too many arguments [19]: Call `Foo.__init__` expects 2 positional arguments, " ^
     "3 were provided."];
  assert_type_errors
    {|
      @dataclasses.dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    ["Too many arguments [19]: Call `Foo.__init__` expects 2 positional arguments, " ^
     "3 were provided."];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x = 1
      def boo() -> None:
          b = Foo(2)
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Too many arguments [19]: Call `Foo.__init__` expects 1 positional argument," ^
      " 2 were provided.";
    ];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo()
    |}
    []


let () =
  "type">:::[
    "initial">::test_initial;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "widen">::test_widen;
    "forward_expression">::test_forward_expression;
    "forward_statement">::test_forward_statement;
    "forward">::test_forward;
    "reveal_type">::test_reveal_type;
    "check_error_traces">::test_show_error_traces;
    "check_with_qualification">::test_check_with_qualification;
    "coverage">::test_coverage;
    "check">::test_check;
    "check_assign">::test_check_assign;
    "check_coverage">::test_check_coverage;
    "check_comprehensions">::test_check_comprehensions;
    "check_optional">::test_check_optional;
    "check_function_parameters">::test_check_function_parameters;
    "check_function_parameters_with_backups">::test_check_function_parameters_with_backups;
    "check_function_parameter_errors">::test_check_function_parameter_errors;
    "check_function_redirects">::test_check_function_redirects;
    "check_invalid_constructor">::test_check_invalid_constructor;
    "check_variable_arguments">::test_check_variable_arguments;
    "check_method_returns">::test_check_method_returns;
    "check_method_parameters">::test_check_method_parameters;
    "check_method_resolution">::test_check_method_resolution;
    "check_self">::test_check_self;
    "check_static">::test_check_static;
    "check_init">::test_check_init;
    "check_attributes">::test_check_attributes;
    "check_globals">::test_check_globals;
    "check_immutables">::test_check_immutables;
    "check_imports">::test_check_imports;
    "check_named_arguments">::test_check_named_arguments;
    "check_missing_return">::test_check_missing_return;
    "check_yield">::test_check_yield;
    "check_ternary">::test_check_ternary;
    "check_union">::test_check_union;
    "check_return_joining">::test_check_return_joining;
    "check_nested">::test_check_nested;
    "check_variable_restrictions">::test_check_variable_restrictions;
    "check_variable_bindings">::test_check_variable_bindings;
    "check_refinement">::test_check_refinement;
    "check_toplevel">::test_check_toplevel;
    "check_tuple">::test_check_tuple;
    "check_meta">::test_check_meta;
    "check_redundant_cast">::test_check_redundant_cast;
    "check_assert">::test_check_assert;
    "check_excepts">::test_check_excepts;
    "check_async">::test_check_async;
    "check_behavioral_subtyping">::test_check_behavioral_subtyping;
    "check_collections">::test_check_collections;
    "check_constructors">::test_check_constructors;
    "check_explicit_method_call">::test_check_explicit_method_call;
    "check_meta_annotations">::test_check_meta_annotations;
    "check_unbound_variables">::test_check_unbound_variables;
    "check_noreturn">::test_check_noreturn;
    "check_contextmanager">::test_check_contextmanager;
    "check_callables">::test_check_callables;
    "check_assert_functions">::test_check_assert_functions;
    "check_undefined_type">::test_check_undefined_type;
    "environment">::test_environment;
    "scheduling">::test_scheduling;
    "check_format_string">::test_format_string;
    "check_dataclass">::test_check_data_class;
  ]
  |> run_test_tt_main
