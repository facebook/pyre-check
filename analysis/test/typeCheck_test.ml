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

let check_errors configuration environment source = (check configuration environment source).errors


let plain_environment =
  let environment = Environment.Builder.create () in
  Environment.populate
    (Environment.reader environment)
    [
      parse {|
        class typing.Sized: ...
        def len(o: typing.Sized) -> int: ...

        class bool(): ...
        class bytes(): ...
        class float():
          def __add__(self, other) -> float: ...
          def __radd__(self, other: float) -> float: ...
          def __neg__(self) -> float: ...
        class int(float):
          def __lt__(self, other) -> int: ...
          def __ne__(self, other) -> bool: ...
          def __add__(self, other: int) -> int: ...
          def __radd__(self, other: int) -> int: ...
          def __neg__(self) -> int: ...
          def __str__(self) -> str: ...
        class complex():
          def __radd__(self, other: int) -> int: ...
        class str(typing.Sized):
          def lower(self) -> str: pass
          def upper(self) -> str: ...
          def substr(self, index: int) -> str: pass
          def __lt__(self, other) -> float: ...
          def __ne__(self, other) -> int: ...
          def __add__(self, other: str) -> str: ...
          def __pos__(self) -> float: ...
          def __repr__(self) -> float: ...
        class obj():
          def non_method_int_to_str(i: int) -> str: ...
        class object():
          def __sizeof__() -> int: pass
        def math.intabs(i: int) -> int: pass
        def int_to_str(i: int) -> str: ...
        def str_to_int(i: str) -> int: ...
        def int_to_bool(i: int) -> bool: ...
        def str_float_to_int(i: str, f: float) -> int: ...
        def str_float_tuple_to_int(t: typing.Tuple[str, float]) -> int: ...
        def nested_tuple_to_int(t: typing.Tuple[typing.Tuple[str, float], float]) -> int: ...
        def return_tuple() -> typing.Tuple[int, int]: ...
        def unknown_to_int(i) -> int: ...
        _T = typing.TypeVar('_T')
        _S = typing.TypeVar('_S')
        class typing.Generic(): pass
        class typing.Iterable(typing.Generic[_T]):
          def __iter__(self)->typing.Iterator[_T]: pass
        class typing.Iterator(typing.Iterable[_T], typing.Generic[_T]):
          def __next__(self) -> _T: ...
        class typing.Sized(): pass
        class tuple(typing.Sized):
          def __init__(self, a:typing.List[int]): ...
          def tuple_method(self, a: int): ...
        class dict(typing.Generic[_T, _S], typing.Iterable[_T]):
          def add_key(self, key: _T) -> None: pass
          def add_value(self, value: _S) -> None: pass
          def add_both(self, key: _T, value: _S) -> None: pass
          def items(self) -> typing.Iterable[typing.Tuple[_T, _S]]: pass
        class list(typing.Iterable[_T], typing.Generic[_T]):
          def __add__(self, x: list[_T]) -> list[_T]: ...
        class set(typing.Iterable[_T], typing.Generic[_T]): pass
        _V = typing.TypeVar('_V')
        class typing.Generator(typing.Generic[_T, _S, _V], typing.Iterable[_T]):
          pass
        class A: ...
        class B(A): ...
        class C(A): ...
        class D(B,C): ...
        _VR = typing.TypeVar("_VR", str, int)
        def value_restricted_identity(x: _VR) -> _VR: pass
        def typing.cast(tp: typing.Type[_T], o) -> _T: ...
        class typing.Awaitable: pass
        def to_int(x: typing.Any) -> int: ...
        class IsAwaitable(typing.Awaitable[int]): pass

        def sum(iterable: typing.Iterable[_T]) -> typing.Union[_T, int]: ...
      |};
    ];
  environment


let environment =
  Environment.reader plain_environment


let empty_define = {
  Define.name = Access.create "$empty";
  parameters = [];
  body = [];
  decorators = [];
  docstring = None;
  return_annotation = None;
  async = false;
  generated = false;
  parent = None;
}


let create
    ?(define = empty_define)
    ?(expected_return = Type.Top)
    ?(immutables = [])
    annotations =
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
  in
  let define =
    +{
      define with
      Define.return_annotation = Some (Type.expression expected_return);
    }
  in
  State.create ~environment ~annotations ~define ()


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
    ?(initial = (fun environment define -> State.initial_forward environment define))
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
    (initial environment (+define))


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
  assert_true (State.less_or_equal (create []) (create []));
  assert_true (State.less_or_equal (create []) (create ["x", Type.integer]));
  assert_true (State.less_or_equal (create []) (create ["x", Type.Top]));
  assert_true
    (State.less_or_equal
       (create ["x", Type.integer])
       (create ["x", Type.integer; "y", Type.integer]));

  (* > *)
  assert_false (State.less_or_equal (create ["x", Type.integer]) (create []));
  assert_false (State.less_or_equal (create ["x", Type.Top]) (create []));

  (* partial order *)
  assert_false (State.less_or_equal (create ["x", Type.integer]) (create ["x", Type.string]));
  assert_false (State.less_or_equal (create ["x", Type.integer]) (create ["y", Type.integer]))


let test_join _ =
  (* <= *)
  assert_state_equal (State.join (create []) (create [])) (create []);
  assert_state_equal
    (State.join (create []) (create ["x", Type.integer]))
    (create ["x", Type.integer]);
  assert_state_equal (State.join (create []) (create ["x", Type.Top])) (create ["x", Type.Top]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.integer; "y", Type.integer]))
    (create ["x", Type.integer; "y", Type.integer]);

  (* > *)
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create []))
    (create ["x", Type.integer]);
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
    (create ["x", Type.integer; "y", Type.integer])


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


let assert_forward precondition statement postcondition =
  let parsed =
    (parse statement
     |> Preprocessing.expand_operators)
    |> function
    | { Source.statements = statement::rest; _ } -> statement::rest
    | _ -> failwith "unable to parse test"
  in
  assert_state_equal
    (create postcondition)
    (List.fold ~f:State.forward ~init:(create precondition) parsed)


let test_forward _ =
  assert_forward ["y", Type.integer] "pass" ["y", Type.integer];

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
  assert_forward ["z", Type.integer] "x, y = z" ["z", Type.integer];

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


  (* Tuples. *)
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
    "x, y = return_tuple()"
    ["x", Type.integer; "y", Type.integer;];
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Unbounded Type.Object)];

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
    ["x", Type.list Type.integer; "y", Type.list Type.integer];

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
    ["x", Type.dictionary ~key:Type.Object ~value:Type.Object];
  assert_forward
    ["iterator", Type.list Type.integer]
    "x = { target: target for target in iterator }"
    [
      "iterator", Type.list Type.integer;
      "x", Type.dictionary ~key:Type.integer ~value:Type.integer
    ];
  assert_forward
    ["iterator", Type.list (Type.tuple [Type.string; Type.integer])]
    "x = { k: v for k, v in iterator }"
    [
      "iterator", Type.list (Type.tuple [Type.string; Type.integer]);
      "x", Type.dictionary ~key:Type.string ~value:Type.integer
    ];
  assert_forward
    ["x", Type.dictionary ~key: Type.string ~value:Type.integer]
    "y = x['derp']"
    ["x", Type.dictionary ~key:Type.string ~value:Type.integer; "y", Type.integer];

  (* Generator. *)
  assert_forward [] "x = (element for target in iterator)" ["x", Type.generator Type.Object];

  (* Lambda. *)
  assert_forward [] "x = lambda: 1.0" ["x", Type.lambda Type.float];

  (* Set. *)
  assert_forward [] "x = { 1.0, }" ["x", Type.set Type.float];
  assert_forward [] "x = { 1.0, 'foo' }" ["x", Type.set (Type.union [Type.float; Type.string])];
  assert_forward
    ["x", Type.list Type.integer]
    "y = { a for a in x }"
    ["x", Type.list Type.integer; "y", Type.set Type.integer];

  (* Starred. *)
  assert_forward [] "x = *1.0" ["x", Type.Object];

  (* Await. *)
  assert_forward
    ["x", Type.awaitable Type.integer]
    "y = await x"
    ["x", Type.awaitable Type.integer; "y", Type.integer];

  (* Redirects. *)
  assert_forward [] "x = str(1)" ["x", Type.string]


let test_forward_immutables _ =
  let assert_forward_immutables
      precondition
      pre_immutables
      statement
      postcondition
      post_immutables =
    let parsed =
      parse statement
      |> Preprocessing.expand_operators
      |> function
      | { Source.statements = statement::rest; _ } -> statement::rest
      | _ -> failwith "unable to parse test"
    in
    assert_state_equal
      (create postcondition ~immutables:post_immutables)
      (List.fold ~f:State.forward ~init:(create precondition ~immutables:pre_immutables) parsed)
  in
  assert_forward_immutables [] [] "global x" ["x", Type.Top] ["x", true];
  assert_forward_immutables [] [] "y: int" ["y", Type.integer] ["y", false];
  assert_forward_immutables [] [] "y: int = x" ["y", Type.integer] ["y", false];
  assert_forward_immutables [] [] "y: int = 'string'" ["y", Type.integer] ["y", false];
  assert_forward_immutables ["y", Type.Top] ["y", false] "y = x" ["y", Type.Top] ["y", false];
  assert_forward_immutables
    ["x", Type.string]
    []
    "y: int = x"
    ["x", Type.string; "y", Type.integer]
    ["y", false];
  assert_forward_immutables
    ["y", Type.string]
    ["y", false]
    "y: int"
    ["y", Type.integer]
    ["y", false]


let test_forward_infer_free_type_variables _ =
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
    ["x", Type.dictionary ~key:Type.string ~value:Type.Bottom] (* We're not yet joining. *)


let test_forward_assert_optionals _ =
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
    ["x", Type.optional Type.Bottom]


let test_forward_assert_isinstance _ =
  assert_forward ["x", Type.Object] "assert isinstance(x, int)" ["x", Type.integer];
  assert_forward
    ["x", Type.Object]
    "assert isinstance(y, str)"
    ["x", Type.Object; "y", Type.string];

  assert_forward
    ["x", Type.Object]
    "assert isinstance(x, (int, str))"
    ["x", Type.union [Type.integer; Type.string]]


let assert_backward precondition statement postcondition =
  let parsed =
    (parse statement)
    |> function
    | { Source.statements = statements; _ } -> statements
  in
  assert_state_equal
    (create postcondition)
    (List.fold_right ~f:State.backward ~init:(create precondition) parsed)


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
    "y = obj.non_method_int_to_str(x)"
    ["y", Type.float; "x", Type.integer];
  assert_backward [] "str_float_tuple_to_int((x, y))" ["x", Type.string; "y", Type.float];
  assert_backward
    []
    "nested_tuple_to_int(((x, y), z))"
    ["x", Type.string; "y", Type.float; "z", Type.float];
  (* TODO(szhu): Extend implementation to pass starred and unstarred tests *)
  assert_backward [] "str_float_to_int(*(x, y))" []; (* "x", Type.string; "y", Type.float *)
  assert_backward
    []
    "str_float_to_int(**{'s': x, 'f': y})"
    [] (* "x", Type.string; "y", Type.float *)


let fixpoint_parse source =
  parse source
  |> Preprocessing.preprocess
  |> Preprocessing.defines
  |> (fun defines -> List.nth_exn defines 1)


let test_fixpoint_forward _ =
  let assert_fixpoint_forward source expected =
    let { Node.value = define; _ } as define_node = fixpoint_parse source in
    assert_equal
      ~cmp:Fixpoint.equal
      ~printer:(fun fixpoint -> Format.asprintf "%a" Fixpoint.pp fixpoint)
      ~pp_diff:(diff ~print:Fixpoint.pp)
      expected
      (Fixpoint.forward
         (Cfg.create define)
         ~initial:(State.initial_forward environment define_node))
  in
  assert_fixpoint_forward
    {| def foo(): pass |}
    (Int.Table.of_alist_exn [
        0, create []; (* Entry *)
        1, create []; (* Exit *)
        4, create []; (* Pass *)
      ]);
  assert_fixpoint_forward
    {|
     def foo():
       x = y
    |}
    (Int.Table.of_alist_exn [
        0, create [];
        1, create ["x", Type.Top];
        4, create [];
      ]);
  assert_fixpoint_forward
    {|
     def foo(y: int):
       x = y
    |}
    (Int.Table.of_alist_exn [
        0, create ~immutables:["y", false] ["y", Type.integer];
        1, create ~immutables:["y", false] [
          "x", Type.integer;
          "y", Type.integer;
        ];
        4, create ~immutables:["y", false] ["y", Type.integer];
      ]);
  assert_fixpoint_forward
    {|
     def foo(y: int):
       if True:
         x = y
       else:
         x = z
    |}
    (Int.Table.of_alist_exn [
        0, create ~immutables:["y", false] ["y", Type.integer]; (* Entry *)
        1, create ~immutables:["y", false] [
          "x", Type.Top;
          "y", Type.integer;
        ]; (* Exit *)
        4, create ~immutables:["y", false] ["y", Type.integer]; (* If *)
        5, create ~immutables:["y", false] [
          "x", Type.Top;
          "y", Type.integer;
        ]; (* Join *)
        6, create ~immutables:["y", false] ["y", Type.integer]; (* Body *)
        7, create ~immutables:["y", false] ["y", Type.integer]; (* Orelse *)
        8, create ~immutables:["y", false] [
          "x", Type.Top;
          "y", Type.integer;
        ]; (* Return *)
      ])


let test_fixpoint_backward _ =
  let assert_fixpoint_backward source expected =
    let { Node.value = define; _ } as define_node = fixpoint_parse source in
    assert_equal
      ~cmp:Fixpoint.equal
      ~printer:(fun fixpoint -> Format.asprintf "%a" Fixpoint.pp fixpoint)
      ~pp_diff:(diff ~print:Fixpoint.pp)
      expected
      (Fixpoint.backward
         (Cfg.create define)
         ~initial_forward:(State.initial_forward environment define_node)
         ~initialize_backward:(State.initial_backward ~environment define_node))
  in
  assert_fixpoint_backward
    {| def foo(): pass |}
    (Int.Table.of_alist_exn [
        0, create ["$return", Type.Top]; (* Entry *)
        1, create ["$return", Type.Top]; (* Exit *)
        4, create ["$return", Type.Top]; (* Pass *)
      ]);
  assert_fixpoint_backward
    {| def foo() -> int: pass |}
    (Int.Table.of_alist_exn [
        0, create ~expected_return:Type.integer ["$return", Type.integer];
        1, create ~expected_return:Type.integer ["$return", Type.integer];
        4, create ~expected_return:Type.integer ["$return", Type.integer];
      ]);
  assert_fixpoint_backward
    {|
     def foo() -> int:
       x = y
       return x
    |}
    (Int.Table.of_alist_exn [
        0, create
          ~expected_return:Type.integer
          ["$return", Type.integer; "x", Type.integer; "y", Type.integer];
        1, create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        4, create
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
        0, create
          ~expected_return:Type.integer
          ["$return", Type.integer; "y", Type.integer];
        1, create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        4, create
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
        0, create
          ~expected_return:Type.integer
          ["$return", Type.integer; "x", Type.integer; "y", Type.integer; "z", Type.integer];
        1, create
          ~expected_return:Type.integer
          ["$return", Type.integer];
        4, create
          ~expected_return:Type.integer
          ["$return", Type.integer];
      ]);

  let b = Type.Primitive ~~"B" in
  let c = Type.Primitive ~~"C" in
  let tuple = Type.tuple [b;c] in
  assert_fixpoint_backward
    {|
     def foo() -> typing.Tuple[B,C]:
       x = y
       return (x,z)
    |}
    (Int.Table.of_alist_exn [
        0, create ~expected_return:tuple ["$return", tuple; "x", b; "y", b; "z", c];
        1, create ~expected_return:tuple ["$return", tuple];
        4, create ~expected_return:tuple ["$return", tuple];
      ])


let assert_type_errors
    ?(gradual = false)
    ?(autogenerated = false)
    ?(debug = true)
    ?(strict = false)
    ?(declare = false)
    ?(infer = false)
    ?(show_error_traces = false)
    ?(ignore_lines = [])
    source
    errors =
  let source =
    let metadata =
      Source.Metadata.create
        ~autogenerated
        ~debug
        ~declare
        ~ignore_lines
        ~strict
        ~version:3
        ()
    in
    parse source
    |> (fun source -> { source with Source.metadata })
    |> Preprocessing.preprocess
  in
  let environment =
    let environment = Environment.Builder.copy plain_environment in
    Environment.populate (Environment.reader environment) [source];
    Environment.reader environment
  in
  let descriptions =
    List.map
      ~f:(fun error -> Error.description error ~detailed:show_error_traces)
      (check_errors
         (Configuration.create
            ~gradual
            ~debug
            ~strict
            ~declare
            ~infer
            ())
         environment
         source)
  in
  let description_list_to_string descriptions =
    Format.asprintf "%a" Sexp.pp (sexp_of_list sexp_of_string descriptions)
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:description_list_to_string
    ~pp_diff:
      (diff
         ~print:(fun format errors ->
             Format.fprintf format "%s" (description_list_to_string errors)))
    errors
    descriptions


let test_ignore_lines _ =
  let assert_gradual_errors = assert_type_errors ~gradual:true in
  assert_gradual_errors ~ignore_lines:[1,[]] "def foo() -> int: return 1.0" [];
  assert_gradual_errors ~ignore_lines:[1,[]] "def foo() -> str: return 1.0" [];
  assert_gradual_errors ~ignore_lines:[1,[]] "def foo() -> str: return" [];
  assert_gradual_errors ~ignore_lines:[1,[]] "def foo() -> typing.List[str]: return 1" [];
  assert_gradual_errors
    ~ignore_lines:[1,[]; 2,[]]
    "def foo() -> str: return 1.0\ndef bar() -> int: return '' "
    [];
  assert_gradual_errors ~ignore_lines:[2,[]] "class A: pass\ndef foo() -> A: return 1" [];
  assert_gradual_errors ~ignore_lines:[1,[]] "def foo() -> str: return bar()" [];
  assert_gradual_errors
    ~debug:false
    ~ignore_lines:[0,[]]
    {|
      def foo() -> other:
        result = 0
        if True:
          result = durp()
        return result
    |}
    [];
  assert_gradual_errors ~ignore_lines:[1,[7]] "def foo() -> typing.List[str]: return 1" [];
  assert_gradual_errors
    ~ignore_lines:[1,[7]; 2,[7]]
    "def foo() -> str: return 1.0\ndef bar() -> int: return '' "
    []


let test_show_error_traces _ =
  assert_type_errors ~show_error_traces:true
    "def foo() -> int: return 1.0"
    [
      "Incompatible return type [7]: expected `int` but got `float`. Type `int` expected on line " ^
      "1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: expected `str` but function does not return."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> typing.List[str]: return 1"
    [
      "Incompatible return type [7]: expected `typing.List[str]` but got `int`. Type " ^
      "`typing.List[str]` expected on line 1, specified on line 1.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [
      "Incompatible return type [7]: expected `typing.Dict[typing.Any, typing.Any]` but got " ^
      "`typing.Dict[unknown, unknown]`. Type `typing.Dict[typing.Any, typing.Any]` expected on " ^
      "line 3, specified on line 3.";
    ];

  assert_type_errors ~show_error_traces:true
    "def foo(): pass"
    [
      "Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[typing.Any]` but no return type " ^
      "is specified. Type `typing.Optional[typing.Any]` was returned on line 3, return type " ^
      "should be specified on line 2.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        self.test_field = ""
    |}
    [
      "Incompatible type [8]: field test_field declared in class Foo has type `int` but " ^
      "is used as type `str`. Field test_field declared on line 3, incorrectly used on line " ^
      "5.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [
      "Incompatible type [9]: constant is declared to have type `int` but is used as type `str`. " ^
      "constant incorrectly used on line 5."
    ];

  assert_type_errors ~show_error_traces:true
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        test_field = 0
    |}
    [
      "Uninitialized field [13]: field test_field is declared in class Foo to have non-" ^
      "optional type `int` but is never initialized. Field test_field is declared on line 3, " ^
      "never initialized and therefore must be `typing.Optional[int]`."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        field = x
      class Bar:
        def bar(self) -> str:
          foo = Foo()
          foo.field = 'string'
          return foo.field
    |}
    [
      "Missing annotation [4]: Field field of class Foo has type `str` but no type " ^
      "is specified. Field field declared on line 3, type `str` deduced from test.py:7:4.";
    ]


let test_coverage _ =
  let assert_coverage source expected =
    let { Node.value = define; _ } as define_node = fixpoint_parse source in
    let coverage =
      Fixpoint.forward
        (Cfg.create define)
        ~initial:(State.initial_forward environment define_node)
      |> Fixpoint.exit
      |> (fun exit -> Option.value_exn exit)
      |> State.coverage
    in
    assert_equal coverage expected
  in
  assert_coverage
    {| def foo(): pass |}
    { State.full = 0; partial = 0; untyped = 0 };
  assert_coverage
    {|
     def foo(y: int):
       if True:
         x = y
       else:
         x = z
    |}
    { State.full = 1; partial = 0; untyped = 1 }


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
    ["Incompatible return type [7]: expected `int` but got `float`."];

  assert_type_errors
    "def foo() -> str: return 1.0"
    ["Incompatible return type [7]: expected `str` but got `float`."];

  assert_type_errors
    "def foo() -> str: return"
    ["Incompatible return type [7]: expected `str` but function does not return."];

  assert_type_errors
    "def foo() -> typing.List[str]: return 1"
    ["Incompatible return type [7]: expected `typing.List[str]` but got `int`."];

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
    [
      "Incompatible return type [7]: expected `typing.Dict[typing.Any, typing.Any]` but got " ^
      "`typing.Dict[unknown, unknown]`.";
    ];

  assert_type_errors
    {|
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];

  assert_type_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [
      "Incompatible return type [7]: expected `str` but got `float`.";
      "Incompatible return type [7]: expected `int` but got `str`.";
    ];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return A()"
    [];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: expected `A` but got `int`."];

  assert_type_errors
    "def bar() -> str: return ''\ndef foo() -> str: return bar()"
    [];

  (* Angelic assumption about unknown functions in non-debug mode. *)
  assert_type_errors ~debug:false
    "def foo() -> str: return bar()"
    [];

  assert_type_errors
    "def foo() -> str: return bar()"
    ["Incompatible return type [7]: expected `str` but got `unknown`."];

  assert_type_errors
    {|
      class other(): pass
      def foo() -> other:
        result = 0
        if True:
          result = durp()
        return result
    |}
    ["Incompatible return type [7]: expected `other` but got `unknown`."];

  assert_type_errors
    {|
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
    [];

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
      def x()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      def x()->int:
        "Comment"
        """
        Multi-line Comment
        """
        pass
    |}
    [];

  assert_type_errors
    {|
      def x()->int:
        return None
    |}
    [
      "Incompatible return type [7]: expected `int` but got `typing.Optional[typing.Any]`."
    ];

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
      "Incompatible return type [7]: expected `typing.Generator[str, None, None]` " ^
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
      "Incompatible return type [7]: expected `typing.Set[int]` but got `typing.Set[str]`."
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
      "Incompatible return type [7]: expected `str` but got `int`.";
      "Incompatible return type [7]: expected `str` but got `int`."

    ]


let test_check_gradual _ =
  let assert_gradual = assert_type_errors ~debug:false ~gradual:true ~strict:false ~declare:false in

  assert_gradual
    "def foo(): pass"
    [];

  (* Suppression of `typing.Any`. *)
  assert_gradual
    {|
      def foo() -> str:
        d: typing.Any
        return d
    |}
    [];
  assert_gradual
    {|
      def foo() -> typing.Dict[str, typing.Any]:
        d: typing.Dict[typing.Any, typing.Any];
        return d
    |}
    [];
  assert_gradual
    {|
      def foo() -> typing.Any:
        d: typing.Dict[typing.Any, typing.Any];
        return d
    |}
    [];
  assert_gradual
    {|
      def foo(input) -> None:
        generator = (i for i in input)
        sum(generator)
    |}
    []


let test_strict _ =
  let assert_strict_errors = assert_type_errors ~gradual:true ~debug:false ~strict:true  in

  assert_strict_errors
    "def foo(): pass"
    [
      "Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`."
    ];
  assert_strict_errors "def foo() -> None: return" [];
  assert_strict_errors "def foo() -> float: return 1.0" [];
  assert_strict_errors "def foo() -> float: return 1" [];
  assert_strict_errors
    "def foo() -> int: return 1.0"
    ["Incompatible return type [7]: expected `int` but got `float`."];
  assert_strict_errors
    "def foo() -> str: return 1.0"
    ["Incompatible return type [7]: expected `str` but got `float`."];
  assert_strict_errors
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: expected `str` but function does not return."
    ];
  assert_strict_errors
    "def foo() -> typing.List[str]: return 1"
    [
      "Incompatible return type [7]: expected `typing.List[str]` but got `int`."
    ];
  assert_strict_errors "def foo() -> typing.List[str]: return []" [];
  assert_strict_errors "def foo() -> typing.Dict[str, int]: return {}" [];
  assert_strict_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [
      "Incompatible return type [7]: expected `str` but got `float`.";
      "Incompatible return type [7]: expected `int` but got `str`.";
    ];
  assert_strict_errors "class A: pass\ndef foo() -> A: return A()" [];
  assert_strict_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: expected `A` but got `int`."];
  assert_strict_errors "def bar() -> str: return ''\ndef foo() -> str: return bar()" [];
  assert_strict_errors
    {|
      def foo(x:int):
        return 0
      foo(y)
    |}
    [
      "Missing return annotation [3]: Returning `int` but no return type is specified.";
      "Incompatible parameter type [6]: 1st parameter `x` to call `foo` expected `int` but " ^
      "got `unknown`.";
    ]


let test_declare _ =
  let assert_declare_errors source =
    assert_type_errors ~gradual:true ~debug:false ~declare:true source []
  in
  assert_declare_errors "def foo(): pass";
  assert_declare_errors "def foo() -> None: return";
  assert_declare_errors "def foo() -> float: return 1.0";
  assert_declare_errors "def foo() -> float: return 1";
  assert_declare_errors "def foo() -> int: return 1.0";
  assert_declare_errors "def foo() -> str: return 1.0";
  assert_declare_errors "def foo() -> str: return";
  assert_declare_errors "def foo() -> typing.List[str]: return 1";
  assert_declare_errors "def foo() -> typing.List[str]: return []";
  assert_declare_errors "def foo() -> typing.Dict[str, int]: return {}";
  assert_declare_errors "def foo() -> str: return 1.0\ndef bar() -> int: return ''";
  assert_declare_errors "class A: pass\ndef foo() -> A: return A()";
  assert_declare_errors "class A: pass\ndef foo() -> A: return 1";
  assert_declare_errors "def bar() -> str: return ''\ndef foo() -> str: return bar()"


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
    ["Incompatible return type [7]: expected `int` but got `typing.Set[str]`."];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.List[int]:
        return [x for x in a]
    |}
    ["Incompatible return type [7]: expected `typing.List[int]` but got `typing.List[str]`."];

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
      "Incompatible return type [7]: expected `typing.Dict[str, int]` but got " ^
      "`typing.Dict[int, str]`.";
    ]


let test_check_optional _ =
  assert_type_errors
    "def foo() -> str: return None"
    ["Incompatible return type [7]: expected `str` but got `typing.Optional[typing.Any]`."];

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
    ["Incompatible return type [7]: expected `typing.Optional[int]` but got `float`."];

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
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional or int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[bool, int]` " ^
      "but type `Any` is specified.";
      "Incompatible parameter type [6]: 1st parameter `i` to call `int_to_bool` expected `int` " ^
      "but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional and int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[typing.Union[bool, int]]` but " ^
      "type `Any` is specified."
    ]


let test_check_function_parameters _ =
  assert_type_errors
    {|
      def foo() -> None:
        math.intabs(1)
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        math.intabs(1.0)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        return math.intabs(1.0)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `float`.";
    ];

  assert_type_errors
    {|
      def foo(i) -> None:
        math.intabs(i)
    |}
    [];

  assert_type_errors
    {|
      class A:
        def foo(self) -> None:
          math.intabs(self.field)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `unknown`.";
    ];

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
      "Incompatible parameter type [6]: 1st parameter `i` to call `int_to_str` expected `int` " ^
      "but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(a: int) -> int:
        return a
      x: typing.Optional[int]
      foo(x if x else 1)
    |}
    []


let test_check_function_parameters_with_backups _ =
  assert_type_errors "(1).__add__(1)" [];
  assert_type_errors "(1).__add__(1j)" [];
  assert_type_errors "(1).__add__(1.0)" []


let test_check_variable_arguments _ =
  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> str:
        return foo ( *b )
    |}
    ["Incompatible return type [7]: expected `str` but got `int`."];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> int:
        return foo ( *b )
    |}
    [];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo ( *b )  # assuming b = []
    |}
    [];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo('asdf', *b)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `a` to call `foo` expected `int` but got " ^
      "`str`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 'asdf' )  # assuming b = []
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `a` to call `foo` expected `int` but got " ^
      "`str`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 1, 'asdf' )  # assuming b = []
    |}
    [];

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
      "Incompatible parameter type [6]: 2nd parameter `b` to call `durp` expected `str` but got " ^
      "`float`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo('asdf', *b)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `a` to call `foo` expected `int` but got " ^
      "`str`.";
    ]


let test_check_method_returns _ =
  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: expected `int` but got `str`."]


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
      "Incompatible parameter type [6]: 1st parameter `index` to call `str.substr` expected " ^
      "`int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `a` to call `foo` expected `str` but got " ^
      "`int`.";
      "Incompatible parameter type [6]: 2nd parameter `b` to call `foo` expected `str` but got " ^
      "`int`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `index` to call `str.substr` expected " ^
      "`int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `index` to call `str.substr` expected " ^
      "`int` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `other` to call `str.__add__` expected " ^
      "`str` but got `int`.";
    ];

  assert_type_errors
    {|
      def foo(input: typing.Any) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: expected `str` but got `int`."];

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
    ["Incompatible return type [7]: expected `int` but got `Foo`."]


let test_check_method_resolution _ =
  assert_type_errors
    {|
      def foo() -> None:
        bar().baz()
    |}
    [];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.lower()
    |}
    [];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.baz()
    |}
    ["Undefined method [10]: Could not resolve call `baz` on `str`."];
  assert_type_errors
    {|
      def undefined() -> Undefined: ...
      def foo() -> None:
        undefined().call()
    |}
    ["Undefined type [11]: Type `Undefined` is not defined."]


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
    ["Incompatible return type [7]: expected `str` but got `int`."]


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

        def baz() -> None:
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
    [
      "Incompatible parameter type [6]: 1st parameter `input` to call `Foo.foo` expected `int` " ^
      "but got `str`.";
    ];

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
    [
      "Incompatible parameter type [6]: 1st parameter `input` to call `Foo.foo` expected `int` " ^
      "but got `str`.";
    ];

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
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.classmethod` expected " ^
      "`int` but got `str`.";
    ];
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
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.staticmethod` expected " ^
      "`int` but got `str`.";
    ];
  assert_type_errors
    {|
      class Foo:
        def instancemethod(self, i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.instancemethod(Foo(), '1234')
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.instancemethod` expected " ^
      "`int` but got `str`.";
    ]


let test_check_init _ =
  assert_type_errors
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        pass
    |}
    [
      "Uninitialized field [13]: field test_field is declared in class Foo to have non-" ^
      "optional type `int` but is never initialized."
    ];

  assert_type_errors
    {|
    class Foo:
      test_field: int = 1
      def __init__(self) -> None:
        pass
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      test_field: int
      test_field_two: str
      def __init__(self) -> None:
        pass
    |}
    [
      "Uninitialized field [13]: field test_field is declared in class Foo to have non-" ^
      "optional type `int` but is never initialized.";
      "Uninitialized field [13]: field test_field_two is declared in class Foo to have non" ^
      "-optional type `str` but is never initialized."
    ];

  assert_type_errors
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        self.test_field = 0
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        test_field = 0
    |}
    [
      "Uninitialized field [13]: field test_field is declared in class Foo to have non-" ^
      "optional type `int` but is never initialized."
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.test_field = 0
    |}
    [
      "Missing annotation [4]: Field test_field of class Foo has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      class Foo:
        test_field: typing.Optional[int]
        def __init__(self) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
    class Foo:
      test_field: int
      def __init__(self) -> None:
        self.test_field = ""
    |}
    [
      "Incompatible type [8]: field test_field declared in class Foo has type `int` but is " ^
      "used as type `str`.";
    ];

  assert_type_errors
    {|
    class Foo:
      def __init__(self, x:int) -> None:
        pass
    a = Foo("")
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `x` to call `Foo.Foo` expected `int` but " ^
      "got `str`.";
    ]


let test_check_fields _ =
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return self.bar
    |}
    ["Incompatible return type [7]: expected `int` but got `unknown`."];
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
    ~debug:true
    ~strict:true
    {|
      class Bar:
        def bar() -> None:
          pass
      class Foo:
        bar: Optional[Bar] = None
        def foo() -> None:
          self.bar.bar()
    |}
    [];

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
      "Missing annotation [4]: Field bar of class Foo has type `str` but no type " ^
      "is specified.";
      "Incompatible return type [7]: expected `int` but got `str`."
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
      "Missing annotation [4]: Field bar of class Foo has type `str` but type `Any` " ^
      "is specified.";
      "Incompatible return type [7]: expected `int` but got `str`."
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
      "Incompatible type [8]: field bar declared in class Foo has type `int` but is used as " ^
      "type `str`.";
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
      "Incompatible type [8]: field bar declared in class Foo has type `int` but is used " ^
      "as type `str`.";
    ];

  assert_type_errors
    {|
      bar: int
      def foo() -> int:
        bar = 'foo'
        return bar
    |}
    ["Incompatible return type [7]: expected `int` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing annotation [4]: Field bar of class Foo has type `str` but no type " ^
      "is specified.";
      "Incompatible return type [7]: expected `int` but got `str`.";
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
      "Incompatible type [8]: field bar declared in class Foo has type `int` but is " ^
      "used as type `str`.";
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
      "Incompatible type [8]: field bar declared in class Foo has type `int` but is used as " ^
      "type `str`.";
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
    ["Incompatible return type [7]: expected `int` but got `typing.Optional[int]`."];

  assert_type_errors
    {|
      class Foo:
          def foo(self, bar: typing.Optional[int]) -> int:
              self.baz = bar
              if self.baz is None:
                  self.baz = 5
              return self.baz
    |}
    ["Missing annotation [4]: Field baz of class Foo has type " ^
     "`typing.Optional[int]` but no type is specified."];

  (* TODO(szhu): support field tests for: class variables, generic annotations *)
  assert_type_errors
    {|
      class Foo:
        bar: typing.ClassVar[int]
      def foo() -> int:
        Foo.bar = "foo"
        return Foo.bar
    |}
    ["Incompatible return type [7]: expected `int` but got `str`."];
  (* [
      "Incompatible type [8]: field Foo.bar declared in class Foo " ^
      "has type `int` but is used as type `str`."
     ]; *)

  assert_type_errors
    {|
      class Foo:
        bar: typing.Generic[_T]
        def foo(self) -> int:
          self.bar = 0
          return self.bar
    |}
    [
      "Incompatible type [8]: field bar declared in class Foo has type `typing.Generic[_T]` " ^
      "but is used as type `int`.";
      "Incompatible return type [7]: expected `int` but got `typing.Generic[_T]`.";
    ];
  (* []; *)

  (* Static fields are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        field: typing.ClassVar[int] = 1

      def foo() -> str:
        return Foo.field
    |}
    ["Incompatible return type [7]: expected `str` but got `int`."]


let test_check_globals _ =
  assert_type_errors
    {|
      constant: int = 1
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: expected `str` but got `int`."]


let test_check_immutables _ =
  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    ["Incompatible type [9]: constant is declared to have type `int` but is used as type `str`."];

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
    constant
    def foo() -> None:
      global constant
      constant = 1
    |}
    [
      "Missing annotation [5]: Globally accessible field constant has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
    constant: typing.Any
    def foo() -> None:
      global constant
      constant = 1
    |}
    [
      "Missing annotation [5]: Globally accessible field constant has type `int` but " ^
      "type `Any` is specified."
    ];

  assert_type_errors
    {|
    constant
    def foo() -> int:
      global constant
      constant = 1
      return constant
    |}
    [
      "Missing annotation [5]: Globally accessible field constant has type `int` but " ^
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
    [];

  assert_type_errors
    {|
    def foo(x: int) -> None:
      x = "hi"
    |}
    ["Incompatible type [9]: x is declared to have type `int` but is used as type `str`."];

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
    [];

  assert_type_errors
    {|
    def foo() -> None:
      x = 1
      y: str
      y = x
      x = y
    |}
    ["Incompatible type [9]: y is declared to have type `str` but is used as type `int`."];

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
        field
      def bar() -> None:
        foo = Foo()
        foo.field = 1
    |}
    [
      "Missing annotation [4]: Field field of class Foo has type `int` but no type" ^
      " is specified.";
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing annotation [5]: Globally accessible field constant has type `int` but " ^
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
      constant
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
      "Missing annotation [5]: Globally accessible field constant has type `int` but " ^
      "no type is specified."
    ];

  (* TODO: error on typing.Any (incompatible usage) rather than suggest it *)
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
      "Missing annotation [5]: Globally accessible field constant has type `typing." ^
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
      "Missing annotation [5]: Globally accessible field constant has type `typing." ^
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
      "Missing annotation [5]: Globally accessible field constant has type `float` but" ^
      " no type is specified."
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
      "Missing annotation [5]: Globally accessible field constant has type `A` but no " ^
      "type is specified."
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
      "Missing annotation [4]: Field constant of class Foo has type `int` but no " ^
      "type is specified.";
      "Missing annotation [5]: Globally accessible field constant has type `str` but " ^
      "no type is specified."
    ];

  (* Illustrate refinement. *)
  assert_type_errors
    {|
    def takes_int(a: int) -> None: pass
    def foo() -> None:
      x: float
      x = 1
      takes_int(x)
      x = 1.0
    |}
    []


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
    ["Incompatible return type [7]: expected `int` but got `unknown`."];
  assert_type_errors
    {|
      def bar()->int:
        return 1 + str_float_to_int(i=2.0,f=1)
      def foo()->int:
        return str_float_to_int(f="No",i="Hi")
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `str_float_to_int` expected " ^
      "`str` but got `float`.";
      "Incompatible parameter type [6]: 1st parameter `f` to call `str_float_to_int` expected " ^
      "`float` but got `str`.";
    ]


let test_check_enumerations _ =
  assert_type_errors
    {|
      import enum
      class Color(enum.Enum):
        RED = 0
      def foo() -> int:
        return Color.RED
    |}
    ["Incompatible return type [7]: expected `int` but got `Color`."];

  assert_type_errors
    {|
      import enum
      class Color(enum.Enum):
        RED = 0
      def foo(whatever) -> Color:
        return Color(whatever)
      def foo(number: int) -> Color:
        return Color(number)
    |}
    [];

  assert_type_errors
    {|
      class util.enum.IntEnum(enum.Enum, int):
        pass
      class Color(util.enum.IntEnum):
        RED = 0
      def foo() -> int:
        return Color.RED
    |}
    []


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
    ["Incompatible return type [7]: expected `None` but got `int`."];

  assert_type_errors
    {|
      def foo():
        return
    |}
    [
      "Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`."
    ];

  assert_type_errors
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[typing.Any]` but no return type " ^
      "is specified.";
    ];

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
      "Incompatible return type [7]: expected `None` but got `int`."
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
    []


let test_check_yield _ =
  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1.0
    |}
    [
      "Incompatible return type [7]: expected `typing.Generator[int, None, None]` " ^
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
      "Incompatible return type [7]: expected `typing.Generator[int, None, None]` " ^
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
    ["Incompatible return type [7]: expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
          math.intabs(x) if x else 0
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
          return math.intabs(x if x is not None else 1)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        a, b = ("hi", math.intabs(x) if x is not None else 1)
        return b
    |}
    []


let test_check_union _ =
  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: expected `typing.Union[int, str]` but got `float`."];

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
      "Incompatible return type [7]: expected `int` but got `typing.Union[float, str]`."
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    [
      "Incompatible return type [7]: expected `float` but got `typing.Union[int, str]`."
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


let test_check_missing_parameter _ =
  assert_type_errors
    ~infer:true
    {|
      def foo(x = 5) -> int:
        return x
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];

  assert_type_errors
    ~infer:true
    {|
      def foo(x: typing.Any) -> None:
        x = 5
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified."];

  assert_type_errors
    ~infer:true
    {|
      def foo(x: typing.Any = 5) -> None:
        pass
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but type `Any` is specified."];

  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def ohgod(derp: typing.Optional[typing.Any] = None) -> None:
        pass
    |}
    []


let test_check_nested _ =
  assert_type_errors
    {|
      def foo() -> None:
        def nested() -> None:
          math.intabs(1.0)
        math.intabs(1.0)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `float`.";
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `float`.";
    ]


let test_check_value_restrictions _ =
  assert_type_errors
    {|
       def f(x:str)->int:
           return value_restricted_identity(x)
    |}
    ["Incompatible return type [7]: expected `int` but got `str`.";];

  assert_type_errors
    {|
       def f(x:str)->str:
           return value_restricted_identity(x)
    |}
    [];

  assert_type_errors
    {|
       def f(x:float)->str:
           return value_restricted_identity(x)
    |}
    ["Incompatible return type [7]: expected `str` but got `unknown`."]


let test_check_conditional_refinement _ =
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
    ["Incompatible return type [7]: expected `int` but got `typing.Optional[int]`."];
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
          if x and math.intabs(x) < 0:
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
    []


let test_check_toplevel _ =
  assert_type_errors
    "math.intabs(1.0)"
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `math.intabs` expected `int` " ^
      "but got `float`.";
    ]


let test_check_tuple _ =
  assert_type_errors
    {|
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `a` to call `tuple.tuple_method` expected " ^
      "`int` but got `float`.";
    ];
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
      "Incompatible return type [7]: expected `typing.Tuple[int, string]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: expected `typing.Tuple[int, ...]` but got " ^
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
          return 1, math.intabs(z) if z is not None else None
    |}
    []


let test_check_meta _ =
  assert_type_errors "typing.cast(asdf, 'asdf')" [];
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[float], a)
    |}
    ["Incompatible return type [7]: expected `typing.List[int]` but got `typing.List[float]`."]


let test_check_assert _ =
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional or len(optional) > 0:
          pass
        if optional is None or len(optional) > 0:
          pass
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
    []


let test_check_await _ =
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
      def bar(a: int) -> None:
        await a
    |}
    ["Incompatible awaitable type [12]: expected an awaitable but got `int`."];
  assert_type_errors ~gradual:true ~debug:false
    {|
      def bar(a: typing.Any) -> None:
        await a
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
    ["Inconsistent override [15]: `foo` overrides method defined in `Foo` inconsistently."];
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
    ["Inconsistent override [15]: `foo` overrides method defined in `Foo` inconsistently."];
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo(): pass
    |}
    [
      "Inconsistent override [15]: `foo` overrides method defined in `Foo` inconsistently.";
      "Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`."
    ];

  (* Weakened precondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo(a: float) -> None: ...
      class Bar(Foo):
        def foo(a: int) -> None: pass
    |}
    ["Inconsistent override [14]: `foo` overrides method defined in `Foo` inconsistently."];
  assert_type_errors
    {|
      class Foo():
        def foo(a) -> None: ...
      class Bar(Foo):
        def foo() -> None: pass
    |}
    ["Inconsistent override [14]: `foo` overrides method defined in `Foo` inconsistently."];
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

  (* Weakening of object precondition is not possible. *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, o: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other) -> bool: pass
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
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.Foo` expected `int` but " ^
      "got `str`.";
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
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.Foo` expected `int` but " ^
      "got `str`.";
      "Incompatible parameter type [6]: 2nd parameter `s` to call `Foo.Foo` expected " ^
      "`typing.Optional[str]` but got `int`.";
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
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `Foo.__init__` expected `int` " ^
      "but got `str`.";
    ];

  (* Super calls. *)
  assert_type_errors
    {|
      class Super:
        def __init__(self, i: int) -> None:
          pass
      class Foo(Super):
        def __init__(self, i: int) -> None:
          super().__init__(self, 'asdf')
    |}
    [
      "Incompatible parameter type [6]: 1st parameter `i` to call `Super.__init__` expected " ^
      "`int` but got `str`.";
    ]


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


let assert_infer
    ?(debug = false)
    ?(infer = true)
    ?(show_error_traces = false)
    ?(recursive_infer = false)
    ?(fields = ["description"])
    source
    errors =
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
    List.map ~f:field_of_error fields
  in
  let source =
    parse source
    |> Preprocessing.preprocess in
  let environment =
    Environment.Builder.copy plain_environment in
  Environment.populate (Environment.reader environment) [source];
  let environment_reader = Environment.reader environment in
  let to_string json =
    Yojson.Safe.sort json
    |> Yojson.Safe.to_string
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(fun errors -> Format.asprintf "%a" Sexp.pp (sexp_of_list sexp_of_string errors))
    ~pp_diff:
      (diff
         ~print:(fun format errors ->
             Format.fprintf format "%a" Sexp.pp (sexp_of_list sexp_of_string errors)))
    (List.map ~f:(fun string -> Yojson.Safe.from_string string |> to_string) errors)
    (List.map
       ~f:fields_of_error
       (check_errors
          (Configuration.create ~debug ~infer ~recursive_infer ()) environment_reader source)
     |> List.concat
     |> List.map ~f:to_string)


let test_infer _ =
  assert_infer
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    ["\"Missing return annotation [3]: Returning `Test` but no return type is specified.\""];

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
    [];

  assert_infer ~fields:["inference.parameters"]
    {|
      def with_params (x: int,y):
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
          if True:
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
    [
      "\"Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`.\""
    ];
  assert_infer
    {|
      def return_none ():
          return
    |}
    [
      "\"Missing return annotation [3]: Function does not return; " ^
      "return type should be specified as `None`.\""
    ];
  assert_infer
    {|
      def return_none ():
          return None
    |}
    [];
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
    [{|[{"name":"x","type":"int","value":"5"},{"name":"y","type":null,"value":null}]|}];

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

  assert_infer ~fields:["inference.parameters"]
    {|
      def foo(y) -> typing.Tuple[int, float]:
          x = y
          z = y
          return (x, z)
    |}
    [{|[{"name":"y","type":"int","value":null}]|}];

  assert_infer ~fields:["inference.parameters"]
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    [{|[{"name":"x","type":"int","value":null}]|}];

  (* The next two illustrate where we mess up with current simple dequalify implementation *)
  assert_infer ~fields:["inference.parameters"]
    {|
      def test_optional_bad(x: Optional[str]):
          return 5
      from typing import Optional
    |}
    [{|[{"name":"x","type":"Optional[str]","value":null}]|}]; (* Should be typing.Optional[str] *)

  assert_infer ~fields:["inference.parameters"]
    {|
      import A
      from A import C
      from B import C
      def test_bad_import(x: A.C):
          return 5
    |}
    [{|[{"name":"x","type":"C","value":null}]|}] (* Should be A.C *)


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
    ]


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
    [{|"int"|};{|"int"|}];

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
      {|"int"|};{|[{"name":"a","type":null,"value":null}]|};
      {|null|};{|[{"name":"a","type":"int","value":null}]|};
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
      {|null|};{|[{"name":"a","type":"int","value":null}]|};
    ]


let () =
  Log.initialize_for_tests ();
  "type">:::[
    "initial">::test_initial;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "widen">::test_widen;
    "forward">::test_forward;
    "forward_immutables">::test_forward_immutables;
    "forward_infer_free_type_variables">::test_forward_infer_free_type_variables;
    "forward_assert_optionals">::test_forward_assert_optionals;
    "forward_assert_isinstance">::test_forward_assert_isinstance;
    "backward">::test_backward;
    "fixpoint_forward">::test_fixpoint_forward;
    "fixpoint_backward">::test_fixpoint_backward;
    "ignore_lines">::test_ignore_lines;
    "check_error_traces">::test_show_error_traces;
    "coverage">::test_coverage;
    "check">::test_check;
    "check_strict">::test_strict;
    "check_declare">::test_declare;
    "check_gradual">::test_check_gradual;
    "check_comprehensions">::test_check_comprehensions;
    "check_optional">::test_check_optional;
    "check_function_parameters">::test_check_function_parameters;
    "check_function_parameters_with_backups">::test_check_function_parameters_with_backups;
    "check_variable_arguments">::test_check_variable_arguments;
    "check_method_returns">::test_check_method_returns;
    "check_method_parameters">::test_check_method_parameters;
    "check_method_resolution">::test_check_method_resolution;
    "check_self">::test_check_self;
    "check_static">::test_check_static;
    "check_init">::test_check_init;
    "check_fields">::test_check_fields;
    "check_globals">::test_check_globals;
    "check_immutables">::test_check_immutables;
    "check_named_arguments">::test_check_named_arguments;
    "check_enumerations">::test_check_enumerations;
    "check_missing_return">::test_check_missing_return;
    "check_yield">::test_check_yield;
    "check_ternary">::test_check_ternary;
    "check_union">::test_check_union;
    "check_return_joining">::test_check_return_joining;
    "check_missing_parameter">::test_check_missing_parameter;
    "check_nested">::test_check_nested;
    "check_value_restrictions">::test_check_value_restrictions;
    "check_conditional_refinement">::test_check_conditional_refinement;
    "check_toplevel">::test_check_toplevel;
    "check_tuple">::test_check_tuple;
    "check_meta">::test_check_meta;
    "check_assert">::test_check_assert;
    "check_excepts">::test_check_excepts;
    "check_await">::test_check_await;
    "check_behavioral_subtyping">::test_check_behavioral_subtyping;
    "check_collections">::test_check_collections;
    "check_constructors">::test_check_constructors;
    "check_explicit_method_call">::test_check_explicit_method_call;
    "check_meta_annotations">::test_check_meta_annotations;
    "infer">::test_infer;
    "infer_backward">::test_infer_backward;
    "recursive_infer">::test_recursive_infer;
  ]
  |> run_test_tt_main
