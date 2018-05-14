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


let configuration = Configuration.create ()


let plain_environment =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    (Environment.handler ~configuration environment)
    ~configuration
    [
      Source.create ~qualifier:(Access.create "typing") [];
      Source.create ~qualifier:(Access.create "unittest.mock") [];
      parse ~qualifier:[] {|
        class typing.Sized: ...
        def len(o: typing.Sized) -> int: ...

        typing.List = typing.TypeAlias(object)

        def not_annotated(input = ...): ...
        class slice:
          @overload
          def __init__(self, stop: typing.Optional[int]) -> None: ...
          @overload
          def __init__(
            self,
            start: typing.Optional[int],
            stop: typing.Optional[int],
            step: typing.Optional[int] = ...
          ) -> None: ...
          def indices(self, len: int) -> Tuple[int, int, int]: ...

        class bool(): ...
        class bytes(): ...
        class float():
          def __add__(self, other) -> float: ...
          def __radd__(self, other: float) -> float: ...
          def __neg__(self) -> float: ...
          def __abs__(self) -> float: ...
        class int(float):
          def __init__(self, value) -> None: ...
          def __lt__(self, other) -> int: ...
          def __ne__(self, other) -> bool: ...
          def __add__(self, other: int) -> int: ...
          def __mod__(self, other) -> int: ...
          def __radd__(self, other: int) -> int: ...
          def __neg__(self) -> int: ...
          def __str__(self) -> bool: ...
        class complex():
          def __radd__(self, other: int) -> int: ...
        class str(typing.Sized):
          @overload
          def __init__(self, o: object = ...) -> None: ...
          @overload
          def __init__(self, o: bytes, encoding: str = ..., errors: str = ...) -> None: ...
          def lower(self) -> str: pass
          def upper(self) -> str: ...
          def substr(self, index: int) -> str: pass
          def __lt__(self, other) -> float: ...
          def __ne__(self, other) -> int: ...
          def __add__(self, other: str) -> str: ...
          def __pos__(self) -> float: ...
          def __repr__(self) -> float: ...
          def __str__(self) -> str: ...
          def __getitem__(self, i: typing.Union[int, slice]) -> str: ...
        class object():
          def __sizeof__(self) -> int: pass

        def to_int(x: typing.Any) -> int: ...
        def int_to_str(i: int) -> str: ...
        def str_to_int(i: str) -> int: ...
        def optional_str_to_int(i: typing.Optional[str]) -> int: ...
        def int_to_bool(i: int) -> bool: ...
        def int_to_int(i: int) -> int: pass
        def str_float_to_int(i: str, f: float) -> int: ...
        def str_float_tuple_to_int(t: typing.Tuple[str, float]) -> int: ...
        def nested_tuple_to_int(t: typing.Tuple[typing.Tuple[str, float], float]) -> int: ...
        def return_tuple() -> typing.Tuple[int, int]: ...
        def unknown_to_int(i) -> int: ...
        def star_int_to_int( *args, x: int) -> int: ...

        def typing.TypeVar(name, bound=None, *args): ...
        _T = typing.TypeVar('_T')
        _T_co = typing.TypeVar('_T_co')
        _S = typing.TypeVar('_S')
        _V = typing.TypeVar('_V')

        class typing.Generic(): pass
        class typing.Sequence(typing.Generic[_T]): pass

        class type:
          __name__: str = ...

        def isinstance(a, b) -> bool: ...

        class typing.Iterable(typing.Generic[_T], typing.Sequence[_T]):
          def __iter__(self)->typing.Iterator[_T]: pass
        class typing.Iterator(typing.Iterable[_T], typing.Generic[_T]):
          def __next__(self) -> _T: ...

        class tuple(typing.Sized):
          def __init__(self, a:typing.List[int]): ...
          def tuple_method(self, a: int): ...
        class dict(typing.Generic[_T, _S], typing.Iterable[_T]):
          def add_key(self, key: _T) -> None: pass
          def add_value(self, value: _S) -> None: pass
          def add_both(self, key: _T, value: _S) -> None: pass
          def items(self) -> typing.Iterable[typing.Tuple[_T, _S]]: pass
          def __getitem__(self, k: _T) -> _S: ...
          @overload
          def get(self, k: _T) -> typing.Optional[_S]: ...
          @overload
          def get(self, k: _T, default: _S) -> _S: ...
          @overload
          def update(self, __m: typing.Dict[_T, int], **kwargs: _S): ...
          @overload
          def update(self, **kwargs: _S): ...
        class list(typing.Iterable[_T], typing.Generic[_T]):
          def __add__(self, x: list[_T]) -> list[_T]: ...
          def __iter__(self) -> typing.Iterator[_T]: ...
          def append(self, element: _T) -> None: ...
          @overload
          def __getitem__(self, i: int) -> _T: ...
          @overload
          def __getitem__(self, s: slice) -> typing.List[_T]: ...
        class set(typing.Iterable[_T], typing.Generic[_T]): pass
        class typing.Generator(typing.Generic[_T, _S, _V], typing.Iterator[_T]):
          pass
        class typing.Mapping(_Collection[_KT], Generic[_KT, _VT_co]):
          pass

        class A: ...
        class B(A): ...
        class C(A): ...
        class D(B,C): ...
        class obj():
          @staticmethod
          def static_int_to_str(i: int) -> str: ...

        def identity(x: _T) -> _T: ...
        _VR = typing.TypeVar("_VR", str, int)
        def variable_restricted_identity(x: _VR) -> _VR: pass

        def typing.cast(tp: typing.Type[_T], o) -> _T: ...

        class typing.Awaitable: pass
        class typing.AsyncGenerator: pass
        class IsAwaitable(typing.Awaitable[int]): pass
        class contextlib.ContextManager(typing.Generic[_T_co]):
          def __enter__(self) -> _T_co:
            pass
        class contextlib.GeneratorContextManager(contextlib.ContextManager[_T], typing.Generic[_T]):
          pass

        def sum(iterable: typing.Iterable[_T]) -> typing.Union[_T, int]: ...
        def returns_undefined()->Undefined: ...
        class Spooky:
          def undefined(self)->Undefined: ...

        class Attributes:
          int_attribute: int

        class OtherAttributes:
          int_attribute: int
          str_attribute: str
      |}
      |> Preprocessing.qualify;
    ];
  environment


let environment =
  let handler = Environment.handler ~configuration plain_environment in
  add_defaults_to_environment ~configuration handler;
  handler


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
    ?(initial = (fun environment define -> State.initial environment define))
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


let assert_forward precondition statement postcondition =
  let parsed =
    (parse statement
     |> Preprocessing.expand_operators
     |> Preprocessing.expand_subscripts)
    |> function
    | { Source.statements = statement::rest; _ } -> statement::rest
    | _ -> failwith "unable to parse test"
  in
  assert_state_equal
    (create postcondition)
    (List.fold
       ~f:(fun state statement -> State.forward state ~statement)
       ~init:(create precondition)
       parsed)


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
  assert_forward ["z", Type.integer] "x, y = z" ["x", Type.Top; "y", Type.Top; "z", Type.integer];

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
    ["x", Type.dictionary ~key:Type.string ~value:Type.integer]
    "y = x['derp']"
    ["x", Type.dictionary ~key:Type.string ~value:Type.integer; "y", Type.integer];

  (* Generator. *)
  assert_forward [] "x = (element for target in iterator)" ["x", Type.generator Type.Object];

  (* Lambda. *)
  assert_forward
    []
    "x = lambda: 1.0"
    ["x", Type.lambda ~parameters:[] ~return_annotation:Type.float];

  assert_forward
    []
    "x = lambda y: 1"
    ["x", Type.lambda ~parameters:[Type.Object] ~return_annotation:Type.integer];

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

  (* Lists of assignments. *)
  assert_forward
    ["x", Type.list Type.integer]
    "[a, b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.integer];

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
    ["x", Type.Bottom; "y", Type.Bottom] (* Limitation: We're losing y's constraints here. *)



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
      (List.fold
         ~f:(fun state statement -> State.forward state ~statement)
         ~init:(create precondition ~immutables:pre_immutables)
         parsed)
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
    ["x", Type.union [Type.integer; Type.string]];

  assert_forward
    ["x", Type.integer]
    "assert isinstance(x, (int, str))"
    ["x", Type.integer]


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
         ~cfg:(Cfg.create define)
         ~initial:(State.initial environment define_node))
  in
  assert_fixpoint_forward
    {| def foo(): pass |}
    (Int.Table.of_alist_exn [
        0, create []; (* Entry *)
        1, create []; (* Exit *)
        3, create []; (* Final *)
        5, create []; (* Pass *)
      ]);
  assert_fixpoint_forward
    {|
     def foo():
       x = y
    |}
    (Int.Table.of_alist_exn [
        0, create [];
        1, create ["$renamed_x", Type.Top];
        3, create ["$renamed_x", Type.Top];
        5, create [];
      ]);
  assert_fixpoint_forward
    {|
     def foo(y: int):
       x = y
    |}
    (Int.Table.of_alist_exn [
        0, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer];
        1, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.integer;
          "$renamed_y", Type.integer;
        ];
        3, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.integer;
          "$renamed_y", Type.integer;
        ];
        5, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer];
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
        0, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer]; (* Entry *)
        1, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.Top;
          "$renamed_y", Type.integer;
        ]; (* Exit *)
        3, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.Top;
          "$renamed_y", Type.integer;
        ]; (* Final *)
        5, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer]; (* If *)
        6, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.Top;
          "$renamed_y", Type.integer;
        ]; (* Join *)
        7, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer]; (* Body *)
        8, create ~immutables:["$renamed_y", false] ["$renamed_y", Type.integer]; (* Orelse *)
        9, create ~immutables:["$renamed_y", false] [
          "$renamed_x", Type.Top;
          "$renamed_y", Type.integer;
        ]; (* Return *)
      ]);
  ()


let check_with_default_environment
    ?(autogenerated = false)
    ?(debug = true)
    ?(strict = false)
    ?(declare = false)
    ?(infer = false)
    ?(qualifier = [])
    ?mode_override
    source =
  let check_errors configuration environment ?mode_override source =
    let { Result.errors; _ } =
      if infer then
        Inference.infer configuration environment mock_call_graph ?mode_override source
      else
        check configuration environment mock_call_graph ?mode_override source
    in
    errors
  in
  let source =
    let metadata =
      Source.Metadata.create
        ~autogenerated
        ~debug
        ~declare
        ~ignore_lines:[]
        ~strict
        ~version:3
        ~number_of_lines:(-1)
        ()
    in
    parse ~qualifier source
    |> (fun source -> { source with Source.metadata })
    |> Preprocessing.preprocess
  in
  let environment =
    let environment = Environment.Builder.copy plain_environment in
    Environment.populate ~configuration (Environment.handler ~configuration environment) [source];
    Environment.handler ~configuration environment
  in
  let configuration = Configuration.create ~debug ~strict ~declare ~infer () in
  check_errors configuration environment ?mode_override source


let assert_type_errors
    ?(autogenerated = false)
    ?(debug = true)
    ?(strict = false)
    ?(declare = false)
    ?(infer = false)
    ?(show_error_traces = false)
    ?(qualifier = [])
    source
    errors =
  Annotated.Class.AttributesCache.clear ();
  let descriptions =
    let mode_override =
      if infer then
        Some Source.Infer
      else if strict then
        Some Source.Strict
      else if declare then
        Some Source.Declare
      else if debug then
        None
      else
        Some Source.Default
    in
    List.map
      ~f:(fun error -> Error.description error ~detailed:show_error_traces)
      (check_with_default_environment
         ~autogenerated
         ~strict
         ~debug
         ~declare
         ~infer
         ~qualifier
         ?mode_override
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
    ];

  assert_type_errors ~show_error_traces:true
    {|
    constant = x
    def foo() -> None:
      global constant
      constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified. Global variable `constant` declared on line 2, type `int` deduced " ^
      "from test.py:5:2.";
      "Undefined name [18]: Global name `x` is undefined.";
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
      "Missing global annotation [5]: Globally accessible variable `constant` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `constant` " ^
      "declared on line 2, type `typing.Union[int, str]` deduced from test.py:5:2, " ^
      "test.py:6:2.";
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
      "type `int` deduced from test.py:5:4."
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
    [];

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
              # TODO(T27001301): This errors because we don't propagate variables from the parent in
              # nested functions.
              return len(global_number)
          def assign_inner_global() -> int:
              global global_number
              return global_number
    |}
    [];

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
    [];
  ()


let test_coverage _ =
  let assert_coverage source expected =
    let { Result.coverage; _ } =
      AnalysisTypeCheck.check
        configuration
        (Environment.handler ~configuration plain_environment)
        mock_call_graph
        (parse source)
    in
    assert_equal ~printer:Coverage.show coverage expected
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
      "Incompatible return type [7]: Expected `int` but got `None`.";
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
      "Incompatible return type [7]: Expected `str` but got `int`."

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
    |}
    []


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
    ["Incompatible variable type [9]: Unable to unpack `int`, expected a `Tuple`."]


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
  assert_not_covered "class B: ERROR";
  assert_not_covered
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
  assert_covered
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

  (* TODO(T26146217): we're not handling format strings yet. *)
  assert_not_covered {|f"format{ERROR}"|};

  (* Generator. *)
  assert_covered "(ERROR for i in list)";

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
    [];

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
      def f() -> int:
        x = lambda y: y
        a = x(1)
        return 0
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
      "Missing return annotation [3]: Returning `typing.Optional[typing.Union[bool, int]]` but " ^
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
      def foo(x: typing.Union[Attributes, OtherAttributes])->int:
        return x.int_attribute
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes])->int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Union[OtherAttributes, Attributes])->int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ]


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
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.undefined)
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `undefined`."]


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
      "has type `int` but no type is specified."
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
      "has type `int` but no type is specified."
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
      "`typing.Generic[_T]` but is used as type `int`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Generic[_T]`.";
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

  (* Check attribute type resolution. *)
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
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `typing.Optional[int]` but is used as type `str`.";
    ]


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
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
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
    ["Incompatible return type [7]: Expected `str` but got `typing.Union[str, typing.Unbound]`."];

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
      "Undefined attribute [16]: `Foo` has no attribute `constant`.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `str` but " ^
      "no type is specified.";
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


let test_check_enumerations _ =
  assert_type_errors
    {|
      class enum.Enum: ...
      class Color(enum.Enum):
        RED = 0
      def foo() -> int:
        return Color.RED
    |}
    ["Incompatible return type [7]: Expected `int` but got `Color`."];

  assert_type_errors
    {|
      class enum.Enum: ...
      class Color(enum.Enum):
        def __init__(self, value): ...
        RED = 0
      def foo(whatever) -> Color:
        return Color(whatever)
      def foo(number: int) -> Color:
        return Color(number)
    |}
    [];

  assert_type_errors
    {|
      class util.enum.IntEnum(enum.Enum, int): ...
      class Color(util.enum.IntEnum):
        RED = 0
      def foo() -> int:
        return Color.RED
    |}
    [];

  assert_type_errors
    {|
      class enum.Enum: ...
      class Color(enum.Enum):
        RED: int = 0
      def foo() -> Color:
        return Color.RED
    |}
    [];

  assert_type_errors
    {|
      class enum.Enum: ...
      class Color(enum.Enum):
        RED, BLUE = list(range(2))
      def foo() -> Color:
        return Color.RED
    |}
    [];

  assert_type_errors
    {|
      class enum.Enum: ...
      class Color(enum.Enum):
        @property
        def property(self) -> str: ...
      def foo(color: Color) -> str:
        return color.property
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
    [
      "Incompatible return type [7]: Expected `str` but got `unknown`.";
    ]


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
    ["Incompatible parameter type [6]: Expected `int` but got `float`."]


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
    ]


let test_check_meta _ =
  assert_type_errors
    {|
      class float():
        ...
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[float], a)
    |}
    [
      "Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[float]`.";
      "Undefined name [18]: Global name `a` is undefined.";
    ];
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[unknown], a)
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
    ["Inconsistent override [15]: `foo` overloads method defined in `Foo` inconsistently."];
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
    ["Inconsistent override [15]: `foo` overloads method defined in `Foo` inconsistently."];
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo(): pass
    |}
    [
      "Inconsistent override [15]: `foo` overloads method defined in `Foo` inconsistently.";
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
    (* TODO(T27409168): Error here. *)
    [];
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
      "Inconsistent override [15]: `bar` overloads method defined in `Foo` " ^
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
    ["Inconsistent override [14]: `foo` overloads method defined in `Foo` inconsistently."];
  assert_type_errors
    {|
      class Foo():
        def foo(a) -> None: ...
      class Bar(Foo):
        def foo() -> None: pass
    |}
    ["Inconsistent override [14]: `foo` overloads method defined in `Foo` inconsistently."];
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
      "Inconsistent override [14]: `bar` overloads method defined in `Foo` " ^
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

  (* Weakening of object precondition is not possible. *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, o: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other) -> bool: ...
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
    ["Incompatible return type [7]: Expected `int` but got `typing.Union[int, typing.Unbound]`."];
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
      "`typing.Callable(Foo.bar)[[Named($renamed_self, unknown), Named($renamed_x, int)], str]`.";
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


let () =
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
    "fixpoint_forward">::test_fixpoint_forward;
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
    "check_named_arguments">::test_check_named_arguments;
    "check_enumerations">::test_check_enumerations;
    "check_missing_return">::test_check_missing_return;
    "check_yield">::test_check_yield;
    "check_ternary">::test_check_ternary;
    "check_union">::test_check_union;
    "check_return_joining">::test_check_return_joining;
    "check_nested">::test_check_nested;
    "check_variable_restrictions">::test_check_variable_restrictions;
    "check_refinement">::test_check_refinement;
    "check_toplevel">::test_check_toplevel;
    "check_tuple">::test_check_tuple;
    "check_meta">::test_check_meta;
    "check_assert">::test_check_assert;
    "check_excepts">::test_check_excepts;
    "check_async">::test_check_async;
    "check_behavioral_subtyping">::test_check_behavioral_subtyping;
    "check_collections">::test_check_collections;
    "check_constructors">::test_check_constructors;
    "check_explicit_method_call">::test_check_explicit_method_call;
    "check_meta_annotations">::test_check_meta_annotations;
    "check_unbound_variables">::test_check_unbound_variables;
    "check_contextmanager">::test_check_contextmanager;
    "check_callables">::test_check_callables;
  ]
  |> run_test_tt_main
