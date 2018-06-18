(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement
open TypeCheck

open Test


let configuration = Configuration.create ()


let typeshed_stubs = (* Yo dawg... *)
  [
    Source.create ~qualifier:(Access.create "sys") [];
    parse
      ~qualifier:(Access.create "hashlib")
      ~path:"hashlib.pyi"
      {|
        _DataType = typing.Union[int, str]
        class _Hash:
          digest_size: int
        def md5(input: _DataType) -> _Hash: ...
      |}
    |> Preprocessing.qualify;
    parse
      ~qualifier:(Access.create "typing")
      {|
        class _SpecialForm: ...

        TypeVar = object()
        List = TypeAlias(object)
        Type: _SpecialForm = ...

        class Sized: ...

        _T = TypeVar('_T')
        _T_co = TypeVar('_T_co')
        _S = TypeVar('_S')
        _V = TypeVar('_V')
        _KT = TypeVar('_KT')
        _VT_co = TypeVar('_VT_co')

        class Generic(): pass

        class Iterable(Generic[_T]):
          def __iter__(self) -> Iterator[_T]: pass
        class Iterator(Iterable[_T], Generic[_T]):
          def __next__(self) -> _T: ...
        if sys.version_info >= (3, 6):
          class Collection(Iterable[_T_co]): ...
          _Collection = Collection
        else:
          class _Collection(Iterable[_T_co]): ...
        class Sequence(_Collection[_T], Iterable[_T]): pass

        class Generator(Generic[_T, _S, _V], Iterator[_T]):
          pass
        class Mapping(_Collection[_KT], Generic[_KT, _VT_co]):
          pass

        class Awaitable: pass
        class AsyncGenerator: pass

        def cast(tp: Type[_T], o) -> _T: ...
      |}
    |> Preprocessing.qualify;
    Source.create ~qualifier:(Access.create "unittest.mock") [];
    parse
      ~qualifier:[]
      {|
        import typing

        _T = typing.TypeVar('_T')
        _T_co = typing.TypeVar('_T_co')
        _S = typing.TypeVar('_S')


        def not_annotated(input = ...): ...

        class type:
          __name__: str = ...
        class object():
          def __sizeof__(self) -> int: pass

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

        class range(typing.Sequence[int]):
          @overload
          def __init__(self, stop: int) -> None: ...

        class super:
           @overload
           def __init__(self, t: typing.Any, obj: typing.Any) -> None: ...
           @overload
           def __init__(self, t: typing.Any) -> None: ...
           @overload
           def __init__(self) -> None: ...

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

        class str(typing.Sized, typing.Sequence[str]):
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
          def __iter__(self) -> Iterator[str]: ...

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

        class list(typing.Sequence[_T], typing.Generic[_T]):
          @overload
          def __init__(self) -> None: ...
          @overload
          def __init__(self, iterable: typing.Iterable[_T]) -> None: ...

          def __add__(self, x: list[_T]) -> list[_T]: ...
          def __iter__(self) -> typing.Iterator[_T]: ...
          def append(self, element: _T) -> None: ...
          @overload
          def __getitem__(self, i: int) -> _T: ...
          @overload
          def __getitem__(self, s: slice) -> typing.List[_T]: ...

        class set(typing.Iterable[_T], typing.Generic[_T]): pass

        def len(o: typing.Sized) -> int: ...
        def isinstance(a, b) -> bool: ...
        def sum(iterable: typing.Iterable[_T]) -> typing.Union[_T, int]: ...

        class IsAwaitable(typing.Awaitable[int]): pass
        class contextlib.ContextManager(typing.Generic[_T_co]):
          def __enter__(self) -> _T_co:
            pass
        class contextlib.GeneratorContextManager(
            contextlib.ContextManager[_T],
            typing.Generic[_T]):
          pass
        def sys.exit(code: int) -> typing.NoReturn: ...

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
        def takes_iterable(x: typing.Iterable[_T]) -> None: ...

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
  ]


let environment ?(sources = typeshed_stubs) ?(configuration = configuration) () =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate (Environment.handler ~configuration environment) ~configuration sources;
  Environment.handler ~configuration environment


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


let resolution ?(sources = typeshed_stubs) () =
  let environment = environment ~sources () in
  add_defaults_to_environment ~configuration environment;
  Environment.resolution
    environment
    ~define:(Define.create_toplevel ~qualifier:[] ~statements:[])
    ()


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
    let check ?mode_override source =
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
        |> Plugin.apply_to_ast
      in
      let environment =
        let environment = environment ~configuration () in
        Environment.populate ~configuration environment [source];
        environment
      in
      let configuration = Configuration.create ~debug ~strict ~declare ~infer () in
      check_errors configuration environment ?mode_override source
    in
    List.map
      (check ?mode_override source)
      ~f:(fun error -> Error.description error ~detailed:show_error_traces)
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:"\n")
    errors
    descriptions
