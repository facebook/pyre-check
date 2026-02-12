(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Test module provides several related things used for fast tests of
 * Pyre's type checking logic:
 * - A library of "small" stubs to use for type checking in tests,
 *   which decouples unit tests from typeshed changes and makes tests faster.
 * - A ScratchProject module for creating projects on the fly to use in tests.
 *   - There are two flavors of ScratchProject:
 *     - Projects with sources in-memory (via a backdoor to ModuleTracker), for speed
 *     - Filesystem-backed projects, which are needed to test incremental updates
 *   - ScratchProject will create an environment, and also provides a variety
 *     of functions to get handles on data for testing or run assertions.
 * - A handful of utility functions that help with interacting with OUnit.
 *)

(* `open Core` hides this module, and does not provide a replacement for `open_process_args_in`. *)
module CamlUnix = Unix
open Core
open Analysis
open Ast
open Pyre
open Statement

let relative_artifact_path ~root ~relative =
  PyrePath.create_relative ~root ~relative |> ArtifactPath.create


let initialize () =
  Log.GlobalState.initialize_for_tests ();
  Memory.initialize_for_tests ();
  Statistics.disable ()


let () = initialize ()

let trim_extra_indentation source =
  let is_non_empty line = not (String.for_all ~f:Char.is_whitespace line) in
  let minimum_indent lines =
    let indent line = String.to_list line |> List.take_while ~f:Char.is_whitespace |> List.length in
    List.filter lines ~f:is_non_empty
    |> List.map ~f:indent
    |> List.fold ~init:Int.max_value ~f:Int.min
  in
  let strip_line minimum_indent line =
    if not (is_non_empty line) then
      line
    else
      String.slice line minimum_indent (String.length line)
  in
  let strip_lines minimum_indent = List.map ~f:(strip_line minimum_indent) in
  let lines = String.rstrip source |> String.split ~on:'\n' in
  let minimum_indent = minimum_indent lines in
  strip_lines minimum_indent lines |> String.concat ~sep:"\n"


let rec coerce_special_methods { Node.location; value } =
  (* Turn all explicit dunder attribute accesses to be special methods accesses. *)
  let open Expression in
  match value with
  | Expression.Name (Name.Attribute ({ base; attribute; _ } as name))
    when is_dunder_attribute attribute ->
      {
        Node.location;
        value =
          Expression.Name
            (Name.Attribute
               {
                 name with
                 base = coerce_special_methods base;
                 origin = Some (Origin.create ~location Origin.ForTestPurpose);
               });
      }
  | Call { callee; arguments; origin } ->
      { Node.location; value = Call { callee = coerce_special_methods callee; arguments; origin } }
  | _ -> { Node.location; value }


let coerce_special_methods_source source =
  let module Transform = Transform.Make (struct
    include Transform.Identity

    type t = unit

    let transform_expression_children _ _ = true

    let expression _ expression = coerce_special_methods expression
  end)
  in
  Transform.transform () source |> Transform.source


let run tests =
  let rec bracket test =
    let bracket_test test context =
      initialize ();
      test context
    in
    match test with
    | OUnitTest.TestLabel (name, test) -> OUnitTest.TestLabel (name, bracket test)
    | OUnitTest.TestList tests -> OUnitTest.TestList (List.map tests ~f:bracket)
    | OUnitTest.TestCase (length, f) -> OUnitTest.TestCase (length, bracket_test f)
  in
  tests |> bracket |> OUnit2.run_test_tt_main


let sanitized_module_name raw_module_name =
  if String.is_prefix ~prefix:"Dune__exe__" raw_module_name then
    String.drop_prefix raw_module_name (String.length "Dune__exe__")
  else
    raw_module_name


let labeled_test_case function_name line_number ?name test_callback =
  let description =
    let function_name =
      function_name |> String.split ~on:'.' |> List.last |> Option.value ~default:"toplevel"
    in
    function_name
    ^ ":"
    ^
    match name with
    | Some name -> name ^ ":line="
    | None -> "testcase:line="
  in
  let open OUnit2 in
  description ^ string_of_int line_number >:: test_callback


let parse_untrimmed ?(handle = "") ?(coerce_special_methods = false) source =
  let do_parse context =
    match PyreCPythonParser.parse_module ~context ~enable_type_comment:true source with
    | Result.Ok statements ->
        let typecheck_flags =
          let qualifier = ModulePath.qualifier_from_relative_path handle in
          Source.TypecheckFlags.parse ~qualifier (String.split source ~on:'\n')
        in
        let source = Source.create ~typecheck_flags ~relative:handle statements in
        let source =
          if coerce_special_methods then coerce_special_methods_source source else source
        in
        source
    | Result.Error { PyreCPythonParser.Error.line; column; message; _ } ->
        let error =
          Format.asprintf
            "Could not parse test source at line %d, column %d.\nReason: %s. Test input:\n%s"
            line
            column
            message
            source
        in
        failwith error
  in
  PyreCPythonParser.with_context do_parse


let parse ?(handle = "") ?(coerce_special_methods = false) source =
  trim_extra_indentation source |> parse_untrimmed ~handle ~coerce_special_methods


let parse_single_statement ?(preprocess = false) ?(coerce_special_methods = false) ?handle source =
  let source =
    if preprocess then
      Preprocessing.preprocess_no_wildcards
        ~string_annotation_preserve_location:true
        (parse ?handle ~coerce_special_methods source)
    else
      parse ~coerce_special_methods source
  in
  match source with
  | { Source.statements = [statement]; _ } -> statement
  | _ -> failwith "Could not parse single statement"


let parse_last_statement source =
  match parse source with
  | { Source.statements; _ } when not (List.is_empty statements) -> List.last_exn statements
  | _ -> failwith "Could not parse last statement"


let parse_single_define source =
  match parse_single_statement source with
  | { Node.value = Statement.Define define; _ } -> define
  | _ -> failwith "Could not parse single define"


let parse_single_class ?(preprocess = false) source =
  match parse_single_statement ~preprocess source with
  | { Node.value = Statement.Class definition; _ } -> definition
  | _ -> failwith "Could not parse single class"


let parse_single_expression ?(preprocess = false) ?(coerce_special_methods = false) source =
  match parse_single_statement ~preprocess ~coerce_special_methods source with
  | { Node.value = Statement.Expression expression; _ } -> expression
  | _ -> failwith "Could not parse single expression."


let parse_single_call ?(preprocess = false) source =
  match parse_single_expression ~preprocess source with
  | { Node.value = Call call; _ } -> call
  | _ -> failwith "Could not parse single call"


let parse_callable ?name ?(aliases = Type.resolved_empty_aliases) callable =
  let variable_aliases _ = None in

  let callable =
    parse_single_expression callable |> Type.create ~variables:variable_aliases ~aliases
  in
  match name, callable with
  | Some name, Type.Callable callable ->
      Type.Callable { callable with Type.Callable.kind = Named name }
  | ( Some name,
      Type.Parametric
        { name = "BoundMethod"; arguments = [Single (Callable callable); Single self_type] } ) ->
      Type.parametric
        "BoundMethod"
        [Single (Callable { callable with Type.Callable.kind = Named name }); Single self_type]
  | _ -> callable


let parse_position position =
  Location.position_from_string position
  |> function
  | Ok position -> position
  | Error error -> failwith error


let parse_location location =
  Location.from_string location
  |> function
  | Ok location -> location
  | Error error -> failwith error


let parse_location_with_module location =
  let module_reference, location = String.lsplit2_exn ~on:':' location in
  parse_location location
  |> Location.with_module ~module_reference:(Reference.create module_reference)


let diff ~print format (left, right) =
  try
    let input =
      let command =
        Format.sprintf
          "diff -u <(printf '%s') <(printf '%s')"
          (Format.asprintf "%a" print left)
          (Format.asprintf "%a" print right)
      in
      CamlUnix.open_process_args_in "/bin/bash" [| "/bin/bash"; "-c"; command |]
    in
    Format.fprintf format "\n%s" (In_channel.input_all input);
    In_channel.close input
  with
  | CamlUnix.Unix_error (CamlUnix.E2BIG, _, _) ->
      Format.fprintf
        format
        "\nWarning: `diff -u <(printf '...') <(printf '...')` failed: input too large"


let map_printer ~key_pp ~data_pp map =
  let to_string (key, data) = Format.asprintf "    %a -> %a" key_pp key data_pp data in
  Map.to_alist map |> List.map ~f:to_string |> String.concat ~sep:"\n"


let show_optional show optional = optional >>| show |> Option.value ~default:"None"

let collect_nodes_as_strings source =
  let module Collector = Visit.NodeCollector (struct
    type t = string * Location.t

    let predicate = function
      | Visit.Expression expression ->
          Some
            (Transform.sanitize_expression expression |> Expression.show, Node.location expression)
      | Visit.Statement _ -> None
      | Visit.Argument { argument = { Node.value; location }; _ } ->
          Some (Identifier.sanitized value, location)
      | Visit.Parameter { Node.value = { Expression.Parameter.name; _ }; location } ->
          Some (Identifier.sanitized name, location)
      | Visit.Reference { Node.value; location } -> Some (Reference.show value, location)
      | Visit.Substring substring ->
          let location =
            match substring with
            | Expression.Substring.Literal { Node.location; _ } -> location
            | Expression.Substring.Format { value; _ } -> Node.location value
          in
          Some (Expression.Substring.show substring, location)
      | Visit.Generator _ -> None
  end)
  in
  Collector.collect source


let make_location ~start:(start_line, start_column) ~stop:(stop_line, stop_column) =
  {
    Location.start = { Location.line = start_line; Location.column = start_column };
    stop = { Location.line = stop_line; Location.column = stop_column };
  }


let node ~start ~stop =
  let location = make_location ~start ~stop in
  Node.create ~location


let assert_source_equal ?(location_insensitive = false) left right =
  let typecheck_flags = Source.TypecheckFlags.create_for_testing () in
  let left = { left with Source.typecheck_flags } in
  let right = { right with Source.typecheck_flags } in
  let cmp =
    if location_insensitive then
      fun left right -> Source.location_insensitive_compare left right = 0
    else
      [%compare.equal: Source.t]
  in
  let print_difference format (left, right) =
    if
      [%compare.equal: Source.t]
        { left with Source.statements = [] }
        { right with Source.statements = [] }
    then
      diff ~print:Source.pp format (left, right)
    else
      diff ~print:Source.pp_all format (left, right)
  in
  OUnit2.assert_equal
    ~cmp
    ~printer:(fun source -> Format.asprintf "%a" Source.pp source)
    ~pp_diff:print_difference
    left
    right


let assert_source_equal_with_locations expected actual =
  let equal_statement left right = Int.equal 0 (Statement.compare left right) in
  let compare_sources expected actual =
    let { Source.statements = left; _ } = expected in
    let { Source.statements = right; _ } = actual in
    List.equal equal_statement left right
  in
  let pp_with_locations format { Source.statements; _ } =
    let rec print_statement ~prefix statement =
      let indented_prefix = prefix ^ "  " in
      let pp_nested_expressions format statement =
        let print_expression (node_string, location) =
          let add_indentation expression_string =
            let indent expression_string =
              String.split ~on:'\n' expression_string |> String.concat ~sep:("\n" ^ indented_prefix)
            in
            indented_prefix ^ indent expression_string
          in
          Format.fprintf
            format
            "%s -> (%a)\n"
            (node_string |> add_indentation)
            Location.pp_line_and_column
            location
        in
        collect_nodes_as_strings (Source.create [statement]) |> List.iter ~f:print_expression
      in
      let pp_nested_statements _ statement =
        let immediate_children =
          match Node.value statement with
          | Statement.Class { Class.body; _ }
          | Define { Define.body; _ }
          | With { With.body; _ } ->
              body
          | For { For.body; orelse; _ }
          | If { If.body; orelse; _ }
          | While { While.body; orelse; _ } ->
              body @ orelse
          | Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
              let handlers =
                let get_handler_body sofar { Try.Handler.body; _ } = body @ sofar in
                List.fold ~init:[] ~f:get_handler_body handlers
              in
              body @ handlers @ orelse @ finally
          | _ -> []
        in
        List.iter ~f:(print_statement ~prefix:indented_prefix) immediate_children
      in
      Format.fprintf
        format
        "%s%a -> (%a)\n%sNested Expressions:\n%a%sNested Statements:\n%a"
        prefix
        pp
        statement
        Location.pp_line_and_column
        statement.Node.location
        indented_prefix
        pp_nested_expressions
        statement
        indented_prefix
        pp_nested_statements
        statement
    in
    List.iter statements ~f:(print_statement ~prefix:"")
  in
  let pp_diff_with_locations format (expected, actual) =
    (* Don't diff location discrepancies due to Location.any. *)
    let create_separate_blocks pp_string =
      pp_string |> String.split ~on:'\n' |> String.concat ~sep:"\n\n"
    in
    let expected_string =
      Format.asprintf "%a" pp_with_locations expected |> create_separate_blocks
    in
    let actual_string = Format.asprintf "%a" pp_with_locations actual |> create_separate_blocks in
    let collect_non_anys difference =
      let matches regex line = Str.string_match (Str.regexp regex) line 0 in
      let is_removed_any = matches "-.*\\(-1:-1--1:-1\\)" in
      let is_difference line = matches "-.*" line || matches "+.*" line in
      let rec collect_non_anys collected = function
        | removed :: _ :: rest when is_removed_any removed -> collect_non_anys collected rest
        | line :: rest when is_difference line -> collect_non_anys (line :: collected) rest
        | _ :: rest -> collect_non_anys collected rest
        | _ -> collected
      in
      collect_non_anys [] difference |> List.rev
    in
    Format.asprintf "%a" (diff ~print:String.pp) (expected_string, actual_string)
    |> String.split ~on:'\n'
    |> collect_non_anys
    |> String.concat ~sep:"\n"
    |> Format.fprintf format "%s"
  in
  OUnit2.assert_equal
    ~cmp:compare_sources
    ~printer:(fun source -> Format.asprintf "\n%a" pp_with_locations source)
    ~pp_diff:pp_diff_with_locations
    expected
    actual


let assert_type_equal = OUnit2.assert_equal ~printer:Type.show ~cmp:Type.equal

(* Assertion helpers. *)
let assert_true = OUnit2.assert_bool ""

let assert_false test = OUnit2.assert_bool "" (not test)

let assert_bool_equals ~expected = if expected then assert_true else assert_false

let assert_is_some test = assert_true (Option.is_some test)

let assert_is_none test = assert_true (Option.is_none test)

let assert_unreached () = assert_true false

(* Override `OUnit`s functions the return absolute paths. *)
let bracket_tmpdir ?suffix context = OUnit2.bracket_tmpdir ?suffix context |> CamlUnix.realpath

let bracket_tmpfile ?suffix context =
  OUnit2.bracket_tmpfile ?suffix context
  |> fun (filename, channel) -> CamlUnix.realpath filename, channel


let pyre_extensions_stubs () =
  [
    ( "pyre_extensions/__init__.pyi",
      {|
        from typing import List, Optional, Type, TypeVar, Callable
        from .generic import Generic as Generic
        import type_variable_operators

        _T = TypeVar("_T")
        _A = TypeVar("_A", bound=int)
        _B = TypeVar("_B", bound=int)
        _T1 = TypeVar("_T1")
        _T2 = TypeVar("_T2")

        class TypeVarTuple:
          def __init__(
              self,
              name: str,
              *constraints: Type[Any],
              bound: Union[None, Type[Any], str] = ...,
              covariant: bool = ...,
              contravariant: bool = ...,
          ) -> None: ...

        def none_throws(optional: Optional[_T]) -> _T: ...
        def safe_cast(new_type: Type[_T], value: Any) -> _T: ...
        def ParameterSpecification(__name: str) -> List[Type]: ...
        def classproperty(f: Any) -> Any: ...
        class Add(Generic[_A, _B], int): pass
        class Multiply(Generic[_A, _B], int): pass
        class Subtract(Generic[_A, _B], int): pass
        class Divide(Generic[_A, _B], int): pass
        _Ts = TypeVarTuple("_Ts")
        class Length(Generic[_Ts], int): pass
        class Product(Generic[_Ts], int): pass

        class Unpack(Generic[_T]): ...
        class Broadcast(Generic[_T1, _T2]): ...
        class BroadcastError(Generic[_T1, _T2]): ...
        class Compose(Generic[_Ts]): ...

        T = TypeVar("T", bound=Callable[..., object])

        def override(func: T) -> T:
            return func

        class PyreReadOnly(Generic[_T]): ...
        |}
    );
    ( "pyre_extensions/generic.pyi",
      {|
        from typing import Any
        class Generic:
            def __class_getitem__(cls, *args: object) -> Any:
                return cls
        |}
    );
    ( "pyre_extensions/type_variable_operators.pyi",
      {|
        from typing import List, Optional, Type, TypeVar, _SpecialForm
        Map: _SpecialForm
        PositionalArgumentsOf: _SpecialForm
        KeywordArgumentsOf: _SpecialForm
        ArgumentsOf: _SpecialForm
        Concatenate: _SpecialForm
        |}
    );
  ]


let django_stubs () =
  [
    ( "django-stubs/http/__init__.pyi",
      {|
        import typing
        from django.http.request import HttpRequest as HttpRequest

        class HttpResponse: ...
        class Request:
          GET: typing.Dict[str, typing.Any] = ...
          POST: typing.Dict[str, typing.Any] = ...
        |}
    );
    ( "django-stubs/http/request.pyi",
      {|
        import typing

        class HttpRequest:
          GET: typing.Dict[str, typing.Any] = ...
          POST: typing.Dict[str, typing.Any] = ...
        |}
    );
    "django-stubs/__init__.pyi", "import django.http";
  ]


let pysa_stubs () =
  [
    ( "pysa/__init__.pyi",
      {|
        from typing import Any, Callable

        def _test_sink(arg: Any) -> None: ...
        def _test_source() -> Any: ...
        def _tito(*x: Any, **kw: Any) -> Any: ...
        def _user_controlled() -> Any: ...
        def _cookies() -> Any: ...
        def _rce(argument: Any) -> None: ...
        def _sql(argument: Any) -> None: ...
        _global_sink: Any
        def copy(obj: object) -> object: ...
        def pyre_dump() -> None: ...

        class ClassWithSinkAttribute:
          attribute: Any = ...

        class TestCallableTarget:
          def __call__(self) -> int: ...
          def async_delay(*args: Any, **kwargs: Any) -> None: ...
          def async_schedule(*args: Any, **kwargs: Any) -> None: ...
        def to_callable_target(f: Callable[..., Any]) -> TestCallableTarget: ...
        |}
    );
  ]


let pytest_stubs () = ["pytest.pyi", ""]

let typeshed_stubs ?(include_helper_builtins = true) ?(include_pyre_extensions = true) () =
  let builtins =
    let helper_builtin_stubs =
      {|
        import typing

        def not_annotated(input = ...): ...

        def expect_int(i: int) -> None: ...
        def to_int(x: Any) -> int: ...
        def int_to_str(i: int) -> str: ...
        def str_to_int(i: str) -> int: ...
        def optional_str_to_int(i: Optional[str]) -> int: ...
        def int_to_bool(i: int) -> bool: ...
        def int_to_int(i: int) -> int: pass
        def str_float_to_int(i: str, f: float) -> int: ...
        def str_float_tuple_to_int(t: Tuple[str, float]) -> int: ...
        def nested_tuple_to_int(t: Tuple[Tuple[str, float], float]) -> int: ...
        def return_tuple() -> Tuple[int, int]: ...
        def unknown_to_int(i) -> int: ...
        def star_int_to_int( *args, x: int) -> int: ...
        def takes_iterable(x: Iterable[_T]) -> None: ...
        def awaitable_int() -> typing.Awaitable[int]: ...
        def condition() -> bool: ...

        class IsAwaitable(typing.Awaitable[int]): pass

        def identity(x: _T) -> _T: ...
        _VR = TypeVar("_VR", str, int)
        def variable_restricted_identity(x: _VR) -> _VR: pass

        def returns_undefined()->Undefined: ...
        class Spooky:
          def undefined(self)->Undefined: ...

        class Attributes:
          int_attribute: int

        class OtherAttributes:
          int_attribute: int
          str_attribute: str

        class A: ...
        class B(A): ...
        class C(A): ...
        class D(B,C): ...
        class obj():
          @staticmethod
          def static_int_to_str(i: int) -> str: ...
        class _PathLike(typing.Generic[typing.AnyStr]): ...
      |}
    in
    let builtin_stubs =
      {|
        from typing import (
          TypeVar, Iterator, Iterable, NoReturn, Never, overload, Container,
          Sequence, MutableSequence, Mapping, MutableMapping, Tuple, List, Any,
          Dict, Callable, Generic, Set, AbstractSet, FrozenSet, MutableSet, Sized,
          Reversible, SupportsInt, SupportsFloat, SupportsAbs, SupportsLenAndGetItem,
          SupportsComplex, SupportsRound, IO, BinaryIO, Union, final, TypeGuard,
          ItemsView, KeysView, ValuesView, ByteString, Optional, AnyStr, Type, Text,
          SupportsIter, SupportsNext, Concatenate, TypeVarTuple, Unpack
        )
        from typing import _PyreReadOnly_
        from typing_extensions import Literal, LiteralString, Self, ParamSpec

        _T = TypeVar('_T')
        _T_co = TypeVar('_T_co', covariant=True)
        _S = TypeVar('_S')
        _KT = TypeVar('_KT')
        _VT = TypeVar('_VT')
        _Self = TypeVar('_Self')
        _R_co = TypeVar('_R_co', covariant=True)
        _P = ParamSpec("_P")

        class type:
          __name__: str = ...
          def __call__(self, *args: Any, **kwargs: Any) -> Any: ...
          @overload
          def __init__(self, o: object) -> None: ...
          @overload
          def __init__(self, name: str, bases: Tuple[type, ...], dict: Dict[str, Any]) -> None: ...
          @overload
          def __new__(cls, o: object) -> type: ...
          @overload
          def __new__(cls, name: str, bases: Tuple[type, ...], namespace: Dict[str, Any]) -> type: ...

        class object():
          __doc__: str
          __module__: str
          @property
          def __class__(self: _T) -> Type[_T]: ...
          def __init__(self) -> None: ...
          def __new__(cls) -> Any: ...
          def __setattr__(self, name: str, value: Any) -> None: ...
          def __eq__(self, o: object) -> bool: ...
          def __ne__(self, o: object) -> bool: ...
          def __str__(self) -> str: ...
          def __repr__(self) -> str: ...
          def __hash__(self) -> int: ...
          def __format__(self, __format_spec: str) -> str: ...
          def __getattribute__(self, name: str) -> Any: ...
          def __delattr__(self, name: str) -> None: ...
          def __sizeof__(self) -> int: ...
          def __reduce__(self) -> tuple: ...
          def __dir__(self) -> Iterable[str]: ...
          def __init_subclass__(cls) -> None: ...

        class ellipsis: ...
        Ellipsis: ellipsis

        class BaseException(object):
          def __init__(self, *args: object) -> None: ...
          def __str__(self) -> str: ...
          def __repr__(self) -> str: ...
        class Exception(BaseException): ...
        class KeyError(Exception): ...
        class NameError(Exception): ...
        class ValueError(Exception): ...

        _BaseExceptionT_co = TypeVar("_BaseExceptionT_co", bound=BaseException, covariant=True)
        _ExceptionT_co = TypeVar("_ExceptionT_co", bound=Exception, covariant=True)
        class BaseExceptionGroup(BaseException, Generic[_BaseExceptionT_co]):
          def __str__(self) -> str: ...
          def __repr__(self) -> str: ...
          def __init__(self, __message: str, __exceptions: Sequence[_BaseExceptionT_co]) -> None: ...
        class ExceptionGroup(BaseExceptionGroup[_ExceptionT_co], Exception): ...

        class slice:
          @overload
          def __init__(self, stop: Optional[int]) -> None: ...
          @overload
          def __init__(
            self,
            start: Optional[int],
            stop: Optional[int],
            step: Optional[int] = ...
          ) -> None: ...
          def indices(self, len: int) -> Tuple[int, int, int]: ...

        class range(Sequence[int]):
          start: int
          stop: int
          step: int
          @overload
          def __init__(self, stop: int) -> None: ...
          @overload
          def __init__(self, start: int, stop: int, step: int = ...) -> None: ...
          def count(self, value: int) -> int: ...
          def index(self, value: int, start: int = ..., stop: Optional[int] = ...) -> int: ...
          def __len__(self) -> int: ...
          def __contains__(self, o: object) -> bool: ...
          def __iter__(self) -> Iterator[int]: ...
          @overload
          def __getitem__(self, i: int) -> int: ...
          @overload
          def __getitem__(self, s: slice) -> range: ...
          def __repr__(self) -> str: ...
          def __reversed__(self) -> Iterator[int]: ...

        class super:
           @overload
           def __init__(self, t: Any, obj: Any) -> None: ...
           @overload
           def __init__(self, t: Any) -> None: ...
           @overload
           def __init__(self) -> None: ...

        class bool(int): ...

        class bytes(): ...

        N1 = TypeVar("N1", bound=int)
        N2 = TypeVar("N2", bound=int)

        class int:
            @overload
            def __new__(cls, __x: str | ReadableBuffer | SupportsInt | SupportsIndex | SupportsTrunc = ...) -> Self: ...
            @overload
            def __new__(cls, __x: str | bytes | bytearray, base: SupportsIndex) -> Self: ...
            if sys.version_info >= (3, 8):
                def as_integer_ratio(self) -> tuple[int, Literal[1]]: ...

            @property
            def real(self) -> int: ...
            @property
            def imag(self) -> Literal[0]: ...
            @property
            def numerator(self) -> int: ...
            @property
            def denominator(self) -> Literal[1]: ...
            def conjugate(self) -> int: ...
            def bit_length(self) -> int: ...
            if sys.version_info >= (3, 10):
                def bit_count(self) -> int: ...

            if sys.version_info >= (3, 11):
                def to_bytes(
                    self, length: SupportsIndex = 1, byteorder: Literal["little", "big"] = "big", *, signed: bool = False
                ) -> bytes: ...
                @classmethod
                def from_bytes(
                    cls,
                    bytes: Iterable[SupportsIndex] | SupportsBytes | ReadableBuffer,
                    byteorder: Literal["little", "big"] = "big",
                    *,
                    signed: bool = False,
                ) -> Self: ...
            else:
                def to_bytes(self, length: SupportsIndex, byteorder: Literal["little", "big"], *, signed: bool = False) -> bytes: ...
                @classmethod
                def from_bytes(
                    cls,
                    bytes: Iterable[SupportsIndex] | SupportsBytes | ReadableBuffer,
                    byteorder: Literal["little", "big"],
                    *,
                    signed: bool = False,
                ) -> Self: ...

            def __add__(self: int, __x: int) -> int: ...
            def __sub__(self: int, __x: int) -> int: ...
            def __mul__(self: int, __x: int) -> int: ...
            def __floordiv__(self: int, __x: int) -> int: ...
            def __truediv__(self, __x: int) -> float: ...
            def __mod__(self, __x: int) -> int: ...
            def __divmod__(self, __x: int) -> tuple[int, int]: ...
            def __radd__(self, __x: int) -> int: ...
            def __rsub__(self, __x: int) -> int: ...
            def __rmul__(self, __x: int) -> int: ...
            def __rfloordiv__(self, __x: int) -> int: ...
            def __rtruediv__(self, __x: int) -> float: ...
            def __rmod__(self, __x: int) -> int: ...
            def __rdivmod__(self, __x: int) -> tuple[int, int]: ...
            @overload
            def __pow__(self, __x: Literal[0]) -> Literal[1]: ...
            @overload
            def __pow__(self, __x: Literal[0], __modulo: None) -> Literal[1]: ...
            @overload
            def __pow__(self, __x: _PositiveInteger, __modulo: None = None) -> int: ...
            @overload
            def __pow__(self, __x: _NegativeInteger, __modulo: None = None) -> float: ...
            # positive x -> int; negative x -> float
            # return type must be Any as `int | float` causes too many false-positive errors
            @overload
            def __pow__(self, __x: int, __modulo: None = None) -> Any: ...
            @overload
            def __pow__(self, __x: int, __modulo: int) -> int: ...
            def __rpow__(self, __x: int, __mod: int | None = None) -> Any: ...
            def __and__(self, __n: int) -> int: ...
            def __or__(self, __n: int) -> int: ...
            def __xor__(self, __n: int) -> int: ...
            def __lshift__(self, __n: int) -> int: ...
            def __rshift__(self, __n: int) -> int: ...
            def __rand__(self, __n: int) -> int: ...
            def __ror__(self, __n: int) -> int: ...
            def __rxor__(self, __n: int) -> int: ...
            def __rlshift__(self, __n: int) -> int: ...
            def __rrshift__(self, __n: int) -> int: ...
            def __neg__(self) -> int: ...
            def __pos__(self) -> int: ...
            def __invert__(self) -> int: ...
            def __trunc__(self) -> int: ...
            def __ceil__(self) -> int: ...
            def __floor__(self) -> int: ...
            def __round__(self, __ndigits: SupportsIndex = ...) -> int: ...
            def __getnewargs__(self) -> tuple[int]: ...
            def __eq__(self, __x: object) -> bool: ...
            def __ne__(self, __x: object) -> bool: ...
            def __lt__(self, __x: int) -> bool: ...
            def __le__(self, __x: int) -> bool: ...
            def __gt__(self, __x: int) -> bool: ...
            def __ge__(self, __x: int) -> bool: ...
            def __float__(self) -> float: ...
            def __int__(self) -> int: ...
            def __abs__(self) -> int: ...
            def __bool__(self) -> bool: ...
            def __index__(self) -> int: ...

        class float:
            def __new__(cls, __x: SupportsFloat | SupportsIndex | str | ReadableBuffer = ...) -> Self: ...
            def as_integer_ratio(self) -> tuple[int, int]: ...
            def hex(self) -> str: ...
            def is_integer(self) -> bool: ...
            @classmethod
            def fromhex(cls, __s: str) -> Self: ...
            @property
            def real(self) -> float: ...
            @property
            def imag(self) -> float: ...
            def conjugate(self) -> float: ...
            def __add__(self, __x: float) -> float: ...
            def __sub__(self, __x: float) -> float: ...
            def __mul__(self, __x: float) -> float: ...
            def __floordiv__(self, __x: float) -> float: ...
            def __truediv__(self, __x: float) -> float: ...
            def __mod__(self, __x: float) -> float: ...
            def __divmod__(self, __x: float) -> tuple[float, float]: ...
            @overload
            def __pow__(self, __x: int, __mod: None = None) -> float: ...
            # positive x -> float; negative x -> complex
            # return type must be Any as `float | complex` causes too many false-positive errors
            @overload
            def __pow__(self, __x: float, __mod: None = None) -> Any: ...
            def __radd__(self, __x: float) -> float: ...
            def __rsub__(self, __x: float) -> float: ...
            def __rmul__(self, __x: float) -> float: ...
            def __rfloordiv__(self, __x: float) -> float: ...
            def __rtruediv__(self, __x: float) -> float: ...
            def __rmod__(self, __x: float) -> float: ...
            def __rdivmod__(self, __x: float) -> tuple[float, float]: ...
            @overload
            def __rpow__(self, __x: _PositiveInteger, __modulo: None = None) -> float: ...
            @overload
            def __rpow__(self, __x: _NegativeInteger, __mod: None = None) -> complex: ...
            # Returning `complex` for the general case gives too many false-positive errors.
            @overload
            def __rpow__(self, __x: float, __mod: None = None) -> Any: ...
            def __getnewargs__(self) -> tuple[float]: ...
            def __trunc__(self) -> int: ...
            if sys.version_info >= (3, 9):
                def __ceil__(self) -> int: ...
                def __floor__(self) -> int: ...

            @overload
            def __round__(self, __ndigits: None = None) -> int: ...
            @overload
            def __round__(self, __ndigits: SupportsIndex) -> float: ...
            def __eq__(self, __x: object) -> bool: ...
            def __ne__(self, __x: object) -> bool: ...
            def __lt__(self, __x: float) -> bool: ...
            def __le__(self, __x: float) -> bool: ...
            def __gt__(self, __x: float) -> bool: ...
            def __ge__(self, __x: float) -> bool: ...
            def __neg__(self) -> float: ...
            def __pos__(self) -> float: ...
            def __int__(self) -> int: ...
            def __float__(self) -> float: ...
            def __abs__(self) -> float: ...
            def __bool__(self) -> bool: ...

        class complex:
            if sys.version_info >= (3, 8):
                # Python doesn't currently accept SupportsComplex for the second argument
                @overload
                def __new__(
                    cls,
                    real: complex | SupportsComplex | SupportsFloat | SupportsIndex = ...,
                    imag: complex | SupportsFloat | SupportsIndex = ...,
                ) -> Self: ...
                @overload
                def __new__(cls, real: str | SupportsComplex | SupportsFloat | SupportsIndex | complex) -> Self: ...
            else:
                @overload
                def __new__(cls, real: complex | SupportsComplex | SupportsFloat = ..., imag: complex | SupportsFloat = ...) -> Self: ...
                @overload
                def __new__(cls, real: str | SupportsComplex | SupportsFloat | complex) -> Self: ...

            @property
            def real(self) -> float: ...
            @property
            def imag(self) -> float: ...
            def conjugate(self) -> complex: ...
            def __add__(self, __x: complex) -> complex: ...
            def __sub__(self, __x: complex) -> complex: ...
            def __mul__(self, __x: complex) -> complex: ...
            def __pow__(self, __x: complex, __mod: None = None) -> complex: ...
            def __truediv__(self, __x: complex) -> complex: ...
            def __radd__(self, __x: complex) -> complex: ...
            def __rsub__(self, __x: complex) -> complex: ...
            def __rmul__(self, __x: complex) -> complex: ...
            def __rpow__(self, __x: complex, __mod: None = None) -> complex: ...
            def __rtruediv__(self, __x: complex) -> complex: ...
            def __eq__(self, __x: object) -> bool: ...
            def __ne__(self, __x: object) -> bool: ...
            def __neg__(self) -> complex: ...
            def __pos__(self) -> complex: ...
            def __abs__(self) -> float: ...
            def __bool__(self) -> bool: ...
            if sys.version_info >= (3, 11):
                def __complex__(self) -> complex: ...

        class str(Sequence[str]):
          @overload
          def __init__(self, o: object = ...) -> None: ...
          @overload
          def __init__(self, o: bytes, encoding: str = ..., errors: str = ...) -> None: ...
          @overload
          def format(self: LiteralString, *args: LiteralString, **kwargs: LiteralString) -> LiteralString: ...
          @overload
          def format(self, *args: object, **kwargs: object) -> str: ...  # type: ignore[misc]
          def lower(self) -> str: pass
          def upper(self) -> str: ...
          def substr(self, index: int) -> str: pass

          @overload
          def join(self: Literal[str], iterable: Iterable[Literal[str]]) -> Literal[str]: ...
          @overload
          def join(self, iterable: Iterable[str]) -> str: ...

          def capitalize(self) -> str: ...

          def __lt__(self, other: int) -> float: ...
          def __ne__(self, other) -> int: ...

          @overload
          def __add__(self: Literal[str], other: Literal[str], /) -> Literal[str]: ...
          @overload
          def __add__(self, other: str, /) -> str: ...
          @overload
          def __mod__(self: LiteralString, __x: LiteralString | tuple[LiteralString, ...]) -> LiteralString: ...
          @overload
          def __mod__(self, __x: Any) -> str: ...  # type: ignore[misc]

          def __pos__(self) -> float: ...
          def __repr__(self) -> float: ...
          def __str__(self) -> str: ...
          def __getitem__(self, i: Union[int, slice]) -> str: ...
          def __iter__(self) -> Iterator[str]: ...
          def __eq__(self, x: object) -> bool: ...
          def __len__(self) -> int: ...

        class tuple(Sequence[_T_co], Sized, Generic[_T_co]):
          def __init__(self, a: List[_T_co]): ...
          def __len__(self) -> int: ...
          def tuple_method(self, a: int): ...
          def __lt__(self, x: Tuple[_T_co, ...]) -> bool: ...
          def __le__(self, x: Tuple[_T_co, ...]) -> bool: ...
          def __gt__(self, x: Tuple[_T_co, ...]) -> bool: ...
          def __ge__(self, x: Tuple[_T_co, ...]) -> bool: ...
          def __add__(self, x: Tuple[_T_co, ...]) -> Tuple[_T_co, ...]: ...
          def __mul__(self, n: int) -> Tuple[_T_co, ...]: ...
          def __rmul__(self, n: int) -> Tuple[_T_co, ...]: ...
          @overload
          def __getitem__(self, x: int) -> _T_co: ...
          @overload
          def __getitem__(self, x: slice) -> Tuple[_T_co, ...]: ...

        class dict(MutableMapping[_KT, _VT], Generic[_KT, _VT]):
          @overload
          def __init__(self, **kwargs: _VT) -> None: ...
          @overload
          def __init__(self, map: Mapping[_KT, _VT], **kwargs: _VT) -> None: ...
          @overload
          def __init__(self, iterable: Iterable[Tuple[_KT, _VT]], **kwargs: _VT) -> None:
            ...
          def add_key(self, key: _KT) -> None: pass
          def add_value(self, value: _VT) -> None: pass
          def add_both(self, key: _KT, value: _VT) -> None: pass
          def items(self) -> Iterable[Tuple[_KT, _VT]]: pass
          def __delitem__(self, __v: _KT) -> None: ...
          def __getitem__(self, __k: _KT, /) -> _VT: ...
          def __setitem__(self, __k: _KT, __v: _VT, /) -> None: ...
          @overload
          def get(self, __key: _KT, /) -> Optional[_VT]: ...
          @overload
          def get(self, __key: _KT, __default: Union[_VT, _T], /) -> Union[_VT, _T]: ...
          def __len__(self) -> int: ...

        class list(MutableSequence[_T], Generic[_T]):
          @overload
          def __init__(self) -> None: ...
          @overload
          def __init__(self, __iterable: Iterable[_T]) -> None: ...

          def __add__(self, __x: list[_T]) -> list[_T]: ...
          def __iadd__(self, __x: Iterable[_T]) -> Self: ...
          def __mul__(self, __n: int) -> list[_T]: ...
          def extend(self, __iterable: Iterable[_T]) -> None: ...

          @overload
          def __iter__(self) -> Iterator[_T]: ...
          @overload
          def __iter__(self: _PyreReadOnly_[Self]) -> Iterator[_PyreReadOnly_[_T]]: ...

          def append(self, object: _T, /) -> None: ...
          def insert(self, __index: int, __object: _T) -> None: ...
          @overload
          def __getitem__(self, index: int) -> _T: ...
          @overload
          def __getitem__(self, index: slice) -> List[_T]: ...
          @overload
          def __setitem__(self, index: int, value: _T) -> None: ...
          @overload
          def __setitem__(self, index: slice, value: Iterable[_T]) -> None: ...
          def __delitem__(self, index: Union[int | slice]) -> None: ...
          def __contains__(self, o: object) -> bool: ...
          def __len__(self) -> int: ...

        class set(Iterable[_T], Generic[_T]):
          def __init__(self, iterable: Iterable[_T] = ...) -> None: ...
          def add(self, __element: _T) -> None: ...
          def copy(self) -> set[_T]: ...
          def difference(self, *s: Iterable[Any]) -> set[_T]: ...
          def difference_update(self, *s: Iterable[Any]) -> None: ...
          def discard(self, __element: _T) -> None: ...
          def intersection(self, *s: Iterable[Any]) -> set[_T]: ...
          def intersection_update(self, *s: Iterable[Any]) -> None: ...
          def isdisjoint(self, __s: Iterable[Any]) -> bool: ...
          def issubset(self, __s: Iterable[Any]) -> bool: ...
          def issuperset(self, __s: Iterable[Any]) -> bool: ...
          def remove(self, __element: _T) -> None: ...
          def symmetric_difference(self, __s: Iterable[_T]) -> set[_T]: ...
          def symmetric_difference_update(self, __s: Iterable[_T]) -> None: ...
          def union(self, *s: Iterable[_S]) -> set[_T | _S]: ...
          def update(self, *s: Iterable[_T]) -> None: ...
          def __contains__(self, __o: object) -> bool: ...
          def __and__(self, __s: AbstractSet[object]) -> set[_T]: ...
          def __iand__(self, __s: AbstractSet[object]) -> Self: ...
          def __or__(self, __s: AbstractSet[_S]) -> set[_T | _S]: ...
          def __ior__(self, __s: AbstractSet[_T]) -> Self: ...  # type: ignore[override,misc]
          def __sub__(self, __s: AbstractSet[_T | None]) -> set[_T]: ...
          def __isub__(self, __s: AbstractSet[object]) -> Self: ...
          def __xor__(self, __s: AbstractSet[_S]) -> set[_T | _S]: ...
          def __ixor__(self, __s: AbstractSet[_T]) -> Self: ...  # type: ignore[override,misc]
          def __le__(self, __s: AbstractSet[object]) -> bool: ...
          def __lt__(self, __s: AbstractSet[object]) -> bool: ...
          def __ge__(self, __s: AbstractSet[object]) -> bool: ...
          def __gt__(self, __s: AbstractSet[object]) -> bool: ...

        def len(o: _PyreReadOnly_[Sized]) -> int: ...
        def isinstance(
          a: object,
          b: Union[type, Tuple[Union[type, Tuple], ...]]
        ) -> bool: ...
        def sum(iterable: Iterable[_T]) -> Union[_T, int]: ...

        def eval(arg: source, /) -> None: ...

        def getattr(
          o: object,
          name: str,
          default: Any = ...,
          /
        ) -> Any: ...

        def all(i: Iterable[_T]) -> bool: ...

        _T1 = TypeVar("_T1")
        _T2 = TypeVar("_T2")
        _T3 = TypeVar("_T3")
        _T4 = TypeVar("_T4")
        _T5 = TypeVar("_T5")

        class enumerate(Iterator[tuple[int, _T]], Generic[_T]):
          def __init__(self, iterable: Iterable[_T], start: int = ...) -> None: ...
          def __iter__(self: Self) -> Self: ...
          def __next__(self) -> Tuple[int, _T]: ...

        class zip(Iterator[_T_co], Generic[_T_co]):
          @overload
          def __new__(cls, __iter1: Iterable[_T1], *, strict: bool = ...) -> zip[Tuple[_T1]]: ...
          @overload
          def __new__(cls, __iter1: Iterable[_T1], __iter2: Iterable[_T2], *, strict: bool = ...) -> zip[Tuple[_T1, _T2]]: ...
          @overload
          def __new__(
              cls, __iter1: Iterable[_T1], __iter2: Iterable[_T2], __iter3: Iterable[_T3], *, strict: bool = ...
          ) -> zip[Tuple[_T1, _T2, _T3]]: ...
          def __iter__(self: _Self) -> _Self: ...
          def __next__(self) -> _T_co: ...

        class map(Iterator[_S], Generic[_S]):
          @overload
          def __init__(self, __func: Callable[[_T1], _S], __iter1: Iterable[_T1]) -> None: ...
          @overload
          def __init__(self, __func: Callable[[_T1, _T2], _S], __iter1: Iterable[_T1], __iter2: Iterable[_T2]) -> None: ...
          @overload
          def __init__(
              self, __func: Callable[[_T1, _T2, _T3], _S], __iter1: Iterable[_T1], __iter2: Iterable[_T2], __iter3: Iterable[_T3]
          ) -> None: ...
          def __iter__(self: _Self) -> _Self: ...
          def __next__(self) -> _S: ...

        class filter(Iterator[_T], Generic[_T]):
          @overload
          def __new__(cls, __function: None, __iterable: Iterable[_T | None], /) -> _Self: ...
          @overload
          def __new__(cls, __function: Callable[[_S], TypeGuard[_T]], __iterable: Iterable[_S], /) -> _Self: ...
          @overload
          def __new__(cls, __function: Callable[[_T], Any], __iterable: Iterable[_T], /) -> _Self: ...
          def __iter__(self: _Self) -> _Self: ...
          def __next__(self) -> _T: ...

        class reversed(Iterator[_T], Generic[_T]):
          @overload
          def __init__(self, __sequence: Reversible[_T]) -> None: ...
          @overload
          def __init__(self, __sequence: SupportsLenAndGetItem[_T]) -> None: ...
          def __iter__(self: Self) -> Self: ...
          def __next__(self) -> _T: ...

        class property:
           def getter(self, fget: Any) -> Any: ...
           def setter(self, fset: Any) -> Any: ...
           def deletler(self, fdel: Any) -> Any: ...

        class staticmethod:
           def __init__(self, f: Callable[..., Any]): ...

        class classmethod(Generic[_T, _P, _R_co]):
           def __init__(self, __f: Callable[Concatenate[type[_T], _P], _R_co]) -> None: ...
           @overload
           def __get__(self, __instance: _T, __owner: type[_T] | None = None) -> Callable[_P, _R_co]: ...
           @overload
           def __get__(self, __instance: None, __owner: type[_T]) -> Callable[_P, _R_co]: ...

        def callable(__o: object) -> bool: ...

        def any(__iterable: Iterable[object]) -> bool: ...

        _SupportsNextT = TypeVar("_SupportsNextT", bound=SupportsNext[Any], covariant=True)

        class _GetItemIterable(Protocol[_T_co]):
          def __getitem__(self, __i: int) -> _T_co: ...

        @overload
        def iter(__iterable: SupportsIter[_SupportsNextT]) -> _SupportsNextT: ...
        @overload
        def iter(__iterable: _GetItemIterable[_T]) -> Iterator[_T]: ...
        @overload
        def iter(__function: Callable[[], _T | None], __sentinel: None) -> Iterator[_T]: ...
        @overload
        def iter(__function: Callable[[], _T], __sentinel: object) -> Iterator[_T]: ...

        @overload
        def next(__i: SupportsNext[_T]) -> _T: ...
        @overload
        def next(__i: SupportsNext[_T], __default: _VT) -> _T | _VT: ...

        if sys.version_info >= (3,):
          class _Writer(Protocol):
              def write(self, __s: str) -> Any: ...
          def print(
              *values: object, sep: Optional[Text] = ..., end: Optional[Text] = ..., file: Optional[_Writer] = ..., flush: bool = ...
          ) -> None: ...
        else:
          class _Writer(Protocol):
              def write(self, __s: Any) -> Any: ...
          # This is only available after from __future__ import print_function.
          def print( *values: object, sep: Optional[Text] = ..., end: Optional[Text] = ..., file: Optional[_Writer] = ...) -> None: ...

        class _NotImplementedType(Any):  # type: ignore
            # A little weird, but typing the __call__ as NotImplemented makes the error message
            # for NotImplemented() much better
            __call__: NotImplemented  # type: ignore

        NotImplemented: _NotImplementedType

        # simplified stub for sorted
        def sorted(__iterable: Iterable[_T]) -> List[_T]: ...
      |}
    in
    if include_helper_builtins then
      String.concat ~sep:"\n" [String.rstrip builtin_stubs; helper_builtin_stubs]
    else
      builtin_stubs
  in
  let sqlalchemy_stubs =
    [
      (* These are simplified versions of the SQLAlchemy stubs. *)
      ( "sqlalchemy/ext/declarative/__init__.pyi",
        {|
            from .api import (
                declarative_base as declarative_base,
                DeclarativeMeta as DeclarativeMeta,
            )
          |}
      );
      ( "sqlalchemy/ext/declarative/api.pyi",
        {|
            def declarative_base(bind: Optional[Any] = ..., metadata: Optional[Any] = ...,
                                 mapper: Optional[Any] = ..., cls: Any = ..., name: str = ...,
                                 constructor: Any = ..., class_registry: Optional[Any] = ...,
                                 metaclass: Any = ...): ...

            class DeclarativeMeta(type):
                def __init__(cls, classname, bases, dict_) -> None: ...
                def __setattr__(cls, key, value): ...
          |}
      );
      ( "sqlalchemy/__init__.pyi",
        {|
            from typing import Generic, Optional, Text as typing_Text, Type, TypeVar, final, overload
            from typing_extensions import Literal
            _T_co = TypeVar('_T_co', covariant=True)
            _T = TypeVar('_T')
            class TypeEngine(Generic[_T_co]): ...
            class Integer(TypeEngine[int]): ...
            class String(TypeEngine[str]): ...

            @final
            class Column(Generic[_T]):
              @overload
              def __new__(
                cls, type_: TypeEngine[_T],
              ) -> Column[Optional[_T]]: ...
              @overload
              def __new__(
                cls, type_: TypeEngine[_T],
                primary_key: Literal[True] = ...
              ) -> Column[_T]: ...
              @overload
              def __new__(
                cls, type_: TypeEngine[_T],
                primary_key: Literal[False] = ...
              ) -> Column[Optional[_T]]: ...
              def __new__(
                cls, type_: TypeEngine[_T],
                primary_key: bool = ...
              ) -> Union[Column[_T], Column[Optional[_T]]]: ...

              @overload
              def __get__(self, instance: None, owner: Any) -> Column[_T]: ...
              @overload
              def __get__(self, instance: object, owner: Any) -> _T: ...
          |}
      );
      ( "sqlalchemy/sql/schema.pyi",
        {|
            class Table: ...
            class MetaData: ...
        |} );
    ]
  in
  let torch_stubs =
    [
      ( "torch/__init__.pyi",
        {|
          class Tensor: ...

          def zeros( *args: object, **kwargs: object) -> Tensor: ...
        |}
      );
      "torch/nn/__init__.pyi", {|
          class Module: ...
        |};
    ]
  in
  let readonly_stubs =
    [
      ( "readonly_stubs_for_testing.pyi",
        {|
          from typing import Callable, TypeVar, Generic

          F = TypeVar("F", bound=Callable[..., object])
          T = TypeVar("T")

          def readonly_entrypoint(f: F) -> F: ...

          class MySafeReadOnlyClass:
            some_attribute: str

          class MySafeReadOnlyInt(int):
            pass

          class MySafeReadOnlyIdType(MySafeReadOnlyInt, Generic[T]):
            pass
|}
      );
      ( "readonly_module_to_ignore.pyi",
        {|
          class Foo:
            def some_method(self, x: int) -> None: ...

          def some_function(x: int) -> None: ...
|}
      );
    ]
  in
  [
    ( "sys.py",
      {|
        from typing import NoReturn
        def exit(code: int) -> NoReturn: ...
    |} );
    ( "hashlib.pyi",
      {|
        _DataType = typing.Union[int, str]
        class _Hash:
          digest_size: int
        def md5(input: _DataType) -> _Hash: ...
        |}
    );
    ( "typing.pyi",
      {|
        from abc import ABCMeta, abstractmethod
        import collections

        class _SpecialForm:
          def __getitem__(self, typeargs: Any) -> Any: ...

        TypeVar = object()
        Annotated = TypeAlias(object)
        List = TypeAlias(object)
        Dict = TypeAlias(object)
        Set = TypeAlias(object)
        Optional: _SpecialForm = ...
        Union: _SpecialForm = ...
        Any = object()
        overload = object()
        if sys.version_info >= (3, 8):
          Final: _SpecialForm = ...
          _F = TypeVar('_F', bound=Callable[..., Any])
          def final(f: _F) -> _F: ...
          Literal: _SpecialForm = ...
          # TypedDict is a (non-subscriptable) special form.
          TypedDict: object
          def override(f: _F) -> _F: ...

        Callable: _SpecialForm = ...
        Protocol: _SpecialForm = ...
        Type: _SpecialForm = ...
        Tuple: _SpecialForm = ...
        Generic: _SpecialForm = ...
        ClassVar: _SpecialForm = ...
        NoReturn = _SpecialForm = ...
        Never = _SpecialForm = ...
        TypeGuard: _SpecialForm = ...

        if sys.version_info < (3, 7):
            class GenericMeta(type): ...

        if sys.version_info >= (3, 10):
          Concatenate: _SpecialForm = ...
          TypeAlias: _SpecialForm = ...
          TypeGuard: _SpecialForm = ...

        if sys.version_info >= (3, 11):
          Unpack: _SpecialForm = ...
          TypeVarTuple: _SpecialForm = ...

        @runtime
        class Sized(Protocol, metaclass=ABCMeta):
            @abstractmethod
            def __len__(self) -> int: ...

        @runtime
        class Hashable(Protocol, metaclass=ABCMeta):
            @abstractmethod
            def __hash__(self) -> int: ...

        _T = TypeVar('_T')
        _S = TypeVar('_S')
        _KT = TypeVar('_KT')
        _VT = TypeVar('_VT')
        _T_co = TypeVar('_T_co', covariant=True)
        _V_co = TypeVar('_V_co', covariant=True)
        _KT_co = TypeVar('_KT_co', covariant=True)
        _VT_co = TypeVar('_VT_co', covariant=True)
        _T_contra = TypeVar('_T_contra', contravariant=True)
        _Self = TypeVar('_Self')

        # Pyre injects a type into the `typing` stub so that we can safely rely on
        # it to handle read-only methods on stdlib types in our patched typeshed.
        class _PyreReadOnly_(Generic[_T]):
            pass


        class Iterable(Protocol[_T_co]):
          def __iter__(self) -> Iterator[_T_co]: pass
        class Iterator(Iterable[_T_co], Protocol[_T_co]):
          def __next__(self) -> _T_co: ...

        class AsyncIterable(Protocol[_T_co]):
          def __aiter__(self) -> AsyncIterator[_T_co]: ...
        class AsyncIterator(AsyncIterable[_T_co], Protocol[_T_co]):
          def __anext__(self) -> Awaitable[_T_co]: ...
          def __aiter__(self) -> AsyncIterator[_T_co]: ...
        class AsyncContextManager(Protocol[_T_co]):
            def __aenter__(self) -> Awaitable[_T_co]:
                ...

            def __aexit__(
                self,
                exc_type: Optional[Type[BaseException]],
                exc_value: Optional[BaseException],
                traceback: Optional[TracebackType],
            ) -> Awaitable[Optional[bool]]:
                ...

        if sys.version_info >= (3, 6):
          class Collection(Iterable[_T_co], Protocol[_T_co]):
            @abstractmethod
            def __len__(self) -> int: ...
          _Collection = Collection
        else:
          class _Collection(Iterable[_T_co], Protocol[_T_co]):
            @abstractmethod
            def __len__(self) -> int: ...
        class Sequence(_Collection[_T_co], Generic[_T_co]): pass

        class Generator(Iterator[_T_co], Generic[_T_co, _T_contra, _V_co]):
          pass

        class AbstractSet(_Collection[_T_co], Generic[_T_co]):
            @abstractmethod
            def __contains__(self, x: object) -> bool: ...
            # Mixin methods
            def __le__(self, s: AbstractSet[typing.Any]) -> bool: ...
            def __lt__(self, s: AbstractSet[typing.Any]) -> bool: ...
            def __gt__(self, s: AbstractSet[typing.Any]) -> bool: ...
            def __ge__(self, s: AbstractSet[typing.Any]) -> bool: ...
            def __and__(self, s: AbstractSet[typing.Any]) -> AbstractSet[_T_co]: ...
            def __or__(self, s: AbstractSet[_T]) -> AbstractSet[Union[_T_co, _T]]: ...
            def __sub__(self, s: AbstractSet[typing.Any]) -> AbstractSet[_T_co]: ...
            def __xor__(self, s: AbstractSet[_T]) -> AbstractSet[Union[_T_co, _T]]: ...
            def isdisjoint(self, s: AbstractSet[typing.Any]) -> bool: ...

        class ValuesView(MappingView, Iterable[_VT_co], Generic[_VT_co]):
            def __contains__(self, o: object) -> bool: ...
            def __iter__(self) -> Iterator[_VT_co]: ...

        class Mapping(_Collection[_KT], Generic[_KT, _VT_co]):
          @abstractmethod
          def __getitem__(self, __k: _KT, /) -> _VT_co:
              ...
          # Mixin methods
          @overload
          def get(self, __key: _KT, /) -> Optional[_VT_co]: ...
          @overload
          def get(self, __key: _KT, /, default: Union[_VT_co, _T]) -> Union[_VT_co, _T]: ...
          def items(self) -> AbstractSet[Tuple[_KT, _VT_co]]: ...
          def keys(self) -> AbstractSet[_KT]: ...
          def values(self) -> ValuesView[_VT_co]: ...
          def __contains__(self, __o: object) -> bool: ...

        class MutableMapping(Mapping[_KT, _VT], Generic[_KT, _VT]):
          @abstractmethod
          def __setitem__(self, __k: _KT, __v: _VT) -> None: ...
          @abstractmethod
          def __delitem__(self, __v: _KT) -> None: ...
          def clear(self) -> None: ...
          @overload
          def pop(self, __key: _KT) -> _VT: ...
          @overload
          def pop(self, __key: _KT, __default: _VT | _T = ...) -> _VT | _T: ...
          def popitem(self) -> tuple[_KT, _VT]: ...
          @overload
          def update(self, __m: SupportsKeysAndGetItem[_KT, _VT], **kwargs: _VT) -> None: ...
          @overload
          def update(self, __m: Iterable[tuple[_KT, _VT]], **kwargs: _VT) -> None: ...
          @overload
          def update(self, **kwargs: _VT) -> None: ...
          @overload
          def setdefault(self: MutableMapping[_KT, _T | None], __key: _KT) -> _T | None: ...
          @overload
          def setdefault(self, __key: _KT, __default: _VT) -> _VT: ...

        class MutableSequence(Sequence[_T], Generic[_T]):
          @abstractmethod
          def insert(self, index: int, value: _T) -> None: ...
          @overload
          @abstractmethod
          def __getitem__(self, index: int) -> _T: ...
          @overload
          @abstractmethod
          def __getitem__(self, index: slice) -> MutableSequence[_T]: ...
          @overload
          @abstractmethod
          def __setitem__(self, index: int, value: _T) -> None: ...
          @overload
          @abstractmethod
          def __setitem__(self, index: slice, value: Iterable[_T]) -> None: ...
          @overload
          @abstractmethod
          def __delitem__(self, index: int) -> None: ...
          @overload
          @abstractmethod
          def __delitem__(self, index: slice) -> None: ...
          # Mixin methods
          def append(self, value: _T) -> None: ...
          def clear(self) -> None: ...
          def extend(self, values: Iterable[_T]) -> None: ...
          def reverse(self) -> None: ...
          def pop(self, index: int = ...) -> _T: ...
          def remove(self, value: _T) -> None: ...
          def __iadd__(self: _Self, values: Iterable[_T]) -> _Self: ...

        class MutableSet(AbstractSet[_T], Generic[_T]):
          @abstractmethod
          def add(self, value: _T) -> None: ...
          @abstractmethod
          def discard(self, value: _T) -> None: ...
          # Mixin methods
          def clear(self) -> None: ...
          def pop(self) -> _T: ...
          def remove(self, value: _T) -> None: ...
          def __ior__(self: _Self, it: AbstractSet[_T]) -> _Self: ...  # type: ignore[override,misc]
          def __iand__(self: _Self, it: AbstractSet[Any]) -> _Self: ...
          def __ixor__(self: _Self, it: AbstractSet[_T]) -> _Self: ...  # type: ignore[override,misc]
          def __isub__(self: _Self, it: AbstractSet[Any]) -> _Self: ...

        class Awaitable(Protocol[_T_co]):
          def __await__(self) -> Generator[Any, None, _T_co]: ...

        class Coroutine(Awaitable[_V_co], Generic[_T_co, _T_contra, _V_co]):
          __name__: str
          __qualname__: str

        class AsyncGenerator(AsyncIterator[_T_co], Generic[_T_co, _T_contra]):
            @abstractmethod
            def __anext__(self) -> Awaitable[_T_co]:
                ...
            @abstractmethod
            def __aiter__(self) -> AsyncGenerator[_T_co, _T_contra]:
                ...

        @overload
        def cast(tp: Type[_T], obj: Any) -> _T: ...
        @overload
        def cast(tp: str, obj: Any) -> Any: ...

        # NamedTuple is special-cased in the type checker
        class NamedTuple(tuple):
            _field_types: collections.OrderedDict[str, Type[Any]]
            _field_defaults: Dict[str, Any] = ...
            _fields: Tuple[str, ...]
            _source: str

            def __init__(self, typename: str, fields: Iterable[Tuple[str, Any]] = ..., *,
                         verbose: bool = ..., rename: bool = ..., **kwargs: Any) -> None: ...

            @classmethod
            def _make(cls: Type[_T], iterable: Iterable[Any]) -> _T: ...

            def _asdict(self) -> collections.OrderedDict[str, Any]: ...
            def _replace(self: _T, **kwargs: Any) -> _T: ...

        class ParamSpec(list):
            args = object()
            kwargs = object()
            def __init__(self, *args: object, **kwargs: object) -> None: ...

        LiteralString: _SpecialForm = ...

        Required: _SpecialForm = ...
        NotRequired: _SpecialForm = ...

        Self: _SpecialForm = ...

        if sys.version_info >= (3, 10):
          def is_typeddict(tp: object) -> bool: ...

        class Reversible(Iterable[_T_co], Protocol[_T_co]):
          @abstractmethod
          def __reversed__(self) -> Iterator[_T_co]: ...

        class SupportsLenAndGetItem(Protocol[_T_co]):
          def __len__(self) -> int: ...
          def __getitem__(self, __k: int) -> _T_co: ...

        class SupportsIter(Protocol[_T_co]):
          def __iter__(self) -> _T_co: ...

        class SupportsNext(Protocol[_T_co]):
          def __next__(self) -> _T_co: ...

        if sys.version_info >= (3, 11):
          def reveal_type(__obj: _T) -> _T: ...
          def assert_type(__val: _T, __typ: Any) -> _T: ...
          def dataclass_transform(
            *,
            eq_default: bool = True,
            order_default: bool = False,
            kw_only_default: bool = False,
            frozen_default: bool = False,  # on 3.11, runtime accepts it as part of kwargs
            field_specifiers: tuple[type[Any] | Callable[..., Any], ...] = (),
            **kwargs: Any,
          ) -> IdentityFunction: ...
          def assert_never(arg: Never, /) -> Never: ...
      |}
    );
    "asyncio/coroutines.pyi", {|
        def coroutine(f: typing.Any) -> typing.Any: ...
        |};
    ( "_asyncio.pyi",
      {|
        from typing import Generic, TypeVar, Iterable, Generator
        _T = TypeVar('_T')
        class Future(Awaitable[_T], Iterable[_T]):
            def __await__(self) -> Generator[Any, None, _T]: ...
        |}
    );
    ( "asyncio/futures.pyi",
      {|
        from typing import Generic, TypeVar, Iterable, Generator
        _T = TypeVar('_T')
        class Future(Awaitable[_T], Iterable[_T]):
            def __await__(self) -> Generator[Any, None, _T]: ...
        |}
    );
    "asyncio/__init__.pyi", "import asyncio.coroutines";
    ( "abc.pyi",
      {|
        from typing import Type, TypeVar, Concatenate
        from collections.abc import Callable
        from typing_extensions import Literal, ParamSpec
        _T = TypeVar("_T")
        _FuncT = TypeVar('FuncT')
        _R_co = TypeVar("_R_co", covariant=True)
        _P = ParamSpec("_P")
        class ABCMeta(type):
          def register(cls: ABCMeta, subclass: Type[_T]) -> Type[_T]: ...
        def abstractmethod(callable: _FuncT) -> _FuncT: ...
        class abstractproperty(property): ...
        class ABC(metaclass=ABCMeta): ...
        class abstractclassmethod(classmethod[_T, _P, _R_co]):
          def __init__(self, callable: Callable[Concatenate[type[_T], _P], _R_co]) -> None: ...
        |}
    );
    ( "mock.pyi",
      {|
        class Base: ...
        class Mock(Base): ...
        class NonCallableMock: ...
        |}
    );
    ( "unittest/mock.pyi",
      {|
        class Base: ...
        class Mock(Base): ...
        class NonCallableMock: ...
        |}
    );
    "builtins.pyi", builtins;
    ( "dataclasses.pyi",
      {|
        from typing import TypeVar, Generic, Type, Mapping, Any
        import sys
        _T = TypeVar('_T')
        class InitVar(Generic[_T]): ...
        def dataclass(_cls: Type[_T]) -> Type[_T]: ...
        if sys.version_info >= (3, 10):
          class KW_ONLY: ...
          @overload  # `default` and `default_factory` are optional and mutually exclusive.
          def field(
              *,
              default: _T,
              init: bool = True,
              repr: bool = True,
              hash: bool | None = None,
              compare: bool = True,
              metadata: Mapping[Any, Any] | None = None,
              kw_only: bool = ...,
          ) -> _T: ...
          @overload
          def field(
              *,
              default_factory: Callable[[], _T],
              init: bool = True,
              repr: bool = True,
              hash: bool | None = None,
              compare: bool = True,
              metadata: Mapping[Any, Any] | None = None,
              kw_only: bool = ...,
          ) -> _T: ...
          @overload
          def field(
              *,
              init: bool = True,
              repr: bool = True,
              hash: bool | None = None,
              compare: bool = True,
              metadata: Mapping[Any, Any] | None = None,
              kw_only: bool = ...,
          ) -> Any: ...

        |}
    );
    ( "functools.pyi",
      {|
        from typing import TypeVar, Generic, Callable, Tuple, Any, Dict, Optional, Sequence
        _AnyCallable = Callable[..., Any]
        _T = TypeVar("_T")
        _S = TypeVar("_S")

        @overload
        def reduce(function: Callable[[_T, _S], _T],
                   sequence: Iterable[_S], initial: _T) -> _T: ...

        @overload
        def reduce(function: Callable[[_T, _T], _T],
                   sequence: Iterable[_T]) -> _T: ...

        class partial(Generic[_T]):
            func: Callable[..., _T]
            args: Tuple[Any, ...]
            keywords: Dict[str, Any]
            def __init__(self, func: Callable[..., _T], *args: Any, **kwargs: Any) -> None: ...
            def __call__(self, *args: Any, **kwargs: Any) -> _T: ...

        class _lru_cache_wrapper(Generic[_T]):
            __wrapped__: Callable[..., _T]
            def __call__(self, *args, **kwargs) -> _T: ...
            def cache_info(self) -> str: ...
            def cache_clear(self) -> None: ...

        def lru_cache(
          maxsize: Optional[int] = ...,
          typed: bool = ...,
        ) -> Callable[[Callable[..., _T]], _lru_cache_wrapper[_T]]:
            ...

        def wraps(
          wrapped: _AnyCallable,
          assigned: Sequence[str] = ...,
          updated: Sequence[str] = ...
        ) -> Callable[[_T], _T]: ...
       |}
    );
    ( "subprocess.pyi",
      {|
        def run(command, shell): ...
        def call(command, shell): ...
        def check_call(command, shell): ...
        def check_output(command, shell): ...
        |}
    );
    ( "multiprocessing/context.pyi",
      {|
        from typing import Optional, Callable, Tuple, Any, Mapping
        class Process:
          _start_method: Optional[str]
          def __init__(
              self,
              group: None = ...,
              target: Optional[Callable[..., Any]] = ...,
              name: Optional[str] = ...,
              args: Tuple[Any, ...] = ...,
              kwargs: Mapping[str, Any] = ...,
              *,
              daemon: Optional[bool] = ...,
          ) -> None: ...
      |}
    );
    "multiprocessing/__init__.pyi", "from multiprocessing.context import Process as Process";
    ( "enum.pyi",
      {|
        from abc import ABCMeta as ABCMeta
        from typing import Type, Mapping
        _magic_enum_attr = property
        _T = typing.TypeVar('_T')
        class EnumMeta(ABCMeta):
          def __members__(self: Type[_T]) -> Mapping[str, _T]: ...
          def __iter__(self: typing.Type[_T]) -> typing.Iterator[_T]: ...
        class Enum(metaclass=EnumMeta):
          def __new__(cls: typing.Type[_T], value: object) -> _T: ...
          _value_: Any
          @_magic_enum_attr
          def value(self) -> Any: ...
          _name_: str
          @_magic_enum_attr
          def name(self) -> str: ...
        class IntEnum(int, Enum):
          _value_: int
          @_magic_enum_attr
          def value(self) -> int: ...
        if sys.version_info >= (3, 6):
          _auto_null: typing.Any
          class Flag(Enum):
            pass
          class IntFlag(int, Flag):  # type: ignore
            pass
        class auto:
          pass
        _EnumMemberT = TypeVar("_EnumMemberT")
        if sys.version_info >= (3, 11):
          class nonmember(Generic[_EnumMemberT]):
            value: _EnumMemberT
            def __init__(self, value: _EnumMemberT) -> None: ...
          class member(Generic[_EnumMemberT]):
            value: _EnumMemberT
            def __init__(self, value: _EnumMemberT) -> None: ...
        |}
    );
    "threading.pyi", {|
        class Thread:
          pass
        |};
    ( "typing_extensions.pyi",
      {|
        from typing import (
          _SpecialForm,
          Final as Final,
          ParamSpec as ParamSpec,
          overload as overload,
          override as override
        )
        Literal: _SpecialForm = ...
        LiteralString: _SpecialForm = ...

        TypeAlias: _SpecialForm = ...

        TypeGuard: _SpecialForm = ...

        ReadOnly: _SpecialForm = ...
        Required: _SpecialForm = ...
        NotRequired: _SpecialForm = ...
        Never = _SpecialForm = ...

        Unpack: _SpecialForm = ...
        TypeVarTuple: _SpecialForm = ...
        Concatenate: _SpecialForm = ...

        Self: _SpecialForm = ...

        if sys.version_info >= (3, 11):
          dataclass_transform = typing.dataclass_transform
        else:
          def dataclass_transform(
              *,
              eq_default: bool = True,
              order_default: bool = False,
              kw_only_default: bool = False,
              frozen_default: bool = False,
              field_specifiers: typing.Tuple[
                  typing.Union[typing.Type[typing.Any], typing.Callable[..., typing.Any]],
                  ...
              ] = (),
              **kwargs: typing.Any,
          ) -> typing.Callable[[T], T]:
        |}
    );
    ( "collections.pyi",
      {|
        from typing import (
            TypeVar,
            Generic,
            Dict,
            overload,
            List,
            Tuple,
            Any,
            Type,
            Optional,
            Union,
            Callable,
            Mapping,
            Iterable,
            Tuple,
        )

        _DefaultDictT = TypeVar("_DefaultDictT", bound=defaultdict)
        _KT = TypeVar("_KT")
        _VT = TypeVar("_VT")

        class OrderedDict(dict[_KT, _VT]):
            def popitem(self, last: bool = True) -> tuple[_KT, _VT]: ...
            def move_to_end(self, key: _KT, last: bool = True) -> None: ...
            def copy(self) -> Self: ...
            def __reversed__(self) -> Iterator[_KT]: ...
            def keys(self) -> _odict_keys[_KT, _VT]: ...
            def items(self) -> _odict_items[_KT, _VT]: ...
            def values(self) -> _odict_values[_KT, _VT]: ...
            # The signature of OrderedDict.fromkeys should be kept in line with `dict.fromkeys`, modulo positional-only differences.
            # Like dict.fromkeys, its true signature is not expressible in the current type system.
            # See #3800 & https://github.com/python/typing/issues/548#issuecomment-683336963.
            @classmethod
            @overload
            def fromkeys(cls, iterable: Iterable[_T], value: None = None) -> OrderedDict[_T, Any | None]: ...
            @classmethod
            @overload
            def fromkeys(cls, iterable: Iterable[_T], value: _S) -> OrderedDict[_T, _S]: ...
            # Keep OrderedDict.setdefault in line with MutableMapping.setdefault, modulo positional-only differences.
            @overload
            def setdefault(self: OrderedDict[_KT, _T | None], key: _KT, default: None = None) -> _T | None: ...
            @overload
            def setdefault(self, key: _KT, default: _VT) -> _VT: ...
            # Same as dict.pop, but accepts keyword arguments
            @overload
            def pop(self, key: _KT) -> _VT: ...
            @overload
            def pop(self, key: _KT, default: _VT) -> _VT: ...
            @overload
            def pop(self, key: _KT, default: _T) -> _VT | _T: ...
            def __eq__(self, value: object, /) -> bool: ...

        class defaultdict(Dict[_KT, _VT], Generic[_KT, _VT]):
            default_factory: Optional[Callable[[], _VT]] = ...

            @overload
            def __init__(self, **kwargs: _VT) -> None:
                ...

            @overload
            def __init__(self, default_factory: Optional[Callable[[], _VT]]) -> None:
                ...

            @overload
            def __init__(
                self, default_factory: Optional[Callable[[], _VT]], **kwargs: _VT
            ) -> None:
                ...

            @overload
            def __init__(
                self, default_factory: Optional[Callable[[], _VT]], map: Mapping[_KT, _VT]
            ) -> None:
                ...

            @overload
            def __init__(
                self,
                default_factory: Optional[Callable[[], _VT]],
                map: Mapping[_KT, _VT],
                **kwargs: _VT
            ) -> None:
                ...

            @overload
            def __init__(
                self,
                default_factory: Optional[Callable[[], _VT]],
                iterable: Iterable[Tuple[_KT, _VT]],
            ) -> None:
                ...

            @overload
            def __init__(
                self,
                default_factory: Optional[Callable[[], _VT]],
                iterable: Iterable[Tuple[_KT, _VT]],
                **kwargs: _VT
            ) -> None:
                ...

            def __missing__(self, key: _KT) -> _VT:
                ...

            def copy(self: _DefaultDictT) -> _DefaultDictT:
                ...
        |}
    );
    ( "contextlib.pyi",
      (* TODO (T41494196): Change the parameter and return type to AnyCallable *)
      {|
        from typing import Any, AsyncContextManager, AsyncIterator, Callable, Generic, Iterator, TypeVar
        _T_co = TypeVar('_T_co', covariant=True)
        _T = TypeVar('_T')
        class ContextManager(Generic[_T_co]):
          def __enter__(self) -> _T_co:
            pass
        class _GeneratorContextManager(
            contextlib.ContextManager[_T],
            Generic[_T]):
          pass
        def contextmanager(func: Callable[..., Iterator[_T]]) -> Callable[..., _GeneratorContextManager[_T]]: ...
        def asynccontextmanager(func: Callable[..., AsyncIterator[_T]]) -> Callable[..., AsyncContextManager[_T]]: ...
        |}
    );
    "taint.pyi", {|
        _global_sink: Any = ...
        |};
    ( "unittest.pyi",
      {|
        from unittest import case
        from unittest import mock
        from unittest.case import TestCase as TestCase
        curdir: str
        pardir: str
        sep: str
        |}
    );
    ( "os/__init__.pyi",
      {|
    from builtins import _PathLike as PathLike
    from . import path as path
    import typing
    environ: typing.Dict[str, str] = ...
        |}
    );
    "os/path.pyi", {|
        curdir: str
        pardir: str
        sep: str
      |};
    ( "unittest/case.pyi",
      {|
        class TestCase:
            def assertIsNotNone(self, x: Any, msg: Any = ...) -> Bool:
              ...
            def assertTrue(self, x: Any, msg: Any = ...) -> Bool:
              ...
            def assertFalse(self, x: Any, msg: Any = ...) -> Bool:
              ...
        |}
    );
    ( "numbers.pyi",
      {|
        class Number(metaclass=ABCMeta):
            def __hash__(self) -> int: ...

        class Complex(Number):
            @abstractmethod
            def __complex__(self) -> complex: ...
            def __bool__(self) -> bool: ...
            @property
            @abstractmethod
            def real(self) -> Any: ...
            @property
            @abstractmethod
            def imag(self) -> Any: ...
            @abstractmethod
            def __add__(self, other: Any) -> Any: ...
            @abstractmethod
            def __radd__(self, other: Any) -> Any: ...
            @abstractmethod
            def __neg__(self) -> Any: ...
            @abstractmethod
            def __pos__(self) -> Any: ...
            def __sub__(self, other: Any) -> Any: ...
            def __rsub__(self, other: Any) -> Any: ...
            @abstractmethod
            def __mul__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rmul__(self, other: Any) -> Any: ...
            @abstractmethod
            def __truediv__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rtruediv__(self, other: Any) -> Any: ...
            @abstractmethod
            def __pow__(self, exponent: Any) -> Any: ...
            @abstractmethod
            def __rpow__(self, base: Any) -> Any: ...
            @abstractmethod
            def __abs__(self) -> Real: ...
            @abstractmethod
            def conjugate(self) -> Any: ...
            @abstractmethod
            def __eq__(self, other: object) -> bool: ...

        class Real(Complex, SupportsFloat):
            @abstractmethod
            def __float__(self) -> float: ...
            @abstractmethod
            def __trunc__(self) -> int: ...
            @abstractmethod
            def __floor__(self) -> int: ...
            @abstractmethod
            def __ceil__(self) -> int: ...
            @abstractmethod
            @overload
            def __round__(self, ndigits: None = None) -> int: ...
            @abstractmethod
            @overload
            def __round__(self, ndigits: int) -> Any: ...
            def __divmod__(self, other: Any) -> Any: ...
            def __rdivmod__(self, other: Any) -> Any: ...
            @abstractmethod
            def __floordiv__(self, other: Any) -> int: ...
            @abstractmethod
            def __rfloordiv__(self, other: Any) -> int: ...
            @abstractmethod
            def __mod__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rmod__(self, other: Any) -> Any: ...
            @abstractmethod
            def __lt__(self, other: Any) -> bool: ...
            @abstractmethod
            def __le__(self, other: Any) -> bool: ...
            def __complex__(self) -> complex: ...
            @property
            def real(self) -> Any: ...
            @property
            def imag(self) -> Any: ...
            def conjugate(self) -> Any: ...

        class Rational(Real):
            @property
            @abstractmethod
            def numerator(self) -> int: ...
            @property
            @abstractmethod
            def denominator(self) -> int: ...
            def __float__(self) -> float: ...

        class Integral(Rational):
            @abstractmethod
            def __int__(self) -> int: ...
            def __index__(self) -> int: ...
            @abstractmethod
            def __pow__(self, exponent: Any, modulus: Any | None = None) -> Any: ...
            @abstractmethod
            def __lshift__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rlshift__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rshift__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rrshift__(self, other: Any) -> Any: ...
            @abstractmethod
            def __and__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rand__(self, other: Any) -> Any: ...
            @abstractmethod
            def __xor__(self, other: Any) -> Any: ...
            @abstractmethod
            def __rxor__(self, other: Any) -> Any: ...
            @abstractmethod
            def __or__(self, other: Any) -> Any: ...
            @abstractmethod
            def __ror__(self, other: Any) -> Any: ...
            @abstractmethod
            def __invert__(self) -> Any: ...
            def __float__(self) -> float: ...
            @property
            def numerator(self) -> int: ...
            @property
            def denominator(self) -> int: ...

        |}
    );
    ( "attr/__init__.pyi",
      {|
        from typing import Optional, TypeVar, Any, Dict
        _C = TypeVar("_C", bound=type)
        def s(
            maybe_cls: None = ...,
            these: Optional[Dict[str, Any]] = ...,
            repr_ns: Optional[str] = ...,
            repr: bool = ...,
            cmp: Optional[bool] = ...,
            hash: Optional[bool] = ...,
            init: bool = ...,
            slots: bool = ...,
            frozen: bool = ...,
            weakref_slot: bool = ...,
            str: bool = ...,
            auto_attribs: bool = ...,
            kw_only: bool = ...,
            cache_hash: bool = ...,
            auto_exc: bool = ...,
            eq: Optional[bool] = ...,
            order: Optional[bool] = ...,
        ) -> Callable[[_C], _C]: ...
      |}
    );
    "random.pyi", {|
        def random() -> float: ...
      |};
    ( "click/__init__.pyi",
      {|
        # -*- coding: utf-8 -*-
        """
            click
            ~~~~~

            Click is a simple Python module that wraps the stdlib's optparse to make
            writing command line scripts fun.  Unlike other modules, it's based around
            a simple API that does not come with too much magic and is composable.

            In case optparse ever gets removed from the stdlib, it will be shipped by
            this module.

            :copyright: (c) 2014 by Armin Ronacher.
            :license: BSD, see LICENSE for more details.
        """

        # Core classes
        from .core import (
            Context as Context,
            BaseCommand as BaseCommand,
            Command as Command,
            MultiCommand as MultiCommand,
            Group as Group,
            CommandCollection as CommandCollection,
            Parameter as Parameter,
            Option as Option,
            Argument as Argument,
        )

        # Globals
        from .globals import get_current_context as get_current_context

        # Decorators
        from .decorators import (
            pass_context as pass_context,
            pass_obj as pass_obj,
            make_pass_decorator as make_pass_decorator,
            command as command,
            group as group,
            argument as argument,
            option as option,
            confirmation_option as confirmation_option,
            password_option as password_option,
            version_option as version_option,
            help_option as help_option,
        )

        # Types
        from .types import (
            ParamType as ParamType,
            File as File,
            FloatRange as FloatRange,
            DateTime as DateTime,
            Path as Path,
            Choice as Choice,
            IntRange as IntRange,
            Tuple as Tuple,
            STRING as STRING,
            INT as INT,
            FLOAT as FLOAT,
            BOOL as BOOL,
            UUID as UUID,
            UNPROCESSED as UNPROCESSED,
        )

        # Utilities
        from .utils import (
            echo as echo,
            get_binary_stream as get_binary_stream,
            get_text_stream as get_text_stream,
            open_file as open_file,
            format_filename as format_filename,
            get_app_dir as get_app_dir,
            get_os_args as get_os_args,
        )

        # Terminal functions
        from .termui import (
            prompt as prompt,
            confirm as confirm,
            get_terminal_size as get_terminal_size,
            echo_via_pager as echo_via_pager,
            progressbar as progressbar,
            clear as clear,
            style as style,
            unstyle as unstyle,
            secho as secho,
            edit as edit,
            launch as launch,
            getchar as getchar,
            pause as pause,
        )

        # Exceptions
        from .exceptions import (
            ClickException as ClickException,
            UsageError as UsageError,
            BadParameter as BadParameter,
            FileError as FileError,
            Abort as Abort,
            NoSuchOption as NoSuchOption,
            BadOptionUsage as BadOptionUsage,
            BadArgumentUsage as BadArgumentUsage,
            MissingParameter as MissingParameter,
        )

        # Formatting
        from .formatting import HelpFormatter as HelpFormatter, wrap_text as wrap_text

        # Parsing
        from .parser import OptionParser as OptionParser

        # Controls if click should emit the warning about the use of unicode
        # literals.
        disable_unicode_literals_warning: bool


        __version__: str
      |}
    );
    ( "click/core.pyi",
      {|
        from typing import (
            Any,
            Callable,
            ContextManager,
            Dict,
            Generator,
            Iterable,
            List,
            Mapping,
            NoReturn,
            Optional,
            Sequence,
            Set,
            Tuple,
            TypeVar,
            Union,
        )

        from click.formatting import HelpFormatter
        from click.parser import OptionParser

        _CC = TypeVar("_CC", bound=Callable[[], Any])

        def invoke_param_callback(
            callback: Callable[[Context, Parameter, Optional[str]], Any],
            ctx: Context,
            param: Parameter,
            value: Optional[str]
        ) -> Any:
            ...


        def augment_usage_errors(
            ctx: Context, param: Optional[Parameter] = ...
        ) -> ContextManager[None]:
            ...


        def iter_params_for_processing(
            invocation_order: Sequence[Parameter],
            declaration_order: Iterable[Parameter],
        ) -> Iterable[Parameter]:
            ...


        class Context:
            parent: Optional[Context]
            command: Command
            info_name: Optional[str]
            params: Dict[Any, Any]
            args: List[str]
            protected_args: List[str]
            obj: Any
            default_map: Mapping[str, Any]
            invoked_subcommand: Optional[str]
            terminal_width: Optional[int]
            max_content_width: Optional[int]
            allow_extra_args: bool
            allow_interspersed_args: bool
            ignore_unknown_options: bool
            help_option_names: List[str]
            token_normalize_func: Optional[Callable[[str], str]]
            resilient_parsing: bool
            auto_envvar_prefix: Optional[str]
            color: Optional[bool]
            _meta: Dict[str, Any]
            _close_callbacks: List[Any]
            _depth: int

            def __init__(
                self,
                command: Command,
                parent: Optional[Context] = ...,
                info_name: Optional[str] = ...,
                obj: Optional[Any] = ...,
                auto_envvar_prefix: Optional[str] = ...,
                default_map: Optional[Mapping[str, Any]] = ...,
                terminal_width: Optional[int] = ...,
                max_content_width: Optional[int] = ...,
                resilient_parsing: bool = ...,
                allow_extra_args: Optional[bool] = ...,
                allow_interspersed_args: Optional[bool] = ...,
                ignore_unknown_options: Optional[bool] = ...,
                help_option_names: Optional[List[str]] = ...,
                token_normalize_func: Optional[Callable[[str], str]] = ...,
                color: Optional[bool] = ...
            ) -> None:
                ...

            @property
            def meta(self) -> Dict[str, Any]:
                ...

            @property
            def command_path(self) -> str:
                ...

            def scope(self, cleanup: bool = ...) -> ContextManager[Context]:
                ...

            def make_formatter(self) -> HelpFormatter:
                ...

            def call_on_close(self, f: _CC) -> _CC: ...

            def close(self) -> None:
                ...

            def find_root(self) -> Context:
                ...

            def find_object(self, object_type: type) -> Any:
                ...

            def ensure_object(self, object_type: type) -> Any:
                ...

            def lookup_default(self, name: str) -> Any:
                ...

            def fail(self, message: str) -> NoReturn:
                ...

            def abort(self) -> NoReturn:
                ...

            def exit(self, code: Union[int, str] = ...) -> NoReturn:
                ...

            def get_usage(self) -> str:
                ...

            def get_help(self) -> str:
                ...

            def invoke(self, callback: Union[Command, Callable[..., Any]], *args, **kwargs) -> Any: ...
            def forward(self, callback: Union[Command, Callable[..., Any]], *args, **kwargs) -> Any: ...

        class BaseCommand:
            allow_extra_args: bool
            allow_interspersed_args: bool
            ignore_unknown_options: bool
            name: str
            context_settings: Dict[Any, Any]
            def __init__(self, name: str, context_settings: Optional[Dict[Any, Any]] = ...) -> None: ...

            def get_usage(self, ctx: Context) -> str:
                ...

            def get_help(self, ctx: Context) -> str:
                ...

            def make_context(
                self, info_name: str, args: List[str], parent: Optional[Context] = ..., **extra
            ) -> Context:
                ...

            def parse_args(self, ctx: Context, args: List[str]) -> List[str]:
                ...

            def invoke(self, ctx: Context) -> Any:
                ...

            def main(
                self,
                args: Optional[List[str]] = ...,
                prog_name: Optional[str] = ...,
                complete_var: Optional[str] = ...,
                standalone_mode: bool = ...,
                **extra
            ) -> Any:
                ...

            def __call__(self, *args, **kwargs) -> Any:
                ...


        class Command(BaseCommand):
            callback: Optional[Callable[..., Any]]
            params: List[Parameter]
            help: Optional[str]
            epilog: Optional[str]
            short_help: Optional[str]
            options_metavar: str
            add_help_option: bool
            hidden: bool
            deprecated: bool

            def __init__(
                self,
                name: str,
                context_settings: Optional[Dict[Any, Any]] = ...,
                callback: Optional[Callable[..., Any]] = ...,
                params: Optional[List[Parameter]] = ...,
                help: Optional[str] = ...,
                epilog: Optional[str] = ...,
                short_help: Optional[str] = ...,
                options_metavar: str = ...,
                add_help_option: bool = ...,
                hidden: bool = ...,
                deprecated: bool = ...,
            ) -> None:
                ...

            def get_params(self, ctx: Context) -> List[Parameter]:
                ...

            def format_usage(
                self,
                ctx: Context,
                formatter: HelpFormatter
            ) -> None:
                ...

            def collect_usage_pieces(self, ctx: Context) -> List[str]:
                ...

            def get_help_option_names(self, ctx: Context) -> Set[str]:
                ...

            def get_help_option(self, ctx: Context) -> Optional[Option]:
                ...

            def make_parser(self, ctx: Context) -> OptionParser:
                ...

            def get_short_help_str(self, limit: int = ...) -> str:
                ...

            def format_help(self, ctx: Context, formatter: HelpFormatter) -> None:
                ...

            def format_help_text(self, ctx: Context, formatter: HelpFormatter) -> None:
                ...

            def format_options(self, ctx: Context, formatter: HelpFormatter) -> None:
                ...

            def format_epilog(self, ctx: Context, formatter: HelpFormatter) -> None:
                ...


        _T = TypeVar('_T')
        _F = TypeVar('_F', bound=Callable[..., Any])


        class MultiCommand(Command):
            no_args_is_help: bool
            invoke_without_command: bool
            subcommand_metavar: str
            chain: bool
            result_callback: Callable[..., Any]

            def __init__(
                self,
                name: Optional[str] = ...,
                invoke_without_command: bool = ...,
                no_args_is_help: Optional[bool] = ...,
                subcommand_metavar: Optional[str] = ...,
                chain: bool = ...,
                result_callback: Optional[Callable[..., Any]] = ...,
                **attrs
            ) -> None:
                ...

            def resultcallback(
                self, replace: bool = ...
            ) -> Callable[[_F], _F]:
                ...

            def format_commands(self, ctx: Context, formatter: HelpFormatter) -> None:
                ...

            def resolve_command(
                self, ctx: Context, args: List[str]
            ) -> Tuple[str, Command, List[str]]:
                ...

            def get_command(self, ctx: Context, cmd_name: str) -> Optional[Command]:
                ...

            def list_commands(self, ctx: Context) -> Iterable[str]:
                ...


        class Group(MultiCommand):
            commands: Dict[str, Command]

            def __init__(
                self, name: Optional[str] = ..., commands: Optional[Dict[str, Command]] = ..., **attrs
            ) -> None:
                ...

            def add_command(self, cmd: Command, name: Optional[str] = ...):
                ...

            def command(self, *args, **kwargs) -> Callable[[Callable[..., Any]], Command]: ...
            def group(self, *args, **kwargs) -> Callable[[Callable[..., Any]], Group]: ...


        class CommandCollection(Group):
            sources: List[Group]

            def __init__(
                self, name: Optional[str] = ..., sources: Optional[List[Group]] = ..., **attrs
            ) -> None:
                ...

            def add_source(self, multi_cmd: Group) -> None:
                ...


        class _ParamType:
            name: str
            is_composite: bool
            envvar_list_splitter: Optional[str]

            def __call__(
                self,
                value: Optional[str],
                param: Optional[Parameter] = ...,
                ctx: Optional[Context] = ...,
            ) -> Any:
                ...

            def get_metavar(self, param: Parameter) -> str:
                ...

            def get_missing_message(self, param: Parameter) -> str:
                ...

            def convert(
                self,
                value: str,
                param: Optional[Parameter],
                ctx: Optional[Context],
            ) -> Any:
                ...

            def split_envvar_value(self, rv: str) -> List[str]:
                ...

            def fail(self, message: str, param: Optional[Parameter] = ..., ctx: Optional[Context] = ...) -> NoReturn:
                ...


        # This type is here to resolve https://github.com/python/mypy/issues/5275
        _ConvertibleType = Union[type, _ParamType, Tuple[Union[type, _ParamType], ...],
                                 Callable[[str], Any], Callable[[Optional[str]], Any]]


        class Parameter:
            param_type_name: str
            name: str
            opts: List[str]
            secondary_opts: List[str]
            type: _ParamType
            required: bool
            callback: Optional[Callable[[Context, Parameter, str], Any]]
            nargs: int
            multiple: bool
            expose_value: bool
            default: Any
            is_eager: bool
            metavar: Optional[str]
            envvar: Union[str, List[str], None]

            def __init__(
                self,
                param_decls: Optional[List[str]] = ...,
                type: Optional[_ConvertibleType] = ...,
                required: bool = ...,
                default: Optional[Any] = ...,
                callback: Optional[Callable[[Context, Parameter, str], Any]] = ...,
                nargs: Optional[int] = ...,
                metavar: Optional[str] = ...,
                expose_value: bool = ...,
                is_eager: bool = ...,
                envvar: Optional[Union[str, List[str]]] = ...
            ) -> None:
                ...

            @property
            def human_readable_name(self) -> str:
                ...

            def make_metavar(self) -> str:
                ...

            def get_default(self, ctx: Context) -> Any:
                ...

            def add_to_parser(self, parser: OptionParser, ctx: Context) -> None:
                ...

            def consume_value(self, ctx: Context, opts: Dict[str, Any]) -> Any:
                ...

            def type_cast_value(self, ctx: Context, value: Any) -> Any:
                ...

            def process_value(self, ctx: Context, value: Any) -> Any:
                ...

            def value_is_missing(self, value: Any) -> bool:
                ...

            def full_process_value(self, ctx: Context, value: Any) -> Any:
                ...

            def resolve_envvar_value(self, ctx: Context) -> str:
                ...

            def value_from_envvar(self, ctx: Context) -> Union[str, List[str]]:
                ...

            def handle_parse_result(
                self, ctx: Context, opts: Dict[str, Any], args: List[str]
            ) -> Tuple[Any, List[str]]:
                ...

            def get_help_record(self, ctx: Context) -> Tuple[str, str]:
                ...

            def get_usage_pieces(self, ctx: Context) -> List[str]:
                ...

            def get_error_hint(self, ctx: Context) -> str:
                ...


        class Option(Parameter):
            prompt: str  # sic
            confirmation_prompt: bool
            hide_input: bool
            is_flag: bool
            flag_value: Any
            is_bool_flag: bool
            count: bool
            multiple: bool
            allow_from_autoenv: bool
            help: Optional[str]
            hidden: bool
            show_default: bool
            show_choices: bool
            show_envvar: bool

            def __init__(
                self,
                param_decls: Optional[List[str]] = ...,
                show_default: bool = ...,
                prompt: Union[bool, str] = ...,
                confirmation_prompt: bool = ...,
                hide_input: bool = ...,
                is_flag: Optional[bool] = ...,
                flag_value: Optional[Any] = ...,
                multiple: bool = ...,
                count: bool = ...,
                allow_from_autoenv: bool = ...,
                type: Optional[_ConvertibleType] = ...,
                help: Optional[str] = ...,
                hidden: bool = ...,
                show_choices: bool = ...,
                show_envvar: bool = ...,
                **attrs
            ) -> None:
                ...

            def prompt_for_value(self, ctx: Context) -> Any:
                ...


        class Argument(Parameter):
            def __init__(
                self,
                param_decls: Optional[List[str]] = ...,
                required: Optional[bool] = ...,
                **attrs
            ) -> None:
                ...
      |}
    );
    ( "click/decorators.pyi",
      {|
        from distutils.version import Version
        from typing import Any, Callable, Dict, List, Optional, Tuple, Type, TypeVar, Union, Text, overload

        from click.core import Command, Group, Argument, Option, Parameter, Context, _ConvertibleType

        _T = TypeVar('_T')
        _F = TypeVar('_F', bound=Callable[..., Any])

        # Until https://github.com/python/mypy/issues/3924 is fixed you can't do the following:
        # _Decorator = Callable[[_F], _F]

        _Callback = Callable[
            [Context, Union[Option, Parameter], Any],
            Any
        ]

        def pass_context(_T) -> _T:
            ...


        def pass_obj(_T) -> _T:
            ...


        def make_pass_decorator(
            object_type: type, ensure: bool = ...
        ) -> Callable[[_T], _T]:
            ...


        # NOTE: Decorators below have **attrs converted to concrete constructor
        # arguments from core.pyi to help with type checking.

        def command(
            name: Optional[str] = ...,
            cls: Optional[Type[Command]] = ...,
            # Command
            context_settings: Optional[Dict[Any, Any]] = ...,
            help: Optional[str] = ...,
            epilog: Optional[str] = ...,
            short_help: Optional[str] = ...,
            options_metavar: str = ...,
            add_help_option: bool = ...,
            hidden: bool = ...,
            deprecated: bool = ...,
        ) -> Callable[[Callable[..., Any]], Command]: ...

        # This inherits attrs from Group, MultiCommand and Command.

        def group(
            name: Optional[str] = ...,
            cls: Type[Command] = ...,
            # Group
            commands: Optional[Dict[str, Command]] = ...,
            # MultiCommand
            invoke_without_command: bool = ...,
            no_args_is_help: Optional[bool] = ...,
            subcommand_metavar: Optional[str] = ...,
            chain: bool = ...,
            result_callback: Optional[Callable[..., Any]] = ...,
            # Command
            help: Optional[str] = ...,
            epilog: Optional[str] = ...,
            short_help: Optional[str] = ...,
            options_metavar: str = ...,
            add_help_option: bool = ...,
            hidden: bool = ...,
            deprecated: bool = ...,
            # User-defined
            **kwargs: Any,
        ) -> Callable[[Callable[..., Any]], Group]: ...

        def argument(
            *param_decls: str,
            cls: Type[Argument] = ...,
            # Argument
            required: Optional[bool] = ...,
            # Parameter
            type: Optional[_ConvertibleType] = ...,
            default: Optional[Any] = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...,
            autocompletion: Optional[Callable[[Any, List[str], str], List[Union[str, Tuple[str, str]]]]] = ...,
        ) -> Callable[[_F], _F]:
            ...


        @overload
        def option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: Optional[bool] = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Optional[_ConvertibleType] = ...,
            help: Optional[str] = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            required: bool = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...,
            # User-defined
            **kwargs: Any,
        ) -> Callable[[_F], _F]:
            ...


        @overload
        def option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: Optional[bool] = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: _T = ...,
            help: Optional[str] = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            required: bool = ...,
            callback: Optional[Callable[[Context, Union[Option, Parameter], Union[bool, int, str]], _T]] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...,
            # User-defined
            **kwargs: Any,
        ) -> Callable[[_F], _F]:
            ...


        @overload
        def option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: Optional[bool] = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Type[str] = ...,
            help: Optional[str] = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            required: bool = ...,
            callback: Callable[[Context, Union[Option, Parameter], str], Any] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...,
            # User-defined
            **kwargs: Any,
        ) -> Callable[[_F], _F]:
            ...


        @overload
        def option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: Optional[bool] = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Type[int] = ...,
            help: Optional[str] = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            required: bool = ...,
            callback: Callable[[Context, Union[Option, Parameter], int], Any] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...,
            # User-defined
            **kwargs: Any,
        ) -> Callable[[_F], _F]:
            ...


        def confirmation_option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: bool = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Optional[_ConvertibleType] = ...,
            help: str = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...
        ) -> Callable[[_F], _F]:
            ...


        def password_option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: Optional[bool] = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Optional[_ConvertibleType] = ...,
            help: Optional[str] = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...
        ) -> Callable[[_F], _F]:
            ...


        def version_option(
            version: Optional[Union[str, Version]] = ...,
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            prog_name: Optional[str] = ...,
            message: Optional[str] = ...,
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: bool = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Optional[_ConvertibleType] = ...,
            help: str = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...
        ) -> Callable[[_F], _F]:
            ...


        def help_option(
            *param_decls: str,
            cls: Type[Option] = ...,
            # Option
            show_default: bool = ...,
            prompt: Union[bool, Text] = ...,
            confirmation_prompt: bool = ...,
            hide_input: bool = ...,
            is_flag: bool = ...,
            flag_value: Optional[Any] = ...,
            multiple: bool = ...,
            count: bool = ...,
            allow_from_autoenv: bool = ...,
            type: Optional[_ConvertibleType] = ...,
            help: str = ...,
            show_choices: bool = ...,
            # Parameter
            default: Optional[Any] = ...,
            callback: Optional[_Callback] = ...,
            nargs: Optional[int] = ...,
            metavar: Optional[str] = ...,
            expose_value: bool = ...,
            is_eager: bool = ...,
            envvar: Optional[Union[str, List[str]]] = ...
        ) -> Callable[[_F], _F]:
            ...
      |}
    );
  ]
  @ sqlalchemy_stubs
  @ torch_stubs
  @ readonly_stubs
  @ django_stubs ()
  @ pysa_stubs ()
  @ pytest_stubs ()
  @ if include_pyre_extensions then pyre_extensions_stubs () else []


let mock_signature =
  {
    Define.Signature.name = Reference.create "$empty";
    parameters = [];
    decorators = [];
    return_annotation = None;
    async = false;
    generator = false;
    parent = NestingContext.create_toplevel ();
    legacy_parent = None;
    type_params = [];
  }


let mock_define =
  { Define.signature = mock_signature; captures = []; unbound_names = []; body = [] }


let create_type_alias_table type_aliases =
  let aliases ?replace_unbound_parameters_with_any:_ primitive =
    type_aliases primitive >>| fun alias -> TypeAliasEnvironment.RawAlias.TypeAlias alias
  in
  aliases


let mock_scheduler () = Scheduler.create_sequential ()

module ScratchProject = struct
  type t = {
    context: OUnit2.test_ctxt;
    controls: EnvironmentControls.t;
    errors_environment: ErrorsEnvironment.t;
  }

  module BuiltTypeEnvironment = struct
    type t = {
      sources: Source.t list;
      type_environment: TypeEnvironment.ReadOnly.t;
    }
  end

  module BuiltGlobalEnvironment = struct
    type t = {
      sources: Source.t list;
      global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
    }
  end

  let module_path_for_in_memory_scratch_project ~configuration ~relative ~should_type_check =
    let raw =
      let { Configuration.Analysis.local_root; _ } = configuration in
      let path_in_local_root = PyrePath.create_relative ~root:local_root ~relative in
      ArtifactPaths.raw_module_path_of_artifact_path
        ~configuration
        (ArtifactPath.create path_in_local_root)
      |> Option.value_exn
    in
    ModulePath.create ~should_type_check raw


  let setup
      ?(track_dependencies = true)
      ~context
      ?(external_sources = [])
      ?(show_error_traces = false)
      ?(include_typeshed_stubs = true)
      ?(include_helper_builtins = true)
      ?(include_pyre_extensions = true)
      ?(in_memory = true)
      ?(populate_call_graph = false)
      ?(use_lazy_module_tracking = false)
      ?(no_validation_on_class_lookup_failure = false)
      ?python_version
      ?system_platform
      ?debug
      ?strict
      ?enable_readonly_analysis
      ?enable_strict_override_check
      ?enable_strict_any_check
      ?enable_unawaited_awaitable_analysis
      ?include_suppressed_errors
      sources
    =
    let local_root, external_root, log_directory =
      if in_memory then
        (* NOTE: any attempt to access disk in an in-memory test will fail. This means nothing that
           tries to log or save to disk can be tested using in-memory mode. *)
        let in_memory_root = "_pyre_no_such_directory_for_in_memory_test" in
        let local_root = PyrePath.create_absolute (in_memory_root ^/ "local_root") in
        let external_root = PyrePath.create_absolute (in_memory_root ^/ "external_root") in
        let log_directory = in_memory_root ^/ "log_directory" in
        local_root, external_root, log_directory
      else (* For simplicity, assume just one checked directory and one external directory *)
        let local_root = bracket_tmpdir context |> PyrePath.create_absolute in
        let external_root = bracket_tmpdir context |> PyrePath.create_absolute in
        let log_directory = bracket_tmpdir context in
        local_root, external_root, log_directory
    in
    let external_sources =
      if include_typeshed_stubs then
        typeshed_stubs ~include_helper_builtins ~include_pyre_extensions () @ external_sources
      else
        external_sources
    in
    let controls =
      let default_python_version =
        { Configuration.PythonVersion.major = 3; minor = 12; micro = 2 }
      in
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~source_paths:[SearchPath.Root local_root]
          ~search_paths:[SearchPath.Root external_root]
          ~log_directory
          ~filter_directories:[local_root]
          ~ignore_all_errors:[external_root]
          ~track_dependencies
          ~show_error_traces
          ~parallel:false
          ~python_version:(Option.value python_version ~default:default_python_version)
          ~use_pyrefly_results:false
          ?system_platform
          ?strict
          ?debug
          ?enable_readonly_analysis
          ?enable_strict_override_check
          ?enable_strict_any_check
          ?enable_unawaited_awaitable_analysis
          ?include_suppressed_errors
          ()
      in
      if in_memory then
        let in_memory_sources =
          let to_in_memory_source (relative, content) ~should_type_check =
            let code = trim_extra_indentation content in
            let module_path =
              module_path_for_in_memory_scratch_project ~configuration ~relative ~should_type_check
            in
            module_path, code
          in
          List.map sources ~f:(to_in_memory_source ~should_type_check:true)
          @ List.map external_sources ~f:(to_in_memory_source ~should_type_check:false)
        in
        EnvironmentControls.create
          ~populate_call_graph
          ~in_memory_sources
          ~no_validation_on_class_lookup_failure
          configuration
      else
        let add_source ~root (relative, content) =
          let content = trim_extra_indentation content in
          let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
          File.write file
        in
        let () = List.iter sources ~f:(add_source ~root:local_root) in
        let () = List.iter external_sources ~f:(add_source ~root:external_root) in
        EnvironmentControls.create
          ~populate_call_graph
          ~use_lazy_module_tracking
          ~no_validation_on_class_lookup_failure
          configuration
    in
    let errors_environment = ErrorsEnvironment.create_with_ast_environment controls in
    let () =
      if not use_lazy_module_tracking then (
        (* Use AstEnvironment push updates to wipe shared memory. This isn't needed for correctness,
           but it cuts down on RAM use. Skip it if we are testing the lazy module tracker; we don't
           need to clean up in that case, and the cleanup relies on APIs that are only available
           using nonlazy module tracking *)
        let ast_environment =
          ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment errors_environment
          |> TypeEnvironment.AssumeAstEnvironment.ast_environment
        in
        AstEnvironment.clear_memory_for_tests ~scheduler:(mock_scheduler ()) ast_environment;
        let set_up_shared_memory _ = () in
        let tear_down_shared_memory () _ = Memory.reset_shared_memory () in
        (* Clean shared memory up after the test *)
        OUnit2.bracket set_up_shared_memory tear_down_shared_memory context)
    in
    { context; controls; errors_environment }


  let configuration_of { controls; _ } = EnvironmentControls.configuration controls

  let local_root_of project =
    let { Configuration.Analysis.local_root; _ } = configuration_of project in
    local_root


  let external_root_of project =
    let { Configuration.Analysis.search_paths; _ } = configuration_of project in
    match search_paths with
    | SearchPath.Root root :: _ -> root
    | _ ->
        failwith "Scratch projects should have the external root at the start of their search path."


  let add_to_root content ~root ~relative =
    let content = trim_extra_indentation content in
    let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
    File.write file


  let delete_from_root ~root ~relative =
    PyrePath.create_relative ~root ~relative |> PyrePath.absolute |> CamlUnix.unlink


  let add_to_local_root project content ~relative =
    let root = local_root_of project in
    add_to_root content ~root ~relative


  let delete_from_local_root project ~relative =
    let root = local_root_of project in
    delete_from_root ~root ~relative


  let add_to_external_root project content ~relative =
    let root = external_root_of project in
    add_to_root content ~root ~relative


  let delete_from_external_root project ~relative =
    let root = external_root_of project in
    delete_from_root ~root ~relative


  module ReadWrite = struct
    let errors_environment { errors_environment; _ } = errors_environment

    let type_environment { errors_environment; _ } =
      errors_environment |> ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment


    (* The names of these hooks are specific because it is important that tests of layers above
       AstEnvironment shouldn't be trying to access the raw environment; we've designed our system
       to allow alternative implementations of the bottom layers, so in general it is incorrect to
       assume there is an underlying AstEnvironment or ModuleTracker. *)
    module AssumeBackedByAstEnvironment = struct
      let ast_environment { errors_environment; _ } =
        ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment errors_environment
        |> TypeEnvironment.AssumeAstEnvironment.ast_environment


      let module_tracker project = ast_environment project |> AstEnvironment.module_tracker
    end
  end

  let errors_environment { errors_environment; _ } =
    errors_environment |> ErrorsEnvironment.read_only


  let type_environment project =
    errors_environment project |> ErrorsEnvironment.ReadOnly.type_environment


  let get_untracked_source_code_api project =
    errors_environment project |> ErrorsEnvironment.ReadOnly.get_untracked_source_code_api


  let global_environment project =
    type_environment project |> TypeEnvironment.ReadOnly.global_environment


  let global_module_paths_api { errors_environment; _ } =
    ErrorsEnvironment.AssumeGlobalModuleListing.global_module_paths_api errors_environment


  let type_check_qualifiers project =
    global_module_paths_api project |> GlobalModulePathsApi.type_check_qualifiers


  let get_project_sources project =
    let source_code_api = get_untracked_source_code_api project in
    type_check_qualifiers project
    |> List.filter_map ~f:(SourceCodeApi.source_of_qualifier source_code_api)


  let build_global_environment project =
    let global_environment = global_environment project in
    let sources = get_project_sources project in
    { BuiltGlobalEnvironment.sources; global_environment }


  let populate_type_environment project =
    let type_environment = ReadWrite.type_environment project in
    type_check_qualifiers project
    |> TypeEnvironment.populate_for_modules
         ~scheduler:(Scheduler.create_sequential ())
         ~scheduler_policies:Configuration.SchedulerPolicies.empty
         type_environment


  let pyre_pysa_read_only_api project =
    (* Some of the PyrePysa api logic uses global queries that assume we've pre-populated symbol
       tables. *)
    populate_type_environment project;
    PyrePysaEnvironment.ReadOnly.create
      ~type_environment:(type_environment project)
      ~global_module_paths_api:(global_module_paths_api project)


  let build_type_environment project =
    populate_type_environment project;
    let sources = get_project_sources project in
    let type_environment = ReadWrite.type_environment project |> TypeEnvironment.read_only in
    { BuiltTypeEnvironment.sources; type_environment }


  let build_type_environment_and_postprocess project =
    let built_type_environment = build_type_environment project in
    let errors =
      List.map
        built_type_environment.sources
        ~f:(fun { Source.module_path = { ModulePath.qualifier; _ }; _ } -> qualifier)
      |> Postprocessing.run
           ~scheduler:(Scheduler.create_sequential ())
           ~environment:built_type_environment.type_environment
    in
    built_type_environment, errors


  let build_global_resolution project =
    let { BuiltGlobalEnvironment.global_environment; _ } = build_global_environment project in
    GlobalResolution.create global_environment


  let build_resolution project =
    let global_resolution = build_global_resolution project in
    TypeCheck.resolution
      global_resolution (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)


  let get_all_errors project =
    global_module_paths_api project
    |> GlobalModulePathsApi.type_check_qualifiers
    |> ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers (errors_environment project)


  let update_environment { errors_environment; _ } ?(scheduler = mock_scheduler ()) artifact_paths =
    ErrorsEnvironment.update_this_and_all_preceding_environments
      errors_environment
      ~scheduler
      artifact_paths
end

module ScratchPyreflyProject = struct
  type t = {
    api: Interprocedural.PyreflyApi.ReadWrite.t;
    configuration: Configuration.Analysis.t;
  }

  let find_pyrefly_binary () = Stdlib.Sys.getenv_opt "PYREFLY_BINARY"

  let setup ~context ~pyrefly_binary ~requires_type_of_expressions ?(external_sources = []) sources =
    let local_root = bracket_tmpdir context |> PyrePath.create_absolute in
    let external_root = bracket_tmpdir context |> PyrePath.create_absolute in
    let add_source ~root (relative, content) =
      let content = trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root ~relative) in
      File.write file
    in
    let external_sources =
      django_stubs ()
      @ pysa_stubs ()
      @ pytest_stubs ()
      @ pyre_extensions_stubs ()
      @ external_sources
    in
    let () = List.iter sources ~f:(add_source ~root:local_root) in
    let () = List.iter external_sources ~f:(add_source ~root:external_root) in
    let () =
      File.write
        (File.create
           ~content:"\n"
           (PyrePath.create_relative ~root:local_root ~relative:"pyrefly.toml"))
    in
    let result_directory = bracket_tmpdir context |> PyrePath.create_absolute in
    let python_version = Configuration.PythonVersion.create () in
    let arguments =
      [
        "check";
        "--threads=1";
        "--verbose";
        Format.sprintf
          "--python-version=%d.%d.%d"
          python_version.major
          python_version.minor
          python_version.micro;
        "--search-path";
        PyrePath.absolute external_root;
        "--report-pysa";
        PyrePath.absolute result_directory;
        PyrePath.absolute local_root;
      ]
    in
    Log.info "Running command: %s" (Stdlib.Filename.quote_command pyrefly_binary arguments);
    let stdout_channel, stdin_channel, stderr_channel =
      CamlUnix.open_process_args_full
        pyrefly_binary
        (Array.of_list ("pyrefly" :: arguments))
        (CamlUnix.environment ())
    in
    let () = Out_channel.close stdin_channel in
    let stdout_content = In_channel.input_all stdout_channel in
    let stderr_content = In_channel.input_all stderr_channel in
    let () = In_channel.close stdout_channel in
    let () = In_channel.close stderr_channel in
    let configuration =
      Configuration.Analysis.create
        ~parallel:false
        ~source_paths:[]
        ~search_paths:[]
        ~python_version
        ~use_pyrefly_results:true
        ()
    in
    let api =
      try
        Interprocedural.PyreflyApi.ReadWrite.create_from_directory
          ~scheduler:(Scheduler.create_sequential ())
          ~scheduler_policies:Configuration.SchedulerPolicies.empty
          ~configuration
          result_directory
      with
      | Interprocedural.PyreflyApi.PyreflyFileFormatError { path; error } ->
          Log.dump "Pyrefly stdout: %s" stdout_content;
          Log.dump "Pyrefly stderr: %s" stderr_content;
          failwith
            (Format.asprintf "%a: %a" PyrePath.pp path Interprocedural.PyreflyApi.Error.pp error)
    in
    let api =
      if requires_type_of_expressions then
        Interprocedural.PyreflyApi.ReadWrite.parse_type_of_expressions
          api
          ~scheduler:(Scheduler.create_sequential ())
          ~scheduler_policies:Configuration.SchedulerPolicies.empty
      else
        api
    in
    let () =
      (* Clean shared memory up after the test *)
      let set_up_shared_memory _ = () in
      let tear_down_shared_memory () _ = Memory.reset_shared_memory () in
      OUnit2.bracket set_up_shared_memory tear_down_shared_memory context
    in
    { api; configuration }


  let pyre_pysa_read_only_api { api; _ } = Interprocedural.PyreflyApi.ReadOnly.of_read_write_api api

  let configuration_of { configuration; _ } = configuration
end

module ScratchPyrePysaProject : sig
  type t

  val setup
    :  context:OUnitTest.ctxt ->
    requires_type_of_expressions:bool ->
    ?use_cache:bool ->
    ?force_pyre1:bool ->
    ?external_sources:(string * string) list ->
    ?decorator_preprocessing_configuration:PyrePysaLogic.DecoratorPreprocessing.Configuration.t ->
    (string * string) list ->
    t

  val errors : t -> AnalysisError.Instantiated.t list

  val read_only_api : t -> Interprocedural.PyrePysaApi.ReadOnly.t

  val configuration_of : t -> Configuration.Analysis.t
end = struct
  type t =
    | Pyre1 of {
        project: ScratchProject.t;
        pyre_api: Analysis.PyrePysaEnvironment.ReadOnly.t;
        errors: Analysis.AnalysisError.t list;
      }
    | Pyrefly of {
        project: ScratchPyreflyProject.t;
        pyrefly_api: Interprocedural.PyreflyApi.ReadOnly.t;
      }

  module ProjectInputs = struct
    module T = struct
      type t = {
        force_pyre1: bool;
        requires_type_of_expressions: bool;
        decorator_preprocessing_configuration:
          PyrePysaLogic.DecoratorPreprocessing.Configuration.t option;
        external_sources: string String.Map.t;
        sources: string String.Map.t;
      }
      [@@deriving compare, equal, sexp]
    end

    include T
    module Map = Map.Make (T)
  end

  type project = t

  module ProjectCache : sig
    type t

    val create : unit -> t

    val try_load : t -> ProjectInputs.t -> project option

    val save : t -> inputs:ProjectInputs.t -> project:project -> unit
  end = struct
    type cache_value = {
      file_path: PyrePath.t;
      project: project;
    }

    (* Maps a ProjectInputs to a file containing the shared memory state, and its associated
       project. *)
    type t = cache_value ProjectInputs.Map.t ref

    let create () = ref ProjectInputs.Map.empty

    let try_load cache inputs =
      match Map.find !cache inputs with
      | Some { file_path; project } ->
          let timer = Timer.start () in
          Memory.initialize_for_tests ();
          Hack_parallel.Std.SharedMemory.load_table (PyrePath.absolute file_path);
          (* Note: we don't save/load the dependency table, this could be a problem for some
             tests. *)
          Log.debug "Loaded project from shared memory cache in %.3fs" (Timer.stop_in_sec timer);
          Some project
      | None -> None


    let save cache ~inputs ~project =
      let timer = Timer.start () in
      let file_path, channel = Stdlib.Filename.open_temp_file "ounit-pysa-project-" ".shm" in
      Hack_parallel.Std.SharedMemory.collect `aggressive;
      Hack_parallel.Std.SharedMemory.save_table file_path;
      cache :=
        Map.add_exn
          !cache
          ~key:inputs
          ~data:{ file_path = PyrePath.create_absolute file_path; project };
      Log.debug "Saved project to shared memory cache in %.3fs" (Timer.stop_in_sec timer);
      (* Remove the file at the end of the tests. We can't use bracket_tmpfile because the cache
         needs to be preserved between brackets. *)
      let () =
        at_exit (fun () ->
            (try Stdlib.close_out channel with
            | _ -> ());
            try Stdlib.Sys.remove file_path with
            | _ -> ())
      in
      ()
  end

  let global_cache = ProjectCache.create ()

  let pyrefly_binary =
    lazy
      (match ScratchPyreflyProject.find_pyrefly_binary () with
      | Some path ->
          let () = Log.dump "Found PYREFLY_BINARY=`%s`, running tests using pyrefly" path in
          Some path
      | None -> None)


  let setup_without_cache
      ~context
      {
        ProjectInputs.force_pyre1;
        requires_type_of_expressions;
        decorator_preprocessing_configuration;
        external_sources;
        sources;
      }
    =
    let timer = Timer.start () in
    let external_sources = Map.to_alist external_sources in
    let sources = Map.to_alist sources in
    let () =
      match decorator_preprocessing_configuration with
      | Some configuration -> PyrePysaLogic.DecoratorPreprocessing.setup_preprocessing configuration
      | None -> ()
    in
    let result =
      match Lazy.force pyrefly_binary with
      | Some pyrefly_binary when not force_pyre1 ->
          let project =
            ScratchPyreflyProject.setup
              ~context
              ~pyrefly_binary
              ~requires_type_of_expressions
              ~external_sources
              sources
          in
          let pyrefly_api = ScratchPyreflyProject.pyre_pysa_read_only_api project in
          Pyrefly { project; pyrefly_api }
      | _ ->
          let project = ScratchProject.setup ~context ~external_sources sources in
          let _, errors = ScratchProject.build_type_environment_and_postprocess project in
          let pyre_api = ScratchProject.pyre_pysa_read_only_api project in
          Pyre1 { project; pyre_api; errors }
    in
    Log.debug
      "Type checked project using %s in %.3fs"
      (match result with
      | Pyrefly _ -> "pyrefly"
      | Pyre1 _ -> "pyre1")
      (Timer.stop_in_sec timer);
    result


  let setup
      ~context
      ~requires_type_of_expressions
      ?(use_cache = true)
      ?(force_pyre1 = false)
      ?(external_sources = [])
      ?decorator_preprocessing_configuration
      sources
    =
    let inputs =
      {
        ProjectInputs.force_pyre1;
        requires_type_of_expressions;
        decorator_preprocessing_configuration;
        external_sources =
          external_sources |> String.Map.of_alist_exn |> String.Map.map ~f:trim_extra_indentation;
        sources = sources |> String.Map.of_alist_exn |> String.Map.map ~f:trim_extra_indentation;
      }
    in
    if not use_cache then
      setup_without_cache ~context inputs
    else
      match ProjectCache.try_load global_cache inputs with
      | Some project -> project
      | None ->
          let project = setup_without_cache ~context inputs in
          let () = ProjectCache.save global_cache ~inputs ~project in
          project


  let read_only_api = function
    | Pyre1 { pyre_api; _ } -> Interprocedural.PyrePysaApi.ReadOnly.Pyre1 pyre_api
    | Pyrefly { pyrefly_api; _ } -> Interprocedural.PyrePysaApi.ReadOnly.Pyrefly pyrefly_api


  let errors = function
    | Pyre1 { pyre_api; errors; _ } ->
        let instantiate =
          PyrePysaLogic.Testing.AnalysisError.instantiate
            ~show_error_traces:false
            ~lookup:(Analysis.PyrePysaEnvironment.ReadOnly.relative_path_of_qualifier pyre_api)
        in
        List.map ~f:instantiate errors
    | Pyrefly { pyrefly_api; _ } ->
        Interprocedural.PyreflyApi.ReadOnly.parse_type_errors pyrefly_api


  let configuration_of = function
    | Pyre1 { project; _ } -> ScratchProject.configuration_of project
    | Pyrefly { project; _ } -> ScratchPyreflyProject.configuration_of project
end

type test_other_sources_t = {
  handle: string;
  source: string;
}
[@@deriving compare, show]

let assert_errors
    ?(debug = true)
    ?(strict = false)
    ?(show_error_traces = false)
    ?(concise = false)
    ?(handle = "test.py")
    ?(other_sources = [])
    ?(include_line_numbers = false)
    ?python_version
    ?system_platform
    ?enable_readonly_analysis
    ?enable_strict_override_check
    ?enable_strict_any_check
    ?enable_unawaited_awaitable_analysis
    ?include_suppressed_errors
    ?include_pyre_extensions
    ~check
    source
    errors
    context
  =
  let in_memory = List.is_empty other_sources in
  (if ModulePath.qualifier_from_relative_path handle |> Reference.is_empty then
     let message =
       Format.sprintf
         "Cannot use %s as test file name: Empty qualifier in test is no longer acceptable."
         handle
     in
     failwith message);

  let descriptions =
    let errors =
      let sources, type_environment, source_code_api =
        let project =
          let external_sources =
            List.map other_sources ~f:(fun { handle; source } -> handle, source)
          in
          ScratchProject.setup
            ~context
            ~external_sources
            ~in_memory
            ~strict
            ~debug
            ?python_version
            ?system_platform
            ?enable_readonly_analysis
            ?enable_strict_override_check
            ?enable_strict_any_check
            ?enable_unawaited_awaitable_analysis
            ?include_suppressed_errors
            ?include_pyre_extensions
            [handle, source]
        in
        let { ScratchProject.BuiltGlobalEnvironment.sources; _ } =
          ScratchProject.build_global_environment project
        in
        let errors_environment = ScratchProject.ReadWrite.errors_environment project in
        ( sources,
          ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment errors_environment,
          ErrorsEnvironment.read_only errors_environment
          |> ErrorsEnvironment.ReadOnly.get_untracked_source_code_api )
      in
      let source =
        List.find_exn sources ~f:(fun { Source.module_path; _ } ->
            String.equal handle (ModulePath.relative module_path))
      in
      check ~environment:type_environment ~source
      |> List.map
           ~f:
             (AnalysisError.instantiate
                ~show_error_traces
                ~lookup:(SourceCodeApi.relative_path_of_qualifier source_code_api))
    in
    let errors_with_any_location =
      List.filter_map errors ~f:(fun error ->
          let location = AnalysisError.Instantiated.location error in
          Option.some_if
            ([%compare.equal: Location.WithPath.t] location Location.WithPath.any)
            location)
    in
    let show_description ~concise error =
      if concise then
        AnalysisError.Instantiated.concise_description error
      else
        AnalysisError.Instantiated.description error
    in
    let found_any = not (List.is_empty errors_with_any_location) in
    (if found_any then
       let errors = List.map ~f:(show_description ~concise) errors |> String.concat ~sep:"\n" in
       Format.sprintf "\nLocation.any cannot be attached to errors: %s\n" errors |> ignore);
    assert_false found_any;
    let to_string error =
      let description = show_description ~concise error in
      if include_line_numbers then
        let line = AnalysisError.Instantiated.location error |> Location.WithPath.line in
        Format.sprintf "%d: %s" line description
      else
        description
    in
    List.map ~f:to_string errors
  in
  Memory.reset_shared_memory ();
  OUnit2.assert_equal
    ~cmp:(List.equal String.equal)
    ~printer:(String.concat ~sep:"\n")
    errors
    descriptions


let assert_instantiated_attribute_equal expected actual =
  let pp_as_sexps format l =
    List.map l ~f:AnnotatedAttribute.sexp_of_instantiated
    |> List.map ~f:Sexp.to_string_hum
    |> String.concat ~sep:"\n"
    |> Format.fprintf format "%s\n"
  in
  let simple_print l =
    let simple attribute =
      let annotation = AnnotatedAttribute.annotation attribute |> TypeInfo.Unit.annotation in
      let name = AnnotatedAttribute.name attribute in
      Printf.sprintf "%s, %s" name (Type.show annotation)
    in
    List.map l ~f:simple |> String.concat ~sep:"\n"
  in
  OUnit2.assert_equal
    ~cmp:[%compare.equal: AnnotatedAttribute.instantiated list]
    ~printer:simple_print
    ~pp_diff:(diff ~print:pp_as_sexps)
    expected
    actual


(* Assert that the class [class_name] in [source], after all transformations, has attributes
   equivalent to the class [class_name] in [expected_equivalent_class_source].

   This is useful when Pyre adds, removes, or modifies the original class's attributes, e.g., by
   adding dunder methods. *)
let assert_equivalent_attributes
    ?(assert_attribute_equal = assert_instantiated_attribute_equal)
    ?external_sources
    ?python_version
    ?system_platform
    ~source
    ~class_name
    expected_equivalent_class_source
    context
  =
  let module_name = "test" in
  let attributes source =
    Memory.reset_shared_memory ();
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup
        ~context
        ?external_sources
        ?python_version
        ?system_platform
        [Format.asprintf "%s.py" module_name, source]
      |> ScratchProject.build_global_environment
    in
    let global_resolution = GlobalResolution.create global_environment in
    let compare_by_name left right =
      String.compare (AnnotatedAttribute.name left) (AnnotatedAttribute.name right)
    in
    Format.asprintf "%s.%s" module_name class_name
    |> GlobalResolution.uninstantiated_attributes global_resolution ~transitive:false
    |> (fun attributes ->
         Option.value_exn
           ~message:(Format.asprintf "Expected to find class `%s` in `%s`" class_name source)
           attributes)
    |> List.sort ~compare:compare_by_name
    |> List.map
         ~f:
           (GlobalResolution.instantiate_attribute
              global_resolution
              ~accessed_through_class:false
              ~accessed_through_readonly:false)
  in
  assert_attribute_equal (attributes expected_equivalent_class_source) (attributes source)


module MockClassHierarchyHandler = struct
  type t = {
    edges: ClassHierarchy.Edges.t Identifier.Table.t;
    all_class_names: Identifier.Hash_set.t;
  }

  let create () =
    { edges = Identifier.Table.create (); all_class_names = Identifier.Hash_set.create () }


  let copy { edges; all_class_names } =
    { edges = Hashtbl.copy edges; all_class_names = Hash_set.copy all_class_names }


  let pp format { edges; _ } =
    let print_edge (source, { ClassHierarchy.Edges.parents; _ }) =
      let targets =
        let target { ClassHierarchy.Target.target; arguments } =
          Format.asprintf "%s [%a]" target Type.Argument.pp_list arguments
        in
        List.map parents ~f:target |> String.concat ~sep:", "
      in
      Format.fprintf format "  %s -> %s\n" source targets
    in
    Format.fprintf format "Edges:\n";
    List.iter ~f:print_edge (Hashtbl.to_alist edges)


  let show order = Format.asprintf "%a" pp order

  let set table ~key ~data = Hashtbl.set table ~key ~data

  let handler order =
    (module struct
      let edges = Hashtbl.find order.edges

      let contains annotation = Hash_set.mem order.all_class_names annotation
    end : ClassHierarchy.Handler)


  (* A special variation of `connect` for the handful of test cases that need non-invariant type
     variables. It can only be used to connect to a generic base class (Generic or Protocol). *)
  let connect_with_variance order ~arguments_with_variances ~predecessor ~successor =
    let edges = order.edges in
    (* Compute the new parent information and possible updated generic metadata *)
    let new_target =
      {
        ClassHierarchy.Target.target = successor;
        arguments = List.map arguments_with_variances ~f:(fun (argument, _) -> argument);
      }
    in
    let generic_metadata_from_this_successor =
      let convert_variable_to_parameter_invariantly (variable, variance) =
        match variable with
        | Type.Variable.TypeVarVariable { Type.Variable.TypeVar.name; constraints; _ } ->
            Type.GenericParameter.GpTypeVar { name; constraints; variance }
        | Type.Variable.TypeVarTupleVariable _ as variable ->
            Type.GenericParameter.GpTypeVarTuple { name = Type.Variable.name variable }
        | Type.Variable.ParamSpecVariable _ as variable ->
            Type.GenericParameter.GpParamSpec { name = Type.Variable.name variable }
      in
      match successor with
      | "typing.Generic"
      | "typing.Protocol" -> (
          match
            List.map arguments_with_variances ~f:(fun (argument, variance) ->
                match Type.Argument.to_variable argument with
                | Some variable -> Some (variable, variance)
                | None -> None)
            |> Option.all
          with
          | Some variables_and_variances ->
              ClassHierarchy.GenericMetadata.GenericBase
                (List.map ~f:convert_variable_to_parameter_invariantly variables_and_variances)
          | None -> failwith "The connect_with_variance helper may not be used on empty arguments")
      | _ -> ClassHierarchy.GenericMetadata.NotGeneric
    in
    (* Fold the new information into what is already there. Note that this operation is messy
       because we're taking an API that is actually functional and computed all at once and creating
       a testing DSL that is mutation-based. *)
    let predecessor_edges =
      match Hashtbl.find edges predecessor with
      | None ->
          {
            ClassHierarchy.Edges.parents = [new_target];
            generic_metadata = generic_metadata_from_this_successor;
          }
      | Some { ClassHierarchy.Edges.parents = old_parents; generic_metadata = old_generic_metadata }
        ->
          let parents = new_target :: old_parents in
          let generic_metadata =
            match generic_metadata_from_this_successor with
            | ClassHierarchy.GenericMetadata.NotGeneric -> old_generic_metadata
            | _ -> generic_metadata_from_this_successor
          in
          { parents; generic_metadata }
    in
    Hashtbl.set edges ~key:predecessor ~data:predecessor_edges


  let connect ?(arguments = []) order ~predecessor ~successor =
    let arguments_with_variances =
      List.map arguments ~f:(fun argument -> argument, Type.Record.PreInferenceVariance.P_Invariant)
    in
    connect_with_variance ~arguments_with_variances order ~predecessor ~successor


  let insert order annotation =
    Hash_set.add order.all_class_names annotation;
    Hashtbl.set
      order.edges
      ~key:annotation
      ~data:
        {
          ClassHierarchy.Edges.parents = [];
          generic_metadata = ClassHierarchy.GenericMetadata.NotGeneric;
        }
end

(* Expression helpers. *)
let ( ~+ ) value = Node.create_with_default_location value

let ( ~- ) value = Expression.Origin.create ~location:Location.any value

let ( ! ) name =
  +Expression.Expression.Name
     (Expression.create_name ~location:Location.any ~create_origin:(fun _ -> None) name)


let ( !! ) name =
  +Statement.Expression
     (+Expression.Expression.Name
         (Expression.create_name ~location:Location.any ~create_origin:(fun _ -> None) name))


let ( !& ) name = Reference.create name
