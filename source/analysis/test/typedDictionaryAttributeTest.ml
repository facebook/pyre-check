(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open OUnit2
open Test

(* Assert that the class [expected_equivalent_class_source] has the same attribute types as the
   `TypedDict` [class_name].

   Pyre generates TypedDict methods based on the declared fields, often using overloads. The base
   signature of those methods usually has undefined parameters and returns Any. This is hard to
   express as a proper Python method in [expected_equivalent_class_source]. Likewise, the `self`
   parameter annotation is Top in these cases.

   So, to keep things simple, just get the attribute type and sanitize uninteresting bits. *)
let assert_equivalent_typed_dictionary_attribute_types
    ~source
    ~class_name
    expected_equivalent_class_source
    context
  =
  let with_sanitized_callable_parameters = function
    | Type.Parametric
        {
          name = "BoundMethod";
          arguments =
            [
              Single (Callable ({ kind = Named method_name; _ } as callable)); Single left_bound_type;
            ];
        } ->
        let open Type.Callable.CallableParamType in
        (* TypedDictionary methods have `self` annotation as `Top`, whereas the equivalent class has
           the annotation as, say, `Movie`. So, clear the annotation. *)
        let sanitize_self_annotation = function
          | Type.Callable.Defined
              (Named ({ name = "self" | "$parameter$self"; default = false; _ } as self_parameter)
              :: parameters) ->
              let all_parameters =
                Named { self_parameter with annotation = Type.Top } :: parameters
              in
              Type.Callable.Defined all_parameters
          | other -> other
        in
        let sanitize_parameter_names = function
          | Type.Callable.Defined all_parameters ->
              let sanitize_parameter_name = function
                | Named ({ name; _ } as parameter) ->
                    Named { parameter with name = Identifier.sanitized name }
                | other -> other
              in
              Type.Callable.Defined (List.map ~f:sanitize_parameter_name all_parameters)
          | other -> other
        in
        let callable =
          callable
          |> Type.Callable.map_parameters ~f:sanitize_self_annotation
          |> Type.Callable.map_parameters ~f:sanitize_parameter_names
        in
        Type.Parametric
          {
            name = "BoundMethod";
            arguments =
              [
                Single
                  (Callable
                     {
                       callable with
                       implementation = { annotation = Type.Top; parameters = Undefined };
                       kind =
                         Named
                           (Reference.create
                              ~prefix:!&"TypedDictionary"
                              (Reference.last method_name));
                     });
                Single left_bound_type;
              ];
          }
    | type_ -> type_
  in
  let with_sanitized_type_variables =
    Type.Variable.GlobalTransforms.TypeVar.replace_all (fun ({ name; _ } as unary_variable) ->
        Type.Variable { unary_variable with name = Reference.create name |> Reference.last }
        |> Option.some)
  in
  let transform_attribute_annotation attribute =
    attribute
    |> AnnotatedAttribute.annotation
    |> TypeInfo.Unit.annotation
    |> with_sanitized_callable_parameters
    |> with_sanitized_type_variables
  in
  let assert_attribute_equal expected actual =
    assert_equal
      ~cmp:[%compare.equal: Type.t list]
      ~printer:[%show: Type.t list]
      ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" ([%show: Type.t list] x)))
      (List.map expected ~f:transform_attribute_annotation)
      (List.map actual ~f:transform_attribute_annotation)
  in
  assert_equivalent_attributes
    ~assert_attribute_equal
    ~source
    ~class_name
    expected_equivalent_class_source
    context


let test_typed_dictionary_attributes =
  let assert_typed_dictionary_attributes
      ~equivalent_sources
      ~class_name
      expected_equivalent_class_source
      context
    =
    List.iter equivalent_sources ~f:(fun source ->
        assert_equivalent_typed_dictionary_attribute_types
          ~source
          ~class_name
          expected_equivalent_class_source
          context)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_typed_dictionary_attributes
           ~equivalent_sources:
             [
               {|
              from typing_extensions import TypedDict

              class Movie(TypedDict):
                name: str
                year: int
            |};
               {|
              from typing_extensions import TypedDict, Required

              class Movie(TypedDict):
                name: Required[str]
                year: Required[int]
            |};
               {|
              from typing_extensions import TypedDict, Required

              class Movie(TypedDict, total=False):
                name: Required[str]
                year: Required[int]
            |};
             ]
           ~class_name:"Movie"
           {|
                from typing import overload, Optional, TypeVar, Union, _PyreReadOnly_ as RO
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class Movie:
                  @overload
                  def __init__(self, *, name: str, year: int) -> None: ...
                  @overload
                  def __init__(self: Movie, movie: Movie, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  @overload
                  def __getitem__(self, k: L["name"]) -> str: ...
                  @overload
                  def __getitem__(self, k: L["year"]) -> int: ...
                  def __getitem__(self) -> DontCare: ...

                  @overload
                  def __setitem__(self, k: L["name"], v: str) -> None: ...
                  @overload
                  def __setitem__(self, k: L["year"], v: int) -> None: ...
                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: L["name"]) -> Optional[str]: ...
                  @overload
                  def get(self: RO[Movie], k: L["name"]) -> RO[Optional[str]]: ...
                  @overload
                  def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def get(self: RO[Movie], k: L["name"], default: _T) -> RO[Union[str, _T]]: ...
                  @overload
                  def get(self, k: L["year"]) -> Optional[int]: ...
                  @overload
                  def get(self: RO[Movie], k: L["year"]) -> RO[Optional[int]]: ...
                  @overload
                  def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  @overload
                  def get(self: RO[Movie], k: L["year"], default: _T) -> RO[Union[int, _T]]: ...
                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  @overload
                  def setdefault(self, k: L["name"], default: str) -> str: ...
                  @overload
                  def setdefault(self, k: L["year"], default: int) -> int: ...
                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self, *, name: str=..., year: int=...) -> None: ...
                  @overload
                  def update(self: Movie, movie: Movie, /) -> None: ...
                  def update(self) -> DontCare: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_typed_dictionary_attributes
           ~equivalent_sources:
             [
               {|
              from typing_extensions import TypedDict

              class MovieNonTotal(TypedDict, total=False):
                name: str
                year: int
            |};
               {|
              from typing_extensions import NotRequired, TypedDict

              class MovieNonTotal(TypedDict, total=False):
                name: NotRequired[str]
                year: NotRequired[int]
            |};
               {|
              from typing_extensions import NotRequired, TypedDict

              class MovieNonTotal(TypedDict):
                name: NotRequired[str]
                year: NotRequired[int]
            |};
             ]
           ~class_name:"MovieNonTotal"
           {|
                from typing import overload, Optional, TypeVar, Union, _PyreReadOnly_ as RO
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class MovieNonTotal:
                  @overload
                  def __init__(self, *, name: str=..., year: int=...) -> None: ...
                  @overload
                  def __init__(self: MovieNonTotal, movie: MovieNonTotal, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  @overload
                  def __getitem__(self, k: L["name"]) -> str: ...
                  @overload
                  def __getitem__(self, k: L["year"]) -> int: ...
                  def __getitem__(self) -> DontCare: ...

                  @overload
                  def __setitem__(self, k: L["name"], v: str) -> None: ...
                  @overload
                  def __setitem__(self, k: L["year"], v: int) -> None: ...
                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: L["name"]) -> Optional[str]: ...
                  @overload
                  def get(self: RO[MovieNonTotal], k: L["name"]) -> RO[Optional[str]]: ...
                  @overload
                  def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def get(self: RO[MovieNonTotal], k: L["name"], default: _T) -> RO[Union[str, _T]]: ...
                  @overload
                  def get(self, k: L["year"]) -> Optional[int]: ...
                  @overload
                  def get(self: RO[MovieNonTotal], k: L["year"]) -> RO[Optional[int]]: ...
                  @overload
                  def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  @overload
                  def get(self: RO[MovieNonTotal], k: L["year"], default: _T) -> RO[Union[int, _T]]: ...
                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  @overload
                  def setdefault(self, k: L["name"], default: str) -> str: ...
                  @overload
                  def setdefault(self, k: L["year"], default: int) -> int: ...
                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self, *, name: str=..., year: int=...) -> None: ...
                  @overload
                  def update(self: MovieNonTotal, movie: MovieNonTotal, /) -> None: ...
                  def update(self) -> DontCare: ...

                  @overload
                  def __delitem__(self, k: L["name"]) -> None: ...
                  @overload
                  def __delitem__(self, k: L["year"]) -> None: ...
                  def __delitem__(self) -> DontCare: ...

                  @overload
                  def pop(self, k: L["name"]) -> str: ...
                  @overload
                  def pop(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def pop(self, k: L["year"]) -> int: ...
                  @overload
                  def pop(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  def pop(self) -> DontCare: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_typed_dictionary_attributes
           ~equivalent_sources:
             [
               {|
              from typing_extensions import TypedDict

              class MovieBaseTotal(TypedDict):
                name: str

              class MovieChildNonTotal(MovieBaseTotal, total=False):
                year: int
            |};
               {|
              from typing_extensions import Required, TypedDict

              class MovieChildNonTotal(TypedDict, total=False):
                name: Required[str]
                year: int
            |};
             ]
           ~class_name:"MovieChildNonTotal"
           {|
                from typing import overload, Optional, TypeVar, Union, _PyreReadOnly_ as RO
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class MovieChildNonTotal:
                  @overload
                  def __init__(self, *, name: str, year: int=...) -> None: ...
                  @overload
                  def __init__(self: MovieChildNonTotal, movie: MovieChildNonTotal, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  @overload
                  def __getitem__(self, k: L["name"]) -> str: ...
                  @overload
                  def __getitem__(self, k: L["year"]) -> int: ...
                  def __getitem__(self) -> DontCare: ...

                  @overload
                  def __setitem__(self, k: L["name"], v: str) -> None: ...
                  @overload
                  def __setitem__(self, k: L["year"], v: int) -> None: ...
                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: L["name"]) -> Optional[str]: ...
                  @overload
                  def get(self: RO[MovieChildNonTotal], k: L["name"]) -> RO[Optional[str]]: ...
                  @overload
                  def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def get(self: RO[MovieChildNonTotal], k: L["name"], default: _T) -> RO[Union[str, _T]]: ...
                  @overload
                  def get(self, k: L["year"]) -> Optional[int]: ...
                  @overload
                  def get(self: RO[MovieChildNonTotal], k: L["year"]) -> RO[Optional[int]]: ...
                  @overload
                  def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  @overload
                  def get(self: RO[MovieChildNonTotal], k: L["year"], default: _T) -> RO[Union[int, _T]]: ...
                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  @overload
                  def setdefault(self, k: L["name"], default: str) -> str: ...
                  @overload
                  def setdefault(self, k: L["year"], default: int) -> int: ...
                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self, *, name: str=..., year: int=...) -> None: ...
                  @overload
                  def update(self: MovieChildNonTotal, movie: MovieChildNonTotal, /) -> None: ...
                  def update(self) -> DontCare: ...

                  @overload
                  def __delitem__(self, k: L["year"]) -> None: ...
                  def __delitem__(self) -> DontCare: ...

                  @overload
                  def pop(self, k: L["year"]) -> int: ...
                  @overload
                  def pop(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  def pop(self) -> DontCare: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_typed_dictionary_attributes
           ~equivalent_sources:
             [
               {|
              from typing_extensions import TypedDict

              class MovieBaseNonTotal(TypedDict, total=False):
                name: str

              class MovieChildTotal(MovieBaseNonTotal):
                year: int
            |};
               {|
              from typing_extensions import NotRequired, TypedDict

              class MovieChildTotal(TypedDict):
                name: NotRequired[str]
                year: int
            |};
             ]
           ~class_name:"MovieChildTotal"
           {|
                from typing import overload, Optional, TypeVar, Union, _PyreReadOnly_ as RO
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class MovieChildTotal:
                  @overload
                  def __init__(self, *, year: int, name: str=...) -> None: ...
                  @overload
                  def __init__(self: MovieChildTotal, movie: MovieChildTotal, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  @overload
                  def __getitem__(self, k: L["name"]) -> str: ...
                  @overload
                  def __getitem__(self, k: L["year"]) -> int: ...
                  def __getitem__(self) -> DontCare: ...

                  @overload
                  def __setitem__(self, k: L["name"], v: str) -> None: ...
                  @overload
                  def __setitem__(self, k: L["year"], v: int) -> None: ...
                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: L["name"]) -> Optional[str]: ...
                  @overload
                  def get(self: RO[MovieChildTotal], k: L["name"]) -> RO[Optional[str]]: ...
                  @overload
                  def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def get(self: RO[MovieChildTotal], k: L["name"], default: _T) -> RO[Union[str, _T]]: ...
                  @overload
                  def get(self, k: L["year"]) -> Optional[int]: ...
                  @overload
                  def get(self: RO[MovieChildTotal], k: L["year"]) -> RO[Optional[int]]: ...
                  @overload
                  def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
                  @overload
                  def get(self: RO[MovieChildTotal], k: L["year"], default: _T) -> RO[Union[int, _T]]: ...
                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  @overload
                  def setdefault(self, k: L["name"], default: str) -> str: ...
                  @overload
                  def setdefault(self, k: L["year"], default: int) -> int: ...
                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self, *, year: int=..., name: str=...) -> None: ...
                  @overload
                  def update(self: MovieChildTotal, movie: MovieChildTotal, /) -> None: ...
                  def update(self) -> DontCare: ...

                  @overload
                  def __delitem__(self, k: L["name"]) -> None: ...
                  def __delitem__(self) -> DontCare: ...

                  @overload
                  def pop(self, k: L["name"]) -> str: ...
                  @overload
                  def pop(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  def pop(self) -> DontCare: ...
              |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_typed_dictionary_attribute_types
           ~source:
             {|
              from typing_extensions import TypedDict

              class EmptyTypedDict(TypedDict):
                pass
            |}
           ~class_name:"EmptyTypedDict"
           {|
                from typing import overload, Optional, TypeVar, Union
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class EmptyTypedDict:
                  @overload
                  def __init__(self) -> None: ...
                  @overload
                  def __init__(self: EmptyTypedDict, movie: EmptyTypedDict, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  def __getitem__(self) -> DontCare: ...

                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self) -> None: ...
                  @overload
                  def update(self: EmptyTypedDict, movie: EmptyTypedDict, /) -> None: ...
                  def update(self) -> DontCare: ...
              |};
      (* Note: If a TypedDict has zero non-required fields, we don't generate `pop` or
         `__delitem__`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_typed_dictionary_attribute_types
           ~source:
             {|
              from typing_extensions import TypedDict

              class EmptyNonTotalTypedDict(TypedDict, total=False):
                pass
            |}
           ~class_name:"EmptyNonTotalTypedDict"
           {|
                from typing import overload, Optional, TypeVar, Union
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class EmptyNonTotalTypedDict:
                  @overload
                  def __init__(self) -> None: ...
                  @overload
                  def __init__(self: EmptyNonTotalTypedDict, movie: EmptyNonTotalTypedDict, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  def __getitem__(self) -> DontCare: ...

                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self) -> None: ...
                  @overload
                  def update(self: EmptyNonTotalTypedDict, movie: EmptyNonTotalTypedDict, /) -> None: ...
                  def update(self) -> DontCare: ...
              |};
      (* Non-attributes are ignored. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_equivalent_typed_dictionary_attribute_types
           ~source:
             {|
              from typing_extensions import TypedDict

              class Movie(TypedDict):
                name: str

                def some_method_that_will_not_exist_at_runtime(self) -> None: ...
            |}
           ~class_name:"Movie"
           {|
                from typing import overload, Optional, TypeVar, Union, _PyreReadOnly_ as RO
                from typing_extensions import Literal as L

                _T = TypeVar("_T")

                class Movie:
                  @overload
                  def __init__(self, *, name: str) -> None: ...
                  @overload
                  def __init__(self: Movie, movie: Movie, /) -> None: ...
                  def __init__(self) -> DontCare: ...

                  @overload
                  def __getitem__(self, k: L["name"]) -> str: ...
                  def __getitem__(self) -> DontCare: ...

                  @overload
                  def __setitem__(self, k: L["name"], v: str) -> None: ...
                  def __setitem__(self) -> DontCare: ...

                  @overload
                  def get(self, k: L["name"]) -> Optional[str]: ...
                  @overload
                  def get(self: RO[Movie], k: L["name"]) -> RO[Optional[str]]: ...
                  @overload
                  def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
                  @overload
                  def get(self: RO[Movie], k: L["name"], default: _T) -> RO[Union[str, _T]]: ...
                  @overload
                  def get(self, k: str) -> Optional[object]: ...
                  @overload
                  def get(self, k: str, default: _T) -> Union[object, _T]: ...
                  def get(self) -> DontCare: ...

                  @overload
                  def setdefault(self, k: L["name"], default: str) -> str: ...
                  def setdefault(self) -> DontCare: ...

                  @overload
                  def update(self, *, name: str=...) -> None: ...
                  @overload
                  def update(self: Movie, movie: Movie, /) -> None: ...
                  def update(self) -> DontCare: ...
              |};
    ]


let () = "typedDictionary" >::: [test_typed_dictionary_attributes] |> Test.run
