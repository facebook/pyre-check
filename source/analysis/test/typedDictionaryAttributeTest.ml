(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_typed_dictionary_attributes context =
  let assert_equivalent_typed_dictionary_attribute_types =
    assert_equivalent_typed_dictionary_attribute_types ~context
  in
  let assert_typed_dictionary_attributes
      ~equivalent_sources
      ~class_name
      expected_equivalent_class_source
    =
    List.iter equivalent_sources ~f:(fun source ->
        assert_equivalent_typed_dictionary_attribute_types
          ~source
          ~class_name
          expected_equivalent_class_source)
  in
  assert_typed_dictionary_attributes
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
        from typing import overload, Optional, TypeVar, Union
        from typing_extensions import Literal as L

        _T = TypeVar("_T")

        class Movie:
          @overload
          def __init__(self, *, name: str, year: int) -> None: ...
          @overload
          def __init__(self, movie: Movie, /) -> None: ...
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
          def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
          @overload
          def get(self, k: L["year"]) -> Optional[int]: ...
          @overload
          def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
          def get(self) -> DontCare: ...

          @overload
          def setdefault(self, k: L["name"], default: str) -> str: ...
          @overload
          def setdefault(self, k: L["year"], default: int) -> int: ...
          def setdefault(self) -> DontCare: ...

          @overload
          def update(self, *, name: str=..., year: int=...) -> None: ...
          @overload
          def update(self, movie: Movie, /) -> None: ...
          def update(self) -> DontCare: ...
      |};
  assert_typed_dictionary_attributes
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
        from typing import overload, Optional, TypeVar, Union
        from typing_extensions import Literal as L

        _T = TypeVar("_T")

        class MovieNonTotal:
          @overload
          def __init__(self, *, name: str=..., year: int=...) -> None: ...
          @overload
          def __init__(self, movie: MovieNonTotal, /) -> None: ...
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
          def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
          @overload
          def get(self, k: L["year"]) -> Optional[int]: ...
          @overload
          def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
          def get(self) -> DontCare: ...

          @overload
          def setdefault(self, k: L["name"], default: str) -> str: ...
          @overload
          def setdefault(self, k: L["year"], default: int) -> int: ...
          def setdefault(self) -> DontCare: ...

          @overload
          def update(self, *, name: str=..., year: int=...) -> None: ...
          @overload
          def update(self, movie: MovieNonTotal, /) -> None: ...
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
  assert_typed_dictionary_attributes
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
        from typing import overload, Optional, TypeVar, Union
        from typing_extensions import Literal as L

        _T = TypeVar("_T")

        class MovieChildNonTotal:
          @overload
          def __init__(self, *, name: str, year: int=...) -> None: ...
          @overload
          def __init__(self, movie: MovieChildNonTotal, /) -> None: ...
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
          def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
          @overload
          def get(self, k: L["year"]) -> Optional[int]: ...
          @overload
          def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
          def get(self) -> DontCare: ...

          @overload
          def setdefault(self, k: L["name"], default: str) -> str: ...
          @overload
          def setdefault(self, k: L["year"], default: int) -> int: ...
          def setdefault(self) -> DontCare: ...

          @overload
          def update(self, *, name: str=..., year: int=...) -> None: ...
          @overload
          def update(self, movie: MovieChildNonTotal, /) -> None: ...
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
  assert_typed_dictionary_attributes
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
        from typing import overload, Optional, TypeVar, Union
        from typing_extensions import Literal as L

        _T = TypeVar("_T")

        class MovieChildTotal:
          @overload
          def __init__(self, *, year: int, name: str=...) -> None: ...
          @overload
          def __init__(self, movie: MovieChildTotal, /) -> None: ...
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
          def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
          @overload
          def get(self, k: L["year"]) -> Optional[int]: ...
          @overload
          def get(self, k: L["year"], default: _T) -> Union[int, _T]: ...
          def get(self) -> DontCare: ...

          @overload
          def setdefault(self, k: L["name"], default: str) -> str: ...
          @overload
          def setdefault(self, k: L["year"], default: int) -> int: ...
          def setdefault(self) -> DontCare: ...

          @overload
          def update(self, *, year: int=..., name: str=...) -> None: ...
          @overload
          def update(self, movie: MovieChildTotal, /) -> None: ...
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
  assert_equivalent_typed_dictionary_attribute_types
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
          def __init__(self, movie: EmptyTypedDict, /) -> None: ...
          def __init__(self) -> DontCare: ...

          def __getitem__(self) -> DontCare: ...

          def __setitem__(self) -> DontCare: ...

          def get(self) -> DontCare: ...

          def setdefault(self) -> DontCare: ...

          @overload
          def update(self) -> None: ...
          @overload
          def update(self, movie: EmptyTypedDict, /) -> None: ...
          def update(self) -> DontCare: ...
      |};
  (* Note: If a TypedDict has zero non-required fields, we don't generate `pop` or `__delitem__`. *)
  assert_equivalent_typed_dictionary_attribute_types
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
          def __init__(self, movie: EmptyNonTotalTypedDict, /) -> None: ...
          def __init__(self) -> DontCare: ...

          def __getitem__(self) -> DontCare: ...

          def __setitem__(self) -> DontCare: ...

          def get(self) -> DontCare: ...

          def setdefault(self) -> DontCare: ...

          @overload
          def update(self) -> None: ...
          @overload
          def update(self, movie: EmptyNonTotalTypedDict, /) -> None: ...
          def update(self) -> DontCare: ...
      |};
  (* Non-attributes are ignored. *)
  assert_equivalent_typed_dictionary_attribute_types
    ~source:
      {|
      from typing_extensions import TypedDict

      class Movie(TypedDict):
        name: str

        def some_method_that_will_not_exist_at_runtime(self) -> None: ...
    |}
    ~class_name:"Movie"
    {|
        from typing import overload, Optional, TypeVar, Union
        from typing_extensions import Literal as L

        _T = TypeVar("_T")

        class Movie:
          @overload
          def __init__(self, *, name: str) -> None: ...
          @overload
          def __init__(self, movie: Movie, /) -> None: ...
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
          def get(self, k: L["name"], default: _T) -> Union[str, _T]: ...
          def get(self) -> DontCare: ...

          @overload
          def setdefault(self, k: L["name"], default: str) -> str: ...
          def setdefault(self) -> DontCare: ...

          @overload
          def update(self, *, name: str=...) -> None: ...
          @overload
          def update(self, movie: Movie, /) -> None: ...
          def update(self) -> DontCare: ...
      |};
  ()


let () =
  "typedDictionary"
  >::: ["attributes" >: test_case ~length:Long test_typed_dictionary_attributes]
  |> Test.run
