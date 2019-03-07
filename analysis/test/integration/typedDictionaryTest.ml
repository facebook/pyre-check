(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest
open Test

open Ast.Expression


let test_check_typed_dictionaries _ =
  let assert_test_typed_dictionary source =
    let typing_stub =
      {
        qualifier = Access.create "typing";
        handle = "typing.pyi";
        source =
          {|
            Any = object()
            class _SpecialForm:
                def __getitem__(self, typeargs: Any) -> Any: ...
          |};
      }
    in
    let mypy_extensions_stub =
      {
        qualifier = Access.create "mypy_extensions";
        handle = "mypy_extensions.pyi";
        source =
          "def TypedDict(typename: str, fields: typing.Dict[str, typing.Type[_T]], \
           total: bool = ...) -> typing.Type[dict]: ..."
      }
    in
    let typed_dictionary_for_import =
      {
        qualifier = Access.create "foo.bar.baz";
        handle = "foo/bar/baz.py";
        source =
          {|
            from mypy_extensions import TypedDict
            class ClassBasedTypedDictGreekLetters(TypedDict):
              alpha: int
              beta: str
              gamma: bool
            class ClassBasedNonTotalTypedDictGreekLetters(TypedDict, total=False):
              alpha: int
              beta: str
              gamma: bool
            def decorator(cls: C) -> C:
              return cls
            @decorator
            class DecoratedClassBasedTypedDictGreekLetters(TypedDict):
              alpha: int
              beta: str
              gamma: bool
          |}
      }
    in
    assert_type_errors
      ~update_environment_with:[
        typing_stub;
        mypy_extensions_stub;
        typed_dictionary_for_import;
      ]
      source
  in

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['year'])
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['name'])
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `foo` but got `str`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['yar'])
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter " ^
      "to call `foo` but got `str`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `yar`."
    ];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        key = "year"
        a = foo(movie[key])
    |}
    [];
  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f(key: str) -> None:
        movie: Movie
        a = foo(movie[key])
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter " ^
      "to call `foo` but got `str`.";
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal. " ^
      "Expected one of ('name', 'year').";
    ];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Film = mypy_extensions.TypedDict('Film', {'name': str, 'year': 'int', 'director': str})
      def foo(movie: Movie) -> str:
        return movie["name"]
      def f() -> None:
        movie: Film
        a = foo(movie)
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Actor = mypy_extensions.TypedDict('Actor', {'name': str, 'birthyear': 'int'})
      def foo(movie: Movie) -> str:
        return movie["name"]
      def f() -> None:
        actor: Actor
        a = foo(actor)
    |}
    [
      "Incompatible parameter type [6]: Expected `TypedDict `Movie` with " ^
      "fields (name: str, year: int)` for 1st anonymous parameter to call `foo` " ^
      "but got `TypedDict `Actor` with fields (name: str, birthyear: int)`."
    ];

  assert_test_typed_dictionary
    {|
      from mypy_extensions import TypedDict
      Movie = TypedDict('Movie', {'name': str, 'year': int})
      Cat = TypedDict('Cat', {'name': str, 'breed': str})
      Named = TypedDict('Named', {'name': str})

      def foo(x: int, a: Movie, b: Cat) -> Named:
        if x == 7:
            q = a
        else:
            q = b
        return q
    |}
    [];

  assert_test_typed_dictionary
    {|
      from mypy_extensions import TypedDict
      Movie = TypedDict('Movie', {'name': str, 'year': int})
      Cat = TypedDict('Cat', {'name': str, 'breed': str})

      def foo(x: int, a: Movie, b: Cat) -> int:
          if x == 7:
              q = a
          else:
              q = b
          return q["year"]
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "TypedDict accessed with a missing key [27]: TypedDict has no key `year`.";
    ];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': str})
      def foo(dictionary: Mapping[str, typing.Any]) -> None:
        pass
      def f() -> None:
        baz: Baz
        a = foo(baz)
    |}
    [];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      class A:
        pass
      class B(A):
        pass
      Baz = mypy_extensions.TypedDict('Baz', {'foo': A, 'bar': B})
      def foo(dictionary: Mapping[str, A]) -> A:
        return dictionary["foo"]
      def f() -> None:
        baz: Baz
        a = foo(baz)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `Mapping[str, A]` for 1st anonymous parameter to call `foo` but got " ^
      "`TypedDict `Baz` with fields (foo: A, bar: B)`."
    ];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      class A:
        pass
      class B(A):
        pass
      class C(A):
        pass
      Baz = mypy_extensions.TypedDict('Baz', {'foo': A, 'bar': B})
      def foo(x: int, a: Baz, b: Mapping[str, C]) -> Mapping[str, A]:
        if x == 7:
            q = a
        else:
            q = b
        return q
    |}
    [
      "Incompatible return type [7]: Expected `Mapping[str, A]` but got `Mapping[str, typing.Any]`."
    ];

  assert_test_typed_dictionary
    {|
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': int})
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            q = a["fou"]
        else:
            q = a["bar"]
        return q
    |}
    [
      "TypedDict accessed with a missing key [27]: TypedDict `Baz` has no key `fou`.";
    ];

  assert_test_typed_dictionary
    {|
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': int})
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            k = "foo"
            q = a[k]
        else:
            q = a["bar"]
        return q
    |}
    [];
  assert_test_typed_dictionary
    {|
      Baz = mypy_extensions.TypedDict(
        'Baz',
        {
           'first_very_long_field': int,
           'second_very_long_field': int,
           'third_very_long_field': int,
           'fourth_very_long_field': int,
           'fifth_very_long_field': int
        })
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            k = "foo"
            q = a[k]
        else:
            q = a["first_very_long_field"]
        return q
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Baz` has no key `foo`."];


  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982)
        return movie['year']
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(year=1982, name='Blade Runner')
        return movie['year']
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name=1982, year='Blade Runner')
        return movie['year']
    |}
    [
      "Incompatible parameter type [6]: Expected `str` for 1st parameter `name` " ^
      "to call `__init__` but got `int`.";
    ];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie('Blade Runner', 1982)
        return movie['year']
    |}
    ["Too many arguments [19]: Call `__init__` expects 0 positional arguments, 2 were provided."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982, extra=42)
        return movie['year']
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `extra` to call `__init__`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(year=1982)
        return movie['year']
    |}
    ["Missing argument [20]: Call `__init__` expects argument `name`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int}, total=False)
      def foo() -> int:
        movie = Movie(year=1982)
        return movie['year']
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 'new name'
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 7
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got " ^
     "`int`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['nme'] = 'new name'
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nme`."];

  assert_test_typed_dictionary
    {|
      class A():
        pass
      class B(A):
        pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something': A})
      def f() -> None:
        movie: Movie
        movie['something'] = B()
    |}
    [];

  assert_test_typed_dictionary
    {|
      class A():
        pass
      class B(A):
        pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something': B})
      def f() -> None:
        movie: Movie
        movie['something'] = A()
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `B` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got `A`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['year'] += 7
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] += 7
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`.";
      "Incompatible parameter type [6]: Expected `str` for 2nd anonymous parameter " ^
      "to call `TypedDictionary.__setitem__` but got `int`.";
    ];

  assert_test_typed_dictionary
    {|
      from foo.bar.baz import ClassBasedTypedDictGreekLetters
      def f() -> int:
        baz = ClassBasedTypedDictGreekLetters(alpha = 7, beta = "a", gamma = True)
        return baz['alpha']
    |}
    [];
  assert_test_typed_dictionary
    {|
      from foo.bar.baz import ClassBasedTypedDictGreekLetters
      def f() -> int:
        baz = ClassBasedTypedDictGreekLetters(alpha = 7, gamma = True)
        return baz['alpha']
    |}
    ["Missing argument [20]: Call `__init__` expects argument `beta`."];
  assert_test_typed_dictionary
    {|
      from foo.bar.baz import ClassBasedNonTotalTypedDictGreekLetters
      def f() -> int:
        baz = ClassBasedNonTotalTypedDictGreekLetters(alpha = 7, gamma = True)
        return baz['alpha']
    |}
    [];
  assert_test_typed_dictionary
    {|
      from foo.bar.baz import DecoratedClassBasedTypedDictGreekLetters
      def f() -> int:
        baz = DecoratedClassBasedTypedDictGreekLetters(alpha = 7, beta = "a", gamma = True)
        return baz['alpha']
    |}
    [];

  (* TODO T37629490 Better error messages for typeddict declaration errors *)
  assert_test_typed_dictionary
    {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: NamelessTypedDict
        a = foo(movie['year'])
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `NamelessTypedDict` " ^
      "has no type specified.";
      "Missing argument [20]: Call `mypy_extensions.TypedDict` expects argument `fields`.";
      "Invalid type [31]: Expression `NamelessTypedDict` is not a valid type.";
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call `foo` " ^
      "but got `unknown`.";
    ]


let () =
  "typed_dictionary">:::[
    "check_typed_dictionarys">::test_check_typed_dictionaries;
  ]
  |> Test.run
