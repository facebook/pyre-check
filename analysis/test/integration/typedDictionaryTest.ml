(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest
open Test

let test_check_typed_dictionaries context =
  let assert_test_typed_dictionary source =
    let typing_stub =
      {
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
        handle = "mypy_extensions.pyi";
        source =
          {|
            import typing
            def TypedDict(
                typename: str,
                fields: typing.Dict[str, typing.Type[_T]],
                total: bool = ...,
            ) -> typing.Type[dict]: ...
          |};
      }
    in
    let typed_dictionary_for_import =
      {
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
          |};
      }
    in
    assert_type_errors
      ~context
      ~update_environment_with:[typing_stub; mypy_extensions_stub; typed_dictionary_for_import]
      source
  in
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['name'])
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `foo` but got `str`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['yar'])
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter "
      ^ "to call `foo` but got `str`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `yar`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f(key: str) -> None:
        movie: Movie
        a = foo(movie[key])
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter "
      ^ "to call `foo` but got `str`.";
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal. "
      ^ "Expected one of ('name', 'year')." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Actor = mypy_extensions.TypedDict('Actor', {'name': str, 'birthyear': 'int'})
      def foo(movie: Movie) -> str:
        return movie["name"]
      def f() -> None:
        actor: Actor
        a = foo(actor)
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Movie` with "
      ^ "fields (name: str, year: int)` for 1st anonymous parameter to call `foo` "
      ^ "but got `TypedDict `Actor` with fields (name: str, birthyear: int)`." ];
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
    [ "Incompatible return type [7]: Expected `int` but got `str`.";
      "TypedDict accessed with a missing key [27]: TypedDict has no key `year`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
      import mypy_extensions
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
    [ "Incompatible parameter type [6]: "
      ^ "Expected `Mapping[str, A]` for 1st anonymous parameter to call `foo` but got "
      ^ "`TypedDict `Baz` with fields (foo: A, bar: B)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
    ["Incompatible return type [7]: Expected `Mapping[str, A]` but got `Mapping[str, typing.Any]`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': int})
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            q = a["fou"]
        else:
            q = a["bar"]
        return q
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Baz` has no key `fou`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
      import mypy_extensions
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
      import mypy_extensions
      Baz = mypy_extensions.TypedDict(
        'Baz',
        {
           'first_very_long_field': int,
           'second_very_long_field': int,
           'third_very_long_field': int,
           'fourth_very_long_field': int,
           'fifth_very_long_field': int
        })
      def foo(a: Baz) -> int:
        ...
      def bar( **kwargs: int) -> None:
        foo(kwargs)
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Baz`` for 1st anonymous parameter to \
       call `foo` but got `typing.Dict[str, int]`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982)
        return movie['year']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(year=1982, name='Blade Runner')
        return movie['year']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name=1982, year='Blade Runner')
        return movie['year']
    |}
    [ "Incompatible parameter type [6]: Expected `str` for 1st parameter `name` "
      ^ "to call `__init__` but got `int`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie('Blade Runner', 1982)
        return movie['year']
    |}
    ["Too many arguments [19]: Call `__init__` expects 0 positional arguments, 2 were provided."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie('Blade Runner')
        return movie['year']
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Movie` with fields (name: str, year: \
       int)` for 1st anonymous parameter to call `__init__` but got `str`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie({ "name": "Blade Runner", "year": 1982 })
        return movie['year']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie({ "name": 1982, "year": "Blade Runner" })
        return movie['year']
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Movie` with fields (name: str, year: \
       int)` for 1st anonymous parameter to call `__init__` but got `TypedDict with fields (name: \
       int, year: str)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982, extra=42)
        return movie['year']
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `extra` to call `__init__`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(year=1982)
        return movie['year']
    |}
    ["Missing argument [20]: Call `__init__` expects argument `name`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int}, total=False)
      def foo() -> int:
        movie = Movie(year=1982)
        return movie['year']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 'new name'
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 7
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `str` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got "
      ^ "`int`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['nme'] = 'new name'
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nme`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
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
    [ "Incompatible parameter type [6]: "
      ^ "Expected `B` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got \
         `A`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['year'] += 7
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        reveal_type(len(movie))
    |}
    ["Revealed type [-1]: Revealed type for `len(movie)` is `int`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        for k in movie:
          reveal_type(k)
    |}
    ["Revealed type [-1]: Revealed type for `k` is `str`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        b = "key" in movie
        reveal_type(b)
    |}
    ["Revealed type [-1]: Revealed type for `b` is `bool`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        v = movie['name']
        reveal_type(v)
        v = movie.get('name')
        reveal_type(v)
        v = movie.get('name', True)
        reveal_type(v)
        v = movie.get('nae', True)
    |}
    [ "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Revealed type [-1]: Revealed type for `v` is `typing.Optional[str]`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[True], str]`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nae`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        v = movie.keys()
        reveal_type(v)
        v = movie.values()
        reveal_type(v)
        v = movie.items()
        reveal_type(v)
    |}
    [ "Revealed type [-1]: Revealed type for `v` is `typing.AbstractSet[str]`.";
      "Revealed type [-1]: Revealed type for `v` is `typing.ValuesView[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.AbstractSet[typing.Tuple[str, typing.Any]]`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        v = movie.copy()
        reveal_type(v)
    |}
    [ "Revealed type [-1]: Revealed type for `v` is "
      ^ "`TypedDict `Movie` with fields (name: str, year: int)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        v = movie.setdefault('name', 'newname')
        reveal_type(v)
        v = movie.setdefault('name', 7)
        v = movie.setdefault('nme', 'newname')
    |}
    [ "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Incompatible parameter type [6]: Expected `str` for 2nd anonymous parameter to "
      ^ "call `TypedDictionary.setdefault` but got `int`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nme`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.update()
        movie.update(name = "newName")
        movie.update(year = 15)
        movie.update(name = "newName", year = 15)
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.update(name = 15, year = "backwards")
        movie.update(yar = "missing")
    |}
    [ "Incompatible parameter type [6]: Expected `str` for 1st parameter `name` to call "
      ^ "`TypedDictionary.update` but got `int`.";
      "Unexpected keyword [28]: Unexpected keyword argument `yar` to call "
      ^ "`TypedDictionary.update`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      MovieNonTotal = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'}, total=False)
      def f() -> None:
        movieNonTotal: MovieNonTotal
        v = movieNonTotal.pop("name")
        reveal_type(v)
        v = movieNonTotal.pop("name", False)
        reveal_type(v)
        v = movieNonTotal.pop("nae", False)
    |}
    [ "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[False], str]`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nae`." ];

  (* You can't pop an item from a total typeddict *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.pop("name")
    |}
    ["Undefined attribute [16]: `TypedDictionary` has no attribute `pop`."];

  (* TODO(T41338881) the del operator is not currently supported *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      MovieNonTotal = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'}, total=False)
      def f() -> None:
        movieNonTotal: MovieNonTotal
        movieNonTotal.__delitem__("name")
        movieNonTotal.__delitem__("nae")
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nae`."];

  (* You can't delete an item from a total typeddict *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.__delitem__("name")
    |}
    ["Undefined attribute [16]: `TypedDictionary` has no attribute `__delitem__`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      MovieNonTotal = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'}, total=False)
      MoviePlus = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int', 'director': str})
      def f() -> None:
        moviePlus: MoviePlus
        movieNonTotal: MovieNonTotal
        v = movieNonTotal.get("name", False)
        reveal_type(v)
        v = len(movieNonTotal)
        reveal_type(v)
        v = movieNonTotal.setdefault('name', "n")
        reveal_type(v)
    |}
    [ "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[False], str]`.";
      "Revealed type [-1]: Revealed type for `v` is `int`.";
      "Revealed type [-1]: Revealed type for `v` is `str`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] += 7
    |}
    [ "Incompatible parameter type [6]: "
      ^ "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`.";
      "Incompatible parameter type [6]: Expected `str` for 2nd anonymous parameter "
      ^ "to call `TypedDictionary.__setitem__` but got `int`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      ReversedMovie = mypy_extensions.TypedDict('Movie', {'year': 'int', 'name': str})
      def f() -> None:
        movie: Movie
        movie['name'] = 7
        reversedMovie: ReversedMovie
        reversedMovie['name'] = 7
    |}
    [ "Incompatible parameter type [6]: Expected `str` for 2nd anonymous parameter "
      ^ "to call `TypedDictionary.__setitem__` but got `int`.";
      "Incompatible parameter type [6]: Expected `str` for 2nd anonymous parameter "
      ^ "to call `TypedDictionary.__setitem__` but got `int`." ];
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
      import mypy_extensions
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: NamelessTypedDict
        a = foo(movie['year'])
    |}
    [ "Missing global annotation [5]: Globally accessible variable `NamelessTypedDict` "
      ^ "has no type specified.";
      "Missing argument [20]: Call `mypy_extensions.TypedDict` expects argument `fields`.";
      "Undefined type [11]: Type `NamelessTypedDict` is not defined.";
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call `foo` "
      ^ "but got `unknown`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f(x: str, y: int) -> None:
        foo({'name' : x, 'year': y})
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f() -> None:
        foo({'name' : "Blade Runner", 'year' : 1982})
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f() -> None:
        foo({'name' : 'Blade Runner', 'year' : '1982'})
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Movie` with fields "
      ^ "(name: str, year: int)` for 1st anonymous parameter to call `foo` but got "
      ^ "`TypedDict with fields (name: str, year: str)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f(x: str, y: int) -> None:
        foo({'name' : 'Blade Runner', x: y})
    |}
    [ "Incompatible parameter type [6]: Expected `TypedDict `Movie` with fields "
      ^ "(name: str, year: int)` for 1st anonymous parameter to call `foo` but got "
      ^ "`typing.Dict[str, typing.Union[int, str]]`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f() -> None:
        foo({'name' : "Blade Runner", 'year' : 1982, 'extra_key': 1})
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> str:
        movie: Movie = {'name' : "Blade Runner", 'year' : 1982}
        return movie['name']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f(x: bool) -> str:
        movie: Movie = {'name' : "Blade Runner", 'year' : 1982, 'bonus' : x}
        reveal_type(movie)
        return movie['name']
    |}
    [ "Revealed type [-1]: Revealed type for `movie` is `TypedDict `Movie` with fields (name: \
       str, year: int)` (inferred: `TypedDict with fields (name: str, year: int, bonus: bool)`)."
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> str:
        movie: Movie = {'name' : "Blade Runner", 'year' : '1982'}
        return movie['name']
    |}
    [ "Incompatible variable type [9]: movie is declared to have type "
      ^ "`TypedDict `Movie` with fields (name: str, year: int)` but is used as type "
      ^ "`TypedDict with fields (name: str, year: str)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
      def f() -> int:
        movie: Movie = {'name' : "Blade Runner"}
        # this will fail at runtime, but that's the cost of doing business with non-total
        # typeddicts
        return movie['year']
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
      def f() -> int:
        movie: Movie = {'name' : 1982}
        return movie['year']
    |}
    [ "Incompatible variable type [9]: movie is declared to have type `TypedDict (non-total) \
       `Movie` with fields (name: str, year: int)` but is used as type `TypedDict (non-total) \
       with fields (name: int, year: int)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> Movie:
        return {'name' : "Blade Runner", 'year' : 1982}
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> Movie:
        return {'name' : "Blade Runner", 'year' : '1982'}
    |}
    [ "Incompatible return type [7]: Expected "
      ^ "`TypedDict `Movie` with fields (name: str, year: int)` but got "
      ^ "`TypedDict with fields (name: str, year: str)`." ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class Base(): pass
      class Child(Base): pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something' : Base})
      def f() -> Movie:
        return {'name' : "Blade Runner", 'something': Child()}
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class BaseTypedDict(mypy_extensions.TypedDict):
        required: int
      class ChildTypedDict(BaseTypedDict, total=False):
        optional: str
    |}
    [ "Invalid inheritance [39]: Building TypedDicts up through inheritance is not yet supported.";
      "Invalid type [31]: Expression `False` is not a valid type.";
      "Uninitialized attribute [13]: Attribute `optional` is declared in class `ChildTypedDict` \
       to have type `str` but is never initialized." ];
  ()


let () =
  "typed_dictionary" >::: ["check_typed_dictionarys" >:: test_check_typed_dictionaries] |> Test.run
