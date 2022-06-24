(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest
open Test

let test_check_typed_dictionaries context =
  let assert_test_typed_dictionary source =
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
      ~update_environment_with:[mypy_extensions_stub; typed_dictionary_for_import]
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
      Empty = mypy_extensions.TypedDict('Empty', {})
      d: Empty
      reveal_type(d)
    |}
    ["Revealed type [-1]: Revealed type for `d` is `Empty`."];
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
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
    ];
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
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `yar`.";
    ];
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
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal. "
      ^ "Expected one of ('name', 'year').";
    ];
  (* Imported from typing_extensions. *)
  assert_test_typed_dictionary
    {|
      import typing_extensions
      Movie = typing_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f(key: str) -> None:
        movie: Movie
        a = foo(movie[key])
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal. "
      ^ "Expected one of ('name', 'year').";
    ];
  (* Imported from typing. *)
  assert_test_typed_dictionary
    {|
      import typing
      Movie = typing.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f(key: str) -> None:
        movie: Movie
        a = foo(movie[key])
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `int` but got `str`.";
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal. "
      ^ "Expected one of ('name', 'year').";
    ];
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
    [
      (* TODO(T37629490): Mention the differing keys. *)
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Movie` but got `Actor`.";
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
      "TypedDict accessed with a missing key [27]: TypedDict `Cat` has no key `year`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      from typing import Mapping, Any
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': str})
      def foo(dictionary: Mapping[str, Any]) -> None:
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
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Mapping[str, A]` but got `Baz`.";
    ];
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
    ["Incompatible return type [7]: Expected `Mapping[str, A]` but got `Mapping[str, object]`."];
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
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Baz` but got `Dict[str, int]`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        reveal_type(Movie.__init__)
        movie = Movie(name='Blade Runner', year=1982)
        return movie['year']
    |}
    [
      "Revealed type [-1]: Revealed type for `Movie.__init__` is `typing.Callable(__init__)[..., \
       unknown][[[Named(self, Movie), KeywordOnly(name, str), KeywordOnly(year, int)], \
       None][[Movie, Movie], None]]`.";
    ];
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
    [
      "Incompatible parameter type [6]: In call `__init__`, for 1st parameter `name` expected \
       `str` but got `int`.";
      "Incompatible parameter type [6]: In call `__init__`, for 2nd parameter `year` expected \
       `int` but got `str`.";
    ];
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
    [
      "Incompatible parameter type [6]: In call `__init__`, for 1st positional only parameter \
       expected `Movie` but got `str`.";
    ];
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
    [
      "TypedDict initialization error [55]: Expected type `str` for `Movie` field `name` but got \
       `int`.";
      "TypedDict initialization error [55]: Expected type `int` for `Movie` field `year` but got \
       `str`.";
    ];
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
    [
      "Invalid TypedDict operation [54]: Expected `str` to be assigned to `Movie` field `name` but \
       got `int`.";
    ];
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
      import mypy_extensions
      class A():
        pass
      class B(A):
        pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something': B})
      def f() -> None:
        movie: Movie
        movie['something'] = A()
    |}
    [
      "Invalid TypedDict operation [54]: Expected `B` to be assigned to `Movie` field `something` \
       but got `A`.";
    ];
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
    [
      "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Revealed type [-1]: Revealed type for `v` is `typing.Optional[str]`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[True], str]`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nae`.";
    ];
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
    [
      "Revealed type [-1]: Revealed type for `v` is `typing.AbstractSet[str]`.";
      "Revealed type [-1]: Revealed type for `v` is `typing.ValuesView[object]`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.AbstractSet[typing.Tuple[str, object]]`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        v = movie.copy()
        reveal_type(v)
    |}
    ["Revealed type [-1]: Revealed type for `v` is " ^ "`Movie`."];
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
    [
      "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Incompatible parameter type [6]: In call `TypedDictionary.setdefault`, for 2nd positional \
       only parameter expected `str` but got `int`.";
      "TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nme`.";
    ];
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
    [
      "Incompatible parameter type [6]: In call `TypedDictionary.update`, for 1st parameter `name` \
       expected `str` but got `int`.";
      "Incompatible parameter type [6]: In call `TypedDictionary.update`, for 2nd parameter `year` \
       expected `int` but got `str`.";
      "Unexpected keyword [28]: Unexpected keyword argument `yar` to call "
      ^ "`TypedDictionary.update`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      movie1: Movie
      movie2: Movie
      movie2.update(movie1)
      movie2.update(7)
    |}
    [
      "Incompatible parameter type [6]: In call `TypedDictionary.update`, for 1st positional only \
       parameter expected `Movie` but got `int`.";
    ];
  assert_test_typed_dictionary
    (* TODO(T37629490): We should handle the alias not being the same as the TypedDict name. *)
    {|
      import mypy_extensions
      # MovieNonTotal = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'}, total=False)
      MovieNonTotal = mypy_extensions.TypedDict('MovieNonTotal', {'name': str, 'year': 'int'}, total=False)
      def f() -> None:
        movieNonTotal: MovieNonTotal
        v = movieNonTotal.pop("name")
        reveal_type(v)
        v = movieNonTotal.pop("name", False)
        reveal_type(v)
        v = movieNonTotal.pop("nae", False)
    |}
    [
      "Revealed type [-1]: Revealed type for `v` is `str`.";
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[False], str]`.";
      "TypedDict accessed with a missing key [27]: TypedDict `MovieNonTotal` has no key `nae`.";
    ];

  (* You can't pop an item from a total typeddict *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.pop("name")
    |}
    ["Undefined attribute [16]: `Movie` has no attribute `pop`."];

  (* TODO(T41338881) the del operator is not currently supported *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      MovieNonTotal = mypy_extensions.TypedDict('MovieNonTotal', {'name': str, 'year': 'int'}, total=False)
      def f() -> None:
        movieNonTotal: MovieNonTotal
        movieNonTotal.__delitem__("name")
        movieNonTotal.__delitem__("nae")
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `MovieNonTotal` has no key `nae`."];

  (* You can't delete an item from a total typeddict *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie.__delitem__("name")
    |}
    ["Undefined attribute [16]: `Movie` has no attribute `__delitem__`."];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      MovieNonTotal = mypy_extensions.TypedDict('MovieNonTotal', {'name': str, 'year': 'int'}, total=False)
      MoviePlus = mypy_extensions.TypedDict('MoviePlus', {'name': str, 'year': 'int', 'director': str})
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
    [
      "Revealed type [-1]: Revealed type for `v` is "
      ^ "`typing.Union[typing_extensions.Literal[False], str]`.";
      "Revealed type [-1]: Revealed type for `v` is `int`.";
      "Revealed type [-1]: Revealed type for `v` is `str`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] += 7
    |}
    [
      "Unsupported operand [58]: `+` is not supported for operand types `str` and `int`.";
      "Invalid TypedDict operation [54]: Expected `str` to be assigned to `Movie` field `name` but \
       got `int`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      ReversedMovie = mypy_extensions.TypedDict('ReversedMovie', {'year': 'int', 'name': str})
      def f() -> None:
        movie: Movie
        movie['name'] = 7
        reversedMovie: ReversedMovie
        reversedMovie['name'] = 7
    |}
    [
      "Invalid TypedDict operation [54]: Expected `str` to be assigned to `Movie` field `name` but \
       got `int`.";
      "Invalid TypedDict operation [54]: Expected `str` to be assigned to `ReversedMovie` field \
       `name` but got `int`.";
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
      import mypy_extensions
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: NamelessTypedDict
        a = foo(movie['year'])
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `NamelessTypedDict` "
      ^ "has no type specified.";
      "Missing argument [20]: Call `mypy_extensions.TypedDict` expects argument `fields`.";
      "Undefined or invalid type [11]: Annotation `NamelessTypedDict` is not defined as a type.";
    ];
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
    [
      "TypedDict initialization error [55]: Expected type `int` for `Movie` field `year` but got \
       `str`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f(x: str, y: int) -> None:
        foo({'name' : 'Blade Runner', x: y})
    |}
    [
      "Incompatible parameter type [6]: In call `foo`, for 1st positional only parameter expected \
       `Movie` but got `Dict[str, Union[int, str]]`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: Movie) -> str:
        return x["name"]
      def f() -> None:
        foo({'name' : "Blade Runner", 'year' : 1982, 'extra_key': 1})
    |}
    ["TypedDict initialization error [55]: TypedDict `Movie` has no field `extra_key`."];
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
    [
      "TypedDict initialization error [55]: TypedDict `Movie` has no field `bonus`.";
      "Revealed type [-1]: Revealed type for `movie` is `Movie`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> str:
        movie: Movie = {'name' : "Blade Runner", 'year' : '1982'}
        return movie['name']
    |}
    [
      "TypedDict initialization error [55]: Expected type `int` for `Movie` field `year` but got \
       `str`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
      def f() -> int:
        movie: Movie = {'name' : "Blade Runner"}
        reveal_type(movie)
        # this will fail at runtime, but that's the cost of doing business with non-total
        # typeddicts
        return movie['year']
    |}
    ["Revealed type [-1]: Revealed type for `movie` is `Movie`."];
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
    [
      "TypedDict initialization error [55]: Expected type `str` for `Movie` field `name` but got \
       `int`.";
    ];
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
      Movie = mypy_extensions.TypedDict('Movie', {'scores': dict})
      def f() -> Movie:
        return Movie(scores = { "imdb": 8.1 })
    |}
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
    ];
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'scores': Dict})
      def f() -> Movie:
        return Movie(scores = { "imdb": 8.1 })
    |}
    [
      "Invalid type parameters [24]: Generic type `dict` expects 2 type parameters, use \
       `typing.Dict` to avoid runtime subscripting errors.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> Movie:
        return {'name' : "Blade Runner", 'year' : '1982'}
    |}
    [
      "TypedDict initialization error [55]: Expected type `int` for `Movie` field `year` but got \
       `str`.";
    ];
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
      class TotalTypedDict(mypy_extensions.TypedDict):
        required: int
      foo = TotalTypedDict(required=0)
    |}
    [];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class NotTotalTypedDict(mypy_extensions.TypedDict, total=False):
        required: int
      foo = NotTotalTypedDict()
    |}
    [];
  assert_test_typed_dictionary
    {|
      import typing
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': typing.Any, 'year': 'int'})
      class Bar(mypy_extensions.TypedDict):
        x: typing.Any
    |}
    [
      "Prohibited any [33]: Explicit annotation for `name` cannot be `Any`.";
      "Prohibited any [33]: Explicit annotation for `x` cannot be `Any`.";
    ];
  (* `items` is found in `Mapping` as well, which would make Pyre complain about inconsistent
     override. Make sure there is no error since `items` is a dictionary field, not a real
     attribute. *)
  assert_test_typed_dictionary
    {|
      import typing
      import mypy_extensions
      class Bar(mypy_extensions.TypedDict):
        items: typing.List[int]
        foo: int
    |}
    [];
  assert_test_typed_dictionary
    {|
     import typing
     import mypy_extensions
     from typing import Protocol
     class HasField(Protocol):
       some_field: int
     class RegularClass:
       some_field: int = 1
     class Bar(mypy_extensions.TypedDict):
       some_field: int

     def expects_has_field(x: HasField) -> None: ...
     x: RegularClass
     d: Bar
     expects_has_field(x)
     expects_has_field(d)
   |}
    [
      "Incompatible parameter type [6]: In call `expects_has_field`, for 1st positional only \
       parameter expected `HasField` but got `Bar`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Movie2 = mypy_extensions.TypedDict('Movie2', {'name': str})
      movie: Movie
      movie2: Movie2
      x = movie if True else movie2
      reveal_type(x)
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type \
       `typing.Union[Movie, Movie2]` but no type is specified.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[Movie, Movie2]`.";
    ];
  assert_test_typed_dictionary
    {|
      from mypy_extensions import TypedDict
      class HasIllegalInitializedField(TypedDict):
        foo: int
        bar: str = "hello"
      movie: HasIllegalInitializedField
      movie["bar"]
      reveal_type(movie["bar"])
    |}
    ["Revealed type [-1]: Revealed type for `movie[\"bar\"]` is `str`."];
  ()


let test_check_typed_dictionary_inference context =
  let assert_test_typed_dictionary source =
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
    assert_type_errors ~context ~update_environment_with:[mypy_extensions_stub] source
  in
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        foo: int
        bar: str
      def baz(data: Dict[str, FooTypedDict]) -> None:
        pass
      baz(data={'hello': {'foo': 3, 'bar': 'hello'}})
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        foo: int
        bar: str
      data: Dict[str, FooTypedDict] = {'hello': {'foo': 3, 'bar': 'hello'}}
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        foo: int
        bar: str
      data: Dict[str, Dict[str, FooTypedDict]] = {'hello': {'nested_dictionary': {'foo': 3, 'bar': 'hello'}}}
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        dict_within_typed_dict: Dict[str, int]
        bar: str
      data: Dict[str, FooTypedDict] = {'hello': {'dict_within_typed_dict': {'x': 3, 'y': 7}, 'bar': 'hello'}}
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict, Mapping, Union
      def baz(data: Dict[str, Union[Mapping[str, int], bool, int]]) -> None:
        pass
      baz(data={'hello': 3})
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        foo: int
        bar: str
      class NestedTypedDict(mypy_extensions.TypedDict):
        foo: FooTypedDict
        bar: str
      data: Dict[str, NestedTypedDict] = {'hello': {'foo': {'foo': 3, 'bar': 'hello'}, 'bar': 'hello'}}
    |}
    [];
  assert_test_typed_dictionary
    {|
      from typing import Dict, Protocol
      import mypy_extensions
      class NonTotal(mypy_extensions.TypedDict, total=False):
        foo: int
        bar: str
      class Total(mypy_extensions.TypedDict):
        foo: int
        bar: str
      class Copyable(Protocol):
        def keys(self) -> object: ...
      class Poppable(Protocol):
        def pop(self) -> object: ...
      def expects_copyable(x: Copyable) -> None: ...
      def expects_poppable(x: Poppable) -> None: ...
      def foo(n: NonTotal, t: Total) -> None:
        expects_copyable(n)
        expects_copyable(t)
        expects_poppable(t) # total dicts are not poppable
        expects_poppable(n)
    |}
    [
      "Incompatible parameter type [6]: In call `expects_poppable`, for 1st positional only \
       parameter expected `Poppable` but got `Total`.";
    ];
  assert_test_typed_dictionary
    {|
      from typing import Dict, Optional
      import mypy_extensions
      class FooTypedDict(mypy_extensions.TypedDict):
        foo: int
        bar: str
      data: Optional[FooTypedDict] = {'foo': 3, 'bar': 'hello'}
    |}
    [];
  ()


let test_check_typed_dictionary_inheritance context =
  let assert_test_typed_dictionary source =
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
    let typed_dictionary_helpers =
      {
        handle = "helpers.py";
        source =
          {|
            import mypy_extensions
            class Base(mypy_extensions.TypedDict):
              foo: int
            class Child(Base):
              bar: str
            class GrandChild(Child):
              baz: str
            class ExplicitChild(mypy_extensions.TypedDict):
              foo: int
              bar: str
            class NonChild(mypy_extensions.TypedDict):
              foo: int
              baz: str

            class Movie(mypy_extensions.TypedDict):
              name: str
              year: int

            class MultipleInheritance(GrandChild, Movie):
              total: int

            def takes_base(d: Base) -> None: ...
            def takes_child(d: Child) -> None: ...
            def takes_grandchild(d: GrandChild) -> None: ...
            def takes_explicit_child(d: ExplicitChild) -> None: ...
            def takes_nonchild(d: NonChild) -> None: ...

            base: Base = {"foo": 3}
            child: Child = {"foo": 3, "bar": "hello"}
            grandchild: GrandChild = {"foo": 3, "bar": "hello", "baz": "world"}
            explicit_child: ExplicitChild = {"foo": 3, "bar": "hello"}
            non_child: NonChild = {"foo": 3, "baz": "hello"}
          |};
      }
    in
    assert_type_errors
      ~context
      ~update_environment_with:[mypy_extensions_stub; typed_dictionary_helpers]
      source
  in
  assert_test_typed_dictionary
    {|
        from helpers import Base, Child, GrandChild, child, grandchild
        d: Base
        reveal_type(d)
        d: GrandChild = child
        child["bar"]
        reveal_type(child["bar"])
        grandchild["bar"]
        reveal_type(grandchild["bar"])
        grandchild["foo"]
        reveal_type(grandchild["foo"])
        grandchild["non_existent"]
        # An attribute from a superclass shouldn't be seen as a field.
        grandchild["__doc__"]
    |}
    [
      "Revealed type [-1]: Revealed type for `d` is `Base`.";
      "Incompatible variable type [9]: d is declared to have type `GrandChild` but is used as type \
       `Child`.";
      "Revealed type [-1]: Revealed type for `helpers.child[\"bar\"]` is `str`.";
      "Revealed type [-1]: Revealed type for `helpers.grandchild[\"bar\"]` is `str`.";
      "Revealed type [-1]: Revealed type for `helpers.grandchild[\"foo\"]` is `int`.";
      "TypedDict accessed with a missing key [27]: TypedDict `GrandChild` has no key \
       `non_existent`.";
      "TypedDict accessed with a missing key [27]: TypedDict `GrandChild` has no key `__doc__`.";
    ];
  (* No attribute access allowed for TypedDictionary. *)
  assert_test_typed_dictionary
    {|
        from helpers import child, Child
        child.bar
        reveal_type(child.bar)
        child.non_existent
        reveal_type(child.non_existent)
    |}
    [
      "Undefined attribute [16]: `Child` has no attribute `bar`.";
      "Revealed type [-1]: Revealed type for `helpers.child.bar` is `unknown`.";
      "Undefined attribute [16]: `Child` has no attribute `non_existent`.";
      "Revealed type [-1]: Revealed type for `helpers.child.non_existent` is `unknown`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import Base, Child, GrandChild
        wrong1: Base = {}
        wrong2: Child = {"foo": 3}
        wrong3: GrandChild = {"foo": 3, "bar": "hello"}
        correct1: Base = {"foo": 3}
        correct2: Child = {"foo": 3, "bar": "hello"}
        correct3: GrandChild = {"foo": 3, "bar": "hello", "baz": "world"}
    |}
    [
      "TypedDict initialization error [55]: Missing required field `foo` for TypedDict `Base`.";
      "TypedDict initialization error [55]: Missing required field `bar` for TypedDict `Child`.";
      "TypedDict initialization error [55]: Missing required field `baz` for TypedDict \
       `GrandChild`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import Base, base, child, grandchild, explicit_child, non_child

        x0: Base = base
        x1: Base = child
        x2: Base = grandchild
        x3: Base = explicit_child
        x4: Base = non_child
    |}
    [];
  assert_test_typed_dictionary
    {|
        from helpers import Base, Child, NonChild, child, base, grandchild, explicit_child, non_child
        from typing_extensions import *
        x0: Child = child
        x1: Child = base
        x2: Child = grandchild
        x3: Child = explicit_child
        x4: Child = non_child
    |}
    [
      "Incompatible variable type [9]: x1 is declared to have type `Child` but is used as type \
       `Base`.";
      "Incompatible variable type [9]: x4 is declared to have type `Child` but is used as type \
       `NonChild`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import Base, Child, ExplicitChild, NonChild, GrandChild
        from helpers import grandchild, base, child, explicit_child, non_child
        x0: GrandChild = grandchild
        x1: GrandChild = base
        x2: GrandChild = child
        x3: GrandChild = explicit_child
        x4: GrandChild = non_child
    |}
    [
      "Incompatible variable type [9]: x1 is declared to have type `GrandChild` but is used as \
       type `Base`.";
      "Incompatible variable type [9]: x2 is declared to have type `GrandChild` but is used as \
       type `Child`.";
      "Incompatible variable type [9]: x3 is declared to have type `GrandChild` but is used as \
       type `ExplicitChild`.";
      "Incompatible variable type [9]: x4 is declared to have type `GrandChild` but is used as \
       type `NonChild`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import ExplicitChild, Base, NonChild
        from helpers import explicit_child, base, child, grandchild, non_child
        x0: ExplicitChild = explicit_child
        x1: ExplicitChild = base
        x2: ExplicitChild = child
        x3: ExplicitChild = grandchild
        x4: ExplicitChild = non_child
    |}
    [
      "Incompatible variable type [9]: x1 is declared to have type `ExplicitChild` but is used as \
       type `Base`.";
      "Incompatible variable type [9]: x4 is declared to have type `ExplicitChild` but is used as \
       type `NonChild`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import NonChild, Child, Base, ExplicitChild
        from helpers import explicit_child, base, child, grandchild, non_child
        x0: NonChild = non_child
        x1: NonChild = base
        x2: NonChild = child
        x3: NonChild = grandchild
        x4: NonChild = explicit_child
    |}
    [
      "Incompatible variable type [9]: x1 is declared to have type `NonChild` but is used as type \
       `Base`.";
      "Incompatible variable type [9]: x2 is declared to have type `NonChild` but is used as type \
       `Child`.";
      "Incompatible variable type [9]: x4 is declared to have type `NonChild` but is used as type \
       `ExplicitChild`.";
    ];
  assert_test_typed_dictionary
    {|
      from helpers import takes_base, base, child, grandchild, explicit_child, non_child

      takes_base(base)
      takes_base(child)
      takes_base(grandchild)
      takes_base(explicit_child)
      takes_base(non_child)
    |}
    [];
  assert_test_typed_dictionary
    {|
      from helpers import Base, Child, NonChild
      from helpers import takes_child, base, child, grandchild, explicit_child, non_child

      takes_child(base)
      takes_child(child)
      takes_child(grandchild)
      takes_child(explicit_child)
      takes_child(non_child)
    |}
    [
      "Incompatible parameter type [6]: In call `takes_child`, for 1st positional only parameter \
       expected `Child` but got `Base`.";
      "Incompatible parameter type [6]: In call `takes_child`, for 1st positional only parameter \
       expected `Child` but got `NonChild`.";
    ];
  assert_test_typed_dictionary
    {|
      from helpers import Base, GrandChild, Child, ExplicitChild, NonChild
      from helpers import takes_grandchild, base, child, grandchild, explicit_child, non_child

      takes_grandchild(base)
      takes_grandchild(child)
      takes_grandchild(grandchild)
      takes_grandchild(explicit_child)
      takes_grandchild(non_child)
    |}
    [
      "Incompatible parameter type [6]: In call `takes_grandchild`, for 1st positional only \
       parameter expected `GrandChild` but got `Base`.";
      "Incompatible parameter type [6]: In call `takes_grandchild`, for 1st positional only \
       parameter expected `GrandChild` but got `Child`.";
      "Incompatible parameter type [6]: In call `takes_grandchild`, for 1st positional only \
       parameter expected `GrandChild` but got `ExplicitChild`.";
      "Incompatible parameter type [6]: In call `takes_grandchild`, for 1st positional only \
       parameter expected `GrandChild` but got `NonChild`.";
    ];
  assert_test_typed_dictionary
    {|
      from helpers import Base, NonChild, Child, ExplicitChild
      from helpers import takes_nonchild, base, child, grandchild, explicit_child, non_child

      takes_nonchild(base)
      takes_nonchild(child)
      takes_nonchild(grandchild)
      takes_nonchild(explicit_child)
      takes_nonchild(non_child)
    |}
    [
      "Incompatible parameter type [6]: In call `takes_nonchild`, for 1st positional only \
       parameter expected `NonChild` but got `Base`.";
      "Incompatible parameter type [6]: In call `takes_nonchild`, for 1st positional only \
       parameter expected `NonChild` but got `Child`.";
      "Incompatible parameter type [6]: In call `takes_nonchild`, for 1st positional only \
       parameter expected `NonChild` but got `ExplicitChild`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class TotalBase(mypy_extensions.TypedDict):
        foo: int
      class NonTotalChild(TotalBase, total=False):
        bar: str
      def f() -> None:
        d: NonTotalChild = {"foo": 1}
        reveal_type(d["bar"])
        d2: NonTotalChild = {"bar": "hello"}
    |}
    [
      "Revealed type [-1]: Revealed type for `d[\"bar\"]` is `str`.";
      "TypedDict initialization error [55]: Missing required field `foo` for TypedDict \
       `NonTotalChild`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class NonTotalBase(mypy_extensions.TypedDict, total=False):
        foo: int
      class TotalChild(NonTotalBase):
        bar: str
      def f() -> None:
        d: TotalChild = {"bar": "hello"}
        reveal_type(d)
        reveal_type(d["foo"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d` is `TotalChild`.";
      "Revealed type [-1]: Revealed type for `d[\"foo\"]` is `int`.";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class NonTotalBase(mypy_extensions.TypedDict, total=False):
          foo: int
        class NonTotalChild(NonTotalBase, total=False):
          bar: str

        d: NonTotalChild
        reveal_type(d["foo"])
        reveal_type(d["bar"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d[\"foo\"]` is `int`.";
      "Revealed type [-1]: Revealed type for `d[\"bar\"]` is `str`.";
    ];
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      class TotalBase(mypy_extensions.TypedDict):
        foo: int
      class NonTotalChild(TotalBase, total=False):
        bar: str
      class TotalChild(TotalBase):
        bar: str
      d: NonTotalChild
      d2: TotalChild
      d3: TotalBase = d
      d4: TotalBase = d2
    |}
    [];
  (* TypedDict operations. *)
  assert_test_typed_dictionary
    {|
        from helpers import Child, child

        child: Child
        child["foo"]
        child["bar"]
        child["non_existent"]
        reveal_type(child["foo"])
        reveal_type(child["bar"])
    |}
    [
      "TypedDict accessed with a missing key [27]: TypedDict `Child` has no key `non_existent`.";
      "Revealed type [-1]: Revealed type for `child[\"foo\"]` is `int`.";
      "Revealed type [-1]: Revealed type for `child[\"bar\"]` is `str`.";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions

        class Base(mypy_extensions.TypedDict):
          foo: int
        class Child(Base, total=False):
          bar: int
        child: Child
        y: int = child.pop("foo")
        y: int = child.pop("bar")
        child.__delitem__("foo")
        child.__delitem__("bar")
    |}
    [
      "Invalid TypedDict operation [54]: Cannot `pop` required field `foo` from TypedDict `Child`.";
      "Invalid TypedDict operation [54]: Cannot delete required field `foo` from TypedDict `Child`.";
    ];
  (* Multiple inheritance. *)
  assert_test_typed_dictionary
    {|
        from helpers import MultipleInheritance

        d: MultipleInheritance
        reveal_type(d)
        x: int = d["bar"]
        y: str = d["total"]
    |}
    [
      "Revealed type [-1]: Revealed type for `d` is `MultipleInheritance`.";
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`.";
      "Incompatible variable type [9]: y is declared to have type `str` but is used as type `int`.";
    ];
  (* Empty TypedDict subclass. *)
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base(mypy_extensions.TypedDict):
          foo: int
        class Child(Base):
          ...
        d: Child
        x: str = d["foo"]
        reveal_type(d)
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `str` but is used as type `int`.";
      "Revealed type [-1]: Revealed type for `d` is `Child`.";
    ];

  (* Key collision between Child and Base. *)

  (* No error when they have the same annotation, as per PEP 589. However, [foo: int] must show up
     only once in the constructor. *)
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base(mypy_extensions.TypedDict):
          foo: int
        class Child(Base):
          foo: int
        reveal_type(Child.__init__)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Child.__init__` is \
       `typing.Callable(__init__)[..., unknown][[[Named(self, Child), KeywordOnly(foo, int)], \
       None][[Child, Child], None]]`.";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base1(mypy_extensions.TypedDict):
          foo: int
        class Base2(mypy_extensions.TypedDict):
          foo: int
        class Child(Base1, Base2):
          foo: int
        reveal_type(Child.__init__)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.Child.__init__` is \
       `typing.Callable(__init__)[..., unknown][[[Named(self, Child), KeywordOnly(foo, int)], \
       None][[Child, Child], None]]`.";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base(mypy_extensions.TypedDict):
          foo: int
        class Base2(mypy_extensions.TypedDict):
          bar: str
        class Child(Base, Base2):
          foo: str
          bar: int
        reveal_type(Child.__init__)
    |}
    [
      "Inconsistent override [15]: `bar` overrides attribute defined in `Base2` inconsistently. \
       Type `int` is not a subtype of the overridden attribute `str`.";
      "Inconsistent override [15]: `foo` overrides attribute defined in `Base` inconsistently. \
       Type `str` is not a subtype of the overridden attribute `int`.";
      (* Only the shadowing field shows up in the constructor. *)
      "Revealed type [-1]: Revealed type for `test.Child.__init__` is \
       `typing.Callable(__init__)[..., unknown][[[Named(self, Child), KeywordOnly(bar, int), \
       KeywordOnly(foo, str)], None][[Child, Child], None]]`.";
    ];
  (* Error when one field is required and the other is not. *)
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base(mypy_extensions.TypedDict):
          foo: int
        class Child(Base, total=False):
          foo: int
    |}
    (* TODO(T61662929): This should say that one is required and the other is not. *)
    [
      "Inconsistent override [15]: `foo` overrides attribute defined in `Base` inconsistently. \
       Type `int` is not a subtype of the overridden attribute `int`.";
    ];
  (* Key collision between superclasses. *)
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base1(mypy_extensions.TypedDict):
          foo: int
          bar: str
        class Base2(mypy_extensions.TypedDict):
          foo: str
          bar: int
        class Child(Base1, Base2):
          baz: str
        d: Child
        x: int = d["bar"]
        y: str = d["bar"]
    |}
    [
      "Invalid inheritance [39]: Field `bar` has type `str` in base class `Base1` and type `int` \
       in base class `Base2`.";
      "Invalid inheritance [39]: Field `foo` has type `int` in base class `Base1` and type `str` \
       in base class `Base2`.";
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`.";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base1(mypy_extensions.TypedDict):
          foo: int
        class Base2(mypy_extensions.TypedDict, total=False):
          foo: int
        class Child(Base1, Base2):
          baz: str
    |}
    [
      "Invalid inheritance [39]: `foo` is a required field in base class `Base1` and a \
       non-required field in base class `Base2` (because of `total=False`).";
    ];
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base1(mypy_extensions.TypedDict):
          foo: int
        class Base2(mypy_extensions.TypedDict):
          foo: str
        class Child(Base1, Base2):
          foo: str
    |}
    [
      "Invalid inheritance [39]: Field `foo` has type `int` in base class `Base1` and type `str` \
       in base class `Base2`.";
    ];
  (* Superclass must be a TypedDict. *)
  assert_test_typed_dictionary
    {|
        import mypy_extensions
        class Base(mypy_extensions.TypedDict):
          foo: int

        class NonTypedDict:
          not_a_field: int
        class Child(Base, NonTypedDict):
          baz: str
        class NonTotalChild(Base, NonTypedDict, total=False):
          non_total_baz: str

        reveal_type(Child.__init__)
        reveal_type(NonTotalChild.__init__)
    |}
    [
      "Uninitialized attribute [13]: Attribute `not_a_field` is declared in class `NonTypedDict` \
       to have type `int` but is never initialized.";
      "Invalid inheritance [39]: `NonTypedDict` is not a valid parent class for a typed \
       dictionary. Expected a typed dictionary.";
      "Invalid inheritance [39]: `NonTypedDict` is not a valid parent class for a typed \
       dictionary. Expected a typed dictionary.";
      "Revealed type [-1]: Revealed type for `test.Child.__init__` is \
       `typing.Callable(__init__)[..., unknown][[[Named(self, Child), KeywordOnly(baz, str), \
       KeywordOnly(foo, int)], None][[Child, Child], None]]`.";
      "Revealed type [-1]: Revealed type for `test.NonTotalChild.__init__` is \
       `typing.Callable(__init__)[..., unknown][[[Named(self, NonTotalChild), KeywordOnly(foo, \
       int), KeywordOnly(non_total_baz, str, default)], None][[NonTotalChild, NonTotalChild], \
       None]]`.";
    ];
  ()


let test_check_typed_dictionary_in_alias context =
  let assert_test_typed_dictionary source =
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
    let typed_dictionary_helpers =
      {
        handle = "helpers.py";
        source =
          {|
            import mypy_extensions
            class Base(mypy_extensions.TypedDict):
              foo: int
            class Child(Base):
              bar: str
            class GrandChild(Child):
              baz: str
            class ExplicitChild(mypy_extensions.TypedDict):
              foo: int
              bar: str
            class NonChild(mypy_extensions.TypedDict):
              foo: int
              baz: str

            class Movie(mypy_extensions.TypedDict):
              name: str
              year: int

            class MultipleInheritance(GrandChild, Movie):
              total: int

            def takes_base(d: Base) -> None: ...
            def takes_child(d: Child) -> None: ...
            def takes_grandchild(d: GrandChild) -> None: ...
            def takes_explicit_child(d: ExplicitChild) -> None: ...
            def takes_nonchild(d: NonChild) -> None: ...

            base: Base = {"foo": 3}
            child: Child = {"foo": 3, "bar": "hello"}
            grandchild: GrandChild = {"foo": 3, "bar": "hello", "baz": "world"}
            explicit_child: ExplicitChild = {"foo": 3, "bar": "hello"}
            non_child: NonChild = {"foo": 3, "baz": "hello"}
          |};
      }
    in
    assert_type_errors
      ~context
      ~update_environment_with:[mypy_extensions_stub; typed_dictionary_helpers]
      source
  in
  assert_test_typed_dictionary
    {|
        from helpers import child, Child
        from typing import List
        X = Child
        xs: X = child
        ys: X
        reveal_type(xs)
        reveal_type(ys)
        Y = List[Child]
        y: Y
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `xs` is `Child`.";
      "Revealed type [-1]: Revealed type for `ys` is `Child`.";
      "Revealed type [-1]: Revealed type for `y` is `List[Child]`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import Child, child
        from typing import List
        X = List[Child]
        xs: X = [child, child]
        ys: X = 1
    |}
    [
      "Incompatible variable type [9]: ys is declared to have type `List[Child]` but is used as \
       type `int`.";
    ];
  assert_test_typed_dictionary
    {|
        from helpers import Child, Base, grandchild, base, child
        from typing import Callable, List

        f: Callable[[Child], None]
        f(grandchild)
        f(base)

        xs: List[Child] = [child, child]
    |}
    [
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `Child` but got `Base`.";
    ];
  assert_test_typed_dictionary
    {|
      from helpers import Child, Base, GrandChild, base, child
      from typing import Generic, TypeVar
      T = TypeVar("T")
      class G(Generic[T]):
        x: T
        def __init__(self, x: T) -> None:
          self.x = x
        def return_T(self) -> T: ...
      class C(G[Child]): ...

      reveal_type(C.__init__)
      reveal_type(C.return_T)
      C(base)
      d: Base = C(child).x
      reveal_type(d)

      d2: GrandChild = C(child).x
      d3: GrandChild = C(child).return_T()
      reveal_type(C(child).x)
      reveal_type(C(child))


      def foo(x: G[T]) -> T:
        return x.return_T()
      def bar(c: C) -> None:
        x = foo(c)
        reveal_type(x)
        y = c.return_T()
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.C.__init__` is \
       `typing.Callable(G.__init__)[[Named(self, G[Child]), Named(x, Child)], None]`.";
      "Revealed type [-1]: Revealed type for `test.C.return_T` is \
       `typing.Callable(G.return_T)[[Named(self, G[Child])], Child]`.";
      "Incompatible parameter type [6]: In call `G.__init__`, for 1st positional only parameter \
       expected `Child` but got `Base`.";
      "Revealed type [-1]: Revealed type for `d` is `Base` (inferred: `Child`).";
      "Incompatible variable type [9]: d2 is declared to have type `GrandChild` but is used as \
       type `Child`.";
      "Incompatible variable type [9]: d3 is declared to have type `GrandChild` but is used as \
       type `Child`.";
      "Revealed type [-1]: Revealed type for `test.C(helpers.child).x` is `Child`.";
      "Revealed type [-1]: Revealed type for `test.C(helpers.child)` is `C`.";
      "Revealed type [-1]: Revealed type for `x` is `Child`.";
      "Revealed type [-1]: Revealed type for `y` is `Child`.";
    ];
  (* Alias within regular TypedDict definition. *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      from typing import List
      X = List[int]
      class RegularTypedDict(mypy_extensions.TypedDict):
        use_alias: X
        other_alias: List[X]
      d: RegularTypedDict
      reveal_type(d["use_alias"])
      reveal_type(d["other_alias"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d[\"use_alias\"]` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `d[\"other_alias\"]` is `List[List[int]]`.";
    ];
  (* Alias within TypedDict subclass definition. *)
  assert_test_typed_dictionary
    {|
      from helpers import *
      from typing import List
      X = List[int]
      class OtherChild(Base):
        use_alias: X
        other_alias: List[X]
      d: OtherChild
      reveal_type(d["use_alias"])
      reveal_type(d["other_alias"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d[\"use_alias\"]` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `d[\"other_alias\"]` is `List[List[int]]`.";
    ];
  (* Alias within aliases and used in regular TypedDict definition. *)
  assert_test_typed_dictionary
    {|
      import mypy_extensions
      from typing import List
      X = List[int]
      class RegularTypedDict(mypy_extensions.TypedDict):
        use_alias: X
        other_alias: List[X]
      Y = List[RegularTypedDict]
      d: Y
      reveal_type(d)
      reveal_type(d[0]["use_alias"])
      reveal_type(d[0]["other_alias"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d` is `List[RegularTypedDict]`.";
      "Revealed type [-1]: Revealed type for `d[0][\"use_alias\"]` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `d[0][\"other_alias\"]` is `List[List[int]]`.";
    ];
  (* Alias within aliases and used in TypedDict subclass definition. *)
  assert_test_typed_dictionary
    {|
      from helpers import *
      from typing import List
      X = List[int]
      class OtherChild(Base):
        use_alias: X
        other_alias: List[X]
      Y = List[OtherChild]
      d: Y
      reveal_type(d[0]["other_alias"])
      reveal_type(d[0]["foo"])
    |}
    [
      "Revealed type [-1]: Revealed type for `d[0][\"other_alias\"]` is `List[List[int]]`.";
      "Revealed type [-1]: Revealed type for `d[0][\"foo\"]` is `int`.";
    ];
  (* Decorators that use a TypedDict subclass. *)
  assert_test_typed_dictionary
    {|
        from helpers import Child
        from typing import Callable, List
        def decorator(f: Callable[[int], str]) -> Callable[[Child], Child]: ...
        @decorator
        def foo(x: int) -> str: ...

        reveal_type(foo(1))
        d: int = foo(1)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo(1)` is `Child`.";
      "Incompatible variable type [9]: d is declared to have type `int` but is used as type \
       `Child`.";
      "Incompatible parameter type [6]: In anonymous call, for 1st positional only parameter \
       expected `Child` but got `int`.";
    ];
  ()


let test_check_optional_typed_dictionary context =
  assert_type_errors
    ~context
    {|
     from typing import Any, TypedDict, Optional

     class MyDict(TypedDict):
       a: str
       b: str


     def foo(b: bool) -> Optional[MyDict]:
       if b:
         return None
       return {"a": str(1), "b": "hi"}
     def bar(b: bool) -> Optional[MyDict]:
       if b:
         return None
       return {"a": str(1), "b": 0}
    |}
    [
      "TypedDict initialization error [55]: Expected type `str` for `MyDict` field `b` but got \
       `int`.";
    ];
  ()


let test_required_not_required_fields context =
  assert_type_errors_inject_typing_and_typing_extensions
    ~context
    {|
      from typing import Any, TypedDict, Optional
      from __TYPING_PLACEHOLDER import NotRequired

      class Movie(TypedDict):
        name: str
        year: NotRequired[int]

      def main(movie: Movie) -> None:
        x: str = movie["name"]
        y: int = movie["year"]

        movie2: Movie = { "name": "The Matrix", "year": 1999 }
        movie3: Movie = { "name": "The Matrix" }
    |}
    [];
  assert_type_errors_inject_typing_and_typing_extensions
    ~context
    {|
      from typing import Any, TypedDict, Optional
      from __TYPING_PLACEHOLDER import Required

      class MovieNonTotal(TypedDict, total=False):
        name: Required[str]
        year: int

      def main(movie: MovieNonTotal) -> None:
        x: str = movie["name"]
        y: int = movie["year"]

        movie2: MovieNonTotal = { "name": "The Matrix", "year": 1999 }
        movie3: MovieNonTotal = { "year": 1999 }
    |}
    [
      "TypedDict initialization error [55]: Missing required field `name` for TypedDict \
       `MovieNonTotal`.";
    ];
  ()


let () =
  "typed_dictionary"
  >::: [
         "check_typed_dictionaries" >:: test_check_typed_dictionaries;
         "check_typed_dictionary_inference" >:: test_check_typed_dictionary_inference;
         "check_typed_dictionary_inheritance" >:: test_check_typed_dictionary_inheritance;
         "check_typed_dictionary_in_alias" >:: test_check_typed_dictionary_in_alias;
         "check_optional_typed_dictionary" >:: test_check_optional_typed_dictionary;
         "required_not_required_fields" >:: test_required_not_required_fields;
       ]
  |> Test.run
