(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Plugin
open Test

let test_transform_ast _ =
  let assert_expand ?(handle = "qualifier.py") source expected =
    let parse source = parse source ~handle |> Analysis.Preprocessing.preprocess in
    assert_source_equal (parse expected) (NamedTuples.transform_ast (parse source))
  in
  assert_expand
    {|
      T = typing.NamedTuple('T')
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls) -> typing.NamedTuple: ...
        _fields: typing.Tuple[()] = ()
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', ['a'])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, a: typing.Any) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str] = ('a',)
        a: typing.Any
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', ['one', 'two'])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, one: typing.Any, two: typing.Any) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Any
        two: typing.Any
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: int
        two: str
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', 'a b c')
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(
          cls,
          a: typing.Any,
          b: typing.Any,
          c: typing.Any) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str, str, str] = ('a', 'b', 'c')
        a: typing.Any
        b: typing.Any
        c: typing.Any
    |};
  assert_expand
    {|
      class Foo(Bar, collections.namedtuple('T', ['one', 'two'])):
        three: int = 1
    |}
    {|
      class Foo(Bar, typing.NamedTuple):
        def __new__(cls, one: typing.Any, two: typing.Any) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Any
        two: typing.Any
        three: int = 1
    |};
  assert_expand
    {|
      class Foo(typing.NamedTuple):
        a: int
        b: str
        c: int = 3
    |}
    {|
      class Foo(typing.NamedTuple):
        def __new__(cls, a: int, b: str, c: int = 3) -> typing.NamedTuple: ...
        _fields: typing.Tuple[str, str, str] = ('a', 'b', 'c')
        a: int
        b: str
        c: int = 3
    |};
  assert_expand
    {|
      class Foo(collections.namedtuple("PatchDocument", ("op", "path", "value", "ts", "lazy"))):
        pass
    |}
    {|
      class Foo(typing.NamedTuple):
         def __new__(
           cls,
           op: typing.Any,
           path: typing.Any,
           value: typing.Any,
           ts: typing.Any,
           lazy: typing.Any) -> typing.NamedTuple:
           ...
         _fields: typing.Tuple[str, str, str, str, str] = ('op', 'path', 'value', 'ts', 'lazy')
         op: typing.Any
         path: typing.Any
         value: typing.Any
         ts: typing.Any
         lazy: typing.Any
         pass
    |};
  assert_expand
    {|
      class Foo:
        T = collections.namedtuple('T', ("a", "b"))
    |}
    {|
      class Foo:
        class T(typing.NamedTuple):
          def __new__(cls, a: typing.Any, b: typing.Any) -> typing.NamedTuple: ...
          _fields: typing.Tuple[str, str] = ('a', 'b')
          a: typing.Any
          b: typing.Any
    |};
  assert_expand
    {|
      def foo():
        T = typing.NamedTuple('T')
    |}
    {|
      def foo():
        class T(typing.NamedTuple):
          def __new__(cls) -> typing.NamedTuple: ...
          _fields: typing.Tuple[()] = ()
    |};
  assert_expand
    {|
      class Foo:
        def __new__(cls) -> typing.NamedTuple:
          cls.t = typing.NamedTuple('T', 'a')
    |}
    {|
      class Foo:
        def __new__(cls) -> typing.NamedTuple:
          cls.t = typing.NamedTuple('T', 'a')
    |};
  assert_expand
    {|
      class Foo(collections.namedtuple('T', ['one', 'two'])):
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |}
    {|
      class Foo(typing.NamedTuple):
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Any
        two: typing.Any
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |}


let () = "plugin_named_tuples" >::: ["transform_ast" >:: test_transform_ast] |> Test.run
