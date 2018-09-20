(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Statement
open Plugin

open Test


let test_filter_classes _ =
  let assert_filter source class_filter expected =
    let assert_class_equal source expected =
      assert_equal
        ~printer:Class.show
        ~cmp:Class.equal
        (Node.value source)
        (parse_single_class expected)
    in
    List.iter2_exn
      ~f:assert_class_equal
      (Filter.filter_classes ~class_filter (Source.statements (parse source)))
      expected
  in
  assert_filter
    {|
      class Foo:
        ...
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~name:(Some "Foo") ())
    [{|
      class Foo:
        ...
    |}];

  assert_filter
    {|
      class Foo:
        ...
      class Boo(Foo):
        ...
    |}
    (Filter.create_class_filter ~bases:(Some ["Foo"]) ())
    [{|
      class Boo(Foo):
        ...
    |}];
  assert_filter
    {|
      class Boo(A, B, C):
        ...
    |}
    (Filter.create_class_filter ~bases:(Some ["A"; "B"; "C";]) ())
    [{|
      class Boo(A, B, C):
        ...
    |}];
  assert_filter
    {|
      class Boo(A1, B2, C3):
        ...
    |}
    (Filter.create_class_filter ~bases:(Some ["C3"; "A1"; "B2";]) ())
    [{|
      class Boo(A1, B2, C3):
        ...
    |}];

  assert_filter
    {|
      @dataclass
      class Foo:
        ...
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~decorator:(Some "dataclass") ())
    [{|
      @dataclass
      class Foo:
        ...
    |}];

  assert_filter
    {|
      class Foo:
        """I am a docstring"""
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~docstring:(Some "I am a docstring") ())
    [{|
      class Foo:
        """I am a docstring"""
    |}];

  assert_filter
    {|
      class Foo:
        def foo():
          ...
      class Boo:
        def boo():
          ...
    |}
    (Filter.create_class_filter
       ~define_filter:(Some (Filter.create_define_filter ~name:(Some "foo") ()))
       ())
    [{|
      class Foo:
        def foo():
          ...
    |}];
  assert_filter
    {|
      class Foo:
        def foo():
          ...
      class Boo:
        def foo():
          ...
    |}
    (Filter.create_class_filter
       ~define_filter:
         (Some (Filter.create_define_filter ~name:(Some "foo") ~parent:(Some "Foo") ()))
       ())
    [{|
      class Foo:
        def foo():
          ...
    |}];

  assert_filter
    {|
      class Boo:
        ...
      @dataclass
      class Foo(Boo):
        """I am a docstring"""
        def __init__(self) -> None:
          pass
    |}
    (Filter.create_class_filter
       ~name:(Some "Foo")
       ~bases:(Some ["Boo"])
       ~decorator:(Some "dataclass")
       ~docstring:(Some "I am a docstring")
       ())
    [{|
      @dataclass
      class Foo(Boo):
        """I am a docstring"""
        def __init__(self) -> None:
          pass
    |}];

  assert_filter
    {|
      class Foo:
        ...
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~decorator:(Some "dataclass") ())
    [];
  assert_filter
    {|
      class Foo:
        ...
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~name:(Some "Coo") ())
    [];
  assert_filter
    {|
      class Boo(A, B, C):
        ...
    |}
    (Filter.create_class_filter ~bases:(Some ["Foo"]) ())
    [];
  assert_filter
    {|
      @dataclass
      class Foo:
        ...
      class Boo:
        ...
    |}
    (Filter.create_class_filter ~name:(Some "T") ~decorator:(Some "dataclass") ())
    [];
  assert_filter
    {|
      class Foo:
        def foo():
          ...
      class Boo:
        def boo():
          ...
    |}
    (Filter.create_class_filter
       ~define_filter:(Some (Filter.create_define_filter ~name:(Some "init") ()))
       ())
    []


let test_filter_defines _ =
  let assert_filter source define_filter expected =
    let assert_define_equal source expected =
      assert_equal
        ~printer:Define.show
        ~cmp:Define.equal
        (Node.value source)
        (parse_single_define expected)
    in
    List.iter2_exn
      ~f:assert_define_equal
      (Filter.filter_defines ~define_filter (Source.statements (parse source)))
      expected
  in
  assert_filter
    {|
      def foo():
        ...
      def boo():
        ...
    |}
    (Filter.create_define_filter ~name:(Some "foo") ())
    ["def foo():\n  ..."];

  assert_filter
    {|
      def foo():
        1
      def boo():
        2
    |}
    (Filter.create_define_filter ~name:(Some "foo") ())
    ["def foo():\n  1"];

  assert_filter
    {|
      @dataclass
      def foo():
        ...
      def boo():
        ...
    |}
    (Filter.create_define_filter ~decorator:(Some "dataclass") ())
    ["@dataclass\ndef foo():\n  ..."];

  assert_filter
    {|
      def foo():
        """I am a docstring"""
      def boo():
        """I am not a search bar"""
    |}
    (Filter.create_define_filter ~docstring:(Some "I am a docstring") ())
    [{|
      def foo():
        """I am a docstring"""
    |}];

  assert_filter
    {|
      @decorator
      def foo():
        """I am a docstring"""
      @decorator
      def boo():
        """I am not a search bar"""
    |}
    (Filter.create_define_filter
       ~name:(Some "foo")
       ~decorator:(Some "decorator")
       ~docstring:(Some "I am a docstring")
       ())
    [{|
      @decorator
      def foo():
        """I am a docstring"""
    |}];

  assert_filter
    {|
      def foo():
        ...
      def boo():
        ...
    |}
    (Filter.create_define_filter ~parent:(Some "T") ())
    [];
  assert_filter
    {|
      def foo():
        ...
      def boo():
        ...
    |}
    (Filter.create_define_filter ~name:(Some "init") ())
    [];
  assert_filter
    {|
      def foo():
        ...
      def boo():
        ...
    |}
    (Filter.create_define_filter ~decorator:(Some "decorator") ())
    []


let test_filter_assigns _ =
  let assert_filter source assign_filter expected =
    let assert_assign_equal source expected =
      assert_equal
        ~printer:Assign.show
        ~cmp:Assign.equal
        (Node.value source)
        (parse_single_assign expected)
    in
    List.iter2_exn
      ~f:assert_assign_equal
      (Filter.filter_assigns ~assign_filter (Source.statements (parse source)))
      expected
  in
  assert_filter
    {|
      x = 1
      y = 1
    |}
    (Filter.create_assign_filter ~target:(Some "x") ())
    ["x = 1"];

  assert_filter
    {|
      x: int = 3
      y: str = "foo"
      z: int = 4
    |}
    (Filter.create_assign_filter ~annotation:(Some "int") ())
    ["x: int = 3"; "z: int = 4"];

  assert_filter
    {|
      x = a.b.c
      y = a.b
    |}
    (Filter.create_assign_filter ~value_regexp:(Some "a.b*") ())
    ["x = a.b.c"; "y = a.b"];

  assert_filter
    {|
      Boo = typing.NotRelated('Boo', ['one', 'two'])
      Boo = typing.NamedTuple('Boo', ['one', 'two'])
    |}
    (Filter.create_assign_filter ~value_regexp:(Some "typing.NamedTuple*") ())
    ["Boo = typing.NamedTuple('Boo', ['one', 'two'])"];

  assert_filter
    {|
      Coo = collections.namedtuple('Coo', 'a b c')
      Foo = collections.namedtuple('Foo', 'a b c')
    |}
    (Filter.create_assign_filter ~value_regexp:(Some "collections.namedtuple*") ())
    [
      "Coo = collections.namedtuple('Coo', 'a b c')";
      "Foo = collections.namedtuple('Foo', 'a b c')";
    ];

  (* Assume that the user of this API knows the pretty print format of expressions *)
  assert_filter
    {|
      x = 1 + 2
      y = 2
    |}
    (Filter.create_assign_filter ~value_regexp:(Some ".*__add__.*") ())
    ["x = 1 + 2"];

  assert_filter
    {|
      x = a[2]
    |}
    (Filter.create_assign_filter ~value_regexp:(Some "a\\[.*\\]") ())
    ["x = a[2]"];

  assert_filter
    "x : int = 3"
    (Filter.create_assign_filter ~parent:(Some "Foo") ())
    []


let () =
  "plugin_utility">:::[
    "filter_classes">::test_filter_classes;
    "filter_defines">::test_filter_defines;
    "filter_assign">::test_filter_assigns;
  ]
  |> Test.run
