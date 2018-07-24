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
    []


let () =
  "plugin_utility">:::[
    "filter_classes">::test_filter_classes;
  ]
  |> run_test_tt_main
