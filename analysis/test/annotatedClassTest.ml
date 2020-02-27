(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Statement
open Test
module Class = Annotated.Class
module Attribute = Annotated.Attribute

let last_statement_exn = function
  | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
  | _ -> failwith "Could not parse last statement"


let test_fallback_attribute context =
  let assert_fallback_attribute ~name source annotation =
    let { ScratchProject.BuiltGlobalEnvironment.ast_environment; global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let global_resolution = GlobalResolution.create global_environment in
    let resolution = TypeCheck.resolution global_resolution () in
    let attribute =
      let source =
        AstEnvironment.ReadOnly.get_source
          (AstEnvironment.read_only ast_environment)
          (Reference.create "test")
      in
      let source = Option.value_exn source in
      last_statement_exn source
      |> Node.value
      |> (function
           | Statement.Class definition -> ClassSummary.create definition
           | _ -> failwith "Last statement was not a class")
      |> ClassSummary.name
      |> Reference.show
      |> Class.fallback_attribute ~resolution ~name
    in
    match annotation with
    | None -> assert_is_none attribute
    | Some annotation ->
        assert_is_some attribute;
        let attribute = Option.value_exn attribute in
        assert_equal
          ~cmp:Type.equal
          ~printer:Type.show
          annotation
          (Attribute.annotation attribute |> Annotation.annotation)
  in
  assert_fallback_attribute ~name:"attribute" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def Foo.__getattr__(self, attribute: str) -> int:
          return 1
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def Foo.__getattr__(self, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"attribute"
    {|
      class Foo:
        def Foo.__getattr__(self, attribute: str) -> int: ...
      class Bar(Foo):
        pass
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def Foo.__add__(self, other: Foo) -> int:
          pass
    |}
    (Some (parse_callable "typing.Callable('test.Foo.__add__')[[Named(other, test.Foo)], int]"));
  assert_fallback_attribute ~name:"__iadd__" {|
      class Foo:
        pass
    |} None;
  assert_fallback_attribute
    ~name:"__iadd__"
    {|
      class Foo:
        def Foo.__getattr__(self, attribute) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"foo"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def Foo.__getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    ~name:"bar"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def Foo.__getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.string);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['foo']) -> int: ...
        @overload
        def Foo.__getattr__(self, attribute: typing_extensions.Literal['bar']) -> str: ...
        @overload
        def Foo.__getattr__(self, attribute: str) -> None: ...
    |}
    (Some Type.none);
  assert_fallback_attribute
    ~name:"baz"
    {|
      from typing import overload
      import typing_extensions
      class Foo:
        @overload
        def Foo.__getattr__(self: Foo, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  ()


let test_overrides context =
  let resolution =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        pass
      class Baz(Bar):
        def foo(): pass
        def baz(): pass
    |}
        );
      ]
    |> ScratchProject.build_global_resolution
  in
  assert_is_none (Class.overrides "test.Baz" ~resolution ~name:"baz");
  let overrides = Class.overrides "test.Baz" ~resolution ~name:"foo" in
  assert_is_some overrides;
  assert_equal ~cmp:String.equal (Attribute.name (Option.value_exn overrides)) "foo";
  assert_equal (Option.value_exn overrides |> Attribute.parent) "test.Foo"


let () =
  "class"
  >::: ["fallback_attribute" >:: test_fallback_attribute; "overrides" >:: test_overrides]
  |> Test.run
