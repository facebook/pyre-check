(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement

open Test
open AnnotatedTest

module Access = Annotated.Access
module Class = Annotated.Class
module Attribute = Annotated.Attribute


let test_fold _ =
  let assert_fold ~environment access expected =
    let equal left right =
      match (snd left), (snd right) with
      | Access.Element.Call _, Access.Element.Call _
      | Access.Element.Attribute _, Access.Element.Attribute _
      | Access.Element.Value, Access.Element.Value
      | Access.Element.Method _, Access.Element.Method _ -> Annotation.equal (fst left) (fst right)
      | _, _ -> false
    in
    let printer elements =
      let print element =
        match snd element with
        | Access.Element.Call _ -> "(" ^ (Annotation.show (fst element)) ^ ", Call)"
        | Access.Element.Attribute _ -> "(" ^ (Annotation.show (fst element)) ^ ", Attribute)"
        | Access.Element.Value -> "(" ^ (Annotation.show (fst element)) ^ ", Value)"
        | Access.Element.Method _ -> "(" ^ (Annotation.show (fst element)) ^ ", Method) "
      in
      List.map ~f:print elements
      |> String.concat ~sep:"\n"
    in
    let fold_results access environment =
      let accumulate_returns accumulator ~annotations:_ ~resolved ~element =
        (resolved, element) :: accumulator
      in
      Access.fold
        ~resolution:(resolution environment)
        ~initial:[]
        ~f:accumulate_returns
        (Access.create access)
    in
    assert_equal
      ~cmp:(List.equal ~equal)
      ~printer
      expected
      (fold_results access environment)
  in

  assert_fold
    ~environment:(populate "")
    (Expression.Access.create "foo")
    [
    ];

  let environment =
    populate {|
      class Foo:
        bar : int
      foo : Foo
    |}
  in
  let mock_class =
    {
      Statement.Class.name = Expression.Access.create "";
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
    |> Node.create_with_default_location
    |> Class.create
  in
  let defined_attribute =
    Access.Element.Attribute {
      Attribute.name = Ast.Expression.Access (Expression.Access.create "");
      parent = mock_class;
      annotation = (Annotation.create_immutable ~global:true Type.Top);
      location = Location.any;
      value = None;
      defined = true;
      class_attribute = false;
      async = false;
    }
  in
  assert_fold
    ~environment
    (Expression.Access.create "foo.bar")
    [
      Annotation.create_immutable ~global:true Type.integer, defined_attribute;
      Annotation.create_immutable ~global:true (Type.Primitive ~~"Foo"), Access.Element.Value;
    ];

  let undefined_attribute =
    Access.Element.Attribute {
      Attribute.name = Ast.Expression.Access (Expression.Access.create "");
      parent = mock_class;
      annotation = (Annotation.create_immutable ~global:true Type.Top);
      location = Location.any;
      value = None;
      defined = false;
      class_attribute = false;
      async = false;
    }
  in
  assert_fold
    ~environment
    (Expression.Access.create "foo.baz")
    [
      Annotation.create Type.Top, undefined_attribute;
      Annotation.create_immutable ~global:true (Type.Primitive ~~"Foo"), Access.Element.Value;
    ]


let assert_resolved sources access expected =
  let resolution =
    populate_with_sources sources
    |> resolution
  in
  let resolved =
    Access.fold
      ~resolution
      ~initial:Type.Top
      ~f:(fun _ ~annotations:_ ~resolved ~element:_ -> Annotation.annotation resolved)
      (Access.create (parse_single_access access))
  in
  assert_equal ~cmp:Type.equal expected resolved


let test_module_exports _ =
  let assert_resolved =
    assert_resolved
      [
        parse
          ~qualifier:(Expression.Access.create "implementing")
          {|
            def implementing.function() -> int: ...
            implementing.constant: int = 1
          |};
        parse
          ~qualifier:(Expression.Access.create "exporting")
          {|
            from implementing import function, constant
            from implementing import function as aliased
          |};
      ]
  in

  assert_resolved "implementing.constant" Type.integer;
  assert_resolved "implementing.function()" Type.integer;
  assert_resolved "implementing.undefined" Type.Top;

  assert_resolved "exporting.constant" Type.integer;
  assert_resolved "exporting.function()" Type.integer;
  assert_resolved "exporting.aliased()" Type.integer;
  assert_resolved "exporting.undefined" Type.Top


let test_object_callables _ =
  let assert_resolved =
    assert_resolved
      [
        parse
          ~qualifier:(Expression.Access.create "module")
          {|
            class module.Call:
              def __call__(self) -> int: ...

            module.call: module.Call = ...
          |};
      ]
  in

  assert_resolved "module.call" (Type.primitive "module.Call");
  assert_resolved "module.call()" Type.integer


let () =
  "access">:::[
    "fold">::test_fold;
    "module_exports">::test_module_exports;
    "object_callables">::test_object_callables;
  ]
  |> run_test_tt_main
