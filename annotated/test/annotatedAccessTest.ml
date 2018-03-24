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

  let ((module Handler: Environment.Handler) as environment) =
    populate {|
      class Foo:
        bar : int
        zod = ""
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
    ];

  let foo_class = Option.value_exn (Handler.class_definition (AnalysisType.primitive "Foo")) in
  Class.create foo_class |> ignore;
  let unannotated_attribute =
    Access.Element.Attribute {
      Attribute.name = Ast.Expression.Access (Expression.Access.create "zod");
      parent = Class.create foo_class;
      annotation = (Annotation.create_immutable ~original:(Some Type.Top) ~global:true Type.string);
      location = {
        Location.path = "test.py";
        start = { Location.line = 4; column = 2 };
        stop = { Location.line = 4; column = 5 };
      };
      value = Some {
          Node.location = {
            Location.path = "test.py";
            start = { Location.line = 3; column = 9 };
            stop = { Location.line = 3; column = 10 };
          };
          value = Ast.Expression.String ""
        };
      defined = true;
      class_attribute = true;
      async = false;
    }
  in
  assert_fold
    ~environment
    (Expression.Access.create "foo.zod")
    [
      Annotation.create_immutable ~global:true ~original:(Some Type.Top) Type.string,
      unannotated_attribute;
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
  assert_equal ~printer:Type.show ~cmp:Type.equal expected resolved


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
  let assert_resolved access annotation =
    assert_resolved
      [
        parse
          ~qualifier:(Expression.Access.create "module")
          {|
            _K = typing.TypeVar('_K')
            _V = typing.TypeVar('_V')

            class module.Call(typing.Generic[_K, _V]):
              generic_callable: typing.Callable[[_K], _V]
              def __call__(self) -> _V: ...

            module.call: module.Call[int, str] = ...
            module.meta: typing.Type[module.Call[int, str]] = ...
            module.callable: typing.Callable[..., int][..., str] = ...
          |};
      ]
      access
      (Type.create ~aliases:(fun _ -> None) (parse_single_expression annotation))
  in

  assert_resolved "module.call" "module.Call[int, str]";
  assert_resolved "module.call.generic_callable" "typing.Callable[[int], str]";
  assert_resolved "module.call()" "str";
  assert_resolved "module.callable()" "int";

  assert_resolved "module.meta" "typing.Type[module.Call[int, str]]";
  assert_resolved "module.meta()" "module.Call[$bottom, $bottom]"


let () =
  "access">:::[
    "fold">::test_fold;
    "module_exports">::test_module_exports;
    "object_callables">::test_object_callables;
  ]
  |> run_test_tt_main
