(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement
open Pyre

open Test



module Assign = Annotated.Assign
module Class = Annotated.Class
module Define = Annotated.Define
module Attribute = Annotated.Attribute
module Method = Annotated.Method
module Access = Annotated.Access


let configuration = Configuration.create ()


let populate source =
  let environment =
    let environment = Environment.Builder.create ~configuration () in
    Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      [parse source];
    environment
  in
  Environment.handler ~configuration environment


let resolution environment =
  Environment.resolution environment ()


let value option =
  Option.value_exn option


let variable name =
  Type.Variable { Type.variable = Identifier.create name; constraints = [] }


let test_return_annotation _ =
  let assert_return_annotation return_annotation async expected =
    let return_annotation =
      let environment =
        populate {|
          class foo():
            def bar(): pass
        |}
      in
      {
        Statement.Define.name = Expression.Access.create "derp";
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async;
        generated = false;
        parent = None;
      }
      |> Define.create
      |> Define.return_annotation ~resolution:(resolution environment)
    in
    assert_equal ~cmp:Type.equal expected return_annotation
  in
  assert_return_annotation (Some (Type.expression Type.integer)) false Type.integer;
  assert_return_annotation (Some (Type.expression Type.integer)) true (Type.awaitable Type.integer)


let test_parent_definition _ =
  let parent_class_definition environment name parent =
    {
      Statement.Define.name = Expression.Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = parent >>| Expression.Access.create;
    }
    |> Define.create
    |> Define.parent_definition ~resolution:(resolution environment)
  in

  let environment =
    populate {|
      class foo():
        def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
    |> value
  in
  assert_equal (Class.name parent) (Expression.Access.create "foo");

  let environment =
    populate {|
      def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
  in
  assert_is_none parent;

  let environment =
    populate {|
      class superfoo(): ...
      class foo(superfoo):
        def bar(): pass
    |} in
  let parent =
    parent_class_definition environment "bar" (Some "foo")
    |> value
  in
  let base_type =
    match (List.hd (Class.bases parent)) with
    | Some {Argument.value; _ } ->
        resolution environment
        |> (fun resolution -> Resolution.parse_annotation resolution value)
    | _ -> Type.Top
  in
  assert_equal (Class.name parent) (Expression.Access.create "foo");
  assert_equal base_type (Type.Primitive ~~"superfoo")


let test_method_definition _ =
  let parent_class_definition environment name parent =
    {
      Statement.Define.name = Expression.Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = parent >>| Expression.Access.create;
    }
    |> Define.create
    |> Define.method_definition ~resolution:(resolution environment)
  in
  assert_is_some
    (parent_class_definition
       (populate {|
        class Foo():
          def far(): pass
       |})
       "foo"
       (Some "Foo"));
  assert_is_none (parent_class_definition (populate "") "foo" None)


let test_parameter_annotations _ =
  let resolution =
    populate {|
      class foo():
        def bar(): pass
    |}
    |> resolution
  in
  let define parameters =
    {
      Statement.Define.name = Expression.Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
    |> Annotated.Define.create
  in
  let create_parameter ?annotation name = +{
    Parameter.name = Identifier.create name;
    value = None;
    annotation;
  }
  in
  let definition = define [
      create_parameter ~annotation:(Type.expression Type.integer) "a";
      create_parameter "b";
    ]
  in
  let parameter_map = Annotated.Define.parameter_annotations definition ~resolution in
  assert_equal (Map.find_exn parameter_map ~~"a") Type.integer;
  assert_equal (Map.find_exn parameter_map ~~"b") Type.Top;
  assert_equal (Map.find parameter_map ~~"c") None


let test_infer_argument_name _ =
  let create_parameter ?annotation name = +{
    Parameter.name = ~~name;
    value = None;
    annotation;
  }
  in
  let parameters = [
    create_parameter ~annotation:(Type.expression Type.integer) "a";
    create_parameter "b";
    create_parameter ~annotation:(Type.expression Type.string) "*c";
  ]
  in
  let create_integer name = {
    Argument.name;
    value = +(Ast.Expression.Integer 2);
  }
  in
  let define parameters =
    {
      Statement.Define.name = Expression.Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
    |> Annotated.Define.create
  in
  let infer_name = Annotated.Define.infer_argument_name (define parameters) in
  let unnamed = create_integer None in
  let named = create_integer (Some ~~"z") in
  assert_equal (infer_name ~index:0 ~argument:unnamed) (Some ~~"a");
  assert_equal (infer_name ~index:1 ~argument:unnamed) (Some ~~"b");
  assert_equal (infer_name ~index:2 ~argument:(create_integer None)) (Some ~~"*c");
  assert_equal (infer_name ~index:3 ~argument:(create_integer None)) (Some ~~"*c");

  assert_equal (infer_name ~index:0 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:1 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:2 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:3 ~argument:named) (Some ~~"z");

  let parameters = [ create_parameter ~annotation:(Type.expression Type.integer) "a" ] in
  let infer_name = Annotated.Define.infer_argument_name (define parameters) in
  assert_equal (infer_name ~index:0 ~argument:unnamed) (Some ~~"a");
  assert_equal (infer_name ~index:1 ~argument:unnamed) None;
  assert_equal (infer_name ~index:0 ~argument:named) (Some ~~"z");
  assert_equal (infer_name ~index:1 ~argument:named) (Some ~~"z")


let test_backup _ =
  let assert_backup call expected =
    let actual = Annotated.Call.create ~kind:Annotated.Call.Function call in
    let expected = expected >>| Annotated.Call.create ~kind:Annotated.Call.Function in
    assert_equal (Annotated.Call.backup actual) expected
  in
  assert_backup
    { Expression.Call.name = !"name"; arguments = [] }
    None;
  assert_backup
    { Expression.Call.name = !"__add__"; arguments = [] }
    (Some { Expression.Call.name = !"__radd__"; arguments = [] });
  assert_backup
    { Expression.Call.name = !"__sub__"; arguments = [] }
    (Some { Expression.Call.name = !"__rsub__"; arguments = [] })


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


let () =
  "define">:::[
    "return_annotation">::test_return_annotation;
    "parent_definition">::test_parent_definition;
    "method_definition">::test_method_definition;
    "infer_argument_name">::test_infer_argument_name;
    "parameter_annotations">::test_parameter_annotations;
  ]
  |> run_test_tt_main;
  "call">:::[
    "backup">::test_backup;
  ]
  |> run_test_tt_main;
  "access">:::[
    "fold">::test_fold;
  ]
  |> run_test_tt_main
