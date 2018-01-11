(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Statement
open Pyre

open Test


module Assign = Annotated.Assign
module Class = Annotated.Class
module Define = Annotated.Define
module Attribute = Annotated.Attribute
module Method = Annotated.Method
module Access = Annotated.Access


let populate source =
  let environment =
    let environment = Environment.Builder.create () in
    Environment.populate (Environment.reader environment) [parse source];
    environment
  in
  Environment.reader environment


let resolution environment =
  Environment.resolution environment ()


let value option =
  Option.value_exn option


let variable name =
  Type.Variable { Type.variable = name; constraints = [] }


let test_assign_fold _ =
  let resolution =
    populate {|
      i = 1
      s = 'asdf'
      t = 1, 1.0
    |}
    |> resolution
  in
  let assert_fold source expected =
    let assign =
      match parse_single_statement source with
      | { Node.value = Statement.Assign assign; _ } -> assign
      | _ -> failwith "No Assign to parse"
    in
    let single_assignments ~access:{ Node.value = access; _ } ~value_annotation assignments =
      (Expression.Access.show access, value_annotation) :: assignments
    in
    let actual =
      Assign.create assign
      |> Assign.fold
        ~resolution
        ~f:single_assignments
        ~initial:[]
      |> List.rev
    in
    assert_equal actual expected
  in

  assert_fold "a = i" ["a", Type.integer];
  assert_fold "a, b = i, s" ["a", Type.integer; "b", Type.string];
  assert_fold "a, b = t" ["a", Type.integer; "b", Type.float];
  assert_fold "a, b = unknown" []


let test_method_overrides _ =
  let resolution =
    populate {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        pass
      class Baz(Bar):
        def foo(): pass
        def baz(): pass
    |}
    |> resolution
  in
  let foo, baz =
    Resolution.class_definition resolution (Type.Primitive ~~"Baz")
    >>| Class.create
    >>| Class.methods
    >>| (fun methods ->
        match methods with
        | [foo; bar] -> foo, bar
        | _ -> failwith "Could not find `foo` and `bar`")
    |> value
  in

  assert_is_none (Method.overrides ~resolution baz);
  let overrides = Method.overrides ~resolution foo in
  assert_is_some overrides;
  assert_equal
    ~cmp:Expression.Access.equal
    (Method.parent (Option.value_exn overrides) |> Class.name)
    (Expression.Access.create "Foo")


let test_method_implements _ =
  let resolution =
    populate {|
      class foo():
        first : int
        second : int
      foo : foo
    |}
    |> resolution
  in
  let definition ?(parameters = []) ?return_annotation name =
    Method.create
      ~define:{
        Statement.Define.name = Expression.Access.create name;
        parameters;
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async = false;
        generated = false;
        parent = Some (Expression.Access.create "Parent");
      }
      ~parent:(Class.create
        (Node.create
          {
            Statement.Class.name = Expression.Access.create "Parent";
            bases = [];
            body = [+Pass];
            decorators = [];
            docstring = None;
          }))
  in

  assert_true
    (Method.implements
       ~resolution
       ~protocol_method:(definition "match")
       (definition "match"));
  assert_false
    (Method.implements
       ~resolution
       ~protocol_method:(definition "mismatch")
       (definition "match"));

  let parameters =
    [
      Parameter.create ~name:(~~"a") ();
      Parameter.create ~name:(~~"b") ();
    ]
  in
  assert_true
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~parameters "match")
       (definition ~parameters "match"));

  (* Naming of parameters doesn't matter. *)
  let definition_parameters =
    [
      Parameter.create ~name:(~~"a") ();
      Parameter.create ~name:(~~"b") ();
    ]
  in
  let protocol_parameters =
    [
      Parameter.create ~name:(~~"a") ();
      Parameter.create ~name:(~~"c") ();
    ]
  in
  assert_true
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~parameters:protocol_parameters "match")
       (definition ~parameters:definition_parameters "match"));

  (* Number of parameters, parameter and return annotations matter. *)
  let definition_parameters =
    [
      Parameter.create ~name:(~~"a") ();
      Parameter.create ~name:(~~"b") ();
    ]
  in
  let protocol_parameters =
    [
      Parameter.create ~name:(~~"a") ();
      Parameter.create ~name:(~~"c") ();
      Parameter.create ~name:(~~"z") ();
    ]
  in
  assert_false
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~parameters:protocol_parameters "match")
       (definition ~parameters:definition_parameters "match"));

  let definition_parameters = [Parameter.create ~name:(~~"a") ~annotation:!"int" ()] in
  let protocol_parameters = [Parameter.create ~name:(~~"a") ()] in
  assert_false
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~parameters:protocol_parameters "match")
       (definition ~parameters:definition_parameters "match"));

  assert_true
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~return_annotation:!"int" "match")
       (definition ~return_annotation:!"int" "match"));
  assert_false
    (Method.implements
       ~resolution
       ~protocol_method:(definition ~return_annotation:!"int" "match")
       (definition ~return_annotation:!"float" "match"))


let test_generics _ =
  let assert_generics source generics =
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ } ->
        let resolution = populate source |> resolution in
        assert_equal
          ~cmp:(List.equal ~equal:Type.equal)
          (Class.create (Node.create definition) |> Class.generics ~resolution)
          generics
    | _ ->
        assert_unreached ()
  in
  assert_generics "class Foo(): pass" [];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]): pass
    |}
    [variable ~~"_T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      class Foo(typing.Generic[_T, _S]): pass
    |}
    [variable ~~"_T"; variable ~~"_S"]


let test_superclasses _ =
  let assert_superclasses result expected =
    let equal left right = Expression.Access.equal (Class.name left) (Class.name right) in
    assert_equal
      ~printer:(fun classes -> Format.asprintf "%a" Sexp.pp (sexp_of_list Class.sexp_of_t classes))
      ~cmp:(List.equal ~equal) result expected
  in
  let environment =
    populate {|
      class object: pass
      class Foo: pass
      class Bar: pass
      class SubFoo(Foo): pass
      class SubFooBar(Foo, Bar): pass
      class SubRecurse(SubFooBar): pass
      class SubRedundant(Foo, SubFooBar): pass
    |} in
  let (!) name =
    {
      Statement.Class.name = Expression.Access.create name;
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
    |> Node.create
    |> Class.create
  in

  assert_superclasses
    (Class.superclasses ~resolution:(resolution environment) !"Foo")
    ([!"object"]);
  assert_superclasses
    (Class.superclasses ~resolution:(resolution environment) !"SubFoo")
    ([!"Foo"; !"object"]);
  assert_superclasses
    (Class.superclasses ~resolution:(resolution environment) !"SubFooBar")
    ([!"Bar"; !"Foo"; !"object"]);
  assert_superclasses
    (Class.superclasses ~resolution:(resolution environment) !"SubRecurse")
    ([!"SubFooBar"; !"Bar"; !"Foo"; !"object"]);
  assert_superclasses
    (Class.superclasses ~resolution:(resolution environment) !"SubRedundant")
    ([!"SubFooBar"; !"Foo"; !"Bar"; !"object"])


type constructor = {
  name: Expression.Access.t;
  parameters: (Expression.t Parameter.t) list;
  annotation: Type.t option;
}


let test_constructors _ =
  let assert_constructors ?(is_generated = false) source constructors =
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ }
    | { Node.value = Statement.Stub (Stub.Class definition); _ } ->
        let resolution = populate source |> resolution in
        let defines =
          let define { name; parameters; annotation } =
            {
              Statement.Define.name;
              parameters;
              body = [+Pass];
              decorators = [];
              docstring = None;
              return_annotation = annotation >>| Type.expression;
              async = false;
              generated = is_generated;
              parent = Some definition.Statement.Class.name;
            }
          in
          List.map ~f:define constructors
        in
        let actuals =
          Node.create definition
          |> Class.create
          |> Class.constructors ~resolution
        in
        assert_equal
          ~cmp:(List.equal ~equal:Statement.Define.equal)
          ~printer:(fun constructors ->
              let constructors =
                List.map
                  ~f:(fun constructor -> Statement.Define.show constructor)
                  constructors in
              Format.asprintf "%a" Sexp.pp (sexp_of_list sexp_of_string constructors))
          defines
          actuals
    | _ ->
        assert_unreached ()
  in

  (* Undefined constructors. *)
  assert_constructors
    ~is_generated:true
    "class Foo: pass"
    [
      {
        name = Expression.Access.create "Foo";
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation = Some (Type.Primitive ~~"Foo")
      };
    ];
  assert_constructors
    ~is_generated:true
    "class Foo: ..."
    [
      {
        name = Expression.Access.create "Foo";
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation = Some (Type.Primitive ~~"Foo");
      };
    ];

  (* Statement.Defined constructors. *)
  assert_constructors
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
    |}
    [
      {
        name = Expression.Access.create "Foo";
        parameters = [
          Parameter.create ~name:~~"self" ();
          Parameter.create ~name:~~"a" ~annotation:(Type.expression Type.integer) ();
        ];
        annotation = Some (Type.Primitive ~~"Foo");
      };
    ];
  assert_constructors
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
        def __init__(self, b: str) -> None: pass
    |}
    [
      {
        name = Expression.Access.create "Foo";
        parameters = [
          Parameter.create ~name:~~"self" ();
          Parameter.create ~name:~~"a" ~annotation:(Type.expression Type.integer) ();
        ];
        annotation = Some (Type.Primitive ~~"Foo");
      };
      {
        name = Expression.Access.create "Foo";
        parameters = [
          Parameter.create ~name:~~"self" ();
          Parameter.create ~name:~~"b" ~annotation:(Type.expression Type.string) ();
        ];
        annotation = Some (Type.Primitive ~~"Foo");
      };
    ];

  (* Generic classes. *)
  assert_constructors
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        def __init__(self) -> None: pass
    |}
    [
      {
        name = Expression.Access.create "Foo";
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation =
          Some
            (Type.Parametric {
                Type.name = ~~"Foo";
                parameters = [variable ~~"_K"; variable ~~"_V"];
              });
      };
    ]


let test_methods _ =
  let assert_methods source methods =
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ }
    | { Node.value = Statement.Stub (Stub.Class definition); _ } ->
        let actuals =
          let method_name { Statement.Define.name; _ } =
            Expression.Access.show name
          in
          Node.create definition
          |> Class.create
          |> Class.methods
          |> List.map ~f:(fun definition -> Method.define definition |> method_name)
        in
        assert_equal
          methods
          actuals
    | _ ->
        assert_unreached ()
  in
  assert_methods "class A: pass" [];
  assert_methods
    {|
      class A:
        def foo(): pass
        def bar(): pass
        1 + 1
        def baz(): ...
    |}
    ["foo"; "bar"; "baz"]


let test_is_protocol _ =
  let assert_is_protocol bases expected =
    let is_protocol =
      {
        Statement.Class.name = Expression.Access.create "Derp";
        bases;
        body = [];
        decorators = [];
        docstring = None;
      }
      |> Node.create
      |> Class.create
      |> Class.is_protocol
    in
    assert_equal expected is_protocol
  in

  assert_is_protocol [] false;
  assert_is_protocol [{ Argument.name = None; value = !"derp" }] false;
  assert_is_protocol [{ Argument.name = None; value = !"typing.Protocol" }] true;
  assert_is_protocol
    [{ Argument.name = Some ~~"metaclass"; value = !"abc.ABCMeta" }]
    true


let test_implements _ =
  let assert_conforms definition protocol conforms =
    let resolution = populate definition |> resolution in
    match parse_last_statement definition with
    | { Node.value = Statement.Class definition; _ }
    | { Node.value = Statement.Stub (Stub.Class definition); _ } ->
        begin
          match parse_last_statement protocol with
          | { Node.value = Statement.Class protocol; _ }
          | { Node.value = Statement.Stub (Stub.Class protocol); _ } ->
              assert_equal
                (Class.implements
                   ~resolution
                   ~protocol:(Class.create (Node.create protocol))
                   (Class.create (Node.create definition)))
                conforms
          | _ ->
              assert_unreached ()
        end
    | _ ->
        assert_unreached ()
  in

  assert_conforms "class A: pass" "class Protocol: pass" true;
  assert_conforms
    {|
      class A:
        def foo(): pass
    |}
    {|
      class Protocol:
        def foo(): pass
    |}
    true;
  assert_conforms
    {|
      class A:
        def foo(): pass
    |}
    {|
      class Protocol:
        def bar(): pass
    |}
    false;
  assert_conforms
    {|
      class A:
        def foo(): pass
    |}
    {|
      class Protocol:
        def foo(): pass
        def bar(): pass
    |}
    false;
  assert_conforms
    {|
      class A:
        def foo(): pass
        def bar(): pass
        def baz(): pass
    |}
    {|
      class Protocol:
        def foo(): pass
        def bar(): pass
    |}
    true


let test_class_attributes _ =
  let resolution, parent =
    let source =
      {|
        foo: foo
        class foo():
          first: int
          second: int
          third: int = 1
      |}
    in
    let parent =
      match parse_last_statement source with
      | { Node.value = Class definition; _ } ->
          definition
      | _ ->
          failwith "Could not parse class"
    in
    populate source |> resolution,
    Class.create (Node.create parent)
  in

  let create_assign
      ?(value = None)
      ?(annotation = Some !"int")
      ?(parent = Some (Expression.Access.create "foo"))
      name =
    +{ Statement.Assign.target = !name; annotation; value; compound = None; parent }
  in

  (* Test `Class.attributes`. *)
  let assert_attributes definition attributes =
    let attribute_list_equal =
      let equal left right =
        Expression.equal_expression (Attribute.name left) (Attribute.name right) &&
        Annotation.equal (Attribute.annotation left) (Attribute.annotation right) &&
        Class.equal (Attribute.parent left) (Attribute.parent right)
      in
      List.equal ~equal
    in
    assert_equal
      ~cmp:attribute_list_equal
      ~printer:(fun attributes ->
          String.concat
            ~sep:"; "
            (List.map ~f:(Format.asprintf "%a" Annotated.Class.Attribute.pp) attributes))
      (Class.attributes ~resolution definition)
      attributes
  in
  assert_attributes
    parent
    [
      Attribute.create ~resolution ~parent (create_assign "first");
      Attribute.create ~resolution ~parent (create_assign "second");
      Attribute.create
        ~resolution
        ~parent
        (create_assign "third" ~value:(Some (+Expression.Integer 1)));
    ];


  (* Test `Attribute`. *)
  let attribute =
    Attribute.create
      ~resolution
      ~parent
      (create_assign ~annotation:(Some !"int") "first")
  in
  assert_equal
    (Attribute.name attribute)
    (Expression.Access (Expression.Access.create "first"));
  assert_equal
    (Attribute.annotation attribute)
    (Annotation.create_immutable ~global:true (Type.Primitive ~~"int"));


  (* Test `attribute_fold`. *)
  let callback
      string_names
      attribute =
    match Attribute.name attribute with
    | Expression.Access access -> string_names ^ (Expression.Access.show access)
    | _ -> string_names
  in
  assert_equal
    (Class.attribute_fold ~resolution ~initial:"" ~f:callback parent)
    ("firstsecondthird")



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
      | Access.Element.Array, Access.Element.Array
      | Access.Element.Call _, Access.Element.Call _
      | Access.Element.Expression, Access.Element.Expression
      | Access.Element.Attribute _, Access.Element.Attribute _
      | Access.Element.Global, Access.Element.Global
      | Access.Element.Identifier, Access.Element.Identifier
      | Access.Element.Method _, Access.Element.Method _ -> Annotation.equal (fst left) (fst right)
      | _, _ -> false
    in
    let printer elements =
      let print element =
        match snd element with
        | Access.Element.Array -> "(" ^ (Annotation.show (fst element)) ^ ", Array)"
        | Access.Element.Call _ -> "(" ^ (Annotation.show (fst element)) ^ ", Call)"
        | Access.Element.Expression ->
            "(" ^ (Annotation.show (fst element)) ^ ", Expression)"
        | Access.Element.Attribute _ -> "(" ^ (Annotation.show (fst element)) ^ ", Attribute)"
        | Access.Element.Global -> "(" ^ (Annotation.show (fst element)) ^ ", Global)"
        | Access.Element.Identifier ->
            "(" ^ (Annotation.show (fst element)) ^ ", Identifier) "
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
      Annotation.create Type.Top, Access.Element.Global;
      Annotation.create Type.Top, Access.Element.Global;
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
    |> Node.create
    |> Class.create
  in
  let defined_attribute =
    Access.Element.Attribute (Access.Element.Defined {
      Attribute.name = Ast.Expression.Access (Expression.Access.create "");
      parent = mock_class;
      annotation = (Annotation.create_immutable ~global:true Type.Top);
      location = Location.any;
      value = None;
      defined = true;
    })
  in
  assert_fold
    ~environment
    (Expression.Access.create "foo.bar")
    [
      Annotation.create_immutable ~global:true Type.integer, defined_attribute;
      Annotation.create_immutable ~global:true (Type.Primitive ~~"Foo"), Access.Element.Global;
      Annotation.create Type.Top, Access.Element.Global;
    ];

  let undefined_attribute =
    Access.Element.Attribute
      (Access.Element.Undefined {
        Access.Element.name = Expression.Access.create "baz";
        parent = mock_class;
      })
  in
  assert_fold
    ~environment
    (Expression.Access.create "foo.baz")
    [
      Annotation.create Type.Top, undefined_attribute;
      Annotation.create_immutable ~global:true (Type.Primitive ~~"Foo"), Access.Element.Global;
      Annotation.create Type.Top, Access.Element.Global;
    ]


let () =
  "assign">:::[
    "fold">::test_assign_fold;
  ]
  |> run_test_tt_main;
  "method">:::[
    "overrides">::test_method_overrides;
    "implements">::test_method_implements;
  ]
  |> run_test_tt_main;
  "class">:::[
    "generics">::test_generics;
    "superclasses">::test_superclasses;
    "constructors">::test_constructors;
    "methods">::test_methods;
    "is_protocol">::test_is_protocol;
    "implements">::test_implements;
    "attributes">::test_class_attributes;
  ]
  |> run_test_tt_main;
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
