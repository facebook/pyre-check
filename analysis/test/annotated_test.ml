(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Statement
open Pyre

open Test


module Class = Annotated.Class
module Define = Annotated.Define
module Field = Annotated.Field
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
    |> (fun values -> Option.value_exn values)
  in

  assert_is_none (Method.overrides ~resolution baz);
  let overrides = Method.overrides ~resolution foo in
  assert_is_some overrides;
  assert_equal
    ~cmp:Instantiated.Access.equal
    (Method.parent (Option.value_exn overrides) |> Class.name)
    (Instantiated.Access.create "Foo")


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
        Statement.Define.name = Instantiated.Access.create name;
        parameters;
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async = false;
        parent = Some (Instantiated.Access.create "Parent");
      }
      ~parent:(Class.create {
          Statement.Class.name = Instantiated.Access.create "Parent";
          bases = [];
          body = [+Pass];
          decorators = [];
          docstring = None;
        })
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
          (Class.create definition |> Class.generics ~resolution)
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
    let equal left right = Instantiated.Access.equal (Class.name left) (Class.name right) in
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
      Statement.Class.name = Instantiated.Access.create name;
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
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
  name: Expression.access;
  parameters: (Expression.t Parameter.t) list;
  annotation: Type.t option;
}


let test_constructors _ =
  let assert_constructors source constructors =
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
              parent = Some definition.Statement.Class.name;
            }
          in
          List.map ~f:define constructors
        in
        let actuals =
          Class.create definition
          |> Class.constructors ~resolution
        in
        assert_equal
          ~cmp:(List.equal ~equal:(Statement.Define.equal Statement.equal))
          ~printer:(fun constructors ->
              let constructors =
                List.map
                  ~f:(fun constructor -> Statement.Define.show Statement.pp constructor)
                  constructors in
              Format.asprintf "%a" Sexp.pp (sexp_of_list sexp_of_string constructors))
          defines
          actuals
    | _ ->
        assert_unreached ()
  in

  (* Undefined constructors. *)
  assert_constructors
    "class Foo: pass"
    [
      {
        name = Instantiated.Access.create "Foo";
        parameters = [];
        annotation = Some (Type.Primitive ~~"Foo")
      };
    ];
  assert_constructors
    "class Foo: ..."
    [
      {
        name = Instantiated.Access.create "Foo";
        parameters = [];
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
        name = Instantiated.Access.create "Foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (Type.expression Type.integer);
          };
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
        name = Instantiated.Access.create "Foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (Type.expression Type.integer);
          };
        ];
        annotation = Some (Type.Primitive ~~"Foo");
      };
      {
        name = Instantiated.Access.create "Foo";
        parameters = [
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some (Type.expression Type.string);
          };
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
        name = Instantiated.Access.create "Foo";
        parameters = [];
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
            Instantiated.Access.show name
          in
          Class.create definition
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
        Statement.Class.name = Instantiated.Access.create "Derp";
        bases;
        body = [];
        decorators = [];
        docstring = None;
      }
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
                   ~protocol:(Class.create protocol)
                   (Class.create definition))
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


let test_class_fields _ =
  let resolution =
    populate {|
      class foo():
        first : int
        second : int
        third : int = 1
      foo : foo
    |}
    |> resolution
  in
  let create_assign ?(value = None) name =
    +{
      Assign.target = !name;
      annotation = Some !"int";
      value;
      compound = None;
      parent = Some (Instantiated.Access.create "foo");
    }
  in
  let create_statement ?(value = None) name =
    +Assign {
      Assign.target = !name;
      annotation = Some !"int";
      value;
      compound = None;
      parent = Some (Instantiated.Access.create "foo");
    }
  in
  let parent_node =
    {
      Statement.Class.name = Instantiated.Access.create "foo";
      bases = [];
      body = [
        create_statement "first";
        create_statement "second";
        create_statement "third" ~value:(Some (+Expression.Integer 1));
      ];
      decorators = [];
      docstring = None;
    }
  in

  let assign_list_equal left right =
    let equal sofar left right =
      if not sofar then sofar else
        Expression.equal_expression (Field.name left) (Field.name right) &&
        Annotation.equal (Field.annotation left) (Field.annotation right) &&
        Class.equal (Field.parent left) (Field.parent right)
    in
    List.fold2_exn ~init:true ~f:equal left right
  in

  assert_equal
    ~cmp:assign_list_equal
    ~printer:(fun fields ->
        String.concat
          ~sep:"; "
          (List.map ~f:(Format.asprintf "%a" Annotated.Class.Field.pp) fields))
    (Class.fields ~resolution (Class.create parent_node))
    ([Option.value_exn
        (Field.create_from_assign ~resolution (create_assign "first"));
      Option.value_exn
        (Field.create_from_assign ~resolution (create_assign "second"));
      Option.value_exn
        (Field.create_from_assign
           ~resolution
           (create_assign "third" ~value:(Some (+Expression.Integer 1))));]);


  let callback
      string_names
      field =
    match Field.name field with
    | Expression.Access access -> string_names ^ (Instantiated.Access.show access)
    | _ -> string_names
  in
  assert_equal
    (Class.field_fold ~resolution ~initial:"" ~f:callback (Class.create parent_node))
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
        Statement.Define.name = Instantiated.Access.create "derp";
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async;
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
      Statement.Define.name = Instantiated.Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = parent >>| Instantiated.Access.create;
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
  assert_equal (Class.name parent) (Instantiated.Access.create "foo");

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
  assert_equal (Class.name parent) (Instantiated.Access.create "foo");
  assert_equal base_type (Type.Primitive ~~"superfoo")


let test_method_definition _ =
  let parent_class_definition environment name parent =
    {
      Statement.Define.name = Instantiated.Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = parent >>| Instantiated.Access.create;
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
      Statement.Define.name = Instantiated.Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
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
      Statement.Define.name = Instantiated.Access.create "";
      parameters;
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
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
    let open Annotated.Call in
    assert_equal (backup (create call)) (expected >>| create)
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


let test_field _ =
  let resolution =
    populate "" |> resolution
  in
  let mock_class =
    {
      Statement.Class.name = Instantiated.Access.create "";
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
    |> Class.create
  in
  let create_field name annotation =
    Field.create
      ~name:(Expression.Access (Instantiated.Access.create name))
      ~parent:mock_class
      ~annotation:(
        Field.make_annotation
          ~resolution
          ~annotation:(Some !annotation)
          ~value:None
      )
      ~value:None
      ~location:Location.any
  in

  let field = create_field "f.field_name" "int" in
  assert_equal
    (Field.name field)
    (Expression.Access (Instantiated.Access.create "f.field_name"));
  assert_equal
    (Field.annotation field)
    (Annotation.create_immutable ~global:true (Type.Primitive ~~"int"))


let test_fold _ =
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
  let equal left right =
    match (snd left), (snd right) with
    | Access.Element.Array, Access.Element.Array
    | Access.Element.Call _, Access.Element.Call _
    | Access.Element.Expression, Access.Element.Expression
    | Access.Element.Field _, Access.Element.Field _
    | Access.Element.Global, Access.Element.Global
    | Access.Element.Identifier, Access.Element.Identifier
    | Access.Element.Method _, Access.Element.Method _ -> Annotation.equal (fst left) (fst right)
    | _, _ -> false
  in
  let printer elements =
    let print sofar element =
      match snd element with
      | Access.Element.Array -> "(" ^ (Annotation.show (fst element)) ^ ", Array) " ^ sofar
      | Access.Element.Call _ -> "(" ^ (Annotation.show (fst element)) ^ ", Call) " ^ sofar
      | Access.Element.Expression -> "(" ^ (Annotation.show (fst element)) ^
                                     ", Expression) " ^ sofar
      | Access.Element.Field _ -> "(" ^ (Annotation.show (fst element)) ^ ", Field) " ^ sofar
      | Access.Element.Global -> "(" ^ (Annotation.show (fst element)) ^ ", Global) " ^ sofar
      | Access.Element.Identifier -> "(" ^ (Annotation.show (fst element)) ^
                                     ", Identifier) " ^ sofar
      | Access.Element.Method _ -> "(" ^ (Annotation.show (fst element)) ^ ", Method) " ^ sofar
    in
    List.fold ~init:"" ~f:print elements
  in
  let assert_fold ~environment access expected =
    assert_equal
      ~cmp:(List.equal ~equal)
      ~printer
      expected
      (fold_results access environment)
  in
  let field_element =
    let mock_class =
      {
        Statement.Class.name = Instantiated.Access.create "";
        bases = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
      }
      |> Class.create
    in
    Access.Element.Field {
      Field.parent = mock_class;
      name = Ast.Expression.Access (Instantiated.Access.create "");
      annotation = (Annotation.create_immutable ~global:true Type.Top);
      location = Location.any;
      value = None;
    }
  in

  let environment = populate "" in
  assert_fold
    ~environment
    (Instantiated.Access.create "foo")
    ([
      (Annotation.create Type.Top, Access.Element.Global);
      (Annotation.create Type.Top, Access.Element.Global);
    ]);

  let environment =
    populate {|
      class Foo:
        bar : int
      foo : Foo
    |}
  in
  assert_fold
    ~environment
    (Instantiated.Access.create "foo.bar")
    ([
      (Annotation.create_immutable ~global:true Type.integer, field_element);
      (Annotation.create_immutable ~global:true (Type.Primitive ~~"Foo"), Access.Element.Global);
      (Annotation.create Type.Top, Access.Element.Global);
    ])


let () =
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
    "fields">::test_class_fields;
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
    "field">::test_field;
  ]
  |> run_test_tt_main;
  "access">:::[
    "fold">::test_fold;
  ]
  |> run_test_tt_main
