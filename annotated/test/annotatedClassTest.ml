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
open AnnotatedTest

module Class = Annotated.Class
module Attribute = Annotated.Attribute
module Method = Annotated.Method
module Argument = Expression.Argument


let test_generics _ =
  let assert_generics source generics =
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ } ->
        let resolution = populate source |> resolution in
        assert_equal
          ~cmp:(List.equal ~equal:Type.equal)
          (Class.create (Node.create_with_default_location definition)
           |> Class.generics ~resolution)
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
    [Type.variable "_T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      class Foo(typing.Generic[_T, _S]): pass
    |}
    [Type.variable "_T"; Type.variable "_S"];

  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Protocol[_T]): pass
    |}
    [Type.variable "_T"];

  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Iterable[_T]): pass
    |}
    [Type.variable "_T"]


let test_superclasses _ =
  let assert_superclasses result expected =
    let equal left right = Access.equal (Class.name left) (Class.name right) in
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
      Statement.Class.name = Access.create name;
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
    |> Node.create_with_default_location
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
  parameters: (Expression.t Parameter.t) list;
  annotation: Type.t option;
}


let test_constructors _ =
  let assert_constructors ?(is_generated = false) source constructors =
    match parse_last_statement source with
    | { Node.value = Statement.Class ({ Record.Class.name; _ } as definition); _ }
    | { Node.value = Stub (Stub.Class ({ Record.Class.name; _ } as definition)); _ } ->
        let resolution = populate source |> resolution in
        let defines =
          let define { parameters; annotation } =
            {
              Define.name = name @ (Access.create "__init__");
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
          Node.create_with_default_location definition
          |> Class.create
          |> Class.constructors ~resolution
        in
        assert_equal
          ~cmp:(List.equal ~equal:Define.equal)
          ~printer:(fun constructors ->
              let constructors =
                List.map
                  ~f:(fun constructor -> Define.show constructor)
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
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation = Some (Type.Primitive ~~"Foo")
      };
    ];
  assert_constructors
    ~is_generated:true
    "class Foo: ..."
    [
      {
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation = Some (Type.Primitive ~~"Foo");
      };
    ];

  (* Statement.Defined constructors. *)
  assert_constructors
    {|
      class Foo:
        def Foo.__init__(self, a: int) -> None: pass
    |}
    [
      {
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
        def Foo.__init__(self, a: int) -> None: pass
        def Foo.__init__(self, b: str) -> None: pass
    |}
    [
      {
        parameters = [
          Parameter.create ~name:~~"self" ();
          Parameter.create ~name:~~"a" ~annotation:(Type.expression Type.integer) ();
        ];
        annotation = Some (Type.Primitive ~~"Foo");
      };
      {
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
        def Foo.__init__(self) -> None: pass
    |}
    [
      {
        parameters = [Parameter.create ~name:~~"self" ()];
        annotation =
          Some
            (Type.Parametric {
                Type.name = ~~"Foo";
                parameters = [Type.variable "_K"; Type.variable "_V"];
              });
      };
    ]


let test_methods _ =
  let assert_methods source methods =
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ }
    | { Node.value = Stub (Stub.Class definition); _ } ->
        let actuals =
          let method_name { Define.name; _ } =
            List.tl_exn name
            |> Access.show
          in
          Node.create_with_default_location definition
          |> Class.create
          |> Class.methods
          |> List.map ~f:(fun definition -> Method.define definition |> method_name)
        in
        assert_equal methods actuals
    | _ ->
        assert_unreached ()
  in
  assert_methods "class A: pass" [];
  assert_methods
    {|
      class A:
        def A.foo(): pass
        def A.bar(): pass
        1 + 1
        def A.baz(): ...
    |}
    ["foo"; "bar"; "baz"]


let test_is_protocol _ =
  let assert_is_protocol bases expected =
    let is_protocol =
      {
        Statement.Class.name = Access.create "Derp";
        bases;
        body = [];
        decorators = [];
        docstring = None;
      }
      |> Node.create_with_default_location
      |> Class.create
      |> Class.is_protocol
    in
    assert_equal expected is_protocol
  in

  assert_is_protocol [] false;
  assert_is_protocol [{ Argument.name = None; value = !"derp" }] false;
  assert_is_protocol [{ Argument.name = None; value = !"typing.Protocol" }] true;
  assert_is_protocol
    [{ Argument.name = Some ~+(~~"metaclass"); value = !"abc.ABCMeta"; }]
    false


let test_implements _ =
  let assert_conforms definition protocol conforms =
    match parse_last_statement definition with
    | { Node.value = Statement.Class definition; _ }
    | { Node.value = Stub (Stub.Class definition); _ } ->
        begin
          match parse_last_statement protocol with
          | { Node.value = Statement.Class protocol; _ }
          | { Node.value = Stub (Stub.Class protocol); _ } ->
              assert_equal
                (Class.implements
                   ~protocol:(Class.create (Node.create_with_default_location protocol))
                   (Class.create (Node.create_with_default_location definition)))
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
  let setup source =
    let parent =
      match parse_last_statement source with
      | { Node.value = Class definition; _ } ->
          definition
      | _ ->
          failwith "Could not parse class"
    in
    populate source |> resolution,
    Class.create (Node.create_with_default_location parent)
  in

  let resolution, parent =
    setup
      {|
        class type:
          type.__name__: str = 'asdf'
        foo: foo
        class Attributes:
          def Attributes.bar(self) -> int:
            pass
          def Attributes.baz(self, x:int) -> int:
            pass
          def Attributes.baz(self, x:str) -> str:
            pass
        class foo():
          def foo.__init__(self):
            self.implicit: int = 1
          foo.first: int
          foo.second: int
          foo.third: int = 1
          foo.class_attribute: typing.ClassVar[int]
      |}
  in

  let create_attribute
      ?value
      ?(annotation = Some !"int")
      ?defines
      ?(async = false)
      ?(setter = false)
      ?(primitive = false)
      name =
    +{
      Statement.Attribute.target = !name;
      annotation;
      defines;
      value;
      async;
      setter;
      primitive;
    }
  in

  (* Test `Class.attributes`. *)
  let assert_attributes definition attributes =
    Annotated.Class.AttributesCache.clear ();
    let attribute_list_equal =
      let equal left right =
        Expression.equal_expression (Attribute.name left) (Attribute.name right) &&
        Class.equal (Attribute.parent left) (Attribute.parent right)
      in
      List.equal ~equal
    in
    let print_attributes attributes =
      let print_attribute { Node.value = { Annotated.Attribute.name; _ }; _ } =
        Format.asprintf "%a" Expression.pp_expression name
      in
      List.map ~f:print_attribute attributes
      |> String.concat ~sep:", "
    in
    assert_equal
      ~cmp:attribute_list_equal
      ~printer:print_attributes
      (Class.attributes ~resolution definition)
      attributes
  in
  assert_attributes
    parent
    [
      Attribute.create ~resolution ~parent (create_attribute "__init__");
      Attribute.create ~resolution ~parent (create_attribute "class_attribute");
      Attribute.create ~resolution ~parent (create_attribute "first");
      Attribute.create ~resolution ~parent (create_attribute "implicit");
      Attribute.create ~resolution ~parent (create_attribute "second");
      Attribute.create
        ~resolution
        ~parent
        (create_attribute "third" ~value:(+Expression.Integer 1));
    ];


  (* Test `Attribute`. *)
  let attribute =
    Attribute.create
      ~resolution
      ~parent
      (create_attribute ~annotation:(Some !"int") "first")
  in
  assert_equal
    (Attribute.name attribute)
    (Expression.Access (Access.create "first"));
  assert_equal
    (Attribute.annotation attribute)
    (Annotation.create_immutable ~global:true (Type.Primitive ~~"int"));
  assert_false (Attribute.class_attribute attribute);

  let attribute =
    Attribute.create
      ~resolution
      ~parent
      (create_attribute
         ~annotation:(Some (Type.expression (Type.parametric "typing.ClassVar" [Type.integer])))
         "first")
  in
  assert_true (Attribute.class_attribute attribute);

  (* Test `attribute_fold`. *)
  let assert_fold ?(class_attributes = false) source fold =
    Annotated.Class.AttributesCache.clear ();
    let callback
        string_names
        attribute =
      match Attribute.name attribute with
      | Expression.Access access -> string_names ^ (Access.show access)
      | _ -> string_names
    in
    let resolution, parent = setup source in
    assert_equal
      ~printer:Fn.id
      (Class.attribute_fold ~class_attributes ~resolution ~initial:"" ~f:callback parent)
      fold
  in

  assert_fold
    {|
      class type:
        type.__name__: str = 'asdf'
      foo: foo
      class foo():
        def foo.__init__(self):
          self.implicit: int = 1
        foo.first: int
        foo.second: int
        foo.third: int = 1
        foo.class_attribute: typing.ClassVar[int]
    |}
    "__init__class_attributefirstimplicitsecondthird";
  assert_fold
    ~class_attributes:true
    {|
      class type:
        type.__name__: str = 'asdf'
      foo: foo
      class foo():
        def foo.__init__(self):
          self.implicit: int = 1
        foo.first: int
        foo.second: int
        foo.third: int = 1
        foo.class_attribute: typing.ClassVar[int]
    |}
    ("__init__class_attributefirstimplicitsecondthird__name__");
  assert_fold
    ~class_attributes:true
    {|
      class type:
        type.__type__: str
      class Meta(type):
        Meta.__meta__: str
      class Foo(metaclass=Meta):
        Foo.__static__: typing.ClassVar[int]
        Foo.__instance__: int
    |}
    "__instance____static____meta____type__";

  (* Test 'attribute' *)
  let assert_attribute ~parent ~parent_instantiated_type ~attribute_name ~expected_attribute =
    let instantiated, class_attributes =
      if Type.is_meta parent_instantiated_type then
        Type.single_parameter parent_instantiated_type, true
      else
        parent_instantiated_type, false
    in
    let actual_attribute =
      Node.create_with_default_location parent
      |> Class.create
      |> Class.attribute
        ~transitive:true
        ~class_attributes
        ~resolution
        ~name:attribute_name
        ~instantiated
      |> Node.value
    in
    assert_equal
      ~cmp:Attribute.equal_attribute
      ~printer:Attribute.show_attribute
      expected_attribute
      actual_attribute
  in
  let parent =
    {|
    class Attributes:
      def Attributes.bar(self) -> int:
        pass
      def Attributes.baz(self, x:int) -> int:
        pass
      def Attributes.baz(self, x:str) -> str:
        pass
    |}
    |> parse_single_class
  in
  let create_expected_attribute name callable =
    {
      Class.Attribute.name = Expression.Access (Access.create name);
      parent = Class.create (+parent);
      annotation = (Annotation.create_immutable ~global:true (parse_callable callable));
      value = None;
      defined = true;
      class_attribute = false;
      async = false;
    }
  in
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.primitive "Attributes")
    ~attribute_name:(Access.create "bar")
    ~expected_attribute:(
      create_expected_attribute
        "bar"
        "typing.Callable('Attributes.bar')[[Named(self, $unknown)], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.primitive "Attributes")
    ~attribute_name:(Access.create "baz")
    ~expected_attribute:(
      create_expected_attribute
        "baz"
        ("typing.Callable('Attributes.baz')[[Named(self, $unknown), Named(x, str)], str]" ^
         "[[Named(self, $unknown), Named(x, int)], int]"))


let test_fallback_attribute _ =
  let assert_fallback_attribute source annotation =
    let resolution =
      populate source
      |> resolution
    in
    let attribute =
      parse_last_statement source
      |> (function
          | { Node.location; value = Statement.Class definition; _ } ->
              Class.create (Node.create ~location definition)
          | _ ->
              failwith "Last statement was not a class")
      |> Class.fallback_attribute ~resolution ~access:[]
    in
    match annotation with
    | None ->
        assert_is_none attribute
    | Some annotation ->
        assert_is_some attribute;
        let attribute = Option.value_exn attribute in
        assert_equal
          ~cmp:Type.equal
          annotation
          (Attribute.annotation attribute |> Annotation.annotation)
  in

  assert_fallback_attribute
    {|
      class Foo:
        pass
    |}
    None;
  assert_fallback_attribute
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int:
          return 1
    |}
    (Some Type.integer);
  assert_fallback_attribute
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
    |}
    (Some Type.integer);
  assert_fallback_attribute
    {|
      class Foo:
        def __getattr__(self, attribute: str) -> int: ...
      class Bar(Foo):
        pass
    |}
    (Some Type.integer)


let test_constraints _ =
  let assert_constraints ~target ~instantiated ?parameters source expected =
    let resolution =
      populate source
      |> resolution
    in
    let target =
      let { Source.statements; _ } = parse source in
      let target = function
        | { Node.location; value = Statement.Class ({ Statement.Class.name; _ } as definition) }
          when Access.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ ->
            None
      in
      List.find_map ~f:target statements
      |> value
    in
    let constraints =
      parse_last_statement source
      |> (function
          | { Node.location; value = Statement.Class definition; _ } ->
              Class.create (Node.create ~location definition)
          | _ ->
              failwith "Last statement was not a class")
      |> Class.constraints ~target ~resolution ?parameters ~instantiated
    in
    assert_equal
      ~cmp:(Type.Map.equal Type.equal)
      (Type.Map.of_alist_exn expected)
      constraints
  in

  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.primitive "Foo")
    {|
      class Foo:
        pass
    |}
    [];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" [Type.Bottom])
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        pass
    |}
    [];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" [Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [Type.variable "_K", Type.integer; Type.variable "_V", Type.float];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" [Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [Type.variable "_K", Type.integer; Type.variable "_V", Type.float];

  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.primitive "Foo")
    {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
    [];
  assert_constraints
    ~target:"Bar"
    ~instantiated:(Type.primitive "Foo")
    {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
    [Type.variable "_T", Type.integer];

  assert_constraints
    ~target:"Bar"
    ~instantiated:(Type.parametric "Foo" [Type.integer])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_V]):
        pass
      class Foo(typing.Generic[_K], Bar[_K]):
        pass
    |}
    [Type.variable "_V", Type.integer];

  assert_constraints
    ~target:"Bar"
    ~instantiated:(Type.parametric "Foo" [Type.integer; Type.float])
    {|
      _T = typing.TypeVar('_T')
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_T]):
        pass
      class Baz(typing.Generic[_T]):
        pass
      class Foo(typing.Generic[_K, _V], Bar[_K], Baz[_V]):
        pass
    |}
    [Type.variable "_T", Type.integer];
  assert_constraints
    ~target:"Baz"
    ~instantiated:(Type.parametric "Foo" [Type.integer; Type.float])
    {|
      _T = typing.TypeVar('_T')
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_T]):
        pass
      class Baz(typing.Generic[_T]):
        pass
      class Foo(typing.Generic[_K, _V], Bar[_K], Baz[_V]):
        pass
    |}
    [Type.variable "_T", Type.float];

  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterator" [Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
    |}
    [Type.variable "_T", Type.integer];

  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterable" [Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.variable "_T", Type.integer];

  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterable" [Type.parametric "Iterable" [Type.integer]])
    ~parameters:[Type.parametric "Iterable" [Type.variable "_T"]]
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.variable "_T", Type.integer]


let test_inferred_generic_base _ =
  let assert_inferred_generic ~target ~aliases source expected =
    let target =
      let { Source.statements; _ } = parse source in
      let target = function
        | { Node.location; value = Statement.Class ({ Statement.Class.name; _ } as definition) }
          when Access.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ ->
            None
      in
      List.find_map ~f:target statements
      |> value
    in
    assert_equal
      ~cmp:(List.equal ~equal:Argument.equal)
      expected
      (Annotated.Class.inferred_generic_base ~aliases target)
  in
  let aliases = function
    | Type.Primitive identifier ->
        if Identifier.show identifier = "_T" then
          Some (Type.variable "_T")
        else
          None
    | _ -> None
  in
  assert_inferred_generic
    ~target:"C"
    ~aliases
    {|
       class C:
         pass
     |}
    [];
  assert_inferred_generic
    ~target:"C"
    ~aliases
    {|
       _T = TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(List[_T]):
         pass
     |}
    [{
      Argument.name = None;
      value = Type.expression (Type.parametric "typing.Generic" [Type.variable "_T"]);
    }];
  assert_inferred_generic
    ~target:"List"
    ~aliases
    {|
       _T = TypeVar("_T")
       class Iterable(typing.Generic[_T]):
         pass
       class List(Iterable[_T], typing.Generic[_T]):
         pass
     |}
    []


let test_method_overloads _ =
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

  assert_is_none (Method.overloads ~resolution baz);
  let overloads = Method.overloads ~resolution foo in
  assert_is_some overloads;
  assert_equal
    ~cmp:Access.equal
    (Method.parent (Option.value_exn overloads) |> Class.name)
    (Access.create "Foo")


let test_method_implements _ =
  let definition ?(parameters = []) ?return_annotation name =
    Method.create
      ~define:{
        Statement.Define.name = Access.create name;
        parameters;
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation;
        async = false;
        generated = false;
        parent = Some (Access.create "Parent");
      }
      ~parent:
        (Class.create
           (Node.create_with_default_location
              {
                Statement.Class.name = Access.create "Parent";
                bases = [];
                body = [+Pass];
                decorators = [];
                docstring = None;
              }))
  in

  assert_true
    (Method.implements
       ~protocol_method:(definition "match")
       (definition "match"));
  assert_false
    (Method.implements
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
       ~protocol_method:(definition ~parameters:protocol_parameters "match")
       (definition ~parameters:definition_parameters "match"));

  let definition_parameters = [Parameter.create ~name:(~~"a") ~annotation:!"int" ()] in
  let protocol_parameters = [Parameter.create ~name:(~~"a") ()] in
  assert_false
    (Method.implements
       ~protocol_method:(definition ~parameters:protocol_parameters "match")
       (definition ~parameters:definition_parameters "match"));

  assert_true
    (Method.implements
       ~protocol_method:(definition ~return_annotation:!"int" "match")
       (definition ~return_annotation:!"int" "match"));
  assert_false
    (Method.implements
       ~protocol_method:(definition ~return_annotation:!"int" "match")
       (definition ~return_annotation:!"float" "match"))


let () =
  "class">:::[
    "generics">::test_generics;
    "superclasses">::test_superclasses;
    "constructors">::test_constructors;
    "methods">::test_methods;
    "is_protocol">::test_is_protocol;
    "implements">::test_implements;
    "attributes">::test_class_attributes;
    "fallback_attribute">::test_fallback_attribute;
    "constraints">::test_constraints;
    "inferred_generic_base">::test_inferred_generic_base;
  ]
  |> run_test_tt_main;
  "method">:::[
    "overloads">::test_method_overloads;
    "implements">::test_method_implements;
  ]
  |> run_test_tt_main
