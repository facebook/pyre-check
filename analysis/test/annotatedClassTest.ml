(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Statement
open Pyre
open Test
module StatementClass = Class
module StatementAttribute = Attribute
module StatementDefine = Define
module Class = Annotated.Class
module Attribute = Annotated.Attribute
module Argument = Call.Argument

let ( !! ) concretes = Type.OrderedTypes.Concrete concretes

let value option = Option.value_exn option

let last_statement_exn = function
  | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
  | _ -> failwith "Could not parse last statement"


let test_generics context =
  let assert_generics source generics =
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
        (Reference.create "test")
    in
    let source = Option.value_exn source in
    match source |> last_statement_exn with
    | { Node.value = Statement.Class definition; _ } ->
        let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
        let printer generics = Format.asprintf "%a" Type.OrderedTypes.pp_concise generics in
        assert_equal
          ~printer
          ~cmp:Type.OrderedTypes.equal
          ( Class.create (Node.create_with_default_location definition)
          |> Class.generics ~resolution )
          generics
    | _ -> assert_unreached ()
  in
  assert_generics "class Foo(): pass" (Concrete []);
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]): pass
    |}
    !![Type.variable "test._T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      class Foo(typing.Generic[_T, _S]): pass
    |}
    !![Type.variable "test._T"; Type.variable "test._S"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Protocol[_T]): pass
    |}
    !![Type.variable "test._T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Iterable[_T]): pass
    |}
    !![Type.variable "test._T"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Dict[_T1, _T2]): pass
    |}
    !![Type.variable "test._T1"; Type.variable "test._T2"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Iterable[_T1], typing.AsyncIterable[_T2]): pass
    |}
    !![Type.variable "test._T1"; Type.variable "test._T2"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      class Foo(typing.Dict[_T1, _T1]): pass
    |}
    !![Type.variable "test._T1"];
  ()


let test_superclasses context =
  let _, _, environment =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
      class Foo: pass
      class Bar: pass
      class SubFoo(Foo): pass
      class SubFooBar(Foo, Bar): pass
      class SubRecurse(SubFooBar): pass
      class SubRedundant(Foo, SubFooBar): pass
    |}
        );
      ]
    |> ScratchProject.build_environment
  in
  let ( ! ) name =
    {
      StatementClass.name = !&name;
      bases = [];
      body = [+Statement.Pass];
      decorators = [];
      docstring = None;
    }
    |> Node.create_with_default_location
    |> Class.create
  in
  let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
  let assert_successors target expected =
    let actual = Class.successors ~resolution target in
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      ~cmp:(List.equal Type.Primitive.equal)
      expected
      actual
  in
  let assert_superclasses target expected =
    let actual = Class.superclasses ~resolution target in
    let equal left right = Reference.equal (Class.name left) (Class.name right) in
    assert_equal
      ~printer:(fun classes -> Format.asprintf "%a" Sexp.pp [%message (classes : Class.t list)])
      ~cmp:(List.equal equal)
      expected
      actual
  in
  assert_successors !"test.Foo" ["object"];
  assert_successors !"test.SubRedundant" ["test.SubFooBar"; "test.Foo"; "test.Bar"; "object"];
  assert_superclasses !"test.Foo" [!"object"];
  assert_superclasses !"test.SubFoo" [!"test.Foo"; !"object"];
  assert_superclasses !"test.SubFooBar" [!"test.Foo"; !"test.Bar"; !"object"];
  assert_superclasses !"test.SubRecurse" [!"test.SubFooBar"; !"test.Foo"; !"test.Bar"; !"object"];
  assert_superclasses !"test.SubRedundant" [!"test.SubFooBar"; !"test.Foo"; !"test.Bar"; !"object"]


type constructor = {
  parameters: Expression.t Parameter.t list;
  annotation: Type.t option;
}

let test_get_decorator context =
  let assert_get_decorator source decorator expected =
    let resolution =
      let _, _, environment =
        ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
      in
      AnnotatedGlobalEnvironment.ReadOnly.resolution environment
    in
    let assert_logic expected =
      match parse_last_statement source with
      | { Node.value = Statement.Class definition; _ } ->
          let actual =
            Node.create_with_default_location definition
            |> Class.create
            |> Class.get_decorator ~resolution ~decorator
          in
          assert_equal
            ~printer:(List.to_string ~f:Class.show_decorator)
            ~cmp:(List.equal Class.equal_decorator)
            expected
            actual
      | _ -> assert_true (List.is_empty expected)
    in
    assert_logic expected
  in
  assert_get_decorator "class A: pass" "decorator" [];
  assert_get_decorator
    {|
      @decorator
      class A:
        pass
    |}
    "decorator"
    [{ name = "decorator"; arguments = None }];
  assert_get_decorator {|
      @decorator.a.b
      class A:
        pass
    |} "decorator.a" [];
  assert_get_decorator {|
      @decorator
      class A:
        pass
    |} "decorator.a" [];
  assert_get_decorator
    {|
      @decorator.a.b
      class A:
        pass
    |}
    "decorator.a.b"
    [{ name = "decorator.a.b"; arguments = None }];
  assert_get_decorator
    {|
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator.a.b"
    [];
  assert_get_decorator
    {|
      @other.decorator
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator"
    [
      {
        name = "decorator";
        arguments =
          Some
            [
              { Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Name (Name.Identifier "d") };
            ];
      };
    ];
  assert_get_decorator
    {|
      @decorator(a=b)
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator"
    [
      {
        name = "decorator";
        arguments = Some [{ Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") }];
      };
      {
        name = "decorator";
        arguments =
          Some
            [
              { Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Name (Name.Identifier "d") };
            ];
      };
    ];
  assert_get_decorator
    (* `enum` imports `ABCMeta` from `abc`. *)
    {|
      @enum.ABCMeta
      class A:
        pass
    |}
    "abc.ABCMeta"
    [{ name = "abc.ABCMeta"; arguments = None }]


let test_constructors context =
  let assert_constructor source instantiated constructors =
    let instantiated = "test." ^ instantiated in
    Class.AttributeCache.clear ();
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
        (Reference.create "test")
    in
    let source = Option.value_exn source in
    let instantiated =
      parse_single_expression instantiated
      |> GlobalResolution.parse_annotation ~allow_invalid_type_parameters:true resolution
    in
    match last_statement_exn source with
    | { Node.value = Statement.Class definition; _ } ->
        let callable =
          constructors
          >>| (fun constructors ->
                GlobalResolution.parse_annotation resolution (parse_single_expression constructors))
          |> Option.value ~default:Type.Top
        in
        let actual =
          Node.create_with_default_location definition
          |> Class.create
          |> Class.constructor ~resolution ~instantiated
        in
        assert_equal ~printer:Type.show ~cmp:Type.equal callable actual
    | _ -> assert_unreached ()
  in
  (* Undefined constructors. *)
  assert_constructor
    "class Foo: pass"
    "Foo"
    (Some "typing.Callable('object.__init__')[[], test.Foo]");
  assert_constructor
    "class Foo: ..."
    "Foo"
    (Some "typing.Callable('object.__init__')[[], test.Foo]");

  (* Statement.Defined constructors. *)
  assert_constructor
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
    |}
    "Foo"
    (Some "typing.Callable('test.Foo.__init__')[[Named(a, int)], test.Foo]");
  assert_constructor
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
        @typing.overload
        def __init__(self, b: str) -> None: pass
    |}
    "Foo"
    (Some
       ( "typing.Callable('test.Foo.__init__')[[Named(a, int)], test.Foo]"
       ^ "[[[Named(b, str)], test.Foo]]" ));

  (* Generic classes. *)
  assert_constructor
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        def __init__(self) -> None: pass
    |}
    "Foo"
    (Some
       "typing.Callable('test.Foo.__init__')[[], \
        test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')]]");
  assert_constructor
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        def __init__(self, x:_K, y:_V) -> None: pass
    |}
    "Foo[int, str]"
    (Some
       "typing.Callable('test.Foo.__init__')[[Named(x, int), Named(y, str)], test.Foo[int, str]]");

  (* Constructors, both __init__ and __new__, are inherited from parents. *)
  assert_constructor
    {|
      class Parent:
        def __init__(self, x: int) -> None:
          pass
      class C(Parent):
        pass
    |}
    "C"
    (Some "typing.Callable('test.Parent.__init__')[[Named(x, int)], test.C]");
  assert_constructor
    {|
      class Parent:
        def __new__(self, x: str) -> None:
          pass
      class C(Parent):
        pass
    |}
    "C"
    (Some "typing.Callable('test.Parent.__new__')[[Named(x, str)], test.C]");
  assert_constructor
    {|
      T = typing.TypeVar('T', bound=C)
      class C:
        def __init__(self, x: int) -> None: pass
    |}
    "T"
    (Some "typing.Callable('test.C.__init__')[[Named(x, int)], test.T]");
  assert_constructor
    {|
      from dataclasses import dataclass
      @dataclass(frozen=True)
      class A:
          foo:int = 1
    |}
    "A"
    (Some "typing.Callable('test.A.__init__')[[Named(foo, int, default)], test.A]");
  ()


let test_has_method context =
  let get_actual source target_method =
    let resolution =
      let _, _, environment =
        ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
      in
      AnnotatedGlobalEnvironment.ReadOnly.resolution environment
    in
    match parse_last_statement source with
    | { Node.value = Statement.Class definition; _ } ->
        let actual =
          Node.create_with_default_location definition
          |> Class.create
          |> Class.has_method ~resolution ~name:target_method
        in
        actual
    | _ -> false
  in
  let assert_has_method source target_method = assert_true (get_actual source target_method) in
  let assert_not_has_method source target_method =
    assert_false (get_actual source target_method)
  in
  assert_not_has_method "class A: pass" "foo";
  assert_has_method
    {|
      class A:
        def A.foo(): pass
        def A.bar(): pass
        1 + 1
        def A.baz(): ...
    |}
    "foo";
  assert_has_method
    {|
      class A:
        def A.foo(): pass
        def A.bar(): pass
        1 + 1
        def A.baz(): ...
    |}
    "baz"


let test_is_protocol _ =
  let assert_is_protocol bases expected =
    let is_protocol bases =
      { StatementClass.name = !&"Derp"; bases; body = []; decorators = []; docstring = None }
      |> Node.create_with_default_location
      |> Class.create
      |> Class.is_protocol
    in
    assert_equal expected (is_protocol bases)
  in
  let parse = parse_single_expression in
  assert_is_protocol [] false;
  assert_is_protocol [{ Argument.name = None; value = parse "derp" }] false;
  assert_is_protocol [{ Argument.name = None; value = parse "typing.Protocol" }] true;
  assert_is_protocol [{ Argument.name = None; value = parse "typing_extensions.Protocol" }] true;
  assert_is_protocol [{ Argument.name = Some ~+"metaclass"; value = parse "abc.ABCMeta" }] false;
  assert_is_protocol [{ Argument.name = None; value = parse "typing.Protocol[T]" }] true;
  ()


let test_class_attributes context =
  let setup source =
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
        (Reference.create "test")
    in
    let source = Option.value_exn source in
    let parent =
      match source |> last_statement_exn with
      | { Node.value = Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    ( AnnotatedGlobalEnvironment.ReadOnly.resolution environment,
      Class.create (Node.create_with_default_location parent) )
  in
  let resolution, parent =
    setup
      {|
        class Metaclass:
          def implicit(cls) -> int:
            return 0

        class Attributes(metaclass=Metaclass):
          def bar(self) -> int:
            pass
          def baz(self, x:int) -> int:
            pass
          def baz(self, x:str) -> str:
            pass
        class foo():
          def __init__(self):
            self.implicit: int = 1
          first: int
          second: int
          third: int = 1
          class_attribute: typing.ClassVar[int]
      |}
  in
  let create_simple_attribute
      ?(annotation = Some !"int")
      ?(frozen = false)
      ?(implicit = false)
      ?(primitive = false)
      ?(toplevel = true)
      ?value
      name
    =
    +{
       StatementAttribute.kind =
         Simple { annotation; frozen; implicit; primitive; toplevel; value };
       name;
     }
  in
  (* Test `Class.attributes`. *)
  let assert_attributes definition attributes =
    Annotated.Class.AttributeCache.clear ();
    let attribute_list_equal =
      let equal left right =
        Attribute.name left = Attribute.name right
        && Type.equal (Attribute.parent left) (Attribute.parent right)
      in
      List.equal equal
    in
    let print_attributes attributes =
      let print_attribute { Node.value = { Annotated.Attribute.name; _ }; _ } = name in
      List.map attributes ~f:print_attribute |> String.concat ~sep:", "
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
      Class.create_attribute ~resolution ~parent (create_simple_attribute "__init__");
      Class.create_attribute ~resolution ~parent (create_simple_attribute "class_attribute");
      Class.create_attribute ~resolution ~parent (create_simple_attribute "first");
      Class.create_attribute ~resolution ~parent (create_simple_attribute "implicit");
      Class.create_attribute ~resolution ~parent (create_simple_attribute "second");
      Class.create_attribute
        ~resolution
        ~parent
        (create_simple_attribute "third" ~value:(+Integer 1));
    ];

  (* Test `Attribute`. *)
  let attribute =
    Class.create_attribute
      ~resolution
      ~parent
      (create_simple_attribute ~annotation:(Some !"int") "first")
  in
  assert_equal (Attribute.name attribute) "first";
  assert_equal
    (Attribute.annotation attribute)
    (Annotation.create_immutable ~global:true (Type.Primitive "int"));
  assert_false (Attribute.class_attribute attribute);
  let attribute =
    Class.create_attribute
      ~resolution
      ~parent
      (create_simple_attribute
         ~annotation:(Some (Type.expression (Type.parametric "typing.ClassVar" !![Type.integer])))
         "first")
  in
  assert_true (Attribute.class_attribute attribute);

  (* Test `attribute_fold`. *)
  let assert_fold ?(class_attributes = false) source fold =
    Annotated.Class.AttributeCache.clear ();
    let callback names attribute = Attribute.name attribute :: names in
    let resolution, parent = setup source in
    let actual =
      Class.attribute_fold ~class_attributes ~resolution ~initial:[] ~f:callback parent
      |> List.sort ~compare:String.compare
    in
    assert_equal ~printer:(List.to_string ~f:ident) (List.sort ~compare:String.compare fold) actual
  in
  assert_fold
    {|
      class foo():
        def __init__(self):
          self.implicit: int = 1
        first: int
        second: int
        third: int = 1
        class_attribute: typing.ClassVar[int]
    |}
    ["__init__"; "class_attribute"; "first"; "implicit"; "second"; "third"];
  assert_fold
    ~class_attributes:true
    {|
      class foo():
        def __init__(self):
          self.implicit: int = 1
        first: int
        second: int
        third: int = 1
        class_attribute: typing.ClassVar[int]
    |}
    [
      "class_attribute";
      "first";
      "implicit";
      "second";
      "third";
      "__call__";
      "__class__";
      "__delattr__";
      "__doc__";
      "__eq__";
      "__format__";
      "__getattribute__";
      "__hash__";
      "__init__";
      "__name__";
      "__ne__";
      "__new__";
      "__reduce__";
      "__repr__";
      "__setattr__";
      "__sizeof__";
      "__str__";
    ];
  assert_fold
    ~class_attributes:true
    {|
      class Meta(type):
        __meta__: str
      class Foo(metaclass=Meta):
        __static__: typing.ClassVar[int]
        __instance__: int
    |}
    [
      "__instance__";
      "__static__";
      "__meta__";
      "__call__";
      "__class__";
      "__delattr__";
      "__doc__";
      "__eq__";
      "__format__";
      "__getattribute__";
      "__hash__";
      "__init__";
      "__name__";
      "__ne__";
      "__new__";
      "__reduce__";
      "__repr__";
      "__setattr__";
      "__sizeof__";
      "__str__";
    ];
  assert_fold
    {|
      @dataclass
      class Foo:
        def foo() -> None:
          pass
    |}
    ["foo"; "__init__"; "__repr__"; "__eq__"];

  (* Test 'attribute' *)
  let resolution, parent =
    setup
      {|
        class Metaclass:
          def implicit(cls) -> int:
            return 0

        class Attributes(metaclass=Metaclass):
          def bar(self) -> int:
            pass
          def baz(self, x:int) -> int:
            pass
          def baz(self, x:str) -> str:
            pass
          @property
          def property(self) -> str:
            pass
      |}
  in
  let assert_attribute ~parent ~parent_instantiated_type ~attribute_name ~expected_attribute =
    let instantiated, class_attributes =
      if Type.is_meta parent_instantiated_type then
        Type.single_parameter parent_instantiated_type, true
      else
        parent_instantiated_type, false
    in
    let actual_attribute =
      Class.attribute
        parent
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
  let create_expected_attribute
      ?(property = None)
      ?(parent = Type.Primitive "test.Attributes")
      ?(initialized = true)
      name
      callable
    =
    {
      Class.Attribute.annotation =
        Annotation.create_immutable ~global:true (parse_callable callable);
      abstract = false;
      async = false;
      class_attribute = false;
      defined = true;
      final = false;
      initialized;
      name;
      parent;
      property;
      static = false;
      value = Node.create_with_default_location Ellipsis;
    }
  in
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.Primitive "Attributes")
    ~attribute_name:"bar"
    ~expected_attribute:
      (create_expected_attribute "bar" "typing.Callable('test.Attributes.bar')[[], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.Primitive "Attributes")
    ~attribute_name:"baz"
    ~expected_attribute:
      (create_expected_attribute
         "baz"
         "typing.Callable('test.Attributes.baz')[[Named(x, int)], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"implicit"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:(Type.Primitive "test.Metaclass")
         "implicit"
         "typing.Callable('test.Metaclass.implicit')[[], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"property"
    ~expected_attribute:
      (create_expected_attribute ~initialized:true ~property:(Some ReadOnly) "property" "str");
  ()


let test_fallback_attribute context =
  let assert_fallback_attribute ~name source annotation =
    Class.AttributeCache.clear ();
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let global_resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    let resolution = TypeCheck.resolution global_resolution () in
    let attribute =
      let source =
        AstEnvironment.ReadOnly.get_source
          (AstEnvironment.read_only ast_environment)
          (Reference.create "test")
      in
      let source = Option.value_exn source in
      last_statement_exn source
      |> (function
           | { Node.location; value = Statement.Class definition; _ } ->
               Class.create (Node.create ~location definition)
           | _ -> failwith "Last statement was not a class")
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
    (Some Type.none)


let test_constraints context =
  let assert_constraints ~target ~instantiated ?parameters source expected =
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
        (Reference.create "test")
    in
    let source = Option.value_exn source in
    let target =
      let { Source.statements; _ } = source in
      let target = function
        | { Node.location; value = Statement.Class ({ StatementClass.name; _ } as definition) }
          when Reference.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ -> None
      in
      List.find_map ~f:target statements |> value
    in
    let constraints =
      last_statement_exn source
      |> (function
           | { Node.location; value = Statement.Class definition; _ } ->
               Class.create (Node.create ~location definition)
           | _ -> failwith "Last statement was not a class")
      |> Class.constraints ~target ~resolution ?parameters ~instantiated
    in
    let expected =
      List.map expected ~f:(fun (variable, value) -> Type.Variable.UnaryPair (variable, value))
    in
    assert_equal
      ~printer:TypeConstraints.Solution.show
      ~cmp:TypeConstraints.Solution.equal
      (TypeConstraints.Solution.create expected)
      constraints
  in
  let int_and_foo_string_union =
    Type.Union [Type.parametric "test.Foo" !![Type.string]; Type.integer]
  in
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![int_and_foo_string_union])
    {|
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_V]):
        pass
    |}
    [Type.Variable.Unary.create "test._V", int_and_foo_string_union];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.Primitive "test.Foo")
    {|
      class Foo:
        pass
    |}
    [];

  (* Consequence of the special case we need to remove *)
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.Bottom])
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        pass
    |}
    [];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [
      Type.Variable.Unary.create "test._K", Type.integer;
      Type.Variable.Unary.create "test._V", Type.float;
    ];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [
      Type.Variable.Unary.create "test._K", Type.integer;
      Type.Variable.Unary.create "test._V", Type.float;
    ];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.Primitive "test.Foo")
    {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
    [];
  assert_constraints
    ~target:"test.Bar"
    ~instantiated:(Type.Primitive "test.Foo")
    {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
    [Type.Variable.Unary.create "test._T", Type.integer];
  assert_constraints
    ~target:"test.Bar"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_V]):
        pass
      class Foo(typing.Generic[_K], Bar[_K]):
        pass
    |}
    [Type.Variable.Unary.create "test._V", Type.integer];
  assert_constraints
    ~target:"test.Bar"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer; Type.float])
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
    [Type.Variable.Unary.create "test._T", Type.integer];
  assert_constraints
    ~target:"test.Baz"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer; Type.float])
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
    [Type.Variable.Unary.create "test._T", Type.float];
  assert_constraints
    ~target:"test.Iterator"
    ~instantiated:(Type.parametric "test.Iterator" !![Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
    |}
    [Type.Variable.Unary.create "test._T", Type.integer];
  assert_constraints
    ~target:"test.Iterator"
    ~instantiated:(Type.parametric "test.Iterable" !![Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.Variable.Unary.create "test._T", Type.integer];
  assert_constraints
    ~target:"test.Iterator"
    ~instantiated:
      (Type.parametric "test.Iterable" !![Type.parametric "test.Iterable" !![Type.integer]])
    ~parameters:!![Type.parametric "test.Iterable" !![Type.variable "test._T"]]
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.Variable.Unary.create "test._T", Type.integer];
  assert_constraints
    ~target:"test.Foo"
    ~parameters:!![Type.parametric "test.Foo" !![Type.variable "test._T"]]
    ~instantiated:(Type.parametric "test.Bar" !![Type.parametric "test.Bar" !![Type.integer]])
    {|
      _V = typing.TypeVar('_V', covariant=True)
      class Foo(typing.Generic[_V]):
        pass
      _V2 = typing.TypeVar('_V2')
      class Bar(Foo[_V2]):
        pass
    |}
    [Type.Variable.Unary.create "test._T", Type.integer];
  let t_bound =
    Type.Variable.Unary.create
      ~constraints:(Type.Variable.Bound (Type.Primitive "test.Bound"))
      "test.T_Bound"
  in
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.Primitive "test.Bound"])
    {|
      class Bound:
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
    [t_bound, Type.Primitive "test.Bound"];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.Primitive "test.UnderBound"])
    {|
      class Bound:
        pass
      class UnderBound(Bound):
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
    [t_bound, Type.Primitive "test.UnderBound"];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.Primitive "test.OverBound"])
    {|
      class Bound:
        pass
      class OverBound():
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
    [];
  let t_explicit =
    Type.Variable.Unary.create
      ~constraints:(Type.Variable.Explicit [Type.integer; Type.string])
      "test.T_Explicit"
  in
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.integer])
    {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
    [t_explicit, Type.integer];
  assert_constraints
    ~target:"test.Foo"
    ~instantiated:(Type.parametric "test.Foo" !![Type.bool])
    {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
    []


let test_metaclasses context =
  let assert_metaclass ~source ~target metaclass =
    let target = "test." ^ target in
    let metaclass =
      if metaclass = "type" then
        metaclass
      else
        "test." ^ metaclass
    in
    let _, ast_environment, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let source =
      AstEnvironment.ReadOnly.get_source
        (AstEnvironment.read_only ast_environment)
        (Reference.create "test")
    in
    let source = Option.value_exn source in
    let { Source.statements; _ } = source in
    let target =
      let target = function
        | { Node.location; value = Statement.Class ({ StatementClass.name; _ } as definition) }
          when Reference.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ -> None
      in
      List.find_map ~f:target statements
    in
    let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    match target with
    | Some target ->
        assert_equal (Type.Primitive metaclass) (Annotated.Class.metaclass ~resolution target)
    | None -> assert_unreached ()
  in
  assert_metaclass ~source:{|
       class C:
         pass
    |} ~target:"C" "type";
  assert_metaclass
    ~source:{|
      class Meta:
        pass
      class C(metaclass=Meta):
        pass
    |}
    ~target:"C"
    "Meta";
  assert_metaclass
    ~source:
      {|
      class Meta:
        pass
      class C(metaclass=Meta):
        pass
      class D(C):
        pass
    |}
    ~target:"D"
    "Meta";
  assert_metaclass
    ~source:
      {|
      class Meta:
        pass
      class MoreMeta(Meta):
        pass
      class C(metaclass=Meta):
        pass
      class Other(metaclass=MoreMeta):
        pass
      class D(C, Other):
        pass
    |}
    ~target:"D"
    "MoreMeta";
  assert_metaclass
    ~source:
      {|
      class Meta:
        pass
      class MoreMeta(Meta):
        pass
      class C(metaclass=Meta):
        pass
      class Other(metaclass=MoreMeta):
        pass
      class D(Other, C):
        pass
    |}
    ~target:"D"
    "MoreMeta";

  (* If we don't have a "most derived metaclass", pick an arbitrary one. *)
  assert_metaclass
    ~source:
      {|
      class Meta:
        pass
      class MoreMeta(Meta):
        pass
      class OtherMeta(Meta):
        pass
      class C(metaclass=MoreMeta):
        pass
      class Other(metaclass=OtherMeta):
        pass
      class D(Other, C):
        pass
    |}
    ~target:"D"
    "OtherMeta";
  assert_metaclass
    ~source:
      {|
      class Meta:
        pass
      class MoreMeta(Meta):
        pass
      class OtherMeta(Meta):
        pass
      class C(metaclass=MoreMeta):
        pass
      class Other(metaclass=OtherMeta):
        pass
      class D(C, Other):
        pass
    |}
    ~target:"D"
    "MoreMeta"


let test_overrides context =
  let resolution =
    let _, _, environment =
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
      |> ScratchProject.build_environment
    in
    AnnotatedGlobalEnvironment.ReadOnly.resolution environment
  in
  let definition =
    let definition =
      GlobalResolution.class_definition resolution (Type.Primitive "test.Baz") >>| Class.create
    in
    Option.value_exn ~message:"Missing definition." definition
  in
  assert_is_none (Class.overrides definition ~resolution ~name:"baz");
  let overrides = Class.overrides definition ~resolution ~name:"foo" in
  assert_is_some overrides;
  assert_equal ~cmp:String.equal (Attribute.name (Option.value_exn overrides)) "foo";
  assert_equal (Option.value_exn overrides |> Attribute.parent |> Type.show) "test.Foo"


let test_implicit_attributes context =
  let assert_unimplemented_attributes_equal ~source ~class_name ~expected =
    let _, _, environment =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_environment
    in
    let resolution = AnnotatedGlobalEnvironment.ReadOnly.resolution environment in
    let definition =
      let definition =
        GlobalResolution.class_definition resolution (Type.Primitive class_name) >>| Class.create
      in
      Option.value_exn ~message:"Missing definition." definition
    in
    let attributes =
      Class.implicit_attributes definition
      |> Identifier.SerializableMap.bindings
      |> List.map ~f:snd
      |> List.map ~f:(fun { Node.value = { StatementAttribute.name; _ }; _ } -> name)
    in
    assert_equal attributes expected
  in
  assert_unimplemented_attributes_equal
    ~expected:["__init__"; "x"; "y"]
    ~source:
      {|
      class Foo:
        def __init__(self):
            self.x = 1
            self.y = ""
    |}
    ~class_name:"test.Foo"


let () =
  "class"
  >::: [
         "attributes" >:: test_class_attributes;
         "constraints" >:: test_constraints;
         "constructors" >:: test_constructors;
         "fallback_attribute" >:: test_fallback_attribute;
         "generics" >:: test_generics;
         "get_decorator" >:: test_get_decorator;
         "is_protocol" >:: test_is_protocol;
         "metaclasses" >:: test_metaclasses;
         "overrides" >:: test_overrides;
         "superclasses" >:: test_superclasses;
         "implicit_attributes" >:: test_implicit_attributes;
       ]
  |> Test.run
