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
module Class = Annotated.Class
module Attribute = Annotated.Attribute
module Method = Annotated.Method
module Argument = Call.Argument

let ( !! ) concretes = Type.OrderedTypes.Concrete concretes

let value option = Option.value_exn option

let last_statement_exn = function
  | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
  | _ -> failwith "Could not parse last statement"


let test_generics context =
  let assert_generics source generics =
    let sources, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    match List.hd_exn sources |> last_statement_exn with
    | { Node.value = Statement.Class definition; _ } ->
        let resolution = Environment.resolution environment () in
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
    !![Type.variable "_T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      class Foo(typing.Generic[_T, _S]): pass
    |}
    !![Type.variable "_T"; Type.variable "_S"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Protocol[_T]): pass
    |}
    !![Type.variable "_T"];
  assert_generics
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Iterable[_T]): pass
    |}
    !![Type.variable "_T"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Dict[_T1, _T2]): pass
    |}
    !![Type.variable "_T1"; Type.variable "_T2"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Iterable[_T1], typing.AsyncIterable[_T2]): pass
    |}
    !![Type.variable "_T1"; Type.variable "_T2"];
  assert_generics
    {|
      _T1 = typing.TypeVar('_T1')
      class Foo(typing.Dict[_T1, _T1]): pass
    |}
    !![Type.variable "_T1"];
  ()


let test_superclasses context =
  let _, _, environment =
    ScratchProject.setup
      ~context
      [ ( "test.py",
          {|
      class Foo: pass
      class Bar: pass
      class SubFoo(Foo): pass
      class SubFooBar(Foo, Bar): pass
      class SubRecurse(SubFooBar): pass
      class SubRedundant(Foo, SubFooBar): pass
    |}
        ) ]
    |> ScratchProject.build_environment
  in
  let ( ! ) name =
    {
      Statement.Class.name = !&name;
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
    |> Node.create_with_default_location
    |> Class.create
  in
  let resolution = Environment.resolution environment () in
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
      Environment.resolution environment ()
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
    [ {
        name = "decorator";
        arguments =
          Some
            [ { Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Name (Name.Identifier "d") } ];
      } ];
  assert_get_decorator
    {|
      @decorator(a=b)
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator"
    [ {
        name = "decorator";
        arguments = Some [{ Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") }];
      };
      {
        name = "decorator";
        arguments =
          Some
            [ { Argument.name = Some ~+"a"; value = +Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Name (Name.Identifier "d") } ];
      } ];
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
    Class.AttributeCache.clear ();
    let resolution =
      let _, _, environment =
        ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
      in
      Environment.resolution environment ()
    in
    let instantiated =
      parse_single_expression instantiated
      |> GlobalResolution.parse_annotation ~allow_invalid_type_parameters:true resolution
    in
    match parse_last_statement source with
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
  assert_constructor "class Foo: pass" "Foo" (Some "typing.Callable('object.__init__')[[], Foo]");
  assert_constructor "class Foo: ..." "Foo" (Some "typing.Callable('object.__init__')[[], Foo]");

  (* Statement.Defined constructors. *)
  assert_constructor
    {|
      class Foo:
        def Foo.__init__(self, a: int) -> None: pass
    |}
    "Foo"
    (Some "typing.Callable('Foo.__init__')[[Named(a, int)], Foo]");
  assert_constructor
    {|
      class Foo:
        def Foo.__init__(self, a: int) -> None: pass
        @typing.overload
        def Foo.__init__(self, b: str) -> None: pass
    |}
    "Foo"
    (Some ("typing.Callable('Foo.__init__')[[Named(a, int)], Foo]" ^ "[[[Named(b, str)], Foo]]"));

  (* Generic classes. *)
  assert_constructor
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        def Foo.__init__(self) -> None: pass
    |}
    "Foo"
    (Some "typing.Callable('Foo.__init__')[[], Foo[typing.TypeVar('_K'),typing.TypeVar('_V')]]");
  assert_constructor
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        def Foo.__init__(self, x:_K, y:_V) -> None: pass
    |}
    "Foo[int, str]"
    (Some "typing.Callable('Foo.__init__')[[Named(x, int), Named(y, str)], Foo[int, str]]");

  (* Tuples. *)
  assert_constructor
    {|
      _T = typing.TypeVar('_T')
      class tuple(typing.Generic[_T]):
        def tuple.__init__(self) -> None: ...
    |}
    "tuple"
    (Some "typing.Callable('tuple.__init__')[[], typing.Tuple[typing.TypeVar('_T'), ...]]");

  (* Constructors, both __init__ and __new__, are inherited from parents. *)
  assert_constructor
    {|
      class Parent:
        def Parent.__init__(self, x: int) -> None:
          pass
      class C(Parent):
        pass
    |}
    "C"
    (Some "typing.Callable('Parent.__init__')[[Named(x, int)], C]");
  assert_constructor
    {|
      class Parent:
        def Parent.__new__(self, x: str) -> None:
          pass
      class C(Parent):
        pass
    |}
    "C"
    (Some "typing.Callable('Parent.__new__')[[Named(x, str)], C]");
  assert_constructor
    {|
      T = typing.TypeVar('T', bound=C)
      class C:
        def C.__init__(self, x: int) -> None: pass
    |}
    "T"
    (Some "typing.Callable('C.__init__')[[Named(x, int)], T]");
  ()


let test_methods context =
  let assert_methods source methods =
    let sources, _ =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.parse_sources
    in
    match List.hd_exn sources |> last_statement_exn with
    | { Node.value = Statement.Class definition; _ } ->
        let actuals =
          let method_name { Define.signature = { name; _ }; _ } = Reference.last name in
          Node.create_with_default_location definition
          |> Class.create
          |> Class.methods
          |> List.map ~f:(fun definition -> Method.define definition |> method_name)
        in
        assert_equal methods actuals
    | _ -> assert_unreached ()
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


let test_has_method context =
  let get_actual source target_method =
    let resolution =
      let _, _, environment =
        ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
      in
      Environment.resolution environment ()
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
      { Statement.Class.name = !&"Derp"; bases; body = []; decorators = []; docstring = None }
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
    let sources, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    let parent =
      match List.hd_exn sources |> last_statement_exn with
      | { Node.value = Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    Environment.resolution environment (), Class.create (Node.create_with_default_location parent)
  in
  let resolution, parent =
    setup
      {|
        class type:
          __name__: str = 'asdf'
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
  let create_attribute
      ?(annotation = Some !"int")
      ?(async = false)
      ?defines
      ?(final = false)
      ?(frozen = false)
      ?(implicit = false)
      ?(primitive = false)
      ?(property = false)
      ?(setter = false)
      ?(static = false)
      ?(toplevel = true)
      ?value
      name
    =
    +{
       Statement.Attribute.annotation;
       async;
       defines;
       final;
       frozen;
       implicit;
       name;
       property;
       primitive;
       setter;
       static;
       toplevel;
       value;
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
    [ Class.create_attribute ~resolution ~parent (create_attribute "__init__");
      Class.create_attribute ~resolution ~parent (create_attribute "class_attribute");
      Class.create_attribute ~resolution ~parent (create_attribute "first");
      Class.create_attribute ~resolution ~parent (create_attribute "implicit");
      Class.create_attribute ~resolution ~parent (create_attribute "second");
      Class.create_attribute ~resolution ~parent (create_attribute "third" ~value:(+Integer 1)) ];

  (* Test `Attribute`. *)
  let attribute =
    Class.create_attribute ~resolution ~parent (create_attribute ~annotation:(Some !"int") "first")
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
      (create_attribute
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
      class type:
        __name__: str = 'asdf'
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
      class type:
        __name__: str = 'asdf'
      class foo():
        def __init__(self):
          self.implicit: int = 1
        first: int
        second: int
        third: int = 1
        class_attribute: typing.ClassVar[int]
    |}
    [ "class_attribute";
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
      "__str__" ];
  assert_fold
    ~class_attributes:true
    {|
      class type:
        __type__: str
      class Meta(type):
        __meta__: str
      class Foo(metaclass=Meta):
        __static__: typing.ClassVar[int]
        __instance__: int
    |}
    [ "__instance__";
      "__static__";
      "__meta__";
      "__type__";
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
      "__str__" ];

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
      ?(parent = Type.Primitive "Attributes")
      name
      callable
    =
    {
      Class.Attribute.annotation =
        Annotation.create_immutable ~global:true (parse_callable callable);
      async = false;
      class_attribute = false;
      defined = true;
      final = false;
      initialized = false;
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
      (create_expected_attribute "bar" "typing.Callable('Attributes.bar')[[], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.Primitive "Attributes")
    ~attribute_name:"baz"
    ~expected_attribute:
      (create_expected_attribute "baz" "typing.Callable('Attributes.baz')[[Named(x, int)], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"implicit"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:(Type.Primitive "Metaclass")
         "implicit"
         "typing.Callable('Metaclass.implicit')[[], int]");
  assert_attribute
    ~parent
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"property"
    ~expected_attribute:(create_expected_attribute ~property:(Some ReadOnly) "property" "str")


let test_fallback_attribute context =
  let assert_fallback_attribute ~name source annotation =
    Class.AttributeCache.clear ();
    let resolution =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_resolution
    in
    let attribute =
      parse_last_statement source
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
    (Some (parse_callable "typing.Callable('Foo.__add__')[[Named(other, Foo)], int]"));
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
    let resolution =
      let _, _, environment =
        ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
      in
      Environment.resolution environment ()
    in
    let target =
      let { Source.statements; _ } = parse source in
      let target = function
        | { Node.location; value = Statement.Class ({ Statement.Class.name; _ } as definition) }
          when Reference.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ -> None
      in
      List.find_map ~f:target statements |> value
    in
    let constraints =
      parse_last_statement source
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
    Type.Union [Type.parametric "Foo" !![Type.string]; Type.integer]
  in
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![int_and_foo_string_union])
    {|
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_V]):
        pass
    |}
    [Type.Variable.Unary.create "_V", int_and_foo_string_union];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.Primitive "Foo")
    {|
      class Foo:
        pass
    |}
    [];

  (* Consequence of the special case we need to remove *)
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.Bottom])
    {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        pass
    |}
    [];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [Type.Variable.Unary.create "_K", Type.integer; Type.Variable.Unary.create "_V", Type.float];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.integer; Type.float])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
    [Type.Variable.Unary.create "_K", Type.integer; Type.Variable.Unary.create "_V", Type.float];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.Primitive "Foo")
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
    ~instantiated:(Type.Primitive "Foo")
    {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
    [Type.Variable.Unary.create "_T", Type.integer];
  assert_constraints
    ~target:"Bar"
    ~instantiated:(Type.parametric "Foo" !![Type.integer])
    {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_V]):
        pass
      class Foo(typing.Generic[_K], Bar[_K]):
        pass
    |}
    [Type.Variable.Unary.create "_V", Type.integer];
  assert_constraints
    ~target:"Bar"
    ~instantiated:(Type.parametric "Foo" !![Type.integer; Type.float])
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
    [Type.Variable.Unary.create "_T", Type.integer];
  assert_constraints
    ~target:"Baz"
    ~instantiated:(Type.parametric "Foo" !![Type.integer; Type.float])
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
    [Type.Variable.Unary.create "_T", Type.float];
  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterator" !![Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
    |}
    [Type.Variable.Unary.create "_T", Type.integer];
  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterable" !![Type.integer])
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.Variable.Unary.create "_T", Type.integer];
  assert_constraints
    ~target:"Iterator"
    ~instantiated:(Type.parametric "Iterable" !![Type.parametric "Iterable" !![Type.integer]])
    ~parameters:!![Type.parametric "Iterable" !![Type.variable "_T"]]
    {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
    [Type.Variable.Unary.create "_T", Type.integer];
  assert_constraints
    ~target:"Foo"
    ~parameters:!![Type.parametric "Foo" !![Type.variable "_T"]]
    ~instantiated:(Type.parametric "Bar" !![Type.parametric "Bar" !![Type.integer]])
    {|
      _V = typing.TypeVar('_V', covariant=True)
      class Foo(typing.Generic[_V]):
        pass
      _V2 = typing.TypeVar('_V2')
      class Bar(Foo[_V2]):
        pass
    |}
    [Type.Variable.Unary.create "_T", Type.integer];
  let t_bound =
    Type.Variable.Unary.create
      ~constraints:(Type.Variable.Bound (Type.Primitive "Bound"))
      "T_Bound"
  in
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.Primitive "Bound"])
    {|
      class Bound:
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
    [t_bound, Type.Primitive "Bound"];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.Primitive "UnderBound"])
    {|
      class Bound:
        pass
      class UnderBound(Bound):
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
    [t_bound, Type.Primitive "UnderBound"];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.Primitive "OverBound"])
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
      "T_Explicit"
  in
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.integer])
    {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
    [t_explicit, Type.integer];
  assert_constraints
    ~target:"Foo"
    ~instantiated:(Type.parametric "Foo" !![Type.bool])
    {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
    []


let test_inferred_generic_base context =
  let assert_inferred_generic ~target source expected =
    let sources, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    let { Source.statements; _ } = List.hd_exn sources in
    let target =
      let target = function
        | { Node.location; value = Statement.Class ({ Statement.Class.name; _ } as definition) }
          when Reference.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ -> None
      in
      List.find_map ~f:target statements |> value
    in
    let resolution = Environment.resolution environment () in
    assert_equal
      ~cmp:(List.equal Argument.equal)
      expected
      (Annotated.Class.inferred_generic_base target ~resolution)
  in
  assert_inferred_generic
    ~target:"C"
    {|
       _T = typing.TypeVar('_T')
       class C:
         pass
     |}
    [];
  assert_inferred_generic
    ~target:"C"
    {|
       _T = typing.TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(List[_T]):
         pass
     |}
    [ {
        Argument.name = None;
        value = Type.expression (Type.parametric "typing.Generic" !![Type.variable "_T"]);
      } ];
  assert_inferred_generic
    ~target:"List"
    {|
       _T = TypeVar("_T")
       class Iterable(typing.Generic[_T]):
         pass
       class List(Iterable[_T], typing.Generic[_T]):
         pass
     |}
    [];
  assert_inferred_generic
    ~target:"Foo"
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Dict[_T1, _T2]): pass
    |}
    [ {
        Argument.name = None;
        value =
          Type.expression
            (Type.parametric "typing.Generic" !![Type.variable "_T1"; Type.variable "_T2"]);
      } ];
  assert_inferred_generic
    ~target:"Foo"
    {|
      _T1 = typing.TypeVar('_T1')
      class Foo(typing.Dict[_T1, _T1]): pass
    |}
    [ {
        Argument.name = None;
        value = Type.expression (Type.parametric "typing.Generic" !![Type.variable "_T1"]);
      } ];
  ()


let test_metaclasses context =
  let assert_metaclass ~source ~target metaclass =
    let sources, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    let { Source.statements; _ } = List.hd_exn sources in
    let target =
      let target = function
        | { Node.location; value = Statement.Class ({ Statement.Class.name; _ } as definition) }
          when Reference.show name = target ->
            Some (Class.create { Node.location; value = definition })
        | _ -> None
      in
      List.find_map ~f:target statements
    in
    let resolution = Environment.resolution environment () in
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
        [ ( "__init__.py",
            {|
      class Foo:
        def Foo.foo(): pass
      class Bar(Foo):
        pass
      class Baz(Bar):
        def Baz.foo(): pass
        def Baz.baz(): pass
    |}
          ) ]
      |> ScratchProject.build_environment
    in
    Environment.resolution environment ()
  in
  let definition =
    let definition =
      GlobalResolution.class_definition resolution (Type.Primitive "Baz") >>| Class.create
    in
    Option.value_exn ~message:"Missing definition." definition
  in
  assert_is_none (Class.overrides definition ~resolution ~name:"baz");
  let overrides = Class.overrides definition ~resolution ~name:"foo" in
  assert_is_some overrides;
  assert_equal ~cmp:String.equal (Attribute.name (Option.value_exn overrides)) "foo";
  assert_equal (Option.value_exn overrides |> Attribute.parent |> Type.show) "Foo"


let test_unimplemented_abstract_methods context =
  let assert_unimplemented_methods_equal ~source ~class_name ~expected =
    let _, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    let resolution = Environment.resolution environment () in
    let definition =
      let definition =
        GlobalResolution.class_definition resolution (Type.Primitive class_name) >>| Class.create
      in
      Option.value_exn ~message:"Missing definition." definition
    in
    let unimplemented_methods =
      Class.unimplemented_abstract_methods ~resolution definition
      |> List.map ~f:Statement.Define.unqualified_name
    in
    assert_equal unimplemented_methods expected
  in
  assert_unimplemented_methods_equal
    ~expected:["foo"]
    ~source:
      {|
      class Foo(metaclass=abc.ABCMeta):
        @abstractmethod
        def foo(self) -> None:
          pass
      class Bar(Foo):
        pass
    |}
    ~class_name:"Bar";
  assert_unimplemented_methods_equal
    ~expected:[]
    ~source:
      {|
      class Foo(metaclass=abc.ABCMeta):
        @abstractmethod
        def foo(self) -> None:
          pass
      class Bar(Foo):
        def foo() -> None:
          pass
    |}
    ~class_name:"Bar"


let test_implicit_attributes context =
  let assert_unimplemented_attributes_equal ~source ~class_name ~expected =
    let _, _, environment =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_environment
    in
    let resolution = Environment.resolution environment () in
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
      |> List.map ~f:(fun { Node.value = { Statement.Attribute.name; _ }; _ } -> name)
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
    ~class_name:"Foo"


let () =
  "class"
  >::: [ "attributes" >:: test_class_attributes;
         "constraints" >:: test_constraints;
         "constructors" >:: test_constructors;
         "fallback_attribute" >:: test_fallback_attribute;
         "generics" >:: test_generics;
         "get_decorator" >:: test_get_decorator;
         "inferred_generic_base" >:: test_inferred_generic_base;
         "is_protocol" >:: test_is_protocol;
         "metaclasses" >:: test_metaclasses;
         "methods" >:: test_methods;
         "overrides" >:: test_overrides;
         "superclasses" >:: test_superclasses;
         "unimplemented_abstract_methods" >:: test_unimplemented_abstract_methods;
         "implicit_attributes" >:: test_implicit_attributes ]
  |> Test.run
