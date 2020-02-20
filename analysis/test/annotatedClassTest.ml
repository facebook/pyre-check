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
module Class = Annotated.Class
module Attribute = Annotated.Attribute
module Argument = Call.Argument

let ( !! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

let last_statement_exn = function
  | { Source.statements; _ } when List.length statements > 0 -> List.last_exn statements
  | _ -> failwith "Could not parse last statement"


let test_superclasses context =
  let resolution =
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
    |> ScratchProject.build_global_resolution
  in
  let assert_successors target expected =
    let actual = GlobalResolution.successors ~resolution target in
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      ~cmp:(List.equal Type.Primitive.equal)
      expected
      actual
  in
  assert_successors "test.Foo" ["object"];
  assert_successors "test.SubRedundant" ["test.SubFooBar"; "test.Foo"; "test.Bar"; "object"];
  assert_successors "test.SubFoo" ["test.Foo"; "object"];
  assert_successors "test.SubFooBar" ["test.Foo"; "test.Bar"; "object"];
  assert_successors "test.SubRecurse" ["test.SubFooBar"; "test.Foo"; "test.Bar"; "object"];
  ()


let test_get_decorator context =
  let assert_get_decorator source decorator expected =
    let resolution =
      ScratchProject.setup ~context ["__init__.py", source]
      |> ScratchProject.build_global_resolution
    in
    let assert_logic expected =
      match parse_last_statement source with
      | { Node.value = Statement.Class definition; _ } ->
          let actual =
            Node.create_with_default_location definition
            |> Node.map ~f:ClassSummary.create
            |> Class.create
            |> AstEnvironment.ReadOnly.get_decorator
                 (GlobalResolution.ast_environment resolution)
                 ~decorator
          in
          let equal_decorator left right =
            let open AstEnvironment.ReadOnly in
            String.equal left.name right.name
            && Option.equal
                 (List.equal (fun left right ->
                      Call.Argument.location_insensitive_compare left right = 0))
                 left.arguments
                 right.arguments
          in
          assert_equal
            ~printer:(List.to_string ~f:AstEnvironment.ReadOnly.show_decorator)
            ~cmp:(List.equal equal_decorator)
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
              { Argument.name = Some ~+"a"; value = +Expression.Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Expression.Name (Name.Identifier "d") };
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
        arguments =
          Some [{ Argument.name = Some ~+"a"; value = +Expression.Name (Name.Identifier "b") }];
      };
      {
        name = "decorator";
        arguments =
          Some
            [
              { Argument.name = Some ~+"a"; value = +Expression.Name (Name.Identifier "b") };
              { Argument.name = Some ~+"c"; value = +Expression.Name (Name.Identifier "d") };
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
    let { ScratchProject.BuiltGlobalEnvironment.ast_environment; global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
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
    | { Node.value = Statement.Class { name; _ }; _ } ->
        let callable =
          constructors
          >>| (fun constructors ->
                GlobalResolution.parse_annotation resolution (parse_single_expression constructors))
          |> Option.value ~default:Type.Top
        in
        let actual =
          Node.value name
          |> Reference.show
          |> GlobalResolution.constructor ~resolution ~instantiated
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
    (Some "typing.Callable('test.Foo.__init__')[[Named(x, int), Named(y, str)], test.Foo[int, str]]");

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


let test_is_protocol _ =
  let assert_is_protocol bases expected =
    let is_protocol bases =
      { StatementClass.name = + !&"Derp"; bases; body = []; decorators = [] }
      |> ClassSummary.create
      |> ClassSummary.is_protocol
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
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    GlobalResolution.create global_environment
  in
  let resolution =
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
      ?(annotation = Type.integer)
      ?(class_attribute = false)
      ?(initialized = Attribute.Explicitly)
      ~parent
      name
    =
    Annotated.Attribute.create
      ~abstract:false
      ~annotation
      ~original_annotation:annotation
      ~async:false
      ~class_attribute
      ~defined:true
      ~initialized
      ~name
      ~parent
      ~visibility:ReadWrite
      ~property:false
      ~static:false
  in
  (* Test `Class.attributes`. *)
  let assert_attributes definition attributes =
    let attribute_list_equal = List.equal Attribute.equal_instantiated in
    let print_attributes attributes =
      let print_attribute attribute =
        Annotated.Attribute.sexp_of_instantiated attribute |> Sexp.to_string_hum
      in
      List.map attributes ~f:print_attribute |> String.concat ~sep:", "
    in
    let print format definition =
      Format.fprintf
        format
        "%s"
        (Sexp.to_string_hum [%message (definition : Attribute.instantiated list)])
    in
    assert_equal
      ~cmp:attribute_list_equal
      ~printer:print_attributes
      ~pp_diff:(diff ~print)
      ( GlobalResolution.attributes ~resolution definition
      |> (fun a -> Option.value_exn a)
      |> List.map ~f:(GlobalResolution.instantiate_attribute ~resolution) )
      attributes
  in
  let constructor =
    Type.Callable.create
      ~name:(Reference.create "test.foo.__init__")
      ~parameters:(Defined [])
      ~annotation:Any
      ()
  in
  assert_attributes
    "test.foo"
    [
      create_simple_attribute
        ~parent:"test.foo"
        ~annotation:constructor
        ~initialized:Implicitly
        "__init__";
      create_simple_attribute
        ~parent:"test.foo"
        ~class_attribute:true
        ~initialized:NotInitialized
        "class_attribute";
      create_simple_attribute ~parent:"test.foo" ~initialized:NotInitialized "first";
      create_simple_attribute ~parent:"test.foo" ~initialized:Implicitly "implicit";
      create_simple_attribute ~parent:"test.foo" ~initialized:NotInitialized "second";
      create_simple_attribute ~parent:"test.foo" "third";
    ];

  (*(* Test 'attribute' *)*)
  let resolution =
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
      GlobalResolution.attribute_from_class_name
        parent
        ~transitive:true
        ~class_attributes
        ~resolution
        ~name:attribute_name
        ~instantiated
    in
    let cmp =
      let equal = Attribute.equal_instantiated in
      Option.equal equal
    in
    let printer = Option.value_map ~default:"None" ~f:Attribute.show_instantiated in
    assert_equal ~cmp ~printer expected_attribute actual_attribute
  in
  let create_expected_attribute
      ?(property = false)
      ?(visibility = Attribute.ReadWrite)
      ?(parent = "test.Attributes")
      ?(initialized = Annotated.Attribute.Implicitly)
      ?(defined = true)
      name
      callable
    =
    let annotation = parse_callable callable in
    Some
      (Annotated.Attribute.create
         ~annotation
         ~original_annotation:annotation
         ~abstract:false
         ~async:false
         ~class_attribute:false
         ~defined
         ~initialized
         ~name
         ~parent
         ~property
         ~visibility
         ~static:false)
  in
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "Attributes")
    ~attribute_name:"bar"
    ~expected_attribute:
      (create_expected_attribute "bar" "typing.Callable('test.Attributes.bar')[[], int]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "Attributes")
    ~attribute_name:"baz"
    ~expected_attribute:
      (create_expected_attribute
         "baz"
         "typing.Callable('test.Attributes.baz')[[Named(x, int)], int]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"implicit"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.Metaclass"
         "implicit"
         "typing.Callable('test.Metaclass.implicit')[[], int]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.meta (Type.Primitive "Attributes"))
    ~attribute_name:"property"
    ~expected_attribute:
      (create_expected_attribute ~property:true ~visibility:(ReadOnly Unrefinable) "property" "str");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "Nonsense")
    ~attribute_name:"property"
    ~expected_attribute:
      (create_expected_attribute ~property:true ~visibility:(ReadOnly Unrefinable) "property" "str");
  ()


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
      |> (function
           | { Node.location; value = Statement.Class definition; _ } ->
               Node.create ~location definition |> Node.map ~f:ClassSummary.create |> Class.create
           | _ -> failwith "Last statement was not a class")
      |> Class.name
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
    (Some Type.none)


let test_constraints context =
  let assert_constraints ~target ~instantiated ?parameters source expected =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let constraints =
      GlobalResolution.constraints ~target ~resolution ?parameters ~instantiated ()
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
    let { ScratchProject.BuiltGlobalEnvironment.ast_environment; global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
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
          when Reference.show (Node.value name) = target ->
            { Node.location; value = definition }
            |> Node.map ~f:ClassSummary.create
            |> Class.create
            |> Option.some
        | _ -> None
      in
      List.find_map ~f:target statements
    in
    let resolution = GlobalResolution.create global_environment in
    match target with
    | Some target ->
        assert_equal (Type.Primitive metaclass) (GlobalResolution.metaclass ~resolution target)
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


let test_implicit_attributes context =
  let assert_unimplemented_attributes_equal ~source ~class_name ~expected =
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_resolution
    in
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
         "get_decorator" >:: test_get_decorator;
         "is_protocol" >:: test_is_protocol;
         "metaclasses" >:: test_metaclasses;
         "overrides" >:: test_overrides;
         "superclasses" >:: test_superclasses;
         "implicit_attributes" >:: test_implicit_attributes;
       ]
  |> Test.run
