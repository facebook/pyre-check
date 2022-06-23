(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Statement
open Pyre
open Test
module StatementClass = Class
module Attribute = Annotated.Attribute
module Argument = Call.Argument

let ( !! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

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


let test_first_matching_decorator context =
  let assert_first_matching_decorator source name expected =
    let resolution =
      ScratchProject.setup ~context ["__init__.py", source]
      |> ScratchProject.build_global_resolution
    in
    let assert_logic expected =
      match parse_last_statement source with
      | { Node.value = Statement.Class definition; _ } ->
          let actual =
            let class_summary = ClassSummary.create ~qualifier:Reference.empty definition in
            UnannotatedGlobalEnvironment.ReadOnly.first_matching_class_decorator
              (GlobalResolution.unannotated_global_environment resolution)
              ~names:[name]
              (Node.create_with_default_location class_summary)
          in
          let equal_decorator left right =
            let open Decorator in
            Reference.equal left.name.value right.name.value
            && Option.equal
                 (List.equal (fun left right ->
                      Call.Argument.location_insensitive_compare left right = 0))
                 left.arguments
                 right.arguments
          in
          let printer = function
            | Some decorator -> Decorator.show decorator
            | None -> "None"
          in
          assert_equal ~printer ~cmp:(Option.equal equal_decorator) expected actual
      | _ -> assert_true (Option.is_empty expected)
    in
    assert_logic expected
  in
  assert_first_matching_decorator "class A: pass" "decorator" None;
  assert_first_matching_decorator
    {|
      @decorator
      class A:
        pass
    |}
    "decorator"
    (Some { name = + !&"decorator"; arguments = None });
  assert_first_matching_decorator
    {|
      @decorator.a.b
      class A:
        pass
    |}
    "decorator.a"
    None;
  assert_first_matching_decorator
    {|
      @decorator
      class A:
        pass
    |}
    "decorator.a"
    None;
  assert_first_matching_decorator
    {|
      @decorator.a.b
      class A:
        pass
    |}
    "decorator.a.b"
    (Some { Decorator.name = + !&"decorator.a.b"; arguments = None });
  assert_first_matching_decorator
    {|
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator.a.b"
    None;
  assert_first_matching_decorator
    {|
      @other.decorator
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator"
    (Some
       {
         Decorator.name = + !&"decorator";
         arguments =
           Some
             [
               { Argument.name = Some ~+"a"; value = +Expression.Name (Name.Identifier "b") };
               { Argument.name = Some ~+"c"; value = +Expression.Name (Name.Identifier "d") };
             ];
       });
  assert_first_matching_decorator
    {|
      @decorator(a=b)
      @decorator(a=b, c=d)
      class A:
        pass
    |}
    "decorator"
    (Some
       {
         Decorator.name = + !&"decorator";
         arguments =
           Some [{ Argument.name = Some ~+"a"; value = +Expression.Name (Name.Identifier "b") }];
       });
  assert_first_matching_decorator
    (* `enum` imports `ABCMeta` from `abc`. *)
    {|
      import enum
      @enum.ABCMeta
      class A:
        pass
    |}
    "abc.ABCMeta"
    (Some { name = + !&"abc.ABCMeta"; arguments = None });
  ()


let test_constructors context =
  let assert_constructor source instantiated constructors =
    let instantiated = "test." ^ instantiated in
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let instantiated =
      parse_single_expression instantiated
      |> GlobalResolution.parse_annotation ~validation:ValidatePrimitives resolution
    in
    let callable =
      constructors
      >>| (fun constructors ->
            GlobalResolution.parse_annotation
              ~validation:NoValidation
              resolution
              (parse_single_expression constructors))
      |> Option.value ~default:Type.Top
    in
    let actual =
      GlobalResolution.attribute_from_annotation
        ~special_method:true
        ~parent:(Type.meta instantiated)
        resolution
        ~name:"__call__"
      |> (fun option -> Option.value_exn option)
      |> Annotated.Attribute.annotation
      |> Annotation.annotation
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal callable actual
  in
  (* Undefined constructors. *)
  assert_constructor
    "class Foo: pass"
    "Foo"
    (Some
       "BoundMethod[typing.Callable('object.__init__')[[Named(self, object)], test.Foo], test.Foo]");
  assert_constructor
    "class Foo: ..."
    "Foo"
    (Some
       "BoundMethod[typing.Callable('object.__init__')[[Named(self, object)], test.Foo], test.Foo]");

  (* Statement.Defined constructors. *)
  assert_constructor
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
    |}
    "Foo"
    (Some
       "BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, test.Foo), Named(a, int)], \
        test.Foo], test.Foo]");
  assert_constructor
    {|
      class Foo:
        def __init__(self, a: int) -> None: pass
        @typing.overload
        def __init__(self, b: str) -> None: pass
    |}
    "Foo"
    (Some
       ("BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, test.Foo), Named(a, int)], \
         test.Foo]"
       ^ "[[[Named(self, test.Foo), Named(b, str)], test.Foo]], test.Foo]"));

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
       "BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, \
        test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')])], \
        test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')]], \
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
       "BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, test.Foo[int, str]), \
        Named(x, int), Named(y, str)], test.Foo[int, str]], test.Foo[int, str]]");

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
    (Some
       "BoundMethod[typing.Callable('test.Parent.__init__')[[Named(self, test.Parent), Named(x, \
        int)], test.C], test.C]");
  assert_constructor
    {|
      class Parent:
        def __new__(self, x: str) -> None:
          pass
      class C(Parent):
        pass
    |}
    "C"
    (Some
       "BoundMethod[typing.Callable(test.Parent.__new__)[[Named(self, typing.Type[test.Parent]), \
        Named(x, str)], test.C], typing.Type[test.C]]");
  assert_constructor
    {|
      T = typing.TypeVar('T', bound=C)
      class C:
        def __init__(self, x: int) -> None: pass
    |}
    "T"
    (Some
       "BoundMethod[typing.Callable('test.C.__init__')[[Named(self, test.C), Named(x, int)], \
        test.T], test.T]");
  assert_constructor
    {|
      from dataclasses import dataclass
      @dataclass(frozen=True)
      class A:
          foo:int = 1
    |}
    "A"
    (Some
       "BoundMethod[typing.Callable('test.A.__init__')[[Named(self, test.A), Named(foo, int, \
        default)], test.A], test.A]");
  assert_constructor
    {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
        typing.Type[test.Base[test.T]]), Named(x, test.T)], test.Base[test.T]], \
        typing.Type[test.Base]]");
  (* With @final. *)
  assert_constructor
    {|
      from typing import final, Generic, Optional, TypeVar
      T = TypeVar("T")
      @final
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
        typing.Type[test.Base[test.T]]), Named(x, test.T)], test.Base[typing.Optional[test.T]]], \
        typing.Type[test.Base]]");
  assert_constructor
    {|
      from typing import final, Generic, Optional, TypeVar
      T = TypeVar("T")
      @final
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
    |}
    "Base[int]"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, typing.Type[test.Base[int]]), \
        Named(x, int)], test.Base[typing.Optional[int]]], typing.Type[test.Base[int]]]");
  assert_constructor
    {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
      class Child(Base[T]): ...
    |}
    "Child"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
        typing.Type[test.Base[test.T]]), Named(x, test.T)], test.Child[test.T]], \
        typing.Type[test.Child]]");
  assert_constructor
    {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      S = TypeVar("S")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
      class Child(Base[T], Generic[T, S]): ...
    |}
    "Child[int, test.S]"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, typing.Type[test.Base[int]]), \
        Named(x, int)], test.Child[int, test.S]], typing.Type[test.Child[int, test.S]]]");
  assert_constructor
    {|
      from typing import final, overload, Generic, List, Optional, TypeVar, Union
      T = TypeVar("T")
      @final
      class Base(Generic[T]):
        @overload
        def __new__(cls, x: List[T]) -> Base[T]: ...
        @overload
        def __new__(cls, x: T) -> Base[Optional[T]]: ...

        def __new__(cls, x: Union[T, List[T]]) -> Union[Base[T], Base[Optional[T]]]: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(cls, \
        typing.Type[test.Base[test.T]]), Named(x, typing.Union[typing.List[test.T], test.T])], \
        typing.Union[test.Base[typing.Optional[test.T]], test.Base[test.T]]][[[Named(cls, \
        typing.Type[test.Base[test.T]]), Named(x, typing.List[test.T])], \
        test.Base[test.T]][[Named(cls, typing.Type[test.Base[test.T]]), Named(x, test.T)], \
        test.Base[typing.Optional[test.T]]]], typing.Type[test.Base]]");
  (* Without @final. *)
  assert_constructor
    {|
      from typing import overload, Generic, List, Optional, TypeVar, Union
      T = TypeVar("T")
      class Base(Generic[T]):
        @overload
        def __new__(cls, x: List[T]) -> Base[T]: ...
        @overload
        def __new__(cls, x: T) -> Base[Optional[T]]: ...

        def __new__(cls, x: Union[T, List[T]]) -> Union[Base[T], Base[Optional[T]]]: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__new__)[[Named(cls, \
        typing.Type[test.Base[test.T]]), Named(x, typing.Union[typing.List[test.T], test.T])], \
        test.Base[test.T]][[[Named(cls, typing.Type[test.Base[test.T]]), Named(x, \
        typing.List[test.T])], test.Base[test.T]][[Named(cls, typing.Type[test.Base[test.T]]), \
        Named(x, test.T)], test.Base[test.T]]], typing.Type[test.Base]]");
  (* __init__ takes precedence over __new__ and ignores any return type for __new__. *)
  assert_constructor
    {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
        def __init__(self, x: T) -> None: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__init__)[[Named(self, test.Base[test.T]), Named(x, \
        test.T)], test.Base[test.T]], test.Base[test.T]]");
  assert_constructor
    {|
      from typing import final, overload, Generic, List, Optional, TypeVar, Union
      T = TypeVar("T")
      @final
      class Base(Generic[T]):
        @overload
        def __new__(cls, x: List[T]) -> Base[T]: ...
        @overload
        def __new__(cls, x: T) -> Base[Optional[T]]: ...

        def __new__(cls, x: Union[T, List[T]]) -> Union[Base[T], Base[Optional[T]]]: ...
        def __init__(self, x: Union[T, List[T]]) -> None: ...
    |}
    "Base"
    (Some
       "BoundMethod[typing.Callable(test.Base.__init__)[[Named(self, test.Base[test.T]), Named(x, \
        typing.Union[typing.List[test.T], test.T])], test.Base[test.T]], test.Base[test.T]]");
  ()


let test_is_protocol _ =
  let assert_is_protocol base_arguments expected =
    let is_protocol base_arguments =
      {
        StatementClass.name = !&"Derp";
        base_arguments;
        body = [];
        decorators = [];
        top_level_unbound_names = [];
      }
      |> ClassSummary.create ~qualifier:Reference.empty
      |> ClassSummary.is_protocol
    in
    assert_equal expected (is_protocol base_arguments)
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
      ?(uninstantiated_annotation = Some Type.integer)
      ?(class_variable = false)
      ?(initialized = Attribute.OnClass)
      ?undecorated_signature
      ~parent
      name
    =
    Annotated.Attribute.create
      ~abstract:false
      ~annotation
      ~original_annotation:annotation
      ~uninstantiated_annotation
      ~async_property:false
      ~class_variable
      ~defined:true
      ~initialized
      ~name
      ~parent
      ~visibility:ReadWrite
      ~property:false
      ~undecorated_signature
      ~problem:None
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
      (GlobalResolution.attributes ~resolution definition
      |> (fun a -> Option.value_exn a)
      |> List.map
           ~f:(GlobalResolution.instantiate_attribute ~resolution ~accessed_through_class:false))
      attributes
  in
  let uninstantiated_constructor =
    {
      Type.Callable.kind = Named (Reference.create "test.foo.__init__");
      implementation =
        {
          parameters =
            Defined
              [
                Named
                  {
                    annotation = Type.Primitive "test.foo";
                    default = false;
                    name = "$parameter$self";
                  };
              ];
          annotation = Type.Any;
        };
      overloads = [];
    }
  in
  let constructor =
    Type.parametric
      "BoundMethod"
      [Single (Callable uninstantiated_constructor); Single (Primitive "test.foo")]
  in

  assert_attributes
    "test.foo"
    [
      create_simple_attribute
        ~parent:"test.foo"
        ~annotation:constructor
        ~uninstantiated_annotation:(Some (Callable uninstantiated_constructor))
        ~undecorated_signature:uninstantiated_constructor
        ~initialized:OnClass
        "__init__";
      create_simple_attribute
        ~parent:"test.foo"
        ~class_variable:true
        ~initialized:NotInitialized
        "class_attribute";
      create_simple_attribute ~parent:"test.foo" ~initialized:NotInitialized "first";
      create_simple_attribute ~parent:"test.foo" ~initialized:OnlyOnInstance "implicit";
      create_simple_attribute ~parent:"test.foo" ~initialized:NotInitialized "second";
      create_simple_attribute ~parent:"test.foo" "third";
    ];

  (*(* Test 'attribute' *)*)
  let resolution =
    setup
      {|
        from dataclasses import dataclass
        from placeholder_stub import StubParent

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

        @dataclass
        class Parent:
          inherited: int

        @dataclass
        class DC(Parent):
          x: int
          y: str

        class NT(typing.NamedTuple):
          x: int
          y: str

        class Prot(typing.Protocol):
          def method(self, x: int) -> str: ...

        class ExplicitProtChild(Prot):
          def other(self, x: int) -> str: ...

        class ChildOfPlaceholderStub(StubParent):
          pass
      |}
  in
  let assert_attribute ~parent ~parent_instantiated_type ~attribute_name ~expected_attribute =
    let instantiated, accessed_through_class =
      if Type.is_meta parent_instantiated_type then
        Type.single_parameter parent_instantiated_type, true
      else
        parent_instantiated_type, false
    in
    let actual_attribute =
      GlobalResolution.attribute_from_class_name
        parent
        ~transitive:true
        ~accessed_through_class
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
      ?(initialized = Annotated.Attribute.OnClass)
      ?(defined = true)
      ?(class_variable = false)
      ?callable_name
      ?uninstantiated_annotation
      name
      callable
    =
    let annotation = parse_callable ?name:callable_name callable in
    let uninstantiated_annotation =
      uninstantiated_annotation >>| parse_callable ?name:callable_name
    in
    let undecorated_signature =
      match uninstantiated_annotation with
      | Some (Type.Callable callable) -> Some callable
      | _ -> None
    in
    Some
      (Annotated.Attribute.create
         ~annotation
         ~original_annotation:annotation
         ~uninstantiated_annotation
         ~abstract:false
         ~async_property:false
         ~class_variable
         ~defined
         ~initialized
         ~name
         ~parent
         ~property
         ~visibility
         ~undecorated_signature
         ~problem:None)
  in
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "test.Attributes")
    ~attribute_name:"bar"
    ~expected_attribute:
      (create_expected_attribute
         "bar"
         ~uninstantiated_annotation:
           "typing.Callable('test.Attributes.bar')[[Named(self, test.Attributes)], int]"
         "BoundMethod[typing.Callable('test.Attributes.bar')[[Named(self, test.Attributes)], int], \
          test.Attributes]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "test.Attributes")
    ~attribute_name:"baz"
    ~expected_attribute:
      (create_expected_attribute
         "baz"
         ~uninstantiated_annotation:
           "typing.Callable('test.Attributes.baz')[[Named(self, test.Attributes), Named(x, int)], \
            int]"
         "BoundMethod[typing.Callable('test.Attributes.baz')[[Named(self, test.Attributes), \
          Named(x, int)], int], test.Attributes]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.meta (Type.Primitive "test.Attributes"))
    ~attribute_name:"implicit"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.Metaclass"
         ~uninstantiated_annotation:
           "typing.Callable('test.Metaclass.implicit')[[Named(cls, test.Metaclass)], int]"
         "implicit"
         "BoundMethod[typing.Callable('test.Metaclass.implicit')[[Named(cls, test.Metaclass)], \
          int], typing.Type[test.Attributes]]");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.meta (Type.Primitive "test.Attributes"))
    ~attribute_name:"property"
    ~expected_attribute:
      (create_expected_attribute
         ~initialized:OnlyOnInstance
         ~property:true
         ~visibility:(ReadOnly Unrefinable)
         "property"
         "str");
  assert_attribute
    ~parent:"test.Attributes"
    ~parent_instantiated_type:(Type.Primitive "Nonsense")
    ~attribute_name:"property"
    ~expected_attribute:
      (create_expected_attribute
         ~property:true
         ~initialized:OnlyOnInstance
         ~visibility:(ReadOnly Unrefinable)
         "property"
         "str");
  assert_attribute
    ~parent:"test.DC"
    ~parent_instantiated_type:(Type.Primitive "test.DC")
    ~attribute_name:"x"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.DC"
         ~visibility:ReadWrite
         ~initialized:OnlyOnInstance
         ~uninstantiated_annotation:"int"
         "x"
         "int");
  assert_attribute
    ~parent:"test.DC"
    ~parent_instantiated_type:(Type.Primitive "test.DC")
    ~attribute_name:"inherited"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.Parent"
         ~visibility:ReadWrite
         ~initialized:OnlyOnInstance
         ~uninstantiated_annotation:"int"
         "inherited"
         "int");
  assert_attribute
    ~parent:"test.NT"
    ~parent_instantiated_type:(Type.Primitive "test.NT")
    ~attribute_name:"x"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.NT"
         ~visibility:(ReadOnly (Refinable { overridable = false }))
         ~initialized:OnlyOnInstance
         ~uninstantiated_annotation:"int"
         "x"
         "int");
  assert_attribute
    ~parent:"test.Prot"
    ~parent_instantiated_type:(Type.Primitive "test.Prot")
    ~attribute_name:"method"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.Prot"
         ~visibility:ReadWrite
         ~uninstantiated_annotation:
           "typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], str]"
         "method"
         "BoundMethod[typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], \
          str], test.Prot]");
  (* This is still not great, since the signature of ExplicitProtChild.method is probably actually
     [[ExplicitProtChild, int], str] not [[Prot, int], str] as this would suggest, but until
     typeshed is fixed to explicitly re-list all of the methods inherited from protocol parents *)
  assert_attribute
    ~parent:"test.Prot"
    ~parent_instantiated_type:(Type.Primitive "test.ExplicitProtChild")
    ~attribute_name:"method"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.Prot"
         ~visibility:ReadWrite
         ~callable_name:(Reference.create "test.Prot.method")
         "method"
         ~uninstantiated_annotation:
           "typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], str]"
         "BoundMethod[typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], \
          str], test.ExplicitProtChild]");
  let tself = Type.variable "TSelf" in
  assert_attribute
    ~parent:"BoundMethod"
    ~parent_instantiated_type:
      (Type.parametric
         "BoundMethod"
         [
           Single
             (Type.Callable.create
                ~name:(Reference.create "was_named")
                ~parameters:
                  (Defined
                     [
                       Named { name = "self"; annotation = tself; default = false };
                       Named { name = "x"; annotation = Type.string; default = false };
                     ])
                ~annotation:tself
                ());
           Single Type.integer;
         ])
    ~attribute_name:"__call__"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"typing.Callable"
         ~visibility:ReadWrite
         ~initialized:OnClass
         ~uninstantiated_annotation:"object"
         "__call__"
         "typing.Callable[[Named(x, str)], int]");
  assert_attribute
    ~parent:"test.ChildOfPlaceholderStub"
    ~parent_instantiated_type:(Type.Primitive "test.ChildOfPlaceholderStub")
    ~attribute_name:"__getattr__"
    ~expected_attribute:
      (create_expected_attribute
         ~parent:"test.ChildOfPlaceholderStub"
         ~visibility:ReadWrite
         ~uninstantiated_annotation:"typing.Callable[..., typing.Any]"
         "__getattr__"
         "BoundMethod[typing.Callable[..., typing.Any], test.ChildOfPlaceholderStub]");
  ()


let test_attribute_type context =
  let assert_attribute ?(source = "") ~parent ~name expected =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let parse annotation =
      parse_single_expression ~preprocess:true annotation
      |> GlobalResolution.parse_annotation global_resolution
    in
    let actual =
      GlobalResolution.attribute_from_annotation ~parent:(parse parent) global_resolution ~name
      >>| Annotated.Attribute.annotation
      >>| Annotation.annotation
      >>| Type.show
    in
    let printer = Option.value_map ~default:"None" ~f:Fn.id in
    assert_equal ~cmp:(Option.equal String.equal) ~printer expected actual
  in
  assert_attribute
    ~source:{|
      class X:
        def __init__(self, x: int) -> None:
          pass
    |}
    ~parent:"typing.Type[test.X]"
    ~name:"__call__"
    (Some
       "BoundMethod[typing.Callable(test.X.__init__)[[Named(self, test.X), Named(x, int)], \
        test.X], test.X]");
  assert_attribute
    ~parent:"typing.Type[typing.Tuple[int, str, bool]]"
    ~name:"__call__"
    (Some
       "BoundMethod[typing.Callable(tuple.__init__)[[Named(self, \
        tuple[Variable[_T_co](covariant)]), Named(a, typing.List[Variable[_T_co](covariant)])], \
        typing.Tuple[Variable[_T_co](covariant), ...]], typing.Tuple[Variable[_T_co](covariant), \
        ...]]");
  ()


let test_invalid_type_parameters context =
  let open AttributeResolution in
  let assert_invalid_type_parameters_direct
      ?(source = "")
      ~given_type
      ~expected_transformed_type
      expected_mismatches
    =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let actual_mismatches, actual_transformed_type =
      GlobalResolution.check_invalid_type_parameters global_resolution given_type
    in
    assert_equal
      ~cmp:[%equal: Type.t]
      ~printer:[%show: Type.t]
      expected_transformed_type
      actual_transformed_type;
    assert_equal
      ~cmp:[%compare.equal: type_parameters_mismatch list]
      ~printer:[%show: type_parameters_mismatch list]
      expected_mismatches
      actual_mismatches
  in
  let assert_invalid_type_parameters
      ?source
      ?(aliases = Type.empty_aliases)
      ~given_type
      ~expected_transformed_type
      expected_mismatches
    =
    let parse annotation =
      parse_single_expression ~preprocess:true annotation
      (* Avoid `GlobalResolution.parse_annotation` because that calls
         `check_invalid_type_parameters`. *)
      |> Type.create ~aliases
    in
    assert_invalid_type_parameters_direct
      ?source
      ~given_type:(parse given_type)
      ~expected_transformed_type:(parse expected_transformed_type)
      expected_mismatches
  in
  assert_invalid_type_parameters
    ~given_type:"typing.List[str, int]"
    ~expected_transformed_type:"typing.List[typing.Any]"
    [
      {
        name = "list";
        kind =
          IncorrectNumberOfParameters
            { actual = 2; expected = 1; can_accept_more_parameters = false };
      };
    ];
  assert_invalid_type_parameters
    ~given_type:"typing.List"
    ~expected_transformed_type:"typing.List[typing.Any]"
    [
      {
        name = "list";
        kind =
          IncorrectNumberOfParameters
            { actual = 0; expected = 1; can_accept_more_parameters = false };
      };
    ];
  assert_invalid_type_parameters
    ~given_type:"typing.Callable[[int, str], bool]"
    ~expected_transformed_type:"typing.Callable[[int, str], bool]"
    [];
  assert_invalid_type_parameters
    ~given_type:"typing.Callable"
    ~expected_transformed_type:"typing.Callable[..., typing.Any]"
    [
      {
        name = "typing.Callable";
        kind =
          IncorrectNumberOfParameters
            { actual = 0; expected = 2; can_accept_more_parameters = false };
      };
    ];
  assert_invalid_type_parameters
    ~given_type:"typing.Tuple[int, ...]"
    ~expected_transformed_type:"typing.Tuple[int, ...]"
    [];
  assert_invalid_type_parameters
    ~given_type:"typing.Tuple[int, str]"
    ~expected_transformed_type:"typing.Tuple[int, str]"
    [];
  assert_invalid_type_parameters
    ~given_type:"tuple"
    ~expected_transformed_type:"typing.Tuple[typing.Any, ...]"
    [
      {
        name = "tuple";
        kind =
          IncorrectNumberOfParameters
            { actual = 0; expected = 1; can_accept_more_parameters = true };
      };
    ];
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_invalid_type_parameters
    ~aliases:
      (fun ?replace_unbound_parameters_with_any:_ -> function
        | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
        | _ -> None)
    ~given_type:"typing.List[pyre_extensions.Unpack[Ts]]"
    ~expected_transformed_type:"typing.List[typing.Any]"
    [
      {
        name = "list";
        kind =
          UnexpectedKind
            {
              actual = Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
              expected = Unary (Type.Variable.Unary.create "_T");
            };
      };
    ];
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "test.TParams" in
  assert_invalid_type_parameters_direct
    ~source:
      {|
      from typing import Generic
      from pyre_extensions import ParameterSpecification

      TParams = ParameterSpecification("TParams")
      class Foo(Generic[TParams]): ...
    |}
    ~given_type:
      (Type.parametric
         "test.Foo"
         [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)])
    ~expected_transformed_type:(Type.parametric "test.Foo" [CallableParameters Undefined])
    [
      {
        name = "test.Foo";
        kind =
          UnexpectedKind
            {
              actual = Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
              expected = Type.Variable.ParameterVariadic parameter_variadic;
            };
      };
    ];
  assert_invalid_type_parameters
    ~source:
      {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
    ~given_type:"test.Foo[int, str]"
    ~expected_transformed_type:"test.Foo[int, str]"
    [];
  assert_invalid_type_parameters
    ~aliases:
      (fun ?replace_unbound_parameters_with_any:_ -> function
        | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
        | _ -> None)
    ~source:
      {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
    ~given_type:"test.Foo[pyre_extensions.Unpack[Ts]]"
    ~expected_transformed_type:"test.Foo[pyre_extensions.Unpack[Ts]]"
    [];
  assert_invalid_type_parameters_direct
    ~source:
      {|
      from typing import Generic
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
    ~given_type:
      (Type.parametric
         "test.Foo"
         [
           CallableParameters
             (ParameterVariadicTypeVariable
                { Type.Callable.head = []; variable = parameter_variadic });
         ])
    ~expected_transformed_type:
      (Type.parametric
         "test.Foo"
         [Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.Any)])
    [
      {
        name = "test.Foo";
        kind =
          UnexpectedKind
            {
              actual =
                Single
                  (Type.parametric
                     Type.Variable.Variadic.Tuple.synthetic_class_name_for_error
                     [
                       CallableParameters
                         (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
                     ]);
              expected = Type.Variable.TupleVariadic (Type.Variable.Variadic.Tuple.create "test.Ts");
            };
      };
    ];
  assert_invalid_type_parameters
    ~aliases:
      (fun ?replace_unbound_parameters_with_any:_ -> function
        | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
        | _ -> None)
    ~source:
      {|
      from typing import Generic, TypeVar
      from pyre_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      T = TypeVar("T")
      class Foo(Generic[T, Unpack[Ts]]): ...
    |}
    ~given_type:"test.Foo[pyre_extensions.Unpack[Ts]]"
    ~expected_transformed_type:
      "test.Foo[typing.Any, pyre_extensions.Unpack[typing.Tuple[typing.Any, ...]]]"
    [
      {
        name = "test.Foo";
        kind =
          IncorrectNumberOfParameters
            { actual = 1; expected = 1; can_accept_more_parameters = true };
      };
    ];
  ()


let test_meet context =
  let assert_meet ?(source = "") ~left ~right expected =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let parse annotation =
      parse_single_expression ~preprocess:true annotation
      |> GlobalResolution.parse_annotation global_resolution
    in
    let actual = GlobalResolution.meet global_resolution (parse left) (parse right) in
    assert_equal ~cmp:Type.equal ~printer:Type.show expected actual
  in
  assert_meet
    ~source:{|
      class C:
        def __init__(self, x: int) -> None:
          pass
    |}
    ~left:"typing.Type[test.C]"
    ~right:"typing.Callable[[int], test.C]"
    Type.Bottom;
  ()


let test_join context =
  let assert_join ?(source = "") ~left ~right expected =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let parse annotation =
      parse_single_expression ~preprocess:true annotation
      |> GlobalResolution.parse_annotation global_resolution
    in
    let actual = GlobalResolution.join global_resolution (parse left) (parse right) in
    assert_equal ~cmp:Type.equal ~printer:Type.show (parse expected) actual
  in
  assert_join
    ~source:
      {|
      class B:
        pass
      class X(B):
        pass
      class Y(B):
        pass
    |}
    ~left:"test.X"
    ~right:"test.B"
    "test.B";
  assert_join
    ~source:{|
      class C:
        def __init__(self, x: int) -> None:
          pass
    |}
    ~left:"typing.Type[test.C]"
    ~right:"typing.Callable[[int], test.C]"
    "typing.Callable[[int], test.C]";
  ()


let test_typed_dictionary_attributes context =
  let assert_attributes sources ~class_name ~expected_attributes =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let attributes =
      GlobalResolution.attributes
        ~resolution
        ~accessed_through_class:true
        ~transitive:true
        ~include_generated_attributes:true
        class_name
    in
    assert_equal
      ~printer:[%show: (string * string) list option]
      expected_attributes
      (Option.map
         ~f:
           (List.map ~f:(fun attribute ->
                Annotated.Attribute.name attribute, Annotated.Attribute.parent attribute))
         attributes)
  in
  assert_attributes
    ["foo.py", "class Foo:\n  x: int\n"]
    ~class_name:"foo.Foo"
    ~expected_attributes:
      (Some
         [
           "x", "foo.Foo";
           "__class__", "object";
           "__delattr__", "object";
           "__dir__", "object";
           "__doc__", "object";
           "__eq__", "object";
           "__format__", "object";
           "__getattribute__", "object";
           "__hash__", "object";
           "__init__", "object";
           "__init_subclass__", "object";
           "__module__", "object";
           "__ne__", "object";
           "__new__", "object";
           "__reduce__", "object";
           "__repr__", "object";
           "__setattr__", "object";
           "__sizeof__", "object";
           "__str__", "object";
           "__call__", "type";
           "__name__", "type";
         ]);
  assert_attributes
    ["test.py", "class Movie(TypedDictionary):\n  name: str\n  year: int"]
    ~class_name:"test.Movie"
    ~expected_attributes:
      (* The fields `name` and `year` are not present. *)
      (Some
         [
           "__init__", "test.Movie";
           "__getitem__", "test.Movie";
           "__setitem__", "test.Movie";
           "get", "test.Movie";
           "setdefault", "test.Movie";
           "update", "test.Movie";
           "__iter__", "TypedDictionary";
           "__len__", "TypedDictionary";
           "copy", "TypedDictionary";
           "__contains__", "typing.Mapping";
           "items", "typing.Mapping";
           "keys", "typing.Mapping";
           "values", "typing.Mapping";
           "__class__", "object";
           "__delattr__", "object";
           "__dir__", "object";
           "__doc__", "object";
           "__eq__", "object";
           "__format__", "object";
           "__getattribute__", "object";
           "__hash__", "object";
           "__init_subclass__", "object";
           "__module__", "object";
           "__ne__", "object";
           "__new__", "object";
           "__reduce__", "object";
           "__repr__", "object";
           "__setattr__", "object";
           "__sizeof__", "object";
           "__str__", "object";
           "__call__", "type";
           "__name__", "type";
         ]);
  ()


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
      ~printer:ConstraintsSet.Solution.show
      ~cmp:ConstraintsSet.Solution.equal
      (ConstraintsSet.Solution.create expected)
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
      if String.equal metaclass "type" then
        metaclass
      else
        "test." ^ metaclass
    in
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    assert_equal (Some (Type.Primitive metaclass)) (GlobalResolution.metaclass ~resolution target)
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
  let create_simple_callable_attribute ?(initialized = Attribute.OnClass) ~signature ~parent name =
    let annotation = Type.Callable signature in
    Annotated.Attribute.create
      ~abstract:false
      ~annotation
      ~original_annotation:annotation
      ~uninstantiated_annotation:(Some annotation)
      ~async_property:false
      ~class_variable:false
      ~defined:true
      ~initialized
      ~name
      ~parent
      ~visibility:ReadWrite
      ~property:false
      ~undecorated_signature:(Some signature)
      ~problem:None
  in
  let create_callable ~name ~parameters ~annotation ~overloads =
    {
      Type.Callable.kind = Named (Reference.create name);
      implementation = { parameters = Defined parameters; annotation };
      overloads;
    }
  in
  let resolution =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
            class Foo:
              def foo() -> int: pass
            class Bar(Foo):
              pass
            class Baz(Bar):
              def foo() -> int: pass
              def baz() -> int: pass
          |}
        );
        ( "overloads.py",
          {|
            from typing import Union
            class Foo:
              @overload
              def foo() -> int: pass
              @overload
              def foo() -> str: pass
              def foo() -> Union[int, str]: pass
            class Bar(Foo):
              def foo() -> int: pass
          |}
        );
      ]
    |> ScratchProject.build_global_resolution
  in
  let assert_overrides ~class_name ~method_name ~expected_override =
    let overrides = GlobalResolution.overrides ~resolution ~name:method_name class_name in
    let print_attribute attribute =
      Annotated.Attribute.sexp_of_instantiated attribute |> Sexp.to_string_hum
    in
    match expected_override with
    | Some expected ->
        let actual = Option.value_exn overrides in
        assert_equal
          ~cmp:Attribute.equal_instantiated
          ~printer:print_attribute
          ~pp_diff:
            (diff ~print:(fun format attribute ->
                 Format.fprintf format "%s" ([%show: Attribute.instantiated] attribute)))
          actual
          expected
    | None -> assert_is_none overrides
  in
  assert_overrides ~class_name:"test.Baz" ~method_name:"baz" ~expected_override:None;
  assert_overrides
    ~class_name:"test.Baz"
    ~method_name:"foo"
    ~expected_override:
      (create_simple_callable_attribute
         ~initialized:OnClass
         ~signature:
           (create_callable
              ~name:"test.Foo.foo"
              ~parameters:[]
              ~annotation:Type.integer
              ~overloads:[])
         ~parent:"test.Foo"
         "foo"
      |> Option.some);

  (* Test overloads. *)
  assert_overrides
    ~class_name:"overloads.Bar"
    ~method_name:"foo"
    ~expected_override:
      (create_simple_callable_attribute
         ~initialized:OnClass
         ~signature:
           (create_callable
              ~name:"overloads.Foo.foo"
              ~parameters:[]
              ~annotation:(Type.union [Type.integer; Type.string])
              ~overloads:
                [
                  { parameters = Defined []; annotation = Type.integer };
                  { parameters = Defined []; annotation = Type.string };
                ])
         ~parent:"overloads.Foo"
         "foo"
      |> Option.some);
  ()


let test_extract_type_parameter context =
  let resolution =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
         from typing import TypeVar, Generic, List, Protocol
         T = TypeVar('T')
         U = TypeVar('U')
         class Derp: ...
         class Foo(Generic[T]): ...
         class Bar(Foo[T]): ...
         class Baz(Foo[T], Generic[T, U]): ...

         class MyProtocol(Protocol[T]):
           def derp(self) -> T: ...
         class MyIntProtocol:
           def derp(self) -> int: ...
         class MyStrProtocol:
           def derp(self) -> str: ...
         class MyGenericProtocol(Generic[T]):
           def derp(self) -> T: ...
         class NotMyProtocol:
           def herp(self) -> int: ...

         ListOfInt = List[int]
       |}
        );
      ]
    |> ScratchProject.build_global_resolution
  in
  let parse_annotation annotation =
    annotation
    (* Preprocess literal TypedDict syntax. *)
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation resolution
  in
  let assert_extracted ~expected ~as_name annotation =
    let actual =
      GlobalResolution.extract_type_parameters resolution ~source:annotation ~target:as_name
    in
    assert_equal
      ~cmp:[%equal: Type.t list option]
      ~printer:(function
        | Some annotations -> List.to_string ~f:Type.show annotations
        | None -> "EXTRACTION FAILED")
      expected
      actual
  in
  let list_name =
    (* Change me in case the canonical name for list type changes *)
    "list"
  in

  assert_extracted Type.Any ~as_name:"test.Derp" ~expected:None;
  assert_extracted Type.Top ~as_name:"test.Derp" ~expected:None;
  assert_extracted Type.Bottom ~as_name:"test.Derp" ~expected:None;
  assert_extracted (Type.list Type.integer) ~as_name:"test.Derp" ~expected:None;
  assert_extracted (parse_annotation "test.Derp") ~as_name:"test.Derp" ~expected:None;

  assert_extracted
    (parse_annotation "test.Foo[int]")
    ~as_name:"test.Foo"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.Bar[str]")
    ~as_name:"test.Foo"
    ~expected:(Some [Type.string]);
  assert_extracted
    (parse_annotation "test.Baz[int, str]")
    ~as_name:"test.Foo"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.Baz[str, int]")
    ~as_name:"test.Foo"
    ~expected:(Some [Type.string]);
  assert_extracted
    (parse_annotation "test.Baz[int, str]")
    ~as_name:"test.Baz"
    ~expected:(Some [Type.integer; Type.string]);

  assert_extracted Type.integer ~as_name:list_name ~expected:None;
  assert_extracted (parse_annotation "test.Foo[int]") ~as_name:list_name ~expected:None;
  assert_extracted
    (parse_annotation "typing.List[int]")
    ~as_name:list_name
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.ListOfInt")
    ~as_name:list_name
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.ListOfInt")
    ~as_name:"typing.Sequence"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.ListOfInt")
    ~as_name:"typing.Iterable"
    ~expected:(Some [Type.integer]);

  assert_extracted
    (parse_annotation "test.MyIntProtocol")
    ~as_name:"test.MyProtocol"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "test.MyStrProtocol")
    ~as_name:"test.MyProtocol"
    ~expected:(Some [Type.string]);
  assert_extracted
    (parse_annotation "test.MyGenericProtocol[float]")
    ~as_name:"test.MyProtocol"
    ~expected:(Some [Type.float]);
  assert_extracted (parse_annotation "test.NotMyProtocol") ~as_name:"test.MyProtocol" ~expected:None;

  assert_extracted
    (parse_annotation "typing.Dict[int, str]")
    ~as_name:"typing.Iterable"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "typing.Dict[int, str]")
    ~as_name:"typing.Mapping"
    ~expected:(Some [Type.integer; Type.string]);
  assert_extracted
    (parse_annotation "typing.Mapping[int, str]")
    ~as_name:"typing.Iterable"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "typing.Generator[int, str, float]")
    ~as_name:"typing.Iterator"
    ~expected:(Some [Type.integer]);
  assert_extracted
    (parse_annotation "typing.Coroutine[typing.Any, typing.Any, typing.Any]")
    ~as_name:"typing.Awaitable"
    ~expected:(Some [Type.Any]);
  assert_extracted
    (parse_annotation "typing.Coroutine[int, str, float]")
    ~as_name:"typing.Awaitable"
    ~expected:(Some [Type.float]);

  assert_extracted (Type.list Type.Any) ~as_name:list_name ~expected:(Some [Type.Any]);
  assert_extracted
    (Type.list Type.object_primitive)
    ~as_name:list_name
    ~expected:(Some [Type.object_primitive]);
  (* TODO (T63159626): Should be [Top] *)
  assert_extracted (Type.list Type.Top) ~as_name:list_name ~expected:None;
  (* TODO (T63159626): Should be [Bottom] *)
  assert_extracted (Type.list Type.Bottom) ~as_name:list_name ~expected:None;
  ()


let test_type_of_iteration_value context =
  let global_resolution =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
         from typing import Iterable, Iterator, Generic, TypeVar
         T = TypeVar('T')

         class IntIterable(Iterable[int]): ...

         class GenericIterator(Iterator[T], Generic[T]): ...
       |}
        );
      ]
    |> ScratchProject.build_global_resolution
  in
  let parse_annotation annotation =
    annotation
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation global_resolution
  in
  let assert_type_of_iteration_value ~annotation ~expected =
    let type_ = parse_annotation annotation in
    let actual = GlobalResolution.type_of_iteration_value ~global_resolution type_ in
    assert_equal
      ~cmp:[%equal: Type.t option]
      ~printer:(function
        | Some type_ -> Type.show type_
        | None -> "EXTRACTION FAILED")
      expected
      actual
  in
  assert_type_of_iteration_value ~annotation:"typing.Iterable[int]" ~expected:(Some Type.integer);
  assert_type_of_iteration_value ~annotation:"typing.Iterator[str]" ~expected:(Some Type.string);
  assert_type_of_iteration_value
    ~annotation:"typing.Generator[int, str, None]"
    ~expected:(Some Type.integer);
  assert_type_of_iteration_value ~annotation:"test.IntIterable" ~expected:(Some Type.integer);
  assert_type_of_iteration_value
    ~annotation:"test.GenericIterator[str]"
    ~expected:(Some Type.string);
  ()


let test_type_of_generator_send_and_return context =
  let global_resolution =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let parse_annotation annotation =
    annotation
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation global_resolution
  in
  let assert_type_of_generator_send_and_return ~annotation ~expected_send ~expected_return =
    let type_ = parse_annotation annotation in
    let actual_send, actual_return =
      GlobalResolution.type_of_generator_send_and_return ~global_resolution type_
    in
    assert_equal ~cmp:[%equal: Type.t] ~printer:Type.show expected_send actual_send;
    assert_equal ~cmp:[%equal: Type.t] ~printer:Type.show expected_return actual_return;
    ()
  in
  assert_type_of_generator_send_and_return
    ~annotation:"typing.Iterator[int]"
    ~expected_send:Type.none
    ~expected_return:Type.none;
  assert_type_of_generator_send_and_return
    ~annotation:"typing.Iterable[int]"
    ~expected_send:Type.none
    ~expected_return:Type.none;
  assert_type_of_generator_send_and_return
    ~annotation:"typing.AsyncGenerator[int, str]"
    ~expected_send:Type.string
    ~expected_return:Type.none;
  assert_type_of_generator_send_and_return
    ~annotation:"typing.Generator[int, None, str]"
    ~expected_send:Type.none
    ~expected_return:Type.string;
  assert_type_of_generator_send_and_return
    ~annotation:"typing.Generator[int, str, None]"
    ~expected_send:Type.string
    ~expected_return:Type.none;
  ()


let test_define context =
  let assert_define ?expected_define_source ~define_name ~source =
    let expected_source =
      expected_define_source >>| parse ~handle:"test.py" >>| Preprocessing.preprocess
    in
    let expected_define =
      match expected_source >>| Source.statements >>= List.last with
      | Some { Node.value = Statement.Define define; _ } -> Some define
      | _ -> None
    in
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_resolution
    in
    assert_equal
      ~cmp:(Option.equal (fun left right -> Define.location_insensitive_compare left right = 0))
      ~printer:[%show: Define.t option]
      ~pp_diff:
        (diff ~print:(fun format x -> Format.fprintf format "%s" ([%show: Define.t option] x)))
      expected_define
      (GlobalResolution.define resolution define_name)
  in
  let assert_no_define_found ~define_name ~source =
    assert_define ~define_name ~source ?expected_define_source:None
  in
  let source =
    {|
    from builtins import _test_sink
    from typing import Callable

    def simple_function(x: int) -> str:
      print(x)
      return "hello"

    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner

    def two_inner_functions(callable: Callable[[str], None]) -> Callable[[str], None]:
      def inner1(y: str) -> None:
        _test_sink(y)

      def inner2(y: str) -> None:
        inner1(y)
        callable(y)

      return inner2

    def decorator_factory(x: int) -> Callable[[Callable[[str], None]], Callable[[str], None]]:
      def wrapper(f: Callable[[str], None]) -> Callable[[str], None]:
        def inner(y: str) -> None:
          _test_sink(y)
          f(y)

        return inner

      return wrapper
  |}
  in
  assert_define
    ~define_name:!&"test.simple_function"
    ~source
    ~expected_define_source:
      {|
    def simple_function(x: int) -> str:
      print(x)
      return "hello"
    |};
  assert_no_define_found ~define_name:!&"test.non_existent" ~source;
  (* Functions containing nested functions. *)
  assert_define
    ~define_name:!&"test.with_logging"
    ~source
    ~expected_define_source:
      {|
    from builtins import _test_sink
    from typing import Callable
    def with_logging(callable: Callable[[str], None]) -> Callable[[str], None]:

      def inner(y: str) -> None:
        _test_sink(y)
        callable(y)

      return inner
    |};
  assert_define
    ~define_name:!&"test.two_inner_functions"
    ~source
    ~expected_define_source:
      {|
    from builtins import _test_sink
    from typing import Callable

    def two_inner_functions(callable: Callable[[str], None]) -> Callable[[str], None]:
      def inner1(y: str) -> None:
        _test_sink(y)

      def inner2(y: str) -> None:
        inner1(y)
        callable(y)

      return inner2
    |};
  assert_define
    ~define_name:!&"test.decorator_factory"
    ~source
    ~expected_define_source:
      {|
    from builtins import _test_sink
    from typing import Callable

    def decorator_factory(x: int) -> Callable[[Callable[[str], None]], Callable[[str], None]]:
      def wrapper(f: Callable[[str], None]) -> Callable[[str], None]:
        def inner(y: str) -> None:
          _test_sink(y)
          f(y)

        return inner

      return wrapper
    |};
  ()


let test_refine context =
  let global_resolution =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  assert_equal
    (GlobalResolution.refine
       ~global_resolution
       (Annotation.create_immutable Type.float)
       Type.integer)
    (Annotation.create_immutable ~original:(Some Type.float) Type.integer);
  assert_equal
    (GlobalResolution.refine
       ~global_resolution
       (Annotation.create_immutable Type.integer)
       Type.float)
    (Annotation.create_immutable Type.integer);
  assert_equal
    (GlobalResolution.refine
       ~global_resolution
       (Annotation.create_immutable Type.integer)
       Type.Bottom)
    (Annotation.create_immutable Type.integer);
  assert_equal
    (GlobalResolution.refine ~global_resolution (Annotation.create_immutable Type.integer) Type.Top)
    (Annotation.create_immutable ~original:(Some Type.integer) Type.Top);
  ()


let () =
  "class"
  >::: [
         "attributes" >:: test_class_attributes;
         "typed_dictionary_attributes" >:: test_typed_dictionary_attributes;
         "constraints" >:: test_constraints;
         "constructors" >:: test_constructors;
         "first_matching_decorator" >:: test_first_matching_decorator;
         "is_protocol" >:: test_is_protocol;
         "metaclasses" >:: test_metaclasses;
         "superclasses" >:: test_superclasses;
         "overrides" >:: test_overrides;
         "extract_type_parameter" >:: test_extract_type_parameter;
         "type_of_iteration_value" >:: test_type_of_iteration_value;
         "type_of_generator_send_and_return" >:: test_type_of_generator_send_and_return;
         "test_attribute_from_annotation" >:: test_attribute_type;
         "test_invalid_type_parameters" >:: test_invalid_type_parameters;
         "test_meet" >:: test_meet;
         "test_join" >:: test_join;
         "define" >:: test_define;
         "refine" >:: test_refine;
       ]
  |> Test.run
