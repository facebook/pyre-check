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
module Attribute = AnnotatedAttribute
module Argument = Call.Argument

let ( !! ) concretes = List.map concretes ~f:(fun single -> Type.Argument.Single single)

let test_superclasses =
  let assert_successors target expected context =
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
      class SubRedundant(SubFooBar, Foo): pass
    |}
          );
        ]
      |> ScratchProject.build_global_resolution
    in
    let actual = GlobalResolution.successors resolution target in
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      ~cmp:(List.equal Type.Primitive.equal)
      expected
      actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_successors "test.Foo" ["object"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_successors "test.SubRedundant" ["test.SubFooBar"; "test.Foo"; "test.Bar"; "object"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_successors "test.SubFoo" ["test.Foo"; "object"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_successors "test.SubFooBar" ["test.Foo"; "test.Bar"; "object"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_successors "test.SubRecurse" ["test.SubFooBar"; "test.Foo"; "test.Bar"; "object"];
    ]


let test_first_matching_decorator =
  let assert_first_matching_decorator source name expected context =
    let resolution =
      ScratchProject.setup ~context ["__init__.py", source]
      |> ScratchProject.build_global_resolution
    in
    let assert_logic expected =
      match parse_last_statement source with
      | { Node.value = Statement.Class definition; _ } ->
          let actual =
            let class_summary = ClassSummary.create ~qualifier:Reference.empty definition in
            GlobalResolution.first_matching_class_decorator
              resolution
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
      | _ -> assert_true (Option.is_none expected)
    in
    assert_logic expected
  in
  let make_decorator ~name ~arguments =
    {
      Decorator.name = + !&name;
      arguments;
      original_expression =
        Decorator.create_original_expression
          ~create_origin_for_reference:(fun _ -> None)
          ~call_origin:None
          ~name:(+ !&name)
          ~arguments;
    }
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator "class A: pass" "decorator" None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator
      class A:
        pass
    |}
           "decorator"
           (Some (make_decorator ~name:"decorator" ~arguments:None));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator.a.b
      class A:
        pass
    |}
           "decorator.a"
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator
      class A:
        pass
    |}
           "decorator.a"
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator.a.b
      class A:
        pass
    |}
           "decorator.a.b"
           (Some (make_decorator ~name:"decorator.a.b" ~arguments:None));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator(a=b, c=d)
      class A:
        pass
    |}
           "decorator.a.b"
           None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @other.decorator
      @decorator(a=b, c=d)
      class A:
        pass
    |}
           "decorator"
           (Some
              (make_decorator
                 ~name:"decorator"
                 ~arguments:
                   (Some
                      [
                        {
                          Argument.name = Some ~+"a";
                          value = +Expression.Name (Name.Identifier "b");
                        };
                        {
                          Argument.name = Some ~+"c";
                          value = +Expression.Name (Name.Identifier "d");
                        };
                      ])));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           {|
      @decorator(a=b)
      @decorator(a=b, c=d)
      class A:
        pass
    |}
           "decorator"
           (Some
              (make_decorator
                 ~name:"decorator"
                 ~arguments:
                   (Some
                      [
                        {
                          Argument.name = Some ~+"a";
                          value = +Expression.Name (Name.Identifier "b");
                        };
                      ])));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_first_matching_decorator
           (* `enum` imports `ABCMeta` from `abc`. *)
           {|
      import enum
      @enum.ABCMeta
      class A:
        pass
    |}
           "abc.ABCMeta"
           (Some (make_decorator ~name:"abc.ABCMeta" ~arguments:None));
    ]


let test_constructors =
  let assert_constructor source instantiated constructors context =
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
        ~parent:(Type.class_type instantiated)
        resolution
        ~name:"__call__"
      |> (fun option -> Option.value_exn option)
      |> AnnotatedAttribute.annotation
      |> TypeInfo.Unit.annotation
    in
    assert_equal ~printer:Type.show ~cmp:Type.equal callable actual
  in
  test_list
    [
      (* Undefined constructors. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           "class Foo: pass"
           "Foo"
           (Some
              "BoundMethod[typing.Callable('object.__init__')[[Named(self, object)], test.Foo], \
               test.Foo]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           "class Foo: ..."
           "Foo"
           (Some
              "BoundMethod[typing.Callable('object.__init__')[[Named(self, object)], test.Foo], \
               test.Foo]");
      (* Statement.Defined constructors. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      class Foo:
        def __init__(self, a: int) -> None: pass
    |}
           "Foo"
           (Some
              "BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, test.Foo), Named(a, \
               int)], test.Foo], test.Foo]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      class Foo:
        def __init__(self, a: int) -> None: pass
        @typing.overload
        def __init__(self, b: str) -> None: pass
    |}
           "Foo"
           (Some
              ("BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, test.Foo), Named(a, \
                int)], test.Foo]"
              ^ "[[[Named(self, test.Foo), Named(b, str)], test.Foo]], test.Foo]"));
      (* Generic classes. *)
      (* TODO T197463208: add the following test after creating a function like assert_constructor
         but which uses declaration parsing intead of parse annotation. Right now as expected, those
         type vars will be parsed to nothing: Ex test.Foo[]. So this test is not meaningful.

         labeled_test_case __FUNCTION__ __LINE__ @@ assert_constructor {| _K = typing.TypeVar('_K')
         _V = typing.TypeVar('_V') class Foo(typing.Generic[_K, _V]): def __init__(self) -> None:
         pass |} "Foo" (Some "BoundMethod[typing.Callable('test.Foo.__init__')[[Named(self, \
         test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')])], \
         test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')]], \
         test.Foo[typing.TypeVar('test._K'),typing.TypeVar('test._V')]]"); *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      class Parent:
        def __init__(self, x: int) -> None:
          pass
      class C(Parent):
        pass
    |}
           "C"
           (Some
              "BoundMethod[typing.Callable('test.Parent.__init__')[[Named(self, test.Parent), \
               Named(x, int)], test.C], test.C]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      class Parent:
        def __new__(self, x: str) -> None:
          pass
      class C(Parent):
        pass
    |}
           "C"
           (Some
              "BoundMethod[typing.Callable(test.Parent.__new__)[[Named(self, \
               typing.Type[test.Parent]), Named(x, str)], test.C], typing.Type[test.C]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      T = typing.TypeVar('T', bound=C)
      class C:
        def __init__(self, x: int) -> None: pass
    |}
           "T"
           (Some
              "BoundMethod[typing.Callable('test.C.__init__')[[Named(self, test.C), Named(x, \
               int)], test.T], test.T]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      from dataclasses import dataclass
      @dataclass(frozen=True)
      class A:
          foo:int = 1
    |}
           "A"
           (Some
              "BoundMethod[typing.Callable('test.A.__init__')[[Named(self, test.A), Named(foo, \
               int, default)], test.A], test.A]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
    |}
           "Base"
           (Some
              "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
               typing.Type[test.Base[test.T]]), Named(x, test.T)], \
               test.Base[typing.Optional[test.T]]], typing.Type[test.Base]]");
      (* With @final. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
               typing.Type[test.Base[test.T]]), Named(x, test.T)], \
               test.Base[typing.Optional[test.T]]], typing.Type[test.Base]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      from typing import final, Generic, Optional, TypeVar
      T = TypeVar("T")
      @final
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
    |}
           "Base[int]"
           (Some
              "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
               typing.Type[test.Base[int]]), Named(x, int)], test.Base[typing.Optional[int]]], \
               typing.Type[test.Base[int]]]");
      (* When `__new__` is inherited, don't use the return type from the parent method, since that
         would be confusing. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
              "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
               typing.Type[test.Base[int]]), Named(x, int)], test.Child[int, test.S]], \
               typing.Type[test.Child[int, test.S]]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
               typing.Type[test.Base[test.T]]), Named(x, typing.Union[typing.List[test.T], \
               test.T])], test.Base[test.T]][[[Named(cls, typing.Type[test.Base[test.T]]), \
               Named(x, typing.List[test.T])], test.Base[test.T]][[Named(cls, \
               typing.Type[test.Base[test.T]]), Named(x, test.T)], \
               test.Base[typing.Optional[test.T]]]], typing.Type[test.Base]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
               typing.Type[test.Base[test.T]]), Named(x, typing.Union[typing.List[test.T], \
               test.T])], test.Base[test.T]][[[Named(cls, typing.Type[test.Base[test.T]]), \
               Named(x, typing.List[test.T])], test.Base[test.T]][[Named(cls, \
               typing.Type[test.Base[test.T]]), Named(x, test.T)], \
               test.Base[typing.Optional[test.T]]]], typing.Type[test.Base]]");
      (* __new__ is marked as returning `Optional[Base]`. While this is technically allowed at
         runtime, be conservative and replace the return type with the class type. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      from typing import Optional

      class Base:
        def __new__(self, x: str) -> Optional[Base]:
          pass
    |}
           "Base"
           (Some
              "BoundMethod[typing.Callable(test.Base.__new__)[[Named(self, \
               typing.Type[test.Base]), Named(x, str)], test.Base], typing.Type[test.Base]]");
      (* __init__ takes precedence over __new__ and ignores any return type for __new__. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
           {|
      from typing import Generic, Optional, TypeVar
      T = TypeVar("T")
      class Base(Generic[T]):
        def __new__(self, x: T) -> Base[Optional[T]]: ...
        def __init__(self, x: T) -> None: ...
    |}
           "Base"
           (Some
              "BoundMethod[typing.Callable(test.Base.__init__)[[Named(self, test.Base[test.T]), \
               Named(x, test.T)], test.Base[test.T]], test.Base[test.T]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constructor
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
              "BoundMethod[typing.Callable(test.Base.__init__)[[Named(self, test.Base[test.T]), \
               Named(x, typing.Union[typing.List[test.T], test.T])], test.Base[test.T]], \
               test.Base[test.T]]");
    ]


let test_is_protocol =
  let assert_is_protocol base_arguments expected _ =
    let is_protocol base_arguments =
      {
        StatementClass.name = !&"Derp";
        base_arguments;
        parent = NestingContext.create_toplevel ();
        body = [];
        decorators = [];
        top_level_unbound_names = [];
        type_params = [];
      }
      |> ClassSummary.create ~qualifier:Reference.empty
      |> ClassSummary.is_protocol
    in
    assert_equal expected (is_protocol base_arguments)
  in
  let parse = parse_single_expression in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_is_protocol [] false;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_is_protocol [{ Argument.name = None; value = parse "derp" }] false;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_is_protocol [{ Argument.name = None; value = parse "typing.Protocol" }] true;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_is_protocol
           [{ Argument.name = None; value = parse "typing_extensions.Protocol" }]
           true;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_is_protocol
           [{ Argument.name = Some ~+"metaclass"; value = parse "abc.ABCMeta" }]
           false;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_is_protocol [{ Argument.name = None; value = parse "typing.Protocol[T]" }] true;
    ]


let test_all_attributes =
  let setup source context =
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
    AnnotatedAttribute.create_instantiated
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
  in
  (* Test `Class.attributes`. *)
  let assert_attributes definition attributes context =
    let attribute_list_equal = List.equal Attribute.equal_instantiated in
    let print_attributes attributes =
      let print_attribute attribute =
        AnnotatedAttribute.sexp_of_instantiated attribute |> Sexp.to_string_hum
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
      (GlobalResolution.uninstantiated_attributes (resolution context) definition
      |> (fun a -> Option.value_exn a)
      |> List.map
           ~f:
             (GlobalResolution.instantiate_attribute
                (resolution context)
                ~accessed_through_class:false
                ~accessed_through_readonly:false))
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
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attributes
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
    ]


let test_attribute_from_class_name =
  let assert_attribute
      ?(accessed_through_class = false)
      ?(accessed_through_readonly = false)
      ~parent
      ~type_for_lookup
      ~attribute_name
      expected_attribute
      context
    =
    let setup source =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let resolution =
      setup
        {|
          from dataclasses import dataclass

          class SimpleClass:
            some_attribute: str = "foo"

            def some_method(self, x: int) -> str: ...

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

        |}
    in
    let actual_attribute =
      GlobalResolution.attribute_from_class_name
        resolution
        parent
        ~transitive:true
        ~accessed_through_class
        ~accessed_through_readonly
        ~name:attribute_name
        ~type_for_lookup
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
      ?(initialized = AnnotatedAttribute.OnClass)
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
      (AnnotatedAttribute.create_instantiated
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
         ~undecorated_signature)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Attributes"
           ~type_for_lookup:(Type.Primitive "test.Attributes")
           ~attribute_name:"bar"
           (create_expected_attribute
              "bar"
              ~uninstantiated_annotation:
                "typing.Callable('test.Attributes.bar')[[Named(self, test.Attributes)], int]"
              "BoundMethod[typing.Callable('test.Attributes.bar')[[Named(self, test.Attributes)], \
               int], test.Attributes]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Attributes"
           ~type_for_lookup:(Type.Primitive "test.Attributes")
           ~attribute_name:"baz"
           (create_expected_attribute
              "baz"
              ~uninstantiated_annotation:
                "typing.Callable('test.Attributes.baz')[[Named(self, test.Attributes), Named(x, \
                 int)], int]"
              "BoundMethod[typing.Callable('test.Attributes.baz')[[Named(self, test.Attributes), \
               Named(x, int)], int], test.Attributes]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Attributes"
           ~type_for_lookup:(Type.Primitive "test.Attributes")
           ~attribute_name:"implicit"
           ~accessed_through_class:true
           (create_expected_attribute
              ~parent:"test.Metaclass"
              ~uninstantiated_annotation:
                "typing.Callable('test.Metaclass.implicit')[[Named(cls, test.Metaclass)], int]"
              "implicit"
              "BoundMethod[typing.Callable('test.Metaclass.implicit')[[Named(cls, \
               test.Metaclass)], int], typing.Type[test.Attributes]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Attributes"
           ~type_for_lookup:(Type.Primitive "test.Attributes")
           ~attribute_name:"property"
           ~accessed_through_class:true
           (create_expected_attribute
              ~initialized:OnlyOnInstance
              ~property:true
              ~visibility:(ReadOnly Unrefinable)
              "property"
              "str");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Attributes"
           ~type_for_lookup:(Type.Primitive "Nonsense")
           ~attribute_name:"property"
           (create_expected_attribute
              ~property:true
              ~initialized:OnlyOnInstance
              ~visibility:(ReadOnly Unrefinable)
              "property"
              "str");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.DC"
           ~type_for_lookup:(Type.Primitive "test.DC")
           ~attribute_name:"x"
           (create_expected_attribute
              ~parent:"test.DC"
              ~visibility:ReadWrite
              ~initialized:OnlyOnInstance
              ~uninstantiated_annotation:"int"
              "x"
              "int");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.DC"
           ~type_for_lookup:(Type.Primitive "test.DC")
           ~attribute_name:"inherited"
           (create_expected_attribute
              ~parent:"test.Parent"
              ~visibility:ReadWrite
              ~initialized:OnlyOnInstance
              ~uninstantiated_annotation:"int"
              "inherited"
              "int");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.NT"
           ~type_for_lookup:(Type.Primitive "test.NT")
           ~attribute_name:"x"
           (create_expected_attribute
              ~parent:"test.NT"
              ~visibility:(ReadOnly (Refinable { overridable = false }))
              ~initialized:OnlyOnInstance
              ~uninstantiated_annotation:"int"
              "x"
              "int");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Prot"
           ~type_for_lookup:(Type.Primitive "test.Prot")
           ~attribute_name:"method"
           (create_expected_attribute
              ~parent:"test.Prot"
              ~visibility:ReadWrite
              ~uninstantiated_annotation:
                "typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], str]"
              "method"
              "BoundMethod[typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, \
               int)], str], test.Prot]");
      (* This is still not great, since the signature of ExplicitProtChild.method is probably
         actually [[ExplicitProtChild, int], str] not [[Prot, int], str] as this would suggest, but
         until typeshed is fixed to explicitly re-list all of the methods inherited from protocol
         parents *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.Prot"
           ~type_for_lookup:(Type.Primitive "test.ExplicitProtChild")
           ~attribute_name:"method"
           (create_expected_attribute
              ~parent:"test.Prot"
              ~visibility:ReadWrite
              ~callable_name:(Reference.create "test.Prot.method")
              "method"
              ~uninstantiated_annotation:
                "typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, int)], str]"
              "BoundMethod[typing.Callable(test.Prot.method)[[Named(self, test.Prot), Named(x, \
               int)], str], test.ExplicitProtChild]");
      (let tself = Type.variable "TSelf" in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_attribute
            ~parent:"BoundMethod"
            ~type_for_lookup:
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
            (create_expected_attribute
               ~parent:"typing.Callable"
               ~visibility:ReadWrite
               ~initialized:OnClass
               ~uninstantiated_annotation:"object"
               "__call__"
               "typing.Callable[[Named(x, str)], int]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.SimpleClass"
           ~type_for_lookup:(Type.Primitive "test.SimpleClass")
           ~attribute_name:"some_attribute"
           ~accessed_through_readonly:true
           (create_expected_attribute
              ~parent:"test.SimpleClass"
              ~uninstantiated_annotation:"str"
              "some_attribute"
              "pyre_extensions.ReadOnly[str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.SimpleClass"
           ~type_for_lookup:(Type.Primitive "test.SimpleClass")
           ~attribute_name:"some_method"
           ~accessed_through_readonly:true
           (create_expected_attribute
              "some_method"
              ~parent:"test.SimpleClass"
              ~uninstantiated_annotation:
                "typing.Callable('test.SimpleClass.some_method')[[Named(self, test.SimpleClass), \
                 Named(x, int)], str]"
              "BoundMethod[typing.Callable('test.SimpleClass.some_method')[[Named(self, \
               test.SimpleClass), Named(x, int)], str], \
               pyre_extensions.ReadOnly[test.SimpleClass]]");
      (* A method looked up on the class will have its original Callable type regardless of whether
         it was accessed through readonly. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"test.SimpleClass"
           ~type_for_lookup:(Type.Primitive "test.SimpleClass")
           ~attribute_name:"some_method"
           ~accessed_through_class:true
           ~accessed_through_readonly:true
           (create_expected_attribute
              "some_method"
              ~parent:"test.SimpleClass"
              ~uninstantiated_annotation:
                "typing.Callable('test.SimpleClass.some_method')[[Named(self, test.SimpleClass), \
                 Named(x, int)], str]"
              "pyre_extensions.ReadOnly[typing.Callable('test.SimpleClass.some_method')[[Named(self, \
               test.SimpleClass), Named(x, int)], str]]");
    ]


let test_attribute_from_annotation =
  let assert_attribute ?(source = "") ~parent ~name expected context =
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
      >>| AnnotatedAttribute.annotation
      >>| TypeInfo.Unit.annotation
      >>| Type.show
    in
    let printer = Option.value_map ~default:"None" ~f:Fn.id in
    assert_equal ~cmp:(Option.equal String.equal) ~printer expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~source:
             {|
      class X:
        def __init__(self, x: int) -> None:
          pass
    |}
           ~parent:"typing.Type[test.X]"
           ~name:"__call__"
           (Some
              "BoundMethod[typing.Callable(test.X.__init__)[[Named(self, test.X), Named(x, int)], \
               test.X], test.X]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~parent:"typing.Type[typing.Tuple[int, str, bool]]"
           ~name:"__call__"
           (Some
              "BoundMethod[typing.Callable(tuple.__init__)[[Named(self, tuple[Variable[_T_co]]), \
               Named(a, typing.List[Variable[_T_co]])], typing.Tuple[Variable[_T_co], ...]], \
               typing.Tuple[Variable[_T_co], ...]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attribute
           ~source:{|
      class Foo:
        x: str
    |}
           ~parent:"pyre_extensions.ReadOnly[test.Foo]"
           ~name:"x"
           (Some "pyre_extensions.PyreReadOnly[str]");
    ]


let test_invalid_type_parameters =
  let open AttributeResolution in
  let assert_invalid_type_parameters_direct
      ?(source = "")
      ~given_type
      ~expected_transformed_type
      expected_mismatches
      context
    =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let actual_mismatches, actual_transformed_type =
      GlobalResolution.validate_and_sanitize_type_arguments global_resolution given_type
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
      ~given_type
      ~expected_transformed_type
      expected_mismatches
    =
    let parse annotation =
      let variable_aliases name =
        match name with
        | "Ts" -> Some (Type.Variable.TypeVarTupleVariable (Type.Variable.TypeVarTuple.create name))
        | _ -> None
      in
      parse_single_expression ~preprocess:true annotation
      (* Avoid `GlobalResolution.parse_annotation` because that calls
         `validate_and_sanitize_type_arguments`. *)
      |> Type.create ~variables:variable_aliases ~aliases:Type.resolved_empty_aliases
    in
    assert_invalid_type_parameters_direct
      ?source
      ~given_type:(parse given_type)
      ~expected_transformed_type:(parse expected_transformed_type)
      expected_mismatches
  in
  let variadic_variable = Type.Variable.TypeVarTuple.create "Ts" in
  let parameter_variadic = Type.Variable.ParamSpec.create "test.TParams" in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~given_type:"typing.Callable[[int, str], bool]"
           ~expected_transformed_type:"typing.Callable[[int, str], bool]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~given_type:"typing.Tuple[int, ...]"
           ~expected_transformed_type:"typing.Tuple[int, ...]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~given_type:"typing.Tuple[int, str]"
           ~expected_transformed_type:"typing.Tuple[int, str]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~given_type:"typing.List[typing.Unpack[Ts]]"
           ~expected_transformed_type:"typing.List[typing.Any]"
           [
             {
               name = "list";
               kind =
                 UnexpectedKind
                   {
                     actual =
                       Unpacked
                         (Type.OrderedTypes.Concatenation.create_unpackable variadic_variable);
                     expected = TypeVarVariable (Type.Variable.TypeVar.create "_T");
                   };
             };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters_direct
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
                [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic_variable)])
           ~expected_transformed_type:(Type.parametric "test.Foo" [CallableParameters Undefined])
           [
             {
               name = "test.Foo";
               kind =
                 UnexpectedKind
                   {
                     actual =
                       Unpacked
                         (Type.OrderedTypes.Concatenation.create_unpackable variadic_variable);
                     expected = Type.Variable.ParamSpecVariable parameter_variadic;
                   };
             };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~source:
             {|
      from typing import Generic, TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
           ~given_type:"test.Foo[int, str]"
           ~expected_transformed_type:"test.Foo[int, str]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~source:
             {|
      from typing import Generic, TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
           ~given_type:"test.Foo[typing.Unpack[Ts]]"
           ~expected_transformed_type:"test.Foo[typing.Unpack[Ts]]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~source:
             {|
      from typing import Generic
      from typing_extensions import TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
           ~given_type:"test.Foo[typing_extensions.Unpack[Ts]]"
           ~expected_transformed_type:"test.Foo[typing_extensions.Unpack[Ts]]"
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters_direct
           ~source:
             {|
      from typing import Generic, TypeVarTuple, Unpack

      Ts = TypeVarTuple("Ts")
      class Foo(Generic[Unpack[Ts]]): ...
    |}
           ~given_type:
             (Type.parametric
                "test.Foo"
                [
                  CallableParameters
                    (FromParamSpec { Type.Callable.head = []; variable = parameter_variadic });
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
                            Type.Variable.TypeVarTuple.synthetic_class_name_for_error
                            [
                              CallableParameters
                                (Type.Variable.ParamSpec.self_reference parameter_variadic);
                            ]);
                     expected =
                       Type.Variable.TypeVarTupleVariable
                         (Type.Variable.TypeVarTuple.create "test.Ts");
                   };
             };
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_type_parameters
           ~source:
             {|
      from typing import Generic, TypeVar, Unpack, TypeVarTuple

      Ts = TypeVarTuple("Ts")
      T = TypeVar("T")
      class Foo(Generic[T, Unpack[Ts]]): ...
    |}
           ~given_type:"test.Foo[typing.Unpack[Ts]]"
           ~expected_transformed_type:
             "test.Foo[typing.Any, typing.Unpack[typing.Tuple[typing.Any, ...]]]"
           [
             {
               name = "test.Foo";
               kind =
                 IncorrectNumberOfParameters
                   { actual = 1; expected = 1; can_accept_more_parameters = true };
             };
           ];
    ]


let test_meet =
  let assert_meet ?(source = "") ~left ~right expected context =
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
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           ~source:
             {|
      class C:
        def __init__(self, x: int) -> None:
          pass
    |}
           ~left:"typing.Type[test.C]"
           ~right:"typing.Callable[[int], test.C]"
           Type.Bottom;
    ]


let test_join =
  let assert_join ?(source = "") ~left ~right expected context =
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
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~source:
             {|
      class C:
        def __init__(self, x: int) -> None:
          pass
    |}
           ~left:"typing.Type[test.C]"
           ~right:"typing.Callable[[int], test.C]"
           "typing.Callable[[int], test.C]";
    ]


let test_typed_dictionary_attributes =
  let assert_attributes sources ~class_name ~expected_attributes context =
    let project = ScratchProject.setup ~context sources in
    let resolution = ScratchProject.build_resolution project in
    let resolution = Resolution.global_resolution resolution in
    let attributes =
      GlobalResolution.uninstantiated_attributes
        resolution
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
                AnnotatedAttribute.name attribute, AnnotatedAttribute.parent attribute))
         attributes)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attributes
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_attributes
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
    ]


let test_constraints =
  let assert_constraints ~source_type_name ~current_type source expected context =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = GlobalResolution.create global_environment in
    let constraints =
      GlobalResolution.Testing.constraints_for_instantiate
        ~source_type_name
        resolution
        ~current_type
        ()
    in
    let expected =
      List.map expected ~f:(fun (variable, value) -> Type.Variable.TypeVarPair (variable, value))
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
  let t_bound =
    Type.Variable.TypeVar.create
      ~constraints:(Type.Record.TypeVarConstraints.Bound (Type.Primitive "test.Bound"))
      "test.T_Bound"
  in
  let t_explicit =
    Type.Variable.TypeVar.create
      ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.integer; Type.string])
      "test.T_Explicit"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![int_and_foo_string_union])
           {|
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_V]):
        pass
    |}
           [Type.Variable.TypeVar.create "test._V", int_and_foo_string_union];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.Primitive "test.Foo")
           {|
      class Foo:
        pass
    |}
           [];
      (* Consequence of the special case we need to remove *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.Bottom])
           {|
      _T = typing.TypeVar('_T')
      class Foo(typing.Generic[_T]):
        pass
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer; Type.float])
           {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
           [
             Type.Variable.TypeVar.create "test._K", Type.integer;
             Type.Variable.TypeVar.create "test._V", Type.float;
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer; Type.float])
           {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Foo(typing.Generic[_K, _V]):
        pass
    |}
           [
             Type.Variable.TypeVar.create "test._K", Type.integer;
             Type.Variable.TypeVar.create "test._V", Type.float;
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.Primitive "test.Foo")
           {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Bar"
           ~current_type:(Type.Primitive "test.Foo")
           {|
      _T = typing.TypeVar('_T')
      class Bar(typing.Generic[_T]):
        pass
      class Foo(Bar[int]):
        pass
    |}
           [Type.Variable.TypeVar.create "test._T", Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Bar"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer])
           {|
      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class Bar(typing.Generic[_V]):
        pass
      class Foo(typing.Generic[_K], Bar[_K]):
        pass
    |}
           [Type.Variable.TypeVar.create "test._V", Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Bar"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer; Type.float])
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
           [Type.Variable.TypeVar.create "test._T", Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Baz"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer; Type.float])
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
           [Type.Variable.TypeVar.create "test._T", Type.float];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Iterator"
           ~current_type:(Type.parametric "test.Iterator" !![Type.integer])
           {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
    |}
           [Type.Variable.TypeVar.create "test._T", Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Iterator"
           ~current_type:(Type.parametric "test.Iterable" !![Type.integer])
           {|
      _T = typing.TypeVar('_T')
      class Iterator(typing.Protocol[_T]):
        pass
      class Iterable(Iterator[_T]):
        pass
    |}
           [Type.Variable.TypeVar.create "test._T", Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.Primitive "test.Bound"])
           {|
      class Bound:
        pass
      T_Bound = typing.TypeVar('T_Bound', bound=Bound)
      class Foo(typing.Generic[T_Bound]):
        pass
    |}
           [t_bound, Type.Primitive "test.Bound"];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.Primitive "test.UnderBound"])
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.Primitive "test.OverBound"])
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.integer])
           {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
           [t_explicit, Type.integer];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_constraints
           ~source_type_name:"test.Foo"
           ~current_type:(Type.parametric "test.Foo" !![Type.bool])
           {|
      T_Explicit = typing.TypeVar('T_Explicit', int, str)
      class Foo(typing.Generic[T_Explicit]):
        pass
    |}
           [];
    ]


let test_metaclasses =
  let assert_metaclass ~source ~target metaclass context =
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
    assert_equal (Some (Type.Primitive metaclass)) (GlobalResolution.metaclass resolution target)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass ~source:{|
       class C:
         pass
    |} ~target:"C" "type";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
           ~source:
             {|
      class Meta:
        pass
      class C(metaclass=Meta):
        pass
    |}
           ~target:"C"
           "Meta";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_metaclass
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
           "MoreMeta";
    ]


let test_overrides =
  let create_simple_callable_attribute ?(initialized = Attribute.OnClass) ~signature ~parent name =
    let annotation = Type.Callable signature in
    AnnotatedAttribute.create_instantiated
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
  in
  let create_callable ~name ~parameters ~annotation ~overloads =
    {
      Type.Callable.kind = Named (Reference.create name);
      implementation = { parameters = Defined parameters; annotation };
      overloads;
    }
  in
  let resolution context =
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
  let assert_overrides ~class_name ~method_name ~expected_override context =
    let overrides = GlobalResolution.overrides (resolution context) ~name:method_name class_name in
    let print_attribute attribute =
      AnnotatedAttribute.sexp_of_instantiated attribute |> Sexp.to_string_hum
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
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_overrides ~class_name:"test.Baz" ~method_name:"baz" ~expected_override:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_overrides
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_overrides
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
    ]


let test_extract_unary_type_arguments =
  let resolution context =
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
  let parse_annotation annotation context =
    annotation
    (* Preprocess literal TypedDict syntax. *)
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation (resolution context)
  in
  let assert_extracted ~expected ~as_name parse_annotation context =
    let actual =
      GlobalResolution.extract_unary_type_arguments__unsafe
        (resolution context)
        ~source:(parse_annotation context)
        ~target:as_name
    in
    assert_equal
      ~cmp:[%equal: Type.t list option]
      ~printer:(function
        | Some annotations -> List.to_string ~f:Type.show annotations
        | None -> "EXTRACTION FAILED")
      expected
      actual
  in
  let assert_extracted_type ~expected ~as_name type_ =
    assert_extracted ~expected ~as_name (fun _ -> type_)
  in
  let list_name =
    (* Change me in case the canonical name for list type changes *)
    "list"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type Type.Any ~as_name:"test.Derp" ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type Type.Top ~as_name:"test.Derp" ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type Type.Bottom ~as_name:"test.Derp" ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type (Type.list Type.integer) ~as_name:"test.Derp" ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted (parse_annotation "test.Derp") ~as_name:"test.Derp" ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.Foo[int]")
           ~as_name:"test.Foo"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.Bar[str]")
           ~as_name:"test.Foo"
           ~expected:(Some [Type.string]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.Baz[int, str]")
           ~as_name:"test.Foo"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.Baz[str, int]")
           ~as_name:"test.Foo"
           ~expected:(Some [Type.string]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.Baz[int, str]")
           ~as_name:"test.Baz"
           ~expected:(Some [Type.integer; Type.string]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type Type.integer ~as_name:list_name ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted (parse_annotation "test.Foo[int]") ~as_name:list_name ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.List[int]")
           ~as_name:list_name
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.ListOfInt")
           ~as_name:list_name
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.ListOfInt")
           ~as_name:"typing.Sequence"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.ListOfInt")
           ~as_name:"typing.Iterable"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.MyIntProtocol")
           ~as_name:"test.MyProtocol"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.MyStrProtocol")
           ~as_name:"test.MyProtocol"
           ~expected:(Some [Type.string]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.MyGenericProtocol[float]")
           ~as_name:"test.MyProtocol"
           ~expected:(Some [Type.float]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "test.NotMyProtocol")
           ~as_name:"test.MyProtocol"
           ~expected:None;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Dict[int, str]")
           ~as_name:"typing.Iterable"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Dict[int, str]")
           ~as_name:"typing.Mapping"
           ~expected:(Some [Type.integer; Type.string]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Mapping[int, str]")
           ~as_name:"typing.Iterable"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Generator[int, str, float]")
           ~as_name:"typing.Iterator"
           ~expected:(Some [Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Coroutine[typing.Any, typing.Any, typing.Any]")
           ~as_name:"typing.Awaitable"
           ~expected:(Some [Type.Any]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted
           (parse_annotation "typing.Coroutine[int, str, float]")
           ~as_name:"typing.Awaitable"
           ~expected:(Some [Type.float]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type (Type.list Type.Any) ~as_name:list_name ~expected:(Some [Type.Any]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type
           (Type.list Type.object_primitive)
           ~as_name:list_name
           ~expected:(Some [Type.object_primitive]);
      (* TODO (T63159626): Should be [Top] *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type (Type.list Type.Top) ~as_name:list_name ~expected:None;
      (* TODO (T63159626): Should be [Bottom] *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_extracted_type (Type.list Type.Bottom) ~as_name:list_name ~expected:None;
    ]


let test_type_of_iteration_value =
  let global_resolution context =
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
  let parse_annotation annotation context =
    annotation
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation (global_resolution context)
  in
  let assert_type_of_iteration_value ~annotation ~expected context =
    let type_ = parse_annotation annotation context in
    let actual = GlobalResolution.type_of_iteration_value (global_resolution context) type_ in
    assert_equal
      ~cmp:[%equal: Type.t option]
      ~printer:(function
        | Some type_ -> Type.show type_
        | None -> "EXTRACTION FAILED")
      expected
      actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_iteration_value
           ~annotation:"typing.Iterable[int]"
           ~expected:(Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_iteration_value
           ~annotation:"typing.Iterator[str]"
           ~expected:(Some Type.string);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_iteration_value
           ~annotation:"typing.Generator[int, str, None]"
           ~expected:(Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_iteration_value ~annotation:"test.IntIterable" ~expected:(Some Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_iteration_value
           ~annotation:"test.GenericIterator[str]"
           ~expected:(Some Type.string);
    ]


let test_type_of_generator_send_and_return =
  let global_resolution context =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let parse_annotation annotation context =
    annotation
    |> parse_single_expression ~preprocess:true
    |> GlobalResolution.parse_annotation (global_resolution context)
  in
  let assert_type_of_generator_send_and_return ~annotation ~expected_send ~expected_return context =
    let type_ = parse_annotation annotation context in
    let actual_send, actual_return =
      GlobalResolution.type_of_generator_send_and_return (global_resolution context) type_
    in
    assert_equal ~cmp:[%equal: Type.t] ~printer:Type.show expected_send actual_send;
    assert_equal ~cmp:[%equal: Type.t] ~printer:Type.show expected_return actual_return;
    ()
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_generator_send_and_return
           ~annotation:"typing.Iterator[int]"
           ~expected_send:Type.none
           ~expected_return:Type.none;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_generator_send_and_return
           ~annotation:"typing.Iterable[int]"
           ~expected_send:Type.none
           ~expected_return:Type.none;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_generator_send_and_return
           ~annotation:"typing.AsyncGenerator[int, str]"
           ~expected_send:Type.string
           ~expected_return:Type.none;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_generator_send_and_return
           ~annotation:"typing.Generator[int, None, str]"
           ~expected_send:Type.none
           ~expected_return:Type.string;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_of_generator_send_and_return
           ~annotation:"typing.Generator[int, str, None]"
           ~expected_send:Type.string
           ~expected_return:Type.none;
    ]


let test_refine =
  let global_resolution context =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let assert_refine_equal annotation refined_type expected_typeinfo context =
    assert_equal
      (GlobalResolution.refine (global_resolution context) annotation refined_type)
      expected_typeinfo
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_refine_equal
           (TypeInfo.Unit.create_immutable Type.float)
           Type.integer
           (TypeInfo.Unit.create_immutable ~original:(Some Type.float) Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_refine_equal
           (TypeInfo.Unit.create_immutable Type.integer)
           Type.float
           (TypeInfo.Unit.create_immutable Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_refine_equal
           (TypeInfo.Unit.create_immutable Type.integer)
           Type.Bottom
           (TypeInfo.Unit.create_immutable Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_refine_equal
           (TypeInfo.Unit.create_immutable Type.integer)
           Type.Top
           (TypeInfo.Unit.create_immutable ~original:(Some Type.integer) Type.Top);
    ]


let () =
  Test.sanitized_module_name __MODULE__
  >::: [
         test_all_attributes;
         test_attribute_from_class_name;
         test_attribute_from_annotation;
         test_typed_dictionary_attributes;
         test_constraints;
         test_constructors;
         test_first_matching_decorator;
         test_is_protocol;
         test_metaclasses;
         test_superclasses;
         test_overrides;
         test_extract_unary_type_arguments;
         test_type_of_iteration_value;
         test_type_of_generator_send_and_return;
         test_invalid_type_parameters;
         test_meet;
         test_join;
         test_refine;
       ]
  |> Test.run
