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
open Pyre
open Test

let value option = Option.value_exn option

let class_hierarchy environment =
  GlobalResolution.create environment |> GlobalResolution.class_hierarchy


let find_unsafe getter value = getter value |> fun optional -> Option.value_exn optional

let create_environments_and_project
    ~context
    ?(include_typeshed_stubs = true)
    ?(include_helpers = false)
    ?(additional_sources = [])
    ?(in_memory = true)
    ()
  =
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs
      ~include_helper_builtins:include_helpers
      ~in_memory
      additional_sources
  in
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    project |> ScratchProject.build_global_environment
  in
  (* TODO (T47159596): This can be done in a more elegant way *)
  let () =
    let set_up_shared_memory _ = () in
    let tear_down_shared_memory () _ = Memory.reset_shared_memory () in
    OUnit2.bracket set_up_shared_memory tear_down_shared_memory context
  in
  global_environment, project


let create_environment ~context ?include_typeshed_stubs ?include_helpers ?additional_sources () =
  create_environments_and_project
    ~context
    ?include_typeshed_stubs
    ?include_helpers
    ?additional_sources
    ()
  |> fst


let populate ?include_typeshed_stubs ?include_helpers sources =
  create_environment ?include_typeshed_stubs ?include_helpers ~additional_sources:sources ()


let order_and_environment ~context source =
  let environment = populate ~context source in
  AnnotatedGlobalEnvironment.ReadOnly.attribute_resolution environment
  |> AttributeResolution.ReadOnly.full_order
  |> fun order -> order, environment


let class_summary environment =
  GlobalResolution.create environment |> GlobalResolution.class_summary


let parse_annotation environment =
  (* Allow untracked because we're not calling all of populate *)
  GlobalResolution.create environment |> GlobalResolution.parse_annotation ~validation:NoValidation


let test_register_aliases context =
  let register_all sources = populate ~context sources in
  let assert_resolved sources aliases =
    let environment = register_all sources in
    let assert_alias (alias, target) =
      let parse_annotation handler source =
        parse_single_expression source |> parse_annotation handler
      in
      assert_equal
        ~printer:(fun string -> string)
        target
        (Type.show (parse_annotation environment alias))
    in
    List.iter aliases ~f:assert_alias
  in
  (* Explicit Aliases *)
  assert_resolved
    [
      ( "test.py",
        {|
          class C: ...
          class D(C): pass
          B: typing_extensions.TypeAlias = D
          A: typing_extensions.TypeAlias = B
          Twiddledee: typing_extensions.TypeAlias
          Twiddledum: typing_extensions.TypeAlias
          Twiddledee, Twiddledum = C, C
        |}
      );
    ]
    [
      "test.C", "test.C";
      "test.D", "test.D";
      "test.B", "test.D";
      "test.A", "test.D";
      "test.Twiddledee", "test.Twiddledee";
      "test.Twiddledum", "test.Twiddledum";
    ];

  assert_resolved
    [
      ( "qualifier.py",
        {|
          class C: ...
          class D(C): pass
          B: typing_extensions.TypeAlias = D
          A: typing_extensions.TypeAlias = B
        |}
      );
    ]
    [
      "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D";
    ];

  (* Non-explicit Alaises *)
  assert_resolved
    [
      ( "test.py",
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
          Twiddledee, Twiddledum = C, C
        |}
      );
    ]
    [
      "test.C", "test.C";
      "test.D", "test.D";
      "test.B", "test.D";
      "test.A", "test.D";
      "test.Twiddledee", "test.Twiddledee";
      "test.Twiddledum", "test.Twiddledum";
    ];

  assert_resolved
    [
      ( "qualifier.py",
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
        |}
      );
    ]
    [
      "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D";
    ];
  assert_resolved ["test.py", "X = None"] [];

  (* Imports *)
  assert_resolved
    [
      ( "collectionz.py",
        {|
          from typing import Iterator as TypingIterator
          from typing import Iterable
        |}
      );
    ]
    [
      "collectionz.TypingIterator", "typing.Iterator[typing.Any]";
      "collectionz.Iterable", "typing.Iterable[typing.Any]";
    ];

  (* Handle builtins correctly. *)
  assert_resolved
    [
      ( "collectionz.py",
        {|
          from builtins import int
          from builtins import dict as CDict
        |}
      );
    ]
    ["collectionz.int", "int"; "collectionz.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [
      ( "collectionz.py",
        {|
          from future.builtins import int
          from future.builtins import dict as CDict
        |}
      );
    ]
    ["collectionz.int", "int"; "collectionz.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [
      ( "asyncio/tasks.py",
        {|
           from typing import TypeVar, Generic, Union
           _T = typing.TypeVar('_T')
           class Future(Generic[_T]): ...
           class Awaitable(Generic[_T]): ...
           _FutureT = Union[Future[_T], Awaitable[_T]]
        |}
      );
    ]
    [
      "asyncio.tasks.Future[int]", "asyncio.tasks.Future[int]";
      ( "asyncio.tasks._FutureT[int]",
        "typing.Union[asyncio.tasks.Awaitable[int], asyncio.tasks.Future[int]]" );
    ];
  assert_resolved
    [
      ( "a.py",
        {|
          import typing
          _T = typing.TypeVar("_T")
          _T2 = typing.TypeVar("UnrelatedName")
        |}
      );
    ]
    ["a._T", "Variable[a._T]"; "a._T2", "Variable[a._T2]"];

  (* Type variable aliases in classes are not supported. *)
  assert_resolved
    [
      ( "qualifier.py",
        {|
          class Class:
            T = typing.TypeVar('T')
            Int = int
        |}
      );
    ]
    ["qualifier.Class.T", "qualifier.Class.T"; "qualifier.Class.Int", "qualifier.Class.Int"];

  (* Stub-suppressed aliases show up as `Any`. *)
  assert_resolved
    [
      "stubbed.pyi", "# pyre-placeholder-stub";
      ( "qualifier.py",
        {|
          class str: ...
          T = stubbed.Something
          Q = typing.Union[stubbed.Something, str]
        |}
      );
    ]
    ["qualifier.T", "typing.Any"; "qualifier.Q", "typing.Any"];
  assert_resolved
    [
      ( "t.py",
        {|
          import x
          X = typing.Dict[int, int]
          T = typing.Dict[int, X]
          C = typing.Callable[[T], int]
        |}
      );
      ( "x.py",
        {|
          import t
          X = typing.Dict[int, int]
          T = typing.Dict[int, t.X]
          C = typing.Callable[[T], int]
        |}
      );
    ]
    [
      "x.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
      "t.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
    ];
  assert_resolved
    [
      "t.py", {|
          from typing import Dict
        |};
      "x.py", {|
          from t import *
        |};
    ]
    ["x.Dict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    ["x.py", {|
          C = typing.Callable[[gurbage], gurbage]
        |}]
    ["x.C", "x.C"];
  assert_resolved
    [
      ( "test.py",
        {|
          A = int
          B: typing.Type[int] = int
          C: typing.Type[int]
          D: typing.Any
          E = typing.Any
          F = ...
          G = A
          H = 1
        |}
      );
    ]
    [
      "test.A", "int";
      "test.B", "test.B";
      "test.C", "test.C";
      "test.D", "test.D";
      "test.E", "typing.Any";
      "test.F", "test.F";
      "test.G", "int";
      "test.H", "test.H";
    ];
  assert_resolved
    ["a.py", {|
          class Foo: ...
        |}; "b.py", {|
          import a
        |}]
    ["b.a.Foo", "a.Foo"];
  assert_resolved
    [
      "a.py", {|
          class Foo: ...
        |};
      "b.py", {|
          from a import Foo
          class Foo:
            ...
        |};
    ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [
      "a.py", {|
          class Bar: ...
        |};
      "b.py", {|
          from a import Bar as Foo
          class Foo:
            ...
        |};
    ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [
      "a.py", {|
          class Foo: ...
        |};
      "b.py", {|
          from a import Foo as Bar
          class Foo: ...
        |};
    ]
    ["b.Foo", "b.Foo"; "b.Bar", "a.Foo"];

  let assert_resolved sources aliases =
    let environment = register_all sources in
    let global_resolution = GlobalResolution.create environment in
    let assert_alias (alias, target) =
      match GlobalResolution.aliases global_resolution alias with
      | Some alias -> assert_equal ~printer:Type.show_alias target alias
      | None -> failwith "Alias is missing"
    in
    List.iter aliases ~f:assert_alias
  in
  assert_resolved
    ["test.py", {|
          Tparams = pyre_extensions.ParameterSpecification('Tparams')
      |}]
    [
      ( "test.Tparams",
        Type.VariableAlias
          (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "test.Tparams"))
      );
    ];
  ()


let test_register_implicit_namespace_modules context =
  let environment = create_environment ~context ~additional_sources:["a/b/c.py", ""] () in
  let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
  let global_resolution = GlobalResolution.create environment in
  assert_bool
    "Can get the source of a/b/c.py"
    (AstEnvironment.ReadOnly.get_raw_source ast_environment (Reference.create "a.b.c")
    |> Option.is_some);
  assert_bool
    "Can get the module definition of a/b/c.py"
    (GlobalResolution.module_exists global_resolution (Reference.create "a.b.c"));
  let is_module = GlobalResolution.module_exists global_resolution in
  (* We do consider a.b a valid namespace module, because it has python files as direct children *)
  assert_true (is_module (Reference.create "a.b"));
  (* We do not consider a a valid namespace module, because it has no python files as direct
     children *)
  assert_false (is_module (Reference.create "a"))


let test_register_globals context =
  let assert_global_with_environment ~environment reference expected =
    let resolution = GlobalResolution.create environment in
    let actual =
      !&reference
      |> GlobalResolution.global resolution
      >>| fun { annotation; _ } -> Annotation.annotation annotation
    in
    assert_equal
      ~printer:(function
        | Some annotation -> Type.show annotation
        | _ -> "(none)")
      ~cmp:(Option.equal Type.equal)
      expected
      actual
  in
  let assert_global =
    let environment =
      populate
        ~context
        [
          ( "qualifier.py",
            {|
              with_join = 1 or 'asdf'
              with_resolve = with_join
              annotated: int = 1
              unannotated = 'string'
              stub: int = ...
              class Class: ...
              if True:
                in_branch: int = 1
              else:
                in_branch: int = 2

              identifier = Class()
              identifier.access: int = 1
              identifier().attribute: int = 1

              class Foo:
                attribute: int = 1
            |}
          );
        ]
    in
    assert_global_with_environment ~environment
  in
  assert_global "qualifier.undefined" None;
  assert_global "qualifier.with_join" (Some (Type.union [Type.integer; Type.string]));
  assert_global "qualifier.with_resolve" (Some Type.Any);
  assert_global "qualifier.annotated" (Some Type.integer);
  assert_global "qualifier.unannotated" (Some Type.string);
  assert_global "qualifier.stub" (Some Type.integer);
  assert_global "qualifier.Class" (Some (Type.meta (Type.Primitive "qualifier.Class")));
  assert_global "qualifier.in_branch" (Some Type.integer);
  assert_global "qualifier.identifier.access" None;
  assert_global "qualifier.identifier().access" None;
  assert_global "Foo.attribute" None;
  let assert_global =
    let environment =
      populate
        ~context
        [
          ( "test.py",
            {|
              class Class: ...
              alias = Class

              GLOBAL: Class = ...
              GLOBAL2: alias = ...
            |}
          );
        ]
    in
    assert_global_with_environment ~environment
  in
  assert_global "test.GLOBAL" (Some (Type.Primitive "test.Class"));
  assert_global "test.GLOBAL2" (Some (Type.Primitive "test.Class"));
  let assert_global =
    let environment =
      populate
        ~context
        [
          ( "tuples.py",
            {|
              def f():
                return 7, 8
              y, z = f()
            |}
          );
        ]
    in
    assert_global_with_environment ~environment
  in
  assert_global "tuples.y" (Some Type.Top);
  assert_global "tuples.z" (Some Type.Top);
  ()


let test_connect_type_order context =
  let project =
    ScratchProject.setup
      ~context
      ~include_helper_builtins:false
      [
        ( "test.py",
          {|
       class C:
         ...
       class D(C):
         pass
       class CallMe:
         def __call__(self, x: int) -> str:
           ...
       B = D
       A = B
       def foo() -> A:
         return D()
        |}
        );
      ]
  in
  let global_environment = ScratchProject.global_environment project in
  let order = class_hierarchy global_environment in
  let assert_successors annotation successors =
    assert_equal
      ~printer:(List.to_string ~f:Type.Primitive.show)
      successors
      (ClassHierarchy.successors order annotation)
  in
  (* Classes get connected to object via ClassHierarchyEnvironment.update. *)
  assert_successors "test.C" ["object"];
  assert_successors "test.D" ["test.C"; "object"];
  assert_successors "test.CallMe" ["object"]


let test_populate context =
  (* Test type resolution. *)
  let environment =
    populate ~context ["__init__.py", {|
      class foo.foo(): ...
      class bar(): ...
    |}]
  in
  assert_equal (parse_annotation environment !"foo.foo") (Type.Primitive "foo.foo");
  assert_equal
    (parse_annotation environment (parse_single_expression "Optional[foo.foo]"))
    (Type.parametric "Optional" [Single (Type.Primitive "foo.foo")]);
  assert_equal (parse_annotation environment !"bar") (Type.Primitive "bar");

  (* Check custom aliases. *)
  assert_equal
    (parse_annotation environment !"typing.DefaultDict")
    (Type.Primitive "collections.defaultdict");

  (* Check custom class definitions. *)
  let global_resolution = GlobalResolution.create environment in
  assert_is_some (GlobalResolution.class_summary global_resolution (Primitive "typing.Optional"));

  (* Check type aliases. *)
  let environment =
    populate
      ~context
      ["test.py", {|
      _T = typing.TypeVar('_T')
      S = str
      S2 = S
    |}]
  in
  assert_equal (parse_annotation environment !"test._T") (Type.variable "test._T");
  assert_equal (parse_annotation environment !"test.S") Type.string;
  assert_equal (parse_annotation environment !"test.S2") Type.string;
  let assert_superclasses ?(superclass_parameters = fun _ -> []) ~environment base ~superclasses =
    let (module TypeOrderHandler) = class_hierarchy environment in
    let index annotation = IndexTracker.index annotation in
    let targets = TypeOrderHandler.edges (index base) in
    let to_target annotation =
      {
        ClassHierarchy.Target.target = index annotation;
        parameters = superclass_parameters annotation;
      }
    in
    let show_targets = function
      | None -> ""
      | Some targets ->
          let show_target { ClassHierarchy.Target.target; parameters } =
            let index = IndexTracker.show target in
            let target = IndexTracker.annotation target in
            Format.asprintf "%s: %s%a" index target (Type.pp_parameters ~pp_type:Type.pp) parameters
          in
          List.to_string targets ~f:show_target
    in
    assert_equal
      ~printer:show_targets
      ~cmp:(Option.equal (List.equal [%compare.equal: ClassHierarchy.Target.t]))
      (Some (List.map superclasses ~f:to_target))
      targets
  in
  (* Metaclasses aren't superclasses. *)
  let environment =
    populate
      ~context
      ~include_helpers:false
      ["test.py", {|
        class C(metaclass=abc.ABCMeta): ...
      |}]
  in
  assert_superclasses ~environment "test.C" ~superclasses:["object"];

  (* Ensure object is a superclass if a class only has unsupported bases. *)
  let environment =
    populate
      ~context
      ~include_helpers:false
      [
        ( "test.py",
          {|
        def foo() -> int:
          return 1
        class C(foo()):
          pass
      |}
        );
      ]
  in
  assert_superclasses ~environment "test.C" ~superclasses:["object"];

  (* Globals *)
  let assert_global_with_environment environment actual expected =
    let global_resolution = GlobalResolution.create environment in
    assert_equal
      ~cmp:(Option.equal Annotation.equal)
      ~printer:(function
        | Some global -> Annotation.show global
        | None -> "None")
      expected
      (GlobalResolution.global global_resolution !&actual >>| fun { annotation; _ } -> annotation)
  in
  let assert_global =
    populate
      ~context
      [
        ( "test.py",
          {|
      class int(): pass
      A: int = 0
      B = 0
      C: int = ...

      class Foo(): pass
      alias = Foo
      G: Foo = ...
      H: alias = ...
    |}
        );
      ]
    |> assert_global_with_environment
  in
  let assert_global actual expected = assert_global actual (Some expected) in
  assert_global "test.A" (Annotation.create_immutable (parse_annotation environment !"test.int"));
  assert_global "test.B" (Annotation.create_immutable Type.integer);
  assert_global "test.C" (Annotation.create_immutable (parse_annotation environment !"test.int"));
  assert_global "test.G" (Annotation.create_immutable (parse_annotation environment !"test.Foo"));
  assert_global "test.H" (Annotation.create_immutable (parse_annotation environment !"test.Foo"));
  let assert_global =
    populate
      ~context
      [
        ( "test.py",
          {|
      global_value_set = 1
      global_annotated: int
      global_both: int = 1
      global_unknown = x
      global_function = function
      class Class():
        def __init__(self):
          pass
      def function():
        pass
    |}
        );
      ]
    |> assert_global_with_environment
  in
  let assert_no_global actual = assert_global actual None in
  let assert_global actual expected = assert_global actual (Some expected) in
  assert_global "test.global_value_set" (Annotation.create_immutable Type.integer);
  assert_global "test.global_annotated" (Annotation.create_immutable Type.integer);
  assert_global "test.global_both" (Annotation.create_immutable Type.integer);
  assert_global
    "test.global_unknown"
    (Annotation.create_immutable ~original:(Some Type.Top) Type.Any);
  assert_global
    "test.function"
    (Annotation.create_immutable
       (Type.Callable.create
          ~name:!&"test.function"
          ~parameters:(Type.Callable.Defined [])
          ~annotation:Type.Any
          ()));
  assert_global
    "test.global_function"
    (Annotation.create_immutable
       ~original:(Some Type.Top)
       (Type.Callable.create
          ~name:!&"test.function"
          ~parameters:(Type.Callable.Defined [])
          ~annotation:Type.Any
          ()));
  assert_global "test.Class" (Annotation.create_immutable (Type.meta (Type.Primitive "test.Class")));
  assert_no_global "test.Class.__init__";

  (* Properties. *)
  let assert_global =
    populate
      ~context
      [
        ( "test.py",
          {|
      class Class:
        @property
        def Class.property(self) -> int: ...
    |}
        );
      ]
    |> assert_global_with_environment
  in
  assert_global "test.Class.property" None;

  (* Loops. *)
  (try
     populate ~context ["test.py", {|
        def foo(cls):
          class cls(cls): pass
      |}]
     |> ignore
   with
  | ClassHierarchy.Cyclic _ -> assert_unreached ());

  (* Check meta variables are registered. *)
  let assert_global =
    populate ~context ["test.py", {|
      class A:
        pass
    |}]
    |> assert_global_with_environment
  in
  assert_global
    "test.A"
    (Some (Type.Primitive "test.A" |> Type.meta |> Annotation.create_immutable));

  (* Callable classes. *)
  let environment =
    populate
      ~context
      [
        ( "test.py",
          {|
      class CallMe:
        def __call__(self, x: int) -> str:
          pass
      class AlsoCallable(CallMe):
        pass
  |}
        );
      ]
  in
  assert_superclasses ~environment "test.CallMe" ~superclasses:["object"];
  ();
  ()


let test_less_or_equal_type_order context =
  let order, environment =
    order_and_environment
      ~context
      ["module.py", {|
      class super(): ...
      class sub(super): ...
    |}]
  in
  let super = parse_annotation environment (parse_single_expression "module.super") in
  assert_equal super (Type.Primitive "module.super");
  let sub = parse_annotation environment (parse_single_expression "module.sub") in
  assert_equal sub (Type.Primitive "module.sub");
  assert_true (TypeOrder.always_less_or_equal order ~left:sub ~right:Type.Top);
  assert_true (TypeOrder.always_less_or_equal order ~left:super ~right:Type.Top);
  assert_true (TypeOrder.always_less_or_equal order ~left:sub ~right:super);
  assert_false (TypeOrder.always_less_or_equal order ~left:super ~right:sub);
  let order, environment =
    order_and_environment
      ~context
      [
        ( "module.py",
          {|
        class sub(super): pass
        class super(top): pass
        class top(): pass
    |}
        );
      ]
  in
  let super = parse_annotation environment (parse_single_expression "module.super") in
  assert_equal super (Type.Primitive "module.super");
  let sub = parse_annotation environment (parse_single_expression "module.sub") in
  let super = parse_annotation environment (parse_single_expression "module.super") in
  let top = parse_annotation environment (parse_single_expression "module.top") in
  assert_true (TypeOrder.always_less_or_equal order ~left:sub ~right:super);
  assert_true (TypeOrder.always_less_or_equal order ~left:super ~right:top);

  (* Optionals. *)
  let order, _ =
    order_and_environment
      ~context
      [
        ( "test.py",
          {|
      class A: ...
      class B(A): ...
      class C(typing.Optional[A]): ...
    |}
        );
      ]
  in
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.optional (Type.Primitive "test.A"))
       ~right:(Type.optional (Type.Primitive "test.A")));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.A")
       ~right:(Type.optional (Type.Primitive "test.A")));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.optional (Type.Primitive "test.A"))
       ~right:(Type.Primitive "test.A"));

  (* We're currently not sound with inheritance and optionals. *)
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.optional (Type.Primitive "test.A"))
       ~right:(Type.Primitive "test.C"));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.A")
       ~right:(Type.Primitive "test.C"));

  (* Unions. *)
  let order, _ =
    order_and_environment
      ~context
      [
        ( "test.py",
          {|
      class A: ...
      class B(A): ...
      class int(): ...
      class float(): ...
    |}
        );
      ]
  in
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"])
       ~right:(Type.Union [Type.Primitive "test.A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.B"])
       ~right:(Type.Union [Type.Primitive "test.A"]));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"])
       ~right:(Type.Union [Type.Primitive "test.B"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.A")
       ~right:(Type.Union [Type.Primitive "test.A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.B")
       ~right:(Type.Union [Type.Primitive "test.A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.A")
       ~right:(Type.Union [Type.Primitive "test.A"; Type.Primitive "test.B"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"; Type.Primitive "test.B"; Type.integer])
       ~right:Type.Any);
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"; Type.Primitive "test.B"; Type.integer])
       ~right:(Type.Union [Type.Top; Type.Any; Type.optional Type.float]));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"; Type.Primitive "test.B"; Type.integer])
       ~right:Type.float);
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "test.A"; Type.Primitive "test.B"; Type.integer])
       ~right:(Type.Union [Type.float; Type.Primitive "test.B"; Type.integer]));

  (* Special cases. *)
  assert_true (TypeOrder.always_less_or_equal order ~left:Type.integer ~right:Type.float)


let test_join_type_order context =
  let order, _ =
    order_and_environment
      ~context
      ["test.py", {|
      class foo(): ...
      class bar(L[T]): ...
    |}]
  in
  let foo = Type.Primitive "test.foo" in
  let bar = Type.Primitive "test.bar" in
  assert_equal (TypeOrder.join order Type.Bottom bar) bar;
  assert_equal (TypeOrder.join order Type.Top bar) Type.Top;
  assert_equal (TypeOrder.join order foo bar) (Type.union [foo; bar]);
  assert_equal (TypeOrder.join order Type.Any Type.Top) Type.Top;
  assert_equal
    (TypeOrder.join order Type.integer (Type.Union [Type.integer; Type.string]))
    (Type.Union [Type.integer; Type.string]);
  assert_equal
    (TypeOrder.join order (Type.Union [Type.integer; Type.string]) Type.integer)
    (Type.Union [Type.integer; Type.string]);
  assert_raises (ClassHierarchy.Untracked "test.durp") (fun _ ->
      TypeOrder.join order bar (Type.Primitive "test.durp"));

  (* Special cases. *)
  assert_equal (TypeOrder.join order Type.integer Type.float) Type.float


let test_meet_type_order context =
  let order, _ =
    order_and_environment
      ~context
      [
        ( "test.py",
          {|
      class foo(): ...
      class bar(L[T]): ...
      class A: ...
      class B(A): ...
      class C(A): ...
      class D(B,C): ...
    |}
        );
      ]
  in
  let assert_meet left right expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:(Format.asprintf "%a" Type.pp)
      ~pp_diff:(diff ~print:Type.pp)
      (TypeOrder.meet order left right)
      expected
  in
  let foo = Type.Primitive "test.foo" in
  let bar = Type.Primitive "test.bar" in
  let a = Type.Primitive "test.A" in
  let b = Type.Primitive "test.B" in
  let c = Type.Primitive "test.C" in
  let d = Type.Primitive "test.D" in
  assert_meet Type.Bottom bar Type.Bottom;
  assert_meet Type.Top bar bar;
  assert_meet Type.Any Type.Top Type.Any;
  assert_meet foo bar Type.Bottom;
  assert_meet Type.integer (Type.Union [Type.integer; Type.string]) Type.integer;
  assert_meet (Type.Union [Type.integer; Type.string]) Type.integer Type.integer;
  assert_meet a b b;
  assert_meet a c c;
  assert_meet b c Type.Bottom;
  assert_meet b d d;
  assert_meet c d d;

  (* Special cases. *)
  assert_meet Type.integer Type.float Type.integer


let test_supertypes_type_order context =
  let environment =
    populate ~context ["test.py", {|
      class foo(): pass
      class bar(foo): pass
    |}]
  in
  let order = class_hierarchy environment in
  assert_equal ["object"] (ClassHierarchy.successors order "test.foo");
  assert_equal ["test.foo"; "object"] (ClassHierarchy.successors order "test.bar")


let test_class_summary context =
  let is_defined environment annotation = class_summary environment annotation |> Option.is_some in
  let environment = populate ~context ["baz.py", {|
      class baz(): pass
    |}] in
  assert_true (is_defined environment (Type.Primitive "baz.baz"));
  assert_true (is_defined environment (Type.parametric "baz.baz" [Single Type.integer]));
  assert_is_some (class_summary environment (Type.Primitive "baz.baz"));
  assert_false (is_defined environment (Type.Primitive "bar.bar"));
  assert_false (is_defined environment (Type.parametric "bar.bar" [Single Type.integer]));
  assert_is_none (class_summary environment (Type.Primitive "bar.bar"));
  let any = class_summary environment Type.object_primitive |> value |> Node.value in
  assert_equal any.ClassSummary.name !&"object"


let test_modules context =
  let environment = populate ~context ["wingus.py", ""; "dingus.py", ""; "os/path.py", ""] in
  let global_resolution = GlobalResolution.create environment in
  assert_true (GlobalResolution.module_exists global_resolution !&"wingus");
  assert_true (GlobalResolution.module_exists global_resolution !&"dingus");
  assert_false (GlobalResolution.module_exists global_resolution !&"zap");
  assert_true (GlobalResolution.module_exists global_resolution !&"os");
  assert_true (GlobalResolution.module_exists global_resolution !&"os.path");
  ()


let test_default_class_hierarchy context =
  let order, environment = order_and_environment ~context [] in
  let global_resolution = GlobalResolution.create environment in
  let open TypeOrder in
  let less_or_equal = always_less_or_equal in
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);

  (* Test special forms. *)
  let assert_has_special_form primitive_name =
    assert_true (GlobalResolution.class_exists global_resolution primitive_name)
  in
  assert_has_special_form "typing.Generic";
  assert_has_special_form "typing.Protocol";
  assert_has_special_form "typing.Callable";
  assert_has_special_form "typing.ClassVar";
  assert_has_special_form "typing.Final";

  (* Object *)
  assert_true (less_or_equal order ~left:(Type.optional Type.integer) ~right:Type.object_primitive);
  assert_true (less_or_equal order ~left:(Type.list Type.integer) ~right:Type.object_primitive);
  assert_false (less_or_equal order ~left:Type.object_primitive ~right:(Type.optional Type.integer));

  (* Mock. *)
  assert_true (less_or_equal order ~left:(Type.Primitive "unittest.mock.Base") ~right:Type.Top);
  assert_true
    (less_or_equal order ~left:(Type.Primitive "unittest.mock.NonCallableMock") ~right:Type.Top);

  (* Numerical types. *)
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.integer);
  assert_false (less_or_equal order ~left:Type.float ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.float ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Number"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Real"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Complex"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Number"));
  assert_false (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.complex ~right:(Type.Primitive "numbers.Complex"));
  assert_false (less_or_equal order ~left:Type.complex ~right:(Type.Primitive "numbers.Real"));

  (* Test join. *)
  assert_type_equal (join order Type.integer Type.integer) Type.integer;
  assert_type_equal (join order Type.float Type.integer) Type.float;
  assert_type_equal (join order Type.integer Type.float) Type.float;
  assert_type_equal (join order Type.integer Type.complex) Type.complex;
  assert_type_equal (join order Type.float Type.complex) Type.complex;

  (* Test meet. *)
  assert_type_equal (meet order Type.integer Type.integer) Type.integer;
  assert_type_equal (meet order Type.float Type.integer) Type.integer;
  assert_type_equal (meet order Type.integer Type.float) Type.integer;
  assert_type_equal (meet order Type.integer Type.complex) Type.integer;
  assert_type_equal (meet order Type.float Type.complex) Type.float


let test_connect_annotations_to_top context =
  (* Partial partial order:*)
  (*  0 - 2                *)
  (*  |                    *)
  (*  1   object           *)
  let project =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
       class One:
         pass
       class Two:
         pass
       class Zero(Two, One):
         pass
    |}
        );
      ]
  in
  let global_environment = ScratchProject.global_environment project in
  let order = class_hierarchy global_environment in
  assert_equal (ClassHierarchy.least_upper_bound order "test.One" "test.Two") ["object"]


let test_deduplicate context =
  let project =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
       class One:
         pass
       class Zero(One[int]):
         pass
       class Zero(One[int, int]):
         pass
    |}
        );
      ]
  in
  let global_environment = ScratchProject.global_environment project in
  let (module Handler) = class_hierarchy global_environment in
  let index_of annotation = IndexTracker.index annotation in
  let module TargetAsserter (ListOrSet : ClassHierarchy.Target.ListOrSet) = struct
    let assert_targets edges from target parameters create =
      assert_equal
        ~cmp:ListOrSet.equal
        ~printer:(ListOrSet.to_string ~f:ClassHierarchy.Target.show)
        (find_unsafe edges (index_of from))
        (create { ClassHierarchy.Target.target = index_of target; parameters })
  end
  in
  let module ForwardAsserter = TargetAsserter (ClassHierarchy.Target.List) in
  ForwardAsserter.assert_targets
    Handler.edges
    "test.Zero"
    "test.One"
    [Single Type.integer; Single Type.integer]
    (fun target -> [target]);
  ()


let test_remove_extra_edges_to_object context =
  (*0 -> 1 -> 2 -> object*)
  (*|----^         ^     *)
  (*|--------------^     *)
  let project =
    ScratchProject.setup
      ~context
      ~include_helper_builtins:false
      [
        ( "test.py",
          {|
       class Two(object):
         pass
       class One(Two):
         pass
       class Zero(One, object):
         pass
    |}
        );
      ]
  in
  let global_environment = ScratchProject.global_environment project in
  let (module Handler) = class_hierarchy global_environment in
  let zero_index = IndexTracker.index "test.Zero" in
  let one_index = IndexTracker.index "test.One" in
  let printer = List.to_string ~f:ClassHierarchy.Target.show in
  assert_equal
    ~printer
    (find_unsafe Handler.edges zero_index)
    [{ ClassHierarchy.Target.target = one_index; parameters = [] }];
  ()


let test_update_and_compute_dependencies context =
  (* Pre-test setup *)
  let global_environment, project =
    create_environments_and_project
      ~context
      ~additional_sources:
        ["source.py", {|
          foo = 1
      |}; "other.py", {|
          bar = "A"
      |}]
      ~in_memory:false
      ()
  in
  let dependency_A =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "A"))
  in
  let dependency_B =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "B"))
  in
  (* Establish dependencies *)
  let untracked_global_resolution = GlobalResolution.create global_environment in
  let dependency_tracked_global_resolution_A =
    GlobalResolution.create ~dependency:dependency_A global_environment
  in
  let dependency_tracked_global_resolution_B =
    GlobalResolution.create ~dependency:dependency_B global_environment
  in
  let global resolution name = GlobalResolution.global resolution (Reference.create name) in
  (* A read Foo *)
  global dependency_tracked_global_resolution_A "source.foo" |> Option.is_some |> assert_true;

  (* B read Bar *)
  global dependency_tracked_global_resolution_B "other.bar" |> Option.is_some |> assert_true;

  let assert_update ~repopulate_source_to ~expected_state_after_update ~expected_dependencies =
    let assert_state (primitive, expected) =
      global untracked_global_resolution primitive |> Option.is_some |> assert_equal expected
    in
    let dependents =
      ScratchProject.delete_file project ~relative:"source.py";
      let repopulate_source_to = Option.value repopulate_source_to ~default:"" in
      ScratchProject.add_file project repopulate_source_to ~relative:"source.py";
      let update_result =
        let { Configuration.Analysis.local_root; _ } = ScratchProject.configuration_of project in
        let path = Test.relative_artifact_path ~root:local_root ~relative:"source.py" in
        ScratchProject.update_environment project [path]
      in
      ErrorsEnvironment.Testing.UpdateResult.annotated_global_environment update_result
      |> AnnotatedGlobalEnvironment.UpdateResult.all_triggered_dependencies
      |> List.fold
           ~f:SharedMemoryKeys.DependencyKey.RegisteredSet.union
           ~init:SharedMemoryKeys.DependencyKey.RegisteredSet.empty
      |> SharedMemoryKeys.DependencyKey.RegisteredSet.filter (function registered ->
             (match SharedMemoryKeys.DependencyKey.get_key registered with
             | SharedMemoryKeys.TypeCheckDefine _ -> true
             | _ -> false))
    in
    List.iter expected_state_after_update ~f:assert_state;
    assert_equal
      ~printer:
        (List.to_string ~f:(fun registered ->
             SharedMemoryKeys.DependencyKey.get_key registered |> SharedMemoryKeys.show_dependency))
      (SharedMemoryKeys.DependencyKey.RegisteredSet.elements dependents)
      expected_dependencies
  in
  (* Removes source without replacing it, triggers dependency *)
  assert_update
    ~repopulate_source_to:None
    ~expected_state_after_update:["source.foo", false]
    ~expected_dependencies:[dependency_A];

  (* Re-adds source, triggers dependency *)
  assert_update
    ~repopulate_source_to:(Some "foo = 7")
    ~expected_state_after_update:["source.foo", true]
    ~expected_dependencies:[dependency_A];

  (* Removes source, but replaces it exactly, does not trigger dependency *)
  assert_update
    ~repopulate_source_to:(Some "foo = 7")
    ~expected_state_after_update:["source.foo", true]
    ~expected_dependencies:[];

  (* Removes source, but replaced it with something new, triggers dependency *)
  assert_update
    ~repopulate_source_to:(Some {|
      foo = "A"
      |})
    ~expected_state_after_update:["source.foo", true]
    ~expected_dependencies:[dependency_A];

  (* Irrelevant update, does not trigger dependencies *)
  assert_update
    ~repopulate_source_to:(Some {|
      foo = "A"
      irrelevant = 45
      |})
    ~expected_state_after_update:["source.foo", true; "source.irrelevant", true]
    ~expected_dependencies:[];
  ()


let () =
  "environment"
  >::: [
         "connect_type_order" >:: test_connect_type_order;
         "join_type_order" >:: test_join_type_order;
         "less_or_equal_type_order" >:: test_less_or_equal_type_order;
         "meet_type_order" >:: test_meet_type_order;
         "supertypes_type_order" >:: test_supertypes_type_order;
         "class_summary" >:: test_class_summary;
         "modules" >:: test_modules;
         "populate" >:: test_populate;
         "register_aliases" >:: test_register_aliases;
         "register_globals" >:: test_register_globals;
         "register_implicit_namespace_modules" >:: test_register_implicit_namespace_modules;
         "default_class_hierarchy" >:: test_default_class_hierarchy;
         "connect_to_top" >:: test_connect_annotations_to_top;
         "deduplicate" >:: test_deduplicate;
         "remove_extra" >:: test_remove_extra_edges_to_object;
         "update_and_compute_dependencies" >:: test_update_and_compute_dependencies;
       ]
  |> Test.run
