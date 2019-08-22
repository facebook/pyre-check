(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Statement
open Test

let value option = Option.value_exn option

let class_hierarchy environment =
  Environment.resolution environment () |> GlobalResolution.class_hierarchy


let find_unsafe getter value = getter value |> fun optional -> Option.value_exn optional

let create_environment_and_project
    ~context
    ?(include_typeshed_stubs = true)
    ?(include_helpers = false)
    ?(additional_sources = [])
    ()
  =
  let project = ScratchProject.setup ~context additional_sources in
  let sources, _, environment =
    project
    |> ScratchProject.build_environment
         ~include_typeshed_stubs
         ~include_helper_builtins:include_helpers
  in
  (* TODO (T47159596): This can be done in a more elegant way *)
  let () =
    let set_up_shared_memory _ = () in
    let tear_down_shared_memory () _ =
      let typeshed_sources =
        if include_typeshed_stubs then
          typeshed_stubs ~include_helper_builtins:include_helpers ()
        else
          []
      in
      let qualifiers =
        List.append sources typeshed_sources
        |> List.map ~f:(fun { Source.qualifier; _ } -> qualifier)
      in
      Environment.purge environment qualifiers
    in
    OUnit2.bracket set_up_shared_memory tear_down_shared_memory context
  in
  environment, project


let create_environment ~context ?include_typeshed_stubs ?include_helpers ?additional_sources () =
  create_environment_and_project
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
  let global_resolution = Environment.resolution environment () in
  ( {
      TypeOrder.handler = GlobalResolution.class_hierarchy global_resolution;
      constructor = (fun _ ~protocol_assumptions:_ -> None);
      attributes = (fun _ ~protocol_assumptions:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      any_is_bottom = false;
      protocol_assumptions = TypeOrder.ProtocolAssumptions.empty;
    },
    environment )


let class_definition environment =
  Environment.resolution environment () |> GlobalResolution.class_definition


let parse_annotation environment =
  (* Allow untracked because we're not calling all of populate *)
  Environment.resolution environment () |> GlobalResolution.parse_annotation ~allow_untracked:true


let create_location path start_line start_column end_line end_column =
  let start = { Location.line = start_line; column = start_column } in
  let stop = { Location.line = end_line; column = end_column } in
  { Location.path; start; stop }


let test_register_class_definitions context =
  let environment = create_environment ~context () in
  Environment.register_class_definitions
    environment
    (parse
       {|
       class C:
         ...
       class D(C):
         pass
       B = D
       A = B
       def foo()->A:
         return C()
    |})
  |> ignore;
  assert_equal (parse_annotation environment !"C") (Type.Primitive "C");
  assert_equal (parse_annotation environment !"D") (Type.Primitive "D");
  assert_equal (parse_annotation environment !"B") (Type.Primitive "B");
  assert_equal (parse_annotation environment !"A") (Type.Primitive "A");
  let order = class_hierarchy environment in
  assert_equal (ClassHierarchy.successors order "C") [];

  (* Annotations for classes are returned even if they already exist in the handler. *)
  let new_annotations =
    Environment.register_class_definitions
      environment
      (parse {|
         class C:
           ...
       |})
  in
  assert_equal
    ~cmp:Type.Primitive.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ next))
    (Type.Primitive.Set.singleton "C")
    new_annotations;
  let new_annotations =
    Environment.register_class_definitions environment (parse "class int: pass")
  in
  assert_equal
    ~cmp:Type.Primitive.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ next))
    (Type.Primitive.Set.singleton "int")
    new_annotations


let test_register_class_metadata context =
  let environment = create_environment ~context ~include_helpers:false () in
  let source =
    parse
      {|
       from placeholder_stub import MadeUpClass
       class A: pass
       class B(A): pass
       class C:
         def __init__(self):
           self.x = 3
       class D(C):
         def __init__(self):
           self.y = 4
         D.z = 5
       class E(D, A): pass
       class F(B, MadeUpClass, A): pass
      |}
    |> Preprocessing.preprocess
  in
  let all_annotations = Environment.register_class_definitions environment source |> Set.to_list in
  let resolution = Environment.resolution environment () in
  Environment.connect_type_order environment resolution source;
  Environment.deduplicate_class_hierarchy ~annotations:all_annotations;
  Environment.connect_annotations_to_object all_annotations;
  Environment.remove_extra_edges_to_object all_annotations;
  Environment.register_class_metadata environment "A";
  Environment.register_class_metadata environment "B";
  Environment.register_class_metadata environment "C";
  Environment.register_class_metadata environment "D";
  Environment.register_class_metadata environment "E";
  Environment.register_class_metadata environment "F";
  let assert_successors class_name expected =
    let { GlobalResolution.successors; _ } =
      let global_resolution = Environment.resolution environment () in
      Option.value_exn (GlobalResolution.class_metadata global_resolution (Primitive class_name))
    in
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      ~cmp:(List.equal Type.Primitive.equal)
      expected
      successors
  in
  assert_successors "C" ["object"];
  assert_successors "D" ["C"; "object"];
  assert_successors "B" ["A"; "object"];
  assert_successors "E" ["D"; "C"; "A"; "object"];
  let assert_extends_placeholder_stub_class class_name expected =
    let { GlobalResolution.extends_placeholder_stub_class; _ } =
      let global_resolution = Environment.resolution environment () in
      Option.value_exn (GlobalResolution.class_metadata global_resolution (Primitive class_name))
    in
    assert_equal expected extends_placeholder_stub_class
  in
  assert_extends_placeholder_stub_class "A" false;
  assert_extends_placeholder_stub_class "F" true;
  ()


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
    [ ( "__init__.py",
        {|
          class C: ...
          class D(C): pass
          B: typing.TypeAlias = D
          A: typing.TypeAlias = B
          Twiddledee: typing.TypeAlias
          Twiddledum: typing.TypeAlias
          Twiddledee, Twiddledum = C, C
        |}
      ) ]
    ["C", "C"; "D", "D"; "B", "D"; "A", "D"; "Twiddledee", "Twiddledee"; "Twiddledum", "Twiddledum"];

  assert_resolved
    [ ( "qualifier.py",
        {|
          class C: ...
          class D(C): pass
          B: typing.TypeAlias = D
          A: typing.TypeAlias = B
        |}
      ) ]
    [ "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D" ];

  (* Non-explicit Alaises *)
  assert_resolved
    [ ( "__init__.py",
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
          Twiddledee, Twiddledum = C, C
        |}
      ) ]
    ["C", "C"; "D", "D"; "B", "D"; "A", "D"; "Twiddledee", "Twiddledee"; "Twiddledum", "Twiddledum"];

  assert_resolved
    [ ( "qualifier.py",
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
        |}
      ) ]
    [ "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D" ];
  assert_resolved ["__init__.py", "X = None"] ["X", "None"];

  (* Imports *)
  assert_resolved
    [ ( "collections.py",
        {|
          from typing import Iterator as TypingIterator
          from typing import Iterable
        |}
      ) ]
    [ "collections.TypingIterator", "typing.Iterator[typing.Any]";
      "collections.Iterable", "typing.Iterable[typing.Any]" ];

  (* Handle builtins correctly. *)
  assert_resolved
    [ ( "collections.py",
        {|
          from builtins import int
          from builtins import dict as CDict
        |}
      ) ]
    ["collections.int", "int"; "collections.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [ ( "collections.py",
        {|
          from future.builtins import int
          from future.builtins import dict as CDict
        |}
      ) ]
    ["collections.int", "int"; "collections.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [ ( "asyncio/tasks.py",
        {|
           from typing import TypeVar, Generic, Union
           _T = typing.TypeVar('_T')
           class Future(Generic[_T]): ...
           class Awaitable(Generic[_T]): ...
           _FutureT = Union[Future[_T], Awaitable[_T]]
        |}
      ) ]
    [ "asyncio.tasks.Future[int]", "asyncio.tasks.Future[int]";
      ( "asyncio.tasks._FutureT[int]",
        "typing.Union[asyncio.tasks.Awaitable[int], asyncio.tasks.Future[int]]" ) ];
  assert_resolved
    [ ( "a.py",
        {|
          import typing
          _T = typing.TypeVar("_T")
          _T2 = typing.TypeVar("UnrelatedName")
        |}
      ) ]
    ["a._T", "Variable[a._T]"; "a._T2", "Variable[a._T2]"];

  (* Type variable aliases in classes. *)
  assert_resolved
    [ ( "qualifier.py",
        {|
          class Class:
            T = typing.TypeVar('T')
            Int = int
        |}
      ) ]
    ["qualifier.Class.T", "Variable[qualifier.Class.T]"; "qualifier.Class.Int", "int"];

  (* Stub-suppressed aliases show up as `Any`. *)
  assert_resolved
    [ "stubbed.pyi", "# pyre-placeholder-stub";
      ( "qualifier.py",
        {|
          class str: ...
          T = stubbed.Something
          Q = typing.Union[stubbed.Something, str]
        |}
      ) ]
    ["qualifier.T", "typing.Any"; "qualifier.Q", "typing.Union[typing.Any, qualifier.str]"];
  assert_resolved
    [ ( "t.py",
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
      ) ]
    [ "x.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
      "t.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]" ];
  assert_resolved
    [ "t.py", {|
          from typing import Dict
        |};
      "x.py", {|
          from t import *
        |} ]
    ["x.Dict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    ["x.py", {|
          C = typing.Callable[[gurbage], gurbage]
        |}]
    ["x.C", "x.C"];
  assert_resolved
    [ ( "__init__.py",
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
      ) ]
    ["A", "int"; "B", "B"; "C", "C"; "D", "D"; "E", "typing.Any"; "F", "F"; "G", "int"; "H", "H"];
  assert_resolved
    ["a.py", {|
          class Foo: ...
        |}; "b.py", {|
          import a
        |}]
    ["b.a.Foo", "a.Foo"];
  assert_resolved
    [ "a.py", {|
          class Foo: ...
        |};
      "b.py", {|
          from a import Foo
          class Foo:
            ...
        |} ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [ "a.py", {|
          class Bar: ...
        |};
      "b.py", {|
          from a import Bar as Foo
          class Foo:
            ...
        |}
    ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [ "a.py", {|
          class Foo: ...
        |};
      "b.py", {|
          from a import Foo as Bar
          class Foo: ...
        |} ]
    ["b.Foo", "b.Foo"; "b.Bar", "a.Foo"];

  let assert_resolved sources aliases =
    let environment = register_all sources in
    let global_resolution = Environment.resolution environment () in
    let assert_alias (alias, target) =
      match GlobalResolution.aliases global_resolution alias with
      | Some alias -> assert_equal ~printer:Type.show_alias target alias
      | None -> failwith "Alias is missing"
    in
    List.iter aliases ~f:assert_alias
  in
  assert_resolved
    [ ( "__init__.py",
        {|
          Tparams = pyre_extensions.ParameterSpecification('Tparams')
          Ts = pyre_extensions.ListVariadic('Ts')
      |}
      ) ]
    [ ( "Tparams",
        Type.VariableAlias
          (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "Tparams")) );
      ( "Ts",
        Type.VariableAlias (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "Ts")) )
    ];
  ()


let test_register_implicit_submodules context =
  let environment = create_environment ~context () in
  Environment.register_implicit_submodules environment (Reference.create "a.b.c");
  let global_resolution = Environment.resolution environment () in
  assert_equal
    None
    (GlobalResolution.module_definition global_resolution (Reference.create "a.b.c"));
  assert_true (Environment.is_module environment (Reference.create "a.b"));
  assert_true (Environment.is_module environment (Reference.create "a"))


let test_connect_definition context =
  let environment = create_environment ~context () in
  let resolution = Environment.resolution environment () in
  let (module TypeOrderHandler : ClassHierarchy.Handler) = class_hierarchy environment in
  Environment.register_class_definitions
    environment
    (parse {|
       class C:
         pass
       class D:
         pass
      |})
  |> ignore;
  let assert_edge ~predecessor ~successor =
    let predecessor_index = find_unsafe TypeOrderHandler.indices predecessor in
    let successor_index = find_unsafe TypeOrderHandler.indices successor in
    assert_true
      (List.mem
         ~equal:ClassHierarchy.Target.equal
         (find_unsafe TypeOrderHandler.edges predecessor_index)
         { ClassHierarchy.Target.target = successor_index; parameters = Concrete [] });
    assert_true
      (ClassHierarchy.Target.Set.mem
         (find_unsafe TypeOrderHandler.backedges successor_index)
         { ClassHierarchy.Target.target = predecessor_index; parameters = Concrete [] })
  in
  let class_definition =
    +{ Class.name = !&"C"; bases = []; body = []; decorators = []; docstring = None }
  in
  Environment.connect_definition environment ~resolution ~definition:class_definition;
  let definition = +Test.parse_single_class {|
       class D(int, float):
         ...
     |} in
  Environment.connect_definition environment ~resolution ~definition;
  assert_edge ~predecessor:"D" ~successor:"int";
  assert_edge ~predecessor:"D" ~successor:"float"


let test_register_globals context =
  let environment = create_environment ~context () in
  let resolution = Environment.resolution environment () in
  let assert_global reference expected =
    let actual =
      !&reference |> GlobalResolution.global resolution >>| Node.value >>| Annotation.annotation
    in
    assert_equal
      ~printer:(function
        | Some annotation -> Type.show annotation
        | _ -> "(none)")
      ~cmp:(Option.equal Type.equal)
      expected
      actual
  in
  let source =
    parse
      ~handle:"qualifier.py"
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
    |> Preprocessing.preprocess
  in
  Environment.register_values environment resolution source;
  assert_global "qualifier.undefined" None;
  assert_global "qualifier.with_join" (Some (Type.union [Type.integer; Type.string]));
  assert_global "qualifier.with_resolve" (Some Type.Top);
  assert_global "qualifier.annotated" (Some Type.integer);
  assert_global "qualifier.unannotated" (Some Type.string);
  assert_global "qualifier.stub" (Some Type.integer);
  assert_global "qualifier.Class" (Some (Type.meta (Type.Primitive "qualifier.Class")));
  assert_global "qualifier.in_branch" (Some Type.integer);
  assert_global "qualifier.identifier.access" None;
  assert_global "qualifier.identifier().access" None;
  assert_global "Foo.attribute" None;
  let source =
    parse
      ~handle:"test.py"
      {|
        class Class: ...
        alias = Class

        GLOBAL: Class = ...
        GLOBAL2: alias = ...
      |}
    |> Preprocessing.preprocess
  in
  Environment.register_values environment resolution source;
  assert_global "test.GLOBAL" (Some (Type.Primitive "test.Class"));
  assert_global "test.GLOBAL2" (Some (Type.Primitive "test.alias"));
  let source =
    parse ~handle:"tuples.py" {|
        def f():
          return 7, 8
        y, z = f()
      |}
    |> Preprocessing.preprocess
  in
  Environment.register_values environment resolution source;
  assert_global "tuples.y" (Some Type.Top);
  assert_global "tuples.z" (Some Type.Top);
  ()


let test_connect_type_order context =
  let environment = create_environment ~context ~include_helpers:false () in
  let resolution = Environment.resolution environment () in
  let source =
    parse
      {|
       class C:
         ...
       class D(C):
         pass
       class CallMe:
         def CallMe.__call__(self, x: int) -> str:
           ...
       B = D
       A = B
       def foo() -> A:
         return D()
    |}
  in
  let order = class_hierarchy environment in
  let all_annotations = Environment.register_class_definitions environment source |> Set.to_list in
  Environment.register_aliases environment [source];
  Environment.connect_type_order environment resolution source;
  let assert_successors annotation successors =
    assert_equal
      ~printer:(List.to_string ~f:Type.Primitive.show)
      successors
      (ClassHierarchy.successors order annotation)
  in
  (* Classes get connected to object via `connect_annotations_to_top`. *)
  assert_successors "C" [];
  assert_successors "D" ["C"];
  Environment.connect_annotations_to_object all_annotations;
  assert_successors "C" ["object"];
  assert_successors "D" ["C"; "object"];
  assert_successors "CallMe" ["typing.Callable"; "object"]


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
    (Type.parametric "Optional" (Concrete [Type.Primitive "foo.foo"]));
  assert_equal (parse_annotation environment !"bar") (Type.Primitive "bar");

  (* Check custom aliases. *)
  assert_equal
    (parse_annotation environment !"typing.DefaultDict")
    (Type.parametric "collections.defaultdict" (Concrete [Type.Any; Type.Any]));

  (* Check custom class definitions. *)
  let global_resolution = Environment.resolution environment () in
  assert_is_some (GlobalResolution.class_definition global_resolution (Primitive "None"));
  assert_is_some
    (GlobalResolution.class_definition global_resolution (Primitive "typing.Optional"));

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
  let assert_superclasses
      ?(superclass_parameters = fun _ -> Type.OrderedTypes.Concrete [])
      ~environment
      base
      ~superclasses
    =
    let (module TypeOrderHandler) = class_hierarchy environment in
    let index annotation = find_unsafe TypeOrderHandler.indices annotation in
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
            let index = Int.to_string target in
            let target = find_unsafe TypeOrderHandler.annotations target in
            Format.asprintf "%s: %s%a" index target Type.OrderedTypes.pp_concise parameters
          in
          List.to_string targets ~f:show_target
    in
    assert_equal
      ~printer:show_targets
      ~cmp:(Option.equal (List.equal ClassHierarchy.Target.equal))
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
      [ ( "test.py",
          {|
        def foo() -> int:
          return 1
        class C(foo()):
          pass
      |}
        ) ]
  in
  assert_superclasses ~environment "test.C" ~superclasses:["object"];

  (* Globals *)
  let assert_global_with_environment environment actual expected =
    let global_resolution = Environment.resolution environment () in
    assert_equal
      ~cmp:(Option.equal (Node.equal Annotation.equal))
      ~printer:(function
        | Some global -> GlobalResolution.show_global global
        | None -> "None")
      (Some (Node.create_with_default_location expected))
      (GlobalResolution.global global_resolution !&actual)
  in
  let assert_global =
    populate
      ~context
      [ ( "test.py",
          {|
      class int(): pass
      A: int = 0
      B = 0
      C = ... # type: int

      class Foo(): pass
      alias = Foo
      G: Foo = ...
      H: alias = ...
    |}
        ) ]
    |> assert_global_with_environment
  in
  assert_global
    "test.A"
    (Annotation.create_immutable ~global:true (parse_annotation environment !"test.int"));
  assert_global "test.B" (Annotation.create_immutable ~global:true Type.integer);
  assert_global
    "test.C"
    (Annotation.create_immutable ~global:true (parse_annotation environment !"test.int"));
  assert_global
    "test.G"
    (Annotation.create_immutable ~global:true (parse_annotation environment !"test.Foo"));
  assert_global
    "test.H"
    (Annotation.create_immutable ~global:true (parse_annotation environment !"test.Foo"));
  let assert_global =
    populate
      ~context
      [ ( "__init__.py",
          {|
      global_value_set = 1
      global_annotated: int
      global_both: int = 1
      global_unknown = x
      global_function = function
      class Class():
        def Class.__init__(self):
          pass
      def function():
        pass
    |}
        ) ]
    |> assert_global_with_environment
  in
  assert_global "global_value_set" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_annotated" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_both" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_unknown" (Annotation.create_immutable ~global:true Type.Top);
  assert_global
    "function"
    (Annotation.create_immutable
       ~global:true
       (Type.Callable.create
          ~name:!&"function"
          ~parameters:(Type.Callable.Defined [])
          ~annotation:Type.Top
          ()));
  assert_global
    "global_function"
    (Annotation.create_immutable ~global:true ~original:(Some Type.Top) Type.Top);
  assert_global
    "Class"
    (Annotation.create_immutable
       ~global:true
       ~original:(Some Type.Top)
       (Type.meta (Type.Primitive "Class")));
  assert_global
    "Class.__init__"
    (Annotation.create_immutable
       ~global:true
       (Type.Callable.create
          ~name:!&"Class.__init__"
          ~parameters:
            (Type.Callable.Defined
               [ Type.Callable.Parameter.Named
                   { name = "self"; annotation = Type.Top; default = false } ])
          ~annotation:Type.Top
          ()));

  (* Properties. *)
  let assert_global =
    populate
      ~context
      [ ( "test.py",
          {|
      class Class:
        @property
        def Class.property(self) -> int: ...
    |}
        ) ]
    |> assert_global_with_environment
  in
  assert_global
    "test.Class.property"
    (Annotation.create_immutable
       ~global:true
       (Type.Callable.create
          ~name:!&"test.Class.property"
          ~parameters:
            (Type.Callable.Defined
               [ Type.Callable.Parameter.Named
                   { name = "self"; annotation = Type.Top; default = false } ])
          ~annotation:Type.integer
          ()));

  (* Loops. *)
  ( try
      populate
        ~context
        ["test.py", {|
        def foo(cls):
          class cls(cls): pass
      |}]
      |> ignore
    with
  | ClassHierarchy.Cyclic -> assert_unreached () );

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
    ( Type.Primitive "test.A"
    |> Type.meta
    |> Annotation.create_immutable ~global:true ~original:(Some Type.Top) );

  (* Callable classes. *)
  let environment =
    populate
      ~context
      [ ( "test.py",
          {|
      class CallMe:
        def CallMe.__call__(self, x: int) -> str:
          pass
      class AlsoCallable(CallMe):
        pass
  |}
        ) ]
  in
  let type_parameters annotation =
    match annotation with
    | "typing.Callable" ->
        Type.OrderedTypes.Concrete
          [ parse_single_expression "typing.Callable('test.CallMe.__call__')[[Named(x, int)], str]"
            |> Type.create ~aliases:(fun _ -> None) ]
    | _ -> Type.OrderedTypes.Concrete []
  in
  assert_superclasses
    ~superclass_parameters:type_parameters
    ~environment
    "test.CallMe"
    ~superclasses:["typing.Callable"];
  ();
  let environment = populate ~context ["test.py", {|
      def foo(x: int) -> str: ...
    |}] in
  let global_resolution = Environment.resolution environment () in
  assert_equal
    ~cmp:(Option.equal (Type.Callable.equal_overload Type.equal))
    ~printer:(function
      | None -> "None"
      | Some callable -> Format.asprintf "Some (%a)" (Type.Callable.pp_overload Type.pp) callable)
    (GlobalResolution.undecorated_signature global_resolution (Reference.create "test.foo"))
    (Some
       {
         Type.Callable.annotation = Type.string;
         parameters =
           Type.Callable.Defined
             [ Type.Callable.Parameter.Named
                 { annotation = Type.integer; name = "x"; default = false } ];
         define_location = None;
       });
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
      [ ( "module.py",
          {|
        class sub(super): pass
        class super(top): pass
        class top(): pass
    |}
        ) ]
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
      [ ( "test.py",
          {|
      class A: ...
      class B(A): ...
      class C(typing.Optional[A]): ...
    |}
        ) ]
  in
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "test.A"))
       ~right:(Type.Optional (Type.Primitive "test.A")));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "test.A")
       ~right:(Type.Optional (Type.Primitive "test.A")));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "test.A"))
       ~right:(Type.Primitive "test.A"));

  (* We're currently not sound with inheritance and optionals. *)
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "test.A"))
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
      [ ( "test.py",
          {|
      class A: ...
      class B(A): ...
      class int(): ...
      class float(): ...
    |}
        ) ]
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
       ~right:(Type.Union [Type.Top; Type.Any; Type.Optional Type.float]));
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
  assert_raises (ClassHierarchy.Untracked (Type.Primitive "test.durp")) (fun _ ->
      TypeOrder.join order bar (Type.Primitive "test.durp"));

  (* Special cases. *)
  assert_equal (TypeOrder.join order Type.integer Type.float) Type.float


let test_meet_type_order context =
  let order, _ =
    order_and_environment
      ~context
      [ ( "test.py",
          {|
      class foo(): ...
      class bar(L[T]): ...
      class A: ...
      class B(A): ...
      class C(A): ...
      class D(B,C): ...
    |}
        ) ]
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
  assert_meet b c d;
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


let test_class_definition context =
  let is_defined environment annotation =
    class_definition environment annotation |> Option.is_some
  in
  let environment = populate ~context ["baz.py", {|
      class baz(): pass
    |}] in
  assert_true (is_defined environment (Type.Primitive "baz.baz"));
  assert_true (is_defined environment (Type.parametric "baz.baz" (Concrete [Type.integer])));
  assert_is_some (class_definition environment (Type.Primitive "baz.baz"));
  assert_false (is_defined environment (Type.Primitive "bar.bar"));
  assert_false (is_defined environment (Type.parametric "bar.bar" (Concrete [Type.integer])));
  assert_is_none (class_definition environment (Type.Primitive "bar.bar"));
  let any = class_definition environment Type.object_primitive |> value |> Node.value in
  assert_equal any.Class.name !&"object"


let test_modules context =
  let environment = populate ~context ["wingus.py", ""; "dingus.py", ""; "os/path.py", ""] in
  let global_resolution = Environment.resolution environment () in
  assert_is_some (GlobalResolution.module_definition global_resolution !&"wingus");
  assert_is_some (GlobalResolution.module_definition global_resolution !&"dingus");
  assert_is_none (GlobalResolution.module_definition global_resolution !&"zap");
  assert_is_some (GlobalResolution.module_definition global_resolution !&"os");
  assert_is_some (GlobalResolution.module_definition global_resolution !&"os.path");
  assert_true (Environment.is_module environment !&"wingus");
  assert_true (Environment.is_module environment !&"dingus");
  assert_false (Environment.is_module environment !&"zap");
  ()


let test_import_dependencies context =
  let environment =
    let source =
      {|
         import a
         from builtins import str
         from subdirectory.b import c
         import sys
         from . import ignored
      |}
    in
    populate ~context ["test.py", source; "a.py", ""; "subdirectory/b.py", ""]
  in
  let dependencies qualifier =
    Environment.dependencies environment !&qualifier
    >>| String.Set.Tree.map ~f:Reference.show
    >>| String.Set.Tree.to_list
  in
  assert_equal (dependencies "subdirectory.b") (Some ["test"]);
  assert_equal (dependencies "a") (Some ["test"]);
  assert_equal (dependencies "") (Some ["test"]);
  assert_equal (dependencies "sys") (Some ["test"])


let test_register_dependencies context =
  let environment = create_environment ~context () in
  let source =
    {|
         import a # a is added here
         from subdirectory.b import c # subdirectory.b is added here
         from . import ignored # no dependency created here
      |}
  in
  Environment.register_dependencies environment (parse ~handle:"test.py" source);
  let dependencies qualifier =
    Environment.dependencies environment !&qualifier
    >>| String.Set.Tree.map ~f:Reference.show
    >>| String.Set.Tree.to_list
  in
  assert_equal (dependencies "subdirectory.b") (Some ["test"]);
  assert_equal (dependencies "a") (Some ["test"])


let test_purge context =
  let source =
    {|
      import a
      class P(typing.Protocol): pass
      class baz(): pass
      _T = typing.TypeVar("_T")
      x = 5
      def foo(): pass
    |}
  in
  let handler =
    populate ~include_typeshed_stubs:false ~include_helpers:false ~context ["test.py", source]
  in
  let global_resolution = Environment.resolution handler () in
  assert_is_some (GlobalResolution.class_definition global_resolution (Primitive "test.baz"));
  assert_is_some (GlobalResolution.aliases global_resolution "test._T");
  let dependencies relative =
    Environment.dependencies handler (SourcePath.qualifier_of_relative relative)
    >>| String.Set.Tree.map ~f:Reference.show
    >>| String.Set.Tree.to_list
  in
  assert_equal (dependencies "a.py") (Some ["test"]);
  assert_is_some (GlobalResolution.class_metadata global_resolution (Primitive "test.P"));
  assert_is_some (GlobalResolution.class_metadata global_resolution (Primitive "test.baz"));
  assert_true (GlobalResolution.is_tracked global_resolution "test.P");
  Environment.check_class_hierarchy_integrity ();
  Environment.purge handler [Reference.create "test"];
  assert_is_none (GlobalResolution.class_definition global_resolution (Primitive "test.baz"));
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "test.P"));
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "test.baz"));
  assert_is_none (GlobalResolution.aliases global_resolution "test._T");
  assert_false (GlobalResolution.is_tracked global_resolution "test.P");
  assert_equal (dependencies "a.py") (Some []);
  Environment.check_class_hierarchy_integrity ();
  ()


let test_purge_hierarchy context =
  let order () =
    let one_source = {|
      from Two import Two
      class One(Two):
        pass
    |} in
    let two_source = {|
      class Two:
        pass
    |} in
    let a_source = {|
      from One import One
      class a(One):
        pass
    |} in
    let b_source = {|
      from One import One
      class b(One):
        pass
    |} in
    populate
      ~include_typeshed_stubs:false
      ~include_helpers:false
      ~context
      ["A.py", a_source; "B.py", b_source; "One.py", one_source; "Two.py", two_source]
  in
  let assert_backedges_equal wrapped_left unwrapped_right =
    let printer set =
      ClassHierarchy.Target.Set.to_list set |> List.to_string ~f:ClassHierarchy.Target.show
    in
    assert_equal
      ~printer
      ~cmp:ClassHierarchy.Target.Set.equal
      wrapped_left
      (ClassHierarchy.Target.Set.of_list unwrapped_right)
  in
  let assert_node_completely_deleted (module Handler : ClassHierarchy.Handler) ~annotation ~index =
    assert_equal
      ( Handler.indices annotation,
        Handler.annotations index,
        Handler.edges index,
        Handler.backedges index )
      (None, None, None, None)
  in
  let () =
    let handler = order () in
    let (module Handler) = class_hierarchy handler in
    let index key = find_unsafe Handler.indices key in
    let one_index = index "One.One" in
    Environment.purge handler [Reference.create "One"];
    assert_node_completely_deleted (module Handler) ~annotation:"One.One" ~index:one_index;
    assert_backedges_equal (find_unsafe Handler.backedges (index "Two.Two")) [];
    assert_equal
      (find_unsafe Handler.edges (index "A.a"))
      [{ ClassHierarchy.Target.target = one_index; parameters = Concrete [] }]
  in
  let () =
    let handler = order () in
    let (module Handler) = class_hierarchy handler in
    let index key = find_unsafe Handler.indices key in
    let a_index = index "A.a" in
    Environment.purge handler [Reference.create "A"];
    assert_node_completely_deleted (module Handler) ~annotation:"A.a" ~index:a_index;
    assert_backedges_equal
      (find_unsafe Handler.backedges (index "One.One"))
      [{ ClassHierarchy.Target.target = index "B.b"; parameters = Concrete [] }]
  in
  let () =
    let handler = order () in
    let (module Handler) = class_hierarchy handler in
    let index key = find_unsafe Handler.indices key in
    let b_index = index "B.b" in
    Environment.purge handler [Reference.create "B"];
    assert_node_completely_deleted (module Handler) ~annotation:"B.b" ~index:b_index;
    assert_backedges_equal
      (find_unsafe Handler.backedges (index "One.One"))
      [{ ClassHierarchy.Target.target = index "A.a"; parameters = Concrete [] }]
  in
  let () =
    let handler = order () in
    let (module Handler) = class_hierarchy handler in
    let index key = find_unsafe Handler.indices key in
    let a_index = index "A.a" in
    let b_index = index "B.b" in
    Environment.purge handler [Reference.create "A"; Reference.create "B"];
    assert_node_completely_deleted (module Handler) ~annotation:"A.a" ~index:a_index;
    assert_node_completely_deleted (module Handler) ~annotation:"B.b" ~index:b_index;
    assert_backedges_equal (find_unsafe Handler.backedges (index "One.One")) []
  in
  ()


let test_propagate_nested_classes context =
  let test_propagate sources aliases =
    Type.Cache.disable ();
    Type.Cache.enable ();
    let handler = populate ~context ~include_helpers:false sources in
    let assert_alias (alias, target) =
      parse_single_expression alias
      |> parse_annotation handler
      |> Type.show
      |> assert_equal ~printer:(fun string -> string) target
    in
    List.iter aliases ~f:assert_alias
  in
  test_propagate
    [ ( "test.py",
        {|
          class B:
            class N:
              pass
          class C(B):
            pass
        |}
      ) ]
    ["test.C.N", "test.B.N"];
  test_propagate
    [ ( "test.py",
        {|
          class B:
            class N:
              pass
          class C(B):
            class N:
              pass
        |}
      ) ]
    ["test.C.N", "test.C.N"];
  test_propagate
    [ ( "test.py",
        {|
          class B1:
            class N:
              pass
          class B2:
            class N:
              pass
          class C(B1, B2):
            pass
        |}
      ) ]
    ["test.C.N", "test.B1.N"];
  test_propagate
    [ "qual.py", {|
          class B:
            class N:
              pass
        |};
      ( "importer.py",
        {|
          from qual import B
          class C(B):
            pass
        |} ) ]
    ["importer.C.N", "qual.B.N"];
  test_propagate
    [ ( "test.py",
        {|
          class B:
            class N:
              class NN:
                class NNN:
                  pass
          class C(B):
            pass
        |}
      ) ]
    ["test.C.N.NN.NNN", "test.B.N.NN.NNN"];
  ()


let test_default_class_hierarchy context =
  let order, _ = order_and_environment ~context [] in
  Environment.fill_shared_memory_with_default_typeorder ();
  let open TypeOrder in
  let less_or_equal = always_less_or_equal in
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);

  (* Test special forms. *)
  let assert_has_special_form primitive_name =
    assert_true (ClassHierarchy.contains order.handler primitive_name)
  in
  assert_has_special_form "typing.Tuple";
  assert_has_special_form "typing.Generic";
  assert_has_special_form "typing.Protocol";
  assert_has_special_form "typing.Callable";
  assert_has_special_form "typing.ClassVar";
  assert_has_special_form "typing.Final";

  (* Object *)
  assert_true (less_or_equal order ~left:(Type.optional Type.integer) ~right:Type.object_primitive);
  assert_true (less_or_equal order ~left:(Type.list Type.integer) ~right:Type.object_primitive);
  assert_false
    (less_or_equal order ~left:Type.object_primitive ~right:(Type.optional Type.integer));

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
  let environment = create_environment ~context () in
  let source =
    parse
      {|
       class One:
         pass
       class Two:
         pass
       class Zero(Two, One):
         pass
    |}
  in
  Environment.register_class_definitions environment source |> ignore;
  let resolution = Environment.resolution environment () in
  Environment.connect_type_order environment resolution source;
  let order = class_hierarchy environment in
  assert_false (ClassHierarchy.least_upper_bound order "One" "Two" = ["object"]);
  assert_false (ClassHierarchy.greatest_lower_bound order "One" "object" = ["One"]);

  Environment.connect_annotations_to_object ["One"; "Two"; "Zero"; "object"];

  assert_equal (ClassHierarchy.least_upper_bound order "One" "Two") ["object"];

  (* Ensure that the backedge gets added as well *)
  assert_equal (ClassHierarchy.greatest_lower_bound order "One" "object") ["One"]


let test_deduplicate context =
  let environment = create_environment ~context () in
  let source =
    parse
      {|
       class One:
         pass
       class Zero(One[int, int]):
         pass
       class Zero(One[int]):
         pass
    |}
  in
  Environment.register_class_definitions environment source |> ignore;
  let resolution = Environment.resolution environment () in
  Environment.connect_type_order environment resolution source;
  Environment.deduplicate_class_hierarchy ~annotations:["One"; "Zero"];
  let (module Handler) = class_hierarchy environment in
  let index_of annotation = find_unsafe Handler.indices annotation in
  let module TargetAsserter (ListOrSet : ClassHierarchy.Target.ListOrSet) = struct
    let assert_targets edges from target parameters create =
      assert_equal
        ~cmp:ListOrSet.equal
        ~printer:(ListOrSet.to_string ~f:ClassHierarchy.Target.show)
        (find_unsafe edges (index_of from))
        (create
           { ClassHierarchy.Target.target = index_of target; parameters = Concrete parameters })
  end
  in
  let module ForwardAsserter = TargetAsserter (ClassHierarchy.Target.List) in
  let module BackwardsAsserter = TargetAsserter (ClassHierarchy.Target.Set) in
  ForwardAsserter.assert_targets Handler.edges "Zero" "One" [Type.integer] (fun target -> [target]);
  BackwardsAsserter.assert_targets Handler.backedges "One" "Zero" [Type.integer] (fun target ->
      ClassHierarchy.Target.Set.of_list [target])


let test_remove_extra_edges_to_object context =
  (*0 -> 1 -> 2 -> object*)
  (*|----^         ^     *)
  (*|--------------^     *)
  let environment = create_environment ~context () in
  let source =
    parse
      {|
       class Two(object):
         pass
       class One(Two):
         pass
       class Zero(One, object):
         pass
    |}
  in
  Environment.register_class_definitions environment source |> ignore;
  let resolution = Environment.resolution environment () in
  Environment.connect_type_order environment resolution source;
  Environment.remove_extra_edges_to_object ["Zero"; "One"; "Two"; "object"];
  let (module Handler) = class_hierarchy environment in
  let zero_index = find_unsafe Handler.indices "Zero" in
  let one_index = find_unsafe Handler.indices "One" in
  let two_index = find_unsafe Handler.indices "Two" in
  let object_index = find_unsafe Handler.indices "object" in
  assert_equal
    (find_unsafe Handler.edges zero_index)
    [{ ClassHierarchy.Target.target = one_index; parameters = Concrete [] }];
  let filter_only_relevant_targets =
    Set.filter ~f:(fun { ClassHierarchy.Target.target; _ } ->
        List.mem [zero_index; one_index; two_index; object_index] target ~equal:Int.equal)
  in
  assert_equal
    ~cmp:ClassHierarchy.Target.Set.equal
    (find_unsafe Handler.backedges object_index |> filter_only_relevant_targets)
    (ClassHierarchy.Target.Set.of_list
       [{ ClassHierarchy.Target.target = two_index; parameters = Concrete [] }]);
  ()


let test_update_and_compute_dependencies context =
  (* Pre-test setup *)
  let environment, project =
    create_environment_and_project
      ~context
      ~additional_sources:
        [ "source.py", {|
      class Foo(): ...
      |};
          "other.py", {|
      class Bar(): ...
      |} ]
      ()
  in
  let dependency_A = Reference.create "A" in
  let dependency_B = Reference.create "B" in
  (* Establish dependencies *)
  let untracked_global_resolution = Environment.resolution environment () in
  let dependency_tracked_global_resolution_A =
    Environment.dependency_tracked_resolution environment ~dependency:dependency_A ()
  in
  let dependency_tracked_global_resolution_B =
    Environment.dependency_tracked_resolution environment ~dependency:dependency_B ()
  in
  (* A read Foo *)
  GlobalResolution.class_definition dependency_tracked_global_resolution_A (Primitive "source.Foo")
  |> Option.is_some
  |> assert_true;

  (* B read Bar *)
  GlobalResolution.class_definition dependency_tracked_global_resolution_B (Primitive "other.Bar")
  |> Option.is_some
  |> assert_true;

  let assert_update
      ~repopulate_source_to
      ~expected_state_in_update
      ~expected_state_after_update
      ~expected_dependencies
    =
    let repopulate source =
      let source = Test.parse ~handle:"source.py" source |> Preprocessing.preprocess in
      Service.Environment.populate
        ~configuration:(ScratchProject.configuration_of project)
        ~scheduler:(Scheduler.mock ())
        environment
        [source]
    in
    let assert_state (primitive, expected) =
      GlobalResolution.class_definition untracked_global_resolution (Primitive primitive)
      |> Option.is_some
      |> assert_equal expected
    in
    let (), dependents =
      let update () =
        List.iter expected_state_in_update ~f:assert_state;
        Option.iter repopulate_source_to ~f:repopulate
      in
      Environment.update_and_compute_dependencies environment [Reference.create "source"] ~update
    in
    assert_equal
      (SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements dependents)
      expected_dependencies;
    List.iter expected_state_after_update ~f:assert_state
  in
  (* Removes source without replacing it, triggers dependency *)
  assert_update
    ~repopulate_source_to:None
    ~expected_state_in_update:["source.Foo", false]
    ~expected_state_after_update:["source.Foo", false]
    ~expected_dependencies:[dependency_A];

  (* Re-adds source, triggers dependency *)
  assert_update
    ~repopulate_source_to:(Some "class Foo: ...")
    ~expected_state_in_update:["source.Foo", false]
    ~expected_state_after_update:["source.Foo", true]
    ~expected_dependencies:[dependency_A];

  (* Removes source, but replaces it exactly, does not trigger dependency *)
  assert_update
    ~repopulate_source_to:(Some "class Foo: ...")
    ~expected_state_in_update:["source.Foo", false]
    ~expected_state_after_update:["source.Foo", true]
    ~expected_dependencies:[];

  (* Removes source, but replaced it with something new, triggers dependency *)
  assert_update
    ~repopulate_source_to:(Some {|
      class Foo:
        x: int
      |})
    ~expected_state_in_update:["source.Foo", false]
    ~expected_state_after_update:["source.Foo", true]
    ~expected_dependencies:[dependency_A];

  (* Irrelevant update, does not trigger dependencies *)
  assert_update
    ~repopulate_source_to:
      (Some {|
      class Foo:
        x: int
      class Irrelevant:
        x: str
      |})
    ~expected_state_in_update:["source.Foo", false; "source.Irrelevant", false]
    ~expected_state_after_update:["source.Foo", true; "source.Irrelevant", true]
    ~expected_dependencies:[];
  ()


let () =
  "environment"
  >::: [ "connect_type_order" >:: test_connect_type_order;
         "join_type_order" >:: test_join_type_order;
         "less_or_equal_type_order" >:: test_less_or_equal_type_order;
         "meet_type_order" >:: test_meet_type_order;
         "supertypes_type_order" >:: test_supertypes_type_order;
         "class_definition" >:: test_class_definition;
         "connect_definition" >:: test_connect_definition;
         "import_dependencies" >:: test_import_dependencies;
         "modules" >:: test_modules;
         "populate" >:: test_populate;
         "purge" >:: test_purge;
         "register_class_metadata" >:: test_register_class_metadata;
         "register_aliases" >:: test_register_aliases;
         "register_class_definitions" >:: test_register_class_definitions;
         "register_dependencies" >:: test_register_dependencies;
         "register_globals" >:: test_register_globals;
         "register_implicit_submodules" >:: test_register_implicit_submodules;
         "propagate_nested_classes" >:: test_propagate_nested_classes;
         "default_class_hierarchy" >:: test_default_class_hierarchy;
         "connect_to_top" >:: test_connect_annotations_to_top;
         "deduplicate" >:: test_deduplicate;
         "remove_extra" >:: test_remove_extra_edges_to_object;
         "purge_hierarchy" >:: test_purge_hierarchy;
         "update_and_compute_dependencies" >:: test_update_and_compute_dependencies ]
  |> Test.run
