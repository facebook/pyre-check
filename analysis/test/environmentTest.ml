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

let configuration = Configuration.Analysis.create ~infer:true ()

let create_environment ?(include_helpers = false) ?(additional_sources = []) () =
  let sources = typeshed_stubs ~include_helper_builtins:include_helpers () @ additional_sources in
  Test.environment ~configuration ~sources ()


let populate_with_sources ?include_helpers sources =
  create_environment ?include_helpers ~additional_sources:sources ()


let populate ?handle ?include_helpers source =
  populate_with_sources ?include_helpers [parse ?handle source]


let populate_preprocess ?handle source =
  populate_with_sources [source |> parse ?handle |> Preprocessing.preprocess]


let order_and_environment source =
  let environment = populate source in
  ( {
      TypeOrder.handler = Environment.class_hierarchy environment;
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


let test_register_class_definitions _ =
  let environment = create_environment () in
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
  let order = Environment.class_hierarchy environment in
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


let test_register_class_metadata _ =
  let environment = create_environment ~include_helpers:false () in
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
  let order = Environment.class_hierarchy environment in
  ClassHierarchy.deduplicate order ~annotations:all_annotations;
  ClassHierarchy.connect_annotations_to_object order all_annotations;
  ClassHierarchy.remove_extra_edges_to_object order all_annotations;
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


let test_register_aliases _ =
  let register_all sources =
    let sources = List.map sources ~f:Preprocessing.preprocess in
    populate_with_sources sources
  in
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
  assert_resolved
    [ parse
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
          Twiddledee, Twiddledum = C, C
        |}
    ]
    ["C", "C"; "D", "D"; "B", "D"; "A", "D"; "Twiddledee", "Twiddledee"; "Twiddledum", "Twiddledum"];
  assert_resolved
    [ parse
        ~handle:"qualifier.py"
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
        |}
    ]
    [ "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D" ];
  assert_resolved [parse "X = None"] ["X", "None"];
  assert_resolved
    [ parse
        ~handle:"collections.py"
        {|
          from typing import Iterator as TypingIterator
          from typing import Iterable
        |}
    ]
    [ "collections.TypingIterator", "typing.Iterator[typing.Any]";
      "collections.Iterable", "typing.Iterable[typing.Any]" ];

  (* Handle builtins correctly. *)
  assert_resolved
    [ parse
        ~handle:"collections.py"
        {|
          from builtins import int
          from builtins import dict as CDict
        |}
    ]
    ["collections.int", "int"; "collections.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [ parse
        ~handle:"collections.py"
        {|
          from future.builtins import int
          from future.builtins import dict as CDict
        |}
    ]
    ["collections.int", "int"; "collections.CDict", "typing.Dict[typing.Any, typing.Any]"];
  assert_resolved
    [ parse
        ~handle:"asyncio/tasks.py"
        {|
           from typing import TypeVar, Generic, Union
           _T = typing.TypeVar('_T')
           class Future(Generic[_T]): ...
           class Awaitable(Generic[_T]): ...
           _FutureT = Union[Future[_T], Awaitable[_T]]
        |}
    ]
    [ "asyncio.tasks.Future[int]", "asyncio.tasks.Future[int]";
      ( "asyncio.tasks._FutureT[int]",
        "typing.Union[asyncio.tasks.Awaitable[int], asyncio.tasks.Future[int]]" ) ];
  assert_resolved
    [ parse
        ~handle:"a.py"
        {|
          import typing
          _T = typing.TypeVar("_T")
          _T2 = typing.TypeVar("UnrelatedName")
        |}
    ]
    ["a._T", "Variable[a._T]"; "a._T2", "Variable[a._T2]"];

  (* Type variable aliases in classes. *)
  assert_resolved
    [ parse
        ~handle:"qualifier.py"
        {|
          class Class:
            T = typing.TypeVar('T')
            Int = int
        |}
    ]
    ["qualifier.Class.T", "Variable[qualifier.Class.T]"; "qualifier.Class.Int", "int"];

  (* Stub-suppressed aliases show up as `Any`. *)
  assert_resolved
    [ parse ~local_mode:Source.PlaceholderStub ~handle:"stubbed.pyi" "";
      parse
        ~handle:"qualifier.py"
        {|
          class str: ...
          T = stubbed.Something
          Q = typing.Union[stubbed.Something, str]
        |}
    ]
    ["qualifier.T", "typing.Any"; "qualifier.Q", "typing.Union[typing.Any, qualifier.str]"];
  assert_resolved
    [ parse
        ~handle:"t.py"
        {|
          import x
          X = typing.Dict[int, int]
          T = typing.Dict[int, X]
          C = typing.Callable[[T], int]
        |};
      parse
        ~handle:"x.py"
        {|
          import t
          X = typing.Dict[int, int]
          T = typing.Dict[int, t.X]
          C = typing.Callable[[T], int]
        |}
    ]
    [ "x.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
      "t.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]" ];
  assert_resolved
    [ parse ~handle:"t.py" {|
          from typing import Dict
        |};
      parse ~handle:"x.py" {|
          from t import *
        |} ]
    ["x.Dict", "x.Dict"];
  assert_resolved
    [parse ~handle:"x.py" {|
          C = typing.Callable[[gurbage], gurbage]
        |}]
    ["x.C", "x.C"];
  assert_resolved
    [ parse
        {|
          A = int
          B: typing.Type[int] = int
          C: typing.Type[int]
          D: typing.Any
          E = ...
          F = A
          G = 1
        |}
    ]
    ["A", "int"; "B", "B"; "C", "C"; "D", "typing.Any"; "E", "E"; "F", "int"; "G", "G"];
  assert_resolved
    [ parse ~handle:"a.py" {|
          class Foo: ...
        |};
      parse ~handle:"b.py" {|
          import a
        |} ]
    ["b.a.Foo", "a.Foo"];
  assert_resolved
    [ parse ~handle:"a.py" {|
          class Foo: ...
        |};
      parse
        ~handle:"b.py"
        {|
          from a import Foo
          class Foo:
            ...
        |} ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [ parse ~handle:"a.py" {|
          class Bar: ...
        |};
      parse
        ~handle:"b.py"
        {|
          from a import Bar as Foo
          class Foo:
            ...
        |} ]
    ["b.Foo", "b.Foo"];
  assert_resolved
    [ parse ~handle:"a.py" {|
          class Foo: ...
        |};
      parse
        ~handle:"b.py"
        {|
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
    [ parse
        {|
          Tparams = pyre_extensions.ParameterSpecification('Tparams')
          Ts = pyre_extensions.ListVariadic('Ts')
      |}
    ]
    [ ( "Tparams",
        Type.VariableAlias
          (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "Tparams")) );
      ( "Ts",
        Type.VariableAlias (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "Ts")) )
    ];
  ()


let test_register_implicit_submodules _ =
  let environment = create_environment () in
  Environment.register_implicit_submodules environment (Reference.create "a.b.c");
  let global_resolution = Environment.resolution environment () in
  assert_equal
    None
    (GlobalResolution.module_definition global_resolution (Reference.create "a.b.c"));
  assert_true (Environment.is_module environment (Reference.create "a.b"));
  assert_true (Environment.is_module environment (Reference.create "a"))


let test_connect_definition _ =
  let environment = create_environment () in
  let resolution = Environment.resolution environment () in
  let (module TypeOrderHandler : ClassHierarchy.Handler) =
    Environment.class_hierarchy environment
  in
  let (module DependencyHandler) = Environment.dependency_handler environment in
  DependencyHandler.add_class_key ~qualifier:(Reference.create "") "C";
  ClassHierarchy.insert (module TypeOrderHandler) "C";
  DependencyHandler.add_class_key ~qualifier:(Reference.create "") "D";
  ClassHierarchy.insert (module TypeOrderHandler) "D";
  let assert_edge ~predecessor ~successor =
    let predecessor_index =
      TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) predecessor
    in
    let successor_index = TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) successor in
    assert_true
      (List.mem
         ~equal:ClassHierarchy.Target.equal
         (TypeOrderHandler.find_unsafe (TypeOrderHandler.edges ()) predecessor_index)
         { ClassHierarchy.Target.target = successor_index; parameters = Concrete [] });
    assert_true
      (ClassHierarchy.Target.Set.mem
         (TypeOrderHandler.find_unsafe (TypeOrderHandler.backedges ()) successor_index)
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


let test_register_globals _ =
  let environment = create_environment () in
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


let test_connect_type_order _ =
  let environment = create_environment ~include_helpers:false () in
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
  let order = Environment.class_hierarchy environment in
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
  ClassHierarchy.connect_annotations_to_object order all_annotations;
  assert_successors "C" ["object"];
  assert_successors "D" ["C"; "object"];
  assert_successors "CallMe" ["typing.Callable"; "object"]


let test_populate _ =
  (* Test type resolution. *)
  let environment = populate {|
      class foo.foo(): ...
      class bar(): ...
    |} in
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
      {|
      class str: ...
      _T = typing.TypeVar('_T')
      S = str
      S2 = S
    |}
  in
  assert_equal (parse_annotation environment !"_T") (Type.variable "_T");
  assert_equal (parse_annotation environment !"S") Type.string;
  assert_equal (parse_annotation environment !"S2") Type.string;
  let assert_superclasses
      ?(superclass_parameters = fun _ -> Type.OrderedTypes.Concrete [])
      ~environment
      base
      ~superclasses
    =
    let (module TypeOrderHandler) = Environment.class_hierarchy environment in
    let index annotation = TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) annotation in
    let targets = TypeOrderHandler.find (TypeOrderHandler.edges ()) (index base) in
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
            let target = TypeOrderHandler.find_unsafe (TypeOrderHandler.annotations ()) target in
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
      ~include_helpers:false
      {|
        class abc.ABCMeta: ...
        class C(metaclass=abc.ABCMeta): ...
      |}
  in
  assert_superclasses ~environment "C" ~superclasses:["object"];

  (* Ensure object is a superclass if a class only has unsupported bases. *)
  let environment =
    populate
      ~include_helpers:false
      {|
        def foo() -> int:
          return 1
        class C(foo()):
          pass
      |}
  in
  assert_superclasses ~environment "C" ~superclasses:["object"];

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
    |> populate_preprocess ~handle:"test.py"
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
    |> populate
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
    {|
      class Class:
        @property
        def Class.property(self) -> int: ...
    |}
    |> populate
    |> assert_global_with_environment
  in
  assert_global
    "Class.property"
    (Annotation.create_immutable
       ~global:true
       (Type.Callable.create
          ~name:!&"Class.property"
          ~parameters:
            (Type.Callable.Defined
               [ Type.Callable.Parameter.Named
                   { name = "self"; annotation = Type.Top; default = false } ])
          ~annotation:Type.integer
          ()));

  (* Loops. *)
  ( try populate {|
        def foo(cls):
          class cls(cls): pass
      |} |> ignore with
  | ClassHierarchy.Cyclic -> assert_unreached () );

  (* Check meta variables are registered. *)
  let assert_global =
    {|
      class A:
        pass
    |} |> populate |> assert_global_with_environment
  in
  assert_global
    "A"
    ( Type.Primitive "A"
    |> Type.meta
    |> Annotation.create_immutable ~global:true ~original:(Some Type.Top) );

  (* Callable classes. *)
  let environment =
    populate
      {|
      class CallMe:
        def CallMe.__call__(self, x: int) -> str:
          pass
      class AlsoCallable(CallMe):
        pass
  |}
  in
  let type_parameters annotation =
    match annotation with
    | "typing.Callable" ->
        Type.OrderedTypes.Concrete
          [ parse_single_expression "typing.Callable('CallMe.__call__')[[Named(x, int)], str]"
            |> Type.create ~aliases:(fun _ -> None) ]
    | _ -> Type.OrderedTypes.Concrete []
  in
  assert_superclasses
    ~superclass_parameters:type_parameters
    ~environment
    "CallMe"
    ~superclasses:["typing.Callable"];
  ();
  let environment = populate {|
      def foo(x: int) -> str: ...
    |} in
  let global_resolution = Environment.resolution environment () in
  assert_equal
    (GlobalResolution.undecorated_signature global_resolution (Reference.create "foo"))
    (Some
       {
         Type.Callable.annotation = Type.string;
         parameters =
           Type.Callable.Defined
             [ Type.Callable.Parameter.Named
                 { annotation = Type.integer; name = "x"; default = false } ];
         define_location = None;
       })


let test_less_or_equal_type_order _ =
  let order, environment =
    order_and_environment
      {|
      class module.super(): ...
      class module.sub(module.super): ...
    |}
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
      {|
        class module.sub(module.super): pass
        class module.super(module.top): pass
        class module.top(): pass
    |}
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
      {|
      class A: ...
      class B(A): ...
      class C(typing.Optional[A]): ...
    |}
  in
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Optional (Type.Primitive "A")));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Optional (Type.Primitive "A")));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Primitive "A"));

  (* We're currently not sound with inheritance and optionals. *)
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Primitive "C"));
  assert_false
    (TypeOrder.always_less_or_equal order ~left:(Type.Primitive "A") ~right:(Type.Primitive "C"));

  (* Unions. *)
  let order, _ =
    order_and_environment
      {|
      class A: ...
      class B(A): ...
      class int(): ...
      class float(): ...
    |}
  in
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"])
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "B"])
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"])
       ~right:(Type.Union [Type.Primitive "B"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "B")
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Union [Type.Primitive "A"; Type.Primitive "B"]));
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:Type.Any);
  assert_true
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:(Type.Union [Type.Top; Type.Any; Type.Optional Type.float]));
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:Type.float);
  assert_false
    (TypeOrder.always_less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:(Type.Union [Type.float; Type.Primitive "B"; Type.integer]));

  (* Special cases. *)
  assert_true (TypeOrder.always_less_or_equal order ~left:Type.integer ~right:Type.float)


let test_join_type_order _ =
  let order, _ =
    order_and_environment {|
      class foo(): ...
      class bar(L[T]): ...
    |}
  in
  let foo = Type.Primitive "foo" in
  let bar = Type.Primitive "bar" in
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
  assert_raises (ClassHierarchy.Untracked (Type.Primitive "durp")) (fun _ ->
      TypeOrder.join order bar (Type.Primitive "durp"));

  (* Special cases. *)
  assert_equal (TypeOrder.join order Type.integer Type.float) Type.float


let test_meet_type_order _ =
  let order, _ =
    order_and_environment
      {|
      class foo(): ...
      class bar(L[T]): ...
      class A: ...
      class B(A): ...
      class C(A): ...
      class D(B,C): ...
    |}
  in
  let assert_meet left right expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:(Format.asprintf "%a" Type.pp)
      ~pp_diff:(diff ~print:Type.pp)
      (TypeOrder.meet order left right)
      expected
  in
  let foo = Type.Primitive "foo" in
  let bar = Type.Primitive "bar" in
  let a = Type.Primitive "A" in
  let b = Type.Primitive "B" in
  let c = Type.Primitive "C" in
  let d = Type.Primitive "D" in
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


let test_supertypes_type_order _ =
  let environment = populate {|
      class foo(): pass
      class bar(foo): pass
    |} in
  let order = Environment.class_hierarchy environment in
  assert_equal ["object"] (ClassHierarchy.successors order "foo");
  assert_equal ["foo"; "object"] (ClassHierarchy.successors order "bar")


let test_class_definition _ =
  let is_defined environment annotation =
    class_definition environment annotation |> Option.is_some
  in
  let environment =
    populate {|
      class baz.baz(): pass
      class object():
        pass
    |}
  in
  assert_true (is_defined environment (Type.Primitive "baz.baz"));
  assert_true (is_defined environment (Type.parametric "baz.baz" (Concrete [Type.integer])));
  assert_is_some (class_definition environment (Type.Primitive "baz.baz"));
  assert_false (is_defined environment (Type.Primitive "bar.bar"));
  assert_false (is_defined environment (Type.parametric "bar.bar" (Concrete [Type.integer])));
  assert_is_none (class_definition environment (Type.Primitive "bar.bar"));
  let any = class_definition environment Type.object_primitive |> value |> Node.value in
  assert_equal any.Class.name !&"object"


let test_modules _ =
  let environment =
    populate_with_sources
      [ Source.create ~relative:"wingus.py" [];
        Source.create ~relative:"dingus.py" [];
        Source.create ~relative:"os/path.py" [] ]
  in
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
  let create_files_and_test _ =
    Out_channel.create "test.py" |> Out_channel.close;
    Out_channel.create "a.py" |> Out_channel.close;
    Out_channel.create "ignored.py" |> Out_channel.close;
    Unix.handle_unix_error (fun () -> Unix.mkdir_p "subdirectory");
    Out_channel.create "subdirectory/b.py" |> Out_channel.close;
    let source =
      {|
         import a
         from builtins import str
         from subdirectory.b import c
         import sys
         from . import ignored
      |}
    in
    let environment =
      populate_with_sources
        [ parse ~handle:"test.py" source;
          parse ~handle:"a.py" "";
          parse ~handle:"subdirectory/b.py" "";
          parse ~handle:"builtins.pyi" "" ]
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
  in
  with_bracket_chdir context (bracket_tmpdir context) create_files_and_test


let test_register_dependencies _ =
  let environment = create_environment () in
  let source =
    {|
         import a # a is added here
         from subdirectory.b import c # subdirectory.b is added here
         from . import ignored # no dependency created here
      |}
  in
  Environment.purge environment [Reference.create "test"; Reference.create "b"];
  Environment.register_dependencies environment (parse ~handle:"test.py" source);
  let dependencies qualifier =
    Environment.dependencies environment !&qualifier
    >>| String.Set.Tree.map ~f:Reference.show
    >>| String.Set.Tree.to_list
  in
  assert_equal (dependencies "subdirectory.b") (Some ["test"]);
  assert_equal (dependencies "a") (Some ["test"])


let test_purge _ =
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
    Test.environment
      ~configuration
      ~sources:[parse ~handle:"test.py" source |> Preprocessing.preprocess]
      ()
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
  let class_hierarchy = Environment.class_hierarchy handler in
  ClassHierarchy.check_integrity class_hierarchy;
  Environment.purge handler [Reference.create "test"];
  assert_is_none (GlobalResolution.class_definition global_resolution (Primitive "test.baz"));
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "test.P"));
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "test.baz"));
  assert_is_none (GlobalResolution.aliases global_resolution "test._T");
  assert_false (GlobalResolution.is_tracked global_resolution "test.P");
  assert_equal (dependencies "a.py") (Some []);
  ClassHierarchy.check_integrity class_hierarchy;
  ()


let test_propagate_nested_classes _ =
  let test_propagate sources aliases =
    Type.Cache.disable ();
    Type.Cache.enable ();
    let sources = List.map sources ~f:(fun source -> source |> Preprocessing.preprocess) in
    let handler = populate_with_sources ~include_helpers:false sources in
    let assert_alias (alias, target) =
      parse_single_expression alias
      |> parse_annotation handler
      |> Type.show
      |> assert_equal ~printer:(fun string -> string) target
    in
    List.iter aliases ~f:assert_alias
  in
  test_propagate
    [ parse
        {|
          class B:
            class N:
              pass
          class C(B):
            pass
        |}
    ]
    ["C.N", "B.N"];
  test_propagate
    [ parse
        {|
          class B:
            class N:
              pass
          class C(B):
            class N:
              pass
        |}
    ]
    ["C.N", "C.N"];
  test_propagate
    [ parse
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
    ]
    ["C.N", "B1.N"];
  test_propagate
    [ parse
        ~handle:"qual.py"
        {|
          class B:
            class N:
              pass
        |};
      parse
        ~handle:"importer.py"
        {|
          from qual import B
          class C(B):
            pass
        |} ]
    ["importer.C.N", "qual.B.N"];
  test_propagate
    [ parse
        {|
          class B:
            class N:
              class NN:
                class NNN:
                  pass
          class C(B):
            pass
        |}
    ]
    ["C.N.NN.NNN", "B.N.NN.NNN"];
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
         "propagate_nested_classes" >:: test_propagate_nested_classes ]
  |> Test.run
