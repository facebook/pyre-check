(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open Statement

open Test


let access names =
  List.concat_map names ~f:Access.create


let value option =
  Option.value_exn option


let configuration = Configuration.Analysis.create ~infer:true ()


let create_environment ?(include_helpers = false) () =
  let environment = Environment.Builder.create () in
  Test.populate
    ~configuration
    (Environment.handler environment)
    (typeshed_stubs ~include_helper_builtins:include_helpers ());
  environment


let plain_populate ~environment sources =
  let handler = Environment.handler environment in
  Test.populate ~configuration handler sources;
  environment


let populate_with_sources ?(environment = create_environment ()) sources =
  plain_populate ~environment sources
  |> Environment.handler


let populate ?(environment = create_environment ()) ?handle ?qualifier source =
  populate_with_sources ~environment [parse ~convert:true ?handle ?qualifier source]


let populate_preprocess ?(environment = create_environment ()) ?handle ?qualifier source =
  populate_with_sources
    ~environment
    [
      source
      |> parse ?handle ?qualifier
      |> Preprocessing.preprocess
    ]


let order_and_environment source =
  let environment = populate source in
  let module Handler = (val environment) in
  {
    TypeOrder.handler = (module Handler.TypeOrderHandler : TypeOrder.Handler);
    constructor = (fun _ -> None);
    implements = (fun ~protocol:_ _ -> TypeOrder.DoesNotImplement);
    any_is_bottom = false;
  },
  environment


let global environment =
  TypeCheck.resolution environment ()
  |> Resolution.global


let class_definition environment =
  TypeCheck.resolution environment ()
  |> Resolution.class_definition


let parse_annotation environment =
  (* Allow untracked because we're not calling all of populate *)
  TypeCheck.resolution environment ()
  |> Resolution.parse_annotation ~allow_untracked:true


let create_location path start_line start_column end_line end_column =
  let start = { Location.line = start_line; column = start_column } in
  let stop = { Location.line = end_line; column = end_column } in
  { Location.path; start; stop; }


let test_register_class_definitions _ =
  let (module Handler: Environment.Handler) = Environment.handler (create_environment ()) in
  Environment.register_class_definitions
    (module Handler)
    (parse
       ~convert:true
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
  assert_equal (parse_annotation (module Handler) (!"C")) (Type.Primitive "C");
  assert_equal (parse_annotation (module Handler) (!"D")) (Type.Primitive "D");
  assert_equal (parse_annotation (module Handler) (!"B")) (Type.Primitive "B");
  assert_equal (parse_annotation (module Handler) (!"A")) (Type.Primitive "A");
  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  assert_equal (TypeOrder.successors order (Type.Primitive "C")) [];
  assert_equal (TypeOrder.predecessors order (Type.Primitive "C")) [];

  (* Annotations for classes are returned even if they already exist in the handler. *)
  let new_annotations =
    Environment.register_class_definitions
      (module Handler)
      (parse
         ~convert:true
         {|
         class C:
           ...
       |})
  in
  assert_equal
    ~cmp:Type.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ (Type.show next)))
    (Type.Set.singleton (Type.Primitive "C"))
    new_annotations;
  let new_annotations =
    Environment.register_class_definitions
      (module Handler)
      (parse ~convert:true "class int: pass")
  in
  assert_equal
    ~cmp:Type.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ (Type.show next)))
    (Type.Set.singleton (Type.integer))
    new_annotations


let test_register_class_metadata _ =
  let (module Handler: Environment.Handler) =
    Environment.handler (create_environment ~include_helpers:false ())
  in
  let source =
    parse
      ~convert:true
      {|
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
      |}
  in
  let all_annotations =
    Environment.register_class_definitions (module Handler) source
    |> Set.to_list
  in
  let resolution = TypeCheck.resolution (module Handler) () in
  Environment.connect_type_order (module Handler) resolution source;
  TypeOrder.deduplicate (module Handler.TypeOrderHandler) ~annotations:all_annotations;
  TypeOrder.connect_annotations_to_top
    (module Handler.TypeOrderHandler)
    ~top:Type.Any
    all_annotations;
  TypeOrder.remove_extra_edges
    (module Handler.TypeOrderHandler)
    ~bottom:Type.Bottom
    ~top:Type.Any
    all_annotations;

  Handler.register_class_metadata "A";
  Handler.register_class_metadata "B";
  Handler.register_class_metadata "C";
  Handler.register_class_metadata "D";
  Handler.register_class_metadata "E";

  let assert_successors class_name expected =
    let { Resolution.successors; _ } =
      Option.value_exn (Handler.class_metadata class_name)
    in
    let expected =
      List.map expected ~f:(fun annotation -> Type.Primitive annotation)
      |> (fun expected -> expected @ [Type.Any; Type.Top])
    in
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ (Type.show next) ^ " "))
      ~cmp:(List.equal ~equal:Type.equal)
      expected
      successors
  in
  assert_successors "C" [];
  assert_successors "D" ["C"];
  assert_successors "B" ["A"];
  assert_successors "E" ["D"; "C"; "A"]


let test_register_aliases _ =
  let assert_resolved sources aliases =
    let (module Handler) =
      let (module Handler: Environment.Handler) =
        Environment.handler (create_environment ())
      in
      let sources = List.map sources ~f:Preprocessing.preprocess in
      let register
          ({
            Source.handle;
            qualifier;
            statements;
            metadata = { Source.Metadata.local_mode; _ };
            _;
          } as source) =
        let stub = String.is_suffix (File.Handle.show handle) ~suffix:".pyi" in
        Handler.register_module
          ~qualifier
          ~local_mode
          ~handle:(Some handle)
          ~stub
          ~statements;
        Environment.register_class_definitions (module Handler) source |> ignore;
      in
      List.iter sources ~f:register;
      Environment.register_aliases (module Handler) sources;
      (module Handler: Environment.Handler)
    in
    let assert_alias (alias, target) =
      let parse_annotation handler source =
        parse_single_expression source
        |> parse_annotation handler
      in
      assert_equal
        ~printer:(fun string -> string)
        target
        (Type.show (parse_annotation (module Handler) alias))
    in
    List.iter aliases ~f:assert_alias
  in

  assert_resolved
    [
      parse
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
          Twiddledee, Twiddledum = C, C
        |};
    ]
    [
      "C", "C";
      "D", "D";
      "B", "D";
      "A", "D";
      "Twiddledee", "Twiddledee";
      "Twiddledum", "Twiddledum";
    ];
  assert_resolved
    [
      parse
        ~qualifier:(!&"qualifier")
        {|
          class C: ...
          class D(C): pass
          B = D
          A = B
        |};
    ]
    [
      "qualifier.C", "qualifier.C";
      "qualifier.D", "qualifier.D";
      "qualifier.B", "qualifier.D";
      "qualifier.A", "qualifier.D";
    ];

  assert_resolved [parse "X = None"] ["X", "None"];

  assert_resolved
    [
      parse
        ~qualifier:(!&"collections")
        {|
          from typing import Iterator as TypingIterator
          from typing import Iterable
        |};
    ]
    [
      "collections.TypingIterator", "typing.Iterator[typing.Any]";
      "collections.Iterable", "typing.Iterable[typing.Any]";
    ];

  (* Handle builtins correctly. *)
  assert_resolved
    [
      parse
        ~qualifier:(!&"collections")
        {|
          from builtins import int
          from builtins import dict as CDict
        |};
    ]
    [
      "collections.int", "int";
      "collections.CDict", "typing.Dict[typing.Any, typing.Any]";
    ];
  assert_resolved
    [
      parse
        ~qualifier:(!&"collections")
        {|
          from future.builtins import int
          from future.builtins import dict as CDict
        |};
    ]
    [
      "collections.int", "int";
      "collections.CDict", "typing.Dict[typing.Any, typing.Any]";
    ];

  assert_resolved
    [
      parse
        ~qualifier:(!&"asyncio.tasks")
        {|
           from typing import TypeVar, Generic, Union
           _T = typing.TypeVar('_T')
           class Future(Generic[_T]): ...
           class Awaitable(Generic[_T]): ...
           _FutureT = Union[Future[_T], Awaitable[_T]]
        |};
    ]
    [
      "asyncio.tasks.Future[int]", "asyncio.tasks.Future[int]";
      "asyncio.tasks._FutureT[int]",
      "typing.Union[asyncio.tasks.Awaitable[int], asyncio.tasks.Future[int]]";
    ];

  assert_resolved
    [
      parse
        ~qualifier:(!&"a")
        {|
          import typing
          _T = typing.TypeVar("_T")
          _T2 = typing.TypeVar("UnrelatedName")
        |}
    ]
    [
      "a._T", "Variable[a._T]";
      "a._T2", "Variable[a._T2]";
    ];

  (* Type variable aliases in classes. *)
  assert_resolved
    [
      parse
        ~qualifier:(!&"qualifier")
        {|
          class Class:
            T = typing.TypeVar('T')
            Int = int
        |};
    ]
    [
      "qualifier.Class.T", "Variable[qualifier.Class.T]";
      "qualifier.Class.Int", "int";
    ];

  (* Stub-suppressed aliases show up as `Any`. *)
  assert_resolved
    [
      parse
        ~qualifier:(!&"stubbed")
        ~local_mode:Source.PlaceholderStub
        ~handle:"stubbed.pyi"
        "";
      parse
        ~qualifier:(!&"qualifier")
        {|
          class str: ...
          T = stubbed.Something
          Q = typing.Union[stubbed.Something, str]
        |};
    ]
    ["qualifier.T", "typing.Any"; "qualifier.Q", "typing.Any"];

  assert_resolved
    [
      parse
        ~qualifier:(!&"t")
        {|
          import x
          X = typing.Dict[int, int]
          T = typing.Dict[int, X]
          C = typing.Callable[[T], int]
        |};
      parse
        ~qualifier:(!&"x")
        {|
          import t
          X = typing.Dict[int, int]
          T = typing.Dict[int, t.X]
          C = typing.Callable[[T], int]
        |};
    ]
    [
      "x.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
      "t.C", "typing.Callable[[typing.Dict[int, typing.Dict[int, int]]], int]";
    ];
  assert_resolved
    [
      parse
        ~qualifier:(!&"t")
        {|
          from typing import Dict
        |};
      parse
        ~qualifier:(!&"x")
        {|
          from t import *
        |};
    ]
    [
      "x.Dict", "x.Dict";
    ];
  assert_resolved
    [
      parse
        ~qualifier:(!&"x")
        {|
          C = typing.Callable[[gurbage], gurbage]
        |};
    ]
    ["x.C", "x.C"];
  assert_resolved
    [
      parse
        {|
          A = int
          B: typing.Type[int] = int
          C: typing.Type[int]
          D: typing.Any
          E = ...
          F = A
          G = 1
        |};
    ]
    [
      "A", "int";
      "B", "B";
      "C", "C";
      "D", "typing.Any";
      "E", "E";
      "F", "int";
      "G", "G";
    ];
  ()


let test_connect_definition _ =
  let (module Handler: Environment.Handler) = Environment.handler (create_environment ()) in
  let resolution = TypeCheck.resolution (module Handler) () in

  let (module TypeOrderHandler: TypeOrder.Handler) = (module Handler.TypeOrderHandler) in
  TypeOrder.insert (module TypeOrderHandler) (Type.Primitive "C");
  TypeOrder.insert (module TypeOrderHandler) (Type.Primitive "D");

  let assert_edge ~predecessor ~successor =
    let predecessor_index =
      TypeOrderHandler.find_unsafe
        (TypeOrderHandler.indices ())
        predecessor
    in
    let successor_index =
      TypeOrderHandler.find_unsafe
        (TypeOrderHandler.indices ())
        successor
    in
    assert_true
      (List.mem
         ~equal:TypeOrder.Target.equal
         (TypeOrderHandler.find_unsafe
            (TypeOrderHandler.edges ())
            predecessor_index)
         { TypeOrder.Target.target = successor_index; parameters = [] });

    assert_true
      (List.mem
         ~equal:TypeOrder.Target.equal
         (TypeOrderHandler.find_unsafe
            (TypeOrderHandler.backedges ())
            successor_index)
         { TypeOrder.Target.target = predecessor_index; parameters = [] })
  in
  let class_definition =
    +{
      Class.name = !&"C";
      bases = [];
      body = [];
      decorators = [];
      docstring = None
    }
  in
  Environment.connect_definition ~resolution ~definition:class_definition;
  assert_edge ~predecessor:Type.Bottom ~successor:(Type.Primitive "C");

  let definition =
    +(Test.parse_single_class {|
       class D(int, float):
         ...
     |})
  in
  Environment.connect_definition ~resolution ~definition;
  assert_edge ~predecessor:Type.Bottom ~successor:(Type.Primitive "D");
  assert_edge ~predecessor:(Type.Primitive "D") ~successor:Type.integer;
  assert_edge ~predecessor:(Type.Primitive "D") ~successor:Type.float


let test_register_globals _ =
  let (module Handler: Environment.Handler) = Environment.handler (create_environment ()) in
  let resolution = TypeCheck.resolution (module Handler) () in

  let assert_global reference expected =
    let actual =
      !&reference
      |> Handler.globals
      >>| Node.value
      >>| Annotation.annotation
    in
    assert_equal
      ~printer:(function | Some annotation -> Type.show annotation | _ -> "(none)")
      ~cmp:(Option.equal Type.equal)
      expected
      actual
  in

  let source =
    parse
      ~qualifier:(!&"qualifier")
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
  Environment.register_values (module Handler) resolution source;
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
      ~qualifier:(!&"test")
      {|
        class Class: ...
        alias = Class

        GLOBAL: Class = ...
        GLOBAL2: alias = ...
      |}
    |> Preprocessing.preprocess
  in
  Environment.register_values (module Handler) resolution source;
  assert_global "test.GLOBAL" (Some (Type.Primitive "test.Class"));
  assert_global "test.GLOBAL2" (Some (Type.Primitive "test.alias"))


let test_connect_type_order _ =
  let (module Handler: Environment.Handler) =
    Environment.handler (create_environment ~include_helpers:false ())
  in
  let resolution = TypeCheck.resolution (module Handler) () in
  let source =
    parse
      ~convert:true
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
  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  let all_annotations =
    Environment.register_class_definitions (module Handler) source
    |> Set.to_list
  in
  Environment.register_aliases (module Handler) [source];
  Environment.connect_type_order (module Handler) resolution source;
  let assert_successors annotation successors =
    assert_equal
      ~printer:(List.to_string ~f:Type.show)
      successors
      (TypeOrder.successors order annotation)
  in
  (* Classes get connected to object via `connect_annotations_to_top`. *)
  assert_successors (Type.Primitive "C") [];
  assert_successors (Type.Primitive "D") [Type.Primitive "C"];

  TypeOrder.connect_annotations_to_top order ~top:Type.Any all_annotations;

  assert_successors (Type.Primitive "C") [Type.Any; Type.Top];
  assert_successors (Type.Primitive "D") [Type.Primitive "C"; Type.Any; Type.Top];
  assert_successors
    (Type.Primitive "CallMe")
    [Type.Primitive "typing.Callable"; Type.object_primitive; Type.Any; Type.Top]


let test_populate _ =
  (* Test type resolution. *)
  let ((module Handler: Environment.Handler) as environment) =
    populate {|
      class foo.foo(): ...
      class bar(): ...
    |}
  in
  assert_equal
    (parse_annotation environment !"foo.foo")
    (Type.Primitive "foo.foo");
  assert_equal
    (parse_annotation
       environment
       (+Access (SimpleAccess (parse_single_access "Optional[foo.foo]"))))
    (Type.parametric "Optional" [Type.Primitive "foo.foo"]);
  assert_equal (parse_annotation environment !"bar") (Type.Primitive "bar");

  (* Check custom aliases. *)
  assert_equal
    (parse_annotation environment !"typing.DefaultDict")
    (Type.parametric "collections.defaultdict" [Type.Any; Type.Any]);

  (* Check custom class definitions. *)
  assert_is_some (Handler.class_definition "None");
  assert_is_some (Handler.class_definition "typing.Optional");

  (* Check type aliases. *)
  let environment =
    populate {|
      class str: ...
      _T = typing.TypeVar('_T')
      S = str
      S2 = S
    |}
  in
  assert_equal (parse_annotation environment !"_T") (Type.variable "_T");
  assert_equal (parse_annotation environment !"S") Type.string;
  assert_equal (parse_annotation environment !"S2") Type.string;

  let assert_superclasses ?(superclass_parameters = fun _ -> []) ~environment base ~superclasses =
    let (module Handler: Environment.Handler) = environment in
    let index annotation =
      Handler.TypeOrderHandler.find_unsafe
        (Handler.TypeOrderHandler.indices ())
        annotation
    in
    let targets =
      (Handler.TypeOrderHandler.find
         (Handler.TypeOrderHandler.edges ())
         (index (Type.Primitive base)))
    in
    let to_target annotation = {
      TypeOrder.Target.target = index annotation;
      parameters = superclass_parameters annotation;
    }
    in
    let show_targets = function
      | None ->
          ""
      | Some targets ->
          let show_target { TypeOrder.Target.target; parameters } =
            let index = Int.to_string target in
            let target =
              Handler.TypeOrderHandler.find_unsafe
                (Handler.TypeOrderHandler.annotations ())
                target
              |> Type.show
            in
            let parameters = List.to_string parameters ~f:Type.show in
            Format.sprintf "%s: %s%s" index target parameters
          in
          List.to_string targets ~f:show_target
    in
    assert_equal
      ~printer:show_targets
      ~cmp:(Option.equal (List.equal ~equal:TypeOrder.Target.equal))
      (Some (List.map superclasses ~f:to_target))
      targets
  in
  (* Metaclasses aren't superclasses. *)
  let environment =
    populate
      ~environment:(create_environment ~include_helpers:false ())
      {|
        class abc.ABCMeta: ...
        class C(metaclass=abc.ABCMeta): ...
      |}
  in
  assert_superclasses ~environment "C" ~superclasses:[Type.object_primitive];

  (* Ensure object is a superclass if a class only has unsupported bases. *)
  let environment =
    populate
      ~environment:(create_environment ~include_helpers:false ())
      {|
        def foo() -> int:
          return 1
        class C(foo()):
          pass
      |}
  in
  assert_superclasses ~environment "C" ~superclasses:[Type.object_primitive];

  (* Globals *)
  let assert_global_with_environment environment actual expected =
    assert_equal
      ~cmp:(Option.equal (Node.equal Annotation.equal))
      ~printer:(function | Some global -> Resolution.show_global global | None -> "None")
      (Some (Node.create_with_default_location expected))
      (global environment (!&actual))
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
    |> populate_preprocess ~handle:"test.py" ~qualifier:(!&"test")
    |> assert_global_with_environment
  in

  assert_global
    "test.A"
    (Annotation.create_immutable ~global:true (parse_annotation environment !"test.int"));
  assert_global
    "test.B"
    (Annotation.create_immutable ~global:true Type.integer);
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
  assert_global
    "global_value_set"
    (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_annotated" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_both" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_unknown" (Annotation.create_immutable ~global:true Type.Top);
  assert_global
    "function"
    (Annotation.create_immutable
       ~global:true
       (Type.Callable.create
          ~name:(!&"function")
          ~parameters:(Type.Callable.Defined [])
          ~annotation:Type.Top
          ()));
  assert_global
    "global_function"
    (Annotation.create_immutable
       ~global:true
       ~original:(Some Type.Top)
       Type.Top);
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
          ~name:(!&"Class.__init__")
          ~parameters:(Type.Callable.Defined [
              Type.Callable.Parameter.Named {
                Type.Callable.Parameter.name = "self";
                annotation = Type.Top;
                default = false;
              };
            ])
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
          ~name:(!&"Class.property")
          ~parameters:(Type.Callable.Defined [
              Type.Callable.Parameter.Named {
                Type.Callable.Parameter.name = "self";
                annotation = Type.Top;
                default = false;
              };
            ])
          ~annotation:Type.integer
          ()));

  (* Loops. *)
  begin
    try
      populate {|
        def foo(cls):
          class cls(cls): pass
      |}
      |> ignore
    with TypeOrder.Cyclic ->
      assert_unreached ()
  end;
  (* Check meta variables are registered. *)
  let assert_global =
    {|
      class A:
        pass
    |}
    |> populate
    |> assert_global_with_environment
  in
  assert_global
    "A"
    (Type.Primitive "A"
     |> Type.meta
     |> Annotation.create_immutable ~global:true ~original:(Some Type.Top));

  (* Callable classes. *)
  let environment = populate {|
      class CallMe:
        def CallMe.__call__(self, x: int) -> str:
          pass
      class AlsoCallable(CallMe):
        pass
  |}
  in
  let type_parameters annotation =
    match Type.show annotation with
    | "typing.Callable" ->
        [
          parse_single_expression "typing.Callable('CallMe.__call__')[[Named(x, int)], str]"
          |> Type.create ~aliases:(fun _ -> None)
        ]
    | _ ->
        []
  in
  assert_superclasses
    ~superclass_parameters:type_parameters
    ~environment
    "CallMe"
    ~superclasses:[Type.Primitive "typing.Callable"];
  ()


let test_infer_protocols_edges _ =
  let edges =
    let environment =
      populate_preprocess
        ~environment:(Environment.Builder.create ())
        {|
          class Empty(typing.Protocol):
            pass
          class Sized(typing.Protocol):
            def empty(self) -> bool: pass
            def len(self) -> int: pass
          class Supersized(typing.Protocol):
            def empty(self) -> bool: pass
            def len(self) -> int: pass
          class List():
            def empty(self) -> bool: pass
            def length(self) -> int: pass
          class Set():
            def empty(self) -> bool: pass
            def len(self) -> int: pass
          class Subset(Set): pass
          class AlmostSet():
            def empty(self, a) -> bool: pass
            def len(self) -> int: pass
          class object:
            def __hash__(self) -> int: ...
          class SuperObject(typing.Protocol):
              @abstractmethod
              def __hash__(self) -> int: ...
        |}
    in
    let resolution = TypeCheck.resolution environment () in

    let methods_to_implementing_classes =
      let add key values map =
        Map.set map ~key ~data:(List.map values ~f:Reference.create)
      in
      Identifier.Map.empty
      |> add "__hash__" ["object"; "SuperObject"]
      |> add "len" ["Sized"; "Supersized"; "Set"; "AlmostSet"]
      |> add "empty" ["Sized"; "Supersized"; "List"; "Set"; "AlmostSet"]
    in
    let implementing_classes ~method_name =
      Map.find methods_to_implementing_classes method_name
    in
    (* We infer implementations for protocols based on the type order. *)
    let empty_edges =
      Environment.infer_implementations
        environment
        resolution
        ~implementing_classes
        ~protocol:(Type.Primitive "Empty")
    in
    assert_equal 11 (Set.length empty_edges);
    Environment.infer_implementations
      environment
      resolution
      ~implementing_classes
      ~protocol:(Type.Primitive "Sized")
    |> Set.union
      (Environment.infer_implementations
         environment
         resolution
         ~implementing_classes
         ~protocol:(Type.Primitive "SuperObject"))
    |> Set.union empty_edges
  in
  let assert_edge_inferred source target parameters =
    assert_true (Set.mem edges { TypeOrder.Edge.source; target; parameters })
  in
  let assert_edge_not_inferred source target parameters =
    assert_false (Set.mem edges { TypeOrder.Edge.source; target; parameters })
  in

  assert_equal ~printer:Int.to_string (Set.length edges) 12;

  assert_edge_not_inferred (Type.Primitive "List") (Type.Primitive "Sized") [];
  assert_edge_inferred (Type.Primitive "Set") (Type.Primitive "Sized") [];
  assert_edge_not_inferred (Type.Primitive "AlmostSet") (Type.Primitive "Sized") [];
  assert_edge_not_inferred (Type.Any) (Type.Primitive "SuperObject") [];
  assert_edge_inferred (Type.Primitive "List") (Type.Primitive "Empty") [];
  assert_edge_not_inferred (Type.Any) (Type.Primitive "Empty") []


let test_less_or_equal_type_order _ =
  let order, environment  =
    order_and_environment {|
      class module.super(): ...
      class module.sub(module.super): ...
    |} in

  let super =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.super"]))) in
  assert_equal super (Type.Primitive "module.super");

  let sub =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.sub"]))) in
  assert_equal
    sub
    (Type.Primitive "module.sub");

  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:Type.Top);
  assert_true (TypeOrder.less_or_equal order ~left:super ~right:Type.Top);
  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:super);
  assert_false (TypeOrder.less_or_equal order ~left:super ~right:sub);

  let order, environment =
    order_and_environment  {|
        class module.sub(module.super): pass
        class module.super(module.top): pass
        class module.top(): pass
    |} in

  let super =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.super"]))) in
  assert_equal super (Type.Primitive "module.super");

  let sub =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.sub"]))) in
  let super =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.super"]))) in
  let top =
    parse_annotation
      environment
      (+Access (SimpleAccess (access ["module.top"]))) in
  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:super);
  assert_true (TypeOrder.less_or_equal order ~left:super ~right:top);

  (* Optionals. *)
  let order, _ =
    order_and_environment  {|
      class A: ...
      class B(A): ...
      class C(typing.Optional[A]): ...
    |} in
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Optional (Type.Primitive "A")));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Optional (Type.Primitive "A")));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Primitive "A"));

  (* We're currently not sound with inheritance and optionals. *)
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.Primitive "A"))
       ~right:(Type.Primitive "C"));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Primitive "C"));

  (* Unions. *)
  let order, _ =
    order_and_environment  {|
      class A: ...
      class B(A): ...
      class int(): ...
      class float(): ...
    |} in
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"])
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "B"])
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"])
       ~right:(Type.Union [Type.Primitive "B"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Primitive "B")
       ~right:(Type.Union [Type.Primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Primitive "A")
       ~right:(Type.Union [Type.Primitive "A"; Type.Primitive "B"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:Type.Any);
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:(Type.Union [Type.Top; Type.Any; Type.Optional Type.float]));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:Type.float);
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.Primitive "A"; Type.Primitive "B"; Type.integer])
       ~right:(Type.Union [Type.float; Type.Primitive "B"; Type.integer]));

  (* Special cases. *)
  assert_true (TypeOrder.less_or_equal order ~left:Type.integer ~right:Type.float)


let test_join_type_order _ =
  let order, _ =
    order_and_environment  {|
      class foo(): ...
      class bar(L[T]): ...
    |} in
  let foo = Type.Primitive "foo" in
  let bar = Type.Primitive "bar" in

  assert_equal
    (TypeOrder.join order Type.Bottom bar)
    bar;
  assert_equal
    (TypeOrder.join order Type.Top bar)
    Type.Top;
  assert_equal
    (TypeOrder.join order foo bar)
    (Type.union [foo; bar]);
  assert_equal
    (TypeOrder.join order Type.Any Type.Top)
    Type.Top;

  assert_equal
    (TypeOrder.join order Type.integer (Type.Union [Type.integer; Type.string]))
    (Type.Union [Type.integer; Type.string]);
  assert_equal
    (TypeOrder.join order (Type.Union [Type.integer; Type.string]) Type.integer)
    (Type.Union [Type.integer; Type.string]);

  assert_raises
    (TypeOrder.Untracked (Type.Primitive "durp"))
    (fun _ -> TypeOrder.join order bar (Type.Primitive "durp"));

  (* Special cases. *)
  assert_equal
    (TypeOrder.join order Type.integer Type.float)
    Type.float


let test_meet_type_order _ =
  let order, _ =
    order_and_environment  {|
      class foo(): ...
      class bar(L[T]): ...
      class A: ...
      class B(A): ...
      class C(A): ...
      class D(B,C): ...
    |} in
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
  let environment =
    (populate {|
      class foo(): pass
      class bar(foo): pass
    |}) in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_equal
    [Type.object_primitive; Type.Any; Type.Top]
    (TypeOrder.successors order (Type.Primitive "foo"));
  assert_equal
    [Type.Primitive "foo"; Type.object_primitive; Type.Any; Type.Top]
    (TypeOrder.successors order (Type.Primitive "bar"));

  let environment =
    populate {|
      _T = typing.TypeVar('_T')
      class typing.Iterable(typing.Generic[_T]): pass
      class typing.Iterator(typing.Generic[_T], typing.Iterable[_T]): pass
    |} in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_equal
    ~printer:(List.to_string ~f:Type.show)
    [
      Type.Parametric {
        name = "typing.Protocol";
        parameters = [Type.integer];
      };
      Type.Parametric {
        name = "typing.Generic";
        parameters = [Type.integer];
      };
      Type.object_primitive;
      Type.Any;
      Type.Top;
    ]
    (TypeOrder.successors order (Type.parametric "typing.Iterable" [Type.integer]))


let test_class_definition _ =
  let is_defined environment annotation =
    class_definition environment annotation
    |> Option.is_some
  in

  let environment =
    populate {|
      class baz.baz(): pass
      class object():
        pass
    |} in
  assert_true (is_defined environment (Type.Primitive "baz.baz"));
  assert_true (is_defined environment (Type.parametric "baz.baz" [Type.integer]));
  assert_is_some (class_definition environment (Type.Primitive "baz.baz"));

  assert_false (is_defined environment (Type.Primitive "bar.bar"));
  assert_false (is_defined environment (Type.parametric "bar.bar" [Type.integer]));
  assert_is_none (class_definition environment (Type.Primitive "bar.bar"));

  let any =
    class_definition environment Type.object_primitive
    |> value
    |> Node.value
  in
  assert_equal any.Class.name (!&"object")


let test_protocols _ =
  let environment =
    let environment = Environment.Builder.create () in
    Test.populate
      ~configuration
      (Environment.handler environment)
      ([]);
    populate
      ~environment
      {|
      class A(): ...
      class B(typing.Protocol): pass
      class C(): pass
      class D(metaclass=abc.ABCMeta): ...
      T = typing.TypeVar("T")
      class E(typing.Protocol[T]): pass
    |} in
  let module Handler = (val environment) in

  assert_equal
    ~cmp:(List.equal ~equal:Type.equal)
    ~printer:(fun protocols -> List.map protocols ~f:Type.show |> String.concat ~sep:",")
    (Handler.protocols ())
    ([Type.Primitive "B"; Type.Primitive "E"]);

  let environment = populate "" in
  let module Handler = (val environment) in
  assert_equal
    ~cmp:(List.equal ~equal:Type.equal)
    ~printer:(fun protocols -> List.map protocols ~f:Type.show |> String.concat ~sep:",")
    (Handler.protocols ())
    ([
      "typing.Sized";
      "typing.AsyncIterator";
      "typing.Hashable";
      "typing.AsyncIterable";
      "typing.Iterator";
      "typing._Collection";
      "typing.Iterable";
      "typing.AsyncContextManager";
      "typing.Collection";
      "typing.Awaitable";
    ]
      |> List.map ~f:(fun primitive -> Type.Primitive primitive));
  ()


let test_modules _ =
  let environment =
    populate_with_sources [
      Source.create ~qualifier:(!&"wingus") [];
      Source.create ~qualifier:(!&"dingus") [];
      Source.create ~qualifier:(!&"os.path") [];
    ]
  in
  let module Handler = (val environment) in

  assert_is_some (Handler.module_definition (!&"wingus"));
  assert_is_some (Handler.module_definition (!&"dingus"));
  assert_is_none (Handler.module_definition (!&"zap"));

  assert_is_some (Handler.module_definition (!&"os"));
  assert_is_some (Handler.module_definition (!&"os.path"));

  assert_true (Handler.is_module (!&"wingus"));
  assert_true (Handler.is_module (!&"dingus"));
  assert_false (Handler.is_module (!&"zap"));
  ()


let test_import_dependencies context =
  let create_files_and_test _ =
    Out_channel.create "test.py" |> Out_channel.close;
    Out_channel.create "a.py" |> Out_channel.close;
    Out_channel.create "ignored.py" |> Out_channel.close;
    Unix.handle_unix_error (fun () -> Unix.mkdir_p "subdirectory");
    Out_channel.create "subdirectory/b.py" |> Out_channel.close;
    let source = {|
         import a
         from builtins import str
         from subdirectory.b import c
         import sys
         from . import ignored
      |}
    in
    let environment =
      populate_with_sources
        [
          parse ~handle:"test.py" ~qualifier:(!&"test") source;
          parse ~handle:"a.py" ~qualifier:(!&"a") "";
          parse ~handle:"subdirectory/b.py" ~qualifier:(!&"subdirectory.b") "";
          parse ~handle:"builtins.pyi" ~qualifier:Reference.empty "";
        ]
    in
    let dependencies handle =
      let handle = File.Handle.create handle in
      Environment.dependencies environment (Source.qualifier ~handle)
      >>| String.Set.Tree.map ~f:File.Handle.show
      >>| String.Set.Tree.to_list
    in
    assert_equal
      (dependencies "subdirectory/b.py")
      (Some ["test.py"]);
    assert_equal
      (dependencies "a.py")
      (Some ["test.py"]);
    assert_equal
      (dependencies "builtins.pyi")
      (Some ["test.py"]);
    assert_equal
      (dependencies "sys.py")
      (Some ["test.py"])
  in
  with_bracket_chdir context (bracket_tmpdir context) create_files_and_test

let test_register_dependencies _ =
  let (module Handler: Environment.Handler) = Environment.handler (create_environment ()) in
  let source = {|
         import a # a is added here
         from subdirectory.b import c # subdirectory.b is added here
         from . import ignored # no dependency created here
      |}
  in
  Environment.register_dependencies
    (module Handler)
    (parse ~handle:"test.py" source);
  let dependencies handle =
    let handle = File.Handle.create handle in
    Environment.dependencies (module Handler) (Source.qualifier ~handle)
    >>| String.Set.Tree.map ~f:File.Handle.show
    >>| String.Set.Tree.to_list
  in
  assert_equal
    (dependencies "subdirectory/b.py")
    (Some ["test.py"]);
  assert_equal
    (dependencies "a.py")
    (Some ["test.py"])


let test_purge _ =
  let environment = Environment.Builder.create () in
  let ((module Handler: Environment.Handler) as handler) = Environment.handler environment in
  let source = {|
      import a
      class P(typing.Protocol): pass
      class baz.baz(): pass
      _T = typing.TypeVar("_T")
      x = 5
      def foo(): pass
    |}
  in
  Test.populate
    ~configuration
    handler
    [parse ~convert:true ~handle:"test.py" source];
  assert_is_some (Handler.class_definition "baz.baz");
  assert_is_some (Handler.aliases (Type.Primitive "_T"));
  let dependencies handle =
    let handle = File.Handle.create handle in
    Handler.dependencies (Source.qualifier ~handle)
    >>| String.Set.Tree.map ~f:File.Handle.show
    >>| String.Set.Tree.to_list
  in
  assert_equal
    (dependencies "a.py")
    (Some ["test.py"]);
  assert_equal
    (Handler.protocols ())
    [Type.Primitive "P"];

  Handler.purge [File.Handle.create "test.py"];

  assert_is_none (Handler.class_definition "baz.baz");
  assert_is_none (Handler.aliases (Type.Primitive "_T"));
  assert_equal (Handler.protocols ()) [];
  assert_equal
    (dependencies "a.py")
    (Some [])


let test_infer_protocols _ =
  let open Analysis in
  let configuration = Configuration.Analysis.create () in
  let assert_protocols ?classes_to_infer source expected_edges =
    Annotated.Class.Attribute.Cache.clear ();
    let expected_edges =
      let to_edge (source, target, parameters) =
        {
          TypeOrder.Edge.source = Type.Primitive source;
          target = Type.Primitive target;
          parameters = List.map parameters ~f:(fun parameter -> Type.Primitive parameter);
        }
      in
      List.map expected_edges ~f:to_edge
    in
    let open TypeOrder in
    let source =
      Test.parse source
      |> Preprocessing.preprocess
    in
    let environment = Environment.handler (create_environment ()) in
    Test.populate ~configuration environment [source];
    let ((module Handler: Environment.Handler) as handler) = environment in
    let resolution = TypeCheck.resolution (module Handler) () in
    let classes_to_infer =
      match classes_to_infer with
      | None ->
          Handler.TypeOrderHandler.keys ()
      | Some classes_to_infer ->
          List.map classes_to_infer ~f:(fun to_infer -> Type.Primitive to_infer)
          |> List.filter_map
            ~f:(Handler.TypeOrderHandler.find (Handler.TypeOrderHandler.indices ()))
    in
    let edges = Environment.infer_protocol_edges ~handler resolution ~classes_to_infer in
    let expected_edges = Edge.Set.of_list expected_edges in
    assert_equal
      ~cmp:Edge.Set.equal
      ~printer:(fun set -> Edge.Set.to_list set |> List.map ~f:Edge.show |> String.concat ~sep:", ")
      expected_edges
      edges
  in
  assert_protocols
    ~classes_to_infer:["A"; "B"]
    {|
      class P(typing.Protocol):
        def foo() -> int:
          pass

      class A:
        def foo() -> int:
          pass
      class B:
        def foo() -> str:
          pass
    |}
    ["A", "P", []];

  assert_protocols
    ~classes_to_infer:["A"]
    {|
      class P(typing.Protocol):
        def foo() -> int:
          pass

      class A:
        def foo() -> int:
          pass
      class B:
        def foo() -> int:
          pass
    |}
    ["A", "P", []];

  assert_protocols
    ~classes_to_infer:["B"]
    {|
      class P(typing.Protocol):
        def foo() -> int:
          pass

      class A:
        def foo() -> int:
          pass
      class B:
        def foo() -> int:
          pass
    |}
    ["B", "P", []];

  assert_protocols
    ~classes_to_infer:["A"; "B"]
    {|
      T = typing.TypeVar("T")
      class P(typing.Protocol, typing.Generic[T]):
        def foo() -> T:
          pass

      class A:
        def foo() -> str:
          pass
      class B():
        def foo() -> int:
          pass
    |}
    ["A", "P", ["str"]; "B", "P", ["int"]];


  assert_protocols
    ~classes_to_infer:["A"; "B"; "C"; "D"]
    {|
      class P(typing.Protocol):
        def foo() -> int:
          pass
        def bar() -> str:
          pass

      class A:
        def foo() -> int:
          pass
        def bar() -> str:
          pass
      class B:
        def foo() -> str:
          pass
        def bar() -> str:
          pass
      class C:
        def bar() -> str:
          pass
        def foo() -> int:
          pass
      class D:
        def bar() -> int:
          pass
        def foo() -> str:
          pass
    |}
    ["A", "P", []; "C", "P", []];

  ()


let test_propagate_nested_classes _ =
  let test_propagate sources aliases =
    Type.Cache.disable ();
    Type.Cache.enable ();
    let sources = List.map sources ~f:Preprocessing.preprocess in
    let handler =
      populate_with_sources
        ~environment:(create_environment ~include_helpers:false ())
        sources
    in

    let assert_alias (alias, target) =
      parse_single_expression alias
      |> parse_annotation handler
      |> Type.show
      |> assert_equal  ~printer:(fun string -> string) target
    in
    List.iter aliases ~f:assert_alias
  in
  test_propagate
    [
      parse
        {|
          class B:
            class N:
              pass
          class C(B):
            pass
        |};
    ]
    ["C.N", "B.N"];
  test_propagate
    [
      parse
        {|
          class B:
            class N:
              pass
          class C(B):
            class N:
              pass
        |};
    ]
    ["C.N", "C.N"];
  test_propagate
    [
      parse
        {|
          class B1:
            class N:
              pass
          class B2:
            class N:
              pass
          class C(B1, B2):
            pass
        |};
    ]
    ["C.N", "B1.N"];
  test_propagate
    [
      parse
        ~qualifier:(!&"qual")
        {|
          class B:
            class N:
              pass
        |};
      parse
        ~qualifier:(!&"importer")
        {|
          from qual import B
          class C(B):
            pass
        |};
    ]
    ["importer.C.N", "qual.B.N"];
  test_propagate
    [
      parse
        {|
          class B:
            class N:
              class NN:
                class NNN:
                  pass
          class C(B):
            pass
        |};
    ]
    ["C.N.NN.NNN", "B.N.NN.NNN"];
  ()


let () =
  "environment">:::[
    "connect_type_order">::test_connect_type_order;
    "join_type_order">::test_join_type_order;
    "less_or_equal_type_order">::test_less_or_equal_type_order;
    "meet_type_order">::test_meet_type_order;
    "supertypes_type_order">::test_supertypes_type_order;
    "class_definition">::test_class_definition;
    "connect_definition">::test_connect_definition;
    "import_dependencies">::test_import_dependencies;
    "infer_protocols_edges">::test_infer_protocols_edges;
    "infer_protocols">::test_infer_protocols;
    "modules">::test_modules;
    "populate">::test_populate;
    "protocols">::test_protocols;
    "purge">::test_purge;
    "register_class_metadata">::test_register_class_metadata;
    "register_aliases">::test_register_aliases;
    "register_class_definitions">::test_register_class_definitions;
    "register_dependencies">::test_register_dependencies;
    "register_globals">::test_register_globals;
    "propagate_nested_classes">::test_propagate_nested_classes;
  ]
  |> Test.run
