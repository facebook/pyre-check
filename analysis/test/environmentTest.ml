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
  List.map
    ~f:Access.create
    names
  |> List.concat


let value option =
  Option.value_exn option


let configuration = Configuration.Analysis.create ~infer:true ()


let plain_populate sources =
  let environment = Environment.Builder.create () in
  let handler = Environment.handler ~configuration environment in
  Service.Environment.populate ~configuration handler sources;
  environment


let populate_with_sources sources =
  plain_populate sources
  |> Environment.handler ~configuration


let populate source =
  populate_with_sources [parse source]

let populate_preprocess source =
  populate_with_sources [
    source
    |> parse
    |> Preprocessing.preprocess
  ]

let global environment =
  TypeCheck.resolution environment ()
  |> Resolution.global


let class_definition environment =
  TypeCheck.resolution environment ()
  |> Resolution.class_definition


let parse_annotation environment =
  TypeCheck.resolution environment ()
  |> Resolution.parse_annotation


let create_location path start_line start_column end_line end_column =
  let start = { Location.line = start_line; column = start_column } in
  let stop = { Location.line = end_line; column = end_column } in
  { Location.path; start; stop; }


let test_register_class_definitions _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  Environment.register_class_definitions
    (module Handler)
    (parse {|
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
  assert_equal (parse_annotation (module Handler) (!"C")) (Type.primitive "C");
  assert_equal (parse_annotation (module Handler) (!"D")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"B")) (Type.primitive "B");
  assert_equal (parse_annotation (module Handler) (!"A")) (Type.primitive "A");
  assert_equal (Handler.function_definitions (access ["foo"])) None;
  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  assert_equal (TypeOrder.successors order (Type.primitive "C")) [];
  assert_equal (TypeOrder.predecessors order (Type.primitive "C")) [];

  (* Annotations for classes are returned even if they already exist in the handler. *)
  let new_annotations =
    Environment.register_class_definitions
      (module Handler)
      (parse {|
         class C:
           ...
       |})
  in
  assert_equal
    ~cmp:Type.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ (Type.show next)))
    (Type.Set.singleton (Type.primitive "C"))
    new_annotations;
  let new_annotations =
    Environment.register_class_definitions
      (module Handler)
      (parse {|
         class int:
           pass
       |})
  in
  assert_equal
    ~cmp:Type.Set.equal
    ~printer:(Set.fold ~init:"" ~f:(fun sofar next -> sofar ^ " " ^ (Type.show next)))
    (Type.Set.singleton (Type.integer))
    new_annotations


let test_refine_class_definitions _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let source =
    parse
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
    ~top:Type.Object
    all_annotations;
  TypeOrder.remove_extra_edges
    (module Handler.TypeOrderHandler)
    ~bottom:Type.Bottom
    ~top:Type.Object
    all_annotations;

  Handler.refine_class_definition (Type.primitive "A");
  Handler.refine_class_definition (Type.primitive "B");
  Handler.refine_class_definition (Type.primitive "C");
  Handler.refine_class_definition (Type.primitive "D");
  Handler.refine_class_definition (Type.primitive "E");
  let attribute_equal
      (expected_target, expected_value)
      { Node.value = { Statement.Attribute.target; value; _ }; _ } =
    Expression.equal expected_target target &&
    Option.equal Expression.equal expected_value value
  in
  let assert_attribute ~implicit class_name attribute_name expected =
    let { Resolution.explicit_attributes; implicit_attributes; _ } =
      Option.value_exn (Handler.class_definition (Type.primitive class_name))
    in
    let map =
      if implicit then
        implicit_attributes
      else
        explicit_attributes
    in
    let actual = Access.SerializableMap.find_opt (Access.create attribute_name) map in
    match expected, actual with
    | Some expected, Some actual ->
        assert_true (attribute_equal expected actual)
    | None, None ->
        ()
    | _ ->
        assert_unreached ()
  in
  assert_attribute ~implicit:false "C" "x" None;
  assert_attribute ~implicit:true "C" "x" (Some (!"x", Some ~+(Expression.Integer 3)));
  assert_attribute ~implicit:true "D" "y" (Some (!"y", Some ~+(Expression.Integer 4)));
  assert_attribute ~implicit:false "D" "z" (Some (!"z", Some ~+(Expression.Integer 5)));

  let assert_successors class_name expected =
    let { Resolution.successors; _ } =
      Option.value_exn (Handler.class_definition (Type.primitive class_name))
    in
    let expected =
      List.map expected ~f:Type.primitive
      |> (fun expected -> expected @ [Type.Object; Type.Deleted; Type.Top])
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
  assert_successors "E" ["D"; "C"; "A"];
  ()


let test_register_aliases _ =
  let assert_resolved sources aliases =
    let (module Handler) =
      let environment = Environment.Builder.create () in
      let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
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
        |};
    ]
    [
      "C", "C";
      "D", "D";
      "B", "D";
      "A", "D";
    ];
  assert_resolved
    [
      parse
        ~qualifier:(Access.create "qualifier")
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
        ~qualifier:(Access.create "typing")
        {|
          class Iterator: ...
          class Iterable: ...
        |};
      parse
        ~qualifier:(Access.create "collections")
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
        ~qualifier:(Access.create "collections")
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
        ~qualifier:(Access.create "collections")
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
        ~qualifier:(Access.create "asyncio.tasks")
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
        ~qualifier:(Access.create "a")
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
        ~qualifier:(Access.create "qualifier")
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
        ~qualifier:(Access.create "stubbed")
        ~local_mode:Source.PlaceholderStub
        ~handle:"stubbed.pyi"
        "";
      parse
        ~qualifier:(Access.create "qualifier")
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
        ~qualifier:(Access.create "t")
        {|
          import x
          X = typing.Dict[int, int]
          T = typing.Dict[int, X]
          C = typing.Callable[[T], int]
        |};
      parse
        ~qualifier:(Access.create "x")
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
        ~qualifier:(Access.create "x")
        {|
          C = typing.Callable[[gurbage], gurbage]
        |};
    ]
    ["x.C", "x.C"];
  ()


let test_connect_definition _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let resolution = TypeCheck.resolution (module Handler) () in

  let (module TypeOrderHandler: TypeOrder.Handler) = (module Handler.TypeOrderHandler) in
  TypeOrder.insert (module TypeOrderHandler) (Type.primitive "C");
  TypeOrder.insert (module TypeOrderHandler) (Type.primitive "D");

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
      Class.name = Access.create "C";
      bases = [];
      body = [];
      decorators = [];
      docstring = None
    }
  in
  Environment.connect_definition ~resolution ~definition:class_definition;
  assert_edge ~predecessor:Type.Bottom ~successor:(Type.primitive "C");

  let definition =
    +(Test.parse_single_class {|
       class D(int, float):
         ...
     |})
  in
  Environment.connect_definition ~resolution ~definition;
  assert_edge ~predecessor:Type.Bottom ~successor:(Type.primitive "D");
  assert_edge ~predecessor:(Type.primitive "D") ~successor:Type.integer;
  assert_edge ~predecessor:(Type.primitive "D") ~successor:Type.float


let test_register_globals _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let resolution = TypeCheck.resolution (module Handler) () in
  let source =
    parse
      {|
        qualifier.with_join = 1 or 'asdf'  # don't join with an incomplete environment
        qualifier.annotated: int = 1
        qualifier.unannotated = 'string'
        qualifier.stub: int = ...
        class qualifier.Class: ...
        if True:
          qualifier.in_branch: int = 1
        else:
          qualifier.in_branch: int = 2
      |}
  in
  Environment.register_globals (module Handler) resolution source;

  let assert_global access expected =
    let actual =
      Access.create access
      |> Handler.globals
      >>| Node.value
      >>| Annotation.annotation
    in
    assert_equal ~cmp:(Option.equal Type.equal) expected actual
  in

  assert_global "qualifier.undefined" None;
  assert_global "qualifier.with_join" (Some Type.Top);
  assert_global "qualifier.annotated" (Some Type.integer);
  assert_global "qualifier.unannotated" (Some Type.string);
  assert_global "qualifier.stub" (Some Type.integer);
  assert_global "qualifier.Class" (Some (Type.meta (Type.primitive "qualifier.Class")));
  assert_global "qualifier.in_branch" (Some Type.integer)


let test_connect_type_order _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let resolution = TypeCheck.resolution (module Handler) () in
  let source =
    parse {|
       class C:
         ...
       class D(C):
         pass
       class CallMe:
         def CallMe.__call__(self, x: int) -> str:
           ...
       B = D
       A = B
       def foo()->A:
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
      (TypeOrder.successors order annotation)
      successors
  in
  (* Classes get connected to object via `connect_annotations_to_top`. *)
  assert_successors (Type.primitive "C") [];
  assert_successors (Type.primitive "D") [Type.primitive "C"];

  TypeOrder.connect_annotations_to_top order ~top:Type.Object all_annotations;

  assert_successors (Type.primitive "C") [Type.Object; Type.Deleted; Type.Top];
  assert_successors (Type.primitive "D") [Type.primitive "C"; Type.Object; Type.Deleted; Type.Top];
  assert_successors
    (Type.primitive "CallMe")
    [Type.primitive "typing.Callable"; Type.Object; Type.Deleted; Type.Top]

let test_register_functions _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let resolution = TypeCheck.resolution (module Handler) () in
  let source =
    parse {|
       def function() -> int: ...
       def function_with_arguments(i: int) -> None: ...

       class Class:
         def Class.__init__(self) -> None: ...
         def Class.method(self, i: int) -> int: ...
         @classmethod
         def Class.classmethod(cls, i: str) -> str: ...
         @property
         def Class.property_method(self) -> int: ...
         class Class.Nested:
           def Class.Nested.nested_instance_method(self) -> str: ...

       class ClassWithOverloadedConstructor:
         @overload
         def ClassWithOverloadedConstructor.__init__(self, s: str) -> None: ...
         def ClassWithOverloadedConstructor.__init__(self, i: int) -> None: ...

       @overload
       def overloaded(i: int) -> None: ...
       @overload
       def overloaded(i: float) -> None: ...
       def overloaded(i: str) -> None:
         pass

       def wrapper() -> None:
         def wrapper.nested_in_function() -> None:
           pass
    |}
  in
  Environment.register_functions (module Handler) resolution source;
  assert_is_some (Handler.function_definitions (access ["function"]));
  assert_is_some (Handler.function_definitions (access ["Class.__init__"]));
  assert_is_none (Handler.function_definitions (access ["nested_in_function"]));
  assert_is_none (Handler.function_definitions (access ["Class.property_method"]));

  let assert_global access expected =
    let actual =
      Access.create access
      |> Handler.globals
      >>| Node.value
      >>| Annotation.annotation
    in
    assert_equal
      ~printer:(function | Some annotation -> Type.show annotation | _ -> "None")
      ~cmp:(Option.equal Type.equal)
      (Some (parse_callable expected))
      actual
  in
  assert_global "function" "typing.Callable('function')[[], int]";
  assert_global
    "function_with_arguments"
    "typing.Callable('function_with_arguments')[[Named(i, int)], None]";

  assert_global
    "Class.__init__"
    "typing.Callable('Class.__init__')[[Named(self, $unknown)], None]";
  assert_global
    "Class.method"
    "typing.Callable('Class.method')[[Named(self, $unknown), Named(i, int)], int]";
  assert_global
    "Class.classmethod"
    "typing.Callable('Class.classmethod')[[Named(i, str)], str]";
  assert_global
    "Class.Nested.nested_instance_method"
    "typing.Callable('Class.Nested.nested_instance_method')[[Named(self, $unknown)], str]";

  assert_global
    "overloaded"
    ("typing.Callable('overloaded')[[Named(i, str)], None]" ^
     "[[[Named(i, int)], None]" ^
     "[[Named(i, float)], None]]");
  assert_global
    "ClassWithOverloadedConstructor.__init__"
    ("typing.Callable('ClassWithOverloadedConstructor.__init__')" ^
     "[[Named(self, $unknown), Named(i, int)], None]" ^
     "[[[Named(self, $unknown), Named(s, str)], None]]")


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
    (Type.primitive "foo.foo");
  assert_equal
    (parse_annotation environment (+Access (parse_single_access "Optional[foo.foo]")))
    (Type.parametric "Optional" [Type.primitive "foo.foo"]);
  assert_equal (parse_annotation environment !"bar") (Type.primitive "bar");

  (* Check custom aliases. *)
  assert_equal
    (parse_annotation environment !"typing.DefaultDict")
    (Type.primitive "collections.defaultdict");

  (* Check custom class definitions. *)
  assert_is_some (Handler.class_definition (Type.primitive "None"));
  assert_is_some (Handler.class_definition (Type.primitive "typing.Optional"));

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
         (index (Type.primitive base)))
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
      targets
      (Some (List.map superclasses ~f:to_target))
  in
  (* Metaclasses aren't superclasses. *)
  let environment =
    populate {|
      class abc.ABCMeta: ...
      class C(metaclass=abc.ABCMeta): ...
    |}
  in
  assert_superclasses ~environment "C" ~superclasses:[Type.Object];

  (* Ensure object is a superclass if a class only has unsupported bases. *)
  let environment =
    populate {|
      def foo() -> int:
        return 1
      class C(foo()):
        pass
    |}
  in
  assert_superclasses ~environment "C" ~superclasses:[Type.Object];

  (* Globals *)
  let assert_global_with_environment environment actual expected =
    assert_equal
      ~cmp:(Option.equal (Node.equal Annotation.equal))
      ~printer:(function | Some global -> Resolution.show_global global | None -> "None")
      (Some (Node.create_with_default_location expected))
      (global environment (parse_single_access actual))
  in

  let assert_global =
    {|
      class int(): pass
      A: int = 0
      B = 0
      C = ... # type: int
    |}
    |> populate
    |> assert_global_with_environment
  in

  assert_global "A" (Annotation.create_immutable ~global:true Type.integer);
  assert_global
    "B"
    (Annotation.create_immutable ~global:true ~original:(Some Type.Top) Type.integer);
  assert_global "C" (Annotation.create_immutable ~global:true Type.integer);

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
    (Annotation.create_immutable ~global:true ~original:(Some Type.Top) Type.integer);
  assert_global "global_annotated" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_both" (Annotation.create_immutable ~global:true Type.integer);
  assert_global "global_unknown" (Annotation.create_immutable ~global:true Type.Top);
  assert_global
    "function"
    (Annotation.create_immutable
       ~global:true
       (Type.callable
          ~name:(Access.create "function")
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
       (Type.meta (Type.primitive "Class")));
  assert_global
    "Class.__init__"
    (Annotation.create_immutable
       ~global:true
       (Type.callable
          ~name:(Access.create "Class.__init__")
          ~parameters:(Type.Callable.Defined [
              Type.Callable.Parameter.Named {
                Type.Callable.Parameter.name = Access.create "self";
                annotation = Type.Top;
                default = false;
              };
            ])
          ~annotation:Type.Top
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
    (Type.primitive "A"
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
    ~superclasses:[Type.primitive "typing.Callable"];
  ()


let test_infer_protocols_edges _ =
  let edges =
    let environment =
      populate_preprocess {|
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
        Map.set
          map
          ~key:(Access.create key)
          ~data:(List.map values ~f:Access.create)
      in
      Access.Map.empty
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
        ~protocol:(Type.primitive "Empty")
    in
    assert_equal 10 (Set.length empty_edges);
    Environment.infer_implementations
      environment
      resolution
      ~implementing_classes
      ~protocol:(Type.primitive "Sized")
    |> Set.union
      (Environment.infer_implementations
         environment
         resolution
         ~implementing_classes
         ~protocol:(Type.primitive "SuperObject"))
    |> Set.union empty_edges
  in
  let assert_edge_inferred source target =
    assert_true (Set.mem edges { TypeOrder.Edge.source; target })
  in
  let assert_edge_not_inferred source target =
    assert_false (Set.mem edges { TypeOrder.Edge.source; target })
  in

  assert_equal ~printer:Int.to_string (Set.length edges) 11;

  assert_edge_not_inferred (Type.primitive "List") (Type.primitive "Sized");
  assert_edge_inferred (Type.primitive "Set") (Type.primitive "Sized");
  assert_edge_not_inferred (Type.primitive "AlmostSet") (Type.primitive "Sized");
  assert_edge_not_inferred (Type.Object) (Type.primitive "SuperObject");
  assert_edge_inferred (Type.primitive "List") (Type.primitive "Empty");
  assert_edge_not_inferred (Type.Object) (Type.primitive "Empty")


let test_less_or_equal _ =
  let environment =
    populate {|
      class module.super(): ...
      class module.sub(module.super): ...
    |} in

  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in

  let super =
    parse_annotation
      environment
      (+Access (access ["module.super"])) in
  assert_equal super (Type.primitive "module.super");

  let sub =
    parse_annotation
      environment
      (+Access (access ["module.sub"])) in
  assert_equal
    sub
    (Type.primitive "module.sub");

  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:Type.Top);
  assert_true (TypeOrder.less_or_equal order ~left:super ~right:Type.Top);
  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:super);
  assert_false (TypeOrder.less_or_equal order ~left:super ~right:sub);

  let environment =
    populate {|
        class module.sub(module.super): pass
        class module.super(module.top): pass
        class module.top(): pass
    |} in

  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in

  let super =
    parse_annotation
      environment
      (+Access (access ["module.super"])) in
  assert_equal super (Type.primitive "module.super");

  let sub =
    parse_annotation
      environment
      (+Access (access ["module.sub"])) in
  let super =
    parse_annotation
      environment
      (+Access (access ["module.super"])) in
  let top =
    parse_annotation
      environment
      (+Access (access ["module.top"])) in
  assert_true (TypeOrder.less_or_equal order ~left:sub ~right:super);
  assert_true (TypeOrder.less_or_equal order ~left:super ~right:top);

  (* Optionals. *)
  let environment =
    populate {|
      class A: ...
      class B(A): ...
      class C(typing.Optional[A]): ...
    |} in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.primitive "A"))
       ~right:(Type.Optional (Type.primitive "A")));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.primitive "A")
       ~right:(Type.Optional (Type.primitive "A")));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.primitive "A"))
       ~right:(Type.primitive "A"));

  (* We're currently not sound with inheritance and optionals. *)
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Optional (Type.primitive "A"))
       ~right:(Type.primitive "C"));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.primitive "A")
       ~right:(Type.primitive "C"));

  (* Unions. *)
  let environment =
    populate {|
      class A: ...
      class B(A): ...
      class int(): ...
      class float(): ...
    |} in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"])
       ~right:(Type.Union [Type.primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "B"])
       ~right:(Type.Union [Type.primitive "A"]));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"])
       ~right:(Type.Union [Type.primitive "B"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.primitive "A")
       ~right:(Type.Union [Type.primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.primitive "B")
       ~right:(Type.Union [Type.primitive "A"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.primitive "A")
       ~right:(Type.Union [Type.primitive "A"; Type.primitive "B"]));
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"; Type.primitive "B"; Type.integer])
       ~right:Type.Object);
  assert_true
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"; Type.primitive "B"; Type.integer])
       ~right:(Type.Union [Type.Top; Type.Object; Type.Optional Type.float]));
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"; Type.primitive "B"; Type.integer])
       ~right:Type.float);
  assert_false
    (TypeOrder.less_or_equal
       order
       ~left:(Type.Union [Type.primitive "A"; Type.primitive "B"; Type.integer])
       ~right:(Type.Union [Type.float; Type.primitive "B"; Type.integer]));

  (* Special cases. *)
  assert_true (TypeOrder.less_or_equal order ~left:Type.integer ~right:Type.float)


let test_join _ =
  let environment =
    populate {|
      class foo(): ...
      class bar(L[T]): ...
    |} in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let foo = Type.primitive "foo" in
  let bar = Type.primitive "bar" in

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
    (TypeOrder.join order Type.Object Type.Top)
    Type.Top;

  assert_equal
    (TypeOrder.join order Type.integer (Type.Union [Type.integer; Type.string]))
    (Type.Union [Type.integer; Type.string]);
  assert_equal
    (TypeOrder.join order (Type.Union [Type.integer; Type.string]) Type.integer)
    (Type.Union [Type.integer; Type.string]);

  assert_raises
    (TypeOrder.Untracked (Type.primitive "durp"))
    (fun _ -> TypeOrder.join order bar (Type.primitive "durp"));

  (* Special cases. *)
  assert_equal
    (TypeOrder.join order Type.integer Type.float)
    Type.float


let test_meet _ =
  let environment =
    populate {|
      class foo(): ...
      class bar(L[T]): ...
      class A: ...
      class B(A): ...
      class C(A): ...
      class D(B,C): ...
    |} in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let assert_meet left right expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:(Format.asprintf "%a" Type.pp)
      ~pp_diff:(diff ~print:Type.pp)
      (TypeOrder.meet order left right)
      expected
  in
  let foo = Type.primitive "foo" in
  let bar = Type.primitive "bar" in
  let a = Type.primitive "A" in
  let b = Type.primitive "B" in
  let c = Type.primitive "C" in
  let d = Type.primitive "D" in

  assert_meet Type.Bottom bar Type.Bottom;
  assert_meet Type.Top bar bar;
  assert_meet Type.Object Type.Top Type.Object;
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


let environment =
  populate {|
    _T = typing.TypeVar('_T')
    def foo.foo(): ...
    def bar.bar(): pass
    def bar(): ...

    def untyped(a, b): pass
    def typed(a: str, b: int, c: int): pass

    class list(): ...
    class baz.baz(): pass
  |}


let base_environment =
  {|
    _T = typing.TypeVar('_T')
    class typing.Generic(): pass
    class typing.Iterable(typing.Generic[_T]): pass
    _T2 = typing.TypeVar('_T2')
    _T3 = typing.TypeVar('_T3')
    class typing.Generator(typing.Generic[_T, _T2, _T3], typing.Iterable[_T]): pass
    class typing.List(): pass
    class str(): ...
    class int(): ...
    class list(typing.Iterable[_T], typing.Generic[_T]): ...
    # implicitly subclasses generic.
    class typing.Collection(typing.Iterable[_T]): ...
  |}


let test_supertypes _ =
  let environment =
    (populate {|
      class foo(): pass
      class bar(foo): pass
    |}) in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_equal
    [Type.Object; Type.Deleted; Type.Top]
    (TypeOrder.successors order (Type.primitive "foo"));
  assert_equal
    [Type.primitive "foo"; Type.Object; Type.Deleted; Type.Top]
    (TypeOrder.successors order (Type.primitive "bar"));

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
        name = ~~"typing.Generic";
        parameters = [Type.integer];
      };
      Type.Object;
      Type.Deleted;
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
  assert_true (is_defined environment (Type.primitive "baz.baz"));
  assert_true (is_defined environment (Type.parametric "baz.baz" [Type.integer]));
  assert_is_some (class_definition environment (Type.primitive "baz.baz"));

  assert_false (is_defined environment (Type.primitive "bar.bar"));
  assert_false (is_defined environment (Type.parametric "bar.bar" [Type.integer]));
  assert_is_none (class_definition environment (Type.primitive "bar.bar"));

  let any =
    class_definition environment Type.Object
    |> value
    |> Node.value
  in
  assert_equal any.Class.name (access ["object"])


let test_protocols _ =
  let environment =
    populate {|
      class A(): ...
      class B(typing.Protocol): pass
      class C(): pass
      class D(metaclass=abc.ABCMeta): ...
    |} in
  let module Handler = (val environment) in

  assert_equal
    ~cmp:(List.equal ~equal:Type.equal)
    (Handler.protocols ())
    ([Type.Primitive ~~"B"])


let test_modules _ =
  let environment =
    populate_with_sources [
      Source.create ~qualifier:(Access.create "wingus") [];
      Source.create ~qualifier:(Access.create "dingus") [];
      Source.create ~qualifier:(Access.create "os.path") [];
    ]
  in
  let module Handler = (val environment) in

  assert_is_some (Handler.module_definition (Access.create "wingus"));
  assert_is_some (Handler.module_definition (Access.create "dingus"));
  assert_is_none (Handler.module_definition (Access.create "zap"));

  assert_is_some (Handler.module_definition (Access.create "os"));
  assert_is_some (Handler.module_definition (Access.create "os.path"));

  assert_true (Handler.is_module (Access.create "wingus"));
  assert_true (Handler.is_module (Access.create "dingus"));
  assert_false (Handler.is_module (Access.create "zap"));
  ()


let test_import_dependencies context =
  let create_files_and_test _ =
    Out_channel.create "test.py" |> Out_channel.close;
    Out_channel.create "a.py" |> Out_channel.close;
    Out_channel.create "ignored.py" |> Out_channel.close;
    Unix.handle_unix_error (fun () -> Unix.mkdir_p "subdirectory");
    Out_channel.create "subdirectory/b.py" |> Out_channel.close;
    let source = {|
         import a # a is added here
         from subdirectory.b import c # subdirectory.b is added here
         import sys # no dependency created here
         from . import ignored # no dependency created here
      |}
    in
    let environment =
      populate_with_sources
        [
          parse ~handle:"test.py" ~qualifier:(Access.create "test") source;
          parse ~handle:"a.py" ~qualifier:(Access.create "a") "";
          parse ~handle:"subdirectory/b.py" ~qualifier:(Access.create "subdirectory.b") "";
        ]
    in
    let dependencies handle =
      let handle = File.Handle.create handle in
      Environment.dependencies environment (Source.qualifier ~handle)
      >>| List.map ~f:File.Handle.show
    in
    assert_equal ~printer:(fun lo -> lo >>| List.to_string ~f:ident |> Option.value ~default:"nun")
      (dependencies "subdirectory/b.py")
      (Some ["test.py"]);
    assert_equal
      (dependencies "a.py")
      (Some ["test.py"]);
  in
  with_bracket_chdir context (bracket_tmpdir context) create_files_and_test

let test_register_dependencies _ =
  let environment = Environment.Builder.create () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
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
    >>| List.map ~f:File.Handle.show
  in
  assert_equal
    (dependencies "subdirectory/b.py")
    (Some ["test.py"]);
  assert_equal
    (dependencies "a.py")
    (Some ["test.py"])


let test_purge _ =
  let environment = Environment.Builder.create () in
  let ((module Handler: Environment.Handler) as handler) =
    Environment.handler ~configuration environment
  in
  let source = {|
      import a
      class baz.baz(): pass
      _T = typing.TypeVar("_T")
      x = 5
      def foo(): pass
    |}
  in
  Service.Environment.populate
    ~configuration
    handler
    [parse ~handle:"test.py" source];
  assert_is_some (Handler.class_definition (Type.primitive "baz.baz"));
  assert_is_some (Handler.function_definitions (Access.create "foo"));
  assert_is_some (Handler.aliases (Type.primitive "_T"));
  assert_equal
    (Handler.dependencies (Source.qualifier ~handle:(File.Handle.create "a.py")))
    (Some [File.Handle.create "test.py"]);

  Handler.purge [File.Handle.create "test.py"];

  assert_is_none (Handler.class_definition (Type.primitive "baz.baz"));
  assert_is_none (Handler.function_definitions (Access.create "foo"));
  assert_is_none (Handler.aliases (Type.primitive "_T"));
  assert_equal
    (Handler.dependencies (Source.qualifier ~handle:(File.Handle.create"a.py")))
    (Some [])


let test_infer_protocols _ =
  let open Analysis in
  let configuration = Configuration.Analysis.create () in
  let type_sources = Test.typeshed_stubs in
  let assert_protocols ?classes_to_infer source expected_edges =
    Annotated.Class.Attribute.Cache.clear ();
    let expected_edges =
      let to_edge (source, target) =
        {
          TypeOrder.Edge.source = Type.primitive source;
          target = Type.primitive target
        }
      in
      List.map expected_edges ~f:to_edge
    in
    let open TypeOrder in
    let source =
      Test.parse source
      |> Preprocessing.preprocess
    in
    let environment = Environment.Builder.create () in
    Service.Environment.populate
      ~configuration
      (Environment.handler ~configuration environment)
      (source :: type_sources);
    let ((module Handler: Environment.Handler) as handler) =
      Environment.handler environment ~configuration
    in
    let resolution = TypeCheck.resolution (module Handler) () in
    let classes_to_infer =
      match classes_to_infer with
      | None ->
          Handler.TypeOrderHandler.keys ()
      | Some classes_to_infer ->
          List.map classes_to_infer ~f:Type.primitive
          |> List.filter_map
            ~f:(Handler.TypeOrderHandler.find (Handler.TypeOrderHandler.indices ()))
    in
    let edges = Environment.infer_protocol_edges ~handler resolution ~classes_to_infer in
    let expected_edges = Edge.Set.of_list expected_edges in
    assert_equal
      ~cmp:Edge.Set.equal
      ~printer:(fun set -> Sexp.to_string [%message (set: Edge.Set.t)])
      expected_edges
      edges
  in
  assert_protocols
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
    ["A", "P"];

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
    ["A", "P"];

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
    ["B", "P"];

  assert_protocols
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
    ["A", "P"; "C", "P"]


let () =
  "environment_type_order">:::[
    "connect_type_order">::test_connect_type_order;
    "join">::test_join;
    "less_or_equal">::test_less_or_equal;
    "meet">::test_meet;
    "supertypes">::test_supertypes;
  ]
  |> Test.run;
  "environment">:::[
    "class_definition">::test_class_definition;
    "connect_definition">::test_connect_definition;
    "import_dependencies">::test_import_dependencies;
    "infer_protocols_edges">::test_infer_protocols_edges;
    "infer_protocols">::test_infer_protocols;
    "modules">::test_modules;
    "populate">::test_populate;
    "protocols">::test_protocols;
    "purge">::test_purge;
    "refine_class_definitions">::test_refine_class_definitions;
    "register_aliases">::test_register_aliases;
    "register_class_definitions">::test_register_class_definitions;
    "register_dependencies">::test_register_dependencies;
    "register_functions">::test_register_functions;
    "register_globals">::test_register_globals;
  ]
  |> Test.run
