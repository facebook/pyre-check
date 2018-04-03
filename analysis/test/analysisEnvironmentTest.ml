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


let variable name =
  Type.Variable { Type.variable = name; constraints = [] }


let configuration = Configuration.create ()


let plain_populate ?source_root ?(check_dependency_exists = false) sources =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    ?source_root
    ~check_dependency_exists
    (Environment.handler ~configuration environment)
    sources;
  environment


let populate_with_sources ?source_root ?(check_dependency_exists = false) sources =
  plain_populate ?source_root ~check_dependency_exists sources
  |> Environment.handler ~configuration


let populate ?source_root ?(check_dependency_exists = false) source =
  populate_with_sources ?source_root ~check_dependency_exists [parse source]


let resolution environment =
  Environment.resolution environment ()


let global environment =
  resolution environment
  |> Resolution.global


let function_signature environment access call arguments =
  resolution environment
  |> (fun resolution -> Resolution.function_signature resolution access call arguments)
  |> List.hd


let method_signature environment access call arguments =
  (Resolution.method_signature (resolution environment) access call arguments)
  |> List.hd


let class_definition environment =
  resolution environment
  |> Resolution.class_definition


let parse_annotation environment =
  resolution environment
  |> Resolution.parse_annotation


let create_location path start_line start_column end_line end_column =
  let start = { Location.line = start_line; column = start_column } in
  let stop = { Location.line = end_line; column = end_column } in
  { Location.path; start; stop; }


let test_create _ =
  let environment =
    Environment.Builder.create ~configuration () in
  assert_is_none
    (function_signature
       (Environment.handler ~configuration environment)
       (access ["foo"])
       { Call.name = !"foo"; arguments = [] }
       [])


let test_copy _ =
  let plain_environment =
    plain_populate [parse {|
      def foo.foo(): ...
    |}] in

  let environment =
    populate {|
      def foo.foo(): ...
      class A():
        def __init__(self): ...
    |} in

  assert_is_some
    (function_signature
       environment
       (access ["foo"])
       { Call.name = !"foo"; arguments = [] }
       []);
  assert_is_none
    (function_signature
       environment
       (access ["foo"])
       { Call.name = !"bar"; arguments = [] }
       []);
  assert_is_some
    (function_signature
       environment
       (access [])
       { Call.name = !"A"; arguments = [] }
       []);

  let localized =
    let localized = Environment.Builder.copy plain_environment in
    Environment.populate
      ~configuration
      (Environment.handler ~configuration localized)
      [parse "def foo.bar(): ..."];
    Environment.handler ~configuration localized in

  (* Old environment is unmodified. *)
  assert_is_some
    (function_signature
       environment
       (access ["foo"])
       { Call.name = !"foo"; arguments = [] }
       []);
  assert_is_none
    (function_signature
       environment
       (access ["foo"])
       { Call.name = !"bar"; arguments = [] }
       []);

  (* New environment contains everything. *)
  assert_is_some
    (function_signature
       localized
       (access ["foo"])
       { Call.name = !"foo"; arguments = [] }
       []);
  assert_is_some
    (function_signature
       localized
       (access ["foo"])
       { Call.name = !"bar"; arguments = [] }
       [])


let test_register_class_definitions _ =
  let environment = Environment.Builder.create ~configuration () in
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
    |});
  assert_equal (parse_annotation (module Handler) (!"C")) (Type.primitive "C");
  assert_equal (parse_annotation (module Handler) (!"D")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"B")) (Type.primitive "B");
  assert_equal (parse_annotation (module Handler) (!"A")) (Type.primitive "A");
  assert_equal (Handler.function_definitions (access ["foo"])) None;
  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  assert_equal (TypeOrder.successors order (Type.primitive "C")) [];
  assert_equal (TypeOrder.predecessors order (Type.primitive "C")) []


let test_register_aliases _ =
  Type.TypeCache.disable ();
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let typing =
    parse ~qualifier:(Access.create "typing") {|
      class Iterator:
        ...
      class Iterable:
        ...
    |}
    |> Preprocessing.qualify
  in
  let other =
    parse ~qualifier:(Access.create "collections") {|
      from typing import Iterator as Iterator
      from typing import Iterable
    |}
    |> Preprocessing.qualify
  in
  let source =
    parse {|
       class C:
         ...
       class D(C):
         pass
       B = D
       A = B
       X = None
       def foo()->A:
         return D()
       def boo()->X:
         return
       if __name__ == '__main__':
         C = D
    |}
  in
  Environment.register_class_definitions (module Handler) typing;
  Environment.register_class_definitions (module Handler) source;
  Environment.register_class_definitions (module Handler) other;
  Environment.register_aliases (module Handler) [typing; other; source];

  assert_equal (parse_annotation (module Handler) (!"C")) (Type.primitive "C");
  assert_equal (parse_annotation (module Handler) (!"D")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"B")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"A")) (Type.primitive "D");
  assert_equal (parse_annotation( module Handler) (!"X")) (Type.none);
  assert_equal (Handler.function_definitions (access ["foo"])) None;

  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  assert_true (TypeOrder.contains order (Type.primitive "typing.Iterator"));
  assert_true (TypeOrder.contains order (Type.primitive "typing.Iterable"));
  assert_equal
    (parse_annotation (module Handler) (!"collections.Iterator"))
    (Type.parametric "typing.Iterator" [Type.Object]);
  assert_equal
    (parse_annotation (module Handler) (!"collections.Iterable"))
    (Type.parametric "typing.Iterable" [Type.Object])


let test_connect_definition _ =
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let c_primitive = Type.primitive "C" in

  let (module TypeOrderHandler: TypeOrder.Handler) = (module Handler.TypeOrderHandler) in
  TypeOrder.insert (module TypeOrderHandler) c_primitive;

  let class_definition =
    +{
      Class.name = Access.create "C";
      bases = [];
      body = [];
      decorators = [];
      docstring = None
    }
  in

  let primitive, parameters =
    Handler.connect_definition
      ~path:"a.py"
      ~predecessor:Type.Bottom
      ~name:(Access.create "C")
      ~definition:(Some class_definition)
  in
  assert_equal primitive c_primitive;
  assert_equal parameters [];
  let c_index = TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) c_primitive in
  let bottom_index = TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) Type.Bottom in

  assert_true
    (List.mem
       ~equal:TypeOrder.Target.equal
       (TypeOrderHandler.find_unsafe (TypeOrderHandler.edges ()) bottom_index)
       { TypeOrder.Target.target = c_index; parameters = [] });

  assert_true
    (List.mem
       ~equal:TypeOrder.Target.equal
       (TypeOrderHandler.find_unsafe (TypeOrderHandler.backedges ()) c_index)
       { TypeOrder.Target.target = bottom_index; parameters = []})



let test_register_globals _ =
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let source =
    parse
      {|
        qualifier.with_join = 1 or 'asdf'  # don't join with an incomplete environment
        qualifier.annotated: int = 1
        qualifier.unannotated = 'string'
        qualifier.stub: int = ...
        class qualifier.Class: ...
      |}
  in
  Environment.register_globals (module Handler) source;

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
  assert_global "qualifier.with_join" None;
  assert_global "qualifier.annotated" (Some Type.integer);
  assert_global "qualifier.unannotated" (Some Type.string);
  assert_global "qualifier.stub" (Some Type.integer);
  assert_global "qualifier.Class" (Some (Type.meta (Type.primitive "qualifier.Class")))


let test_connect_type_order _ =
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let source =
    parse {|
       class C:
         ...
       class D(C):
         pass
       B = D
       A = B
       def foo()->A:
         return D()
    |}
  in
  let order = (module Handler.TypeOrderHandler: TypeOrder.Handler) in
  Environment.register_class_definitions (module Handler) source;
  Environment.register_aliases (module Handler) [source];
  Environment.connect_type_order (module Handler) source;
  assert_equal (parse_annotation (module Handler) (!"C")) (Type.primitive "C");
  assert_equal (parse_annotation (module Handler) (!"D")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"B")) (Type.primitive "D");
  assert_equal (parse_annotation (module Handler) (!"A")) (Type.primitive "D");
  assert_is_none (Handler.function_definitions (access ["foo"]));
  assert_equal (TypeOrder.successors order (Type.primitive "C")) [Type.Object; Type.Top];
  assert_equal
    (TypeOrder.successors order (Type.primitive "D"))
    [Type.primitive "C"; Type.Object; Type.Top]


let test_register_functions _ =
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let source =
    parse {|
       def function() -> int: ...
       def function_with_arguments(i: int) -> None: ...
       class Class: ...
    |}
  in
  Environment.register_functions (module Handler) source;
  assert_is_some (Handler.function_definitions (access ["function"]));
  assert_is_some (Handler.function_definitions (access ["Class"]));

  let assert_global access expected =
    let actual =
      Access.create access
      |> Handler.globals
      >>| Node.value
      >>| Annotation.annotation
    in
    let expected =
      expected
      >>| parse_single_expression
      >>| Type.create ~aliases:(fun _ -> None)
    in
    assert_equal
      ~printer:(function | Some annotation -> Type.show annotation | _ -> "None")
      ~cmp:(Option.equal Type.equal)
      expected
      actual
  in
  assert_global "function" (Some "typing.Callable('function')[[], int]");
  assert_global
    "function_with_arguments"
    (Some "typing.Callable('function_with_arguments')[[Named(i, int)], None]");
  assert_global
    "Class"
    (Some "typing.Callable('Class.Class')[[Named(self, $unknown)], Class]")  (* Eww... *)


let test_populate _ =
  (* Test type resolution. *)
  let environment =
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

  (* Check type aliases. *)
  let environment =
    populate {|
      class str: ...
      _T = typing.TypeVar('_T')
      S = str
      S2 = S
    |}
  in
  assert_equal (parse_annotation environment !"_T") (variable ~~"_T");
  assert_equal (parse_annotation environment !"S") Type.string;
  assert_equal (parse_annotation environment !"S2") Type.string;

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
       (Type.callable
          ~name:(Access.create "function")
          ~parameters:(Type.Callable.Defined [])
          ~annotation:Type.Top
          ()));
  assert_global
    "Class"
    (Annotation.create_immutable
       ~global:true
       ~original:(Some Type.Top)
       (Type.meta (Type.primitive "Class")));

  (* Loops. *)
  try
    populate {|
        def foo(cls):
          class cls(cls): pass
      |}
    |> ignore
  with TypeOrder.Cyclic ->
    assert_unreached ();

    (* Check meta variables are registered. *)
    let assert_global =
      {|
        class A:
          pass
      |}
      |> populate
      |> assert_global_with_environment
    in
    assert_global "A" (Type.primitive "A" |> Type.meta |> Annotation.create_immutable ~global:true)


let test_infer_protocols _ =
  let edges =
    let environment =
      populate {|
        class Empty(typing.Protocol):
          pass
        class Sized(typing.Protocol):
          def empty() -> bool: pass
          def len() -> int: pass
        class Supersized(typing.Protocol):
          def empty() -> bool: pass
          def len() -> int: pass
        class List():
          def empty() -> bool: pass
          def length() -> int: pass
        class Set():
          def empty() -> bool: pass
          def len() -> int: pass
        class Subset(Set): pass
        class AlmostSet():
          def empty(a) -> bool: pass
          def len() -> int: pass
        class object:
          def __hash__(self) -> int: ...
        class SuperObject(typing.Protocol):
            @abstractmethod
            def __hash__(self) -> int: ...
      |}
    in

    assert_equal
      (Environment.infer_implementations
         environment
         ~protocol:(Type.primitive "Empty")
       |> Set.length)
      0;
    Environment.infer_implementations environment ~protocol:(Type.primitive "Sized")
    |> Set.union
      (Environment.infer_implementations environment ~protocol:(Type.primitive "SuperObject"))
  in
  let assert_edge_inferred source target =
    assert_true (Set.mem edges { TypeOrder.Edge.source; target })
  in
  let assert_edge_not_inferred source target =
    assert_false (Set.mem edges { TypeOrder.Edge.source; target })
  in

  assert_equal (Set.length edges) 1;

  assert_edge_not_inferred (Type.primitive "List") (Type.primitive "Sized");
  assert_edge_inferred (Type.primitive "Set") (Type.primitive "Sized");
  assert_edge_not_inferred (Type.primitive "AlmostSet") (Type.primitive "Sized");
  assert_edge_not_inferred (Type.Object) (Type.primitive "SuperObject")


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


let normal =
  let dummy = !"dummy" in
  List.map ~f:(fun argument -> Signature.Normal { Signature.annotation = argument; value = dummy })


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

let assert_instantiated
    environment
    ?(qualifier = [])
    name
    arguments
    expected_parameters
    expected_return =
  let instantiated define =
    let parameter (name, annotation) =
      {
        Parameter.name = ~~name;
        value = None;
        annotation = annotation >>| Type.expression;
      }
    in
    let parameter_equal left right =
      (* Ignore the value. *)
      Identifier.equal left.Parameter.name right.Parameter.name &&
      Option.equal
        Expression.equal
        left.Parameter.annotation
        right.Parameter.annotation
    in
    (List.equal ~equal:parameter_equal)
      (List.map ~f:parameter expected_parameters)
      (List.map ~f:Node.value define.Define.parameters)
    &&
    (Option.equal Expression.equal expected_return define.Define.return_annotation)
  in

  let environment = populate environment in
  Environment.populate
    ~configuration
    ~check_dependency_exists:false
    environment
    [parse base_environment];

  Resolution.function_signature
    (resolution environment)
    qualifier
    { Call.name = !name; arguments = [] }
    (List.map ~f:(~+) arguments)
  |> List.map ~f:(fun { Signature.instantiated; _ } -> instantiated)
  |> List.exists ~f:instantiated
  |> assert_true


let assert_not_instantiated
    environment
    name
    arguments =
  let environment = populate environment in
  Environment.populate
    ~configuration
    ~check_dependency_exists:false
    environment
    [parse base_environment];
  assert_is_none
    (function_signature
       environment
       []
       { Call.name = !name; arguments = [] }
       (List.map ~f:(~+) arguments))


let test_function_signature _ =
  assert_instantiated
    "def foo.foo(): pass"
    ~qualifier:(access ["foo"])
    "foo"
    []
    []
    None;

  assert_instantiated
    {|
      class str(): pass
      def foo(a: str):
        pass
    |}
    "foo"
    (normal [Type.Top])
    ["a", Some Type.string]
    None;

  assert_instantiated
    {|
      class str(): pass
      class int(): pass
      def foo(a: typing.Tuple[int, str]):
        pass
    |}
    "foo"
    (normal [(Type.tuple [Type.integer; Type.string])])
    ["a", Some (Type.tuple [Type.integer; Type.string])]
    None;

  assert_instantiated
    {|
      class str(): pass
      class int(): pass
      def foo(a: typing.Tuple[int, str]):
        pass
    |}
    "foo"
    (normal [Type.Top])
    ["a", Some (Type.tuple [Type.integer; Type.string])]
    None;

  assert_instantiated
    {|
      class str(): pass
      class int(): pass
      def foo(a: typing.Tuple[int, str]):
        pass
    |}
    "foo"
    (normal [(Type.tuple [Type.Top; Type.Top])])
    ["a", Some (Type.tuple [Type.integer; Type.string])]
    None


let test_function_signature_variable_instantiation _ =
  assert_instantiated
    {|
      _T = typing.TypeVar('_T')
      def foo(a: _T):
        pass
    |}
    "foo"
    (normal [Type.integer])
    ["a", Some Type.integer]
    None;

  assert_instantiated
    {|
      class int(): pass
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      def foo(a: _T, b: _S, c: int):
        pass
    |}
    "foo"
    (normal [Type.string; Type.float; Type.integer])
    ["a", Some Type.string; "b", Some Type.float; "c", Some Type.integer]
    None;

  assert_instantiated
    {|
      class int(): pass
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      def foo(a: _T, b: int = 1):
        pass
    |}
    "foo"
    (normal [Type.string]) (* Not all parameter types provided. *)
    ["a", Some Type.string; "b", Some Type.integer]
    None;

  assert_instantiated
    {|
      _T = typing.TypeVar('_T')
      def foo(a: typing.Iterable[_T]):
        pass
    |}
    "foo"
    (normal [Type.parametric "typing.Collection" [Type.integer]])
    ["a", Some (Type.parametric "typing.Iterable" [Type.integer])]
    None;

  assert_instantiated
    {|
      class int(): pass
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      def foo(a: _T, b: typing.Optional[_S] = ...):
        pass
    |}
    "foo"
    (normal [Type.string]) (* Not all parameter types provided. *)
    ["a", Some Type.string; "b", Some (Type.optional (variable ~~"_S"))]
    None;

  assert_not_instantiated
    {|
      class str(): pass
      class int(): pass
      _T = typing.TypeVar('_T')
      def foo(a: _T, b: _T):
        pass
    |}
    "foo"
    (normal [Type.string; Type.integer]); (* Requires join. *)

  (* Incompatible return type. *)
  assert_instantiated
    {|
      _T = typing.TypeVar('_T')
      def foo(a: _T) -> _T:
        pass
    |}
    "foo"
    (normal [Type.string])
    ["a", Some Type.string]
    (Some (Type.expression Type.string));

  (* Parametric types. *)
  assert_instantiated
    {|
      def foo(a: typing.Iterable[_T], b: _T):
        pass
    |}
    "foo"
    (normal [Type.list Type.string; Type.string])
    ["a", Some (Type.iterable Type.string); "b", Some Type.string]
    None;

  assert_instantiated
    {|
      def foo(a: typing.Iterable[_T], b: _T):
        pass
    |}
    "foo"
    (normal [Type.generator Type.string; Type.string])
    ["a", Some (Type.iterable Type.string); "b", Some Type.string]
    None;

  assert_instantiated
    {|
    def foo(a: typing.Union[_T, _T]):
      pass
    |}
    "foo"
    (normal [Type.string])
    ["a", Some Type.string]
    None;

  assert_not_instantiated
    {|
      def foo(a: typing.Iterable[_T], b: _T) -> str:
        pass
    |}
    "foo"
    (normal [Type.list Type.string; Type.integer]); (* Requires join. *)

  assert_not_instantiated
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      def foo(a: _T, b: _S):
        pass
    |}
    "foo"
    (normal [Type.integer]); (* Incomplete resolution. *)

  (* Incomplete information at call-site. *)
  assert_instantiated
    {|
      _T = typing.TypeVar('_T')
      def max(a: _T, b: _T) -> _T:
        pass
    |}
    "max"
    (normal [Type.Top; Type.integer])
    ["a", Some Type.integer; "b", Some Type.integer]
    (Some (Type.expression Type.integer));

  assert_instantiated
    {|
      _T = typing.TypeVar('_T')
      def max(a: _T, b: _T) -> _T:
        pass
    |}
    "max"
    (normal [Type.integer; Type.Top])
    ["a", Some Type.integer; "b", Some Type.integer]
    (Some (Type.expression Type.integer));

  assert_not_instantiated
    {|
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      def max(a: _T, b: _S):
        pass
    |}
    "max"
    (normal [Type.Top; Type.Top]);

  (* Don't ever return just bottom. *)
  assert_not_instantiated
    {|
      _T = typing.TypeVar('_T')
      def identity(a: _T) -> _T:
        pass
    |}
    "identity"
    (normal [Type.Top]);

  (* Indirect constraints. *)
  assert_instantiated
    {|
      class typing.Sequence:
        pass
      class range(typing.Sequence[int]):
        pass
      _T = typing.TypeVar('_T')
      def reversed(i: typing.Sequence[_T]) -> typing.Iterable[_T]:
        pass
    |}
    "reversed"
    (normal [Type.primitive "range"])
    ["i", Some (Type.parametric "typing.Sequence" [Type.integer])]
    (Some (Type.expression (Type.iterable Type.integer)))


let test_function_signature_starred _ =
  assert_instantiated
    "def untyped(a, b): pass"
    "untyped"
    (normal [Type.integer; Type.integer])
    ["a", None; "b", None]
    None;

  assert_instantiated
    "def untyped(a, b): pass"
    "untyped"
    [Signature.Starred (Type.list Type.integer)]
    ["a", None; "b", None]
    None;

  assert_instantiated
    "def typed(a: int, b: int): pass"
    "typed"
    (normal [Type.integer; Type.integer])
    ["a", Some Type.integer; "b", Some Type.integer]
    None;

  assert_instantiated
    {|
      class int(): pass
      def typed(a: int, b: int): pass
    |}
    "typed"
    [Signature.Starred (Type.list Type.integer)]
    ["a", Some Type.integer; "b", Some Type.integer]
    None;

  assert_instantiated
    {|
      class int(): pass
      class str(): pass
      def typed(a: int, b: int): pass
    |}
    "typed"
    [Signature.Starred (Type.list Type.string)]
    ["a", Some Type.integer; "b", Some Type.integer]
    None;

  assert_instantiated
    {|
      class int(): pass
      class str(): pass
      def typed(a: int, b: str): pass
    |}
    "typed"
    ([Signature.Starred (Type.list Type.integer)] @ (normal [Type.integer]))
    ["a", Some Type.integer; "b", Some Type.string]
    None


let test_function_signature_constructor _ =
  assert_instantiated
    {|
      class A:
        def __init__(self):
          pass
    |}
    "A"
    []
    ["self", None]
    (Some (Type.expression (Type.primitive "A")));

  assert_instantiated
    {|
      class A:
        def __init__(self, i: int): ...
    |}
    "A"
    (normal [Type.integer])
    ["self", None; "i", Some Type.integer]
    (Some (Type.expression (Type.primitive "A")));

  assert_instantiated
    {|
      class A:
        def __init__(self, i: int = ...): ...
    |}
    "A"
    (normal [Type.integer])
    ["self", None; "i", Some Type.integer]
    (Some (Type.expression (Type.primitive "A")));
  assert_instantiated
    {|
      class A:
        def __init__(self, i: int = ...): ...
    |}
    "A"
    []
    ["self", None; "i", Some Type.integer]
    (Some (Type.expression (Type.primitive "A")));

  assert_instantiated
    {|
      class set(typing.Generic[_T]):
        def __init__(self, iterable: typing.Iterable[_T] = ...) -> None: ...
    |}
    "set"
    (normal [Type.iterable Type.integer])
    ["self", None; "iterable", Some (Type.iterable Type.integer)]
    (Some (Type.expression (Type.set Type.integer)));

  assert_instantiated
    {|
      class set(typing.Generic[_T]):
        def __init__(self, iterable: typing.Iterable[_T] = ...) -> None: ...
    |}
    "set"
    []
    ["self", None; "iterable", Some (Type.iterable Type.Top)]
    (Some (Type.expression (Type.set Type.Bottom)))


let test_function_overloading _ =
  let source =
    {|
      class int(): pass
      class str(): pass
      class float(): pass
      @overload
      def a(x: int)->int: ...
      @overload
      def a(x: str)->str: ...
      @overload
      def b(x:str)->str: ...
      @overload
      def b()->int: ...
      @overload
      def c(a:str, b:str)->str: ...
      @overload
      def c(a:int, b:str)->int: ...
      @overload
      def with_keywords( **kwargs:str)->int: ...
      @overload
      def with_keywords(x:str)->str: ...
    |}
  in
  assert_instantiated
    source
    "a"
    (normal [Type.integer])
    ["x", Some Type.integer]
    (Some (Type.expression Type.integer));
  assert_instantiated
    source
    "a"
    (normal [Type.string])
    ["x", Some Type.string]
    (Some (Type.expression Type.string));
  assert_instantiated
    source
    "a"
    (normal [Type.float])
    ["x", Some Type.integer]
    (Some (Type.expression Type.integer));
  assert_instantiated
    source
    "b"
    (normal [Type.string])
    ["x", Some Type.string]
    (Some (Type.expression Type.string));
  assert_instantiated
    source
    "b"
    []
    []
    (Some (Type.expression Type.integer));
  assert_instantiated
    source
    "b"
    (normal [Type.Object])
    ["x", Some Type.string]
    (Some (Type.expression Type.string));
  assert_instantiated
    source
    "c"
    (normal [Type.integer; Type.integer])
    ["a", Some Type.integer; "b", Some Type.string]
    (Some (Type.expression Type.integer));
  assert_instantiated
    source
    "c"
    (normal [Type.string; Type.integer])
    ["a", Some Type.string; "b", Some Type.string]
    (Some (Type.expression Type.string));

  assert_instantiated
    source
    "with_keywords"
    (normal [Type.string])
    ["x", Some Type.string]
    (Some (Type.expression Type.string))


let test_supertypes _ =
  let environment =
    (populate {|
      class foo(): pass
      class bar(foo): pass
    |}) in
  let module Handler = (val environment) in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  assert_equal
    (TypeOrder.successors order (Type.primitive "foo"))
    [Type.Object; Type.Top];
  assert_equal
    (TypeOrder.successors order (Type.primitive "bar"))
    [Type.primitive "foo"; Type.Object; Type.Top];

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
    (TypeOrder.successors
       order
       (Type.Parametric {
           Type.name = ~~"typing.Iterable";
           parameters = [Type.integer];
         }))
    [
      Type.Parametric {
        Type.name = ~~"typing.Generic";
        parameters = [Type.integer];
      };
      Type.Object;
      Type.Top;
    ]

let test_method_signature _ =
  let environment =
    populate {|
      def foo.foo(): ...
      def bar.bar(): pass
      class baz.baz(): pass
      def bar(): ...
      class object():
        def foo(): pass
        def bar(): pass
    |} in

  (* Check `method_signature`. *)
  assert_is_none
    (method_signature
       environment
       Type.Top
       { Call.name = !"bar"; arguments = [] }
       []);
  assert_is_some
    (method_signature
       environment
       Type.Object
       { Call.name = !"foo"; arguments = [] }
       []);
  assert_is_some
    (method_signature
       environment
       Type.Object
       { Call.name = !"bar"; arguments = [] }
       []);
  assert_is_none
    (method_signature
       environment
       Type.Object
       { Call.name = !"baz"; arguments = [] }
       []);

  let environment =
    populate {|
      class bar():
        def bar(): pass
        def both(self): pass
      class foo(bar):
        def foo(): pass
        def both(self, a: str): pass
    |} in
  assert_is_some
    (method_signature
       environment
       (Type.primitive "foo")
       { Call.name = !"foo"; arguments = [] }
       []);
  assert_is_some
    (method_signature
       environment
       (Type.primitive "bar")
       { Call.name = !"bar"; arguments = [] }
       []);
  assert_is_some
    (method_signature
       environment
       (Type.primitive "foo")
       { Call.name = !"bar"; arguments = [] }
       []);
  assert_is_none
    (method_signature
       environment
       (Type.primitive "bar")
       { Call.name = !"foo"; arguments = [] }
       []);

  (* Test that we don't overload with methods from the superclass. *)
  assert_is_none
    (method_signature
       environment
       (Type.primitive "foo")
       { Call.name = !"both"; arguments = [] }
       []);

  let environment =
    populate {|
      class bar():
        @overload
        def f(self)->str: ...
        @overload
        def f(self, a:int)->int: ...
    |}
  in
  let callee = method_signature
      environment
      (Type.primitive "bar")
      { Call.name = !"f"; arguments = [] }
      (List.map ~f:(~+) (normal [Type.primitive "bar"]))
  in
  begin
    match callee with
    | Some {
        Signature.instantiated = { Define.return_annotation = Some annotation; _ };
        _;
      } -> assert_equal (parse_annotation environment annotation) Type.integer
    | _ -> assert_unreached ()
  end;

  let environment =
    populate {|
      _T = typing.TypeVar('_T')
      class int(): pass
      class list(typing.Generic[_T]):
        def append(self, element: _T) -> typing.List[_T]: pass
        def head(self) -> _T: pass
        def tail(self) -> typing.List[_T]: pass

      _K = typing.TypeVar('_K')
      _V = typing.TypeVar('_V')
      class dict(typing.Generic[_K, _V]):
        def get(key: _K) -> typing.Optional[_V]: pass
        def update(self, m: typing.Dict[_K, _V], **kwargs) -> None: pass
        def add(self, *args) -> None: pass
    |} in
  let callee =
    method_signature
      environment
      (Type.list Type.integer)
      { Call.name = !"head"; arguments = [] }
      (List.map ~f:(~+) (normal [Type.list Type.integer])) in
  begin
    match callee with
    | Some {
        Signature.instantiated = { Define.name; return_annotation = Some annotation; _ };
        _;
      }->
        assert_equal name (Access.create "head");
        assert_equal
          (parse_annotation environment annotation)
          Type.integer
    | _ ->
        assert_unreached ()
  end;
  let callee =
    method_signature
      environment
      (Type.list Type.integer)
      { Call.name = !"append"; arguments = [] }
      (List.map ~f:(~+) (normal [Type.list Type.integer; Type.integer])) in
  begin
    match callee with
    | Some {
        Signature.instantiated = { Define.name; return_annotation = Some annotation; _ };
        _;
      }->
        assert_equal name (Access.create "append");
        assert_equal
          ~printer:(Format.asprintf "%a" Type.pp)
          (parse_annotation environment annotation)
          (Type.list Type.integer)
    | _ ->
        assert_unreached ()
  end;

  (* Two type parameters. *)
  let callee =
    method_signature
      environment
      (Type.dictionary ~key:Type.string ~value:Type.integer)
      { Call.name = !"get"; arguments = [] }
      (List.map ~f:(~+) (normal [Type.dictionary ~key:Type.string ~value:Type.integer])) in
  begin
    match callee with
    | Some {
        Signature.instantiated = { Define.name; return_annotation = Some annotation; _ };
        _;
      }->
        assert_equal name (Access.create "get");
        assert_equal
          ~printer:(Format.asprintf "%a" Type.pp)
          (parse_annotation environment annotation)
          (Type.Optional Type.integer)
    | _ ->
        assert_unreached ()
  end;

  (* Keyword arguments. *)
  let callee =
    method_signature
      environment
      (Type.dictionary ~key:Type.string ~value:Type.integer)
      { Call.name = !"update"; arguments = [] }
      (List.map
         ~f:(~+)
         (normal [
             Type.dictionary ~key:Type.string ~value:Type.integer;
             Type.dictionary ~key:Type.string ~value:Type.integer;
           ])) in
  begin
    match callee with
    | Some { Signature.instantiated = { Define.name; _ }; _ } ->
        assert_equal name (Access.create "update");
    | _ ->
        assert_unreached ()
  end;

  (* Variable arguments. *)
  let callee =
    method_signature
      environment
      (Type.dictionary ~key:Type.string ~value:Type.integer)
      { Call.name = !"add"; arguments = [] }
      (List.map ~f:(~+) (normal [Type.dictionary ~key:Type.string ~value:Type.integer])) in
  begin
    match callee with
    | Some { Signature.instantiated = { Define.name; _ }; _ } ->
        assert_equal name (Access.create "add");
    | _ ->
        assert_unreached ()
  end


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
  assert_true
    (is_defined
       environment
       (Type.Parametric {
           Type.name = ~~"baz.baz";
           parameters = [Type.integer];
         }));
  assert_is_some
    (class_definition environment (Type.primitive "baz.baz"));

  assert_false (is_defined environment (Type.primitive "bar.bar"));
  assert_false
    (is_defined
       environment
       (Type.Parametric {
           Type.name = ~~"bar.bar";
           parameters = [Type.integer];
         }));
  assert_is_none
    (class_definition environment (Type.primitive "bar.bar"));

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
      populate
        ~source_root:(Path.current_working_directory ())
        ~check_dependency_exists:true
        source
    in
    assert_equal
      (Environment.dependencies environment "subdirectory/b.py")
      (Some ["test.py"]);
    assert_equal
      (Environment.dependencies environment "a.py")
      (Some ["test.py"]);
  in
  with_bracket_chdir context (bracket_tmpdir context) create_files_and_test

let test_register_dependencies _ =
  let environment = Environment.Builder.create ~configuration () in
  let (module Handler: Environment.Handler) = Environment.handler ~configuration environment in
  let source = {|
         import a # a is added here
         from subdirectory.b import c # subdirectory.b is added here
         from . import ignored # no dependency created here
      |}
  in
  Environment.register_dependencies
    ~check_dependency_exists:false
    (module Handler)
    (parse ~path:"test.py" source);
  assert_equal
    (Environment.dependencies (module Handler) "subdirectory/b.py")
    (Some ["test.py"]);
  assert_equal
    (Environment.dependencies (module Handler) "a.py")
    (Some ["test.py"])


let test_purge _ =
  let environment = Environment.Builder.create ~configuration () in
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
  Environment.populate
    ~configuration
    ~check_dependency_exists:false
    handler
    [parse ~path:"test.py" source];
  assert_is_some (Handler.class_definition (Type.primitive "baz.baz"));
  assert_is_some (Handler.function_definitions (Access.create "foo"));
  assert_is_some (Handler.aliases (Type.primitive "_T"));
  assert_equal (Handler.dependencies "a.py") (Some ["test.py"]);

  Handler.purge (File.Handle.create "test.py");

  assert_is_none (Handler.class_definition (Type.primitive "baz.baz"));
  assert_is_none (Handler.function_definitions (Access.create "foo"));
  assert_is_none (Handler.aliases (Type.primitive "_T"));
  assert_equal (Handler.dependencies "a.py") (Some [])


let () =
  Analysis.Type.TypeCache.disable ();
  "environment">:::[
    "create">::test_create;
    "copy">::test_copy;
    "register_class_definitions">::test_register_class_definitions;
    "register_aliases">::test_register_aliases;
    "connect_definition">::test_connect_definition;
    "register_globals">::test_register_globals;
    "register_functions">::test_register_functions;
    "populate">::test_populate;
    "infer_protocols">::test_infer_protocols;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "meet">::test_meet;
    "function_signature">::test_function_signature;
    "function_signature_variable_instantiate">::test_function_signature_variable_instantiation;
    "function_signature_starred">::test_function_signature_starred;
    "function_signature_constructor">::test_function_signature_constructor;
    "function_overloading">::test_function_overloading;
    "supertypes">::test_supertypes;
    "method_signature">::test_method_signature;
    "class_definition">::test_class_definition;
    "protocols">::test_protocols;
    "modules">::test_modules;
    "import_dependencies">::test_import_dependencies;
    "register_dependencies">::test_register_dependencies;
    "purge">::test_purge;
  ]
  |> run_test_tt_main
