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
open TypeCheck
open Test

module DefaultContext = struct
  let qualifier = Reference.empty

  let constraint_solving_style = Configuration.Analysis.default_constraint_solving_style

  let debug = false

  let define = +Test.mock_define

  let resolution_fixpoint = None

  let error_map = None

  module Builder = Callgraph.NullBuilder
end

let create_annotation_store ?(immutables = []) annotations =
  let immutables = String.Map.of_alist_exn immutables in
  let annotify (name, annotation) =
    let annotation =
      let create annotation =
        match Map.find immutables name with
        | Some original ->
            Refinement.Unit.create
              (Annotation.create_immutable ~original:(Some original) annotation)
        | _ -> Refinement.Unit.create (Annotation.create_mutable annotation)
      in
      create annotation
    in
    !&name, annotation
  in
  {
    Refinement.Store.annotations = List.map annotations ~f:annotify |> Reference.Map.of_alist_exn;
    temporary_annotations = Reference.Map.empty;
  }


let assert_annotation_store ~expected actual =
  let actual = Resolution.annotation_store actual in
  let compare_annotation_store
      {
        Refinement.Store.annotations = left_annotations;
        temporary_annotations = left_temporary_annotations;
      }
      {
        Refinement.Store.annotations = right_annotations;
        temporary_annotations = right_temporary_annotations;
      }
    =
    let equal_map = Reference.Map.equal [%equal: Refinement.Unit.t] in
    equal_map left_annotations right_annotations
    && equal_map left_temporary_annotations right_temporary_annotations
  in
  let pp_annotation_store formatter { Refinement.Store.annotations; temporary_annotations } =
    let annotation_to_string (name, refinement_unit) =
      Format.asprintf "%a -> %a" Reference.pp name Refinement.Unit.pp refinement_unit
    in
    let printed_annotations =
      Map.to_alist annotations |> List.map ~f:annotation_to_string |> String.concat ~sep:"\n"
    in
    let printed_temporary_annotations =
      Map.to_alist temporary_annotations
      |> List.map ~f:annotation_to_string
      |> String.concat ~sep:"\n"
    in
    Format.fprintf
      formatter
      "Annotations: %s\nTemporaryAnnotations: %s"
      printed_annotations
      printed_temporary_annotations
  in
  assert_equal
    ~cmp:compare_annotation_store
    ~printer:(Format.asprintf "%a" pp_annotation_store)
    ~pp_diff:(diff ~print:pp_annotation_store)
    expected
    actual


module Create (Context : TypeCheck.Context) = struct
  let create ?(bottom = false) ?(immutables = []) ~resolution annotations =
    let module State = State (Context) in
    if bottom then
      State.unreachable
    else
      let resolution =
        let annotation_store = create_annotation_store ~immutables annotations in
        Resolution.with_annotation_store resolution ~annotation_store
      in
      State.create ~resolution
end

let description ~resolution error =
  let ast_environment =
    Resolution.global_resolution resolution |> GlobalResolution.ast_environment
  in
  Error.instantiate
    ~show_error_traces:false
    ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment)
    error
  |> Error.Instantiated.description


let test_initial context =
  let assert_initial ?parent ?(environment = "") ?(immutables = []) ~annotations define =
    let define =
      match parse_single_statement define with
      | { Node.value = Define ({ signature; _ } as define); _ } ->
          let signature = { signature with parent = parent >>| Reference.create } in
          { define with signature }
      | _ -> failwith "Unable to parse define."
    in
    let module Context = struct
      let debug = false

      let constraint_solving_style = Configuration.Analysis.default_constraint_solving_style

      let qualifier = Reference.empty

      let define = +define

      let resolution_fixpoint = Some (LocalAnnotationMap.empty ())

      let error_map = Some (LocalErrorMap.empty ())

      module Builder = Callgraph.NullBuilder
    end
    in
    let resolution =
      ScratchProject.setup ~context ["test.py", environment] |> ScratchProject.build_resolution
    in
    let module Create = Create (Context) in
    let state = Create.create ~immutables ~resolution annotations in
    let module State = State (Context) in
    let assert_state_equal =
      assert_equal
        ~cmp:State.equal
        ~printer:(Format.asprintf "%a" State.pp)
        ~pp_diff:(diff ~print:State.pp)
    in
    let initial =
      let variables =
        let extract_variables { Node.value = { Expression.Parameter.annotation; _ }; _ } =
          match annotation with
          | None -> []
          | Some annotation ->
              let annotation =
                GlobalResolution.parse_annotation
                  (Resolution.global_resolution resolution)
                  annotation
              in
              Type.Variable.all_free_variables annotation
        in
        List.concat_map define.signature.parameters ~f:extract_variables
        |> List.dedup_and_sort ~compare:Type.Variable.compare
      in
      let add_variable resolution variable = Resolution.add_type_variable resolution ~variable in
      let resolution = List.fold variables ~init:resolution ~f:add_variable in
      State.initial ~resolution
    in
    assert_state_equal state initial
  in
  assert_initial
    "def foo(x: int) -> None: ..."
    ~immutables:["x", Type.integer]
    ~annotations:["x", Type.integer];
  assert_initial
    "def foo(x: int = 1.0) -> None: ..."
    ~immutables:["x", Type.integer]
    ~annotations:["x", Type.integer];
  assert_initial ~annotations:["x", Type.Any] "def foo(x = 1.0) -> None: ...";
  assert_initial
    "def foo(x: int) -> int: ..."
    ~immutables:["x", Type.integer]
    ~annotations:["x", Type.integer];
  assert_initial
    "def foo(x: float, y: str) -> None: ..."
    ~immutables:["x", Type.float; "y", Type.string]
    ~annotations:["x", Type.float; "y", Type.string];
  assert_initial "def foo(x) -> None: ..." ~annotations:["x", Type.Any];
  assert_initial
    "def foo(x: typing.Any) -> None: ..."
    ~immutables:["x", Type.Any]
    ~annotations:["x", Type.Any];
  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    "def __eq__(self, other: object) -> None: ..."
    ~immutables:["other", Type.object_primitive]
    ~annotations:["self", Type.Primitive "Foo"; "other", Type.object_primitive];
  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    "def foo(self) -> None: ..."
    ~annotations:["self", Type.Primitive "Foo"];
  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    "@staticmethod\ndef foo(a) -> None: ..."
    ~annotations:["a", Type.Any];
  assert_initial
    ~environment:"T = typing.TypeVar('T')"
    "def foo(x: test.T) -> None: ..."
    ~immutables:["x", Type.Variable.mark_all_variables_as_bound (Type.variable "test.T")]
    ~annotations:["x", Type.Variable.mark_all_variables_as_bound (Type.variable "test.T")]


let test_less_or_equal context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let create =
    let module Create = Create (DefaultContext) in
    Create.create ~resolution
  in
  let module State = State (DefaultContext) in
  (* == *)
  assert_true (State.less_or_equal ~left:(create []) ~right:(create []));
  assert_true
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["x", Type.integer]));
  (* <= *)
  assert_true (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create []));
  assert_true (State.less_or_equal ~left:(create ["x", Type.Top]) ~right:(create []));
  assert_true
    (State.less_or_equal
       ~left:(create ["x", Type.integer; "y", Type.integer])
       ~right:(create ["x", Type.integer]));
  (* > *)
  assert_false (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.integer]));
  assert_false (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.Top]));
  (* not comparable *)
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["x", Type.string]));
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["y", Type.integer]))


let test_widen context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let create =
    let module Create = Create (DefaultContext) in
    Create.create ~resolution
  in
  let module State = State (DefaultContext) in
  let assert_state_equal =
    assert_equal
      ~cmp:State.equal
      ~printer:(Format.asprintf "%a" State.pp)
      ~pp_diff:(diff ~print:State.pp)
  in
  let widening_threshold = 3 in
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:0)
    (create ["x", Type.union [Type.integer; Type.string]]);
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:(widening_threshold + 1))
    (create ["x", Type.Top])


let test_check_annotation context =
  let assert_check_annotation source variable_name descriptions =
    let resolution =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
    in
    let module State = State (DefaultContext) in
    let errors =
      let expression =
        let location =
          {
            Location.start = { line = 1; column = 1 };
            stop = { line = 1; column = 1 + String.length variable_name };
          }
        in
        let value = Expression.Expression.Name (Expression.create_name ~location variable_name) in
        Node.create ~location value
      in
      State.parse_and_check_annotation ~resolution expression |> fst |> AnalysisError.deduplicate
    in
    let errors = List.map ~f:(description ~resolution) errors in
    assert_equal
      ~cmp:(List.equal String.equal)
      ~printer:(String.concat ~sep:"\n")
      descriptions
      errors
  in
  assert_check_annotation
    ""
    "x"
    ["Undefined or invalid type [11]: Annotation `x` is not defined as a type."];
  assert_check_annotation
    "x: int = 1"
    "test.x"
    ["Undefined or invalid type [11]: Annotation `test.x` is not defined as a type."];
  assert_check_annotation
    "x: typing.Type[int] = int"
    "test.x"
    ["Undefined or invalid type [11]: Annotation `test.x` is not defined as a type."];
  assert_check_annotation "x = int" "test.x" [];
  assert_check_annotation
    "x: typing.Any"
    "test.x"
    ["Undefined or invalid type [11]: Annotation `test.x` is not defined as a type."];
  assert_check_annotation "x = typing.Any" "test.x" [];
  assert_check_annotation "x: typing_extensions.TypeAlias = typing.Any" "test.x" [];
  assert_check_annotation {|
      class Foo: ...
      x = Foo
    |} "test.x" [];
  assert_check_annotation
    {|
      class Foo: ...
      x = Foo()
    |}
    "test.x"
    ["Undefined or invalid type [11]: Annotation `test.x` is not defined as a type."];
  assert_check_annotation
    {|
      class Foo:
        def __getitem__(self, other) -> typing.Any:
          ...
      x = Foo[Undefined]
    |}
    "test.x"
    ["Undefined or invalid type [11]: Annotation `test.x` is not defined as a type."]


let assert_resolved ~context sources expression expected =
  let resolution = ScratchProject.setup ~context sources |> ScratchProject.build_resolution in
  let resolved =
    Resolution.resolve_expression_to_type resolution (parse_single_expression expression)
  in
  assert_equal ~printer:Type.show ~cmp:Type.equal expected resolved


let test_module_exports context =
  let assert_exports_resolved expression expected =
    let sources =
      [
        "implementing.py", {|
        def function() -> int: ...
        constant: int = 1
      |};
        ( "exporting.py",
          {|
        from implementing import function, constant
        from implementing import function as aliased
        from indirect import cyclic
      |}
        );
        "indirect.py", {|
        from exporting import constant, cyclic
      |};
        "wildcard.py", {|
        from exporting import *
      |};
        ( "exporting_wildcard_default.py",
          {|
        from implementing import function, constant
        from implementing import function as aliased
        __all__ = ["constant"]
      |}
        );
        "wildcard_default.py", {|
        from exporting_wildcard_default import *
      |};
      ]
    in
    assert_resolved ~context sources expression expected
  in
  assert_exports_resolved "implementing.constant" Type.integer;
  assert_exports_resolved "implementing.function()" Type.integer;
  assert_exports_resolved "implementing.undefined" Type.Top;
  assert_exports_resolved "exporting.constant" Type.integer;
  assert_exports_resolved "exporting.function()" Type.integer;
  assert_exports_resolved "exporting.aliased()" Type.integer;
  assert_exports_resolved "exporting.undefined" Type.Top;
  assert_exports_resolved "indirect.constant" Type.integer;
  assert_exports_resolved "indirect.cyclic" Type.Top;
  assert_exports_resolved "wildcard.constant" Type.integer;
  assert_exports_resolved "wildcard.cyclic" Type.Top;
  assert_exports_resolved "wildcard.aliased()" Type.integer;
  assert_exports_resolved "wildcard_default.constant" Type.integer;
  assert_exports_resolved "wildcard_default.aliased()" Type.Any;
  let assert_fixpoint_stop =
    assert_resolved
      ~context
      [
        "loop/b.py", {|
            b: int = 1
          |};
        "loop/a.py", {|
            from loop.b import b
          |};
        "loop/a.py", {|
            from loop.a import b
          |};
        "no_loop/b.py", {|
            b: int = 1
          |};
        "no_loop/a.py", {|
            from no_loop.b import b as c
          |};
        "no_loop/__init__.py", {|
            from no_loop.a import c
          |};
      ]
  in
  assert_fixpoint_stop "loop.b" Type.Top;
  assert_fixpoint_stop "no_loop.c" Type.integer


let test_object_callables context =
  let assert_resolved expression annotation =
    assert_resolved
      ~context
      [
        ( "module.py",
          {|
            _K = typing.TypeVar('_K')
            _V = typing.TypeVar('_V')
            _T = typing.TypeVar('_T')

            class object:
              def __init__(self) -> None:
                pass
            class Call(object, typing.Generic[_K, _V]):
              attribute: _K
              generic_callable: typing.Callable[[_K], _V]
              def __call__(self) -> _V: ...

            class Submodule(Call[_T, _T], typing.Generic[_T]):
              pass

            call: Call[int, str] = ...
            meta: typing.Type[Call[int, str]] = ...
            submodule: Submodule[int] = ...
          |}
        );
      ]
      expression
      (Type.create ~aliases:Type.empty_aliases (parse_single_expression annotation))
  in
  assert_resolved "module.call" "module.Call[int, str]";
  assert_resolved "module.call.attribute" "int";
  assert_resolved "module.call.generic_callable" "typing.Callable[[int], str]";
  assert_resolved "module.call()" "str";
  assert_resolved "module.meta" "typing.Type[module.Call[int, str]]";
  assert_resolved "module.meta()" "module.Call[int, str]";
  assert_resolved "module.submodule.generic_callable" "typing.Callable[[int], int]"


let test_callable_selection context =
  let assert_resolved source expression annotation =
    assert_resolved
      ~context
      ["test.py", source]
      expression
      (Type.create ~aliases:Type.empty_aliases (parse_single_expression annotation))
  in
  assert_resolved "call: typing.Callable[[], int]" "test.call()" "int";
  assert_resolved "call: typing.Callable[[int], int]" "test.call()" "int"


type parameter_kind =
  | NamedParameter
  | VariableParameter
  | KeywordParameter

let test_forward_expression context =
  let assert_forward
      ?(precondition = [])
      ?(postcondition = [])
      ?(environment = "")
      expression
      annotation
    =
    let expression =
      let expression = parse expression |> Preprocessing.preprocess in
      expression
      |> function
      | { Source.statements = [{ Node.value = Expression expression; _ }]; _ } -> expression
      | _ -> failwith "Unable to extract expression"
    in
    let global_resolution =
      ScratchProject.setup ~context ["test.py", environment]
      |> ScratchProject.build_global_resolution
    in
    let new_resolution, resolved =
      let resolution =
        let annotation_store = create_annotation_store precondition in
        TypeCheck.resolution ~annotation_store global_resolution (module TypeCheck.DummyContext)
      in

      Resolution.resolve_expression resolution expression
    in
    assert_annotation_store ~expected:(create_annotation_store postcondition) new_resolution;
    assert_equal ~cmp:Type.equal ~printer:Type.show annotation resolved
  in
  (* Await. *)
  assert_forward "await awaitable_int()" Type.integer;
  assert_forward "await undefined" Type.Any;

  (* Boolean operator. *)
  assert_forward "1 or 'string'" (Type.literal_integer 1);
  assert_forward "1 and 'string'" (Type.literal_string "string");
  assert_forward "undefined or 1" Type.Top;
  assert_forward "1 or undefined" (Type.literal_integer 1);
  assert_forward "undefined and undefined" Type.Top;
  assert_forward
    ~precondition:["y", Type.string]
    ~postcondition:["y", Type.string]
    "0 or y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.string]
    ~postcondition:["y", Type.string]
    "False or y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.string]
    ~postcondition:["y", Type.string]
    "None or y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.integer]
    ~postcondition:["y", Type.integer]
    "'' or y"
    Type.integer;
  assert_forward
    ~precondition:["y", Type.integer]
    ~postcondition:["y", Type.integer]
    "b'' or y"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.NoneType; "y", Type.integer]
    ~postcondition:["x", Type.NoneType; "y", Type.integer]
    "x or y"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.literal_integer 0; "y", Type.integer]
    ~postcondition:["x", Type.literal_integer 0; "y", Type.integer]
    "x or y"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.Literal (Type.Boolean false); "y", Type.integer]
    ~postcondition:["x", Type.Literal (Type.Boolean false); "y", Type.integer]
    "x or y"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.union [Type.NoneType; Type.integer; Type.string]]
    ~postcondition:["x", Type.union [Type.NoneType; Type.integer; Type.string]]
    "x or 1"
    (Type.Union [Type.integer; Type.string]);
  assert_forward
    ~precondition:["x", Type.union [Type.NoneType; Type.literal_integer 0]; "y", Type.string]
    ~postcondition:["x", Type.union [Type.NoneType; Type.literal_integer 0]; "y", Type.string]
    "x or y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.string]
    ~postcondition:["y", Type.string]
    "1 and y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.string]
    ~postcondition:["y", Type.string]
    "1 and y"
    Type.string;
  assert_forward
    ~precondition:["y", Type.integer]
    ~postcondition:["y", Type.integer]
    "'foo' and y"
    Type.integer;
  assert_forward
    ~precondition:["y", Type.integer]
    ~postcondition:["y", Type.integer]
    "b'foo' and y"
    Type.integer;
  assert_forward
    ~precondition:
      ["x", Type.union [Type.literal_integer 1; Type.literal_integer 2]; "y", Type.string]
    ~postcondition:
      ["x", Type.union [Type.literal_integer 1; Type.literal_integer 2]; "y", Type.string]
    "x and y"
    Type.string;
  let assert_optional_forward ?(postcondition = ["x", Type.optional Type.integer]) =
    assert_forward ~precondition:["x", Type.optional Type.integer] ~postcondition
  in
  assert_optional_forward "x or 1" Type.integer;
  assert_optional_forward "x or x" (Type.optional Type.integer);
  assert_optional_forward "x and 1" (Type.optional Type.integer);
  assert_optional_forward "1 and x" (Type.optional Type.integer);
  assert_optional_forward "x and x" (Type.optional Type.integer);

  (* Call *)
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    "x.add_key('string')"
    Type.none;
  assert_forward
    ~precondition:["unknown", Type.Top]
    ~postcondition:["unknown", Type.Top]
    ~environment:
      {|
        class Foo:
          def __init__(self) -> None:
            self.attribute: int = 1
        def foo(x: int) -> typing.Optional[Foo]: ...
      |}
    "test.foo(unknown).attribute"
    Type.Top;
  assert_forward
    ~precondition:["foo_instance", Type.Primitive "Foo"]
    ~postcondition:["foo_instance", Type.Primitive "Foo"]
    ~environment:
      {|
        class Foo:
          def __init__(self) -> None:
            self.attribute: int = 1
        def foo(x: typing.Any) -> Foo: ...
      |}
    "test.foo(foo_instance.unknown).attribute"
    Type.integer;
  assert_forward
    ~precondition:["foo_instance", Type.Primitive "test.Foo"]
    ~postcondition:["foo_instance", Type.Primitive "test.Foo"]
    ~environment:
      {|
        class Foo:
          def __init__(self) -> None:
            self.attribute: int = 1
        def foo(x: typing.Any) -> Foo: ...
      |}
    "test.foo(foo_instance.unknown).another_unknown"
    Type.Top;

  (* Comparison operator. *)
  assert_forward "1 < 2" Type.bool;
  assert_forward "1 < 2 < 3" Type.bool;
  assert_forward "1 is 2" Type.bool;
  assert_forward
    ~precondition:["container", Type.list Type.integer]
    ~postcondition:["container", Type.list Type.integer]
    "1 in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.list Type.integer]
    ~postcondition:["container", Type.list Type.integer]
    "1 not in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.iterator Type.integer]
    ~postcondition:["container", Type.iterator Type.integer]
    "1 in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.iterator Type.integer]
    ~postcondition:["container", Type.iterator Type.integer]
    "1 not in container"
    Type.bool;
  assert_forward
    ~environment:
      {|
      class MetaFoo(type):
        def __contains__(self, x:int) -> bool: ...
      class Foo(metaclass=MetaFoo):
        def foo(self) -> int:
          return 9
    |}
    ~precondition:["Container", Type.meta (Type.Primitive "test.Foo")]
    ~postcondition:["Container", Type.meta (Type.Primitive "test.Foo")]
    "1 in Container"
    Type.bool;
  let dictionary_set_union =
    Type.Union [Type.dictionary ~key:Type.integer ~value:Type.string; Type.set Type.integer]
  in
  assert_forward
    ~precondition:["Container", dictionary_set_union]
    ~postcondition:["Container", dictionary_set_union]
    "1 in Container"
    Type.bool;
  assert_forward "undefined < 1" Type.bool;
  assert_forward "undefined == undefined" Type.Any;
  assert_forward
    ~environment:{|
        class Foo:
          field: int
      |}
    ~precondition:
      ["x", Type.Primitive "test.Foo"; "y", Type.Literal (String (LiteralValue "field"))]
    ~postcondition:
      ["x", Type.Primitive "test.Foo"; "y", Type.Literal (String (LiteralValue "field"))]
    "getattr(x, y)"
    Type.integer;
  assert_forward
    ~environment:{|
        class Foo:
          field: int
      |}
    ~precondition:["x", Type.Primitive "test.Foo"; "y", Type.string]
    ~postcondition:["x", Type.Primitive "test.Foo"; "y", Type.string]
    "getattr(x, y)"
    Type.Any;
  assert_forward
    ~environment:{|
        class Foo:
          field: int
      |}
    ~precondition:
      ["x", Type.Primitive "test.Foo"; "y", Type.Literal (String (LiteralValue "field"))]
    ~postcondition:
      ["x", Type.Primitive "test.Foo"; "y", Type.Literal (String (LiteralValue "field"))]
    "getattr(x, y, None)"
    Type.integer;

  (* Complex literal. *)
  assert_forward "1j" Type.complex;
  assert_forward "1" (Type.literal_integer 1);
  assert_forward {|""|} (Type.literal_string "");
  assert_forward {|b""|} (Type.literal_bytes "");

  (* Dictionaries. *)
  assert_forward "{1: 1}" (Type.dictionary ~key:Type.integer ~value:Type.integer);
  assert_forward "{1: 'string'}" (Type.dictionary ~key:Type.integer ~value:Type.string);
  assert_forward "{b'': ''}" (Type.dictionary ~key:Type.bytes ~value:Type.string);
  assert_forward
    "{1: 1, 'string': 1}"
    (Type.dictionary ~key:(Type.union [Type.integer; Type.string]) ~value:Type.integer);
  assert_forward
    "{1: 1, 1: 'string'}"
    (Type.dictionary ~key:Type.integer ~value:(Type.union [Type.integer; Type.string]));
  assert_forward "{**{1: 1}}" (Type.dictionary ~key:Type.integer ~value:Type.integer);
  assert_forward
    "{**{1: 1}, **{'a': 'b'}}"
    (Type.dictionary
       ~key:(Type.union [Type.integer; Type.string])
       ~value:(Type.union [Type.integer; Type.string]));
  assert_forward "{1: 'string', **{undefined: 1}}" Type.Top;
  assert_forward "{undefined: 1}" (Type.dictionary ~key:Type.Top ~value:Type.integer);
  assert_forward "{1: undefined}" (Type.dictionary ~key:Type.integer ~value:Type.Top);
  assert_forward
    "{1: undefined, undefined: undefined}"
    (Type.dictionary ~key:Type.Top ~value:Type.Top);
  assert_forward
    "{key: value for key in [1] for value in ['string']}"
    (Type.dictionary ~key:Type.integer ~value:Type.string);

  (* Ellipsis. *)
  assert_forward "..." Type.Any;

  (* False literal. *)
  assert_forward "False" (Type.Literal (Type.Boolean false));

  (* Float literal. *)
  assert_forward "1.0" Type.float;

  (* Generator expressions. *)
  assert_forward "(element for element in [1])" (Type.generator_expression Type.integer);
  assert_forward "(element for element in [])" (Type.generator_expression Type.Any);
  assert_forward
    "((element, independent) for element in [1] for independent in ['string'])"
    (Type.generator_expression (Type.tuple [Type.integer; Type.string]));
  assert_forward
    "(nested for element in [[1]] for nested in element)"
    (Type.generator_expression Type.integer);
  assert_forward "(undefined for element in [1])" (Type.generator_expression Type.Top);
  assert_forward "(element for element in undefined)" (Type.generator_expression Type.Any);

  (* Lambda. *)
  let callable ~parameters ~annotation =
    let parameters =
      let open Type.Callable in
      let to_parameter (name, kind, default) =
        match kind with
        | NamedParameter -> Parameter.Named { name; annotation = Type.Any; default }
        | VariableParameter -> Parameter.Variable (Concrete Type.Any)
        | KeywordParameter -> Parameter.Keywords Type.Any
      in
      Defined (List.map parameters ~f:to_parameter)
    in
    Type.Callable.create ~parameters ~annotation ()
  in
  assert_forward "lambda: 1" (callable ~parameters:[] ~annotation:Type.integer);
  assert_forward
    "lambda parameter: parameter"
    (callable ~parameters:["parameter", NamedParameter, false] ~annotation:Type.Any);
  assert_forward
    "lambda parameter=1: parameter"
    (callable ~parameters:["parameter", NamedParameter, true] ~annotation:Type.Any);
  assert_forward
    "lambda *parameter: 42"
    (callable ~parameters:["parameter", VariableParameter, false] ~annotation:Type.integer);
  assert_forward
    "lambda **parameter: 42"
    (callable ~parameters:["parameter", KeywordParameter, false] ~annotation:Type.integer);
  assert_forward "lambda: undefined" (callable ~parameters:[] ~annotation:Type.Top);

  (* Lists. *)
  Type.Variable.Namespace.reset ();
  let empty_list =
    Type.list (Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped)
  in
  Type.Variable.Namespace.reset ();
  assert_forward "[]" empty_list;
  assert_forward "[1]" (Type.list Type.integer);
  assert_forward "[1, 'string']" (Type.list (Type.union [Type.integer; Type.string]));
  assert_forward "[undefined]" (Type.list Type.Top);
  assert_forward "[undefined, undefined]" (Type.list Type.Top);
  assert_forward "[element for element in [1]]" (Type.list Type.integer);
  assert_forward "[1 for _ in [1]]" (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "[*x]"
    (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "[1, *x]"
    (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "['', *x]"
    (Type.list (Type.union [Type.string; Type.integer]));

  (* Name. *)
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x"
    Type.integer;

  (* Sets. *)
  assert_forward "{1}" (Type.set Type.integer);
  assert_forward "{1, 'string'}" (Type.set (Type.union [Type.integer; Type.string]));
  assert_forward "{undefined}" (Type.set Type.Top);
  assert_forward "{undefined, undefined}" (Type.set Type.Top);
  assert_forward "{element for element in [1]}" (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "{*x}"
    (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "{1, *x}"
    (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.set Type.integer]
    ~postcondition:["x", Type.set Type.integer]
    "{'', *x}"
    (Type.set (Type.union [Type.string; Type.integer]));

  (* Starred expressions. *)
  assert_forward "*1" Type.Top;
  assert_forward "*undefined" Type.Top;

  (* String literals. *)
  assert_forward "'string'" (Type.literal_string "string");
  assert_forward "f'string'" Type.string;
  assert_forward "f'string{1}'" Type.string;
  assert_forward "f'string{undefined}'" Type.string;

  (* Ternaries. *)
  assert_forward "3 if True else 1" (Type.union [Type.literal_integer 3; Type.literal_integer 1]);
  assert_forward
    "True if True else False"
    (Type.union [Type.Literal (Type.Boolean true); Type.Literal (Type.Boolean false)]);
  assert_forward
    "'foo' if True else 'bar'"
    (Type.union [Type.literal_string "foo"; Type.literal_string "bar"]);
  assert_forward "1.0 if True else 1" Type.float;
  assert_forward "1 if True else 1.0" Type.float;
  assert_forward "undefined if True else 1" Type.Top;
  assert_forward "1 if undefined else 1" (Type.literal_integer 1);
  assert_forward "1 if True else undefined" Type.Top;
  assert_forward "undefined if undefined else undefined" Type.Top;
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x if x is not None else 32"
    Type.integer;

  (* True literal. *)
  assert_forward "True" (Type.Literal (Boolean true));

  (* Tuples. *)
  assert_forward "1," (Type.tuple [Type.literal_integer 1]);
  assert_forward "1, 'string'" (Type.tuple [Type.literal_integer 1; Type.literal_string "string"]);
  assert_forward "undefined," (Type.tuple [Type.Top]);
  assert_forward "undefined, undefined" (Type.tuple [Type.Top; Type.Top]);

  (* Unary expressions. *)
  assert_forward "not 1" Type.bool;
  assert_forward "not undefined" Type.bool;
  assert_forward "+1" (Type.literal_integer 1);
  assert_forward "~1" Type.integer;
  assert_forward "-undefined" Type.Any;

  (* Walrus operator. *)
  assert_forward
    "(x := True)"
    (Type.Literal (Boolean true))
    ~postcondition:["x", Type.Literal (Boolean true)];

  (* Broadcasts *)
  assert_forward
    ~environment:
      {|
      Ts = pyre_extensions.TypeVarTuple("Ts")
      Rs = pyre_extensions.TypeVarTuple("Rs")
      def foo(
        x: typing.Tuple[pyre_extensions.Unpack[Ts]],
        y: typing.Tuple[pyre_extensions.Unpack[Rs]]
      ) -> pyre_extensions.Broadcast[
        typing.Tuple[pyre_extensions.Unpack[Ts]],
        typing.Tuple[pyre_extensions.Unpack[Rs]],
      ]: ...
    |}
    "foo((2, 2), (3, 3))"
    Type.Any;

  (* Meta-types *)
  assert_forward
    "typing.Optional[int]"
    (* TODO (T65870531): This should be typing.Union or typing._GenericAlias *)
    (Type.meta (Type.optional Type.integer));
  assert_forward
    "typing.Callable[[int, str], int]"
    (Type.meta (Type.Callable.create ~annotation:Type.integer ()));
  assert_forward "typing_extensions.Literal[1, 2, 3]" (Type.meta Type.Any);
  assert_forward
    "typing.ClassVar[int]"
    (Type.meta (Type.parametric "typing.ClassVar" [Single Type.integer]));
  assert_forward "typing.Union[int, str]" (Type.meta Type.Any);

  (* Resolved annotation field. *)
  let assert_annotation ?(precondition = []) ?(environment = "") expression annotation =
    let expression =
      let expression = parse expression in
      expression
      |> function
      | { Source.statements = [{ Node.value = Expression expression; _ }]; _ } -> expression
      | _ -> failwith "Unable to extract expression"
    in
    let resolution =
      let global_resolution =
        ScratchProject.setup ~context ["test.py", environment]
        |> ScratchProject.build_global_resolution
      in
      let annotation_store = create_annotation_store precondition in
      TypeCheck.resolution ~annotation_store global_resolution (module TypeCheck.DummyContext)
    in

    let resolved_annotation = Resolution.resolve_expression_to_annotation resolution expression in
    assert_equal ~cmp:Annotation.equal ~printer:Annotation.show annotation resolved_annotation
  in
  assert_annotation ~environment:"x = 1" "test.x" (Annotation.create_immutable Type.integer);
  assert_annotation
    ~environment:"x: typing.Union[int, str] = 1"
    "test.x"
    (Annotation.create_immutable
       ~original:(Some (Type.union [Type.string; Type.integer]))
       (Type.union [Type.string; Type.integer]));
  assert_annotation
    ~environment:
      {|
        class Foo:
          def __init__(self):
            self.attribute: int = 1
      |}
    "test.Foo().attribute"
    (Annotation.create_immutable Type.integer)


let test_forward_statement context =
  let global_resolution =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let assert_forward
      ?(precondition_immutables = [])
      ?(postcondition_immutables = [])
      ?(bottom = false)
      precondition
      statement
      postcondition
    =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement :: rest; _ } -> statement :: rest
        | _ -> failwith "unable to parse test"
      in
      let resolution =
        let annotation_store =
          create_annotation_store ~immutables:precondition_immutables precondition
        in
        TypeCheck.resolution global_resolution ~annotation_store (module TypeCheck.DummyContext)
      in

      let rec process_statement resolution = function
        | [] -> Some resolution
        | statement :: rest -> (
            match Resolution.resolve_statement resolution statement with
            | Resolution.Unreachable -> None
            | Resolution.Reachable { resolution; _ } -> process_statement resolution rest)
      in
      process_statement resolution parsed
    in
    match forwarded with
    | None -> assert_true bottom
    | Some actual_resolution ->
        assert_false bottom;
        assert_annotation_store
          ~expected:(create_annotation_store ~immutables:postcondition_immutables postcondition)
          actual_resolution
  in
  (* Assignments. *)
  assert_forward ["y", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = z"
    ["x", Type.Top; "y", Type.integer; "z", Type.Top];
  assert_forward ["x", Type.integer] "x += 1" ["x", Type.integer];
  assert_forward
    ["z", Type.integer]
    "x = y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.integer];
  assert_forward ~postcondition_immutables:["x", Type.Any] [] "x: Derp" ["x", Type.Any];
  assert_forward ~postcondition_immutables:["x", Type.string] [] "x: str = 1" ["x", Type.string];
  assert_forward
    ~postcondition_immutables:["x", Type.union [Type.string; Type.integer]]
    []
    "x: typing.Union[int, str] = 1"
    ["x", Type.literal_integer 1];

  (* Assignments with tuples. *)
  assert_forward
    ["c", Type.integer; "d", Type.Top]
    "a, b = c, d"
    ["a", Type.integer; "b", Type.Top; "c", Type.integer; "d", Type.Top];
  assert_forward ["z", Type.integer] "x, y = z" ["x", Type.Any; "y", Type.Any; "z", Type.integer];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string; Type.string]]
    "x, y = z"
    ["x", Type.Any; "y", Type.Any; "z", Type.tuple [Type.integer; Type.string; Type.string]];
  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.Top]; "y", Type.integer; "z", Type.Top];
  assert_forward
    ~postcondition_immutables:["x", Type.tuple [Type.Any; Type.Any]]
    []
    "x: typing.Tuple[typing.Any, typing.Any] = 1, 2"
    ["x", Type.tuple [Type.literal_integer 1; Type.literal_integer 2]];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string]]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_forward
    ["z", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)]
    "x, y = z"
    [
      "x", Type.integer;
      "y", Type.integer;
      "z", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer);
    ];
  assert_forward [] "(x, y), z = 1" ["x", Type.Any; "y", Type.Any; "z", Type.Any];
  assert_forward
    ["z", Type.list Type.integer]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.list Type.integer];
  assert_forward [] "x, y = return_tuple()" ["x", Type.integer; "y", Type.integer];
  assert_forward [] "x = ()" ["x", Type.Tuple (Concrete [])];

  (* Assignments with list. *)
  assert_forward
    ["x", Type.list Type.integer]
    "[a, b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.integer];
  assert_forward
    ["x", Type.list Type.integer]
    "[a, *b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.list Type.integer];
  assert_forward
    ["x", Type.list Type.integer]
    "a, *b = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.list Type.integer];

  (* Assignments with uniform sequences. *)
  assert_forward
    ["x", Type.iterable Type.integer]
    "[a, b] = x"
    ["x", Type.iterable Type.integer; "a", Type.integer; "b", Type.integer];
  assert_forward
    ["c", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)]
    "a, b = c"
    [
      "a", Type.integer;
      "b", Type.integer;
      "c", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer);
    ];
  assert_forward
    ["c", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)]
    "*a, b = c"
    [
      "a", Type.list Type.integer;
      "b", Type.integer;
      "c", Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer);
    ];

  (* Assignments with non-uniform sequences. *)
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "*a, b = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.list (Type.union [Type.integer; Type.string]);
      "b", Type.float;
    ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "a, *b = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.float]);
    ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float]]
    "a, *b, c = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.integer]);
      "c", Type.float;
    ];
  assert_forward
    ["x", Type.tuple [Type.integer]]
    "a, *b = x"
    ["x", Type.tuple [Type.integer]; "a", Type.integer; "b", Type.tuple []];
  assert_forward
    ["x", Type.tuple [Type.integer]]
    "*b, c = x"
    ["x", Type.tuple [Type.integer]; "b", Type.tuple []; "c", Type.integer];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.float]]
    "a, *b, c = x"
    [
      "x", Type.tuple [Type.integer; Type.float];
      "a", Type.integer;
      "b", Type.tuple [];
      "c", Type.float;
    ];

  (* Assignments with immutables. *)
  assert_forward ~postcondition_immutables:["y", Type.integer] [] "y: int" ["y", Type.integer];
  assert_forward ~postcondition_immutables:["y", Type.integer] [] "y: int = x" ["y", Type.integer];
  assert_forward
    ~precondition_immutables:["y", Type.Top]
    ~postcondition_immutables:["y", Type.Top]
    ["x", Type.Top; "y", Type.Top]
    "y = x"
    ["x", Type.Top; "y", Type.Top];
  assert_forward
    ~precondition_immutables:["y", Type.string]
    ~postcondition_immutables:["y", Type.integer]
    ["y", Type.string]
    "y: int"
    ["y", Type.integer];

  (* Delete. *)
  assert_forward
    ["d", Type.dictionary ~key:Type.string ~value:Type.integer]
    "del d[0]"
    ["d", Type.dictionary ~key:Type.string ~value:Type.integer];

  (* Assert. *)
  assert_forward ["x", Type.optional Type.integer] "assert x" ["x", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.integer]
    "assert y"
    ["x", Type.optional Type.integer; "y", Type.integer];
  assert_forward ["x", Type.optional Type.integer] "assert x is not None" ["x", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x and y"
    ["x", Type.integer; "y", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float; "z", Type.optional Type.float]
    "assert x and (y and z)"
    ["x", Type.integer; "y", Type.float; "z", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x or y"
    ["x", Type.optional Type.integer; "y", Type.optional Type.float];
  assert_forward ["x", Type.optional Type.integer] "assert x is None" ["x", Type.none];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert (not x) or 1"
    ["x", Type.optional Type.integer];
  assert_forward
    ["x", Type.list (Type.optional Type.integer)]
    "assert all(x)"
    ["x", Type.list Type.integer];
  assert_forward
    ["x", Type.iterable (Type.optional Type.integer)]
    "assert all(x)"
    ["x", Type.iterable Type.integer];
  assert_forward
    ["x", Type.list (Type.union [Type.none; Type.integer; Type.string])]
    "assert all(x)"
    ["x", Type.list (Type.union [Type.integer; Type.string])];
  assert_forward (* no refimenent: Awaitable is not iterable *)
    ["x", Type.awaitable (Type.union [Type.none; Type.integer])]
    "assert all(x)"
    ["x", Type.awaitable (Type.union [Type.none; Type.integer])];
  assert_forward (* no refinement: dict is iterable, but with too many type parameters *)
    ["x", Type.dictionary ~key:(Type.optional Type.integer) ~value:Type.integer]
    "assert all(x)"
    ["x", Type.dictionary ~key:(Type.optional Type.integer) ~value:Type.integer];
  assert_forward
    ["x", Type.dictionary ~key:Type.integer ~value:Type.string; "y", Type.float]
    "assert y in x"
    ["x", Type.dictionary ~key:Type.integer ~value:Type.string; "y", Type.integer];
  assert_forward
    ["x", Type.list Type.string; "y", Type.union [Type.integer; Type.string]]
    "assert y in x"
    ["x", Type.list Type.string; "y", Type.string];
  assert_forward
    ["x", Type.list Type.Top; "y", Type.integer]
    "assert y in x"
    ["x", Type.list Type.Top; "y", Type.integer];
  assert_forward [] "assert None in [1]" [];
  assert_forward ["x", Type.list Type.Top] "assert None in x" ["x", Type.list Type.Top];
  assert_forward
    ~precondition_immutables:["x", Type.float]
    ~postcondition_immutables:["x", Type.float]
    ["x", Type.float]
    "assert x in [1]"
    ["x", Type.integer];
  assert_forward ~bottom:true ["x", Type.none] "assert x" ["x", Type.none];
  assert_forward ~bottom:true ["x", Type.none] "assert x is not None" ["x", Type.none];

  (* isinstance, type(_) is _ *)
  let assert_refinement_by_type_comparison
      ?(bottom = false)
      ~precondition
      ?(negated = false)
      ~variable
      ~type_expression
      ~postcondition
      ()
    =
    let assert_forward_expression assertion_expression =
      assert_forward
        ~bottom
        precondition
        (Format.asprintf "assert %s" assertion_expression)
        postcondition
    in
    Format.asprintf "%s isinstance(%s, %s)" (if negated then "not" else "") variable type_expression
    |> assert_forward_expression;
    Format.asprintf "type(%s) %s %s" variable (if negated then "is not" else "is") type_expression
    |> assert_forward_expression;
    Format.asprintf "type(%s) %s %s" variable (if negated then "!=" else "==") type_expression
    |> assert_forward_expression
  in
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.Any]
    ~variable:"x"
    ~type_expression:"int"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.Any; "y", Type.Top]
    ~variable:"y"
    ~type_expression:"str"
    ~postcondition:["x", Type.Any; "y", Type.string]
    ();
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.Any]
    ~variable:"x"
    ~type_expression:"(int, str)"
    ~postcondition:["x", Type.union [Type.integer; Type.string]]
    ();
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.integer]
    ~variable:"x"
    ~type_expression:"(int, str)"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:true
    ~precondition:["x", Type.integer]
    ~variable:"x"
    ~type_expression:"str"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:false
    ~precondition:["x", Type.Bottom]
    ~variable:"x"
    ~type_expression:"str"
    ~postcondition:["x", Type.string]
    ();
  assert_refinement_by_type_comparison
    ~bottom:false
    ~precondition:["x", Type.float]
    ~variable:"x"
    ~type_expression:"int"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:false
    ~precondition:["x", Type.integer]
    ~variable:"x"
    ~type_expression:"1"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:true
    ~precondition:["x", Type.integer]
    ~negated:true
    ~variable:"x"
    ~type_expression:"int"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:true
    ~precondition:["x", Type.integer]
    ~negated:true
    ~variable:"x"
    ~type_expression:"float"
    ~postcondition:["x", Type.integer]
    ();
  assert_refinement_by_type_comparison
    ~bottom:false
    ~precondition:["x", Type.float]
    ~negated:true
    ~variable:"x"
    ~type_expression:"int"
    ~postcondition:["x", Type.float]
    ();
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.optional (Type.union [Type.integer; Type.string])]
    ~negated:true
    ~variable:"x"
    ~type_expression:"int"
    ~postcondition:["x", Type.optional Type.string]
    ();
  assert_refinement_by_type_comparison
    ~precondition:["x", Type.optional (Type.union [Type.integer; Type.string])]
    ~negated:true
    ~variable:"x"
    ~type_expression:"type(None)"
    ~postcondition:["x", Type.union [Type.integer; Type.string]]
    ();
  assert_refinement_by_type_comparison
    ~precondition:
      ["my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string]; "x", Type.Top]
    ~variable:"x"
    ~type_expression:"my_type"
    ~postcondition:
      [
        "my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string];
        "x", Type.union [Type.integer; Type.string];
      ]
    ();
  assert_refinement_by_type_comparison
    ~precondition:
      [
        ( "my_type",
          Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation (Type.meta Type.integer)) );
        "x", Type.Top;
      ]
    ~variable:"x"
    ~type_expression:"my_type"
    ~postcondition:
      [
        ( "my_type",
          Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation (Type.meta Type.integer)) );
        "x", Type.integer;
      ]
    ();

  (* Works for general expressions. *)
  assert_forward
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x + 1, int)"
    ["x", Type.integer];
  assert_forward ~bottom:false ["x", Type.Bottom] "assert not isinstance(x, int)" ["x", Type.Bottom];
  assert_forward ~bottom:true [] "assert False" [];
  assert_forward ~bottom:true [] "assert None" [];
  assert_forward ~bottom:true [] "assert 0" [];
  assert_forward ~bottom:true [] "assert 0.0" [];
  assert_forward ~bottom:true [] "assert 0.0j" [];
  assert_forward ~bottom:true [] "assert ''" [];
  assert_forward ~bottom:true [] "assert b''" [];
  assert_forward ~bottom:true [] "assert []" [];
  assert_forward ~bottom:true [] "assert ()" [];
  assert_forward ~bottom:true [] "assert {}" [];
  assert_forward ~bottom:false [] "assert (not True)" [];

  (* Raise. *)
  assert_forward [] "raise 1" [];
  assert_forward [] "raise Exception" [];
  assert_forward [] "raise Exception()" [];
  assert_forward [] "raise undefined" [];
  assert_forward [] "raise" [];

  (* Return. *)
  assert_forward [] "return 1" [];

  (* Pass. *)
  assert_forward ["y", Type.integer] "pass" ["y", Type.integer]


let test_forward context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let create =
    let module Create = Create (DefaultContext) in
    Create.create ~resolution
  in
  let module State = State (DefaultContext) in
  let assert_state_equal =
    assert_equal
      ~cmp:State.equal
      ~printer:(Format.asprintf "%a" State.pp)
      ~pp_diff:(diff ~print:State.pp)
  in
  let assert_forward
      ?(precondition_bottom = false)
      ?(postcondition_bottom = false)
      precondition
      statement
      postcondition
    =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement :: rest; _ } -> statement :: rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward ~statement_key:Cfg.entry_index ~statement state)
        ~init:(create ~bottom:precondition_bottom precondition)
        parsed
    in
    assert_state_equal (create ~bottom:postcondition_bottom postcondition) forwarded
  in
  assert_forward [] "x = 1" ["x", Type.literal_integer 1];
  assert_forward ~precondition_bottom:true ~postcondition_bottom:true [] "x = 1" [];
  assert_forward ~postcondition_bottom:true [] "sys.exit(1)" []


type method_call = {
  direct_target: string;
  class_name: string;
  dispatch: Callgraph.dispatch;
  is_optional_class_attribute: bool;
}

type property_setter_call = {
  direct_target: string;
  class_name: string;
}

let function_caller name = Callgraph.FunctionCaller !&name

let property_setter_caller name = Callgraph.PropertySetterCaller !&name

let test_calls context =
  let assert_calls source calls =
    let project =
      ScratchProject.setup ~context ~populate_call_graph:true ["qualifier.py", source]
    in
    let _ = ScratchProject.build_type_environment project in

    (* Check calls. *)
    let assert_calls (caller, callees) =
      let expected_callees =
        let callee = function
          | `Method { direct_target; class_name; dispatch; is_optional_class_attribute } ->
              Callgraph.Method
                {
                  direct_target = Reference.create direct_target;
                  class_name = Type.Primitive class_name;
                  dispatch;
                  is_optional_class_attribute;
                }
          | `Function name -> Callgraph.Function (Reference.create name)
          | `PropertySetter { direct_target; class_name } ->
              Callgraph.PropertySetter
                {
                  direct_target = Reference.create direct_target;
                  class_name = Type.Primitive class_name;
                }
        in
        List.map callees ~f:callee |> List.map ~f:Callgraph.show_callee |> String.Set.of_list
      in
      let actual_callees =
        let show { Callgraph.callee; _ } = Callgraph.show_callee callee in
        Callgraph.get ~caller |> List.map ~f:show |> String.Set.of_list
      in
      assert_equal
        ~printer:(fun set -> Set.to_list set |> String.concat ~sep:",")
        ~cmp:String.Set.equal
        expected_callees
        actual_callees
    in
    List.iter calls ~f:assert_calls;
    Memory.reset_shared_memory ()
  in
  assert_calls
    {|
     def foo(): ...
     def calls_foo():
       foo()
   |}
    [
      function_caller "qualifier.foo", [];
      function_caller "qualifier.calls_foo", [`Function "qualifier.foo"];
    ];
  assert_calls
    {|
     def foo(): ...
     def bar(): ...
     def calls_on_same_line():
       foo(); bar()
   |}
    [
      ( function_caller "qualifier.calls_on_same_line",
        [`Function "qualifier.foo"; `Function "qualifier.bar"] );
    ];

  (* Methods. *)
  assert_calls
    {|
     class Class:
       def method(): ...
     def calls_method(c: Class):
       c.method()
   |}
    [
      ( function_caller "qualifier.calls_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.method";
              class_name = "qualifier.Class";
              dispatch = Dynamic;
              is_optional_class_attribute = false;
            };
        ] );
    ];

  (* Constructors and `super`. *)
  assert_calls
    {|
     class Class: ...
     class ClassWithInit:
       def __init__(self):
         super().__init__()
     def calls_Class():
       Class()
     def calls_ClassWithInit():
       ClassWithInit()
     def calls_ClassWithInit__init__(object: object):
       ClassWithInit.__init__(object)
   |}
    [
      ( function_caller "qualifier.ClassWithInit.__init__",
        [
          `Method
            {
              direct_target = "object.__init__";
              class_name = "object";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_Class",
        [
          `Method
            {
              direct_target = "object.__init__";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_ClassWithInit",
        [
          `Method
            {
              direct_target = "qualifier.ClassWithInit.__init__";
              class_name = "qualifier.ClassWithInit";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_ClassWithInit__init__",
        [
          `Method
            {
              direct_target = "qualifier.ClassWithInit.__init__";
              class_name = "qualifier.ClassWithInit";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
    ];
  assert_calls
    {|
     class Class:
       @classmethod
       def classmethod(cls): ...
     def calls_class_method():
       Class.classmethod()
   |}
    [
      ( function_caller "qualifier.calls_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.classmethod";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
    ];

  (* Inheritance. *)
  assert_calls
    {|
      class Class:
        def method(self): ...
      class Indirect(Class):
        ...
      class Subclass(Indirect):
        ...
      class OverridingSubclass(Subclass):
        def method(self): ...
      def calls_Class_method(c: Class):
        c.method()
      def calls_Indirect_method(i: Indirect):
        i.method()
      def calls_Subclass_method(s: Subclass):
        s.method()
      def calls_OverridingSubclass_method(o: OverridingSubclass):
        o.method()
    |}
    [
      ( function_caller "qualifier.calls_Class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.method";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
      ( function_caller "qualifier.calls_Indirect_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.method";
              class_name = "qualifier.Indirect";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
      ( function_caller "qualifier.calls_Subclass_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.method";
              class_name = "qualifier.Subclass";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
      ( function_caller "qualifier.calls_OverridingSubclass_method",
        [
          `Method
            {
              direct_target = "qualifier.OverridingSubclass.method";
              class_name = "qualifier.OverridingSubclass";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* Classmethods. *)
  assert_calls
    {|
      class Class:
        @classmethod
        def class_method(cls): ...
      class Indirect(Class):
        ...
      class Subclass(Indirect):
        @classmethod
        def class_method(cls): ...
      def calls_Class_class_method():
        Class.class_method()
      def calls_Indirect_class_method():
        Indirect.class_method()
      def calls_Subclass_class_method():
        Subclass.class_method()
      def calls_Type_Class_class_method(c: typing.Type[Class]):
        c.class_method()
      def calls_Type_Indirect_class_method(c: typing.Type[Indirect]):
        c.class_method()
      def calls_Type_Subclass_class_method(c: typing.Type[Subclass]):
        c.class_method()
    |}
    [
      ( function_caller "qualifier.calls_Class_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.class_method";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_Indirect_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.class_method";
              class_name = "qualifier.Indirect";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_Subclass_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Subclass.class_method";
              class_name = "qualifier.Subclass";
              is_optional_class_attribute = false;
              dispatch = Static;
            };
        ] );
      ( function_caller "qualifier.calls_Type_Class_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.class_method";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
      ( function_caller "qualifier.calls_Type_Indirect_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Class.class_method";
              class_name = "qualifier.Indirect";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
      ( function_caller "qualifier.calls_Type_Subclass_class_method",
        [
          `Method
            {
              direct_target = "qualifier.Subclass.class_method";
              class_name = "qualifier.Subclass";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* Unions. *)
  assert_calls
    {|
     class Class:
       def method(): ...
     class OtherClass:
       def method(): ...
     def calls_method_on_union(union: typing.Union[Class, OtherClass]):
       union.method()
   |}
    [
      ( function_caller "qualifier.calls_method_on_union",
        [
          `Method
            {
              direct_target = "qualifier.Class.method";
              class_name = "qualifier.Class";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
          `Method
            {
              direct_target = "qualifier.OtherClass.method";
              class_name = "qualifier.OtherClass";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* We deduplicate calls. *)
  assert_calls
    {|
    class Foo:
      def method(): ...
    def call_twice(foo: Foo):
      foo.method()
      bar = foo
      bar.method()
  |}
    [
      ( function_caller "qualifier.call_twice",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* Properties. *)
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      def call_property(foo: Foo):
        x = foo.method
    |}
    [
      ( function_caller "qualifier.call_property",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      class Bar(Foo):
        pass
      def call_property(bar: Bar):
        x = bar.method
    |}
    [
      ( function_caller "qualifier.call_property",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Bar";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, item: int) -> None: ...
      def call_property_setter(foo: Foo) -> None:
        foo.x = 1
    |}
    [
      ( function_caller "qualifier.call_property_setter",
        [`PropertySetter { direct_target = "qualifier.Foo.x"; class_name = "qualifier.Foo" }] );
    ];

  (* Don't attempt to register calls for non-property attribute accesses. *)
  assert_calls
    {|
      class Foo:
        def method(self) -> int: ...
      class Bar:
        pass
      def call_property(bar: Bar):
        x = bar.method
    |}
    [function_caller "qualifier.call_property", []];
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      class Bar:
        @property
        def method(self) -> int: ...
      def call_property(parameter: typing.Union[Foo, Bar]):
        x = parameter.method
    |}
    [
      ( function_caller "qualifier.call_property",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
          `Method
            {
              direct_target = "qualifier.Bar.method";
              class_name = "qualifier.Bar";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* Statically invoking properties is illegal in the runtime - we default to dynamic
     is_optional_class_attribute = false; dispatch, but this should be a type error, really. *)
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      def call_property():
        x = Foo.method
    |}
    [
      ( function_caller "qualifier.call_property",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];

  (* Include calls for optionals, as the callgraph is an overapproximation. *)
  assert_calls
    {|
      import typing
      class Foo:
        def method(self) -> int: ...
      def calls_optional(foo: typing.Optional[Foo]) -> None:
        foo.method()
    |}
    [
      ( function_caller "qualifier.calls_optional",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = true;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
      import typing
      class Foo:
        def method(self) -> int: ...
      class Bar(Foo):
        pass
      def calls_optional(bar: typing.Optional[Bar]) -> None:
        bar.method()
    |}
    [
      ( function_caller "qualifier.calls_optional",
        [
          `Method
            {
              direct_target = "qualifier.Foo.method";
              class_name = "qualifier.Bar";
              is_optional_class_attribute = true;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
      from typing import Optional
      class Foo:
        @property
        def x(self) -> int: ...
      def optional_property(foo: typing.Optional[Foo]) -> None:
        y = foo.x
    |}
    [
      ( function_caller "qualifier.optional_property",
        [
          `Method
            {
              direct_target = "qualifier.Foo.x";
              class_name = "qualifier.Foo";
              is_optional_class_attribute = true;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
      def calls_isinstance():
        x = 1
        if isinstance(x, int):
          return 0
        return 1
    |}
    [function_caller "qualifier.calls_isinstance", [`Function "isinstance"]];
  assert_calls
    {|
      from typing import Generic, TypeVar
      T = TypeVar("T")
      class C(Generic[T]):
        def method(self) -> int: ...
      class D(C[int]):
        def method(self) -> int: ...
      def calls_C_str(c: C[str]) -> None:
        c.method()
    |}
    [
      ( function_caller "qualifier.calls_C_str",
        [
          `Method
            {
              direct_target = "qualifier.C.method";
              class_name = "qualifier.C[str]";
              is_optional_class_attribute = false;
              dispatch = Dynamic;
            };
        ] );
    ];
  assert_calls
    {|
    def foo(): ...
    def bar(): ...
    class C:
      @property
      def p(self) -> int:
        foo()
        return 0
      @p.setter
      def p(self, value: int) -> None:
        bar()
  |}
    [
      function_caller "qualifier.C.p", [`Function "qualifier.foo"];
      property_setter_caller "qualifier.C.p", [`Function "qualifier.bar"];
    ]


let test_unpack_callable_and_self_argument context =
  let parse ~global_resolution annotation =
    parse_single_expression ~preprocess:true annotation
    |> GlobalResolution.parse_annotation global_resolution
  in
  let assert_unpack_type ?(source = "") ~signature_select given expected =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let actual =
      TypeCheck.unpack_callable_and_self_argument
        ~signature_select
        ~global_resolution
        (parse ~global_resolution given)
    in
    let expected =
      expected >>| fun (callable, self_argument) -> { TypeOperation.callable; self_argument }
    in
    assert_equal
      ~printer:[%show: TypeOperation.callable_and_self_argument option]
      ~cmp:[%eq: TypeOperation.callable_and_self_argument option]
      actual
      expected
  in
  let assert_unpack ?(source = "") given expected =
    let global_resolution =
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
      in
      GlobalResolution.create global_environment
    in
    let expected =
      expected
      >>= fun (expected_string, self_argument) ->
      match parse ~global_resolution expected_string with
      | Callable callable -> Some (callable, self_argument)
      | _ -> assert false
    in
    assert_unpack_type ~source given expected
  in
  let signature_select ?(pairs = []) ~arguments:_ ~callable ~self_argument:_ =
    match pairs with
    | [(Type.Any, right)] -> SignatureSelectionTypes.Found { selected_return_annotation = right }
    | _ ->
        List.find ~f:(fun (left, _) -> [%eq: Type.t] left (Type.Callable callable)) pairs
        >>| (fun (_, right) -> SignatureSelectionTypes.Found { selected_return_annotation = right })
        |> Option.value
             ~default:
               (SignatureSelectionTypes.NotFound
                  { closest_return_annotation = Type.Bottom; reason = None })
  in
  let default_select ~arguments:_ ~callable:_ ~self_argument:_ =
    SignatureSelectionTypes.Found { selected_return_annotation = Type.integer }
  in

  assert_unpack
    ~signature_select:default_select
    "typing.Callable[[int], int]"
    (Some ("typing.Callable[[int], int]", None));
  assert_unpack_type
    ~signature_select:default_select
    "typing.Any"
    (Some
       ( {
           kind = Anonymous;
           implementation = { annotation = Type.Any; parameters = Undefined };
           overloads = [];
         },
         None ));
  assert_unpack_type
    ~signature_select:default_select
    "typing.Any"
    (Some
       ( {
           kind = Anonymous;
           implementation = { annotation = Type.Any; parameters = Undefined };
           overloads = [];
         },
         None ));
  assert_unpack
    ~signature_select:default_select
    "typing.Callable[[int], int]"
    (Some ("typing.Callable[[int], int]", None));
  assert_unpack
    ~signature_select:default_select
    "BoundMethod[typing.Callable[[int], bool], str]"
    (Some ("typing.Callable[[int], bool]", Some Type.string));
  assert_unpack
    ~signature_select:default_select
    ~source:
      {|
        from typing import Generic, TypeVar
        T = TypeVar("T")
        class Foo(Generic[T]):
          def __call__(self, x: int) -> T: ...
      |}
    "BoundMethod[test.Foo[str], int]"
    (Some ("typing.Callable[[Named(x, int)], str]", Some Type.integer));
  assert_unpack_type
    ~signature_select:default_select
    ~source:
      {|
        class Foo:
          def __call__(self, x: bool) -> str: ...
        class Bar:
          def __call__(self, x: str) -> int: ...
      |}
    "pyre_extensions.Compose[test.Foo, test.Bar]"
    (Some
       ( {
           Type.Callable.kind = Type.Callable.Named (Reference.create "test.Foo.__call__");
           implementation =
             {
               parameters =
                 Type.Callable.Defined
                   [
                     Type.Callable.Parameter.Named
                       {
                         name = "$parameter$self";
                         annotation = Type.Primitive "test.Foo";
                         default = false;
                       };
                     Type.Callable.Parameter.Named
                       { name = "$parameter$x"; annotation = Type.bool; default = false };
                   ];
               annotation = Type.integer;
             };
           overloads = [];
         },
         Some (Type.Primitive "test.Foo") ));
  assert_unpack
    ~signature_select:default_select
    ~source:
      {|
        from typing import Callable
        class Bar:
          __call__: Callable[[int], int] = ...
        class Foo():
          __call__ : Bar = ...
      |}
    "BoundMethod[test.Foo, int]"
    (Some ("typing.Callable[[int], int]", Some Type.integer));
  assert_unpack
    ~signature_select:default_select
    ~source:
      {|
        from typing import Callable
        class Baz:
          __call__: Callable[[int], int] = ...
        class Bar:
          __call__: Baz = ...
        class Foo():
          __call__ : Bar = ...
      |}
    "BoundMethod[test.Foo, int]"
    None;
  assert_unpack
    ~signature_select:default_select
    ~source:
      {|
        class Bar:
          __call__: int
        class Foo():
          __call__ : Bar = ...
      |}
    "BoundMethod[test.Foo, int]"
    None;
  assert_unpack
    ~signature_select:default_select
    ~source:
      {|
        from typing import Callable
        from pyre_extensions import Compose
        class Bar2:
          __call__: Compose[Callable[[int], int], Callable[[int], int]] = ...
        class Baz2:
          __call__: Compose[Callable[[int], int], Callable[[int], int]] = ...
      |}
    "pyre_extensions.Compose[test.Bar2, test.Baz2]"
    (Some ("typing.Callable[[int], int]", None));
  assert_unpack ~signature_select:default_select "typing.Tuple[int, str]" None;
  assert_unpack
    ~signature_select:
      (signature_select
         ~pairs:
           [
             ( Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [PositionalOnly { index = 0; annotation = Type.integer; default = false }])
                 ~annotation:Type.float
                 (),
               Type.float );
             ( Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [PositionalOnly { index = 0; annotation = Type.float; default = false }])
                 ~annotation:Type.string
                 (),
               Type.string );
           ])
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          typing.Callable[[bool], int],
          typing.Callable[[int], float]
        ],
        typing.Callable[[float], str]
      ]
    |}
    (Some ("typing.Callable[[bool], str]", None));
  ()


let () =
  "type"
  >::: [
         "initial" >:: test_initial;
         "less_or_equal" >:: test_less_or_equal;
         "widen" >:: test_widen;
         "check_annotation" >:: test_check_annotation;
         "forward_expression" >:: test_forward_expression;
         "forward_statement" >:: test_forward_statement;
         "forward" >:: test_forward;
         "module_exports" >:: test_module_exports;
         "object_callables" >:: test_object_callables;
         "callable_selection" >:: test_callable_selection;
         "calls" >:: test_calls;
         "unpack_callable_and_self_argument" >:: test_unpack_callable_and_self_argument;
       ]
  |> Test.run
