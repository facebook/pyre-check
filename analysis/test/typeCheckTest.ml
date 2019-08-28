(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open TypeCheck
open Test

module DefaultContext = struct
  let configuration = Configuration.Analysis.create ()

  let define = +Test.mock_define

  let calls = Location.Reference.Table.create ()
end

module Create (Context : TypeCheck.Context) = struct
  let create ?(bottom = false) ?(immutables = []) ~resolution annotations =
    let module State = State (Context) in
    let resolution =
      let annotations =
        let immutables = String.Map.of_alist_exn immutables in
        let annotify (name, annotation) =
          let annotation =
            let create annotation =
              match Map.find immutables name with
              | Some (global, original) ->
                  Annotation.create_immutable ~original:(Some original) ~global annotation
              | _ -> Annotation.create annotation
            in
            create annotation
          in
          !&name, annotation
        in
        List.map annotations ~f:annotify |> Reference.Map.of_alist_exn
      in
      Resolution.with_annotations resolution ~annotations
    in
    State.create ~bottom ~resolution ()
end

let list_orderless_equal left right =
  List.equal
    String.equal
    (List.dedup_and_sort ~compare:String.compare left)
    (List.dedup_and_sort ~compare:String.compare right)


let description ~resolution error =
  let ast_environment =
    Resolution.global_resolution resolution |> GlobalResolution.ast_environment
  in
  Error.instantiate ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment) error
  |> Error.Instantiated.description ~show_error_traces:false


let test_initial context =
  let assert_initial
      ?parent
      ?(errors = [])
      ?(environment = "")
      ?(immutables = [])
      ~annotations
      define
    =
    let define =
      match parse_single_statement define with
      | { Node.value = Define ({ signature; _ } as define); _ } ->
          let signature = { signature with parent = parent >>| Reference.create } in
          { define with signature }
      | _ -> failwith "Unable to parse define."
    in
    let module Context = struct
      let configuration = Configuration.Analysis.create ()

      let define = +define

      let calls = Location.Reference.Table.create ()
    end
    in
    let resolution =
      ScratchProject.setup ~context ["__init__.py", environment] |> ScratchProject.build_resolution
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
        let extract_variables { Node.value = { Parameter.annotation; _ }; _ } =
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
    assert_state_equal state initial;
    assert_equal
      ~cmp:(List.equal String.equal)
      ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements : string list)])
      errors
      (List.map (State.errors initial) ~f:(description ~resolution))
  in
  assert_initial
    "def foo(x: int) -> None: ..."
    ~immutables:["x", (false, Type.integer)]
    ~annotations:["x", Type.integer];
  assert_initial
    "def foo(x: int = 1.0) -> None: ..."
    ~errors:
      [ "Incompatible variable type [9]: x is declared to have type `int` but is used as type "
        ^ "`float`." ]
    ~immutables:["x", (false, Type.integer)]
    ~annotations:["x", Type.integer];
  assert_initial
    ~errors:
      ["Missing parameter annotation [2]: Parameter `x` has type `float` but no type is specified."]
    ~annotations:["x", Type.float]
    "def foo(x = 1.0) -> None: ...";
  assert_initial
    "def foo(x: int) -> int: ..."
    ~immutables:["x", (false, Type.integer)]
    ~annotations:["x", Type.integer];
  assert_initial
    "def foo(x: float, y: str) -> None: ..."
    ~immutables:["x", (false, Type.float); "y", (false, Type.string)]
    ~annotations:["x", Type.float; "y", Type.string];
  assert_initial
    "def foo(x) -> None: ..."
    ~errors:["Missing parameter annotation [2]: Parameter `x` has no type specified."]
    ~annotations:["x", Type.Any];
  assert_initial
    "def foo(x: typing.Any) -> None: ..."
    ~errors:["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."]
    ~immutables:["x", (false, Type.Any)]
    ~annotations:["x", Type.Any];
  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    "def __eq__(self, other: object) -> None: ..."
    ~errors:[]
    ~immutables:["other", (false, Type.object_primitive)]
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
    ~errors:["Missing parameter annotation [2]: Parameter `a` has no type specified."]
    ~annotations:["a", Type.Any];
  assert_initial
    ~environment:"T = typing.TypeVar('T')"
    "def foo(x: T) -> None: ..."
    ~immutables:["x", (false, Type.Variable.mark_all_variables_as_bound (Type.variable "T"))]
    ~annotations:["x", Type.Variable.mark_all_variables_as_bound (Type.variable "T")]


let test_less_or_equal context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let create =
    let module Create = Create (DefaultContext) in
    Create.create ~resolution
  in
  let module State = State (DefaultContext) in
  (* <= *)
  assert_true (State.less_or_equal ~left:(create []) ~right:(create []));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.integer]));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.Top]));
  assert_true
    (State.less_or_equal
       ~left:(create ["x", Type.integer])
       ~right:(create ["x", Type.integer; "y", Type.integer]));

  (* > *)
  assert_false (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create []));
  assert_false (State.less_or_equal ~left:(create ["x", Type.Top]) ~right:(create []));

  (* partial order *)
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["x", Type.string]));
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["y", Type.integer]))


let test_join context =
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
  (* <= *)
  assert_state_equal (State.join (create []) (create [])) (create []);
  assert_state_equal (State.join (create []) (create ["x", Type.integer])) (create ["x", Type.Top]);
  assert_state_equal (State.join (create []) (create ["x", Type.Top])) (create ["x", Type.Top]);
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create ["x", Type.integer; "y", Type.integer]))
    (create ["x", Type.integer; "y", Type.Top]);

  (* > *)
  assert_state_equal (State.join (create ["x", Type.integer]) (create [])) (create ["x", Type.Top]);
  assert_state_equal (State.join (create ["x", Type.Top]) (create [])) (create ["x", Type.Top]);

  (* partial order *)
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create ["x", Type.string]))
    (create ["x", Type.union [Type.string; Type.integer]]);
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create ["y", Type.integer]))
    (create ["x", Type.Top; "y", Type.Top])


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
  let widening_threshold = 10 in
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
  let assert_check_annotation source expression descriptions =
    let resolution =
      ScratchProject.setup ~context ["__init__.py", source] |> ScratchProject.build_resolution
    in
    let create =
      let module Create = Create (DefaultContext) in
      Create.create ~resolution
    in
    let module State = State (DefaultContext) in
    let state = create [] in
    let errors = State.parse_and_check_annotation ~state !expression |> fst |> State.errors in
    let errors = List.map ~f:(description ~resolution) errors in
    assert_equal
      ~cmp:(List.equal String.equal)
      ~printer:(String.concat ~sep:"\n")
      descriptions
      errors
  in
  assert_check_annotation "" "x" ["Undefined type [11]: Type `x` is not defined."];
  assert_check_annotation
    "x: int = 1"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation
    "x: typing.Type[int] = int"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation "x = int" "x" [];
  assert_check_annotation
    "x: typing.Any"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation "x = typing.Any" "x" [];
  assert_check_annotation "x: typing.TypeAlias = typing.Any" "x" [];
  assert_check_annotation {|
      class Foo: ...
      x = Foo
    |} "x" [];
  assert_check_annotation
    {|
      class Foo: ...
      x = Foo()
    |}
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation
    {|
      class Foo:
        def __getitem__(self, other) -> typing.Any:
          ...
      x = Foo[Undefined]
    |}
    "x"
    ["Undefined type [11]: Type `x` is not defined."]


type definer =
  | Module of Reference.t
  | Type of Type.t
[@@deriving compare, eq, show]

and stripped =
  | Attribute of string
  | MissingAttribute of {
      name: string;
      missing_definer: definer;
    }
  | Unknown
  | SignatureFound of {
      callable: string;
      callees: string list;
    }
  | SignatureNotFound of Annotated.Signature.reason option
  | NotCallable of Type.t
  | Value

and step = {
  annotation: Type.t;
  element: stripped;
}
[@@deriving compare, eq, show]

let assert_resolved ~context sources expression expected =
  let module State = State (DefaultContext) in
  let resolution = ScratchProject.setup ~context sources |> ScratchProject.build_resolution in
  let resolved =
    let state = State.create ~resolution () in
    let { State.resolved; _ } =
      State.forward_expression ~state ~expression:(parse_single_expression expression)
    in
    resolved
  in
  assert_equal ~printer:Type.show ~cmp:Type.equal expected resolved


let test_module_exports context =
  let assert_exports_resolved expression expected =
    let sources =
      [ ( "implementing.py",
          {|
        def implementing.function() -> int: ...
        constant: int = 1
      |} );
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
      |} ]
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
  assert_exports_resolved "wildcard_default.aliased()" Type.Top;
  let assert_fixpoint_stop =
    assert_resolved
      ~context
      [ "loop/b.py", {|
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
          |} ]
  in
  assert_fixpoint_stop "loop.b" Type.Top;
  assert_fixpoint_stop "no_loop.c" Type.integer


let test_object_callables context =
  let assert_resolved expression annotation =
    assert_resolved
      ~context
      [ ( "module.py",
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
            callable: typing.Callable[..., unknown][[..., int][..., str]] = ...
            submodule: Submodule[int] = ...
          |}
        ) ]
      expression
      (Type.create ~aliases:(fun _ -> None) (parse_single_expression annotation))
  in
  assert_resolved "module.call" "module.Call[int, str]";
  assert_resolved "module.call.attribute" "int";
  assert_resolved "module.call.generic_callable" "typing.Callable[[int], str]";
  assert_resolved "module.call()" "str";
  assert_resolved "module.callable()" "int";
  assert_resolved "module.meta" "typing.Type[module.Call[int, str]]";
  assert_resolved "module.meta()" "module.Call[int, str]";
  assert_resolved "module.submodule.generic_callable" "typing.Callable[[int], int]"


let test_callable_selection context =
  let assert_resolved source expression annotation =
    assert_resolved
      ~context
      ["test.py", source]
      expression
      (Type.create ~aliases:(fun _ -> None) (parse_single_expression annotation))
  in
  assert_resolved "call: typing.Callable[[], int]" "test.call()" "int";
  assert_resolved "call: typing.Callable[[int], int]" "test.call()" "int"


type parameter_kind =
  | NamedParameter
  | VariableParameter
  | KeywordParameter

let test_forward_expression context =
  let module State = State (DefaultContext) in
  let create =
    let module Create = Create (DefaultContext) in
    Create.create
  in
  let assert_forward
      ?(precondition = [])
      ?(postcondition = [])
      ?(errors = `Undefined 0)
      ?(environment = "")
      expression
      annotation
    =
    let expression =
      let expression = parse expression |> Preprocessing.expand_format_string in
      expression
      |> function
      | { Source.statements = [{ Node.value = Statement.Expression expression; _ }]; _ } ->
          expression
      | { Source.statements = [{ Node.value = Statement.Yield expression; _ }]; _ } -> expression
      | _ -> failwith "Unable to extract expression"
    in
    let resolution =
      ScratchProject.setup ~context ["__init__.py", environment] |> ScratchProject.build_resolution
    in
    let { State.state = forwarded; resolved; _ } =
      State.forward_expression ~state:(create ~resolution precondition) ~expression
    in
    let assert_state_equal =
      assert_equal
        ~cmp:State.equal
        ~printer:(Format.asprintf "%a" State.pp)
        ~pp_diff:(diff ~print:State.pp)
    in
    let errors =
      match errors with
      | `Specific errors -> errors
      | `Undefined count ->
          let rec errors sofar count =
            let error =
              "Undefined name [18]: Global name `undefined` is not defined, or there is at least \
               one control flow path that doesn't define `undefined`."
            in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_state_equal (create ~resolution postcondition) forwarded;
    assert_equal
      ~cmp:list_orderless_equal
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(description ~resolution));
    assert_equal ~cmp:Type.equal ~printer:Type.show annotation resolved
  in
  (* Await. *)
  assert_forward "await awaitable_int()" Type.integer;
  assert_forward
    ~errors:
      (`Specific
        [ "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
          "Undefined name [18]: Global name `undefined` is not defined, or there is at least one \
           control flow path that doesn't define `undefined`." ])
    "await undefined"
    Type.Top;

  (* Boolean operator. *)
  assert_forward "1 or 'string'" (Type.union [Type.integer; Type.string]);
  assert_forward "1 and 'string'" (Type.union [Type.integer; Type.string]);
  assert_forward ~errors:(`Undefined 1) "undefined or 1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "1 or undefined" Type.Top;
  assert_forward ~errors:(`Undefined 2) "undefined and undefined" Type.Top;
  let assert_optional_forward ?(postcondition = ["x", Type.optional Type.integer]) =
    assert_forward ~precondition:["x", Type.optional Type.integer] ~postcondition
  in
  assert_optional_forward "x or 1" Type.integer;
  assert_optional_forward "1 or x" (Type.optional Type.integer);
  assert_optional_forward "x or x" (Type.optional Type.integer);
  assert_optional_forward "x and 1" (Type.optional Type.integer);
  assert_optional_forward "1 and x" (Type.optional Type.integer);
  assert_optional_forward "x and x" (Type.optional Type.integer);

  (* Call *)
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~errors:
      (`Specific
        [ "Incompatible parameter type [6]: "
          ^ "Expected `int` for 1st anonymous parameter to call `dict.add_key` but got `str`." ])
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
    ~errors:
      (`Specific
        [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
           `foo` but got `unknown`.";
          "Undefined attribute [16]: Optional type has no attribute `attribute`." ])
    "foo(unknown).attribute"
    Type.Top;
  assert_forward
    ~precondition:["undefined", Type.Union [Type.integer; Type.undeclared]]
    ~postcondition:["undefined", Type.Union [Type.integer; Type.undeclared]]
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `undefined` is not defined, or there is at least one "
          ^ "control flow path that doesn't define `undefined`." ])
    "undefined()"
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
    ~errors:(`Specific ["Undefined attribute [16]: `Foo` has no attribute `unknown`."])
    "foo(foo_instance.unknown).attribute"
    Type.integer;
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
    ~errors:
      (`Specific
        [ "Undefined attribute [16]: `Foo` has no attribute `unknown`.";
          "Undefined attribute [16]: `Foo` has no attribute `another_unknown`." ])
    "foo(foo_instance.unknown).another_unknown"
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
        def MetaFoo.__contains__(self, x:int) -> bool: ...
      class Foo(metaclass=MetaFoo):
        def Foo.foo(self) -> int:
          return 9
    |}
    ~precondition:["Container", Type.meta (Type.Primitive "Foo")]
    ~postcondition:["Container", Type.meta (Type.Primitive "Foo")]
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
  assert_forward ~errors:(`Undefined 1) "undefined < 1" Type.Top;
  assert_forward ~errors:(`Undefined 2) "undefined == undefined" Type.Top;

  (* Complex literal. *)
  assert_forward "1j" Type.complex;
  assert_forward "1" (Type.literal_integer 1);
  assert_forward "\"\"" (Type.literal_string "");
  assert_forward "b\"\"" Type.bytes;

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
  assert_forward "{**{1: 1}, **{'a': 'b'}}" (Type.dictionary ~key:Type.Any ~value:Type.Any);
  assert_forward
    ~errors:(`Undefined 1)
    "{1: 'string', **{undefined: 1}}"
    (Type.dictionary ~key:Type.Top ~value:Type.Any);
  assert_forward
    ~errors:(`Undefined 1)
    "{undefined: 1}"
    (Type.dictionary ~key:Type.Top ~value:Type.integer);
  assert_forward
    ~errors:(`Undefined 1)
    "{1: undefined}"
    (Type.dictionary ~key:Type.integer ~value:Type.Top);
  assert_forward
    ~errors:(`Undefined 3)
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

  (* Generators. *)
  assert_forward "(element for element in [1])" (Type.generator Type.integer);
  assert_forward
    ~errors:
      (`Specific
        [ "Incomplete type [37]: Type `typing.List[Variable[_T]]` inferred for `[]` is "
          ^ "incomplete, so attribute `__iter__` cannot be accessed. Separate the expression into "
          ^ "an assignment and give it an explicit annotation." ])
    "(element for element in [])"
    (Type.generator Type.Any);
  assert_forward
    "((element, independent) for element in [1] for independent in ['string'])"
    (Type.generator (Type.tuple [Type.integer; Type.string]));
  assert_forward
    "(nested for element in [[1]] for nested in element)"
    (Type.generator Type.integer);
  assert_forward ~errors:(`Undefined 1) "(undefined for element in [1])" (Type.generator Type.Top);
  assert_forward
    ~errors:(`Undefined 1)
    "(element for element in undefined)"
    (Type.generator Type.Top);

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
  assert_forward
    ~errors:(`Undefined 1)
    "lambda: undefined"
    (callable ~parameters:[] ~annotation:Type.Top);

  (* Lists. *)
  Type.Variable.Namespace.reset ();
  let empty_list =
    Type.list (Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped)
  in
  Type.Variable.Namespace.reset ();
  assert_forward "[]" empty_list;
  assert_forward "[1]" (Type.list Type.integer);
  assert_forward "[1, 'string']" (Type.list (Type.union [Type.integer; Type.string]));
  assert_forward ~errors:(`Undefined 1) "[undefined]" (Type.list Type.Top);
  assert_forward ~errors:(`Undefined 2) "[undefined, undefined]" (Type.list Type.Top);
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
  assert_forward
    ~precondition:["x", Type.undeclared]
    ~postcondition:["x", Type.undeclared]
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
           flow path that doesn't define `x`." ])
    "[x]"
    (Type.list Type.undeclared);

  (* Name. *)
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x"
    Type.integer;

  (* Sets. *)
  assert_forward "{1}" (Type.set Type.integer);
  assert_forward "{1, 'string'}" (Type.set (Type.union [Type.integer; Type.string]));
  assert_forward ~errors:(`Undefined 1) "{undefined}" (Type.set Type.Top);
  assert_forward ~errors:(`Undefined 2) "{undefined, undefined}" (Type.set Type.Top);
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
  assert_forward "**1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "*undefined" Type.Top;

  (* String literals. *)
  assert_forward "'string'" (Type.literal_string "string");
  assert_forward "f'string'" Type.string;
  assert_forward "f'string{1}'" Type.string;
  assert_forward ~errors:(`Undefined 1) "f'string{undefined}'" Type.string;

  (* Ternaries. *)
  assert_forward "3 if True else 1" Type.integer;
  assert_forward "1.0 if True else 1" Type.float;
  assert_forward "1 if True else 1.0" Type.float;
  assert_forward ~errors:(`Undefined 1) "undefined if True else 1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "1 if undefined else 1" (Type.literal_integer 1);
  assert_forward ~errors:(`Undefined 1) "1 if True else undefined" Type.Top;
  assert_forward ~errors:(`Undefined 3) "undefined if undefined else undefined" Type.Top;
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
  assert_forward ~errors:(`Undefined 1) "undefined," (Type.tuple [Type.Top]);
  assert_forward ~errors:(`Undefined 2) "undefined, undefined" (Type.tuple [Type.Top; Type.Top]);

  (* Unary expressions. *)
  assert_forward "not 1" Type.bool;
  assert_forward ~errors:(`Undefined 1) "not undefined" Type.bool;
  assert_forward "-1" Type.integer;
  assert_forward "+1" Type.integer;
  assert_forward "~1" Type.integer;
  assert_forward ~errors:(`Undefined 1) "-undefined" Type.Top;

  (* Yield. *)
  assert_forward "yield 1" (Type.generator (Type.literal_integer 1));
  assert_forward ~errors:(`Undefined 1) "yield undefined" (Type.generator Type.Top);
  assert_forward "yield" (Type.generator Type.none);

  (* Resolved annotation field. *)
  let assert_annotation ?(precondition = []) ?(environment = "") expression annotation =
    let expression =
      let expression = parse expression |> Preprocessing.expand_format_string in
      expression
      |> function
      | { Source.statements = [{ Node.value = Statement.Expression expression; _ }]; _ } ->
          expression
      | { Source.statements = [{ Node.value = Statement.Yield expression; _ }]; _ } -> expression
      | _ -> failwith "Unable to extract expression"
    in
    let resolution =
      ScratchProject.setup ~context ["__init__.py", environment] |> ScratchProject.build_resolution
    in
    let { State.resolved_annotation; _ } =
      State.forward_expression ~state:(create ~resolution precondition) ~expression
    in
    assert_equal
      ~cmp:(Option.equal Annotation.equal)
      ~printer:(fun annotation -> annotation >>| Annotation.show |> Option.value ~default:"None")
      annotation
      resolved_annotation
  in
  assert_annotation "1" None;
  assert_annotation
    ~environment:"x = 1"
    "x"
    (Some (Annotation.create_immutable ~global:true Type.integer));
  assert_annotation
    ~environment:"x: typing.Union[int, str] = 1"
    "x"
    (Some
       (Annotation.create_immutable
          ~global:true
          ~original:(Some (Type.union [Type.string; Type.integer]))
          (Type.union [Type.string; Type.integer])));
  assert_annotation
    ~environment:
      {|
        class Foo:
          def __init__(self):
            self.attribute: int = 1
      |}
    "Foo().attribute"
    (Some (Annotation.create_immutable ~global:true Type.integer))


let test_forward_statement context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let assert_forward
      ?(precondition_immutables = [])
      ?(postcondition_immutables = [])
      ?(errors = `Undefined 0)
      ?(bottom = false)
      ?expected_return
      precondition
      statement
      postcondition
    =
    let define =
      let annotation =
        expected_return >>| Format.asprintf " -> %a" Type.pp |> Option.value ~default:""
      in
      Format.asprintf "def foo()%s: ..." annotation
      |> parse_single_statement
      |> function
      | { Node.value = Define define; _ } -> define
      | _ -> failwith "Unable to parse define."
    in
    let module Context = struct
      let configuration = Configuration.Analysis.create ()

      let define = +define

      let calls = Location.Reference.Table.create ()
    end
    in
    let module Create = Create (Context) in
    let module State = State (Context) in
    let assert_state_equal =
      assert_equal
        ~cmp:State.equal
        ~printer:(Format.asprintf "%a" State.pp)
        ~pp_diff:(diff ~print:State.pp)
    in
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement :: rest; _ } -> statement :: rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward_statement ~state ~statement)
        ~init:(Create.create ~immutables:precondition_immutables ~resolution precondition)
        parsed
    in
    let errors =
      match errors with
      | `Specific errors -> errors
      | `Undefined count ->
          let rec errors sofar count =
            let error =
              "Undefined name [18]: Global name `undefined` is not defined, or there is at least \
               one control flow path that doesn't define `undefined`."
            in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_state_equal
      (Create.create ~bottom ~immutables:postcondition_immutables ~resolution postcondition)
      forwarded;
    assert_equal
      ~cmp:list_orderless_equal
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(description ~resolution))
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
  assert_forward
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `y` is not defined, or there is at least one control \
           flow path that doesn't define `y`." ])
    ["y", Type.undeclared]
    "x = y"
    ["x", Type.Any; "y", Type.undeclared];
  assert_forward
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `y` is not defined, or there is at least one control \
           flow path that doesn't define `y`." ])
    ["y", Type.Union [Type.integer; Type.undeclared]]
    "x = y"
    ["x", Type.integer; "y", Type.Union [Type.integer; Type.undeclared]];
  assert_forward
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `y` is not defined, or there is at least one control \
           flow path that doesn't define `y`." ])
    ["y", Type.undeclared]
    "x = [y]"
    ["x", Type.list Type.Any; "y", Type.undeclared];
  assert_forward
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `y` is not defined, or there is at least one control \
           flow path that doesn't define `y`." ])
    ["y", Type.Union [Type.integer; Type.undeclared]]
    "x = [y]"
    ["x", Type.list Type.integer; "y", Type.Union [Type.integer; Type.undeclared]];
  assert_forward
    ~errors:(`Specific ["Undefined type [11]: Type `Derp` is not defined."])
    ~postcondition_immutables:["x", (false, Type.Any)]
    []
    "x: Derp"
    ["x", Type.Any];
  assert_forward
    ~errors:
      (`Specific
        [ "Incompatible variable type [9]: x is declared to have type `str` "
          ^ "but is used as type `int`." ])
    ~postcondition_immutables:["x", (false, Type.string)]
    []
    "x: str = 1"
    ["x", Type.string];
  assert_forward
    ~postcondition_immutables:["x", (false, Type.union [Type.string; Type.integer])]
    []
    "x: typing.Union[int, str] = 1"
    ["x", Type.literal_integer 1];

  (* Assignments with tuples. *)
  assert_forward
    ["c", Type.integer; "d", Type.Top]
    "a, b = c, d"
    ["a", Type.integer; "b", Type.Top; "c", Type.integer; "d", Type.Top];
  assert_forward
    ~errors:(`Specific ["Unable to unpack [23]: Unable to unpack `int` into 2 values."])
    ["z", Type.integer]
    "x, y = z"
    ["x", Type.Top; "y", Type.Top; "z", Type.integer];
  assert_forward
    ~errors:(`Specific ["Unable to unpack [23]: Unable to unpack 3 values, 2 were expected."])
    ["z", Type.tuple [Type.integer; Type.string; Type.string]]
    "x, y = z"
    ["x", Type.Top; "y", Type.Top; "z", Type.tuple [Type.integer; Type.string; Type.string]];
  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.Top]; "y", Type.integer; "z", Type.Top];
  assert_forward
    ~postcondition_immutables:["x", (false, Type.tuple [Type.Any; Type.Any])]
    ~errors:
      (`Specific
        [ "Prohibited any [33]: Expression `x` is used as type `typing.Tuple[int, int]`; "
          ^ "given explicit type cannot contain `Any`." ])
    []
    "x: typing.Tuple[typing.Any, typing.Any] = 1, 2"
    ["x", Type.tuple [Type.literal_integer 1; Type.literal_integer 2]];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string]]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_forward
    ["z", Type.Tuple (Type.Unbounded Type.integer)]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.Tuple (Type.Unbounded Type.integer)];
  assert_forward
    ~errors:(`Specific ["Unable to unpack [23]: Unable to unpack `int` into 2 values."])
    []
    "(x, y), z = 1"
    ["x", Type.Top; "y", Type.Top; "z", Type.Top];
  assert_forward
    ["z", Type.list Type.integer]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.list Type.integer];
  assert_forward [] "x, y = return_tuple()" ["x", Type.integer; "y", Type.integer];
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Bounded (Concrete []))];

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
    ["c", Type.Tuple (Type.Unbounded Type.integer)]
    "a, b = c"
    ["a", Type.integer; "b", Type.integer; "c", Type.Tuple (Type.Unbounded Type.integer)];
  assert_forward
    ["c", Type.Tuple (Type.Unbounded Type.integer)]
    "*a, b = c"
    ["a", Type.list Type.integer; "b", Type.integer; "c", Type.Tuple (Type.Unbounded Type.integer)];

  (* Assignments with non-uniform sequences. *)
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "*a, b = x"
    [ "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.list (Type.union [Type.integer; Type.string]);
      "b", Type.float ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "a, *b = x"
    [ "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.float]) ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float]]
    "a, *b, c = x"
    [ "x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.integer]);
      "c", Type.float ];
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
    [ "x", Type.tuple [Type.integer; Type.float];
      "a", Type.integer;
      "b", Type.tuple [];
      "c", Type.float ];

  (* Assignments with immutables. *)
  assert_forward ~postcondition_immutables:["x", (true, Type.Top)] [] "global x" ["x", Type.Top];
  assert_forward
    ~postcondition_immutables:["y", (false, Type.integer)]
    []
    "y: int"
    ["y", Type.integer];
  assert_forward
    ~errors:
      (`Specific
        [ "Incompatible variable type [9]: y is declared to have type `int` "
          ^ "but is used as type `unknown`.";
          "Undefined name [18]: Global name `x` is not defined, or there is at least one control \
           flow path that doesn't define `x`." ])
    ~postcondition_immutables:["y", (false, Type.integer)]
    []
    "y: int = x"
    ["y", Type.integer];
  assert_forward
    ~precondition_immutables:["y", (false, Type.Top)]
    ~postcondition_immutables:["y", (false, Type.Top)]
    ["x", Type.Top; "y", Type.Top]
    "y = x"
    ["x", Type.Top; "y", Type.Top];
  assert_forward
    ~precondition_immutables:["y", (false, Type.string)]
    ~postcondition_immutables:["y", (false, Type.integer)]
    ["y", Type.string]
    "y: int"
    ["y", Type.integer];

  (* Delete. *)
  assert_forward
    ~errors:
      (`Specific
        [ "Incompatible parameter type [6]: Expected `str` for 1st anonymous parameter to call \
           `dict.__getitem__` but got `int`." ])
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
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is None"
    ["x", Type.optional Type.Bottom];
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
  assert_forward
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
    ~precondition_immutables:["x", (false, Type.float)]
    ~postcondition_immutables:["x", (false, Type.float)]
    ["x", Type.float]
    "assert x in [1]"
    ["x", Type.float];
  assert_forward
    ~errors:
      (`Specific
        ["Impossible assertion [25]: `x` has type `None`, assertion `x` will always fail."])
    ~bottom:true
    ["x", Type.none]
    "assert x"
    ["x", Type.none];
  assert_forward
    ~errors:
      (`Specific
        ["Impossible assertion [25]: `x` has type `None`, assertion `x` will always fail."])
    ~bottom:true
    ["x", Type.none]
    "assert x is not None"
    ["x", Type.none];

  (* Isinstance. *)
  assert_forward ["x", Type.Any] "assert isinstance(x, int)" ["x", Type.integer];
  assert_forward
    ["x", Type.Any; "y", Type.Top]
    "assert isinstance(y, str)"
    ["x", Type.Any; "y", Type.string];
  assert_forward
    ["x", Type.Any]
    "assert isinstance(x, (int, str))"
    ["x", Type.union [Type.integer; Type.string]];
  assert_forward ["x", Type.integer] "assert isinstance(x, (int, str))" ["x", Type.integer];
  assert_forward ~bottom:false ["x", Type.integer] "assert isinstance(x, str)" ["x", Type.string];
  assert_forward ~bottom:false ["x", Type.Bottom] "assert isinstance(x, str)" ["x", Type.string];
  assert_forward ~bottom:false ["x", Type.float] "assert isinstance(x, int)" ["x", Type.integer];
  assert_forward
    ~bottom:false
    ~errors:
      (`Specific
        [ "Incompatible parameter type [6]: "
          ^ "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` "
          ^ "but got `int`." ])
    ["x", Type.integer]
    "assert isinstance(x, 1)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
        [ "Impossible assertion [25]: `x` has type `int`, assertion `not isinstance(x,int)` will \
           always fail." ])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, int)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
        [ "Impossible assertion [25]: `x` has type `int`, assertion `not isinstance(x,float)` \
           will always fail." ])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, float)"
    ["x", Type.integer];
  assert_forward ~bottom:false ["x", Type.float] "assert not isinstance(x, int)" ["x", Type.float];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, int)"
    ["x", Type.optional Type.string];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, type(None))"
    ["x", Type.union [Type.integer; Type.string]];
  assert_forward
    ["my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string]; "x", Type.Top]
    "assert isinstance(x, my_type)"
    [ "my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string];
      "x", Type.union [Type.integer; Type.string] ];
  assert_forward
    ["my_type", Type.Tuple (Type.Unbounded (Type.meta Type.integer)); "x", Type.Top]
    "assert isinstance(x, my_type)"
    ["my_type", Type.Tuple (Type.Unbounded (Type.meta Type.integer)); "x", Type.integer];

  (* Works for general expressions. *)
  assert_forward
    ~errors:
      (`Specific
        [ "Impossible assertion [25]: `x.__add__(1)` has type `int`, assertion `not \
           isinstance(x.__add__(1),int)` will always fail." ])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x + 1, int)"
    ["x", Type.integer];
  assert_forward ~bottom:false ["x", Type.Bottom] "assert not isinstance(x, int)" ["x", Type.Bottom];
  assert_forward ~bottom:true [] "assert False" [];
  assert_forward ~bottom:false [] "assert (not True)" [];

  (* Raise. *)
  assert_forward
    ~errors:
      (`Specific
        [ "Invalid Exception [48]: Expression `1` has type `typing_extensions.Literal[1]` but \
           must extend BaseException." ])
    []
    "raise 1"
    [];
  assert_forward [] "raise Exception" [];
  assert_forward [] "raise Exception()" [];
  assert_forward
    ~errors:
      (`Specific
        [ "Undefined name [18]: Global name `undefined` is not defined, or there is at least one \
           control flow path that doesn't define `undefined`.";
          "Invalid Exception [48]: Expression `undefined` has type `unknown` but must extend \
           BaseException." ])
    []
    "raise undefined"
    [];
  assert_forward [] "raise" [];

  (* Return. *)
  assert_forward
    ~errors:
      (`Specific
        ["Missing return annotation [3]: Returning `int` but no return type is specified."])
    []
    "return 1"
    [];
  assert_forward ~expected_return:Type.integer [] "return 1" [];
  assert_forward
    ~expected_return:Type.string
    ~errors:(`Specific ["Incompatible return type [7]: Expected `str` but got `int`."])
    []
    "return 1"
    [];

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
        ~f:(fun state statement -> State.forward ~statement state)
        ~init:(create ~bottom:precondition_bottom precondition)
        parsed
    in
    assert_state_equal (create ~bottom:postcondition_bottom postcondition) forwarded
  in
  assert_forward [] "x = 1" ["x", Type.literal_integer 1];
  assert_forward ~precondition_bottom:true ~postcondition_bottom:true [] "x = 1" [];
  assert_forward ~postcondition_bottom:true [] "sys.exit(1)" []


let test_coverage context =
  let assert_coverage source expected =
    let coverage =
      let project = ScratchProject.setup ~context ["coverage_test.py", source] in
      let sources, _, environment = ScratchProject.build_environment project in
      let source = List.hd_exn sources in
      let configuration = ScratchProject.configuration_of project in
      let global_resolution = Environment.resolution environment () in
      TypeCheck.run ~configuration ~global_resolution ~source |> ignore;
      Coverage.get ~qualifier:!&"coverage_test" |> fun coverage -> Option.value_exn coverage
    in
    assert_equal ~printer:Coverage.show expected coverage
  in
  assert_coverage
    {| def foo(): pass |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 0 };
  assert_coverage
    {|
      def foo(y: int):
        if condition():
          x = y
        else:
          x = z
    |}
    { Coverage.full = 1; partial = 0; untyped = 1; ignore = 0; crashes = 0 };
  assert_coverage
    {|
      def foo(y: asdf):
        if condition():
          x = y
        else:
          x = 1
    |}
    { Coverage.full = 0; partial = 0; untyped = 2; ignore = 0; crashes = 0 }


type method_call = {
  direct_target: string;
  static_target: string;
  dispatch: Dependencies.Callgraph.dispatch;
}

let test_calls context =
  let assert_calls source calls =
    let project = ScratchProject.setup ~context ["qualifier.py", source] in
    let sources, _, environment = ScratchProject.build_environment project in
    let source = List.hd_exn sources in
    let configuration = ScratchProject.configuration_of project in
    (* Clear dependencies for all defines. *)
    let clear_calls
        { Node.value = { Statement.Define.signature = { Statement.Define.name; _ }; _ }; _ }
      =
      Dependencies.Callgraph.set ~caller:name ~callees:[]
    in
    Preprocessing.defines ~include_stubs:true ~include_nested:true ~include_toplevels:true source
    |> List.iter ~f:clear_calls;
    let global_resolution = Environment.resolution environment () in
    TypeCheck.run ~configuration ~global_resolution ~source |> ignore;

    (* Check calls. *)
    let assert_calls (caller, callees) =
      let expected_callees =
        let callee = function
          | `Method { direct_target; static_target; dispatch } ->
              Dependencies.Callgraph.Method
                {
                  direct_target = Reference.create direct_target;
                  static_target = Reference.create static_target;
                  dispatch;
                }
          | `Function name -> Dependencies.Callgraph.Function (Reference.create name)
        in
        List.map callees ~f:callee
        |> List.map ~f:Dependencies.Callgraph.show_callee
        |> String.Set.of_list
      in
      let actual_callees =
        Dependencies.Callgraph.get ~caller:(Reference.create caller)
        |> List.map ~f:Dependencies.Callgraph.show_callee
        |> String.Set.of_list
      in
      assert_equal
        ~printer:(fun set -> Set.to_list set |> String.concat ~sep:",")
        ~cmp:String.Set.equal
        expected_callees
        actual_callees
    in
    List.iter calls ~f:assert_calls
  in
  assert_calls
    {|
     def foo(): ...
     def calls_foo():
       foo()
   |}
    ["qualifier.foo", []; "qualifier.calls_foo", [`Function "qualifier.foo"]];
  assert_calls
    {|
     def foo(): ...
     def bar(): ...
     def calls_on_same_line():
       foo(); bar()
   |}
    ["qualifier.calls_on_same_line", [`Function "qualifier.foo"; `Function "qualifier.bar"]];

  (* Methods. *)
  assert_calls
    {|
     class Class:
       def method(): ...
     def calls_method(c: Class):
       c.method()
   |}
    [ ( "qualifier.calls_method",
        [ `Method
            {
              direct_target = "qualifier.Class.method";
              static_target = "qualifier.Class.method";
              dispatch = Dynamic;
            } ] ) ];

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
    [ ( "qualifier.ClassWithInit.__init__",
        [ `Method
            {
              direct_target = "object.__init__";
              static_target = "object.__init__";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_Class",
        [ `Method
            {
              direct_target = "object.__init__";
              static_target = "qualifier.Class.__init__";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_ClassWithInit",
        [ `Method
            {
              direct_target = "qualifier.ClassWithInit.__init__";
              static_target = "qualifier.ClassWithInit.__init__";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_ClassWithInit__init__",
        [ `Method
            {
              direct_target = "qualifier.ClassWithInit.__init__";
              static_target = "qualifier.ClassWithInit.__init__";
              dispatch = Static;
            } ] ) ];
  assert_calls
    {|
     class Class:
       @classmethod
       def classmethod(cls): ...
     def calls_class_method():
       Class.classmethod()
   |}
    [ ( "qualifier.calls_class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.classmethod";
              static_target = "qualifier.Class.classmethod";
              dispatch = Static;
            } ] ) ];

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
    [ ( "qualifier.calls_Class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.method";
              static_target = "qualifier.Class.method";
              dispatch = Dynamic;
            } ] );
      ( "qualifier.calls_Indirect_method",
        [ `Method
            {
              direct_target = "qualifier.Class.method";
              static_target = "qualifier.Indirect.method";
              dispatch = Dynamic;
            } ] );
      ( "qualifier.calls_Subclass_method",
        [ `Method
            {
              direct_target = "qualifier.Class.method";
              static_target = "qualifier.Subclass.method";
              dispatch = Dynamic;
            } ] );
      ( "qualifier.calls_OverridingSubclass_method",
        [ `Method
            {
              direct_target = "qualifier.OverridingSubclass.method";
              static_target = "qualifier.OverridingSubclass.method";
              dispatch = Dynamic;
            } ] ) ];

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
    [ ( "qualifier.calls_Class_class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.class_method";
              static_target = "qualifier.Class.class_method";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_Indirect_class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.class_method";
              static_target = "qualifier.Indirect.class_method";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_Subclass_class_method",
        [ `Method
            {
              direct_target = "qualifier.Subclass.class_method";
              static_target = "qualifier.Subclass.class_method";
              dispatch = Static;
            } ] );
      ( "qualifier.calls_Type_Class_class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.class_method";
              static_target = "qualifier.Class.class_method";
              dispatch = Dynamic;
            } ] );
      ( "qualifier.calls_Type_Indirect_class_method",
        [ `Method
            {
              direct_target = "qualifier.Class.class_method";
              static_target = "qualifier.Indirect.class_method";
              dispatch = Dynamic;
            } ] );
      ( "qualifier.calls_Type_Subclass_class_method",
        [ `Method
            {
              direct_target = "qualifier.Subclass.class_method";
              static_target = "qualifier.Subclass.class_method";
              dispatch = Dynamic;
            } ] ) ];

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
    [ ( "qualifier.calls_method_on_union",
        [ `Method
            {
              direct_target = "qualifier.Class.method";
              static_target = "qualifier.Class.method";
              dispatch = Dynamic;
            };
          `Method
            {
              direct_target = "qualifier.OtherClass.method";
              static_target = "qualifier.OtherClass.method";
              dispatch = Dynamic;
            } ] ) ];

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
    [ ( "qualifier.call_twice",
        [ `Method
            {
              direct_target = "qualifier.Foo.method";
              static_target = "qualifier.Foo.method";
              dispatch = Dynamic;
            } ] ) ];

  (* Properties. *)
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      def call_property(foo: Foo):
        x = foo.method
    |}
    [ ( "qualifier.call_property",
        [ `Method
            {
              direct_target = "qualifier.Foo.method";
              static_target = "qualifier.Foo.method";
              dispatch = Dynamic;
            } ] ) ];
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
    [ ( "qualifier.call_property",
        [ `Method
            {
              direct_target = "qualifier.Foo.method";
              static_target = "qualifier.Bar.method";
              dispatch = Dynamic;
            } ] ) ];

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
    ["qualifier.call_property", []];
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
    [ ( "qualifier.call_property",
        [ `Method
            {
              direct_target = "qualifier.Foo.method";
              static_target = "qualifier.Foo.method";
              dispatch = Dynamic;
            };
          `Method
            {
              direct_target = "qualifier.Bar.method";
              static_target = "qualifier.Bar.method";
              dispatch = Dynamic;
            } ] ) ];

  (* Statically invoking properties is illegal in the runtime - we default to dynamic dispatch, but
     this should be a type error, really. *)
  assert_calls
    {|
      class Foo:
        @property
        def method(self) -> int: ...
      def call_property():
        x = Foo.method
    |}
    [ ( "qualifier.call_property",
        [ `Method
            {
              direct_target = "qualifier.Foo.method";
              static_target = "qualifier.Foo.method";
              dispatch = Dynamic;
            } ] ) ]


let () =
  "type"
  >::: [ "initial" >:: test_initial;
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "widen" >:: test_widen;
         "check_annotation" >:: test_check_annotation;
         "forward_expression" >:: test_forward_expression;
         "forward_statement" >:: test_forward_statement;
         "forward" >:: test_forward;
         "coverage" >:: test_coverage;
         "module_exports" >:: test_module_exports;
         "object_callables" >:: test_object_callables;
         "callable_selection" >:: test_callable_selection;
         "calls" >:: test_calls ]
  |> Test.run
