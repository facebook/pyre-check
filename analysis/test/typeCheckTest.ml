(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Pyre
open Statement
open TypeCheck


open Test


let resolution = Test.resolution ()


let create
    ?(bottom = false)
    ?(define = Test.mock_define)
    ?expected_return
    ?(resolution = Test.resolution ())
    ?(immutables = [])
    annotations =
  let resolution =
    let annotations =
      let immutables = String.Map.of_alist_exn immutables in
      let annotify (name, annotation) =
        let annotation =
          let create annotation =
            match Map.find immutables name with
            | Some (global, original) ->
                Annotation.create_immutable ~original:(Some original) ~global annotation
            | _ ->
                Annotation.create annotation
          in
          create annotation
        in
        Access.create name, annotation
      in
      List.map annotations ~f:annotify
      |> Access.Map.of_alist_exn
    in
    Resolution.with_annotations resolution ~annotations
  in
  let define =
    +{
      define with
      Define.return_annotation = expected_return >>| Type.expression;
    }
  in
  State.create ~bottom ~resolution ~define ()


let assert_state_equal =
  assert_equal
    ~cmp:State.equal
    ~printer:(Format.asprintf "%a" State.pp)
    ~pp_diff:(diff ~print:State.pp)


let test_initial _ =
  let assert_initial ?parent ?(errors = []) ?(environment = "") define state =
    let resolution =
      parse environment
      |> (fun source -> source :: (Test.typeshed_stubs ()))
      |> (fun sources -> Test.resolution ~sources ())
    in
    let initial =
      let define =
        match parse_single_statement define with
        | { Node.value = Define define; _ } ->
            { define with Define.parent = parent >>| Access.create }
        | _ ->
            failwith "Unable to parse define."
      in
      let variables =
        let extract_variables { Node.value = { Parameter.annotation; _ }; _ } =
          match annotation with
          | None -> []
          | Some annotation ->
              let annotation = Resolution.parse_annotation resolution annotation in
              Type.free_variables annotation
        in
        List.concat_map define.parameters ~f:extract_variables
        |> List.dedup_and_sort ~compare:Type.compare
      in
      let add_variable resolution variable =
        Resolution.add_type_variable resolution ~variable
      in
      let resolution = List.fold variables ~init:resolution ~f:add_variable in
      State.initial ~resolution (+define)
    in
    assert_state_equal state initial;
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(fun elements -> Format.asprintf "%a" Sexp.pp [%message (elements: string list)])
      (List.map (State.errors initial) ~f:(Error.description ~show_error_traces:false))
      errors
  in

  assert_initial
    "def foo(x: int): ..."
    (create ~immutables:["x", (false, Type.integer)] ["x", Type.integer]);

  assert_initial
    ~errors:[
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type " ^
      "`float`.";
    ]
    "def foo(x: int = 1.0): ..."
    (create ~immutables:["x", (false, Type.integer)] ["x", Type.integer]);

  assert_initial
    ~errors:[
      "Missing parameter annotation [2]: Parameter `x` has type `float` but no type is specified.";
    ]
    "def foo(x = 1.0): ..."
    (create ["x", Type.float]);

  assert_initial
    "def foo(x: int) -> int: ..."
    (create
      ~immutables:["x", (false, Type.integer)]
      ~expected_return:Type.integer ["x", Type.integer]);

  assert_initial
    "def foo(x: float, y: str): ..."
    (create
      ~immutables:["x", (false, Type.float); "y", (false, Type.string)]
      ["x", Type.float; "y", Type.string]);

  assert_initial
    ~errors:["Missing parameter annotation [2]: Parameter `x` has no type specified."]
    "def foo(x): ..."
    (create ["x", Type.Any]);
  assert_initial
    ~errors:["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."]
    "def foo(x: typing.Any): ..."
    (create ~immutables:["x", (false, Type.Any)] ["x", Type.Any]);
  assert_initial
    ~parent:"Foo"
    ~errors:[]
    ~environment:"class Foo: ..."
    "def __eq__(self, other: object): ..."
    (create
      ~immutables:["other", (false, Type.object_primitive)]
      ["self", Type.Primitive "Foo"; "other", Type.object_primitive]);

  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    "def foo(self): ..."
    (create ["self", Type.Primitive "Foo"]);
  assert_initial
    ~parent:"Foo"
    ~environment:"class Foo: ..."
    ~errors:["Missing parameter annotation [2]: Parameter `a` has no type specified."]
    "@staticmethod\ndef foo(a): ..."
    (create ["a", Type.Any]);

  assert_initial
    ~environment:"T = typing.TypeVar('T')"
    "def foo(x: T): ..."
    (create
       ~immutables:["x", (false, Type.mark_variables_as_bound (Type.variable "T"))]
       ["x", Type.mark_variables_as_bound (Type.variable "T")])


let test_less_or_equal _ =
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


let test_join _ =
  (* <= *)
  assert_state_equal (State.join (create []) (create [])) (create []);
  assert_state_equal
    (State.join (create []) (create ["x", Type.integer]))
    (create ["x", Type.Top]);
  assert_state_equal (State.join (create []) (create ["x", Type.Top])) (create ["x", Type.Top]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.integer; "y", Type.integer]))
    (create ["x", Type.integer; "y", Type.Top]);

  (* > *)
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create []))
    (create ["x", Type.Top]);
  assert_state_equal
    (State.join (create ["x", Type.Top]) (create []))
    (create ["x", Type.Top]);

  (* partial order *)
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.string]))
    (create ["x", Type.union [Type.string; Type.integer]]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["y", Type.integer]))
    (create
       ["x", Type.Top; "y", Type.Top])


let test_widen _ =
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


let test_check_annotation _ =
  let assert_check_annotation source expression descriptions =
    let resolution = Test.resolution ~sources:(parse source :: Test.typeshed_stubs ()) () in
    let state = create ~resolution [] in
    let { State.errors; _ }, _ = State.parse_and_check_annotation ~state !expression in
    let errors = List.map ~f:(Error.description ~show_error_traces:false) (Set.to_list errors) in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      descriptions
      errors
  in
  assert_check_annotation
    ""
    "x"
    ["Undefined type [11]: Type `x` is not defined."];
  assert_check_annotation
    "x: int = 1"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation
    "x: typing.Type[int] = int"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation
    "x = int"
    "x"
    [];
  assert_check_annotation
    "x: typing.Any"
    "x"
    [];
  assert_check_annotation
    {|
      class Foo: ...
      x = Foo
    |}
    "x"
    [];
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


type attribute = {
  name: string;
  defined: bool;
}


and stripped =
  | Attribute of attribute
  | Unknown
  | SignatureFound of { callable: string; callees: string list }
  | SignatureNotFound of Annotated.Signature.reason option
  | NotCallable of Type.t
  | Value


and step = {
  annotation: Type.t;
  element: stripped;
}
[@@deriving compare, eq, show]


let test_redirect _ =
  let assert_redirect ?parent ?source access (expected_access, expected_locals) =
    let resolution =
      let sources =
        match source with
        | Some source ->
            [
              parse
                ~qualifier:[]
                ~handle:"source.pyi"
                source
              |> Preprocessing.preprocess;
            ]
        | None ->
            []
      in
      AnnotatedTest.populate_with_sources (sources @ Test.typeshed_stubs ())
      |> (fun environment -> TypeCheck.resolution environment ())
      |> Resolution.with_parent ~parent
    in
    let access = parse_single_access access in
    let access, resolution = AccessState.redirect ~resolution access in
    assert_equal
      ~printer:Expression.Access.show_general_access
      ~cmp:Expression.Access.equal_general_access
      access
      (Access.SimpleAccess (parse_single_access expected_access));
    let assert_in_scope (expected_name, expected_type) =
      Access.create expected_name
      |> (fun access -> Option.value_exn (Resolution.get_local ~access resolution))
      |> Annotation.annotation
      |> assert_equal ~printer:Type.show expected_type
    in
    List.iter expected_locals ~f:assert_in_scope
  in
  assert_redirect ~source:"a = 1" "a" ("a", []);
  assert_redirect
    ~parent:(Access.create "Subclass")
    ~source:
      {|
        class Superclass: pass
        class Subclass(Superclass): pass
      |}
    "super()"
    ("$super", ["$super", Type.Primitive "Superclass"]);
  assert_redirect
    ~parent:(Access.create "Superclass")
    ~source:
      {|
        class Superclass: pass
        class Subclass(Superclass): pass
      |}
    "super().foo()"
    ("$super.foo()", ["$super", Type.object_primitive]);

  assert_redirect
    ~parent:(Access.create "Superclass")
    ~source:
      {|
        class Superclass: pass
        class Subclass(Superclass): pass
      |}
    "Subclass.super().foo()"
    ("Subclass.super().foo()", []);

  assert_redirect
    ~source:
      {|
        a = 1
      |}
    "type(a)"
    ("$type", ["$type", Type.meta Type.integer]);

  assert_redirect
    ~source:
      {|
        a = 1
      |}
    "type(type(a))"
    ("$type", ["$type", Type.meta (Type.meta Type.integer)]);

  assert_redirect
    ~source:
      {|
        a = 1
      |}
    "type(type(a))"
    ("$type", ["$type", Type.meta (Type.meta Type.integer)])


let test_resolve_exports _ =
  let assert_resolve ~sources access expected_access =
    let resolution =
      let sources =
        let to_source (qualifier, source) =
          parse
            ~qualifier:(Access.create qualifier)
            ~handle:(qualifier ^ ".pyi")
            source
          |> Preprocessing.preprocess
        in
        List.map sources ~f:to_source
      in
      AnnotatedTest.populate_with_sources (sources @ Test.typeshed_stubs ())
      |> (fun environment -> TypeCheck.resolution environment ())
    in
    let access =
      parse_single_access access
      |> AccessState.resolve_exports ~resolution
    in
    assert_equal ~printer:Access.show ~cmp:Access.equal access (parse_single_access expected_access)
  in
  assert_resolve
    ~sources:[]
    "a.b"
    "a.b";
  assert_resolve
    ~sources:[
      "a", "from b import foo";
      "b", "foo = 1";
    ]
    "a.foo"
    "b.foo";
  assert_resolve
    ~sources:[
      "a", "from b import foo";
      "b", "from c import bar as foo";
      "c", "from d import cow as bar";
      "d", "cow = 1"
    ]
    "a.foo"
    "d.cow";
  assert_resolve
    ~sources:[
      "qualifier", "from qualifier.foo import foo";  (* __init__.py module. *)
      "qualifier.foo", "foo = 1";
    ]
    "qualifier.foo.foo"
    "qualifier.foo.foo"


let test_forward_access _ =
  let to_resolution sources =
    AnnotatedTest.populate_with_sources (sources @ Test.typeshed_stubs ())
    |> (fun environment -> TypeCheck.resolution environment ())
  in
  let parse_annotation ~resolution annotation =
    annotation
    |> parse_single_expression
    |> Resolution.parse_annotation resolution
  in
  let assert_fold ?(additional_sources = []) ?parent ~source access expected =
    let resolution =
      let source =
        parse source
        |> Preprocessing.preprocess
      in
      to_resolution (source :: additional_sources)
      |> Resolution.with_parent ~parent
    in
    let access, resolution =
      let access = parse_single_access access ~preprocess:true in
      match AccessState.redirect ~resolution access with
      | Access.SimpleAccess access, resolution ->
          access, resolution
      | _ ->
          access, resolution
    in
    let steps =
      let steps steps ~resolution:_ ~resolved ~element ~lead:_ =
        let step =
          let stripped element: stripped =
            let open TypeCheck.AccessState in
            match element with
            | Attribute { origin = Module []; _ } ->
                Unknown
            | Attribute { attribute; defined; _ } ->
                Attribute { name = Access.show attribute; defined }
            | Signature {
                signature = Annotated.Signature.Found { callable; _ };
                callees;
                _;
              } ->
                let callees =
                  let show_callee { Type.Callable.kind; _ } =
                    match kind with
                    | Type.Callable.Named name -> Access.show name
                    | _ -> "Anonymous"
                  in
                  List.map callees ~f:show_callee
                in
                SignatureFound { callable = Type.show (Type.Callable callable); callees }
            | Signature {
                signature = Annotated.Signature.NotFound { reason; _; };
                _;
              } ->
                SignatureNotFound reason
            | NotCallable annotation ->
                NotCallable annotation
            | Value ->
                Value
          in
          { annotation = Annotation.annotation resolved; element = stripped element }
        in
        step :: steps
      in
      let resolution = Resolution.with_parent resolution ~parent in
      access
      |> TypeCheck.State.forward_access ~resolution ~initial:[] ~f:steps
      |> List.rev
    in
    assert_equal
      ~printer:(fun steps -> List.map ~f:show_step steps |> String.concat ~sep:"\n")
      ~cmp:(List.equal ~equal:equal_step)
      expected
      steps
  in
  let signature_not_found signature = SignatureNotFound signature in

  assert_fold ~source:"" "unknown" [{ annotation = Type.Top; element = Unknown }];
  assert_fold ~source:"" "unknown.unknown" [{ annotation = Type.Top; element = Unknown }];

  assert_fold
    ~source:"integer: int = 1"
    "integer"
    [{ annotation = Type.integer; element = Value }];
  assert_fold
    ~source:"string: str = \"\""
    "string"
    [{ annotation = Type.string; element = Value }];

  (* Unions. *)
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union"
    [{ annotation = Type.union [Type.string; Type.integer]; element = Value }];
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union.__doc__"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      { annotation = Type.string; element = Attribute { name = "__doc__"; defined = true } };
    ];
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union.__lt__"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      {
        annotation =
          {|
            typing.Union[
              typing.Callable('int.__lt__')[
                [Named($parameter$other, int)],
                bool,
              ],
              typing.Callable('str.__lt__')[
                [Named($parameter$other, int)],
                float,
              ],
            ]
          |}
          |> parse_annotation ~resolution;
        element = Attribute { name = "__lt__"; defined = true };
      };
    ];
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union.__lt__(1)"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      {
        annotation =
          {|
            typing.Union[
              typing.Callable('int.__lt__')[
                [Named($parameter$other, int)],
                bool,
              ],
              typing.Callable('str.__lt__')[
                [Named($parameter$other, int)],
                float,
              ],
            ]
          |}
          |> parse_annotation ~resolution;
        element = Attribute { name = "__lt__"; defined = true };
      };
      {
        annotation = Type.union [Type.bool; Type.float];
        element = SignatureFound {
            callable =
              "typing.Callable" ^
              "[[Named(other, int)], typing.Union[bool, float]]";
            callees = ["int.__lt__"; "str.__lt__"];
          };
      };
    ];
  (* Passing the wrong type. *)
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union.__add__('string')"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      {
        annotation =
          {|
            typing.Union[
              typing.Callable('int.__add__')[
                [Named($parameter$other, int)],
                int,
              ],
              typing.Callable('str.__add__')[
                [Named($parameter$other, str)],
                str,
              ],
            ]
          |}
          |> parse_annotation ~resolution;
        element = Attribute { name = "__add__"; defined = true };
      };
      {
        annotation = Type.integer;
        element =
          {
            Annotated.Signature.actual = Type.string;
            expected = Type.integer;
            name = None;
            position = 1;
          }
          |> Node.create_with_default_location
          |> (fun node -> Annotated.Signature.Mismatch node)
          |> Option.some
          |> signature_not_found;
      };
    ];
  (* Names don't match up. *)
  assert_fold
    ~source:"union: typing.Union[str, int] = 1"
    "union.__ne__(unknown)"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      {
        annotation =
          {|
            typing.Union[
              typing.Callable('int.__ne__')[
                [Named($parameter$other_integer, $unknown)],
                bool,
              ],
              typing.Callable('str.__ne__')[
                [Named($parameter$other, $unknown)],
                int,
              ],
            ]
          |}
          |> parse_annotation ~resolution;
        element = Attribute { name = "__ne__"; defined = true };
      };
      {
        annotation = Type.bool;
        element = SignatureNotFound None;
      };
    ];

  (* Classes. *)
  assert_fold
    ~source:
      {|
        class Class: pass
      |}
    "Class"
    [{ annotation = Type.meta (Type.Primitive "Class"); element = Value }];
  assert_fold
    ~source:
      {|
        class Class: pass
        instance: Class
      |}
    "instance"
    [{ annotation = Type.Primitive "Class"; element = Value }];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
        instance: Class
      |}
    "instance.attribute"
    [
      { annotation = Type.Primitive "Class"; element = Value };
      { annotation = Type.integer; element = Attribute { name = "attribute"; defined = true } };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
        instance: Class
      |}
    "instance.undefined.undefined"
    [
      { annotation = Type.Primitive "Class"; element = Value };
      { annotation = Type.Top; element = Attribute { name = "undefined"; defined = false } };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          def method(self) -> int: ...
        instance: Class
      |}
    "instance.method()"
    [
      { annotation = Type.Primitive "Class"; element = Value };
      {
        annotation =
          parse_annotation ~resolution "typing.Callable('Class.method')[[], int]";
        element = Attribute { name = "method"; defined = true };
      };
      {
        annotation = Type.integer;
        element = SignatureFound {
            callable = "typing.Callable(Class.method)[[], int]";
            callees = ["Class.method"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          def method(self) -> int: ...
        instance: Class
      |}
    "instance()"
    [
      { annotation = Type.Primitive "Class"; element = Value };
      { annotation = Type.Top; element = NotCallable (Type.Primitive "Class") };
    ];

  let bound_type_variable =
    Type.variable "TV_Bound" ~constraints:(Bound (Type.Primitive "Class"))
  in

  assert_fold
    ~source:
      {|
        class Class: pass
        TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
        v_instance: TV_Bound
      |}
    "v_instance" [{ annotation = bound_type_variable; element = Value }];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
        TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
        v_instance: TV_Bound
      |}
    "v_instance.attribute"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.integer; element = Attribute { name = "attribute"; defined = true } };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
        TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
        v_instance: TV_Bound
      |}
    "v_instance.undefined.undefined"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.Top; element = Attribute { name = "undefined"; defined = false } };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          def method(self) -> int: ...
        TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
        v_instance: TV_Bound
      |}
    "v_instance.method()"
    [
      { annotation = bound_type_variable; element = Value };
      {
        annotation =
          parse_annotation ~resolution "typing.Callable('Class.method')[[], int]";
        element = Attribute { name = "method"; defined = true };
      };
      {
        annotation = Type.integer;
        element = SignatureFound {
            callable = "typing.Callable(Class.method)[[], int]";
            callees = ["Class.method"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          def method(self) -> int: ...
        TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
        v_instance: TV_Bound
      |}
    "v_instance()"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.Top; element = NotCallable bound_type_variable };
    ];

  let explicit_type_variable =
    Type.variable
      "TV_Explicit"
      ~constraints:(Explicit [Type.Primitive "Class"; Type.Primitive "Other"])
  in

  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
          def method(self) -> int: ...
        class Other:
          attribute: str = "A"
          def method(self) -> str: ...
        TV_Explicit = typing.TypeVar("TV_Explicit", Class, Other)
        v_explicit_instance: TV_Explicit
      |}
    "v_explicit_instance"
    [{ annotation = explicit_type_variable; element = Value }];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
          def method(self) -> int: ...
        class Other:
          attribute: str = "A"
          def method(self) -> str: ...
        TV_Explicit = typing.TypeVar("TV_Explicit", Class, Other)
        v_explicit_instance: TV_Explicit
      |}
    "v_explicit_instance.attribute"
    [
      { annotation = explicit_type_variable; element = Value };
      {
        annotation = Type.Union [Type.integer; Type.string];
        element = Attribute { name = "attribute"; defined = true };
      };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
          def method(self) -> int: ...
        class Other:
          attribute: str = "A"
          def method(self) -> str: ...
        TV_Explicit = typing.TypeVar("TV_Explicit", Class, Other)
        v_explicit_instance: TV_Explicit
      |}
    "v_explicit_instance.undefined.undefined"
    [
      { annotation = explicit_type_variable; element = Value };
      { annotation = Type.Top; element = Attribute { name = "undefined"; defined = false } };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
          def method(self) -> int: ...
        class Other:
          attribute: str = "A"
          def method(self) -> str: ...
        TV_Explicit = typing.TypeVar("TV_Explicit", Class, Other)
        v_explicit_instance: TV_Explicit
      |}
    "v_explicit_instance.method()"
    [
      { annotation = explicit_type_variable; element = Value };
      {
        annotation =
          Type.Union [
            Type.Callable {
              kind = Named (Access.create "Class.method");
              implementation = {
                annotation= Type.integer;
                parameters= Defined [];
              };
              overloads = [];
              implicit = Some {
                  Type.Callable.implicit_annotation = Type.Primitive "Class";
                  name = Access.create "self";
                };
            };
            Type.Callable {
              kind = Named (Access.create "Other.method");
              implementation = {
                annotation= Type.string;
                parameters= Defined [];
              };
              overloads = [];
              implicit = Some {
                  Type.Callable.implicit_annotation = Type.Primitive "Other";
                  name = Access.create "self";
                };
            };
          ];
        element = Attribute { name = "method"; defined = true };
      };
      {
        annotation = Type.Union [Type.integer; Type.string];
        element = SignatureFound {
            callable = "typing.Callable[[], typing.Union[int, str]]";
            callees = ["Class.method"; "Other.method"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
          def method(self) -> int: ...
        class Other:
          attribute: str = "A"
          def method(self) -> str: ...
        TV_Explicit = typing.TypeVar("TV_Explicit", Class, Other)
        v_explicit_instance: TV_Explicit
      |}
    "v_explicit_instance()"
    [
      { annotation = explicit_type_variable; element = Value };
      { annotation = Type.Top; element = NotCallable explicit_type_variable };
    ];


  assert_fold
    ~source:
      {|
        class Super:
          pass
        class Class(Super):
          attribute: int = 1
          def method(self) -> int: ...
      |}
    ~parent:(Access.create "Class")
    "super().__init__()"
    [
      { annotation = Type.Primitive "Super"; element = Value };
      {
        annotation =
          parse_annotation ~resolution "typing.Callable('object.__init__')[[], None]";
        element = Attribute { name = "__init__"; defined = true };
      };
      {
        annotation = Type.none;
        element = SignatureFound {
            callable = "typing.Callable(object.__init__)[[], None]";
            callees = ["object.__init__"];
          };
      };
    ];

  (* Functions. *)
  assert_fold
    ~source:
      {|
        def function() -> str: ...
      |}
    "function()"
    [
      {
        annotation = parse_annotation ~resolution "typing.Callable('function')[[], str]";
        element = Value;
      };
      {
        annotation = Type.string;
        element = SignatureFound {
            callable = "typing.Callable(function)[[], str]";
            callees = ["function"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        def function() -> str:
          def nested() -> str:
            ...
      |}
    "function.nested()"
    [
      {
        annotation = parse_annotation ~resolution "typing.Callable('function')[[], str]";
        element = Value;
      };
      {
        annotation = parse_annotation ~resolution "typing.Callable('function.nested')[[], str]";
        element = Value;
      };
      {
        annotation = Type.string;
        element = SignatureFound {
            callable = "typing.Callable(function.nested)[[], str]";
            callees = ["function.nested"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        def function() -> str:
          def nested() -> str:
            ...
      |}
    "function.unknown_nested()"
    [
      {
        annotation = parse_annotation ~resolution "typing.Callable('function')[[], str]";
        element = Value;
      };
      { annotation = Type.Top; element = Value };
    ];

  let source_with_generics =
    {|
        TSelf = typing.TypeVar('TSelf', bound="C")
        TG = typing.TypeVar('TG')
        class C:
          def inner(self, x: int) -> None:
            pass
          def verbose(self: TSelf, x: int) -> TSelf:
            self.inner(x)
            return self
        class G(C, typing.Generic[TG]): pass
        g: G[int]
    |}
  in
  let resolution_with_generics =
    to_resolution [parse source_with_generics |>  Preprocessing.preprocess]
  in
  assert_fold
    ~source:source_with_generics
    "g.verbose"
    [
      {
        annotation = parse_annotation ~resolution:resolution_with_generics "G[int]";
        element = Value
      };
      {
        annotation =
          parse_annotation
            ~resolution:resolution_with_generics
            "typing.Callable('C.verbose')[[Named(x, int)], G[int]]";
        element = Attribute { name = "verbose"; defined = true };
      };
    ];

  (* Modules. *)
  assert_fold
    ~additional_sources:[
      parse
        ~qualifier:(Access.create "os")
        {|
          sep: str = '/'
        |};
    ]
    ~source:""
    "os.sep"
    [{ annotation = Type.string; element = Value }];

  assert_fold
    ~additional_sources:[
      parse
        ~qualifier:(Access.create "empty.stub")
        ~local_mode:Source.PlaceholderStub
        ~handle:"empty/stub.pyi"
        ""
    ]
    ~source:""
    "empty.stub.unknown"
    [
      { annotation = Type.Top; element = Value };
    ];
  assert_fold
    ~additional_sources:[
      parse
        ~qualifier:(Access.create "empty.stub")
        ~local_mode:Source.PlaceholderStub
        ~handle:"empty/stub.pyi"
        ""
    ]
    ~source:
      {|
        suppressed: empty.stub.submodule.Suppressed = ...
      |}
    "suppressed.attribute"
    [{ annotation = Type.Top; element = Value }];

  assert_fold
    ~additional_sources:[
      parse
        ~qualifier:(Access.create "empty.stub")
        ~local_mode:Source.PlaceholderStub
        ~handle:"empty/stub.pyi"
        ""
    ]
    ~source:
      {|
        suppressed: empty.stub.submodule.Suppressed = ...
      |}
    "empty.stub.any_attribute"
    [{ annotation = Type.Top; element = Value }];
  assert_fold
    ~additional_sources:[
      parse
        ~qualifier:(Access.create "has_getattr")
        "def __getattr__(name: str) -> typing.Any: ..."
      |> Preprocessing.preprocess
    ]
    ~source:""
    "has_getattr.any_attribute"
    [{ annotation = parse_annotation ~resolution "typing.Any"; element = Value }];

  assert_fold
    ~source:
      {|
        class Class:
          attribute: int = 1
        instance: Class
      |}
    "instance.attribute + 1.0"
    [
      { annotation = Type.Primitive "Class"; element = Value };
      { annotation = Type.integer; element = Attribute { name = "attribute"; defined = true } };
      {
        annotation = parse_annotation ~resolution "typing.Callable('int.__add__')[[Named(other, int)], int]";
        element = Attribute { name = "__add__"; defined = true };
      };
      {
        annotation = Type.float;
        element = SignatureFound {
            callable = "typing.Callable(float.__radd__)[[Named(other, float)], float]";
            callees = ["float.__radd__"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        T = typing.TypeVar('T')
        class Class(typing.Generic[T]):
          def __init__(self, x: T) -> None:
            pass
        Class_int: typing.Type[Class[int]]
      |}
    "Class_int(7)"
    [
      { annotation = Type.meta (Type.parametric "Class" [Type.integer]); element = Value };
      {
        annotation = Type.parametric "Class" [Type.integer];
        element = SignatureFound {
            callable = "typing.Callable(Class.__init__)[[Named(x, int)], Class[int]]";
            callees = ["Class.__init__"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        T = typing.TypeVar('T')
        class Class(typing.Generic[T]):
          def __init__(self, x: T) -> None:
            pass
        Class_int: typing.Type[Class[int]]
      |}
    "Class_int('seven')"
    [
      { annotation = Type.meta (Type.parametric "Class" [Type.integer]); element = Value };
      {
        annotation = Type.parametric "Class" [Type.integer];
        element =
          {
            Annotated.Signature.actual = Type.string;
            expected = Type.integer;
            name = None;
            position = 1;
          }
          |> Node.create_with_default_location
          |> (fun node -> Annotated.Signature.Mismatch node)
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    ~source:
      {|
        T = typing.TypeVar('T')
        class Class(typing.Generic[T]):
          def __init__(self, x: T) -> None:
            pass
        Class_type: typing.Type[Class]
      |}
    "Class_type(True)"
    [
      { annotation = Type.meta (Type.Primitive "Class"); element = Value };
      {
        annotation = Type.parametric "Class" [Type.bool];
        element = SignatureFound {
            callable = "typing.Callable(Class.__init__)[[Named(x, bool)], Class[bool]]";
            callees = ["Class.__init__"];
          };
      };
    ];

  (* Typed dictionaries. *)
  let movie_typed_dictionary = {
    annotation = Type.TypedDictionary {
        name = "Movie";
        fields = [
          { name = "year"; annotation = Type.integer };
          { name = "title"; annotation = Type.string };
        ];
        total = true;
      };
    element = Value;
  } in

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie.title"
    [
      movie_typed_dictionary;
      { annotation = Type.Top; element = Attribute { name = "title"; defined = false } };
    ];

  let get_item = {
    annotation =
      parse_annotation ~resolution (
        "typing.Callable('typing.Mapping.__getitem__')" ^
        "[[Named(k, typing._KT)], typing._VT_co]");
    element = Attribute { name = "__getitem__"; defined = true } ;
  } in

  let resolution_with_movie =
    to_resolution
      [
        parse "Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})"
        |>  Preprocessing.preprocess
      ]
  in
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['title']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "str";
        element = SignatureFound {
            callable =
              "typing.Callable(typing.Mapping.__getitem__)" ^
              "[[Named(k, Variable[typing._KT])], str]";
            callees = ["typing.Mapping.__getitem__"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['year']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "int";
        element = SignatureFound {
            callable =
              "typing.Callable(typing.Mapping.__getitem__)" ^
              "[[Named(k, Variable[typing._KT])], int]";
            callees = ["typing.Mapping.__getitem__"];
          };
      };
    ];

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['missing']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "$unknown";
        element =
          Annotated.Signature.TypedDictionaryMissingKey {
            typed_dictionary_name = "Movie";
            missing_key = "missing";
          }
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie[string]"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "$unknown";
        element =
          Annotated.Signature.TypedDictionaryAccessWithNonLiteral [ "year"; "title" ]
          |> Option.some
          |> signature_not_found;
      };
    ];

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "Movie(title='Blade Runner', year=1982)"
    [
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "typing.Type[Movie]";
        element = Value;
      };
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "Movie";
        element = SignatureFound {
            callable =
              "typing.Callable(__init__)" ^
              "[[Variable(, unknown), Named(year, int), Named(title, str)]," ^
              " TypedDict `Movie` with fields (year: int, title: str)]";
            callees = ["__init__"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "Movie(year=1982, title='Blade Runner')"
    [
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "typing.Type[Movie]";
        element = Value;
      };
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "Movie";
        element = SignatureFound {
            callable =
              "typing.Callable(__init__)" ^
              "[[Variable(, unknown), Named(year, int), Named(title, str)]," ^
              " TypedDict `Movie` with fields (year: int, title: str)]";
            callees = ["__init__"];
          };
      };
    ];
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "Movie(year='Blade Runner', title=1982)"
    [
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "typing.Type[Movie]";
        element = Value;
      };
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "Movie";
        element =
          {
            Annotated.Signature.actual = parse_annotation ~resolution:resolution_with_movie "str";
            expected = parse_annotation ~resolution:resolution_with_movie "int";
            name = (Some "$parameter$year");
            position = 1;
          }
          |> Node.create_with_default_location
          |> (fun node -> Annotated.Signature.Mismatch node)
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "Movie('Blade Runner', 1982)"
    [
      {
        annotation = parse_annotation ~resolution:resolution_with_movie ("typing.Type[Movie]");
        element = Value;
      };
      {
        annotation = parse_annotation ~resolution:resolution_with_movie "Movie";
        element =
          Annotated.Signature.TooManyArguments { expected = 0; provided = 2 }
          |> Option.some
          |> signature_not_found;
      };
    ];

  let set_item =
    {
      annotation =
        parse_annotation ~resolution:resolution_with_movie (
          "typing.Callable('TypedDictionary.__setitem__')" ^
          "[[Named(key, $unknown), Named(value, $unknown)], None]");
      element = Attribute { name = "__setitem__"; defined = true } ;
    }
  in

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['year'] = 7"
    [
      movie_typed_dictionary;
      set_item;
      {
        annotation = Type.none;
        element = SignatureFound {
            callable =
              "typing.Callable(TypedDictionary.__setitem__)" ^
              "[[Named(key, str), Named(value, int)], None]";
            callees = ["TypedDictionary.__setitem__"];
          };
      };
    ];

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['year'] = 'string'"
    [
      movie_typed_dictionary;
      set_item;
      {
        annotation = Type.none;
        element =
          +{
            Annotated.Signature.actual = Type.string;
            expected = Type.integer;
            name = None;
            position = 2;
          }
          |> (fun node -> Annotated.Signature.Mismatch node)
          |> Option.some
          |> signature_not_found;
      };
    ];

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie['missing'] = 7"
    [
      movie_typed_dictionary;
      set_item;
      {
        annotation = Type.none;
        element =
          Annotated.Signature.TypedDictionaryMissingKey {
            typed_dictionary_name = "Movie";
            missing_key = "missing";
          }
          |> Option.some
          |> signature_not_found;
      };
    ];

  assert_fold
    ~source:
      {|
        Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
        movie: Movie
      |}
    "movie[string] = 7"
    [
      movie_typed_dictionary;
      set_item;
      {
        annotation = Type.none;
        element =
          Annotated.Signature.TypedDictionaryAccessWithNonLiteral [ "year"; "title" ]
          |> Option.some
          |> signature_not_found;
      };
    ]


let assert_resolved sources access expected =
  let resolution =
    AnnotatedTest.populate_with_sources (sources @ typeshed_stubs ())
    |> fun environment -> TypeCheck.resolution environment ()
  in
  let resolved =
    parse_single_access access
    |> TypeCheck.State.forward_access
      ~resolution
      ~initial:Type.Top
      ~f:(fun _ ~resolution:_ ~resolved ~element:_ ~lead:_ -> Annotation.annotation resolved)
  in
  assert_equal ~printer:Type.show ~cmp:Type.equal expected resolved


let test_module_exports _ =
  let assert_exports_resolved access expected =
    [
      "implementing.py",
      {|
        def implementing.function() -> int: ...
        constant: int = 1
      |};
      "exporting.py",
      {|
        from implementing import function, constant
        from implementing import function as aliased
        from indirect import cyclic
      |};
      "indirect.py",
      {|
        from exporting import constant, cyclic
      |};
      "wildcard.py",
      {|
        from exporting import *
      |};
      "exporting_wildcard_default.py",
      {|
        from implementing import function, constant
        from implementing import function as aliased
        __all__ = ["constant"]
      |};
      "wildcard_default.py",
      {|
        from exporting_wildcard_default import *
      |};
    ]
    |> parse_list
    |> List.map ~f:(fun handle -> Option.value_exn (Ast.SharedMemory.Sources.get handle))
    |> (fun sources -> assert_resolved sources access expected)
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
      [
        parse
          ~qualifier:(Access.create "loop.b")
          {|
            b: int = 1
          |};
        parse
          ~qualifier:(Access.create "loop.a")
          {|
            from loop.b import b
          |};
        parse
          ~qualifier:(Access.create "loop")
          {|
            from loop.a import b
          |};
        parse
          ~qualifier:(Access.create "no_loop.b")
          {|
            b: int = 1
          |};
        parse
          ~qualifier:(Access.create "no_loop.a")
          {|
            from no_loop.b import b as c
          |};
        parse
          ~qualifier:(Access.create "no_loop")
          {|
            from no_loop.a import c
          |};
      ]
  in
  assert_fixpoint_stop "loop.b" Type.Top;
  assert_fixpoint_stop "no_loop.c" Type.integer


let test_object_callables _ =
  let assert_resolved access annotation =
    assert_resolved
      [
        parse
          ~qualifier:(Access.create "module")
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
        |> Preprocessing.qualify;
      ]
      access
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


let test_callable_selection _ =
  let assert_resolved source access annotation =
    assert_resolved
      [parse source]
      access
      (Type.create ~aliases:(fun _ -> None) (parse_single_expression annotation))
  in

  assert_resolved "call: typing.Callable[[], int]" "call()" "int";
  assert_resolved "call: typing.Callable[[int], int]" "call()" "int"


let test_forward_expression _ =
  let assert_forward
      ?(precondition = [])
      ?(postcondition = [])
      ?(errors = `Undefined 0)
      expression
      annotation =
    let expression =
      parse expression
      |> Preprocessing.expand_format_string
      |> function
      | { Source.statements = [{ Node.value = Statement.Expression expression; _ }]; _ } ->
          expression
      | { Source.statements = [{ Node.value = Statement.Yield expression; _ }]; _ } ->
          expression
      | _ ->
          failwith "Unable to extract expression"
    in
    let { State.state = forwarded; resolved } =
      State.forward_expression
        ~state:(create precondition)
        ~expression
    in
    assert_equal ~cmp:Type.equal ~printer:Type.show annotation resolved;
    assert_state_equal (create postcondition) forwarded;
    let errors =
      match errors with
      | `Specific errors ->
          errors
      | `Undefined count ->
          let rec errors sofar count =
            let error = "Undefined name [18]: Global name `undefined` is undefined." in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(Error.description ~show_error_traces:false))
  in

  (* Access. *)
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    "x.add_key(1)"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.integer]
    "x.add_value(1)"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.string]
    "x.add_both(1, 'string')"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~errors:(`Specific [
        "Incompatible parameter type [6]: "^
        "Expected `int` for 1st anonymous parameter to call `dict.add_key` but got `str`.";
      ])
    "x.add_key('string')"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.Top ~value:Type.Bottom]
    ~errors:(`Undefined 1)
    "x.add_key(undefined)"
    Type.none;

  (* Await. *)
  assert_forward "await awaitable_int()" Type.integer;
  assert_forward
    ~errors:(`Specific [
        "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
        "Undefined name [18]: Global name `undefined` is undefined.";
      ])
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

  assert_optional_forward ~postcondition:["x", Type.integer] "x and 1" (Type.optional Type.integer);
  assert_optional_forward "1 and x" (Type.optional Type.integer);
  assert_optional_forward ~postcondition:["x", Type.integer] "x and x" (Type.optional Type.integer);

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
  assert_forward ~errors:(`Undefined 1) "undefined < 1" Type.Top;
  assert_forward ~errors:(`Undefined 2) "undefined == undefined" Type.Top;

  (* Complex literal. *)
  assert_forward "1j" Type.complex;
  assert_forward "1" Type.integer;
  assert_forward "\"\"" Type.string;
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
  assert_forward
    "{**{1: 1}, **{'a': 'b'}}"
    (Type.dictionary ~key:Type.Any ~value:Type.Any);
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
  assert_forward "..." Type.ellipsis;

  (* False literal. *)
  assert_forward "False" Type.bool;

  (* Float literal. *)
  assert_forward "1.0" Type.float;

  (* Generators. *)
  assert_forward "(element for element in [1])" (Type.generator Type.integer);
  assert_forward "(element for element in [])" (Type.generator Type.Bottom);
  assert_forward
    "((element, independent) for element in [1] for independent in ['string'])"
    (Type.generator (Type.tuple [Type.integer; Type.string]));
  assert_forward
    "(nested for element in [[1]] for nested in element)"
    (Type.generator Type.integer);
  assert_forward
    ~errors:(`Undefined 1)
    "(undefined for element in [1])"
    (Type.generator Type.Top);
  assert_forward
    ~errors:(`Undefined 1)
    "(element for element in undefined)"
    (Type.generator Type.Top);

  (* Lambda. *)
  let callable ~parameters ~annotation =
    let parameters =
      let open Type.Callable in
      let to_parameter name =
        Parameter.Named {
          Parameter.name = Access.create name;
          annotation = Type.Any;
          default = false;
        }
      in
      Defined (List.map parameters ~f:to_parameter)
    in
    Type.Callable.create ~parameters ~annotation ()
  in
  assert_forward "lambda: 1" (callable ~parameters:[] ~annotation:Type.integer);
  assert_forward
    "lambda parameter: parameter"
    (callable
       ~parameters:["parameter"]
       ~annotation:Type.Any);
  assert_forward
    ~errors:(`Undefined 1)
    "lambda: undefined"
    (callable ~parameters:[] ~annotation:Type.Top);

  (* Lists. *)
  assert_forward "[]" (Type.list Type.Bottom);
  assert_forward "[1]" (Type.list Type.integer);
  assert_forward "[1, 'string']" (Type.list (Type.union [Type.integer; Type.string]));
  assert_forward ~errors:(`Undefined 1) "[undefined]" (Type.list Type.Top);
  assert_forward ~errors:(`Undefined 2) "[undefined, undefined]" (Type.list Type.Top);
  assert_forward "[element for element in [1]]" (Type.list Type.integer);
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
    ~errors:(`Specific ["Undefined name [18]: Global name `x` is undefined."])
    "[x]"
    (Type.list Type.undeclared);

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
  assert_forward "'string'" Type.string;
  assert_forward "f'string'" Type.string;
  assert_forward "f'string{1}'" Type.string;
  assert_forward ~errors:(`Undefined 1) "f'string{undefined}'" Type.string;

  (* Ternaries. *)
  assert_forward "3 if True else 1" Type.integer;
  assert_forward "1.0 if True else 1" Type.float;
  assert_forward "1 if True else 1.0" Type.float;
  assert_forward ~errors:(`Undefined 1) "undefined if True else 1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "1 if undefined else 1" Type.integer;
  assert_forward ~errors:(`Undefined 1) "1 if True else undefined" Type.Top;
  assert_forward ~errors:(`Undefined 3) "undefined if undefined else undefined" Type.Top;
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x if x is not None else 32"
    Type.integer;

  (* True literal. *)
  assert_forward "True" Type.bool;

  (* Tuples. *)
  assert_forward "1," (Type.tuple [Type.integer]);
  assert_forward "1, 'string'" (Type.tuple [Type.integer; Type.string]);
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
  assert_forward "yield 1" (Type.generator Type.integer);
  assert_forward ~errors:(`Undefined 1) "yield undefined" (Type.generator Type.Top);
  assert_forward "yield" (Type.generator Type.none)


let test_forward_statement _ =
  let assert_forward
      ?(precondition_immutables = [])
      ?(postcondition_immutables = [])
      ?expected_return
      ?(errors = `Undefined 0)
      ?(bottom = false)
      precondition
      statement
      postcondition =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement::rest; _ } -> statement::rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward_statement ~state ~statement)
        ~init:(create ?expected_return ~immutables:precondition_immutables precondition)
        parsed
    in
    assert_state_equal
      (create ~bottom ~immutables:postcondition_immutables postcondition)
      forwarded;
    let errors =
      match errors with
      | `Specific errors ->
          errors
      | `Undefined count ->
          let rec errors sofar count =
            let error = "Undefined name [18]: Global name `undefined` is undefined." in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(Error.description ~show_error_traces:false))
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
      (`Specific ["Undefined name [18]: Global name `y` is undefined."])
    ["y", Type.undeclared]
    "x = y"
    ["x", Type.Any; "y", Type.undeclared];

  assert_forward
    ~errors:
      (`Specific ["Undefined name [18]: Global name `y` is undefined."])
    ["y", Type.Union [Type.integer; Type.undeclared]]
    "x = y"
    ["x", Type.integer; "y", Type.Union [Type.integer; Type.undeclared]];

  assert_forward
    ~errors:
      (`Specific ["Undefined name [18]: Global name `y` is undefined."])
    ["y", Type.undeclared]
    "x = [y]"
    ["x", Type.list Type.Any; "y", Type.undeclared];

  assert_forward
    ~errors:
      (`Specific ["Undefined name [18]: Global name `y` is undefined."])
    ["y", Type.Union [Type.integer; Type.undeclared]]
    "x = [y]"
    ["x", Type.list Type.integer; "y", Type.Union [Type.integer; Type.undeclared]];

  assert_forward
    ~errors:
      (`Specific ["Undefined type [11]: Type `Derp` is not defined."])
    ~postcondition_immutables:["x", (false, Type.Top)]
    []
    "x: Derp"
    ["x", Type.Top];

  assert_forward
    ~errors:
      (`Specific [
        "Incompatible variable type [9]: x is declared to have type `str` " ^
        "but is used as type `int`."])
    ~postcondition_immutables:["x", (false, Type.string)]
    []
    "x: str = 1"
    ["x", Type.string];

  assert_forward
    ~postcondition_immutables:["x", (false, Type.union [Type.string; Type.integer])]
    []
    "x: typing.Union[int, str] = 1"
    ["x", Type.integer];

  (* Assignments with tuples. *)
  assert_forward
    ["c", Type.integer; "d", Type.Top]
    "a, b = c, d"
    ["a", Type.integer; "b", Type.Top; "c", Type.integer; "d", Type.Top];
  assert_forward
    ~errors:
      (`Specific ["Unable to unpack [23]: Unable to unpack `int` into 2 values."])
    ["z", Type.integer]
    "x, y = z"
    ["x", Type.Top; "y", Type.Top; "z", Type.integer];

  assert_forward
    ~errors:
      (`Specific ["Unable to unpack [23]: Unable to unpack 3 values, 2 were expected."])
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
      (`Specific [
        "Prohibited any [33]: Expression `x` is used as type `typing.Tuple[int, int]`; " ^
        "given explicit type cannot contain `Any`."])
    []
    "x: typing.Tuple[typing.Any, typing.Any] = 1, 2"
    ["x", Type.tuple [Type.integer; Type.integer]];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string]]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_forward
    ["z", Type.Tuple (Type.Unbounded Type.integer)]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.Tuple (Type.Unbounded Type.integer)];
  assert_forward
    ~errors:
      (`Specific [
          "Unable to unpack [23]: Unable to unpack `int` into 2 values.";
          "Unable to unpack [23]: Unable to unpack `unknown` into 2 values.";
        ])
    []
    "(x, y), z = 1"
    ["x", Type.Top; "y", Type.Top; "z", Type.Top];
  assert_forward
    ["z", Type.list Type.integer]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.list Type.integer];
  assert_forward
    []
    "x, y = return_tuple()"
    ["x", Type.integer; "y", Type.integer;];
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Bounded [])];

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

  (* Assignments with immutables. *)
  assert_forward ~postcondition_immutables:["x", (true, Type.Top)] [] "global x" ["x", Type.Top];
  assert_forward
    ~postcondition_immutables:["y", (false, Type.integer)]
    []
    "y: int"
    ["y", Type.integer];
  assert_forward
    ~errors:(`Specific [
        "Incompatible variable type [9]: y is declared to have type `int` " ^
        "but is used as type `unknown`.";
        "Undefined name [18]: Global name `x` is undefined.";
      ])
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

  (* Assert. *)
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x"
    ["x", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.integer]
    "assert y"
    ["x", Type.optional Type.integer; "y", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is not None"
    ["x", Type.integer];

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
  assert_forward
    ["x", Type.integer]
    "assert isinstance(x, (int, str))"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ["x", Type.integer]
    "assert isinstance(x, str)"
    ["x", Type.string];
  assert_forward
    ~bottom:false
    ["x", Type.Bottom]
    "assert isinstance(x, str)"
    ["x", Type.string];
  assert_forward
    ~bottom:false
    ["x", Type.float]
    "assert isinstance(x, int)"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ~errors:
      (`Specific
         ["Incompatible parameter type [6]: " ^
          "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
          "but got `int`."])
    ["x", Type.integer]
    "assert isinstance(x, 1)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x` has type `int`, checking if `x` not " ^
          "isinstance `int` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, int)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x` has type `int`, checking if `x` not " ^
          "isinstance `float` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, float)"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ["x", Type.float]
    "assert not isinstance(x, int)"
    ["x", Type.float];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, int)"
    ["x", Type.optional Type.string];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, type(None))"
    [
      "$type", Type.meta Type.none;
      "x", Type.union [Type.integer; Type.string];
    ];
  assert_forward
    [
      "my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string];
      "x", Type.Top;
    ]
    "assert isinstance(x, my_type)"
    [
      "my_type", Type.tuple [Type.meta Type.integer; Type.meta Type.string];
      "x", Type.union [Type.integer; Type.string];
    ];
  assert_forward
    [
      "my_type", Type.Tuple (Type.Unbounded (Type.meta Type.integer));
      "x", Type.Top;
    ]
    "assert isinstance(x, my_type)"
    [
      "my_type", Type.Tuple (Type.Unbounded (Type.meta Type.integer));
      "x", Type.integer;
    ];

  (* Works for general expressions. *)
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x.__add__(1)` has type `int`, checking if " ^
          "`x.__add__(1)` not isinstance `int` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x + 1, int)"
    ["x", Type.integer];

  assert_forward
    ~bottom:false
    ["x", Type.Bottom]
    "assert not isinstance(x, int)"
    ["x", Type.Bottom];

  assert_forward
    ~bottom:true
    []
    "assert False"
    [];
  assert_forward
    ~bottom:false
    []
    "assert (not True)"
    [];

  (* Raise. *)
  assert_forward [] "raise 1" [];
  assert_forward ~errors:(`Undefined 1) [] "raise undefined" [];
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


let test_forward _ =
  let assert_forward
      ?(precondition_bottom = false)
      ?(postcondition_bottom = false)
      precondition
      statement
      postcondition =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement::rest; _ } -> statement::rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward ~statement state)
        ~init:(create ~bottom:precondition_bottom precondition)
        parsed
    in
    assert_state_equal (create ~bottom:postcondition_bottom postcondition) forwarded;
  in

  assert_forward [] "x = 1" ["x", Type.integer];
  assert_forward ~precondition_bottom:true ~postcondition_bottom:true [] "x = 1" [];

  assert_forward ~postcondition_bottom:true [] "sys.exit(1)" []


let test_coverage _ =
  let assert_coverage source expected =
    let coverage =
      let environment = Test.environment () in
      let handle = "coverage_test.py" in
      TypeCheck.run
        ~configuration:Test.mock_configuration
        ~environment
        ~source:(parse ~handle source)
      |> ignore;
      Coverage.get ~handle:(File.Handle.create handle)
      |> (fun coverage -> Option.value_exn coverage)
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
    { Coverage.full = 0; partial = 0; untyped = 2; ignore = 0; crashes = 0 };

  assert_coverage
    {|
      def foo(y) -> int:
        x = returns_undefined()
        return x
    |}
    { Coverage.full = 0; partial = 0; untyped = 2; ignore = 0; crashes = 0 }


let () =
  "type">:::[
    "initial">::test_initial;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "widen">::test_widen;
    "check_annotation">::test_check_annotation;
    "redirect">::test_redirect;
    "resolve_exports">::test_resolve_exports;
    "forward_access">::test_forward_access;
    "forward_expression">::test_forward_expression;
    "forward_statement">::test_forward_statement;
    "forward">::test_forward;
    "coverage">::test_coverage;
    "module_exports">::test_module_exports;
    "object_callables">::test_object_callables;
    "callable_selection">::test_callable_selection;
  ]
  |> Test.run
