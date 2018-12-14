(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement

open Test
open AnnotatedTest

module Attribute = Annotated.Attribute
module Class = Annotated.Class


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


let test_fold _ =
  let resolution =
    let sources = [
      parse
        ~qualifier:(Access.create "empty.stub")
        ~local_mode:Source.PlaceholderStub
        ~handle:"empty/stub.pyi"
        "";
      parse
        ~qualifier:(Access.create "empty.stub.submodule")
        ~handle:"empty/stub/submodule.py"
        "class Suppressed: ...";
      parse
        ~qualifier:(Access.create "has_getattr")
        ~handle:"has_getattr.pyi"
        "def __getattr__(name: str) -> Any: ..."
      |> Preprocessing.preprocess;
      parse
        {|
          integer: int = 1
          string: str = 'string'

          union: typing.Union[str, int] = 1

          class Super: pass
          class Class(Super):
            attribute: int = 1
            def method(self) -> int: ...
          instance: Class
          TV_Bound = typing.TypeVar("TV_Bound", bound=Class)
          v_instance: TV_Bound

          def function() -> str:
            def nested() -> str: ...

          suppressed: empty.stub.submodule.Suppressed = ...
        |}
      |> Preprocessing.preprocess;
      parse
        ~qualifier:(Access.create "os")
        {|
          sep: str = '/'
        |};
      parse
        {|
          Movie = mypy_extensions.TypedDict('Movie', {'year': int, 'title': str})
          movie: Movie
        |}
      |> Preprocessing.preprocess;
    ] in
    populate_with_sources (sources @ Test.typeshed_stubs)
    |> fun environment -> TypeCheck.resolution environment ()
  in
  let parse_annotation annotation =
    annotation
    |> parse_single_expression
    |> Resolution.parse_annotation resolution
  in
  let assert_fold ?parent access expected =
    let steps =
      let steps steps ~resolution:_ ~resolved ~element ~lead:_ =
        let step =
          let stripped element: stripped =
            match element with
            | Annotated.Access.Attribute { origin = Annotated.Access.Module []; _ } ->
                Unknown
            | Annotated.Access.Attribute { attribute; defined; _ } ->
                Attribute { name = Access.show attribute; defined }
            | Annotated.Access.Signature {
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
            | Annotated.Access.Signature {
                signature = Annotated.Signature.NotFound { reason; _; };
                _;
              } ->
                SignatureNotFound reason
            | Annotated.Access.NotCallable annotation ->
                NotCallable annotation
            | Annotated.Access.Value ->
                Value
          in
          { annotation = Annotation.annotation resolved; element = stripped element }
        in
        step :: steps
      in
      let resolution = Resolution.with_parent resolution ~parent in
      parse_single_access access ~preprocess:true
      |> Annotated.Access.create
      |> Annotated.Access.fold ~resolution ~initial:[] ~f:steps
      |> List.rev
    in
    assert_equal
      ~printer:(fun steps -> List.map ~f:show_step steps |> String.concat ~sep:"\n")
      ~cmp:(List.equal ~equal:equal_step)
      expected
      steps
  in

  let signature_not_found signature = SignatureNotFound signature in

  assert_fold "unknown" [{ annotation = Type.Top; element = Unknown }];
  assert_fold "unknown.unknown" [{ annotation = Type.Top; element = Unknown }];

  assert_fold "integer" [{ annotation = Type.integer; element = Value }];
  assert_fold "string" [{ annotation = Type.string; element = Value }];

  (* Unions. *)
  assert_fold "union" [{ annotation = Type.union [Type.string; Type.integer]; element = Value }];
  assert_fold
    "union.__doc__"
    [
      { annotation = Type.union [Type.string; Type.integer]; element = Value };
      { annotation = Type.string; element = Attribute { name = "__doc__"; defined = true } };
    ];
  assert_fold
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
          |> parse_annotation;
        element = Attribute { name = "__lt__"; defined = true };
      };
    ];
  assert_fold
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
          |> parse_annotation;
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
          |> parse_annotation;
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
          |> parse_annotation;
        element = Attribute { name = "__ne__"; defined = true };
      };
      {
        annotation = Type.bool;
        element = SignatureNotFound None;
      };
    ];

  (* Classes. *)
  assert_fold "Class" [{ annotation = Type.meta (Type.primitive "Class"); element = Value }];
  assert_fold "instance" [{ annotation = Type.primitive "Class"; element = Value }];
  assert_fold
    "instance.attribute"
    [
      { annotation = Type.primitive "Class"; element = Value };
      { annotation = Type.integer; element = Attribute { name = "attribute"; defined = true } };
    ];
  assert_fold
    "instance.undefined.undefined"
    [
      { annotation = Type.primitive "Class"; element = Value };
      { annotation = Type.Top; element = Attribute { name = "undefined"; defined = false } };
    ];
  assert_fold
    "instance.method()"
    [
      { annotation = Type.primitive "Class"; element = Value };
      {
        annotation =
          parse_annotation "typing.Callable('Class.method')[[], int]";
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
    "instance()"
    [
      { annotation = Type.primitive "Class"; element = Value };
      { annotation = Type.Top; element = NotCallable (Type.primitive "Class") };
    ];

  let bound_type_variable =
    Type.Variable {
      variable = Identifier.create "TV_Bound";
      constraints = Bound (Type.primitive "Class");
      variance = Invariant;
    }
  in

  assert_fold "v_instance" [{ annotation = bound_type_variable; element = Value }];
  assert_fold
    "v_instance.attribute"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.integer; element = Attribute { name = "attribute"; defined = true } };
    ];
  assert_fold
    "v_instance.undefined.undefined"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.Top; element = Attribute { name = "undefined"; defined = false } };
    ];
  assert_fold
    "v_instance.method()"
    [
      { annotation = bound_type_variable; element = Value };
      {
        annotation =
          parse_annotation "typing.Callable('Class.method')[[], int]";
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
    "v_instance()"
    [
      { annotation = bound_type_variable; element = Value };
      { annotation = Type.Top; element = NotCallable bound_type_variable };
    ];

  assert_fold
    ~parent:(Access.create "Class")
    "super().__init__()"
    [
      { annotation = Type.primitive "Super"; element = Value };
      {
        annotation =
          parse_annotation "typing.Callable('object.__init__')[[], None]";
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
    "function()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      {
        annotation = Type.string;
        element = SignatureFound {
            callable = "typing.Callable(function)[[], str]";
            callees = ["function"];
          };
      };
    ];
  assert_fold
    "function.nested()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      {
        annotation = parse_annotation "typing.Callable('function.nested')[[], str]";
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
    "function.unknown_nested()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      { annotation = Type.Top; element = Value };
    ];

  (* Modules. *)
  assert_fold "os.sep" [{ annotation = Type.string; element = Value }];

  assert_fold "empty.stub.unknown" [{ annotation = Type.Top; element = Value }];
  assert_fold "suppressed.attribute" [{ annotation = Type.Top; element = Value }];

  assert_fold "empty.stub.any_attribute" [{ annotation = Type.Top; element = Value }];
  assert_fold
    "has_getattr.any_attribute"
    [{ annotation = parse_annotation "Any"; element = Value }];

  (* Typed dictionaries. *)
  let movie_typed_dictionary = {
    annotation = Type.TypedDictionary {
        name = Identifier.create "Movie";
        fields = [
          { name = "year"; annotation = Type.integer };
          { name = "title"; annotation = Type.string };
        ];
      };
    element = Value;
  } in

  assert_fold
    "movie.title"
    [
      movie_typed_dictionary;
      { annotation = Type.Top; element = Attribute { name = "title"; defined = false } };
    ];

  let get_item = {
    annotation =
      parse_annotation (
        "typing.Callable('typing.Mapping.__getitem__')" ^
        "[[Named(k, typing._KT)], typing._VT_co]");
    element = Attribute { name = "__getitem__"; defined = true } ;
  } in

  assert_fold
    "movie['title']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation "str";
        element = SignatureFound {
            callable =
              "typing.Callable(typing.Mapping.__getitem__)" ^
              "[[Named(k, Variable[typing._KT])], str]";
            callees = ["typing.Mapping.__getitem__"];
          };
      };
    ];
  assert_fold
    "movie['year']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation "int";
        element = SignatureFound {
            callable =
              "typing.Callable(typing.Mapping.__getitem__)" ^
              "[[Named(k, Variable[typing._KT])], int]";
            callees = ["typing.Mapping.__getitem__"];
          };
      };
    ];

  assert_fold
    "movie['missing']"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation "$unknown";
        element =
          Annotated.Signature.TypedDictionaryMissingKey {
            typed_dictionary_name = Identifier.create "Movie";
            missing_key = "missing";
          }
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    "movie[string]"
    [
      movie_typed_dictionary;
      get_item;
      {
        annotation = parse_annotation "$unknown";
        element =
          Annotated.Signature.TypedDictionaryAccessWithNonLiteral [ "year"; "title" ]
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    "Movie(title='Blade Runner', year=1982)"
    [
      {
        annotation = parse_annotation ("typing.Type[Movie]");
        element = Value;
      };
      {
        annotation = parse_annotation "Movie";
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
    "Movie(year=1982, title='Blade Runner')"
    [
      {
        annotation = parse_annotation ("typing.Type[Movie]");
        element = Value;
      };
      {
        annotation = parse_annotation "Movie";
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
    "Movie(year='Blade Runner', title=1982)"
    [
      {
        annotation = parse_annotation ("typing.Type[Movie]");
        element = Value;
      };
      {
        annotation = parse_annotation "Movie";
        element =
          {
            Annotated.Signature.actual = parse_annotation "str";
            expected = parse_annotation "int";
            name = (Some (Identifier.create "$parameter$year"));
            position = 1;
          }
          |> Node.create_with_default_location
          |> (fun node -> Annotated.Signature.Mismatch node)
          |> Option.some
          |> signature_not_found;
      };
    ];
  assert_fold
    "Movie('Blade Runner', 1982)"
    [
      {
        annotation = parse_annotation ("typing.Type[Movie]");
        element = Value;
      };
      {
        annotation = parse_annotation "Movie";
        element =
          Annotated.Signature.TooManyArguments { expected = 3; provided = 5 }
          |> Option.some
          |> signature_not_found;
      };
    ];

  let set_item =
    {
      annotation =
        parse_annotation (
          "typing.Callable('TypedDictionary.__setitem__')" ^
          "[[Named(key, $unknown), Named(value, $unknown)], None]");
      element = Attribute { name = "__setitem__"; defined = true } ;
    }
  in

  assert_fold
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
    "movie['missing'] = 7"
    [
      movie_typed_dictionary;
      set_item;
      {
        annotation = Type.none;
        element =
          Annotated.Signature.TypedDictionaryMissingKey {
            typed_dictionary_name = Identifier.create "Movie";
            missing_key = "missing";
          }
          |> Option.some
          |> signature_not_found;
      };
    ];

  assert_fold
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
    populate_with_sources (sources @ typeshed_stubs)
    |> fun environment -> TypeCheck.resolution environment ()
  in
  let resolved =
    parse_single_access access
    |> Annotated.Access.create
    |> Annotated.Access.fold
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
  (* TODO(T37956736): Restore this test to return $bottom. *)
  (* assert_resolved "module.call()" "$bottom"; *)
  assert_resolved "module.callable()" "int";

  assert_resolved "module.meta" "typing.Type[module.Call[int, str]]";
  (* TODO(T37956736): Restore this test. *)
  (* assert_resolved "module.meta" "module.Call[$bottom, $bottom]"; *)
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


let () =
  "access">:::[
    "fold">::test_fold;
    "module_exports">::test_module_exports;
    "object_callables">::test_object_callables;
    "callable_selection">::test_callable_selection;
  ]
  |> Test.run
