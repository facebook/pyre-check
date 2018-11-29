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
  | SignatureFound
  | SignatureNotFound of Annotated.Signature.reason option
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

          class Class:
            attribute: int = 1
            def method(self) -> int: ...
          instance: Class

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
  let assert_fold access expected =
    let steps =
      let steps steps ~resolution:_ ~resolved ~element ~lead:_ =
        let step =
          let module Element = Annotated.Access.Element in
          let stripped element: stripped =
            match element with
            | Element.Attribute { Element.origin = Element.Module []; _ } ->
                Unknown
            | Element.Attribute { Element.attribute; defined; _ } ->
                Attribute { name = Access.show attribute; defined }
            | Element.Signature {
                Element.signature = Annotated.Signature.Found _;
                _;
              } ->
                SignatureFound
            | Element.Signature {
                Element.signature = Annotated.Signature.NotFound { reason; _; };
                _;
              } ->
                SignatureNotFound reason
            | Element.Value ->
                Value
          in
          { annotation = Annotation.annotation resolved; element = stripped element }
        in
        step :: steps
      in
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

  assert_fold "unknown" [{ annotation = Type.Top; element = Unknown }];
  assert_fold "unknown.unknown" [{ annotation = Type.Top; element = Unknown }];

  assert_fold "integer" [{ annotation = Type.integer; element = Value }];
  assert_fold "string" [{ annotation = Type.string; element = Value }];

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
          parse_annotation "typing.Callable('Class.method')[[Named(self, $unknown)], int]";
        element = Attribute { name = "method"; defined = true };
      };
      { annotation = Type.integer; element = SignatureFound };
    ];

  assert_fold
    "function()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      { annotation = Type.string; element = SignatureFound };
    ];
  assert_fold
    "function.nested()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      {
        annotation = parse_annotation "typing.Callable('function.nested')[[], str]";
        element = Value;
      };
      { annotation = Type.string; element = SignatureFound };
    ];
  assert_fold
    "function.unknown_nested()"
    [
      { annotation = parse_annotation "typing.Callable('function')[[], str]"; element = Value };
      { annotation = Type.Top; element = Value };
    ];

  assert_fold "os.sep" [{ annotation = Type.string; element = Value }];

  assert_fold "empty.stub.unknown" [{ annotation = Type.Top; element = Value }];
  assert_fold "suppressed.attribute" [{ annotation = Type.Top; element = Value }];

  assert_fold "empty.stub.any_attribute" [{ annotation = Type.Top; element = Value }];
  assert_fold
    "has_getattr.any_attribute"
    [{ annotation = parse_annotation "Any"; element = Value }];

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
        "[[Named(self, $unknown),  Named(k, typing._KT)], typing._VT_co]");
    element = Attribute { name = "__getitem__"; defined = true } ;
  } in

  assert_fold
    "movie['title']"
    [
      movie_typed_dictionary;
      get_item;
      { annotation = parse_annotation "str"; element = SignatureFound };
    ];
  assert_fold
    "movie['year']"
    [
      movie_typed_dictionary;
      get_item;
      { annotation = parse_annotation "int"; element = SignatureFound };
    ];

  let signature_not_found signature = SignatureNotFound signature in
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
        element = SignatureFound;
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
        element = SignatureFound;
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
            position = 2;
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
          Annotated.Signature.TooManyArguments { Annotated.Signature.expected = 4; provided = 6 }
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
  assert_resolved "module.call()" "$bottom";
  assert_resolved "module.callable()" "int";

  assert_resolved "module.meta" "typing.Type[module.Call[int, str]]";
  assert_resolved "module.meta()" "module.Call[$bottom, $bottom]";
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
