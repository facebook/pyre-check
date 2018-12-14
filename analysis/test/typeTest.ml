(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre

open Test


let test_create _ =
  let assert_create ?(aliases = (fun _ -> None)) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create ~aliases (parse_single_expression source))
  in

  assert_create "foo" (Type.primitive "foo");
  assert_create "foo.bar" (Type.primitive "foo.bar");
  assert_create "foo.$local_qualifier$bar" (Type.primitive "foo.bar");

  assert_create "$deleted" Type.Deleted;
  assert_create "object" Type.Object;
  assert_create "$unknown" Type.Top;

  assert_create "foo[bar]" (Type.parametric "foo" [Type.primitive "bar"]);
  assert_create
    "foo[bar, baz]"
    (Type.parametric "foo" [Type.primitive "bar"; Type.primitive "baz"]);

  assert_create "typing.List.__getitem__(int)" (Type.list Type.integer);
  assert_create
    "typing.Dict.__getitem__((int, str))"
    (Type.dictionary ~key:Type.integer ~value:Type.string);
  (* Type aliases in typeshed. *)
  assert_create "typing.Counter" (Type.parametric "collections.Counter" [Type.Object]);
  assert_create "typing.Counter[int]" (Type.parametric "collections.Counter" [Type.integer]);
  assert_create "typing.ChainMap" (Type.parametric "collections.ChainMap" [Type.Object]);
  assert_create "typing.ChainMap[int]" (Type.parametric "collections.ChainMap" [Type.integer]);
  assert_create "typing.Deque" (Type.parametric "collections.deque" [Type.Object]);
  assert_create "typing.Deque[int]" (Type.parametric "collections.deque" [Type.integer]);

  (* Check renaming. *)
  assert_create "typing.List[int]" (Type.list Type.integer);
  assert_create "typing.List" (Type.list Type.Object);
  assert_create
    "typing.DefaultDict[int, str]"
    (Type.parametric "collections.defaultdict" [Type.integer; Type.string]);
  assert_create
    "typing.Dict[int, str]"
    (Type.dictionary ~key:Type.integer ~value:Type.string);

  assert_create "typing.Tuple[int, str]" (Type.tuple [Type.integer; Type.string]);
  assert_create "typing.Tuple[int, ...]" (Type.Tuple (Type.Unbounded Type.integer));

  assert_create "typing.Any" Type.Object;
  assert_create "typing.Optional[int]" (Type.optional Type.integer);
  assert_create "typing.Optional.__getitem__(int)" (Type.optional Type.integer);
  assert_create "typing.Set[int]" (Type.set Type.integer);

  assert_create "typing.Union[int, str]" (Type.union [Type.integer; Type.string]);
  assert_create "typing.Union[int, typing.Any]" Type.Object;
  assert_create "typing.Union[int, typing.Optional[$bottom]]" (Type.optional Type.integer);
  assert_create "typing.Union[int, typing.Optional[$bottom], str, typing.Tuple[int, str]]"
    (Type.optional
       (Type.union [
           Type.integer;
           Type.string;
           Type.tuple [Type.integer; Type.string];
         ]
       ));

  (* Nested renaming. *)
  assert_create "typing.Set[typing.Any]" (Type.set Type.Object);
  assert_create
    "typing.Dict[str, typing.Any]"
    (Type.dictionary ~key:Type.string ~value:Type.Object);

  (* Check variables. *)
  assert_create "typing.TypeVar('_T')" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$covariant=True)"
    (Type.variable ~variance:Covariant "_T");
  assert_create "typing.TypeVar('_T', $parameter$covariant=False)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$contravariant=True)"
    (Type.variable ~variance:Contravariant "_T");
  assert_create "typing.TypeVar('_T', $parameter$contravariant=False)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', int)"
    (Type.variable ~constraints:(Type.Explicit [Type.integer]) "_T");
  assert_create "typing.TypeVar('_T', name=int)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$bound=int)"
    (Type.variable ~constraints:(Type.Bound Type.integer) "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$bound='C')"
    (Type.variable ~constraints:(Type.Bound (Type.primitive "C")) "_T");
  assert_create
    "typing.TypeVar('_T', 'C', X)"
    (Type.variable ~constraints:(Type.Explicit [Type.primitive "C"; Type.primitive "X"]) "_T");
  assert_create
    "typing.TypeVar('_T', int, name=float)"
    (Type.variable ~constraints:(Type.Explicit [Type.integer]) "_T");

  (* Check that type aliases are resolved. *)
  let assert_alias source resolved =
    let aliases =
      Type.Table.of_alist_exn [
        Type.primitive "Alias", Type.primitive "Aliased";
        Type.primitive "_Future",
        Type.union [
          Type.parametric "Future" [Type.integer; Type.variable "_T"];
          Type.awaitable (Type.variable "_T");
        ];
      ]
      |> Type.Table.find
    in
    assert_create ~aliases source resolved
  in
  assert_alias "Alias" (Type.primitive "Aliased");
  assert_alias "Aliased" (Type.primitive "Aliased");
  assert_alias "typing.Optional[Alias]" (Type.optional (Type.primitive "Aliased"));
  assert_alias "Parametric[Alias]" (Type.parametric "Parametric" [Type.primitive "Aliased"]);
  assert_alias "Alias[int]" (Type.parametric "Aliased" [Type.integer]);
  (* TODO(T30095392): we're blindly stripping away the first type parameter of `Future`. *)
  assert_alias
    "_Future[int]"
    (Type.union [Type.parametric "Future" [Type.integer]; Type.awaitable Type.integer]);

  (* String literals. *)
  assert_create "'foo'" (Type.primitive "foo");
  assert_create "'foo.bar'" (Type.primitive "foo.bar");
  assert_create "foo['bar']" (Type.parametric "foo" [Type.primitive "bar"]);
  assert_create "'Type[str]'" (Type.parametric "Type" [Type.primitive "str"]);
  assert_create "'Type[[[]str]'" (Type.primitive "Type[[[]str]");

  (* Recursive aliasing. *)
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" -> Some (Type.primitive "B")
    | Type.Primitive name when Identifier.show name = "B" -> Some (Type.primitive "C")
    | _ -> None
  in
  assert_create ~aliases "A" (Type.primitive "C");

  (* Recursion with loop. *)
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" ->
        Some (Type.primitive "A")
    | _ ->
        None
  in
  assert_create ~aliases "A" (Type.primitive "A");
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" ->
        Some (Type.list (Type.primitive "A"))
    | _ ->
        None
  in
  assert_create ~aliases "A" (Type.list (Type.list (Type.primitive "A")));

  (* Nested aliasing. *)
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" -> Some (Type.list (Type.primitive "B"))
    | Type.Primitive name when Identifier.show name = "B" -> Some (Type.primitive "C")
    | _ -> None
  in
  assert_create ~aliases "A" (Type.list (Type.primitive "C"));

  (* Aliases with Unions. *)
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" ->
        Some (Type.union [Type.string; Type.bytes])
    | _ ->
        None
  in
  assert_create ~aliases "typing.Union[A, str]" (Type.union [Type.string; Type.bytes]);

  (* Callables. *)
  let open Type.Callable in
  assert_create "typing.Callable" (Type.callable ~annotation:Type.Top ());
  assert_create "typing.Callable[..., int]" (Type.callable ~annotation:Type.integer ());
  assert_create
    "typing.Callable.__getitem__((..., int))"
    (Type.callable ~annotation:Type.integer ());
  assert_create
    "typing.Callable[..., int][[..., str]]"
    (Type.callable
       ~overloads:[
         {
           annotation = Type.string;
           parameters = Undefined;
         };
       ]
       ~annotation:Type.integer
       ());

  assert_create
    "typing.Callable('name')[..., int]"
    (Type.Callable {
        kind = Type.Callable.Named (Access.create "name");
        implementation = { annotation = Type.integer; parameters = Undefined };
        overloads = [];
        implicit = None;
      });

  assert_create
    "typing.Callable('foo')[..., $unknown]"
    (Type.Callable {
        kind = Type.Callable.Named (Access.create "foo");
        implementation = { annotation = Type.Top; parameters = Undefined };
        overloads = [];
        implicit = None;
      });

  assert_create
    "typing.Other('name')[..., int]"
    Type.Top;

  assert_create
    "typing.Callable[[int, str], int]"
    (Type.Callable {
        kind = Type.Callable.Anonymous;
        implementation = {
          annotation = Type.integer;
          parameters = Defined [
              Parameter.Named {
                Parameter.name = Access.create "$0";
                annotation = Type.integer;
                default = false;
              };
              Parameter.Named {
                Parameter.name = Access.create "$1";
                annotation = Type.string;
                default = false;
              };
            ];
        };
        overloads = [];
        implicit = None;
      });
  assert_create
    "typing.Callable[[int, Named(a, int), Variable(variable), Keywords(keywords)], int]"
    (Type.Callable {
        kind = Anonymous;
        implementation = {
          annotation = Type.integer;
          parameters = Defined [
              Parameter.Named {
                Parameter.name = Access.create "$0";
                annotation = Type.integer;
                default = false;
              };
              Parameter.Named {
                Parameter.name = Access.create "a";
                annotation = Type.integer;
                default = false;
              };
              Parameter.Variable {
                Parameter.name = Access.create "variable";
                annotation = Type.Top;
                default = false;
              };
              Parameter.Keywords {
                Parameter.name = Access.create "keywords";
                annotation = Type.Top;
                default = false;
              };
            ];
        };
        overloads = [];
        implicit = None;
      });
  assert_create
    "typing.Callable[[int, Variable(variable, int), Keywords(keywords, str)], int]"
    (Type.Callable {
        kind = Anonymous;
        implementation = {
          annotation = Type.integer;
          parameters = Defined [
              Parameter.Named {
                Parameter.name = Access.create "$0";
                annotation = Type.integer;
                default = false;
              };
              Parameter.Variable {
                Parameter.name = Access.create "variable";
                annotation = Type.integer;
                default = false;
              };
              Parameter.Keywords {
                Parameter.name = Access.create "keywords";
                annotation = Type.string;
                default = false;
              };
            ];
        };
        overloads = [];
        implicit = None;
      });
  assert_create
    "typing.Callable[[Named(a, int, default)], int]"
    (Type.Callable {
        kind = Anonymous;
        implementation = {
          annotation = Type.integer;
          parameters = Defined [
              Parameter.Named {
                Parameter.name = Access.create "a";
                annotation = Type.integer;
                default = true;
              };
            ];
        };
        overloads = [];
        implicit = None;
      });
  assert_create "typing.Callable[int]" (Type.callable ~annotation:Type.Top ());
  assert_create "function" (Type.callable ~annotation:Type.Object ());
  assert_create
    "typing.Callable[..., function]"
    (Type.callable ~annotation:(Type.callable ~annotation:Type.Object ()) ());

  assert_create
    "mypy_extensions.TypedDict[('Movie', ('year', int), ('name', str))]"
    (Type.TypedDictionary {
        name = (Identifier.create "Movie");
        fields = [
          { name = "year"; annotation = Type.integer };
          { name = "name"; annotation = Type.string };
        ];
      })


let test_instantiate _ =
  let assert_instantiate mappings ~generic ~expected =
    let map = Type.Map.of_alist_exn mappings in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      (Type.instantiate ~constraints:(Map.find map) generic)
  in

  assert_instantiate [] ~generic:(Type.primitive "foo") ~expected:(Type.primitive "foo");
  (* Union[_T, _VT] + (_T = int, _VT = None) -> Optional[int] *)
  assert_instantiate
    [
      (Type.variable "_T", Type.integer);
      (Type.variable "_VT", Type.Optional Type.Bottom);
    ]
    ~generic:(Type.Union [Type.variable "_T"; Type.variable "_VT"])
    ~expected:(Type.Optional (Type.integer))


let test_expression _ =
  let assert_expression annotation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (Type.expression annotation)
      (parse_single_expression expression)
  in

  assert_expression (Type.primitive "foo") "foo";
  assert_expression (Type.primitive "...") "...";
  assert_expression (Type.primitive "foo.bar") "foo.bar";
  assert_expression Type.Top "$unknown";
  assert_expression Type.Deleted "$deleted";

  assert_expression
    (Type.Parametric {
        name = ~~"foo.bar";
        parameters = [Type.primitive "baz"];
      })
    "foo.bar.__getitem__(baz)";

  assert_expression
    (Type.Tuple (Type.Bounded [Type.integer; Type.string]))
    "typing.Tuple.__getitem__((int, str))";
  assert_expression
    (Type.Tuple (Type.Unbounded Type.integer))
    "typing.Tuple.__getitem__((int, ...))";
  assert_expression
    (Type.Parametric { name = ~~"list"; parameters = [Type.integer] })
    "typing.List.__getitem__(int)";

  (* Callables. *)
  let open Type.Callable in
  assert_expression
    (Type.callable ~annotation:Type.integer ())
    "typing.Callable.__getitem__((..., int))";
  assert_expression
    (Type.callable ~name:(Access.create "name") ~annotation:Type.integer ())
    "typing.Callable.__getitem__((..., int))";

  assert_expression
    (Type.callable
       ~overloads:[
         {
           Type.Callable.annotation = Type.string;
           parameters = Type.Callable.Undefined;
         };
       ]
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__((..., int)).__getitem__(__getitem__((..., str)))";

  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Defined [
           Parameter.Named {
             Parameter.name = Access.create "$0";
             annotation = Type.integer;
             default = false;
           };
           Parameter.Named {
             Parameter.name = Access.create "$1";
             annotation = Type.string;
             default = false;
           };
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named($0, int), Named($1, str)], int))";
  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Defined [
           Parameter.Named {
             Parameter.name = Access.create "a";
             annotation = Type.integer;
             default = false;
           };
           Parameter.Named {
             Parameter.name = Access.create "b";
             annotation = Type.string;
             default = false;
           };
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(a, int), Named(b, str)], int))";
  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Defined [
           Parameter.Named {
             Parameter.name = Access.create "a";
             annotation = Type.integer;
             default = true;
           };
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(a, int, default)], int))";
  assert_expression
    (Type.callable
       ~parameters:(Defined [
           Parameter.Named {
             Parameter.name = Access.create "$0";
             annotation = Type.integer;
             default = false;
           };
           Parameter.Variable {
             Parameter.name = Access.create "variable";
             annotation = Type.integer;
             default = false;
           };
           Parameter.Keywords {
             Parameter.name = Access.create "keywords";
             annotation = Type.string;
             default = false;
           };
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named($0, int), Variable(variable, int), Keywords(keywords, str)], int))";

  assert_expression
    (Type.TypedDictionary {
        name = Identifier.create "Movie";
        fields = [
          { name = "title"; annotation = Type.string };
          { name = "year"; annotation = Type.integer };
        ];
      }
    )
    "mypy_extensions.TypedDict[(\"Movie\", (\"title\", str), (\"year\", int))]"


let test_union _ =
  assert_equal
    (Type.union [Type.string; Type.float])
    (Type.Union [Type.float; Type.string]);
  assert_equal
    (Type.union [Type.float; Type.string])
    (Type.Union [Type.float; Type.string]);
  assert_equal
    (Type.union [Type.optional Type.string; Type.float])
    (Type.Union [Type.optional Type.string; Type.float]);
  assert_equal
    (Type.union [Type.float; Type.string; Type.optional Type.float])
    (Type.Union [Type.optional Type.float; Type.string]);

  assert_true (Type.equal (Type.union [Type.float; Type.Object]) Type.Object);
  assert_true (Type.equal (Type.union [Type.float; Type.Top]) Type.Top);

  assert_true
    (Type.equal (Type.union [Type.string; Type.float]) (Type.Union [Type.float; Type.string]));
  assert_true
    (Type.equal (Type.union [Type.float; Type.string]) (Type.Union [Type.float; Type.string]));
  assert_true (Type.equal (Type.union [Type.float]) Type.float);

  (* Flatten unions. *)
  assert_equal
    (Type.union [Type.float; Type.union[Type.string; Type.bytes]])
    (Type.union [Type.float; Type.string; Type.bytes])


let test_primitives _ =
  assert_equal
    []
    (Type.primitives (Type.callable ~annotation:Type.Top ()));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.callable ~annotation:Type.integer ()));

  assert_equal
    []
    (Type.primitives (Type.optional Type.Top));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.optional Type.integer));

  assert_equal
    []
    (Type.primitives (Type.Tuple (Type.Unbounded Type.Top)));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.Tuple (Type.Unbounded Type.integer)));

  assert_equal
    []
    (Type.primitives (Type.variable ~constraints:(Type.Explicit [Type.Top]) "T"));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.variable ~constraints:(Type.Explicit [Type.integer]) "T"));

  assert_equal
    [Type.integer]
    (Type.primitives (Type.parametric "parametric" [Type.integer; Type.Top]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.parametric "parametric" [Type.integer; Type.string]));

  assert_equal
    [Type.string]
    (Type.primitives (Type.tuple [Type.Top; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.tuple [Type.integer; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.union [Type.integer; Type.string]));

  assert_equal
    []
    (Type.primitives Type.Top);
  assert_equal
    []
    (Type.primitives Type.Bottom);
  assert_equal
    [Type.integer]
    (Type.primitives Type.integer);
  assert_equal
    []
    (Type.primitives Type.Object);

  assert_equal
    [Type.integer; Type.string]
    (Type.TypedDictionary {
        name = (Identifier.create "Movie");
        fields = [
          { name = "year"; annotation = Type.integer };
          { name = "name"; annotation = Type.string };
        ];
      } |> Type.primitives)


let test_exists _ =
  let top_exists = Type.exists ~predicate:(function | Type.Top -> true | _ -> false) in

  assert_true (top_exists (Type.callable ~annotation:Type.Top ()));
  assert_false (top_exists (Type.callable ~annotation:Type.integer ()));
  assert_true (top_exists (Type.optional Type.Top));
  assert_false (top_exists (Type.optional Type.integer));
  assert_true (top_exists (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (top_exists (Type.Tuple (Type.Unbounded Type.integer)));

  assert_true (top_exists (Type.variable ~constraints:(Type.Explicit [Type.Top]) "T"));
  assert_false (top_exists (Type.variable ~constraints:(Type.Explicit [Type.integer]) "T"));
  assert_true (top_exists (Type.parametric "parametric" [Type.integer; Type.Top]));
  assert_false (top_exists (Type.parametric "parametric" [Type.integer; Type.string]));
  assert_true (top_exists (Type.tuple [Type.Top; Type.string]));
  assert_false (top_exists (Type.tuple [Type.integer; Type.string]));
  assert_true (top_exists (Type.union [Type.integer; Type.Top]));
  assert_false (top_exists (Type.union [Type.integer; Type.string]));

  assert_true (top_exists Type.Top);
  assert_false (top_exists Type.Bottom);
  assert_false (top_exists Type.integer);
  assert_false (top_exists Type.Object)


let test_is_iterator _ =
  assert_true (Type.is_iterator (Type.iterator Type.string));
  assert_false (Type.is_iterator Type.string);
  assert_false (Type.is_iterator (Type.primitive "typing.Iterator"))


let test_is_generator _ =
  assert_true (Type.is_generator (Type.generator Type.string));
  assert_false (Type.is_generator Type.string);
  assert_true (Type.is_generator (Type.generator ~async:true Type.string))


let test_contains_callable _ =
  assert_true (Type.contains_callable (Type.callable ~annotation:Type.integer ()));
  assert_true (Type.contains_callable (Type.Optional (Type.callable ~annotation:Type.integer ())));
  assert_true
    (Type.contains_callable (Type.union[Type.string; (Type.callable ~annotation:Type.integer ())]));
  assert_false (Type.contains_callable (Type.Primitive (Identifier.create "foo")))


let test_is_not_instantiated _ =
  assert_true (Type.is_not_instantiated Type.Bottom);
  assert_true (Type.is_not_instantiated (Type.dictionary ~key:Type.Bottom ~value:Type.Bottom));
  assert_true (Type.is_not_instantiated (Type.Optional Type.Bottom));
  assert_false (Type.is_not_instantiated Type.Top);
  assert_true (Type.is_not_instantiated (Type.variable "_T"))


let test_is_meta _ =
  assert_true
    (Type.is_meta
       (Type.Parametric { name = ~~"type"; parameters = [Type.integer] }));
  assert_false (Type.is_meta Type.integer);
  assert_false
    (Type.is_meta
       (Type.Parametric { name = ~~"typing.Type"; parameters = [Type.integer] }))



let test_is_none _ =
  assert_false (Type.is_none (Type.primitive "None"));
  assert_false (Type.is_none Type.integer);
  assert_false (Type.is_none (Type.primitive "foo"));
  assert_true (Type.is_none (Type.Optional Type.Bottom))



let test_is_type_alias _ =
  assert_true (Type.is_type_alias (Type.primitive "typing.TypeAlias"));
  assert_false (Type.is_type_alias (Type.parametric "typing.TypeAlias" [Type.Top]))

let test_is_unknown _ =
  assert_false (Type.is_unknown Type.Bottom);
  assert_false (Type.is_unknown Type.Object);

  assert_true (Type.is_unknown (Type.optional Type.Top));
  assert_false (Type.is_unknown (Type.optional Type.integer));

  assert_true
    (Type.is_unknown (
        Type.Optional
          (Type.Parametric {
              name = ~~"foo";
              parameters = [Type.integer; Type.Top];
            })));

  assert_true
    (Type.is_unknown (
        (Type.Parametric {
            name = ~~"foo";
            parameters = [Type.integer; Type.Top];
          })));
  assert_false
    (Type.is_unknown (
        (Type.Parametric {
            name = ~~"foo";
            parameters = [Type.integer];
          })));

  assert_false (Type.is_unknown Type.integer);

  assert_true (Type.is_unknown Type.Deleted);

  assert_true (Type.is_unknown Type.Top);

  assert_true (Type.is_unknown (Type.Union [Type.integer; Type.Top]));
  assert_false (Type.is_unknown (Type.Union [Type.integer; Type.string]));

  assert_false (Type.is_unknown (Type.variable "derp"));

  assert_true (Type.is_unknown (Type.Tuple (Type.Bounded [Type.integer; Type.Top])));
  assert_false (Type.is_unknown (Type.Tuple (Type.Bounded [Type.integer; Type.string])));
  assert_true (Type.is_unknown (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (Type.is_unknown (Type.Tuple (Type.Unbounded Type.integer)))


let test_is_resolved _ =
  assert_false (Type.is_resolved (Type.variable "_T"));
  assert_false (Type.is_resolved (Type.union [Type.integer; Type.variable "_T"]));

  assert_true (Type.is_resolved Type.integer);
  assert_true (Type.is_resolved (Type.union [Type.integer; Type.string]))


let test_mismatch_with_any _ =
  assert_false (Type.mismatch_with_any Type.Bottom Type.Top);
  assert_false (Type.mismatch_with_any Type.integer Type.string);

  assert_true (Type.mismatch_with_any Type.Object Type.string);
  assert_true (Type.mismatch_with_any Type.integer Type.Object);

  assert_false (Type.mismatch_with_any (Type.Optional Type.integer) (Type.Optional Type.string));
  assert_true (Type.mismatch_with_any (Type.Optional Type.Object) (Type.Optional Type.string));

  assert_false (Type.mismatch_with_any (Type.list Type.integer) (Type.list Type.string));
  assert_true (Type.mismatch_with_any (Type.list Type.Object) (Type.list Type.string));
  assert_false
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.string ~value:Type.integer)
       (Type.dictionary ~key:Type.string ~value:Type.string));
  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.string ~value:Type.Object)
       (Type.dictionary ~key:Type.string ~value:Type.string));
  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.Object ~value:Type.Object)
       (Type.dictionary ~key:Type.string ~value:(Type.list Type.integer)));
  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.Object ~value:Type.Object)
       (Type.dictionary
          ~key:Type.string
          ~value:(Type.dictionary ~key:Type.string ~value:Type.integer)));
  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.Object ~value:Type.Object)
       (Type.Optional
          (Type.dictionary
             ~key:Type.string
             ~value:Type.string)));
  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.Object ~value:Type.bool)
       (Type.parametric "typing.Mapping" [Type.integer; Type.bool]));

  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.Object ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" [Type.integer; Type.bool]));

  assert_true
    (Type.mismatch_with_any
       (Type.dictionary ~key:Type.integer ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" [Type.Object; Type.bool]));

  assert_true
    (Type.mismatch_with_any
       (Type.iterable Type.string)
       (Type.parametric "typing.List" [Type.Object]));

  assert_true
    (Type.mismatch_with_any
       (Type.sequence Type.Object)
       (Type.list Type.integer));

  assert_true
    (Type.mismatch_with_any
       (Type.iterable Type.string)
       (Type.parametric "typing.Optional" [Type.Object]));

  assert_false
    (Type.mismatch_with_any
       (Type.iterable Type.string)
       (Type.parametric "typing.Optional" [Type.string]));

  assert_true
    (Type.mismatch_with_any
       (Type.iterable Type.string)
       (Type.list Type.Object));
  assert_true
    (Type.mismatch_with_any
       (Type.iterable Type.Object)
       (Type.list Type.string));
  assert_true
    (Type.mismatch_with_any
       (Type.iterable Type.integer)
       (Type.set Type.Object));

  assert_false
    (Type.mismatch_with_any
       (Type.tuple [Type.string; Type.string])
       (Type.tuple [Type.string; Type.integer]));
  assert_true
    (Type.mismatch_with_any
       (Type.tuple [Type.string; Type.string])
       (Type.tuple [Type.string; Type.Object]));
  assert_false
    (Type.mismatch_with_any
       (Type.Tuple (Type.Unbounded Type.integer))
       (Type.Tuple (Type.Unbounded Type.string)));
  assert_true
    (Type.mismatch_with_any
       (Type.Tuple (Type.Unbounded Type.integer))
       (Type.Tuple (Type.Unbounded Type.Object)));
  assert_true
    (Type.mismatch_with_any
       (Type.Tuple (Type.Bounded [Type.integer; Type.Object]))
       (Type.Tuple (Type.Unbounded Type.integer)));
  assert_true
    (Type.mismatch_with_any
       (Type.Tuple (Type.Bounded [Type.integer; Type.string]))
       (Type.Tuple (Type.Unbounded Type.Object)));
  assert_false
    (Type.mismatch_with_any
       (Type.Tuple (Type.Bounded [Type.integer; Type.string]))
       (Type.Tuple (Type.Unbounded Type.string)));

  assert_false
    (Type.mismatch_with_any
       (Type.union [Type.integer; Type.string])
       (Type.union [Type.integer; Type.float]));
  assert_true
    (Type.mismatch_with_any
       (Type.union [Type.integer; Type.string])
       (Type.union [Type.integer; Type.Object]));

  assert_true
    (Type.mismatch_with_any
       (Type.union [Type.integer; Type.Object])
       Type.integer);


  assert_true (Type.mismatch_with_any (Type.iterator Type.integer) (Type.generator Type.Object));
  assert_true
    (Type.mismatch_with_any
       (Type.iterator (Type.list Type.integer))
       (Type.generator (Type.list Type.Object)));
  assert_false (Type.mismatch_with_any (Type.iterator Type.integer) (Type.generator Type.float));

  assert_true
    (Type.mismatch_with_any
       (Type.Union [Type.list Type.integer; Type.string])
       (Type.list Type.Object));

  assert_false
    (Type.mismatch_with_any
       (Type.Union [Type.list Type.integer; Type.string])
       (Type.parametric "unknown" [Type.Object]));

  assert_true
    (Type.mismatch_with_any
       (Type.callable ~annotation:Type.integer ())
       Type.Object);
  assert_true
    (Type.mismatch_with_any
       Type.Object
       (Type.callable ~annotation:Type.integer ()));
  assert_true
    (Type.mismatch_with_any
       Type.Object
       (Type.union [Type.integer; Type.callable ~annotation:Type.integer ()]));

  assert_true
    (Type.mismatch_with_any
       (parse_callable "typing.Callable[[typing.Any], int]")
       (parse_callable "typing.Callable[[str], int]"));
  assert_true
    (Type.mismatch_with_any
       (parse_callable "typing.Callable[[int], typing.Any]")
       (parse_callable "typing.Callable[[int], int]"));
  assert_true
    (Type.mismatch_with_any
       (parse_callable "typing.Callable[[int], typing.Any]")
       (parse_callable "typing.Callable[[str], int]"));
  assert_false
    (Type.mismatch_with_any
       (parse_callable "typing.Callable[[typing.Any, typing.Any], typing.Any]")
       (parse_callable "typing.Callable[[typing.Any], typing.Any]"))


let test_class_name _ =
  let assert_class_name annotation expected =
    assert_equal
      ~cmp:Access.equal
      ~printer:Access.show
      (parse_single_access expected)
      (Type.class_name annotation)
  in

  assert_class_name (Type.primitive "qualified.primitive") "qualified.primitive";
  assert_class_name (Type.list Type.integer) "list";
  assert_class_name
    (Type.union [Type.string; Type.integer])
    "typing.Union.__getitem__((int, str))"; (* Ugh... *)
  assert_class_name (Type.variable "_T") "_T"


let test_optional_value _ =
  assert_equal
    (Type.optional_value (
        Type.Optional
          (Type.Parametric {
              name = ~~"foo";
              parameters = [Type.integer; Type.Top];
            })))
    (Type.Parametric {
        name = ~~"foo";
        parameters = [Type.integer; Type.Top];
      });
  assert_equal
    (Type.optional_value
       (Type.Parametric {
           name = ~~"foo";
           parameters = [Type.integer; Type.Top];
         }))
    (Type.Parametric {
        name = ~~"foo";
        parameters = [Type.integer; Type.Top];
      })


let test_async_generator_value _ =
  assert_equal
    ~printer:(Format.asprintf "%a" Type.pp)
    (Type.async_generator_value (
        Type.Parametric {
          name = Identifier.create "typing.AsyncGenerator";
          parameters = [Type.integer; Type.Optional Type.Bottom];
        }))
    (Type.Parametric {
        name = Identifier.create "typing.Generator";
        parameters = [Type.integer; Type.Optional Type.Bottom; Type.Optional Type.Bottom];
      })


let test_dequalify _ =
  let map =
    {|
      from typing import (
           Optional,
           List,
      )
      import typing
      from A.B import C
      import d as e
    |}
  in
  let assert_dequalify source expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:(Format.asprintf "%a" Type.pp)
      ~pp_diff:(diff ~print:Type.pp)
      (Type.dequalify
         (Preprocessing.dequalify_map (parse map))
         source)
      expected
  in
  let create name = Type.primitive name in
  assert_dequalify
    (Type.optional (Type.string))
    (Type.parametric "Optional" [Type.string]);

  assert_dequalify
    (Type.parametric "list" [Type.string])
    (Type.parametric "List" [Type.string]);

  assert_dequalify
    (Type.Union [Type.string; create "A.B.C"])
    (Type.parametric "typing.Union" [create "C"; Type.string]);

  assert_dequalify
    (create "d")
    (create "e");

  assert_dequalify
    (Type.parametric "A.B.C" [Type.optional (Type.integer)])
    (Type.parametric "C" [Type.parametric "Optional" [Type.integer]])


let test_from_overloads _ =
  let assert_create ?(aliases = (fun _ -> None)) sources expected =
    let merged =
      let parse_callable source =
        match Type.create ~aliases (parse_single_expression source) with
        | Type.Callable callable -> callable
        | _ -> failwith ("Could not extract callable from " ^ source)
      in
      sources
      |> List.map ~f:parse_callable
      |> Type.Callable.from_overloads
      >>| (fun callable -> Type.Callable callable)
      |> Option.value ~default:Type.Top
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (Type.create ~aliases (parse_single_expression expected))
      merged
  in

  assert_create ["typing.Callable('foo')[..., int]"; "typing.Callable('bar')[..., int]"] "$unknown";
  assert_create
    ["typing.Callable('foo')[..., int]"; "typing.Callable('foo')[..., str]"]
    "typing.Callable('foo')[..., str]";
  assert_create
    [
      "typing.Callable('foo')[..., int]";
      "typing.Callable('foo')[[int, str], str]";
      "typing.Callable('foo')[[int, str, str], int]";
    ]
    "typing.Callable('foo')[[int, str, str], int]";
  assert_create
    [
      "typing.Callable('foo')[..., $unknown][[[int], int]]";
      "typing.Callable('foo')[[str], str]";
      "typing.Callable('foo')[[int], int][[[str], str]]";
    ]
    "typing.Callable('foo')[[int], int][[[int], int][[str], str]]"

let test_with_return_annotation _ =
  let assert_with_return_annotation annotation callable expected =
    let callable =
      match Type.create ~aliases:(fun _ -> None) (parse_single_expression callable) with
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (Type.create ~aliases:(fun _ -> None) (parse_single_expression expected))
      (Type.Callable (Type.Callable.with_return_annotation ~annotation callable))
  in

  assert_with_return_annotation
    Type.string
    "typing.Callable('foo')[..., int]"
    "typing.Callable('foo')[..., str]";

  assert_with_return_annotation
    Type.string
    "typing.Callable('foo')[..., int][[[int], int]]"
    "typing.Callable('foo')[..., str][[[int], str]]"


let test_overload_parameters _ =
  let assert_parameters callable expected =
    let { Type.Callable.overloads; _ } =
      Type.create ~aliases:(fun _ -> None) (parse_single_expression callable)
      |> function
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    let parameters =
      List.hd_exn overloads
      |> Type.Callable.Overload.parameters
      |> Option.value ~default:[]
      |> List.map ~f:Type.Callable.Parameter.annotation
      |> List.map ~f:Type.show
    in
    assert_equal parameters expected
  in
  assert_parameters "typing.Callable('foo')[..., $unknown][[[int], str]]" ["int"];
  assert_parameters "typing.Callable('foo')[..., $unknown][[[int, str], str]]" ["int"; "str"];
  assert_parameters "typing.Callable('foo')[..., $unknown][[[], str]]" []


let test_variables _ =
  let assert_variables source expected =
    let aliases =
      let aliases =
        Type.Map.of_alist_exn [
          Type.primitive "T", Type.variable "T";
          Type.primitive "S", Type.variable "S";
        ]
      in
      Map.find aliases
    in
    let variables =
      Type.create ~aliases (parse_single_expression source)
      |> Type.variables
    in
    assert_equal (List.map expected ~f:Type.variable) variables
  in
  assert_variables "T" ["T"];
  assert_variables "Parametric[int, T]" ["T"];
  assert_variables "Parametric[T, S]" ["T"; "S"];
  assert_variables "typing.Callable[..., int]" [];
  assert_variables "typing.Callable[..., T]" ["T"];
  assert_variables "typing.Callable[[T, int], str]" ["T"]


let test_parameter_name_compatibility _ =
  let parameter name =
    Type.Callable.Parameter.Named {
      Type.Callable.Parameter.name = Access.create name;
      annotation = Type.integer;
      default = false;
    }
  in
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "argument")
       (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "argument")
       (parameter "$0"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "$0")
       (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "$0")
       (parameter "$1"));

  (* Underscores are ignored for the purposes of typechecking parameter compatibility. *)
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "argument")
       (parameter "_argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "_argument")
       (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "__argument")
       (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible
       (parameter "argument")
       (parameter "__argument"));
  assert_false
    (Type.Callable.Parameter.names_compatible
       (parameter "argument")
       (parameter "other"));
  assert_false
    (Type.Callable.Parameter.names_compatible
       (parameter "_argument")
       (parameter "other"))


let test_lambda _ =
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Named(x, str)], int]")
       (Type.lambda
          ~parameters:[Access.create "x", Type.string]
          ~return_annotation:Type.integer));
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Keywords(kwargs, str)], int]")
       (Type.lambda
          ~parameters:[Access.create "**kwargs", Type.string]
          ~return_annotation:Type.integer));
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Variable(args, str)], int]")
       (Type.lambda
          ~parameters:[Access.create "*args", Type.string]
          ~return_annotation:Type.integer))


let test_visit _ =
  let create source =
    Type.create ~aliases:(fun _ -> None) (parse_single_expression source)
  in
  let assert_types_equal annotation expected  =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      annotation
  in

  let module CountTransform = Type.Transform.Make(struct
      type state = int
      let visit state _ = (state + 1), Type.integer
      let visit_children _  _ = true
    end)
  in
  let end_state, transformed = CountTransform.visit 0 (create "typing.List[int]") in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 2 end_state;

  let end_state, transformed = CountTransform.visit 0 (create "Foo[Bar[Baz, Bop], Bang]") in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 5 end_state;

  let module SubstitutionTransform = Type.Transform.Make(struct
      type state = int
      let visit state annotation =
        match annotation with
        | Type.Primitive integer when Identifier.show integer = "int" && state > 0 ->
            (state - 1), Type.string
        | _ ->
            state, annotation
      let visit_children _ = function | Type.Optional _ -> false | _ -> true
    end)
  in

  let end_state, transformed =
    SubstitutionTransform.visit 1 (create "typing.Callable[[int], int]")
  in
  assert_types_equal transformed (create "typing.Callable[[str], int]");
  assert_equal ~printer:string_of_int  0 end_state;

  let end_state, transformed =
    SubstitutionTransform.visit 1 (create "typing.Callable[[typing.Optional[int], int], int]")
  in
  assert_types_equal transformed (create "typing.Callable[[typing.Optional[int], str], int]");
  assert_equal ~printer:string_of_int  0 end_state;

  let end_state, transformed =
    SubstitutionTransform.visit
      1
      (create "mypy_extensions.TypedDict[('int', ('int', int), ('str', int))]")
  in
  assert_types_equal
    transformed
    (create "mypy_extensions.TypedDict[('int', ('int', str), ('str', int))]");
  assert_equal ~printer:string_of_int 0 end_state;

  let module ConcatenateTransform = Type.Transform.Make(struct
      type state = string
      let visit state annotation =
        match annotation with
        | Type.Primitive primitive ->
            state ^ (Identifier.show primitive), annotation
        | Type.Parametric { name; parameters; } ->
            "",
            Type.Parametric
              { name = Identifier.create ((Identifier.show name) ^ state); parameters }
        | _ ->
            state, annotation
      let visit_children _  _ = true
    end)
  in
  let end_state, transformed =
    ConcatenateTransform.visit "" (create "Foo[Bar[Baz, Bop], Bro[Loop, typing.Optional[Land]]]")
  in
  assert_types_equal
    transformed
    (create "Foo[BarBazBop[Baz, Bop], BroLoopLand[Loop, typing.Optional[Land]]]");
  assert_equal "" end_state;

  ()




let () =
  "type">:::[
    "create">::test_create;
    "instantiate">::test_instantiate;
    "expression">::test_expression;
    "union">::test_union;
    "primitives">::test_primitives;
    "exists">::test_exists;
    "is_async_generator">::test_is_generator;
    "contains_callable">::test_contains_callable;
    "is_not_instantiated">::test_is_not_instantiated;
    "is_meta">::test_is_meta;
    "is_none">::test_is_none;
    "is_type_alias">::test_is_type_alias;
    "is_unknown">::test_is_unknown;
    "is_resolved">::test_is_resolved;
    "is_iterator">::test_is_iterator;
    "mismatch_with_any">::test_mismatch_with_any;
    "class_name">::test_class_name;
    "optional_value">::test_optional_value;
    "async_generator_value">::test_async_generator_value;
    "dequalify">::test_dequalify;
    "variables">::test_variables;
    "lambda">::test_lambda;
    "visit">:: test_visit;
  ]
  |> Test.run;
  "callable">:::[
    "from_overloads">::test_from_overloads;
    "with_return_annotation">::test_with_return_annotation;
    "overload_parameters">::test_overload_parameters;
  ]
  |> Test.run
