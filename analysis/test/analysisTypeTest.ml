(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Analysis
open Expression

open Test


let identifier name =
  Access.Identifier ~~name


let variable name =
  Type.Variable { Type.variable = name; constraints = [] }


let test_create _ =
  let assert_create ?(aliases = (fun _ -> None)) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create ~aliases (parse_single_expression source))
  in

  assert_create "foo" (Type.Primitive ~~"foo");
  assert_create "foo.bar" (Type.Primitive ~~"foo.bar");

  assert_create "object" Type.Object;
  assert_create "$unknown" Type.Top;

  assert_create "foo[bar]" (Type.parametric "foo" [Type.primitive "bar"]);
  assert_create
    "foo[bar, baz]"
    (Type.parametric "foo" [Type.primitive "bar"; Type.primitive "baz"]);

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
  assert_create "typing.Set[int]" (Type.set Type.integer);

  assert_create "typing.Union[int, str]" (Type.union [Type.integer; Type.string]);
  assert_create "typing.Union[int, typing.Any]" Type.Object;
  assert_create "typing.Union[int, typing.Optional[$bottom]]" (Type.optional Type.integer);

  (* Nested renaming. *)
  assert_create "typing.Set[typing.Any]" (Type.set Type.Object);
  assert_create
    "typing.Dict[str, typing.Any]"
    (Type.dictionary ~key:Type.string ~value:Type.Object);

  (* Renaming numbers (PEP 3141). *)
  assert_create "numbers.Number" Type.complex;
  assert_create "numbers.Complex" Type.complex;
  assert_create "numbers.Real" Type.float;
  assert_create "numbers.Integral" Type.integer;

  (* Check variables. *)
  assert_create "typing.TypeVar('_T')" (Type.variable "_T");
  assert_create "typing.TypeVar('_T', covariant=True)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', numbers.Integral)"
    (Type.variable ~constraints:[Type.integer] "_T");
  assert_create
    "typing.TypeVar('_T', name=numbers.Integral)"
    (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', numbers.Integral, name=numbers.Real)"
    (Type.variable ~constraints:[Type.integer] "_T");

  (* Check that type aliases are resolved. *)
  let aliases =
    Type.Table.of_alist_exn [
      Type.Primitive ~~"_T",
      variable ~~"_T";
    ]
    |> Type.Table.find
  in
  assert_create ~aliases "_T" (Type.variable "_T");
  assert_create ~aliases "foo[_T]" (Type.parametric "foo" [Type.variable "_T"]);

  (* String literals. *)
  assert_create "'foo'" (Type.primitive "foo");
  assert_create "'foo.bar'" (Type.primitive "foo.bar");
  assert_create "foo['bar']" (Type.parametric "foo" [Type.primitive "bar"]);
  assert_create "'Type[str]'" (Type.parametric "Type" [Type.primitive "str"]);
  assert_create "'Type[[[]str]'" (Type.primitive "Type[[[]str]");

  (* Recursive aliasing. *)
  let aliases = function
    | Type.Primitive name when Identifier.show name = "A" ->
        Some (Type.Primitive ~~"B")
    | Type.Primitive name when Identifier.show name = "B" ->
        Some (Type.Primitive ~~"C")
    | _ ->
        None
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
    | Type.Primitive name when Identifier.show name = "A" ->
        Some (Type.list (Type.Primitive ~~"B"))
    | Type.Primitive name when Identifier.show name = "B" ->
        Some (Type.Primitive ~~"C")
    | _ ->
        None
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
    "typing.Callable[..., int][..., str]"
    (Type.callable
       ~overrides:[
         {
           annotation = Type.string;
           parameters = Parameter.Undefined;
         };
       ]
       ~annotation:Type.integer
       ());

  assert_create
    "typing.Callable('name')[..., int]"
    (Type.Callable {
        kind = Type.Callable.Named (Access.create "name");
        overrides = [ { annotation = Type.integer; parameters = Parameter.Undefined }];
      });
  assert_create
    "typing.Other('name')[..., int]"
    Type.Top;

  assert_create
    "typing.Callable[[int, str], int]"
    (Type.Callable {
        kind = Type.Callable.Anonymous;
        overrides = [
          {
            annotation = Type.integer;
            parameters = Parameter.Defined [
                Parameter.Anonymous Type.integer;
                Parameter.Anonymous Type.string;
              ];
          };
        ];
      });
  assert_create
    "typing.Callable[[int, Named(a, int), Variable(variable), Keywords(keywords)], int]"
    (Type.Callable {
        kind = Anonymous;
        overrides = [
          {
            annotation = Type.integer;
            parameters = Parameter.Defined [
                Parameter.Anonymous Type.integer;
                Parameter.Named {
                  Parameter.name = Access.create "a";
                  annotation = Type.integer;
                };
                Parameter.Variable (Access.create "variable");
                Parameter.Keywords (Access.create "keywords");
              ];
          };
        ];
      });
  assert_create "typing.Callable[int]" Type.Top


let test_expression _ =
  let assert_expression annotation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (Type.expression annotation)
      (parse_single_expression expression)
  in

  assert_expression (Type.Primitive ~~"foo") "foo";
  assert_expression (Type.Primitive ~~"foo.bar") "foo.bar";
  assert_expression Type.Top "$unknown";

  assert_expression
    (Type.Parametric {
        Type.name = ~~"foo.bar";
        parameters = [Type.Primitive ~~"baz"];
      })
    "foo.bar[baz]";

  assert_expression
    (Type.Tuple (Type.Bounded [Type.integer; Type.string]))
    "typing.Tuple[int, str]";
  assert_expression (Type.Tuple (Type.Unbounded Type.integer)) "typing.Tuple[int, ...]";
  assert_expression
    (Type.Parametric { Type.name = ~~"list"; parameters = [Type.integer] })
    "typing.List[int]";

  (* Callables. *)
  assert_expression (Type.callable ~annotation:Type.integer ()) "typing.Callable[..., int]";
  assert_expression
    (Type.callable ~name:(Access.create "name") ~annotation:Type.integer ())
    "typing.Callable[..., int]";

  assert_expression
    (Type.callable
       ~overrides:[
         {
           Type.Callable.annotation = Type.string;
           parameters = Type.Callable.Parameter.Undefined;
         };
       ]
       ~annotation:Type.integer
       ())
    "typing.Callable[..., int][..., str]";

  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Parameter.Defined [
           Type.Callable.Parameter.Anonymous Type.integer;
           Type.Callable.Parameter.Anonymous Type.string;
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[int, str], int]";
  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Parameter.Defined [
           Type.Callable.Parameter.Named {
             Type.Callable.Parameter.name = Access.create "a";
             annotation = Type.integer;
           };
           Type.Callable.Parameter.Named {
             Type.Callable.Parameter.name = Access.create "b";
             annotation = Type.string;
           };
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[Named(a, int), Named(b, str)], int]";
  assert_expression
    (Type.callable
       ~parameters:(Type.Callable.Parameter.Defined [
           Type.Callable.Parameter.Anonymous Type.integer;
           Type.Callable.Parameter.Variable (Access.create "variable");
           Type.Callable.Parameter.Keywords (Access.create "keywords");
         ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[int, Variable(variable), Keywords(keywords)], int]"


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

  assert_true
    (Type.equal (Type.union [Type.string; Type.float]) (Type.Union [Type.float; Type.string]));
  assert_true
    (Type.equal (Type.union [Type.float; Type.string]) (Type.Union [Type.float; Type.string]));
  assert_true (Type.equal (Type.union [Type.float]) Type.float);

  (* Flatten unions. *)
  assert_equal
    (Type.union [Type.float; Type.union[Type.string; Type.bytes]])
    (Type.union [Type.float; Type.string; Type.bytes])


let test_exists _ =
  let top_exists = Type.exists ~predicate:(function | Type.Top -> true | _ -> false) in

  assert_true (top_exists (Type.callable ~annotation:Type.Top ()));
  assert_false (top_exists (Type.callable ~annotation:Type.integer ()));
  assert_true (top_exists (Type.optional Type.Top));
  assert_false (top_exists (Type.optional Type.integer));
  assert_true (top_exists (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (top_exists (Type.Tuple (Type.Unbounded Type.integer)));

  assert_true (top_exists (Type.Variable { Type.variable = ~~"T"; constraints = [Type.Top] }));
  assert_false (top_exists (Type.Variable { Type.variable = ~~"T"; constraints = [Type.integer] }));
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


let test_is_generator _ =
  assert_true (Type.is_generator (Type.generator Type.string));
  assert_false (Type.is_generator Type.string);
  assert_true (Type.is_generator (Type.generator ~async:true Type.string))


let test_is_callable _ =
  assert_true (Type.is_callable (Type.callable ~annotation:Type.integer ()));
  assert_true (Type.is_callable (Type.Optional (Type.callable ~annotation:Type.integer ())));
  assert_true
    (Type.is_callable (Type.union[Type.string; (Type.callable ~annotation:Type.integer ())]));
  assert_false (Type.is_callable (Type.Primitive (Identifier.create "foo")))


let test_is_not_instantiated _ =
  assert_true (Type.is_not_instantiated Type.Bottom);
  assert_true (Type.is_not_instantiated (Type.dictionary ~key:Type.Bottom ~value:Type.Bottom));
  assert_true (Type.is_not_instantiated (Type.Optional Type.Bottom));
  assert_false (Type.is_not_instantiated Type.Top);
  assert_true
    (Type.is_not_instantiated
       (Type.Variable { Type.variable = Identifier.create "_T"; constraints = [] }))


let test_is_meta _ =
  assert_true
    (Type.is_meta
       (Type.Parametric { Type.name = ~~"typing.Type"; parameters = [Type.integer] }));
  assert_false (Type.is_meta Type.integer)


let test_is_none _ =
  assert_false (Type.is_none (Type.Primitive ~~"None"));
  assert_false (Type.is_none Type.integer);
  assert_false (Type.is_none (Type.Primitive ~~"foo"));
  assert_true (Type.is_none (Type.Optional Type.Bottom))


let test_is_unknown _ =
  assert_false (Type.is_unknown Type.Bottom);
  assert_false (Type.is_unknown Type.Object);

  assert_true (Type.is_unknown (Type.optional Type.Top));
  assert_false (Type.is_unknown (Type.optional Type.integer));

  assert_true
    (Type.is_unknown (
        Type.Optional
          (Type.Parametric {
              Type.name = ~~"foo";
              parameters = [Type.integer; Type.Top];
            })));

  assert_true
    (Type.is_unknown (
        (Type.Parametric {
            Type.name = ~~"foo";
            parameters = [Type.integer; Type.Top];
          })));
  assert_false
    (Type.is_unknown (
        (Type.Parametric {
            Type.name = ~~"foo";
            parameters = [Type.integer];
          })));

  assert_false (Type.is_unknown Type.integer);

  assert_true (Type.is_unknown Type.Top);

  assert_true (Type.is_unknown (Type.Union [Type.integer; Type.Top]));
  assert_false (Type.is_unknown (Type.Union [Type.integer; Type.string]));

  assert_false (Type.is_unknown (variable ~~"derp"));

  assert_true (Type.is_unknown (Type.Tuple (Type.Bounded [Type.integer; Type.Top])));
  assert_false (Type.is_unknown (Type.Tuple (Type.Bounded [Type.integer; Type.string])));
  assert_true (Type.is_unknown (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (Type.is_unknown (Type.Tuple (Type.Unbounded Type.integer)))


let test_is_resolved _ =
  assert_false (Type.is_resolved (variable ~~"_T"));
  assert_false (Type.is_resolved (Type.union [Type.integer; variable ~~"_T"]));

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
       (Type.Parametric {
           Type.name = ~~"typing.Mapping";
           parameters = [Type.integer; Type.bool];
         }));

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
  assert_false (Type.mismatch_with_any (Type.iterator Type.integer) (Type.generator Type.float))


let test_optional_value _ =
  assert_equal
    (Type.optional_value (
        Type.Optional
          (Type.Parametric {
              Type.name = ~~"foo";
              parameters = [Type.integer; Type.Top];
            })))
    (Type.Parametric {
        Type.name = ~~"foo";
        parameters = [Type.integer; Type.Top];
      });
  assert_equal
    (Type.optional_value
       (Type.Parametric {
           Type.name = ~~"foo";
           parameters = [Type.integer; Type.Top];
         }))
    (Type.Parametric {
        Type.name = ~~"foo";
        parameters = [Type.integer; Type.Top];
      })


let test_async_generator_value _ =
  assert_equal
    ~printer:(Format.asprintf "%a" Type.pp)
    (Type.async_generator_value (
        Type.Parametric {
          Type.name = Identifier.create "typing.AsyncGenerator";
          parameters = [Type.integer; Type.Optional Type.Bottom];
        }))
    (Type.Parametric {
        Type.name = Identifier.create "typing.Generator";
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
  let create name = Type.Primitive ~~name in
  assert_dequalify
    (Type.optional (Type.string))
    (Type.parametric "Optional" [Type.string]);

  assert_dequalify
    (Type.parametric "list" [Type.string])
    (Type.parametric "List" [Type.string]);

  assert_dequalify
    (Type.Union [Type.string; create "A.B.C"])
    (Type.parametric "typing.Union" [Type.string; create "C"]);

  assert_dequalify
    (create "d")
    (create "e");

  assert_dequalify
    (Type.parametric "A.B.C" [Type.optional (Type.integer)])
    (Type.parametric "C" [Type.parametric "Optional" [Type.integer]])


let () =
  Analysis.Type.TypeCache.disable ();
  "type">:::[
    "create">::test_create;
    "expression">::test_expression;
    "union">::test_union;
    "exists">::test_exists;
    "is_async_generator">::test_is_generator;
    "is_callable">::test_is_callable;
    "is_not_instantiated">::test_is_not_instantiated;
    "is_meta">::test_is_meta;
    "is_none">::test_is_none;
    "is_unknown">::test_is_unknown;
    "is_resolved">::test_is_resolved;
    "mismatch_with_any">::test_mismatch_with_any;
    "optional_value">::test_optional_value;
    "async_generator_value">::test_async_generator_value;
    "dequalify">::test_dequalify;
  ]
  |> run_test_tt_main
