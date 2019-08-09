(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let ( ! ) concretes = Type.OrderedTypes.Concrete concretes

let create_concatenation ?head ?tail ?mappers variable
    : (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
  =
  let mappers = Option.value mappers ~default:[] in
  Type.OrderedTypes.Concatenation.create
    ?head
    ?tail
    (Type.OrderedTypes.Concatenation.Middle.create ~mappers ~variable)


let test_create _ =
  let assert_create ?(aliases = fun _ -> None) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create ~aliases (parse_single_expression source))
  in
  assert_create "foo" (Type.Primitive "foo");
  assert_create "foo.bar" (Type.Primitive "foo.bar");
  assert_create "foo.$local_qualifier$bar" (Type.Primitive "foo.bar");
  assert_create "object" (Type.Primitive "object");
  assert_create "$unknown" Type.Top;
  assert_create "foo[bar]" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create
    "foo[bar, baz]"
    (Type.parametric "foo" ![Type.Primitive "bar"; Type.Primitive "baz"]);
  let variadic = Type.Variable.Variadic.List.create "Ts" in
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[Ts]"
    (Type.parametric "foo" (Type.Variable.Variadic.List.self_reference variadic));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    (Type.parametric "foo" (Concatenation (create_concatenation ~mappers:["list"] variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, bool, Ts]]"
    (Type.parametric
       "foo"
       (Concatenation (create_concatenation ~head:[Type.integer; Type.bool] variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[Ts, int, bool]]"
    (Type.parametric
       "foo"
       (Concatenation (create_concatenation ~tail:[Type.integer; Type.bool] variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, Ts, bool]]"
    (Type.parametric
       "foo"
       (Concatenation (create_concatenation ~head:[Type.integer] ~tail:[Type.bool] variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, \
     pyre_extensions.type_variable_operators.Map[list, Ts], bool]]"
    (Type.parametric
       "foo"
       (Concatenation
          (create_concatenation ~head:[Type.integer] ~tail:[Type.bool] ~mappers:["list"] variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[...]"
    (Type.parametric "foo" Any);
  assert_create "typing.List.__getitem__(int)" (Type.list Type.integer);
  assert_create
    "typing.Dict.__getitem__((int, str))"
    (Type.dictionary ~key:Type.integer ~value:Type.string);

  (* Type aliases in typeshed. *)
  assert_create "typing.Counter" (Type.Primitive "collections.Counter");
  assert_create "typing.Counter[int]" (Type.parametric "collections.Counter" ![Type.integer]);
  assert_create "typing.ChainMap" (Type.Primitive "collections.ChainMap");
  assert_create "typing.ChainMap[int]" (Type.parametric "collections.ChainMap" ![Type.integer]);
  assert_create "typing.Deque" (Type.Primitive "collections.deque");
  assert_create "typing.Deque[int]" (Type.parametric "collections.deque" ![Type.integer]);
  assert_create
    "typing_extensions.Protocol[int]"
    (Type.parametric "typing.Protocol" ![Type.integer]);
  assert_create "typing_extensions.Protocol" (Type.Primitive "typing.Protocol");

  (* Check renaming. *)
  assert_create "typing.List[int]" (Type.list Type.integer);
  assert_create "typing.List" (Primitive "list");
  assert_create
    "typing.DefaultDict[int, str]"
    (Type.parametric "collections.defaultdict" ![Type.integer; Type.string]);
  assert_create "typing.Dict[int, str]" (Type.dictionary ~key:Type.integer ~value:Type.string);
  assert_create "typing.Tuple[int, str]" (Type.tuple [Type.integer; Type.string]);
  assert_create "typing.Tuple[int, ...]" (Type.Tuple (Type.Unbounded Type.integer));
  assert_create "typing.Tuple[()]" (Type.tuple []);
  assert_create "tuple" (Type.Tuple (Type.Unbounded Type.Any));
  assert_create "typing.Any" Type.Any;
  assert_create "typing.Optional[int]" (Type.optional Type.integer);
  assert_create "typing.Optional.__getitem__(int)" (Type.optional Type.integer);
  assert_create "typing.Set[int]" (Type.set Type.integer);
  assert_create "typing.Union[int, str]" (Type.union [Type.integer; Type.string]);
  assert_create "typing.Union[int, typing.Any]" (Type.union [Type.integer; Type.Any]);
  assert_create "typing.Union[int, typing.Optional[$bottom]]" (Type.optional Type.integer);
  assert_create
    "typing.Union[int, typing.Optional[$bottom], str, typing.Tuple[int, str]]"
    (Type.optional (Type.union [Type.integer; Type.string; Type.tuple [Type.integer; Type.string]]));
  assert_create
    "typing.Union[typing.Optional[int], typing.Optional[str]]"
    (Type.optional (Type.union [Type.integer; Type.string]));
  assert_create
    "typing.Union[typing.Optional[int], str]"
    (Type.optional (Type.union [Type.integer; Type.string]));

  (* Annotated. *)
  assert_create "typing.Annotated[int]" (Type.annotated Type.integer);
  assert_create "typing.Annotated[int, Derp()]" (Type.annotated Type.integer);

  (* Nested renaming. *)
  assert_create "typing.Set[typing.Any]" (Type.set Type.Any);
  assert_create "typing.Dict[str, typing.Any]" (Type.dictionary ~key:Type.string ~value:Type.Any);

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
    (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "_T");
  assert_create "typing.TypeVar('_T', name=int)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$bound=int)"
    (Type.variable ~constraints:(Type.Variable.Bound Type.integer) "_T");
  assert_create
    "typing.TypeVar('_T', $parameter$bound='C')"
    (Type.variable ~constraints:(Type.Variable.Bound (Type.Primitive "C")) "_T");
  assert_create
    "typing.TypeVar('_T', 'C', X)"
    (Type.variable
       ~constraints:(Type.Variable.Explicit [Type.Primitive "C"; Type.Primitive "X"])
       "_T");
  assert_create
    "typing.TypeVar('_T', int, name=float)"
    (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "_T");
  assert_create
    "typing.TypeVar('_CallableT', bound='typing.Callable')"
    (Type.variable
       ~constraints:(Type.Variable.Bound (Type.Callable.create ~annotation:Type.Any ()))
       "_CallableT");

  (* Check that type aliases are resolved. *)
  let assert_alias source resolved =
    let aliases primitive =
      Identifier.Table.of_alist_exn
        [ "Alias", Type.Primitive "Aliased";
          ( "_Future",
            Type.union
              [ Type.parametric "Future" ![Type.integer; Type.variable "_T"];
                Type.awaitable (Type.variable "_T") ] ) ]
      |> (fun table -> Identifier.Table.find table primitive)
      >>| fun alias -> Type.TypeAlias alias
    in
    assert_create ~aliases source resolved
  in
  assert_alias "Alias" (Type.Primitive "Aliased");
  assert_alias "Aliased" (Type.Primitive "Aliased");
  assert_alias "typing.Optional[Alias]" (Type.optional (Type.Primitive "Aliased"));
  assert_alias "Parametric[Alias]" (Type.parametric "Parametric" ![Type.Primitive "Aliased"]);
  assert_alias "Alias[int]" (Type.parametric "Aliased" ![Type.integer]);

  (* TODO(T44787675): Implement actual generic aliases *)
  assert_alias
    "_Future[int]"
    (Type.union [Type.parametric "Future" ![Type.integer]; Type.awaitable Type.integer]);

  (* String literals. *)
  assert_create "'foo'" (Type.Primitive "foo");
  assert_create "'foo.bar'" (Type.Primitive "foo.bar");
  assert_create "foo['bar']" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create "'Type[str]'" (Type.parametric "Type" ![Type.Primitive "str"]);
  assert_create "'Type[[[]str]'" (Type.Primitive "Type[[[]str]");

  (* Recursive aliasing. *)
  let aliases = function
    | "A" -> Some (Type.Primitive "B")
    | "B" -> Some (Type.Primitive "C")
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "A" (Type.Primitive "C");

  (* Recursion with loop. *)
  let aliases = function
    | "A" -> Some (Type.Primitive "A")
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "A" (Type.Primitive "A");
  let aliases = function
    | "A" -> Some (Type.list (Type.Primitive "A"))
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "A" (Type.list (Type.Primitive "A"));

  (* Nested aliasing. *)
  let aliases = function
    | "A" -> Some (Type.list (Type.Primitive "B"))
    | "B" -> Some (Type.Primitive "C")
    | "X" -> Some (Type.Callable.create ~annotation:(Type.Primitive "A") ())
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "A" (Type.list (Type.Primitive "C"));
  assert_create ~aliases "X" (Type.Callable.create ~annotation:(Type.list (Type.Primitive "C")) ());

  (* Aliasing of subclasses through imports. *)
  let aliases = function
    | "A" -> Some (Type.Primitive "B")
    | "module.R" -> Some (Type.Primitive "module.R.R")
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "A" (Type.Primitive "B");
  assert_create ~aliases "A.InnerClass" (Type.Primitive "B.InnerClass");
  assert_create ~aliases "A.InnerClass[int]" (Type.parametric "B.InnerClass" ![Type.integer]);
  assert_create ~aliases "module.R.R" (Type.Primitive "module.R.R.R");
  assert_create
    ~aliases
    "A.InnerClass.InnerInnerClass"
    (Type.Primitive "B.InnerClass.InnerInnerClass");

  (* Aliases with Unions. *)
  let aliases = function
    | "A" -> Some (Type.union [Type.string; Type.bytes])
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create ~aliases "typing.Union[A, str]" (Type.union [Type.string; Type.bytes]);

  (* Callables. *)
  let default_overload =
    { Type.Callable.annotation = Type.Top; parameters = Undefined; define_location = None }
  in
  let open Type.Callable in
  assert_create "typing.Callable" (Type.Callable.create ~annotation:Type.Any ());
  assert_create "typing.Callable[..., int]" (Type.Callable.create ~annotation:Type.integer ());
  assert_create
    "typing.Callable.__getitem__((..., int))"
    (Type.Callable.create ~annotation:Type.integer ());
  assert_create
    "typing.Callable[..., int][[..., str]]"
    (Type.Callable.create
       ~overloads:[{ default_overload with annotation = Type.string }]
       ~annotation:Type.integer
       ());
  assert_create
    "typing.Callable('name')[..., int]"
    (Type.Callable
       {
         kind = Type.Callable.Named !&"name";
         implementation = { default_overload with annotation = Type.integer };
         overloads = [];
         implicit = None;
       });
  assert_create
    "typing.Callable('foo')[..., $unknown]"
    (Type.Callable
       {
         kind = Type.Callable.Named !&"foo";
         implementation = default_overload;
         overloads = [];
         implicit = None;
       });
  assert_create "typing.Other('name')[..., int]" Type.Top;
  assert_create
    "typing.Callable[[int, str], int]"
    (Type.Callable
       {
         kind = Type.Callable.Anonymous;
         implementation =
           {
             annotation = Type.integer;
             parameters =
               Defined
                 [ Parameter.Anonymous { index = 0; annotation = Type.integer; default = false };
                   Parameter.Anonymous { index = 1; annotation = Type.string; default = false } ];
             define_location = None;
           };
         overloads = [];
         implicit = None;
       });
  assert_create
    "typing.Callable[[int, Named(a, int), Variable(), Keywords()], int]"
    (Type.Callable
       {
         kind = Anonymous;
         implementation =
           {
             annotation = Type.integer;
             parameters =
               Defined
                 [ Parameter.Anonymous { index = 0; annotation = Type.integer; default = false };
                   Parameter.Named { name = "a"; annotation = Type.integer; default = false };
                   Parameter.Variable (Concrete Type.Top);
                   Parameter.Keywords Type.Top ];
             define_location = None;
           };
         overloads = [];
         implicit = None;
       });
  assert_create
    "typing.Callable[[int, Variable(int), Keywords(str)], int]"
    (Type.Callable
       {
         kind = Anonymous;
         implementation =
           {
             annotation = Type.integer;
             parameters =
               Defined
                 [ Parameter.Anonymous { index = 0; annotation = Type.integer; default = false };
                   Parameter.Variable (Concrete Type.integer);
                   Parameter.Keywords Type.string ];
             define_location = None;
           };
         overloads = [];
         implicit = None;
       });
  assert_create
    "typing.Callable[[Named(a, int, default)], int]"
    (Type.Callable
       {
         kind = Anonymous;
         implementation =
           {
             annotation = Type.integer;
             parameters =
               Defined [Parameter.Named { name = "a"; annotation = Type.integer; default = true }];
             define_location = None;
           };
         overloads = [];
         implicit = None;
       });
  assert_create "typing.Callable[int]" (Type.Callable.create ~annotation:Type.Top ());
  assert_create "function" (Type.Callable.create ~annotation:Type.Any ());
  assert_create
    "typing.Callable[..., function]"
    (Type.Callable.create ~annotation:(Type.Callable.create ~annotation:Type.Any ()) ());
  assert_create
    "mypy_extensions.TypedDict[('Movie', True, ('year', int), ('name', str))]"
    (Type.TypedDictionary
       {
         name = "Movie";
         fields =
           [ { name = "year"; annotation = Type.integer };
             { name = "name"; annotation = Type.string } ];
         total = true;
       });
  assert_create
    "mypy_extensions.TypedDict[('Movie', False, ('year', int), ('name', str))]"
    (Type.TypedDictionary
       {
         name = "Movie";
         fields =
           [ { name = "year"; annotation = Type.integer };
             { name = "name"; annotation = Type.string } ];
         total = false;
       });
  assert_create
    ~aliases:(function
      | "Ts" -> Some (Type.VariableAlias (ListVariadic (Type.Variable.Variadic.List.create "Ts")))
      | _ -> None)
    "typing.Tuple[Ts]"
    (Type.Tuple
       (Bounded (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")))));
  assert_create "typing.Tuple[...]" (Type.Tuple (Bounded Any));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (Type.VariableAlias (ListVariadic (Type.Variable.Variadic.List.create "Ts")))
      | _ -> None)
    "typing.Callable[[Ts], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [ Variable
                (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts"))) ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (Type.VariableAlias (ListVariadic (Type.Variable.Variadic.List.create "Ts")))
      | _ -> None)
    "typing.Callable[[int, Variable(Ts)], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [ Anonymous { index = 0; annotation = Type.integer; default = false };
              Variable
                (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts"))) ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (Type.VariableAlias (ListVariadic (Type.Variable.Variadic.List.create "Ts")))
      | _ -> None)
    "typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    (Type.Tuple
       (Bounded
          (Concatenation
             (create_concatenation ~mappers:["list"] (Type.Variable.Variadic.List.create "Ts")))));
  ()


let test_instantiate _ =
  let assert_instantiate mappings ~generic ~expected =
    let map = Type.Map.of_alist_exn mappings in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      (Type.instantiate ~constraints:(Map.find map) generic)
  in
  assert_instantiate [] ~generic:(Type.Primitive "foo") ~expected:(Type.Primitive "foo");

  (* Union[_T, _VT] + (_T = int, _VT = None) -> Optional[int] *)
  assert_instantiate
    [Type.variable "_T", Type.integer; Type.variable "_VT", Type.Optional Type.Bottom]
    ~generic:(Type.Union [Type.variable "_T"; Type.variable "_VT"])
    ~expected:(Type.Optional Type.integer)


let test_expression _ =
  let assert_expression annotation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:Expression.equal
      (parse_single_expression ~coerce_special_methods:true expression)
      (Type.expression annotation)
  in
  assert_expression (Type.Primitive "foo") "foo";
  assert_expression (Type.Primitive "...") "...";
  assert_expression (Type.Primitive "foo.bar") "foo.bar";
  assert_expression Type.Top "$unknown";
  assert_expression
    (Type.Parametric { name = "foo.bar"; parameters = ![Type.Primitive "baz"] })
    "foo.bar.__getitem__(baz)";
  assert_expression
    (Type.Tuple (Type.Bounded (Type.OrderedTypes.Concrete [Type.integer; Type.string])))
    "typing.Tuple.__getitem__((int, str))";
  assert_expression
    (Type.Tuple (Type.Unbounded Type.integer))
    "typing.Tuple.__getitem__((int, ...))";
  assert_expression
    (Type.Parametric { name = "list"; parameters = ![Type.integer] })
    "typing.List.__getitem__(int)";
  assert_expression
    (Type.Parametric
       {
         name = "foo.Variadic";
         parameters =
           Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts"));
       })
    "foo.Variadic.__getitem__(Ts)";
  assert_expression
    (Type.Parametric { name = "foo.Variadic"; parameters = Any })
    "foo.Variadic.__getitem__(...)";
  assert_expression
    (Type.Parametric
       {
         name = "foo.Variadic";
         parameters =
           Concatenation
             (create_concatenation ~mappers:["Foo"] (Type.Variable.Variadic.List.create "Ts"));
       })
    "foo.Variadic.__getitem__(pyre_extensions.type_variable_operators.Map.__getitem__((Foo, Ts)))";

  (* Callables. *)
  let open Type.Callable in
  assert_expression
    (Type.Callable.create ~annotation:Type.integer ())
    "typing.Callable.__getitem__((..., int))";
  assert_expression
    (Type.Callable.create ~name:!&"name" ~annotation:Type.integer ())
    "typing.Callable.__getitem__((..., int))";
  assert_expression
    (Type.Callable.create
       ~overloads:
         [ {
             Type.Callable.annotation = Type.string;
             parameters = Type.Callable.Undefined;
             define_location = None;
           };
           {
             Type.Callable.annotation = Type.integer;
             parameters = Type.Callable.Undefined;
             define_location = None;
           } ]
       ~annotation:Type.integer
       ())
    "typing.Callable[(..., int)].__getitem__(__getitem__((..., str))[(..., int)])";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [ Parameter.Named { name = "__0"; annotation = Type.integer; default = false };
              Parameter.Named { name = "__1"; annotation = Type.string; default = false } ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(__0, int), Named(__1, str)], int))";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [ Parameter.Named { name = "a"; annotation = Type.integer; default = false };
              Parameter.Named { name = "b"; annotation = Type.string; default = false } ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(a, int), Named(b, str)], int))";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [Parameter.Named { name = "a"; annotation = Type.integer; default = true }])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(a, int, default)], int))";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Defined
            [ Parameter.Named { name = "$0"; annotation = Type.integer; default = false };
              Parameter.Variable (Concrete Type.integer);
              Parameter.Keywords Type.string ])
       ~annotation:Type.integer
       ())
    ("typing.Callable.__getitem__(([Named($0, int), Variable(int), " ^ "Keywords(str)], int))");
  assert_expression
    (Type.TypedDictionary
       {
         name = "Movie";
         fields =
           [ { name = "title"; annotation = Type.string };
             { name = "year"; annotation = Type.integer } ];
         total = true;
       })
    "mypy_extensions.TypedDict[(\"Movie\", True, (\"title\", str), (\"year\", int))]";
  assert_expression
    (Type.TypedDictionary
       {
         name = "Movie";
         fields =
           [ { name = "title"; annotation = Type.string };
             { name = "year"; annotation = Type.integer } ];
         total = false;
       })
    "mypy_extensions.TypedDict[(\"Movie\", False, (\"title\", str), (\"year\", int))]"


let test_concise _ =
  let assert_concise annotation expected =
    assert_equal ~printer:(fun annotation -> annotation) expected (Type.show_concise annotation)
  in
  assert_concise Type.Bottom "?";
  assert_concise Type.Top "unknown";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:Type.Callable.Undefined
       ())
    "(...) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [ Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = false };
              Type.Callable.Parameter.Named
                { name = "y"; annotation = Type.float; default = false } ])
       ())
    "(x: Any, y: float) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [Type.Callable.Parameter.Anonymous { index = 0; annotation = Type.Any; default = true }])
       ())
    "(Any=...) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = true }])
       ())
    "(x: Any = ...) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [ Type.Callable.Parameter.Named
                {
                  name = "callable";
                  default = false;
                  annotation =
                    Type.Callable.create
                      ~name:!&"bar"
                      ~annotation:Type.float
                      ~parameters:
                        (Type.Callable.Defined
                           [ Type.Callable.Parameter.Named
                               { name = "x"; annotation = Type.integer; default = false } ])
                      ();
                } ])
       ())
    "(callable: (x: int) -> float) -> int";
  assert_concise Type.Any "Any";
  assert_concise (Type.Optional Type.Bottom) "None";
  assert_concise (Type.Optional Type.integer) "Optional[int]";
  assert_concise (Type.parametric "parametric" ![Type.Top; Type.Top]) "parametric[]";
  assert_concise
    (Type.parametric "parametric" ![Type.Top; Type.float])
    "parametric[unknown, float]";
  assert_concise (Type.Primitive "a.b.c") "c";
  assert_concise (Type.tuple [Type.integer; Type.Any]) "Tuple[int, Any]";
  assert_concise (Type.Tuple (Type.Unbounded Type.integer)) "Tuple[int, ...]";
  assert_concise
    (Type.TypedDictionary
       {
         name = "Movie";
         fields =
           [ { name = "year"; annotation = Type.integer };
             { name = "name"; annotation = Type.string } ];
         total = true;
       })
    "Movie";
  assert_concise
    (Type.TypedDictionary
       {
         name = "$anonymous";
         fields =
           [ { name = "year"; annotation = Type.integer };
             { name = "name"; annotation = Type.string } ];
         total = true;
       })
    "TypedDict(year: int, name: str)";
  assert_concise (Type.union [Type.integer; Type.string]) "Union[int, str]";
  assert_concise (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T") "T"


let test_union _ =
  assert_equal (Type.union [Type.string; Type.float]) (Type.Union [Type.float; Type.string]);
  assert_equal (Type.union [Type.float; Type.string]) (Type.Union [Type.float; Type.string]);
  assert_equal
    (Type.union [Type.optional Type.string; Type.float])
    (Type.Optional (Type.Union [Type.string; Type.float]));
  assert_equal
    (Type.union [Type.float; Type.string; Type.optional Type.float])
    (Type.Optional (Type.Union [Type.float; Type.string]));
  assert_false (Type.equal (Type.union [Type.float; Type.Any]) Type.Any);
  assert_true (Type.equal (Type.union [Type.float; Type.Top]) Type.Top);
  assert_true
    (Type.equal (Type.union [Type.string; Type.float]) (Type.Union [Type.float; Type.string]));
  assert_true
    (Type.equal (Type.union [Type.float; Type.string]) (Type.Union [Type.float; Type.string]));
  assert_true (Type.equal (Type.union [Type.float]) Type.float);
  assert_true (Type.equal (Type.union [Type.float; Type.Bottom]) Type.float);
  assert_true (Type.equal (Type.union [Type.Bottom; Type.Bottom]) Type.Bottom);

  (* Flatten unions. *)
  assert_equal
    (Type.union [Type.float; Type.union [Type.string; Type.bytes]])
    (Type.union [Type.float; Type.string; Type.bytes]);
  assert_equal
    (Type.union [Type.Optional (Type.list Type.integer); Type.list Type.integer])
    (Type.Optional (Type.list Type.integer));
  assert_equal
    (Type.union [Type.Optional (Type.variable "A"); Type.variable "A"])
    (Type.Optional (Type.variable "A"));
  ()


let test_primitives _ =
  assert_equal [] (Type.primitives (Type.Callable.create ~annotation:Type.Top ()));
  assert_equal [Type.integer] (Type.primitives (Type.Callable.create ~annotation:Type.integer ()));
  assert_equal [] (Type.primitives (Type.optional Type.Top));
  assert_equal [Type.integer] (Type.primitives (Type.optional Type.integer));
  assert_equal [] (Type.primitives (Type.Tuple (Type.Unbounded Type.Top)));
  assert_equal [Type.integer] (Type.primitives (Type.Tuple (Type.Unbounded Type.integer)));
  assert_equal
    []
    (Type.primitives (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T"));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "T"));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.parametric "parametric" ![Type.integer; Type.Top]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.parametric "parametric" ![Type.integer; Type.string]));
  assert_equal [Type.string] (Type.primitives (Type.tuple [Type.Top; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.tuple [Type.integer; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.primitives (Type.union [Type.integer; Type.string]));
  assert_equal [] (Type.primitives Type.Top);
  assert_equal [] (Type.primitives Type.Bottom);
  assert_equal [Type.integer] (Type.primitives Type.integer);
  assert_equal [] (Type.primitives Type.Any);
  assert_equal
    [Type.integer; Type.string]
    ( Type.TypedDictionary
        {
          name = "Movie";
          fields =
            [ { name = "year"; annotation = Type.integer };
              { name = "name"; annotation = Type.string } ];
          total = true;
        }
    |> Type.primitives )


let test_elements _ =
  let assert_equal = assert_equal ~printer:(List.to_string ~f:Fn.id) in
  assert_equal ["typing.Callable"] (Type.elements (Type.Callable.create ~annotation:Type.Top ()));
  assert_equal
    ["int"; "typing.Callable"]
    (Type.elements (Type.Callable.create ~annotation:Type.integer ()));
  assert_equal ["int"; "typing.Optional"] (Type.elements (Type.optional Type.integer));
  assert_equal ["tuple"] (Type.elements (Type.Tuple (Type.Unbounded Type.Top)));
  assert_equal ["int"; "tuple"] (Type.elements (Type.Tuple (Type.Unbounded Type.integer)));
  assert_equal
    []
    (Type.elements (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T"));
  assert_equal
    ["int"]
    (Type.elements (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "T"));
  assert_equal
    ["int"; "parametric"]
    (Type.elements (Type.parametric "parametric" ![Type.integer; Type.Top]));
  assert_equal
    ["int"; "str"; "parametric"]
    (Type.elements (Type.parametric "parametric" ![Type.integer; Type.string]));
  assert_equal ["str"; "tuple"] (Type.elements (Type.tuple [Type.Top; Type.string]));
  assert_equal ["int"; "str"; "tuple"] (Type.elements (Type.tuple [Type.integer; Type.string]));
  assert_equal
    ["int"; "str"; "typing.Union"]
    (Type.elements (Type.union [Type.integer; Type.string]));
  assert_equal [] (Type.elements Type.Top);
  assert_equal [] (Type.elements Type.Bottom);
  assert_equal ["int"] (Type.elements Type.integer);
  assert_equal [] (Type.elements Type.Any);
  assert_equal
    ["int"; "str"; "TypedDictionary"]
    ( Type.TypedDictionary
        {
          name = "Movie";
          fields =
            [ { name = "year"; annotation = Type.integer };
              { name = "name"; annotation = Type.string } ];
          total = true;
        }
    |> Type.elements )


let test_exists _ =
  let top_exists =
    Type.exists ~predicate:(function
        | Type.Top -> true
        | _ -> false)
  in
  assert_true (top_exists (Type.Callable.create ~annotation:Type.Top ()));
  assert_false (top_exists (Type.Callable.create ~annotation:Type.integer ()));
  assert_true (top_exists (Type.optional Type.Top));
  assert_false (top_exists (Type.optional Type.integer));
  assert_true (top_exists (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (top_exists (Type.Tuple (Type.Unbounded Type.integer)));
  assert_true (top_exists (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T"));
  assert_false
    (top_exists (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "T"));
  assert_true (top_exists (Type.parametric "parametric" ![Type.integer; Type.Top]));
  assert_false (top_exists (Type.parametric "parametric" ![Type.integer; Type.string]));
  assert_true (top_exists (Type.tuple [Type.Top; Type.string]));
  assert_false (top_exists (Type.tuple [Type.integer; Type.string]));
  assert_true (top_exists (Type.union [Type.integer; Type.Top]));
  assert_false (top_exists (Type.union [Type.integer; Type.string]));
  assert_true (top_exists Type.Top);
  assert_false (top_exists Type.Bottom);
  assert_false (top_exists Type.integer);
  assert_false (top_exists Type.Any)


let test_is_iterator _ =
  assert_true (Type.is_iterator (Type.iterator Type.string));
  assert_false (Type.is_iterator Type.string);
  assert_false (Type.is_iterator (Type.Primitive "typing.Iterator"))


let test_is_generator _ =
  assert_true (Type.is_generator (Type.generator Type.string));
  assert_false (Type.is_generator Type.string);
  assert_true (Type.is_generator (Type.generator ~async:true Type.string))


let test_contains_callable _ =
  assert_true (Type.contains_callable (Type.Callable.create ~annotation:Type.integer ()));
  assert_true
    (Type.contains_callable (Type.Optional (Type.Callable.create ~annotation:Type.integer ())));
  assert_true
    (Type.contains_callable
       (Type.union [Type.string; Type.Callable.create ~annotation:Type.integer ()]));
  assert_false (Type.contains_callable (Type.Primitive "foo"))


let test_contains_any _ = assert_true (Type.contains_any Type.Any)

let test_is_concrete _ =
  assert_true (Type.is_concrete Type.none);
  assert_true (Type.is_concrete (Type.parametric "typing.Optional" ![Type.Bottom]));
  assert_true (Type.is_concrete (Type.Callable.create ~annotation:Type.none ()));
  assert_false (Type.is_concrete (Type.Callable.create ~annotation:(Type.list Type.Bottom) ()));
  ()


let test_is_not_instantiated _ =
  assert_true (Type.is_not_instantiated Type.Bottom);
  assert_true (Type.is_not_instantiated (Type.dictionary ~key:Type.Bottom ~value:Type.Bottom));
  assert_true (Type.is_not_instantiated (Type.Optional Type.Bottom));
  assert_false (Type.is_not_instantiated Type.Top);
  assert_true (Type.is_not_instantiated (Type.variable "_T"))


let test_is_meta _ =
  assert_true (Type.is_meta (Type.Parametric { name = "type"; parameters = ![Type.integer] }));
  assert_false (Type.is_meta Type.integer);
  assert_false
    (Type.is_meta (Type.Parametric { name = "typing.Type"; parameters = ![Type.integer] }))


let test_is_none _ =
  assert_false (Type.is_none (Type.Primitive "None"));
  assert_false (Type.is_none Type.integer);
  assert_false (Type.is_none (Type.Primitive "foo"));
  assert_true (Type.is_none (Type.Optional Type.Bottom))


let test_is_type_alias _ =
  assert_true (Type.is_type_alias (Type.Primitive "typing.TypeAlias"));
  assert_false (Type.is_type_alias (Type.parametric "typing.TypeAlias" ![Type.Top]))


let test_is_unknown _ =
  assert_false (Type.is_unknown Type.Bottom);
  assert_false (Type.is_unknown Type.Any);
  assert_true (Type.is_unknown (Type.optional Type.Top));
  assert_false (Type.is_unknown (Type.optional Type.integer));
  assert_true
    (Type.is_unknown
       (Type.Optional (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] })));
  assert_true
    (Type.is_unknown (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] }));
  assert_false (Type.is_unknown (Type.Parametric { name = "foo"; parameters = ![Type.integer] }));
  assert_false (Type.is_unknown Type.integer);
  assert_true (Type.is_unknown Type.Top);
  assert_true (Type.is_unknown (Type.Union [Type.integer; Type.Top]));
  assert_false (Type.is_unknown (Type.Union [Type.integer; Type.string]));
  assert_false (Type.is_unknown (Type.variable "derp"));
  assert_true (Type.is_unknown (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.Top]))));
  assert_false (Type.is_unknown (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.string]))));
  assert_true (Type.is_unknown (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (Type.is_unknown (Type.Tuple (Type.Unbounded Type.integer)))


let test_is_resolved _ =
  assert_false (Type.Variable.all_variables_are_resolved (Type.variable "_T"));
  assert_false
    (Type.Variable.all_variables_are_resolved (Type.union [Type.integer; Type.variable "_T"]));
  assert_true (Type.Variable.all_variables_are_resolved Type.integer);
  assert_true (Type.Variable.all_variables_are_resolved (Type.union [Type.integer; Type.string]));
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  assert_false
    (Type.Variable.all_variables_are_resolved
       (Type.Callable.create
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
          ~annotation:Type.integer
          ()));
  let parameter_variadic = parameter_variadic |> Type.Variable.Variadic.Parameters.mark_as_bound in
  assert_true
    (Type.Variable.all_variables_are_resolved
       (Type.Callable.create
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
          ~annotation:Type.integer
          ()));
  let list_variadic = Type.Variable.Variadic.List.create "Ts" in
  assert_false
    (Type.Variable.all_variables_are_resolved
       (Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))));
  let list_variadic = list_variadic |> Type.Variable.Variadic.List.mark_as_bound in
  assert_true
    (Type.Variable.all_variables_are_resolved
       (Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))));
  ()


let test_class_name _ =
  let assert_class_name annotation expected =
    assert_equal
      ~cmp:Reference.equal
      ~printer:Reference.show
      (Reference.create expected)
      (Type.class_name annotation)
  in
  assert_class_name (Type.Primitive "qualified.primitive") "qualified.primitive";
  assert_class_name (Type.list Type.integer) "list";
  assert_class_name (Type.union [Type.string; Type.integer]) "typing.Union";
  assert_class_name (Type.variable "_T") "_T"


let test_optional_value _ =
  assert_equal
    (Type.optional_value
       (Type.Optional (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] })))
    (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] });
  assert_equal
    (Type.optional_value (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] }))
    (Type.Parametric { name = "foo"; parameters = ![Type.integer; Type.Top] })


let test_async_generator_value _ =
  assert_equal
    ~printer:(Format.asprintf "%a" Type.pp)
    (Type.async_generator_value
       (Type.Parametric
          {
            name = "typing.AsyncGenerator";
            parameters = ![Type.integer; Type.Optional Type.Bottom];
          }))
    (Type.Parametric
       {
         name = "typing.Generator";
         parameters = ![Type.integer; Type.Optional Type.Bottom; Type.Optional Type.Bottom];
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
      (Type.dequalify (Preprocessing.dequalify_map (parse map)) source)
      expected
  in
  let create name = Type.Primitive name in
  assert_dequalify (Type.optional Type.string) (Type.parametric "Optional" ![Type.string]);
  assert_dequalify (Type.parametric "list" ![Type.string]) (Type.parametric "List" ![Type.string]);
  assert_dequalify
    (Type.Union [Type.string; create "A.B.C"])
    (Type.parametric "typing.Union" ![create "C"; Type.string]);
  assert_dequalify (create "d") (create "e");
  assert_dequalify
    (Type.parametric "A.B.C" ![Type.optional Type.integer])
    (Type.parametric "C" ![Type.parametric "Optional" ![Type.integer]]);
  let assert_dequalify_variable source expected =
    assert_equal
      ~cmp:Type.Variable.equal
      ~printer:(Format.asprintf "%a" Type.Variable.pp)
      ~pp_diff:(diff ~print:Type.Variable.pp)
      (Type.Variable.dequalify (Preprocessing.dequalify_map (parse map)) source)
      expected
  in
  assert_dequalify_variable
    (Type.Variable.Unary (Type.Variable.Unary.create "A.B.C"))
    (Type.Variable.Unary (Type.Variable.Unary.create "C"));
  assert_dequalify_variable
    (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "A.B.C"))
    (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "C"));
  assert_dequalify_variable
    (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "A.B.C"))
    (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "C"));
  ()


let test_from_overloads _ =
  let assert_create ?(aliases = fun _ -> None) sources expected =
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
    [ "typing.Callable('foo')[..., int]";
      "typing.Callable('foo')[[int, str], str]";
      "typing.Callable('foo')[[int, str, str], int]" ]
    "typing.Callable('foo')[[int, str, str], int]";
  assert_create
    [ "typing.Callable('foo')[..., $unknown][[[int], int]]";
      "typing.Callable('foo')[[str], str]";
      "typing.Callable('foo')[[int], int][[[str], str]]" ]
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
      |> List.map ~f:(function
             | Type.Callable.Parameter.Anonymous { annotation; _ } -> annotation
             | _ -> failwith "impossible")
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
      let aliases = Identifier.Map.of_alist_exn ["T", Type.variable "T"; "S", Type.variable "S"] in
      Map.find aliases
    in
    let aliases = create_type_alias_table aliases in
    let variables =
      Type.create ~aliases (parse_single_expression source)
      |> Type.Variable.all_free_variables
      |> List.filter_map ~f:(function
             | Type.Variable.Unary variable -> Some variable
             | _ -> None)
      |> List.map ~f:(fun variable -> Type.Variable variable)
    in
    assert_equal (List.map expected ~f:Type.variable) variables
  in
  assert_variables "T" ["T"];
  assert_variables "Parametric[int, T]" ["T"];
  assert_variables "Parametric[T, S]" ["T"; "S"];
  assert_variables "typing.Callable[..., int]" [];
  assert_variables "typing.Callable[..., T]" ["T"];
  assert_variables "typing.Callable[[T, int], str]" ["T"];
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  let unary = Type.Variable.Unary.create "T" in
  assert_equal
    [Type.Variable.Unary unary; Type.Variable.ParameterVariadic parameter_variadic]
    (Type.Variable.all_free_variables
       (Type.Callable.create
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
          ~annotation:(Type.Variable unary)
          ()));
  ()


let test_parameter_name_compatibility _ =
  let parameter name =
    Type.Callable.Parameter.Named { name; annotation = Type.integer; default = false }
  in
  assert_true
    (Type.Callable.Parameter.names_compatible (parameter "argument") (parameter "argument"));
  assert_true (Type.Callable.Parameter.names_compatible (parameter "argument") (parameter "$0"));
  assert_true (Type.Callable.Parameter.names_compatible (parameter "$0") (parameter "argument"));
  assert_true (Type.Callable.Parameter.names_compatible (parameter "$0") (parameter "$1"));

  (* Underscores are ignored for the purposes of typechecking parameter compatibility. *)
  assert_true
    (Type.Callable.Parameter.names_compatible (parameter "argument") (parameter "_argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible (parameter "_argument") (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible (parameter "__argument") (parameter "argument"));
  assert_true
    (Type.Callable.Parameter.names_compatible (parameter "argument") (parameter "__argument"));
  assert_false
    (Type.Callable.Parameter.names_compatible (parameter "argument") (parameter "other"));
  assert_false
    (Type.Callable.Parameter.names_compatible (parameter "_argument") (parameter "other"))


let test_lambda _ =
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Named(x, str)], int]")
       (Type.lambda ~parameters:["x", Type.string] ~return_annotation:Type.integer));
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Keywords(str)], int]")
       (Type.lambda ~parameters:["**kwargs", Type.string] ~return_annotation:Type.integer));
  assert_true
    (Type.equal
       (parse_callable "typing.Callable[[Variable(str)], int]")
       (Type.lambda ~parameters:["*args", Type.string] ~return_annotation:Type.integer))


let test_visit _ =
  let create source = Type.create ~aliases:(fun _ -> None) (parse_single_expression source) in
  let assert_types_equal annotation expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected annotation
  in
  let module CountTransform = Type.Transform.Make (struct
    type state = int

    let visit state _ =
      { Type.Transform.transformed_annotation = Type.integer; new_state = state + 1 }


    let visit_children_before _ _ = true

    let visit_children_after = false
  end)
  in
  let end_state, transformed = CountTransform.visit 0 (create "typing.List[int]") in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 2 end_state;
  let end_state, transformed = CountTransform.visit 0 (create "Foo[Bar[Baz, Bop], Bang]") in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 5 end_state;
  let module SubstitutionTransform = Type.Transform.Make (struct
    type state = int

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive integer when integer = "int" && state > 0 -> state - 1, Type.string
        | _ -> state, annotation
      in
      { Type.Transform.transformed_annotation; new_state }


    let visit_children_before _ = function
      | Type.Optional _ -> false
      | _ -> true


    let visit_children_after = false
  end)
  in
  let end_state, transformed =
    SubstitutionTransform.visit 1 (create "typing.Callable[[int], int]")
  in
  assert_types_equal transformed (create "typing.Callable[[str], int]");
  assert_equal ~printer:string_of_int 0 end_state;
  let end_state, transformed =
    SubstitutionTransform.visit 1 (create "typing.Callable[[typing.Optional[int], int], int]")
  in
  assert_types_equal transformed (create "typing.Callable[[typing.Optional[int], str], int]");
  assert_equal ~printer:string_of_int 0 end_state;
  let end_state, transformed =
    SubstitutionTransform.visit
      1
      (create "mypy_extensions.TypedDict[('int', True, ('int', int), ('str', int))]")
  in
  assert_types_equal
    transformed
    (create "mypy_extensions.TypedDict[('int', True, ('int', str), ('str', int))]");
  assert_equal ~printer:string_of_int 0 end_state;
  let module ConcatenateTransform = Type.Transform.Make (struct
    type state = string

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive primitive -> state ^ primitive, annotation
        | Type.Parametric { name; parameters } ->
            "", Type.Parametric { name = name ^ state; parameters }
        | _ -> state, annotation
      in
      { Type.Transform.transformed_annotation; new_state }


    let visit_children_before _ _ = true

    let visit_children_after = false
  end)
  in
  let end_state, transformed =
    ConcatenateTransform.visit "" (create "Foo[Bar[Baz, Bop], Bro[Loop, typing.Optional[Land]]]")
  in
  assert_types_equal
    transformed
    (create "Foo[BarBazBop[Baz, Bop], BroLoopLand[Loop, typing.Optional[Land]]]");
  assert_equal "" end_state;
  let module TopDownConcatenateTransform = Type.Transform.Make (struct
    type state = string

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive primitive -> "", Type.Primitive (state ^ primitive)
        | Type.Parametric { name; parameters } ->
            state ^ name, Type.Parametric { name; parameters }
        | _ -> state, annotation
      in
      { Type.Transform.transformed_annotation; new_state }


    let visit_children_before _ _ = false

    let visit_children_after = true
  end)
  in
  let end_state, transformed =
    TopDownConcatenateTransform.visit "" (create "Foo[Bar[Bro[typing.Optional[Land]]]]")
  in
  assert_types_equal transformed (create "Foo[Bar[Bro[typing.Optional[FooBarBroLand]]]]");
  assert_equal "" end_state;
  ()


let test_collapse_escaped_variable_unions _ =
  let assert_types_equal annotation expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected annotation
  in
  let escaped = Type.Variable.mark_all_free_variables_as_escaped (Type.variable "Escapee") in
  assert_types_equal (Type.union [Type.integer; escaped]) (Type.Union [Type.integer; escaped]);
  assert_types_equal
    (Type.Variable.collapse_all_escaped_variable_unions (Type.Union [Type.integer; escaped]))
    Type.integer;
  let unescaped = Type.variable "NotEscaped" in
  assert_types_equal
    (Type.Variable.collapse_all_escaped_variable_unions (Type.Union [Type.integer; unescaped]))
    (Type.Union [Type.integer; unescaped]);
  ()


let test_namespace_insensitive_compare _ =
  let no_namespace_variable = Type.Variable.Unary.create "A" in
  let namespaced_variable_1 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  let namespaced_variable_2 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  assert_false (Type.compare namespaced_variable_1 namespaced_variable_2 == 0);
  assert_equal (Type.namespace_insensitive_compare namespaced_variable_1 namespaced_variable_2) 0;
  assert_equal
    (Type.namespace_insensitive_compare
       (Type.list namespaced_variable_1)
       (Type.list namespaced_variable_2))
    0;
  ()


let test_namespace _ =
  let no_namespace_variable = Type.Variable.Unary.create "A" in
  let namespaced_variable_1 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable.Unary { no_namespace_variable with namespace }
  in
  let namespace_2 = Type.Variable.Namespace.create_fresh () in
  let namespaced_variable_2 =
    Type.Variable.Unary { no_namespace_variable with namespace = namespace_2 }
  in
  assert_equal
    (Type.Variable.namespace namespaced_variable_1 ~namespace:namespace_2)
    namespaced_variable_2;
  ()


let test_mark_all_variables_as_bound _ =
  let variable = Type.Variable (Type.Variable.Unary.create "T") in
  assert_false (Type.Variable.all_variables_are_resolved variable);
  let variable = Type.Variable.mark_all_variables_as_bound variable in
  assert_true (Type.Variable.all_variables_are_resolved variable);
  let callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.all_variables_are_resolved callable);
  let callable = Type.Variable.mark_all_variables_as_bound callable in
  assert_true (Type.Variable.all_variables_are_resolved callable);
  let tuple =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_false (Type.Variable.all_variables_are_resolved tuple);
  let tuple = Type.Variable.mark_all_variables_as_bound tuple in
  assert_true (Type.Variable.all_variables_are_resolved tuple);
  let callable =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.all_variables_are_resolved callable);
  let callable = Type.Variable.mark_all_variables_as_bound callable in
  assert_true (Type.Variable.all_variables_are_resolved callable);
  ()


let test_namespace_all_free_variables _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let bound_variable =
    Type.Variable.Unary.create "T2"
    |> Type.Variable.Unary.mark_as_bound
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; bound_variable] in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free =
    Type.Variable.Unary.create "T"
    |> Type.Variable.Unary.namespace ~namespace
    |> fun variable -> Type.Variable variable
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free; bound_variable]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let annotation = Type.parametric "p" ![free_variable_callable; bound_variable_callable] in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free_callable; bound_variable_callable]);
  let free_variable_tuple =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let bound_variable_tuple =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let annotation = Type.parametric "p" ![free_variable_tuple; bound_variable_tuple] in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free_tuple =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free_tuple; bound_variable_tuple]);
  let free_variable_star_args_callable =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let bound_variable_star_args_callable =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let annotation =
    Type.parametric "p" ![free_variable_star_args_callable; bound_variable_star_args_callable]
  in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free_star_args_callable =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free_star_args_callable; bound_variable_star_args_callable]);
  ()


let test_mark_all_free_variables_as_escaped _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let bound_variable =
    Type.Variable.Unary.create "T2"
    |> Type.Variable.Unary.mark_as_bound
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; bound_variable] in
  Type.Variable.Namespace.reset ();
  let escaped_free =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable.Unary.create "T"
    |> Type.Variable.Unary.mark_as_escaped
    |> Type.Variable.Unary.namespace ~namespace
    |> fun variable -> Type.Variable variable
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free; bound_variable]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let annotation = Type.parametric "p" ![free_variable_callable; bound_variable_callable] in
  Type.Variable.Namespace.reset ();
  let escaped_free_callable =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_escaped
      |> Type.Variable.Variadic.Parameters.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free_callable; bound_variable_callable]);
  let free_variable_tuple =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let bound_variable_tuple =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let annotation = Type.parametric "p" ![free_variable_tuple; bound_variable_tuple] in
  Type.Variable.Namespace.reset ();
  let escaped_free_tuple =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts"
      |> Type.Variable.Variadic.List.mark_as_escaped
      |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free_tuple; bound_variable_tuple]);
  let free_variable_star_args_callable =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let bound_variable_star_args_callable =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let annotation =
    Type.parametric "p" ![free_variable_star_args_callable; bound_variable_star_args_callable]
  in
  Type.Variable.Namespace.reset ();
  let escaped_free_star_args_tuple =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts"
      |> Type.Variable.Variadic.List.mark_as_escaped
      |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free_star_args_tuple; bound_variable_star_args_callable]);
  ()


let test_contains_escaped_free_variable _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable);
  let escaped_free =
    Type.Variable.Unary.create "T"
    |> Type.Variable.Unary.mark_as_escaped
    |> fun variable -> Type.Variable variable
  in
  assert_true (Type.Variable.contains_escaped_free_variable escaped_free);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable_callable);
  let escaped_free_variable_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_escaped
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  assert_true (Type.Variable.contains_escaped_free_variable escaped_free_variable_callable);
  let free_variable_tuple =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable_tuple);
  let escaped_free_variable_tuple =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_escaped
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_true (Type.Variable.contains_escaped_free_variable escaped_free_variable_tuple);
  let free_variable_star_args_callable =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable_star_args_callable);
  let escaped_free_variable_star_args_callable =
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_escaped
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  assert_true
    (Type.Variable.contains_escaped_free_variable escaped_free_variable_star_args_callable);
  ()


let test_convert_all_escaped_free_variables_to_anys _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let escaped_free =
    Type.Variable.Unary.create "T"
    |> Type.Variable.Unary.mark_as_escaped
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; escaped_free] in
  assert_equal
    (Type.Variable.convert_all_escaped_free_variables_to_anys annotation)
    (Type.parametric "p" ![free_variable; Type.Any]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let escaped_free_callable =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_escaped
      |> Type.Variable.Variadic.Parameters.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let annotation = Type.parametric "p" ![free_variable_callable; escaped_free_callable] in
  let any_callable =
    Type.Callable.create ~parameters:Type.Callable.Undefined ~annotation:Type.integer ()
  in
  assert_equal
    (Type.Variable.convert_all_escaped_free_variables_to_anys annotation)
    (Type.parametric "p" ![free_variable_callable; any_callable]);
  let free_variable_tuple =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let escaped_free_tuple =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts"
      |> Type.Variable.Variadic.List.mark_as_escaped
      |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  let annotation = Type.parametric "p" ![free_variable_tuple; escaped_free_tuple] in
  assert_equal
    (Type.Variable.convert_all_escaped_free_variables_to_anys annotation)
    (Type.parametric "p" ![free_variable_tuple; Type.Tuple (Bounded Any)]);
  let free_variable_star_args_callable =
    let list_variadic = Type.Variable.Variadic.List.create "Ts" in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let escaped_free_star_args_tuple =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let list_variadic =
      Type.Variable.Variadic.List.create "Ts"
      |> Type.Variable.Variadic.List.mark_as_escaped
      |> Type.Variable.Variadic.List.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic)) ])
      ~annotation:Type.integer
      ()
  in
  let star_args_any_callable =
    Type.Callable.create
      ~parameters:
        (Defined
           [ Anonymous { index = 0; annotation = Type.bool; default = false };
             Variable (Concrete Type.Any) ])
      ~annotation:Type.integer
      ()
  in
  let annotation =
    Type.parametric "p" ![free_variable_star_args_callable; escaped_free_star_args_tuple]
  in
  assert_equal
    (Type.Variable.convert_all_escaped_free_variables_to_anys annotation)
    (Type.parametric "p" ![free_variable_star_args_callable; star_args_any_callable]);
  ()


let test_replace_all _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all (fun _ -> Some Type.integer) annotation)
    (Type.parametric "p" ![Type.integer; Type.integer]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  let no_parameter_callable =
    Type.Callable.create ~parameters:(Type.Callable.Defined []) ~annotation:Type.integer ()
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ParameterVariadic.replace_all
       (fun _ -> Some (Type.Callable.Defined []))
       (Type.parametric "p" ![Type.integer; free_variable_callable]))
    (Type.parametric "p" ![Type.integer; no_parameter_callable]);
  let list_variadic = Type.Variable.Variadic.List.create "Ts" in
  let free_variable_tuple =
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Type.parametric "p" ![Type.integer; free_variable_tuple]))
    (Type.parametric
       "p"
       ![Type.integer; Type.Tuple (Bounded (Concrete [Type.integer; Type.string]))]);
  let replaced =
    Type.Callable.Parameter.create
      [ { name = "__x"; annotation = Type.bool; default = false };
        { name = "__a"; annotation = Type.integer; default = false };
        { name = "__b"; annotation = Type.string; default = false } ]
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Type.Callable.create
          ~parameters:
            (Defined
               [ Anonymous { index = 0; annotation = Type.bool; default = false };
                 Variable (Concatenation (create_concatenation list_variadic)) ])
          ~annotation:Type.integer
          ()))
    (Type.Callable.create ~parameters:(Defined replaced) ~annotation:Type.integer ());
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Tuple
          (Bounded (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic)))))
    (Tuple
       (Bounded
          (Concrete
             [ Parametric
                 {
                   name = "Foo";
                   parameters = ![Type.Parametric { name = "Bar"; parameters = ![Type.integer] }];
                 };
               Parametric
                 {
                   name = "Foo";
                   parameters = ![Type.Parametric { name = "Bar"; parameters = ![Type.string] }];
                 } ])));
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Type.parametric
          "Baz"
          (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")))))
    (Type.parametric "Baz" ![Type.integer; Type.string]);
  ()


let test_collect_all _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.collect_all annotation)
    [Type.Variable.Unary.create "T"];
  let free_variable_callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameter_variadic)
      ~annotation:Type.integer
      ()
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ParameterVariadic.collect_all
       (Type.parametric "p" ![Type.integer; free_variable_callable]))
    [Type.Variable.Variadic.Parameters.create "T"];
  let list_variadic = Type.Variable.Variadic.List.create "Ts" in
  let free_variable_tuple =
    Type.Tuple (Bounded (Concatenation (create_concatenation list_variadic)))
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Type.parametric "p" ![Type.integer; free_variable_tuple]))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Type.Callable.create
          ~parameters:
            (Defined
               [ Anonymous { index = 0; annotation = Type.bool; default = false };
                 Variable (Concatenation (create_concatenation list_variadic)) ])
          ~annotation:Type.integer
          ()))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Tuple
          (Bounded (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic)))))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Type.parametric
          "Huh"
          (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic))))
    [Type.Variable.Variadic.List.create "Ts"];
  ()


let test_parse_type_variable_declarations _ =
  let assert_parses_declaration expression expected =
    assert_equal
      (Some expected)
      (Type.Variable.parse_declaration (parse_single_expression expression))
  in
  let assert_declaration_does_not_parse expression =
    assert_equal None (Type.Variable.parse_declaration (parse_single_expression expression))
  in
  assert_parses_declaration
    "pyre_extensions.ParameterSpecification('Tparams')"
    (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "Tparams"));
  assert_declaration_does_not_parse "pyre_extensions.ParameterSpecification('Tparams', int, str)";
  assert_parses_declaration
    "pyre_extensions.ListVariadic('Ts')"
    (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "Ts"));
  assert_declaration_does_not_parse "pyre_extensions.ListVariadic('Ts', int, str)";
  ()


let test_middle_singleton_replace_variable _ =
  let assert_replaces_into ~middle ~replacement expected =
    assert_equal
      (Type.OrderedTypes.Concatenation.Middle.singleton_replace_variable middle ~replacement)
      expected
  in
  let variable = Type.Variable.Variadic.List.create "Ts" in
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:Type.integer
    (Type.Parametric
       {
         name = "Foo";
         parameters = Concrete [Parametric { name = "Bar"; parameters = Concrete [Type.integer] }];
       });

  (* This approach is used to solve concretes against maps *)
  let unary_variable = Type.Variable.Unary.create "T" in
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(Type.Variable unary_variable)
    (Type.Parametric
       {
         name = "Foo";
         parameters =
           Concrete
             [Parametric { name = "Bar"; parameters = Concrete [Type.Variable unary_variable] }];
       });
  ()


let test_union_upper_bound _ =
  let assert_union_upper_bound map expected =
    assert_equal (Type.OrderedTypes.union_upper_bound map) expected
  in
  assert_union_upper_bound
    (Concrete [Type.integer; Type.string; Type.bool])
    (Type.union [Type.integer; Type.string; Type.bool]);

  assert_union_upper_bound Any Any;

  let variable = Type.Variable.Variadic.List.create "Ts" in
  assert_union_upper_bound (Concatenation (create_concatenation variable)) Type.object_primitive;

  assert_union_upper_bound
    (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] variable))
    Type.object_primitive;
  ()


let test_concatenation_operator_variable _ =
  let variable = Type.Variable.Variadic.List.create "Ts" in
  assert_equal
    (Type.OrderedTypes.Concatenation.variable
       (create_concatenation ~head:[Type.integer] ~tail:[Type.bool] variable))
    variable;
  assert_equal
    (Type.OrderedTypes.Concatenation.variable
       (create_concatenation ~head:[Type.integer] ~tail:[Type.bool] ~mappers:["list"] variable))
    variable;
  ()


let test_concatenation_operator_replace_variable _ =
  let assert_replaces_into ~concatenation ~replacement expected =
    assert_equal
      (Type.OrderedTypes.Concatenation.replace_variable concatenation ~replacement)
      expected
  in
  let variable = Type.Variable.Variadic.List.create "Ts" in
  assert_replaces_into
    ~concatenation:(create_concatenation ~head:[Type.integer] ~tail:[Type.bool] variable)
    ~replacement:(fun _ -> Some (Concrete [Type.string]))
    (Some (Concrete [Type.integer; Type.string; Type.bool]));
  let assert_replaces_into ~middle ~replacement expected =
    let concatenation = Type.OrderedTypes.Concatenation.create ?head:None ?tail:None middle in
    assert_replaces_into ~concatenation ~replacement expected
  in
  let variable = Type.Variable.Variadic.List.create "Ts" in
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(fun _ -> Some (Concrete [Type.integer]))
    (Some
       (Concrete
          [ Parametric
              {
                name = "Foo";
                parameters =
                  Concrete [Parametric { name = "Bar"; parameters = Concrete [Type.integer] }];
              } ]));
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(fun _ -> Some (Concrete [Type.integer; Type.string]))
    (Some
       (Concrete
          [ Parametric
              {
                name = "Foo";
                parameters =
                  Concrete [Parametric { name = "Bar"; parameters = Concrete [Type.integer] }];
              };
            Parametric
              {
                name = "Foo";
                parameters = ![Type.Parametric { name = "Bar"; parameters = ![Type.string] }];
              } ]));
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"] ~variable)
    ~replacement:(fun _ -> Some (Concatenation (create_concatenation ~mappers:["Bar"] variable)))
    (Some (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] variable)));
  let other_variable = Type.Variable.Variadic.List.create "Ts2" in
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(function
      | _ -> Some (Concatenation (create_concatenation other_variable)))
    (Some (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] other_variable)));
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(function
      | _ -> Some Any)
    (Some Any);
  assert_replaces_into
    ~middle:(Type.OrderedTypes.Concatenation.Middle.create ~mappers:["Foo"; "Bar"] ~variable)
    ~replacement:(function
      | _ -> None)
    None;
  ()


let test_concatenation_zip _ =
  let assert_zips ~concatenation ~against expected =
    assert_equal (Type.OrderedTypes.Concatenation.zip concatenation ~against) expected
  in
  let concatenation = Type.OrderedTypes.Concatenation.create ~head:[1; 2] ~tail:[3; 4] "M" in
  assert_zips
    ~concatenation
    ~against:["A"; "B"; "C"; "D"; "E"]
    (Some
       (Type.OrderedTypes.Concatenation.create
          ~head:[1, "A"; 2, "B"]
          ~tail:[3, "D"; 4, "E"]
          ("M", ["C"])));
  assert_zips
    ~concatenation
    ~against:["A"; "B"; "C"; "D"; "E"; "F"]
    (Some
       (Type.OrderedTypes.Concatenation.create
          ~head:[1, "A"; 2, "B"]
          ~tail:[3, "E"; 4, "F"]
          ("M", ["C"; "D"])));
  assert_zips ~concatenation ~against:["A"; "B"; "C"] None;
  ()


let () =
  "type"
  >::: [ "create" >:: test_create;
         "instantiate" >:: test_instantiate;
         "expression" >:: test_expression;
         "concise" >:: test_concise;
         "union" >:: test_union;
         "primitives" >:: test_primitives;
         "elements" >:: test_elements;
         "exists" >:: test_exists;
         "is_async_generator" >:: test_is_generator;
         "contains_callable" >:: test_contains_callable;
         "contains_any" >:: test_contains_any;
         "is_concrete" >:: test_is_concrete;
         "is_not_instantiated" >:: test_is_not_instantiated;
         "is_meta" >:: test_is_meta;
         "is_none" >:: test_is_none;
         "is_type_alias" >:: test_is_type_alias;
         "is_unknown" >:: test_is_unknown;
         "is_resolved" >:: test_is_resolved;
         "is_iterator" >:: test_is_iterator;
         "class_name" >:: test_class_name;
         "optional_value" >:: test_optional_value;
         "async_generator_value" >:: test_async_generator_value;
         "dequalify" >:: test_dequalify;
         "variables" >:: test_variables;
         "lambda" >:: test_lambda;
         "visit" >:: test_visit;
         "collapse_escaped_variable_unions" >:: test_collapse_escaped_variable_unions;
         "namespace_insensitive_compare" >:: test_namespace_insensitive_compare;
         "namespace" >:: test_namespace;
         "mark_all_variables_as_bound" >:: test_mark_all_variables_as_bound;
         "namespace_all_free_variables" >:: test_namespace_all_free_variables;
         "mark_all_free_variables_as_escaped" >:: test_mark_all_free_variables_as_escaped;
         "contains_escaped_free_variable" >:: test_contains_escaped_free_variable;
         "convert_all_escaped_free_variables_to_anys"
         >:: test_convert_all_escaped_free_variables_to_anys;
         "replace_all" >:: test_replace_all;
         "collect_all" >:: test_collect_all;
         "parse_type_variable_declarations" >:: test_parse_type_variable_declarations;
         "map_operator_singleton_replace_variable" >:: test_middle_singleton_replace_variable;
         "union_upper_bound" >:: test_union_upper_bound;
         "concatenation_operator_variable" >:: test_concatenation_operator_variable;
         "concatenation_operator_replace_variable" >:: test_concatenation_operator_replace_variable;
         "concatenation_zip" >:: test_concatenation_zip ]
  |> Test.run;
  "callable"
  >::: [ "from_overloads" >:: test_from_overloads;
         "with_return_annotation" >:: test_with_return_annotation;
         "overload_parameters" >:: test_overload_parameters ]
  |> Test.run
