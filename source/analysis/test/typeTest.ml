(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let empty_head variable = { Type.Callable.head = []; variable }

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

let create_concatenation ?head ?tail ?mappers variable
    : (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
  =
  let mappers = Option.value mappers ~default:[] in
  Type.OrderedTypes.Concatenation.create
    ?head
    ?tail
    (Type.OrderedTypes.Concatenation.Middle.create
       ~mappers:
         (List.map
            ~f:(fun name -> Type.Record.OrderedTypes.RecordConcatenate.Middle.ClassMapper name)
            mappers)
       ~variable)


let create_concatenation_with_generic_alias_mapper ?head ?tail ?mappers variable
    : (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
  =
  let mappers = Option.value mappers ~default:[] in
  Type.OrderedTypes.Concatenation.create
    ?head
    ?tail
    (Type.OrderedTypes.Concatenation.Middle.create
       ~mappers:
         (List.map
            ~f:(fun annotation ->
              Type.Record.OrderedTypes.RecordConcatenate.Middle.GenericAliasMapper annotation)
            mappers)
       ~variable)


let test_create _ =
  let assert_create ?(aliases = fun _ -> None) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create
         ~aliases:(fun ?replace_unbound_parameters_with_any:_ -> aliases)
         (parse_single_expression ~preprocess:true source))
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
  let unary_variable = Type.Variable.Unary.create "T" in
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[Ts]"
    (Type.parametric "foo" [Group (Type.Variable.Variadic.List.self_reference variadic)]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    (Type.parametric
       "foo"
       [Group (Concatenation (create_concatenation ~mappers:["list"] variadic))]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | "T" -> Some (TypeAlias (Type.Variable unary_variable))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Map[typing.Union[T, int], Ts]]"
    (Type.parametric
       "foo"
       [
         Group
           (Concatenation
              (create_concatenation_with_generic_alias_mapper
                 ~mappers:[Type.union [Type.Variable unary_variable; Type.integer]]
                 variadic));
       ]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, bool, Ts]]"
    (Type.parametric
       "foo"
       [Group (Concatenation (create_concatenation ~head:[Type.integer; Type.bool] variadic))]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[Ts, int, bool]]"
    (Type.parametric
       "foo"
       [Group (Concatenation (create_concatenation ~tail:[Type.integer; Type.bool] variadic))]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, Ts, bool]]"
    (Type.parametric
       "foo"
       [
         Group (Concatenation (create_concatenation ~head:[Type.integer] ~tail:[Type.bool] variadic));
       ]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[pyre_extensions.type_variable_operators.Concatenate[int, \
     pyre_extensions.type_variable_operators.Map[list, Ts], bool]]"
    (Type.parametric
       "foo"
       [
         Group
           (Concatenation
              (create_concatenation
                 ~head:[Type.integer]
                 ~tail:[Type.bool]
                 ~mappers:["list"]
                 variadic));
       ]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.ListVariadic variadic))
      | _ -> None)
    "foo[...]"
    (Type.parametric "foo" [Group Any]);
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
  assert_create "typing.Set" (Primitive "set");
  assert_create
    "typing.DefaultDict[int, str]"
    (Type.parametric "collections.defaultdict" ![Type.integer; Type.string]);
  assert_create "typing.Dict[int, str]" (Type.dictionary ~key:Type.integer ~value:Type.string);
  assert_create "typing.Tuple[int, str]" (Type.tuple [Type.integer; Type.string]);
  assert_create "typing.Tuple[int, ...]" (Type.Tuple (Type.Unbounded Type.integer));
  assert_create "typing.Tuple[()]" (Type.tuple []);
  assert_create "tuple" (Type.Primitive "tuple");
  assert_create "typing.Any" Type.Any;
  assert_create "typing.Optional[int]" (Type.optional Type.integer);
  assert_create "typing.Optional.__getitem__(int)" (Type.optional Type.integer);
  assert_create "typing.Set[int]" (Type.set Type.integer);
  assert_create "typing.Union[int, str]" (Type.union [Type.integer; Type.string]);
  assert_create "typing.Union[int, typing.Any]" (Type.union [Type.integer; Type.Any]);
  assert_create "typing.Union[int, typing.Optional[$bottom]]" Type.integer;
  assert_create "typing.Union[int, None]" (Type.optional Type.integer);
  assert_create
    "typing.Union[int, None, str, typing.Tuple[int, str]]"
    (Type.union [Type.NoneType; Type.integer; Type.string; Type.tuple [Type.integer; Type.string]]);
  assert_create
    "typing.Union[typing.Optional[int], typing.Optional[str]]"
    (Type.union [Type.NoneType; Type.integer; Type.string]);
  assert_create
    "typing.Union[typing.Optional[int], str]"
    (Type.union [Type.NoneType; Type.integer; Type.string]);

  (* Annotated. *)
  assert_create "typing.Annotated[int]" (Type.annotated Type.integer);
  assert_create "typing.Annotated[int, Derp()]" (Type.annotated Type.integer);

  assert_create "typing_extensions.Annotated[int]" (Type.annotated Type.integer);
  assert_create "typing_extensions.Annotated[int, Derp()]" (Type.annotated Type.integer);

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
       ~constraints:(Type.Variable.Bound (Type.Primitive "typing.Callable"))
       "_CallableT");

  (* Check that type aliases are resolved. *)
  let assert_alias source resolved =
    let aliases primitive =
      Identifier.Table.of_alist_exn
        [
          "Alias", Type.Primitive "Aliased";
          "IntList", Type.list Type.integer;
          ( "_Future",
            Type.union
              [
                Type.parametric "Future" ![Type.integer; Type.variable "_T"];
                Type.awaitable (Type.variable "_T");
              ] );
        ]
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

  assert_alias "IntList" (Type.list Type.integer);
  assert_alias "IntList[str]" (Type.list Type.integer);

  assert_alias
    "_Future[int]"
    (Type.union
       [Type.parametric "Future" ![Type.integer; Type.integer]; Type.awaitable Type.integer]);

  (* String literals. *)
  assert_create "'foo'" (Type.Primitive "foo");
  assert_create "'foo.bar'" (Type.Primitive "foo.bar");
  assert_create "foo['bar']" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create "'Type[str]'" (Type.parametric "Type" ![Type.Primitive "str"]);
  assert_create "'Type[[[]str]'" (Type.Primitive "Type[[[]str]");

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
  let default_overload = { Type.Callable.annotation = Type.Top; parameters = Undefined } in
  let open Type.Callable in
  assert_create "typing.Callable" (Type.Primitive "typing.Callable");
  assert_create "typing.Callable[..., int]" (Type.Callable.create ~annotation:Type.integer ());
  assert_create
    "typing.Callable.__getitem__((..., int))"
    (Type.Callable.create ~annotation:Type.integer ());
  assert_create
    "typing.Callable[(..., int)].__getitem__(__getitem__((..., str))[(..., int)])"
    (Type.Callable.create
       ~overloads:
         [
           { Type.Callable.annotation = Type.string; parameters = Type.Callable.Undefined };
           { Type.Callable.annotation = Type.integer; parameters = Type.Callable.Undefined };
         ]
       ~annotation:Type.integer
       ());
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
       });
  assert_create
    "typing.Callable('foo')[..., $unknown]"
    (Type.Callable
       { kind = Type.Callable.Named !&"foo"; implementation = default_overload; overloads = [] });
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
                 [
                   Parameter.PositionalOnly { index = 0; annotation = Type.integer; default = false };
                   Parameter.PositionalOnly { index = 1; annotation = Type.string; default = false };
                 ];
           };
         overloads = [];
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
                 [
                   Parameter.PositionalOnly { index = 0; annotation = Type.integer; default = false };
                   Parameter.Named { name = "a"; annotation = Type.integer; default = false };
                   Parameter.Variable (Concrete Type.Top);
                   Parameter.Keywords Type.Top;
                 ];
           };
         overloads = [];
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
                 [
                   Parameter.PositionalOnly { index = 0; annotation = Type.integer; default = false };
                   Parameter.Variable (Concrete Type.integer);
                   Parameter.Keywords Type.string;
                 ];
           };
         overloads = [];
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
           };
         overloads = [];
       });
  assert_create "typing.Callable[int]" (Type.Callable.create ~annotation:Type.Top ());
  assert_create "typing.Callable[int, str]" (Type.Callable.create ~annotation:Type.Top ());
  assert_create "function" (Type.Callable.create ~annotation:Type.Any ());
  assert_create
    "typing.Callable[..., function]"
    (Type.Callable.create ~annotation:(Type.Callable.create ~annotation:Type.Any ()) ());
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
    "typing.Callable[Ts, int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")));
            ])
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
            [
              PositionalOnly { index = 0; annotation = Type.integer; default = false };
              Variable
                (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")));
            ])
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

  assert_create "typing_extensions.Literal['foo']" (Type.literal_string "foo");
  assert_create "typing_extensions.Literal[u'foo']" (Type.literal_string "foo");
  assert_create "typing_extensions.Literal[b'foo']" (Type.literal_bytes "foo");
  assert_create
    "typing_extensions.Literal[u'foo', b'foo']"
    (Type.union [Type.literal_string "foo"; Type.literal_bytes "foo"]);
  assert_create
    "typing_extensions.Literal[Foo.ONE]"
    (Type.Literal
       (Type.EnumerationMember { enumeration_type = Type.Primitive "Foo"; member_name = "ONE" }));
  assert_create
    "typing_extensions.Literal[Foo.ONE, Foo.TWO]"
    (Type.union
       [
         Type.Literal
           (Type.EnumerationMember { enumeration_type = Type.Primitive "Foo"; member_name = "ONE" });
         Type.Literal
           (Type.EnumerationMember { enumeration_type = Type.Primitive "Foo"; member_name = "TWO" });
       ]);
  assert_create "typing_extensions.Literal[ONE]" Type.Top;
  assert_create "typing_extensions.Literal[None]" Type.none;
  assert_create "_NotImplementedType" Type.Any;
  ()


let test_resolve_aliases _ =
  let assert_resolved ~aliases annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = aliases in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      (Type.resolve_aliases ~aliases annotation)
  in
  let aliases = function
    | "MyInt" -> Some (Type.TypeAlias Type.integer)
    | "IntList" -> Some (Type.TypeAlias (Type.list Type.integer))
    | _ -> None
  in
  assert_resolved ~aliases (Type.Primitive "NotAlias") (Type.Primitive "NotAlias");
  assert_resolved ~aliases (Type.Primitive "MyInt") Type.integer;
  assert_resolved ~aliases (Type.Primitive "IntList") (Type.list Type.integer);
  (* `IntList` resolves to `List[int]`. So, it ignores the `str` argument. *)
  assert_resolved ~aliases (Type.parametric "IntList" [Single Type.string]) (Type.list Type.integer);

  let variable_t = Type.Variable (Type.Variable.Unary.create "T") in
  let variable_k = Type.Variable (Type.Variable.Unary.create "K") in
  let variable_v = Type.Variable (Type.Variable.Unary.create "V") in
  let aliases = function
    | "IntList" -> Some (Type.TypeAlias (Type.list Type.integer))
    | "foo.Optional" -> Some (Type.TypeAlias (Type.optional variable_t))
    | "foo.Dict" -> Some (Type.TypeAlias (Type.dictionary ~key:variable_k ~value:variable_v))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric
       "foo.Optional"
       [Single (Type.parametric "foo.Dict" [Single Type.string; Single Type.integer])])
    (Type.optional (Type.dictionary ~key:Type.string ~value:Type.integer));

  let tree_body = Type.union [Type.integer; Type.list (Type.Primitive "Tree")] in
  let aliases ?replace_unbound_parameters_with_any:_ name =
    match name with
    | "Tree" -> Some (Type.TypeAlias (Type.RecursiveType { name = "Tree"; body = tree_body }))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.Primitive "Tree")
    (Type.RecursiveType { name = "Tree"; body = tree_body });
  (* Don't resolve the `Tree` reference within the body. *)
  assert_resolved
    ~aliases
    (Type.RecursiveType { name = "Tree"; body = tree_body })
    (Type.RecursiveType { name = "Tree"; body = tree_body });
  assert_resolved
    ~aliases
    (Type.list (Type.Primitive "Tree"))
    (Type.list (Type.RecursiveType { name = "Tree"; body = tree_body }));
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
    [Type.variable "_T", Type.integer; Type.variable "_VT", Type.NoneType]
    ~generic:(Type.Union [Type.variable "_T"; Type.variable "_VT"])
    ~expected:(Type.optional Type.integer)


let test_expression _ =
  let assert_expression annotation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:(fun left right -> Expression.location_insensitive_compare left right = 0)
      (parse_single_expression ~coerce_special_methods:true expression)
      (Type.expression annotation)
  in
  assert_expression (Type.Primitive "foo") "foo";
  assert_expression (Type.Primitive "...") "...";
  assert_expression (Type.Primitive "foo.bar") "foo.bar";
  assert_expression Type.Top "$unknown";
  assert_expression (Type.parametric "foo.bar" ![Type.Primitive "baz"]) "foo.bar.__getitem__(baz)";
  assert_expression
    (Type.Tuple (Type.Bounded (Type.OrderedTypes.Concrete [Type.integer; Type.string])))
    "typing.Tuple.__getitem__((int, str))";
  assert_expression
    (Type.Tuple (Type.Unbounded Type.integer))
    "typing.Tuple.__getitem__((int, ...))";
  assert_expression (Type.parametric "list" ![Type.integer]) "typing.List.__getitem__(int)";
  assert_expression
    (Type.parametric
       "foo.Variadic"
       [Group (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")))])
    "foo.Variadic.__getitem__(Ts)";
  assert_expression (Type.parametric "foo.Variadic" [Group Any]) "foo.Variadic.__getitem__(...)";
  assert_expression
    (Type.parametric
       "foo.Variadic"
       [
         Group
           (Concatenation
              (create_concatenation ~mappers:["Foo"] (Type.Variable.Variadic.List.create "Ts")));
       ])
    "foo.Variadic.__getitem__(pyre_extensions.type_variable_operators.Map.__getitem__((Foo, Ts)))";
  assert_expression
    (Type.parametric
       "foo.SomeVariadic"
       [
         Group
           (Concatenation
              (create_concatenation_with_generic_alias_mapper
                 ~mappers:[Type.list (Type.Variable (Type.Variable.Unary.create "T"))]
                 (Type.Variable.Variadic.List.create "Ts")));
       ])
    "foo.SomeVariadic.__getitem__(pyre_extensions.type_variable_operators.Map.__getitem__((typing.List.__getitem__((T)), \
     Ts)))";

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
         [
           { Type.Callable.annotation = Type.string; parameters = Type.Callable.Undefined };
           { Type.Callable.annotation = Type.integer; parameters = Type.Callable.Undefined };
         ]
       ~annotation:Type.integer
       ())
    "typing.Callable[(..., int)].__getitem__(__getitem__((..., str))[(..., int)])";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [
              Parameter.Named { name = "__0"; annotation = Type.integer; default = false };
              Parameter.Named { name = "__1"; annotation = Type.string; default = false };
            ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Named(__0, int), Named(__1, str)], int))";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [
              Parameter.Named { name = "a"; annotation = Type.integer; default = false };
              Parameter.Named { name = "b"; annotation = Type.string; default = false };
            ])
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
            [
              Parameter.Named { name = "$0"; annotation = Type.integer; default = false };
              Parameter.Variable (Concrete Type.integer);
              Parameter.Keywords Type.string;
            ])
       ~annotation:Type.integer
       ())
    ("typing.Callable.__getitem__(([Named($0, int), Variable(int), " ^ "Keywords(str)], int))");
  assert_expression
    (Type.parametric
       "G"
       [
         CallableParameters
           (Type.Variable.Variadic.Parameters.self_reference
              (Type.Variable.Variadic.Parameters.create "TParams"));
       ])
    "G[TParams]";
  assert_expression
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    "typing_extensions.Literal[test.MyEnum.ONE]";
  ()


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
            [
              Type.Callable.Parameter.Named { name = "x"; annotation = Type.Any; default = false };
              Type.Callable.Parameter.Named { name = "y"; annotation = Type.float; default = false };
            ])
       ())
    "(x: Any, y: float) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [
              Type.Callable.Parameter.PositionalOnly
                { index = 0; annotation = Type.Any; default = true };
            ])
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
            [
              Type.Callable.Parameter.Named
                {
                  name = "callable";
                  default = false;
                  annotation =
                    Type.Callable.create
                      ~name:!&"bar"
                      ~annotation:Type.float
                      ~parameters:
                        (Type.Callable.Defined
                           [
                             Type.Callable.Parameter.Named
                               { name = "x"; annotation = Type.integer; default = false };
                           ])
                      ();
                };
            ])
       ())
    "(callable: (x: int) -> float) -> int";
  assert_concise Type.Any "Any";
  assert_concise Type.NoneType "None";
  assert_concise (Type.optional Type.integer) "Optional[int]";
  assert_concise (Type.parametric "parametric" ![Type.Top; Type.Top]) "parametric[]";
  assert_concise (Type.parametric "parametric" ![Type.Top; Type.float]) "parametric[unknown, float]";
  assert_concise (Type.Primitive "a.b.c") "c";
  assert_concise (Type.tuple [Type.integer; Type.Any]) "Tuple[int, Any]";
  assert_concise (Type.Tuple (Type.Unbounded Type.integer)) "Tuple[int, ...]";
  assert_concise (Type.union [Type.integer; Type.string]) "Union[int, str]";
  assert_concise (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T") "T";
  assert_concise
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    "typing_extensions.Literal[test.MyEnum.ONE]";
  ()


let test_weaken_literals _ =
  let assert_weakened_literal literal expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected (Type.weaken_literals literal)
  in
  assert_weakened_literal (Type.literal_integer 1) Type.integer;
  assert_weakened_literal (Type.literal_string "foo") Type.string;
  assert_weakened_literal (Type.literal_bytes "foo") Type.bytes;
  assert_weakened_literal (Type.Literal (Type.Boolean true)) Type.bool;
  assert_weakened_literal
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    (Type.Primitive "test.MyEnum");
  assert_weakened_literal (Type.list (Type.literal_integer 1)) (Type.list Type.integer);
  ()


let test_union _ =
  let assert_union arguments expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected (Type.union arguments)
  in
  assert_union
    [Type.string; Type.optional (Type.Union [Type.integer; Type.string])]
    (Type.Union [Type.none; Type.integer; Type.string]);
  assert_union [Type.string; Type.float] (Type.Union [Type.float; Type.string]);
  assert_union [Type.float; Type.string] (Type.Union [Type.float; Type.string]);
  assert_union
    [Type.optional Type.string; Type.float]
    (Type.Union [Type.NoneType; Type.float; Type.string]);
  assert_union
    [Type.float; Type.string; Type.optional Type.float]
    (Type.Union [Type.NoneType; Type.float; Type.string]);
  assert_union [Type.float; Type.Any] Type.Any;
  assert_union [Type.float; Type.Top] Type.Top;
  assert_union [Type.string; Type.float] (Type.Union [Type.float; Type.string]);
  assert_union [Type.float; Type.string] (Type.Union [Type.float; Type.string]);
  assert_union [Type.float] Type.float;
  assert_union [Type.float; Type.Bottom] Type.float;
  assert_union [Type.Bottom; Type.Bottom] Type.Bottom;

  (* Flatten unions. *)
  assert_union
    [Type.float; Type.union [Type.string; Type.bytes]]
    (Type.Union [Type.bytes; Type.float; Type.string]);
  assert_union
    [Type.optional (Type.list Type.integer); Type.list Type.integer]
    (Type.optional (Type.list Type.integer));
  assert_union
    [Type.optional (Type.variable "A"); Type.variable "A"]
    (Type.optional (Type.variable "A"));
  assert_union
    [Type.string; Type.optional (Type.Union [Type.integer; Type.string])]
    (Type.Union [Type.NoneType; Type.integer; Type.string]);
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
  ()


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
    ["int"; "tuple"]
    (Type.elements
       (Type.RecursiveType
          { name = "Tree"; body = Type.tuple [Type.integer; Type.Primitive "Tree"] }));
  ()


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
  assert_false (top_exists (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "T"));
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
    (Type.contains_callable (Type.optional (Type.Callable.create ~annotation:Type.integer ())));
  assert_true
    (Type.contains_callable
       (Type.union [Type.string; Type.Callable.create ~annotation:Type.integer ()]));
  assert_false (Type.contains_callable (Type.Primitive "foo"))


let test_map_callable_annotation _ =
  let assert_mapped ~f callable expected =
    assert_equal
      ~cmp:Type.Callable.equal
      ~printer:Type.Callable.show
      expected
      (Type.Callable.map_annotation ~f callable)
  in
  let callable =
    {
      Type.Record.Callable.kind = Type.Record.Callable.Named (Reference.create "foo");
      implementation =
        {
          Type.Record.Callable.annotation = Type.union [Type.string; Type.integer];
          parameters = Type.Record.Callable.Defined [];
        };
      overloads =
        [
          {
            Type.Record.Callable.annotation = Type.string;
            parameters = Type.Record.Callable.Defined [];
          };
        ];
    }
  in
  let mapped_callable =
    {
      Type.Record.Callable.kind = Type.Record.Callable.Named (Reference.create "foo");
      implementation =
        {
          Type.Record.Callable.annotation = Type.bool;
          parameters = Type.Record.Callable.Defined [];
        };
      overloads =
        [
          {
            Type.Record.Callable.annotation = Type.bool;
            parameters = Type.Record.Callable.Defined [];
          };
        ];
    }
  in
  assert_mapped ~f:(fun _ -> Type.bool) callable mapped_callable;
  ()


let test_type_parameters_for_bounded_tuple_union _ =
  let assert_type_parameters actual expected =
    assert_equal
      ~cmp:[%equal: Type.t list option]
      ~printer:[%show: Type.t list option]
      expected
      (Type.type_parameters_for_bounded_tuple_union actual)
  in
  assert_type_parameters Type.integer None;
  assert_type_parameters
    (Type.union [Type.tuple [Type.integer; Type.string]; Type.tuple [Type.bool]])
    None;
  assert_type_parameters
    (Type.union
       [Type.tuple [Type.integer; Type.string]; Type.tuple [Type.bool; Type.list Type.integer]])
    (Some [Type.union [Type.integer; Type.bool]; Type.union [Type.string; Type.list Type.integer]]);
  ()


let test_contains_any _ = assert_true (Type.contains_any Type.Any)

let test_expression_contains_any _ =
  assert_true
    (Type.expression_contains_any
       (parse_single_expression ~preprocess:true "typing.Dict[typing.Any, typing.Any]"));
  assert_false (Type.expression_contains_any (parse_single_expression ~preprocess:true "dict"));
  assert_false
    (Type.expression_contains_any (parse_single_expression ~preprocess:true "typing.Type"));
  assert_false
    (Type.expression_contains_any (parse_single_expression ~preprocess:true "typing.Callable"));
  assert_false
    (Type.expression_contains_any (parse_single_expression ~preprocess:true "typing.Tuple"));
  assert_true
    (Type.expression_contains_any
       (parse_single_expression ~preprocess:true "typing.Union[typing.Any, None]"));
  assert_false
    (Type.expression_contains_any
       (parse_single_expression ~preprocess:true "typing.Union[typing.Callable]"));
  assert_false
    (Type.expression_contains_any
       (parse_single_expression
          ~preprocess:true
          "pyre_extensions.type_variable_operators.Map[typing.List, int]"));
  assert_false
    (Type.expression_contains_any
       (parse_single_expression
          ~preprocess:true
          "foo[pyre_extensions.type_variable_operators.Concatenate[int, bool, Ts]]"));
  ()


let test_is_concrete _ =
  assert_true (Type.is_concrete Type.none);
  assert_true (Type.is_concrete (Type.parametric "typing.Optional" ![Type.Bottom]));
  assert_true (Type.is_concrete (Type.Callable.create ~annotation:Type.none ()));
  assert_false (Type.is_concrete (Type.Callable.create ~annotation:(Type.list Type.Bottom) ()));
  ()


let test_is_not_instantiated _ =
  assert_true (Type.is_not_instantiated Type.Bottom);
  assert_true (Type.is_not_instantiated (Type.dictionary ~key:Type.Bottom ~value:Type.Bottom));
  assert_false (Type.is_not_instantiated Type.NoneType);
  assert_false (Type.is_not_instantiated Type.Top);
  assert_true (Type.is_not_instantiated (Type.variable "_T"))


let test_is_meta _ =
  assert_true (Type.is_meta (Type.parametric "type" ![Type.integer]));
  assert_false (Type.is_meta Type.integer);
  assert_false (Type.is_meta (Type.parametric "typing.Type" ![Type.integer]))


let test_is_none _ =
  assert_false (Type.is_none (Type.Primitive "None"));
  assert_false (Type.is_none Type.integer);
  assert_false (Type.is_none (Type.Primitive "foo"));
  assert_true (Type.is_none Type.NoneType)


let test_is_type_alias _ =
  assert_true (Type.is_type_alias (Type.Primitive "typing.TypeAlias"));
  assert_false (Type.is_type_alias (Type.parametric "typing.TypeAlias" ![Type.Top]))


let test_unfold_recursive_type _ =
  let assert_unfolded recursive_type expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected
      (Type.RecursiveType.unfold_recursive_type recursive_type)
  in
  let tree_name, tree_body =
    "Tree", Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; Type.Primitive "Tree"]]
  in
  let tree_annotation = Type.RecursiveType { name = tree_name; body = tree_body } in
  assert_unfolded
    { name = tree_name; body = tree_body }
    (Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; tree_annotation]]);
  ()


let test_contains_unknown _ =
  assert_false (Type.contains_unknown Type.Bottom);
  assert_false (Type.contains_unknown Type.Any);
  assert_true (Type.contains_unknown (Type.optional Type.Top));
  assert_false (Type.contains_unknown (Type.optional Type.integer));
  assert_true
    (Type.contains_unknown (Type.optional (Type.parametric "foo" ![Type.integer; Type.Top])));
  assert_true (Type.contains_unknown (Type.parametric "foo" ![Type.integer; Type.Top]));
  assert_false (Type.contains_unknown (Type.parametric "foo" ![Type.integer]));
  assert_false (Type.contains_unknown Type.integer);
  assert_true (Type.contains_unknown Type.Top);
  assert_true (Type.contains_unknown (Type.Union [Type.integer; Type.Top]));
  assert_false (Type.contains_unknown (Type.Union [Type.integer; Type.string]));
  assert_false (Type.contains_unknown (Type.variable "derp"));
  assert_true
    (Type.contains_unknown (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.Top]))));
  assert_false
    (Type.contains_unknown (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.string]))));
  assert_true (Type.contains_unknown (Type.Tuple (Type.Unbounded Type.Top)));
  assert_false (Type.contains_unknown (Type.Tuple (Type.Unbounded Type.integer)))


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
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
          ~annotation:Type.integer
          ()));
  let parameter_variadic = parameter_variadic |> Type.Variable.Variadic.Parameters.mark_as_bound in
  assert_true
    (Type.Variable.all_variables_are_resolved
       (Type.Callable.create
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
    (Option.value_exn
       (Type.optional_value (Type.optional (Type.parametric "foo" ![Type.integer; Type.Top]))))
    (Type.parametric "foo" ![Type.integer; Type.Top]);
  assert_true
    (Option.is_none (Type.optional_value (Type.parametric "foo" ![Type.integer; Type.Top])))


let test_async_generator_value _ =
  assert_equal
    ~printer:(Format.asprintf "%a" Type.pp)
    (Option.value_exn
       (Type.async_generator_value
          (Type.parametric "typing.AsyncGenerator" ![Type.integer; Type.NoneType])))
    (Type.parametric "typing.Generator" ![Type.integer; Type.NoneType; Type.NoneType])


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
  assert_dequalify
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "A.B.C.MyEnum"; member_name = "ONE" }))
    (Type.Literal
       (Type.EnumerationMember { enumeration_type = Type.Primitive "C.MyEnum"; member_name = "ONE" }));

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
  let assert_create ?(aliases = Type.empty_aliases) sources expected =
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
      match Type.create ~aliases:Type.empty_aliases (parse_single_expression callable) with
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (Type.create ~aliases:Type.empty_aliases (parse_single_expression expected))
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
      Type.create ~aliases:Type.empty_aliases (parse_single_expression callable)
      |> function
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    let parameters =
      List.hd_exn overloads
      |> Type.Callable.Overload.parameters
      |> Option.value ~default:[]
      |> List.map ~f:(function
             | Type.Callable.Parameter.PositionalOnly { annotation; _ } -> annotation
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
          ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
          ~annotation:(Type.Variable unary)
          ()));
  ()


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
  let create source = Type.create ~aliases:Type.empty_aliases (parse_single_expression source) in
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
  let end_state, transformed = CountTransform.visit 0 (create "typing.Literal[test.MyEnum.ONE]") in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 2 end_state;

  let module SubstitutionTransform = Type.Transform.Make (struct
    type state = int

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive integer when String.equal integer "int" && state > 0 ->
            state - 1, Type.string
        | _ -> state, annotation
      in
      { Type.Transform.transformed_annotation; new_state }


    let visit_children_before _ = function
      | Type.Union [Type.NoneType; _]
      | Type.Union [_; Type.NoneType] ->
          false
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
  let module ConcatenateTransform = Type.Transform.Make (struct
    type state = string

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive primitive -> state ^ primitive, annotation
        | Type.Parametric { name; parameters } -> "", Type.parametric (name ^ state) parameters
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
        | Type.Parametric { name; parameters } -> state ^ name, Type.parametric name parameters
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.Variadic.Parameters.create "T"
      |> Type.Variable.Variadic.Parameters.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
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
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concatenation (create_concatenation list_variadic));
           ])
      ~annotation:Type.integer
      ()
  in
  let star_args_any_callable =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             PositionalOnly { index = 0; annotation = Type.bool; default = false };
             Variable (Concrete Type.Any);
           ])
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
      [
        { name = "__x"; annotation = Type.bool; default = false };
        { name = "__a"; annotation = Type.integer; default = false };
        { name = "__b"; annotation = Type.string; default = false };
      ]
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Type.Callable.create
          ~parameters:
            (Defined
               [
                 PositionalOnly { index = 0; annotation = Type.bool; default = false };
                 Variable (Concatenation (create_concatenation list_variadic));
               ])
          ~annotation:Type.integer
          ()))
    (Type.Callable.create ~parameters:(Defined replaced) ~annotation:Type.integer ());
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Tuple (Bounded (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic)))))
    (Tuple
       (Bounded
          (Concrete
             [
               Parametric { name = "Foo"; parameters = ![Type.parametric "Bar" ![Type.integer]] };
               Parametric { name = "Foo"; parameters = ![Type.parametric "Bar" ![Type.string]] };
             ])));
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.replace_all
       (fun _ -> Some (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
       (Type.parametric
          "Baz"
          [Group (Concatenation (create_concatenation (Type.Variable.Variadic.List.create "Ts")))]))
    (Type.parametric "Baz" [Group (Concrete [Type.integer; Type.string])]);
  assert_equal
    (Type.Variable.GlobalTransforms.ParameterVariadic.replace_all
       (fun _ ->
         Some
           (Type.Callable.Defined [Named { name = "p"; annotation = Type.integer; default = false }]))
       (Type.parametric
          "G"
          [
            CallableParameters
              (Type.Variable.Variadic.Parameters.self_reference
                 (Type.Variable.Variadic.Parameters.create "TParams"));
          ]))
    (Type.parametric
       "G"
       [
         CallableParameters
           (Defined [Named { name = "p"; annotation = Type.integer; default = false }]);
       ]);
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
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
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
               [
                 PositionalOnly { index = 0; annotation = Type.bool; default = false };
                 Variable (Concatenation (create_concatenation list_variadic));
               ])
          ~annotation:Type.integer
          ()))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Tuple (Bounded (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic)))))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ListVariadic.collect_all
       (Type.parametric
          "Huh"
          [Group (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] list_variadic))]))
    [Type.Variable.Variadic.List.create "Ts"];
  assert_equal
    (Type.Variable.GlobalTransforms.ParameterVariadic.collect_all
       (Type.parametric
          "G"
          [
            CallableParameters
              (Type.Variable.Variadic.Parameters.self_reference
                 (Type.Variable.Variadic.Parameters.create "TParams"));
          ]))
    [Type.Variable.Variadic.Parameters.create "TParams"];
  ()


let test_parse_type_variable_declarations _ =
  let assert_parses_declaration expression expected =
    assert_equal
      (Some expected)
      (Type.Variable.parse_declaration
         (parse_single_expression expression)
         ~target:(Reference.create "target"))
  in
  let assert_declaration_does_not_parse expression =
    assert_equal
      None
      (Type.Variable.parse_declaration
         (parse_single_expression expression)
         ~target:(Reference.create "target"))
  in
  assert_parses_declaration
    "pyre_extensions.ParameterSpecification('Tparams')"
    (Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "target"));
  assert_declaration_does_not_parse "pyre_extensions.ParameterSpecification('Tparams', int, str)";
  assert_parses_declaration
    "pyre_extensions.ListVariadic('Ts')"
    (Type.Variable.ListVariadic (Type.Variable.Variadic.List.create "target"));
  assert_declaration_does_not_parse "pyre_extensions.ListVariadic('Ts', int, str)";
  ()


let test_middle_singleton_replace_variable _ =
  let assert_replaces_into ~middle ~replacement expected =
    assert_equal
      ~printer:Type.show
      (Type.OrderedTypes.Concatenation.Middle.singleton_replace_variable middle ~replacement)
      expected
  in
  let variable = Type.Variable.Variadic.List.create "Ts" in
  let unary_variable = Type.Variable.Unary.create "T" in
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create
         ~mappers:
           [
             Type.Record.OrderedTypes.RecordConcatenate.Middle.ClassMapper "Foo";
             Type.Record.OrderedTypes.RecordConcatenate.Middle.ClassMapper "Bar";
           ]
         ~variable)
    ~replacement:Type.integer
    (Type.parametric
       "Foo"
       [Single (Parametric { name = "Bar"; parameters = [Single Type.integer] })]);
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create
         ~mappers:
           [
             Type.Record.OrderedTypes.RecordConcatenate.Middle.GenericAliasMapper
               (Type.union [Type.Variable unary_variable; Type.integer]);
             Type.Record.OrderedTypes.RecordConcatenate.Middle.GenericAliasMapper
               (Type.Tuple
                  (Type.Bounded
                     (Concrete [Type.Variable unary_variable; Type.Variable unary_variable])));
           ]
         ~variable)
    ~replacement:Type.string
    (Type.union [Type.integer; Type.Tuple (Type.Bounded (Concrete [Type.string; Type.string]))]);

  (* This approach is used to solve concretes against maps *)
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create
         ~mappers:
           [
             Type.Record.OrderedTypes.RecordConcatenate.Middle.ClassMapper "Foo";
             Type.Record.OrderedTypes.RecordConcatenate.Middle.ClassMapper "Bar";
           ]
         ~variable)
    ~replacement:(Type.Variable unary_variable)
    (Type.parametric
       "Foo"
       [Single (Parametric { name = "Bar"; parameters = [Single (Type.Variable unary_variable)] })]);
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create
         ~mappers:
           [
             Type.Record.OrderedTypes.RecordConcatenate.Middle.GenericAliasMapper
               (Type.union [Type.Variable unary_variable; Type.integer]);
             Type.Record.OrderedTypes.RecordConcatenate.Middle.GenericAliasMapper
               (Type.Tuple
                  (Type.Bounded
                     (Concrete [Type.Variable unary_variable; Type.Variable unary_variable])));
           ]
         ~variable)
    ~replacement:(Type.Variable unary_variable)
    (Type.union
       [
         Type.integer;
         Type.Tuple
           (Type.Bounded (Concrete [Type.Variable unary_variable; Type.Variable unary_variable]));
       ]);
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
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers
         ~mappers:["Foo"; "Bar"]
         ~variable)
    ~replacement:(fun _ -> Some (Concrete [Type.integer]))
    (Some
       (Concrete
          [
            Parametric
              {
                name = "Foo";
                parameters =
                  [Single (Parametric { name = "Bar"; parameters = [Single Type.integer] })];
              };
          ]));
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers
         ~mappers:["Foo"; "Bar"]
         ~variable)
    ~replacement:(fun _ -> Some (Concrete [Type.integer; Type.string]))
    (Some
       (Concrete
          [
            Parametric
              {
                name = "Foo";
                parameters =
                  [Single (Parametric { name = "Bar"; parameters = [Single Type.integer] })];
              };
            Parametric { name = "Foo"; parameters = ![Type.parametric "Bar" ![Type.string]] };
          ]));
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers ~mappers:["Foo"] ~variable)
    ~replacement:(fun _ -> Some (Concatenation (create_concatenation ~mappers:["Bar"] variable)))
    (Some (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] variable)));
  let other_variable = Type.Variable.Variadic.List.create "Ts2" in
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers
         ~mappers:["Foo"; "Bar"]
         ~variable)
    ~replacement:(function
      | _ -> Some (Concatenation (create_concatenation other_variable)))
    (Some (Concatenation (create_concatenation ~mappers:["Foo"; "Bar"] other_variable)));
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers
         ~mappers:["Foo"; "Bar"]
         ~variable)
    ~replacement:(function
      | _ -> Some Any)
    (Some Any);
  assert_replaces_into
    ~middle:
      (Type.OrderedTypes.Concatenation.Middle.create_from_class_mappers
         ~mappers:["Foo"; "Bar"]
         ~variable)
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


let test_infer_transform _ =
  let assert_transform ~expected ~annotation =
    assert_equal (Type.infer_transform annotation) expected
  in
  assert_transform
    ~annotation:(Type.parametric "_PathLike" ![Type.Primitive "string"])
    ~expected:(Type.parametric "PathLike" ![Type.Primitive "string"]);
  assert_transform
    ~annotation:(Type.parametric "typing.Dict" ![Type.Bottom; Type.Bottom])
    ~expected:(Type.parametric "dict" ![Type.Any; Type.Any]);
  assert_transform
    ~annotation:
      (Type.Tuple
         (Type.Bounded
            (Concrete [Type.Primitive "string"; Type.Primitive "string"; Type.Primitive "string"])))
    ~expected:(Type.Tuple (Type.Unbounded (Type.Primitive "string")));
  assert_transform
    ~annotation:
      (Type.Tuple (Type.Bounded (Concrete [Type.Primitive "string"; Type.Primitive "string"])))
    ~expected:
      (Type.Tuple (Type.Bounded (Concrete [Type.Primitive "string"; Type.Primitive "string"])));
  assert_transform
    ~annotation:
      (Type.parametric
         "Union"
         ![Type.Primitive "string"; Type.Primitive "string"; Type.Primitive "int"])
    ~expected:(Type.Union [Type.Primitive "int"; Type.Primitive "string"]);
  assert_transform
    ~annotation:(Type.parametric "Union" ![Type.Primitive "string"; Type.Primitive "string"])
    ~expected:(Type.Primitive "string");
  assert_transform
    ~annotation:(Type.parametric "Union" ![Type.NoneType; Type.Primitive "string"])
    ~expected:(Type.optional (Type.Primitive "string"))


let test_fields_from_constructor _ =
  let assert_fields ~constructor ~expected =
    assert_equal
      ~printer:[%show: Type.t Type.Record.TypedDictionary.typed_dictionary_field list option]
      expected
      (Type.TypedDictionary.fields_from_constructor constructor)
  in
  let fields =
    [
      { Type.Record.TypedDictionary.name = "name"; annotation = Type.string; required = true };
      { Type.Record.TypedDictionary.name = "year"; annotation = Type.integer; required = false };
    ]
  in
  let non_constructor =
    match Type.Callable.create ~annotation:Type.integer () with
    | Type.Callable callable -> callable
    | _ -> failwith "expected callable"
  in
  assert_fields ~constructor:non_constructor ~expected:None;
  assert_fields
    ~constructor:(Type.TypedDictionary.constructor ~name:"Movie" ~fields)
    ~expected:(Some fields);
  assert_fields
    ~constructor:(Type.TypedDictionary.constructor ~name:"Movie" ~fields)
    ~expected:(Some fields);
  ()


let test_is_unit_test _ =
  let assert_is_unit_test name expected =
    Type.Primitive.is_unit_test name |> assert_equal expected
  in
  assert_is_unit_test "unittest.TestCase" true;
  assert_is_unit_test "unittest.case.TestCase" true;
  assert_is_unit_test "a.TestCase" false;
  ()


let polynomial_show_normal =
  Type.Polynomial.show_normal
    ~show_variable:Type.polynomial_show_variable
    ~show_variadic:Type.polynomial_show_variadic


let polynomial_add = Type.Polynomial.add ~compare_t:Type.compare

let polynomial_subtract = Type.Polynomial.subtract ~compare_t:Type.compare

let polynomial_multiply = Type.Polynomial.multiply ~compare_t:Type.compare

let polynomial_divide = Type.Polynomial.divide ~compare_t:Type.compare

let polynomial_create_from_variables_list =
  Type.Polynomial.create_from_variables_list ~compare_t:Type.compare


let test_polynomial_create_from_list _ =
  let assert_create given expected =
    let given = polynomial_create_from_variables_list given in
    assert_equal ~printer:Fn.id expected (polynomial_show_normal given)
  in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  assert_create [] "0";
  assert_create [1, []] "1";
  assert_create [1, [x, 1]] "x";
  assert_create [3, [x, 1]] "3x";
  assert_create [3, [x, 2]] "3x^2";
  assert_create [5, [y, 1; z, 1; x, 1]] "5xyz";
  assert_create [5, [y, 2; z, 1; x, 3]] "5x^3y^2z";
  assert_create [4, []; 3, [x, 2]] "4 + 3x^2";
  assert_create [3, [x, 2]; 4, [y, 2]] "3x^2 + 4y^2";
  assert_create [3, [x, 2; y, 1]; 4, [y, 2]] "4y^2 + 3x^2y";
  assert_create [1, [y, 1]; 1, [x, 1]; 2, []] "2 + x + y";
  assert_create
    [
      2, [y, 1]; 1, [x, 1]; 1, [x, 2; y, 1]; 1, [z, 1]; 1, [y, 1; z, 1; x, 1]; 1, [x, 1; y, 2]; 2, [];
    ]
    "2 + x + 2y + z + x^2y + xy^2 + xyz";
  ()


let test_add_polynomials _ =
  let assert_add given1 given2 expected =
    let given1 = polynomial_create_from_variables_list given1 in
    let given2 = polynomial_create_from_variables_list given2 in
    assert_equal ~printer:Fn.id expected (polynomial_show_normal (polynomial_add given1 given2));
    assert_equal ~printer:Fn.id expected (polynomial_show_normal (polynomial_add given2 given1))
  in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  assert_add [3, []] [2, []] "5";
  assert_add [3, []] [-3, []; 2, [x, 2]] "2x^2";
  assert_add [1, []; 3, [x, 1]; 2, [y, 1]] [2, []; 1, [x, 1]; 1, [z, 1]] "3 + 4x + 2y + z";
  ()


let test_subtract_polynomials _ =
  let assert_subtract given1 given2 expected =
    let given1 = polynomial_create_from_variables_list given1 in
    let given2 = polynomial_create_from_variables_list given2 in
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_subtract given1 given2))
  in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  assert_subtract [] [3, []] "-3";
  assert_subtract [3, []] [3, []] "0";
  assert_subtract [3, [x, 1]] [3, [x, 1]] "0";
  assert_subtract [] [1, [x, 1]] "-x";
  assert_subtract [2, []] [3, []] "-1";
  assert_subtract [1, []; 3, [x, 1]; 2, [y, 1]] [2, []; 1, [x, 1]; 1, [z, 1]] "-1 + 2x + 2y + -z";
  ()


let test_multiply_polynomial _ =
  let assert_multiply given1 given2 expected =
    let given1 = polynomial_create_from_variables_list given1 in
    let given2 = polynomial_create_from_variables_list given2 in
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_multiply given1 given2));
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_multiply given2 given1))
  in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  assert_multiply [1, []] [] "0";
  assert_multiply [1, [x, 1]] [] "0";
  assert_multiply [2, []] [4, []] "8";
  assert_multiply [2, [y, 1]] [1, [z, 1]] "2yz";
  assert_multiply [2, [y, 1]] [1, [x, 1]] "2xy";
  assert_multiply [2, [y, 1]] [1, [x, 1; z, 1]] "2xyz";
  assert_multiply [3, []; 1, [x, 1]] [1, [y, 1]] "3y + xy";
  assert_multiply [1, [x, 1]; 3, [z, 1]] [2, []; 1, [y, 2]] "2x + 6z + xy^2 + 3y^2z";
  ()


let test_divide_polynomial _ =
  let assert_divide given1 given2 expected =
    let given1 = polynomial_create_from_variables_list given1 in
    let given2 = polynomial_create_from_variables_list given2 in
    assert_equal ~printer:Fn.id expected (polynomial_show_normal (polynomial_divide given1 given2))
  in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  (*Division by zero is checked before calling this function*)
  assert_divide [1, [x, 1]] [] "0";
  assert_divide [2, []] [2, []] "1";
  assert_divide [4, []] [2, []] "2";
  assert_divide [-3, []] [2, []] "-2";
  assert_divide [2, [y, 1]] [1, [z, 1]] "(2y//z)";
  assert_divide [2, [x, 1]; 1, [y, 1]] [1, [y, 1]; 1, [z, 1]] "((2x + y)//(y + z))";
  assert_divide [2, [y, 1]] [2, [x, 1]] "(y//x)";
  assert_divide [2, [y, 1; z, 1]] [1, [x, 1; z, 1]] "(2y//x)";
  assert_divide [2, [y, 1; z, 2]] [1, [z, 1]] "2yz";
  (*2x^3y + 3x^2z // 4x^2y*)
  assert_divide [2, [x, 3; y, 1]; 2, [x, 2; z, 1]] [4, [x, 2; y, 1]] "((z + xy)//2y)";
  ()


let test_parameter_create _ =
  assert_equal
    (Type.Callable.Parameter.create
       [{ Type.Callable.Parameter.name = "__"; annotation = Type.integer; default = false }])
    [
      Type.Callable.Parameter.PositionalOnly
        { index = 0; annotation = Type.integer; default = false };
    ]


let test_add_polynomials_with_variadics _ =
  let assert_add given1 given2 expected =
    assert_equal ~printer:Fn.id expected (polynomial_show_normal (polynomial_add given1 given2));
    assert_equal ~printer:Fn.id expected (polynomial_show_normal (polynomial_add given2 given1))
  in
  let x = Type.Variable.Unary.create "x" in
  let ts =
    Type.Variable.Variadic.List.create "Ts"
    |> Type.OrderedTypes.Concatenation.Middle.create_bare
    |> Type.OrderedTypes.Concatenation.create
  in
  let shape =
    Type.Variable.Variadic.List.create "Shape"
    |> Type.OrderedTypes.Concatenation.Middle.create_bare
    |> Type.OrderedTypes.Concatenation.create
  in
  let polynomial_3_2x = polynomial_create_from_variables_list [3, []; 2, [x, 1]] in
  let polynomial_ts = Type.Polynomial.create_from_variadic ts ~operation:Type.Monomial.Length in
  let polynomial_shape =
    Type.Polynomial.create_from_variadic shape ~operation:Type.Monomial.Product
  in
  assert_add
    (polynomial_add polynomial_3_2x polynomial_ts)
    (polynomial_add polynomial_ts polynomial_shape)
    "3 + 2x + 2Length[Ts] + Product[Shape]";
  ()


let test_subtract_polynomials_with_variadics _ =
  let assert_subtract given1 given2 expected =
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_subtract given1 given2))
  in
  let x = Type.Variable.Unary.create "x" in
  let ts =
    Type.Variable.Variadic.List.create "Ts"
    |> Type.OrderedTypes.Concatenation.Middle.create_bare
    |> Type.OrderedTypes.Concatenation.create
  in
  let shape =
    Type.Variable.Variadic.List.create "Shape"
    |> Type.OrderedTypes.Concatenation.Middle.create_bare
    |> Type.OrderedTypes.Concatenation.create
  in
  let polynomial_3_2x = polynomial_create_from_variables_list [3, []; 2, [x, 1]] in
  let polynomial_ts = Type.Polynomial.create_from_variadic ts ~operation:Type.Monomial.Length in
  let polynomial_shape =
    Type.Polynomial.create_from_variadic shape ~operation:Type.Monomial.Product
  in
  assert_subtract
    (polynomial_add polynomial_3_2x polynomial_ts)
    (polynomial_add polynomial_ts polynomial_shape)
    "3 + 2x + -Product[Shape]";
  ()


let test_multiply_polynomials_with_variadics _ =
  let assert_multiply given1 given2 expected =
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_multiply given1 given2));
    assert_equal
      ~printer:Fn.id
      expected
      (polynomial_show_normal (polynomial_multiply given2 given1))
  in
  let x = Type.Variable.Unary.create "x" in
  let ts =
    Type.Variable.Variadic.List.create "Ts"
    |> Type.OrderedTypes.Concatenation.Middle.create_bare
    |> Type.OrderedTypes.Concatenation.create
  in
  let polynomial_3_2x = polynomial_create_from_variables_list [3, []; 2, [x, 1]] in
  let polynomial_ts = Type.Polynomial.create_from_variadic ts ~operation:Type.Monomial.Length in
  assert_multiply
    (polynomial_add polynomial_3_2x polynomial_ts)
    polynomial_ts
    "3Length[Ts] + 2xLength[Ts] + Length[Ts]^2";
  ()


let test_resolve_class _ =
  let assert_resolved_class annotation expected =
    assert_equal
      ~printer:(fun x -> [%sexp_of: Type.class_data list option] x |> Sexp.to_string_hum)
      expected
      (Type.resolve_class annotation)
  in
  assert_resolved_class Type.Any (Some []);
  assert_resolved_class
    (Type.meta Type.integer)
    (Some [{ instantiated = Type.integer; accessed_through_class = true; class_name = "int" }]);
  assert_resolved_class
    (Type.optional Type.integer)
    (Some
       [
         {
           instantiated = Type.optional Type.integer;
           accessed_through_class = false;
           class_name = "typing.Optional";
         };
       ]);
  assert_resolved_class
    (Type.union [Type.integer; Type.string])
    (Some
       [
         { instantiated = Type.integer; accessed_through_class = false; class_name = "int" };
         { instantiated = Type.string; accessed_through_class = false; class_name = "str" };
       ]);
  assert_resolved_class
    (Type.union [Type.Primitive "Foo"; Type.list Type.integer])
    (Some
       [
         {
           instantiated = Type.list Type.integer;
           accessed_through_class = false;
           class_name = "list";
         };
         { instantiated = Type.Primitive "Foo"; accessed_through_class = false; class_name = "Foo" };
       ]);
  assert_resolved_class
    (Type.union [Type.Primitive "Foo"; Type.list Type.integer])
    (Some
       [
         {
           instantiated = Type.list Type.integer;
           accessed_through_class = false;
           class_name = "list";
         };
         { instantiated = Type.Primitive "Foo"; accessed_through_class = false; class_name = "Foo" };
       ]);
  ()


let () =
  "type"
  >::: [
         "create" >:: test_create;
         "resolve_aliases" >:: test_resolve_aliases;
         "instantiate" >:: test_instantiate;
         "expression" >:: test_expression;
         "concise" >:: test_concise;
         "weaken_literals" >:: test_weaken_literals;
         "union" >:: test_union;
         "primitives" >:: test_primitives;
         "elements" >:: test_elements;
         "exists" >:: test_exists;
         "is_async_generator" >:: test_is_generator;
         "contains_callable" >:: test_contains_callable;
         "contains_any" >:: test_contains_any;
         "expression_contains_any" >:: test_expression_contains_any;
         "is_concrete" >:: test_is_concrete;
         "is_not_instantiated" >:: test_is_not_instantiated;
         "is_meta" >:: test_is_meta;
         "is_none" >:: test_is_none;
         "is_type_alias" >:: test_is_type_alias;
         "unfold_recursive_type" >:: test_unfold_recursive_type;
         "contains_unknown" >:: test_contains_unknown;
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
         "concatenation_zip" >:: test_concatenation_zip;
         "infer_transform" >:: test_infer_transform;
         "fields_from_constructor" >:: test_fields_from_constructor;
         "map_callable_annotation" >:: test_map_callable_annotation;
         "type_parameters_for_bounded_tuple_union" >:: test_type_parameters_for_bounded_tuple_union;
         "polynomial_create_from_list" >:: test_polynomial_create_from_list;
         "add_polynomials" >:: test_add_polynomials;
         "subtract_polynomials" >:: test_subtract_polynomials;
         "multiply_polynomial" >:: test_multiply_polynomial;
         "divide_polynomial" >:: test_divide_polynomial;
         "add_polynomials_with_variadics" >:: test_add_polynomials_with_variadics;
         "subtract_polynomials_with_variadics" >:: test_subtract_polynomials_with_variadics;
         "multiply_polynomials_with_variadics" >:: test_multiply_polynomials_with_variadics;
         "resolve_class" >:: test_resolve_class;
       ]
  |> Test.run;
  "primitive" >::: ["is unit test" >:: test_is_unit_test] |> Test.run;
  "callable"
  >::: [
         "from_overloads" >:: test_from_overloads;
         "with_return_annotation" >:: test_with_return_annotation;
         "overload_parameters" >:: test_overload_parameters;
         "parameter_create" >:: test_parameter_create;
       ]
  |> Test.run
