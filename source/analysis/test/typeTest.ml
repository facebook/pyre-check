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
open Test

let empty_head variable = { Type.Callable.head = []; variable }

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

let make_callable_from_arguments annotations =
  Type.Callable.Defined
    (List.mapi
       ~f:(fun index annotation ->
         Type.Callable.RecordParameter.PositionalOnly { index; annotation; default = false })
       annotations)


let assert_create ?(aliases = fun _ -> None) source annotation =
  assert_equal
    ~printer:Type.show
    ~cmp:Type.equal
    annotation
    (Type.create
       ~aliases:(fun ?replace_unbound_parameters_with_any:_ -> aliases)
       (parse_single_expression ~preprocess:true source))


let test_create _ =
  assert_create "foo" (Type.Primitive "foo");
  assert_create "foo.bar" (Type.Primitive "foo.bar");
  assert_create "object" (Type.Primitive "object");
  assert_create "foo[bar]" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create
    "foo[bar, baz]"
    (Type.parametric "foo" ![Type.Primitive "bar"; Type.Primitive "baz"]);
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
  assert_create
    "typing.Tuple[int, ...]"
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer));
  assert_create "typing.Tuple[()]" (Type.tuple []);
  assert_create "tuple" (Type.Primitive "tuple");
  assert_create "typing.Any" Type.Any;
  assert_create "typing.Optional[int]" (Type.optional Type.integer);
  assert_create "typing.Optional.__getitem__(int)" (Type.optional Type.integer);
  assert_create "typing.Set[int]" (Type.set Type.integer);
  assert_create "typing.Union[int, str]" (Type.union [Type.integer; Type.string]);
  assert_create "typing.Union[int, typing.Any]" (Type.union [Type.integer; Type.Any]);
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
  assert_create "typing.TypeVar('_T', covariant=True)" (Type.variable ~variance:Covariant "_T");
  assert_create "typing.TypeVar('_T', covariant=False)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', contravariant=True)"
    (Type.variable ~variance:Contravariant "_T");
  assert_create "typing.TypeVar('_T', contravariant=False)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', int)"
    (Type.variable ~constraints:(Type.Variable.Explicit [Type.integer]) "_T");
  assert_create "typing.TypeVar('_T', name=int)" (Type.variable "_T");
  assert_create
    "typing.TypeVar('_T', bound=int)"
    (Type.variable ~constraints:(Type.Variable.Bound Type.integer) "_T");
  assert_create
    "typing.TypeVar('_T', bound='C')"
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

  (* String literals. *)
  assert_create "'foo'" (Type.Primitive "foo");
  assert_create "'foo.bar'" (Type.Primitive "foo.bar");
  assert_create "foo['bar']" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create "'Type[str]'" (Type.parametric "Type" ![Type.Primitive "str"]);
  assert_create "'Type[[[]str]'" (Type.Primitive "Type[[[]str]");

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
  assert_create "typing_extensions.Literal[str]" (Type.Literal (Type.String AnyLiteral));
  assert_create "_NotImplementedType" Type.Any;

  (* ParamSpec class. *)
  assert_create
    "ParamSpecClass[[int, str], [bool]]"
    (Type.parametric
       "ParamSpecClass"
       [
         CallableParameters
           (Defined
              [
                PositionalOnly { index = 0; annotation = Type.integer; default = false };
                PositionalOnly { index = 1; annotation = Type.string; default = false };
              ]);
         CallableParameters
           (Defined [PositionalOnly { index = 0; annotation = Type.bool; default = false }]);
       ]);
  ()


let test_create_callable _ =
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
  ()


let test_create_alias _ =
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
          "baz.Callable", Type.Callable.create ~annotation:Type.Any ~parameters:Undefined ();
          ( "Predicate",
            Type.Callable.create
              ~annotation:Type.integer
              ~parameters:
                (Defined
                   [PositionalOnly { index = 0; annotation = Type.variable "T"; default = false }])
              () );
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

  assert_alias
    "baz.Callable[[int], str]"
    (Type.Callable
       {
         kind = Type.Callable.Anonymous;
         implementation =
           {
             annotation = Type.string;
             parameters =
               Defined [PositionalOnly { index = 0; annotation = Type.integer; default = false }];
           };
         overloads = [];
       });
  assert_alias
    "Predicate[str]"
    (Type.Callable
       {
         kind = Type.Callable.Anonymous;
         implementation =
           {
             annotation = Type.integer;
             parameters =
               Defined [PositionalOnly { index = 0; annotation = Type.string; default = false }];
           };
         overloads = [];
       });
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
  ()


let test_create_type_operator _ =
  let assert_create ?(aliases = fun _ -> None) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create
         ~aliases:(fun ?replace_unbound_parameters_with_any:_ -> aliases)
         (parse_single_expression ~preprocess:true source))
  in

  (* Compose. *)
  let variable = Type.Variable.Unary.create "T" in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_create
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], str],
        typing.Callable[[str], bool],
        typing.Callable[[bool], typing.Tuple[int, int]]
      ]
    |}
    (Type.TypeOperation
       (Compose
          (Type.OrderedTypes.Concrete
             [
               Type.Callable.create
                 ~parameters:(make_callable_from_arguments [Type.integer])
                 ~annotation:Type.string
                 ();
               Type.Callable.create
                 ~parameters:(make_callable_from_arguments [Type.string])
                 ~annotation:Type.bool
                 ();
               Type.Callable.create
                 ~parameters:(make_callable_from_arguments [Type.bool])
                 ~annotation:(Type.tuple [Type.integer; Type.integer])
                 ();
             ])));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Compose[
        int,
        pyre_extensions.Unpack[Ts],
        str
      ]
    |}
    (Type.TypeOperation
       (Type.TypeOperation.Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create
                ~prefix:[Type.integer]
                ~suffix:[Type.string]
                variadic))));
  assert_create
    "pyre_extensions.Compose[typing.Callable[[int], int], ...]"
    (Type.TypeOperation
       (Type.TypeOperation.Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                (Type.Callable.create
                   ~parameters:(make_callable_from_arguments [Type.integer])
                   ~annotation:Type.integer
                   ())))));
  let aliases = function
    | "T" -> Some (Type.Variable variable)
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create
    ~aliases
    "pyre_extensions.Compose[T, Foo[int], typing.Callable[[str], bool]]"
    (Type.TypeOperation
       (Type.TypeOperation.Compose
          (Concrete
             [
               Type.Variable variable;
               Type.Parametric { name = "Foo"; parameters = [Type.Parameter.Single Type.integer] };
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.Parameter.PositionalOnly
                          { index = 0; annotation = Type.string; default = false };
                      ])
                 ~annotation:Type.bool
                 ();
             ])));
  assert_create "pyre_extensions.Compose[typing.Tuple[bool, str], ...]" Type.Top;
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Compose[
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[3]]
        ],
        typing.Callable[[int], int]
      ]
    |}
    Type.Top;
  assert_create
    "pyre_extensions.Compose[typing.Tuple[bool, str], typing.Callable[[int], str]]"
    Type.Top;
  assert_create
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          pyre_extensions.Compose[
            typing.Callable[[bool], bytes],
            typing.Callable[[bytes], int]
          ],
          typing.Callable[[int], float]
        ],
        typing.Callable[[float], str]
      ]
    |}
    (Type.TypeOperation
       (Type.TypeOperation.Compose
          (Concrete
             [
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.Parameter.PositionalOnly
                          { index = 0; annotation = Type.bool; default = false };
                      ])
                 ~annotation:Type.bytes
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.Parameter.PositionalOnly
                          { index = 0; annotation = Type.bytes; default = false };
                      ])
                 ~annotation:Type.integer
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.Parameter.PositionalOnly
                          { index = 0; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.float
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.Parameter.PositionalOnly
                          { index = 0; annotation = Type.float; default = false };
                      ])
                 ~annotation:Type.string
                 ();
             ])));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          pyre_extensions.Compose[
            pyre_extensions.Unpack[Ts],
            typing.Callable[[bytes], int]
          ],
          pyre_extensions.Compose[
            typing.Callable[[int], int],
            ...
          ]
        ],
        typing.Callable[[float], str]
      ]
    |}
    Type.Top;
  (* Subtract. *)
  assert_create
    {|
      pyre_extensions.Subtract[
        typing_extensions.Literal[3],
        typing_extensions.Literal[2]
      ]
    |}
    (Type.literal_integer 1);
  let variable =
    Type.Variable.Unary.create ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int")) "N"
  in
  assert_create
    ~aliases:(function
      | "N" -> Some (TypeAlias (Type.Variable variable))
      | _ -> None)
    {|
      pyre_extensions.Subtract[
        N,
        typing_extensions.Literal[1]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.subtract
          ~compare_t:Type.compare
          (Type.Polynomial.create_from_variable variable)
          (Type.Polynomial.create_from_int 1)));
  assert_create
    {|
      pyre_extensions.Subtract[
        typing_extensions.Literal[3],
        str
      ]
    |}
    (Type.Parametric
       {
         name = "pyre_extensions.Subtract";
         parameters = [Single (Type.literal_integer 3); Single Type.string];
       });

  (* Product. *)
  assert_create
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[3],
        typing_extensions.Literal[2]
      ]
    |}
    (Type.literal_integer 6);
  assert_create
    ~aliases:(function
      | "N" -> Some (TypeAlias (Type.Variable variable))
      | _ -> None)
    {|
      pyre_extensions.Product[
        N,
        typing_extensions.Literal[2]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.multiply
          ~compare_t:Type.compare
          (Type.Polynomial.create_from_variable variable)
          (Type.Polynomial.create_from_int 2)));
  assert_create
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[1],
        typing_extensions.Literal[2],
        typing_extensions.Literal[3],
        typing_extensions.Literal[4]
      ]
    |}
    (Type.literal_integer 24);
  let variable2 =
    Type.Variable.Unary.create ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int")) "N2"
  in
  let aliases = function
    | "N" -> Some (Type.Variable variable)
    | "N2" -> Some (Type.Variable variable2)
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create
    ~aliases
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[2],
        N,
        N2,
        N
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_variables_list
          ~compare_t:Type.compare
          [2, [variable, 2; variable2, 1]]));

  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Product[
        str,
        pyre_extensions.Unpack[Ts]
      ]
    |}
    Type.Top;
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[2],
        typing_extensions.Literal[0],
        pyre_extensions.Unpack[Ts]
      ]
    |}
    (Type.literal_integer 0);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[1],
        typing_extensions.Literal[1],
        pyre_extensions.Unpack[Ts],
        typing_extensions.Literal[1],
        typing_extensions.Literal[1],
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic),
                  1 );
              ] );
          ]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Product[
        int,
        pyre_extensions.Unpack[Ts],
      ]
    |}
    (Type.Primitive "int");
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    {|
      pyre_extensions.Product[
        typing.Any,
        pyre_extensions.Unpack[Ts],
      ]
    |}
    Type.Any;

  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
        pyre_extensions.Product[
          N2,
          N2
        ],
        typing_extensions.Literal[4],
        pyre_extensions.Unpack[Ts],
        typing_extensions.Literal[2],
        N,
        typing_extensions.Literal[3]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 24,
              [
                Type.Monomial.create_variable variable, 1;
                Type.Monomial.create_variable variable2, 2;
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic),
                  1 );
              ] );
          ]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
        N2,
        N2,
        typing_extensions.Literal[4],
        pyre_extensions.Unpack[Ts],
        typing_extensions.Literal[2],
        N,
        typing_extensions.Literal[3]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 24,
              [
                Type.Monomial.create_variable variable, 1;
                Type.Monomial.create_variable variable2, 2;
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic),
                  1 );
              ] );
          ]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
        pyre_extensions.Product[
          N2,
          pyre_extensions.Unpack[Ts]
        ],
        typing_extensions.Literal[4],
        pyre_extensions.Unpack[Ts],
        typing_extensions.Literal[2],
        N,
        typing_extensions.Literal[3]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 24,
              [
                Type.Monomial.create_variable variable, 1;
                Type.Monomial.create_variable variable2, 1;
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic),
                  2 );
              ] );
          ]));

  assert_create
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[
          typing.Tuple[
            typing_extensions.Literal[2],
            ...
          ]
        ]
      ]
    |}
    Type.integer;
  assert_create
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[
          typing.Tuple[
            int,
            ...
          ]
        ]
      ]
    |}
    Type.integer;
  assert_create
    ~aliases
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[
          typing.Tuple[
            pyre_extensions.Add[N, typing_extensions.Literal[1]],
            ...
          ]
        ]
      ]
    |}
    Type.integer;
  assert_create
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[2],
        pyre_extensions.Unpack[
          typing.Tuple[
            typing_extensions.Literal[0],
            ...
          ]
        ]
      ]
    |}
    (Type.literal_integer 0);
  assert_create
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[2],
        pyre_extensions.Unpack[
          typing.Tuple[
            typing_extensions.Literal[1],
            ...
          ]
        ],
        typing_extensions.Literal[3],
      ]
    |}
    (Type.literal_integer 6);

  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
        N,
        N2,
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            typing.Tuple[
              typing_extensions.Literal[2],
              typing_extensions.Literal[2]
            ]
          ]
        ],
        N2,
        typing_extensions.Literal[5]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 5,
              [
                Type.Monomial.create_variable variable, 1;
                Type.Monomial.create_variable variable2, 2;
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concrete_against_concatenation
                       ~concrete:[Type.literal_integer 2; Type.literal_integer 2]
                       ~concatenation:(Type.OrderedTypes.Concatenation.create variadic)),
                  1 );
              ] );
          ]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
        N,
        N2,
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            typing.Tuple[int, ...]
          ]
        ],
        N2,
        typing_extensions.Literal[5]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 5,
              [
                Type.Monomial.create_variable variable, 1;
                Type.Monomial.create_variable variable2, 2;
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concatenation_against_concatenation
                       ~compare_t:Type.compare
                       (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer)
                       (Type.OrderedTypes.Concatenation.create variadic)),
                  1 );
              ] );
          ]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
          pyre_extensions.Unpack[Ts],
          pyre_extensions.Unpack[Ts]
      ]
    |}
    Type.Top;
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | other -> aliases other)
    {|
      pyre_extensions.Product[
          pyre_extensions.Unpack[Ts],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts]
          ]
      ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic),
                  2 );
              ] );
          ]));
  ()


let test_create_variadic_tuple _ =
  let assert_create ?(aliases = fun _ -> None) source annotation =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create
         ~aliases:(fun ?replace_unbound_parameters_with_any:_ -> aliases)
         (parse_single_expression ~preprocess:true source))
  in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  (* Parametric types. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "Foo[pyre_extensions.Unpack[Ts]]"
    (Type.parametric "Foo" [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "Foo[int, pyre_extensions.Unpack[Ts], str]"
    (Type.parametric
       "Foo"
       [
         Single Type.integer;
         Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
         Single Type.string;
       ]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "Foo[pyre_extensions.Unpack[typing.Tuple[int, str]]]"
    (Type.parametric "Foo" [Single Type.integer; Single Type.string]);
  (* Nested unpacks get normalized. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "Foo[int, pyre_extensions.Unpack[typing.Tuple[str, pyre_extensions.Unpack[Ts]]]]"
    (Type.parametric
       "Foo"
       [
         Single Type.integer;
         Single Type.string;
         Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
       ]);
  assert_create
    "Foo[pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    (Type.parametric
       "Foo"
       [Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer)]);

  (* Tuples. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[pyre_extensions.Unpack[Ts]]"
    (Type.Tuple (Concatenation (Type.OrderedTypes.Concatenation.create variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create
             ~prefix:[Type.integer]
             ~suffix:[Type.string]
             variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[pyre_extensions.Unpack[Ts], pyre_extensions.Unpack[Ts]]"
    Type.Top;
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[pyre_extensions.Unpack[typing.Tuple[int, str]]]"
    (Type.tuple [Type.integer; Type.string]);
  (* Nested concrete unpacks get normalized. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[bool, pyre_extensions.Unpack[typing.Tuple[int, \
     pyre_extensions.Unpack[typing.Tuple[int, str]]]]]"
    (Type.tuple [Type.bool; Type.integer; Type.integer; Type.string]);

  (* Callables. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Callable[[int, pyre_extensions.Unpack[Ts], str], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      variadic));
            ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Callable[[int, pyre_extensions.Unpack[typing.Tuple[bool, pyre_extensions.Unpack[Ts], \
     bool]], str], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer; Type.bool]
                      ~suffix:[Type.bool; Type.string]
                      variadic));
            ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None)
    "typing.Callable[[Variable(int, pyre_extensions.Unpack[Ts], str)], \
     typing.Callable[[Variable(int, pyre_extensions.Unpack[Ts2], str)], int]]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      variadic));
            ])
       ~annotation:
         (Type.Callable.create
            ~parameters:
              (Defined
                 [
                   Variable
                     (Concatenation
                        (Type.OrderedTypes.Concatenation.create
                           ~prefix:[Type.integer]
                           ~suffix:[Type.string]
                           variadic2));
                 ])
            ~annotation:Type.integer
            ())
       ());

  (* Broadcasting. *)
  let variable_t1 =
    Type.Variable.Unary.create ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int")) "T1"
  in
  let variable_t2 =
    Type.Variable.Unary.create ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int")) "T2"
  in
  let literal_tuple = List.map ~f:Type.literal_integer in

  assert_create
    {|
      pyre_extensions.Broadcast[
        pyre_extensions.Broadcast[
          typing.Tuple[int, ...],
          typing.Tuple[typing_extensions.Literal[1], typing_extensions.Literal[2]],
        ],
        typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[1]],
      ]
    |}
    (Type.Tuple
       (Concatenation (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer)));
  assert_create
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[typing_extensions.Literal[5], typing_extensions.Literal[2]],
        typing.Tuple[typing_extensions.Literal[1], typing_extensions.Literal[1]]
      ]
    |}
    (Type.tuple (literal_tuple [5; 2]));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None)
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[typing_extensions.Literal[1], typing_extensions.Literal[2]],
        typing.Tuple[pyre_extensions.Unpack[Ts]],
      ]
    |}
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
             ~prefix:[]
             ~suffix:[]
             ~concrete:(literal_tuple [1; 2])
             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None)
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[pyre_extensions.Unpack[Ts]],
        typing.Tuple[pyre_extensions.Unpack[Ts]],
      ]
    |}
    (Type.Tuple (Concatenation (Type.OrderedTypes.Concatenation.create variadic)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None)
    {|
      typing.Tuple[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            typing.Tuple[pyre_extensions.Unpack[Ts]],
          ]
        ]
      ]
    |}
    (Type.Tuple (Concatenation (Type.OrderedTypes.Concatenation.create variadic)));
  let aliases = function
    | "T1" -> Some (Type.Variable variable_t1)
    | "T2" -> Some (Type.Variable variable_t2)
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_create
    ~aliases
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[T1, typing_extensions.Literal[5]],
        typing.Tuple[T2, typing_extensions.Literal[5]],
      ]
    |}
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             ~left:[Type.Variable variable_t1; Type.literal_integer 5]
             ~right:[Type.Variable variable_t2; Type.literal_integer 5])));
  assert_create
    ~aliases
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[T1, T2],
        typing.Tuple[T2, T1],
      ]
    |}
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             ~left:[Type.Variable variable_t1; Type.Variable variable_t2]
             ~right:[Type.Variable variable_t2; Type.Variable variable_t1])));
  assert_create
    ~aliases
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[typing_extensions.Literal[2]],
        typing.Tuple[typing_extensions.Literal[3]],
      ]
    |}
    (Type.Parametric
       {
         name = "pyre_extensions.BroadcastError";
         parameters =
           [
             Type.Parameter.Single (Type.tuple [Type.literal_integer 2]);
             Type.Parameter.Single (Type.tuple [Type.literal_integer 3]);
           ];
       });
  assert_create
    ~aliases
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[T1, typing_extensions.Literal[2]],
        typing.Tuple[T2, typing_extensions.Literal[3]],
      ]
    |}
    (Type.Parametric
       {
         name = "pyre_extensions.BroadcastError";
         parameters =
           [
             Type.Parameter.Single (Type.tuple [Type.Variable variable_t1; Type.literal_integer 2]);
             Type.Parameter.Single (Type.tuple [Type.Variable variable_t2; Type.literal_integer 3]);
           ];
       });
  assert_create {|
      pyre_extensions.Broadcast[1, 2]
    |} Type.Bottom;
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
    | "Tree" -> Some (Type.TypeAlias (Type.RecursiveType.create ~name:"Tree" ~body:tree_body))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.Primitive "Tree")
    (Type.RecursiveType.create ~name:"Tree" ~body:tree_body);
  (* Don't resolve the `Tree` reference within the body. *)
  assert_resolved
    ~aliases
    (Type.RecursiveType.create ~name:"Tree" ~body:tree_body)
    (Type.RecursiveType.create ~name:"Tree" ~body:tree_body);
  assert_resolved
    ~aliases
    (Type.list (Type.Primitive "Tree"))
    (Type.list (Type.RecursiveType.create ~name:"Tree" ~body:tree_body));
  (* Ignore spurious parameters to a non-generic recursive alias. *)
  assert_resolved
    ~aliases
    (Type.parametric "Tree" [Single Type.integer])
    (Type.RecursiveType.create ~name:"Tree" ~body:tree_body);
  let aliases = function
    | "MyDict" -> Some (Type.TypeAlias (Type.dictionary ~key:variable_t ~value:variable_k))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "MyDict" ![Type.integer; Type.string])
    (Type.dictionary ~key:Type.integer ~value:Type.string);
  let aliases = function
    | "Mix" ->
        Some
          (Type.TypeAlias
             (Type.parametric
                "Foo"
                ![variable_t; Type.list variable_v; Type.union [variable_k; variable_v]]))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "Mix" ![Type.integer; Type.string; Type.bool])
    (Type.parametric
       "Foo"
       ![Type.integer; Type.list Type.string; Type.union [Type.bool; Type.string]]);
  let aliases = function
    | "Foo" -> Some (Type.TypeAlias (Type.parametric "Bar" ![variable_t; variable_v]))
    | _ -> None
  in
  assert_resolved ~aliases (Type.Primitive "Foo") (Type.parametric "Bar" ![Type.Any; Type.Any]);
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "TParams" in
  let aliases = function
    | "TParams" -> Some (Type.VariableAlias (Type.Variable.ParameterVariadic parameter_variadic))
    | "FooParamSpec" ->
        Some
          (Type.TypeAlias
             (Type.parametric
                "Bar"
                [
                  CallableParameters
                    (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
                ]))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "FooParamSpec" ![Type.integer; Type.string])
    (Type.parametric
       "Bar"
       [
         CallableParameters
           (Defined
              [
                PositionalOnly { index = 0; annotation = Type.integer; default = false };
                PositionalOnly { index = 1; annotation = Type.string; default = false };
              ]);
       ]);
  assert_resolved
    ~aliases
    (Type.Primitive "FooParamSpec")
    (Type.parametric "Bar" [CallableParameters Undefined]);
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let aliases = function
    | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
    | "FloatTensor" ->
        Some
          (Type.TypeAlias
             (Type.parametric
                "Tensor"
                [
                  Single Type.float;
                  Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
                ]))
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "FloatTensor" ![Type.integer; Type.string])
    (Type.parametric "Tensor" [Single Type.float; Single Type.integer; Single Type.string]);
  assert_resolved
    ~aliases
    (Type.Primitive "FloatTensor")
    (Type.parametric
       "Tensor"
       [
         Single Type.float;
         Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.Any);
       ]);
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
  assert_expression (Type.parametric "foo.bar" ![Type.Primitive "baz"]) "foo.bar.__getitem__(baz)";
  assert_expression
    (Type.Tuple (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
    "typing.Tuple.__getitem__((int, str))";
  assert_expression
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
    "typing.Tuple.__getitem__((int, ...))";
  assert_expression (Type.parametric "list" ![Type.integer]) "typing.List.__getitem__(int)";

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
  assert_expression (Type.Literal (Type.String AnyLiteral)) "typing_extensions.Literal[str]";

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_expression
    (Type.parametric "Foo" [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)])
    "Foo[pyre_extensions.Unpack[Ts]]";
  assert_expression
    (Type.Tuple
       (Type.OrderedTypes.Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
             ~prefix:[]
             ~suffix:[]
             ~concrete:[Type.literal_integer 5]
             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))))
    {|
      typing.Tuple[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[(
            typing.Tuple[typing_extensions.Literal[5]],
            typing.Tuple[pyre_extensions.Unpack[Ts]]
          )]
        ]
      ]
    |};
  assert_expression
    (Type.Tuple
       (Type.OrderedTypes.Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             (Type.OrderedTypes.Concatenation.create variadic)
             (Type.OrderedTypes.Concatenation.create variadic))))
    {|
      typing.Tuple[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[(
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            typing.Tuple[pyre_extensions.Unpack[Ts]]
          )]
        ]
      ]
    |};
  assert_expression
    (Type.Tuple
       (Type.OrderedTypes.Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             ~left:[Type.variable "T1"]
             ~right:[Type.variable "T2"])))
    {|
      typing.Tuple[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[(
            typing.Tuple[T2],
            typing.Tuple[T1]
          )]
        ]
      ]
    |};
  assert_expression
    (Type.Tuple
       (Type.OrderedTypes.Concatenation
          (Type.OrderedTypes.Concatenation.create
             ~prefix:[Type.integer]
             ~suffix:[Type.string]
             variadic)))
    "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Parameter.Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      variadic));
            ])
       ~annotation:Type.integer
       ())
    "typing.Callable.__getitem__(([Variable(int, pyre_extensions.Unpack[Ts], str)], int))";

  (* Compose *)
  let callable1 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.integer])
      ~annotation:Type.string
      ()
  in
  let callable2 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.string])
      ~annotation:Type.bool
      ()
  in
  assert_expression
    (Type.TypeOperation (Compose (Type.OrderedTypes.Concrete [callable1; callable2])))
    "pyre_extensions.Compose[(typing.Callable[([PositionalOnly(int)], str)], \
     typing.Callable[([PositionalOnly(str)], bool)])]";
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
  assert_concise
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
    "Tuple[int, ...]";
  assert_concise (Type.union [Type.integer; Type.string]) "Union[int, str]";
  assert_concise (Type.variable ~constraints:(Type.Variable.Explicit [Type.Top]) "T") "T";
  assert_concise
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    "typing_extensions.Literal[test.MyEnum.ONE]";
  assert_concise (Type.Literal (Type.String AnyLiteral)) "typing_extensions.LiteralString";
  ()


let test_weaken_literals _ =
  let assert_weakened_literal literal expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected (Type.weaken_literals literal)
  in
  assert_weakened_literal (Type.literal_integer 1) Type.integer;
  assert_weakened_literal (Type.literal_string "foo") Type.string;
  assert_weakened_literal (Type.Literal (Type.String AnyLiteral)) Type.string;
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
  assert_equal
    []
    (Type.primitives (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_equal
    [Type.integer]
    (Type.primitives (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
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
  assert_equal
    ["tuple"]
    (Type.elements (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_equal
    ["int"; "tuple"]
    (Type.elements (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
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
       (Type.RecursiveType.create
          ~name:"Tree"
          ~body:(Type.tuple [Type.integer; Type.Primitive "Tree"])));
  ();
  assert_equal
    ["typing_extensions.Literal"]
    (Type.elements
       (Type.Literal
          (Type.EnumerationMember
             { enumeration_type = Type.Primitive "A.B.C.MyEnum"; member_name = "ONE" })));
  assert_equal
    ["str"; "typing_extensions.Literal"]
    (Type.elements (Type.Literal (Type.String AnyLiteral)));
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
  assert_true (top_exists (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_false
    (top_exists (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
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
  assert_true (Type.is_type_alias (Type.Primitive "typing_extensions.TypeAlias"));
  assert_false (Type.is_type_alias (Type.Primitive "typing.TypeAlias"));
  assert_false (Type.is_type_alias (Type.parametric "typing_extensions.TypeAlias" ![Type.Top]))


let test_create_recursive_type _ =
  let tree_name, tree_body =
    "Tree", Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; Type.Primitive "Tree"]]
  in
  (* No error. *)
  let tree_annotation = Type.RecursiveType.create ~name:tree_name ~body:tree_body in
  assert_raises
    (Failure "Body of recursive type contains a recursive type with the same name")
    (fun () -> Type.RecursiveType.create ~name:tree_name ~body:tree_annotation);
  ()


let test_unfold_recursive_type _ =
  let assert_unfolded recursive_type expected =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      expected
      (match recursive_type with
      | Type.RecursiveType record -> Type.RecursiveType.unfold_recursive_type record
      | _ -> failwith "expected RecursiveType")
  in
  let tree_name, tree_body =
    "Tree", Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; Type.Primitive "Tree"]]
  in
  let tree_annotation = Type.RecursiveType.create ~name:tree_name ~body:tree_body in
  assert_unfolded
    (Type.RecursiveType.create ~name:tree_name ~body:tree_body)
    (Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; tree_annotation]]);
  ()


let test_contains_unknown _ =
  assert_true (Type.contains_unknown Type.Top);
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
  assert_true (Type.contains_unknown (Type.Tuple (Concrete [Type.integer; Type.Top])));
  assert_false (Type.contains_unknown (Type.Tuple (Concrete [Type.integer; Type.string])));
  assert_true
    (Type.contains_unknown (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_false
    (Type.contains_unknown
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)))


let test_contains_undefined _ =
  assert_true (Type.contains_undefined Type.Bottom);
  assert_false (Type.contains_undefined Type.Top);
  assert_false (Type.contains_undefined Type.Any);
  assert_true (Type.contains_undefined (Type.optional Type.Bottom));
  assert_false (Type.contains_undefined (Type.optional Type.integer));
  assert_true
    (Type.contains_undefined (Type.optional (Type.parametric "foo" ![Type.integer; Type.Bottom])));
  assert_true (Type.contains_undefined (Type.parametric "foo" ![Type.integer; Type.Bottom]));
  assert_false (Type.contains_undefined (Type.parametric "foo" ![Type.integer]));
  assert_false (Type.contains_undefined Type.integer);
  assert_true (Type.contains_undefined Type.Bottom);
  assert_true (Type.contains_undefined (Type.Union [Type.integer; Type.Bottom]));
  assert_false (Type.contains_undefined (Type.Union [Type.integer; Type.string]));
  assert_false (Type.contains_undefined (Type.variable "derp"));
  assert_true (Type.contains_undefined (Type.Tuple (Concrete [Type.integer; Type.Bottom])));
  assert_false (Type.contains_undefined (Type.Tuple (Concrete [Type.integer; Type.string])));
  assert_true
    (Type.contains_undefined
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Bottom)));
  assert_false
    (Type.contains_undefined
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)))


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
  ()


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
  assert_parameters "typing.Callable('foo')[..., Unknown][[[int], str]]" ["int"];
  assert_parameters "typing.Callable('foo')[..., Unknown][[[int, str], str]]" ["int"; "str"];
  assert_parameters "typing.Callable('foo')[..., Unknown][[[], str]]" []


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
  let ts = Type.Variable.Variadic.Tuple.create "Ts" in
  let mixed_t =
    Type.OrderedTypes.Concatenation.create_from_unpackable
      (Type.OrderedTypes.Concatenation.create_unpackable_from_concrete_against_concatenation
         ~concrete:[Type.literal_integer 1]
         ~concatenation:(Type.OrderedTypes.Concatenation.create ts))
  in
  let end_state, _ = CountTransform.visit 0 (Tuple (Type.OrderedTypes.Concatenation mixed_t)) in
  assert_equal ~printer:string_of_int 2 end_state;
  let end_state, _ =
    CountTransform.visit
      0
      (Tuple
         (Type.OrderedTypes.Concatenation
            (Type.OrderedTypes.Concatenation.create_from_unpackable
               (Type.OrderedTypes.Concatenation
                .create_unpackable_from_concatenation_against_concatenation
                  ~compare_t:Type.compare
                  (Type.OrderedTypes.Concatenation.create ts)
                  (Type.OrderedTypes.Concatenation.create ts)))))
  in
  assert_equal ~printer:string_of_int 1 end_state;
  let end_state, _ =
    CountTransform.visit
      0
      (Tuple
         (Type.OrderedTypes.Concatenation
            (Type.OrderedTypes.Concatenation.create_from_unpackable
               (Type.OrderedTypes.Concatenation
                .create_unpackable_from_concrete_against_concatenation
                  ~concrete:[Type.literal_integer 1]
                  ~concatenation:mixed_t))))
  in

  assert_equal ~printer:string_of_int 3 end_state;
  let broadcast_unaries =
    Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
      ~prefix:[]
      ~suffix:[]
      ~compare_t:Type.compare
      ~left:[Type.variable "T1"]
      ~right:[Type.variable "T2"]
  in
  let end_state, _ =
    CountTransform.visit 0 (Tuple (Type.OrderedTypes.Concatenation broadcast_unaries))
  in
  assert_equal ~printer:string_of_int 3 end_state;

  let callable1 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.integer])
      ~annotation:Type.string
      ()
  in
  let callable2 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.string])
      ~annotation:Type.bool
      ()
  in
  let end_state, _ =
    CountTransform.visit
      0
      (TypeOperation (Compose (Type.OrderedTypes.Concrete [callable1; callable2])))
  in
  assert_equal ~printer:string_of_int 7 end_state;

  let end_state, _ =
    CountTransform.visit
      0
      (Type.IntExpression.create
         (Type.Polynomial.create_from_monomial_variables_list
            ~compare_t:Type.compare
            [
              ( 1,
                [
                  ( Type.Monomial.create_product
                      (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer),
                    1 );
                ] );
            ]))
  in
  assert_equal ~printer:string_of_int 2 end_state;

  let variable = Type.Variable.Unary.create "T" in
  let end_state, _ =
    CountTransform.visit
      0
      (Type.IntExpression.create
         (Type.Polynomial.create_from_monomial_variables_list
            ~compare_t:Type.compare
            [1, [Type.Monomial.create_variable variable, 1]]))
  in
  assert_equal ~printer:string_of_int 1 end_state;

  let variable = Type.Variable.Unary.create "T" in
  let end_state, _ =
    CountTransform.visit
      0
      (Type.IntExpression.create
         (Type.Polynomial.divide
            ~compare_t:Type.compare
            (Type.Polynomial.create_from_monomial_variables_list
               ~compare_t:Type.compare
               [
                 ( 1,
                   [
                     ( Type.Monomial.create_product
                         (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer),
                       1 );
                   ] );
               ])
            (Type.Polynomial.create_from_monomial_variables_list
               ~compare_t:Type.compare
               [1, [Type.Monomial.create_variable variable, 1]])))
  in
  assert_equal ~printer:string_of_int 2 end_state;

  let end_state, _ =
    CountTransform.visit
      0
      (Type.IntExpression.create
         (Type.Polynomial.create_from_monomial_variables_list
            ~compare_t:Type.compare
            [
              ( 1,
                [
                  ( Type.Monomial.create_product
                      (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer),
                    1 );
                ] );
              ( 1,
                [
                  ( Type.Monomial.create_product
                      (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer),
                    1 );
                ] );
            ]))
  in
  assert_equal ~printer:string_of_int 3 end_state;

  let end_state, _ =
    CountTransform.visit
      0
      (Type.IntExpression.create
         (Type.Polynomial.create_from_monomial_variables_list
            ~compare_t:Type.compare
            [
              ( 1,
                [
                  ( Type.Monomial.create_product
                      (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer),
                    1 );
                  ( Type.Monomial.create_product
                      (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.string),
                    1 );
                ] );
            ]))
  in
  assert_equal ~printer:string_of_int 3 end_state;

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
  ()


let test_mark_all_variables_as_free _ =
  let variable =
    Type.Variable (Type.Variable.Unary.create "T") |> Type.Variable.mark_all_variables_as_bound
  in
  assert_true (Type.Variable.all_variables_are_resolved variable);
  let variable = Type.Variable.mark_all_variables_as_free variable in
  assert_false (Type.Variable.all_variables_are_resolved variable);
  let callable =
    let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
    |> Type.Variable.mark_all_variables_as_bound
  in
  assert_true (Type.Variable.all_variables_are_resolved callable);
  let callable = Type.Variable.mark_all_variables_as_free callable in
  assert_false (Type.Variable.all_variables_are_resolved callable);
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
  ()


let polynomial_create_from_variables_list =
  Type.Polynomial.create_from_variables_list ~compare_t:Type.compare


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
  ()


let test_int_expression_create _ =
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let assert_create_list_type given expected =
    assert_equal
      ~printer:[%show: Type.type_t]
      ~cmp:[%equal: Type.type_t]
      (Type.IntExpression.create (polynomial_create_from_variables_list given))
      expected
  in
  let assert_create_list_list given expected =
    match Type.IntExpression.create (polynomial_create_from_variables_list given) with
    | Type.IntExpression (Data result_polynomial) ->
        assert_equal
          ~printer:[%show: Type.type_t Type.Polynomial.t]
          ~cmp:[%equal: Type.type_t Type.Polynomial.t]
          result_polynomial
          (polynomial_create_from_variables_list expected)
    | _ -> assert false
  in
  assert_create_list_list [1, [x, 1]; 1, []; -1, []] [1, [x, 1]; 1, []; -1, []];
  assert_create_list_list [1, [y, 1; y, 1]] [1, [y, 1; y, 1]];

  assert_create_list_type [7, []] (Type.literal_integer 7);
  assert_create_list_type [] (Type.literal_integer 0);
  assert_create_list_type [1, [x, 1]] (Type.Variable x);

  assert_create_list_list [0, [x, 1]; 0, []; 1, [x, 1; y, 1]] [1, [x, 1; y, 1]];
  assert_create_list_list [1, [x, 1]; 1, [y, 1]] [1, [y, 1]; 1, [x, 1]];
  assert_create_list_list [1, [x, 1; y, 1]] [1, [y, 1; x, 1]];
  ()


let test_replace_all _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  let assert_equal actual expected =
    assert_equal ~cmp:Type.equal ~printer:Type.show expected actual
  in
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all (fun _ -> Some Type.integer) annotation)
    (Type.parametric "p" ![Type.integer; Type.integer]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some Type.integer)
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> None)
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun variable -> Some (Type.Annotated (Type.Variable variable)))
       free_variable)
    (Type.annotated free_variable);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some Type.float)
       (Type.union [Type.literal_integer 2; Type.integer; free_variable]))
    (Type.union [Type.literal_integer 2; Type.integer; Type.float]);

  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  let w = Type.Variable.Unary.create "w" in
  let variable_list_to_type variable_list =
    polynomial_create_from_variables_list variable_list |> Type.polynomial_to_type
  in
  let divide_to_type numerator denominator =
    Type.IntExpression.create (Type.Polynomial.divide ~compare_t:Type.compare numerator denominator)
  in
  (* TODO: Task T90507423. This needs to be normalized before output. *)
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some Type.Any)
       (variable_list_to_type [1, []; 1, [x, 1]]))
    Type.Any;
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some Type.integer)
       (variable_list_to_type [1, []; 1, [x, 1]]))
    Type.integer;

  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 7))
       (divide_to_type (Type.Polynomial.create_from_int 7) (Type.Polynomial.create_from_variable x)))
    (Type.IntExpression.create (polynomial_create_from_variables_list [1, []]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 6))
       (divide_to_type
          (Type.Polynomial.create_from_int 10)
          (Type.Polynomial.create_from_variable x)))
    (Type.IntExpression.create (polynomial_create_from_variables_list [1, []]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when [%equal: Type.Variable.Unary.t] variable x ->
             Some
               (divide_to_type
                  (Type.Polynomial.create_from_int 7)
                  (polynomial_create_from_variables_list [1, [y, 1]]))
         | _ -> None)
       (variable_list_to_type [1, [x, 1; y, 1]]))
    (Type.IntExpression.create
       (Type.Polynomial.multiply
          ~compare_t:Type.compare
          (Type.Polynomial.create_from_variable y)
          (Type.Polynomial.divide
             ~compare_t:Type.compare
             (Type.Polynomial.create_from_int 7)
             (Type.Polynomial.create_from_variable y))));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when not ([%equal: Type.Variable.Unary.t] variable z) -> Some (Type.Variable z)
         | _ -> None)
       (Type.IntExpression.create
          (Type.Polynomial.add
             ~compare_t:Type.compare
             (Type.Polynomial.divide
                ~compare_t:Type.compare
                (polynomial_create_from_variables_list [7, [x, 1]])
                (polynomial_create_from_variables_list [1, [z, 3]]))
             (Type.Polynomial.divide
                ~compare_t:Type.compare
                (Type.Polynomial.create_from_int 7)
                (polynomial_create_from_variables_list [1, [y, 1; z, 1]])))))
    (Type.IntExpression.create
       (Type.Polynomial.multiply
          ~compare_t:Type.compare
          (Type.Polynomial.create_from_int 2)
          (Type.Polynomial.divide
             ~compare_t:Type.compare
             (Type.Polynomial.create_from_int 7)
             (polynomial_create_from_variables_list [1, [z, 2]]))));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when [%equal: Type.Variable.Unary.t] variable x -> Some (Type.Variable y)
         | _ -> None)
       (Type.IntExpression.create
          (Type.Polynomial.add
             ~compare_t:Type.compare
             (Type.Polynomial.create_from_int 1)
             (Type.Polynomial.divide
                ~compare_t:Type.compare
                (Type.Polynomial.create_from_variable x)
                (Type.Polynomial.create_from_variable y)))))
    (Type.IntExpression.create (polynomial_create_from_variables_list [2, []]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 0))
       (divide_to_type (Type.Polynomial.create_from_int 7) (Type.Polynomial.create_from_variable x)))
    Type.Bottom;

  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 4))
       (variable_list_to_type [2, [x, 3]; 3, [y, 1; z, 1]]))
    (Type.IntExpression.create (polynomial_create_from_variables_list [176, []]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when [%equal: Type.Variable.Unary.t] variable x -> Some (Type.literal_integer 5)
         | _ -> None)
       (variable_list_to_type [3, [x, 2]; 5, [y, 1]]))
    (variable_list_to_type [75, []; 5, [y, 1]]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (variable_list_to_type [3, [x, 2]; 5, [y, 2; z, 7]]))
       (variable_list_to_type [15150, []]))
    (variable_list_to_type [15150, []]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some Type.integer)
       (variable_list_to_type [2, [x, 3]]))
    Type.integer;
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.Variable x))
       (variable_list_to_type [2, [x, 1; y, 2]; 3, [x, 2; y, 1]]))
    (variable_list_to_type [5, [x, 3]]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (variable_list_to_type [3, [y, 2; z, 1]; 5, [y, 1; z, 3]]))
       (variable_list_to_type [2, [x, 2]; 5, [x, 1]]))
    (variable_list_to_type
       [18, [y, 4; z, 2]; 60, [y, 3; z, 4]; 50, [y, 2; z, 6]; 15, [y, 2; z, 1]; 25, [y, 1; z, 3]]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (variable_list_to_type [3, [w, 1]; 1, [z, 2]]))
       (variable_list_to_type [2, [x, 1]; 1, [y, 1]]))
    (variable_list_to_type [9, [w, 1]; 3, [z, 2]]);
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (variable_list_to_type [1, [x, 1; y, 1]]))
       (variable_list_to_type [1, [x, 1]; 1, [y, 1]]))
    (variable_list_to_type [1, [x, 1; y, 1]; 1, [x, 2; y, 1]]);
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  (* Product, unary *)
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 7))
       (Type.IntExpression.create
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation
                        .create_unpackable_from_concrete_against_concatenation
                          ~concrete:[Type.literal_integer 2; Type.Variable x]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic)),
                     1 );
                 ] );
             ])))
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concrete_against_concatenation
                       ~concrete:[Type.literal_integer 2; Type.literal_integer 7]
                       ~concatenation:(Type.OrderedTypes.Concatenation.create variadic)),
                  1 );
              ] );
          ]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 7))
       (Type.IntExpression.create
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation
                        .create_unpackable_from_concatenation_against_concatenation
                          ~compare_t:Type.compare
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[]
                             ~suffix:[]
                             ~concrete:[Type.literal_integer 2; Type.Variable y]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[]
                             ~suffix:[]
                             ~concrete:[Type.literal_integer 2; Type.Variable x]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                     1 );
                 ] );
             ])))
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concatenation_against_concatenation
                       ~compare_t:Type.compare
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[]
                          ~suffix:[]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 7]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[]
                          ~suffix:[]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 7]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                  1 );
              ] );
          ]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when [%equal: Type.Variable.Unary.t] variable x -> Some (Type.literal_integer 1)
         | _ -> Some (Type.literal_integer 2))
       (Type.IntExpression.create
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation
                        .create_unpackable_from_concatenation_against_concatenation
                          ~compare_t:Type.compare
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[Type.Variable x]
                             ~suffix:[Type.Variable y]
                             ~concrete:[Type.literal_integer 2; Type.Variable y]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[Type.Variable y]
                             ~suffix:[Type.Variable x]
                             ~concrete:[Type.literal_integer 2; Type.Variable x]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                     1 );
                 ] );
             ])))
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concatenation_against_concatenation
                       ~compare_t:Type.compare
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[Type.literal_integer 1]
                          ~suffix:[Type.literal_integer 2]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 2]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[Type.literal_integer 2]
                          ~suffix:[Type.literal_integer 1]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 1]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                  1 );
              ] );
          ]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (fun _ -> Some (Type.literal_integer 7))
       (Type.IntExpression.create
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation.create_unbounded_unpackable
                          (Type.tuple [Type.Variable x; Type.Variable y; Type.literal_integer 2])),
                     1 );
                 ] );
             ])))
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unbounded_unpackable
                       (Type.tuple
                          [Type.literal_integer 7; Type.literal_integer 7; Type.literal_integer 2])),
                  1 );
              ] );
          ]));
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.replace_all
       (function
         | variable when [%equal: Type.Variable.Unary.t] variable x -> Some (Type.literal_integer 1)
         | _ -> Some (Type.literal_integer 2))
       (divide_to_type
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation
                        .create_unpackable_from_concatenation_against_concatenation
                          ~compare_t:Type.compare
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[Type.Variable x]
                             ~suffix:[Type.Variable y]
                             ~concrete:[Type.literal_integer 2; Type.Variable y]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                          (Type.OrderedTypes.Concatenation
                           .create_from_concrete_against_concatenation
                             ~prefix:[Type.Variable y]
                             ~suffix:[Type.Variable x]
                             ~concrete:[Type.literal_integer 2; Type.Variable x]
                             ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                     1 );
                 ] );
             ])
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation.create_unbounded_unpackable
                          (Type.tuple [Type.Variable x; Type.Variable y; Type.literal_integer 2])),
                     1 );
                 ] );
             ])))
    (divide_to_type
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation
                     .create_unpackable_from_concatenation_against_concatenation
                       ~compare_t:Type.compare
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[Type.literal_integer 1]
                          ~suffix:[Type.literal_integer 2]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 2]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))
                       (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                          ~prefix:[Type.literal_integer 2]
                          ~suffix:[Type.literal_integer 1]
                          ~concrete:[Type.literal_integer 2; Type.literal_integer 1]
                          ~concatenation:(Type.OrderedTypes.Concatenation.create variadic))),
                  1 );
              ] );
          ])
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 1,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unbounded_unpackable
                       (Type.tuple
                          [Type.literal_integer 1; Type.literal_integer 2; Type.literal_integer 2])),
                  1 );
              ] );
          ]));

  (* Broadcast tuple with unaries. *)
  let unary1 = Type.Variable.Unary.create ~constraints:(Bound Type.integer) "T1" in
  let unary2 = Type.Variable.Unary.create ~constraints:(Bound Type.integer) "T2" in
  let unary3 = Type.Variable.Unary.create ~constraints:(Bound Type.integer) "T3" in
  let unary4 = Type.Variable.Unary.create ~constraints:(Bound Type.integer) "T4_not_replaced" in
  let assert_replaced ~replace annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T1" -> Some (Type.TypeAlias (Type.Variable unary1))
      | "T2" -> Some (Type.TypeAlias (Type.Variable unary2))
      | "T3" -> Some (Type.TypeAlias (Type.Variable unary3))
      | "T4_not_replaced" -> Some (Type.TypeAlias (Type.Variable unary4))
      | _ -> None
    in
    let parse annotation = parse_single_expression ~preprocess:true annotation in
    assert_equal
      (Type.Variable.GlobalTransforms.Unary.replace_all
         replace
         (Type.create ~aliases (parse annotation)))
      (Type.create ~aliases (parse expected))
  in
  let replace_with_literals = function
    | variable when Type.Variable.Unary.equal variable unary1 -> Some (Type.literal_integer 5)
    | variable when Type.Variable.Unary.equal variable unary2 -> Some (Type.literal_integer 5)
    | variable when Type.Variable.Unary.equal variable unary3 -> Some (Type.literal_integer 42)
    | _ -> None
  in
  assert_replaced
    ~replace:replace_with_literals
    {|
        pyre_extensions.Broadcast[
          typing.Tuple[T1, T2],
          typing.Tuple[T2, T1],
        ]
    |}
    "typing.Tuple[typing_extensions.Literal[5], typing_extensions.Literal[5]]";
  assert_replaced
    ~replace:replace_with_literals
    {|
        pyre_extensions.Broadcast[
          typing.Tuple[T1, T2],
          typing.Tuple[T2, T3],
        ]
    |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[5], \
     typing_extensions.Literal[5]], typing.Tuple[typing_extensions.Literal[5], \
     typing_extensions.Literal[42]]]";
  assert_replaced
    ~replace:replace_with_literals
    {|
        typing.Tuple[
            typing_extensions.Literal[99],
            pyre_extensions.Unpack[
                pyre_extensions.Broadcast[
                    typing.Tuple[T1, T2],
                    typing.Tuple[T2, T1],
                ]
            ],
            typing_extensions.Literal[99],
        ]
    |}
    "typing.Tuple[typing_extensions.Literal[99], typing_extensions.Literal[5], \
     typing_extensions.Literal[5], typing_extensions.Literal[99]]";
  assert_replaced
    ~replace:replace_with_literals
    {|
        typing.Tuple[
            typing_extensions.Literal[99],
            pyre_extensions.Unpack[
                pyre_extensions.Broadcast[
                    typing.Tuple[T1, T2],
                    typing.Tuple[T2, typing_extensions.Literal[99]],
                ]
            ],
            typing_extensions.Literal[99],
        ]
    |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[5], \
     typing_extensions.Literal[5]], typing.Tuple[typing_extensions.Literal[5], \
     typing_extensions.Literal[99]]]";
  assert_replaced
    ~replace:replace_with_literals
    {|
        typing.Tuple[
            typing_extensions.Literal[99],
            pyre_extensions.Unpack[
                pyre_extensions.Broadcast[
                    typing.Tuple[T1, T2],
                    typing.Tuple[T2, T4_not_replaced],
                ]
            ],
            typing_extensions.Literal[99],
        ]
    |}
    "typing.Tuple[typing_extensions.Literal[99], \
     pyre_extensions.Unpack[pyre_extensions.Broadcast[typing.Tuple[typing_extensions.Literal[5], \
     typing_extensions.Literal[5]], typing.Tuple[typing_extensions.Literal[5], T4_not_replaced]]], \
     typing_extensions.Literal[99]]";

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
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let assert_replaced ~replace annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None
    in
    assert_equal
      (Type.Variable.GlobalTransforms.TupleVariadic.replace_all
         replace
         (Type.create ~aliases (parse_single_expression ~preprocess:true annotation)))
      (Type.create ~aliases (parse_single_expression ~preprocess:true expected))
  in
  (* Variadic tuples. *)
  let replace_with_concrete given =
    Option.some_if
      (Type.Variable.Variadic.Tuple.equal given variadic)
      (Type.OrderedTypes.Concrete [Type.bool; Type.bool])
  in
  let replace_with_concatenation _ =
    Some
      (Type.OrderedTypes.Concatenation
         (Type.OrderedTypes.Concatenation.create ~prefix:[Type.bool] ~suffix:[Type.bool] variadic))
  in
  assert_replaced
    ~replace:replace_with_concrete
    "Foo[int, pyre_extensions.Unpack[Ts], str]"
    "Foo[int, bool, bool, str]";
  assert_replaced
    ~replace:replace_with_concatenation
    "Foo[int, pyre_extensions.Unpack[Ts], str]"
    "Foo[int, bool, pyre_extensions.Unpack[Ts], bool, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "Foo[Bar[int, pyre_extensions.Unpack[Ts], str], Bar[int, pyre_extensions.Unpack[Ts2], str]]"
    "Foo[Bar[int, bool, bool, str], Bar[int, pyre_extensions.Unpack[Ts2], str]]";
  assert_replaced ~replace:replace_with_concrete "typing.Tuple[int, str]" "typing.Tuple[int, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"
    "typing.Tuple[int, bool, bool, str]";
  assert_replaced
    ~replace:replace_with_concatenation
    "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"
    "typing.Tuple[int, bool, pyre_extensions.Unpack[Ts], bool, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "typing.Callable[[int, pyre_extensions.Unpack[Ts], str], None]"
    "typing.Callable[[int, bool, bool, str], None]";
  assert_replaced
    ~replace:replace_with_concatenation
    "typing.Callable[[int, pyre_extensions.Unpack[Ts], str], None]"
    "typing.Callable[[int, bool, pyre_extensions.Unpack[Ts], bool, str], None]";

  (* Broadcasts. *)
  let replace_with_concrete = function
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic ->
        Some (Type.OrderedTypes.Concrete [Type.literal_integer 5])
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic2 ->
        Some (Type.OrderedTypes.Concrete [Type.literal_integer 1; Type.literal_integer 4])
    | _ -> None
  in
  let replace_with_concatenation = function
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic ->
        Some
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer))
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic2 ->
        Some (Type.OrderedTypes.Concatenation (Type.OrderedTypes.Concatenation.create variadic))
    | _ -> None
  in
  (* Concrete against concatenation. *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[pyre_extensions.Unpack[Ts]],
        typing.Tuple[typing_extensions.Literal[5]],
      ]
    |}
    "typing.Tuple[typing_extensions.Literal[5]]";
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[pyre_extensions.Unpack[Ts]],
        typing.Tuple[typing_extensions.Literal[5]],
      ]
    |}
    "typing.Tuple[int, ...]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[
          typing_extensions.Literal[2],
          typing_extensions.Literal[1],
        ],
        typing.Tuple[pyre_extensions.Unpack[Ts]],
      ]
    |}
    {|
      typing.Tuple[
        typing_extensions.Literal[2],
        typing_extensions.Literal[5],
      ]
    |};
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[
          typing_extensions.Literal[2],
          typing_extensions.Literal[2],
        ],
        typing.Tuple[pyre_extensions.Unpack[Ts]],
      ]
    |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[2]], typing.Tuple[typing_extensions.Literal[5]]]";

  (* Concatenation against concatenation. *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[pyre_extensions.Unpack[Ts]],
        ]
    |}
    "typing.Tuple[typing_extensions.Literal[5]]";
  assert_replaced
    ~replace:replace_with_concatenation
    {|
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[pyre_extensions.Unpack[Ts]],
        ]
    |}
    "typing.Tuple[int, ...]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[pyre_extensions.Unpack[Ts]],
        typing.Tuple[pyre_extensions.Unpack[Ts2]],
      ]
  |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[1], \
     typing_extensions.Literal[4]], typing.Tuple[typing_extensions.Literal[5]]]";

  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[
          typing_extensions.Literal[6],
          typing_extensions.Literal[1],
          typing_extensions.Literal[5],
        ],
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[pyre_extensions.Unpack[Ts2], typing_extensions.Literal[5]],
        ]
      ]
    |}
    {|
      typing.Tuple[
        typing_extensions.Literal[6],
        typing_extensions.Literal[4],
        typing_extensions.Literal[5],
      ]
    |};
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[
          typing_extensions.Literal[6],
          typing_extensions.Literal[1],
          typing_extensions.Literal[5],
        ],
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[pyre_extensions.Unpack[Ts2]],
        ]
      ]
    |}
    {|
      pyre_extensions.Broadcast[
        typing.Tuple[
          typing_extensions.Literal[6],
          typing_extensions.Literal[1],
          typing_extensions.Literal[5]
        ],
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[int, ...]
        ]
      ]
    |};

  (* Parametric *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
      Foo[
        int,
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[
              typing_extensions.Literal[2],
              typing_extensions.Literal[1],
            ],
            typing.Tuple[pyre_extensions.Unpack[Ts]],
          ]
        ]
      ]
    |}
    "Foo[int, typing_extensions.Literal[2], typing_extensions.Literal[5]]";
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      Foo[
        int,
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[
              typing_extensions.Literal[2],
              typing_extensions.Literal[1],
            ],
            typing.Tuple[pyre_extensions.Unpack[Ts]],
          ]
        ]
      ]
    |}
    "Foo[int, pyre_extensions.Unpack[typing.Tuple[int, ...]]]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      Foo[
        int,
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[
              typing_extensions.Literal[2],
              typing_extensions.Literal[2],
            ],
            typing.Tuple[pyre_extensions.Unpack[Ts]],
          ]
        ]
      ]
    |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[2]], typing.Tuple[typing_extensions.Literal[5]]]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      List[
        Tensor[
          int,
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[
                typing_extensions.Literal[2],
                typing_extensions.Literal[2],
              ],
              typing.Tuple[pyre_extensions.Unpack[Ts]],
            ]
          ]
        ]
      ]
    |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[2]], typing.Tuple[typing_extensions.Literal[5]]]";

  (* Callable *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
      typing.Callable[
        [
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[1]],
              typing.Tuple[pyre_extensions.Unpack[Ts]],
            ]
          ]
        ],
        int
      ]
    |}
    {|
      typing.Callable[
        [typing_extensions.Literal[2], typing_extensions.Literal[5]],
        int
      ]
    |};
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      typing.Callable[
        [
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[1]],
              typing.Tuple[pyre_extensions.Unpack[Ts]],
            ]
          ]
        ],
        int
      ]
    |}
    {|
      typing.Callable[
        [pyre_extensions.Unpack[typing.Tuple[int, ...]]],
        int
      ]
    |};
  assert_replaced
    ~replace:replace_with_concrete
    {|
      typing.Callable[
        [
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
              typing.Tuple[pyre_extensions.Unpack[Ts]],
            ]
          ]
        ],
        int
      ]
    |}
    {|
      pyre_extensions.BroadcastError[
        typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
        typing.Tuple[typing_extensions.Literal[5]]
      ]
    |};
  assert_replaced
    ~replace:replace_with_concrete
    {|
      typing.Callable[
        [
          Named(
            x,
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
              typing.Tuple[pyre_extensions.Unpack[Ts]],
            ])
        ],
        int
      ]
    |}
    {|
      typing.Callable[
        [Named(
          x,
          pyre_extensions.BroadcastError[
            typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
            typing.Tuple[typing_extensions.Literal[5]]
          ]
        )],
        int
      ]
    |};
  assert_replaced
    ~replace:replace_with_concrete
    {|
      typing.Callable[
        [
          pyre_extensions.Broadcast[
            typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
            typing.Tuple[pyre_extensions.Unpack[Ts]],
          ]
        ],
        int
      ]
    |}
    {|
      typing.Callable[
        [pyre_extensions.BroadcastError[
          typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
          typing.Tuple[typing_extensions.Literal[5]]
        ]],
        int
      ]
    |};

  let parse_string string =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None
    in
    Type.create ~aliases (parse_single_expression ~preprocess:true string)
  in
  let replace_with_concrete = function
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic ->
        Some
          (Type.OrderedTypes.Concrete
             [
               parse_string "typing.Callable[[int], str]";
               parse_string "typing.Callable[[str], bool]";
             ])
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic2 ->
        Some
          (Type.OrderedTypes.Concrete
             [
               parse_string "typing.Callable[[float], bytes]";
               parse_string "typing.Callable[[bytes], int]";
             ])
    | _ -> None
  in
  let replace_with_concatenation = function
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic ->
        Some
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                (parse_string "typing.Callable[[int], int]")))
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic2 ->
        Some (Type.OrderedTypes.Concatenation (Type.OrderedTypes.Concatenation.create variadic))
    | _ -> None
  in

  (* Compose. *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Compose[
        typing.Callable[[float], int],
        pyre_extensions.Unpack[Ts]
      ]
    |}
    "pyre_extensions.Compose[typing.Callable[[float], int], typing.Callable[[int], str], \
     typing.Callable[[str], bool]]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          pyre_extensions.Unpack[Ts],
          typing.Callable[[bool], bool]
        ],
        typing.Callable[[bool], float],
      ]
    |}
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], str],
        typing.Callable[[str], bool],
        typing.Callable[[bool], bool],
        typing.Callable[[bool], float]
      ]
    |};
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        pyre_extensions.Unpack[Ts]
      ]
    |}
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        pyre_extensions.Unpack[
          typing.Tuple[
            typing.Callable[[int], int],
            ...
          ]
        ]
      ]
    |};
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        pyre_extensions.Unpack[Ts2]
      ]
    |}
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        pyre_extensions.Unpack[Ts]
      ]
    |};
  ()


let test_product_replace_variadic _ =
  let assert_equal actual expected =
    assert_equal ~cmp:Type.equal ~printer:Type.show expected actual
  in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let variable = Type.Variable.Unary.create "N" in
  let aliases ?replace_unbound_parameters_with_any:_ = function
    | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
    | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
    | "N" -> Some (TypeAlias (Type.Variable variable))
    | _ -> None
  in
  let assert_replaced_type ~replace annotation expected =
    assert_equal
      (Type.Variable.GlobalTransforms.TupleVariadic.replace_all
         replace
         (Type.create ~aliases (parse_single_expression ~preprocess:true annotation)))
      expected
  in
  let assert_replaced ~replace annotation expected =
    assert_replaced_type
      ~replace
      annotation
      (Type.create ~aliases (parse_single_expression ~preprocess:true expected))
  in
  let replace_with_concrete given =
    Option.some_if
      (Type.Variable.Variadic.Tuple.equal given variadic)
      (Type.OrderedTypes.Concrete [Type.literal_integer 2; Type.literal_integer 3])
  in
  let replace_with_concatenation given =
    Option.some_if
      (Type.Variable.Variadic.Tuple.equal given variadic)
      (Type.OrderedTypes.Concatenation
         (Type.OrderedTypes.Concatenation.create
            ~prefix:[Type.literal_integer 2]
            ~suffix:[]
            variadic2))
  in
  assert_replaced
    ~replace:replace_with_concrete
    "pyre_extensions.Product[pyre_extensions.Unpack[Ts]]"
    "typing_extensions.Literal[6]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          typing_extensions.Literal[2],
          pyre_extensions.Unpack[Ts],
          typing_extensions.Literal[4]
        ]
      |}
    "typing_extensions.Literal[48]";
  assert_replaced
    ~replace:replace_with_concatenation
    {|
        pyre_extensions.Product[
          typing_extensions.Literal[2],
          pyre_extensions.Unpack[Ts],
          typing_extensions.Literal[4]
        ]
      |}
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[16],
        pyre_extensions.Unpack[Ts2]
      ]
    |};
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[Ts],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts],
            typing_extensions.Literal[1]
          ]
        ]
      |}
    "typing_extensions.Literal[36]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[1]],
              typing.Tuple[pyre_extensions.Unpack[Ts]]
            ]
          ]
        ]
      |}
    "typing_extensions.Literal[6]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[
            typing.Tuple[
              pyre_extensions.Product[
                pyre_extensions.Unpack[
                  pyre_extensions.Broadcast[
                    typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[1]],
                    typing.Tuple[pyre_extensions.Unpack[Ts]]
                  ]
                ]
              ],
              ...
            ]
          ]
        ]
      |}
    "int";
  let replace_with_ones given =
    Option.some_if
      (Type.Variable.Variadic.Tuple.equal given variadic)
      (Type.OrderedTypes.Concrete [Type.literal_integer 1; Type.literal_integer 1])
  in
  assert_replaced
    ~replace:replace_with_ones
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[
            typing.Tuple[
              pyre_extensions.Product[
                pyre_extensions.Unpack[
                  pyre_extensions.Broadcast[
                    typing.Tuple[typing_extensions.Literal[1], typing_extensions.Literal[1]],
                    typing.Tuple[pyre_extensions.Unpack[Ts]]
                  ]
                ]
              ],
              ...
            ]
          ]
        ]
      |}
    "int";
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
              typing.Tuple[pyre_extensions.Unpack[Ts]]
            ]
          ]
        ]
      |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[2]], typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[3]]]";
  assert_replaced
    ~replace:replace_with_concatenation
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
            typing.Tuple[pyre_extensions.Unpack[Ts]]
          ]
        ]
      ]
    |}
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
            typing.Tuple[
              typing_extensions.Literal[2], pyre_extensions.Unpack[Ts2]
            ]
          ]
        ]
      ]
    |};
  assert_replaced_type
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[Ts2],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2]
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts]
          ]
        ]
      |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [
            ( 6,
              [
                ( Type.Monomial.create_product
                    (Type.OrderedTypes.Concatenation.create_unpackable variadic2),
                  2 );
              ] );
          ]));
  let replace_with_both = function
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic ->
        Some (Type.OrderedTypes.Concrete [Type.literal_integer 1; Type.literal_integer 4])
    | variable when Type.Variable.Variadic.Tuple.equal variable variadic2 ->
        Some (Type.OrderedTypes.Concrete [Type.literal_integer 5; Type.literal_integer 2])
    | _ -> None
  in
  assert_replaced_type
    ~replace:replace_with_both
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[Ts2],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2]
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts]
          ]
        ]
    |}
    (Type.literal_integer 400);
  assert_replaced_type
    ~replace:replace_with_both
    {|
        pyre_extensions.Add[
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2],
            pyre_extensions.Product[
              pyre_extensions.Unpack[Ts2]
            ],
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2],
            pyre_extensions.Product[
              pyre_extensions.Unpack[Ts2]
            ],
          ]
        ]
    |}
    (Type.literal_integer 200);
  assert_replaced_type
    ~replace:replace_with_both
    {|
        pyre_extensions.Add[
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2],
            pyre_extensions.Product[
              pyre_extensions.Unpack[Ts2]
            ],
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2],
            pyre_extensions.Product[
              pyre_extensions.Unpack[Ts2]
            ],
          ]
        ]
    |}
    (Type.literal_integer 200);
  assert_replaced_type
    ~replace:replace_with_both
    {|
        pyre_extensions.Product[
          N,
          pyre_extensions.Unpack[Ts2],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2]
          ]
        ]
    |}
    (Type.IntExpression.create
       (Type.Polynomial.create_from_monomial_variables_list
          ~compare_t:Type.compare
          [100, [Type.Monomial.create_variable variable, 1]]));
  assert_replaced
    ~replace:replace_with_concrete
    {|
        pyre_extensions.Product[
          pyre_extensions.Unpack[
            pyre_extensions.Broadcast[
              typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[2]],
              typing.Tuple[pyre_extensions.Unpack[Ts]]
            ]
          ]
        ]
      |}
    "pyre_extensions.BroadcastError[typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[2]], typing.Tuple[typing_extensions.Literal[2], \
     typing_extensions.Literal[3]]]";
  assert_replaced
    ~replace:replace_with_both
    {|
        pyre_extensions.Divide[
          pyre_extensions.Product[
            pyre_extensions.Unpack[
              pyre_extensions.Broadcast[
                typing.Tuple[typing_extensions.Literal[3], typing_extensions.Literal[4]],
                typing.Tuple[pyre_extensions.Unpack[Ts]]
              ]
            ]
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2]
          ]
        ]
      |}
    "typing_extensions.Literal[1]";
  assert_replaced
    ~replace:replace_with_both
    (* Prod[Prod[*BC[Tuple[2, 4], Tuple[*Ts]]] // 4, 8] // Prod[*Ts2]) *)
    {|
        pyre_extensions.Divide[
          pyre_extensions.Product[
            pyre_extensions.Divide[
              pyre_extensions.Product[
                pyre_extensions.Unpack[
                  pyre_extensions.Broadcast[
                    typing.Tuple[typing_extensions.Literal[2], typing_extensions.Literal[4]],
                    typing.Tuple[pyre_extensions.Unpack[Ts]]
                  ]
                ]
              ],
              typing_extensions.Literal[4]
            ],
            typing_extensions.Literal[8]
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2]
          ]
        ]
      |}
    "typing_extensions.Literal[1]";
  ()


let test_less_or_equal _ =
  let assert_solve ~left ~right expected =
    let impossible = [] in
    let rec solve ~left ~right =
      match left, right with
      | Type.Variable variable1, Type.Variable variable2 ->
          List.sort
            [[variable1, Type.Variable variable2]; [variable2, Type.Variable variable1]]
            ~compare:[%compare: (Type.Variable.Unary.t * Type.t) list]
      | Type.Variable variable, bound
      | bound, Type.Variable variable ->
          [[variable, bound]]
      | IntExpression _, _
      | _, IntExpression _ ->
          Type.solve_less_or_equal_polynomial ~left ~right ~solve ~impossible
      | _ when Type.equal left right -> [[]]
      | _ -> impossible
    in

    assert_equal
      ~cmp:[%equal: (Type.t Type.Variable.Unary.record * Type.t) list list]
      ~printer:[%show: (Type.t Type.Variable.Unary.record * Type.t) list list]
      expected
      (solve ~left ~right);
    assert_equal
      ~cmp:[%equal: (Type.t Type.Variable.Unary.record * Type.t) list list]
      ~printer:[%show: (Type.t Type.Variable.Unary.record * Type.t) list list]
      expected
      (solve ~left:right ~right:left)
  in
  let assert_impossible ~left ~right = assert_solve ~left ~right [] in
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  let variable_list_to_type variable_list =
    polynomial_create_from_variables_list variable_list |> Type.polynomial_to_type
  in
  let divide_to_type numerator denominator =
    Type.IntExpression.create (Type.Polynomial.divide ~compare_t:Type.compare numerator denominator)
  in
  let very_complicated_type =
    Type.tuple
      [
        Type.Any;
        Type.Union [Type.Bottom; Type.bool];
        Type.Variable x;
        variable_list_to_type [3, [x, 2; y, 1; z, 42]];
      ]
  in
  assert_solve
    ~left:(Type.literal_integer 6)
    ~right:(variable_list_to_type [1, []; 1, [x, 1]])
    [[x, Type.literal_integer 5]];
  assert_solve
    ~left:(Type.literal_integer 1)
    ~right:(variable_list_to_type [6, []; 1, [x, 1]])
    [[x, Type.literal_integer (-5)]];
  assert_impossible
    ~left:(variable_list_to_type [1, [x, 2]])
    ~right:(variable_list_to_type [1, [x, 3]]);

  assert_solve ~left:very_complicated_type ~right:(Type.Variable x) [[x, very_complicated_type]];

  assert_impossible ~left:(Type.literal_integer 5) ~right:very_complicated_type;

  assert_impossible
    ~left:(variable_list_to_type [1, [x, 1]; 1, [y, 1]])
    ~right:(Type.literal_integer 5);

  assert_impossible ~left:(variable_list_to_type [1, [x, 1; y, 1]]) ~right:(Type.literal_integer 5);

  assert_solve
    ~left:(variable_list_to_type [6, [x, 1]])
    ~right:(Type.literal_integer 18)
    [[x, Type.literal_integer 3]];

  assert_solve
    ~left:(variable_list_to_type [2, []; 4, [x, 1]])
    ~right:(Type.literal_integer 18)
    [[x, Type.literal_integer 4]];

  assert_impossible ~left:(variable_list_to_type [1, [x, 2]]) ~right:(Type.literal_integer 9);

  assert_solve
    ~left:(variable_list_to_type [1, [x, 1]])
    ~right:(variable_list_to_type [1, [y, 1]])
    [[x, Type.Variable y]; [y, Type.Variable x]];
  assert_impossible
    ~left:(variable_list_to_type [2, []; 3, [x, 1]])
    ~right:(variable_list_to_type [2, []; 9, [y, 1]]);

  assert_impossible
    ~left:
      (divide_to_type
         (polynomial_create_from_variables_list [1, [x, 1]])
         (Type.Polynomial.create_from_int 7))
    ~right:
      (divide_to_type
         (polynomial_create_from_variables_list [1, [y, 1]])
         (Type.Polynomial.create_from_int 7));
  assert_impossible
    ~left:
      (divide_to_type
         (Type.Polynomial.create_from_int 1)
         (polynomial_create_from_variables_list [3, [x, 1]]))
    ~right:
      (divide_to_type
         (Type.Polynomial.create_from_int 1)
         (polynomial_create_from_variables_list [3, [x, 1]]));
  ()


let test_collect_all _ =
  let free_variable = Type.Variable (Type.Variable.Unary.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.collect_all annotation)
    [Type.Variable.Unary.create "T"];
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.collect_all
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    [Type.Variable.Unary.create "T"];
  let unary1 = Type.Variable.Unary.create "T1" in
  let unary2 = Type.Variable.Unary.create "T2" in
  let assert_collected annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T1" -> Some (Type.TypeAlias (Type.Variable unary1))
      | "T2" -> Some (Type.TypeAlias (Type.Variable unary2))
      | _ -> None
    in
    assert_equal
      ~printer:[%show: Type.Variable.Unary.t list]
      expected
      (Type.Variable.GlobalTransforms.Unary.collect_all
         (Type.create ~aliases (parse_single_expression ~preprocess:true annotation)))
  in
  assert_collected
    {|
      typing.Tuple[
        pyre_extensions.Broadcast[
          typing.Tuple[T1, int],
          typing.Tuple[T2, int],
        ],
      ]
    |}
    [unary1; unary2];

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

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let assert_collected annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
      | _ -> None
    in
    assert_equal
      ~printer:[%show: Type.Variable.Variadic.Tuple.t list]
      expected
      (Type.Variable.GlobalTransforms.TupleVariadic.collect_all
         (Type.create ~aliases (parse_single_expression ~preprocess:true annotation)))
  in
  assert_collected "typing.Tuple[int, str]" [];
  assert_collected "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]" [variadic];
  assert_collected "Foo[int, pyre_extensions.Unpack[Ts], str]" [variadic];
  assert_collected
    "Foo[Bar[int, pyre_extensions.Unpack[Ts], str], Baz[pyre_extensions.Unpack[Ts2]]]"
    [variadic; variadic2];
  assert_collected
    "typing.Callable[[Variable(int, pyre_extensions.Unpack[Ts], str)], typing.Tuple[int, \
     pyre_extensions.Unpack[Ts2], str]]"
    [variadic2; variadic];
  assert_collected
    "typing.Callable[[Variable(int, pyre_extensions.Unpack[Ts], str)], \
     typing.Callable[[Variable(int, pyre_extensions.Unpack[Ts2], str)], bool]]"
    [variadic2; variadic];
  assert_collected
    {|
      typing.Tuple[
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[typing_extensions.Literal[1]],
        ],
      ]
    |}
    [variadic];
  assert_collected
    {|
      typing.Tuple[
        pyre_extensions.Broadcast[
          typing.Tuple[int, ...],
          typing.Tuple[typing_extensions.Literal[1]],
        ],
      ]
    |}
    [];
  assert_collected
    {|
      typing.Tuple[
        pyre_extensions.Broadcast[
          typing.Tuple[pyre_extensions.Unpack[Ts]],
          typing.Tuple[pyre_extensions.Unpack[Ts2]],
        ],
      ]
    |}
    [variadic2; variadic];
  assert_collected
    {|
      typing.Tuple[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[
              pyre_extensions.Unpack[
                pyre_extensions.Broadcast[
                  typing.Tuple[typing_extensions.Literal[1], typing_extensions.Literal[2]],
                  typing.Tuple[pyre_extensions.Unpack[Ts]]
                ]
              ]
            ],
            typing.Tuple[pyre_extensions.Unpack[Ts2]]
          ]
        ]
      ]
    |}
    [variadic; variadic2];
  assert_collected
    {|
      typing.Callable[
        [pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            pyre_extensions.Broadcast[
              typing.Tuple[pyre_extensions.Unpack[Ts]],
              typing.Tuple[pyre_extensions.Unpack[Ts2]]
            ]
          ]
        ]],
        str
      ]
    |}
    [variadic2; variadic; variadic];
  assert_collected
    {|
      Foo[
        pyre_extensions.Unpack[
          pyre_extensions.Broadcast[
            typing.Tuple[pyre_extensions.Unpack[Ts]],
            pyre_extensions.Broadcast[
              typing.Tuple[pyre_extensions.Unpack[Ts]],
              typing.Tuple[pyre_extensions.Unpack[Ts2]]
            ]
          ]
        ]
      ]
    |}
    [variadic2; variadic; variadic];

  (* Compose. *)
  assert_collected
    {|
      pyre_extensions.Compose[
        pyre_extensions.Unpack[Ts],
        typing.Callable[[int], int]
      ]
    |}
    [variadic];
  assert_collected
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          typing.Callable[[int], int],
          pyre_extensions.Unpack[Ts],
        ],
        typing.Callable[[int], int]
      ]
    |}
    [variadic];

  (* Product. *)
  let variable = Type.Variable.Unary.create "T" in
  assert_equal
    (Type.Variable.GlobalTransforms.Unary.collect_all
       (Type.IntExpression.create
          (Type.Polynomial.create_from_monomial_variables_list
             ~compare_t:Type.compare
             [
               ( 1,
                 [
                   ( Type.Monomial.create_product
                       (Type.OrderedTypes.Concatenation.create_unbounded_unpackable
                          (Type.Variable variable)),
                     1 );
                 ] );
             ])))
    [variable];
  assert_collected
    {|
      pyre_extensions.Product[
        pyre_extensions.Unpack[Ts],
        typing_extensions.Literal[2]
      ]
    |}
    [variadic];
  assert_collected
    {|
      pyre_extensions.Product[
        typing_extensions.Literal[2],
        typing_extensions.Literal[3]
      ]
    |}
    [];
  assert_collected
    {|
      pyre_extensions.Product[
        pyre_extensions.Product[
          pyre_extensions.Unpack[Ts],
          typing_extensions.Literal[2]
        ],
        pyre_extensions.Product[
          pyre_extensions.Unpack[Ts],
          typing_extensions.Literal[3]
        ]
      ]
    |}
    [variadic];
  assert_collected
    {|
      pyre_extensions.Product[
        pyre_extensions.Divide[
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts],
            typing_extensions.Literal[2]
          ],
          pyre_extensions.Product[
            pyre_extensions.Unpack[Ts2],
            typing_extensions.Literal[3]
          ]
        ]
      ]
    |}
    [variadic2; variadic];
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
    "pyre_extensions.TypeVarTuple('Ts')"
    (Type.Variable.TupleVariadic (Type.Variable.Variadic.Tuple.create "target"));
  assert_declaration_does_not_parse "pyre_extensions.TypeVarTuple('Ts', covariant=True)";
  ()


let test_starred_annotation_expression _ =
  let assert_starred_expression concatenation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:(fun left right -> Expression.location_insensitive_compare left right = 0)
      (parse_single_expression ~coerce_special_methods:true expression)
      (Type.OrderedTypes.to_starred_annotation_expression ~expression:Type.expression concatenation)
  in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_starred_expression
    (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic)
    "pyre_extensions.Unpack[Ts]";
  assert_starred_expression
    (Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] ~suffix:[Type.string] variadic)
    "pyre_extensions.Unpack[(int, pyre_extensions.Unpack[Ts], str)]";
  ()


let test_concatenation_from_unpack_expression _ =
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let assert_concatenation expression concatenation =
    let parse_annotation expression =
      let aliases ?replace_unbound_parameters_with_any:_ = function
        | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
        | _ -> None
      in
      Type.create ~aliases (parse_single_expression ~preprocess:true (Expression.show expression))
    in
    assert_equal
      ~printer:[%show: Type.t Type.OrderedTypes.Concatenation.t option]
      ~cmp:[%equal: Type.t Type.OrderedTypes.Concatenation.t option]
      concatenation
      (Type.OrderedTypes.concatenation_from_unpack_expression
         ~parse_annotation
         (parse_single_expression ~coerce_special_methods:true expression))
  in
  assert_concatenation
    "pyre_extensions.Unpack[Ts]"
    (Some (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic));
  assert_concatenation
    "pyre_extensions.Unpack[typing.Tuple[int, pyre_extensions.Unpack[Ts], str]]"
    (Some
       (Type.OrderedTypes.Concatenation.create
          ~prefix:[Type.integer]
          ~suffix:[Type.string]
          variadic));
  assert_concatenation "int" None;
  ()


let test_broadcast _ =
  let assert_broadcast left_type right_type expected =
    (* Broadcast is a commutative operator. *)
    assert_equal
      ~printer:[%show: Type.t]
      ~cmp:[%equal: Type.t]
      (Type.OrderedTypes.broadcast left_type right_type)
      expected;
    assert_equal
      ~printer:[%show: Type.t]
      ~cmp:[%equal: Type.t]
      (Type.OrderedTypes.broadcast right_type left_type)
      expected
  in
  let literal_tuple input = Type.tuple (List.map ~f:Type.literal_integer input) in
  let unbounded_int =
    Type.Tuple
      (Concatenation (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.integer))
  in
  let unbounded_any =
    Type.Tuple
      (Concatenation (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.Any))
  in
  let x =
    Type.Variable
      (Type.Variable.Unary.create
         ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int"))
         "x")
  in
  let y =
    Type.Variable
      (Type.Variable.Unary.create
         ~constraints:(Type.Record.Variable.Bound (Type.Primitive "int"))
         "y")
  in
  let not_bound_to_int = Type.Variable (Type.Variable.Unary.create "not_bound_to_int") in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let variadic_t = Type.OrderedTypes.Concatenation.create variadic in
  let variadic2_t = Type.OrderedTypes.Concatenation.create variadic2 in
  let x_and_unbounded_int =
    Type.OrderedTypes.Concatenation.create_from_unbounded_element ~prefix:[x] Type.integer
  in
  let x_and_variadic = Type.OrderedTypes.Concatenation.create ~prefix:[x] variadic in
  let broadcast_error left right =
    Type.Parametric
      {
        name = "pyre_extensions.BroadcastError";
        parameters = [Type.Parameter.Single left; Type.Parameter.Single right];
      }
  in
  (* Basic *)
  assert_broadcast (literal_tuple [1]) (literal_tuple [5]) (literal_tuple [5]);
  assert_broadcast
    (literal_tuple [5])
    (literal_tuple [3])
    (broadcast_error (literal_tuple [3]) (literal_tuple [5]));
  assert_broadcast (Type.tuple []) (literal_tuple [5]) (literal_tuple [5]);

  (* Any *)
  assert_broadcast (literal_tuple [1; 3]) Type.Any Any;
  assert_broadcast
    (literal_tuple [1; 3])
    (Type.tuple [Any; Type.literal_integer 1])
    (Type.tuple [Any; Type.literal_integer 3]);
  assert_broadcast (literal_tuple [1; 3]) unbounded_any unbounded_any;

  (* Integers *)
  assert_broadcast Type.integer (literal_tuple [1; 3]) Bottom;
  assert_broadcast
    (Type.tuple [Type.literal_integer 1; Type.integer])
    (literal_tuple [1; 3])
    (Type.tuple [Type.literal_integer 1; Type.integer]);
  assert_broadcast (literal_tuple [1; 3]) unbounded_int unbounded_int;
  assert_broadcast (Type.tuple [Type.string]) unbounded_int Bottom;

  (* Broadcast[Tuple[x, *Ts], Tuple[x, *Ts]] *)
  assert_broadcast
    (Type.Tuple (Concatenation x_and_variadic))
    (Type.Tuple (Concatenation x_and_variadic))
    (Type.Tuple (Concatenation x_and_variadic));
  (* Broadcast[Tuple[x, *Ts], Tuple[x, *Ts2]] *)
  assert_broadcast
    (Type.Tuple (Concatenation x_and_variadic))
    (Type.Tuple (Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[x] variadic2)))
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
             ~compare_t:Type.compare
             x_and_variadic
             (Type.OrderedTypes.Concatenation.create ~prefix:[x] variadic2))));
  (* Broadcast[Tuple[x, *Tuple[int, ...]], Tuple[x, *Tuple[int, ...]]] *)
  assert_broadcast
    (Type.Tuple (Concatenation x_and_unbounded_int))
    (Type.Tuple (Concatenation x_and_unbounded_int))
    (Type.Tuple (Concatenation x_and_unbounded_int));
  (* Broadcast[Tuple[x, *Tuple[int, ...]], Tuple[x, *Tuple[Any, ...]]] *)
  assert_broadcast
    (Type.Tuple (Concatenation x_and_unbounded_int))
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_unbounded_element ~prefix:[x] Type.Any)))
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
             ~compare_t:Type.compare
             x_and_unbounded_int
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element ~prefix:[x] Type.Any))));
  let first_concrete_against_concatenation =
    Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
      ~prefix:[x]
      ~suffix:[]
      ~concrete:[y]
      ~concatenation:variadic_t
  in
  let second_concrete_against_concatenation =
    Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
      ~prefix:[y]
      ~suffix:[]
      ~concrete:[y]
      ~concatenation:variadic_t
  in
  (* Broadcast[ Tuple[x, *Broadcast[Tuple[y], Tuple[*Ts]], Tuple[x, *Broadcast[Tuple[y],
     Tuple[*Ts]]] ] *)
  assert_broadcast
    (Type.Tuple (Concatenation first_concrete_against_concatenation))
    (Type.Tuple (Concatenation first_concrete_against_concatenation))
    (Type.Tuple (Concatenation first_concrete_against_concatenation));
  (* Broadcast[ Tuple[x, *Broadcast[Tuple[y], Tuple[*Ts]], Tuple[y, *Broadcast[Tuple[y],
     Tuple[*Ts]]] ] *)
  assert_broadcast
    (Type.Tuple (Concatenation first_concrete_against_concatenation))
    (Type.Tuple (Concatenation second_concrete_against_concatenation))
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
             ~compare_t:Type.compare
             first_concrete_against_concatenation
             second_concrete_against_concatenation)));
  let first_concatenation_against_concatenation =
    Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
      ~prefix:[x]
      ~suffix:[]
      ~compare_t:Type.compare
      variadic_t
      variadic_t
  in
  let second_concatenation_against_concatenation =
    Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
      ~prefix:[x]
      ~suffix:[]
      ~compare_t:Type.compare
      variadic_t
      variadic2_t
  in
  assert_broadcast
    (Type.Tuple (Concatenation first_concatenation_against_concatenation))
    (Type.Tuple (Concatenation second_concatenation_against_concatenation))
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concatenation_against_concatenation
             ~compare_t:Type.compare
             first_concatenation_against_concatenation
             second_concatenation_against_concatenation)));

  (* Variables *)
  assert_broadcast (Type.tuple [x]) (literal_tuple [1]) (Type.tuple [x]);
  assert_broadcast (Type.tuple [x]) (Type.tuple [x]) (Type.tuple [x]);

  (* Broadcast sorts the tuples so that it is commutative. *)
  assert_broadcast
    (Type.tuple [x; y])
    (Type.tuple [y; x])
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             ~left:[y; x]
             ~right:[x; y])));
  assert_broadcast
    (Type.tuple [x])
    (Type.tuple [Type.literal_integer 5])
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create_from_concrete_against_concrete
             ~prefix:[]
             ~suffix:[]
             ~compare_t:Type.compare
             ~left:[x]
             ~right:[Type.literal_integer 5])));
  assert_broadcast
    (Type.tuple [not_bound_to_int])
    (Type.tuple [not_bound_to_int])
    (broadcast_error (Type.tuple [not_bound_to_int]) (Type.tuple [not_bound_to_int]));

  (* Literals *)
  let left, right = literal_tuple [1; 3; 5], literal_tuple [1; 3] in
  assert_broadcast left right (broadcast_error right left);
  assert_broadcast (literal_tuple [1; 3; 5]) (literal_tuple [5; 3; 1]) (literal_tuple [5; 3; 5]);
  assert_broadcast (literal_tuple [1; 3; 5]) (literal_tuple [3; 5]) (literal_tuple [1; 3; 5]);
  assert_broadcast (literal_tuple [5; 3; 1]) (literal_tuple [3; 1]) (literal_tuple [5; 3; 1]);
  ()


let test_split_ordered_types _ =
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let assert_split ?(split_both_ways = true) left right expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None
    in
    let left =
      match
        Type.create ~aliases (parse_single_expression ~preprocess:true ("typing.Tuple" ^ left))
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple elements"
    in
    let right =
      match
        Type.create ~aliases (parse_single_expression ~preprocess:true ("typing.Tuple" ^ right))
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple elements"
    in
    assert_equal
      ~printer:[%show: Type.t Type.OrderedTypes.ordered_type_split option]
      expected
      (Type.OrderedTypes.split_matching_elements_by_length left right);
    if split_both_ways then
      let flip_splits { Type.Record.OrderedTypes.prefix_pairs; middle_pair; suffix_pairs } =
        let swap (a, b) = b, a in
        {
          Type.Record.OrderedTypes.prefix_pairs = List.map prefix_pairs ~f:swap;
          middle_pair = swap middle_pair;
          suffix_pairs = List.map suffix_pairs ~f:swap;
        }
      in
      assert_equal
        ~printer:[%show: Type.t Type.OrderedTypes.ordered_type_split option]
        (expected >>| flip_splits)
        (Type.OrderedTypes.split_matching_elements_by_length right left)
  in
  let open Type.OrderedTypes in
  assert_split
    "[int, str]"
    "[int, str]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split
    "[int, str, bool, int, str]"
    "[int, pyre_extensions.Unpack[Ts], int, str]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concrete [Type.string; Type.bool],
             Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic) );
         suffix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
       });
  (* Not enough elements. *)
  assert_split "[int]" "[int, str, pyre_extensions.Unpack[Ts]]" None;
  assert_split "[str]" "[pyre_extensions.Unpack[Ts], int, str]" None;
  assert_split "[int, int]" "[int, pyre_extensions.Unpack[Ts], int, str]" None;
  assert_split "[int, int]" "[int, pyre_extensions.Unpack[Ts], int, str]" None;
  (* *Ts can match against zero elements. *)
  assert_split
    "[int, int, str]"
    "[int, pyre_extensions.Unpack[Ts], int, str]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concrete [],
             Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic) );
         suffix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
       });

  (* Concatenation vs concatenation. *)
  assert_split
    "[int, pyre_extensions.Unpack[Ts], bool]"
    "[int, pyre_extensions.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic),
             Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic) );
         suffix_pairs = [Type.bool, Type.integer];
       });
  assert_split
    "[int, str, pyre_extensions.Unpack[Ts], bool]"
    "[int, pyre_extensions.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[Type.string] ~suffix:[] variadic),
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[Type.string] variadic) );
         suffix_pairs = [Type.bool, Type.integer];
       });
  (* There are no matching elements of known length in either the prefix_pairs or the suffix_pairs. *)
  assert_split
    "[pyre_extensions.Unpack[Ts], str]"
    "[int, pyre_extensions.Unpack[Ts]]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[Type.string] variadic),
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] ~suffix:[] variadic) );
         suffix_pairs = [];
       });
  assert_split
    "[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], str, bool]"
    "[int, pyre_extensions.Unpack[typing.Tuple[int, ...]], bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.integer];
         middle_pair = Concrete [Type.string], Concrete [Type.integer];
         suffix_pairs = [Type.string, Type.integer; Type.bool, Type.bool];
       });
  assert_split
    "[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    "[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], str, bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
         middle_pair = Concrete [Type.string], Concrete [Type.string];
         suffix_pairs = [Type.string, Type.string; Type.bool, Type.bool];
       });
  assert_split
    "[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    "[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], str, bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
         middle_pair = Concrete [Type.string], Concrete [Type.string];
         suffix_pairs = [Type.string, Type.string; Type.bool, Type.bool];
       });
  assert_split
    "[pyre_extensions.Unpack[typing.Tuple[str, ...]], int]"
    "[int, pyre_extensions.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.string, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create variadic) );
         suffix_pairs = [Type.integer, Type.integer];
       });
  assert_split
    "[int, int, pyre_extensions.Unpack[typing.Tuple[str, ...]], int]"
    "[int, pyre_extensions.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                  ~prefix:[Type.integer]
                  Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create variadic) );
         suffix_pairs = [Type.integer, Type.integer];
       });
  assert_split
    "[pyre_extensions.Unpack[typing.Tuple[str, ...]]]"
    "[pyre_extensions.Unpack[Ts], str]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create variadic) );
         suffix_pairs = [Type.string, Type.string];
       });
  assert_split
    "[pyre_extensions.Unpack[typing.Tuple[str, ...]], int]"
    "[int, pyre_extensions.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [Type.string, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create variadic) );
         suffix_pairs = [Type.string, Type.string; Type.integer, Type.integer];
       });
  assert_split
    "[str, pyre_extensions.Unpack[typing.Tuple[str, ...]], int]"
    "[pyre_extensions.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                  ~prefix:[Type.string]
                  Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create variadic) );
         suffix_pairs = [Type.string, Type.string; Type.integer, Type.integer];
       });
  assert_split
    "[pyre_extensions.Unpack[typing.Tuple[str, ...]]]"
    "[str, str]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.string];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split
    "[str, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    "[str, str]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.string];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split
    "[str, pyre_extensions.Unpack[typing.Tuple[int, ...]], str]"
    "[str, bool]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.bool];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split "[str, str, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]" "[str, str]" None;
  assert_split
    "[str, ...]"
    "[int]"
    (Some
       {
         prefix_pairs = [Type.string, Type.integer];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  ()


let test_coalesce_ordered_types _ =
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let assert_coalesce ordered_types expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None
    in
    let parse_ordered_type type_ =
      match
        Type.create ~aliases (parse_single_expression ~preprocess:true ("typing.Tuple" ^ type_))
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple elements"
    in
    let ordered_types = List.map ordered_types ~f:parse_ordered_type in
    let expected = expected >>| parse_ordered_type in
    assert_equal
      ~printer:[%show: Type.t Type.OrderedTypes.record option]
      expected
      (Type.OrderedTypes.coalesce_ordered_types ordered_types)
  in
  assert_coalesce ["[int, str]"; "[bool, bool]"] (Some "[int, str, bool, bool]");
  assert_coalesce
    ["[int, str]"; "[int, ...]"; "[bool, bool]"]
    (Some "[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]], bool, bool]");
  assert_coalesce
    ["[int, str]"; "[pyre_extensions.Unpack[Ts]]"; "[bool, bool]"]
    (Some "[int, str, pyre_extensions.Unpack[Ts], bool, bool]");
  assert_coalesce ["[int, ...]"; "[pyre_extensions.Unpack[Ts]]"] None;
  assert_coalesce ["[int, ...]"; "[int, ...]"] (Some "[int, ...]");
  assert_coalesce ["[int, ...]"; "[str, ...]"] (Some "[typing.Union[int, str], ...]");
  assert_coalesce
    [
      "[int, int]";
      "[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]], bool, bool]";
      "[bool, bool]";
      "[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool, bool]";
      "[bool, bool]";
      "[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool, bool]";
      "[bool, bool]";
    ]
    (Some
       "[int, int, int, str, pyre_extensions.Unpack[typing.Tuple[typing.Union[int, bool, str], \
        ...]], bool, bool, bool, bool]");
  ()


let test_drop_prefix_ordered_type _ =
  let open Type.OrderedTypes in
  let assert_drop_prefix ~length actual expected_tuple =
    let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None
    in
    let extract_ordered_type string =
      match parse_single_expression string |> Type.create ~aliases with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple"
    in
    assert_equal
      ~cmp:[%equal: Type.t record option]
      ~printer:[%show: Type.t record option]
      (expected_tuple >>| extract_ordered_type)
      (extract_ordered_type actual |> Type.OrderedTypes.drop_prefix ~length)
  in
  assert_drop_prefix ~length:0 "typing.Tuple[int, str]" (Some "typing.Tuple[int, str]");
  assert_drop_prefix ~length:2 "typing.Tuple[int, str]" (Some "typing.Tuple[()]");
  assert_drop_prefix ~length:2 "typing.Tuple[int, str, bool, int]" (Some "typing.Tuple[bool, int]");
  assert_drop_prefix ~length:3 "typing.Tuple[int, str]" None;
  assert_drop_prefix
    ~length:0
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]");
  assert_drop_prefix
    ~length:1
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]], str]"
    (Some "typing.Tuple[str, pyre_extensions.Unpack[typing.Tuple[int, ...]], str]");
  assert_drop_prefix
    ~length:2
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, ...]");
  assert_drop_prefix
    ~length:2
    "typing.Tuple[int, str, pyre_extensions.Unpack[Ts]]"
    (Some "typing.Tuple[pyre_extensions.Unpack[Ts]]");
  assert_drop_prefix
    ~length:3
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, ...]");
  assert_drop_prefix ~length:3 "typing.Tuple[int, str, pyre_extensions.Unpack[Ts]]" None;
  ()


let test_index_ordered_type _ =
  let assert_index ~python_index tuple expected =
    let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None
    in
    let extract_ordered_type string =
      match parse_single_expression string |> Type.create ~aliases with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple"
    in
    assert_equal
      ~cmp:[%equal: Type.t option]
      ~printer:[%show: Type.t option]
      (expected >>| parse_single_expression >>| Type.create ~aliases)
      (extract_ordered_type tuple |> Type.OrderedTypes.index ~python_index)
  in
  assert_index ~python_index:0 "typing.Tuple[int, str]" (Some "int");
  assert_index ~python_index:1 "typing.Tuple[int, str]" (Some "str");
  assert_index ~python_index:(-1) "typing.Tuple[int, str]" (Some "str");
  assert_index ~python_index:(-2) "typing.Tuple[int, str]" (Some "int");
  assert_index ~python_index:2 "typing.Tuple[int, str]" None;
  assert_index ~python_index:(-3) "typing.Tuple[int, str]" None;
  assert_index
    ~python_index:0
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]]]"
    (Some "int");
  assert_index
    ~python_index:2
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]]]"
    (Some "bool");
  assert_index
    ~python_index:99
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]]]"
    (Some "bool");
  assert_index
    ~python_index:(-1)
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "str");
  assert_index
    ~python_index:(-2)
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "bool");
  assert_index
    ~python_index:(-99)
    "typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "bool");
  assert_index
    ~python_index:1
    "typing.Tuple[int, str, pyre_extensions.Unpack[Ts], bool]"
    (Some "str");
  assert_index
    ~python_index:(-1)
    "typing.Tuple[int, str, pyre_extensions.Unpack[Ts], bool]"
    (Some "bool");
  assert_index ~python_index:2 "typing.Tuple[int, str, pyre_extensions.Unpack[Ts], bool]" None;
  assert_index ~python_index:(-2) "typing.Tuple[int, str, pyre_extensions.Unpack[Ts], bool]" None;
  ()


let test_zip_variables_with_parameters _ =
  let unary = Type.Variable.Unary.create "T" in
  let unary2 = Type.Variable.Unary.create "T2" in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "TParams" in
  let assert_zipped ~generic_class ~instantiation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T" -> Some (Type.TypeAlias (Type.Variable unary))
      | "T2" -> Some (Type.TypeAlias (Type.Variable unary2))
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
      | "TParams" -> Some (Type.VariableAlias (Type.Variable.ParameterVariadic parameter_variadic))
      | _ -> None
    in
    let parameters =
      match Type.create ~aliases (parse_single_expression ~preprocess:true instantiation) with
      | Type.Parametric { parameters; _ } -> parameters
      | _ -> failwith "expected Parametric"
    in
    let variables =
      match Type.create ~aliases (parse_single_expression ~preprocess:true generic_class) with
      | Type.Parametric { parameters; _ } ->
          let variables = List.map ~f:Type.Parameter.to_variable parameters |> Option.all in
          Option.value_exn variables
      | _ -> failwith "expected Parametric"
    in
    assert_equal
      ~printer:[%show: Type.Variable.variable_zip_result list option]
      ~cmp:[%equal: Type.Variable.variable_zip_result list option]
      expected
      (Type.Variable.zip_variables_with_parameters_including_mismatches ~parameters variables)
  in
  assert_zipped
    ~generic_class:"Generic[T, T2]"
    ~instantiation:"Foo[int, str]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair = Type.Variable.UnaryPair (unary2, Type.string);
           received_parameter = Single Type.string;
         };
       ]);
  assert_zipped ~generic_class:"Generic[T, T2]" ~instantiation:"Foo[int]" None;

  (* ParamSpec. *)
  assert_zipped
    ~generic_class:"Generic[T, TParams]"
    ~instantiation:"Foo[int, TParams]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParameterVariadicPair
               ( parameter_variadic,
                 Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic) );
           received_parameter =
             CallableParameters
               (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
         };
       ]);
  (* Not enough parameters. *)
  assert_zipped ~generic_class:"Generic[T, TParams]" ~instantiation:"Foo[int]" None;
  (* Wrong kind of parameter passed to unary and ParamSpec. *)
  assert_zipped
    ~generic_class:"Generic[T, TParams]"
    ~instantiation:"Foo[TParams, int]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.Any);
           received_parameter =
             CallableParameters
               (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
         };
         {
           variable_pair = Type.Variable.ParameterVariadicPair (parameter_variadic, Undefined);
           received_parameter = Single Type.integer;
         };
       ]);
  (* This is pretty unintuitive, but PEP 612 dictates it. *)
  assert_zipped
    ~generic_class:"Generic[TParams]"
    ~instantiation:"Foo[int, str]"
    (Some
       [
         {
           variable_pair =
             Type.Variable.ParameterVariadicPair
               ( parameter_variadic,
                 Defined
                   (Type.Callable.prepend_anonymous_parameters
                      ~head:[Type.integer; Type.string]
                      ~tail:[]) );
           received_parameter =
             CallableParameters
               (Defined
                  (Type.Callable.prepend_anonymous_parameters
                     ~head:[Type.integer; Type.string]
                     ~tail:[]));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, TParams]"
    ~instantiation:"Foo[int, [str, bool]]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParameterVariadicPair
               ( parameter_variadic,
                 Defined
                   [
                     PositionalOnly { index = 0; annotation = Type.string; default = false };
                     PositionalOnly { index = 1; annotation = Type.bool; default = false };
                   ] );
           received_parameter =
             CallableParameters
               (Defined
                  [
                    PositionalOnly { index = 0; annotation = Type.string; default = false };
                    PositionalOnly { index = 1; annotation = Type.bool; default = false };
                  ]);
         };
       ]);

  (* Variadic tuples. *)
  assert_zipped
    ~generic_class:"Generic[pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[pyre_extensions.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair =
             Type.Variable.TupleVariadicPair
               (variadic, Concatenation (Type.OrderedTypes.Concatenation.create variadic));
           received_parameter =
             Single (Tuple (Concatenation (Type.OrderedTypes.Concatenation.create variadic)));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[int]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair = Type.Variable.TupleVariadicPair (variadic, Concrete []);
           received_parameter = Single (Tuple (Concrete []));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[int, str, int, bool]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.TupleVariadicPair
               (variadic, Concrete [Type.string; Type.integer; Type.bool]);
           received_parameter = Single (Tuple (Concrete [Type.string; Type.integer; Type.bool]));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, pyre_extensions.Unpack[Ts], T2]"
    ~instantiation:"Foo[int, str, pyre_extensions.Unpack[Ts2], bool, str]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.TupleVariadicPair
               ( variadic,
                 Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.string]
                      ~suffix:[Type.bool]
                      variadic2) );
           received_parameter =
             Single
               (Tuple
                  (Concatenation
                     (Type.OrderedTypes.Concatenation.create
                        ~prefix:[Type.string]
                        ~suffix:[Type.bool]
                        variadic2)));
         };
         {
           variable_pair = Type.Variable.UnaryPair (unary2, Type.string);
           received_parameter = Single Type.string;
         };
       ]);
  (* Mix of unary, variadic, and ParamSpec. *)
  assert_zipped
    ~generic_class:"Generic[T, TParams, pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[int, TParams, str, bool]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParameterVariadicPair
               ( parameter_variadic,
                 Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic) );
           received_parameter =
             CallableParameters
               (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
         };
         {
           variable_pair =
             Type.Variable.TupleVariadicPair (variadic, Concrete [Type.string; Type.bool]);
           received_parameter = Single (Tuple (Concrete [Type.string; Type.bool]));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, TParams, pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[int, TParams, str, pyre_extensions.Unpack[Ts2]]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.integer);
           received_parameter = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParameterVariadicPair
               ( parameter_variadic,
                 Type.Callable.ParameterVariadicTypeVariable (empty_head parameter_variadic) );
           received_parameter =
             CallableParameters
               (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
         };
         {
           variable_pair =
             Type.Variable.TupleVariadicPair
               ( variadic,
                 Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.string]
                      ~suffix:[]
                      variadic2) );
           received_parameter =
             Single
               (Tuple
                  (Concatenation
                     (Type.OrderedTypes.Concatenation.create
                        ~prefix:[Type.string]
                        ~suffix:[]
                        variadic2)));
         };
       ]);

  (* Wrong kind of parameter passed to `T` and `TParams`. *)
  assert_zipped
    ~generic_class:"Generic[T]"
    ~instantiation:"Foo[pyre_extensions.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair = Type.Variable.UnaryPair (unary, Type.Any);
           received_parameter =
             Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[TParams]"
    ~instantiation:"Foo[pyre_extensions.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair = Type.Variable.ParameterVariadicPair (parameter_variadic, Undefined);
           received_parameter =
             Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic);
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[TParams]"
    (Some
       [
         {
           variable_pair =
             Type.Variable.TupleVariadicPair (variadic, Type.Variable.Variadic.Tuple.any);
           received_parameter =
             Single
               (Type.parametric
                  Type.Variable.Variadic.Tuple.synthetic_class_name_for_error
                  [
                    CallableParameters
                      (Type.Variable.Variadic.Parameters.self_reference parameter_variadic);
                  ]);
         };
       ]);
  (* We forbid

     class Foo(Generic[T, *Ts]): ...

     def foo(x: Foo[*Ts]) -> None: ...

     because `Ts` might be an empty tuple, in which case the generic `T` won't be bound to any type. *)
  assert_zipped
    ~generic_class:"Generic[T, pyre_extensions.Unpack[Ts]]"
    ~instantiation:"Foo[pyre_extensions.Unpack[Ts2]]"
    None;
  assert_zipped
    ~generic_class:"Generic[pyre_extensions.Unpack[Ts], pyre_extensions.Unpack[Ts2]]"
    ~instantiation:"Foo[int, str]"
    None;
  ()


let test_zip_on_two_parameter_lists _ =
  let unary = Type.Variable.Unary.create "T" in
  let unary2 = Type.Variable.Unary.create "T2" in
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "TParams" in
  let assert_zipped ~generic_class ~left ~right expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T" -> Some (Type.TypeAlias (Type.Variable unary))
      | "T2" -> Some (Type.TypeAlias (Type.Variable unary2))
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | "Ts2" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic2))
      | "TParams" -> Some (Type.VariableAlias (Type.Variable.ParameterVariadic parameter_variadic))
      | _ -> None
    in
    let left_parameters =
      match Type.create ~aliases (parse_single_expression ~preprocess:true left) with
      | Type.Parametric { parameters; _ } -> parameters
      | _ -> failwith "expected Parametric"
    in
    let right_parameters =
      match Type.create ~aliases (parse_single_expression ~preprocess:true right) with
      | Type.Parametric { parameters; _ } -> parameters
      | _ -> failwith "expected Parametric"
    in
    let variables =
      match Type.create ~aliases (parse_single_expression ~preprocess:true generic_class) with
      | Type.Parametric { parameters; _ } ->
          let variables = List.map ~f:Type.Parameter.to_variable parameters |> Option.all in
          Option.value_exn variables
      | _ -> failwith "expected Parametric"
    in
    assert_equal
      ~printer:[%show: (Type.Variable.pair * Type.Variable.pair) list option]
      ~cmp:[%equal: (Type.Variable.pair * Type.Variable.pair) list option]
      expected
      (Type.Variable.zip_variables_with_two_parameter_lists
         ~left_parameters
         ~right_parameters
         variables)
  in
  assert_zipped
    ~generic_class:"Generic[T]"
    ~left:"Child[int]"
    ~right:"Base[str]"
    (Some
       [Type.Variable.UnaryPair (unary, Type.integer), Type.Variable.UnaryPair (unary, Type.string)]);
  assert_zipped ~generic_class:"Generic[T]" ~left:"Child[int]" ~right:"Base[str, bool]" None;
  ()


let test_union_upper_bound _ =
  let assert_union_upper_bound map expected =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (Type.OrderedTypes.union_upper_bound map)
      expected
  in
  assert_union_upper_bound
    (Concrete [Type.integer; Type.string; Type.bool])
    (Type.union [Type.integer; Type.string; Type.bool]);

  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_union_upper_bound
    (Concatenation (Type.OrderedTypes.Concatenation.create variadic))
    Type.object_primitive;

  assert_union_upper_bound
    (Concatenation
       (Type.OrderedTypes.Concatenation.create
          ~prefix:[Type.integer]
          ~suffix:[Type.string]
          variadic))
    Type.object_primitive;
  assert_union_upper_bound
    (Type.OrderedTypes.create_unbounded_concatenation Type.integer)
    Type.integer;
  assert_union_upper_bound
    (Concatenation
       (Type.OrderedTypes.Concatenation.create_from_unbounded_element
          ~prefix:[Type.integer]
          ~suffix:[Type.bool]
          Type.string))
    (Type.union [Type.integer; Type.string; Type.bool]);
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
         (Concrete [Type.Primitive "string"; Type.Primitive "string"; Type.Primitive "string"]))
    ~expected:
      (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation (Type.Primitive "string")));
  assert_transform
    ~annotation:(Type.Tuple (Concrete [Type.Primitive "string"; Type.Primitive "string"]))
    ~expected:(Type.Tuple (Concrete [Type.Primitive "string"; Type.Primitive "string"]));
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
  Type.Polynomial.show_normal ~show_variable:Type.polynomial_show_variable ~show_type:Type.pp


let polynomial_add = Type.Polynomial.add ~compare_t:Type.compare

let polynomial_subtract = Type.Polynomial.subtract ~compare_t:Type.compare

let polynomial_multiply = Type.Polynomial.multiply ~compare_t:Type.compare

let polynomial_divide = Type.Polynomial.divide ~compare_t:Type.compare

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
    "2 + x + 2y + z + xyz + xy^2 + x^2y";
  ()


let test_polynomial_to_type _ =
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let aliases ?replace_unbound_parameters_with_any:_ = function
    | "x" -> Some (Type.TypeAlias (Type.Variable x))
    | "y" -> Some (Type.TypeAlias (Type.Variable y))
    | _ -> None
  in
  let assert_to_type given expected =
    let expected_type = Type.create ~aliases (parse_single_expression ~preprocess:true expected) in
    let given = polynomial_create_from_variables_list given in
    let result = Type.polynomial_to_type given in
    assert_equal ~printer:Type.show ~cmp:Type.equal expected_type result
  in
  assert_to_type [] "typing_extensions.Literal[0]";
  assert_to_type [2, []] "typing_extensions.Literal[2]";
  assert_to_type [1, [x, 1]] "x";
  assert_to_type
    [2, [x, 1; y, 3]]
    "pyre_extensions.Multiply[pyre_extensions.Multiply[typing_extensions.Literal[2], \
     pyre_extensions.Multiply[x, y]], pyre_extensions.Multiply[y, y]]";
  ()


let test_polynomial_replace _ =
  let x = Type.Variable.Unary.create "x" in
  let y = Type.Variable.Unary.create "y" in
  let z = Type.Variable.Unary.create "z" in
  let w = Type.Variable.Unary.create "w" in
  let assert_polynomial_replace given ~replacements:(variable, by) expected =
    let replaced =
      Type.Polynomial.replace
        ~compare_t:Type.compare
        (polynomial_create_from_variables_list given)
        ~by:(polynomial_create_from_variables_list by)
        ~variable:(Type.Monomial.create_variable variable)
    in
    let expected_polynomial = polynomial_create_from_variables_list expected in
    assert_equal
      ~printer:Fn.id
      (polynomial_show_normal expected_polynomial)
      (polynomial_show_normal replaced)
  in
  (* [y/x] 0 => 0 *)
  assert_polynomial_replace [] ~replacements:(x, [1, [y, 1]]) [];
  (* [0/x] 2x => 0 *)
  assert_polynomial_replace [2, [x, 1]] ~replacements:(x, []) [];
  (* [x/x] 2x => 2x *)
  assert_polynomial_replace [2, [x, 1]] ~replacements:(x, [1, [x, 1]]) [2, [x, 1]];
  (* [0/x] 3y^2 => 3y^2 *)
  assert_polynomial_replace [3, [y, 2]] ~replacements:(x, []) [3, [y, 2]];
  (* [3y/x] 2x => 6y *)
  assert_polynomial_replace [2, [x, 1]] ~replacements:(x, [3, [y, 1]]) [6, [y, 1]];
  (* [2y^3/x] 5x^2 => 20y^6 *)
  assert_polynomial_replace [5, [x, 2]] ~replacements:(x, [2, [y, 3]]) [20, [y, 6]];
  (* [2z^4/y] 2x^2y^3 => 16x^2z^12 *)
  assert_polynomial_replace [2, [x, 2; y, 3]] ~replacements:(y, [2, [z, 4]]) [16, [x, 2; z, 12]];

  (* [3yz + 6x^3/x] 2x => 6yz + 12x^3 *)
  assert_polynomial_replace
    [2, [x, 1]]
    ~replacements:(x, [3, [y, 2; z, 1]; 6, [x, 3]])
    [6, [y, 2; z, 1]; 12, [x, 3]];
  (* [2x^2 + 3yz^3/x] 4x^2y^3z => 16x^4y^3z + 48x^2y^4z^4 + 36y^5z^7 *)
  assert_polynomial_replace
    [4, [x, 2; y, 3; z, 1]]
    ~replacements:(x, [2, [x, 2]; 3, [y, 1; z, 3]])
    [16, [x, 4; y, 3; z, 1]; 48, [x, 2; y, 4; z, 4]; 36, [y, 5; z, 7]];

  (* [0/z] 2xy + 3z => 2xy *)
  assert_polynomial_replace [2, [x, 1; y, 1]; 3, [z, 1]] ~replacements:(z, []) [2, [x, 1; y, 1]];
  (* [2/y] 2x^2y^3 + 3y^2z => 16x^2 + 12z *)
  assert_polynomial_replace
    [2, [x, 2; y, 3]; 3, [y, 2; z, 1]]
    ~replacements:(y, [2, []])
    [16, [x, 2]; 12, [z, 1]];
  (* [3x^2/z] 8xy^2z^2 + 3x^3y^2z => 81x^5y^2 *)
  assert_polynomial_replace
    [8, [x, 1; y, 2; z, 2]; 3, [x, 3; y, 2; z, 1]]
    ~replacements:(z, [3, [x, 2]])
    [81, [x, 5; y, 2]];

  (* [2x^2 + 5y^4/z] 3yz^2 + 2x^2yz => 35x^4y + 15y^9 + 10x^2y^5 *)
  assert_polynomial_replace
    [3, [y, 1; z, 2]; 2, [x, 2; y, 1; z, 1]]
    ~replacements:(z, [2, [x, 2]; 5, [y, 4]])
    [16, [x, 4; y, 1]; 70, [x, 2; y, 5]; 75, [y, 9]];
  (* [2x^2 + 5y^4z/x] 3xyz^2 + 2x^2yz + 2w^2 => 6x^2yz^2 + 15y^5z^3 + 8x^4yz + 40x^2y^5z^2 +
     50y^9z^3 + 2w^2 *)
  assert_polynomial_replace
    [3, [x, 1; y, 1; z, 2]; 2, [x, 2; y, 1; z, 1]; 2, [w, 2]]
    ~replacements:(x, [2, [x, 2]; 5, [y, 4; z, 1]])
    [
      6, [x, 2; y, 1; z, 2];
      15, [y, 5; z, 3];
      8, [x, 4; y, 1; z, 1];
      40, [x, 2; y, 5; z, 2];
      50, [y, 9; z, 3];
      2, [w, 2];
    ];
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
  assert_add [1, []; 1, [x, 1; y, 2]] [1, [x, 2; y, 1]] "1 + xy^2 + x^2y";
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


let test_resolve_getitem_callee _ =
  let open Expression in
  let assert_resolved_getitem_callee ?(resolve_aliases = Fn.id) actual expected =
    let parse_callee given =
      match parse_single_expression given |> Node.value with
      | Expression.Call { callee = { Node.value = callee; _ }; _ } -> callee
      | _ -> failwith "expected Call"
    in
    assert_equal
      ~cmp:(fun left right ->
        Expression.location_insensitive_compare
          (Node.create_with_default_location left)
          (Node.create_with_default_location right)
        = 0)
      ~printer:[%show: Expression.expression]
      (parse_callee expected)
      (Type.Callable.resolve_getitem_callee ~resolve_aliases (parse_callee actual))
  in
  assert_resolved_getitem_callee "NotAlias[int]" "NotAlias[int]";
  assert_resolved_getitem_callee "typing.Callable[[int], str]" "typing.Callable[[int], str]";
  assert_resolved_getitem_callee
    "typing.Callable[[int], str][[int], str]"
    "typing.Callable[[int], str][[int], str]";
  assert_resolved_getitem_callee
    "typing.Callable[[int], str][[int], str][[int], str]"
    "typing.Callable[[int], str][[int], str][[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> annotation)
    "bar.baz.Callable[[int], str]"
    "typing.Callable[[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.Callable" ->
          Type.Callable.create ~annotation:Type.Any ~parameters:Undefined ()
      | annotation -> annotation)
    "bar.baz.Callable[[int], str]"
    "typing.Callable[[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.CallableAlias" ->
          Type.Callable.create
            ~annotation:Type.integer
            ~parameters:
              (Defined
                 [PositionalOnly { index = 0; annotation = Type.variable "T"; default = false }])
            ()
      | annotation -> annotation)
    "bar.baz.CallableAlias[str]"
    "bar.baz.CallableAlias[str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> annotation)
    "bar.baz.Callable[[int], str][[int], str][[int], str]"
    "typing.Callable[[int], str][[int], str][[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> annotation)
    "not_an_alias.Callable[[int], str]"
    "not_an_alias.Callable[[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | Type.Primitive "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> annotation)
    "Foo[int].Callable[[int], str]"
    "Foo[int].Callable[[int], str]";
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
  let tree_annotation =
    Type.RecursiveType.create
      ~name:"Tree"
      ~body:(Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; Type.Primitive "Tree"]])
  in

  assert_resolved_class
    tree_annotation
    (Some
       [
         { instantiated = Type.integer; accessed_through_class = false; class_name = "int" };
         {
           instantiated = Type.tuple [Type.Primitive "Foo"; tree_annotation];
           accessed_through_class = false;
           class_name = "tuple";
         };
       ]);
  let recursive_list =
    Type.RecursiveType.create
      ~name:"RecursiveList"
      ~body:(Type.list (Type.union [Type.integer; Type.Primitive "RecursiveList"]))
  in

  assert_resolved_class
    recursive_list
    (Some
       [
         {
           instantiated = Type.list (Type.union [Type.integer; recursive_list]);
           accessed_through_class = false;
           class_name = "list";
         };
       ]);
  (* TODO(T44784951): We should forbid defining a directly recursive type like this. This regression
     test is here just to test we don't go into an infinite loop in case one makes it through. *)
  let directly_recursive_type =
    Type.RecursiveType.create ~name:"Tree" ~body:(Type.union [Type.integer; Type.Primitive "Tree"])
  in
  assert_resolved_class
    directly_recursive_type
    (Some [{ instantiated = Type.integer; accessed_through_class = false; class_name = "int" }]);
  ()


let test_show _ =
  let assert_show given ~expected_full ~expected_concise =
    assert_equal ~cmp:String.equal ~printer:Fn.id ([%show: Type.t] given) expected_full;
    assert_equal ~cmp:String.equal ~printer:Fn.id (Type.show_concise given) expected_concise
  in
  let callable1 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.integer])
      ~annotation:Type.string
      ()
  in
  let callable2 =
    Type.Callable.create
      ~parameters:(make_callable_from_arguments [Type.string])
      ~annotation:Type.bool
      ()
  in
  let ts = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_show
    (Type.TypeOperation (Compose (Type.OrderedTypes.Concrete [callable1; callable2])))
    ~expected_full:
      "pyre_extensions.Compose[typing.Callable[[int], str], typing.Callable[[str], bool]]"
    ~expected_concise:"Compose[(int) -> str, (str) -> bool]";
  assert_show
    (Type.TypeOperation
       (Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create ~prefix:[callable1] ts))))
    ~expected_full:"pyre_extensions.Compose[typing.Callable[[int], str], *Ts]"
    ~expected_concise:"Compose[(int) -> str, *Ts]";
  assert_show
    (Type.TypeOperation
       (Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                ~prefix:[callable1]
                callable2))))
    ~expected_full:
      "pyre_extensions.Compose[typing.Callable[[int], str], *Tuple[typing.Callable[[str], bool], \
       ...]]"
    ~expected_concise:"Compose[(int) -> str, *Tuple[(str) -> bool, ...]]";
  assert_show
    (Type.TypeOperation
       (Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_concrete_against_concatenation
                ~prefix:[]
                ~suffix:[]
                ~concrete:[callable1]
                ~concatenation:(Type.OrderedTypes.Concatenation.create ~prefix:[callable2] ts)))))
    ~expected_full:
      "pyre_extensions.Compose[*Broadcast[typing.Tuple[typing.Callable[[int], str]], \
       typing.Tuple[typing.Callable[[str], bool], *Ts]]]"
    ~expected_concise:
      "Compose[*Broadcast[typing.Tuple[(int) -> str], typing.Tuple[(str) -> bool, *Ts]]]";
  ()


let () =
  "type"
  >::: [
         "create" >:: test_create;
         "create_callable" >:: test_create_callable;
         "create_alias" >:: test_create_alias;
         "create_type_operator" >:: test_create_type_operator;
         "create_variadic_tuple" >:: test_create_variadic_tuple;
         "resolve_aliases" >:: test_resolve_aliases;
         "instantiate" >:: test_instantiate;
         "expression" >:: test_expression;
         "concise" >:: test_concise;
         "weaken_literals" >:: test_weaken_literals;
         "union" >:: test_union;
         "primitives" >:: test_primitives;
         "elements" >:: test_elements;
         "exists" >:: test_exists;
         "contains_callable" >:: test_contains_callable;
         "contains_any" >:: test_contains_any;
         "expression_contains_any" >:: test_expression_contains_any;
         "is_concrete" >:: test_is_concrete;
         "is_not_instantiated" >:: test_is_not_instantiated;
         "is_meta" >:: test_is_meta;
         "is_none" >:: test_is_none;
         "is_type_alias" >:: test_is_type_alias;
         "create_recursive_type" >:: test_create_recursive_type;
         "unfold_recursive_type" >:: test_unfold_recursive_type;
         "contains_unknown" >:: test_contains_unknown;
         "contains_undefined" >:: test_contains_undefined;
         "is_resolved" >:: test_is_resolved;
         "is_iterator" >:: test_is_iterator;
         "class_name" >:: test_class_name;
         "optional_value" >:: test_optional_value;
         "dequalify" >:: test_dequalify;
         "variables" >:: test_variables;
         "lambda" >:: test_lambda;
         "visit" >:: test_visit;
         "collapse_escaped_variable_unions" >:: test_collapse_escaped_variable_unions;
         "namespace_insensitive_compare" >:: test_namespace_insensitive_compare;
         "namespace" >:: test_namespace;
         "mark_all_variables_as_bound" >:: test_mark_all_variables_as_bound;
         "mark_all_variables_as_free" >:: test_mark_all_variables_as_free;
         "namespace_all_free_variables" >:: test_namespace_all_free_variables;
         "mark_all_free_variables_as_escaped" >:: test_mark_all_free_variables_as_escaped;
         "contains_escaped_free_variable" >:: test_contains_escaped_free_variable;
         "convert_all_escaped_free_variables_to_anys"
         >:: test_convert_all_escaped_free_variables_to_anys;
         "int_expression_create" >:: test_int_expression_create;
         "replace_all" >:: test_replace_all;
         "product_replace_variadic" >:: test_product_replace_variadic;
         "less_or_equal_polynomial" >:: test_less_or_equal;
         "collect_all" >:: test_collect_all;
         "parse_type_variable_declarations" >:: test_parse_type_variable_declarations;
         "starred_annotation_expression" >:: test_starred_annotation_expression;
         "concatenation_from_unpack_expression" >:: test_concatenation_from_unpack_expression;
         "broadcast" >:: test_broadcast;
         "split_ordered_types" >:: test_split_ordered_types;
         "coalesce_ordered_types" >:: test_coalesce_ordered_types;
         "drop_prefix_ordered_type" >:: test_drop_prefix_ordered_type;
         "index_ordered_type" >:: test_index_ordered_type;
         "zip_variables_with_parameters" >:: test_zip_variables_with_parameters;
         "zip_on_two_parameter_lists" >:: test_zip_on_two_parameter_lists;
         "union_upper_bound" >:: test_union_upper_bound;
         "infer_transform" >:: test_infer_transform;
         "fields_from_constructor" >:: test_fields_from_constructor;
         "map_callable_annotation" >:: test_map_callable_annotation;
         "type_parameters_for_bounded_tuple_union" >:: test_type_parameters_for_bounded_tuple_union;
         "polynomial_create_from_list" >:: test_polynomial_create_from_list;
         "polynomial_to_type" >:: test_polynomial_to_type;
         "polynomial_replace" >:: test_polynomial_replace;
         "add_polynomials" >:: test_add_polynomials;
         "subtract_polynomials" >:: test_subtract_polynomials;
         "multiply_polynomial" >:: test_multiply_polynomial;
         "divide_polynomial" >:: test_divide_polynomial;
         "resolve_class" >:: test_resolve_class;
         "show" >:: test_show;
       ]
  |> Test.run;
  "primitive" >::: ["is unit test" >:: test_is_unit_test] |> Test.run;
  "callable"
  >::: [
         "from_overloads" >:: test_from_overloads;
         "with_return_annotation" >:: test_with_return_annotation;
         "overload_parameters" >:: test_overload_parameters;
         "parameter_create" >:: test_parameter_create;
         "resolve_getitem_callee" >:: test_resolve_getitem_callee;
       ]
  |> Test.run
