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

let empty_lookup _ = None

let type_var_declaration_and_variable
    ?(declaration_constraints = Type.Record.TypeVarConstraints.Unconstrained)
    ?(type_constraints = Type.Record.TypeVarConstraints.Unconstrained)
    ?(variance = Type.Record.PreInferenceVariance.P_Invariant)
    name
  =
  ( Type.Variable.Declaration.DTypeVar
      { name; constraints = declaration_constraints; variance; infer_variance = false },
    Type.Variable.TypeVar.create name ~constraints:type_constraints )


let resolved_aliases aliases ?replace_unbound_parameters_with_any:_ name =
  match aliases name with
  | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
  | _ -> None


let type_var_tuple_declaration_and_variable name =
  Type.Variable.Declaration.DTypeVarTuple { name }, Type.Variable.TypeVarTuple.create name


let param_spec_declaration_and_variable name =
  Type.Variable.Declaration.DParamSpec { name }, Type.Variable.ParamSpec.create name


let empty_head variable = { Type.Callable.head = []; variable }

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Argument.Single single)

let make_callable_from_arguments annotations =
  Type.Callable.Defined
    (List.mapi
       ~f:(fun index annotation ->
         Type.Callable.CallableParamType.PositionalOnly { index; annotation; default = false })
       annotations)


let make_variables ~aliases name =
  match aliases name with
  | Some (TypeAliasEnvironment.RawAlias.VariableDeclaration variable) ->
      let type_variables =
        Type.Variable.of_declaration
          ~create_type:
            (Type.create
               ~aliases:Type.resolved_empty_aliases
               ~variables:Type.resolved_empty_variables)
          variable
      in
      Some type_variables
  | _ -> None


let assert_create ?(aliases = fun _ -> None) source annotation =
  let aliases ?replace_unbound_parameters_with_any:_ = aliases in
  let variables = make_variables ~aliases in
  assert_equal
    ~printer:Type.show
    ~cmp:Type.equal
    annotation
    (Type.create
       ~variables
       ~aliases:(resolved_aliases aliases)
       (parse_single_expression ~preprocess:true source))


let test_create _ =
  assert_create "foo" (Type.Primitive "foo");
  assert_create "foo.bar" (Type.Primitive "foo.bar");
  assert_create "object" (Type.Primitive "object");
  assert_create "foo[bar]" (Type.parametric "foo" ![Type.Primitive "bar"]);
  assert_create
    "foo[bar, baz]"
    (Type.parametric "foo" ![Type.Primitive "bar"; Type.Primitive "baz"]);
  assert_create "typing.List[int]" (Type.list Type.integer);
  assert_create "typing.Dict[int, str]" (Type.dictionary ~key:Type.integer ~value:Type.string);

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

  (* Nested renaming. *)
  assert_create "typing.Set[typing.Any]" (Type.set Type.Any);
  assert_create "typing.Dict[str, typing.Any]" (Type.dictionary ~key:Type.string ~value:Type.Any);

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
                   CallableParamType.PositionalOnly
                     { index = 0; annotation = Type.integer; default = false };
                   CallableParamType.PositionalOnly
                     { index = 1; annotation = Type.string; default = false };
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
                   CallableParamType.PositionalOnly
                     { index = 0; annotation = Type.integer; default = false };
                   CallableParamType.Named
                     { name = "a"; annotation = Type.integer; default = false };
                   CallableParamType.Variable (Concrete Type.Top);
                   CallableParamType.Keywords Type.Top;
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
                   CallableParamType.PositionalOnly
                     { index = 0; annotation = Type.integer; default = false };
                   CallableParamType.Variable (Concrete Type.integer);
                   CallableParamType.Keywords Type.string;
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
               Defined
                 [CallableParamType.Named { name = "a"; annotation = Type.integer; default = true }];
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
      |> (fun table -> Hashtbl.find table primitive)
      >>| fun alias -> TypeAliasEnvironment.RawAlias.TypeAlias alias
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
    let aliases ?replace_unbound_parameters_with_any:_ = aliases in
    let variables = make_variables ~aliases in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create
         ~variables
         ~aliases:(resolved_aliases aliases)
         (parse_single_expression ~preprocess:true source))
  in

  (* Compose. *)
  let _t_declaration, t_variable = type_var_declaration_and_variable "T" in
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
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
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    {|
      pyre_extensions.Compose[
        int,
        typing.Unpack[Ts],
        str
      ]
    |}
    (Type.TypeOperation
       (Type.TypeOperation.Compose
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create
                ~prefix:[Type.integer]
                ~suffix:[Type.string]
                ts_variable))));
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
    | "T" -> Some (Type.Variable t_variable)
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
               Type.Variable t_variable;
               Type.Parametric { name = "Foo"; arguments = [Type.Argument.Single Type.integer] };
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.CallableParamType.PositionalOnly
                          { index = 0; annotation = Type.string; default = false };
                      ])
                 ~annotation:Type.bool
                 ();
             ])));
  assert_create "pyre_extensions.Compose[typing.Tuple[bool, str], ...]" Type.Top;
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
                        Type.Callable.CallableParamType.PositionalOnly
                          { index = 0; annotation = Type.bool; default = false };
                      ])
                 ~annotation:Type.bytes
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.CallableParamType.PositionalOnly
                          { index = 0; annotation = Type.bytes; default = false };
                      ])
                 ~annotation:Type.integer
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.CallableParamType.PositionalOnly
                          { index = 0; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.float
                 ();
               Type.Callable.create
                 ~parameters:
                   (Type.Callable.Defined
                      [
                        Type.Callable.CallableParamType.PositionalOnly
                          { index = 0; annotation = Type.float; default = false };
                      ])
                 ~annotation:Type.string
                 ();
             ])));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          pyre_extensions.Compose[
            typing.Unpack[Ts],
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
  ()


let test_create_variadic_tuple _ =
  let assert_create ?(aliases = fun _ -> None) source annotation =
    let aliases ?replace_unbound_parameters_with_any:_ = aliases in
    let variables = make_variables ~aliases in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      annotation
      (Type.create
         ~variables
         ~aliases:(resolved_aliases aliases)
         (parse_single_expression ~preprocess:true source))
  in
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let ts2_declaration, ts2_variable = type_var_tuple_declaration_and_variable "Ts2" in
  (* Parametric types. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "Foo[typing.Unpack[Ts]]"
    (Type.parametric
       "Foo"
       [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable)]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "Foo[int, typing.Unpack[Ts], str]"
    (Type.parametric
       "Foo"
       [
         Single Type.integer;
         Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable);
         Single Type.string;
       ]);
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "Foo[typing.Unpack[typing.Tuple[int, str]]]"
    (Type.parametric "Foo" [Single Type.integer; Single Type.string]);
  (* Nested unpacks get normalized. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "Foo[int, typing.Unpack[typing.Tuple[str, typing.Unpack[Ts]]]]"
    (Type.parametric
       "Foo"
       [
         Single Type.integer;
         Single Type.string;
         Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable);
       ]);
  assert_create
    "Foo[typing.Unpack[typing.Tuple[int, ...]]]"
    (Type.parametric
       "Foo"
       [Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.integer)]);

  (* Tuples. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Tuple[typing.Unpack[Ts]]"
    (Type.Tuple (Concatenation (Type.OrderedTypes.Concatenation.create ts_variable)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Tuple[int, typing.Unpack[Ts], str]"
    (Type.Tuple
       (Concatenation
          (Type.OrderedTypes.Concatenation.create
             ~prefix:[Type.integer]
             ~suffix:[Type.string]
             ts_variable)));
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Tuple[typing.Unpack[Ts], typing.Unpack[Ts]]"
    Type.Top;
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Tuple[typing.Unpack[typing.Tuple[int, str]]]"
    (Type.tuple [Type.integer; Type.string]);
  (* Nested concrete unpacks get normalized. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Tuple[bool, typing.Unpack[typing.Tuple[int, typing.Unpack[typing.Tuple[int, str]]]]]"
    (Type.tuple [Type.bool; Type.integer; Type.integer; Type.string]);

  (* Callables. *)
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Callable[[int, typing.Unpack[Ts], str], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      ts_variable));
            ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | _ -> None)
    "typing.Callable[[int, typing.Unpack[typing.Tuple[bool, typing.Unpack[Ts], bool]], str], int]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer; Type.bool]
                      ~suffix:[Type.bool; Type.string]
                      ts_variable));
            ])
       ~annotation:Type.integer
       ());
  assert_create
    ~aliases:(function
      | "Ts" -> Some (VariableDeclaration ts_declaration)
      | "Ts2" -> Some (VariableDeclaration ts2_declaration)
      | _ -> None)
    "typing.Callable[[Variable(int, typing.Unpack[Ts], str)], typing.Callable[[Variable(int, \
     typing.Unpack[Ts2], str)], int]]"
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      ts_variable));
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
                           ts2_variable));
                 ])
            ~annotation:Type.integer
            ())
       ());
  ()


let test_create_readonly _ =
  assert_create "pyre_extensions.ReadOnly[Foo]" (Type.PyreReadOnly.create (Type.Primitive "Foo"));
  assert_create
    "pyre_extensions.ReadOnly[pyre_extensions.ReadOnly[Foo]]"
    (Type.PyreReadOnly.create (Type.Primitive "Foo"));
  assert_create
    "typing.List[pyre_extensions.ReadOnly[Foo]]"
    (Type.list (Type.PyreReadOnly.create (Type.Primitive "Foo")));
  assert_create "pyre_extensions.ReadOnly[None]" Type.none;
  assert_create
    "pyre_extensions.ReadOnly[typing.Optional[Foo]]"
    (Type.Union [Type.none; Type.PyreReadOnly (Type.Primitive "Foo")]);
  assert_create
    "pyre_extensions.ReadOnly[typing.Union[Foo, str, pyre_extensions.ReadOnly[Bar]]]"
    (Type.Union
       [
         Type.PyreReadOnly (Type.Primitive "Foo");
         Type.PyreReadOnly Type.string;
         Type.PyreReadOnly (Type.Primitive "Bar");
       ]);
  assert_create "pyre_extensions.ReadOnly[typing.Any]" Type.Any;
  assert_create "pyre_extensions.ReadOnly[float]" Type.float;
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
    | "MyInt" -> Some Type.integer
    | "IntList" -> Some (Type.list Type.integer)
    | _ -> None
  in
  assert_resolved ~aliases (Type.Primitive "NotAlias") (Type.Primitive "NotAlias");
  assert_resolved ~aliases (Type.Primitive "MyInt") Type.integer;
  assert_resolved ~aliases (Type.Primitive "IntList") (Type.list Type.integer);
  (* `IntList` resolves to `List[int]`. So, it ignores the `str` argument. *)
  assert_resolved ~aliases (Type.parametric "IntList" [Single Type.string]) (Type.list Type.integer);

  let variable_t = Type.Variable (Type.Variable.TypeVar.create "T") in
  let variable_k = Type.Variable (Type.Variable.TypeVar.create "K") in
  let variable_v = Type.Variable (Type.Variable.TypeVar.create "V") in
  let aliases = function
    | "IntList" -> Some (Type.list Type.integer)
    | "foo.Optional" -> Some (Type.optional variable_t)
    | "foo.Dict" -> Some (Type.dictionary ~key:variable_k ~value:variable_v)
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
    | "Tree" -> Some (Type.RecursiveType.create ~name:"Tree" ~body:tree_body)
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
    | "MyDict" -> Some (Type.dictionary ~key:variable_t ~value:variable_k)
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "MyDict" ![Type.integer; Type.string])
    (Type.dictionary ~key:Type.integer ~value:Type.string);
  let aliases = function
    | "Mix" ->
        Some
          (Type.parametric
             "Foo"
             ![variable_t; Type.list variable_v; Type.union [variable_k; variable_v]])
    | _ -> None
  in
  assert_resolved
    ~aliases
    (Type.parametric "Mix" ![Type.integer; Type.string; Type.bool])
    (Type.parametric
       "Foo"
       ![Type.integer; Type.list Type.string; Type.union [Type.bool; Type.string]]);
  let aliases = function
    | "Foo" -> Some (Type.parametric "Bar" ![variable_t; variable_v])
    | _ -> None
  in
  assert_resolved ~aliases (Type.Primitive "Foo") (Type.parametric "Bar" ![Type.Any; Type.Any]);
  let tparams_declaration, tparams_variable = param_spec_declaration_and_variable "TParams" in
  let aliases = function
    | "TParams" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration tparams_declaration)
    | "FooParamSpec" ->
        Some
          (TypeAliasEnvironment.RawAlias.TypeAlias
             (Type.parametric
                "Bar"
                [CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable)]))
    | _ -> None
  in
  assert_resolved
    ~aliases:(resolved_aliases aliases)
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
    ~aliases:(resolved_aliases aliases)
    (Type.Primitive "FooParamSpec")
    (Type.parametric "Bar" [CallableParameters Undefined]);
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let aliases = function
    | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
    | "FloatTensor" ->
        Some
          (TypeAliasEnvironment.RawAlias.TypeAlias
             (Type.parametric
                "Tensor"
                [
                  Single Type.float;
                  Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable);
                ]))
    | _ -> None
  in
  assert_resolved
    ~aliases:(resolved_aliases aliases)
    (Type.parametric "FloatTensor" ![Type.integer; Type.string])
    (Type.parametric "Tensor" [Single Type.float; Single Type.integer; Single Type.string]);
  assert_resolved
    ~aliases:(resolved_aliases aliases)
    (Type.Primitive "FloatTensor")
    (Type.parametric
       "Tensor"
       [
         Single Type.float;
         Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.Any);
       ]);
  ()


let test_apply_type_map _ =
  let assert_apply_type_map type_maps ~generic ~expected =
    let map = Type.Map.of_alist_exn type_maps in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      (Type.apply_type_map ~type_map:(Map.find map) generic)
  in
  assert_apply_type_map [] ~generic:(Type.Primitive "foo") ~expected:(Type.Primitive "foo");

  (* Union[_T, _VT] + (_T = int, _VT = None) -> Optional[int] *)
  assert_apply_type_map
    [Type.variable "_T", Type.integer; Type.variable "_VT", Type.NoneType]
    ~generic:(Type.Union [Type.variable "_T"; Type.variable "_VT"])
    ~expected:(Type.optional Type.integer);
  assert_apply_type_map
    [Type.variable "_T", Type.integer]
    ~generic:(Type.PyreReadOnly.create (Type.variable "_T"))
    ~expected:Type.integer;
  assert_apply_type_map
    [Type.variable "_T", Type.Primitive "Foo"]
    ~generic:(Type.PyreReadOnly.create (Type.variable "_T"))
    ~expected:(Type.PyreReadOnly.create (Type.Primitive "Foo"));
  ()


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
  assert_expression (Type.parametric "foo.bar" ![Type.Primitive "baz"]) "foo.bar[baz]";
  assert_expression
    (Type.Tuple (Type.OrderedTypes.Concrete [Type.integer; Type.string]))
    "typing.Tuple[int, str]";
  assert_expression
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
    "typing.Tuple[int, ...]";
  assert_expression (Type.parametric "list" ![Type.integer]) "typing.List[int]";

  (* Callables. *)
  let open Type.Callable in
  assert_expression (Type.Callable.create ~annotation:Type.integer ()) "typing.Callable[..., int]";
  assert_expression
    (Type.Callable.create ~name:!&"name" ~annotation:Type.integer ())
    "typing.Callable[..., int]";
  assert_expression
    (Type.Callable.create
       ~overloads:
         [
           { Type.Callable.annotation = Type.string; parameters = Type.Callable.Undefined };
           { Type.Callable.annotation = Type.integer; parameters = Type.Callable.Undefined };
         ]
       ~annotation:Type.integer
       ())
    "typing.Callable[(..., int)][[..., str][..., int]]";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [
              CallableParamType.Named { name = "__0"; annotation = Type.integer; default = false };
              CallableParamType.Named { name = "__1"; annotation = Type.string; default = false };
            ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[Named(__0, int), Named(__1, str)], int]";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [
              CallableParamType.Named { name = "a"; annotation = Type.integer; default = false };
              CallableParamType.Named { name = "b"; annotation = Type.string; default = false };
            ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[Named(a, int), Named(b, str)], int]";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Type.Callable.Defined
            [CallableParamType.Named { name = "a"; annotation = Type.integer; default = true }])
       ~annotation:Type.integer
       ())
    "typing.Callable[[Named(a, int, default)], int]";
  assert_expression
    (Type.parametric
       "G"
       [
         CallableParameters
           (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
       ])
    "G[TParams]";
  assert_expression
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    "typing_extensions.Literal[test.MyEnum.ONE]";
  assert_expression (Type.Literal (Type.String AnyLiteral)) "typing_extensions.Literal[str]";

  (* Variadic tuples. *)
  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
  assert_expression
    (Type.parametric "Foo" [Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)])
    "Foo[typing.Unpack[Ts]]";
  assert_expression
    (Type.Tuple
       (Type.OrderedTypes.Concatenation
          (Type.OrderedTypes.Concatenation.create
             ~prefix:[Type.integer]
             ~suffix:[Type.string]
             variadic)))
    "typing.Tuple[int, typing.Unpack[Ts], str]";
  assert_expression
    (Type.Callable.create
       ~parameters:
         (Defined
            [
              CallableParamType.Variable
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.integer]
                      ~suffix:[Type.string]
                      variadic));
            ])
       ~annotation:Type.integer
       ())
    "typing.Callable[[Variable(int, typing.Unpack[Ts], str)], int]";

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
  assert_expression
    (Type.PyreReadOnly (Type.list Type.integer))
    "pyre_extensions.PyreReadOnly[typing.List[int]]";
  ()


let test_concise _ =
  let assert_concise annotation expected =
    assert_equal ~printer:(fun annotation -> annotation) expected (Type.show_concise annotation)
  in
  assert_concise Type.Bottom "?";
  assert_concise Type.Top "unknown";
  assert_concise (Type.Primitive "...") "...";
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
              Type.Callable.CallableParamType.Named
                { name = "x"; annotation = Type.Any; default = false };
              Type.Callable.CallableParamType.Named
                { name = "y"; annotation = Type.float; default = false };
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
              Type.Callable.CallableParamType.PositionalOnly
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
            [
              Type.Callable.CallableParamType.Named
                { name = "x"; annotation = Type.Any; default = true };
            ])
       ())
    "(x: Any = ...) -> int";
  assert_concise
    (Type.Callable.create
       ~name:!&"foo"
       ~annotation:Type.integer
       ~parameters:
         (Type.Callable.Defined
            [
              Type.Callable.CallableParamType.Named
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
                             Type.Callable.CallableParamType.Named
                               { name = "x"; annotation = Type.integer; default = false };
                           ])
                      ();
                };
            ])
       ())
    "(callable: (x: int) -> float) -> int";
  assert_concise Type.Any "Any";
  assert_concise Type.NoneType "None";
  assert_concise (Type.optional Type.integer) "int | None";
  assert_concise (Type.parametric "parametric" ![Type.Top; Type.Top]) "parametric[]";
  assert_concise (Type.parametric "parametric" ![Type.Top; Type.float]) "parametric[unknown, float]";
  assert_concise (Type.Primitive "a.b.c") "c";
  assert_concise (Type.tuple [Type.integer; Type.Any]) "tuple[int, Any]";
  assert_concise
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
    "tuple[int, ...]";
  assert_concise (Type.union [Type.integer; Type.string]) "int | str";
  assert_concise
    (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.Top]) "T")
    "T";
  assert_concise
    (Type.Literal
       (Type.EnumerationMember
          { enumeration_type = Type.Primitive "test.MyEnum"; member_name = "ONE" }))
    "Literal[test.MyEnum.ONE]";
  assert_concise (Type.Literal (Type.String AnyLiteral)) "LiteralString";
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
  assert_equal [] (Type.collect_primitive_types (Type.Callable.create ~annotation:Type.Top ()));
  assert_equal
    [Type.integer]
    (Type.collect_primitive_types (Type.Callable.create ~annotation:Type.integer ()));
  assert_equal [] (Type.collect_primitive_types (Type.optional Type.Top));
  assert_equal [Type.integer] (Type.collect_primitive_types (Type.optional Type.integer));
  assert_equal
    []
    (Type.collect_primitive_types
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_equal
    [Type.integer]
    (Type.collect_primitive_types
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
  assert_equal
    []
    (Type.collect_primitive_types
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.Top]) "T"));
  assert_equal
    [Type.integer]
    (Type.collect_primitive_types
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.integer]) "T"));
  assert_equal
    [Type.integer]
    (Type.collect_primitive_types (Type.parametric "parametric" ![Type.integer; Type.Top]));
  assert_equal
    [Type.integer; Type.string]
    (Type.collect_primitive_types (Type.parametric "parametric" ![Type.integer; Type.string]));
  assert_equal [Type.string] (Type.collect_primitive_types (Type.tuple [Type.Top; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.collect_primitive_types (Type.tuple [Type.integer; Type.string]));
  assert_equal
    [Type.integer; Type.string]
    (Type.collect_primitive_types (Type.union [Type.integer; Type.string]));
  assert_equal [] (Type.collect_primitive_types Type.Top);
  assert_equal [] (Type.collect_primitive_types Type.Bottom);
  assert_equal [Type.integer] (Type.collect_primitive_types Type.integer);
  assert_equal [] (Type.collect_primitive_types Type.Any);
  ()


let test_elements _ =
  let assert_equal = assert_equal ~printer:(List.to_string ~f:Fn.id) in
  assert_equal
    ["typing.Callable"]
    (Type.collect_names (Type.Callable.create ~annotation:Type.Top ()));
  assert_equal
    ["int"; "typing.Callable"]
    (Type.collect_names (Type.Callable.create ~annotation:Type.integer ()));
  assert_equal ["int"; "typing.Optional"] (Type.collect_names (Type.optional Type.integer));
  assert_equal
    ["tuple"]
    (Type.collect_names (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Top)));
  assert_equal
    ["int"; "tuple"]
    (Type.collect_names
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
  assert_equal
    []
    (Type.collect_names
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.Top]) "T"));
  assert_equal
    ["int"]
    (Type.collect_names
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.integer]) "T"));
  assert_equal
    ["int"; "parametric"]
    (Type.collect_names (Type.parametric "parametric" ![Type.integer; Type.Top]));
  assert_equal
    ["int"; "str"; "parametric"]
    (Type.collect_names (Type.parametric "parametric" ![Type.integer; Type.string]));
  assert_equal ["str"; "tuple"] (Type.collect_names (Type.tuple [Type.Top; Type.string]));
  assert_equal ["int"; "str"; "tuple"] (Type.collect_names (Type.tuple [Type.integer; Type.string]));
  assert_equal
    ["int"; "str"; "typing.Union"]
    (Type.collect_names (Type.union [Type.integer; Type.string]));
  assert_equal [] (Type.collect_names Type.Top);
  assert_equal [] (Type.collect_names Type.Bottom);
  assert_equal ["int"] (Type.collect_names Type.integer);
  assert_equal [] (Type.collect_names Type.Any);
  assert_equal
    ["int"; "tuple"]
    (Type.collect_names
       (Type.RecursiveType.create
          ~name:"Tree"
          ~body:(Type.tuple [Type.integer; Type.Primitive "Tree"])));
  ();
  assert_equal
    ["typing_extensions.Literal"]
    (Type.collect_names
       (Type.Literal
          (Type.EnumerationMember
             { enumeration_type = Type.Primitive "A.B.C.MyEnum"; member_name = "ONE" })));
  assert_equal
    ["str"; "typing_extensions.Literal"]
    (Type.collect_names (Type.Literal (Type.String AnyLiteral)));
  assert_equal
    ["int"; "list"; "typing._PyreReadOnly_"]
    (Type.collect_names (Type.PyreReadOnly (Type.list Type.integer)));
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
  assert_true
    (top_exists
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.Top]) "T"));
  assert_false
    (top_exists
       (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.integer]) "T"));
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
      (Type.type_arguments_for_bounded_tuple_union actual)
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

let test_expression_contains_any =
  let assert_true actual _context = assert_true actual in
  let assert_false actual _context = assert_false actual in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (Type.expression_contains_any
              (parse_single_expression ~preprocess:true "typing.Dict[typing.Any, typing.Any]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any (parse_single_expression ~preprocess:true "dict"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any (parse_single_expression ~preprocess:true "typing.Type"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression ~preprocess:true "typing.Callable"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any (parse_single_expression ~preprocess:true "typing.Tuple"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (Type.expression_contains_any
              (parse_single_expression ~preprocess:true "typing.Union[typing.Any, None]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression ~preprocess:true "typing.Union[typing.Callable]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression
                 ~preprocess:true
                 "pyre_extensions.type_variable_operators.Map[typing.List, int]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression ~preprocess:true "foo[typing.Concatenate[int, bool, Ts]]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression
                 ~preprocess:true
                 "foo[pyre_extensions.type_variable_operators.Concatenate[int, bool, Ts]]"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (Type.expression_contains_any
              (parse_single_expression
                 ~preprocess:true
                 "foo[type_extensions.Concatenate[int, bool, Ts]]"));
    ]


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
  assert_true (Type.is_builtins_type (Type.parametric "type" ![Type.integer]));
  assert_false (Type.is_builtins_type Type.integer);
  assert_false (Type.is_builtins_type (Type.parametric "typing.Type" ![Type.integer]))


let test_is_none _ =
  assert_false (Type.is_none (Type.Primitive "None"));
  assert_false (Type.is_none Type.integer);
  assert_false (Type.is_none (Type.Primitive "foo"));
  assert_true (Type.is_none Type.NoneType)


let test_is_literal_string _ =
  assert_true (Type.is_primitive_string (Type.Primitive "str"));
  assert_false (Type.is_literal_string (Type.Primitive "str"));
  assert_false (Type.is_primitive_string (Type.Literal (String AnyLiteral)));
  assert_true (Type.is_literal_string (Type.Literal (String AnyLiteral)));
  assert_false (Type.is_primitive_string (Type.Literal (String (LiteralValue "foo"))));
  assert_true (Type.is_literal_string (Type.Literal (String (LiteralValue "foo"))));
  ()


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
  let parameter_variadic = Type.Variable.ParamSpec.create "T" in
  assert_false
    (Type.Variable.all_variables_are_resolved
       (Type.Callable.create
          ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
          ~annotation:Type.integer
          ()));
  let parameter_variadic = parameter_variadic |> Type.Variable.ParamSpec.mark_as_bound in
  assert_true
    (Type.Variable.all_variables_are_resolved
       (Type.Callable.create
          ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
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
    (Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "A.B.C"))
    (Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "C"));
  assert_dequalify_variable
    (Type.Variable.ParamSpecVariable (Type.Variable.ParamSpec.create "A.B.C"))
    (Type.Variable.ParamSpecVariable (Type.Variable.ParamSpec.create "C"));
  ()


let test_from_overloads _ =
  let assert_create ?(aliases = Type.resolved_empty_aliases) sources expected =
    let merged =
      let parse_callable source =
        match Type.create ~aliases ~variables:empty_lookup (parse_single_expression source) with
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
      (Type.create ~aliases ~variables:empty_lookup (parse_single_expression expected))
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
      match
        Type.create
          ~variables:empty_lookup
          ~aliases:Type.resolved_empty_aliases
          (parse_single_expression callable)
      with
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (Type.create
         ~variables:empty_lookup
         ~aliases:Type.resolved_empty_aliases
         (parse_single_expression expected))
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
      Type.create
        ~variables:empty_lookup
        ~aliases:Type.resolved_empty_aliases
        (parse_single_expression callable)
      |> function
      | Type.Callable callable -> callable
      | _ -> failwith ("Could not extract callable from " ^ callable)
    in
    let parameters =
      List.hd_exn overloads
      |> Type.Callable.Overload.parameters
      |> Option.value ~default:[]
      |> List.map ~f:(function
             | Type.Callable.CallableParamType.PositionalOnly { annotation; _ } -> annotation
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
    let variables = make_variables ~aliases in
    let variables =
      Type.create ~variables ~aliases:(resolved_aliases aliases) (parse_single_expression source)
      |> Type.Variable.all_free_variables
      |> List.filter_map ~f:(function
             | Type.Variable.TypeVarVariable variable -> Some variable
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
  let parameter_variadic = Type.Variable.ParamSpec.create "T" in
  let unary = Type.Variable.TypeVar.create "T" in
  assert_equal
    [Type.Variable.TypeVarVariable unary; Type.Variable.ParamSpecVariable parameter_variadic]
    (Type.Variable.all_free_variables
       (Type.Callable.create
          ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
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
  let create source =
    Type.create ~aliases:Type.resolved_empty_aliases (parse_single_expression source)
  in

  let assert_types_equal annotation expected =
    assert_equal ~printer:Type.show ~cmp:Type.equal expected annotation
  in
  let module CountTransform = Type.VisitWithTransform.Make (struct
    type state = int

    let visit state _ =
      { Type.VisitWithTransform.transformed_annotation = Type.integer; new_state = state + 1 }


    let visit_children_before _ _ = true

    let visit_children_after = false
  end)
  in
  let end_state, transformed =
    CountTransform.visit 0 (create ~variables:(fun _ -> None) "typing.List[int]")
  in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 2 end_state;
  let end_state, transformed =
    CountTransform.visit 0 (create ~variables:(fun _ -> None) "Foo[Bar[Baz, Bop], Bang]")
  in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 5 end_state;
  let end_state, transformed =
    CountTransform.visit 0 (create ~variables:(fun _ -> None) "typing.Literal[test.MyEnum.ONE]")
  in
  assert_types_equal transformed Type.integer;
  assert_equal ~printer:string_of_int 2 end_state;

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

  let module SubstitutionTransform = Type.VisitWithTransform.Make (struct
    type state = int

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive integer when String.equal integer "int" && state > 0 ->
            state - 1, Type.string
        | _ -> state, annotation
      in
      { Type.VisitWithTransform.transformed_annotation; new_state }


    let visit_children_before _ = function
      | Type.Union [Type.NoneType; _]
      | Type.Union [_; Type.NoneType] ->
          false
      | _ -> true


    let visit_children_after = false
  end)
  in
  let end_state, transformed =
    SubstitutionTransform.visit 1 (create ~variables:(fun _ -> None) "typing.Callable[[int], int]")
  in
  assert_types_equal transformed (create ~variables:(fun _ -> None) "typing.Callable[[str], int]");
  assert_equal ~printer:string_of_int 0 end_state;
  let end_state, transformed =
    SubstitutionTransform.visit
      1
      (create ~variables:(fun _ -> None) "typing.Callable[[typing.Optional[int], int], int]")
  in
  assert_types_equal
    transformed
    (create ~variables:(fun _ -> None) "typing.Callable[[typing.Optional[int], str], int]");
  assert_equal ~printer:string_of_int 0 end_state;
  let module ConcatenateTransform = Type.VisitWithTransform.Make (struct
    type state = string

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive primitive -> state ^ primitive, annotation
        | Type.Parametric { name; arguments } -> "", Type.parametric (name ^ state) arguments
        | _ -> state, annotation
      in
      { Type.VisitWithTransform.transformed_annotation; new_state }


    let visit_children_before _ _ = true

    let visit_children_after = false
  end)
  in
  let end_state, transformed =
    ConcatenateTransform.visit
      ""
      (create ~variables:(fun _ -> None) "Foo[Bar[Baz, Bop], Bro[Loop, typing.Optional[Land]]]")
  in
  assert_types_equal
    transformed
    (create
       ~variables:(fun _ -> None)
       "Foo[BarBazBop[Baz, Bop], BroLoopLand[Loop, typing.Optional[Land]]]");
  assert_equal "" end_state;
  let module TopDownConcatenateTransform = Type.VisitWithTransform.Make (struct
    type state = string

    let visit state annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Primitive primitive -> "", Type.Primitive (state ^ primitive)
        | Type.Parametric { name; arguments } -> state ^ name, Type.parametric name arguments
        | _ -> state, annotation
      in
      { Type.VisitWithTransform.transformed_annotation; new_state }


    let visit_children_before _ _ = false

    let visit_children_after = true
  end)
  in
  let end_state, transformed =
    TopDownConcatenateTransform.visit
      ""
      (create ~variables:(fun _ -> None) "Foo[Bar[Bro[typing.Optional[Land]]]]")
  in
  assert_types_equal
    transformed
    (create ~variables:(fun _ -> None) "Foo[Bar[Bro[typing.Optional[FooBarBroLand]]]]");
  assert_equal "" end_state;
  let module CollectAnnotations = Type.VisitWithTransform.Make (struct
    type state = Type.t list

    let visit state annotation =
      {
        Type.VisitWithTransform.transformed_annotation = annotation;
        new_state = annotation :: state;
      }


    let visit_children_before _ _ = false

    let visit_children_after = true
  end)
  in
  let visited_annotations =
    CollectAnnotations.visit
      []
      (create ~variables:(fun _ -> None) "pyre_extensions.ReadOnly[typing.List[int]]")
    |> fst
  in
  assert_equal
    ~cmp:[%compare.equal: string list]
    ~printer:[%show: string list]
    ["int"; "typing.List[int]"; "pyre_extensions.PyreReadOnly[typing.List[int]]"]
    (List.map visited_annotations ~f:Type.show);
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
  let no_namespace_variable = Type.Variable.TypeVar.create "A" in
  let namespaced_variable_1 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  let namespaced_variable_2 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable { no_namespace_variable with namespace }
  in
  assert_false (Type.equal namespaced_variable_1 namespaced_variable_2);
  assert_equal (Type.namespace_insensitive_compare namespaced_variable_1 namespaced_variable_2) 0;
  assert_equal
    (Type.namespace_insensitive_compare
       (Type.list namespaced_variable_1)
       (Type.list namespaced_variable_2))
    0;
  ()


let test_namespace _ =
  let no_namespace_variable = Type.Variable.TypeVar.create "A" in
  let namespaced_variable_1 =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable.TypeVarVariable { no_namespace_variable with namespace }
  in
  let namespace_2 = Type.Variable.Namespace.create_fresh () in
  let namespaced_variable_2 =
    Type.Variable.TypeVarVariable { no_namespace_variable with namespace = namespace_2 }
  in
  assert_equal
    (Type.Variable.namespace namespaced_variable_1 ~namespace:namespace_2)
    namespaced_variable_2;
  ()


let test_mark_all_variables_as_bound _ =
  let variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  assert_false (Type.Variable.all_variables_are_resolved variable);
  let variable = Type.Variable.mark_all_variables_as_bound variable in
  assert_true (Type.Variable.all_variables_are_resolved variable);
  let callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.all_variables_are_resolved callable);
  let callable = Type.Variable.mark_all_variables_as_bound callable in
  assert_true (Type.Variable.all_variables_are_resolved callable);
  ()


let test_mark_all_variables_as_free _ =
  let variable =
    Type.Variable (Type.Variable.TypeVar.create "T") |> Type.Variable.mark_all_variables_as_bound
  in
  assert_true (Type.Variable.all_variables_are_resolved variable);
  let variable = Type.Variable.mark_all_variables_as_free variable in
  assert_false (Type.Variable.all_variables_are_resolved variable);
  let callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
    |> Type.Variable.mark_all_variables_as_bound
  in
  assert_true (Type.Variable.all_variables_are_resolved callable);
  let callable = Type.Variable.mark_all_variables_as_free callable in
  assert_false (Type.Variable.all_variables_are_resolved callable);
  ()


let test_namespace_all_free_variables _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  let bound_variable =
    Type.Variable.TypeVar.create "T2"
    |> Type.Variable.TypeVar.mark_as_bound
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; bound_variable] in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free =
    Type.Variable.TypeVar.create "T"
    |> Type.Variable.TypeVar.namespace ~namespace
    |> fun variable -> Type.Variable variable
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free; bound_variable]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T" |> Type.Variable.ParamSpec.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let annotation = Type.parametric "p" ![free_variable_callable; bound_variable_callable] in
  let namespace = Type.Variable.Namespace.create_fresh () in
  let namespaced_free_callable =
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T" |> Type.Variable.ParamSpec.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  assert_equal
    (Type.Variable.namespace_all_free_variables annotation ~namespace)
    (Type.parametric "p" ![namespaced_free_callable; bound_variable_callable]);
  ()


let test_mark_all_free_variables_as_escaped _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  let bound_variable =
    Type.Variable.TypeVar.create "T2"
    |> Type.Variable.TypeVar.mark_as_bound
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; bound_variable] in
  Type.Variable.Namespace.reset ();
  let escaped_free =
    let namespace = Type.Variable.Namespace.create_fresh () in
    Type.Variable.TypeVar.create "T"
    |> Type.Variable.TypeVar.mark_as_escaped
    |> Type.Variable.TypeVar.namespace ~namespace
    |> fun variable -> Type.Variable variable
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free; bound_variable]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let bound_variable_callable =
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T" |> Type.Variable.ParamSpec.mark_as_bound
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let annotation = Type.parametric "p" ![free_variable_callable; bound_variable_callable] in
  Type.Variable.Namespace.reset ();
  let escaped_free_callable =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T"
      |> Type.Variable.ParamSpec.mark_as_escaped
      |> Type.Variable.ParamSpec.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  Type.Variable.Namespace.reset ();
  assert_equal
    (Type.Variable.mark_all_free_variables_as_escaped annotation)
    (Type.parametric "p" ![escaped_free_callable; bound_variable_callable]);
  ()


let test_contains_escaped_free_variable _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable);
  let escaped_free =
    Type.Variable.TypeVar.create "T"
    |> Type.Variable.TypeVar.mark_as_escaped
    |> fun variable -> Type.Variable variable
  in
  assert_true (Type.Variable.contains_escaped_free_variable escaped_free);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  assert_false (Type.Variable.contains_escaped_free_variable free_variable_callable);
  let escaped_free_variable_callable =
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T" |> Type.Variable.ParamSpec.mark_as_escaped
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  assert_true (Type.Variable.contains_escaped_free_variable escaped_free_variable_callable);
  ()


let test_convert_all_escaped_free_variables_to_anys _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  let escaped_free =
    Type.Variable.TypeVar.create "T"
    |> Type.Variable.TypeVar.mark_as_escaped
    |> fun variable -> Type.Variable variable
  in
  let annotation = Type.parametric "p" ![free_variable; escaped_free] in
  assert_equal
    (Type.Variable.convert_all_escaped_free_variables_to_anys annotation)
    (Type.parametric "p" ![free_variable; Type.Any]);
  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let escaped_free_callable =
    let namespace = Type.Variable.Namespace.create_fresh () in
    let parameter_variadic =
      Type.Variable.ParamSpec.create "T"
      |> Type.Variable.ParamSpec.mark_as_escaped
      |> Type.Variable.ParamSpec.namespace ~namespace
    in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
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


let test_replace_all _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  let assert_equal actual expected =
    assert_equal ~cmp:Type.equal ~printer:Type.show expected actual
  in
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.replace_all (fun _ -> Some Type.integer) annotation)
    (Type.parametric "p" ![Type.integer; Type.integer]);
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.replace_all
       (fun _ -> Some Type.integer)
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer));
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.replace_all
       (fun _ -> None)
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable));
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.replace_all
       (fun _ -> Some Type.float)
       (Type.union [Type.literal_integer 2; Type.integer; free_variable]))
    (Type.union [Type.literal_integer 2; Type.integer; Type.float]);

  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  let no_parameter_callable =
    Type.Callable.create ~parameters:(Type.Callable.Defined []) ~annotation:Type.integer ()
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ParamSpec.replace_all
       (fun _ -> Some (Type.Callable.Defined []))
       (Type.parametric "p" ![Type.integer; free_variable_callable]))
    (Type.parametric "p" ![Type.integer; no_parameter_callable]);
  assert_equal
    (Type.Variable.GlobalTransforms.ParamSpec.replace_all
       (fun _ ->
         Some
           (Type.Callable.Defined [Named { name = "p"; annotation = Type.integer; default = false }]))
       (Type.parametric
          "G"
          [
            CallableParameters
              (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
          ]))
    (Type.parametric
       "G"
       [
         CallableParameters
           (Defined [Named { name = "p"; annotation = Type.integer; default = false }]);
       ]);
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let ts2_declaration, ts2_variable = type_var_tuple_declaration_and_variable "Ts2" in
  let assert_replaced ~replace annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | "Ts2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts2_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    assert_equal
      (Type.Variable.GlobalTransforms.TypeVarTuple.replace_all
         replace
         (Type.create
            ~variables
            ~aliases:(resolved_aliases aliases)
            (parse_single_expression ~preprocess:true annotation)))
      (Type.create
         ~variables
         ~aliases:(resolved_aliases aliases)
         (parse_single_expression ~preprocess:true expected))
  in
  (* Variadic tuples. *)
  let replace_with_concrete given =
    Option.some_if
      (Type.Variable.TypeVarTuple.equal given ts_variable)
      (Type.OrderedTypes.Concrete [Type.bool; Type.bool])
  in
  let replace_with_concatenation _ =
    Some
      (Type.OrderedTypes.Concatenation
         (Type.OrderedTypes.Concatenation.create
            ~prefix:[Type.bool]
            ~suffix:[Type.bool]
            ts_variable))
  in
  assert_replaced
    ~replace:replace_with_concrete
    "Foo[int, typing.Unpack[Ts], str]"
    "Foo[int, bool, bool, str]";
  assert_replaced
    ~replace:replace_with_concatenation
    "Foo[int, typing.Unpack[Ts], str]"
    "Foo[int, bool, typing.Unpack[Ts], bool, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "Foo[Bar[int, typing.Unpack[Ts], str], Bar[int, typing.Unpack[Ts2], str]]"
    "Foo[Bar[int, bool, bool, str], Bar[int, typing.Unpack[Ts2], str]]";
  assert_replaced ~replace:replace_with_concrete "typing.Tuple[int, str]" "typing.Tuple[int, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "typing.Tuple[int, typing.Unpack[Ts], str]"
    "typing.Tuple[int, bool, bool, str]";
  assert_replaced
    ~replace:replace_with_concatenation
    "typing.Tuple[int, typing.Unpack[Ts], str]"
    "typing.Tuple[int, bool, typing.Unpack[Ts], bool, str]";
  assert_replaced
    ~replace:replace_with_concrete
    "typing.Callable[[int, typing.Unpack[Ts], str], None]"
    "typing.Callable[[int, bool, bool, str], None]";
  assert_replaced
    ~replace:replace_with_concatenation
    "typing.Callable[[int, typing.Unpack[Ts], str], None]"
    "typing.Callable[[int, bool, typing.Unpack[Ts], bool, str], None]";

  let parse_string string =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | "Ts2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts2_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    Type.create
      ~variables
      ~aliases:(resolved_aliases aliases)
      (parse_single_expression ~preprocess:true string)
  in
  let replace_with_concrete = function
    | variable when Type.Variable.TypeVarTuple.equal variable ts_variable ->
        Some
          (Type.OrderedTypes.Concrete
             [
               parse_string "typing.Callable[[int], str]";
               parse_string "typing.Callable[[str], bool]";
             ])
    | variable when Type.Variable.TypeVarTuple.equal variable ts2_variable ->
        Some
          (Type.OrderedTypes.Concrete
             [
               parse_string "typing.Callable[[float], bytes]";
               parse_string "typing.Callable[[bytes], int]";
             ])
    | _ -> None
  in
  let replace_with_concatenation = function
    | variable when Type.Variable.TypeVarTuple.equal variable ts_variable ->
        Some
          (Type.OrderedTypes.Concatenation
             (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                (parse_string "typing.Callable[[int], int]")))
    | variable when Type.Variable.TypeVarTuple.equal variable ts2_variable ->
        Some (Type.OrderedTypes.Concatenation (Type.OrderedTypes.Concatenation.create ts_variable))
    | _ -> None
  in

  (* Compose. *)
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Compose[
        typing.Callable[[float], int],
        typing.Unpack[Ts]
      ]
    |}
    "pyre_extensions.Compose[typing.Callable[[float], int], typing.Callable[[int], str], \
     typing.Callable[[str], bool]]";
  assert_replaced
    ~replace:replace_with_concrete
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          typing.Unpack[Ts],
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
        typing.Unpack[Ts]
      ]
    |}
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        typing.Unpack[
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
        typing.Unpack[Ts2]
      ]
    |}
    {|
      pyre_extensions.Compose[
        typing.Callable[[int], int],
        typing.Unpack[Ts]
      ]
    |};
  ()


let test_collect_all _ =
  let free_variable = Type.Variable (Type.Variable.TypeVar.create "T") in
  let annotation = Type.parametric "p" ![free_variable; Type.integer] in
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.collect_all annotation)
    [Type.Variable.TypeVar.create "T"];
  assert_equal
    (Type.Variable.GlobalTransforms.TypeVar.collect_all
       (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation free_variable)))
    [Type.Variable.TypeVar.create "T"];
  let free_variable_callable =
    let parameter_variadic = Type.Variable.ParamSpec.create "T" in
    Type.Callable.create
      ~parameters:(Type.Callable.FromParamSpec (empty_head parameter_variadic))
      ~annotation:Type.integer
      ()
  in
  assert_equal
    (Type.Variable.GlobalTransforms.ParamSpec.collect_all
       (Type.parametric "p" ![Type.integer; free_variable_callable]))
    [Type.Variable.ParamSpec.create "T"];
  assert_equal
    (Type.Variable.GlobalTransforms.ParamSpec.collect_all
       (Type.parametric
          "G"
          [
            CallableParameters
              (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
          ]))
    [Type.Variable.ParamSpec.create "TParams"];

  (* Variadic tuples. *)
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let ts2_declaration, ts2_variable = type_var_tuple_declaration_and_variable "Ts2" in
  let assert_collected annotation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | "Ts2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts2_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    assert_equal
      ~printer:[%show: Type.Variable.TypeVarTuple.t list]
      expected
      (Type.Variable.GlobalTransforms.TypeVarTuple.collect_all
         (Type.create
            ~variables
            ~aliases:(resolved_aliases aliases)
            (parse_single_expression ~preprocess:true annotation)))
  in
  assert_collected "typing.Tuple[int, str]" [];
  assert_collected "typing.Tuple[int, typing.Unpack[Ts], str]" [ts_variable];
  assert_collected "Foo[int, typing.Unpack[Ts], str]" [ts_variable];
  assert_collected
    "Foo[Bar[int, typing.Unpack[Ts], str], Baz[typing.Unpack[Ts2]]]"
    [ts_variable; ts2_variable];
  assert_collected
    "typing.Callable[[Variable(int, typing.Unpack[Ts], str)], typing.Tuple[int, \
     typing.Unpack[Ts2], str]]"
    [ts2_variable; ts_variable];
  assert_collected
    "typing.Callable[[Variable(int, typing.Unpack[Ts], str)], typing.Callable[[Variable(int, \
     typing.Unpack[Ts2], str)], bool]]"
    [ts2_variable; ts_variable];

  (* Compose. *)
  assert_collected
    {|
      pyre_extensions.Compose[
        typing.Unpack[Ts],
        typing.Callable[[int], int]
      ]
    |}
    [ts_variable];
  assert_collected
    {|
      pyre_extensions.Compose[
        pyre_extensions.Compose[
          typing.Callable[[int], int],
          typing.Unpack[Ts],
        ],
        typing.Callable[[int], int]
      ]
    |}
    [ts_variable];
  ()


(* location insensitive compare for type variables with bounds *)
(* TODO T197463208: improve the comparator function *)
let declaration_equality (d1 : 'd) (d2 : 'd) =
  match d1, d2 with
  | Some d1, Some d2 ->
      String.equal
        ([%show: Type.Variable.Declaration.t] d1)
        ([%show: Type.Variable.Declaration.t] d2)
  | _ -> false


let test_parse_type_variable_declarations _ =
  let assert_parses_declaration_with_bounds expression expected =
    assert_equal
      ?cmp:(Some declaration_equality)
      (Some expected)
      (Type.Variable.Declaration.parse
         (parse_single_expression expression)
         ~target:(Reference.create "target"))
  in

  let assert_parses_declaration expression expected =
    assert_equal
      (Some expected)
      (Type.Variable.Declaration.parse
         (parse_single_expression expression)
         ~target:(Reference.create "target"))
  in
  let assert_declaration_does_not_parse expression =
    assert_equal
      None
      (Type.Variable.Declaration.parse
         (parse_single_expression expression)
         ~target:(Reference.create "target"))
  in
  assert_parses_declaration
    "pyre_extensions.ParameterSpecification('Tparams')"
    (Type.Variable.Declaration.DParamSpec { name = "target" });
  assert_declaration_does_not_parse "pyre_extensions.ParameterSpecification('Tparams', int, str)";
  assert_parses_declaration
    "typing.TypeVarTuple('Ts')"
    (Type.Variable.Declaration.DTypeVarTuple { name = "target" });
  assert_parses_declaration
    "typing_extensions.TypeVarTuple('Ts')"
    (Type.Variable.Declaration.DTypeVarTuple { name = "target" });
  assert_parses_declaration
    "pyre_extensions.TypeVarTuple('Ts')"
    (Type.Variable.Declaration.DTypeVarTuple { name = "target" });

  assert_parses_declaration
    "typing.TypeVar('_T')"
    (DTypeVar
       {
         name = "target";
         constraints = Unconstrained;
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration
    "typing.TypeVar('_T', covariant=True)"
    (DTypeVar
       {
         name = "target";
         constraints = Unconstrained;
         variance = Type.Record.PreInferenceVariance.P_Covariant;
         infer_variance = false;
       });

  assert_parses_declaration
    "typing.TypeVar('_T', covariant=False)"
    (DTypeVar
       {
         name = "target";
         constraints = Unconstrained;
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration
    "typing.TypeVar('_T', contravariant=True)"
    (DTypeVar
       {
         name = "target";
         constraints = Unconstrained;
         variance = Type.Record.PreInferenceVariance.P_Contravariant;
         infer_variance = false;
       });

  assert_parses_declaration
    "typing.TypeVar('_T', name=int)"
    (DTypeVar
       {
         name = "target";
         constraints = Unconstrained;
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });
  assert_parses_declaration_with_bounds
    "typing.TypeVar('_T', int)"
    (DTypeVar
       {
         name = "target";
         constraints = Type.Record.TypeVarConstraints.Explicit [Type.expression Type.integer];
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration_with_bounds
    "typing.TypeVar('_T', bound='C')"
    (DTypeVar
       {
         name = "target";
         constraints =
           Type.Record.TypeVarConstraints.Bound (Type.expression (Type.Primitive "\"C\""));
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration_with_bounds
    "typing.TypeVar('_T', 'C', X)"
    (DTypeVar
       {
         name = "target";
         constraints =
           Type.Record.TypeVarConstraints.Explicit
             [Type.expression (Type.Primitive "\"C\""); Type.expression (Type.Primitive "X")];
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration_with_bounds
    "typing.TypeVar('_T', int, name=float)"
    (DTypeVar
       {
         name = "target";
         constraints = Type.Record.TypeVarConstraints.Explicit [Type.expression Type.integer];
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_parses_declaration_with_bounds
    "typing.TypeVar('_CallableT', bound='typing.Callable')"
    (DTypeVar
       {
         name = "target";
         constraints =
           Type.Record.TypeVarConstraints.Bound
             (Type.expression (Type.Primitive "\"typing.Callable\""));
         variance = Type.Record.PreInferenceVariance.P_Invariant;
         infer_variance = false;
       });

  assert_declaration_does_not_parse "typing.TypeVarTuple('Ts', covariant=True)";

  ()


let test_starred_annotation_expression _ =
  let assert_starred_expression concatenation expression =
    assert_equal
      ~printer:Expression.show
      ~cmp:(fun left right -> Expression.location_insensitive_compare left right = 0)
      (parse_single_expression ~coerce_special_methods:true expression)
      (Type.OrderedTypes.to_starred_annotation_expression ~expression:Type.expression concatenation)
  in
  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
  assert_starred_expression
    (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic)
    "typing.Unpack[Ts]";
  assert_starred_expression
    (Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] ~suffix:[Type.string] variadic)
    "typing.Unpack[(int, typing.Unpack[Ts], str)]";
  ()


let test_concatenation_from_unpack_expression _ =
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let assert_concatenation expression concatenation =
    let parse_annotation expression =
      let aliases ?replace_unbound_parameters_with_any:_ = function
        | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
        | _ -> None
      in
      let variables = make_variables ~aliases in
      Type.create
        ~variables
        ~aliases:(resolved_aliases aliases)
        (parse_single_expression ~preprocess:true (Expression.show expression))
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
    "typing.Unpack[Ts]"
    (Some (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] ts_variable));
  assert_concatenation
    "typing.Unpack[typing.Tuple[int, typing.Unpack[Ts], str]]"
    (Some
       (Type.OrderedTypes.Concatenation.create
          ~prefix:[Type.integer]
          ~suffix:[Type.string]
          ts_variable));
  assert_concatenation "int" None;
  ()


let test_split_ordered_types _ =
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let assert_split ?(split_both_ways = true) left right expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let left =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true ("typing.Tuple" ^ left))
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple elements"
    in
    let right =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true ("typing.Tuple" ^ right))
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple elements"
    in
    assert_equal
      ~printer:[%show: Type.t Type.OrderedTypes.ordered_type_split option]
      expected
      (Type.OrderedTypes.split_matching_elements_by_length left right);
    if split_both_ways then
      let flip_splits { Type.OrderedTypes.prefix_pairs; middle_pair; suffix_pairs } =
        let swap (a, b) = b, a in
        {
          Type.OrderedTypes.prefix_pairs = List.map prefix_pairs ~f:swap;
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
    "[int, typing.Unpack[Ts], int, str]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concrete [Type.string; Type.bool],
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] ts_variable) );
         suffix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
       });
  (* Not enough elements. *)
  assert_split "[int]" "[int, str, typing.Unpack[Ts]]" None;
  assert_split "[str]" "[typing.Unpack[Ts], int, str]" None;
  assert_split "[int, int]" "[int, typing.Unpack[Ts], int, str]" None;
  assert_split "[int, int]" "[int, typing.Unpack[Ts], int, str]" None;
  (* *Ts can match against zero elements. *)
  assert_split
    "[int, int, str]"
    "[int, typing.Unpack[Ts], int, str]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concrete [],
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] ts_variable) );
         suffix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
       });

  (* Concatenation vs concatenation. *)
  assert_split
    "[int, typing.Unpack[Ts], bool]"
    "[int, typing.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] ts_variable),
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] ts_variable) );
         suffix_pairs = [Type.bool, Type.integer];
       });
  assert_split
    "[int, str, typing.Unpack[Ts], bool]"
    "[int, typing.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[Type.string] ~suffix:[] ts_variable),
             Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[Type.string] ts_variable)
           );
         suffix_pairs = [Type.bool, Type.integer];
       });
  (* There are no matching elements of known length in either the prefix_pairs or the
     suffix_pairs. *)
  assert_split
    "[typing.Unpack[Ts], str]"
    "[int, typing.Unpack[Ts]]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[Type.string] ts_variable),
             Concatenation
               (Type.OrderedTypes.Concatenation.create
                  ~prefix:[Type.integer]
                  ~suffix:[]
                  ts_variable) );
         suffix_pairs = [];
       });
  assert_split
    "[int, str, typing.Unpack[typing.Tuple[str, ...]], str, bool]"
    "[int, typing.Unpack[typing.Tuple[int, ...]], bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.integer];
         middle_pair = Concrete [Type.string], Concrete [Type.integer];
         suffix_pairs = [Type.string, Type.integer; Type.bool, Type.bool];
       });
  assert_split
    "[int, typing.Unpack[typing.Tuple[str, ...]], bool]"
    "[int, str, typing.Unpack[typing.Tuple[str, ...]], str, bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
         middle_pair = Concrete [Type.string], Concrete [Type.string];
         suffix_pairs = [Type.string, Type.string; Type.bool, Type.bool];
       });
  assert_split
    "[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    "[int, typing.Unpack[typing.Tuple[str, ...]], str, bool]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer; Type.string, Type.string];
         middle_pair = Concrete [Type.string], Concrete [Type.string];
         suffix_pairs = [Type.string, Type.string; Type.bool, Type.bool];
       });
  assert_split
    "[typing.Unpack[typing.Tuple[str, ...]], int]"
    "[int, typing.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.string, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create ts_variable) );
         suffix_pairs = [Type.integer, Type.integer];
       });
  assert_split
    "[int, int, typing.Unpack[typing.Tuple[str, ...]], int]"
    "[int, typing.Unpack[Ts], int]"
    (Some
       {
         prefix_pairs = [Type.integer, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                  ~prefix:[Type.integer]
                  Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create ts_variable) );
         suffix_pairs = [Type.integer, Type.integer];
       });
  assert_split
    "[typing.Unpack[typing.Tuple[str, ...]]]"
    "[typing.Unpack[Ts], str]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create ts_variable) );
         suffix_pairs = [Type.string, Type.string];
       });
  assert_split
    "[typing.Unpack[typing.Tuple[str, ...]], int]"
    "[int, typing.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [Type.string, Type.integer];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create ts_variable) );
         suffix_pairs = [Type.string, Type.string; Type.integer, Type.integer];
       });
  assert_split
    "[str, typing.Unpack[typing.Tuple[str, ...]], int]"
    "[typing.Unpack[Ts], str, int]"
    (Some
       {
         prefix_pairs = [];
         middle_pair =
           ( Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                  ~prefix:[Type.string]
                  Type.string),
             Concatenation (Type.OrderedTypes.Concatenation.create ts_variable) );
         suffix_pairs = [Type.string, Type.string; Type.integer, Type.integer];
       });
  assert_split
    "[typing.Unpack[typing.Tuple[str, ...]]]"
    "[str, str]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.string];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split
    "[str, str, typing.Unpack[typing.Tuple[int, ...]]]"
    "[str, str]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.string];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split
    "[str, typing.Unpack[typing.Tuple[int, ...]], str]"
    "[str, bool]"
    (Some
       {
         prefix_pairs = [Type.string, Type.string; Type.string, Type.bool];
         middle_pair = Concrete [], Concrete [];
         suffix_pairs = [];
       });
  assert_split "[str, str, str, typing.Unpack[typing.Tuple[int, ...]]]" "[str, str]" None;
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
  let ts_declaration, _ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let assert_coalesce ordered_types expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let parse_ordered_type type_ =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true ("typing.Tuple" ^ type_))
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
    (Some "[int, str, typing.Unpack[typing.Tuple[int, ...]], bool, bool]");
  assert_coalesce
    ["[int, str]"; "[typing.Unpack[Ts]]"; "[bool, bool]"]
    (Some "[int, str, typing.Unpack[Ts], bool, bool]");
  assert_coalesce ["[int, ...]"; "[typing.Unpack[Ts]]"] None;
  assert_coalesce ["[int, ...]"; "[int, ...]"] (Some "[int, ...]");
  assert_coalesce ["[int, ...]"; "[str, ...]"] (Some "[typing.Union[int, str], ...]");
  assert_coalesce
    [
      "[int, int]";
      "[int, str, typing.Unpack[typing.Tuple[int, ...]], bool, bool]";
      "[bool, bool]";
      "[int, str, typing.Unpack[typing.Tuple[str, ...]], bool, bool]";
      "[bool, bool]";
      "[int, str, typing.Unpack[typing.Tuple[str, ...]], bool, bool]";
      "[bool, bool]";
    ]
    (Some
       "[int, int, int, str, typing.Unpack[typing.Tuple[typing.Union[int, bool, str], ...]], bool, \
        bool, bool, bool]");
  ()


let test_drop_prefix_ordered_type _ =
  let open Type.OrderedTypes in
  let assert_drop_prefix ~length actual expected_tuple =
    let ts_declaration, _ts_variable = type_var_tuple_declaration_and_variable "Ts" in
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let extract_ordered_type string =
      match
        parse_single_expression string |> Type.create ~variables ~aliases:(resolved_aliases aliases)
      with
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
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, str, typing.Unpack[typing.Tuple[int, ...]]]");
  assert_drop_prefix
    ~length:1
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[int, ...]], str]"
    (Some "typing.Tuple[str, typing.Unpack[typing.Tuple[int, ...]], str]");
  assert_drop_prefix
    ~length:2
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, ...]");
  assert_drop_prefix
    ~length:2
    "typing.Tuple[int, str, typing.Unpack[Ts]]"
    (Some "typing.Tuple[typing.Unpack[Ts]]");
  assert_drop_prefix
    ~length:3
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[int, ...]]]"
    (Some "typing.Tuple[int, ...]");
  assert_drop_prefix ~length:3 "typing.Tuple[int, str, typing.Unpack[Ts]]" None;
  ()


let test_index_ordered_type _ =
  let assert_index ~python_index tuple expected =
    let ts_declaration, _ts_variable = type_var_tuple_declaration_and_variable "Ts" in
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let extract_ordered_type string =
      match
        parse_single_expression string |> Type.create ~variables ~aliases:(resolved_aliases aliases)
      with
      | Type.Tuple ordered_type -> ordered_type
      | _ -> failwith "expected tuple"
    in
    assert_equal
      ~cmp:[%equal: Type.t option]
      ~printer:[%show: Type.t option]
      (expected
      >>| parse_single_expression
      >>| Type.create ~variables ~aliases:(resolved_aliases aliases))
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
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]]]"
    (Some "int");
  assert_index
    ~python_index:2
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]]]"
    (Some "bool");
  assert_index
    ~python_index:99
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]]]"
    (Some "bool");
  assert_index
    ~python_index:(-1)
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "str");
  assert_index
    ~python_index:(-2)
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "bool");
  assert_index
    ~python_index:(-99)
    "typing.Tuple[int, str, typing.Unpack[typing.Tuple[bool, ...]], str]"
    (Some "bool");
  assert_index ~python_index:1 "typing.Tuple[int, str, typing.Unpack[Ts], bool]" (Some "str");
  assert_index ~python_index:(-1) "typing.Tuple[int, str, typing.Unpack[Ts], bool]" (Some "bool");
  assert_index ~python_index:2 "typing.Tuple[int, str, typing.Unpack[Ts], bool]" None;
  assert_index ~python_index:(-2) "typing.Tuple[int, str, typing.Unpack[Ts], bool]" None;
  ()


let test_zip_variables_with_parameters _ =
  let t_declaration, t_variable = type_var_declaration_and_variable "T" in
  let t2_declaration, t2_variable = type_var_declaration_and_variable "T2" in
  let ts_declaration, ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let ts2_declaration, ts2_variable = type_var_tuple_declaration_and_variable "Ts2" in
  let tparams_declaration, tparams_variable = param_spec_declaration_and_variable "TParams" in
  let assert_zipped ~generic_class ~instantiation expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration t_declaration)
      | "T2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration t2_declaration)
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | "Ts2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts2_declaration)
      | "TParams" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration tparams_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let arguments =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true instantiation)
      with
      | Type.Parametric { arguments; _ } -> arguments
      | _ -> failwith "expected Parametric"
    in
    let variables =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true generic_class)
      with
      | Type.Parametric { arguments; _ } ->
          let variables = List.map ~f:Type.Argument.to_variable arguments |> Option.all in
          Option.value_exn variables
      | _ -> failwith "expected Parametric"
    in
    assert_equal
      ~printer:[%show: Type.Variable.variable_zip_result list option]
      ~cmp:[%equal: Type.Variable.variable_zip_result list option]
      expected
      (Type.Variable.zip_variables_with_arguments_including_mismatches ~arguments variables)
  in
  assert_zipped
    ~generic_class:"Generic[T, T2]"
    ~instantiation:"Foo[int, str]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair = Type.Variable.TypeVarPair (t2_variable, Type.string);
           received_argument = Single Type.string;
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
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParamSpecPair
               (tparams_variable, Type.Callable.FromParamSpec (empty_head tparams_variable));
           received_argument =
             CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable);
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
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.Any);
           received_argument =
             CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable);
         };
         {
           variable_pair = Type.Variable.ParamSpecPair (tparams_variable, Undefined);
           received_argument = Single Type.integer;
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
             Type.Variable.ParamSpecPair
               ( tparams_variable,
                 Defined
                   (Type.Callable.prepend_anonymous_parameters
                      ~head:[Type.integer; Type.string]
                      ~tail:[]) );
           received_argument =
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
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParamSpecPair
               ( tparams_variable,
                 Defined
                   [
                     PositionalOnly { index = 0; annotation = Type.string; default = false };
                     PositionalOnly { index = 1; annotation = Type.bool; default = false };
                   ] );
           received_argument =
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
    ~generic_class:"Generic[typing.Unpack[Ts]]"
    ~instantiation:"Foo[typing.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair
               (ts_variable, Concatenation (Type.OrderedTypes.Concatenation.create ts_variable));
           received_argument =
             Single (Tuple (Concatenation (Type.OrderedTypes.Concatenation.create ts_variable)));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, typing.Unpack[Ts]]"
    ~instantiation:"Foo[int]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair = Type.Variable.TypeVarTuplePair (ts_variable, Concrete []);
           received_argument = Single (Tuple (Concrete []));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, typing.Unpack[Ts]]"
    ~instantiation:"Foo[int, str, int, bool]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair
               (ts_variable, Concrete [Type.string; Type.integer; Type.bool]);
           received_argument = Single (Tuple (Concrete [Type.string; Type.integer; Type.bool]));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, typing.Unpack[Ts], T2]"
    ~instantiation:"Foo[int, str, typing.Unpack[Ts2], bool, str]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair
               ( ts_variable,
                 Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.string]
                      ~suffix:[Type.bool]
                      ts2_variable) );
           received_argument =
             Single
               (Tuple
                  (Concatenation
                     (Type.OrderedTypes.Concatenation.create
                        ~prefix:[Type.string]
                        ~suffix:[Type.bool]
                        ts2_variable)));
         };
         {
           variable_pair = Type.Variable.TypeVarPair (t2_variable, Type.string);
           received_argument = Single Type.string;
         };
       ]);
  (* Mix of unary, variadic, and ParamSpec. *)
  assert_zipped
    ~generic_class:"Generic[T, TParams, typing.Unpack[Ts]]"
    ~instantiation:"Foo[int, TParams, str, bool]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParamSpecPair
               (tparams_variable, Type.Callable.FromParamSpec (empty_head tparams_variable));
           received_argument =
             CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable);
         };
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair (ts_variable, Concrete [Type.string; Type.bool]);
           received_argument = Single (Tuple (Concrete [Type.string; Type.bool]));
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[T, TParams, typing.Unpack[Ts]]"
    ~instantiation:"Foo[int, TParams, str, typing.Unpack[Ts2]]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.integer);
           received_argument = Single Type.integer;
         };
         {
           variable_pair =
             Type.Variable.ParamSpecPair
               (tparams_variable, Type.Callable.FromParamSpec (empty_head tparams_variable));
           received_argument =
             CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable);
         };
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair
               ( ts_variable,
                 Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      ~prefix:[Type.string]
                      ~suffix:[]
                      ts2_variable) );
           received_argument =
             Single
               (Tuple
                  (Concatenation
                     (Type.OrderedTypes.Concatenation.create
                        ~prefix:[Type.string]
                        ~suffix:[]
                        ts2_variable)));
         };
       ]);

  (* Wrong kind of parameter passed to `T` and `TParams`. *)
  assert_zipped
    ~generic_class:"Generic[T]"
    ~instantiation:"Foo[typing.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair = Type.Variable.TypeVarPair (t_variable, Type.Any);
           received_argument =
             Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable);
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[TParams]"
    ~instantiation:"Foo[typing.Unpack[Ts]]"
    (Some
       [
         {
           variable_pair = Type.Variable.ParamSpecPair (tparams_variable, Undefined);
           received_argument =
             Unpacked (Type.OrderedTypes.Concatenation.create_unpackable ts_variable);
         };
       ]);
  assert_zipped
    ~generic_class:"Generic[typing.Unpack[Ts]]"
    ~instantiation:"Foo[TParams]"
    (Some
       [
         {
           variable_pair =
             Type.Variable.TypeVarTuplePair (ts_variable, Type.Variable.TypeVarTuple.any);
           received_argument =
             Single
               (Type.parametric
                  Type.Variable.TypeVarTuple.synthetic_class_name_for_error
                  [CallableParameters (Type.Variable.ParamSpec.self_reference tparams_variable)]);
         };
       ]);
  (* We forbid

     class Foo(Generic[T, *Ts]): ...

     def foo(x: Foo[*Ts]) -> None: ...

     because `Ts` might be an empty tuple, in which case the generic `T` won't be bound to any
     type. *)
  assert_zipped
    ~generic_class:"Generic[T, typing.Unpack[Ts]]"
    ~instantiation:"Foo[typing.Unpack[Ts2]]"
    None;
  assert_zipped
    ~generic_class:"Generic[typing.Unpack[Ts], typing.Unpack[Ts2]]"
    ~instantiation:"Foo[int, str]"
    None;
  ()


let test_zip_on_two_parameter_lists _ =
  let t_declaration, t_variable = type_var_declaration_and_variable "T" in
  let t2_declaration, _t2_variable = type_var_declaration_and_variable "T2" in
  let ts_declaration, _ts_variable = type_var_tuple_declaration_and_variable "Ts" in
  let ts2_declaration, _ts2_variable = type_var_tuple_declaration_and_variable "Ts2" in
  let tparams_declaration, _tparams_variable = param_spec_declaration_and_variable "TParams" in
  let assert_zipped ~generic_class ~left ~right expected =
    let aliases ?replace_unbound_parameters_with_any:_ = function
      | "T" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration t_declaration)
      | "T2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration t2_declaration)
      | "Ts" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts_declaration)
      | "Ts2" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration ts2_declaration)
      | "TParams" -> Some (TypeAliasEnvironment.RawAlias.VariableDeclaration tparams_declaration)
      | _ -> None
    in
    let variables = make_variables ~aliases in
    let left_arguments =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true left)
      with
      | Type.Parametric { arguments; _ } -> arguments
      | _ -> failwith "expected Parametric"
    in
    let right_arguments =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true right)
      with
      | Type.Parametric { arguments; _ } -> arguments
      | _ -> failwith "expected Parametric"
    in
    let variables =
      match
        Type.create
          ~variables
          ~aliases:(resolved_aliases aliases)
          (parse_single_expression ~preprocess:true generic_class)
      with
      | Type.Parametric { arguments; _ } ->
          let variables = List.map ~f:Type.Argument.to_variable arguments |> Option.all in
          Option.value_exn variables
      | _ -> failwith "expected Parametric"
    in
    assert_equal
      ~printer:[%show: (Type.Variable.pair * Type.Variable.pair) list option]
      ~cmp:[%equal: (Type.Variable.pair * Type.Variable.pair) list option]
      expected
      (Type.Variable.zip_variables_with_two_argument_lists
         ~left_arguments
         ~right_arguments
         variables)
  in
  assert_zipped
    ~generic_class:"Generic[T]"
    ~left:"Child[int]"
    ~right:"Base[str]"
    (Some
       [
         ( Type.Variable.TypeVarPair (t_variable, Type.integer),
           Type.Variable.TypeVarPair (t_variable, Type.string) );
       ]);
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

  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
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
      ~printer:[%show: Type.TypedDictionary.typed_dictionary_field list option]
      expected
      (Type.TypedDictionary.fields_from_constructor constructor String.Map.empty)
  in
  let fields =
    [
      {
        Type.TypedDictionary.name = "name";
        annotation = Type.string;
        required = true;
        readonly = false;
      };
      {
        Type.TypedDictionary.name = "year";
        annotation = Type.integer;
        required = false;
        readonly = false;
      };
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


let test_parameter_create _ =
  assert_equal
    (Type.Callable.CallableParamType.create
       [{ Type.Callable.CallableParamType.name = "__"; annotation = Type.integer; default = false }])
    [
      Type.Callable.CallableParamType.Named
        { name = "__"; annotation = Type.integer; default = false };
    ]


let test_resolve_alias_before_handling_callable _ =
  let assert_resolved_getitem_callee ~resolve_aliases aliased bare =
    let aliases ?replace_unbound_parameters_with_any:_ (annotation : string) =
      Some (TypeAliasEnvironment.RawAlias.TypeAlias (resolve_aliases annotation))
    in
    let variables = make_variables ~aliases in
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression bare |> Type.create ~variables ~aliases:Type.resolved_empty_aliases)
      (parse_single_expression aliased |> Type.create ~variables ~aliases:(resolved_aliases aliases))
  in

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> Type.Primitive annotation)
    "bar.baz.Callable[[int], str]"
    "typing.Callable[[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | "bar.baz.Callable" -> Type.Callable.create ~annotation:Type.Any ~parameters:Undefined ()
      | annotation -> Type.Primitive annotation)
    "bar.baz.Callable[[int], str]"
    "typing.Callable[[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | "bar.baz.CallableAlias" ->
          Type.Callable.create
            ~annotation:Type.integer
            ~parameters:
              (Defined
                 [PositionalOnly { index = 0; annotation = Type.variable "T"; default = false }])
            ()
      | annotation -> Type.Primitive annotation)
    "bar.baz.CallableAlias[str]"
    "typing.Callable[[str], int]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(function
      | "bar.baz.Callable" -> Type.Primitive "typing.Callable"
      | annotation -> Type.Primitive annotation)
    "bar.baz.Callable[[int], str][[int], str][[int], str]"
    "typing.Callable[[int], str][[int], str][[int], str]";

  assert_resolved_getitem_callee
    ~resolve_aliases:(fun annotation -> Type.Primitive annotation)
    "not_an_alias.Callable[[int], str]"
    "not_an_alias.Callable";

  assert_resolved_getitem_callee
    ~resolve_aliases:(fun annotation -> Type.Primitive annotation)
    "Foo[int].Callable[[int], str]"
    "Foo.Callable";
  ()


let test_class_attribute_lookups_for_type _ =
  let assert_class_data annotation expected =
    assert_equal
      ~printer:(fun x ->
        [%sexp_of: Type.class_attribute_lookup_data list option] x |> Sexp.to_string_hum)
      expected
      (Type.class_attribute_lookups_for_type annotation)
  in
  assert_class_data Type.Any (Some []);
  assert_class_data
    (Type.builtins_type Type.integer)
    (Some
       [
         {
           type_for_lookup = Type.integer;
           accessed_through_class = true;
           class_name = "int";
           accessed_through_readonly = false;
         };
       ]);
  assert_class_data
    (Type.optional Type.integer)
    (Some
       [
         {
           type_for_lookup = Type.optional Type.integer;
           accessed_through_class = false;
           class_name = "typing.Optional";
           accessed_through_readonly = false;
         };
       ]);
  assert_class_data
    (Type.union [Type.integer; Type.string])
    (Some
       [
         {
           type_for_lookup = Type.integer;
           accessed_through_class = false;
           class_name = "int";
           accessed_through_readonly = false;
         };
         {
           type_for_lookup = Type.string;
           accessed_through_class = false;
           class_name = "str";
           accessed_through_readonly = false;
         };
       ]);
  assert_class_data
    (Type.union [Type.Primitive "Foo"; Type.list Type.integer])
    (Some
       [
         {
           type_for_lookup = Type.list Type.integer;
           accessed_through_class = false;
           class_name = "list";
           accessed_through_readonly = false;
         };
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = false;
           class_name = "Foo";
           accessed_through_readonly = false;
         };
       ]);
  assert_class_data
    (Type.union [Type.Primitive "Foo"; Type.list Type.integer])
    (Some
       [
         {
           type_for_lookup = Type.list Type.integer;
           accessed_through_class = false;
           class_name = "list";
           accessed_through_readonly = false;
         };
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = false;
           class_name = "Foo";
           accessed_through_readonly = false;
         };
       ]);
  let tree_annotation =
    Type.RecursiveType.create
      ~name:"Tree"
      ~body:(Type.union [Type.integer; Type.tuple [Type.Primitive "Foo"; Type.Primitive "Tree"]])
  in

  assert_class_data
    tree_annotation
    (Some
       [
         {
           type_for_lookup = Type.integer;
           accessed_through_class = false;
           class_name = "int";
           accessed_through_readonly = false;
         };
         {
           type_for_lookup = Type.tuple [Type.Primitive "Foo"; tree_annotation];
           accessed_through_class = false;
           class_name = "tuple";
           accessed_through_readonly = false;
         };
       ]);
  let recursive_list =
    Type.RecursiveType.create
      ~name:"RecursiveList"
      ~body:(Type.list (Type.union [Type.integer; Type.Primitive "RecursiveList"]))
  in

  assert_class_data
    recursive_list
    (Some
       [
         {
           type_for_lookup = Type.list (Type.union [Type.integer; recursive_list]);
           accessed_through_class = false;
           class_name = "list";
           accessed_through_readonly = false;
         };
       ]);
  (* TODO(T44784951): We should forbid defining a directly recursive type like this. This regression
     test is here just to test we don't go into an infinite loop in case one makes it through. *)
  let directly_recursive_type =
    Type.RecursiveType.create ~name:"Tree" ~body:(Type.union [Type.integer; Type.Primitive "Tree"])
  in
  assert_class_data
    directly_recursive_type
    (Some
       [
         {
           type_for_lookup = Type.integer;
           accessed_through_class = false;
           class_name = "int";
           accessed_through_readonly = false;
         };
       ]);
  (* ReadOnly types. *)
  assert_class_data
    (Type.PyreReadOnly.create (Type.Primitive "Foo"))
    (Some
       [
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = false;
           class_name = "Foo";
           accessed_through_readonly = true;
         };
       ]);
  assert_class_data
    (Type.union [Type.PyreReadOnly.create (Type.Primitive "Foo"); Type.list (Type.Primitive "Foo")])
    (Some
       [
         {
           type_for_lookup = Type.list (Type.Primitive "Foo");
           accessed_through_class = false;
           class_name = "list";
           accessed_through_readonly = false;
         };
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = false;
           class_name = "Foo";
           accessed_through_readonly = true;
         };
       ]);
  assert_class_data
    (Type.builtins_type (Type.PyreReadOnly.create (Type.Primitive "Foo")))
    (Some
       [
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = true;
           class_name = "Foo";
           accessed_through_readonly = true;
         };
       ]);
  assert_class_data
    (Type.PyreReadOnly.create (Type.builtins_type (Type.Primitive "Foo")))
    (Some
       [
         {
           type_for_lookup = Type.Primitive "Foo";
           accessed_through_class = true;
           class_name = "Foo";
           accessed_through_readonly = true;
         };
       ]);
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
  let ts = Type.Variable.TypeVarTuple.create "Ts" in
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
    (Type.PyreReadOnly (Type.list Type.integer))
    ~expected_full:"pyre_extensions.PyreReadOnly[typing.List[int]]"
    ~expected_concise:"pyre_extensions.PyreReadOnly[list[int]]";
  ()


let test_is_truthy _ =
  let assert_truthy ~expected type_ =
    parse_single_expression type_
    |> Type.create ~variables:(fun _ -> None) ~aliases:Type.resolved_empty_aliases
    |> Type.is_truthy
    |> assert_bool_equals ~expected
  in
  assert_truthy ~expected:true "typing_extensions.Literal[True]";
  assert_truthy ~expected:false "None";
  assert_truthy ~expected:true "typing.Callable[[int, str], int]";
  assert_truthy ~expected:true "typing_extensions.Literal[42]";
  assert_truthy ~expected:false "typing_extensions.Literal[0]";
  assert_truthy ~expected:false "typing_extensions.Literal['']";
  assert_truthy ~expected:false "typing_extensions.Literal[b'']";
  assert_truthy ~expected:true "typing_extensions.Literal['hello']";
  assert_truthy ~expected:true "typing_extensions.Literal[b'hello']";
  assert_truthy ~expected:true "typing.Annotated[typing_extensions.Literal[True], 1]";
  assert_truthy ~expected:true "pyre_extensions.ReadOnly[typing_extensions.Literal[True]]";
  assert_truthy
    ~expected:true
    "typing.Union[typing_extensions.Literal[True], typing_extensions.Literal[1]]";
  assert_truthy
    ~expected:false
    "typing.Union[typing_extensions.Literal[True], typing_extensions.Literal[0]]";
  ()


let test_is_falsy _ =
  let assert_falsy ~expected type_ =
    parse_single_expression type_
    |> Type.create ~variables:(fun _ -> None) ~aliases:Type.resolved_empty_aliases
    |> Type.is_falsy
    |> assert_bool_equals ~expected
  in
  assert_falsy ~expected:false "typing_extensions.Literal[True]";
  assert_falsy ~expected:true "None";
  assert_falsy ~expected:true "typing_extensions.Literal[False]";
  assert_falsy ~expected:true "typing_extensions.Literal[0]";
  assert_falsy ~expected:false "typing_extensions.Literal[42]";
  assert_falsy ~expected:true "typing_extensions.Literal['']";
  assert_falsy ~expected:true "typing_extensions.Literal[b'']";
  assert_falsy ~expected:false "typing_extensions.Literal['hello']";
  assert_falsy ~expected:false "typing_extensions.Literal[b'hello']";
  assert_falsy ~expected:true "typing.Annotated[typing_extensions.Literal[False], 1]";
  assert_falsy ~expected:true "pyre_extensions.ReadOnly[typing_extensions.Literal[False]]";
  assert_falsy
    ~expected:true
    "typing.Union[typing_extensions.Literal[False], typing_extensions.Literal[0]]";
  assert_falsy
    ~expected:false
    "typing.Union[typing_extensions.Literal[True], typing_extensions.Literal[0]]";
  ()


let test_lift_readonly_if_possible _ =
  let assert_lifted ~make_container ~expected element_type =
    element_type
    |> parse_single_expression
    |> Type.create ~variables:(fun _ -> None) ~aliases:Type.resolved_empty_aliases
    |> Type.PyreReadOnly.lift_readonly_if_possible ~make_container
    |> assert_equal ~printer:Type.show ~cmp:Type.equal expected
  in
  assert_lifted
    ~expected:(Type.PyreReadOnly (Type.list (Type.Primitive "Foo")))
    ~make_container:Type.list
    "pyre_extensions.ReadOnly[Foo]";
  assert_lifted ~expected:(Type.list (Type.Primitive "Foo")) ~make_container:Type.list "Foo";
  ()


let () =
  "type"
  >::: [
         "create" >:: test_create;
         "create_callable" >:: test_create_callable;
         "create_alias" >:: test_create_alias;
         "create_type_operator" >:: test_create_type_operator;
         "create_variadic_tuple" >:: test_create_variadic_tuple;
         "create_readonly" >:: test_create_readonly;
         "resolve_aliases" >:: test_resolve_aliases;
         "apply_type_map" >:: test_apply_type_map;
         "expression" >:: test_expression;
         "concise" >:: test_concise;
         "weaken_literals" >:: test_weaken_literals;
         "union" >:: test_union;
         "primitives" >:: test_primitives;
         "elements" >:: test_elements;
         "exists" >:: test_exists;
         "contains_callable" >:: test_contains_callable;
         "contains_any" >:: test_contains_any;
         test_expression_contains_any;
         "is_concrete" >:: test_is_concrete;
         "is_not_instantiated" >:: test_is_not_instantiated;
         "is_meta" >:: test_is_meta;
         "is_none" >:: test_is_none;
         "is_literal_string" >:: test_is_literal_string;
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
         "replace_all" >:: test_replace_all;
         "collect_all" >:: test_collect_all;
         "parse_type_variable_declarations" >:: test_parse_type_variable_declarations;
         "starred_annotation_expression" >:: test_starred_annotation_expression;
         "concatenation_from_unpack_expression" >:: test_concatenation_from_unpack_expression;
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
         "class_attribute_lookups_for_type" >:: test_class_attribute_lookups_for_type;
         "show" >:: test_show;
         "is_truthy" >:: test_is_truthy;
         "is_falsy" >:: test_is_falsy;
         "is_unit_test" >:: test_is_unit_test;
         "from_overloads" >:: test_from_overloads;
         "with_return_annotation" >:: test_with_return_annotation;
         "overload_parameters" >:: test_overload_parameters;
         "parameter_create" >:: test_parameter_create;
         "resolve_alias_before_handling_callable" >:: test_resolve_alias_before_handling_callable;
         "lift_readonly_for_container" >:: test_lift_readonly_if_possible;
       ]
  |> Test.run
