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
open Expression
open Pyre
open Test
module Resolution = Analysis.Resolution

let make_normal_and_escaped_variable ?constraints name =
  Type.Variable.Namespace.reset ();
  let variable = Type.variable ?constraints name in
  let escaped_variable = Type.Variable.mark_all_free_variables_as_escaped variable in
  variable, escaped_variable


let variable_t, escaped_variable_t = make_normal_and_escaped_variable "_T"

let variable_s, escaped_variable_s = make_normal_and_escaped_variable "_S"

let variable_r, escaped_variable_r =
  make_normal_and_escaped_variable ~constraints:(Explicit [Type.integer; Type.float]) "_R"


let compare_instantiated_return_annotation left right =
  let open SignatureSelectionTypes in
  let default = [%compare.equal: instantiated_return_annotation] left right in
  match left, right with
  | ( NotFound { reason = Some left_reason; closest_return_annotation = left_closest },
      NotFound { reason = Some right_reason; closest_return_annotation = right_closest } )
    when Type.equal left_closest right_closest -> (
      let equal_invalid_argument left right =
        Option.compare Expression.location_insensitive_compare left.expression right.expression = 0
        && Type.equal left.annotation right.annotation
      in
      match left_reason, right_reason with
      | InvalidKeywordArgument left, InvalidKeywordArgument right
      | InvalidVariableArgument left, InvalidVariableArgument right ->
          equal_invalid_argument left.value right.value
      | Mismatches left, Mismatches right ->
          let equal_single_mismatch left right =
            match left, right with
            | Mismatch left, Mismatch right -> [%compare.equal: mismatch] left.value right.value
            | _, _ -> [%compare.equal: mismatch_reason] left right
          in
          List.equal equal_single_mismatch left right
      | _ -> default)
  | _ -> default


let test_unresolved_select context =
  let assert_select ?(allow_undefined = false) ?name callable arguments expected =
    let replace_specials name =
      name
      |> String.substr_replace_all ~pattern:"$literal_one" ~with_:"typing_extensions.Literal[1]"
      |> String.substr_replace_all
           ~pattern:"$literal_string"
           ~with_:"typing_extensions.Literal[\"string\"]"
    in
    let parse_callable_and_signature ~callable ~arguments =
      let callable = replace_specials callable in
      let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
        ScratchProject.setup
          ~context
          [
            ( "test.py",
              Format.sprintf
                {|
                  _T = typing.TypeVar('_T')
                  _S = typing.TypeVar('_S')
                  _R = typing.TypeVar('_R', int, float)
                  _T_float_or_str = typing.TypeVar('_U', float, str)
                  _T_float_str_or_union = (
                    typing.TypeVar('_T_float_str_or_union', float, str, typing.Union[float, str])
                  )
                  _T_bound_by_float_str_union = (
                    typing.TypeVar('_T_bound_by_float_str_union', bound=typing.Union[float, str])
                  )

                  class C(): ...
                  class B(C): ...

                  meta: typing.Type[typing.List[int]] = ...
                  union: typing.Union[int, str] = ...
                  int_to_int_dictionary: typing.Dict[int, int] = ...
                  union_list: typing.Union[typing.List[int], typing.List[str]] = ...

                  unknown: Unknown = ...
                  g: typing.Callable[[int], bool]
                  f: typing.Callable[[int], typing.List[bool]]
                  Tparams = pyre_extensions.ParameterSpecification("Tparams")
                  int_string_tuple: typing.Tuple[int, str]
                  unbounded_tuple: typing.Tuple[int, ...]

                  class ExtendsDictStrInt(typing.Dict[str, int]): pass
                  typing.Callable%s
                  call%s
                |}
                callable
                arguments );
          ]
        |> ScratchProject.build_global_environment
      in
      let global_resolution = GlobalResolution.create global_environment in
      let resolution =
        TypeCheck.resolution global_resolution (module TypeCheck.DummyContext)
        |> Resolution.new_local
             ~reference:(Reference.create "optional")
             ~annotation:(Annotation.create_mutable (Type.optional Type.integer))
      in
      let enforce_callable = function
        | Type.Callable ({ Type.Callable.implementation; overloads; _ } as callable) ->
            let undefined { Type.Callable.parameters; _ } =
              match parameters with
              | Type.Callable.Undefined -> true
              | _ -> false
            in
            if List.exists (implementation :: overloads) ~f:undefined && not allow_undefined then
              failwith "Undefined parameters"
            else
              name
              >>| Reference.create
              >>| (fun name -> { callable with kind = Named name })
              |> Option.value ~default:callable
        | _ -> failwith "Could not extract signatures"
      in
      Type.Variable.Namespace.reset ();
      let callable, signature =
        let arguments, expression =
          match
            AstEnvironment.ReadOnly.get_processed_source
              (AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment)
              (Reference.create "test")
            >>| Source.statements
            >>| List.rev
          with
          | Some
              ({ Node.value = Expression { Node.value = Expression.Call { arguments; _ }; _ }; _ }
              :: { Node.value = Expression expression; _ } :: _) ->
              arguments, expression
          | _ -> failwith "couldnt extract"
        in
        let callable =
          expression
          |> GlobalResolution.parse_annotation
               ~validation:NoValidation
               (Resolution.global_resolution resolution)
          |> enforce_callable
        in
        let resolved_arguments =
          let create_argument argument =
            let expression, kind = Ast.Expression.Call.Argument.unpack argument in
            let resolved =
              (Resolution.resolve_expression_to_type_with_locals resolution) ~locals:[] expression
            in
            { AttributeResolution.Argument.expression = Some expression; kind; resolved }
          in
          List.map arguments ~f:create_argument
        in
        ( callable,
          GlobalResolution.signature_select
            ~arguments:resolved_arguments
            ~global_resolution
            ~callable
            ~self_argument:None
            ~resolve_with_locals:(Resolution.resolve_expression_to_type_with_locals resolution) )
      in
      callable, signature
    in
    let callable, signature = parse_callable_and_signature ~callable ~arguments in
    let callable = { callable with Type.Callable.overloads = [] } in
    let implementation_annotation { Type.Callable.implementation = { annotation; _ }; _ } =
      annotation
    in
    let closest_return_annotation = implementation_annotation callable in
    let parse_return return =
      replace_specials return |> parse_single_expression |> Type.create ~aliases:Type.empty_aliases
    in
    let expected =
      let open SignatureSelectionTypes in
      match expected with
      | `Found expected -> Found { selected_return_annotation = parse_return expected }
      | `NotFoundNoReason -> NotFound { closest_return_annotation; reason = None }
      | `NotFoundInvalidKeywordArgument (expression, annotation) ->
          let reason =
            { expression = Some expression; annotation }
            |> Node.create_with_default_location
            |> fun invalid_argument -> Some (InvalidKeywordArgument invalid_argument)
          in
          NotFound { closest_return_annotation; reason }
      | `NotFoundInvalidVariableArgument (expression, annotation) ->
          let reason =
            { expression = Some expression; annotation }
            |> Node.create_with_default_location
            |> fun invalid_argument -> Some (InvalidVariableArgument invalid_argument)
          in
          NotFound { closest_return_annotation; reason }
      | `NotFoundMissingArgument name ->
          NotFound { closest_return_annotation; reason = Some (MissingArgument (Named name)) }
      | `NotFoundMissingAnonymousArgument index ->
          NotFound
            { closest_return_annotation; reason = Some (MissingArgument (PositionalOnly index)) }
      | `NotFoundMissingArgumentWithClosest (closest, name) ->
          NotFound
            {
              closest_return_annotation = parse_return closest;
              reason = Some (MissingArgument (Named name));
            }
      | `NotFoundMissingAnonymousArgumentWithClosest (closest, index) ->
          NotFound
            {
              closest_return_annotation = parse_return closest;
              reason = Some (MissingArgument (PositionalOnly index));
            }
      | `NotFoundTooManyArguments (expected, provided) ->
          NotFound
            { closest_return_annotation; reason = Some (TooManyArguments { expected; provided }) }
      | `NotFoundTooManyArgumentsWithClosest (closest, expected, provided) ->
          NotFound
            {
              closest_return_annotation = parse_return closest;
              reason = Some (TooManyArguments { expected; provided });
            }
      | `NotFoundUnexpectedKeyword name ->
          NotFound
            { closest_return_annotation; reason = Some (UnexpectedKeyword ("$parameter$" ^ name)) }
      | `NotFoundUnexpectedKeywordWithClosest (closest, name) ->
          NotFound
            {
              closest_return_annotation = parse_return closest;
              reason = Some (UnexpectedKeyword ("$parameter$" ^ name));
            }
      | `NotFoundMismatch mismatch_reasons ->
          let reason =
            List.map mismatch_reasons ~f:(fun (actual, expected, name, position) ->
                { actual; expected; name; position }
                |> Node.create_with_default_location
                |> fun mismatch -> Mismatch mismatch)
            |> fun mismatches -> Some (Mismatches mismatches)
          in
          NotFound { closest_return_annotation; reason }
      | `NotFoundMismatchWithClosest (closest, actual, expected, name, position) ->
          let reason =
            { actual; expected; name; position }
            |> Node.create_with_default_location
            |> fun mismatch -> Some (Mismatches [Mismatch mismatch])
          in
          NotFound { closest_return_annotation = parse_return closest; reason }
      | `NotFound (closest, reason) ->
          NotFound { closest_return_annotation = parse_return closest; reason }
    in
    assert_equal
      ~printer:SignatureSelectionTypes.show_instantiated_return_annotation
      ~cmp:compare_instantiated_return_annotation
      expected
      signature
  in
  let assert_select_direct ~arguments ~callable expected =
    Type.Variable.Namespace.reset ();
    let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
    let global_resolution = Resolution.global_resolution resolution in
    let signature =
      let resolved_arguments =
        let create_argument argument =
          let expression, kind = Ast.Expression.Call.Argument.unpack argument in
          let resolved =
            (Resolution.resolve_expression_to_type_with_locals resolution) ~locals:[] expression
          in
          { AttributeResolution.Argument.expression = Some expression; kind; resolved }
        in
        List.map arguments ~f:create_argument
      in
      GlobalResolution.signature_select
        ~arguments:resolved_arguments
        ~global_resolution
        ~callable
        ~self_argument:None
        ~resolve_with_locals:(Resolution.resolve_expression_to_type_with_locals resolution)
    in
    let printer x =
      SignatureSelectionTypes.sexp_of_instantiated_return_annotation x |> Sexp.to_string_hum
    in
    assert_equal ~cmp:compare_instantiated_return_annotation ~printer expected signature
  in
  (* Undefined callables always match. *)
  assert_select ~allow_undefined:true "[..., int]" "()" (`Found "int");
  assert_select ~allow_undefined:true "[..., int]" "(a, b)" (`Found "int");
  assert_select
    ~allow_undefined:true
    "[..., int]"
    "(a, b='depr', *variable, **keywords)"
    (`Found "int");
  assert_select ~allow_undefined:true "[..., unknown][[..., int][[int], int]]" "(1)" (`Found "int");

  (* Traverse anonymous arguments. *)
  assert_select "[[], int]" "()" (`Found "int");
  assert_select "[[int], int]" "()" (`NotFoundMissingAnonymousArgument 0);
  assert_select "[[], int]" "(1)" (`NotFoundTooManyArguments (0, 1));
  assert_select "[[int], int]" "(1)" (`Found "int");
  assert_select "[[Named(i, int)], int]" "(1)" (`Found "int");
  assert_select "[[typing.Any], int]" "(unknown)" (`Found "int");
  assert_select
    "[[int], int]"
    "('string')"
    (`NotFoundMismatch [Type.literal_string "string", Type.integer, None, 1]);
  assert_select "[[int], int]" "(name='string')" (`NotFoundUnexpectedKeyword "name");
  assert_select "[[int], int]" "(*[1])" (`Found "int");
  assert_select "[[str], int]" "(*[1])" (`NotFoundMismatch [Type.integer, Type.string, None, 1]);
  assert_select "[[int, str], int]" "(*[1], 'asdf')" (`NotFoundTooManyArguments (2, 3));
  assert_select "[[object], None]" "(union)" (`Found "None");
  assert_select
    "[[int], None]"
    "(union)"
    (`NotFoundMismatch [Type.union [Type.integer; Type.string], Type.integer, None, 1]);
  assert_select "[[int, Named(i, int)], int]" "(1, 2, i=3)" (`NotFoundTooManyArguments (1, 2));

  (* Traverse variable arguments. *)
  assert_select "[[Variable()], int]" "()" (`Found "int");
  assert_select "[[Variable()], int]" "(1, 2)" (`Found "int");
  assert_select "[[Variable(int)], int]" "(1, 2)" (`Found "int");
  assert_select
    "[[Variable(str)], int]"
    "(1, 2)"
    (`NotFoundMismatch
      [Type.literal_integer 1, Type.string, None, 1; Type.literal_integer 2, Type.string, None, 2]);
  assert_select
    "[[Variable(str)], int]"
    "('string', 2)"
    (`NotFoundMismatch [Type.literal_integer 2, Type.string, None, 2]);
  assert_select "[[Variable(int)], int]" "(*[1, 2], 3)" (`Found "int");
  assert_select "[[Variable(int), Named(a, str)], int]" "(*[1, 2], a='string')" (`Found "int");
  assert_select
    "[[Variable(int), Named(a, str)], int]"
    "(*[1, 2], *[3, 4], a='string')"
    (`Found "int");
  assert_select
    "[[Variable(int)], int]"
    "(*['string'])"
    (`NotFoundMismatch [Type.string, Type.integer, None, 1]);

  (* KeywordOnly *)
  assert_select "[[KeywordOnly(i, int)], int]" "(i=1)" (`Found "int");
  assert_select "[[KeywordOnly(i, int)], int]" "(2, i=1)" (`NotFoundTooManyArguments (0, 1));
  assert_select "[[KeywordOnly(i, int)], int]" "(**{'A': 7})" (`Found "int");
  assert_select
    "[[Named(x, str), KeywordOnly(i, bool, default)], int]"
    "(*['a', 'b'])"
    (`Found "int");

  (* Named arguments. *)
  assert_select "[[Named(i, int), Named(j, int)], int]" "(i=1, j=2)" (`Found "int");
  assert_select "[[Named(i, int), Named(j, int, default)], int]" "(i=1)" (`Found "int");
  assert_select "[[Named(i, int), Named(j, int)], int]" "(j=1, i=2)" (`Found "int");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, q=2)"
    (`NotFoundUnexpectedKeyword "q");
  assert_select "[[], int]" "(j=1, q=2)" (`NotFoundUnexpectedKeyword "j");

  (* May want new class of error for `keyword argument repeated` *)
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, j=2, q=3)"
    (`NotFoundUnexpectedKeyword "j");
  assert_select
    "[[Named(i, int), Named(j, int), Named(k, int)], int]"
    "(j=1, a=2, b=3)"
    (`NotFoundUnexpectedKeyword "a");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, a=2, b=3)"
    (`NotFoundUnexpectedKeyword "a");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(i='string', a=2, b=3)"
    (`NotFoundUnexpectedKeyword "a");
  assert_select
    "[[Named(i, int), Named(j, str)], int]"
    "(i=1, j=2)"
    (`NotFoundMismatch [Type.literal_integer 2, Type.string, Some "$parameter$j", 2]);
  assert_select "[[Named(i, int), Named(j, int)], int]" "(**{'j': 1, 'i': 2})" (`Found "int");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "( **{'j': 'string', 'i': 'string'})"
    (`NotFoundMismatch [Type.string, Type.integer, None, 1; Type.string, Type.integer, None, 1]);

  (* Test iterable and mapping expansions. *)
  assert_select "[[int], int]" "(*[1])" (`Found "int");
  assert_select
    "[[int], int]"
    "(*a)"
    (`NotFoundInvalidVariableArgument (+Expression.Name (Name.Identifier "a"), Type.Top));
  assert_select
    "[[int], int]"
    "(**a)"
    (`NotFoundInvalidKeywordArgument (+Expression.Name (Name.Identifier "a"), Type.Top));
  assert_select
    "[[int], int]"
    "(**int_to_int_dictionary)"
    (`NotFoundInvalidKeywordArgument
      ( +Expression.Name (Name.Identifier "$local_test$int_to_int_dictionary"),
        Type.dictionary ~key:Type.integer ~value:Type.integer ));
  assert_select "[[int, Named(i, int)], int]" "(1, **{'a': 1})" (`Found "int");
  assert_select "[[Named(i, int), Named(j, int)], int]" "(**{'i': 1}, j=2)" (`Found "int");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**(ExtendsDictStrInt()), j=2)"
    (`Found "int");
  assert_select
    "[[Named(i, str)], int]"
    "(**(ExtendsDictStrInt()))"
    (`NotFoundMismatch [Type.integer, Type.string, None, 1]);
  assert_select "[[Named(i, int), Named(j, int)], int]" "(**({}), j=2)" (`Found "int");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**({} if optional is None else {'i': optional}), j=2)"
    (`Found "int");

  (* Constructor resolution. *)
  assert_select "[[typing.Callable[[typing.Any], int]], int]" "(int)" (`Found "int");
  assert_select
    "[[typing.Callable[[typing.Any], int]], int]"
    "(str)"
    (`NotFoundMismatch
      [
        ( Type.meta Type.string,
          Type.Callable.create
            ~parameters:
              (Type.Callable.Defined
                 [
                   Type.Callable.Parameter.PositionalOnly
                     { index = 0; annotation = Type.Any; default = false };
                 ])
            ~annotation:Type.integer
            (),
          None,
          1 );
      ]);

  (* Keywords. *)
  assert_select "[[Keywords()], int]" "()" (`Found "int");
  assert_select "[[Keywords()], int]" "(a=1, b=2)" (`Found "int");
  assert_select "[[Keywords(int)], int]" "(a=1, b=2)" (`Found "int");
  assert_select
    "[[Named(a, int), Named(c, int), Keywords(int)], int]"
    "(a=1, b=2, c=3)"
    (`Found "int");
  assert_select
    "[[Keywords(str)], int]"
    "(a=1, b=2)"
    (`NotFoundMismatch
      [
        Type.literal_integer 1, Type.string, Some "$parameter$a", 1;
        Type.literal_integer 2, Type.string, Some "$parameter$b", 2;
      ]);
  assert_select
    "[[Keywords(str)], int]"
    "(a='string', b=2)"
    (`NotFoundMismatch [Type.literal_integer 2, Type.string, Some "$parameter$b", 2]);

  (* Constraint resolution. *)
  assert_select "[[_T], _T]" "(1)" (`Found "$literal_one");
  assert_select "[[typing.Callable[[], _T]], _T]" "(lambda: 1)" (`Found "int");
  assert_select "[[_T, _S], _T]" "(1, 'string')" (`Found "$literal_one");
  assert_select "[[_T, _T], int]" "(1, 'string')" (`Found "int");
  assert_select "[[_T], typing.Union[str, _T]]" "(1)" (`Found "typing.Union[str, $literal_one]");
  assert_select "[[typing.Union[int, typing.List[_T]]], _T]" "([1])" (`Found "int");

  assert_select_direct
    ~arguments:[{ name = None; value = parse_single_expression "1" }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = variable_s;
            parameters =
              Defined
                [PositionalOnly { index = 0; annotation = Type.variable "_T"; default = false }];
          };
        overloads = [];
      }
    (Found { selected_return_annotation = escaped_variable_s });

  assert_select "[[typing.List[_T]], int]" "([1])" (`Found "int");
  assert_select "[[typing.Sequence[_T]], int]" "([1])" (`Found "int");
  assert_select "[[typing.List[C]], int]" "([B()])" (`Found "int");
  assert_select "[[typing.List[C]], int]" "([B() for x in range(3)])" (`Found "int");
  assert_select "[[typing.Set[C]], int]" "({ B(), B() })" (`Found "int");
  assert_select "[[typing.Set[C]], int]" "({ B() for x in range(3) })" (`Found "int");
  assert_select "[[typing.Dict[int, C]], int]" "({ 7: B() })" (`Found "int");
  assert_select "[[typing.Dict[int, C]], int]" "({n: B() for n in range(5)})" (`Found "int");
  assert_select
    "[[typing.Iterable[typing.Tuple[_T, _S]]], typing.Dict[_T, _S]]"
    "([('a', 1), ('b', 2)])"
    (`Found "typing.Dict[str, int]");
  assert_select
    "[[typing.Sequence[_T]], int]"
    "(1)"
    (`NotFoundMismatchWithClosest
      ( "int",
        Type.literal_integer 1,
        Type.parametric "typing.Sequence" [Single (Type.variable "test._T")],
        None,
        1 ));
  assert_select "[[_R], _R]" "(1)" (`Found "int");
  assert_select_direct
    ~arguments:[{ name = None; value = parse_single_expression "'string'" }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = variable_r;
            parameters =
              Defined [PositionalOnly { index = 0; annotation = variable_r; default = false }];
          };
        overloads = [];
      }
    (NotFound
       {
         closest_return_annotation = escaped_variable_r;
         reason =
           Some
             (Mismatches
                [
                  Mismatch
                    (Node.create_with_default_location
                       {
                         SignatureSelectionTypes.actual = Type.literal_string "string";
                         expected = variable_r;
                         name = None;
                         position = 1;
                       });
                ]);
       });
  assert_select "[[typing.List[_R]], _R]" "([1])" (`Found "int");

  assert_select_direct
    ~arguments:[{ name = None; value = parse_single_expression "['string']" }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = variable_r;
            parameters =
              Defined
                [PositionalOnly { index = 0; annotation = Type.list variable_r; default = false }];
          };
        overloads = [];
      }
    (NotFound
       {
         closest_return_annotation = escaped_variable_r;
         reason =
           Some
             (Mismatches
                [
                  Mismatch
                    (Node.create_with_default_location
                       {
                         SignatureSelectionTypes.actual = Type.list Type.string;
                         expected = Type.list variable_r;
                         name = None;
                         position = 1;
                       });
                ]);
       });
  assert_select_direct
    ~arguments:[]
    ~callable:
      {
        kind = Anonymous;
        implementation = { annotation = variable_r; parameters = Defined [] };
        overloads = [];
      }
    (Found { selected_return_annotation = escaped_variable_r });

  assert_select "[[typing.Type[_T]], _T]" "(int)" (`Found "int");
  assert_select "[[typing.Type[typing.List[_T]]], _T]" "(meta)" (`Found "int");
  assert_select "[[typing.Type[_T]], _T]" "(typing.List[str])" (`Found "typing.List[str]");
  assert_select "[[Variable(_T)], int]" "(1, 2)" (`Found "int");
  assert_select "[[Keywords(_T)], int]" "(a=1, b=2)" (`Found "int");
  assert_select
    "[[_T_float_or_str], None]"
    "(union)"
    (`NotFoundMismatchWithClosest
      ( "None",
        Type.union [Type.integer; Type.string],
        Type.variable
          "test._T_float_or_str"
          ~constraints:(Type.Variable.Explicit [Type.float; Type.string]),
        None,
        1 ));
  assert_select
    "[[_T_float_str_or_union], _T_float_str_or_union]"
    "(union)"
    (`Found "typing.Union[float, str]");
  assert_select
    "[[_T_bound_by_float_str_union], _T_bound_by_float_str_union]"
    "(union)"
    (`Found "typing.Union[int, str]");
  (* T44393553 : typing.Union[typing.List[int], typing.List[str]] desired. *)
  assert_select
    "[[typing.Iterable[_T]], typing.List[_T]]"
    "(union_list)"
    (`Found "typing.List[typing.Union[int, str]]");

  let assert_marks_as_escaped_with_argument ~constraints =
    let variable, escaped_variable = make_normal_and_escaped_variable ~constraints "_S" in
    assert_select_direct
      ~arguments:[{ name = None; value = parse_single_expression "5" }]
      ~callable:
        {
          kind = Anonymous;
          implementation =
            {
              annotation = variable;
              parameters =
                Defined [PositionalOnly { index = 0; annotation = Type.integer; default = false }];
            };
          overloads = [];
        }
      (Found { selected_return_annotation = escaped_variable })
  in
  assert_marks_as_escaped_with_argument ~constraints:Unconstrained;
  assert_marks_as_escaped_with_argument ~constraints:(Explicit [Type.float; Type.string]);
  assert_marks_as_escaped_with_argument ~constraints:(Bound (Union [Type.float; Type.string]));

  let assert_marks_as_escaped_with_no_arguments ~constraints =
    let variable, escaped_variable = make_normal_and_escaped_variable ~constraints "_S" in
    assert_select_direct
      ~arguments:[]
      ~callable:
        {
          kind = Anonymous;
          implementation = { annotation = variable; parameters = Defined [] };
          overloads = [];
        }
      (Found { selected_return_annotation = escaped_variable })
  in
  assert_marks_as_escaped_with_no_arguments ~constraints:Unconstrained;
  assert_marks_as_escaped_with_no_arguments ~constraints:(Explicit [Type.float; Type.string]);
  assert_marks_as_escaped_with_no_arguments ~constraints:(Bound (Union [Type.float; Type.string]));

  (* Ranking. *)
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0)"
    (* Ambiguous, pick the first one. *)
    (`NotFoundMissingAnonymousArgumentWithClosest ("int", 1));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[str], str][[int, str], int]]"
    "(1)"
    (* Ambiguous, prefer the one with the closer arity over the type match. *)
    (`NotFoundMismatchWithClosest ("str", Type.literal_integer 1, Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[int, Keywords()], int][[int, str], int]]"
    "(1, 1)" (* Prefer anonymous unmatched parameters over keywords. *)
    (`NotFoundMismatchWithClosest ("int", Type.literal_integer 1, Type.string, None, 2));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[str], str][[], str]]"
    "(1)"
    (`NotFoundMismatchWithClosest ("str", Type.literal_integer 1, Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[str, Keywords()], int][[Keywords()], int]]"
    "(1)" (* Prefer arity matches. *)
    (`NotFoundMismatchWithClosest ("int", Type.literal_integer 1, Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0, 'string')"
    (* Clear winner. *)
    (`NotFoundMissingAnonymousArgumentWithClosest ("int", 2));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[int, str, str, str], int][[int, str, bool], int]]"
    "(0, 'string')"
    (`NotFoundMissingAnonymousArgumentWithClosest ("int", 2));

  (* Match not found in overloads: intentionally do not consider the implementation. *)
  assert_select
    "[[typing.Union[str, int]], typing.Union[str, int]][[[str], str][[int], int]]"
    "(unknown)"
    (`NotFoundMismatchWithClosest ("str", Type.Top, Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[Named(a, int), Named(b, int)], int][[Named(c, int), Named(d, int)], int]]"
    "(i=1, d=2)"
    (`NotFoundUnexpectedKeywordWithClosest ("int", "i"));

  (* Prefer the overload where the mismatch comes latest *)
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[int, str], int][[str, int], str]]"
    "(1, 1)"
    (`NotFoundMismatchWithClosest ("int", Type.literal_integer 1, Type.string, None, 2));
  assert_select
    ~allow_undefined:true
    "[..., Unknown][[[str, int], str][[int, str], int]]"
    "(1, 1)"
    (`NotFoundMismatchWithClosest ("int", Type.literal_integer 1, Type.string, None, 2));

  (* Void functions. *)
  assert_select ~allow_undefined:true "[..., None]" "()" (`Found "None");
  assert_select "[[int], None]" "(1)" (`Found "None");
  assert_select
    "[[int], None]"
    "('string')"
    (`NotFoundMismatch [Type.literal_string "string", Type.integer, None, 1]);
  assert_select "[[typing.Callable[[_T], bool]], _T]" "(g)" (`Found "int");
  assert_select "[[typing.Callable[[_T], typing.List[bool]]], _T]" "(f)" (`Found "int");

  (* Special dictionary constructor *)
  assert_select
    ~name:"dict.__init__"
    "[[Keywords(_S)], dict[_T, _S]]"
    "(a=1)"
    (`Found "dict[str, $literal_one]");

  (* TODO(T41074174): Error here rather than defaulting back to the initial signature *)
  assert_select
    ~name:"dict.__init__"
    "[[Named(map, typing.Mapping[_T, _S]), Keywords(_S)], dict[_T, _S]]"
    "({1: 1}, a=1)"
    (`Found "dict[int, int]");

  assert_select_direct
    ~arguments:[]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.dictionary ~key:variable_t ~value:variable_s;
            parameters = Defined [Keywords variable_s];
          };
        overloads = [];
      }
    (Found
       {
         selected_return_annotation =
           Type.dictionary ~key:escaped_variable_t ~value:escaped_variable_s;
       });

  assert_select_direct
    ~arguments:
      [{ name = Some (Node.create_with_default_location "a"); value = parse_single_expression "1" }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.dictionary ~key:variable_t ~value:variable_s;
            parameters = Defined [Keywords variable_s];
          };
        overloads = [];
      }
    (Found
       {
         selected_return_annotation =
           Type.dictionary ~key:escaped_variable_t ~value:(Type.literal_integer 1);
       });

  assert_select_direct
    ~arguments:
      [{ name = Some (Node.create_with_default_location "a"); value = parse_single_expression "1" }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.integer;
            parameters =
              ParameterVariadicTypeVariable
                {
                  head = [];
                  variable =
                    Type.Variable.Variadic.Parameters.create "TParams"
                    |> Type.Variable.Variadic.Parameters.mark_as_bound;
                };
          };
        overloads = [];
      }
    (NotFound
       {
         closest_return_annotation = Type.integer;
         reason = Some SignatureSelectionTypes.CallingParameterVariadicTypeVariable;
       });
  ()


let test_resolved_select context =
  let assert_select ~arguments ~callable expected =
    Type.Variable.Namespace.reset ();
    let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
    let global_resolution = Resolution.global_resolution resolution in
    let signature =
      GlobalResolution.signature_select
        ~arguments
        ~global_resolution
        ~callable
        ~self_argument:None
        ~resolve_with_locals:(Resolution.resolve_expression_to_type_with_locals resolution)
    in
    let printer x =
      SignatureSelectionTypes.sexp_of_instantiated_return_annotation x |> Sexp.to_string_hum
    in
    assert_equal ~cmp:compare_instantiated_return_annotation ~printer expected signature
  in
  assert_select
    ~arguments:[{ expression = None; kind = Positional; resolved = Type.string }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.integer;
            parameters =
              Defined [PositionalOnly { index = 0; annotation = Type.string; default = false }];
          };
        overloads = [];
      }
    (Found { selected_return_annotation = Type.integer });
  assert_select
    ~arguments:[{ expression = None; kind = Positional; resolved = Type.integer }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.integer;
            parameters =
              Defined [PositionalOnly { index = 0; annotation = Type.string; default = false }];
          };
        overloads = [];
      }
    (NotFound
       {
         closest_return_annotation = Type.integer;
         reason =
           Some
             (Mismatches
                [
                  Mismatch
                    {
                      location = Location.any;
                      value =
                        { actual = Type.integer; expected = Type.string; name = None; position = 1 };
                    };
                ]);
       });
  assert_select
    ~arguments:[{ expression = None; kind = Positional; resolved = Type.literal_integer 1 }]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation = Type.string;
            parameters =
              Defined [PositionalOnly { index = 0; annotation = Type.integer; default = false }];
          };
        overloads =
          [
            {
              annotation = Type.literal_string "zero";
              parameters =
                Defined
                  [
                    PositionalOnly
                      { index = 0; annotation = Type.literal_integer 0; default = false };
                  ];
            };
            {
              annotation = Type.literal_string "one";
              parameters =
                Defined
                  [
                    PositionalOnly
                      { index = 0; annotation = Type.literal_integer 1; default = false };
                  ];
            };
          ];
      }
    (Found { selected_return_annotation = Type.literal_string "one" });
  Type.Variable.Namespace.reset ();
  let namespace = Type.Variable.Namespace.create_fresh () in
  Type.Variable.Namespace.reset ();
  assert_select
    ~arguments:
      [
        {
          expression = None;
          kind = Positional;
          resolved =
            Type.Callable.create
              ~annotation:(Type.variable "S")
              ~parameters:
                (Defined [Named { name = "X"; annotation = Type.variable "S"; default = false }])
              ();
        };
      ]
    ~callable:
      {
        kind = Anonymous;
        implementation =
          {
            annotation =
              Type.parametric "P" [Single (Type.variable "T"); Single (Type.variable "X")];
            parameters =
              Defined
                [
                  PositionalOnly { index = 0; annotation = Type.variable "T"; default = false };
                  PositionalOnly { index = 0; annotation = Type.variable "X"; default = true };
                ];
          };
        overloads = [];
      }
    (Found
       {
         selected_return_annotation =
           Type.parametric
             "P"
             [
               Single
                 (Type.Callable.create
                    ~annotation:(Type.variable "S")
                    ~parameters:
                      (Defined
                         [Named { name = "X"; annotation = Type.variable "S"; default = false }])
                    ());
               (* Only "local" variables should be marked as escaped *)
               Single
                 (Variable
                    (Type.Variable.Unary.create "X"
                    |> Type.Variable.Unary.mark_as_escaped
                    |> Type.Variable.Unary.namespace ~namespace));
             ];
       });
  ()


let () =
  "signature"
  >::: [
         "unresolved" >: test_case ~length:Long test_unresolved_select;
         "resolved" >:: test_resolved_select;
       ]
  |> Test.run
