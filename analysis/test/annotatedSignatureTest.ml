(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Pyre
open Test
open AnnotatedTest
module Resolution = Analysis.Resolution
module Signature = Annotated.Signature
open Signature

let resolution =
  populate
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

      unknown: $unknown = ...
      g: typing.Callable[[int], bool]
      f: typing.Callable[[int], typing.List[bool]]
      Tparams = pyre_extensions.ParameterSpecification("Tparams")
      Ts = pyre_extensions.ListVariadic("Ts")
      int_string_tuple: typing.Tuple[int, str]
      unbounded_tuple: typing.Tuple[int, ...]

      class ExtendsDictStrInt(typing.Dict[str, int]): pass
      optional: typing.Optional[int]
    |}
  |> fun environment -> TypeCheck.resolution (Environment.resolution environment ()) ()


let parse_annotation annotation =
  (* Allow untracked to create callables with unknowns, which would otherwise be generated from
     Callable.create on defines. *)
  annotation
  |> parse_single_expression
  |> GlobalResolution.parse_annotation
       ~allow_untracked:true
       ~allow_invalid_type_parameters:true
       (Resolution.global_resolution resolution)


let test_select _ =
  let assert_select ?(allow_undefined = false) ?name callable arguments expected =
    let parse_callable callable =
      callable
      |> String.substr_replace_all ~pattern:"$literal_one" ~with_:"typing_extensions.Literal[1]"
      |> String.substr_replace_all
           ~pattern:"$literal_string"
           ~with_:"typing_extensions.Literal[\"string\"]"
      |> Format.asprintf "typing.Callable%s"
      |> parse_annotation
      |> function
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
    let callable = parse_callable callable in
    Type.Variable.Namespace.reset ();
    let signature =
      let { Call.arguments; _ } = parse_single_call (Format.asprintf "call%s" arguments) in
      Signature.select ~arguments ~resolution ~callable
    in
    let callable = { callable with Type.Callable.overloads = [] } in
    let parse_callable callable =
      Type.Variable.Namespace.reset ();
      parse_callable callable
      |> Type.Callable.map ~f:Type.Variable.mark_all_free_variables_as_escaped
      |> fun callable -> Option.value_exn callable
    in
    let expected =
      match expected with
      | `Found expected -> Found (parse_callable expected)
      | `NotFoundNoReason -> NotFound { callable; reason = None }
      | `NotFoundInvalidKeywordArgument (expression, annotation) ->
          let reason =
            { expression; annotation }
            |> Node.create_with_default_location
            |> fun invalid_argument -> Some (InvalidKeywordArgument invalid_argument)
          in
          NotFound { callable; reason }
      | `NotFoundInvalidVariableArgument (expression, annotation) ->
          let reason =
            { expression; annotation }
            |> Node.create_with_default_location
            |> fun invalid_argument -> Some (InvalidVariableArgument invalid_argument)
          in
          NotFound { callable; reason }
      | `NotFoundMissingArgument name ->
          NotFound { callable; reason = Some (MissingArgument (Named name)) }
      | `NotFoundMissingAnonymousArgument index ->
          NotFound { callable; reason = Some (MissingArgument (Anonymous index)) }
      | `NotFoundMissingArgumentWithClosest (closest, name) ->
          NotFound
            { callable = parse_callable closest; reason = Some (MissingArgument (Named name)) }
      | `NotFoundMissingAnonymousArgumentWithClosest (closest, index) ->
          NotFound
            {
              callable = parse_callable closest;
              reason = Some (MissingArgument (Anonymous index));
            }
      | `NotFoundTooManyArguments (expected, provided) ->
          NotFound { callable; reason = Some (TooManyArguments { expected; provided }) }
      | `NotFoundTooManyArgumentsWithClosest (closest, expected, provided) ->
          NotFound
            {
              callable = parse_callable closest;
              reason = Some (TooManyArguments { expected; provided });
            }
      | `NotFoundUnexpectedKeyword name ->
          NotFound { callable; reason = Some (UnexpectedKeyword name) }
      | `NotFoundUnexpectedKeywordWithClosest (closest, name) ->
          NotFound { callable = parse_callable closest; reason = Some (UnexpectedKeyword name) }
      | `NotFoundMismatch (actual, actual_expression, expected, name, position) ->
          let actual_expression = parse_single_expression actual_expression in
          let reason =
            { actual; actual_expression; expected; name; position }
            |> Node.create_with_default_location
            |> fun mismatch -> Some (Mismatch mismatch)
          in
          NotFound { callable; reason }
      | `NotFoundMismatchWithClosest (closest, actual, actual_expression, expected, name, position)
        ->
          let actual_expression = parse_single_expression actual_expression in
          let reason =
            { actual; actual_expression; expected; name; position }
            |> Node.create_with_default_location
            |> fun mismatch -> Some (Mismatch mismatch)
          in
          NotFound { callable = parse_callable closest; reason }
      | `NotFound (closest, reason) -> NotFound { callable = parse_callable closest; reason }
    in
    assert_equal ~printer:Signature.show ~cmp:Signature.equal expected signature
  in
  (* Undefined callables always match. *)
  assert_select ~allow_undefined:true "[..., int]" "()" (`Found "[..., int]");
  assert_select ~allow_undefined:true "[..., int]" "(a, b)" (`Found "[..., int]");
  assert_select
    ~allow_undefined:true
    "[..., int]"
    "(a, b='depr', *variable, **keywords)"
    (`Found "[..., int]");
  assert_select
    ~allow_undefined:true
    "[..., unknown][[..., int][[int], int]]"
    "(1)"
    (`Found "[..., int]");

  (* Traverse anonymous arguments. *)
  assert_select "[[], int]" "()" (`Found "[[], int]");
  assert_select "[[int], int]" "()" (`NotFoundMissingAnonymousArgument 0);
  assert_select "[[], int]" "(1)" (`NotFoundTooManyArguments (0, 1));
  assert_select "[[int], int]" "(1)" (`Found "[[int], int]");
  assert_select "[[Named(i, int)], int]" "(1)" (`Found "[[Named(i, int)], int]");
  assert_select "[[typing.Any], int]" "(unknown)" (`Found "[[typing.Any], int]");
  assert_select
    "[[int], int]"
    "('string')"
    (`NotFoundMismatch (Type.literal_string "string", "\"string\"", Type.integer, None, 1));
  assert_select "[[int], int]" "(name='string')" (`NotFoundUnexpectedKeyword "name");
  assert_select "[[int], int]" "(*[1])" (`Found "[[int], int]");
  assert_select
    "[[str], int]"
    "(*[1])"
    (`NotFoundMismatch (Type.integer, "*[1]", Type.string, None, 1));
  assert_select "[[int, str], int]" "(*[1], 'asdf')" (`NotFoundTooManyArguments (2, 3));
  assert_select "[[object], None]" "(union)" (`Found "[[object], None]");
  assert_select
    "[[int], None]"
    "(union)"
    (`NotFoundMismatch (Type.union [Type.integer; Type.string], "union", Type.integer, None, 1));
  assert_select "[[int, Named(i, int)], int]" "(1, 2, i=3)" (`NotFoundTooManyArguments (1, 2));

  (* Traverse variable arguments. *)
  assert_select "[[Variable()], int]" "()" (`Found "[[Variable()], int]");
  assert_select "[[Variable()], int]" "(1, 2)" (`Found "[[Variable()], int]");
  assert_select "[[Variable(int)], int]" "(1, 2)" (`Found "[[Variable(int)], int]");
  assert_select
    "[[Variable(str)], int]"
    "(1, 2)"
    (`NotFoundMismatch (Type.literal_integer 1, "1", Type.string, None, 1));
  assert_select
    "[[Variable(str)], int]"
    "('string', 2)"
    (`NotFoundMismatch (Type.literal_integer 2, "2", Type.string, None, 2));
  assert_select "[[Variable(int)], int]" "(*[1, 2], 3)" (`Found "[[Variable(int)], int]");
  assert_select
    "[[Variable(int), Named(a, str)], int]"
    "(*[1, 2], a='string')"
    (`Found "[[Variable(int), Named(a, str)], int]");
  assert_select
    "[[Variable(int), Named(a, str)], int]"
    "(*[1, 2], *[3, 4], a='string')"
    (`Found "[[Variable(int), Named(a, str)], int]");
  assert_select
    "[[Variable(int)], int]"
    "(*['string'])"
    (`NotFoundMismatch (Type.string, "*[\"string\"]", Type.integer, None, 1));

  (* KeywordOnly *)
  assert_select "[[KeywordOnly(i, int)], int]" "(i=1)" (`Found "[[KeywordOnly(i, int)], int]");
  assert_select "[[KeywordOnly(i, int)], int]" "(2, i=1)" (`NotFoundTooManyArguments (0, 1));
  assert_select
    "[[KeywordOnly(i, int)], int]"
    "(**{'A': 7})"
    (`Found "[[KeywordOnly(i, int)], int]");
  assert_select
    "[[Named(x, str), KeywordOnly(i, bool, default)], int]"
    "(*['a', 'b'])"
    (`Found "[[Named(x, str), KeywordOnly(i, bool, default)], int]");

  (* Named arguments. *)
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(i=1, j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int, default)], int]"
    "(i=1)"
    (`Found "[[Named(i, int), Named(j, int, default)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, i=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
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
    (`NotFoundMismatch (Type.literal_integer 2, "2", Type.string, Some "j", 2));
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'j': 1, 'i': 2})"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'j': 'string', 'i': 'string'})"
    (`NotFoundMismatch (Type.string, "**{'j': 'string', 'i': 'string'}", Type.integer, None, 1));

  (* Test iterable and mapping expansions. *)
  assert_select "[[int], int]" "(*[1])" (`Found "[[int], int]");
  assert_select
    "[[int], int]"
    "(*a)"
    (`NotFoundInvalidVariableArgument (+Name (Name.Identifier "a"), Type.Top));
  assert_select
    "[[int], int]"
    "(**a)"
    (`NotFoundInvalidKeywordArgument (+Name (Name.Identifier "a"), Type.Top));
  assert_select
    "[[int], int]"
    "(**int_to_int_dictionary)"
    (`NotFoundInvalidKeywordArgument
      ( +Name (Name.Identifier "int_to_int_dictionary"),
        Type.dictionary ~key:Type.integer ~value:Type.integer ));
  assert_select
    "[[int, Named(i, int)], int]"
    "(1, **{'a': 1})"
    (`Found "[[int, Named(i, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'i': 1}, j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**(ExtendsDictStrInt()), j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, str)], int]"
    "(**(ExtendsDictStrInt()))"
    (`NotFoundMismatch (Type.integer, "**ExtendsDictStrInt()", Type.string, None, 1));
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**({}), j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**({} if optional is None else {'i': optional}), j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");

  (* Constructor resolution. *)
  assert_select
    "[[typing.Callable[[typing.Any], int]], int]"
    "(int)"
    (`Found "[[typing.Callable[[typing.Any], int]], int]");
  assert_select
    "[[typing.Callable[[typing.Any], int]], int]"
    "(str)"
    (`NotFoundMismatch
      ( Type.meta Type.string,
        "str",
        Type.Callable.create
          ~parameters:
            (Type.Callable.Defined
               [ Type.Callable.Parameter.Anonymous
                   { index = 0; annotation = Type.Any; default = false } ])
          ~annotation:Type.integer
          (),
        None,
        1 ));

  (* Keywords. *)
  assert_select "[[Keywords()], int]" "()" (`Found "[[Keywords()], int]");
  assert_select "[[Keywords()], int]" "(a=1, b=2)" (`Found "[[Keywords()], int]");
  assert_select "[[Keywords(int)], int]" "(a=1, b=2)" (`Found "[[Keywords(int)], int]");
  assert_select
    "[[Named(a, int), Named(c, int), Keywords(int)], int]"
    "(a=1, b=2, c=3)"
    (`Found "[[Named(a, int), Named(c, int), Keywords(int)], int]");
  assert_select
    "[[Keywords(str)], int]"
    "(a=1, b=2)"
    (`NotFoundMismatch (Type.literal_integer 1, "1", Type.string, Some "a", 1));
  assert_select
    "[[Keywords(str)], int]"
    "(a='string', b=2)"
    (`NotFoundMismatch (Type.literal_integer 2, "2", Type.string, Some "b", 2));

  (* Constraint resolution. *)
  assert_select "[[_T], _T]" "(1)" (`Found "[[$literal_one], $literal_one]");
  assert_select
    "[[typing.Callable[[], _T]], _T]"
    "(lambda: 1)"
    (`Found "[[typing.Callable[[], int]], int]");
  assert_select
    "[[_T, _S], _T]"
    "(1, 'string')"
    (`Found "[[$literal_one, $literal_string], $literal_one]");
  assert_select
    "[[_T, _T], int]"
    "(1, 'string')"
    (`Found "[[typing.Union[int, str], typing.Union[int, str]], int]");
  assert_select
    "[[_T], typing.Union[str, _T]]"
    "(1)"
    (`Found "[[$literal_one], typing.Union[str, $literal_one]]");
  assert_select
    "[[typing.Union[int, typing.List[_T]]], _T]"
    "([1])"
    (`Found "[[typing.Union[int, typing.List[int]]], int]");
  assert_select "[[_T], _S]" "(1)" (`Found "[[$literal_one], _S]");
  assert_select "[[typing.List[_T]], int]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_select "[[typing.Sequence[_T]], int]" "([1])" (`Found "[[typing.Sequence[int]], int]");
  assert_select "[[typing.List[C]], int]" "([B()])" (`Found "[[typing.List[C]], int]");
  assert_select
    "[[typing.List[C]], int]"
    "([B() for x in range(3)])"
    (`Found "[[typing.List[C]], int]");
  assert_select "[[typing.Set[C]], int]" "({ B(), B() })" (`Found "[[typing.Set[C]], int]");
  assert_select
    "[[typing.Set[C]], int]"
    "({ B() for x in range(3) })"
    (`Found "[[typing.Set[C]], int]");
  assert_select
    "[[typing.Dict[int, C]], int]"
    "({ 7: B() })"
    (`Found "[[typing.Dict[int, C]], int]");
  assert_select
    "[[typing.Dict[int, C]], int]"
    "({n: B() for n in range(5)})"
    (`Found "[[typing.Dict[int, C]], int]");
  assert_select
    "[[typing.Iterable[typing.Tuple[_T, _S]]], typing.Dict[_T, _S]]"
    "([('a', 1), ('b', 2)])"
    (`Found "[[typing.Iterable[typing.Tuple[str, int]]], typing.Dict[str, int]]");
  assert_select
    "[[typing.Sequence[_T]], int]"
    "(1)"
    (`NotFoundMismatchWithClosest
      ( "[[typing.Sequence[_T]], int]",
        Type.literal_integer 1,
        "1",
        Type.parametric "typing.Sequence" (Concrete [Type.variable "_T"]),
        None,
        1 ));
  assert_select "[[_R], _R]" "(1)" (`Found "[[int], int]");
  assert_select
    "[[_R], _R]"
    "('string')"
    (`NotFoundMismatchWithClosest
      ( "[[_R], _R]",
        Type.literal_string "string",
        "\"string\"",
        Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.integer; Type.float]) "_R",
        None,
        1 ));
  assert_select "[[typing.List[_R]], _R]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_select
    "[[typing.List[_R]], _R]"
    "(['string'])"
    (`NotFoundMismatchWithClosest
      ( "[[typing.List[_R]], _R]",
        Type.list Type.string,
        "['string']",
        Type.list
          (Type.variable
             ~constraints:(Type.Variable.Unary.Explicit [Type.integer; Type.float])
             "_R"),
        None,
        1 ));
  assert_select "[[], _R]" "()" (`Found "[[], _R]");
  assert_select "[[typing.Type[_T]], _T]" "(int)" (`Found "[[typing.Type[int]], int]");
  assert_select
    "[[typing.Type[typing.List[_T]]], _T]"
    "(meta)"
    (`Found "[[typing.Type[typing.List[int]]], int]");
  assert_select
    "[[typing.Type[_T]], _T]"
    "(typing.List[str])"
    (`Found "[[typing.Type[typing.List[str]]], typing.List[str]]");
  assert_select "[[Variable(_T)], int]" "(1, 2)" (`Found "[[Variable(int)], int]");
  assert_select "[[Keywords(_T)], int]" "(a=1, b=2)" (`Found "[[Keywords(int)], int]");
  assert_select
    "[[_T_float_or_str], None]"
    "(union)"
    (`NotFoundMismatchWithClosest
      ( "[[_T_float_or_str], None]",
        Type.union [Type.integer; Type.string],
        "union",
        Type.variable
          "_T_float_or_str"
          ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]),
        None,
        1 ));
  assert_select
    "[[_T_float_str_or_union], _T_float_str_or_union]"
    "(union)"
    (`Found "[[typing.Union[float, str]], typing.Union[float, str]]");
  assert_select
    "[[_T_bound_by_float_str_union], _T_bound_by_float_str_union]"
    "(union)"
    (`Found "[[typing.Union[int, str]], typing.Union[int, str]]");
  assert_select "[[int], _T]" "(5)" (`Found "[[int], _T]");
  assert_select "[[int], _T_float_or_str]" "(5)" (`Found "[[int], _T_float_or_str]");
  assert_select
    "[[int], _T_bound_by_float_str_union]"
    "(5)"
    (`Found "[[int], _T_bound_by_float_str_union]");
  assert_select "[[], _T]" "()" (`Found "[[], _T]");
  assert_select "[[], _T_float_or_str]" "()" (`Found "[[], _T_float_or_str]");
  assert_select
    "[[], _T_bound_by_float_str_union]"
    "()"
    (`Found "[[], _T_bound_by_float_str_union]");

  (* Ranking. *)
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0)"
    (* Ambiguous, pick the first one. *)
    (`NotFoundMissingAnonymousArgumentWithClosest ("[[int, int, str], int]", 1));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str], str][[int, str], int]]"
    "(1)"
    (* Ambiguous, prefer the one with the closer arity over the type match. *)
    (`NotFoundMismatchWithClosest
      ("[[str], str]", Type.literal_integer 1, "1", Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, Keywords()], int][[int, str], int]]"
    "(1, 1)" (* Prefer anonymous unmatched parameters over keywords. *)
    (`NotFoundMismatchWithClosest
      ("[[int, str], int]", Type.literal_integer 1, "1", Type.string, None, 2));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str], str][[], str]]"
    "(1)"
    (`NotFoundMismatchWithClosest
      ("[[str], str]", Type.literal_integer 1, "1", Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str, Keywords()], int][[Keywords()], int]]"
    "(1)" (* Prefer arity matches. *)
    (`NotFoundMismatchWithClosest
      ("[[str, Keywords()], int]", Type.literal_integer 1, "1", Type.string, None, 1));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0, 'string')"
    (* Clear winner. *)
    (`NotFoundMissingAnonymousArgumentWithClosest ("[[int, str, str], int]", 2));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, str, str, str], int][[int, str, bool], int]]"
    "(0, 'string')"
    (`NotFoundMissingAnonymousArgumentWithClosest ("[[int, str, bool], int]", 2));

  (* Match not found in overloads: error against implementation if it exists. *)
  assert_select
    "[[typing.Union[str, int]], typing.Union[str, int]][[[str], str][[int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, "unknown", Type.union [Type.integer; Type.string], None, 1));
  assert_select
    "[[bool], bool][[[str], str][[int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, "unknown", Type.bool, None, 1));
  assert_select
    "[[bool], bool][[[str, str], str][[int, int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, "unknown", Type.bool, None, 1));
  assert_select
    "[[bool], bool][[[str, str], str][[int, int], int]]"
    "(int, str)"
    (`NotFoundTooManyArguments (1, 2));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[Named(a, int), Named(b, int)], int][[Named(c, int), Named(d, int)], int]]"
    "(i=1, d=2)"
    (`NotFoundUnexpectedKeywordWithClosest ("[[Named(c, int), Named(d, int)], int]", "i"));

  (* Prefer the overload where the mismatch comes latest *)
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, str], int][[str, int], str]]"
    "(1, 1)"
    (`NotFoundMismatchWithClosest
      ("[[int, str], int]", Type.literal_integer 1, "1", Type.string, None, 2));
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str, int], str][[int, str], int]]"
    "(1, 1)"
    (`NotFoundMismatchWithClosest
      ("[[int, str], int]", Type.literal_integer 1, "1", Type.string, None, 2));

  (* Void functions. *)
  assert_select ~allow_undefined:true "[..., None]" "()" (`Found "[..., None]");
  assert_select "[[int], None]" "(1)" (`Found "[[int], None]");
  assert_select
    "[[int], None]"
    "('string')"
    (`NotFoundMismatch (Type.literal_string "string", "\"string\"", Type.integer, None, 1));
  assert_select
    "[[typing.Callable[[_T], bool]], _T]"
    "(g)"
    (`Found "[[typing.Callable[[int], bool]], int]");
  assert_select
    "[[typing.Callable[[_T], typing.List[bool]]], _T]"
    "(f)"
    (`Found "[[typing.Callable[[int], typing.List[bool]]], int]");

  (* Special dictionary constructor *)
  assert_select
    ~name:"dict.__init__"
    "[[Keywords(_S)], dict[_T, _S]]"
    "(a=1)"
    (`Found "[[Keywords($literal_one)], dict[str, $literal_one]]");

  (* TODO(T41074174): Error here rather than defaulting back to the initial signature *)
  assert_select
    ~name:"dict.__init__"
    "[[Named(map, typing.Mapping[_T, _S]), Keywords(_S)], dict[_T, _S]]"
    "({1: 1}, a=1)"
    (`Found ("[[Named(map, typing.Mapping[int, int]), Keywords(int)], " ^ "dict[int, int]]"));
  assert_select
    ~name:"dict.__init__"
    "[[Keywords(_S)], dict[_T, _S]]"
    "()"
    (`Found "[[Keywords(_S)], dict[_T, _S]]");
  assert_select
    "[[Keywords(_S)], dict[_T, _S]]"
    "(a=1)"
    (`Found "[[Keywords($literal_one)], dict[_T, $literal_one]]");
  assert_select
    "[Tparams, int]"
    "(a=1)"
    (`NotFound ("[Tparams, int]", Some CallingParameterVariadicTypeVariable));
  assert_select
    "[[Ts], int]"
    "(1, 'string', 1, 'string')"
    (`Found "[[$literal_one, $literal_string, $literal_one, $literal_string], int]");
  assert_select "[[int, Variable(Ts)], int]" "(1)" (`Found "[[int], int]");
  assert_select
    "[[Variable(Ts)], int]"
    "(1, *int_string_tuple, *int_string_tuple, 1)"
    (`Found "[[$literal_one, int, str, int, str, $literal_one], int]");
  assert_select
    "[[Variable(Ts)], int]"
    "(1, *unbounded_tuple)"
    (`NotFound
      ( "[[Variable(Ts)], int]",
        Some
          (MismatchWithListVariadicTypeVariable
             ( Variable (Type.Variable.Variadic.List.create "Ts"),
               NotDefiniteTuple
                 {
                   expression = +Name (Name.Identifier "unbounded_tuple");
                   annotation = Type.Tuple (Unbounded Type.integer);
                 } )) ));
  assert_select
    "[[typing.Tuple[Ts], Variable(Ts)], int]"
    "((1, 'string'), 1.1)"
    (`NotFound
      ( "[[typing.Tuple[$literal_one, $literal_string], $literal_one, $literal_string], int]",
        Some
          (MismatchWithListVariadicTypeVariable
             ( Variable (Type.Variable.Variadic.List.create "Ts"),
               ConstraintFailure (Concrete [Type.float]) )) ));
  assert_select
    "[[pyre_extensions.type_variable_operators.Map[typing.List, Ts]], int]"
    "([1,2,3], ['string', 'string'])"
    (`Found "[[typing.List[int], typing.List[str]], int]");
  assert_select
    "[[pyre_extensions.type_variable_operators.Map[typing.List, Ts]], \
     typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.Type, Ts]]]"
    "([1,2,3], ['string', 'string'])"
    (`Found
      "[[typing.List[int], typing.List[str]], typing.Tuple[typing.Type[int], typing.Type[str]]]");
  assert_select
    "[[bool, Variable(pyre_extensions.type_variable_operators.Map[typing.List, Ts])], int]"
    "(True, [1,2,3], ['string', 'string'])"
    (`Found "[[bool, typing.List[int], typing.List[str]], int]");
  ()


let () = "signature" >::: ["select" >:: test_select] |> Test.run
