(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Statement
open Pyre

open Test
open AnnotatedTest

module Resolution = Analysis.Resolution

module Signature = Annotated.Signature

open Signature


let resolution =
  populate
    {|
      class int: ...
      class str: ...
      _T = typing.TypeVar('_T')
      _S = typing.TypeVar('_S')
      _R = typing.TypeVar('_R', int, float)
      _U = typing.TypeVar('_U', int, str)
      class typing.Generic: ...
      class typing.Type(typing.Generic[_T]): ...

      class typing.Sequence(typing.Generic[_S]): ...
      class typing.Mapping(typing.Generic[_T, _S]): ...

      class list(typing.Generic[_T], typing.Sequence[_T]): ...
      class dict(typing.Generic[_T, _S], typing.Mapping[_T, _S]): ...

      meta: typing.Type[typing.List[int]] = ...
      union: typing.Union[int, str] = ...

      unknown: $unknown = ...
    |}
  |> fun environment -> TypeCheck.resolution environment ()


let parse_annotation annotation =
  annotation
  |> parse_single_expression
  |> Resolution.parse_annotation resolution


let test_select _ =
  let assert_select ?(allow_undefined = false) callable arguments expected =
    let parse_callable callable =
      Format.asprintf "typing.Callable%s" callable
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
            callable
      | _ ->
          failwith "Could not extract signatures"
    in
    let callable = parse_callable callable in
    let signature =
      let arguments =
        match parse_single_access (Format.asprintf "call%s" arguments) with
        | [Access.Identifier _; Access.Call { Node.value = arguments; _ }] -> arguments
        | _ -> failwith "Could not parse call"
      in
      Signature.select ~arguments ~resolution ~callable
    in
    let callable = { callable with Type.Callable.overloads = [] } in
    let expected =
      match expected with
      | `Found expected ->
          Found { callable = parse_callable expected; constraints = Type.Map.empty }
      | `NotFoundNoReason ->
          NotFound { callable; reason = None }
      | `NotFoundMissingArgument name ->
          NotFound { callable; reason = Some (MissingArgument (Access.create name)) }
      | `NotFoundMissingArgumentWithClosest (closest, name) ->
          NotFound {
            callable = parse_callable closest;
            reason = Some (MissingArgument (Access.create name));
          }
      | `NotFoundTooManyArguments (expected, provided) ->
          NotFound {
            callable;
            reason = Some (TooManyArguments { expected; provided });
          }
      | `NotFoundTooManyArgumentsWithClosest (closest, expected, provided) ->
          NotFound {
            callable = parse_callable closest;
            reason = Some (TooManyArguments { expected; provided });
          }
      | `NotFoundUnexpectedKeyword name ->
          NotFound { callable; reason = Some (UnexpectedKeyword (Identifier.create name)) }
      | `NotFoundUnexpectedKeywordWithClosest (closest, name) ->
          NotFound {
            callable = parse_callable closest;
            reason = Some (UnexpectedKeyword (Identifier.create name));
          }
      | `NotFoundMismatch (actual, expected, name, position) ->
          let reason =
            { actual; expected; name = name >>| Identifier.create; position }
            |> Node.create_with_default_location
            |> fun mismatch -> Some (Mismatch mismatch)
          in
          NotFound { callable; reason }
      | `NotFoundMismatchWithClosest (closest, actual, expected, name, position) ->
          let reason =
            { actual; expected; name = name >>| Identifier.create; position }
            |> Node.create_with_default_location
            |> fun mismatch -> Some (Mismatch mismatch)
          in
          NotFound { callable = parse_callable closest; reason }
      | `NotFound (closest, reason) ->
          NotFound { callable = parse_callable closest; reason }
    in
    assert_equal
      ~printer:Signature.show
      ~cmp:Signature.equal
      expected
      signature
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

  assert_select "[[int], int]" "()" (`NotFoundMissingArgument "$0");
  assert_select "[[], int]" "(1)" (`NotFoundTooManyArguments (0, 1));

  assert_select "[[int], int]" "(1)" (`Found "[[int], int]");
  assert_select "[[Named(i, int)], int]" "(1)" (`Found "[[Named(i, int)], int]");

  assert_select "[[typing.Any], int]" "(unknown)" (`Found "[[typing.Any], int]");

  assert_select
    "[[int], int]"
    "('string')"
    (`NotFoundMismatch (Type.string, Type.integer, None, 1));
  assert_select "[[int], int]" "(name='string')" (`NotFoundUnexpectedKeyword "name");

  assert_select "[[int], int]" "(*[1])" (`Found "[[int], int]");
  assert_select "[[str], int]" "(*[1])" (`NotFoundMismatch (Type.integer, Type.string, None, 1));
  assert_select "[[int, str], int]" "(*[1], 'asdf')" (`NotFoundTooManyArguments (2, 3));

  assert_select "[[object], None]" "(union)" (`Found "[[object], None]");
  assert_select "[[int], None]" "(union)" (`NotFoundMismatch (Type.string, Type.integer, None, 1));

  (* Traverse variable arguments. *)
  assert_select "[[Variable(variable)], int]" "()" (`Found "[[Variable(variable)], int]");
  assert_select "[[Variable(variable)], int]" "(1, 2)" (`Found "[[Variable(variable)], int]");
  assert_select
    "[[Variable(variable, int)], int]"
    "(1, 2)"
    (`Found "[[Variable(variable, int)], int]");
  assert_select
    "[[Variable(variable, str)], int]"
    "(1, 2)"
    (`NotFoundMismatch (Type.integer, Type.string, None, 1));
  assert_select
    "[[Variable(variable, str)], int]"
    "('string', 2)"
    (`NotFoundMismatch (Type.integer, Type.string, None, 2));
  assert_select
    "[[Variable(variable, int)], int]"
    "(*[1, 2], 3)"
    (`Found "[[Variable(variable, int)], int]");
  assert_select
    "[[Variable(variable, int), Named(a, str)], int]"
    "(*[1, 2], a='string')"
    (`Found "[[Variable(variable, int), Named(a, str)], int]");
  assert_select
    "[[Variable(variable, int), Named(a, str)], int]"
    "(*[1, 2], *[3, 4], a='string')"
    (`Found "[[Variable(variable, int), Named(a, str)], int]");
  assert_select
    "[[Variable(variable, int)], int]"
    "(*['string'])"
    (`NotFoundMismatch (Type.string, Type.integer, None, 1));
  (* A single * is special. *)
  assert_select
    "[[Variable($parameter$, int), Named(i, int)], int]"
    "(i=1)"
    (`Found "[[Variable($parameter$, int), Named(i, int)], int]");
  assert_select
    "[[Variable($parameter$, int), Named(i, int)], int]"
    "(2, i=1)"
    (`NotFoundTooManyArguments (2, 3));

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
    "[[Named(i, int), Named(j, int)], int]" "(j=1, q=2)"
    (`NotFoundUnexpectedKeyword "q");
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
    (`NotFoundMismatch (Type.integer, Type.string, Some "j", 2));
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'j': 1, 'i': 2})"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'j': 'string', 'i': 'string'})"
    (`NotFoundMismatch (Type.string, Type.integer, None, 1));
  assert_select
    "[[int], int]" "(**a)"
    (`NotFoundMismatch (Type.Top, Type.integer, None, 1));
  assert_select
    "[[int, Named(i, int)], int]"
    "(1, **{'a': 1})"
    (`Found "[[int, Named(i, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(**{'i': 1}, j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");


  (* Keywords. *)
  assert_select "[[Keywords(keywords)], int]" "()" (`Found "[[Keywords(keywords)], int]");
  assert_select "[[Keywords(keywords)], int]" "(a=1, b=2)" (`Found "[[Keywords(keywords)], int]");
  assert_select
    "[[Keywords(keywords, int)], int]"
    "(a=1, b=2)"
    (`Found "[[Keywords(keywords, int)], int]");
  assert_select
    "[[Named(a, int), Named(c, int), Keywords(keywords, int)], int]"
    "(a=1, b=2, c=3)"
    (`Found "[[Named(a, int), Named(c, int), Keywords(keywords, int)], int]");
  assert_select
    "[[Keywords(keywords, str)], int]"
    "(a=1, b=2)"
    (`NotFoundMismatch (Type.integer, Type.string, Some "a", 1));
  assert_select
    "[[Keywords(keywords, str)], int]"
    "(a='string', b=2)"
    (`NotFoundMismatch (Type.integer, Type.string, Some "b", 2));

  (* Constraint resolution. *)
  assert_select "[[_T], _T]" "(1)" (`Found "[[int], int]");
  assert_select
    "[[typing.Callable[[], _T]], _T]"
    "(lambda: 1)"
    (`Found "[[typing.Callable[[], int]], int]");
  assert_select "[[_T, _S], _T]" "(1, 'string')" (`Found "[[int, str], int]");
  assert_select
    "[[_T, _T], int]"
    "(1, 'string')"
    (`Found "[[typing.Union[int, str], typing.Union[int, str]], int]");
  assert_select
    "[[_T], typing.Union[str, _T]]"
    "(1)"
    (`Found "[[int], typing.Union[str, int]]");
  assert_select
    "[[typing.Union[int, typing.List[_T]]], _T]"
    "([1])"
    (`Found "[[typing.Union[int, typing.List[int]]], int]");
  assert_select "[[_T], _S]" "(1)" (`Found "[[int], $bottom]");

  assert_select "[[typing.List[_T]], int]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_select "[[typing.Sequence[_T]], int]" "([1])" (`Found "[[typing.Sequence[int]], int]");
  assert_select
    "[[typing.Sequence[_T]], int]"
    "(1)"
    (`NotFoundMismatch
       (Type.integer, Type.parametric "typing.Sequence" [Type.variable "_T"], None, 1));

  assert_select "[[_R], _R]" "(1)" (`Found "[[int], int]");
  assert_select
    "[[_R], _R]"
    "('string')"
    (`NotFoundMismatch
       (Type.string,
        Type.variable ~constraints:(Type.Explicit [Type.integer; Type.float]) "_R", None, 1));
  assert_select "[[typing.List[_R]], _R]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_select
    "[[typing.List[_R]], _R]"
    "(['string'])"
    (`NotFoundMismatch
       (Type.list Type.string,
        Type.list (Type.variable ~constraints:(Type.Explicit [Type.integer; Type.float]) "_R"),
        None,
        1));
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

  assert_select
    "[[Variable(variable, _T)], int]"
    "(1, 2)"
    (`Found "[[Variable(variable, int)], int]");
  assert_select
    "[[Keywords(keywords, _T)], int]"
    "(a=1, b=2)"
    (`Found "[[Keywords(keywords, int)], int]");

  assert_select "[[_U], None]" "(union)" (`Found "[[_U], None]");

  (* Ranking. *)
  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0)"
    (* Ambiguous, pick the first one. *)
    (`NotFoundMissingArgumentWithClosest
       ("[[int, int, str], int]", "$1"));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str], str][[int, str], int]]"
    "(1)"
    (* Ambiguous, prefer the one with the closer arity over the type match. *)
    (`NotFoundMismatchWithClosest
       ("[[str], str]", Type.integer, Type.string, None, 1));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, Keywords(keywords)], int][[int, str], int]]"
    "(1, 1)" (* Prefer anonymous unmatched parameters over keywords. *)
    (`NotFoundMismatchWithClosest
       ("[[int, str], int]", Type.integer, Type.string, None, 2));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str], str][[], str]]"
    "(1)"
    (`NotFoundMismatchWithClosest
       ("[[str], str]", Type.integer, Type.string, None, 1));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[str, Keywords(keywords)], int][[Keywords(keywords)], int]]"
    "(1)" (* Prefer arity matches. *)
    (`NotFoundMismatchWithClosest
       ("[[str, Keywords(keywords)], int]", Type.integer, Type.string, None, 1));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, int, str], int][[int, str, str], int]]"
    "(0, 'string')"
    (* Clear winner. *)
    (`NotFoundMissingArgumentWithClosest
       ("[[int, str, str], int]",
        "$2"));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[int, str, str, str], int][[int, str, bool], int]]"
    "(0, 'string')"
    (`NotFoundMissingArgumentWithClosest
       ("[[int, str, bool], int]", "$2"));

  (* Match not found in overloads: error against implementation if it exists. *)
  assert_select
    "[[typing.Union[str, int]], typing.Union[str, int]][[[str], str][[int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, Type.union [Type.integer; Type.string], None, 1));

  assert_select
    "[[bool], bool][[[str], str][[int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, Type.bool, None, 1));

  assert_select
    "[[bool], bool][[[str, str], str][[int, int], int]]"
    "(unknown)"
    (`NotFoundMismatch (Type.Top, Type.bool, None, 1));

  assert_select
    "[[bool], bool][[[str, str], str][[int, int], int]]"
    "(int, str)"
    (`NotFoundTooManyArguments (1, 2));

  assert_select
    ~allow_undefined:true
    "[..., $unknown][[[Named(a, int), Named(b, int)], int][[Named(c, int), Named(d, int)], int]]"
    "(i=1, d=2)"
    (`NotFoundUnexpectedKeywordWithClosest ("[[Named(c, int), Named(d, int)], int]", "i"));

  (* Void functions. *)
  assert_select ~allow_undefined:true "[..., None]" "()" (`Found "[..., None]");
  assert_select "[[int], None]" "(1)" (`Found "[[int], None]");
  assert_select
    "[[int], None]"
    "('string')"
    (`NotFoundMismatch (Type.string, Type.integer, None, 1))


let test_determine _ =
  let assert_determine ?(found = true) constraints actual expected =
    let signature =
      let callable =
        match Type.callable ~annotation:Type.integer () with
        | Type.Callable callable -> callable
        | _ -> failwith "Could not extract callable"
      in
      if found then
        Found { callable; constraints = Type.Map.of_alist_exn constraints }
      else
        NotFound { callable; reason = None }
    in
    assert_equal
      ~cmp:(Option.equal Type.equal)
      ~printer:(function | Some annotation -> Type.show annotation | _ -> "None")
      expected
      (Signature.determine signature ~resolution ~annotation:actual)
  in
  assert_determine [] Type.integer None;
  assert_determine
    [Type.variable "_T", Type.integer]
    (Type.list Type.Bottom)
    (Some (Type.list Type.integer));

  (* Unrelated variable does not determine a type. *)
  assert_determine
    [Type.variable "_S", Type.integer]
    (Type.list Type.Bottom)
    (Some (Type.list Type.Bottom));

  (* Unresolved signatures do not determine types. *)
  assert_determine
    ~found:false
    [Type.variable "_T", Type.integer]
    (Type.list Type.Bottom)
    None


let () =
  "signature">:::[
    "select">::test_select;
    "determine">::test_determine;
  ]
  |> Test.run;
