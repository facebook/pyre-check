(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Statement

open Test
open AnnotatedTest

module Resolution = Analysis.Resolution
module Type = AnalysisType

module Call = Annotated.Call
module Signature = Annotated.Signature

open Signature


let test_select _ =
  let assert_select callable call expected =
    let resolution =
      populate
        {|
          class int: ...
          class str: ...
          _T = typing.TypeVar('_T')
          _S = typing.TypeVar('_S')
          class typing.Generic: ...
          class typing.Sequence(typing.Generic[_S]): ...
          class list(typing.Generic[_T], typing.Sequence[_T]): ...
        |}
      |> resolution
    in
    let parse_callable callable =
      Format.asprintf "typing.Callable%s" callable
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
      |> function
      | Type.Callable callable -> callable
      | _ -> failwith "Could not extract overloads"
    in
    let callable = parse_callable callable in
    let overload =
      let call =
        match parse_single_access (Format.asprintf "call%s" call) with
        | [Access.Call { Node.value = call; _ }] -> call
        | _ -> failwith "Could not parse call"
      in
      Call.create ~kind:Call.Function call
      |> Signature.select ~resolution ~callable
    in
    let expected =
      match expected with
      | `Found expected ->
          Found (parse_callable expected)
      | `NotFoundIdentical reason ->
          NotFound { rank = 0; callable; reason }
      | `NotFound (closest, reason) ->
          NotFound { rank = 0; callable = parse_callable closest; reason }
    in
    assert_equal
      ~printer:Signature.show_overload
      ~cmp:Signature.equal_overload
      expected
      overload
  in

  (* Undefined callables always match. *)
  assert_select "[..., int]" "()" (`Found "[..., int]");
  assert_select "[..., int]" "(a, b)" (`Found "[..., int]");
  assert_select "[..., int]" "(a, b='depr', *variable, **keywords)" (`Found "[..., int]");
  assert_select "[..., int][[int], int]" "(1)" (`Found "[..., int]");

  (* Traverse anonymous arguments. *)
  assert_select "[[], int]" "()" (`Found "[[], int]");

  assert_select "[[int], int]" "()" (`NotFoundIdentical None);
  assert_select "[[], int]" "(1)" (`NotFoundIdentical None);

  assert_select "[[int], int]" "(1)" (`Found "[[int], int]");
  assert_select "[[Named(i, int)], int]" "(1)" (`Found "[[Named(i, int)], int]");

  assert_select
    "[[int], int]"
    "('string')"
    (`NotFoundIdentical (Some (Mismatch { actual = Type.string; expected = Type.integer })));
  assert_select "[[int], int]" "(name='string')" (`NotFoundIdentical None);

  (* Traverse variable arguments. *)
  assert_select "[[Variable(variable)], int]" "()" (`Found "[[Variable(variable)], int]");
  assert_select "[[Variable(variable)], int]" "(1, 2)" (`Found "[[Variable(variable)], int]");

  assert_select "[[int], int]" "(*a)" (`Found "[[int], int]");
  assert_select "[[int, Named(i, int)], int]" "(*a)" (`Found "[[int, Named(i, int)], int]");

  assert_select
    "[[int, Variable(variable)], int]"
    "(1, 2)"
    (`Found "[[int, Variable(variable)], int]");

  (* Named arguments. *)
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(i=1, j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, i=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_select "[[Named(i, int), Named(j, int)], int]" "(j=1, q=2)" (`NotFoundIdentical None);
  assert_select
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, j=2, q=3)"
    (`NotFoundIdentical None);
  assert_select
    "[[Named(i, int), Named(j, str)], int]"
    "(i=1, j=2)"
    (`NotFoundIdentical (Some (Mismatch { actual = Type.integer; expected = Type.string })));

  (* Keywords. *)
  assert_select "[[Keywords(keywords)], int]" "()" (`Found "[[Keywords(keywords)], int]");
  assert_select "[[Keywords(keywords)], int]" "(a=1, b=2)" (`Found "[[Keywords(keywords)], int]");

  assert_select "[[int], int]" "(**a)" (`NotFoundIdentical None);
  assert_select "[[Named(i, int)], int]" "(**a)" (`Found "[[Named(i, int)], int]");
  assert_select "[[int, Named(i, int)], int]" "(1, **a)" (`Found "[[int, Named(i, int)], int]");

  (* Constraint resolution. *)
  assert_select "[[_T], _T]" "(1)" (`Found "[[int], int]");
  assert_select "[[_T, _S], _T]" "(1, 'string')" (`Found "[[int, str], int]");
  assert_select
    "[[_T, _T], int]"
    "(1, 'string')"
    (`Found "[[typing.Union[int, str], typing.Union[int, str]], int]");

  assert_select "[[typing.List[_T]], int]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_select "[[typing.Sequence[_T]], int]" "([1])" (`Found "[[typing.Sequence[int]], int]");
  assert_select
    "[[typing.Sequence[_T]], int]"
    "(1)"
    (`NotFoundIdentical
       (Some
          (Mismatch {
              actual = Type.integer;
              expected = Type.parametric "typing.Sequence" [Type.variable "_T"];
            })));

  (* Ranking. *)
  assert_select
    "[[int, int, str], int][[int, str, str], int]"
    "(0)"
    (`NotFound ("[[int, int, str], int]", None));  (* Ambiguous, pick the first one. *)
  assert_select
    "[[int, int, str], int][[int, str, str], int]"
    "(0, 'string')"
    (`NotFound ("[[int, str, str], int]", None));  (* Clear winner. *)
  assert_select
    "[[int, str, str, str], int][[int, str, bool], int]"
    "(0, 'string')"
    (`NotFound ("[[int, str, bool], int]", None))


let () =
  "signature">:::[
    "select">::test_select;
  ]
  |> run_test_tt_main;
