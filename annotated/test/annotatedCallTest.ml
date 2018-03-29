(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Statement
open Pyre

open Test
open AnnotatedTest

module Call = Annotated.Call


let test_backup _ =
  let assert_backup call expected =
    let actual = Annotated.Call.create ~kind:Annotated.Call.Function call in
    let expected = expected >>| Annotated.Call.create ~kind:Annotated.Call.Function in
    assert_equal (Annotated.Call.backup actual) expected
  in
  assert_backup
    { Expression.Call.name = !"name"; arguments = [] }
    None;
  assert_backup
    { Expression.Call.name = !"__add__"; arguments = [] }
    (Some { Expression.Call.name = !"__radd__"; arguments = [] });
  assert_backup
    { Expression.Call.name = !"__sub__"; arguments = [] }
    (Some { Expression.Call.name = !"__rsub__"; arguments = [] })


let test_overload _ =
  let open Call in
  let assert_overload callable call expected =
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
      |> Call.overload ~resolution ~callable
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
      ~printer:Call.show_overload
      ~cmp:Call.equal_overload
      expected
      overload
  in

  (* Undefined callables always match. *)
  assert_overload "[..., int]" "()" (`Found "[..., int]");
  assert_overload "[..., int]" "(a, b)" (`Found "[..., int]");
  assert_overload "[..., int]" "(a, b='depr', *variable, **keywords)" (`Found "[..., int]");
  assert_overload "[..., int][[int], int]" "(1)" (`Found "[..., int]");

  (* Traverse anonymous arguments. *)
  assert_overload "[[], int]" "()" (`Found "[[], int]");

  assert_overload "[[int], int]" "()" (`NotFoundIdentical None);
  assert_overload "[[], int]" "(1)" (`NotFoundIdentical None);

  assert_overload "[[int], int]" "(1)" (`Found "[[int], int]");
  assert_overload "[[Named(i, int)], int]" "(1)" (`Found "[[Named(i, int)], int]");

  assert_overload
    "[[int], int]"
    "('string')"
    (`NotFoundIdentical (Some (Mismatch { actual = Type.string; expected = Type.integer })));
  assert_overload "[[int], int]" "(name='string')" (`NotFoundIdentical None);

  (* Traverse variable arguments. *)
  assert_overload "[[Variable(variable)], int]" "()" (`Found "[[Variable(variable)], int]");
  assert_overload "[[Variable(variable)], int]" "(1, 2)" (`Found "[[Variable(variable)], int]");

  assert_overload "[[int], int]" "(*a)" (`Found "[[int], int]");
  assert_overload "[[int, Named(i, int)], int]" "(*a)" (`Found "[[int, Named(i, int)], int]");

  assert_overload
    "[[int, Variable(variable)], int]"
    "(1, 2)"
    (`Found "[[int, Variable(variable)], int]");

  (* Named arguments. *)
  assert_overload
    "[[Named(i, int), Named(j, int)], int]"
    "(i=1, j=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_overload
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, i=2)"
    (`Found "[[Named(i, int), Named(j, int)], int]");
  assert_overload "[[Named(i, int), Named(j, int)], int]" "(j=1, q=2)" (`NotFoundIdentical None);
  assert_overload
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, j=2, q=3)"
    (`NotFoundIdentical None);
  assert_overload "[[Named(i, int), Named(j, str)], int]" "(i=1, j=2)" (`NotFoundIdentical None);

  (* Keywords. *)
  assert_overload "[[Keywords(keywords)], int]" "()" (`Found "[[Keywords(keywords)], int]");
  assert_overload "[[Keywords(keywords)], int]" "(a=1, b=2)" (`Found "[[Keywords(keywords)], int]");

  assert_overload "[[int], int]" "(**a)" (`NotFoundIdentical None);
  assert_overload "[[Named(i, int)], int]" "(**a)" (`Found "[[Named(i, int)], int]");
  assert_overload "[[int, Named(i, int)], int]" "(1, **a)" (`Found "[[int, Named(i, int)], int]");

  (* Constraint resolution. *)
  assert_overload "[[_T], _T]" "(1)" (`Found "[[int], int]");
  assert_overload "[[_T, _S], _T]" "(1, 'string')" (`Found "[[int, str], int]");
  assert_overload
    "[[_T, _T], int]"
    "(1, 'string')"
    (`Found "[[typing.Union[int, str], typing.Union[int, str]], int]");

  assert_overload "[[typing.List[_T]], int]" "([1])" (`Found "[[typing.List[int]], int]");
  assert_overload "[[typing.Sequence[_T]], int]" "([1])" (`Found "[[typing.Sequence[int]], int]");
  assert_overload
    "[[typing.Sequence[_T]], int]"
    "(1)"
    (`NotFoundIdentical
       (Some
          (Mismatch {
              actual = Type.integer;
              expected = Type.parametric "typing.Sequence" [Type.variable "_T"];
            })));

  (* Ranking. *)
  assert_overload
    "[[int, int, str], int][[int, str, str], int]"
    "(0)"
    (`NotFound ("[[int, int, str], int]", None));  (* Ambiguous, pick the first one. *)
  assert_overload
    "[[int, int, str], int][[int, str, str], int]"
    "(0, 'string')"
    (`NotFound ("[[int, str, str], int]", None));  (* Clear winner. *)
  assert_overload
    "[[int, str, str, str], int][[int, str, bool], int]"
    "(0, 'string')"
    (`NotFound ("[[int, str, bool], int]", None))


let () =
  "call">:::[
    "backup">::test_backup;
    "overload">::test_overload;
  ]
  |> run_test_tt_main;
