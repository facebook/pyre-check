(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
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
  let assert_overload callable call expected =
    let resolution =
      populate
        {|
          _T = typing.TypeVar('_T')
          _S = typing.TypeVar('_S')
          class int: ...
          class str: ...
        |}
      |> resolution
    in
    let parse_callable callable =
      Format.asprintf "typing.Callable%s" callable
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
    in
    let call =
      match parse_single_access (Format.asprintf "call%s" call) with
      | [Access.Call { Node.value = call; _ }] -> call
      | _ -> failwith "Could not parse call"
    in
    let overloads =
      match parse_callable callable with
      | Type.Callable { Type.Callable.overloads; _ } -> overloads
      | _ -> failwith "Could not extract overloads"
    in
    let overload =
      Call.create ~kind:Call.Function call
      |> Call.overload ~overloads ~resolution
      >>| fun overload ->
      Type.Callable {
        Type.Callable.kind = Type.Callable.Anonymous;
        overloads = [overload];
      }
    in
    assert_equal
      ~printer:(function | Some annotation -> Type.show annotation | _ -> "None")
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_callable)
      overload
  in

  (* Undefined callables always match. *)
  assert_overload "[..., int]" "()" (Some "[..., int]");
  assert_overload "[..., int]" "(a, b)" (Some "[..., int]");
  assert_overload "[..., int]" "(a, b='depr', *variable, **keywords)" (Some "[..., int]");
  assert_overload "[..., int][[int], int]" "(1)" (Some "[..., int]");

  (* Traverse anonymous arguments. *)
  assert_overload "[[], int]" "()" (Some "[[], int]");

  assert_overload "[[int], int]" "()" None;
  assert_overload "[[], int]" "(1)" None;

  assert_overload "[[int], int]" "(1)" (Some "[[int], int]");
  assert_overload "[[Named(i, int)], int]" "(1)" (Some "[[Named(i, int)], int]");

  assert_overload "[[int], int]" "('string')" None;
  assert_overload "[[int], int]" "(name='string')" None;

  (* Traverse variable arguments. *)
  assert_overload "[[Variable(variable)], int]" "()" (Some "[[Variable(variable)], int]");
  assert_overload "[[Variable(variable)], int]" "(1, 2)" (Some "[[Variable(variable)], int]");

  assert_overload "[[int], int]" "(*a)" (Some "[[int], int]");
  assert_overload "[[int, Named(i, int)], int]" "(*a)" (Some "[[int, Named(i, int)], int]");

  assert_overload
    "[[int, Variable(variable)], int]"
    "(1, 2)"
    (Some "[[int, Variable(variable)], int]");

  (* Named arguments. *)
  assert_overload
    "[[Named(i, int), Named(j, int)], int]"
    "(i=1, j=2)"
    (Some "[[Named(i, int), Named(j, int)], int]");
  assert_overload
    "[[Named(i, int), Named(j, int)], int]"
    "(j=1, i=2)"
    (Some "[[Named(i, int), Named(j, int)], int]");
  assert_overload "[[Named(i, int), Named(j, int)], int]" "(j=1, q=2)" None;
  assert_overload "[[Named(i, int), Named(j, int)], int]" "(j=1, j=2, q=3)" None;
  assert_overload "[[Named(i, int), Named(j, str)], int]" "(i=1, j=2)" None;

  (* Keywords. *)
  assert_overload "[[Keywords(keywords)], int]" "()" (Some "[[Keywords(keywords)], int]");
  assert_overload "[[Keywords(keywords)], int]" "(a=1, b=2)" (Some "[[Keywords(keywords)], int]");

  assert_overload "[[int], int]" "(**a)" None;
  assert_overload "[[Named(i, int)], int]" "(**a)" (Some "[[Named(i, int)], int]");
  assert_overload "[[int, Named(i, int)], int]" "(1, **a)" (Some "[[int, Named(i, int)], int]");

  (* Constraint resolution. *)
  assert_overload "[[_T], _T]" "(1)" (Some "[[int], int]");
  assert_overload "[[_T, _S], _T]" "(1, 'string')" (Some "[[int, str], int]");
  assert_overload
    "[[_T, _T], int]"
    "(1, 'string')"
    (Some "[[typing.Union[int, str], typing.Union[int, str]], int]")



let () =
  "call">:::[
    "backup">::test_backup;
    "overload">::test_overload;
  ]
  |> run_test_tt_main;
