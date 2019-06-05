(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open Ast
open Core
open Taint

let test_normalize_access _ =
  let parse_access expression =
    match Test.parse_single_expression ~convert:true expression with
    | { Node.value = Expression.Access access; _ } -> access
    | _ -> failwith "not an access"
  in
  let assert_normalized ?(modules = []) access expected =
    let access = parse_access access in
    let resolution =
      let sources =
        if List.is_empty modules then
          None
        else
          List.map modules ~f:(fun name -> Source.create ~qualifier:(Reference.create name) [])
          |> Option.some
      in
      Test.resolution ?sources ()
    in
    let normalized = AccessPath.normalize_access access ~resolution in
    assert_equal
      ~cmp:Expression.equal
      ~printer:Expression.show
      (Node.create_with_default_location (Expression.Access access))
      (AccessPath.as_expression normalized |> Expression.convert);
    assert_equal
      ~cmp:AccessPath.equal_normalized_expression
      ~printer:AccessPath.show_normalized_expression
      expected
      normalized
  in
  let local name = AccessPath.Local name in
  let global name = AccessPath.Global (Reference.create name) in
  let literal literal =
    Expression.String (Expression.StringLiteral.create literal)
    |> Node.create_with_default_location
  in
  assert_normalized "a" (global "a");
  assert_normalized "a()" (AccessPath.Call { callee = global "a"; arguments = [] });
  assert_normalized
    ~modules:["a"]
    "a.b.c"
    (AccessPath.Access { expression = global "a.b"; member = "c" });
  assert_normalized ~modules:["a"; "a.b"] "a.b.c" (global "a.b.c");
  assert_normalized
    ~modules:["a"; "a.b"]
    "a.b.c()"
    (AccessPath.Call { callee = global "a.b.c"; arguments = [] });
  assert_normalized
    ~modules:["a"; "a.b"]
    "a.b.c.d.e"
    (AccessPath.Access
       { expression = AccessPath.Access { expression = global "a.b.c"; member = "d" };
         member = "e"
       });
  assert_normalized "$a" (local "$a");
  assert_normalized "$a()" (AccessPath.Call { callee = local "$a"; arguments = [] });
  assert_normalized "$a.b" (AccessPath.Access { expression = local "$a"; member = "b" });
  assert_normalized
    "'some string'.join"
    (AccessPath.Access
       { expression = AccessPath.Expression (literal "some string"); member = "join" })


let () = "taintaccesspath" >::: ["normalize" >:: test_normalize_access] |> Test.run
