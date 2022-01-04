(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Pyre
open Test
open TypeOperation

let to_defined parameters =
  Type.Callable.Defined
    (List.mapi
       ~f:(fun i element ->
         Type.Callable.Parameter.PositionalOnly { index = i; annotation = element; default = false })
       parameters)


let make_overload ~parameters ~return =
  Some { Type.Callable.annotation = return; parameters = to_defined parameters }


let test_compose_list _ =
  let assert_compose_list ?(aliases = fun _ -> None) ~signature_select given expected =
    let parse source =
      Type.create
        ~aliases:(fun ?replace_unbound_parameters_with_any:_ -> aliases)
        (parse_single_expression ~preprocess:true source)
    in
    let merge_option_list = function
      | [] -> Some []
      | first :: rest ->
          List.fold_left ~init:(first >>| List.return) ~f:(Option.map2 ~f:(Fn.flip List.cons)) rest
          >>| List.rev
    in
    let get_callable = function
      | Type.Callable callable -> Some { callable; self_argument = None }
      | _ -> None
    in
    let annotation_list =
      List.map ~f:parse given |> List.map ~f:get_callable |> merge_option_list
    in
    let actual = annotation_list >>= TypeOperation.Compose.compose_list ~signature_select in
    assert_equal
      ~cmp:
        (Option.equal
           (fun
             { callable = left_callable; self_argument = left_self_argument }
             { callable = right_callable; self_argument = right_self_argument }
           ->
             [%eq: Type.t option] left_self_argument right_self_argument
             && Type.namespace_insensitive_compare
                  (Type.Callable left_callable)
                  (Type.Callable right_callable)
                = 0))
      ~printer:[%show: callable_and_self_argument option]
      actual
      (expected
      >>| fun expected ->
      {
        callable =
          {
            Type.Callable.kind = Type.Callable.Anonymous;
            implementation = expected;
            overloads = [];
          };
        self_argument = None;
      })
  in
  let constant_select annotation ~arguments:_ ~callable:_ ~self_argument:_ =
    SignatureSelectionTypes.Found { selected_return_annotation = annotation }
  in
  let fail_select ~arguments:_ ~callable:_ ~self_argument:_ =
    SignatureSelectionTypes.NotFound { closest_return_annotation = Type.Bottom; reason = None }
  in
  assert_compose_list
    ~signature_select:(constant_select Type.bool)
    ["typing.Callable[[int], str]"; "typing.Callable[[str], bool]"]
    (make_overload ~parameters:[Type.integer] ~return:Type.bool);
  assert_compose_list
    ~signature_select:fail_select
    ["typing.Callable[[int], bool]"; "typing.Callable[[str], bool]"]
    None;

  let variable = Type.Variable.Unary.create "T" in
  let variable2 = Type.Variable.Unary.create "R" in
  let identity_select ~arguments ~callable ~self_argument =
    match arguments with
    | [{ AttributeResolution.Argument.resolved; _ }] ->
        SignatureSelectionTypes.Found { selected_return_annotation = resolved }
    | _ -> fail_select ~arguments ~callable ~self_argument
  in
  let aliases = function
    | "T" -> Some (Type.Variable variable)
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_compose_list
    ~aliases
    ~signature_select:identity_select
    ["typing.Callable[[int], int]"; "typing.Callable[[T], T]"]
    (make_overload ~parameters:[Type.integer] ~return:Type.integer);
  let aliases = function
    | "T" -> Some (Type.Variable variable)
    | "R" -> Some (Type.Variable variable2)
    | _ -> None
  in
  let aliases = create_type_alias_table aliases in
  assert_compose_list
    ~aliases
    ~signature_select:identity_select
    ["typing.Callable[[R], R]"; "typing.Callable[[T], T]"]
    (make_overload ~parameters:[Type.Variable variable2] ~return:(Type.Variable variable2));

  assert_compose_list
    ~signature_select:(constant_select Type.integer)
    ["typing.Callable[[int], int]"]
    (make_overload ~parameters:[Type.integer] ~return:Type.integer);
  assert_compose_list ~signature_select:identity_select [] None;
  assert_compose_list
    ~aliases
    ~signature_select:identity_select
    [
      "typing.Callable[[int], int]";
      "typing.Callable[[T], T]";
      "typing.Callable[[R], R]";
      "typing.Callable[[T], T]";
    ]
    (make_overload ~parameters:[Type.integer] ~return:Type.integer);
  ()


let () = "typeOperation" >::: ["compose_list" >:: test_compose_list] |> Test.run
