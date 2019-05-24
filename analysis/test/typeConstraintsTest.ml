(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre

open Analysis
open TypeConstraints
open Test


let child = Type.Primitive "Child"
let left_parent = Type.Primitive "left_parent"
let right_parent = Type.Primitive "right_parent"
let grandparent = Type.Primitive "Grandparent"


module DiamondOrder = struct
  type t = unit
  let less_or_equal _ ~left ~right =
    match left, right with
    | _, _ when Type.equal left right -> true
    | _, Type.Top -> true
    | Type.Bottom, _ -> true
    | Type.Primitive "Child", Type.Primitive "left_parent" -> true
    | Type.Primitive "Child", Type.Primitive "right_parent" -> true
    | Type.Primitive "Child", Type.Primitive "Grandparent" -> true
    | Type.Primitive "left_parent", Type.Primitive "Grandparent" -> true
    | Type.Primitive "right_parent", Type.Primitive "Grandparent" -> true
    | _ -> false

  let meet _ left right =
    match left, right with
    | left, right when less_or_equal () ~left ~right -> left
    | right, left when less_or_equal () ~left ~right -> left
    | Type.Primitive "left_parent", Type.Primitive "right_parent" -> Type.Primitive "Child"
    | _ -> Type.Bottom

  let join _ left right =
    match left, right with
    | left, right when less_or_equal () ~left ~right -> right
    | right, left when less_or_equal () ~left ~right -> right
    | Type.Primitive "left_parent", Type.Primitive "right_parent" -> Type.Primitive "Grandparent"
    | _ -> Type.Top
end


module DiamondOrderedConstraints = OrderedConstraints(DiamondOrder)


let variable ?(name = "_V") constraints =
  Type.Variable.Unary.create name ~constraints


let add_bound constraints (variable, kind, bound) =
  let pair = Type.Variable.UnaryPair (variable, bound) in
  let order = () in
  constraints
  >>= begin
    fun constraints ->
      match kind with
      | `Lower ->
          DiamondOrderedConstraints.add_lower_bound constraints ~order ~pair
      | `Upper ->
          DiamondOrderedConstraints.add_upper_bound constraints ~order ~pair
  end


let add_parameter_variadic_bound constraints (variable, kind, bound) =
  let pair = Type.Variable.ParameterVariadicPair (variable, bound) in
  let order = () in
  constraints
  >>= begin
    fun constraints ->
      match kind with
      | `Lower ->
          DiamondOrderedConstraints.add_lower_bound constraints ~order ~pair
      | `Upper ->
          DiamondOrderedConstraints.add_upper_bound constraints ~order ~pair
  end


let test_add_bound _ =
  let assert_bound_result ?(preconstraints = Some empty) ~variable ~kind ~bound expected_is_some =
    let result =
      add_bound preconstraints (variable, kind, bound)
      |> Option.is_some
    in
    assert_equal
      ~printer:(Printf.sprintf "%B")
      expected_is_some
      result
  in
  let unconstrained = variable Type.Variable.Unary.Unconstrained in
  assert_bound_result
    ~variable:unconstrained
    ~kind:`Lower
    ~bound:child
    true;
  assert_bound_result
    ~preconstraints:(add_bound (Some empty) (unconstrained, `Lower, left_parent))
    ~variable:unconstrained
    ~kind:`Upper
    ~bound:right_parent
    false;

  assert_bound_result
    ~variable:(variable (Type.Variable.Unary.Bound child))
    ~kind:`Lower
    ~bound:left_parent
    false;
  assert_bound_result
    ~variable:(variable (Type.Variable.Unary.Bound child))
    ~kind:`Lower
    ~bound:child
    true;
  assert_bound_result
    ~variable:(variable (Type.Variable.Unary.Bound child))
    ~kind:`Upper
    ~bound:left_parent
    true;

  let explicit_parent_a_parent_b =
    variable (Type.Variable.Unary.Explicit [left_parent; right_parent])
  in
  assert_bound_result
    ~variable:explicit_parent_a_parent_b
    ~kind:`Lower
    ~bound:left_parent
    true;
  assert_bound_result
    ~variable:explicit_parent_a_parent_b
    ~kind:`Lower
    ~bound:right_parent
    true;
  assert_bound_result
    ~preconstraints:(add_bound (Some empty) (explicit_parent_a_parent_b, `Lower, left_parent))
    ~variable:explicit_parent_a_parent_b
    ~kind:`Lower
    ~bound:right_parent
    false;

  let assert_bound_result ?(preconstraints = empty) ~variable ~kind ~bound expected_is_some =
    let result =
      add_parameter_variadic_bound (Some preconstraints) (variable, kind, bound)
      |> Option.is_some
    in
    assert_equal
      ~printer:(Printf.sprintf "%B")
      expected_is_some
      result
  in
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  (* Adding a constraint to a parameter variadic with no preconstraints should always work *)
  assert_bound_result
    ~variable:parameter_variadic
    ~kind:`Lower
    ~bound:(Type.Callable.Defined [])
    true;
  let preconstraints =
    add_parameter_variadic_bound (Some empty) (parameter_variadic, `Lower, Type.Callable.Defined [])
    |> (fun preconstraints -> Option.value_exn preconstraints)
  in
  (* Adding the same constraint twice should be permitted *)
  assert_bound_result
    ~preconstraints
    ~variable:parameter_variadic
    ~kind:`Lower
    ~bound:(Type.Callable.Defined [])
    true;
  (* We currently always reject adding a different bound to something with a bound already *)
  assert_bound_result
    ~preconstraints
    ~variable:parameter_variadic
    ~kind:`Lower
    ~bound:(Type.Callable.Defined [Type.Callable.Parameter.create ~annotation:Type.integer "x" 0])
    false;
  ()


let optional_map_compare left right =
  match left, right with
  | Some left, Some right -> TypeConstraints.Solution.equal left right
  | None, None -> true
  | _ , _ -> false


let optional_map_print map =
  map
  >>| TypeConstraints.Solution.show
  |> Option.value ~default:"None"


let expect_sequence_solution
    ?callable_parameters_bounds
    bounds
    ?callable_parameters_solution
    expected =
  let result =
    let add_callable_parameters_bounds init =
      match callable_parameters_bounds with
      | Some bounds ->
          List.fold bounds ~init ~f:add_parameter_variadic_bound
      | None ->
          init
    in
    List.fold bounds ~init:(Some empty) ~f:add_bound
    |> add_callable_parameters_bounds
    >>= DiamondOrderedConstraints.solve ~order:()
  in
  let expected =
    expected
    >>| TypeConstraints.Solution.create ?callable_parameters:callable_parameters_solution
  in
  assert_equal
    ~cmp:optional_map_compare
    ~printer:optional_map_print
    expected
    result


let test_single_variable_solution _ =
  expect_sequence_solution
    []
    (Some []);

  let unconstrained = variable Type.Variable.Unary.Unconstrained in

  expect_sequence_solution
    [unconstrained, `Lower, child]
    (Some [unconstrained, child]);
  (* Solving unconstrained to bottom would be sound as it fulfills the bound, but we want to
     eliminate bottoms whenever possible, so this should be fine *)
  expect_sequence_solution
    [unconstrained, `Upper, child]
    (Some [unconstrained, child]);
  expect_sequence_solution
    [unconstrained, `Lower, child; unconstrained, `Lower, left_parent]
    (Some [unconstrained, left_parent]);
  expect_sequence_solution
    [unconstrained, `Lower, left_parent; unconstrained, `Lower, right_parent]
    (Some [unconstrained, grandparent]);
  expect_sequence_solution
    [unconstrained, `Upper, left_parent; unconstrained, `Lower, grandparent]
    None;

  expect_sequence_solution
    [unconstrained, `Upper, Type.Variable unconstrained]
    (Some []);
  expect_sequence_solution
    [unconstrained, `Upper, Type.list (Type.Variable unconstrained)]
    None;

  let bounded_by_parent_A = variable (Type.Variable.Unary.Bound left_parent) in

  expect_sequence_solution
    [bounded_by_parent_A, `Lower, child]
    (Some [bounded_by_parent_A, child]);
  expect_sequence_solution
    [bounded_by_parent_A, `Lower, right_parent]
    None;

  let explicit_int_string_parent_A =
    variable (Type.Variable.Unary.Explicit [Type.integer; Type.string; left_parent])
  in

  expect_sequence_solution
    [explicit_int_string_parent_A, `Lower, child]
    (Some [explicit_int_string_parent_A, left_parent]);
  expect_sequence_solution
    [explicit_int_string_parent_A, `Lower, grandparent]
    None;

  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  let empty_parameters = Type.Callable.Defined [] in

  let one_named_parameter =
    Type.Callable.Defined [Type.Callable.Parameter.create ~annotation:Type.integer "x" 0]
  in
  (* The simplest case for parameter variadics: adding a single lower bound of empty parameters to
     a variable yields a solution of a replacement of that variable with empty parameters *)
  expect_sequence_solution
    ~callable_parameters_bounds:[parameter_variadic, `Lower, empty_parameters]
    []
    ~callable_parameters_solution:[parameter_variadic, empty_parameters]
    (Some []);
  (* Attempting to bound a parameter variadic by more than one set of non-identical parameters
     fails *)
  expect_sequence_solution
    ~callable_parameters_bounds:[
      parameter_variadic, `Lower, empty_parameters;
      parameter_variadic, `Lower, one_named_parameter;
    ]
    []
    None;
  ()


let test_multiple_variable_solution _ =
  let unconstrained_a = variable ~name:"A" Type.Variable.Unary.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unary.Unconstrained in
  expect_sequence_solution
    [
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, child;
    ]
    (Some [unconstrained_a, child; unconstrained_b, child]);
  (* Could be solvable, choosing not to deal with this yet *)
  expect_sequence_solution
    [
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, Type.Variable unconstrained_a;
    ]
    None;
  let unconstrained_c = variable ~name:"C" Type.Variable.Unary.Unconstrained in
  expect_sequence_solution
    [
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, Type.Variable unconstrained_c;
      unconstrained_c, `Lower, child;
    ]
    (Some [unconstrained_a, child; unconstrained_b, child; unconstrained_c, child]);
  let unrelated = variable ~name:"unrelated" Type.Variable.Unary.Unconstrained in
  expect_sequence_solution
    [unconstrained_a, `Lower, Type.Variable unrelated]
    (Some [unconstrained_a, Type.Variable unrelated]);
  expect_sequence_solution
    [
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, Type.Variable unconstrained_a;
      unconstrained_c, `Lower, child;
    ]
    None;

  let parameters_a = Type.Variable.Variadic.Parameters.create "Ta" in
  let parameters_b = Type.Variable.Variadic.Parameters.create "Tb" in
  let empty_parameters = Type.Callable.Defined [] in
  (* A is greater than B, and B is greater than empty => both A and B solve to empty *)
  expect_sequence_solution
    ~callable_parameters_bounds:[
      parameters_a, `Lower, Type.Callable.ParameterVariadicTypeVariable parameters_b;
      parameters_b, `Lower, empty_parameters;
    ]
    []
    ~callable_parameters_solution:[parameters_a, empty_parameters; parameters_b, empty_parameters]
    (Some []);
  (* As with unaries, this trivial loop could be solvable, but we are choosing not to deal with
     this yet *)
  expect_sequence_solution
    ~callable_parameters_bounds:[
      parameters_a, `Lower, Type.Callable.ParameterVariadicTypeVariable parameters_b;
      parameters_b, `Lower, Type.Callable.ParameterVariadicTypeVariable parameters_a;
    ]
    []
    None;
  let parameters_with_unconstrained_a =
    Type.Callable.Defined
      [Type.Callable.Parameter.create ~annotation:(Type.Variable unconstrained_a) "x" 0]
  in
  let parameters_with_integer =
    Type.Callable.Defined [Type.Callable.Parameter.create ~annotation:(Type.integer) "x" 0]
  in
  (* A is greater than [T] and T is greater than int => T solves to int and A solves to [int].
     This is a test of unaries and parameter variadics getting along *)
  expect_sequence_solution
    ~callable_parameters_bounds:[
      parameters_a, `Lower, parameters_with_unconstrained_a
    ]
    [unconstrained_a, `Lower, Type.integer]
    ~callable_parameters_solution:[parameters_a, parameters_with_integer]
    (Some [unconstrained_a, Type.integer]);
  (* This is truly unsolvable, because A is supposed to be greater than [T], but T is supposed to
     be greater than typing.Callable[A, int]. *)
  expect_sequence_solution
    ~callable_parameters_bounds:[
      parameters_a, `Lower, parameters_with_unconstrained_a
    ]
    [
      unconstrained_a,
      `Lower,
      Type.Callable.create
        ~parameters:(Type.Callable.ParameterVariadicTypeVariable parameters_a)
        ~annotation:Type.integer
        ()
    ]
    None;
  ()


let test_partial_solution _ =
  let expect_split_solution
      ~bounds ~variables expected_partial_solution expected_remainder_solution =
    let partial_result, remainder_solution =
      List.fold bounds ~init:(Some empty) ~f:add_bound
      >>= DiamondOrderedConstraints.extract_partial_solution ~order:() ~variables
      >>| (fun (remainder, partial_solution) ->
          Some partial_solution, DiamondOrderedConstraints.solve ~order:() remainder)
      |> Option.value ~default:(None, None)
    in
    let parse expected =
      expected
      >>| List.map ~f:(fun (variable, solution) -> variable, solution)
      >>| TypeConstraints.Solution.create
    in
    let double_compare (left_first, left_second) (right_first, right_second) =
      optional_map_compare left_first right_first &&
      optional_map_compare left_second right_second
    in
    let double_print (first, second) =
      Printf.sprintf "%s ; %s" (optional_map_print first) (optional_map_print second)
    in
    assert_equal
      ~cmp:double_compare
      ~printer:double_print
      (parse expected_partial_solution, parse expected_remainder_solution)
      (partial_result, remainder_solution)
  in
  let unconstrained_a = variable ~name:"A" Type.Variable.Unary.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unary.Unconstrained in
  let unconstrained_c = variable ~name:"C" Type.Variable.Unary.Unconstrained in

  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:[
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, Type.Variable unconstrained_a;
    ]
    (Some [unconstrained_a, Type.Variable unconstrained_b])
    (Some []);

  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:[
      unconstrained_a, `Lower, Type.list (Type.Variable unconstrained_b);
      unconstrained_b, `Lower, Type.Variable unconstrained_a;
    ]
    (Some [unconstrained_a, Type.list (Type.Variable unconstrained_b)])
    None;

  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:[
      unconstrained_a, `Lower, Type.Variable unconstrained_b;
      unconstrained_b, `Lower, Type.Variable unconstrained_c;
      unconstrained_c, `Lower, Type.Variable unconstrained_b;
    ]
    (Some [unconstrained_a, Type.Variable unconstrained_b])
    None;
  ()


let test_exists _ =
  let order = () in
  let unconstrained_a = variable ~name:"A" Type.Variable.Unary.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unary.Unconstrained in
  let constraints_with_unconstrained_b =
    let pair = Type.Variable.UnaryPair (unconstrained_a, Type.Variable unconstrained_b) in
    DiamondOrderedConstraints.add_lower_bound
      TypeConstraints.empty
      ~order
      ~pair
    |> function | Some constraints -> constraints | None -> failwith "add bound failed"
  in
  assert_true
    (TypeConstraints.exists_in_bounds
       constraints_with_unconstrained_b
       ~variables:[Type.Variable.Unary unconstrained_b]);
  assert_false
    (TypeConstraints.exists_in_bounds
       constraints_with_unconstrained_b
       ~variables:[Type.Variable.Unary unconstrained_a]);
  ()


let () =
  "constraints">:::[
    "add_bound">::test_add_bound;
    "single_variable">::test_single_variable_solution;
    "multiple_variables">::test_multiple_variable_solution;
    "partial_solution">::test_partial_solution;
    "exists">::test_exists;
  ]
  |> Test.run;
