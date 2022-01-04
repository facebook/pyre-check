(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre
open Analysis
open TypeConstraints
open Test
open Type.Variable

let empty_head variable = { Type.Callable.head = []; variable }

let child = Type.Primitive "Child"

let left_parent = Type.Primitive "left_parent"

let right_parent = Type.Primitive "right_parent"

let grandparent = Type.Primitive "Grandparent"

module DiamondOrder = struct
  type t = unit

  let rec always_less_or_equal _ ~left ~right =
    match left, right with
    | _, _ when Type.equal left right -> true
    | _, Type.Top -> true
    | Type.Bottom, _ -> true
    | Type.Primitive "Child", Type.Primitive "left_parent" -> true
    | Type.Primitive "Child", Type.Primitive "right_parent" -> true
    | Type.Primitive "Child", Type.Primitive "Grandparent" -> true
    | Type.Primitive "left_parent", Type.Primitive "Grandparent" -> true
    | Type.Primitive "right_parent", Type.Primitive "Grandparent" -> true
    | left, Union rights ->
        List.exists rights ~f:(fun right -> always_less_or_equal () ~left ~right)
    | Tuple (Concrete left), Tuple (Concrete right) -> (
        match
          List.for_all2 left right ~f:(fun left right -> always_less_or_equal () ~left ~right)
        with
        | Ok result -> result
        | _ -> false)
    | _ -> false


  let meet _ left right =
    match left, right with
    | left, right when always_less_or_equal () ~left ~right -> left
    | right, left when always_less_or_equal () ~left ~right -> left
    | Type.Primitive "left_parent", Type.Primitive "right_parent" -> Type.Primitive "Child"
    | _ -> Type.Bottom


  let join _ left right =
    match left, right with
    | left, right when always_less_or_equal () ~left ~right -> right
    | right, left when always_less_or_equal () ~left ~right -> right
    | Type.Primitive "left_parent", Type.Primitive "right_parent" -> Type.Primitive "Grandparent"
    | _ -> Type.Top
end

module DiamondOrderedConstraints = OrderedConstraints (DiamondOrder)

let variable ?(name = "_V") constraints = Type.Variable.Unary.create name ~constraints

let add_bound constraints bound =
  let order = () in
  constraints
  >>= fun constraints ->
  match bound with
  | `Lower pair -> DiamondOrderedConstraints.add_lower_bound constraints ~order ~pair
  | `Upper pair -> DiamondOrderedConstraints.add_upper_bound constraints ~order ~pair
  | `Fallback variable ->
      DiamondOrderedConstraints.add_fallback_to_any constraints variable |> Option.some


let test_add_bound _ =
  let assert_add_bound_has_result ?(preconstraints = Some empty) bound ~expected_is_some =
    let result = add_bound preconstraints bound |> Option.is_some in
    assert_equal ~printer:(Printf.sprintf "%B") expected_is_some result
  in
  let assert_add_bound_succeeds = assert_add_bound_has_result ~expected_is_some:true in
  let assert_add_bound_fails = assert_add_bound_has_result ~expected_is_some:false in
  let unconstrained = variable Type.Variable.Unconstrained in
  assert_add_bound_succeeds (`Lower (UnaryPair (unconstrained, child)));
  assert_add_bound_fails
    ~preconstraints:(add_bound (Some empty) (`Lower (UnaryPair (unconstrained, left_parent))))
    (`Upper (UnaryPair (unconstrained, right_parent)));
  assert_add_bound_fails (`Lower (UnaryPair (variable (Type.Variable.Bound child), left_parent)));
  assert_add_bound_succeeds (`Lower (UnaryPair (variable (Type.Variable.Bound child), child)));
  assert_add_bound_succeeds (`Upper (UnaryPair (variable (Type.Variable.Bound child), left_parent)));
  let explicit_parent_a_parent_b = variable (Type.Variable.Explicit [left_parent; right_parent]) in
  assert_add_bound_succeeds (`Lower (UnaryPair (explicit_parent_a_parent_b, left_parent)));
  assert_add_bound_succeeds (`Lower (UnaryPair (explicit_parent_a_parent_b, right_parent)));
  assert_add_bound_succeeds
    (`Lower
      (UnaryPair
         ( explicit_parent_a_parent_b,
           Variable (Type.Variable.Unary.mark_as_bound explicit_parent_a_parent_b) )));
  assert_add_bound_succeeds
    (`Upper
      (UnaryPair
         ( explicit_parent_a_parent_b,
           Variable (Type.Variable.Unary.mark_as_bound explicit_parent_a_parent_b) )));
  assert_add_bound_fails
    ~preconstraints:
      (add_bound (Some empty) (`Lower (UnaryPair (explicit_parent_a_parent_b, left_parent))))
    (`Lower (UnaryPair (explicit_parent_a_parent_b, right_parent)));
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  (* Adding a constraint to a parameter variadic with no preconstraints should always work *)
  assert_add_bound_succeeds
    (`Lower (ParameterVariadicPair (parameter_variadic, Type.Callable.Defined [])));
  let preconstraints =
    add_bound
      (Some empty)
      (`Lower (ParameterVariadicPair (parameter_variadic, Type.Callable.Defined [])))
  in
  (* Adding the same constraint twice should be permitted *)
  assert_add_bound_succeeds
    ~preconstraints
    (`Lower (ParameterVariadicPair (parameter_variadic, Type.Callable.Defined [])));

  (* We currently always reject adding a different bound to something with a bound already *)
  assert_add_bound_fails
    ~preconstraints
    (`Lower
      (ParameterVariadicPair
         ( parameter_variadic,
           Type.Callable.Defined [Named { name = "x"; annotation = Type.integer; default = false }]
         )));

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let bound =
    TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string])
  in
  assert_add_bound_succeeds (`Lower bound);
  let preconstraints = add_bound (Some empty) (`Lower bound) in
  (* Adding an equal bound succeeds because the interval is the same as before. *)
  assert_add_bound_succeeds ~preconstraints (`Lower bound);
  assert_add_bound_fails
    ~preconstraints
    (`Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.bool; Type.bool])));
  ()


let optional_map_compare left right =
  match left, right with
  | Some left, Some right -> TypeConstraints.Solution.equal left right
  | None, None -> true
  | _, _ -> false


let optional_map_print map = map >>| TypeConstraints.Solution.show |> Option.value ~default:"None"

let assert_solution ~sequentially_applied_bounds expected =
  let result =
    List.fold sequentially_applied_bounds ~init:(Some empty) ~f:add_bound
    >>= DiamondOrderedConstraints.solve ~order:()
  in
  let expected = expected >>| TypeConstraints.Solution.create in
  assert_equal ~cmp:optional_map_compare ~printer:optional_map_print expected result


let test_single_variable_solution _ =
  assert_solution ~sequentially_applied_bounds:[] (Some []);
  let unconstrained = variable Type.Variable.Unconstrained in
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (unconstrained, child))]
    (Some [UnaryPair (unconstrained, child)]);

  (* Solving unconstrained to bottom would be sound as it fulfills the bound, but we want to
     eliminate bottoms whenever possible, so this should be fine *)
  assert_solution
    ~sequentially_applied_bounds:[`Upper (UnaryPair (unconstrained, child))]
    (Some [UnaryPair (unconstrained, child)]);
  assert_solution
    ~sequentially_applied_bounds:
      [`Lower (UnaryPair (unconstrained, child)); `Lower (UnaryPair (unconstrained, left_parent))]
    (Some [UnaryPair (unconstrained, left_parent)]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained, left_parent));
        `Lower (UnaryPair (unconstrained, right_parent));
      ]
    (Some [UnaryPair (unconstrained, grandparent)]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Upper (UnaryPair (unconstrained, left_parent));
        `Lower (UnaryPair (unconstrained, grandparent));
      ]
    None;
  assert_solution
    ~sequentially_applied_bounds:[`Upper (UnaryPair (unconstrained, Type.Variable unconstrained))]
    (Some []);
  assert_solution
    ~sequentially_applied_bounds:
      [`Upper (UnaryPair (unconstrained, Type.list (Type.Variable unconstrained)))]
    None;
  let bounded_by_parent_A = variable (Type.Variable.Bound left_parent) in
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (bounded_by_parent_A, child))]
    (Some [UnaryPair (bounded_by_parent_A, child)]);
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (bounded_by_parent_A, right_parent))]
    None;
  let explicit_int_string_parent_A =
    variable (Type.Variable.Explicit [Type.integer; Type.string; left_parent])
  in
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (explicit_int_string_parent_A, child))]
    (Some [UnaryPair (explicit_int_string_parent_A, left_parent)]);
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (explicit_int_string_parent_A, grandparent))]
    None;
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (UnaryPair
             ( explicit_int_string_parent_A,
               Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A) ));
      ]
    (Some
       [
         UnaryPair
           ( explicit_int_string_parent_A,
             Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A) );
       ]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Upper
          (UnaryPair
             ( explicit_int_string_parent_A,
               Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A) ));
      ]
    (Some
       [
         UnaryPair
           ( explicit_int_string_parent_A,
             Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A) );
       ]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (UnaryPair
             ( explicit_int_string_parent_A,
               Type.optional
                 (Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A)) ));
      ]
    None;
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Upper
          (UnaryPair
             ( explicit_int_string_parent_A,
               Type.optional
                 (Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A)) ));
      ]
    (Some
       [
         UnaryPair
           ( explicit_int_string_parent_A,
             Variable (Type.Variable.Unary.mark_as_bound explicit_int_string_parent_A) );
       ]);
  let parameter_variadic = Type.Variable.Variadic.Parameters.create "T" in
  let empty_parameters = Type.Callable.Defined [] in
  let one_named_parameter =
    Type.Callable.Defined [Named { name = "x"; annotation = Type.integer; default = false }]
  in
  (* The simplest case for parameter variadics: adding a single lower bound of empty parameters to a
     variable yields a solution of a replacement of that variable with empty parameters *)
  assert_solution
    ~sequentially_applied_bounds:
      [`Lower (ParameterVariadicPair (parameter_variadic, empty_parameters))]
    (Some [ParameterVariadicPair (parameter_variadic, empty_parameters)]);

  (* Attempting to bound a parameter variadic by more than one set of non-identical parameters fails *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (ParameterVariadicPair (parameter_variadic, empty_parameters));
        `Lower (ParameterVariadicPair (parameter_variadic, one_named_parameter));
      ]
    None;

  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string]));
      ]
    (Some [TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string])]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string]));
        `Lower
          (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string]));
      ]
    (Some [TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string])]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.string]));
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.bool]));
      ]
    None;
  (* We pick one of the provided concrete types, if the others are consistent with it. *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             (variadic, Type.OrderedTypes.Concrete [Type.Primitive "Child"; Type.Primitive "Child"]));
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concrete
                 [Type.Primitive "Grandparent"; Type.Primitive "Grandparent"] ));
      ]
    (Some
       [
         TupleVariadicPair
           ( variadic,
             Type.OrderedTypes.Concrete [Type.Primitive "Grandparent"; Type.Primitive "Grandparent"]
           );
       ]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concrete [Type.Primitive "Child"; Type.Primitive "Grandparent"] ));
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concrete [Type.Primitive "Grandparent"; Type.Primitive "Child"] ));
      ]
    None;
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.Primitive "Child"]));
        `Lower (TupleVariadicPair (variadic, Type.OrderedTypes.Concrete [Type.string]));
      ]
    None;
  ()


let test_multiple_variable_solution _ =
  let unconstrained_a = variable ~name:"A" Type.Variable.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unconstrained in
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, child));
      ]
    (Some [UnaryPair (unconstrained_a, child); UnaryPair (unconstrained_b, child)]);

  (* Could be solvable, choosing not to deal with this yet *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_a));
      ]
    None;
  let unconstrained_c = variable ~name:"C" Type.Variable.Unconstrained in
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_c));
        `Lower (UnaryPair (unconstrained_c, child));
      ]
    (Some
       [
         UnaryPair (unconstrained_a, child);
         UnaryPair (unconstrained_b, child);
         UnaryPair (unconstrained_c, child);
       ]);
  let unrelated = variable ~name:"unrelated" Type.Variable.Unconstrained in
  assert_solution
    ~sequentially_applied_bounds:[`Lower (UnaryPair (unconstrained_a, Type.Variable unrelated))]
    (Some [UnaryPair (unconstrained_a, Type.Variable unrelated)]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_a));
        `Lower (UnaryPair (unconstrained_c, child));
      ]
    None;
  let parameters_a = Type.Variable.Variadic.Parameters.create "Ta" in
  let parameters_b = Type.Variable.Variadic.Parameters.create "Tb" in
  let empty_parameters = Type.Callable.Defined [] in
  (* A is greater than B, and B is greater than empty => both A and B solve to empty *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (ParameterVariadicPair
             (parameters_a, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_b)));
        `Lower (ParameterVariadicPair (parameters_b, empty_parameters));
      ]
    (Some
       [
         ParameterVariadicPair (parameters_a, empty_parameters);
         ParameterVariadicPair (parameters_b, empty_parameters);
       ]);

  (* As with unaries, this trivial loop could be solvable, but we are choosing not to deal with this
     yet *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (ParameterVariadicPair
             (parameters_a, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_b)));
        `Lower
          (ParameterVariadicPair
             (parameters_b, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_a)));
      ]
    None;
  let parameters_with_unconstrained_a =
    Type.Callable.Defined
      [Named { name = "x"; annotation = Type.Variable unconstrained_a; default = false }]
  in
  let parameters_with_integer =
    Type.Callable.Defined [Named { name = "x"; annotation = Type.integer; default = false }]
  in
  (* A is greater than [T] and T is greater than int => T solves to int and A solves to [int]. This
     is a test of unaries and parameter variadics getting along *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (ParameterVariadicPair (parameters_a, parameters_with_unconstrained_a));
        `Lower (UnaryPair (unconstrained_a, Type.integer));
      ]
    (Some
       [
         ParameterVariadicPair (parameters_a, parameters_with_integer);
         UnaryPair (unconstrained_a, Type.integer);
       ]);

  (* This is truly unsolvable, because A is supposed to be greater than [T], but T is supposed to be
     greater than typing.Callable[A, int]. *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (ParameterVariadicPair (parameters_a, parameters_with_unconstrained_a));
        `Lower
          (UnaryPair
             ( unconstrained_a,
               Type.Callable.create
                 ~parameters:(Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_a))
                 ~annotation:Type.integer
                 () ));
      ]
    None;

  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Fallback (Unary unconstrained_b);
      ]
    (Some [UnaryPair (unconstrained_a, Type.Any); UnaryPair (unconstrained_b, Type.Any)]);

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create
                    ~prefix:[Type.integer]
                    ~suffix:[Type.string]
                    variadic2) ));
        `Lower (TupleVariadicPair (variadic2, Type.OrderedTypes.Concrete [Type.bool]));
      ]
    (Some
       [
         TupleVariadicPair
           (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.bool; Type.string]);
         TupleVariadicPair (variadic2, Type.OrderedTypes.Concrete [Type.bool]);
       ]);
  (* The constraint-solver doesn't solve for cycles, just like with unaries. *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic2) ));
        `Lower
          (TupleVariadicPair
             ( variadic2,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic) ));
      ]
    None;
  (* Test that unaries and variadic tuples work together. *)
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create
                    ~prefix:[Type.Variable unconstrained_a]
                    ~suffix:[Type.string]
                    variadic2) ));
        `Lower (TupleVariadicPair (variadic2, Type.OrderedTypes.Concrete [Type.bool]));
        `Lower (UnaryPair (unconstrained_a, Type.integer));
      ]
    (Some
       [
         UnaryPair (unconstrained_a, Type.integer);
         TupleVariadicPair
           (variadic, Type.OrderedTypes.Concrete [Type.integer; Type.bool; Type.string]);
         TupleVariadicPair (variadic2, Type.OrderedTypes.Concrete [Type.bool]);
       ]);
  assert_solution
    ~sequentially_applied_bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create
                    ~prefix:[Type.integer]
                    ~suffix:[Type.string]
                    variadic2) ));
        `Fallback (TupleVariadic variadic2);
      ]
    (Some
       [
         TupleVariadicPair
           ( variadic,
             Concatenation
               (Type.OrderedTypes.Concatenation.create_from_unbounded_element
                  ~prefix:[Type.integer]
                  ~suffix:[Type.string]
                  Type.Any) );
         TupleVariadicPair
           ( variadic2,
             Concatenation (Type.OrderedTypes.Concatenation.create_from_unbounded_element Type.Any)
           );
       ]);
  ()


let test_partial_solution _ =
  let expect_split_solution ~bounds ~variables expected_partial_solution expected_remainder_solution
    =
    let partial_result, remainder_solution =
      List.fold bounds ~init:(Some empty) ~f:add_bound
      >>= DiamondOrderedConstraints.extract_partial_solution ~order:() ~variables
      >>| (fun (remainder, partial_solution) ->
            Some partial_solution, DiamondOrderedConstraints.solve ~order:() remainder)
      |> Option.value ~default:(None, None)
    in
    let parse expected = expected >>| TypeConstraints.Solution.create in
    let double_compare (left_first, left_second) (right_first, right_second) =
      optional_map_compare left_first right_first && optional_map_compare left_second right_second
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
  let unconstrained_a = variable ~name:"A" Type.Variable.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unconstrained in
  let unconstrained_c = variable ~name:"C" Type.Variable.Unconstrained in
  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_a));
      ]
    (Some [UnaryPair (unconstrained_a, Type.Variable unconstrained_b)])
    (Some []);
  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.list (Type.Variable unconstrained_b)));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_a));
      ]
    (Some [UnaryPair (unconstrained_a, Type.list (Type.Variable unconstrained_b))])
    None;
  expect_split_solution
    ~variables:[Type.Variable.Unary unconstrained_a]
    ~bounds:
      [
        `Lower (UnaryPair (unconstrained_a, Type.Variable unconstrained_b));
        `Lower (UnaryPair (unconstrained_b, Type.Variable unconstrained_c));
        `Lower (UnaryPair (unconstrained_c, Type.Variable unconstrained_b));
      ]
    (Some [UnaryPair (unconstrained_a, Type.Variable unconstrained_b)])
    None;
  let parameters_a = Type.Variable.Variadic.Parameters.create "Ta" in
  let parameters_b = Type.Variable.Variadic.Parameters.create "Tb" in
  expect_split_solution
    ~variables:[Type.Variable.ParameterVariadic parameters_a]
    ~bounds:
      [
        `Lower
          (ParameterVariadicPair
             (parameters_a, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_b)));
        `Lower
          (ParameterVariadicPair
             (parameters_b, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_a)));
      ]
    (Some
       [
         ParameterVariadicPair
           (parameters_a, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_b));
       ])
    (Some []);

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  (* Ts <: Ts2 and Ts2 <: Ts. Solve for Ts. *)
  expect_split_solution
    ~variables:[Type.Variable.TupleVariadic variadic]
    ~bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic2) ));
        `Lower
          (TupleVariadicPair
             ( variadic2,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic) ));
      ]
    (Some
       [
         TupleVariadicPair
           ( variadic,
             Type.OrderedTypes.Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic2) );
       ])
    (* This is a trivial solution because the remaining constraint just becomes `Ts2 <: Ts2`. *)
    (Some []);
  expect_split_solution
    ~variables:[Type.Variable.TupleVariadic variadic]
    ~bounds:
      [
        `Lower
          (TupleVariadicPair
             ( variadic,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic2) ));
        `Lower
          (TupleVariadicPair
             ( variadic2,
               Type.OrderedTypes.Concatenation
                 (Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] ~suffix:[] variadic)
             ));
      ]
    (Some
       [
         TupleVariadicPair
           ( variadic,
             Type.OrderedTypes.Concatenation
               (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic2) );
       ])
    (* This fails because the remaining constraint becomes `[*Ts2] <: [int, *Ts2]`. *)
    None;
  ()


let test_exists _ =
  let order = () in
  let unconstrained_a = variable ~name:"A" Type.Variable.Unconstrained in
  let unconstrained_b = variable ~name:"B" Type.Variable.Unconstrained in
  let constraints_with_unconstrained_b =
    let pair = Type.Variable.UnaryPair (unconstrained_a, Type.Variable unconstrained_b) in
    DiamondOrderedConstraints.add_lower_bound TypeConstraints.empty ~order ~pair
    |> function
    | Some constraints -> constraints
    | None -> failwith "add bound failed"
  in
  assert_true
    (TypeConstraints.exists_in_bounds
       constraints_with_unconstrained_b
       ~variables:[Type.Variable.Unary unconstrained_b]);
  assert_false
    (TypeConstraints.exists_in_bounds
       constraints_with_unconstrained_b
       ~variables:[Type.Variable.Unary unconstrained_a]);
  let parameters_a = Type.Variable.Variadic.Parameters.create "Ta" in
  let parameters_b = Type.Variable.Variadic.Parameters.create "Tb" in
  let constraints_with_parameters_b =
    let pair =
      Type.Variable.ParameterVariadicPair
        (parameters_a, Type.Callable.ParameterVariadicTypeVariable (empty_head parameters_b))
    in
    DiamondOrderedConstraints.add_lower_bound TypeConstraints.empty ~order ~pair
    |> fun constraints_option -> Option.value_exn constraints_option
  in
  assert_true
    (TypeConstraints.exists_in_bounds
       constraints_with_parameters_b
       ~variables:[Type.Variable.ParameterVariadic parameters_b]);
  assert_false
    (TypeConstraints.exists_in_bounds
       constraints_with_parameters_b
       ~variables:[Type.Variable.ParameterVariadic parameters_a]);

  (* Variadic tuples. *)
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "Ts2" in
  let constraints_with_variadic2_in_bounds =
    let pair =
      TupleVariadicPair
        ( variadic,
          Type.OrderedTypes.Concatenation
            (Type.OrderedTypes.Concatenation.create
               ~prefix:[Type.Variable unconstrained_a]
               ~suffix:[Type.string]
               variadic2) )
    in
    DiamondOrderedConstraints.add_lower_bound TypeConstraints.empty ~order ~pair
    |> fun constraints_option -> Option.value_exn constraints_option
  in
  assert_true
    (TypeConstraints.exists_in_bounds
       constraints_with_variadic2_in_bounds
       ~variables:[Type.Variable.TupleVariadic variadic2]);
  assert_false
    (TypeConstraints.exists_in_bounds
       constraints_with_variadic2_in_bounds
       ~variables:[Type.Variable.TupleVariadic variadic]);
  assert_true
    (TypeConstraints.exists_in_bounds
       constraints_with_variadic2_in_bounds
       ~variables:[Type.Variable.Unary unconstrained_a]);
  ()


let () =
  "constraints"
  >::: [
         "add_bound" >:: test_add_bound;
         "single_variable" >:: test_single_variable_solution;
         "multiple_variables" >:: test_multiple_variable_solution;
         "partial_solution" >:: test_partial_solution;
         "exists" >:: test_exists;
       ]
  |> Test.run
