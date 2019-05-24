(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre


type interval = {
  upper_bound: Type.t;
  lower_bound: Type.t;
}
[@@deriving show]


module VariableMap = Type.Variable.Unary.Map


type t = interval VariableMap.t


let pp format constraints =
  let show ~key ~data sofar =
    Printf.sprintf "%s => %s" (Type.Variable.Unary.show key) (show_interval data) :: sofar
  in
  VariableMap.fold constraints ~init:[] ~f:show
  |> String.concat ~sep:"\n"
  |> Format.fprintf format "{%s}"


let show annotation =
  Format.asprintf "%a" pp annotation


let empty = VariableMap.empty


let exists constraints ~predicate =
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    Type.exists upper_bound ~predicate || Type.exists lower_bound ~predicate
  in
  VariableMap.exists constraints ~f:exists_in_interval_bounds


module Solution = struct
  type t = Type.t VariableMap.t

  let equal =
    VariableMap.equal Type.equal

  let show solution =
    let show_line ~key:{ Type.Variable.Unary.variable; _ } ~data accumulator =
      Format.sprintf "%s -> %s" variable (Type.show data) :: accumulator
    in
    VariableMap.fold solution ~init:[] ~f:show_line
    |> List.rev
    |> String.concat ~sep:"\n"
    |> Format.sprintf "{%s}"

  let empty =
    VariableMap.empty

  let instantiate solution =
    let constraints = function
      | Type.Variable variable ->
          VariableMap.find solution variable
      | _ ->
          None
    in
    Type.instantiate ~constraints ~widen:false

  let instantiate_single_variable =
    VariableMap.find

  let create =
    VariableMap.of_alist_exn
end



module type OrderedConstraintsType = sig
  type order
  val add_lower_bound
    :  t
    -> order: order
    -> variable: Type.Variable.Unary.t
    -> bound: Type.t
    -> t option
  val add_upper_bound
    :  t
    -> order: order
    -> variable: Type.Variable.Unary.t
    -> bound: Type.t
    -> t option
  val solve: t -> order: order -> Solution.t option
  val extract_partial_solution
    :  t
    -> order: order
    -> variables: Type.Variable.t list
    -> (t * Solution.t) option
end


module type OrderType = sig
  type t
  val less_or_equal: t -> left: Type.t -> right: Type.t -> bool
  val meet: t -> Type.t -> Type.t -> Type.t
  val join: t -> Type.t -> Type.t -> Type.t
end


module OrderedConstraints(Order: OrderType) = struct
  module Interval = struct
    let lower_bound { lower_bound; _ } = lower_bound
    let upper_bound { upper_bound; _ } = upper_bound

    let create ?(upper_bound = Type.Top) ?(lower_bound = Type.Bottom) () =
      { upper_bound; lower_bound }

    let join order left right =
      {
        upper_bound = Order.meet order left.upper_bound right.upper_bound;
        lower_bound = Order.join order left.lower_bound right.lower_bound;
      }

    (* Returns the lowest non-bottom value within the interval, such that it fulfills the
       requirements given by the exogenous constraint (from a type variable declaration) *)
    let actualize interval ~order ~exogenous_constraint =
      let lowest_non_bottom_member interval ~order =
        let non_empty { upper_bound; lower_bound } ~order =
          Order.less_or_equal order ~left:lower_bound ~right:upper_bound
        in
        Option.some_if (non_empty interval ~order) (lower_bound interval)
        >>| (function | Type.Bottom  -> upper_bound interval | other -> other)
      in
      match exogenous_constraint with
      | Type.Variable.Unary.Explicit explicits ->
          let explicits =
            match lower_bound interval with
            | Type.Variable { constraints = Explicit left_constraints; _ } ->
                let exists_in_explicits left_constraint =
                  List.exists explicits ~f:(Type.equal left_constraint)
                in
                if List.for_all left_constraints ~f:exists_in_explicits then
                  (* The only other thing that an explicit type variable can instantiate to is
                     another type variable with a subset of its values *)
                  (lower_bound interval) :: explicits
                else
                  explicits
            | _ ->
                explicits
          in
          let contains { upper_bound; lower_bound } candidate ~order =
            Order.less_or_equal order ~left:candidate ~right:upper_bound &&
            Order.less_or_equal order ~left:lower_bound ~right:candidate
          in
          (* When doing multiple solves, all of these options ought to be considered, *)
          (* and solved in a fixpoint *)
          List.find ~f:(contains interval ~order) explicits
      | Bound exogenous_bound ->
          join order interval (create ~upper_bound:exogenous_bound ())
          |> lowest_non_bottom_member ~order
      | Unconstrained ->
          lowest_non_bottom_member interval ~order
      | LiteralIntegers ->
          let is_literal_integer = function
            | Type.Literal Type.Integer _ -> true
            | Variable { constraints = LiteralIntegers; _ } -> true
            | _ -> false
          in
          let member = lowest_non_bottom_member interval ~order in
          match member with
          | Some found_member when is_literal_integer found_member -> member
          | Some (Type.Union union) when List.for_all union ~f:is_literal_integer -> member
          | _ -> None
  end

  type order = Order.t

  let add_bound constraints ~order ~variable ~bound ~is_lower_bound =
    if Type.equal bound (Type.Variable variable) then
      Some constraints
    else
      let new_constraint =
        if is_lower_bound then
          Interval.create ~lower_bound:bound ()
        else
          Interval.create ~upper_bound:bound ()
      in
      let existing =
        Map.find constraints variable
        |> Option.value ~default:(Interval.create ())
      in
      let joined = Interval.join order existing new_constraint in
      Interval.actualize joined ~order ~exogenous_constraint:variable.constraints
      >>| (fun _ -> Map.set constraints ~key:variable ~data:joined)

  let add_lower_bound =
    add_bound ~is_lower_bound:true

  let add_upper_bound =
    add_bound ~is_lower_bound:false

  let merge_solution constraints solution =
    let instantiate_interval ~key ~data:{upper_bound; lower_bound} =
      let smart_instantiate annotation =
        let instantiated = Solution.instantiate solution annotation in
        Option.some_if (not (Type.equal instantiated (Type.Variable key))) instantiated
      in
      let upper_bound = smart_instantiate upper_bound in
      let lower_bound = smart_instantiate lower_bound in
      Interval.create ?upper_bound ?lower_bound ()
    in
    let remove_trivial_constraints =
      let is_not_trivial = function
        | { upper_bound = Type.Top; lower_bound = Type.Bottom } -> false
        | _ -> true
      in
      VariableMap.filter ~f:is_not_trivial
    in
    VariableMap.mapi constraints ~f:instantiate_interval
    |> remove_trivial_constraints

  let solve constraints ~order =
    let rec build_solution ~remaining_constraints ~partial_solution =
      let independent_constraints, dependent_constraints =
        let is_independent { upper_bound; lower_bound } =
          let contains_key = function
            | Type.Variable variable ->
                VariableMap.mem remaining_constraints variable
            | _ ->
                false
          in
          (not (Type.exists upper_bound ~predicate:contains_key)) &&
          (not (Type.exists lower_bound ~predicate:contains_key))
        in
        VariableMap.partition_tf remaining_constraints ~f:is_independent
      in
      let handle_independent_constraint ~key:variable ~data:interval partial_solution =
        let add_actualized_interval partial_solution =
          let { Type.Variable.Unary.constraints = exogenous_constraint; _ } = variable in
          Interval.actualize interval ~order ~exogenous_constraint
          >>| (fun data -> VariableMap.set partial_solution ~key:variable ~data)
        in
        partial_solution
        >>= add_actualized_interval
      in
      let handle_dependent_constraints partial_solution =
        if VariableMap.is_empty dependent_constraints then
          Some partial_solution
        else if VariableMap.is_empty independent_constraints then
          None
        else
          let remaining_constraints = merge_solution dependent_constraints partial_solution in
          build_solution ~remaining_constraints ~partial_solution
      in
      Map.fold
        independent_constraints
        ~f:handle_independent_constraint
        ~init:(Some partial_solution)
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:VariableMap.empty

  let extract_partial_solution constraints ~order ~variables =
    let variables = List.map variables ~f:(function Type.Variable.Unary variable -> variable) in
    let extracted_constraints, remaining_constraints =
      let matches ~key ~data:_ =
        List.exists variables ~f:((=) key)
      in
      VariableMap.partitioni_tf constraints ~f:matches
    in
    solve extracted_constraints ~order
    >>| (fun solution -> merge_solution remaining_constraints solution, solution)
end
