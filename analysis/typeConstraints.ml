(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
module UnaryVariable = Type.Variable.Unary


type unary_interval = {
  upper_bound: Type.t;
  lower_bound: Type.t;
}
[@@deriving show]


module VariableMap = Type.Variable.Unary.Map

type t = {
  unaries: unary_interval UnaryVariable.Map.t;
}


let show_map map ~show_key ~show_data ~short_name =
  if Map.is_empty map then
    ""
  else
    let show ~key ~data sofar =
      Printf.sprintf "%s => %s" (show_key key) (show_data data) :: sofar
    in
    Map.fold map ~init:[] ~f:show
    |> String.concat ~sep:"\n"
    |> Format.sprintf "%s: [%s]" short_name


let pp format { unaries } =
  show_map
    unaries
    ~show_key:UnaryVariable.show
    ~show_data:show_unary_interval
    ~short_name:"un"
  |> Format.fprintf format "{%s}"


let show annotation =
  Format.asprintf "%a" pp annotation


let empty =
  {
    unaries = UnaryVariable.Map.empty;
  }


let exists_in_bounds { unaries } ~variables =
  let contains_variable annotation =
    Type.Variable.UnaryGlobalTransforms.collect_all annotation
    |> List.exists
      ~f:(fun variable ->
          List.mem variables (Type.Variable.Unary variable) ~equal:Type.Variable.equal)
  in
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    contains_variable upper_bound || contains_variable lower_bound
  in
  UnaryVariable.Map.exists unaries ~f:exists_in_interval_bounds



let is_empty { unaries } =
  UnaryVariable.Map.is_empty unaries


let contains_key { unaries } = function
  | Type.Variable.Unary unary ->
      Map.mem unaries unary


module Solution = struct
  type t = {
    unaries: Type.t UnaryVariable.Map.t;
  }

  let equal left right  =
    UnaryVariable.Map.equal Type.equal left.unaries right.unaries

  let show { unaries } =
    let unaries =
      show_map unaries ~show_key:UnaryVariable.show ~show_data:Type.show ~short_name:"un"
    in
    Format.sprintf "{%s}" unaries

  let empty =
    { unaries = UnaryVariable.Map.empty }

  let instantiate { unaries } annotation =
    Type.Variable.UnaryGlobalTransforms.replace_all
      (fun variable -> UnaryVariable.Map.find unaries variable)
      annotation

  let instantiate_single_variable { unaries; _ } =
    UnaryVariable.Map.find unaries

  let create unaries =
    { unaries = UnaryVariable.Map.of_alist_exn unaries }

  let set ({ unaries } as solution) = function
    | Type.Variable.UnaryPair (key, data) ->
        { solution with unaries = UnaryVariable.Map.set unaries ~key ~data }
end



module type OrderedConstraintsType = sig
  type order
  val add_lower_bound: t -> order: order -> pair: Type.Variable.pair -> t option
  val add_upper_bound: t -> order: order -> pair: Type.Variable.pair -> t option
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
  module IntervalContainer = struct
    module type Interval = sig
      module Variable: Type.Variable.VariableKind
      type t
      val create: ?upper_bound: Variable.domain -> ?lower_bound: Variable.domain -> unit -> t
      val intersection: t -> t -> order: Order.t -> t
      (* Returns the lowest non-bottom value within the interval, such that it fulfills the
         requirements potentially given in the variable *)
      val narrowest_valid_value
        :  t
        -> order: Order.t
        -> variable: Variable.t
        -> Variable.domain option
      val merge_solution_in: t -> variable: Variable.t -> solution: Solution.t -> t
      val is_trivial: t -> variable: Variable.t -> bool
      val free_variables: t -> Type.Variable.t list
    end

    module Make (Interval: Interval) = struct
      let add_bound container ~order ~variable ~bound ~is_lower_bound =
        if Interval.Variable.equal_domain bound (Interval.Variable.self_reference variable) then
          Some container
        else
          let new_constraint =
            if is_lower_bound then
              Interval.create ~lower_bound:bound ()
            else
              Interval.create ~upper_bound:bound ()
          in
          let existing =
            Map.find container variable
            |> Option.value ~default:(Interval.create ())
          in
          let intersection = Interval.intersection existing new_constraint ~order in
          Interval.narrowest_valid_value intersection ~order ~variable
          >>| (fun _ -> Map.set container ~key:variable ~data:intersection)

      let merge_solution container ~solution =
        Map.mapi
          container
          ~f:(fun ~key ~data -> Interval.merge_solution_in data ~variable:key ~solution)
        |> Map.filteri ~f:(fun ~key ~data -> not (Interval.is_trivial data ~variable:key))

      let partition_independent_dependent container ~with_regards_to =
        let is_independent target =
          Interval.free_variables target
          |> List.exists ~f:(contains_key with_regards_to)
          |> not
        in
        Map.partition_tf container ~f:is_independent

      let add_solution container partial_solution ~order =
        let add_solution ~key:variable ~data:target = function
          | Some partial_solution ->
              Interval.narrowest_valid_value target ~order ~variable
              >>| (fun value -> Interval.Variable.pair variable value)
              >>| Solution.set partial_solution
          | None ->
              None
        in
        Map.fold container ~f:add_solution ~init:(Some partial_solution)
    end
  end

  module UnaryTypeInterval = struct
    module Variable = UnaryVariable

    type t = unary_interval

    let lower_bound { lower_bound; _ } = lower_bound

    let upper_bound { upper_bound; _ } = upper_bound

    let create ?(upper_bound = Type.Top) ?(lower_bound = Type.Bottom) () =
      { upper_bound; lower_bound }

    let intersection left right ~order =
      {
        upper_bound = Order.meet order left.upper_bound right.upper_bound;
        lower_bound = Order.join order left.lower_bound right.lower_bound;
      }

    let narrowest_valid_value
        interval
        ~order
        ~variable:{ UnaryVariable.constraints = exogenous_constraint; _} =
      let lowest_non_bottom_member interval ~order =
        let non_empty { upper_bound; lower_bound } ~order =
          Order.less_or_equal order ~left:lower_bound ~right:upper_bound
        in
        Option.some_if (non_empty interval ~order) (lower_bound interval)
        >>| (function | Type.Bottom  -> upper_bound interval | other -> other)
      in
      match exogenous_constraint with
      | UnaryVariable.Explicit explicits ->
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
          intersection interval (create ~upper_bound:exogenous_bound ()) ~order
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

    let merge_solution_in { upper_bound; lower_bound } ~variable ~solution =
      let smart_instantiate annotation =
        let instantiated = Solution.instantiate solution annotation in
        Option.some_if (not (Type.equal instantiated (Type.Variable variable))) instantiated
      in
      let upper_bound = smart_instantiate upper_bound in
      let lower_bound = smart_instantiate lower_bound in
      create ?upper_bound ?lower_bound ()

    let is_trivial interval ~variable:_ =
      match interval with
      | { upper_bound = Type.Top; lower_bound = Type.Bottom } -> true
      | _ -> false

    let free_variables { upper_bound; lower_bound } =
      (Type.Variable.all_free_variables upper_bound) @
      (Type.Variable.all_free_variables lower_bound)
  end

  module UnaryIntervalContainer = IntervalContainer.Make(UnaryTypeInterval)

  type order = Order.t

  let add_bound ({ unaries } as constraints) ~order ~pair ~is_lower_bound =
    match pair with
    | Type.Variable.UnaryPair (variable, bound) ->
        UnaryIntervalContainer.add_bound unaries ~order ~variable ~bound ~is_lower_bound
        >>| (fun unaries -> { constraints with unaries })

  let add_lower_bound =
    add_bound ~is_lower_bound:true

  let add_upper_bound =
    add_bound ~is_lower_bound:false

  let merge_solution { unaries } solution =
    {
      unaries = UnaryIntervalContainer.merge_solution unaries ~solution;
    }

  let solve constraints ~order =
    let rec build_solution ~remaining_constraints ~partial_solution =
      let independent_constraints, dependent_constraints =
        let independent_unaries, dependent_unaries =
          UnaryIntervalContainer.partition_independent_dependent
            remaining_constraints.unaries
            ~with_regards_to:remaining_constraints
        in
        { unaries = independent_unaries },
        { unaries = dependent_unaries }
      in
      let handle_dependent_constraints partial_solution =
        if is_empty dependent_constraints then
          Some partial_solution
        else if is_empty independent_constraints then
          None
        else
          let remaining_constraints = merge_solution dependent_constraints partial_solution in
          build_solution ~remaining_constraints ~partial_solution
      in
      UnaryIntervalContainer.add_solution independent_constraints.unaries partial_solution ~order
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:Solution.empty

  let extract_partial_solution { unaries } ~order ~variables =
    let extracted_constraints, remaining_constraints =
      let unary_matches ~key ~data:_ =
        List.exists variables ~f:((=) (Type.Variable.Unary key))
      in
      let extracted_unaries, remaining_unaries =
        UnaryVariable.Map.partitioni_tf unaries ~f:unary_matches
      in
      { unaries = extracted_unaries },
      { unaries = remaining_unaries }
    in
    solve extracted_constraints ~order
    >>| (fun solution -> merge_solution remaining_constraints solution, solution)
end
