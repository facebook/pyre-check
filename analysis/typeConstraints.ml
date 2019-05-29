(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
module ParameterVariable = Type.Variable.Variadic.Parameters
module UnaryVariable = Type.Variable.Unary

type unary_interval = {
  upper_bound: Type.t;
  lower_bound: Type.t
}
[@@deriving show]

type callable_parameter_interval =
  | Top
  | Singleton of Type.Callable.parameters
  | Bottom
[@@deriving show]

type t = {
  unaries: unary_interval UnaryVariable.Map.t;
  callable_parameters: callable_parameter_interval ParameterVariable.Map.t
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


let pp format { unaries; callable_parameters } =
  let unaries =
    show_map unaries ~show_key:UnaryVariable.show ~show_data:show_unary_interval ~short_name:"un"
  in
  let callable_parameters =
    show_map
      callable_parameters
      ~show_key:ParameterVariable.show
      ~show_data:show_callable_parameter_interval
      ~short_name:"cb"
  in
  Format.fprintf format "{%s%s}" unaries callable_parameters


let show annotation = Format.asprintf "%a" pp annotation

let empty =
  { unaries = UnaryVariable.Map.empty; callable_parameters = ParameterVariable.Map.empty }


let exists_in_bounds { unaries; callable_parameters } ~variables =
  let contains_variable annotation =
    let contains_unary =
      Type.Variable.UnaryGlobalTransforms.collect_all annotation
      |> List.exists ~f:(fun variable ->
             List.mem variables (Type.Variable.Unary variable) ~equal:Type.Variable.equal)
    in
    let contains_parameter_variadic =
      let parameter_variadic_contained_in_list variable =
        List.mem variables (Type.Variable.ParameterVariadic variable) ~equal:Type.Variable.equal
      in
      Type.Variable.ParameterVariadicGlobalTransforms.collect_all annotation
      |> List.exists ~f:parameter_variadic_contained_in_list
    in
    contains_unary || contains_parameter_variadic
  in
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    contains_variable upper_bound || contains_variable lower_bound
  in
  let exists_in_callable_parameter_interval_bounds = function
    | Singleton parameters ->
        Type.Callable.create ~parameters ~annotation:Type.Any () |> contains_variable
    | _ -> false
  in
  UnaryVariable.Map.exists unaries ~f:exists_in_interval_bounds
  || ParameterVariable.Map.exists
       callable_parameters
       ~f:exists_in_callable_parameter_interval_bounds


let is_empty { unaries; callable_parameters } =
  UnaryVariable.Map.is_empty unaries && ParameterVariable.Map.is_empty callable_parameters


let contains_key { unaries; callable_parameters } = function
  | Type.Variable.Unary unary -> Map.mem unaries unary
  | Type.Variable.ParameterVariadic parameters -> Map.mem callable_parameters parameters


module Solution = struct
  type t = {
    unaries: Type.t UnaryVariable.Map.t;
    callable_parameters: Type.Callable.parameters ParameterVariable.Map.t
  }

  let equal left right =
    UnaryVariable.Map.equal Type.equal left.unaries right.unaries
    && ParameterVariable.Map.equal
         Type.Callable.equal_parameters
         left.callable_parameters
         right.callable_parameters


  let show { unaries; callable_parameters } =
    let unaries =
      show_map unaries ~show_key:UnaryVariable.show ~show_data:Type.show ~short_name:"un"
    in
    let callable_parameters =
      show_map
        callable_parameters
        ~show_key:ParameterVariable.show
        ~show_data:Type.Callable.show_parameters
        ~short_name:"cb"
    in
    Format.sprintf "{%s%s}" unaries callable_parameters


  let empty =
    { unaries = UnaryVariable.Map.empty; callable_parameters = ParameterVariable.Map.empty }


  let instantiate { unaries; callable_parameters } annotation =
    Type.Variable.UnaryGlobalTransforms.replace_all
      (fun variable -> UnaryVariable.Map.find unaries variable)
      annotation
    |> Type.Variable.ParameterVariadicGlobalTransforms.replace_all (fun variable ->
           ParameterVariable.Map.find callable_parameters variable)


  let instantiate_single_variable { unaries; _ } = UnaryVariable.Map.find unaries

  let set ({ unaries; callable_parameters } as solution) = function
    | Type.Variable.UnaryPair (key, data) ->
        { solution with unaries = UnaryVariable.Map.set unaries ~key ~data }
    | Type.Variable.ParameterVariadicPair (key, data) ->
        { solution with
          callable_parameters = ParameterVariable.Map.set callable_parameters ~key ~data
        }


  let create = List.fold ~f:set ~init:empty
end

module type OrderedConstraintsType = sig
  type order

  val add_lower_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val add_upper_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val solve : t -> order:order -> Solution.t option

  val extract_partial_solution
    :  t ->
    order:order ->
    variables:Type.Variable.t list ->
    (t * Solution.t) option
end

module type OrderType = sig
  type t

  val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val meet : t -> Type.t -> Type.t -> Type.t

  val join : t -> Type.t -> Type.t -> Type.t
end

module OrderedConstraints (Order : OrderType) = struct
  module IntervalContainer = struct
    module type Interval = sig
      module Variable : Type.Variable.VariableKind

      type t

      val create : ?upper_bound:Variable.domain -> ?lower_bound:Variable.domain -> unit -> t

      val intersection : t -> t -> order:Order.t -> t

      (* Returns the lowest non-bottom value within the interval, such that it fulfills the
         requirements potentially given in the variable *)
      val narrowest_valid_value
        :  t ->
        order:Order.t ->
        variable:Variable.t ->
        Variable.domain option

      val merge_solution_in : t -> variable:Variable.t -> solution:Solution.t -> t

      val is_trivial : t -> variable:Variable.t -> bool

      val free_variables : t -> Type.Variable.t list
    end

    module Make (Interval : Interval) = struct
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
            Map.find container variable |> Option.value ~default:(Interval.create ())
          in
          let intersection = Interval.intersection existing new_constraint ~order in
          Interval.narrowest_valid_value intersection ~order ~variable
          >>| fun _ -> Map.set container ~key:variable ~data:intersection


      let merge_solution container ~solution =
        Map.mapi container ~f:(fun ~key ~data ->
            Interval.merge_solution_in data ~variable:key ~solution)
        |> Map.filteri ~f:(fun ~key ~data -> not (Interval.is_trivial data ~variable:key))


      let partition_independent_dependent container ~with_regards_to =
        let is_independent target =
          Interval.free_variables target |> List.exists ~f:(contains_key with_regards_to) |> not
        in
        Map.partition_tf container ~f:is_independent


      let add_solution container partial_solution ~order =
        let add_solution ~key:variable ~data:target = function
          | Some partial_solution ->
              Interval.narrowest_valid_value target ~order ~variable
              >>| (fun value -> Interval.Variable.pair variable value)
              >>| Solution.set partial_solution
          | None -> None
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
      { upper_bound = Order.meet order left.upper_bound right.upper_bound;
        lower_bound = Order.join order left.lower_bound right.lower_bound
      }


    let narrowest_valid_value
        interval
        ~order
        ~variable:{ UnaryVariable.constraints = exogenous_constraint; _ }
      =
      let lowest_non_bottom_member interval ~order =
        let non_empty { upper_bound; lower_bound } ~order =
          Order.less_or_equal order ~left:lower_bound ~right:upper_bound
        in
        Option.some_if (non_empty interval ~order) (lower_bound interval)
        >>| function
        | Type.Bottom -> upper_bound interval
        | other -> other
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
                  lower_bound interval :: explicits
                else
                  explicits
            | _ -> explicits
          in
          let contains { upper_bound; lower_bound } candidate ~order =
            Order.less_or_equal order ~left:candidate ~right:upper_bound
            && Order.less_or_equal order ~left:lower_bound ~right:candidate
          in
          (* When doing multiple solves, all of these options ought to be considered, *)
          (* and solved in a fixpoint *)
          List.find ~f:(contains interval ~order) explicits
      | Bound exogenous_bound ->
          intersection interval (create ~upper_bound:exogenous_bound ()) ~order
          |> lowest_non_bottom_member ~order
      | Unconstrained -> lowest_non_bottom_member interval ~order
      | LiteralIntegers -> (
          let is_literal_integer = function
            | Type.Literal (Type.Integer _) -> true
            | Variable { constraints = LiteralIntegers; _ } -> true
            | _ -> false
          in
          let member = lowest_non_bottom_member interval ~order in
          match member with
          | Some found_member when is_literal_integer found_member -> member
          | Some (Type.Union union) when List.for_all union ~f:is_literal_integer -> member
          | _ -> None )


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
      Type.Variable.all_free_variables upper_bound @ Type.Variable.all_free_variables lower_bound
  end

  module CallableParametersInterval = struct
    module Variable = ParameterVariable

    type t = callable_parameter_interval

    let create ?upper_bound ?lower_bound () =
      match upper_bound, lower_bound with
      | None, None -> Bottom
      | Some only, None
      | None, Some only ->
          Singleton only
      | Some left, Some right when Type.Callable.equal_parameters left right -> Singleton left
      | Some _, Some _ -> Top


    let narrowest_valid_value interval ~order:_ ~variable:_ =
      match interval with
      | Top
      | Bottom ->
          None
      | Singleton parameters -> Some parameters


    let intersection left right ~order:_ =
      match left, right with
      | Top, _
      | _, Top ->
          Top
      | other, Bottom
      | Bottom, other ->
          other
      | Singleton left, Singleton right when Type.Callable.equal_parameters left right ->
          Singleton left
      | _, _ -> Top


    let merge_solution_in target ~variable:_ ~solution =
      match target with
      | Top
      | Bottom ->
          target
      | Singleton parameters -> (
          let callable = Type.Callable.create ~parameters ~annotation:Type.Any () in
          match Solution.instantiate solution callable with
          | Type.Callable { implementation = { parameters = instantiated_parameters; _ }; _ } ->
              Singleton instantiated_parameters
          | _ -> failwith "impossible" )


    let is_trivial interval ~variable =
      match interval with
      | Singleton (Type.Callable.ParameterVariadicTypeVariable target_variable) ->
          ParameterVariable.equal variable target_variable
      | _ -> false


    let free_variables = function
      | Top
      | Bottom ->
          []
      | Singleton parameters ->
          Type.Callable.create ~parameters ~annotation:Type.Any ()
          |> Type.Variable.all_free_variables
  end

  module CallableParametersIntervalContainer = IntervalContainer.Make (CallableParametersInterval)
  module UnaryIntervalContainer = IntervalContainer.Make (UnaryTypeInterval)

  type order = Order.t

  let add_bound ({ unaries; callable_parameters } as constraints) ~order ~pair ~is_lower_bound =
    match pair with
    | Type.Variable.UnaryPair (variable, bound) ->
        UnaryIntervalContainer.add_bound unaries ~order ~variable ~bound ~is_lower_bound
        >>| fun unaries -> { constraints with unaries }
    | Type.Variable.ParameterVariadicPair (variable, bound) ->
        CallableParametersIntervalContainer.add_bound
          callable_parameters
          ~order
          ~variable
          ~bound
          ~is_lower_bound
        >>| fun callable_parameters -> { constraints with callable_parameters }


  let add_lower_bound = add_bound ~is_lower_bound:true

  let add_upper_bound = add_bound ~is_lower_bound:false

  let merge_solution { unaries; callable_parameters } solution =
    { unaries = UnaryIntervalContainer.merge_solution unaries ~solution;
      callable_parameters =
        CallableParametersIntervalContainer.merge_solution callable_parameters ~solution
    }


  let solve constraints ~order =
    let rec build_solution ~remaining_constraints ~partial_solution =
      let independent_constraints, dependent_constraints =
        let independent_unaries, dependent_unaries =
          UnaryIntervalContainer.partition_independent_dependent
            remaining_constraints.unaries
            ~with_regards_to:remaining_constraints
        in
        let independent_parameters, dependent_parameters =
          CallableParametersIntervalContainer.partition_independent_dependent
            remaining_constraints.callable_parameters
            ~with_regards_to:remaining_constraints
        in
        ( { unaries = independent_unaries; callable_parameters = independent_parameters },
          { unaries = dependent_unaries; callable_parameters = dependent_parameters } )
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
      >>= CallableParametersIntervalContainer.add_solution
            independent_constraints.callable_parameters
            ~order
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:Solution.empty


  let extract_partial_solution { unaries; callable_parameters } ~order ~variables =
    let extracted_constraints, remaining_constraints =
      let unary_matches ~key ~data:_ =
        List.exists variables ~f:(( = ) (Type.Variable.Unary key))
      in
      let callable_parameters_matches ~key ~data:_ =
        List.exists variables ~f:(( = ) (Type.Variable.ParameterVariadic key))
      in
      let extracted_unaries, remaining_unaries =
        UnaryVariable.Map.partitioni_tf unaries ~f:unary_matches
      in
      let extracted_variadics, remaining_variadics =
        ParameterVariable.Map.partitioni_tf callable_parameters ~f:callable_parameters_matches
      in
      ( { unaries = extracted_unaries; callable_parameters = extracted_variadics },
        { unaries = remaining_unaries; callable_parameters = remaining_variadics } )
    in
    solve extracted_constraints ~order
    >>| fun solution -> merge_solution remaining_constraints solution, solution
end
