(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module ParameterVariable = Type.Variable.Variadic.Parameters
module UnaryVariable = Type.Variable.Unary
module TupleVariable = Type.Variable.Variadic.Tuple

type unary_interval = {
  upper_bound: Type.t;
  lower_bound: Type.t;
}
[@@deriving compare, show]

type callable_parameter_interval =
  | Top
  | Singleton of Type.Callable.parameters
  | Bottom
[@@deriving compare, show]

(* A variadic tuple Ts must solve to a concrete tuple of known length and dimensions.

   For example:

   def expects_same_types(xs: Tuple[*Ts], ys: Tuple[*Ts]) -> Tuple[*Ts]: ...

   expects_same_types((1, 2), ("hello", "world"))

   This call should raise an error. It should not solve `[int, int] <: *Ts && [str, str] <: *Ts` to
   get `Ts = Tuple[int | str, int | str]`. Otherwise, it would be unintuitive because users would
   expect the two arguments to have the same type.

   Similarly, we should not join tuples of different lengths. That is, `[int, int] <: *Ts && [str]
   <: *Ts` should not be `Ts = Tuple[int, int] | Tuple[str]`. The same goes for unequal lower and
   upper bound.

   In other words, the type lattice is just Top, Bottom, and individual tuples. The `join` of
   unequal types is Top and the `meet` of unequal types is Bottom. *)
type tuple_interval =
  | TopTuple
  | BottomTuple
  | SingletonTuple of Type.t Type.OrderedTypes.record
[@@deriving compare, show]

type t = {
  unaries: unary_interval UnaryVariable.Map.t;
  callable_parameters: callable_parameter_interval ParameterVariable.Map.t;
  tuple_variadics: tuple_interval TupleVariable.Map.t;
  have_fallbacks: Type.Variable.Set.t;
}
[@@deriving compare]

let show_map map ~show_key ~show_data ~short_name =
  if Map.is_empty map then
    ""
  else
    let show ~key ~data sofar =
      Printf.sprintf "%s => %s" (show_key key) (show_data data) :: sofar
    in
    Map.fold map ~init:[] ~f:show |> String.concat ~sep:"\n" |> Format.sprintf "%s: [%s]" short_name


let pp format { unaries; callable_parameters; tuple_variadics; have_fallbacks } =
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
  let tuple_variadics =
    show_map
      tuple_variadics
      ~show_key:TupleVariable.show
      ~show_data:show_tuple_interval
      ~short_name:"variadic_tuple"
  in
  let have_fallbacks =
    Set.to_list have_fallbacks
    |> List.to_string ~f:Type.Variable.show
    |> Format.sprintf "\nHave Fallbacks to Any: %s"
  in
  Format.fprintf format "{%s%s%s%s}" unaries callable_parameters tuple_variadics have_fallbacks


let show annotation = Format.asprintf "%a" pp annotation

let empty =
  {
    unaries = UnaryVariable.Map.empty;
    callable_parameters = ParameterVariable.Map.empty;
    tuple_variadics = TupleVariable.Map.empty;
    have_fallbacks = Type.Variable.Set.empty;
  }


let exists_in_bounds { unaries; callable_parameters; tuple_variadics; _ } ~variables =
  let contains_variable annotation =
    let contains_unary =
      Type.Variable.GlobalTransforms.Unary.collect_all annotation
      |> List.exists ~f:(fun variable ->
             List.mem variables (Type.Variable.Unary variable) ~equal:Type.Variable.equal)
    in
    let contains_parameter_variadic =
      let parameter_variadic_contained_in_list variable =
        List.mem variables (Type.Variable.ParameterVariadic variable) ~equal:Type.Variable.equal
      in
      Type.Variable.GlobalTransforms.ParameterVariadic.collect_all annotation
      |> List.exists ~f:parameter_variadic_contained_in_list
    in
    let contains_tuple_variadic =
      Type.Variable.GlobalTransforms.TupleVariadic.collect_all annotation
      |> List.exists ~f:(fun variable ->
             List.mem variables (Type.Variable.TupleVariadic variable) ~equal:Type.Variable.equal)
    in
    contains_unary || contains_parameter_variadic || contains_tuple_variadic
  in
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    contains_variable upper_bound || contains_variable lower_bound
  in
  let exists_in_callable_parameter_interval_bounds = function
    | Singleton parameters ->
        Type.Callable.create ~parameters ~annotation:Type.Any () |> contains_variable
    | _ -> false
  in
  let exists_in_tuple_interval_bound = function
    | SingletonTuple ordered_type -> Type.Tuple ordered_type |> contains_variable
    | _ -> false
  in
  UnaryVariable.Map.exists unaries ~f:exists_in_interval_bounds
  || ParameterVariable.Map.exists
       callable_parameters
       ~f:exists_in_callable_parameter_interval_bounds
  || TupleVariable.Map.exists tuple_variadics ~f:exists_in_tuple_interval_bound


module Solution = struct
  type t = {
    unaries: Type.t UnaryVariable.Map.t;
    callable_parameters: Type.Callable.parameters ParameterVariable.Map.t;
    tuple_variadics: Type.t Type.OrderedTypes.record TupleVariable.Map.t;
  }

  let equal left right =
    UnaryVariable.Map.equal Type.equal left.unaries right.unaries
    && ParameterVariable.Map.equal
         Type.Callable.equal_parameters
         left.callable_parameters
         right.callable_parameters
    && TupleVariable.Map.equal
         (Type.OrderedTypes.equal_record Type.equal)
         left.tuple_variadics
         right.tuple_variadics


  let show { unaries; callable_parameters; tuple_variadics } =
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
    let tuple_variadics =
      show_map
        tuple_variadics
        ~show_key:TupleVariable.show
        ~show_data:(Type.OrderedTypes.show_record Type.pp)
        ~short_name:"variadic_tuple"
    in
    Format.sprintf "{%s%s%s}" unaries callable_parameters tuple_variadics


  let pp format solution = Format.fprintf format "%s" (show solution)

  let empty =
    {
      unaries = UnaryVariable.Map.empty;
      callable_parameters = ParameterVariable.Map.empty;
      tuple_variadics = TupleVariable.Map.empty;
    }


  let instantiate { unaries; callable_parameters; tuple_variadics } annotation =
    let annotation =
      if UnaryVariable.Map.is_empty unaries then
        annotation
      else
        Type.Variable.GlobalTransforms.Unary.replace_all
          (fun variable -> UnaryVariable.Map.find unaries variable)
          annotation
    in
    let annotation =
      if ParameterVariable.Map.is_empty callable_parameters then
        annotation
      else
        Type.Variable.GlobalTransforms.ParameterVariadic.replace_all
          (fun variable -> ParameterVariable.Map.find callable_parameters variable)
          annotation
    in
    let annotation =
      if TupleVariable.Map.is_empty tuple_variadics then
        annotation
      else
        Type.Variable.GlobalTransforms.TupleVariadic.replace_all
          (fun variable -> TupleVariable.Map.find tuple_variadics variable)
          annotation
    in
    annotation


  let instantiate_single_variable { unaries; _ } = UnaryVariable.Map.find unaries

  let instantiate_single_parameter_variadic { callable_parameters; _ } =
    ParameterVariable.Map.find callable_parameters


  let instantiate_ordered_types solution ordered_type =
    match instantiate solution (Type.Tuple ordered_type) with
    | Type.Tuple instantiated_ordered_type -> instantiated_ordered_type
    | _ -> failwith "expected Tuple"


  let instantiate_callable_parameters solution parameters =
    match instantiate solution (Type.Callable.create ~parameters ~annotation:Type.Any ()) with
    | Type.Callable { implementation = { parameters; _ }; _ } -> parameters
    | _ -> failwith "instantiate is not preserving callables"


  let set ({ unaries; callable_parameters; tuple_variadics } as solution) = function
    | Type.Variable.UnaryPair (key, data) ->
        { solution with unaries = UnaryVariable.Map.set unaries ~key ~data }
    | Type.Variable.ParameterVariadicPair (key, data) ->
        {
          solution with
          callable_parameters = ParameterVariable.Map.set callable_parameters ~key ~data;
        }
    | Type.Variable.TupleVariadicPair (key, data) ->
        { solution with tuple_variadics = TupleVariable.Map.set tuple_variadics ~key ~data }


  let create = List.fold ~f:set ~init:empty
end

module type OrderedConstraintsType = sig
  type order

  val add_lower_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val add_upper_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val add_fallback_to_any : t -> Type.Variable.t -> t

  val solve : t -> order:order -> Solution.t option

  val extract_partial_solution
    :  t ->
    order:order ->
    variables:Type.Variable.t list ->
    (t * Solution.t) option
end

module type OrderType = sig
  type t

  val always_less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val meet : t -> Type.t -> Type.t -> Type.t

  val join : t -> Type.t -> Type.t -> Type.t
end

module OrderedConstraints (Order : OrderType) = struct
  module IntervalContainer = struct
    module type Interval = sig
      module Variable : Type.Variable.VariableKind

      type t

      val create : ?upper_bound:Variable.domain -> ?lower_bound:Variable.domain -> unit -> t

      val intersection : t -> t -> order:Order.t -> t option

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
          Interval.intersection existing new_constraint ~order
          >>= fun intersection ->
          Interval.narrowest_valid_value intersection ~order ~variable
          >>| fun _ -> Map.set container ~key:variable ~data:intersection


      let merge_solution container ~solution =
        Map.mapi container ~f:(fun ~key ~data ->
            Interval.merge_solution_in data ~variable:key ~solution)
        |> Map.filteri ~f:(fun ~key ~data -> not (Interval.is_trivial data ~variable:key))


      let partition_independent_dependent container ~with_regards_to =
        let contains_key { unaries; callable_parameters; tuple_variadics; have_fallbacks } key =
          let has_constraints =
            match key with
            | Type.Variable.Unary unary -> Map.mem unaries unary
            | Type.Variable.ParameterVariadic parameters -> Map.mem callable_parameters parameters
            | Type.Variable.TupleVariadic variadic -> Map.mem tuple_variadics variadic
          in
          has_constraints || Set.mem have_fallbacks key
        in
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
      Some
        {
          upper_bound = Order.meet order left.upper_bound right.upper_bound;
          lower_bound = Order.join order left.lower_bound right.lower_bound;
        }


    let narrowest_valid_value
        interval
        ~order
        ~variable:{ UnaryVariable.constraints = exogenous_constraint; _ }
      =
      let lowest_non_bottom_member interval ~order =
        let non_empty { upper_bound; lower_bound } ~order =
          Order.always_less_or_equal order ~left:lower_bound ~right:upper_bound
        in
        Option.some_if (non_empty interval ~order) (lower_bound interval)
        >>| function
        | Type.Bottom -> upper_bound interval
        | other -> other
      in
      match exogenous_constraint with
      | Explicit explicits ->
          let collect annotation sofar =
            let add_to_explicits_if_safe sofar candidate =
              match candidate with
              | { Type.Variable.Unary.constraints = Explicit left_constraints; _ } as candidate ->
                  let exists_in_explicits left_constraint =
                    List.exists explicits ~f:(Type.equal left_constraint)
                  in
                  if List.for_all left_constraints ~f:exists_in_explicits then
                    (* The only other thing that an explicit type variable can instantiate to is
                       another type variable with a subset of its values *)
                    Type.Variable candidate :: sofar
                  else
                    sofar
              | _ -> sofar
            in
            Type.Variable.GlobalTransforms.Unary.collect_all annotation
            |> List.fold ~f:add_to_explicits_if_safe ~init:sofar
          in
          let explicits =
            collect (lower_bound interval) explicits |> collect (upper_bound interval)
          in
          let contains { upper_bound; lower_bound } candidate ~order =
            Order.always_less_or_equal order ~left:candidate ~right:upper_bound
            && Order.always_less_or_equal order ~left:lower_bound ~right:candidate
          in
          (* When doing multiple solves, all of these options ought to be considered, *)
          (* and solved in a fixpoint *)
          List.find ~f:(contains interval ~order) explicits
      | Bound exogenous_bound ->
          intersection interval (create ~upper_bound:exogenous_bound ()) ~order
          >>= lowest_non_bottom_member ~order
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
          | _ -> None)


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
          Some Top
      | other, Bottom
      | Bottom, other ->
          Some other
      | Singleton left, Singleton right when Type.Callable.equal_parameters left right ->
          Some (Singleton left)
      | _, _ -> Some Top


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
          | _ -> failwith "impossible")


    let is_trivial interval ~variable =
      match interval with
      | Singleton
          (Type.Callable.ParameterVariadicTypeVariable { head = []; variable = target_variable }) ->
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

  module TupleInterval = struct
    module Variable = TupleVariable

    type t = tuple_interval

    let create ?upper_bound ?lower_bound () =
      match lower_bound, upper_bound with
      | None, None -> BottomTuple
      | None, Some only
      | Some only, None ->
          SingletonTuple only
      | Some left, Some right when Type.OrderedTypes.equal_record Type.equal left right ->
          SingletonTuple left
      | _ -> TopTuple


    let intersection left right ~order =
      match left, right with
      | TopTuple, _
      | _, TopTuple ->
          Some TopTuple
      | other, BottomTuple
      | BottomTuple, other ->
          Some other
      | SingletonTuple left, SingletonTuple right ->
          if Order.always_less_or_equal order ~left:(Type.Tuple left) ~right:(Type.Tuple right) then
            Some (SingletonTuple right)
          else if Order.always_less_or_equal order ~left:(Type.Tuple right) ~right:(Type.Tuple left)
          then
            Some (SingletonTuple left)
          else
            None


    let narrowest_valid_value interval ~order:_ ~variable:_ =
      match interval with
      | TopTuple
      | BottomTuple ->
          None
      | SingletonTuple ordered_type -> Some ordered_type


    let merge_solution_in target ~variable:_ ~solution =
      match target with
      | TopTuple
      | BottomTuple ->
          target
      | SingletonTuple ordered_type -> (
          match Solution.instantiate solution (Type.Tuple ordered_type) with
          | Type.Tuple instantiated_ordered_type -> SingletonTuple instantiated_ordered_type
          | _ -> failwith "impossible")


    let is_trivial interval ~variable =
      match interval with
      | SingletonTuple (Type.OrderedTypes.Concatenation concatenation) ->
          Type.OrderedTypes.Concatenation.extract_sole_variadic concatenation
          >>| (fun variadic -> TupleVariable.equal variadic variable)
          |> Option.value ~default:false
      | _ -> (* TODO(T84854853): Should this be true? *) false


    let free_variables = function
      | TopTuple
      | BottomTuple ->
          []
      | SingletonTuple ordered_type -> Type.Variable.all_free_variables (Type.Tuple ordered_type)
  end

  module CallableParametersIntervalContainer = IntervalContainer.Make (CallableParametersInterval)
  module UnaryIntervalContainer = IntervalContainer.Make (UnaryTypeInterval)
  module TupleIntervalContainer = IntervalContainer.Make (TupleInterval)

  type order = Order.t

  let add_bound
      ({ unaries; callable_parameters; tuple_variadics; _ } as constraints)
      ~order
      ~pair
      ~is_lower_bound
    =
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
    | Type.Variable.TupleVariadicPair (variable, bound) ->
        TupleIntervalContainer.add_bound tuple_variadics ~order ~variable ~bound ~is_lower_bound
        >>| fun tuple_variadics -> { constraints with tuple_variadics }


  let add_lower_bound = add_bound ~is_lower_bound:true

  let add_upper_bound = add_bound ~is_lower_bound:false

  let add_fallback_to_any ({ have_fallbacks; _ } as constraints) addition =
    { constraints with have_fallbacks = Set.add have_fallbacks addition }


  let merge_solution { unaries; callable_parameters; tuple_variadics; have_fallbacks } solution =
    {
      unaries = UnaryIntervalContainer.merge_solution unaries ~solution;
      callable_parameters =
        CallableParametersIntervalContainer.merge_solution callable_parameters ~solution;
      tuple_variadics = TupleIntervalContainer.merge_solution tuple_variadics ~solution;
      have_fallbacks;
    }


  let apply_fallbacks solution ~have_fallbacks =
    let optional_add map key data =
      match Map.add map ~key ~data with
      | `Ok map -> map
      | `Duplicate -> map
    in
    let add_fallback ({ Solution.unaries; callable_parameters; tuple_variadics } as solution)
      = function
      | Type.Variable.Unary variable ->
          { solution with unaries = optional_add unaries variable Type.Any }
      | Type.Variable.ParameterVariadic variable ->
          {
            solution with
            callable_parameters = optional_add callable_parameters variable Type.Callable.Undefined;
          }
      | Type.Variable.TupleVariadic variadic ->
          {
            solution with
            tuple_variadics = optional_add tuple_variadics variadic TupleVariable.any;
          }
    in
    Set.to_list have_fallbacks |> List.fold ~init:solution ~f:add_fallback


  let solve constraints ~order =
    let rec build_solution ~remaining_constraints ~partial_solution =
      let independent_constraints, dependent_constraints =
        let independent_unaries, dependent_unaries =
          UnaryIntervalContainer.partition_independent_dependent
            remaining_constraints.unaries
            ~with_regards_to:remaining_constraints
        in
        let independent_tuple_variadics, dependent_tuple_variadics =
          TupleIntervalContainer.partition_independent_dependent
            remaining_constraints.tuple_variadics
            ~with_regards_to:remaining_constraints
        in
        let independent_parameters, dependent_parameters =
          CallableParametersIntervalContainer.partition_independent_dependent
            remaining_constraints.callable_parameters
            ~with_regards_to:remaining_constraints
        in
        let independent_fallbacks, dependent_fallbacks =
          let matches = function
            | Type.Variable.Unary key -> not (Map.mem dependent_unaries key)
            | ParameterVariadic key -> not (Map.mem dependent_parameters key)
            | TupleVariadic key -> not (Map.mem dependent_tuple_variadics key)
          in
          Set.partition_tf remaining_constraints.have_fallbacks ~f:matches
        in
        ( {
            unaries = independent_unaries;
            callable_parameters = independent_parameters;
            tuple_variadics = independent_tuple_variadics;
            have_fallbacks = independent_fallbacks;
          },
          {
            unaries = dependent_unaries;
            callable_parameters = dependent_parameters;
            tuple_variadics = dependent_tuple_variadics;
            have_fallbacks = dependent_fallbacks;
          } )
      in
      let handle_dependent_constraints partial_solution =
        let is_empty { unaries; callable_parameters; tuple_variadics; have_fallbacks } =
          UnaryVariable.Map.is_empty unaries
          && ParameterVariable.Map.is_empty callable_parameters
          && TupleVariable.Map.is_empty tuple_variadics
          && Set.is_empty have_fallbacks
        in
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
      >>= TupleIntervalContainer.add_solution independent_constraints.tuple_variadics ~order
      >>| apply_fallbacks ~have_fallbacks:independent_constraints.have_fallbacks
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:Solution.empty


  let extract_partial_solution
      { unaries; callable_parameters; tuple_variadics; have_fallbacks }
      ~order
      ~variables
    =
    let extracted_constraints, remaining_constraints =
      let unary_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.Unary key))
      in
      let callable_parameters_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.ParameterVariadic key))
      in
      let tuple_variadic_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.TupleVariadic key))
      in
      let extracted_unaries, remaining_unaries =
        UnaryVariable.Map.partitioni_tf unaries ~f:unary_matches
      in
      let extracted_variadics, remaining_variadics =
        ParameterVariable.Map.partitioni_tf callable_parameters ~f:callable_parameters_matches
      in
      let extracted_tuple_variadics, remaining_tuple_variadics =
        TupleVariable.Map.partitioni_tf tuple_variadics ~f:tuple_variadic_matches
      in
      let extracted_fallbacks, remaining_fallbacks =
        let matches = function
          | Type.Variable.Unary key -> unary_matches ~key ~data:()
          | ParameterVariadic key -> callable_parameters_matches ~key ~data:()
          | TupleVariadic key -> tuple_variadic_matches ~key ~data:()
        in
        Set.partition_tf have_fallbacks ~f:matches
      in
      ( {
          unaries = extracted_unaries;
          callable_parameters = extracted_variadics;
          tuple_variadics = extracted_tuple_variadics;
          have_fallbacks = extracted_fallbacks;
        },
        {
          unaries = remaining_unaries;
          callable_parameters = remaining_variadics;
          tuple_variadics = remaining_tuple_variadics;
          have_fallbacks = remaining_fallbacks;
        } )
    in
    solve extracted_constraints ~order
    >>| fun solution -> merge_solution remaining_constraints solution, solution
end
