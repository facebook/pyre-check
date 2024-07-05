(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre
module ParamSpec = Type.Variable.ParamSpec
module TypeVar = Type.Variable.TypeVar
module TypeVarTuple = Type.Variable.TypeVarTuple

type type_var_interval = {
  upper_bound: Type.t;
  lower_bound: Type.t;
}
[@@deriving compare, show]

type param_spec_interval =
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
type type_var_tuple_interval =
  | TopTuple
  | BottomTuple
  | SingletonTuple of Type.t Type.OrderedTypes.record
[@@deriving compare, show]

type t = {
  type_vars: type_var_interval TypeVar.Map.t;
  param_specs: param_spec_interval ParamSpec.Map.t;
  type_var_tuples: type_var_tuple_interval TypeVarTuple.Map.t;
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


let pp format { type_vars; param_specs; type_var_tuples; have_fallbacks } =
  let type_vars =
    show_map type_vars ~show_key:TypeVar.show ~show_data:show_type_var_interval ~short_name:"un"
  in
  let param_specs =
    show_map
      param_specs
      ~show_key:ParamSpec.show
      ~show_data:show_param_spec_interval
      ~short_name:"cb"
  in
  let type_var_tuples =
    show_map
      type_var_tuples
      ~show_key:TypeVarTuple.show
      ~show_data:show_type_var_tuple_interval
      ~short_name:"variadic_tuple"
  in
  let have_fallbacks =
    Set.to_list have_fallbacks
    |> List.to_string ~f:Type.Variable.show
    |> Format.sprintf "\nHave Fallbacks to Any: %s"
  in
  Format.fprintf format "{%s%s%s%s}" type_vars param_specs type_var_tuples have_fallbacks


let show annotation = Format.asprintf "%a" pp annotation

let empty =
  {
    type_vars = TypeVar.Map.empty;
    param_specs = ParamSpec.Map.empty;
    type_var_tuples = TypeVarTuple.Map.empty;
    have_fallbacks = Type.Variable.Set.empty;
  }


let exists_in_bounds { type_vars; param_specs; type_var_tuples; _ } ~variables =
  let contains_variable annotation =
    let contains_unary =
      Type.Variable.GlobalTransforms.TypeVar.collect_all annotation
      |> List.exists ~f:(fun variable ->
             List.mem variables (Type.Variable.TypeVarVariable variable) ~equal:Type.Variable.equal)
    in
    let contains_parameter_variadic =
      let parameter_variadic_contained_in_list variable =
        List.mem variables (Type.Variable.ParamSpecVariable variable) ~equal:Type.Variable.equal
      in
      Type.Variable.GlobalTransforms.ParamSpec.collect_all annotation
      |> List.exists ~f:parameter_variadic_contained_in_list
    in
    let contains_tuple_variadic =
      Type.Variable.GlobalTransforms.TypeVarTuple.collect_all annotation
      |> List.exists ~f:(fun variable ->
             List.mem
               variables
               (Type.Variable.TypeVarTupleVariable variable)
               ~equal:Type.Variable.equal)
    in
    contains_unary || contains_parameter_variadic || contains_tuple_variadic
  in
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    contains_variable upper_bound || contains_variable lower_bound
  in
  let exists_in_param_spec_interval_bounds = function
    | Singleton parameters ->
        Type.Callable.create ~parameters ~annotation:Type.Any () |> contains_variable
    | _ -> false
  in
  let exists_in_type_var_tuple_interval = function
    | SingletonTuple ordered_type -> Type.Tuple ordered_type |> contains_variable
    | _ -> false
  in
  Map.exists type_vars ~f:exists_in_interval_bounds
  || Map.exists param_specs ~f:exists_in_param_spec_interval_bounds
  || Map.exists type_var_tuples ~f:exists_in_type_var_tuple_interval


module Solution = struct
  type t = {
    type_vars: Type.t TypeVar.Map.t;
    param_specs: Type.Callable.parameters ParamSpec.Map.t;
    type_var_tuples: Type.t Type.OrderedTypes.record TypeVarTuple.Map.t;
  }
  [@@deriving sexp]

  let equal left right =
    TypeVar.Map.equal Type.equal left.type_vars right.type_vars
    && ParamSpec.Map.equal Type.Callable.equal_parameters left.param_specs right.param_specs
    && TypeVarTuple.Map.equal
         (Type.OrderedTypes.equal_record Type.equal)
         left.type_var_tuples
         right.type_var_tuples


  let show { type_vars; param_specs; type_var_tuples } =
    let type_vars =
      show_map type_vars ~show_key:TypeVar.show ~show_data:Type.show ~short_name:"un"
    in
    let param_specs =
      show_map
        param_specs
        ~show_key:ParamSpec.show
        ~show_data:Type.Callable.show_parameters
        ~short_name:"cb"
    in
    let type_var_tuples =
      show_map
        type_var_tuples
        ~show_key:TypeVarTuple.show
        ~show_data:(Type.OrderedTypes.show_record Type.pp)
        ~short_name:"variadic_tuple"
    in
    Format.sprintf "{%s%s%s}" type_vars param_specs type_var_tuples


  let pp format solution = Format.fprintf format "%s" (show solution)

  let empty =
    {
      type_vars = TypeVar.Map.empty;
      param_specs = ParamSpec.Map.empty;
      type_var_tuples = TypeVarTuple.Map.empty;
    }


  let instantiate { type_vars; param_specs; type_var_tuples } annotation =
    let annotation =
      if Map.is_empty type_vars then
        annotation
      else
        Type.Variable.GlobalTransforms.TypeVar.replace_all
          (fun variable -> Map.find type_vars variable)
          annotation
    in
    let annotation =
      if Map.is_empty param_specs then
        annotation
      else
        Type.Variable.GlobalTransforms.ParamSpec.replace_all
          (fun variable -> Map.find param_specs variable)
          annotation
    in
    let annotation =
      if Map.is_empty type_var_tuples then
        annotation
      else
        Type.Variable.GlobalTransforms.TypeVarTuple.replace_all
          (fun variable -> Map.find type_var_tuples variable)
          annotation
    in
    annotation


  let instantiate_single_type_var { type_vars; _ } = Map.find type_vars

  let instantiate_single_param_spec { param_specs; _ } = Map.find param_specs

  let instantiate_ordered_types solution ordered_type =
    match instantiate solution (Type.Tuple ordered_type) with
    | Type.Tuple instantiated_ordered_type -> instantiated_ordered_type
    | _ -> failwith "expected Tuple"


  let instantiate_callable_parameters solution parameters =
    match instantiate solution (Type.Callable.create ~parameters ~annotation:Type.Any ()) with
    | Type.Callable { implementation = { parameters; _ }; _ } -> parameters
    | _ -> failwith "instantiate is not preserving callables"


  let set ({ type_vars; param_specs; type_var_tuples } as solution) = function
    | Type.Variable.TypeVarPair (key, data) ->
        { solution with type_vars = Map.set type_vars ~key ~data }
    | Type.Variable.ParamSpecPair (key, data) ->
        { solution with param_specs = Map.set param_specs ~key ~data }
    | Type.Variable.TypeVarTuplePair (key, data) ->
        { solution with type_var_tuples = Map.set type_var_tuples ~key ~data }


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
        let contains_key { type_vars; param_specs; type_var_tuples; have_fallbacks } key =
          let has_constraints =
            match key with
            | Type.Variable.TypeVarVariable unary -> Map.mem type_vars unary
            | Type.Variable.ParamSpecVariable parameters -> Map.mem param_specs parameters
            | Type.Variable.TypeVarTupleVariable variadic -> Map.mem type_var_tuples variadic
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

  module TypeVarInterval = struct
    module Variable = TypeVar

    type t = type_var_interval

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
        ~variable:{ TypeVar.constraints = exogenous_constraint; _ }
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
              | { Type.Variable.TypeVar.constraints = Explicit left_constraints; _ } as candidate ->
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
            Type.Variable.GlobalTransforms.TypeVar.collect_all annotation
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

  module ParamSpecInterval = struct
    module Variable = ParamSpec

    type t = param_spec_interval

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
      | Singleton (Type.Callable.FromParamSpec { head = []; variable = target_variable }) ->
          ParamSpec.equal variable target_variable
      | _ -> false


    let free_variables = function
      | Top
      | Bottom ->
          []
      | Singleton parameters ->
          Type.Callable.create ~parameters ~annotation:Type.Any ()
          |> Type.Variable.all_free_variables
  end

  module TypeVarTupleInterval = struct
    module Variable = TypeVarTuple

    type t = type_var_tuple_interval

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
          >>| (fun variadic -> TypeVarTuple.equal variadic variable)
          |> Option.value ~default:false
      | _ -> (* TODO(T84854853): Should this be true? *) false


    let free_variables = function
      | TopTuple
      | BottomTuple ->
          []
      | SingletonTuple ordered_type -> Type.Variable.all_free_variables (Type.Tuple ordered_type)
  end

  module ParamSpecIntervalContainer = IntervalContainer.Make (ParamSpecInterval)
  module TypeVarIntervalContainer = IntervalContainer.Make (TypeVarInterval)
  module TypeVarTupleIntervalContainer = IntervalContainer.Make (TypeVarTupleInterval)

  type order = Order.t

  let add_bound
      ({ type_vars; param_specs; type_var_tuples; _ } as constraints)
      ~order
      ~pair
      ~is_lower_bound
    =
    match pair with
    | Type.Variable.TypeVarPair (variable, bound) ->
        TypeVarIntervalContainer.add_bound type_vars ~order ~variable ~bound ~is_lower_bound
        >>| fun type_vars -> { constraints with type_vars }
    | Type.Variable.ParamSpecPair (variable, bound) ->
        ParamSpecIntervalContainer.add_bound param_specs ~order ~variable ~bound ~is_lower_bound
        >>| fun param_specs -> { constraints with param_specs }
    | Type.Variable.TypeVarTuplePair (variable, bound) ->
        TypeVarTupleIntervalContainer.add_bound
          type_var_tuples
          ~order
          ~variable
          ~bound
          ~is_lower_bound
        >>| fun type_var_tuples -> { constraints with type_var_tuples }


  let add_lower_bound = add_bound ~is_lower_bound:true

  let add_upper_bound = add_bound ~is_lower_bound:false

  let add_fallback_to_any ({ have_fallbacks; _ } as constraints) addition =
    { constraints with have_fallbacks = Set.add have_fallbacks addition }


  let merge_solution { type_vars; param_specs; type_var_tuples; have_fallbacks } solution =
    {
      type_vars = TypeVarIntervalContainer.merge_solution type_vars ~solution;
      param_specs = ParamSpecIntervalContainer.merge_solution param_specs ~solution;
      type_var_tuples = TypeVarTupleIntervalContainer.merge_solution type_var_tuples ~solution;
      have_fallbacks;
    }


  let apply_fallbacks solution ~have_fallbacks =
    let optional_add map key data =
      match Map.add map ~key ~data with
      | `Ok map -> map
      | `Duplicate -> map
    in
    let add_fallback ({ Solution.type_vars; param_specs; type_var_tuples } as solution) = function
      | Type.Variable.TypeVarVariable variable ->
          { solution with type_vars = optional_add type_vars variable Type.Any }
      | Type.Variable.ParamSpecVariable variable ->
          { solution with param_specs = optional_add param_specs variable Type.Callable.Undefined }
      | Type.Variable.TypeVarTupleVariable variadic ->
          { solution with type_var_tuples = optional_add type_var_tuples variadic TypeVarTuple.any }
    in
    Set.to_list have_fallbacks |> List.fold ~init:solution ~f:add_fallback


  let solve constraints ~order =
    let rec build_solution ~remaining_constraints ~partial_solution =
      let independent_constraints, dependent_constraints =
        let independent_type_vars, dependent_type_vars =
          TypeVarIntervalContainer.partition_independent_dependent
            remaining_constraints.type_vars
            ~with_regards_to:remaining_constraints
        in
        let independent_type_var_tuples, dependent_type_var_tuples =
          TypeVarTupleIntervalContainer.partition_independent_dependent
            remaining_constraints.type_var_tuples
            ~with_regards_to:remaining_constraints
        in
        let independent_parameters, dependent_parameters =
          ParamSpecIntervalContainer.partition_independent_dependent
            remaining_constraints.param_specs
            ~with_regards_to:remaining_constraints
        in
        let independent_fallbacks, dependent_fallbacks =
          let matches = function
            | Type.Variable.TypeVarVariable key -> not (Map.mem dependent_type_vars key)
            | ParamSpecVariable key -> not (Map.mem dependent_parameters key)
            | TypeVarTupleVariable key -> not (Map.mem dependent_type_var_tuples key)
          in
          Set.partition_tf remaining_constraints.have_fallbacks ~f:matches
        in
        ( {
            type_vars = independent_type_vars;
            param_specs = independent_parameters;
            type_var_tuples = independent_type_var_tuples;
            have_fallbacks = independent_fallbacks;
          },
          {
            type_vars = dependent_type_vars;
            param_specs = dependent_parameters;
            type_var_tuples = dependent_type_var_tuples;
            have_fallbacks = dependent_fallbacks;
          } )
      in
      let handle_dependent_constraints partial_solution =
        let is_empty { type_vars; param_specs; type_var_tuples; have_fallbacks } =
          Map.is_empty type_vars
          && Map.is_empty param_specs
          && Map.is_empty type_var_tuples
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
      TypeVarIntervalContainer.add_solution
        independent_constraints.type_vars
        partial_solution
        ~order
      >>= ParamSpecIntervalContainer.add_solution independent_constraints.param_specs ~order
      >>= TypeVarTupleIntervalContainer.add_solution independent_constraints.type_var_tuples ~order
      >>| apply_fallbacks ~have_fallbacks:independent_constraints.have_fallbacks
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:Solution.empty


  let extract_partial_solution
      { type_vars; param_specs; type_var_tuples; have_fallbacks }
      ~order
      ~variables
    =
    let extracted_constraints, remaining_constraints =
      let unary_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.TypeVarVariable key))
      in
      let param_specs_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.ParamSpecVariable key))
      in
      let tuple_variadic_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.TypeVarTupleVariable key))
      in
      let extracted_type_vars, remaining_type_vars = Map.partitioni_tf type_vars ~f:unary_matches in
      let extracted_variadics, remaining_variadics =
        Map.partitioni_tf param_specs ~f:param_specs_matches
      in
      let extracted_type_var_tuples, remaining_type_var_tuples =
        Map.partitioni_tf type_var_tuples ~f:tuple_variadic_matches
      in
      let extracted_fallbacks, remaining_fallbacks =
        let matches = function
          | Type.Variable.TypeVarVariable key -> unary_matches ~key ~data:()
          | ParamSpecVariable key -> param_specs_matches ~key ~data:()
          | TypeVarTupleVariable key -> tuple_variadic_matches ~key ~data:()
        in
        Set.partition_tf have_fallbacks ~f:matches
      in
      ( {
          type_vars = extracted_type_vars;
          param_specs = extracted_variadics;
          type_var_tuples = extracted_type_var_tuples;
          have_fallbacks = extracted_fallbacks;
        },
        {
          type_vars = remaining_type_vars;
          param_specs = remaining_variadics;
          type_var_tuples = remaining_type_var_tuples;
          have_fallbacks = remaining_fallbacks;
        } )
    in
    solve extracted_constraints ~order
    >>| fun solution -> merge_solution remaining_constraints solution, solution
end
