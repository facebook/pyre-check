(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
module ParameterVariable = Type.Variable.Variadic.Parameters
module UnaryVariable = Type.Variable.Unary
module ListVariadic = Type.Variable.Variadic.List

type unary_interval = {
  upper_bound: Type.t;
  lower_bound: Type.t;
}
[@@deriving show]

type callable_parameter_interval =
  | Top
  | Singleton of Type.Callable.parameters
  | Bottom
[@@deriving show]

(* This approach of making which bounds actually exist explicit allows us to avoid making artificial
   Top and Bottom members of (Type.t Type.OrderedTypes.t) *)
type list_variadic_interval =
  | NoBounds
  | OnlyUpperBound of Type.OrderedTypes.t
  | OnlyLowerBound of Type.OrderedTypes.t
  | BothBounds of {
      upper: Type.OrderedTypes.t;
      lower: Type.OrderedTypes.t;
    }
[@@deriving show]

type t = {
  unaries: unary_interval UnaryVariable.Map.t;
  callable_parameters: callable_parameter_interval ParameterVariable.Map.t;
  list_variadics: list_variadic_interval ListVariadic.Map.t;
  have_fallbacks: Type.Variable.Set.t;
}

let show_map map ~show_key ~show_data ~short_name =
  if Map.is_empty map then
    ""
  else
    let show ~key ~data sofar =
      Printf.sprintf "%s => %s" (show_key key) (show_data data) :: sofar
    in
    Map.fold map ~init:[] ~f:show |> String.concat ~sep:"\n" |> Format.sprintf "%s: [%s]" short_name


let pp format { unaries; callable_parameters; list_variadics; have_fallbacks } =
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
  let list_variadics =
    show_map
      list_variadics
      ~show_key:ListVariadic.show
      ~show_data:show_list_variadic_interval
      ~short_name:"lv"
  in
  let have_fallbacks =
    Set.to_list have_fallbacks
    |> List.to_string ~f:Type.Variable.show
    |> Format.sprintf "\nHave Fallbacks to Any: %s"
  in
  Format.fprintf format "{%s%s%s%s}" unaries callable_parameters list_variadics have_fallbacks


let show annotation = Format.asprintf "%a" pp annotation

let empty =
  {
    unaries = UnaryVariable.Map.empty;
    callable_parameters = ParameterVariable.Map.empty;
    list_variadics = ListVariadic.Map.empty;
    have_fallbacks = Type.Variable.Set.empty;
  }


let exists_in_bounds { unaries; callable_parameters; list_variadics; _ } ~variables =
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
    let contains_list_variadic =
      let list_variadic_contained_in_list variable =
        List.mem variables (Type.Variable.ListVariadic variable) ~equal:Type.Variable.equal
      in
      Type.Variable.GlobalTransforms.ListVariadic.collect_all annotation
      |> List.exists ~f:list_variadic_contained_in_list
    in
    contains_unary || contains_parameter_variadic || contains_list_variadic
  in
  let exists_in_interval_bounds { upper_bound; lower_bound } =
    contains_variable upper_bound || contains_variable lower_bound
  in
  let exists_in_callable_parameter_interval_bounds = function
    | Singleton parameters ->
        Type.Callable.create ~parameters ~annotation:Type.Any () |> contains_variable
    | _ -> false
  in
  let exists_in_list_variadic_interval_bounds interval =
    let exists = function
      | Type.OrderedTypes.Concrete types -> List.exists types ~f:contains_variable
      | Concatenation concatenation ->
          let contains = List.exists ~f:contains_variable in
          let in_head () = Type.OrderedTypes.Concatenation.head concatenation |> contains in
          let in_middle () =
            Type.OrderedTypes.Concatenation.variable concatenation
            |> (fun variable -> Type.Variable.ListVariadic variable)
            |> List.mem variables ~equal:Type.Variable.equal
          in
          let in_tail () = Type.OrderedTypes.Concatenation.tail concatenation |> contains in
          in_head () || in_middle () || in_tail ()
      | _ -> false
    in
    match interval with
    | NoBounds -> false
    | OnlyLowerBound bound
    | OnlyUpperBound bound ->
        exists bound
    | BothBounds { upper; lower } -> exists upper || exists lower
  in
  UnaryVariable.Map.exists unaries ~f:exists_in_interval_bounds
  || ParameterVariable.Map.exists
       callable_parameters
       ~f:exists_in_callable_parameter_interval_bounds
  || ListVariadic.Map.exists list_variadics ~f:exists_in_list_variadic_interval_bounds


module Solution = struct
  type t = {
    unaries: Type.t UnaryVariable.Map.t;
    callable_parameters: Type.Callable.parameters ParameterVariable.Map.t;
    list_variadics: Type.OrderedTypes.t ListVariadic.Map.t;
  }

  let equal left right =
    UnaryVariable.Map.equal Type.equal left.unaries right.unaries
    && ParameterVariable.Map.equal
         Type.Callable.equal_parameters
         left.callable_parameters
         right.callable_parameters
    && ListVariadic.Map.equal Type.OrderedTypes.equal left.list_variadics right.list_variadics


  let show { unaries; callable_parameters; list_variadics } =
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
    let list_variadics =
      show_map
        list_variadics
        ~show_key:ListVariadic.show
        ~show_data:Type.OrderedTypes.show
        ~short_name:"lv"
    in
    Format.sprintf "{%s%s%s}" unaries callable_parameters list_variadics


  let empty =
    {
      unaries = UnaryVariable.Map.empty;
      callable_parameters = ParameterVariable.Map.empty;
      list_variadics = ListVariadic.Map.empty;
    }


  let instantiate { unaries; callable_parameters; list_variadics } annotation =
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
      if ListVariadic.Map.is_empty list_variadics then
        annotation
      else
        Type.Variable.GlobalTransforms.ListVariadic.replace_all
          (fun variable -> ListVariadic.Map.find list_variadics variable)
          annotation
    in
    annotation


  let instantiate_single_variable { unaries; _ } = UnaryVariable.Map.find unaries

  let instantiate_single_list_variadic_variable { list_variadics; _ } =
    ListVariadic.Map.find list_variadics


  let instantiate_single_parameter_variadic { callable_parameters; _ } =
    ParameterVariable.Map.find callable_parameters


  let instantiate_ordered_types solution = function
    | Type.OrderedTypes.Concrete concretes ->
        List.map concretes ~f:(instantiate solution)
        |> fun concretes -> Type.OrderedTypes.Concrete concretes
    | Any -> Any
    | Concatenation concatenation ->
        let mapped =
          Type.OrderedTypes.Concatenation.map_head_and_tail concatenation ~f:(instantiate solution)
        in
        let replacement = instantiate_single_list_variadic_variable solution in
        Type.OrderedTypes.Concatenation.replace_variable mapped ~replacement
        |> Option.value ~default:(Type.OrderedTypes.Concatenation mapped)


  let instantiate_callable_parameters solution parameters =
    match instantiate solution (Type.Callable.create ~parameters ~annotation:Type.Any ()) with
    | Type.Callable { implementation = { parameters; _ }; _ } -> parameters
    | _ -> failwith "instantiate is not preserving callables"


  let set ({ unaries; callable_parameters; list_variadics } as solution) = function
    | Type.Variable.UnaryPair (key, data) ->
        { solution with unaries = UnaryVariable.Map.set unaries ~key ~data }
    | Type.Variable.ParameterVariadicPair (key, data) ->
        {
          solution with
          callable_parameters = ParameterVariable.Map.set callable_parameters ~key ~data;
        }
    | Type.Variable.ListVariadicPair (key, data) ->
        { solution with list_variadics = ListVariadic.Map.set list_variadics ~key ~data }


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
        let contains_key { unaries; callable_parameters; list_variadics; have_fallbacks } key =
          let has_constraints =
            match key with
            | Type.Variable.Unary unary -> Map.mem unaries unary
            | Type.Variable.ParameterVariadic parameters -> Map.mem callable_parameters parameters
            | Type.Variable.ListVariadic variable -> Map.mem list_variadics variable
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
          | _ -> failwith "impossible" )


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

  module ListVariadicInterval = struct
    module Variable = ListVariadic

    type t = list_variadic_interval

    let create ?upper_bound ?lower_bound () =
      match upper_bound, lower_bound with
      | None, None -> NoBounds
      | Some upper, None -> OnlyUpperBound upper
      | None, Some lower -> OnlyLowerBound lower
      | Some upper, Some lower -> BothBounds { upper; lower }


    let rec less_or_equal order ~left ~right =
      let compare_concretes ~left ~right =
        match List.zip left right with
        | Ok bounds ->
            List.for_all bounds ~f:(fun (left, right) ->
                Order.always_less_or_equal order ~left ~right)
        | _ -> false
      in
      let concrete_less_concatenation
          ~inverse
          ~concretes
          ~concatenation:
            {
              Type.OrderedTypes.RecordConcatenate.middle =
                { Type.OrderedTypes.RecordConcatenate.Middle.variable = { constraints; _ }; _ };
              wrapping = { head; tail };
            }
        =
        let valid = List.length head + List.length tail <= List.length concretes in
        let left_concrete, right_concrete =
          List.take concretes (List.length head), List.take (List.rev concretes) (List.length tail)
        in
        let middle_concretes =
          List.drop concretes (List.length head)
          |> List.rev
          |> fun x -> List.drop x (List.length tail) |> List.rev
        in
        match constraints with
        | Bound upper_bound ->
            let middle_as_concrete =
              Type.OrderedTypes.Concrete (List.map middle_concretes ~f:(fun _ -> upper_bound))
            in
            if inverse then
              valid
              && compare_concretes ~left:head ~right:left_concrete
              && compare_concretes ~left:tail ~right:right_concrete
              && less_or_equal
                   order
                   ~left:middle_as_concrete
                   ~right:(Type.OrderedTypes.Concrete concretes)
            else
              valid
              && compare_concretes ~right:head ~left:left_concrete
              && compare_concretes ~right:tail ~left:right_concrete
              && less_or_equal
                   order
                   ~left:(Type.OrderedTypes.Concrete concretes)
                   ~right:middle_as_concrete
        | Explicit explicits ->
            List.for_all explicits ~f:(fun concrete ->
                List.exists concretes ~f:(Type.equal concrete))
        | _ -> true
      in
      let is_free_concatenation
          {
            Type.OrderedTypes.RecordConcatenate.middle =
              { Type.OrderedTypes.RecordConcatenate.Middle.variable; _ };
            _;
          }
        =
        Type.Variable.Variadic.List.is_free variable
      in
      if Type.OrderedTypes.equal left right then
        true
      else
        match left, right with
        | _, Any
        | Any, _ ->
            true
        | Concatenation concatenation, Concrete concretes
        | Concrete concretes, Concatenation concatenation
          when is_free_concatenation concatenation ->
            concrete_less_concatenation ~concretes ~concatenation ~inverse:false
        | Concrete left, Concrete right -> compare_concretes ~left ~right
        | ( Concatenation
              ( {
                  Type.OrderedTypes.RecordConcatenate.middle =
                    { variable = { constraints = constraints_left; _ }; mappers = [] };
                  wrapping = { head = head_left; tail = tail_left };
                } as concatenation_left ),
            Concatenation
              ( {
                  Type.OrderedTypes.RecordConcatenate.middle =
                    { variable = { constraints = constraints_right; _ }; mappers = [] };
                  wrapping = { head = head_right; tail = tail_right };
                } as concatenation_right ) )
          when is_free_concatenation concatenation_left && is_free_concatenation concatenation_right
          ->
            (* Example [A1,B1,Ts1,C1] <: [A2,Ts2,C2] [A1] <: [A2] && [C1] <: [C2] && [B1, Ts1] <:
               [Ts2] *)
            let drop_head = min (List.length head_left) (List.length head_right) in
            let drop_tail = min (List.length tail_left) (List.length tail_right) in
            let (prefix_head_left, head_left), (prefix_head_right, head_right) =
              List.split_n head_left drop_head, List.split_n head_right drop_head
            in
            let (prefix_tail_left, tail_left), (prefix_tail_right, tail_right) =
              List.split_n tail_left drop_tail, List.split_n tail_right drop_tail
            in
            let valid =
              compare_concretes ~left:prefix_head_left ~right:prefix_head_right
              && compare_concretes ~left:prefix_tail_left ~right:prefix_tail_right
            in
            let valid =
              valid
              &&
              match constraints_left, constraints_right with
              | Bound left, Bound right -> Order.always_less_or_equal order ~left ~right
              | Explicit lefts, Bound bound ->
                  List.for_all lefts ~f:(fun left ->
                      Order.always_less_or_equal order ~left ~right:bound)
              | Bound _, Explicit _ -> false
              | Explicit lefts, Explicit rights ->
                  List.for_all lefts ~f:(List.mem rights ~equal:Type.equal)
              | _, Unconstrained -> true
              | Unconstrained, _ -> false
              | _, _ -> false
            in
            let valid =
              valid
              &&
              match head_left @ tail_left, head_right @ tail_right with
              (* [A,B,Ts1, C] <: [Ts2] *)
              | head_tail_left, [] -> less_or_equal order ~left:(Concrete head_tail_left) ~right
              | [], head_tail_right -> less_or_equal order ~left ~right:(Concrete head_tail_right)
              | _, _ -> false
            in
            valid
        | Concatenation _, _ -> false
        | _, Concatenation _ -> false


    let intersection left right ~order =
      let meet left right =
        if Type.OrderedTypes.equal left right then
          Some left
        else if less_or_equal order ~left ~right then
          Some left
        else if less_or_equal order ~left:right ~right:left then
          Some right
        else
          match left, right with
          | Concrete left, Concrete right -> (
              match List.zip left right with
              | Ok zipped ->
                  List.map zipped ~f:(fun (left, right) -> Order.meet order left right)
                  |> fun concrete_list -> Some (Type.OrderedTypes.Concrete concrete_list)
              | _ -> None )
          | _ -> None
      in
      let join left right =
        if Type.OrderedTypes.equal left right then
          Some left
        else if less_or_equal order ~left ~right then
          Some right
        else if less_or_equal order ~left:right ~right:left then
          Some left
        else
          match left, right with
          | Concrete left, Concrete right -> (
              match List.zip left right with
              | Ok zipped ->
                  List.map zipped ~f:(fun (left, right) -> Order.join order left right)
                  |> fun concrete_list -> Some (Type.OrderedTypes.Concrete concrete_list)
              | _ -> None )
          | _ -> None
      in
      match left, right with
      | NoBounds, other
      | other, NoBounds ->
          Some other
      | OnlyLowerBound lower, OnlyUpperBound upper
      | OnlyUpperBound upper, OnlyLowerBound lower ->
          Some (BothBounds { lower; upper })
      | OnlyLowerBound left, OnlyLowerBound right ->
          join left right >>| fun lower -> OnlyLowerBound lower
      | OnlyUpperBound left, OnlyUpperBound right ->
          meet left right >>| fun upper -> OnlyUpperBound upper
      | OnlyUpperBound upper, BothBounds both
      | BothBounds both, OnlyUpperBound upper ->
          meet upper both.upper >>| fun upper -> BothBounds { both with upper }
      | OnlyLowerBound lower, BothBounds both
      | BothBounds both, OnlyLowerBound lower ->
          join lower both.lower >>| fun lower -> BothBounds { both with lower }
      | BothBounds left, BothBounds right -> (
          match meet left.upper right.upper, join left.lower right.lower with
          | Some upper, Some lower -> Some (BothBounds { upper; lower })
          | _ -> None )


    let narrowest_valid_value interval ~order ~(variable : Type.Variable.list_variadic_t) =
      let { Type.Variable.RecordVariadic.RecordList.constraints = exogenous_constraint; _ } =
        variable
      in
      let prune_interval interval =
        match interval with
        | NoBounds -> None
        | OnlyLowerBound bound
        | OnlyUpperBound bound ->
            Some bound
        | BothBounds { upper; lower } ->
            Option.some_if (less_or_equal order ~left:lower ~right:upper) lower
      in
      let variable = ListVariadic.self_reference variable in
      let intersected_interval =
        match exogenous_constraint with
        | Bound _ -> intersection interval (create ~upper_bound:variable ()) ~order
        | Explicit _ ->
            intersection interval (create ~upper_bound:variable ~lower_bound:variable ()) ~order
        | _ -> Some interval
      in
      intersected_interval >>= prune_interval


    let merge_solution_in interval ~variable ~solution =
      let upper_bound, lower_bound =
        match interval with
        | NoBounds -> None, None
        | OnlyUpperBound upper -> Some upper, None
        | OnlyLowerBound lower -> None, Some lower
        | BothBounds { upper; lower } -> Some upper, Some lower
      in
      let smart_instantiate ordered_types =
        let instantiated = Solution.instantiate_ordered_types solution ordered_types in
        if Type.OrderedTypes.equal (ListVariadic.self_reference variable) instantiated then
          (* We don't want variables bounded by themselves *)
          None
        else
          Some instantiated
      in
      create
        ?upper_bound:(upper_bound >>= smart_instantiate)
        ?lower_bound:(lower_bound >>= smart_instantiate)
        ()


    let is_trivial interval ~variable:_ =
      match interval with
      | NoBounds -> true
      | _ -> false


    let free_variables interval =
      let bounds =
        match interval with
        | NoBounds -> []
        | OnlyUpperBound upper -> [upper]
        | OnlyLowerBound lower -> [lower]
        | BothBounds { upper; lower } -> [upper; lower]
      in
      let extract = function
        | Type.OrderedTypes.Any -> []
        | Concrete types -> List.concat_map types ~f:Type.Variable.all_free_variables
        | Concatenation concatenation ->
            let outer =
              Type.OrderedTypes.Concatenation.head concatenation
              @ Type.OrderedTypes.Concatenation.tail concatenation
              |> List.concat_map ~f:Type.Variable.all_free_variables
            in
            let inner = Type.OrderedTypes.Concatenation.variable concatenation in
            if Type.Variable.Variadic.List.is_free inner then
              ListVariadic inner :: outer
            else
              outer
      in
      List.concat_map bounds ~f:extract
  end

  module CallableParametersIntervalContainer = IntervalContainer.Make (CallableParametersInterval)
  module UnaryIntervalContainer = IntervalContainer.Make (UnaryTypeInterval)
  module ListVariadicIntervalContainer = IntervalContainer.Make (ListVariadicInterval)

  type order = Order.t

  let add_bound
      ({ unaries; callable_parameters; list_variadics; _ } as constraints)
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
    | Type.Variable.ListVariadicPair (variable, bound) ->
        ListVariadicIntervalContainer.add_bound
          list_variadics
          ~order
          ~variable
          ~bound
          ~is_lower_bound
        >>| fun list_variadics -> { constraints with list_variadics }


  let add_lower_bound = add_bound ~is_lower_bound:true

  let add_upper_bound = add_bound ~is_lower_bound:false

  let add_fallback_to_any ({ have_fallbacks; _ } as constraints) addition =
    { constraints with have_fallbacks = Set.add have_fallbacks addition }


  let merge_solution { unaries; callable_parameters; list_variadics; have_fallbacks } solution =
    {
      unaries = UnaryIntervalContainer.merge_solution unaries ~solution;
      callable_parameters =
        CallableParametersIntervalContainer.merge_solution callable_parameters ~solution;
      list_variadics = ListVariadicIntervalContainer.merge_solution list_variadics ~solution;
      have_fallbacks;
    }


  let apply_fallbacks solution ~have_fallbacks =
    let optional_add map key data =
      match Map.add map ~key ~data with
      | `Ok map -> map
      | `Duplicate -> map
    in
    let add_fallback ({ Solution.unaries; callable_parameters; list_variadics } as solution)
      = function
      | Type.Variable.Unary variable ->
          { solution with unaries = optional_add unaries variable Type.Any }
      | Type.Variable.ParameterVariadic variable ->
          {
            solution with
            callable_parameters = optional_add callable_parameters variable Type.Callable.Undefined;
          }
      | Type.Variable.ListVariadic variable ->
          {
            solution with
            list_variadics = optional_add list_variadics variable Type.OrderedTypes.Any;
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
        let independent_parameters, dependent_parameters =
          CallableParametersIntervalContainer.partition_independent_dependent
            remaining_constraints.callable_parameters
            ~with_regards_to:remaining_constraints
        in
        let independent_list_variadics, dependent_list_variadics =
          ListVariadicIntervalContainer.partition_independent_dependent
            remaining_constraints.list_variadics
            ~with_regards_to:remaining_constraints
        in
        let independent_fallbacks, dependent_fallbacks =
          let matches = function
            | Type.Variable.Unary key -> not (Map.mem dependent_unaries key)
            | ParameterVariadic key -> not (Map.mem dependent_parameters key)
            | ListVariadic key -> not (Map.mem dependent_list_variadics key)
          in
          Set.partition_tf remaining_constraints.have_fallbacks ~f:matches
        in
        ( {
            unaries = independent_unaries;
            callable_parameters = independent_parameters;
            list_variadics = independent_list_variadics;
            have_fallbacks = independent_fallbacks;
          },
          {
            unaries = dependent_unaries;
            callable_parameters = dependent_parameters;
            list_variadics = dependent_list_variadics;
            have_fallbacks = dependent_fallbacks;
          } )
      in
      let handle_dependent_constraints partial_solution =
        let is_empty { unaries; callable_parameters; list_variadics; have_fallbacks } =
          UnaryVariable.Map.is_empty unaries
          && ParameterVariable.Map.is_empty callable_parameters
          && ListVariadic.Map.is_empty list_variadics
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
      >>= ListVariadicIntervalContainer.add_solution independent_constraints.list_variadics ~order
      >>| apply_fallbacks ~have_fallbacks:independent_constraints.have_fallbacks
      >>= handle_dependent_constraints
    in
    build_solution ~remaining_constraints:constraints ~partial_solution:Solution.empty


  let extract_partial_solution
      { unaries; callable_parameters; list_variadics; have_fallbacks }
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
      let list_variadic_matches ~key ~data:_ =
        List.exists variables ~f:(Type.Variable.equal (Type.Variable.ListVariadic key))
      in
      let extracted_unaries, remaining_unaries =
        UnaryVariable.Map.partitioni_tf unaries ~f:unary_matches
      in
      let extracted_variadics, remaining_variadics =
        ParameterVariable.Map.partitioni_tf callable_parameters ~f:callable_parameters_matches
      in
      let extracted_list_variadics, remaining_list_variadics =
        ListVariadic.Map.partitioni_tf list_variadics ~f:list_variadic_matches
      in
      let extracted_fallbacks, remaining_fallbacks =
        let matches = function
          | Type.Variable.Unary key -> unary_matches ~key ~data:()
          | ParameterVariadic key -> callable_parameters_matches ~key ~data:()
          | ListVariadic key -> list_variadic_matches ~key ~data:()
        in
        Set.partition_tf have_fallbacks ~f:matches
      in
      ( {
          unaries = extracted_unaries;
          callable_parameters = extracted_variadics;
          list_variadics = extracted_list_variadics;
          have_fallbacks = extracted_fallbacks;
        },
        {
          unaries = remaining_unaries;
          callable_parameters = remaining_variadics;
          list_variadics = remaining_list_variadics;
          have_fallbacks = remaining_fallbacks;
        } )
    in
    solve extracted_constraints ~order
    >>| fun solution -> merge_solution remaining_constraints solution, solution
end
