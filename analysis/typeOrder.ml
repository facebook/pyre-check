(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module Callable = Type.Callable

module ProtocolAssumptions : sig
  type t

  val find_assumed_protocol_parameters
    :  candidate:Type.t ->
    protocol:Identifier.t ->
    t ->
    Type.OrderedTypes.t option

  val add
    :  candidate:Type.t ->
    protocol:Identifier.t ->
    protocol_parameters:Type.OrderedTypes.t ->
    t ->
    t

  val empty : t
end = struct
  type protocol_parameters = Type.OrderedTypes.t

  type assumption = {
    candidate: Type.t;
    protocol: Identifier.t;
  }
  [@@deriving eq]

  type t = (assumption, protocol_parameters) List.Assoc.t

  let find_assumed_protocol_parameters ~candidate ~protocol assumptions =
    List.Assoc.find assumptions { candidate; protocol } ~equal:equal_assumption


  let add ~candidate ~protocol ~protocol_parameters existing_assumptions =
    List.Assoc.add
      existing_assumptions
      { candidate; protocol }
      protocol_parameters
      ~equal:equal_assumption


  let empty = []
end

type order = {
  handler: (module ClassHierarchy.Handler);
  constructor: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> Type.t option;
  attributes:
    Type.t -> protocol_assumptions:ProtocolAssumptions.t -> AnnotatedAttribute.t list option;
  is_protocol: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> bool;
  any_is_bottom: bool;
  protocol_assumptions: ProtocolAssumptions.t;
}

module type FullOrderTypeWithoutT = sig
  val solve_less_or_equal
    :  order ->
    constraints:TypeConstraints.t ->
    left:Type.t ->
    right:Type.t ->
    TypeConstraints.t list

  val always_less_or_equal : order -> left:Type.t -> right:Type.t -> bool

  val meet : order -> Type.t -> Type.t -> Type.t

  val join : order -> Type.t -> Type.t -> Type.t

  val instantiate_protocol_parameters
    :  order ->
    candidate:Type.t ->
    protocol:Ast.Identifier.t ->
    Type.OrderedTypes.t option

  val solve_ordered_types_less_or_equal
    :  order ->
    left:Type.OrderedTypes.t ->
    right:Type.OrderedTypes.t ->
    constraints:TypeConstraints.t ->
    TypeConstraints.t sexp_list
end

module type FullOrderType = sig
  type t = order

  include FullOrderTypeWithoutT
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module OrderImplementation = struct
  module Make (OrderedConstraints : OrderedConstraintsType) = struct
    type t = order

    (* TODO(T40105833): merge this with actual signature select *)
    let rec simulate_signature_select
        order
        ~callable:{ Type.Callable.implementation; overloads; _ }
        ~called_as
        ~constraints
      =
      let open Callable in
      let solve implementation ~initial_constraints =
        try
          let rec solve_parameters_against_list_variadic ~is_lower_bound ~concretes ~variable ~tail
            =
            let before_first_keyword, after_first_keyword_inclusive =
              let is_not_keyword_only = function
                | Type.Callable.Parameter.Keywords _
                | Type.Callable.Parameter.KeywordOnly _ ->
                    false
                | _ -> true
              in
              List.split_while concretes ~f:is_not_keyword_only
            in
            let single_annotation = function
              | Type.Callable.Parameter.Anonymous { annotation; _ } -> Some annotation
              | Named { annotation; _ } when not is_lower_bound ->
                  (* Named arguments can be called positionally, but positionals can't be called
                     with a name *)
                  Some annotation
              | _ -> None
            in
            let continue =
              if is_lower_bound then
                solve_parameters
                  ~left_parameters:after_first_keyword_inclusive
                  ~right_parameters:tail
              else
                solve_parameters
                  ~left_parameters:tail
                  ~right_parameters:after_first_keyword_inclusive
            in
            let add_bound concretes =
              let left, right =
                if is_lower_bound then
                  concretes, variable
                else
                  variable, concretes
              in
              solve_ordered_types_less_or_equal order ~left ~right ~constraints
            in
            List.map before_first_keyword ~f:single_annotation
            |> Option.all
            >>| (fun concretes -> Type.OrderedTypes.Concrete concretes)
            >>| add_bound
            >>| List.concat_map ~f:continue
            |> Option.value ~default:[]
          and solve_parameters ~left_parameters ~right_parameters constraints =
            match left_parameters, right_parameters with
            | Parameter.Anonymous _ :: _, Parameter.Named _ :: _ -> []
            | ( Parameter.Anonymous { annotation = left_annotation; _ } :: left_parameters,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters )
            | ( Parameter.Named { annotation = left_annotation; _ } :: left_parameters,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters ) ->
                solve_less_or_equal
                  order
                  ~constraints
                  ~left:right_annotation
                  ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
            | ( Parameter.Variable (Concrete left_annotation) :: left_parameters,
                Parameter.Variable (Concrete right_annotation) :: right_parameters )
            | ( Parameter.Keywords left_annotation :: left_parameters,
                Parameter.Keywords right_annotation :: right_parameters ) ->
                solve_less_or_equal
                  order
                  ~constraints
                  ~left:right_annotation
                  ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
            | ( Parameter.KeywordOnly ({ annotation = left_annotation; _ } as left)
                :: left_parameters,
                Parameter.KeywordOnly ({ annotation = right_annotation; _ } as right)
                :: right_parameters )
            | ( Parameter.Named ({ annotation = left_annotation; _ } as left) :: left_parameters,
                Parameter.Named ({ annotation = right_annotation; _ } as right) :: right_parameters
              ) ->
                if Parameter.names_compatible (Parameter.Named left) (Parameter.Named right) then
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:right_annotation
                    ~right:left_annotation
                  |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
                else
                  []
            | ( Parameter.Variable (Concrete left_annotation) :: _,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters ) ->
                solve_less_or_equal
                  order
                  ~constraints
                  ~left:right_annotation
                  ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
            | ( Parameter.Variable (Variadic left_variable) :: left_parameters,
                Parameter.Variable (Variadic right_variable) :: right_parameters ) ->
                solve_ordered_types_less_or_equal
                  order
                  ~left:(Type.OrderedTypes.Variable left_variable)
                  ~right:(Type.OrderedTypes.Variable right_variable)
                  ~constraints
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
            | left, Parameter.Variable (Variadic right_variable) :: right_parameters ->
                solve_parameters_against_list_variadic
                  ~is_lower_bound:false
                  ~concretes:left
                  ~variable:(Variable right_variable)
                  ~tail:right_parameters
            | Parameter.Variable (Variadic left_variable) :: left_parameters, right ->
                solve_parameters_against_list_variadic
                  ~is_lower_bound:true
                  ~concretes:right
                  ~variable:(Variable left_variable)
                  ~tail:left_parameters
            | left, Parameter.Variable (Map map) :: right_parameters ->
                solve_parameters_against_list_variadic
                  ~is_lower_bound:false
                  ~concretes:left
                  ~variable:(Map map)
                  ~tail:right_parameters
            | Parameter.Variable (Map map) :: left_parameters, right ->
                solve_parameters_against_list_variadic
                  ~is_lower_bound:true
                  ~concretes:right
                  ~variable:(Map map)
                  ~tail:left_parameters
            | Parameter.Variable _ :: left_parameters, []
            | Parameter.Keywords _ :: left_parameters, [] ->
                solve_parameters ~left_parameters ~right_parameters:[] constraints
            | ( Parameter.Variable (Concrete variable_annotation)
                :: Parameter.Keywords keywords_annotation :: _,
                Parameter.Named { annotation = named_annotation; _ } :: right_parameters ) ->
                (* SOLVE *)
                let is_compatible =
                  Type.equal variable_annotation keywords_annotation
                  && always_less_or_equal order ~left:named_annotation ~right:keywords_annotation
                in
                if is_compatible then
                  solve_parameters ~left_parameters ~right_parameters constraints
                else
                  []
            | left :: left_parameters, [] ->
                if Parameter.default left then
                  solve_parameters ~left_parameters ~right_parameters:[] constraints
                else
                  []
            | [], [] -> [constraints]
            | _ -> []
          in
          match implementation.parameters, called_as.parameters with
          | Undefined, Undefined -> [initial_constraints]
          | Defined implementation, Defined called_as ->
              solve_parameters
                ~left_parameters:implementation
                ~right_parameters:called_as
                initial_constraints
          (* We exhibit unsound behavior when a concrete callable is passed into one expecting a
             Callable[..., T] - Callable[..., T] admits any parameters, which is not true when a
             callable that doesn't admit kwargs and varargs is passed in. We need this since there
             is no good way of representing "leave the parameters alone and change the return type"
             in the Python type system at the moment. *)
          | Defined _, Undefined -> [initial_constraints]
          | Undefined, Defined _ -> [initial_constraints]
          | bound, ParameterVariadicTypeVariable variable
            when Type.Variable.Variadic.Parameters.is_free variable ->
              let pair = Type.Variable.ParameterVariadicPair (variable, bound) in
              OrderedConstraints.add_upper_bound initial_constraints ~order ~pair |> Option.to_list
          | ParameterVariadicTypeVariable left, ParameterVariadicTypeVariable right
            when Type.Variable.Variadic.Parameters.equal left right ->
              [initial_constraints]
          | _, _ -> []
        with
        | _ -> []
      in
      let overload_to_instantiated_return_and_altered_constraints overload =
        let namespace = Type.Variable.Namespace.create_fresh () in
        let namespaced_variables =
          Type.Callable
            {
              Type.Callable.kind = Anonymous;
              implementation = overload;
              overloads = [];
              implicit = None;
            }
          |> Type.Variable.all_free_variables
          |> List.map ~f:(Type.Variable.namespace ~namespace)
        in
        let overload =
          map_implementation overload ~f:(Type.Variable.namespace_all_free_variables ~namespace)
        in
        let does_not_leak_namespaced_variables (external_constraints, _) =
          not
            (TypeConstraints.exists_in_bounds external_constraints ~variables:namespaced_variables)
        in
        let instantiate_return (external_constraints, partial_solution) =
          let instantiated_return =
            TypeConstraints.Solution.instantiate partial_solution overload.annotation
            |> Type.Variable.mark_all_free_variables_as_escaped ~specific:namespaced_variables
          in
          instantiated_return, external_constraints
        in
        solve overload ~initial_constraints:constraints
        |> List.map
             ~f:
               (OrderedConstraints.extract_partial_solution ~order ~variables:namespaced_variables)
        |> List.concat_map ~f:Option.to_list
        |> List.filter ~f:does_not_leak_namespaced_variables
        |> List.map ~f:instantiate_return
      in
      let overloads =
        if List.is_empty overloads then
          [implementation]
        else if Type.Callable.Overload.is_undefined implementation then
          overloads
        else
          (* TODO(T41195241) always ignore implementation when has overloads. Currently put
             implementation as last resort *)
          overloads @ [implementation]
      in
      List.concat_map overloads ~f:overload_to_instantiated_return_and_altered_constraints


    (* ## solve_less_or_equal, a brief explanation: ##

       At the heart of our handling of generics is this function, solve_less_or_equal.

       This function takes:

       * a statement of the form F(T1, T2, ... Tn) =<= G(T1, T2, ... Tn), where F and G are types
       which may contain any number of free type variables. In this context a free type variable is
       one whose value we are trying to determine. This could be a function level generic, an
       "escaped" type variable from some sort of incomplete initialization, or even some sort of
       synthetic type variable we're using to answer a question like, "if this is an iterable, what
       kind of iterable is it?" for correctly handling *args parameters.

       * a precondition set of constraints (as defined in TypeConstraints.mli) from a previous call
       to solve_less_or_equal (or from somewhere else). This is how you're able to define
       conjunctions of =<= statements, as when you are trying to satisfy a number of argument <->
       parameter pairs in signature selection

       and returns:

       * an arbitrarily ordered list of constraints (again as defined in Typeconstraints.mli) that
       each are sufficient to satisfy the given statement and the precondition constraints. If this
       list is empty, there is no way to satify those requirements (at least as well as we can know
       that).

       The general strategy undertaken to achieve this behavior is to pairwise recursively break up
       the types in the same way, e.g. we expect to get a tuple on the right if we have a tuple on
       the left, and to handle that by enforcing that each of the contained types be less than
       their pair on the other side (as tuples are covariant). Certain pairs, such as X =<=
       Union[...] or comparing callables with overloads naturally create multiple disjoint
       possibilities which give rise to the list of constraints that we end up returning.

       Once you have enforced all of the statements you would like to ensure are true, you can
       extract possible solutions to the constraints set you have built up with List.filter_map
       ~f:OrderedConstraints.solve *)
    and solve_less_or_equal
        ({ handler; constructor; any_is_bottom; is_protocol; protocol_assumptions; _ } as order)
        ~constraints
        ~left
        ~right
      =
      match left, right with
      | _, _ when Type.equal left right -> [constraints]
      | _, Type.Primitive "object"
      | _, Type.Any ->
          [constraints]
      | other, Type.Top ->
          if Type.exists other ~predicate:(fun annotation -> Type.equal annotation Type.undeclared)
          then
            []
          else
            [constraints]
      | Type.ParameterVariadicComponent _, _
      | _, Type.ParameterVariadicComponent _ ->
          []
      | Type.Annotated left, _ -> solve_less_or_equal order ~constraints ~left ~right
      | _, Type.Annotated right -> solve_less_or_equal order ~constraints ~left ~right
      | Type.Any, _ when any_is_bottom -> [constraints]
      | Type.Variable left_variable, Type.Variable right_variable
        when Type.Variable.Unary.is_free left_variable
             && Type.Variable.Unary.is_free right_variable ->
          (* Either works because constraining V1 to be less or equal to V2 implies that V2 is
             greater than or equal to V1. Therefore either constraint is sufficient, and we should
             consider both. This approach simplifies things downstream for the constraint solver *)
          let right_greater_than_left, left_less_than_right =
            ( OrderedConstraints.add_lower_bound
                constraints
                ~order
                ~pair:(Type.Variable.UnaryPair (right_variable, left))
              |> Option.to_list,
              OrderedConstraints.add_upper_bound
                constraints
                ~order
                ~pair:(Type.Variable.UnaryPair (left_variable, right))
              |> Option.to_list )
          in
          right_greater_than_left @ left_less_than_right
      | Type.Variable variable, bound when Type.Variable.Unary.is_free variable ->
          let pair = Type.Variable.UnaryPair (variable, bound) in
          OrderedConstraints.add_upper_bound constraints ~order ~pair |> Option.to_list
      | bound, Type.Variable variable when Type.Variable.Unary.is_free variable ->
          let pair = Type.Variable.UnaryPair (variable, bound) in
          OrderedConstraints.add_lower_bound constraints ~order ~pair |> Option.to_list
      | Type.Any, _
      | _, Type.Bottom
      | Type.Top, _ ->
          []
      | Type.Bottom, _ -> [constraints]
      | Type.Callable _, Type.Primitive protocol when is_protocol right ~protocol_assumptions ->
          if instantiate_protocol_parameters order ~protocol ~candidate:left = Some (Concrete [])
          then
            [constraints]
          else
            []
      | Type.Callable _, Type.Parametric { name; _ } when is_protocol right ~protocol_assumptions
        ->
          instantiate_protocol_parameters order ~protocol:name ~candidate:left
          >>| Type.parametric name
          >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
          |> Option.value ~default:[]
      | Type.Union lefts, right ->
          solve_ordered_types_less_or_equal
            order
            ~left:(Concrete lefts)
            ~right:(Concrete (List.map lefts ~f:(fun _ -> right)))
            ~constraints
      (* We have to consider both the variables' constraint and its full value against the union. *)
      | Type.Variable bound_variable, Type.Union union ->
          solve_less_or_equal
            order
            ~constraints
            ~left:(Type.Variable.Unary.upper_bound bound_variable)
            ~right
          @ List.concat_map
              ~f:(fun right -> solve_less_or_equal order ~constraints ~left ~right)
              union
      | _, Type.Union rights ->
          if
            Type.Variable.all_variables_are_resolved left
            && Type.Variable.all_variables_are_resolved right
          then
            (* This is a pure performance optimization, but is practically mandatory because there
               are some humongous unions out there *)
            let simple_solve right =
              solve_less_or_equal order ~constraints ~left ~right |> List.is_empty |> not
            in
            if List.exists rights ~f:simple_solve then
              [constraints]
            else
              []
          else
            List.concat_map rights ~f:(fun right ->
                solve_less_or_equal order ~constraints ~left ~right)
      | _, Type.Parametric { name = right_name; parameters = right_parameters } ->
          let solve_parameters left_parameters =
            let handle_variables variables =
              match variables, left_parameters, right_parameters with
              | ( ClassHierarchy.Unaries variables,
                  Type.OrderedTypes.Concrete left_parameters,
                  Concrete right_parameters ) ->
                  let solve_parameter_pair constraints (variable, (left, right)) =
                    match left, right, variable with
                    (* TODO kill these special cases *)
                    | Type.Bottom, _, _ ->
                        (* T[Bottom] is a subtype of T[_T2], for any _T2 and regardless of its
                           variance. *)
                        constraints
                    | _, Type.Top, _ ->
                        (* T[_T2] is a subtype of T[Top], for any _T2 and regardless of its
                           variance. *)
                        constraints
                    | Type.Top, _, _ -> []
                    | _, _, { Type.Variable.Unary.variance = Covariant; _ } ->
                        constraints
                        |> List.concat_map ~f:(fun constraints ->
                               solve_less_or_equal order ~constraints ~left ~right)
                    | _, _, { variance = Contravariant; _ } ->
                        constraints
                        |> List.concat_map ~f:(fun constraints ->
                               solve_less_or_equal order ~constraints ~left:right ~right:left)
                    | _, _, { variance = Invariant; _ } ->
                        constraints
                        |> List.concat_map ~f:(fun constraints ->
                               solve_less_or_equal order ~constraints ~left ~right)
                        |> List.concat_map ~f:(fun constraints ->
                               solve_less_or_equal order ~constraints ~left:right ~right:left)
                  in
                  let zip_on_parameters variables =
                    match List.zip left_parameters right_parameters with
                    | Unequal_lengths -> None
                    | Ok zipped -> (
                      match List.zip variables zipped with
                      | Unequal_lengths -> None
                      | Ok zipped -> Some zipped )
                  in
                  variables
                  |> zip_on_parameters
                  >>| List.fold ~f:solve_parameter_pair ~init:[constraints]
              | Unaries _, Any, _
              | Unaries _, Variable _, _
              | Unaries _, Map _, _ ->
                  (* These should be impossible, since we shouldn't be propagating a variadic into
                     something that is defined to be not variadic *)
                  None
              | Unaries _, _, Variable _
              | Unaries _, _, Any
              | Unaries _, _, Map _ ->
                  (* These should be impossible, since we shouldn't be able to have a not variadic
                     primitive paired with a variadic in a parametric *)
                  None
              | ListVariadic _, left_parameters, right_parameters ->
                  (* TODO(T47346673): currently all variadics are invariant, revisit this when we
                     add variance *)
                  solve_ordered_types_less_or_equal
                    order
                    ~constraints
                    ~left:left_parameters
                    ~right:right_parameters
                  |> List.concat_map ~f:(fun constraints ->
                         solve_ordered_types_less_or_equal
                           order
                           ~constraints
                           ~left:right_parameters
                           ~right:left_parameters)
                  |> Option.some
            in
            ClassHierarchy.variables handler right_name >>= handle_variables
          in
          let parameters : Type.OrderedTypes.t option =
            let parameters =
              ClassHierarchy.instantiate_successors_parameters
                handler
                ~source:left
                ~target:right_name
            in
            match parameters with
            | None when is_protocol right ~protocol_assumptions ->
                instantiate_protocol_parameters order ~protocol:right_name ~candidate:left
            | _ -> parameters
          in
          parameters >>= solve_parameters |> Option.value ~default:[]
      | Type.Primitive source, Type.Primitive target
      | Type.Parametric { name = source; _ }, Type.Primitive target ->
          if ClassHierarchy.is_transitive_successor handler ~source ~target then
            [constraints]
          else if
            is_protocol right ~protocol_assumptions
            && instantiate_protocol_parameters order ~candidate:left ~protocol:target
               = Some (Concrete [])
          then
            [constraints]
          else
            []
      (* We have to consider both the variables' constraint and its full value against the
         optional. *)
      | Type.Variable variable, Type.Optional optional ->
          solve_less_or_equal order ~constraints ~left ~right:optional
          @ solve_less_or_equal
              order
              ~constraints
              ~left:(Type.Variable.Unary.upper_bound variable)
              ~right
      (* A <= B -> A <= Optional[B].*)
      | Optional left, Optional right
      | left, Optional right
      | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
          solve_less_or_equal order ~constraints ~left ~right
      | Optional _, _ -> []
      | Type.Tuple (Type.Bounded lefts), Type.Tuple (Type.Unbounded right) ->
          let left = Type.OrderedTypes.union_upper_bound lefts in
          solve_less_or_equal order ~constraints ~left ~right
      | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right) ->
          solve_ordered_types_less_or_equal order ~left ~right ~constraints
      | Type.Tuple (Type.Unbounded parameter), Type.Primitive _ ->
          solve_less_or_equal
            order
            ~constraints
            ~left:(Type.parametric "tuple" (Concrete [parameter]))
            ~right
      | Type.Tuple (Type.Bounded (Concrete (left :: tail))), Type.Primitive _ ->
          let parameter = List.fold ~f:(join order) ~init:left tail in
          solve_less_or_equal
            order
            ~constraints
            ~left:(Type.parametric "tuple" (Concrete [parameter]))
            ~right
      | Type.Primitive name, Type.Tuple _ ->
          if Type.Primitive.equal name "tuple" then [constraints] else []
      | Type.Tuple _, _
      | _, Type.Tuple _ ->
          []
      | Type.Variable bound_variable, _ ->
          solve_less_or_equal
            order
            ~constraints
            ~left:(Type.Variable.Unary.upper_bound bound_variable)
            ~right
      | _, Type.Variable _bound_variable -> []
      | ( Type.Callable { Callable.kind = Callable.Named left; _ },
          Type.Callable { Callable.kind = Callable.Named right; _ } )
        when Reference.equal left right ->
          [constraints]
      | Type.Callable callable, Type.Callable { implementation; overloads; _ } ->
          let fold_overload sofar called_as =
            let call_as_overload constraints =
              simulate_signature_select order ~callable ~called_as ~constraints
              |> List.concat_map ~f:(fun (left, constraints) ->
                     solve_less_or_equal order ~constraints ~left ~right:called_as.annotation)
            in
            List.concat_map sofar ~f:call_as_overload
          in
          List.fold (implementation :: overloads) ~f:fold_overload ~init:[constraints]
      | _, Type.Callable _ when Type.is_meta left ->
          Type.single_parameter left
          |> constructor ~protocol_assumptions
          >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
          |> Option.value ~default:[]
      | left, Type.Callable _ -> (
          let joined =
            join order (Type.parametric "typing.Callable" (Concrete [Type.Bottom])) left
          in
          match joined with
          | Type.Parametric { name; parameters = Concrete [left] }
            when Identifier.equal name "typing.Callable" ->
              solve_less_or_equal order ~constraints ~left ~right
          | _ -> [] )
      | Type.Callable _, _ -> []
      | Type.TypedDictionary left, Type.TypedDictionary right ->
          let field_not_found field =
            not (List.exists left.fields ~f:(Type.equal_typed_dictionary_field field))
          in
          if left.total = right.total && not (List.exists right.fields ~f:field_not_found) then
            [constraints]
          else
            []
      | Type.TypedDictionary _, _ ->
          solve_less_or_equal order ~constraints ~left:(Type.Primitive "TypedDictionary") ~right
      | _, Type.TypedDictionary _ ->
          solve_less_or_equal order ~constraints ~left ~right:(Type.Primitive "TypedDictionary")
      | _, Type.Literal _ -> []
      | Type.Literal _, _ ->
          solve_less_or_equal order ~constraints ~left:(Type.weaken_literals left) ~right


    and always_less_or_equal order ~left ~right =
      solve_less_or_equal
        order
        ~constraints:TypeConstraints.empty
        ~left:(Type.Variable.mark_all_variables_as_bound left)
        ~right:(Type.Variable.mark_all_variables_as_bound right)
      |> List.is_empty
      |> not


    and solve_ordered_types_less_or_equal order ~left ~right ~constraints =
      let solve_against_map ~is_lower_bound ~bound ~map =
        let variable = Type.OrderedTypes.Map.variable map in
        if Type.Variable.Variadic.List.is_free variable then
          (* Our strategy for solving Concrete[X0, X1, ... Xn] <: Map[mapper, mapped_var]
           *   is as follows:
           * construct n "synthetic" unary type variables
           * substitute them through the map, generating
           *   mapper[Synth0], mapper[Synth1], ... mapper[SynthN]
           * pairwise solve the concrete memebers against the synthetics:
           *   X0 <: mapper[Synth0] && X1 <: mapper[Synth1] && ... Xn <: Mapper[SynthN]
           * Solve the resulting constraints to Soln
           * Add both upper and lower bounds on mapped_var to be
           *   Soln[Synth0], Soln[Synth1], ... Soln[SynthN]
           *)
          let synthetic_variables, synthetic_variable_constraints_set =
            let namespace = Type.Variable.Namespace.create_fresh () in
            let synthetic_solve index (synthetics_created_sofar, constraints_set) concrete =
              let new_synthetic_variable =
                Type.Variable.Unary.create (Int.to_string index)
                |> Type.Variable.Unary.namespace ~namespace
              in
              let solve_against_concrete constraints =
                let generated =
                  Type.OrderedTypes.Map.singleton_replace_variable
                    map
                    ~replacement:(Type.Variable new_synthetic_variable)
                in
                let left, right =
                  if is_lower_bound then
                    concrete, generated
                  else
                    generated, concrete
                in
                solve_less_or_equal order ~constraints ~left ~right
              in
              ( new_synthetic_variable :: synthetics_created_sofar,
                List.concat_map constraints_set ~f:solve_against_concrete )
            in
            List.foldi bound ~f:synthetic_solve ~init:([], [constraints])
          in
          let consider_synthetic_variable_constraints synthetic_variable_constraints =
            let instantiate_synthetic_variables solution =
              List.map
                synthetic_variables
                ~f:(TypeConstraints.Solution.instantiate_single_variable solution)
              |> Option.all
            in
            let add_bound concrete =
              let add_bound ~adder constraints =
                adder
                  constraints
                  ~order
                  ~pair:(Type.Variable.ListVariadicPair (variable, concrete))
              in
              add_bound ~adder:OrderedConstraints.add_lower_bound constraints
              >>= add_bound ~adder:OrderedConstraints.add_upper_bound
            in
            OrderedConstraints.solve ~order synthetic_variable_constraints
            >>= instantiate_synthetic_variables
            >>| List.rev
            >>| (fun substituted -> Type.Record.OrderedTypes.Concrete substituted)
            >>= add_bound
          in
          List.filter_map
            synthetic_variable_constraints_set
            ~f:consider_synthetic_variable_constraints
        else
          []
      in
      let open Type.OrderedTypes in
      let open Type.Variable.Variadic.List in
      match left, right with
      | Variable left_variable, Variable right_variable
        when is_free left_variable && is_free right_variable ->
          (* Just as with unaries, we need to consider both possibilities *)
          let right_greater_than_left, left_less_than_right =
            ( OrderedConstraints.add_lower_bound
                constraints
                ~order
                ~pair:(Type.Variable.ListVariadicPair (right_variable, Variable left_variable))
              |> Option.to_list,
              OrderedConstraints.add_upper_bound
                constraints
                ~order
                ~pair:(Type.Variable.ListVariadicPair (left_variable, Variable right_variable))
              |> Option.to_list )
          in
          right_greater_than_left @ left_less_than_right
      | Variable variable, bound_variable_or_concrete when is_free variable ->
          OrderedConstraints.add_upper_bound
            constraints
            ~order
            ~pair:(Type.Variable.ListVariadicPair (variable, bound_variable_or_concrete))
          |> Option.to_list
      | bound_variable_or_concrete, Variable variable when is_free variable ->
          OrderedConstraints.add_lower_bound
            constraints
            ~order
            ~pair:(Type.Variable.ListVariadicPair (variable, bound_variable_or_concrete))
          |> Option.to_list
      | Any, _
      | _, Any ->
          [constraints]
      | Variable left_bound_variable, Variable right_bound_variable ->
          if equal left_bound_variable right_bound_variable then
            [constraints]
          else
            []
      | Variable _bound_variable, Concrete _
      | Concrete _, Variable _bound_variable ->
          []
      | Concrete lefts, Concrete rights -> (
          let folded_constraints =
            let solve_pair constraints left right =
              constraints
              |> List.concat_map ~f:(fun constraints ->
                     solve_less_or_equal order ~constraints ~left ~right)
            in
            List.fold2 ~init:[constraints] ~f:solve_pair lefts rights
          in
          match folded_constraints with
          | List.Or_unequal_lengths.Ok constraints -> constraints
          | List.Or_unequal_lengths.Unequal_lengths -> [] )
      | Concrete bound, Map map -> solve_against_map ~is_lower_bound:true ~bound ~map
      | Map map, Concrete bound -> solve_against_map ~is_lower_bound:false ~bound ~map
      | Variable _bound_variable, Map _
      | Map _, Variable _bound_variable ->
          []
      | Map left, Map right ->
          if Type.OrderedTypes.Map.equal left right then
            [constraints]
          else
            []


    and join_implementations ~parameter_join ~return_join order left right =
      let open Callable in
      let parameters =
        match left.parameters, right.parameters with
        | Undefined, Undefined -> Some Undefined
        | Defined left, Defined right -> (
          try
            let join_parameter sofar left right =
              match sofar with
              | Some sofar ->
                  let joined =
                    if Type.Callable.Parameter.names_compatible left right then
                      match left, right with
                      | Parameter.Anonymous left, Parameter.Anonymous right
                        when left.default = right.default ->
                          Some
                            (Parameter.Anonymous
                               {
                                 left with
                                 annotation = parameter_join order left.annotation right.annotation;
                               })
                      | Parameter.Anonymous anonymous, Parameter.Named named
                      | Parameter.Named named, Parameter.Anonymous anonymous
                        when named.default = anonymous.default ->
                          Some
                            (Parameter.Anonymous
                               {
                                 anonymous with
                                 annotation =
                                   parameter_join order named.annotation anonymous.annotation;
                               })
                      | Parameter.Named left, Parameter.Named right
                        when left.default = right.default ->
                          Some
                            (Parameter.Named
                               {
                                 left with
                                 annotation = parameter_join order left.annotation right.annotation;
                               })
                      | Parameter.Variable (Concrete left), Parameter.Variable (Concrete right) ->
                          Some (Parameter.Variable (Concrete (parameter_join order left right)))
                      | Parameter.Keywords left, Parameter.Keywords right ->
                          Some (Parameter.Keywords (parameter_join order left right))
                      | _ -> None
                    else
                      None
                  in
                  joined >>| fun joined -> joined :: sofar
              | None -> None
            in
            List.fold2_exn ~init:(Some []) ~f:join_parameter left right
            >>| List.rev
            >>| fun parameters -> Defined parameters
          with
          | _ -> None )
        | Undefined, Defined right -> Some (Defined right)
        | Defined left, Undefined -> Some (Defined left)
        | _ -> None
      in
      parameters
      >>| fun parameters ->
      {
        annotation = return_join order left.annotation right.annotation;
        parameters;
        define_location = None;
      }


    and join
        ( {
            handler = (module Handler : ClassHierarchy.Handler) as handler;
            constructor;
            is_protocol;
            protocol_assumptions;
            _;
          } as order )
        left
        right
      =
      let union = Type.union [left; right] in
      if Type.equal left right then
        left
      else if
        Type.Variable.contains_escaped_free_variable left
        || Type.Variable.contains_escaped_free_variable right
      then
        union
      else
        match left, right with
        | Type.Bottom, other
        | other, Type.Bottom ->
            other
        | undeclared, _ when Type.equal undeclared Type.undeclared -> Type.union [left; right]
        | _, undeclared when Type.equal undeclared Type.undeclared -> Type.union [left; right]
        | Type.Top, _
        | _, Type.Top ->
            Type.Top
        | Type.Any, _
        | _, Type.Any ->
            Type.Any
        | Type.ParameterVariadicComponent _, _
        | _, Type.ParameterVariadicComponent _ ->
            union
        | Type.Annotated left, _ -> Type.annotated (join order left right)
        | _, Type.Annotated right -> Type.annotated (join order left right)
        (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
        | Type.Union left, Type.Union right -> Type.union (left @ right)
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) -> (
            if always_less_or_equal order ~left:other ~right:union then
              union
            else
              match other with
              | Type.Optional Type.Bottom -> Type.Optional union
              | Type.Optional element -> Type.Optional (Type.union (element :: elements))
              | _ ->
                  List.map elements ~f:(join order other)
                  |> List.fold ~f:(join order) ~init:Type.Bottom )
        | _, Type.Variable _
        | Type.Variable _, _ ->
            union
        | ( Type.Parametric { name = left_primitive; _ },
            Type.Parametric { name = right_primitive; _ } )
        | Type.Parametric { name = left_primitive; _ }, Type.Primitive right_primitive
        | Type.Primitive left_primitive, Type.Parametric { name = right_primitive; _ } ->
            if always_less_or_equal order ~left ~right then
              right
            else if always_less_or_equal order ~left:right ~right:left then
              left
            else
              let target =
                try
                  if
                    always_less_or_equal
                      ~left:(Primitive left_primitive)
                      ~right:(Primitive right_primitive)
                      order
                  then
                    Some right_primitive
                  else if
                    always_less_or_equal
                      ~left:(Primitive right_primitive)
                      ~right:(Primitive left_primitive)
                      order
                  then
                    Some left_primitive
                  else
                    match join order (Primitive left_primitive) (Primitive right_primitive) with
                    | Primitive target -> Some target
                    | _ -> None
                with
                | ClassHierarchy.Untracked _ -> None
              in
              let handle_target target =
                if Handler.contains (Handler.indices ()) target then
                  let left_parameters =
                    ClassHierarchy.instantiate_successors_parameters handler ~source:left ~target
                  in
                  let right_parameters =
                    ClassHierarchy.instantiate_successors_parameters handler ~source:right ~target
                  in
                  let variables = ClassHierarchy.variables handler target in
                  let parameters =
                    let join_parameters left right { Type.Variable.Unary.variance; _ } =
                      match left, right, variance with
                      | Type.Bottom, other, _
                      | other, Type.Bottom, _ ->
                          other
                      | Type.Top, _, _
                      | _, Type.Top, _ ->
                          Type.Top
                      | _, _, Covariant -> join order left right
                      | _, _, Contravariant -> meet order left right
                      | _, _, Invariant ->
                          if
                            always_less_or_equal order ~left ~right
                            && always_less_or_equal order ~left:right ~right:left
                          then
                            left
                          else
                            (* We fallback to Type.Any if type equality fails to help display
                               meaningful error messages. *)
                            Type.Any
                    in
                    match left_parameters, right_parameters, variables with
                    | Some (Concrete left), Some (Concrete right), Some (Unaries variables)
                      when List.length left = List.length right
                           && List.length left = List.length variables ->
                        let join_parameters left right variable =
                          let replace_free_unary_variables_with_top =
                            let replace_if_free variable =
                              Option.some_if (Type.Variable.Unary.is_free variable) Type.Top
                            in
                            Type.Variable.GlobalTransforms.Unary.replace_all replace_if_free
                          in
                          join_parameters left right variable
                          |> replace_free_unary_variables_with_top
                        in
                        Some
                          (Type.OrderedTypes.Concrete
                             (List.map3_exn ~f:join_parameters left right variables))
                    | _ ->
                        (* TODO(T47348395): Implement joining for variadics *)
                        None
                  in
                  match parameters with
                  | Some parameters -> Type.Parametric { name = target; parameters }
                  | None -> Type.Primitive target
                else (* TODO(T41082573) throw here instead of unioning *)
                  union
              in
              target >>| handle_target |> Option.value ~default:union
        (* Special case joins of optional collections with their uninstantated counterparts. *)
        | ( Type.Parametric ({ parameters = Concrete [Type.Bottom]; _ } as other),
            Type.Optional
              (Type.Parametric ({ parameters = Concrete [parameter]; _ } as collection)) )
        | ( Type.Optional (Type.Parametric ({ parameters = Concrete [parameter]; _ } as collection)),
            Type.Parametric ({ parameters = Concrete [Type.Bottom]; _ } as other) )
          when Identifier.equal other.name collection.name ->
            Type.Parametric { other with parameters = Concrete [parameter] }
        (* A <= B -> lub(A, Optional[B]) = Optional[B]. *)
        | other, Type.Optional parameter
        | Type.Optional parameter, other ->
            Type.optional (join order other parameter)
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded (Variable _)), other
        | other, Type.Tuple (Type.Bounded (Variable _)) ->
            join order other (Type.Tuple (Type.Unbounded Type.object_primitive))
        | Type.Tuple (Type.Bounded (Concrete left)), Type.Tuple (Type.Bounded (Concrete right))
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(join order) |> Type.tuple
        | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
            Type.Tuple (Type.Unbounded (join order left right))
        | Type.Tuple (Type.Bounded (Concrete (left :: tail))), Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (Concrete (left :: tail)))
          when List.for_all ~f:(fun element -> Type.equal element left) tail
               && always_less_or_equal order ~left ~right ->
            Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded parameter), (Type.Parametric _ as annotation)
        | Type.Tuple (Type.Unbounded parameter), (Type.Primitive _ as annotation)
        | (Type.Parametric _ as annotation), Type.Tuple (Type.Unbounded parameter)
        | (Type.Primitive _ as annotation), Type.Tuple (Type.Unbounded parameter) ->
            join order (Type.parametric "tuple" (Concrete [parameter])) annotation
        | Type.Tuple (Type.Bounded (Concrete parameters)), (Type.Parametric _ as annotation) ->
            (* Handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
            let parameter = List.fold ~init:Type.Bottom ~f:(join order) parameters in
            join order (Type.parametric "tuple" (Concrete [parameter])) annotation
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            Type.union [left; right]
        | ( (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            callable
        | ( Type.TypedDictionary { fields = left_fields; total = left_total; _ },
            Type.TypedDictionary { fields = right_fields; total = right_total; _ } ) ->
            if
              Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields
              || left_total <> right_total
            then
              Type.Parametric
                { name = "typing.Mapping"; parameters = Concrete [Type.string; Type.Any] }
            else
              let join_fields =
                if always_less_or_equal order ~left ~right then
                  right_fields
                else if always_less_or_equal order ~left:right ~right:left then
                  left_fields
                else
                  let found_match field =
                    List.exists left_fields ~f:(Type.equal_typed_dictionary_field field)
                  in
                  List.filter right_fields ~f:found_match
              in
              Type.TypedDictionary.anonymous ~total:left_total join_fields
        | Type.TypedDictionary _, other
        | other, Type.TypedDictionary _ ->
            let class_join = join order (Type.Primitive "TypedDictionary") other in
            let failed =
              Type.exists class_join ~predicate:(function
                  | Type.Primitive "TypedDictionary" -> true
                  | _ -> false)
            in
            if failed then union else class_join
        | Type.Callable left, Type.Callable right ->
            if List.is_empty left.Callable.overloads && List.is_empty right.Callable.overloads then
              let kind =
                if Type.Callable.equal_kind left.kind right.kind then
                  left.kind
                else
                  Type.Callable.Anonymous
              in
              join_implementations
                ~parameter_join:meet
                ~return_join:join
                order
                left.Callable.implementation
                right.Callable.implementation
              >>| (fun implementation -> Type.Callable { left with Callable.kind; implementation })
              |> Option.value ~default:union
            else
              union
        | Type.Callable callable, other
        | other, Type.Callable callable ->
            let default =
              let other =
                join order (Type.parametric "typing.Callable" (Concrete [Type.Bottom])) other
              in
              match other with
              | Type.Parametric { name; parameters = Concrete [other_callable] }
                when Identifier.equal name "typing.Callable" ->
                  join order (Type.Callable callable) other_callable
              | _ -> Type.union [left; right]
            in
            Option.some_if (Type.is_meta other) other
            >>= constructor ~protocol_assumptions
            >>| join order (Type.Callable callable)
            |> Option.value ~default
        | (Type.Literal _ as literal), other
        | other, (Type.Literal _ as literal) ->
            join order other (Type.weaken_literals literal)
        | _ when is_protocol right ~protocol_assumptions && always_less_or_equal order ~left ~right
          ->
            right
        | _
          when is_protocol left ~protocol_assumptions
               && always_less_or_equal order ~left:right ~right:left ->
            left
        | Primitive left, Primitive right -> (
          match List.hd (ClassHierarchy.least_upper_bound handler left right) with
          | Some joined ->
              if Type.Primitive.equal joined left then
                Type.Primitive left
              else if Type.Primitive.equal joined right then
                Type.Primitive right
              else
                union
          | None -> union )


    and meet
        ( {
            handler = (module Handler : ClassHierarchy.Handler) as handler;
            constructor;
            is_protocol;
            protocol_assumptions;
            _;
          } as order )
        left
        right
      =
      if Type.equal left right then
        left
      else
        match left, right with
        | Type.Top, other
        | other, Type.Top ->
            other
        | Type.Any, other
        | other, Type.Any
          when not (Type.is_unknown other) ->
            other
        | Type.Bottom, _
        | _, Type.Bottom ->
            Type.Bottom
        | Type.ParameterVariadicComponent _, _
        | _, Type.ParameterVariadicComponent _ ->
            Type.Bottom
        | Type.Annotated left, _ -> Type.annotated (meet order left right)
        | _, Type.Annotated right -> Type.annotated (meet order left right)
        | (Type.Variable _ as variable), other
        | other, (Type.Variable _ as variable) ->
            if always_less_or_equal order ~left:variable ~right:other then
              variable
            else
              Type.Bottom
        | Type.Union left, Type.Union right ->
            let union =
              Set.inter (Type.Set.of_list left) (Type.Set.of_list right) |> Set.to_list
            in
            Type.union union
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if always_less_or_equal order ~left:other ~right:union then
              other
            else
              List.map elements ~f:(meet order other) |> List.fold ~f:(meet order) ~init:Type.Top
        | ( Type.Parametric { name = left_primitive; _ },
            Type.Parametric { name = right_primitive; _ } ) -> (
            if always_less_or_equal order ~left ~right then
              left
            else if always_less_or_equal order ~left:right ~right:left then
              right
            else
              let target = meet order (Primitive left_primitive) (Primitive right_primitive) in
              match target with
              | Primitive target when Handler.contains (Handler.indices ()) target -> (
                  let step ~predecessor_variables ~parameters =
                    solve_ordered_types_less_or_equal
                      order
                      ~constraints:TypeConstraints.empty
                      ~left:predecessor_variables
                      ~right:parameters
                    |> List.filter_map ~f:(OrderedConstraints.solve ~order)
                    |> List.hd
                  in
                  let left_parameters =
                    ClassHierarchy.instantiate_predecessors_parameters
                      handler
                      ~source:left
                      ~target
                      ~step
                  in
                  let right_parameters =
                    ClassHierarchy.instantiate_predecessors_parameters
                      handler
                      ~source:right
                      ~target
                      ~step
                  in
                  let variables = ClassHierarchy.variables handler target in
                  let parameters =
                    let meet_parameters left right { Type.Variable.Unary.variance; _ } =
                      match left, right, variance with
                      | Type.Bottom, _, _
                      | _, Type.Bottom, _ ->
                          Type.Bottom
                      | Type.Top, other, _
                      | other, Type.Top, _ ->
                          other
                      | Type.Any, _, _
                      | _, Type.Any, _ ->
                          Type.Bottom
                      | _, _, Covariant -> meet order left right
                      | _, _, Contravariant -> join order left right
                      | _, _, Invariant ->
                          if
                            always_less_or_equal order ~left ~right
                            && always_less_or_equal order ~left:right ~right:left
                          then
                            left
                          else
                            (* We fallback to Type.Bottom if type equality fails to help display
                               meaningful error messages. *)
                            Type.Bottom
                    in
                    match left_parameters, right_parameters, variables with
                    | Some (Concrete []), Some (Concrete []), None -> Some []
                    | Some (Concrete left), Some (Concrete right), Some (Unaries variables)
                      when List.length left = List.length right
                           && List.length left = List.length variables ->
                        Some (List.map3_exn ~f:meet_parameters left right variables)
                    | _ ->
                        (* TODO:(T47348519) handle meeting variadics *)
                        None
                  in
                  match parameters with
                  | Some [] -> Type.Primitive target
                  | Some parameters ->
                      Type.Parametric { name = target; parameters = Concrete parameters }
                  | _ -> Type.Bottom )
              | _ -> Type.Bottom )
        (* A <= B -> glb(A, Optional[B]) = A. *)
        | other, Type.Optional parameter
        | Type.Optional parameter, other ->
            if always_less_or_equal order ~left:other ~right:parameter then
              other
            else
              Type.Bottom
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded (Variable _)), _
        | _, Type.Tuple (Type.Bounded (Variable _)) ->
            Type.Bottom
        | Type.Tuple (Type.Bounded (Concrete left)), Type.Tuple (Type.Bounded (Concrete right))
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(meet order) |> Type.tuple
        | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
            Type.Tuple (Type.Unbounded (meet order left right))
        | Type.Tuple (Type.Bounded (Concrete (left :: tail))), Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (Concrete (left :: tail)))
          when List.for_all ~f:(fun element -> Type.equal element left) tail
               && always_less_or_equal order ~left ~right ->
            Type.Tuple (Type.Unbounded left) (* My brain hurts... *)
        | (Type.Tuple _ as tuple), (Type.Parametric _ as parametric)
        | (Type.Parametric _ as parametric), (Type.Tuple _ as tuple) ->
            if always_less_or_equal order ~left:tuple ~right:parametric then
              tuple
            else
              Type.Bottom
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            Type.Bottom
        | Type.Parametric _, Type.Primitive _
        | Type.Primitive _, Type.Parametric _ ->
            if always_less_or_equal order ~left ~right then
              left
            else if always_less_or_equal order ~left:right ~right:left then
              right
            else
              Type.Bottom
        | ( Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as left),
            Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as right) ) ->
            join_implementations
              ~parameter_join:join
              ~return_join:meet
              order
              left.Callable.implementation
              right.Callable.implementation
            >>| (fun implementation -> Type.Callable { left with Callable.implementation })
            |> Option.value ~default:Type.Bottom
        | ( (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            callable
        | Type.Callable callable, other
        | other, Type.Callable callable ->
            Option.some_if (Type.is_meta other) other
            >>= constructor ~protocol_assumptions
            >>| meet order (Type.Callable callable)
            |> Option.value ~default:Type.Bottom
        | ( Type.TypedDictionary { fields = left_fields; total = left_total; _ },
            Type.TypedDictionary { fields = right_fields; total = right_total; _ } ) ->
            if
              Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields
              || left_total <> right_total
            then
              Type.Bottom
            else
              let meet_fields =
                if always_less_or_equal order ~left ~right then
                  left_fields
                else if always_less_or_equal order ~left:right ~right:left then
                  right_fields
                else
                  List.dedup_and_sort
                    (left_fields @ right_fields)
                    ~compare:Type.compare_typed_dictionary_field
              in
              Type.TypedDictionary.anonymous ~total:left_total meet_fields
        | Type.TypedDictionary _, _
        | _, Type.TypedDictionary _ ->
            Type.Bottom
        | Type.Literal _, _
        | _, Type.Literal _ ->
            Type.Bottom
        | Type.Primitive _, _ when always_less_or_equal order ~left ~right -> left
        | _, Type.Primitive _ when always_less_or_equal order ~left:right ~right:left -> right
        | _ when is_protocol right ~protocol_assumptions && always_less_or_equal order ~left ~right
          ->
            left
        | _
          when is_protocol left ~protocol_assumptions
               && always_less_or_equal order ~left:right ~right:left ->
            right
        | Type.Primitive left, Type.Primitive right -> (
          match List.hd (ClassHierarchy.greatest_lower_bound handler left right) with
          | Some node -> Type.Primitive node
          | None -> Type.Bottom )
        | _ ->
            Log.debug "No lower bound found for %a and %a" Type.pp left Type.pp right;
            Type.Bottom


    and instantiate_protocol_parameters
        ( {
            attributes;
            handler = (module Handler : ClassHierarchy.Handler) as handler;
            protocol_assumptions;
            _;
          } as order )
        ~candidate
        ~protocol
        : Type.OrderedTypes.t option
      =
      match candidate with
      | Type.Primitive candidate_name
        when Option.is_some (ClassHierarchy.variables handler candidate_name) ->
          (* If we are given a "stripped" generic, we decline to do structural analysis, as these
             kinds of comparisons only exists for legacy reasons to do nominal comparisons *)
          None
      | Type.Primitive candidate_name when Identifier.equal candidate_name protocol ->
          Some (Concrete [])
      | Type.Parametric { name; parameters } when Identifier.equal name protocol -> Some parameters
      | _ -> (
          let assumed_protocol_parameters =
            ProtocolAssumptions.find_assumed_protocol_parameters
              protocol_assumptions
              ~candidate
              ~protocol
          in
          match assumed_protocol_parameters with
          | Some result -> Some result
          | None -> (
              let protocol_generics = ClassHierarchy.variables handler protocol in
              let new_assumptions =
                let protocol_parameters =
                  match protocol_generics with
                  | Some (Unaries variables) ->
                      List.map variables ~f:(fun variable -> Type.Variable variable)
                      |> fun variables -> Type.OrderedTypes.Concrete variables
                  | Some (ListVariadic variable) -> Variable variable
                  | None -> Concrete []
                in
                ProtocolAssumptions.add
                  protocol_assumptions
                  ~candidate
                  ~protocol
                  ~protocol_parameters
              in
              let protocol_attributes =
                let is_not_object_or_generic_method
                    { Ast.Node.value = { AnnotatedAttribute.parent; _ }; _ }
                  =
                  (not (Type.is_object parent)) && not (Type.is_generic_primitive parent)
                in
                attributes ~protocol_assumptions:new_assumptions (Type.Primitive protocol)
                >>| List.filter ~f:is_not_object_or_generic_method
              in
              let candidate_attributes, desanitize_map =
                match candidate with
                | Type.Callable _ as callable ->
                    let attributes =
                      [ Ast.Node.create_with_default_location
                          {
                            AnnotatedAttribute.annotation = Annotation.create callable;
                            async = false;
                            class_attribute = false;
                            defined = true;
                            final = false;
                            initialized = true;
                            name = "__call__";
                            property = None;
                            parent = callable;
                            static = false;
                            value = Ast.Node.create_with_default_location Expression.Ellipsis;
                          } ]
                      |> Option.some
                    in
                    attributes, []
                | _ ->
                    (* We don't return constraints for the candidate's free variables, so we must
                       underapproximate and determine conformance in the worst case *)
                    let desanitize_map, sanitized_candidate =
                      let namespace = Type.Variable.Namespace.create_fresh () in
                      let module SanitizeTransform = Type.Transform.Make (struct
                        type state = Type.Variable.pair list

                        let visit_children_before _ _ = true

                        let visit_children_after = false

                        let visit sofar = function
                          | Type.Variable variable when Type.Variable.Unary.is_free variable ->
                              let transformed_variable =
                                Type.Variable.Unary.namespace variable ~namespace
                                |> Type.Variable.Unary.mark_as_bound
                              in
                              {
                                Type.Transform.transformed_annotation =
                                  Type.Variable transformed_variable;
                                new_state =
                                  Type.Variable.UnaryPair
                                    (transformed_variable, Type.Variable variable)
                                  :: sofar;
                              }
                          | transformed_annotation ->
                              { Type.Transform.transformed_annotation; new_state = sofar }
                      end)
                      in
                      SanitizeTransform.visit [] candidate
                    in
                    ( attributes ~protocol_assumptions:new_assumptions sanitized_candidate,
                      desanitize_map )
              in
              match candidate_attributes, protocol_attributes with
              | Some all_candidate_attributes, Some all_protocol_attributes ->
                  let build_attribute_map =
                    let add_to_map map data =
                      match Identifier.Map.add map ~key:(AnnotatedAttribute.name data) ~data with
                      | `Ok x -> x
                      (* Attributes are listed in resolution order *)
                      | `Duplicate -> map
                    in
                    List.fold ~f:add_to_map ~init:Identifier.Map.empty
                  in
                  let merge_attributes ~key:_ = function
                    | `Both pair -> Some (`Found pair)
                    (* In candidate but not protocol *)
                    | `Left _ -> None
                    (* In protocol but not candidate *)
                    | `Right _ -> Some `Missing
                  in
                  let order_with_new_assumption =
                    { order with protocol_assumptions = new_assumptions }
                  in
                  let attribute_implements ~key:_ ~data constraints_set =
                    match data with
                    | `Found (candidate_attribute, protocol_attribute) ->
                        let attribute_annotation attribute =
                          AnnotatedAttribute.annotation attribute |> Annotation.annotation
                        in
                        List.concat_map constraints_set ~f:(fun constraints ->
                            solve_less_or_equal
                              order_with_new_assumption
                              ~left:(attribute_annotation candidate_attribute)
                              ~right:(attribute_annotation protocol_attribute)
                              ~constraints)
                    | `Missing -> []
                  in
                  let instantiate_protocol_generics solution =
                    let desanitize =
                      let desanitization_solution =
                        TypeConstraints.Solution.create desanitize_map
                      in
                      TypeConstraints.Solution.instantiate_ordered_types desanitization_solution
                    in
                    let instantiate = function
                      | ClassHierarchy.Unaries variables ->
                          List.map variables ~f:(fun variable -> Type.Variable variable)
                          |> List.map ~f:(TypeConstraints.Solution.instantiate solution)
                          |> fun instantiated -> Type.OrderedTypes.Concrete instantiated
                      | ListVariadic variable ->
                          TypeConstraints.Solution.instantiate_single_list_variadic_variable
                            solution
                            variable
                          |> Option.value ~default:(Type.OrderedTypes.Variable variable)
                    in
                    protocol_generics
                    >>| instantiate
                    >>| desanitize
                    |> Option.value ~default:(Type.OrderedTypes.Concrete [])
                  in
                  Identifier.Map.merge
                    (build_attribute_map all_candidate_attributes)
                    (build_attribute_map all_protocol_attributes)
                    ~f:merge_attributes
                  |> Identifier.Map.fold ~init:[TypeConstraints.empty] ~f:attribute_implements
                  |> List.filter_map ~f:(OrderedConstraints.solve ~order:order_with_new_assumption)
                  |> List.hd
                  >>| instantiate_protocol_generics
              | _ -> None ) )
  end
end

module rec Constraints : OrderedConstraintsType =
  TypeConstraints.OrderedConstraints (Implementation)

and Implementation : FullOrderType = OrderImplementation.Make (Constraints)

module OrderedConstraints = Constraints

module IncludableImplementation : FullOrderTypeWithoutT = Implementation

include IncludableImplementation

let rec is_consistent_with order left right =
  always_less_or_equal { order with any_is_bottom = true } ~left ~right


let rec is_compatible_with order ~left ~right =
  match left, right with
  (* Any *)
  | _, Type.Any
  | Type.Any, _ ->
      true
  (* Top *)
  | _, Type.Top -> true
  | Type.Top, _ -> false
  (* Optional *)
  | Type.Optional left, Type.Optional right -> is_compatible_with order ~left ~right
  | _, Type.Optional parameter -> is_compatible_with order ~left ~right:parameter
  | Type.Optional _, _ -> false
  (* Tuple *)
  | Type.Tuple (Type.Bounded (Concrete left)), Type.Tuple (Type.Bounded (Concrete right))
    when List.length left = List.length right ->
      List.for_all2_exn left right ~f:(fun left right -> is_compatible_with order ~left ~right)
  | Type.Tuple (Type.Bounded (Concrete bounded)), Type.Tuple (Type.Unbounded right) ->
      List.for_all bounded ~f:(fun bounded_type ->
          is_compatible_with order ~left:bounded_type ~right)
  (* Union *)
  | Type.Union left, right ->
      List.fold
        ~init:true
        ~f:(fun current left -> current && is_compatible_with order ~left ~right)
        left
  | left, Type.Union right ->
      List.exists ~f:(fun right -> is_compatible_with order ~left ~right) right
  (* Parametric *)
  | ( Parametric { name = left_name; parameters = Concrete left_parameters },
      Parametric { name = right_name; parameters = Concrete right_parameters } )
    when Type.Primitive.equal left_name right_name
         && Int.equal (List.length left_parameters) (List.length right_parameters) ->
      List.for_all2_exn left_parameters right_parameters ~f:(fun left right ->
          is_compatible_with order ~left ~right)
  (* Fallback *)
  | _, _ -> is_consistent_with order left right


let rec consistent_solution_exists order left right =
  let order = { order with any_is_bottom = true } in
  solve_less_or_equal order ~left ~right ~constraints:TypeConstraints.empty
  |> List.filter_map ~f:(OrderedConstraints.solve ~order)
  |> List.is_empty
  |> not


let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next
