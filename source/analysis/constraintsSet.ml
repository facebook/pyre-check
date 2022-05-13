(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Assumptions

type class_hierarchy = {
  instantiate_successors_parameters:
    source:Type.t -> target:Type.Primitive.t -> Type.Parameter.t list option;
  is_transitive_successor: source:Type.Primitive.t -> target:Type.Primitive.t -> bool;
  variables: Type.Primitive.t -> Type.Variable.t list option;
  least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t list;
}

type order = {
  class_hierarchy: class_hierarchy;
  all_attributes:
    Type.t -> assumptions:Assumptions.t -> AnnotatedAttribute.instantiated list option;
  attribute:
    Type.t ->
    assumptions:Assumptions.t ->
    name:Ast.Identifier.t ->
    AnnotatedAttribute.instantiated option;
  is_protocol: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> bool;
  get_typed_dictionary: Type.t -> Type.t Type.Record.TypedDictionary.record option;
  metaclass: Type.Primitive.t -> assumptions:Assumptions.t -> Type.t option;
  assumptions: Assumptions.t;
}

module Solution = struct
  (* For now we're just arbitrarily picking the first solution, but we want to allow us the
     opportunity to augment that behavior in the future *)
  include TypeConstraints.Solution
end

type t = TypeConstraints.t list [@@deriving show]

let empty = [TypeConstraints.empty]

let impossible = []

let potentially_satisfiable constraints_set = List.is_empty constraints_set |> not

type kind =
  | LessOrEqual of {
      left: Type.t;
      right: Type.t;
    }
  | OrderedTypesLessOrEqual of {
      left: Type.OrderedTypes.t;
      right: Type.OrderedTypes.t;
    }
  | VariableIsExactly of Type.Variable.pair

module type OrderedConstraintsSetType = sig
  val add : t -> new_constraint:kind -> order:order -> t

  val solve : ?only_solve_for:Type.Variable.t list -> t -> order:order -> Solution.t option

  val get_parameter_specification_possibilities
    :  t ->
    order:order ->
    parameter_specification:Type.Variable.Variadic.Parameters.t ->
    Type.Callable.parameters list

  val instantiate_protocol_parameters
    :  order ->
    candidate:Type.t ->
    protocol:Ast.Identifier.t ->
    Type.Parameter.t list option
end

let resolve_callable_protocol
    ~assumption
    ~order:{ attribute; assumptions = { callable_assumptions; _ } as assumptions; _ }
    annotation
  =
  match
    CallableAssumptions.find_assumed_callable_type ~candidate:annotation callable_assumptions
  with
  | Some assumption -> Some assumption
  | None -> (
      let new_assumptions =
        {
          assumptions with
          callable_assumptions =
            CallableAssumptions.add ~candidate:annotation ~callable:assumption callable_assumptions;
        }
      in

      attribute annotation ~assumptions:new_assumptions ~name:"__call__"
      >>| AnnotatedAttribute.annotation
      >>| Annotation.annotation
      >>= fun annotation ->
      match annotation with
      | Type.Parametric { name = "BoundMethod"; parameters = [Single _; Single _] }
      | Callable _ ->
          Some annotation
      | _ -> None)


module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module Make (OrderedConstraints : OrderedConstraintsType) = struct
  (* TODO(T41127207): merge this with actual signature select *)
  let rec simulate_signature_select
      order
      ~callable:{ Type.Callable.implementation; overloads; _ }
      ~called_as
      ~constraints
    =
    let open Callable in
    let solve implementation ~initial_constraints =
      try
        let rec solve_parameters_against_tuple_variadic
            ~is_lower_bound
            ~concretes
            ~ordered_type
            ~remaining_parameters
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
          let solve_remaining_parameters constraints =
            if is_lower_bound then
              solve_parameters
                ~left_parameters:after_first_keyword_inclusive
                ~right_parameters:remaining_parameters
                constraints
            else
              solve_parameters
                ~left_parameters:remaining_parameters
                ~right_parameters:after_first_keyword_inclusive
                constraints
          in
          let add_bound concretes =
            let left, right =
              if is_lower_bound then
                concretes, ordered_type
              else
                ordered_type, concretes
            in
            solve_ordered_types_less_or_equal order ~left ~right ~constraints
          in
          let ordered_type_from_non_keyword_parameters =
            let extract_component = function
              | Type.Callable.Parameter.PositionalOnly { annotation; _ } ->
                  Some (Type.OrderedTypes.Concrete [annotation])
              | Named { annotation; _ } when not is_lower_bound ->
                  (* Named arguments can be called positionally, but positionals can't be called
                     with a name *)
                  Some (Type.OrderedTypes.Concrete [annotation])
              | Variable (Concatenation concatenation) ->
                  Some (Type.OrderedTypes.Concatenation concatenation)
              | _ -> None
            in
            let concatenate left right =
              left >>= fun left -> Type.OrderedTypes.concatenate ~left ~right
            in
            List.map before_first_keyword ~f:extract_component
            |> Option.all
            >>= List.fold ~init:(Some (Type.OrderedTypes.Concrete [])) ~f:concatenate
          in
          ordered_type_from_non_keyword_parameters
          >>| add_bound
          >>| List.concat_map ~f:solve_remaining_parameters
          |> Option.value ~default:impossible
        and solve_parameters ~left_parameters ~right_parameters constraints =
          match left_parameters, right_parameters with
          | Parameter.PositionalOnly _ :: _, Parameter.Named _ :: _ -> []
          | ( Parameter.PositionalOnly { annotation = left_annotation; _ } :: left_parameters,
              Parameter.PositionalOnly { annotation = right_annotation; _ } :: right_parameters )
          | ( Parameter.Named { annotation = left_annotation; _ } :: left_parameters,
              Parameter.PositionalOnly { annotation = right_annotation; _ } :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( Parameter.Variable (Concrete left_annotation) :: left_parameters,
              Parameter.Variable (Concrete right_annotation) :: right_parameters )
          | ( Parameter.Keywords left_annotation :: left_parameters,
              Parameter.Keywords right_annotation :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( Parameter.KeywordOnly ({ annotation = left_annotation; _ } as left) :: left_parameters,
              Parameter.KeywordOnly ({ annotation = right_annotation; _ } as right)
              :: right_parameters )
          | ( Parameter.Named ({ annotation = left_annotation; _ } as left) :: left_parameters,
              Parameter.Named ({ annotation = right_annotation; _ } as right) :: right_parameters )
          | ( Parameter.Named ({ annotation = left_annotation; default = true; _ } as left)
              :: left_parameters,
              Parameter.KeywordOnly ({ annotation = right_annotation; _ } as right)
              :: right_parameters )
          | ( Parameter.Named ({ annotation = left_annotation; default = false; _ } as left)
              :: left_parameters,
              Parameter.KeywordOnly ({ annotation = right_annotation; default = false; _ } as right)
              :: right_parameters ) ->
              if Parameter.names_compatible (Parameter.Named left) (Parameter.Named right) then
                solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
              else
                impossible
          | ( Parameter.Variable (Concrete left_annotation) :: _,
              Parameter.PositionalOnly { annotation = right_annotation; _ } :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( Parameter.Variable (Concatenation left) :: left_parameters,
              Parameter.Variable (Concatenation right) :: right_parameters ) ->
              solve_ordered_types_less_or_equal
                order
                ~left:(Type.OrderedTypes.Concatenation left)
                ~right:(Type.OrderedTypes.Concatenation right)
                ~constraints
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | left, Parameter.Variable (Concatenation concatenation) :: remaining_parameters ->
              solve_parameters_against_tuple_variadic
                ~is_lower_bound:false
                ~concretes:left
                ~ordered_type:(Concatenation concatenation)
                ~remaining_parameters
          | Parameter.Variable (Concatenation concatenation) :: remaining_parameters, right ->
              solve_parameters_against_tuple_variadic
                ~is_lower_bound:true
                ~concretes:right
                ~ordered_type:(Concatenation concatenation)
                ~remaining_parameters
          | ( Parameter.Variable (Concrete variable_annotation)
              :: Parameter.Keywords keywords_annotation :: _,
              Parameter.Named { annotation = named_annotation; _ } :: right_parameters ) ->
              if Type.equal variable_annotation keywords_annotation then
                solve_less_or_equal
                  order
                  ~constraints
                  ~left:named_annotation
                  ~right:keywords_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
              else
                impossible
          | Parameter.Variable _ :: left_parameters, right_parameters
          | Parameter.Keywords _ :: left_parameters, right_parameters ->
              solve_parameters ~left_parameters ~right_parameters constraints
          | left :: left_parameters, [] ->
              if Parameter.default left then
                solve_parameters ~left_parameters ~right_parameters:[] constraints
              else
                impossible
          | [], [] -> [constraints]
          | _ -> impossible
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
           callable that doesn't admit kwargs and varargs is passed in. We need this since there is
           no good way of representing "leave the parameters alone and change the return type" in
           the Python type system at the moment. *)
        | Defined _, Undefined -> [initial_constraints]
        | Undefined, Defined _ -> [initial_constraints]
        | bound, ParameterVariadicTypeVariable { head = []; variable }
          when Type.Variable.Variadic.Parameters.is_free variable ->
            let pair = Type.Variable.ParameterVariadicPair (variable, bound) in
            OrderedConstraints.add_upper_bound initial_constraints ~order ~pair |> Option.to_list
        | bound, ParameterVariadicTypeVariable { head; variable }
          when Type.Variable.Variadic.Parameters.is_free variable ->
            let constraints, remainder =
              match bound with
              | Undefined -> [initial_constraints], Undefined
              | ParameterVariadicTypeVariable { head = left_head; variable = left_variable } ->
                  let paired, remainder = List.split_n left_head (List.length head) in
                  ( solve_ordered_types_less_or_equal
                      order
                      ~left:(Type.OrderedTypes.Concrete paired)
                      ~right:(Type.OrderedTypes.Concrete head)
                      ~constraints:initial_constraints,
                    ParameterVariadicTypeVariable { head = remainder; variable = left_variable } )
              | Defined defined ->
                  let paired, remainder = List.split_n defined (List.length head) in
                  ( solve_parameters
                      ~left_parameters:paired
                      ~right_parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
                      initial_constraints,
                    Defined remainder )
            in
            let pair = Type.Variable.ParameterVariadicPair (variable, remainder) in
            List.filter_map constraints ~f:(OrderedConstraints.add_upper_bound ~order ~pair)
        | ParameterVariadicTypeVariable left, ParameterVariadicTypeVariable right
          when Type.Callable.equal_parameter_variadic_type_variable Type.equal left right ->
            [initial_constraints]
        | _, _ -> impossible
      with
      | _ -> impossible
    in
    let overload_to_instantiated_return_and_altered_constraints overload =
      let namespace = Type.Variable.Namespace.create_fresh () in
      let namespaced_variables =
        Type.Callable { Type.Callable.kind = Anonymous; implementation = overload; overloads = [] }
        |> Type.Variable.all_free_variables
        |> List.map ~f:(Type.Variable.namespace ~namespace)
      in
      let overload =
        map_implementation overload ~f:(Type.Variable.namespace_all_free_variables ~namespace)
      in
      let does_not_leak_namespaced_variables (external_constraints, _) =
        not (TypeConstraints.exists_in_bounds external_constraints ~variables:namespaced_variables)
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
           ~f:(OrderedConstraints.extract_partial_solution ~order ~variables:namespaced_variables)
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

     * a precondition set of constraints (as defined in TypeConstraints.mli) from a previous call to
     solve_less_or_equal (or from somewhere else). This is how you're able to define conjunctions of
     =<= statements, as when you are trying to satisfy a number of argument <-> parameter pairs in
     signature selection

     and returns:

     * an arbitrarily ordered list of constraints (again as defined in Typeconstraints.mli) that
     each are sufficient to satisfy the given statement and the precondition constraints. If this
     list is empty, there is no way to satify those requirements (at least as well as we can know
     that).

     The general strategy undertaken to achieve this behavior is to pairwise recursively break up
     the types in the same way, e.g. we expect to get a tuple on the right if we have a tuple on the
     left, and to handle that by enforcing that each of the contained types be less than their pair
     on the other side (as tuples are covariant). Certain pairs, such as X =<= Union[...] or
     comparing callables with overloads naturally create multiple disjoint possibilities which give
     rise to the list of constraints that we end up returning.

     Once you have enforced all of the statements you would like to ensure are true, you can extract
     possible solutions to the constraints set you have built up with List.filter_map
     ~f:OrderedConstraints.solve *)
  and solve_less_or_equal
      ({
         class_hierarchy =
           { instantiate_successors_parameters; variables; is_transitive_successor; _ };
         is_protocol;
         assumptions = { protocol_assumptions; _ } as assumptions;
         get_typed_dictionary;
         metaclass;
         _;
       } as order)
      ~constraints
      ~left
      ~right
    =
    let open Type.Record.TypedDictionary in
    let add_fallbacks other =
      Type.Variable.all_free_variables other
      |> List.fold ~init:constraints ~f:OrderedConstraints.add_fallback_to_any
    in
    let solve_less_or_equal_primitives ~source ~target =
      if is_transitive_successor ~source ~target then
        [constraints]
      else if
        is_protocol right ~protocol_assumptions
        && [%compare.equal: Type.Parameter.t list option]
             (instantiate_protocol_parameters order ~candidate:left ~protocol:target)
             (Some [])
      then
        [constraints]
      else
        impossible
    in
    match left, right with
    | _, _ when Type.equal left right -> [constraints]
    | _, Type.Primitive "object" -> [constraints]
    | other, Type.Any -> [add_fallbacks other]
    | _, Type.Top -> [constraints]
    | Type.ParameterVariadicComponent component, _ ->
        let left =
          match Type.Variable.Variadic.Parameters.Components.component component with
          | KeywordArguments ->
              Type.parametric
                Type.mapping_primitive
                [Single Type.string; Single Type.object_primitive]
          | PositionalArguments ->
              Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.object_primitive)
        in
        solve_less_or_equal order ~constraints ~left ~right
    | _, Type.ParameterVariadicComponent _ -> impossible
    | Type.Annotated left, _ -> solve_less_or_equal order ~constraints ~left ~right
    | _, Type.Annotated right -> solve_less_or_equal order ~constraints ~left ~right
    | Type.Any, other -> [add_fallbacks other]
    | Type.Variable left_variable, Type.Variable right_variable
      when Type.Variable.Unary.is_free left_variable && Type.Variable.Unary.is_free right_variable
      ->
        (* Either works because constraining V1 to be less or equal to V2 implies that V2 is greater
           than or equal to V1. Therefore either constraint is sufficient, and we should consider
           both. This approach simplifies things downstream for the constraint solver *)
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
    | _, Type.Bottom
    | Type.Top, _ ->
        impossible
    | Type.Bottom, _ -> [constraints]
    | _, Type.NoneType -> impossible
    | _, Type.RecursiveType recursive_type ->
        if
          [%compare.equal: Type.Parameter.t list option]
            (instantiate_recursive_type_parameters order ~candidate:left ~recursive_type)
            (Some [])
        then
          [constraints]
        else
          impossible
    | Type.RecursiveType recursive_type, _ ->
        solve_less_or_equal
          order
          ~constraints
          ~left:(Type.RecursiveType.unfold_recursive_type recursive_type)
          ~right
    | (Type.Callable _ | Type.NoneType), Type.Primitive protocol
      when is_protocol right ~protocol_assumptions ->
        if
          [%compare.equal: Type.Parameter.t list option]
            (instantiate_protocol_parameters order ~protocol ~candidate:left)
            (Some [])
        then
          [constraints]
        else
          impossible
    | (Type.Callable _ | Type.NoneType), Type.Parametric { name; _ }
      when is_protocol right ~protocol_assumptions ->
        instantiate_protocol_parameters order ~protocol:name ~candidate:left
        >>| Type.parametric name
        >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
        |> Option.value ~default:impossible
    | Type.Union lefts, right ->
        solve_ordered_types_less_or_equal
          order
          ~left:(Concrete lefts)
          ~right:(Concrete (List.map lefts ~f:(fun _ -> right)))
          ~constraints
    | Type.NoneType, Type.Union rights when List.exists rights ~f:Type.is_none ->
        (* Technically speaking, removing this special-case still leads to correct, but somewhat
           redundant solutions, when `rights` contains both None and type varaibles *)
        [constraints]
    | Type.NoneType, Type.Union rights ->
        List.concat_map rights ~f:(fun right -> solve_less_or_equal order ~constraints ~left ~right)
    | Type.NoneType, _ -> impossible
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
    | Type.Variable bound_variable, _ ->
        solve_less_or_equal
          order
          ~constraints
          ~left:(Type.Variable.Unary.upper_bound bound_variable)
          ~right
    | _, Type.Variable _bound_variable -> impossible
    | Type.IntExpression _, _
    | _, Type.IntExpression _ ->
        Type.solve_less_or_equal_polynomial
          ~left
          ~right
          ~impossible
          ~solve:(solve_less_or_equal order ~constraints)
    | _, Type.Union rights ->
        if
          Type.Variable.all_variables_are_resolved left
          && Type.Variable.all_variables_are_resolved right
        then
          (* This is a pure performance optimization, but is practically mandatory because there are
             some humongous unions out there *)
          let simple_solve right =
            solve_less_or_equal order ~constraints ~left ~right |> List.is_empty |> not
          in
          if List.exists rights ~f:simple_solve then
            [constraints]
          else
            impossible
        else if
          Type.Variable.all_variables_are_resolved left
          && List.exists
               ~f:(fun right ->
                 Type.Variable.all_variables_are_resolved right
                 && solve_less_or_equal order ~constraints ~left ~right |> List.is_empty |> not)
               rights
        then (* If X <= Union[Y, Z] with X and Y already resolved and X <= Y *)
          [constraints]
        else
          List.concat_map rights ~f:(fun right ->
              solve_less_or_equal order ~constraints ~left ~right)
    | ( Type.Parametric { name = "type"; parameters = [Single left] },
        Type.Parametric { name = "type"; parameters = [Single right] } ) ->
        solve_less_or_equal order ~constraints ~left ~right
    | Type.Parametric { name = "type"; parameters = [Single meta_parameter] }, _ ->
        let through_meta_hierarchy =
          match meta_parameter, right with
          | Primitive meta_parameter, Primitive _ ->
              metaclass meta_parameter ~assumptions
              >>| (fun left -> solve_less_or_equal order ~left ~right ~constraints)
              |> Option.value ~default:impossible
          | _ -> impossible
        in
        let through_protocol_hierarchy =
          match right, is_protocol right ~protocol_assumptions with
          | Primitive right_name, true ->
              if
                [%compare.equal: Type.Parameter.t list option]
                  (instantiate_protocol_parameters order ~candidate:left ~protocol:right_name)
                  (Some [])
              then
                [constraints]
              else
                impossible
          | Parametric { name = right_name; _ }, true ->
              instantiate_protocol_parameters order ~protocol:right_name ~candidate:left
              >>| Type.parametric right_name
              >>| (fun left -> solve_less_or_equal order ~left ~right ~constraints)
              |> Option.value ~default:impossible
          | Callable _, _ -> (
              match meta_parameter with
              | Type.Union types ->
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:(Type.union (List.map ~f:Type.meta types))
                    ~right
              | _ ->
                  resolve_callable_protocol ~order ~assumption:right left
                  >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
                  |> Option.value ~default:impossible)
          | _ -> impossible
        in
        List.append through_protocol_hierarchy through_meta_hierarchy
    | _, Type.Parametric { name = right_name; parameters = right_parameters } ->
        let solve_respecting_variance constraints = function
          | Type.Variable.UnaryPair (unary, left), Type.Variable.UnaryPair (_, right) -> (
              match left, right, unary with
              (* TODO kill these special cases *)
              | Type.Bottom, _, _ ->
                  (* T[Bottom] is a subtype of T[_T2], for any _T2 and regardless of its variance. *)
                  constraints
              | _, Type.Top, _ ->
                  (* T[_T2] is a subtype of T[Top], for any _T2 and regardless of its variance. *)
                  constraints
              | Top, _, _ -> impossible
              | left, right, { Type.Variable.Unary.variance = Covariant; _ } ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left ~right)
              | left, right, { variance = Contravariant; _ } ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left:right ~right:left)
              | left, right, { variance = Invariant; _ } ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left ~right)
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left:right ~right:left))
          | ( Type.Variable.ParameterVariadicPair (_, left),
              Type.Variable.ParameterVariadicPair (_, right) ) ->
              let left = Type.Callable.create ~parameters:left ~annotation:Type.Any () in
              let right = Type.Callable.create ~parameters:right ~annotation:Type.Any () in
              List.concat_map constraints ~f:(fun constraints ->
                  solve_less_or_equal order ~constraints ~left ~right)
          | Type.Variable.TupleVariadicPair (_, left), Type.Variable.TupleVariadicPair (_, right) ->
              (* We assume variadic classes are covariant by default since they represent the
                 immutable shape of a datatype. *)
              constraints
              |> List.concat_map ~f:(fun constraints ->
                     solve_ordered_types_less_or_equal order ~left ~right ~constraints)
          | _ -> impossible
        in
        let solve_parameters left_parameters right_parameters =
          variables right_name
          >>= Type.Variable.zip_variables_with_two_parameter_lists
                ~left_parameters
                ~right_parameters
          >>| List.fold ~f:solve_respecting_variance ~init:[constraints]
        in
        let left_parameters =
          let left_parameters = instantiate_successors_parameters ~source:left ~target:right_name in
          match left_parameters with
          | None when is_protocol right ~protocol_assumptions ->
              instantiate_protocol_parameters order ~protocol:right_name ~candidate:left
          | _ -> left_parameters
        in
        left_parameters
        >>= (fun left_parameters -> solve_parameters left_parameters right_parameters)
        |> Option.value ~default:impossible
    | Type.Primitive source, Type.Primitive target -> (
        let left_typed_dictionary = get_typed_dictionary left in
        let right_typed_dictionary = get_typed_dictionary right in
        match left_typed_dictionary, right_typed_dictionary with
        | Some { fields = left_fields; _ }, Some { fields = right_fields; _ } ->
            let field_not_found field =
              not
                (List.exists
                   left_fields
                   ~f:([%equal: Type.t Type.Record.TypedDictionary.typed_dictionary_field] field))
            in
            if not (List.exists right_fields ~f:field_not_found) then
              [constraints]
            else
              impossible
        | Some { fields; _ }, None ->
            let left =
              Type.Primitive
                (Type.TypedDictionary.class_name
                   ~total:(Type.TypedDictionary.are_fields_total fields))
            in
            solve_less_or_equal order ~constraints ~left ~right
        | None, Some { fields; _ } ->
            let right =
              Type.Primitive
                (Type.TypedDictionary.class_name
                   ~total:(Type.TypedDictionary.are_fields_total fields))
            in
            solve_less_or_equal order ~constraints ~left ~right
        | None, None -> solve_less_or_equal_primitives ~source ~target)
    | Type.Parametric { name = source; _ }, Type.Primitive target ->
        solve_less_or_equal_primitives ~source ~target
    (* A <= B -> A <= Optional[B].*)
    | Type.Tuple left, Type.Tuple right ->
        solve_ordered_types_less_or_equal order ~left ~right ~constraints
    | Type.Tuple (Concatenation concatenation), Type.Primitive _ ->
        Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
        >>| (fun parameter ->
              solve_less_or_equal
                order
                ~constraints
                ~left:(Type.parametric "tuple" [Single parameter])
                ~right)
        |> Option.value ~default:impossible
    | Type.Tuple (Concrete members), Type.Primitive _ ->
        solve_less_or_equal
          order
          ~constraints
          ~left:(Type.parametric "tuple" [Single (Type.union members)])
          ~right
    | Type.Primitive name, Type.Tuple _ ->
        if Type.Primitive.equal name "tuple" then [constraints] else impossible
    | Type.Tuple _, _
    | _, Type.Tuple _ ->
        impossible
    | ( Type.Callable { Callable.kind = Callable.Named left; _ },
        Type.Callable { Callable.kind = Callable.Named right; _ } ) ->
        if Reference.equal left right then
          [constraints]
        else
          impossible
    | ( Type.Callable
          {
            Callable.kind = Callable.Anonymous;
            implementation = left_implementation;
            overloads = left_overloads;
          },
        Type.Callable
          {
            Callable.kind = Callable.Named _;
            implementation = right_implementation;
            overloads = right_overloads;
          } )
      when Callable.equal_overload Type.equal left_implementation right_implementation
           && List.equal (Callable.equal_overload Type.equal) left_overloads right_overloads ->
        impossible
    | Type.Callable callable, Type.Callable { implementation; overloads; _ } ->
        let fold_overload sofar (called_as : Type.t Callable.overload) =
          let call_as_overload constraints =
            simulate_signature_select order ~callable ~called_as ~constraints
            |> List.concat_map ~f:(fun (left, constraints) ->
                   solve_less_or_equal order ~constraints ~left ~right:called_as.annotation)
          in
          List.concat_map sofar ~f:call_as_overload
        in
        List.fold (implementation :: overloads) ~f:fold_overload ~init:[constraints]
    | left, Type.Callable _ ->
        resolve_callable_protocol ~order ~assumption:right left
        >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
        |> Option.value ~default:impossible
    | Type.Callable _, _ -> impossible
    | Type.Literal (String (LiteralValue _)), Type.Literal (String AnyLiteral) -> [constraints]
    | _, Type.Literal _ -> impossible
    | Type.Literal _, _ ->
        solve_less_or_equal order ~constraints ~left:(Type.weaken_literals left) ~right
    | Type.TypeOperation _, _
    | _, Type.TypeOperation _ ->
        impossible


  and solve_ordered_types_less_or_equal order ~left ~right ~constraints =
    let solve_non_variadic_pairs ~pairs constraints =
      let solve_pair constraints (left, right) =
        List.concat_map constraints ~f:(fun constraints ->
            solve_less_or_equal order ~constraints ~left ~right)
      in
      List.fold ~init:[constraints] ~f:solve_pair pairs
    in
    let open Type.OrderedTypes in
    if Type.OrderedTypes.equal left right then
      [constraints]
    else
      let solve_split_ordered_types { prefix_pairs; middle_pair; suffix_pairs } =
        match middle_pair with
        (* TODO(T84854853): Optimization: Avoid this splitting and concatenating for Concrete vs
           Concrete. *)
        | Concrete left_middle, Concrete right_middle -> (
            match List.zip left_middle right_middle with
            | Ok middle_pairs ->
                solve_non_variadic_pairs
                  ~pairs:(prefix_pairs @ middle_pairs @ suffix_pairs)
                  constraints
            | Unequal_lengths -> impossible)
        | Concrete concrete, Concatenation concatenation -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_variadic concatenation,
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation )
            with
            | Some variadic, _ when Type.Variable.Variadic.Tuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_lower_bound
                          ~order
                          ~pair:(Type.Variable.TupleVariadicPair (variadic, Concrete concrete)))
            | _, Some unbounded_element ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.concat_map ~f:(fun constraints ->
                       solve_less_or_equal
                         order
                         ~constraints
                         ~left:(Type.union concrete)
                         ~right:unbounded_element)
            | _ -> impossible)
        | Concatenation concatenation, Concrete concrete -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_variadic concatenation,
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation )
            with
            | Some variadic, _ when Type.Variable.Variadic.Tuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_upper_bound
                          ~order
                          ~pair:(Type.Variable.TupleVariadicPair (variadic, Concrete concrete)))
            | _ -> impossible)
        | Concatenation left_concatenation, Concatenation right_concatenation -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_variadic left_concatenation,
                Type.OrderedTypes.Concatenation.extract_sole_variadic right_concatenation )
            with
            | _, Some variadic when Type.Variable.Variadic.Tuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_lower_bound
                          ~order
                          ~pair:
                            (Type.Variable.TupleVariadicPair
                               (variadic, Concatenation left_concatenation)))
            | Some variadic, _ when Type.Variable.Variadic.Tuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_upper_bound
                          ~order
                          ~pair:
                            (Type.Variable.TupleVariadicPair
                               (variadic, Concatenation right_concatenation)))
            | _ -> (
                match
                  ( Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
                      left_concatenation,
                    Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
                      right_concatenation )
                with
                | Some left_unbounded_element, Some right_unbounded_element ->
                    solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                    |> List.concat_map ~f:(fun constraints ->
                           solve_less_or_equal
                             order
                             ~constraints
                             ~left:left_unbounded_element
                             ~right:right_unbounded_element)
                | None, Some right_unbounded_element ->
                    solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                    |> List.concat_map ~f:(fun constraints ->
                           solve_less_or_equal
                             order
                             ~constraints
                             ~left:Type.object_primitive
                             ~right:right_unbounded_element)
                | _ -> impossible))
      in
      Type.OrderedTypes.split_matching_elements_by_length left right
      >>| solve_split_ordered_types
      |> Option.value ~default:impossible


  (** Find parameters to instantiate `protocol` such that `candidate <: protocol[parameters]`, where
      `<:` is `solve_candidate_less_or_equal_protocol`.

      NOTE: unlike several of the other methods defined by this `let rec` block, here an empty list
      does not mean failure, it means a success with no constraints due to generics. A failure is
      indicated by an output of None.

      This can handle recursive instances of (candidate <: protocol). The first time it sees a
      candidate-protocol pair, it stores an assumed set of parameters. The next time it sees the
      same pair, it returns the assumed parameters. When the protocol is not generic, the solution
      if it exists will be [].

      We need this because Python protocols can refer to themselves. So, the subtyping relation for
      protocols is the one for equirecursive types. See section 21.9 of Types and Programming
      Languages for the subtyping algorithm.

      Note that classes that refer to themselves don't suffer from this since subtyping for two
      classes just follows from the class hierarchy. *)
  and instantiate_protocol_parameters_with_solve
      ({
         class_hierarchy = { variables; _ };
         assumptions = { protocol_assumptions; _ } as assumptions;
         _;
       } as order)
      ~solve_candidate_less_or_equal_protocol
      ~candidate
      ~protocol
      : Type.Parameter.t list option
    =
    match candidate with
    | Type.Primitive candidate_name when Option.is_some (variables candidate_name) ->
        (* If we are given a "stripped" generic, we decline to do structural analysis, as these
           kinds of comparisons only exists for legacy reasons to do nominal comparisons *)
        None
    | Type.Primitive candidate_name when Identifier.equal candidate_name protocol -> Some []
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
        | None ->
            let protocol_generics = variables protocol in
            let protocol_generic_parameters =
              protocol_generics >>| List.map ~f:Type.Variable.to_parameter
            in
            let new_assumptions =
              ProtocolAssumptions.add
                protocol_assumptions
                ~candidate
                ~protocol
                ~protocol_parameters:(Option.value protocol_generic_parameters ~default:[])
            in
            let assumptions = { assumptions with protocol_assumptions = new_assumptions } in
            let order_with_new_assumption = { order with assumptions } in
            let candidate, desanitize_map =
              match candidate with
              | Type.Callable _ -> candidate, []
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
                  sanitized_candidate, desanitize_map
            in
            let instantiate_protocol_generics solution =
              let desanitize =
                let desanitization_solution = TypeConstraints.Solution.create desanitize_map in
                let instantiate = function
                  | Type.Parameter.Single single ->
                      [
                        Type.Parameter.Single
                          (TypeConstraints.Solution.instantiate desanitization_solution single);
                      ]
                  | CallableParameters parameters ->
                      [
                        CallableParameters
                          (TypeConstraints.Solution.instantiate_callable_parameters
                             desanitization_solution
                             parameters);
                      ]
                  | Unpacked unpackable ->
                      TypeConstraints.Solution.instantiate_ordered_types
                        desanitization_solution
                        (Concatenation
                           (Type.OrderedTypes.Concatenation.create_from_unpackable unpackable))
                      |> Type.OrderedTypes.to_parameters
                in
                List.concat_map ~f:instantiate
              in
              let instantiate = function
                | Type.Variable.Unary variable ->
                    TypeConstraints.Solution.instantiate_single_variable solution variable
                    |> Option.value ~default:(Type.Variable variable)
                    |> fun instantiated -> [Type.Parameter.Single instantiated]
                | ParameterVariadic variable ->
                    TypeConstraints.Solution.instantiate_single_parameter_variadic solution variable
                    |> Option.value
                         ~default:
                           (Type.Callable.ParameterVariadicTypeVariable { head = []; variable })
                    |> fun instantiated -> [Type.Parameter.CallableParameters instantiated]
                | TupleVariadic variadic ->
                    TypeConstraints.Solution.instantiate_ordered_types
                      solution
                      (Concatenation (Type.OrderedTypes.Concatenation.create variadic))
                    |> Type.OrderedTypes.to_parameters
              in
              protocol_generics
              >>| List.concat_map ~f:instantiate
              >>| desanitize
              |> Option.value ~default:[]
            in
            let protocol_annotation =
              protocol_generic_parameters
              >>| Type.parametric protocol
              |> Option.value ~default:(Type.Primitive protocol)
            in
            solve_candidate_less_or_equal_protocol
              order_with_new_assumption
              ~candidate
              ~protocol_annotation
            >>| instantiate_protocol_generics)


  (** As with `instantiate_protocol_parameters_with_solve`, here `None` means a failure to match
      `candidate` type with the protocol, whereas `Some []` means no generic constraints were
      induced. *)
  and instantiate_protocol_parameters
      : order -> candidate:Type.t -> protocol:Ast.Identifier.t -> Type.Parameter.t list option
    =
    (* A candidate is less-or-equal to a protocol if candidate.x <: protocol.x for each attribute
       `x` in the protocol. *)
    let solve_all_protocol_attributes_less_or_equal
        ({ attribute; all_attributes; assumptions; _ } as order)
        ~candidate
        ~protocol_annotation
      =
      let attribute_implements constraints_set protocol_attribute =
        match constraints_set with
        | [] -> []
        | _ ->
            let attribute_type attribute =
              AnnotatedAttribute.annotation attribute |> Annotation.annotation
            in
            let attribute_type_with_getattr_fallback ~attribute_lookup ~name =
              match attribute_lookup ~name with
              | Some found -> Some (attribute_type found)
              | None -> (
                  match attribute_lookup ~name:"__getattr__" >>| attribute_type with
                  | Some
                      (Type.Parametric
                        { name = "BoundMethod"; parameters = [Single (Type.Callable callable); _] })
                  | Some (Type.Callable callable) ->
                      Some callable.implementation.annotation
                  | _ -> None)
            in

            attribute_type_with_getattr_fallback
              ~attribute_lookup:(attribute ~assumptions candidate)
              ~name:(AnnotatedAttribute.name protocol_attribute)
            >>| (fun left ->
                  let right =
                    match attribute_type protocol_attribute with
                    | Type.Parametric { name = "BoundMethod"; _ } as bound_method ->
                        attribute ~assumptions bound_method ~name:"__call__"
                        >>| attribute_type
                        |> Option.value ~default:Type.object_primitive
                    | annotation -> annotation
                  in
                  List.concat_map constraints_set ~f:(fun constraints ->
                      solve_less_or_equal order ~left ~right ~constraints))
            |> Option.value ~default:[]
      in
      let protocol_attributes =
        let is_not_object_or_generic_method attribute =
          let parent = AnnotatedAttribute.parent attribute in
          (not (Type.is_object (Primitive parent)))
          && not (Type.is_generic_primitive (Primitive parent))
        in
        all_attributes ~assumptions protocol_annotation
        >>| List.filter ~f:is_not_object_or_generic_method
      in
      protocol_attributes
      >>| List.fold ~init:[TypeConstraints.empty] ~f:attribute_implements
      >>| List.filter_map ~f:(OrderedConstraints.solve ~order)
      >>= List.hd
    in
    instantiate_protocol_parameters_with_solve
      ~solve_candidate_less_or_equal_protocol:solve_all_protocol_attributes_less_or_equal


  and instantiate_recursive_type_parameters order ~candidate ~recursive_type
      : Type.Parameter.t list option
    =
    (* TODO(T44784951): Allow passing in the generic parameters for generic recursive types. *)
    let solve_recursive_type_less_or_equal order ~candidate ~protocol_annotation:_ =
      let expanded_body = Type.RecursiveType.unfold_recursive_type recursive_type in
      solve_less_or_equal
        order
        ~left:candidate
        ~right:expanded_body
        ~constraints:TypeConstraints.empty
      |> List.filter_map ~f:(OrderedConstraints.solve ~order)
      |> List.hd
    in
    instantiate_protocol_parameters_with_solve
      order
      ~solve_candidate_less_or_equal_protocol:solve_recursive_type_less_or_equal
      ~candidate
      ~protocol:(Type.RecursiveType.name recursive_type)


  let add existing_constraints ~new_constraint ~order =
    match new_constraint with
    | LessOrEqual { left; right } ->
        List.concat_map existing_constraints ~f:(fun constraints ->
            solve_less_or_equal order ~constraints ~left ~right)
    | OrderedTypesLessOrEqual { left; right } ->
        List.concat_map existing_constraints ~f:(fun constraints ->
            solve_ordered_types_less_or_equal order ~constraints ~left ~right)
    | VariableIsExactly pair ->
        let add_both_bounds constraints =
          OrderedConstraints.add_upper_bound constraints ~order ~pair
          >>= OrderedConstraints.add_lower_bound ~order ~pair
        in
        List.filter_map existing_constraints ~f:add_both_bounds


  let solve ?only_solve_for constraints_set ~order =
    match only_solve_for with
    | Some variables ->
        let partial_solve constraints =
          OrderedConstraints.extract_partial_solution constraints ~order ~variables >>| snd
        in
        List.find_map constraints_set ~f:partial_solve
    | None -> List.find_map constraints_set ~f:(OrderedConstraints.solve ~order)


  let get_parameter_specification_possibilities constraints_set ~order ~parameter_specification =
    List.filter_map
      constraints_set
      ~f:
        (OrderedConstraints.extract_partial_solution
           ~order
           ~variables:[Type.Variable.ParameterVariadic parameter_specification])
    |> List.map ~f:snd
    |> List.filter_map ~f:(fun solution ->
           TypeConstraints.Solution.instantiate_single_parameter_variadic
             solution
             parameter_specification)
end
