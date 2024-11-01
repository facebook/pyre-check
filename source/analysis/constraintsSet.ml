(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Module for adding and solving constraints of the form type A <: type B. For example, a function
   call results in a set of constraints of the form `argument_type_i <: parameter_type_i` (hence the
   awkward name for the module: `constraintsSet`). We solve for any free type variables in either A
   or B.

   The constraint-solving is mostly a "straightforward" implementation of type argument synthesis
   from section 3 of:

   Pierce, B. C., & Turner, D. N. (2000). Local type inference. ACM Transactions on Programming
   Languages and Systems (TOPLAS), 22(1), 1-44.
   https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf

   It is called "type argument synthesis" because, for generic functions f and g in `f(x) <: g(y)`,
   the problem is that of synthesizing the type arguments for the generic type variables of f and g:
   `f[type1](x) <: g[type2](x)`. Colloquially, we call this "type inference".

   We need this because Python doesn't require (or allow) you to instantiate generic functions. So,
   Pyre has to infer those types.

   * For example, in `identity("hello")`, Pyre will infer that the generic function `identity` is
   being used with `T = str`, i.e., `identity[str]("hello")`.

   * For `identity(some_int)`, Pyre will infer that it is `identity[int](some_int)`.

   Warning: This module has easily caused perf regressions and even infinite loops, since we solve
   constraints recursively. Even small changes in the order of variants in the match statement can
   lead to big perf changes. *)

open Core
open Ast
open Pyre
open CycleDetection

type class_hierarchy = {
  instantiate_successors_parameters:
    source:Type.t -> target:Type.Primitive.t -> Type.Argument.t list option;
  has_transitive_successor: successor:Type.Primitive.t -> Type.Primitive.t -> bool;
  generic_parameters: Type.Primitive.t -> Type.GenericParameter.t list option;
  least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t option;
}

type order = {
  class_hierarchy: class_hierarchy;
  instantiated_attributes:
    Type.t -> cycle_detections:CycleDetection.t -> AnnotatedAttribute.instantiated list option;
  attribute:
    Type.t ->
    cycle_detections:CycleDetection.t ->
    name:Ast.Identifier.t ->
    AnnotatedAttribute.instantiated option;
  is_protocol: Type.t -> bool;
  get_typed_dictionary: Type.t -> Type.TypedDictionary.t option;
  get_named_tuple_fields: Type.t -> Type.t list option;
  metaclass: Type.Primitive.t -> cycle_detections:CycleDetection.t -> Type.t option;
  cycle_detections: CycleDetection.t;
  variance_map:
    class_name:string ->
    parameters:Type.GenericParameter.t list ->
    Type.Record.Variance.t Ast.Identifier.Map.t;
}

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
  val add_and_simplify : t -> new_constraint:kind -> order:order -> t

  val solve
    :  ?only_solve_for:Type.Variable.t list ->
    t ->
    order:order ->
    TypeConstraints.Solution.t option

  val get_parameter_specification_possibilities
    :  t ->
    order:order ->
    parameter_specification:Type.Variable.ParamSpec.t ->
    Type.Callable.parameters list

  val instantiate_protocol_parameters
    :  candidate:Type.t ->
    protocol:Ast.Identifier.t ->
    ?protocol_arguments:Type.Argument.t list ->
    order ->
    Type.Argument.t list option
end

let resolve_callable_protocol
    ~assumption
    ~order:{ attribute; cycle_detections = { assumed_callable_types; _ } as cycle_detections; _ }
    annotation
  =
  match
    AssumedCallableTypes.find_assumed_callable_type ~candidate:annotation assumed_callable_types
  with
  | Some assumption -> Some assumption
  | None -> (
      let new_cycle_detections =
        {
          cycle_detections with
          assumed_callable_types =
            AssumedCallableTypes.add
              ~candidate:annotation
              ~callable:assumption
              assumed_callable_types;
        }
      in

      attribute annotation ~cycle_detections:new_cycle_detections ~name:"__call__"
      >>| AnnotatedAttribute.annotation
      >>| TypeInfo.Unit.annotation
      >>= fun annotation ->
      match annotation with
      | Type.Parametric { name = "BoundMethod"; arguments = [Single _; Single _] }
      | Callable _ ->
          Some annotation
      | _ -> None)


module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module Make (OrderedConstraints : OrderedConstraintsType) = struct
  (* This function takes a possibly-overloaded function `callable` and asks it whether it can be
     called as if it had a particular overload signature `called_as`. Multiple solutions are
     possible if more than one overload matches; for each one we produce the returning return type
     and constraint sets.

     This is used as an inner loop when determining whether one possibly-overloaded callable is a
     subtype of another, see where it is used for a comment explaining how it fits into the bigger
     picture.

     TODO(T41127207): merge this with actual signature select logic in AttributeResolution.ml *)
  let rec simulate_signature_select
      order
      ~callable:({ Type.Callable.implementation; overloads; _ } as _callable)
      ~called_as
      ~constraints
      ~get_typed_dictionary
    =
    let open Callable in
    let solve implementation ~initial_constraints =
      let get_kwargs_type parameters =
        let has_kwargs =
          List.exists ~f:(fun parameter ->
              match parameter with
              | CallableParamType.Keywords _ -> true
              | _ -> false)
        in
        let has_unpacked_kwargs =
          List.exists ~f:(fun parameter ->
              match parameter with
              | CallableParamType.Keywords annotation -> Type.is_unpack annotation
              | _ -> false)
        in
        if has_unpacked_kwargs parameters then
          `UnpackedKwargs
        else if has_kwargs parameters then
          `RegularKwargs
        else
          `NoKwargs
      in
      try
        let rec solve_parameters_against_tuple_variadic
            ~is_lower_bound
            ~concretes
            ~ordered_type
            ~remaining_parameters
          =
          let before_first_keyword, after_first_keyword_inclusive =
            let is_not_keyword_only = function
              | Type.Callable.CallableParamType.Keywords _
              | Type.Callable.CallableParamType.KeywordOnly _ ->
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
              | Type.Callable.CallableParamType.PositionalOnly { annotation; _ } ->
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
          let expand_typed_dictionary_parameters =
            List.map ~f:(fun { Type.TypedDictionary.name; annotation; required; _ } ->
                CallableParamType.KeywordOnly { name; annotation; default = not required })
          in
          let all_left_parameters = left_parameters in
          let all_right_parameters = right_parameters in
          match left_parameters, right_parameters with
          | CallableParamType.PositionalOnly _ :: _, CallableParamType.Named _ :: _ -> impossible
          | ( CallableParamType.PositionalOnly { annotation = left_annotation; _ } :: left_parameters,
              CallableParamType.PositionalOnly { annotation = right_annotation; _ }
              :: right_parameters )
          | ( CallableParamType.Named { annotation = left_annotation; _ } :: left_parameters,
              CallableParamType.PositionalOnly { annotation = right_annotation; _ }
              :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( CallableParamType.Keywords left_annotation :: left_parameters,
              CallableParamType.Keywords right_annotation :: right_parameters ) -> (
              match Type.unpack_value left_annotation, Type.unpack_value right_annotation with
              | Some left_annotation, Some right_annotation ->
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:right_annotation
                    ~right:left_annotation
                  |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
              | _, Some right_annotation -> (
                  match get_typed_dictionary right_annotation with
                  | None -> impossible
                  | Some { Type.TypedDictionary.fields; _ } ->
                      solve_parameters
                        ~left_parameters:all_left_parameters
                        ~right_parameters:
                          (expand_typed_dictionary_parameters fields @ right_parameters)
                        constraints)
              | Some left_annotation, _ -> (
                  match get_typed_dictionary left_annotation with
                  | None -> impossible
                  | Some { Type.TypedDictionary.fields; _ } ->
                      solve_parameters
                        ~left_parameters:
                          (expand_typed_dictionary_parameters fields @ left_parameters)
                        ~right_parameters:all_right_parameters
                        constraints)
              | None, None ->
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:right_annotation
                    ~right:left_annotation
                  |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters))
          | _, CallableParamType.Keywords right_annotation :: right_parameters
            when Type.is_unpack right_annotation -> (
              match Type.unpack_value right_annotation >>= get_typed_dictionary with
              | None -> impossible
              | Some { Type.TypedDictionary.fields; _ } ->
                  solve_parameters
                    ~left_parameters
                    ~right_parameters:(expand_typed_dictionary_parameters fields @ right_parameters)
                    constraints)
          | CallableParamType.Keywords left_annotation :: left_parameters, _
            when Type.is_unpack left_annotation -> (
              match Type.unpack_value left_annotation >>= get_typed_dictionary with
              | None -> impossible
              | Some { Type.TypedDictionary.fields; _ } ->
                  solve_parameters
                    ~left_parameters:(expand_typed_dictionary_parameters fields @ left_parameters)
                    ~right_parameters
                    constraints)
          | ( CallableParamType.Variable (Concrete left_annotation) :: left_parameters,
              CallableParamType.Variable (Concrete right_annotation) :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( CallableParamType.Keywords left_annotation :: _,
              ( CallableParamType.KeywordOnly { annotation = right_annotation; _ }
              | CallableParamType.Named { annotation = right_annotation; _ } )
              :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( CallableParamType.KeywordOnly ({ annotation = left_annotation; _ } as left)
              :: left_parameters,
              CallableParamType.KeywordOnly ({ annotation = right_annotation; _ } as right)
              :: right_parameters )
          | ( CallableParamType.Named ({ annotation = left_annotation; _ } as left)
              :: left_parameters,
              CallableParamType.Named ({ annotation = right_annotation; _ } as right)
              :: right_parameters )
          | ( CallableParamType.Named ({ annotation = left_annotation; default = true; _ } as left)
              :: left_parameters,
              CallableParamType.KeywordOnly ({ annotation = right_annotation; _ } as right)
              :: right_parameters )
          | ( CallableParamType.Named ({ annotation = left_annotation; default = false; _ } as left)
              :: left_parameters,
              CallableParamType.KeywordOnly
                ({ annotation = right_annotation; default = false; _ } as right)
              :: right_parameters ) ->
              if
                CallableParamType.names_compatible
                  (CallableParamType.Named left)
                  (CallableParamType.Named right)
              then
                (* If the callable requires a particular parameter, it cannot be called as a
                   signature that allows the parameter to be omitted *)
                if right.default && not left.default then
                  impossible
                else
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:right_annotation
                    ~right:left_annotation
                  |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
              else
                impossible
          | ( CallableParamType.Variable (Concrete left_annotation) :: _,
              CallableParamType.PositionalOnly { annotation = right_annotation; _ }
              :: right_parameters ) ->
              solve_less_or_equal order ~constraints ~left:right_annotation ~right:left_annotation
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | ( CallableParamType.Variable (Concatenation left) :: left_parameters,
              CallableParamType.Variable (Concatenation right) :: right_parameters ) ->
              solve_ordered_types_less_or_equal
                order
                ~left:(Type.OrderedTypes.Concatenation left)
                ~right:(Type.OrderedTypes.Concatenation right)
                ~constraints
              |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
          | left, CallableParamType.Variable (Concatenation concatenation) :: remaining_parameters
            ->
              solve_parameters_against_tuple_variadic
                ~is_lower_bound:false
                ~concretes:left
                ~ordered_type:(Concatenation concatenation)
                ~remaining_parameters
          | CallableParamType.Variable (Concatenation concatenation) :: remaining_parameters, right
            ->
              solve_parameters_against_tuple_variadic
                ~is_lower_bound:true
                ~concretes:right
                ~ordered_type:(Concatenation concatenation)
                ~remaining_parameters
          | ( CallableParamType.Variable (Concrete variable_annotation)
              :: CallableParamType.Keywords keywords_annotation
              :: _,
              CallableParamType.Named { annotation = named_annotation; _ } :: right_parameters ) ->
              if Type.equal variable_annotation keywords_annotation then
                solve_less_or_equal
                  order
                  ~constraints
                  ~left:named_annotation
                  ~right:keywords_annotation
                |> List.concat_map ~f:(solve_parameters ~left_parameters ~right_parameters)
              else
                impossible
          | CallableParamType.Variable _ :: left_parameters, right_parameters
          | CallableParamType.Keywords _ :: left_parameters, right_parameters ->
              solve_parameters ~left_parameters ~right_parameters constraints
          | left :: left_parameters, [] ->
              if CallableParamType.default left then
                solve_parameters ~left_parameters ~right_parameters:[] constraints
              else
                impossible
          | [], [] -> [constraints]
          | _ -> impossible
        in
        match implementation.parameters, called_as.parameters with
        | Undefined, Undefined -> [initial_constraints]
        | Defined implementation, Defined called_as -> (
            match get_kwargs_type implementation, get_kwargs_type called_as with
            (* Functions without kwargs cannot be called as if they had unpacked kwargs, because the
               call may introduce additional unexpected parameters *)
            | `NoKwargs, `UnpackedKwargs -> impossible
            (* Functions functions with unpacked kwargs cannot be called as if they had regular
               kwargs, since the names are not checked *)
            | `UnpackedKwargs, `RegularKwargs -> impossible
            (* In other cases, it should be safe to expand unpacked typed dictionary fields and
               check them against each other or regular parameters. *)
            | _ ->
                solve_parameters
                  ~left_parameters:implementation
                  ~right_parameters:called_as
                  initial_constraints)
        (* We exhibit unsound behavior when a concrete callable is passed into one expecting a
           Callable[..., T] - Callable[..., T] admits any parameters, which is not true when a
           callable that doesn't admit kwargs and varargs is passed in. We need this since there is
           no good way of representing "leave the parameters alone and change the return type" in
           the Python type system at the moment. *)
        | Defined _, Undefined -> [initial_constraints]
        | Undefined, Defined _ -> [initial_constraints]
        | ( FromParamSpec { head = left_head; variable = left_variable },
            FromParamSpec { head = right_head; variable = right_variable } )
          when Type.Variable.ParamSpec.is_free left_variable
               && Type.Variable.ParamSpec.is_free right_variable ->
            let add_parameter_specification_bounds constraints =
              constraints
              |> OrderedConstraints.add_upper_bound
                   ~order
                   ~pair:
                     (Type.Variable.ParamSpecPair
                        (right_variable, FromParamSpec { head = []; variable = left_variable }))
              >>= OrderedConstraints.add_lower_bound
                    ~order
                    ~pair:
                      (Type.Variable.ParamSpecPair
                         (left_variable, FromParamSpec { head = []; variable = right_variable }))
              |> Option.to_list
            in
            solve_ordered_types_less_or_equal
              order
              ~left:(Type.OrderedTypes.Concrete right_head)
              ~right:(Type.OrderedTypes.Concrete left_head)
              ~constraints:initial_constraints
            |> List.concat_map ~f:add_parameter_specification_bounds
        | bound, FromParamSpec { head = []; variable } when Type.Variable.ParamSpec.is_free variable
          ->
            let pair = Type.Variable.ParamSpecPair (variable, bound) in
            OrderedConstraints.add_upper_bound initial_constraints ~order ~pair |> Option.to_list
        | bound, FromParamSpec { head; variable } when Type.Variable.ParamSpec.is_free variable ->
            let constraints, remainder =
              match bound with
              | Undefined -> [initial_constraints], Undefined
              | FromParamSpec { head = left_head; variable = left_variable } ->
                  let paired, remainder = List.split_n left_head (List.length head) in
                  ( solve_ordered_types_less_or_equal
                      order
                      ~left:(Type.OrderedTypes.Concrete paired)
                      ~right:(Type.OrderedTypes.Concrete head)
                      ~constraints:initial_constraints,
                    FromParamSpec { head = remainder; variable = left_variable } )
              | Defined defined ->
                  let paired, remainder = List.split_n defined (List.length head) in
                  ( solve_parameters
                      ~left_parameters:paired
                      ~right_parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
                      initial_constraints,
                    Defined remainder )
            in
            let pair = Type.Variable.ParamSpecPair (variable, remainder) in
            List.filter_map constraints ~f:(OrderedConstraints.add_upper_bound ~order ~pair)
        | FromParamSpec left, FromParamSpec right
          when Type.Callable.equal_params_from_param_spec Type.equal left right ->
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
      else
        overloads
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
           { instantiate_successors_parameters; generic_parameters; has_transitive_successor; _ };
         is_protocol;
         cycle_detections;
         get_typed_dictionary;
         get_named_tuple_fields;
         metaclass;
         variance_map;
         _;
       } as order)
      ~constraints
      ~left
      ~right
    =
    let open Type.TypedDictionary in
    let add_fallbacks other =
      Type.Variable.all_free_variables other
      |> List.fold ~init:constraints ~f:OrderedConstraints.add_fallback_to_any
    in
    let solve_less_or_equal_primitives ~source ~target =
      if has_transitive_successor ~successor:target source then
        [constraints]
      else if
        is_protocol right
        && [%compare.equal: Type.Argument.t list option]
             (instantiate_protocol_parameters order ~candidate:left ~protocol:target)
             (Some [])
      then
        [constraints]
      else
        impossible
    in
    match left, right with
    | _, _ when Type.equal left right -> [constraints]
    | Type.PyreReadOnly _, Type.Primitive "object" -> impossible
    | Type.Union items, Type.Primitive "object"
      when List.exists items ~f:Type.PyreReadOnly.is_readonly ->
        impossible
    | _, Type.Primitive "object" -> [constraints]
    | other, Type.Any -> [add_fallbacks other]
    | _, Type.Top -> [constraints]
    | Type.ParamSpecComponent component, _ ->
        let left =
          match Type.Variable.ParamSpec.Components.component component with
          | KeywordArguments ->
              Type.parametric
                Type.mapping_primitive
                [Single Type.string; Single Type.object_primitive]
          | PositionalArguments ->
              Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.object_primitive)
        in
        solve_less_or_equal order ~constraints ~left ~right
    | _, Type.ParamSpecComponent _ -> impossible
    | Type.Any, other -> [add_fallbacks other]
    | Type.Variable left_variable, Type.Variable right_variable
      when Type.Variable.TypeVar.is_free left_variable
           && Type.Variable.TypeVar.is_free right_variable ->
        (* Either works because constraining V1 to be less or equal to V2 implies that V2 is greater
           than or equal to V1. Therefore either constraint is sufficient, and we should consider
           both. This approach simplifies things downstream for the constraint solver *)
        let right_greater_than_left, left_less_than_right =
          ( OrderedConstraints.add_lower_bound
              constraints
              ~order
              ~pair:(Type.Variable.TypeVarPair (right_variable, left))
            |> Option.to_list,
            OrderedConstraints.add_upper_bound
              constraints
              ~order
              ~pair:(Type.Variable.TypeVarPair (left_variable, right))
            |> Option.to_list )
        in
        right_greater_than_left @ left_less_than_right
    | Type.Variable variable, bound when Type.Variable.TypeVar.is_free variable ->
        let pair = Type.Variable.TypeVarPair (variable, bound) in
        OrderedConstraints.add_upper_bound constraints ~order ~pair |> Option.to_list
    | bound, Type.Variable variable when Type.Variable.TypeVar.is_free variable ->
        let pair = Type.Variable.TypeVarPair (variable, bound) in
        OrderedConstraints.add_lower_bound constraints ~order ~pair |> Option.to_list
    | Type.PyreReadOnly left, Type.PyreReadOnly right ->
        solve_less_or_equal order ~constraints ~left ~right
    | _, Type.Bottom -> impossible
    | Type.Bottom, _ -> [constraints]
    | _, Type.NoneType -> impossible
    | _, Type.RecursiveType recursive_type ->
        if
          [%compare.equal: Type.Argument.t list option]
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
    | (Type.Callable _ | Type.NoneType), Type.Primitive protocol when is_protocol right ->
        if
          [%compare.equal: Type.Argument.t list option]
            (instantiate_protocol_parameters order ~protocol ~candidate:left)
            (Some [])
        then
          [constraints]
        else
          impossible
    | (Type.Callable _ | Type.NoneType), Type.Parametric { name; _ } when is_protocol right ->
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
    | _, Type.PyreReadOnly right -> solve_less_or_equal order ~constraints ~left ~right
    | Type.Top, _ -> impossible
    | Type.NoneType, Type.Union rights when List.exists rights ~f:Type.is_none ->
        (* Technically speaking, removing this special-case still leads to correct, but somewhat
           redundant solutions, when `rights` contains both None and type varaibles *)
        [constraints]
    | Type.NoneType, Type.Union rights ->
        List.concat_map rights ~f:(fun right -> solve_less_or_equal order ~constraints ~left ~right)
    | Type.NoneType, _ -> impossible
    | Type.Variable bound_variable, Type.Union union ->
        (* We have to consider two cases: (a) the `bound_variable <: each element of union` or (b)
           `upper-bound of bound_variable <: union`.

           We need to try (b) because type variables on the left-hand side may have upper bound as a
           `Union`. e.g., `T (upper-bound int | str) <: int | str`. If we tried only (a), then we
           would check `T (upper-bound int | str) <: int` and `T (upper-bound int | str) <: str`,
           both of which would fail. So, we also need to check that the upper bound (int | str) <:
           union.

           Note: We must do (a) before (b). If we do (b) before (a), then, for type variables that
           have no explicit upper bound, we would solve upper bound for unconstrained type variables
           <: union. This is `object` <: union, which will bind any free type variable T in the
           union as `object`. And, since we pick the first valid solution, we would ignore any
           solution from the other approach (a) and return the confusing solution that `T =
           object`. *)
        List.concat_map ~f:(fun right -> solve_less_or_equal order ~constraints ~left ~right) union
        @ solve_less_or_equal
            order
            ~constraints
            ~left:(Type.Variable.TypeVar.upper_bound bound_variable)
            ~right
    | Type.Variable bound_variable, _ ->
        solve_less_or_equal
          order
          ~constraints
          ~left:(Type.Variable.TypeVar.upper_bound bound_variable)
          ~right
    | _, Type.Variable _bound_variable -> impossible
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
    | Type.PyreReadOnly _, _ -> impossible
    | ( Type.Parametric
          { name = "typing.TypeGuard" | "typing_extensions.TypeGuard"; arguments = [Single left] },
        Type.Parametric
          { name = "typing.TypeGuard" | "typing_extensions.TypeGuard"; arguments = [Single right] }
      ) ->
        solve_less_or_equal order ~constraints ~left ~right
    | ( Type.Parametric { name = "type"; arguments = [Single left] },
        Type.Parametric { name = "type"; arguments = [Single right] } ) ->
        solve_less_or_equal order ~constraints ~left ~right
    | Type.Parametric { name = "type"; arguments = [Single meta_argument] }, _ ->
        let through_meta_hierarchy =
          match meta_argument, right with
          | Primitive meta_argument, Primitive _ ->
              metaclass meta_argument ~cycle_detections
              >>| (fun left -> solve_less_or_equal order ~left ~right ~constraints)
              |> Option.value ~default:impossible
          | _ -> impossible
        in
        let through_protocol_hierarchy =
          match right, is_protocol right with
          | Primitive right_name, true ->
              if
                [%compare.equal: Type.Argument.t list option]
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
              match meta_argument with
              | Type.Union types ->
                  solve_less_or_equal
                    order
                    ~constraints
                    ~left:(Type.union (List.map ~f:Type.builtins_type types))
                    ~right
              | _ ->
                  resolve_callable_protocol ~order ~assumption:right left
                  >>| (fun left -> solve_less_or_equal order ~constraints ~left ~right)
                  |> Option.value ~default:impossible)
          | _ -> impossible
        in
        List.append through_protocol_hierarchy through_meta_hierarchy
    | _, Type.Parametric { name = right_name; arguments = right_arguments } ->
        let solve_respecting_variance ~class_name ~parameters constraints = function
          | Type.GenericParameter.ZipTwoArgumentsLists.TypeVarZipResult { name; left; right; _ }
            -> (
              let variance =
                Map.find (variance_map ~parameters ~class_name) name
                |> Option.value ~default:Type.Record.Variance.Invariant
              in
              match left, right, variance with
              (* TODO kill these special cases *)
              | Type.Bottom, _, _ ->
                  (* T[Bottom] is a subtype of T[_T2], for any _T2 and regardless of its
                     variance. *)
                  constraints
              | _, Type.Top, _ ->
                  (* T[_T2] is a subtype of T[Top], for any _T2 and regardless of its variance. *)
                  constraints
              | Top, _, _ -> impossible
              | left, right, Type.Record.Variance.Covariant ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left ~right)
              | left, right, Type.Record.Variance.Contravariant ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left:right ~right:left)
              | left, right, Type.Record.Variance.Invariant ->
                  constraints
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left ~right)
                  |> List.concat_map ~f:(fun constraints ->
                         solve_less_or_equal order ~constraints ~left:right ~right:left)
              | _, _, Type.Record.Variance.Bivariant -> constraints)
          | Type.GenericParameter.ZipTwoArgumentsLists.TypeVarTupleZipResult { left; right; _ } ->
              (* We assume variadic classes are covariant by default since they represent the
                 immutable shape of a datatype. *)
              constraints
              |> List.concat_map ~f:(fun constraints ->
                     solve_ordered_types_less_or_equal order ~left ~right ~constraints)
          | Type.GenericParameter.ZipTwoArgumentsLists.ParamSpecZipResult { left; right; _ } ->
              let left = Type.Callable.create ~parameters:left ~annotation:Type.Any () in
              let right = Type.Callable.create ~parameters:right ~annotation:Type.Any () in
              List.concat_map constraints ~f:(fun constraints ->
                  solve_less_or_equal order ~constraints ~left ~right)
          | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedKindsZipResult _
          | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedLengthsZipResult _
          | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedVariadicZipResult _ ->
              impossible
        in
        let solve_arguments left_arguments right_arguments =
          let parameters = generic_parameters right_name in
          parameters
          >>| Type.GenericParameter.ZipTwoArgumentsLists.zip ~left_arguments ~right_arguments
          >>| List.fold
                ~f:
                  (solve_respecting_variance
                     ~class_name:right_name
                     ~parameters:(parameters |> Option.value ~default:[]))
                ~init:[constraints]
        in
        let left_arguments =
          let left_arguments = instantiate_successors_parameters ~source:left ~target:right_name in
          match left_arguments with
          | None when is_protocol right ->
              instantiate_protocol_parameters
                order
                ~protocol:right_name
                ~protocol_arguments:right_arguments
                ~candidate:left
          | _ -> left_arguments
        in
        left_arguments
        >>= (fun left_arguments -> solve_arguments left_arguments right_arguments)
        |> Option.value ~default:impossible
    | Type.Primitive source, Type.Primitive target -> (
        let left_typed_dictionary = get_typed_dictionary left in
        let right_typed_dictionary = get_typed_dictionary right in
        match left_typed_dictionary, right_typed_dictionary with
        | Some { fields = left_fields; _ }, Some { fields = right_fields; _ } ->
            (* Implements assignability rules from
               https://typing.readthedocs.io/en/latest/spec/typeddict.html#id4 *)
            let fields_needed_from_left =
              let field_needed { annotation; required; readonly; _ } =
                not (readonly && (not required) && Type.is_object annotation)
              in
              List.filter ~f:field_needed right_fields
            in
            let left_fields_map =
              left_fields
              |> List.map ~f:(fun (field : Type.TypedDictionary.typed_dictionary_field) ->
                     field.name, field)
              |> Map.of_alist_exn (module String)
            in
            let solve_for (right_field : Type.TypedDictionary.typed_dictionary_field) =
              match Map.find left_fields_map right_field.name with
              | None -> impossible
              | Some left_field ->
                  if right_field.required && not left_field.required then
                    impossible
                  else if (not right_field.readonly) && left_field.readonly then
                    impossible
                  else if
                    (not right_field.required) && (not right_field.readonly) && left_field.required
                  then
                    impossible
                  else if right_field.readonly then
                    solve_less_or_equal
                      order
                      ~left:left_field.annotation
                      ~right:right_field.annotation
                      ~constraints
                  else if Type.equal left_field.annotation right_field.annotation then
                    [constraints]
                  else
                    impossible
            in
            let partial_solutions = List.map ~f:solve_for fields_needed_from_left in
            if List.for_all ~f:potentially_satisfiable partial_solutions then
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
        >>| (fun argument ->
              solve_less_or_equal
                order
                ~constraints
                ~left:(Type.parametric "tuple" [Single argument])
                ~right)
        |> Option.value ~default:impossible
    | Type.Tuple (Concrete members), Type.Primitive _ ->
        solve_less_or_equal
          order
          ~constraints
          ~left:(Type.parametric "tuple" [Single (Type.union members)])
          ~right
    | Type.Primitive name, Type.Tuple right -> (
        if Type.Primitive.equal name "tuple" then
          [constraints]
        else
          match get_named_tuple_fields (Type.Primitive name) with
          | Some fields ->
              solve_ordered_types_less_or_equal order ~left:(Concrete fields) ~right ~constraints
          | _ -> impossible)
    | Type.Tuple _, _
    | _, Type.Tuple _ ->
        impossible
    | ( Type.Callable { Callable.kind = Callable.Named left; _ },
        Type.Callable { Callable.kind = Callable.Named right; _ } ) ->
        if Reference.equal left right then
          [constraints]
        else (* TODO(T200727911): this is wrong, but Pysa needs it *)
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
        (* This logic approximates the intersection nature of overloads. A possibly-overloaded
           function `left` is less-or-equal to a possibly-overloaded function `right` if for *every*
           overload signature in `right`, there exists some overload of `left` such that a function
           with the left overload signature can be called as if it had the right overload signature.

           The call to `simulated_signature_select` is handling the parameter type portion of the
           logic for each particular overload signature of `right` (and all of the overloads of
           `left`); we fold over them, verifying that the return types are compatible, and collect
           constraints accumulated in the process. *)
        let called_as_options =
          match overloads with
          | [] -> [implementation]
          | overloads -> overloads
        in
        let fold_overload sofar (called_as : Type.t Callable.overload) =
          let call_as_overload constraints =
            simulate_signature_select order ~callable ~called_as ~constraints ~get_typed_dictionary
            |> List.concat_map ~f:(fun (left, constraints) ->
                   solve_less_or_equal order ~constraints ~left ~right:called_as.annotation)
          in
          List.concat_map sofar ~f:call_as_overload
        in
        List.fold called_as_options ~f:fold_overload ~init:[constraints]
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
            | Some variadic, _ when Type.Variable.TypeVarTuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_lower_bound
                          ~order
                          ~pair:(Type.Variable.TypeVarTuplePair (variadic, Concrete concrete)))
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
            | Some variadic, _ when Type.Variable.TypeVarTuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_upper_bound
                          ~order
                          ~pair:(Type.Variable.TypeVarTuplePair (variadic, Concrete concrete)))
            | _ -> impossible)
        | Concatenation left_concatenation, Concatenation right_concatenation -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_variadic left_concatenation,
                Type.OrderedTypes.Concatenation.extract_sole_variadic right_concatenation )
            with
            | _, Some variadic when Type.Variable.TypeVarTuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_lower_bound
                          ~order
                          ~pair:
                            (Type.Variable.TypeVarTuplePair
                               (variadic, Concatenation left_concatenation)))
            | Some variadic, _ when Type.Variable.TypeVarTuple.is_free variadic ->
                solve_non_variadic_pairs ~pairs:(prefix_pairs @ suffix_pairs) constraints
                |> List.filter_map
                     ~f:
                       (OrderedConstraints.add_upper_bound
                          ~order
                          ~pair:
                            (Type.Variable.TypeVarTuplePair
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
      `<:` is `solve_candidate_less_or_equal`.

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
  and instantiate_recursive_type_with_solve
      ~solve_candidate_less_or_equal
      ~candidate
      ~protocol
      ?protocol_arguments
      ({
         class_hierarchy = { generic_parameters; _ };
         cycle_detections = { assumed_protocol_instantiations; _ } as cycle_detections;
         _;
       } as order)
      : Type.Argument.t list option
    =
    match candidate with
    | Type.Primitive candidate_name when Option.is_some (generic_parameters candidate_name) ->
        (* If we are given a "stripped" generic, we decline to do structural analysis, as these
           kinds of comparisons only exists for legacy reasons to do nominal comparisons *)
        None
    | Type.Primitive candidate_name when Identifier.equal candidate_name protocol -> Some []
    | Type.Parametric { name; arguments } when Identifier.equal name protocol -> Some arguments
    | _ -> (
        let assumed_protocol_parameters =
          AssumedProtocolInstantiations.find_assumed_protocol_parameters
            assumed_protocol_instantiations
            ~candidate
            ~protocol
        in
        match assumed_protocol_parameters with
        | Some result -> Some result
        | None ->
            let protocol_generics =
              generic_parameters protocol >>| List.map ~f:Type.GenericParameter.to_variable
            in
            let protocol_generics_as_args =
              protocol_generics >>| List.map ~f:Type.Variable.to_argument
            in
            let find_solution protocol_annotation =
              (* To handle recursive typing, we
               * (1) transform the candadate by marking all free vars as bound,
               *     (and in a fresh namespace) which will cause the constraint
               *     solver to solve for all vars.
               * (2) save the inverse mapping of the transform and write a function
               *     that will compose the constraint solution with that mapping
               *     in order to convert solved protocol type arguments back to the
               *     original candidate's type variables.
               *)
              let sanitized_candidate, desanitize_using_solution =
                (* Step (1): mark all free vars from the candidate as bound in a new namespace,
                 * saving the inverse transform mapping. *)
                let sanitized_candidate, desanitize_map =
                  match candidate with
                  | Type.Callable _ -> candidate, []
                  | _ ->
                      (* We don't return constraints for the candidate's free variables, so we must
                         underapproximate and determine conformance in the worst case *)
                      let desanitize_map, sanitized_candidate =
                        let namespace = Type.Variable.Namespace.create_fresh () in
                        let module SanitizeTransform = Type.VisitWithTransform.Make (struct
                          type state = Type.Variable.pair list

                          let visit_children_before _ _ = true

                          let visit_children_after = false

                          let visit sofar = function
                            | Type.Variable variable when Type.Variable.TypeVar.is_free variable ->
                                let transformed_variable =
                                  Type.Variable.TypeVar.namespace variable ~namespace
                                  |> Type.Variable.TypeVar.mark_as_bound
                                in
                                {
                                  Type.VisitWithTransform.transformed_annotation =
                                    Type.Variable transformed_variable;
                                  new_state =
                                    Type.Variable.TypeVarPair
                                      (transformed_variable, Type.Variable variable)
                                    :: sofar;
                                }
                            | transformed_annotation ->
                                {
                                  Type.VisitWithTransform.transformed_annotation;
                                  new_state = sofar;
                                }
                        end)
                        in
                        SanitizeTransform.visit [] candidate
                      in
                      sanitized_candidate, desanitize_map
                in
                (* Step (2): compose applying the constraints with the desanitize map to transform
                 * solved type arguments of the protocol to be in terms of the free vars of the
                 * candidate once more.
                 *
                 * The implementation is confusing but conceptually it's just two steps of search-and-replace.
                 *)
                let desanitize_using_solution_and_map solution =
                  (* This function is hard to skim, but it just desanitizes by composing the
                     constraint solution with the `desanitize_map`. *)
                  let desanitize =
                    let desanitization_solution = TypeConstraints.Solution.create desanitize_map in
                    let instantiate = function
                      | Type.Argument.Single single ->
                          [
                            Type.Argument.Single
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
                          |> Type.OrderedTypes.to_arguments
                    in
                    List.concat_map ~f:instantiate
                  in
                  let instantiate = function
                    | Type.Variable.TypeVarVariable variable ->
                        TypeConstraints.Solution.instantiate_single_type_var solution variable
                        |> Option.value ~default:(Type.Variable variable)
                        |> fun instantiated -> [Type.Argument.Single instantiated]
                    | ParamSpecVariable variable ->
                        TypeConstraints.Solution.instantiate_single_param_spec solution variable
                        |> Option.value
                             ~default:(Type.Callable.FromParamSpec { head = []; variable })
                        |> fun instantiated -> [Type.Argument.CallableParameters instantiated]
                    | TypeVarTupleVariable variadic ->
                        TypeConstraints.Solution.instantiate_ordered_types
                          solution
                          (Concatenation (Type.OrderedTypes.Concatenation.create variadic))
                        |> Type.OrderedTypes.to_arguments
                  in
                  protocol_generics
                  >>| List.concat_map ~f:instantiate
                  >>| desanitize
                  |> Option.value ~default:[]
                in
                sanitized_candidate, desanitize_using_solution_and_map
              in
              let order_with_new_assumption =
                let cycle_detections =
                  {
                    cycle_detections with
                    assumed_protocol_instantiations =
                      AssumedProtocolInstantiations.add
                        assumed_protocol_instantiations
                        ~candidate:sanitized_candidate
                        ~protocol
                        ~protocol_parameters:(Option.value protocol_generics_as_args ~default:[]);
                  }
                in
                { order with cycle_detections }
              in
              solve_candidate_less_or_equal
                order_with_new_assumption
                ~candidate:sanitized_candidate
                ~protocol_annotation
              >>| desanitize_using_solution
            in
            let protocol_annotations =
              let generic_protocol_annotation =
                protocol_generics_as_args
                >>| Type.parametric protocol
                |> Option.value ~default:(Type.Primitive protocol)
              in
              (* When protocol arguments are provided by the caller, we first try solving for them
                 before falling back to a protocol annotation with generic parameters. We keep only
                 the non-variable arguments because using the variable names from the protocol
                 definition produces better error messages. Falling back to the generic annotation
                 handles the case of `candidate` being an empty container. *)
              match protocol_arguments, protocol_generics_as_args with
              | Some arguments, Some generic_arguments
                when Int.equal (List.length arguments) (List.length generic_arguments) ->
                  let map argument generic_argument =
                    match argument with
                    | Type.Argument.Single (Type.Variable _) -> generic_argument
                    | _ -> argument
                  in
                  let protocol_annotation =
                    Type.Parametric
                      {
                        name = protocol;
                        arguments = List.map2_exn ~f:map arguments generic_arguments;
                      }
                  in
                  [protocol_annotation; generic_protocol_annotation]
              | _ -> [generic_protocol_annotation]
            in
            List.find_map ~f:find_solution protocol_annotations)


  (** As with `instantiate_recursive_type_with_solve`, here `None` means a failure to match
      `candidate` type with the protocol, whereas `Some []` means no generic constraints were
      induced. *)
  and instantiate_protocol_parameters ~candidate ~protocol ?protocol_arguments order =
    (* A candidate is less-or-equal to a protocol if candidate.x <: protocol.x for each attribute
       `x` in the protocol. *)
    let solve_all_protocol_attributes_less_or_equal
        ({ attribute; instantiated_attributes; cycle_detections; _ } as order)
        ~candidate
        ~protocol_annotation
      =
      let attribute_implements constraints_set protocol_attribute =
        match constraints_set with
        | [] -> []
        | _ ->
            let attribute_type attribute =
              AnnotatedAttribute.annotation attribute |> TypeInfo.Unit.annotation
            in
            let attribute_type_with_getattr_fallback ~attribute_lookup ~name =
              match attribute_lookup ~name with
              | Some found -> Some (attribute_type found)
              | None -> (
                  match attribute_lookup ~name:"__getattr__" >>| attribute_type with
                  | Some
                      (Type.Parametric
                        { name = "BoundMethod"; arguments = [Single (Type.Callable callable); _] })
                  | Some (Type.Callable callable) ->
                      Some callable.implementation.annotation
                  | _ -> None)
            in

            attribute_type_with_getattr_fallback
              ~attribute_lookup:(attribute ~cycle_detections candidate)
              ~name:(AnnotatedAttribute.name protocol_attribute)
            >>| (fun left ->
                  let right =
                    match attribute_type protocol_attribute with
                    | Type.Parametric { name = "BoundMethod"; _ } as bound_method ->
                        attribute ~cycle_detections bound_method ~name:"__call__"
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
        instantiated_attributes ~cycle_detections protocol_annotation
        >>| List.filter ~f:is_not_object_or_generic_method
      in
      protocol_attributes
      >>| List.fold ~init:[TypeConstraints.empty] ~f:attribute_implements
      >>| List.filter_map ~f:(OrderedConstraints.solve ~order)
      >>= List.hd
    in
    instantiate_recursive_type_with_solve
      order
      ~solve_candidate_less_or_equal:solve_all_protocol_attributes_less_or_equal
      ~candidate
      ~protocol
      ~protocol_arguments:(Option.value ~default:[] protocol_arguments)


  and instantiate_recursive_type_parameters order ~candidate ~recursive_type
      : Type.Argument.t list option
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
    instantiate_recursive_type_with_solve
      order
      ~solve_candidate_less_or_equal:solve_recursive_type_less_or_equal
      ~candidate
      ~protocol:(Type.RecursiveType.name recursive_type)


  let add_and_simplify existing_constraints ~new_constraint ~order =
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
           ~variables:[Type.Variable.ParamSpecVariable parameter_specification])
    |> List.map ~f:snd
    |> List.filter_map ~f:(fun solution ->
           TypeConstraints.Solution.instantiate_single_param_spec solution parameter_specification)
end
