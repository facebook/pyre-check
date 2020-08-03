(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Assumptions

type class_hierarchy = ConstraintsSet.class_hierarchy

type order = ConstraintsSet.order

module type FullOrderTypeWithoutT = sig
  val always_less_or_equal : order -> left:Type.t -> right:Type.t -> bool

  val meet : order -> Type.t -> Type.t -> Type.t

  val join : order -> Type.t -> Type.t -> Type.t
end

module type FullOrderType = sig
  type t = order

  include FullOrderTypeWithoutT
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module OrderImplementation = struct
  module Make (OrderedConstraintsSet : ConstraintsSet.OrderedConstraintsSetType) = struct
    type t = order

    let rec always_less_or_equal order ~left ~right =
      OrderedConstraintsSet.add
        ConstraintsSet.empty
        ~new_constraint:
          (LessOrEqual
             {
               left = Type.Variable.mark_all_variables_as_bound left;
               right = Type.Variable.mark_all_variables_as_bound right;
             })
        ~order
      (* This potential is not just potential in this case, since this will always be accurate when
         there are no free type variables, as in this case *)
      |> ConstraintsSet.potentially_satisfiable


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
                        | Parameter.PositionalOnly left, Parameter.PositionalOnly right
                          when Bool.equal left.default right.default ->
                            Some
                              (Parameter.PositionalOnly
                                 {
                                   left with
                                   annotation =
                                     parameter_join order left.annotation right.annotation;
                                 })
                        | Parameter.PositionalOnly anonymous, Parameter.Named named
                        | Parameter.Named named, Parameter.PositionalOnly anonymous
                          when Bool.equal named.default anonymous.default ->
                            Some
                              (Parameter.PositionalOnly
                                 {
                                   anonymous with
                                   annotation =
                                     parameter_join order named.annotation anonymous.annotation;
                                 })
                        | Parameter.Named left, Parameter.Named right
                          when Bool.equal left.default right.default ->
                            Some
                              (Parameter.Named
                                 {
                                   left with
                                   annotation =
                                     parameter_join order left.annotation right.annotation;
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
      { annotation = return_join order left.annotation right.annotation; parameters }


    and join
        ( {
            ConstraintsSet.class_hierarchy =
              { least_upper_bound; instantiate_successors_parameters; variables; _ };
            is_protocol;
            assumptions = { protocol_assumptions; _ };
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
        | Type.Top, _
        | _, Type.Top ->
            Type.Top
        | Type.Any, _
        | _, Type.Any ->
            Type.Any
        | Type.ParameterVariadicComponent _, _
        | _, Type.ParameterVariadicComponent _ ->
            union
        | Type.NoneType, _
        | _, Type.NoneType ->
            union
        | Type.Annotated left, _ -> Type.annotated (join order left right)
        | _, Type.Annotated right -> Type.annotated (join order left right)
        (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
        | Type.Union left, Type.Union right -> Type.union (left @ right)
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if always_less_or_equal order ~left:other ~right:union then
              union
            else
              List.map elements ~f:(join order other) |> List.fold ~f:(join order) ~init:Type.Bottom
        | Type.IntExpression polynomial, other when Type.Polynomial.is_base_case polynomial ->
            join order other (Type.polynomial_to_type polynomial)
        | other, Type.IntExpression polynomial when Type.Polynomial.is_base_case polynomial ->
            join order other (Type.polynomial_to_type polynomial)
        | Type.IntExpression _, other
        | other, Type.IntExpression _ ->
            join order other (Type.Primitive "int")
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
                let left_parameters = instantiate_successors_parameters ~source:left ~target in
                let right_parameters = instantiate_successors_parameters ~source:right ~target in
                let variables = variables target in
                let join_parameters (left, right, variable) =
                  match left, right, variable with
                  | Type.Parameter.Group _, _, _
                  | _, Type.Parameter.Group _, _
                  | _, _, Type.Variable.ListVariadic _
                  | CallableParameters _, _, _
                  | _, CallableParameters _, _
                  | _, _, ParameterVariadic _ ->
                      (* TODO(T47348395): Implement joining for variadics *)
                      None
                  | Single Type.Bottom, Single other, _
                  | Single other, Single Type.Bottom, _ ->
                      Some other
                  | Single Type.Top, _, _
                  | _, Single Type.Top, _ ->
                      Some Type.Top
                  | Single left, Single right, Unary { variance = Covariant; _ } ->
                      Some (join order left right)
                  | Single left, Single right, Unary { variance = Contravariant; _ } -> (
                      match meet order left right with
                      | Type.Bottom -> None
                      | not_bottom -> Some not_bottom )
                  | Single left, Single right, Unary { variance = Invariant; _ } ->
                      if
                        always_less_or_equal order ~left ~right
                        && always_less_or_equal order ~left:right ~right:left
                      then
                        Some left
                      else
                        None
                in
                match left_parameters, right_parameters, variables with
                | Some left_parameters, Some right_parameters, Some variables ->
                    let replace_free_unary_variables_with_top =
                      let replace_if_free variable =
                        Option.some_if (Type.Variable.Unary.is_free variable) Type.Top
                      in
                      Type.Variable.GlobalTransforms.Unary.replace_all replace_if_free
                    in
                    Type.Variable.zip_on_two_parameter_lists
                      ~left_parameters
                      ~right_parameters
                      variables
                    >>| List.map ~f:join_parameters
                    >>= Option.all
                    >>| List.map ~f:replace_free_unary_variables_with_top
                    >>| List.map ~f:(fun single -> Type.Parameter.Single single)
                    >>| fun parameters -> Type.Parametric { name = target; parameters }
                | _ -> None
              in
              target >>= handle_target |> Option.value ~default:union
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded (Concatenation _)), other
        | other, Type.Tuple (Type.Bounded (Concatenation _)) ->
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
            join order (Type.parametric "tuple" [Single parameter]) annotation
        | Type.Tuple (Type.Bounded (Concrete parameters)), (Type.Parametric _ as annotation) ->
            (* Handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
            let parameter = List.fold ~init:Type.Bottom ~f:(join order) parameters in
            join order (Type.parametric "tuple" [Single parameter]) annotation
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            Type.union [left; right]
        | ( (Type.Callable { Callable.kind = Callable.Named left_name; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right_name; _ } ) ->
            if Reference.equal left_name right_name then
              callable
            else
              Type.union [left; right]
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
        | other, Type.Callable callable -> (
            match
              ConstraintsSet.resolve_callable_protocol ~order ~assumption:(Callable callable) other
            with
            | Some other_callable -> join order other_callable (Type.Callable callable)
            | None -> Type.union [left; right] )
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
            match List.hd (least_upper_bound left right) with
            | Some joined ->
                if Type.Primitive.equal joined left then
                  Type.Primitive left
                else if Type.Primitive.equal joined right then
                  Type.Primitive right
                else
                  union
            | None -> union )


    and meet ({ is_protocol; assumptions = { protocol_assumptions; _ }; _ } as order) left right =
      if Type.equal left right then
        left
      else
        match left, right with
        | Type.Top, other
        | other, Type.Top ->
            other
        | Type.Any, other when not (Type.contains_unknown other) -> other
        | other, Type.Any when not (Type.contains_unknown other) -> other
        | Type.Bottom, _
        | _, Type.Bottom ->
            Type.Bottom
        | Type.ParameterVariadicComponent _, _
        | _, Type.ParameterVariadicComponent _ ->
            Type.Bottom
        | Type.NoneType, _
        | _, Type.NoneType ->
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
            let union = Set.inter (Type.Set.of_list left) (Type.Set.of_list right) |> Set.to_list in
            Type.union union
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if always_less_or_equal order ~left:other ~right:union then
              other
            else
              List.map elements ~f:(meet order other) |> List.fold ~f:(meet order) ~init:Type.Top
        | Type.Parametric _, Type.Parametric _
        | Type.Primitive _, Type.Primitive _ ->
            if always_less_or_equal order ~left ~right then
              left
            else if always_less_or_equal order ~left:right ~right:left then
              right
            else
              Type.Bottom
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded (Concatenation _)), _
        | _, Type.Tuple (Type.Bounded (Concatenation _)) ->
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
        | Type.Callable _, _
        | _, Type.Callable _ ->
            Bottom
        | (Type.IntExpression _ as int_expression), other
        | other, (Type.IntExpression _ as int_expression)
        | (Type.Literal _ as int_expression), other
        | other, (Type.Literal _ as int_expression) ->
            if always_less_or_equal order ~left:int_expression ~right:other then
              int_expression
            else
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
        | _ ->
            Log.debug "No lower bound found for %a and %a" Type.pp left Type.pp right;
            Type.Bottom
  end
end

module rec Constraints : OrderedConstraintsType = TypeConstraints.OrderedConstraints (Implementation)

and OrderedConstraintsSet : ConstraintsSet.OrderedConstraintsSetType =
  ConstraintsSet.Make (Constraints)

and Implementation : FullOrderType = OrderImplementation.Make (OrderedConstraintsSet)

let instantiate_protocol_parameters = OrderedConstraintsSet.instantiate_protocol_parameters

module OrderedConstraints = Constraints

module IncludableImplementation : FullOrderTypeWithoutT = Implementation

include IncludableImplementation

let rec is_compatible_with order ~left ~right =
  match left, right with
  (* Any *)
  | _, Type.Any
  | Type.Any, _ ->
      true
  (* Top *)
  | _, Type.Top -> true
  | Type.Top, _ -> false
  (* None *)
  | Type.NoneType, Type.NoneType -> true
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
  | ( Parametric { name = left_name; parameters = left_parameters },
      Parametric { name = right_name; parameters = right_parameters } )
    when Type.Primitive.equal left_name right_name
         && Int.equal (List.length left_parameters) (List.length right_parameters) -> (
      match
        Type.Parameter.all_singles left_parameters, Type.Parameter.all_singles right_parameters
      with
      | Some left_parameters, Some right_parameters ->
          List.for_all2_exn left_parameters right_parameters ~f:(fun left right ->
              is_compatible_with order ~left ~right)
      | _ -> always_less_or_equal order ~left ~right )
  (* Fallback *)
  | _, _ -> always_less_or_equal order ~left ~right


let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next
