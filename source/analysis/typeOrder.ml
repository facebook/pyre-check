(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Pyre
open CycleDetection

type class_hierarchy = ConstraintsSet.class_hierarchy

type order = ConstraintsSet.order

module type FullOrderType = sig
  type t = order

  val always_less_or_equal : order -> left:Type.t -> right:Type.t -> bool

  val meet : order -> Type.t -> Type.t -> Type.t

  val join : order -> Type.t -> Type.t -> Type.t
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module OrderImplementation = struct
  module Make (OrderedConstraintsSet : ConstraintsSet.OrderedConstraintsSetType) = struct
    type t = order

    let rec always_less_or_equal order ~left ~right =
      OrderedConstraintsSet.add_and_simplify
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
                      if Type.Callable.CallableParamType.names_compatible left right then
                        match left, right with
                        | ( CallableParamType.PositionalOnly left,
                            CallableParamType.PositionalOnly right )
                          when Bool.equal left.default right.default ->
                            Some
                              (CallableParamType.PositionalOnly
                                 {
                                   left with
                                   annotation =
                                     parameter_join order left.annotation right.annotation;
                                 })
                        | CallableParamType.PositionalOnly anonymous, CallableParamType.Named named
                        | CallableParamType.Named named, CallableParamType.PositionalOnly anonymous
                          when Bool.equal named.default anonymous.default ->
                            Some
                              (CallableParamType.PositionalOnly
                                 {
                                   anonymous with
                                   annotation =
                                     parameter_join order named.annotation anonymous.annotation;
                                 })
                        | CallableParamType.Named left, CallableParamType.Named right
                          when Bool.equal left.default right.default ->
                            Some
                              (CallableParamType.Named
                                 {
                                   left with
                                   annotation =
                                     parameter_join order left.annotation right.annotation;
                                 })
                        | ( CallableParamType.Variable (Concrete left),
                            CallableParamType.Variable (Concrete right) ) ->
                            Some
                              (CallableParamType.Variable
                                 (Concrete (parameter_join order left right)))
                        | CallableParamType.Keywords left, CallableParamType.Keywords right ->
                            Some (CallableParamType.Keywords (parameter_join order left right))
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
            | _ -> None)
        | Undefined, Defined right -> Some (Defined right)
        | Defined left, Undefined -> Some (Defined left)
        | _ -> None
      in
      parameters
      >>| fun parameters ->
      { annotation = return_join order left.annotation right.annotation; parameters }


    and join
        ({
           ConstraintsSet.class_hierarchy =
             { least_upper_bound; instantiate_successors_parameters; generic_parameters; _ };
           is_protocol;
           variance_map;
           _;
         } as order)
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
        | Type.ParamSpecComponent _, _
        | _, Type.ParamSpecComponent _ ->
            union
        | Type.NoneType, _
        | _, Type.NoneType ->
            union
        | Type.PyreReadOnly left, _ -> Type.PyreReadOnly.create (join order left right)
        | _, Type.PyreReadOnly right -> Type.PyreReadOnly.create (join order left right)
        | Type.RecursiveType left_recursive_type, Type.RecursiveType right_recursive_type ->
            let new_name = Type.RecursiveType.Namespace.create_fresh_name () in
            (* Based on https://cstheory.stackexchange.com/a/38415. *)
            Type.RecursiveType.create
              ~name:new_name
              ~body:
                (join
                   order
                   (Type.RecursiveType.body_with_replaced_name ~new_name left_recursive_type)
                   (Type.RecursiveType.body_with_replaced_name ~new_name right_recursive_type))
        | Type.RecursiveType _, _
        | _, Type.RecursiveType _ ->
            if always_less_or_equal order ~left ~right then
              right
            else if always_less_or_equal order ~left:right ~right:left then
              left
            else
              Type.union [left; right]
        (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
        | Type.Union left, Type.Union right -> Type.union (left @ right)
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if always_less_or_equal order ~left:other ~right:union && not (Type.contains_any other)
            then
              union
            else
              let rec flat_join elements new_element =
                match elements with
                | [] -> [new_element]
                | [head] -> (
                    match join order head new_element with
                    | Type.Union _ -> [head; new_element]
                    | joined -> [joined])
                | head :: tail -> (
                    match join order head new_element with
                    | Type.Union _ -> head :: flat_join tail new_element
                    | joined -> joined :: tail)
              in
              Type.union (List.fold ~f:flat_join ~init:[] (other :: elements))
        | _, Type.Variable _
        | Type.Variable _, _ ->
            union
        | ( Type.Parametric { name = left_primitive; _ },
            Type.Parametric { name = right_primitive; _ } )
        | Type.Parametric { name = left_primitive; _ }, Type.Primitive right_primitive
        | Type.Primitive left_primitive, Type.Parametric { name = right_primitive; _ } ->
            if always_less_or_equal order ~left ~right && not (Type.contains_any left) then
              right
            else if
              always_less_or_equal order ~left:right ~right:left && not (Type.contains_any right)
            then
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
              let handle_target target ~parameters =
                let left_arguments = instantiate_successors_parameters ~source:left ~target in
                let right_arguments = instantiate_successors_parameters ~source:right ~target in
                let join_arguments_respecting_variance = function
                  | Type.GenericParameter.ZipTwoArgumentsLists.TypeVarZipResult
                      { name; left; right; _ } -> (
                      let variance =
                        Map.find (variance_map ~class_name:target ~parameters) name
                        |> Option.value ~default:Type.Record.Variance.Invariant
                      in

                      match left, right, variance with
                      | Type.Bottom, other, _
                      | other, Type.Bottom, _ ->
                          Some other
                      | Type.Top, _, _
                      | _, Type.Top, _ ->
                          Some Type.Top
                      | left, right, Type.Record.Variance.Covariant -> Some (join order left right)
                      | left, right, Type.Record.Variance.Contravariant -> (
                          match meet order left right with
                          | Type.Bottom -> None
                          | not_bottom -> Some not_bottom)
                      | left, right, Type.Record.Variance.Invariant ->
                          if
                            always_less_or_equal order ~left ~right
                            && always_less_or_equal order ~left:right ~right:left
                          then
                            Some left
                          else
                            None
                      | _, _, Type.Record.Variance.Bivariant -> Some Type.Bottom)
                  | Type.GenericParameter.ZipTwoArgumentsLists.TypeVarTupleZipResult _
                  | Type.GenericParameter.ZipTwoArgumentsLists.ParamSpecZipResult _ ->
                      (* TODO(T47348395): Implement joining for variadics *)
                      None
                  | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedKindsZipResult _
                  | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedLengthsZipResult _
                  | Type.GenericParameter.ZipTwoArgumentsLists.MismatchedVariadicZipResult _ ->
                      None
                in
                match left_arguments, right_arguments, generic_parameters target with
                | Some left_arguments, Some right_arguments, Some parameters ->
                    let replace_free_unary_variables_with_top =
                      let replace_if_free variable =
                        Option.some_if (Type.Variable.TypeVar.is_free variable) Type.Top
                      in
                      Type.Variable.GlobalTransforms.TypeVar.replace_all replace_if_free
                    in
                    Type.GenericParameter.ZipTwoArgumentsLists.zip
                      ~left_arguments
                      ~right_arguments
                      parameters
                    |> List.map ~f:join_arguments_respecting_variance
                    |> Option.all
                    >>| List.map ~f:replace_free_unary_variables_with_top
                    >>| List.map ~f:(fun single -> Type.Argument.Single single)
                    >>| fun arguments -> Type.parametric target arguments
                | _ -> None
              in
              target
              >>= handle_target
                    ~parameters:
                      (generic_parameters (target |> Option.value ~default:"")
                      |> Option.value ~default:[])
              |> Option.value ~default:union
        | Type.Tuple (Concrete left), Type.Tuple (Concrete right)
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(join order) |> Type.tuple
        | Type.Tuple (Concatenation left), Type.Tuple (Concatenation right) ->
            let left_unbounded_element =
              Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation left
              |> Option.value ~default:Type.object_primitive
            in
            let right_unbounded_element =
              Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation right
              |> Option.value ~default:Type.object_primitive
            in
            Type.Tuple
              (Type.OrderedTypes.create_unbounded_concatenation
                 (join order left_unbounded_element right_unbounded_element))
        | Type.Tuple (Concrete concrete), Type.Tuple (Concatenation concatenation)
        | Type.Tuple (Concatenation concatenation), Type.Tuple (Concrete concrete) -> (
            let unbounded_element =
              Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
              |> Option.value ~default:Type.object_primitive
            in
            match concrete with
            | concrete_head :: tail
              when List.for_all ~f:(fun element -> Type.equal element concrete_head) tail
                   && always_less_or_equal order ~left:concrete_head ~right:unbounded_element ->
                Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation unbounded_element)
            | _ ->
                Type.union
                  [
                    Tuple (Concrete concrete);
                    Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation unbounded_element);
                  ])
        | other, Type.Tuple (Concatenation concatenation)
        | Type.Tuple (Concatenation concatenation), other -> (
            let unbounded_element =
              Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
              |> Option.value ~default:Type.object_primitive
            in
            match other with
            | (Type.Parametric _ as annotation)
            | (Type.Primitive _ as annotation) ->
                join order (Type.parametric "tuple" [Single unbounded_element]) annotation
            | _ -> Type.union [left; right])
        | Type.Tuple (Concrete arguments), (Type.Parametric _ as annotation)
        | (Type.Parametric _ as annotation), Type.Tuple (Concrete arguments) ->
            (* Handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
            let argument = List.fold ~init:Type.Bottom ~f:(join order) arguments in
            join order (Type.parametric "tuple" [Single argument]) annotation
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
            | None -> Type.union [left; right])
        | Type.Literal (String _), Type.Literal (String _) -> Type.Literal (String AnyLiteral)
        | (Type.Literal _ as literal), other
        | other, (Type.Literal _ as literal) ->
            join order other (Type.weaken_literals literal)
        | _ when is_protocol right && always_less_or_equal order ~left ~right -> right
        | _ when is_protocol left && always_less_or_equal order ~left:right ~right:left -> left
        | Primitive left, Primitive right -> (
            match least_upper_bound left right with
            | Some joined ->
                if Type.Primitive.equal joined left then
                  Type.Primitive left
                else if Type.Primitive.equal joined right then
                  Type.Primitive right
                else
                  union
            | None -> union)
        | TypeOperation _, _
        | _, TypeOperation _ ->
            union


    and meet_callable_implementations
        order
        { Type.Callable.parameters = left_parameters; annotation = left_annotation; _ }
        { Type.Callable.parameters = right_parameters; annotation = right_annotation; _ }
      =
      let open Callable in
      let parameters =
        match left_parameters, right_parameters with
        | Undefined, Undefined -> Some Undefined
        | Defined left, Defined right -> (
            let meet_of_parameters left right =
              if Type.Callable.CallableParamType.names_compatible left right then
                match left, right with
                | CallableParamType.PositionalOnly left, CallableParamType.PositionalOnly right
                  when Bool.equal left.default right.default ->
                    Some
                      (CallableParamType.PositionalOnly
                         { left with annotation = join order left.annotation right.annotation })
                | CallableParamType.PositionalOnly anonymous, CallableParamType.Named named
                | CallableParamType.Named named, CallableParamType.PositionalOnly anonymous
                  when Bool.equal named.default anonymous.default ->
                    Some
                      (CallableParamType.Named
                         {
                           named with
                           annotation = join order named.annotation anonymous.annotation;
                         })
                | CallableParamType.Named left, CallableParamType.Named right
                  when Bool.equal left.default right.default ->
                    Some
                      (CallableParamType.Named
                         { left with annotation = join order left.annotation right.annotation })
                | ( CallableParamType.Variable (Concrete left),
                    CallableParamType.Variable (Concrete right) ) ->
                    Some (CallableParamType.Variable (Concrete (join order left right)))
                | CallableParamType.Keywords left, CallableParamType.Keywords right ->
                    Some (CallableParamType.Keywords (join order left right))
                | _ -> None
              else
                None
            in
            let add_meet_of_parameters sofar left right =
              sofar
              >>= fun sofar ->
              meet_of_parameters left right
              >>| fun meet_of_parameters -> meet_of_parameters :: sofar
            in
            match List.fold2 ~init:(Some []) ~f:add_meet_of_parameters left right with
            | Ok parameters -> parameters >>| fun parameters -> Defined (List.rev parameters)
            | Unequal_lengths -> None)
        | Undefined, Defined _
        | Defined _, Undefined ->
            Some Undefined
        | _ -> None
      in
      parameters
      >>| fun parameters -> { annotation = meet order left_annotation right_annotation; parameters }


    and meet ({ is_protocol; _ } as order) left right =
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
        | Type.ParamSpecComponent _, _
        | _, Type.ParamSpecComponent _ ->
            Type.Bottom
        | PyreReadOnly left, PyreReadOnly right -> Type.PyreReadOnly.create (meet order left right)
        | PyreReadOnly left, _ -> meet order left right
        | _, PyreReadOnly right -> meet order left right
        | (Type.Variable _ as variable), other
        | other, (Type.Variable _ as variable) ->
            if always_less_or_equal order ~left:variable ~right:other then
              variable
            else
              Type.Bottom
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if always_less_or_equal order ~left:other ~right:union then
              other
            else
              List.map elements ~f:(meet order other) |> List.fold ~f:(join order) ~init:Type.Bottom
        | Type.NoneType, _
        | _, Type.NoneType ->
            Type.Bottom
        | Type.Parametric _, Type.Parametric _
        | Type.Primitive _, Type.Primitive _ ->
            if always_less_or_equal order ~left ~right then
              left
            else if always_less_or_equal order ~left:right ~right:left then
              right
            else
              Type.Bottom
        | Type.Tuple (Concrete left), Type.Tuple (Concrete right)
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(meet order) |> Type.tuple
        | Type.Tuple (Concatenation left), Type.Tuple (Concatenation right) -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation left,
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation right )
            with
            | Some left_unbounded_element, Some right_unbounded_element ->
                Type.Tuple
                  (Type.OrderedTypes.create_unbounded_concatenation
                     (meet order left_unbounded_element right_unbounded_element))
            | _ -> Type.Bottom)
        | Type.Tuple (Concrete concrete), Type.Tuple (Concatenation concatenation)
        | Type.Tuple (Concatenation concatenation), Type.Tuple (Concrete concrete) -> (
            match
              ( Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation,
                concrete )
            with
            | Some unbounded_element, concrete_head :: tail
              when List.for_all ~f:(fun element -> Type.equal element concrete_head) tail
                   && always_less_or_equal order ~left:concrete_head ~right:unbounded_element ->
                Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation concrete_head)
            | _ -> Type.Bottom)
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
        | Type.RecursiveType _, _
        | _, Type.RecursiveType _ ->
            if always_less_or_equal order ~left ~right then
              left
            else if always_less_or_equal order ~left:right ~right:left then
              right
            else
              Type.Bottom
        | ( (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            callable
        | ( Type.Callable
              {
                overloads = [];
                kind = left_kind;
                implementation = { annotation = left_annotation; _ } as left_implementation;
              },
            Type.Callable
              {
                overloads = [];
                kind = right_kind;
                implementation = { annotation = right_annotation; _ } as right_implementation;
              } ) ->
            let kind =
              if Type.Callable.equal_kind left_kind right_kind then
                left_kind
              else
                Type.Callable.Anonymous
            in
            meet_callable_implementations order left_implementation right_implementation
            >>| (fun implementation ->
                  Type.Callable { Callable.kind; implementation; overloads = [] })
            |> Option.value
                 ~default:
                   (Type.Callable.create
                      ~annotation:(meet order left_annotation right_annotation)
                      ())
        | Type.Callable _, _
        | _, Type.Callable _ ->
            Bottom
        | (Type.Literal _ as int_expression), other
        | other, (Type.Literal _ as int_expression) ->
            if always_less_or_equal order ~left:int_expression ~right:other then
              int_expression
            else
              Type.Bottom
        | Type.Primitive _, _ when always_less_or_equal order ~left ~right -> left
        | _, Type.Primitive _ when always_less_or_equal order ~left:right ~right:left -> right
        | _ when is_protocol right && always_less_or_equal order ~left ~right -> left
        | _ when is_protocol left && always_less_or_equal order ~left:right ~right:left -> right
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

module IncludableImplementation : FullOrderType = Implementation

include IncludableImplementation

let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next
