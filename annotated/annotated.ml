(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Call = AnnotatedCall
module Access = AnnotatedAccess


let rec resolve ~resolution expression =
  let with_generators resolution generators =
    let add_generator resolution {
        Comprehension.target = { Node.value = target_value; _ } as target;
        iterator;
        conditions;
        async = _; (* TODO(T23723699): resolve async comprehensions. *)
      } =
      let iterator_type =
        match
          TypeOrder.join
            (Resolution.order resolution)
            (resolve ~resolution iterator)
            (Type.iterable Type.Bottom)
        with
        | Type.Parametric { Type.parameters = [parameter]; _ } ->
            parameter
        | _ ->
            Type.Object
      in
      let rec collect_optionals { Node.value; _ } =
        match value with
        | BooleanOperator {
            BooleanOperator.left;
            operator = BooleanOperator.And;
            right;
          } ->
            List.rev_append (collect_optionals left) (collect_optionals right)

        | Access access ->
            [access]

        | ComparisonOperator {
            Expression.ComparisonOperator.left = { Node.value = Access access; _ };
            right =
              [
                Expression.ComparisonOperator.IsNot,
                { Node.value = Access [Expression.Access.Identifier identifier]; _ }
              ];
          } when Identifier.show identifier = "None" ->
            [access]

        | _ -> []
      in
      let optional_accesses = List.concat_map ~f:collect_optionals conditions in
      let iterator_type = match iterator_type, target_value with
        | Type.Optional annotation, Access target_value ->
            if List.mem ~equal:Expression.Access.equal optional_accesses target_value then
              annotation
            else
              iterator_type
        | _ -> iterator_type
      in
      let annotations =
        let rec add annotations_sofar target annotation =
          match Node.value target, annotation with
          | Access access, _ ->
              Map.set annotations_sofar ~key:access ~data:(Annotation.create annotation)
          | Tuple accesses, Type.Tuple (Type.Bounded parameters)
            when List.length accesses = List.length parameters ->
              List.fold2_exn ~init:annotations_sofar ~f:add accesses parameters
          | Tuple accesses, Type.Tuple (Type.Unbounded parameter) ->
              let parameters = List.map ~f:(fun _ -> parameter) accesses in
              List.fold2_exn ~init:annotations_sofar ~f:add accesses parameters
          | _ -> annotations_sofar
        in
        add (Resolution.annotations resolution) target iterator_type
      in
      Resolution.with_annotations resolution annotations
    in
    List.fold ~init:resolution ~f:add_generator generators;
  in

  match Node.value expression with
  | Access access ->
      let annotation _ ~annotations:_ ~resolved ~element:_ = resolved in
      Access.fold
        ~resolution
        ~initial:(Annotation.create Type.Top)
        ~f:annotation
        (Access.create access)
      |> Annotation.annotation

  | Await expression ->
      resolve ~resolution expression
      |> Type.awaitable_value

  | BinaryOperator _ ->
      failwith "Binary Operator inference not supported"

  | BooleanOperator { BooleanOperator.left; right; operator } ->
      let left_type =
        match operator with
        | BooleanOperator.Or ->
            Type.optional_value (resolve ~resolution left)
        | _ ->
            resolve ~resolution left
      in
      TypeOrder.join
        (Resolution.order resolution)
        left_type
        (resolve ~resolution right)

  | Bytes _ ->
      Type.bytes

  | ComparisonOperator operator ->
      let fold_comparisons sofar = function
        | Some call ->
            TypeOrder.meet
              (Resolution.order resolution)
              sofar
              (resolve ~resolution call)
        | None ->
            TypeOrder.meet
              (Resolution.order resolution)
              sofar
              Type.bool
      in
      ComparisonOperator.override operator
      |> List.fold ~init:Type.Top ~f:fold_comparisons

  | Complex _ ->
      Type.complex

  | Dictionary { Dictionary.entries; _ } ->
      let key, values =
        let pair_wise_join (key_sofar, values) { Dictionary.key; value } =
          TypeOrder.join (Resolution.order resolution) key_sofar (resolve ~resolution key),
          (resolve ~resolution value) :: values
        in
        List.fold entries ~init:(Type.Bottom, []) ~f:pair_wise_join
      in
      if List.is_empty entries then
        Type.dictionary ~key:Type.Bottom ~value:Type.Bottom
      else
        Type.dictionary ~key ~value:(Type.union values)

  | DictionaryComprehension { Comprehension.element = { Dictionary.key; value }; generators } ->
      let resolution = with_generators resolution generators in
      Type.dictionary
        ~key:(Type.assume_any (resolve ~resolution key))
        ~value:(Type.assume_any (resolve ~resolution value))

  | Generator { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.generator (resolve ~resolution element |> Type.assume_any)

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Format _ ->
      Type.string

  | Integer _ ->
      Type.integer

  | Lambda { Lambda.body; Lambda.parameters; _ } ->
      Type.lambda
        ~parameters:(List.map ~f:(fun _ -> Type.Object) parameters)
        ~return_annotation:(resolve ~resolution body)

  | List elements ->
      List.map ~f:(resolve ~resolution) elements
      |> List.fold
        ~init:Type.Bottom
        ~f:(TypeOrder.join (Resolution.order resolution))
      |> Type.list

  | ListComprehension { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.list (resolve ~resolution element)

  | Set elements ->
      List.map ~f:(resolve ~resolution) elements
      |> List.fold
        ~init:Type.Bottom
        ~f:(TypeOrder.join (Resolution.order resolution))
      |> Type.set

  | SetComprehension { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.set (resolve ~resolution element)

  | Starred _ ->
      Type.Object

  | String _ ->
      Type.string

  | Ternary { Ternary.target; alternative; test; } ->
      let deoptionalize key access resolution =
        let updated_annotation =
          match resolve ~resolution access with
          | Type.Optional parameter -> Annotation.create parameter
          | parameter -> Annotation.create parameter
        in
        Map.set ~key ~data:updated_annotation (Resolution.annotations resolution)
        |> Resolution.with_annotations resolution
      in
      let updated_resolution =
        match Node.value test with
        | Expression.Access access ->
            deoptionalize access test resolution
        | Expression.ComparisonOperator {
            Expression.ComparisonOperator.left = {
              Node.value = Expression.Access access;
              _;
            } as access_node;
            right = [
              Expression.ComparisonOperator.IsNot,
              { Node.value = Access [Expression.Access.Identifier identifier]; _ }
            ];
          } when Identifier.show identifier = "None" ->
            deoptionalize access access_node resolution
        | _ -> resolution
      in
      TypeOrder.join
        (Resolution.order updated_resolution)
        (resolve ~resolution:updated_resolution target)
        (resolve ~resolution alternative)

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve ~resolution))

  | UnaryOperator operator ->
      UnaryOperator.override operator
      >>| resolve ~resolution
      |> Option.value ~default:Type.bool

  | Expression.Yield _ ->
      Type.yield Type.Object


(* In general, python expressions can be self-referential. This non-recursive resolution only checks
   literals and annotations found in the resolution map, without any resolutions/joins. *)
let rec resolve_literal ~resolution expression =
  match Node.value expression with
  | Access access ->
      begin
        match
          Map.find (Resolution.annotations resolution) access,
          Resolution.global resolution access with
        | Some local, _ ->
            Annotation.annotation local
        | None, Some { Resolution.annotation; _ } ->
            Annotation.annotation annotation
        | None, None ->
            Type.Object
      end

  | Await expression ->
      resolve_literal ~resolution expression
      |> Type.awaitable_value

  | Bytes _ ->
      Type.bytes

  | Complex _ ->
      Type.complex

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Format _ ->
      Type.string

  | Integer _ ->
      Type.integer
  | Starred _ ->
      Type.Object

  | String _ ->
      Type.string

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve_literal ~resolution))

  | Expression.Yield _ ->
      Type.yield Type.Object

  | _ ->
      Type.Object
