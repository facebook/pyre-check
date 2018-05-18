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

module Assign = AnnotatedAssign
module Callable = AnnotatedCallable
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Access = AnnotatedAccess
module Signature = AnnotatedSignature


let rec resolve ~resolution expression =
  let with_generators resolution generators =
    let add_generator resolution {
        Comprehension.target;
        iterator;
        conditions;
        async = _; (* TODO(T23723699): resolve async comprehensions. *)
      } =
      let iterator_annotation =
        let annotation =
          Resolution.join
            resolution
            (resolve ~resolution iterator)
            (Type.iterable Type.Bottom)
        in
        match annotation with
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
      let iterator_annotation =
        let rec refine_optionals annotation { Node.value; _ } =
          match annotation, value with
          | Type.Tuple (Type.Bounded annotations), Tuple accesses
            when List.length annotations = List.length accesses ->
              Type.Tuple (Type.Bounded (List.map2_exn ~f:refine_optionals annotations accesses))
          | Type.Optional annotation, Access target_value ->
              if List.mem ~equal:Expression.Access.equal optional_accesses target_value then
                annotation
              else
                Type.Optional annotation
          | _ ->
              annotation
        in
        refine_optionals iterator_annotation target
      in
      let rec add resolution target annotation =
        match Node.value target, annotation with
        | Access access, _ ->
            Resolution.set_local
              resolution
              ~access:access
              ~annotation:(Annotation.create annotation)
        | Tuple accesses, Type.Tuple (Type.Bounded parameters)
          when List.length accesses = List.length parameters ->
            List.fold2_exn ~init:resolution ~f:add accesses parameters
        | Tuple accesses, Type.Tuple (Type.Unbounded parameter) ->
            let parameters = List.map ~f:(fun _ -> parameter) accesses in
            List.fold2_exn ~init:resolution ~f:add accesses parameters
        | _ ->
            resolution
      in
      add resolution target iterator_annotation
    in
    List.fold ~init:resolution ~f:add_generator generators;
  in

  match Node.value expression with
  | Access access ->
      let annotation _ ~resolution:_ ~resolved ~element:_ = resolved in
      Access.fold
        ~resolution
        ~initial:(Annotation.create Type.Top)
        ~f:annotation
        (Access.create access)
      |> Annotation.annotation

  | Await expression ->
      resolve ~resolution expression
      |> Resolution.join resolution (Type.awaitable Type.Bottom)
      |> Type.awaitable_value

  | BooleanOperator { BooleanOperator.left; right; operator } ->
      let left_type =
        match operator with
        | BooleanOperator.Or ->
            Type.optional_value (resolve ~resolution left)
        | _ ->
            resolve ~resolution left
      in
      Resolution.join resolution left_type (resolve ~resolution right)

  | Bytes _ ->
      Type.bytes

  | ComparisonOperator operator ->
      let fold_comparisons sofar = function
        | Some call ->
            Resolution.meet resolution sofar (resolve ~resolution call)
        | None ->
            Resolution.meet resolution sofar Type.bool
      in
      ComparisonOperator.override operator
      |> List.fold ~init:Type.Top ~f:fold_comparisons

  | Complex _ ->
      Type.complex

  | Dictionary { Dictionary.entries; _ } ->
      let key, values =
        let pair_wise_join (key_sofar, values) { Dictionary.key; value } =
          Resolution.join resolution key_sofar (resolve ~resolution key),
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
      |> List.fold ~init:Type.Bottom ~f:(Resolution.join resolution)
      |> Type.list

  | ListComprehension { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.list (resolve ~resolution element)

  | Set elements ->
      List.map ~f:(resolve ~resolution) elements
      |> List.fold ~init:Type.Bottom ~f:(Resolution.join resolution)
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
        let annotation =
          match resolve ~resolution access with
          | Type.Optional parameter -> Annotation.create parameter
          | parameter -> Annotation.create parameter
        in
        Resolution.set_local resolution ~access:key ~annotation
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
      Resolution.join
        updated_resolution
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

  | String _ ->
      Type.string

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve_literal ~resolution))

  | Expression.Yield _ ->
      Type.yield Type.Top

  | _ ->
      Type.Top
