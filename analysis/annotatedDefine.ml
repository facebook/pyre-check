(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement
module Callable = AnnotatedCallable
module Class = AnnotatedClass

type t = Define.t Node.t [@@deriving compare, eq, sexp, show, hash]

let create definition = definition

let define annotated = annotated

let parameter_annotations
    { Node.value = { Define.signature = { parameters; _ }; _ }; _ }
    ~resolution
  =
  let element index { Node.value = { Expression.Parameter.annotation; _ }; _ } =
    let annotation =
      annotation
      >>| (fun annotation -> GlobalResolution.parse_annotation resolution annotation)
      |> Option.value ~default:Type.Top
    in
    index, annotation
  in
  List.mapi ~f:element parameters |> Int.Map.of_alist_exn


let parent_definition { Node.value = { Define.signature = { parent; _ }; _ }; _ } ~resolution =
  match parent with
  | Some parent ->
      let parent_type = Type.Primitive (Reference.show parent) in
      GlobalResolution.class_definition resolution parent_type >>| Class.create
  | _ -> None


let decorate
    ( {
        Node.value =
          {
            Define.signature =
              { Define.Signature.decorators; parameters = original_parameters; _ } as signature;
            _;
          } as define;
        location;
      } as define_node )
    ~resolution
  =
  match decorators with
  | [] -> define_node
  | _ ->
      let { Type.Callable.parameters; annotation; _ } =
        Node.create signature ~location |> GlobalResolution.create_overload ~resolution
      in
      let parameters =
        match parameters with
        | Defined parameters ->
            let convert (placed_single_star, sofar) parameter =
              let location = Location.any in
              let placed_single_star, sofar =
                match placed_single_star, parameter with
                | false, Type.Callable.Parameter.KeywordOnly _ ->
                    true, Expression.Parameter.create ~location ~name:"*" () :: sofar
                | _ -> placed_single_star, sofar
              in
              let new_parameter =
                match parameter with
                | Type.Callable.Parameter.Anonymous { annotation; _ } ->
                    (* This means it will be read back in as an anonymous *)
                    Expression.Parameter.create
                      ~location
                      ~annotation:(Type.expression annotation)
                      ~name:"__"
                      ()
                | Type.Callable.Parameter.KeywordOnly { name; annotation; _ }
                | Type.Callable.Parameter.Named { name; annotation; _ } ->
                    Expression.Parameter.create
                      ~location
                      ~annotation:(Type.expression annotation)
                      ~name
                      ()
                | Type.Callable.Parameter.Variable (Concrete annotation) ->
                    Expression.Parameter.create
                      ~location
                      ~annotation:(Type.expression annotation)
                      ~name:"*args"
                      ()
                | Type.Callable.Parameter.Variable (Concatenation concatenation) ->
                    Expression.Parameter.create
                      ~location
                      ~annotation:(Type.OrderedTypes.Concatenation.expression concatenation)
                      ~name:"*args"
                      ()
                | Type.Callable.Parameter.Keywords annotation ->
                    Expression.Parameter.create
                      ~location
                      ~annotation:(Type.expression annotation)
                      ~name:"**kwargs"
                      ()
              in
              placed_single_star, new_parameter :: sofar
            in
            List.fold parameters ~f:convert ~init:(false, []) |> snd |> List.rev
        | ParameterVariadicTypeVariable _
        | Undefined ->
            original_parameters
      in
      let return_annotation = Some (Type.expression annotation) in
      {
        define with
        Define.signature = { signature with Define.Signature.parameters; return_annotation };
      }
      |> Node.create ~location


let is_constructor definition ~resolution =
  match parent_definition definition ~resolution with
  | Some parent_class ->
      let in_test =
        let superclasses = GlobalResolution.superclasses ~resolution parent_class in
        List.exists ~f:Class.is_unit_test (parent_class :: superclasses)
      in
      Define.is_constructor ~in_test (Node.value definition)
  | None -> false
