(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


let transform_ast ({ Source.statements; _ } as source) =
  let tuple_attributes ~parent ~expression =
    match expression with
    | {
      Node.location;
      value =
        Access [
          Access.Identifier module_name;
          Access.Identifier named_tuple;
          Access.Call { Node.value = arguments; _ };
        ];
    } when (Identifier.show module_name = "typing" &&
            Identifier.show named_tuple = "NamedTuple") ||
           (Identifier.show module_name = "collections" &&
            Identifier.show named_tuple = "namedtuple") ->
        let attributes =
          match arguments with
          | [_; { Argument.value = { Node.location; value = String serialized }; _ }] ->
              let attribute name =
                Access (parent @ Access.create name)
                |> Node.create ~location
              in
              String.split serialized ~on:' '
              |> List.map ~f:attribute
          | [_; { Argument.value = { Node.value = List arguments; _ }; _ }] ->
              let rec accessify ({ Node.value; _ } as expression) =
                let value =
                  match value with
                  | String name -> Access (parent @ Access.create name)
                  | Tuple [name; annotation] -> Tuple [accessify name; annotation]
                  | _ -> value
                in
                { expression with Node.value }
              in
              List.map arguments ~f:accessify
          | _ ->
              []
        in
        let attribute ({ Node.location; value } as expression) =
          let target, annotation =
            match value with
            | Tuple [target; annotation] ->
                target, annotation
            | _ ->
                expression, { Node.location; value = Access (Access.create "typing.Any")}
          in
          Assign {
            Assign.target;
            annotation = Some annotation;
            value = None;
            parent = Some parent;
          }
          |> Node.create ~location
        in
        let attributes = List.map attributes ~f:attribute in
        if List.is_empty attributes then
          Some [Node.create ~location Pass]
        else
          Some attributes
    | _ ->
        None
  in
  let tuple_base ~location =
    {
      Argument.name = None;
      value = Node.create ~location (Access (Access.create "typing.NamedTuple"));
    }
  in
  let expand_named_tuples ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign {
          Assign.target = {
            Node.value = Access name;
            _;
          };
          value = Some expression;
          _;
        } ->
          let name = Access.delocalize name in
          tuple_attributes ~parent:name ~expression
          >>| (fun body ->
              Class {
                Class.name;
                bases = [tuple_base ~location];
                body;
                decorators = [];
                docstring = None;
              })
          |> Option.value ~default:value
      | Class ({ Class.name; bases; body; _; } as original) ->
          let extract_named_tuples (bases, attributes_sofar) ({ Argument.value; _ } as base) =
            tuple_attributes ~parent:name ~expression:value
            >>| (fun attributes -> (tuple_base ~location) :: bases, attributes_sofar @ attributes)
            |> Option.value ~default:(base :: bases, attributes_sofar)
          in
          let reversed_bases, attributes = List.fold bases ~init:([], []) ~f:extract_named_tuples in
          Class {
            original with
            Class.bases = List.rev reversed_bases;
            body = attributes @ body;
          }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }
