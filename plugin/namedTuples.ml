(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


let transform_ast ({ Source.statements; _ } as source) =
  let extract_attributes expression =
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
        let any_annotation = Node.create ~location (Access (Access.create "typing.Any")) in
        let attributes =
          match arguments with
          | [
            _;
            {
              Argument.value = {
                value = String { StringLiteral.value = serialized; _ };
                _
              };
              _;
            };
          ] ->
              String.split serialized ~on:' '
              |> List.map ~f:(fun name -> name, any_annotation, None)
          | [_; { Argument.value = { Node.value = List arguments; _ }; _ }]
          | [_; { Argument.value = { Node.value = Tuple arguments; _ }; _ }] ->
              let accessify ({ Node.value; _ } as expression) =
                match value with
                | String { StringLiteral.value = name; _ } ->
                    name, any_annotation, None
                | Tuple [
                    { Node.value = String { StringLiteral.value = name; _ }; _ };
                    annotation;
                  ] ->
                    name, annotation, None
                | _ ->
                    Expression.show expression, any_annotation, None
              in
              List.map arguments ~f:accessify
          | _ ->
              []
        in
        Some attributes
    | _ ->
        None
  in
  let tuple_attributes ~parent ~location attributes =
    let attribute_statements =
      let attribute (name, annotation, value) =
        let target =
          Access (parent @ (Access.create name))
          |> Node.create ~location
        in
        Assign {
          Assign.target;
          annotation = Some annotation;
          value = Option.value value ~default:(Node.create Ellipses ~location);
          parent = Some parent;
        }
        |> Node.create ~location
      in
      List.map attributes ~f:attribute
    in
    attribute_statements
  in
  let tuple_constructor ~parent ~location attributes =
    let parameters =
      let self_parameter = Parameter.create ~name:(Identifier.create "self") () in
      let to_parameter (name, annotation, value) =
        let value =
          match value with
          | Some { Node.value = Ellipses; _ } -> None
          | _ -> value
        in
        Parameter.create ?value ~annotation ~name:(Identifier.create ("$parameter$" ^ name)) ()
      in
      self_parameter :: List.map attributes ~f:to_parameter
    in
    Statement.Define {
      Define.name = parent @ Access.create "__init__";
      parameters;
      body = [
        Node.create ~location (Statement.Expression (Node.create ~location Expression.Ellipses));
      ];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = Some parent;
    }
    |> Node.create ~location
  in
  let tuple_base ~location =
    {
      Argument.name = None;
      value = Node.create ~location (Access (Access.create "typing.NamedTuple"));
    }
  in
  let rec expand_named_tuples ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign {
          Assign.target = {
            Node.value = Access name;
            _;
          };
          value = expression;
          _;
        } ->
          let name = Access.delocalize name in
          begin
            match extract_attributes expression with
            | Some attributes ->
                let constructor = tuple_constructor ~parent:name ~location attributes in
                let attributes = tuple_attributes ~parent:name ~location attributes in
                Class {
                  Class.name;
                  bases = [tuple_base ~location];
                  body = constructor :: attributes;
                  decorators = [];
                  docstring = None;
                }
            | None ->
                value
          end
      | Class ({ Class.name; bases; body; _ } as original) ->
          let is_named_tuple_primitive = function
            | { Statement.Argument.value = { Node.value = Access name; _ }; _ } ->
                Access.show name = "typing.NamedTuple"
            | _ ->
                false
          in
          if List.exists ~f:is_named_tuple_primitive bases then
            let extract_assign = function
              | {
                Node.value = Assign {
                    Assign.target = { Node.value = Access target; _ };
                    value;
                    annotation;
                    _;
                  };
                _;
              } ->
                  let annotation =
                    Option.value
                      annotation
                      ~default:(Node.create ~location (Access (Access.create "typing.Any")))
                  in
                  List.last target
                  >>| (fun target -> Access.show [target], annotation, Some value)
              | _ ->
                  None
            in
            List.filter_map body ~f:extract_assign
            |> tuple_constructor ~parent:name ~location
            |> fun constructor -> Class { original with Class.body = constructor :: body }
          else
            let extract_named_tuples (bases, attributes_sofar) ({ Argument.value; _ } as base) =
              match extract_attributes value with
              | Some attributes ->
                  let constructor = tuple_constructor ~parent:name ~location attributes in
                  let attributes = tuple_attributes ~parent:name ~location attributes in
                  (tuple_base ~location) :: bases, attributes_sofar @ (constructor :: attributes)
              | None ->
                  base :: bases, attributes_sofar
            in
            let reversed_bases, attributes =
              List.fold bases ~init:([], []) ~f:extract_named_tuples
            in
            Class {
              original with
              Class.bases = List.rev reversed_bases;
              body = attributes @ (List.map ~f:expand_named_tuples body);
            }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }
