(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement
open Analysis


let transform_ast ({ Source.statements; _ } as source) =
  let rec expand_named_tuples ({ Node.location; value } as statement) =
    let extract_attributes expression =
      match expression with
      | {
        Node.location;
        value =
          Access
            (SimpleAccess [
                Access.Identifier module_name;
                Access.Identifier named_tuple;
                Access.Call { Node.value = arguments; _ };
              ]);
      } when (module_name = "typing" &&
              named_tuple = "NamedTuple") ||
             (module_name = "collections" &&
              named_tuple = "namedtuple") ->
          let any_annotation =
            Node.create
              (Access (SimpleAccess (Access.create "typing.Any")))
              ~location
          in
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
    let fields_attribute ~parent ~location attributes =
      let node = Node.create ~location in
      let value =
        attributes
        |> List.map ~f:(fun (name, _, _) -> String (StringLiteral.create name) |> node)
        |> (fun parameters -> Tuple parameters)
        |> node
      in
      let annotation =
        List.init (List.length attributes) ~f:(fun _ -> Type.string)
        |> Type.tuple
        |> Type.expression
      in
      Assign {
        Assign.target = Access (SimpleAccess (parent @ (Access.create "_fields"))) |> node;
        annotation = Some annotation;
        value;
        parent = Some parent;
      }
      |> Node.create ~location
    in
    let tuple_attributes ~parent ~location attributes =
      let attribute_statements =
        let attribute (name, annotation, value) =
          let target =
            Access (SimpleAccess (parent @ (Access.create name)))
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
      let fields_attribute = fields_attribute ~parent ~location attributes in
      fields_attribute :: attribute_statements
    in
    let tuple_constructor ~parent ~location attributes =
      let parameters =
        let self_parameter = Parameter.create ~name:"self" () in
        let to_parameter (name, annotation, value) =
          let value =
            match value with
            | Some { Node.value = Ellipses; _ } -> None
            | _ -> value
          in
          Parameter.create ?value ~annotation ~name:("$parameter$" ^ name) ()
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
        value = Node.create ~location (Access (SimpleAccess (Access.create "typing.NamedTuple")));
      }
    in
    let value =
      match value with
      | Assign {
          Assign.target = {
            Node.value = Access (SimpleAccess name);
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
            | { Statement.Argument.value = { Node.value = Access (SimpleAccess name); _ }; _ } ->
                Access.show name = "typing.NamedTuple"
            | _ ->
                false
          in
          if List.exists ~f:is_named_tuple_primitive bases then
            let extract_assign = function
              | {
                Node.value = Assign {
                    Assign.target = { Node.value = Access (SimpleAccess target); _ };
                    value;
                    annotation;
                    _;
                  };
                _;
              } ->
                  let annotation =
                    Option.value
                      annotation
                      ~default:(Node.create
                                  ~location
                                  (Access (SimpleAccess (Access.create "typing.Any"))))
                  in
                  List.last target
                  >>| (fun target -> Access.show [target], annotation, Some value)
              | _ ->
                  None
            in
            let attributes = List.filter_map body ~f:extract_assign in
            let constructor = tuple_constructor ~parent:name ~location attributes in
            let fields_attribute = fields_attribute ~parent:name ~location attributes in
            Class { original with Class.body = constructor :: fields_attribute :: body }
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
      | Define ({ Define.body; _ } as define) ->
          Define { define with Define.body = (List.map ~f:expand_named_tuples body) }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }
