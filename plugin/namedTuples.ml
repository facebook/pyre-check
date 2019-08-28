(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Expression
open Statement
open Analysis

let transform_ast ({ Source.statements; _ } as source) =
  let rec expand_named_tuples ({ Node.location; value } as statement) =
    let extract_attributes expression =
      match expression with
      | {
          Node.location;
          value =
            Call
              {
                callee =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "typing"); _ };
                            attribute = "NamedTuple";
                            _;
                          });
                    _;
                  };
                arguments;
              };
        }
      | {
          Node.location;
          value =
            Call
              {
                callee =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "collections"); _ };
                            attribute = "namedtuple";
                            _;
                          });
                    _;
                  };
                arguments;
              };
        } ->
          let any_annotation =
            Name (Expression.create_name ~location "typing.Any") |> Node.create ~location
          in
          let attributes =
            match arguments with
            | [ _;
                {
                  Call.Argument.value =
                    { value = String { StringLiteral.value = serialized; _ }; _ };
                  _;
                } ] ->
                String.split serialized ~on:' '
                |> List.map ~f:(fun name -> name, any_annotation, None)
            | [_; { Call.Argument.value = { Node.value = List arguments; _ }; _ }]
            | [_; { Call.Argument.value = { Node.value = Tuple arguments; _ }; _ }] ->
                let get_name ({ Node.value; _ } as expression) =
                  match value with
                  | String { StringLiteral.value = name; _ } -> name, any_annotation, None
                  | Tuple [{ Node.value = String { StringLiteral.value = name; _ }; _ }; annotation]
                    ->
                      name, annotation, None
                  | _ -> Expression.show expression, any_annotation, None
                in
                List.map arguments ~f:get_name
            | _ -> []
          in
          Some attributes
      | _ -> None
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
      Assign
        {
          Assign.target =
            Reference.create ~prefix:parent "_fields" |> Expression.from_reference ~location;
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
            Reference.create ~prefix:parent name |> Expression.from_reference ~location
          in
          Assign
            {
              Assign.target;
              annotation = Some annotation;
              value = Option.value value ~default:(Node.create Ellipsis ~location);
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
        let self_parameter = Parameter.create ~location ~name:"$parameter$cls" () in
        let to_parameter (name, annotation, value) =
          let value =
            match value with
            | Some { Node.value = Ellipsis; _ } -> None
            | _ -> value
          in
          Parameter.create ?value ~location ~annotation ~name:("$parameter$" ^ name) ()
        in
        self_parameter :: List.map attributes ~f:to_parameter
      in
      Statement.Define
        {
          signature =
            {
              name = Reference.create ~prefix:parent "__new__";
              parameters;
              decorators = [];
              docstring = None;
              return_annotation =
                Some
                  (Node.create
                     ~location
                     (Name
                        (Name.Attribute
                           {
                             base = { Node.value = Name (Name.Identifier "typing"); location };
                             attribute = "NamedTuple";
                             special = false;
                           })));
              async = false;
              parent = Some parent;
            };
          body =
            [ Node.create
                ~location
                (Statement.Expression (Node.create ~location Expression.Ellipsis)) ];
        }
      |> Node.create ~location
    in
    let tuple_base ~location =
      {
        Expression.Call.Argument.name = None;
        value =
          { Node.location; value = Name (Expression.create_name ~location "typing.NamedTuple") };
      }
    in
    let value =
      match value with
      | Assign { Assign.target = { Node.value = Name name; _ }; value = expression; _ } -> (
          let name = Expression.name_to_reference name >>| Reference.delocalize in
          match extract_attributes expression, name with
          | Some attributes, Some name
          (* TODO (T42893621): properly handle the excluded case *)
            when not (Reference.is_prefix ~prefix:(Reference.create "$parameter$cls") name) ->
              let constructor = tuple_constructor ~parent:name ~location attributes in
              let attributes = tuple_attributes ~parent:name ~location attributes in
              Class
                {
                  Class.name;
                  bases = [tuple_base ~location];
                  body = constructor :: attributes;
                  decorators = [];
                  docstring = None;
                }
          | _ -> value )
      | Class ({ Class.name; bases; body; _ } as original) ->
          let is_named_tuple_primitive = function
            | {
                Expression.Call.Argument.value =
                  {
                    Node.value =
                      Name
                        (Name.Attribute
                          {
                            base = { Node.value = Name (Name.Identifier "typing"); _ };
                            attribute = "NamedTuple";
                            _;
                          });
                    _;
                  };
                _;
              } ->
                true
            | _ -> false
          in
          if List.exists ~f:is_named_tuple_primitive bases then
            let extract_assign = function
              | {
                  Node.value =
                    Assign
                      { Assign.target = { Node.value = Name target; _ }; value; annotation; _ };
                  _;
                } ->
                  let last =
                    match target with
                    | Name.Identifier identifier -> identifier
                    | Name.Attribute { attribute; _ } -> attribute
                  in
                  let annotation =
                    let any =
                      Name (Expression.create_name ~location "typing.Any") |> Node.create ~location
                    in
                    Option.value annotation ~default:any
                  in
                  Some (last, annotation, Some value)
              | _ -> None
            in
            let attributes = List.filter_map body ~f:extract_assign in
            let constructor = tuple_constructor ~parent:name ~location attributes in
            let fields_attribute = fields_attribute ~parent:name ~location attributes in
            Class { original with Class.body = constructor :: fields_attribute :: body }
          else
            let extract_named_tuples
                (bases, attributes_sofar)
                ({ Expression.Call.Argument.value; _ } as base)
              =
              match extract_attributes value with
              | Some attributes ->
                  let constructor =
                    let is_dunder_new = function
                      | { Node.value = Define { Define.signature = { Define.name; _ }; _ }; _ } ->
                          Reference.last name = "__new__"
                      | _ -> false
                    in
                    if List.exists body ~f:is_dunder_new then
                      []
                    else
                      [tuple_constructor ~parent:name ~location attributes]
                  in
                  let attributes = tuple_attributes ~parent:name ~location attributes in
                  tuple_base ~location :: bases, attributes_sofar @ constructor @ attributes
              | None -> base :: bases, attributes_sofar
            in
            let reversed_bases, attributes =
              List.fold bases ~init:([], []) ~f:extract_named_tuples
            in
            Class
              {
                original with
                Class.bases = List.rev reversed_bases;
                body = attributes @ List.map ~f:expand_named_tuples body;
              }
      | Define ({ Define.body; _ } as define) ->
          Define { define with Define.body = List.map ~f:expand_named_tuples body }
      | _ -> value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }
