(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Statement

let transform_ast ({ Source.statements; qualifier; _ } as source) =
  let expand_new_type ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign
          {
            Assign.value =
              {
                Node.value =
                  Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base = { Node.value = Name (Name.Identifier "typing"); _ };
                                  attribute = "NewType";
                                  _;
                                });
                          _;
                        };
                      arguments =
                        [ {
                            Call.Argument.value =
                              { Node.value = String { StringLiteral.value = name; _ }; _ };
                            _;
                          };
                          ( {
                              (* TODO (T44209017): Error on invalid annotation expression *)
                              Call.Argument.value = base;
                              _;
                            } as base_argument ) ];
                    };
                _;
              };
            _;
          } ->
          let name = Reference.create ~prefix:qualifier name in
          let constructor =
            Define
              {
                signature =
                  {
                    name = Reference.create ~prefix:name "__init__";
                    parameters =
                      [ Parameter.create ~location ~name:"self" ();
                        Parameter.create ~location ~annotation:base ~name:"input" () ];
                    decorators = [];
                    docstring = None;
                    return_annotation =
                      Some (Node.create ~location (Name (Name.Identifier "None")));
                    async = false;
                    parent = Some name;
                  };
                body = [Node.create Pass ~location];
              }
            |> Node.create ~location
          in
          Class
            {
              Class.name;
              bases = [base_argument];
              body = [constructor];
              decorators = [];
              docstring = None;
            }
      | _ -> value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map statements ~f:expand_new_type }
