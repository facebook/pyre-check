(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement


let transform_ast ({ Source.statements; qualifier; _ } as source) =
  let expand_new_type ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign {
          Assign.value =
            {
              Node.value =
                Access
                  (SimpleAccess [
                      Access.Identifier typing;
                      Access.Identifier new_type;
                      Access.Call {
                        Node.value = [
                          {
                            Argument.value =
                              {
                                Node.value = String { StringLiteral.value = name; _ };
                                _
                              };
                            _;
                          };
                          {
                            Argument.value = ({ Node.value = Access _; _ } as base);
                            _;
                          } as base_argument;
                        ];
                        _;
                      };
                    ]);
              _;
            };
          _;
        } when typing = "typing" && new_type = "NewType" ->
          let name = Reference.create ~prefix:qualifier name in
          let constructor =
            Define {
              signature = {
                name = Reference.create ~prefix:name "__init__";
                parameters = [
                  Parameter.create ~location ~name:"self" ();
                  Parameter.create ~location ~annotation:base ~name:"input" ();
                ];
                decorators = [];
                docstring = None;
                return_annotation = None;
                async = false;
                parent = Some name;
              };
              body = [Node.create Pass ~location];
            }
            |> Node.create ~location
          in
          Class {
            Class.name;
            bases = [base_argument];
            body = [constructor];
            decorators = [];
            docstring = None;
          }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map statements ~f:expand_new_type }
