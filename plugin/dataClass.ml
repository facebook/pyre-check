(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement
open Analysis
open Environment


let transform_environment (module Handler: Handler) { Source.statements; _ } =
  let update_dataclasses = function
    | { Node.value = Class ({ Class.name = parent; body; _ } as ast_class); location }
      when Class.has_decorator ast_class "dataclass" ||
           Class.has_decorator ast_class "dataclasses.dataclass" ->
        (* TODO(T30619164): Collect the attributes declared in the base data classes *)
        (* TODO(T30619164): Parse decorator arguments and add more generated methods *)
        let body =
          let generated_methods =
            let annotated_class = Annotated.Class.create { Node.location; value = ast_class } in
            let create_method ~name ~parameters ~return_annotation =
              {
                Define.name = parent @ (Access.create name);
                parameters =
                  Parameter.create ~name:(Identifier.create "self") ()
                  :: parameters;
                body = [Node.create ~location Pass];
                decorators = [];
                docstring = None;
                return_annotation =
                  Some (Node.create ~location (Access (Access.create return_annotation)));
                async = false;
                generated = false;
                parent = Some parent;
              }
            in
            let add_init methods =
              if not (Annotated.Class.has_method annotated_class ~name:"__init__") then
                let collect_parameters parameters { Node.value; _ } =
                  match value with
                  | Assign {
                      Assign.target = { Node.value = Access access; _ };
                      annotation = Some annotation;
                      value;
                      _;
                    } ->
                      begin
                        match List.last access with
                        | Some (Access.Identifier name) ->
                            let value =
                              match value with
                              | { Node.value = Ellipses; _ } -> None
                              | _ -> Some value
                            in
                            Parameter.create ~name ~annotation ?value () :: parameters
                        | _ ->
                            parameters
                      end
                  | _ ->
                      parameters
                in
                create_method
                  ~name:"__init__"
                  ~parameters:(List.rev (List.fold ~init:[] ~f:collect_parameters body))
                  ~return_annotation:"None"
                :: methods
              else
                methods
            in
            let add_repr methods =
              if not (Annotated.Class.has_method annotated_class ~name:"__repr__") then
                create_method ~name:"__repr__" ~parameters:[] ~return_annotation:"str" :: methods
              else
                methods
            in
            let add_eq methods =
              if not (Annotated.Class.has_method annotated_class ~name:"__eq__") then
                create_method
                  ~name:"__eq__"
                  ~parameters:[Parameter.create ~name:(Identifier.create "o") ()]
                  ~return_annotation:"bool"
                :: methods
              else
                methods
            in
            let preprocess methods =
              methods
              |> List.map ~f:(fun define -> Node.create ~location (Define define))
              |> List.rev
              |> Source.create
              |> Analysis.Preprocessing.preprocess
              |> fun { Source.statements; _ } -> statements
            in
            [] |> add_init |> add_repr |> add_eq |> preprocess
          in
          match body with
          | [{ Node.value = Expression { Node.value = Ellipses; _ }; _ }] -> generated_methods
          | _ -> body @ generated_methods
        in
        Handler.update_class_definition
          ~primitive:
            (Type.create
               ~aliases:Handler.aliases
               { Node.location; value = Access parent }
             |> Type.split
             |> fst)
          ~definition:{ ast_class with Class.body }
    | _ ->
        ()
  in
  List.iter ~f:update_dataclasses statements
