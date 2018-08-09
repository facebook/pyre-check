(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement
open Analysis
open Environment


let transform_environment
    (module Handler: Handler)
    { Source.statements; _ } =
  let transform { Node.location; value } =
    match value with
    | Class ({ Class.name = parent; body; _ } as origin)
      when Class.has_decorator origin "dataclass" ||
           Class.has_decorator origin "dataclasses.dataclass" ->
        (* TODO(T30619164): Collect the attributes declared in the base data classes;
           parse the decorator and field arguments *)
        let has_method body ~name =
          let is_matching_method statement =
            match statement with
            | { Node.value = Define { Define.name = access; _ }; _ } ->
                begin
                  match List.last access with
                  | Some (Access.Identifier method_name) ->
                      Identifier.equal method_name (Identifier.create name)
                  | _ ->
                      false
                end
            | _ ->
                false
          in
          List.exists ~f:is_matching_method body
        in
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
        (* TODO(T30619164): Parse decorator arguments and add more generated methods *)
        let methods =
          if not (has_method body ~name:"__init__") then
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
                        Parameter.create ~name ~annotation ?value () :: parameters
                    | _ ->
                        parameters
                  end
              | _ ->
                  parameters
            in
            [create_method
               ~name:"__init__"
               ~parameters:(List.rev (List.fold ~init:[] ~f:collect_parameters body))
               ~return_annotation:"None"]
          else
            []
        in
        let methods =
          if not (has_method body ~name:"__repr__") then
            create_method ~name:"__repr__" ~parameters:[] ~return_annotation:"str" :: methods
          else
            methods
        in
        let methods =
          if not (has_method body ~name:"__eq__") then
            create_method
              ~name:"__eq__"
              ~parameters:[Parameter.create ~name:(Identifier.create "o") ()]
              ~return_annotation:"bool"
            :: methods
          else
            methods
        in
        let body =
          let preprocessed_methods =
            methods
            |> List.map ~f:(fun define -> Node.create ~location (Define define))
            |> List.rev
            |> Source.create
            |> Analysis.Preprocessing.preprocess
            |> fun { Source.statements; _ } -> statements
          in
          match body with
          | [{ Node.value = Expression { Node.value = Ellipses; _ }; _ }] -> preprocessed_methods
          | _ -> body @ preprocessed_methods
        in
        Handler.update_class_definition
          ~primitive:
            (Type.create
               ~aliases:Handler.aliases
               (Node.create_with_default_location (Access parent))
             |> Type.split
             |> fst)
          ~definition:{ origin with Class.body }
    | _ ->
        ()
  in
  List.iter ~f:transform statements
