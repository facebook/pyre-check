(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement
open Analysis
open Environment


type dataclass_options = {
  init: bool;
  repr: bool;
  eq: bool;
  order: bool;
  unsafe_hash: bool;
  frozen: bool;
}


let transform_environment ((module Handler: Handler) as order) { Source.statements; _ } =
  let get_dataclass_decorator { Node.location; value = ast_class } =
    let open Annotated in
    let annotated = Class.create { Node.location; value = ast_class } in
    Class.get_decorator annotated ~decorator:"dataclasses.dataclass"
    @ Class.get_decorator annotated ~decorator:"dataclass"
  in
  let update_dataclasses = function
    | { Node.value = Class ({ Class.name = parent;  _ } as ast_class); location }
      when not (List.is_empty (get_dataclass_decorator { Node.location; value = ast_class })) ->
        let annotated_class = Annotated.Class.create { Node.location; value = ast_class } in
        let { init; repr; eq; _ } =
          let default =
            {
              init = true;
              repr = true;
              eq = true;
              order = false;
              unsafe_hash = false;
              frozen = false;
            }
          in
          match get_dataclass_decorator { Node.location; value = ast_class } with
          | { Annotated.Class.arguments = Some arguments; _ } :: _ ->
              let rec read_arguments options arguments =
                match arguments with
                | [] ->
                    options
                | { Argument.name = Some { Node.value = name; _ }; value = { Node.value; _ } }
                  :: arguments ->
                    let updated_options =
                      match Identifier.show_sanitized name, value with
                      | "init", False -> { options with init = false }
                      | "repr", False -> { options with repr = false }
                      | "eq", False -> { options with eq = false }
                      | "order", True -> { options with order = true }
                      | "unsafe_hash", True -> { options with unsafe_hash = true }
                      | "frozen", True -> { options with frozen = true }
                      | _ -> options
                    in
                    read_arguments updated_options arguments
                | _ :: tail ->
                    read_arguments options tail
              in
              read_arguments default arguments
          | _ ->
              default
        in
        let resolution = Environment.resolution order () in
        let class_type = Annotated.Class.annotation ~resolution annotated_class in
        let generated_methods =
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
          let methods =
            if init && not (
                Annotated.Class.has_method
                  annotated_class
                  ~resolution
                  ~name:(Access.create "__init__")) then
              let parameters =
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
                            let rec override_existing_parameters unchecked_parameters =
                              match unchecked_parameters with
                              | [] ->
                                  [Parameter.create ~name ~annotation ?value ()]
                              | {
                                Node.value = { Parameter.name = old_name; value = old_value; _ };
                                _;
                              } :: tail
                                when Identifier.equal old_name name ->
                                  let value = if Option.is_some value then value else old_value in
                                  Parameter.create ~name ~annotation ?value () :: tail
                              | head :: tail ->
                                  head :: override_existing_parameters tail
                            in
                            override_existing_parameters parameters
                        | _ ->
                            parameters
                      end
                  | _ ->
                      parameters
                in
                let parent_dataclasses =
                  Annotated.Class.superclasses ~resolution annotated_class
                  |> (fun superclasses -> annotated_class :: superclasses)
                  |> List.rev
                in
                parent_dataclasses
                |> List.map ~f:Annotated.Class.body
                |> List.fold
                  ~init:[]
                  ~f:(fun parameters -> List.fold ~init:parameters ~f:collect_parameters)
              in
              [create_method ~name:"__init__" ~parameters ~return_annotation:"None"]
            else
              []
          in
          let methods =
            if repr && not (
                Annotated.Class.has_method
                  annotated_class
                  ~resolution
                  ~name:(Access.create "__repr__")) then
              create_method ~name:"__repr__" ~parameters:[] ~return_annotation:"str" :: methods
            else
              methods
          in
          let methods =
            if eq && not (
                Annotated.Class.has_method
                  annotated_class
                  ~resolution
                  ~name:(Access.create "__eq__")) then
              create_method
                ~name:"__eq__"
                ~parameters:[Parameter.create ~name:(Identifier.create "o") ()]
                ~return_annotation:"bool"
              :: methods
            else
              methods
          in
          methods
          |> List.map ~f:(fun define -> Node.create ~location (Define define))
          |> List.rev
          |> Source.create
          |> Analysis.Preprocessing.preprocess
          |> fun { Source.statements; _ } -> statements
        in
        Handler.set_class_definition
          ~primitive:class_type
          ~definition:{ Node.location; value = { ast_class with Class.body = generated_methods } }
    | _ ->
        ()
  in
  List.iter ~f:update_dataclasses statements
