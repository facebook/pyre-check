(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Expression
open Statement

type annotation_lookup = Type.t Location.Table.t

type definition_lookup = Location.t Location.Table.t

type t = {
  annotations_lookup: annotation_lookup;
  definitions_lookup: definition_lookup;
}

(** The result state of this visitor is ignored. We need two read-only pieces of information to
    build the location table: the types resolved for this statement, and a reference to the
    (mutable) location table to update. *)
module NodeVisitor = struct
  type t = {
    pre_resolution: Resolution.t;
    post_resolution: Resolution.t;
    annotations_lookup: annotation_lookup;
    definitions_lookup: definition_lookup;
  }

  let node_base
      ~postcondition
      ({ pre_resolution; post_resolution; annotations_lookup; definitions_lookup; _ } as state)
      node
    =
    let annotate_expression ({ Node.location; value } as expression) =
      let resolution = if postcondition then post_resolution else pre_resolution in
      let global_resolution = Resolution.global_resolution resolution in
      let resolve ~resolution ~expression =
        try
          let annotation = Resolution.resolve_expression_to_annotation resolution expression in
          let original = Annotation.original annotation in
          if Type.is_top original || Type.is_unbound original then
            let annotation = Annotation.annotation annotation in
            if Type.is_top annotation || Type.is_unbound annotation then
              None
            else
              Some annotation
          else
            Some original
        with
        | ClassHierarchy.Untracked _ -> None
      in
      let resolve_definition ~expression =
        let find_definition reference =
          GlobalResolution.global_location global_resolution reference
          >>| Location.strip_module
          >>= fun location -> if Location.equal location Location.any then None else Some location
        in
        match Node.value expression with
        | Expression.Name (Name.Identifier identifier) ->
            find_definition (Reference.create identifier)
        | Name (Name.Attribute { base; attribute; _ } as name) -> (
            let definition = name_to_reference name >>= find_definition in
            match definition with
            | Some definition -> Some definition
            | None ->
                (* Resolve prefix to check if this is a method. *)
                resolve ~resolution ~expression:base
                >>| Type.class_name
                >>| (fun prefix -> Reference.create ~prefix attribute)
                >>= find_definition)
        | _ -> None
      in
      let store_lookup ~table ~location ~data =
        if not (Location.equal location Location.any) then
          Hashtbl.set table ~key:location ~data |> ignore
      in
      let store_annotation location annotation =
        store_lookup ~table:annotations_lookup ~location ~data:annotation
      in
      let store_definition location data = store_lookup ~table:definitions_lookup ~location ~data in
      resolve ~resolution ~expression >>| store_annotation location |> ignore;
      resolve_definition ~expression >>| store_definition location |> ignore;
      let store_generator_and_compute_resolution
          resolution
          { Comprehension.Generator.target; iterator; conditions; _ }
        =
        (* The basic idea here is to simulate element for x in generator if cond as the following: x
           = generator.__iter__().__next__() assert cond element *)
        let annotate_expression resolution ({ Node.location; _ } as expression) =
          resolve ~resolution ~expression >>| store_annotation location |> ignore
        in
        annotate_expression resolution iterator;
        let resolution =
          let target_assignment =
            let iterator_element_call =
              let to_call function_name base =
                Expression.Call
                  {
                    callee =
                      Node.create_with_default_location
                        (Expression.Name
                           (Name.Attribute { base; attribute = function_name; special = false }));
                    arguments = [];
                  }
                |> Node.create_with_default_location
              in

              iterator |> to_call "__iter__" |> to_call "__next__"
            in
            { Assign.target; value = iterator_element_call; annotation = None; parent = None }
          in
          Resolution.resolve_assignment resolution target_assignment
        in
        let store_condition_and_refine resolution condition =
          annotate_expression resolution condition;
          Resolution.resolve_assertion resolution ~asserted_expression:condition
          |> Option.value ~default:resolution
        in
        let resolution = List.fold conditions ~f:store_condition_and_refine ~init:resolution in
        annotate_expression resolution target;
        resolution
      in
      match value with
      | Call { arguments; _ } ->
          let annotate_argument_name { Call.Argument.name; value = { Node.location; _ } as value } =
            match name, resolve ~resolution ~expression:value with
            | Some { Node.location = { Location.start; _ }; _ }, Some annotation ->
                let location = { location with Location.start } in
                store_annotation location annotation
            | _ -> ()
          in
          List.iter ~f:annotate_argument_name arguments
      | DictionaryComprehension { element = { key; value }; generators; _ } ->
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          let annotate_expression ({ Node.location; _ } as expression) =
            store_annotation location (Resolution.resolve_expression_to_type resolution expression)
          in
          annotate_expression key;
          annotate_expression value
      | ListComprehension { element; generators; _ }
      | SetComprehension { element; generators; _ } ->
          let annotate resolution ({ Node.location; _ } as expression) =
            resolve ~resolution ~expression >>| store_annotation location |> ignore
          in
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          annotate resolution element
      | _ -> ()
    in
    match node with
    | Visit.Expression expression ->
        annotate_expression expression;
        state
    | Visit.Reference { Node.value = reference; location } ->
        annotate_expression (Ast.Expression.from_reference ~location reference);
        state
    | _ -> state


  let node = node_base ~postcondition:false

  let node_postcondition = node_base ~postcondition:true

  let visit_statement_children _ _ = true
end

module Visit = struct
  include Visit.MakeNodeVisitor (NodeVisitor)

  let visit state source =
    let state = ref state in
    let visit_statement_override ~state statement =
      (* Special-casing for statements that require lookup using the postcondition. *)
      let precondition_visit = visit_expression ~state ~visitor_override:NodeVisitor.node in
      let postcondition_visit =
        visit_expression ~state ~visitor_override:NodeVisitor.node_postcondition
      in
      (* Special-casing for annotations that should be parsed rather than resolved as expressions. *)
      let store_annotation annotation =
        let { NodeVisitor.pre_resolution; annotations_lookup; _ } = !state in
        let resolved =
          GlobalResolution.parse_annotation (Resolution.global_resolution pre_resolution) annotation
          |> Type.meta
        in
        let location = Node.location annotation in
        if not (Location.equal location Location.any) then
          Hashtbl.add annotations_lookup ~key:location ~data:resolved |> ignore
      in
      match Node.value statement with
      | Statement.Assign { Assign.target; annotation; value; _ } ->
          postcondition_visit target;
          annotation >>| store_annotation |> ignore;
          precondition_visit value
      | Define
          {
            Define.signature =
              {
                name = { Node.value = name; location = name_location };
                parameters;
                decorators;
                return_annotation;
                _;
              };
            _;
          } ->
          let visit_parameter { Node.value = { Parameter.annotation; value; name }; location } =
            Expression.Name (Name.Identifier name) |> Node.create ~location |> postcondition_visit;
            Option.iter ~f:postcondition_visit value;
            annotation >>| store_annotation |> ignore
          in
          precondition_visit (Ast.Expression.from_reference ~location:name_location name);
          List.iter parameters ~f:visit_parameter;
          List.map decorators ~f:Ast.Statement.Decorator.to_expression
          |> List.iter ~f:postcondition_visit;
          Option.iter ~f:postcondition_visit return_annotation
      | Import { Import.from; imports } ->
          let visit_import { Import.name = { Node.value = name; location = name_location }; alias } =
            let qualifier =
              match from with
              | Some { Node.value = from; _ } -> from
              | None -> Reference.empty
            in
            let create_qualified_expression ~location =
              Reference.combine qualifier name |> Ast.Expression.from_reference ~location
            in
            precondition_visit (create_qualified_expression ~location:name_location);
            Option.iter
              ~f:(fun { Node.location; _ } ->
                precondition_visit (create_qualified_expression ~location))
              alias
          in
          List.iter imports ~f:visit_import
      | _ -> visit_statement ~state statement
    in
    List.iter ~f:(visit_statement_override ~state) source.Source.statements;
    !state
end

let create_of_module type_environment qualifier =
  let annotations_lookup = Location.Table.create () in
  let definitions_lookup = Location.Table.create () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  let walk_define
      ({
         Node.value = { Define.signature = { name = { Node.value = name; _ }; _ }; _ } as define;
         _;
       } as define_node)
    =
    let annotation_lookup =
      TypeCheck.get_or_recompute_local_annotations ~environment:type_environment name
      |> function
      | Some annotation_lookup -> annotation_lookup
      | None -> LocalAnnotationMap.empty () |> LocalAnnotationMap.read_only
    in
    let cfg = Cfg.create define in
    let walk_statement node_id statement_index statement =
      let pre_annotations, post_annotations =
        let key = [%hash: int * int] (node_id, statement_index) in
        ( LocalAnnotationMap.ReadOnly.get_precondition annotation_lookup key
          |> Option.value ~default:Resolution.empty_annotation_store,
          LocalAnnotationMap.ReadOnly.get_postcondition annotation_lookup key
          |> Option.value ~default:Resolution.empty_annotation_store )
      in
      let pre_resolution =
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        TypeCheck.resolution
          global_resolution
          ~annotation_store:pre_annotations
          (module TypeCheck.DummyContext)
      in
      let post_resolution =
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        TypeCheck.resolution
          global_resolution
          ~annotation_store:post_annotations
          (module TypeCheck.DummyContext)
      in
      Visit.visit
        { NodeVisitor.pre_resolution; post_resolution; annotations_lookup; definitions_lookup }
        (Source.create [statement])
      |> ignore
    in
    let walk_cfg_node ~key:node_id ~data:cfg_node =
      let statements = Cfg.Node.statements cfg_node in
      List.iteri statements ~f:(walk_statement node_id)
    in
    Hashtbl.iteri cfg ~f:walk_cfg_node;

    (* Special-case define signature processing, since this is not included in the define's cfg. *)
    let define_signature =
      { define_node with value = Statement.Define { define with Define.body = [] } }
    in
    walk_statement Cfg.entry_index 0 define_signature
  in
  let all_defines =
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module
      unannotated_global_environment
      qualifier
    |> List.filter_map
         ~f:(UnannotatedGlobalEnvironment.ReadOnly.get_define_body unannotated_global_environment)
  in
  List.iter all_defines ~f:walk_define;
  { annotations_lookup; definitions_lookup }


let get_best_location lookup_table ~position =
  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line }
    =
    let start_ok = start_line < line || (start_line = line && start_column <= column) in
    let stop_ok = stop_line > line || (stop_line = line && stop_column > column) in
    start_ok && stop_ok
  in
  let weight
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
    =
    ((stop_line - start_line) * 1000) + stop_column - start_column
  in
  Hashtbl.filter_keys lookup_table ~f:(fun key -> location_contains_position key position)
  |> Hashtbl.to_alist
  |> List.min_elt ~compare:(fun (location_left, _) (location_right, _) ->
         weight location_left - weight location_right)


let get_annotation { annotations_lookup; _ } ~position =
  get_best_location annotations_lookup ~position


let get_definition { definitions_lookup; _ } ~position =
  get_best_location definitions_lookup ~position >>| snd


let get_all_annotations { annotations_lookup; _ } = Hashtbl.to_alist annotations_lookup

let get_all_definitions { definitions_lookup; _ } = Hashtbl.to_alist definitions_lookup
