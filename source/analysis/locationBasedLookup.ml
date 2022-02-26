(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Expression
open Statement

type resolved_type_lookup = Type.t Location.Table.t

type definition_lookup = Location.WithModule.t Location.Table.t

type t = {
  resolved_types_lookup: resolved_type_lookup;
  definitions_lookup: definition_lookup;
}

(** This visitor stores two kinds of information for an expression: its definition and resolved
    type.

    Definition: For names (such as `foo` or `bar.baz.some_method`), it stores the definition in the
    `definitions_lookup` table on the key of the name's location.

    Resolved type: For all expressions, store the resolved type in `resolved_types_lookup` on the
    key of the expression's location. Special-case names such as named arguments or the names in
    comprehensions and generators.

    The result state of this visitor is ignored. We need two read-only pieces of information to
    build the location table: the types resolved for this statement, and a reference to the
    (mutable) location table to update. *)
module CreateDefinitionAndAnnotationLookupVisitor = struct
  type t = {
    pre_resolution: Resolution.t;
    post_resolution: Resolution.t;
    resolved_types_lookup: resolved_type_lookup;
    definitions_lookup: definition_lookup;
  }

  let node_base
      ~postcondition
      ({ pre_resolution; post_resolution; resolved_types_lookup; definitions_lookup; _ } as state)
      node
    =
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
    let store_definition ({ Node.location; _ } as expression) =
      let resolve_definition_for_name ~resolution ~expression =
        let find_definition reference =
          GlobalResolution.global_location (Resolution.global_resolution resolution) reference
          >>= fun location ->
          Option.some_if
            (not ([%compare.equal: Location.WithModule.t] location Location.WithModule.any))
            location
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
      let store_lookup ~table ~location data =
        if not (Location.equal location Location.any) then
          Hashtbl.set table ~key:location ~data |> ignore
      in
      let resolution = if postcondition then post_resolution else pre_resolution in
      resolve_definition_for_name ~resolution ~expression
      |> Option.iter ~f:(store_lookup ~table:definitions_lookup ~location)
    in
    let store_resolved_type ({ Node.location; value } as expression) =
      let store_lookup ~table ~location data =
        if not (Location.equal location Location.any) then
          Hashtbl.set table ~key:location ~data |> ignore
      in
      let store_resolved_type = store_lookup ~table:resolved_types_lookup in
      let store_generator_and_compute_resolution
          resolution
          { Comprehension.Generator.target; iterator; conditions; _ }
        =
        (* The basic idea here is to simulate element for x in generator if cond as the following: x
           = generator.__iter__().__next__() assert cond element *)
        let annotate_expression resolution ({ Node.location; _ } as expression) =
          resolve ~resolution ~expression >>| store_resolved_type ~location |> ignore
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
            { Assign.target; value = iterator_element_call; annotation = None }
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
      let resolution = if postcondition then post_resolution else pre_resolution in
      resolve ~resolution ~expression >>| store_resolved_type ~location |> ignore;
      match value with
      | Call { arguments; _ } ->
          let annotate_argument_name { Call.Argument.name; value } =
            match name, resolve ~resolution ~expression:value with
            | Some { Node.location; _ }, Some annotation -> store_resolved_type ~location annotation
            | _ -> ()
          in
          List.iter ~f:annotate_argument_name arguments
      | DictionaryComprehension { element = { key; value }; generators; _ } ->
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          let annotate_expression ({ Node.location; _ } as expression) =
            store_resolved_type
              ~location
              (Resolution.resolve_expression_to_type resolution expression)
          in
          annotate_expression key;
          annotate_expression value
      | ListComprehension { element; generators; _ }
      | SetComprehension { element; generators; _ } ->
          let annotate resolution ({ Node.location; _ } as expression) =
            resolve ~resolution ~expression >>| store_resolved_type ~location |> ignore
          in
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          annotate resolution element
      | _ -> ()
    in
    match node with
    | Visit.Expression expression ->
        store_definition expression;
        store_resolved_type expression;
        state
    | Visit.Reference { Node.value = reference; location } ->
        store_definition (Ast.Expression.from_reference ~location reference);
        store_resolved_type (Ast.Expression.from_reference ~location reference);
        state
    | _ -> state


  let node = node_base ~postcondition:false

  let node_postcondition = node_base ~postcondition:true

  let visit_statement_children _ _ = true

  let visit_format_string_children _ _ = false
end

(** This is a simple wrapper around [CreateDefinitionAndAnnotationLookupVisitor]. It ensures that
    the lookup for type annotations, such as `x: Foo`, points to the definition of the type `Foo`,
    not `Type[Foo]`. *)
module CreateLookupsIncludingTypeAnnotationsVisitor = struct
  include Visit.MakeNodeVisitor (CreateDefinitionAndAnnotationLookupVisitor)

  let visit state source =
    let state = ref state in
    let visit_statement_override ~state statement =
      (* Special-casing for statements that require lookup using the postcondition. *)
      let precondition_visit =
        visit_expression ~state ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node
      in
      let postcondition_visit =
        visit_expression
          ~state
          ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node_postcondition
      in
      let store_type_annotation annotation =
        let { CreateDefinitionAndAnnotationLookupVisitor.pre_resolution; resolved_types_lookup; _ } =
          !state
        in
        let resolved =
          GlobalResolution.parse_annotation (Resolution.global_resolution pre_resolution) annotation
          |> Type.meta
        in
        let location = Node.location annotation in
        if not (Location.equal location Location.any) then
          Hashtbl.add resolved_types_lookup ~key:location ~data:resolved |> ignore
      in
      match Node.value statement with
      | Statement.Assign { Assign.target; annotation; value; _ } ->
          postcondition_visit target;
          annotation >>| store_type_annotation |> ignore;
          precondition_visit value
      | Define
          ({ Define.signature = { name; parameters; decorators; return_annotation; _ }; _ } as
          define) ->
          let visit_parameter { Node.value = { Parameter.annotation; value; name }; location } =
            (* Location in the AST includes both the parameter name and the annotation. For our
               purpose, we just need the location of the name. *)
            let location =
              let { Location.start = { Location.line = start_line; column = start_column }; _ } =
                location
              in
              {
                Location.start = { Location.line = start_line; column = start_column };
                stop =
                  {
                    Location.line = start_line;
                    column = start_column + String.length (Identifier.sanitized name);
                  };
              }
            in
            Expression.Name (Name.Identifier name) |> Node.create ~location |> postcondition_visit;
            Option.iter ~f:postcondition_visit value;
            annotation >>| store_type_annotation |> ignore
          in
          precondition_visit
            (Ast.Expression.from_reference
               ~location:(Define.name_location ~body_location:statement.location define)
               name);
          List.iter parameters ~f:visit_parameter;
          List.iter decorators ~f:postcondition_visit;
          Option.iter ~f:postcondition_visit return_annotation
      | Import { Import.from; imports } ->
          let visit_import { Node.value = { Import.name; _ }; location = import_location } =
            let qualifier =
              match from with
              | Some from -> from
              | None -> Reference.empty
            in
            let create_qualified_expression ~location =
              Reference.combine qualifier name |> Ast.Expression.from_reference ~location
            in
            precondition_visit (create_qualified_expression ~location:import_location)
          in
          List.iter imports ~f:visit_import
      | _ -> visit_statement ~state statement
    in
    List.iter ~f:(visit_statement_override ~state) source.Source.statements;
    !state
end

let create_of_module type_environment qualifier =
  let resolved_types_lookup = Location.Table.create () in
  let definitions_lookup = Location.Table.create () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  let walk_define
      ({ Node.value = { Define.signature = { name; _ }; _ } as define; _ } as define_node)
    =
    let resolved_type_lookup =
      TypeCheck.get_or_recompute_local_annotations ~environment:type_environment name
      |> function
      | Some resolved_type_lookup -> resolved_type_lookup
      | None -> LocalAnnotationMap.empty () |> LocalAnnotationMap.read_only
    in
    let cfg = Cfg.create define in
    let walk_statement node_id statement_index statement =
      let pre_annotations, post_annotations =
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        ( LocalAnnotationMap.ReadOnly.get_precondition resolved_type_lookup ~statement_key
          |> Option.value ~default:Refinement.Store.empty,
          LocalAnnotationMap.ReadOnly.get_postcondition resolved_type_lookup ~statement_key
          |> Option.value ~default:Refinement.Store.empty )
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
      CreateLookupsIncludingTypeAnnotationsVisitor.visit
        {
          CreateDefinitionAndAnnotationLookupVisitor.pre_resolution;
          post_resolution;
          resolved_types_lookup;
          definitions_lookup;
        }
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
  { resolved_types_lookup; definitions_lookup }


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


let get_resolved_type { resolved_types_lookup; _ } ~position =
  get_best_location resolved_types_lookup ~position


let get_definition { definitions_lookup; _ } ~position =
  get_best_location definitions_lookup ~position >>| snd


let get_all_resolved_types { resolved_types_lookup; _ } = Hashtbl.to_alist resolved_types_lookup

let get_all_definitions { definitions_lookup; _ } = Hashtbl.to_alist definitions_lookup
