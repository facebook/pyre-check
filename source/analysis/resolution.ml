(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

type t = {
  global_resolution: GlobalResolution.t;
  annotation_store: annotation_store;
  type_variables: Type.Variable.Set.t;
  resolve_expression: resolution:t -> Expression.t -> t * Annotation.t;
  resolve_statement: resolution:t -> Statement.t -> resolve_statement_result_t;
  parent: Reference.t option;
}

and annotation_store = {
  annotations: RefinementUnit.t Reference.Map.t;
  temporary_annotations: RefinementUnit.t Reference.Map.t;
}

and resolve_statement_result_t =
  | Unreachable
  | Reachable of {
      resolution: t;
      errors: AnalysisError.t list;
    }

let create ~global_resolution ~annotation_store ~resolve_expression ~resolve_statement ?parent () =
  {
    global_resolution;
    annotation_store;
    type_variables = Type.Variable.Set.empty;
    resolve_expression;
    resolve_statement;
    parent;
  }


let pp format { annotation_store = { annotations; temporary_annotations }; type_variables; _ } =
  let annotation_store_entry (reference, refinement_unit) =
    Format.asprintf "%a -> %a" Reference.pp reference RefinementUnit.pp refinement_unit
  in
  Type.Variable.Set.to_list type_variables
  |> List.map ~f:Type.Variable.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Map.to_alist annotations
  |> List.map ~f:annotation_store_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Annotation Store: [%s]";
  Map.to_alist temporary_annotations
  |> List.map ~f:annotation_store_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Temporary Annotation Store: [%s]"


let show resolution = Format.asprintf "%a" pp resolution

let empty_annotation_store =
  { annotations = Reference.Map.empty; temporary_annotations = Reference.Map.empty }


let is_global { global_resolution; _ } ~reference =
  Reference.delocalize reference |> GlobalResolution.global global_resolution |> Option.is_some


let resolve_expression ({ resolve_expression; _ } as resolution) expression =
  let resolution, annotation = resolve_expression ~resolution expression in
  resolution, Annotation.annotation annotation


let resolve_expression_to_type ({ resolve_expression; _ } as resolution) expression =
  resolve_expression ~resolution expression |> snd |> Annotation.annotation


let resolve_expression_to_annotation ({ resolve_expression; _ } as resolution) expression =
  resolve_expression ~resolution expression |> snd


let resolve_reference ({ resolve_expression; _ } as resolution) reference =
  Expression.from_reference ~location:Location.any reference
  |> resolve_expression ~resolution
  |> snd
  |> Annotation.annotation


let resolve_statement ({ resolve_statement; _ } as resolution) statement =
  resolve_statement ~resolution statement


let resolve_assignment ({ resolve_statement; _ } as resolution) assign =
  Statement.Statement.Assign assign
  |> Ast.Node.create_with_default_location
  |> resolve_statement ~resolution
  |> function
  | Unreachable -> resolution
  | Reachable { resolution; _ } -> resolution


let resolve_assertion ({ resolve_statement; _ } as resolution) ~asserted_expression =
  Statement.Statement.Assert
    {
      Statement.Assert.test = asserted_expression;
      message = None;
      origin = Ast.Statement.Assert.Origin.Assertion;
    }
  |> Ast.Node.create_with_default_location
  |> resolve_statement ~resolution
  |> function
  | Unreachable -> None
  | Reachable { resolution; _ } -> Some resolution


let partition_name resolution ~name =
  let identifiers = Reference.as_list (Expression.name_to_reference_exn name) in
  match identifiers with
  | head :: attributes ->
      let rec partition_attribute base attribute_list =
        let base_type = resolve_reference resolution base in
        if Type.is_untyped base_type then
          match attribute_list with
          | [] -> Reference.create head, attributes, None
          | attribute :: attribute_list ->
              partition_attribute Reference.(attribute |> create |> combine base) attribute_list
        else
          base, attribute_list, Some (Annotation.create base_type)
      in
      partition_attribute (Reference.create head) attributes
      |> fun (base, attributes, annotation) ->
      base, Reference.create_from_list attributes, annotation
  | _ -> Reference.create_from_list identifiers, Reference.create "", None


let set_local
    ?(temporary = false)
    ({ annotation_store = { annotations; temporary_annotations }; _ } as resolution)
    ~reference
    ~annotation
  =
  let annotations, temporary_annotations =
    if temporary then
      ( annotations,
        Map.set
          temporary_annotations
          ~key:reference
          ~data:(RefinementUnit.create () |> RefinementUnit.set_base ~base:annotation) )
    else
      ( Map.set
          annotations
          ~key:reference
          ~data:(RefinementUnit.create () |> RefinementUnit.set_base ~base:annotation),
        temporary_annotations )
  in
  { resolution with annotation_store = { annotations; temporary_annotations } }


let set_local_with_attributes
    ?(temporary = false)
    ({ annotation_store = { annotations; temporary_annotations }; _ } as resolution)
    ~name
    ~annotation
  =
  let object_reference, attribute_path, base = partition_name resolution ~name in
  let set_base refinement_unit ~base =
    match RefinementUnit.base refinement_unit, base with
    | None, Some base -> RefinementUnit.set_base refinement_unit ~base
    | _ -> refinement_unit
  in
  let annotations, temporary_annotations =
    if temporary then
      ( annotations,
        Map.set
          temporary_annotations
          ~key:object_reference
          ~data:
            (Map.find temporary_annotations object_reference
            |> (fun existing -> Option.first_some existing (Map.find annotations object_reference))
            |> Option.value ~default:(RefinementUnit.create ())
            |> RefinementUnit.add_attribute_refinement ~reference:attribute_path ~base:annotation
            |> set_base ~base) )
    else
      ( Map.set
          annotations
          ~key:object_reference
          ~data:
            (Map.find annotations object_reference
            |> Option.value ~default:(RefinementUnit.create ())
            |> RefinementUnit.add_attribute_refinement ~reference:attribute_path ~base:annotation
            |> set_base ~base),
        temporary_annotations )
  in
  { resolution with annotation_store = { annotations; temporary_annotations } }


let get_local
    ?(global_fallback = true)
    ~reference
    ({ annotation_store = { annotations; temporary_annotations }; global_resolution; _ } as
    resolution)
  =
  match
    Option.first_some (Map.find temporary_annotations reference) (Map.find annotations reference)
  with
  | Some result when global_fallback || not (is_global resolution ~reference) ->
      RefinementUnit.base result
  | _ when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.delocalize reference |> global >>| fun { annotation; _ } -> annotation
  | _ -> None


let get_local_with_attributes
    ?(global_fallback = true)
    ~name
    ({ annotation_store = { annotations; temporary_annotations }; global_resolution; _ } as
    resolution)
  =
  let object_reference, attribute_path, _ = partition_name resolution ~name in
  match
    Option.first_some
      (Map.find temporary_annotations object_reference)
      (Map.find annotations object_reference)
  with
  | Some result when global_fallback || not (is_global resolution ~reference:object_reference) ->
      RefinementUnit.annotation result ~reference:attribute_path
  | _ when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.(combine object_reference attribute_path |> delocalize)
      |> global
      >>| fun { annotation; _ } -> annotation
  | _ -> None


let unset_local
    ({ annotation_store = { annotations; temporary_annotations }; _ } as resolution)
    ~reference
  =
  {
    resolution with
    annotation_store =
      {
        annotations = Map.remove annotations reference;
        temporary_annotations = Map.remove temporary_annotations reference;
      };
  }


let clear_temporary_annotations ({ annotation_store; _ } as resolution) =
  {
    resolution with
    annotation_store = { annotation_store with temporary_annotations = Reference.Map.empty };
  }


let resolve_attribute_access resolution ~base_type ~attribute =
  let unique_name = Reference.create "$n" in
  let resolution =
    set_local resolution ~reference:unique_name ~annotation:(Annotation.create base_type)
  in
  let expression_to_analyze =
    Expression.from_reference
      ~location:Location.any
      (Reference.create ~prefix:unique_name attribute)
  in
  resolve_expression_to_type resolution expression_to_analyze


let resolve_expression_to_type_with_locals
    ({ resolve_expression; _ } as resolution)
    ~locals
    expression
  =
  let add_local resolution (reference, annotation) = set_local resolution ~reference ~annotation in
  let resolution_with_locals = List.fold ~init:resolution ~f:add_local locals in
  resolve_expression ~resolution:resolution_with_locals expression |> snd |> Annotation.annotation


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Type.Variable.Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable =
  Type.Variable.Set.mem type_variables variable


let all_type_variables_in_scope { type_variables; _ } = Type.Variable.Set.to_list type_variables

let annotation_store { annotation_store; _ } = annotation_store

let annotations { annotation_store = { annotations; _ }; _ } = annotations

let temporary_annotations { annotation_store = { temporary_annotations; _ }; _ } =
  temporary_annotations


let with_annotation_store resolution ~annotation_store = { resolution with annotation_store }

let parent { parent; _ } = parent

let with_parent resolution ~parent = { resolution with parent }

let is_consistent_with ({ global_resolution; _ } as resolution) =
  GlobalResolution.is_consistent_with
    global_resolution
    ~resolve:(resolve_expression_to_type resolution)


let global_resolution { global_resolution; _ } = global_resolution

let fallback_attribute ?(accessed_through_class = false) ~resolution ~name class_name =
  let class_name_reference = Reference.create class_name in
  let global_resolution = global_resolution resolution in
  let compound_backup =
    let name =
      match name with
      | "__iadd__" -> Some "__add__"
      | "__isub__" -> Some "__sub__"
      | "__imul__" -> Some "__mul__"
      | "__imatmul__" -> Some "__matmul__"
      | "__itruediv__" -> Some "__truediv__"
      | "__ifloordiv__" -> Some "__floordiv__"
      | "__imod__" -> Some "__mod__"
      | "__idivmod__" -> Some "__divmod__"
      | "__ipow__" -> Some "__pow__"
      | "__ilshift__" -> Some "__lshift__"
      | "__irshift__" -> Some "__rshift__"
      | "__iand__" -> Some "__and__"
      | "__ixor__" -> Some "__xor__"
      | "__ior__" -> Some "__or__"
      | _ -> None
    in
    match name with
    | Some name ->
        GlobalResolution.attribute_from_class_name
          ~resolution:global_resolution
          class_name
          ~accessed_through_class:false
          ~transitive:true
          ~instantiated:(Type.Primitive class_name)
          ~name
    | _ -> None
  in
  let getattr_backup () =
    let fallback =
      GlobalResolution.attribute_from_class_name
        class_name
        ~accessed_through_class
        ~special_method:true
        ~transitive:true
        ~resolution:global_resolution
        ~name:"__getattr__"
        ~instantiated:(Type.Primitive class_name)
    in
    match fallback with
    | Some fallback when AnnotatedAttribute.defined fallback -> (
        let annotation = fallback |> AnnotatedAttribute.annotation |> Annotation.annotation in
        match annotation with
        | Parametric
            {
              name = "BoundMethod";
              parameters =
                [Single (Callable ({ implementation; _ } as callable)); Single self_argument];
            } ->
            let return_annotation =
              match
                GlobalResolution.signature_select
                  ~global_resolution
                  ~resolve_with_locals:(resolve_expression_to_type_with_locals resolution)
                  ~arguments:
                    [{ expression = None; kind = Positional; resolved = Type.literal_string name }]
                  ~callable
                  ~self_argument:(Some self_argument)
              with
              | Found { selected_return_annotation } -> selected_return_annotation
              | NotFound _ -> Type.Callable.Overload.return_annotation implementation
            in
            Some
              (AnnotatedAttribute.create
                 ~annotation:return_annotation
                 ~original_annotation:return_annotation
                 ~uninstantiated_annotation:(Some return_annotation)
                 ~abstract:false
                 ~async_property:false
                 ~class_variable:false
                 ~defined:true
                 ~initialized:NotInitialized
                 ~name
                 ~parent:(Reference.show class_name_reference)
                 ~visibility:ReadWrite
                 ~property:false
                 ~undecorated_signature:None
                 ~problem:None)
        | _ -> None)
    | _ -> None
  in
  match compound_backup with
  | Some backup when AnnotatedAttribute.defined backup -> Some backup
  | _ -> getattr_backup ()
