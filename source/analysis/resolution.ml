(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Pyre

type t = {
  global_resolution: GlobalResolution.t;
  annotation_store: Refinement.Store.t;
  type_variables: Type.Variable.Set.t;
  resolve_expression: resolution:t -> Expression.t -> t * Annotation.t;
  resolve_statement: resolution:t -> Statement.t -> resolve_statement_result_t;
  parent: Reference.t option;
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


let pp format { annotation_store; type_variables; _ } =
  Type.Variable.Set.to_list type_variables
  |> List.map ~f:Type.Variable.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Format.fprintf format "%a" Refinement.Store.pp annotation_store


let show resolution = Format.asprintf "%a" pp resolution

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
          base, attribute_list, Some (Annotation.create_mutable base_type)
      in
      partition_attribute (Reference.create head) attributes
      |> fun (base, attributes, annotation) ->
      base, Reference.create_from_list attributes, annotation
  | _ -> Reference.create_from_list identifiers, Reference.create "", None


let has_nontemporary_annotation ~reference resolution =
  Refinement.Store.has_nontemporary_annotation ~name:reference resolution.annotation_store


let new_local ?(temporary = false) resolution ~reference ~annotation =
  {
    resolution with
    annotation_store =
      resolution.annotation_store
      |> Refinement.Store.new_as_base ~temporary ~name:reference ~base:annotation;
  }


let refine_local ?(temporary = false) resolution ~reference ~annotation =
  {
    resolution with
    annotation_store =
      resolution.annotation_store
      |> Refinement.Store.set_base ~temporary ~name:reference ~base:annotation;
  }


let set_local_with_attributes ~wipe_subtree ?(temporary = false) resolution ~name ~annotation =
  let name, attribute_path, base = partition_name resolution ~name in
  {
    resolution with
    annotation_store =
      resolution.annotation_store
      |> Refinement.Store.set_annotation
           ~temporary
           ~wipe_subtree
           ~name
           ~attribute_path
           ~base
           ~annotation;
  }


let new_local_with_attributes = set_local_with_attributes ~wipe_subtree:true

let refine_local_with_attributes = set_local_with_attributes ~wipe_subtree:false

let get_local ?(global_fallback = true) ~reference { annotation_store; global_resolution; _ } =
  match Refinement.Store.get_base ~name:reference annotation_store with
  | Some _ as result -> result
  | None when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.delocalize reference |> global >>| fun { annotation; _ } -> annotation
  | None -> None


let get_local_with_attributes
    ?(global_fallback = true)
    ~name
    ({ annotation_store; global_resolution; _ } as resolution)
  =
  let name, attribute_path, _ = partition_name resolution ~name in
  match Refinement.Store.get_annotation ~name ~attribute_path annotation_store with
  | Some _ as result -> result
  | None when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.(combine name attribute_path |> delocalize)
      |> global
      >>| fun { annotation; _ } -> annotation
  | None -> None


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
    new_local resolution ~reference:unique_name ~annotation:(Annotation.create_mutable base_type)
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
  let new_local resolution (reference, annotation) = new_local resolution ~reference ~annotation in
  let resolution_with_locals = List.fold ~init:resolution ~f:new_local locals in
  resolve_expression ~resolution:resolution_with_locals expression |> snd |> Annotation.annotation


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Type.Variable.Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable =
  Type.Variable.Set.mem type_variables variable


let all_type_variables_in_scope { type_variables; _ } = Type.Variable.Set.to_list type_variables

let annotation_store { annotation_store; _ } = annotation_store

let refinements_equal left right =
  Refinement.Store.equal left.annotation_store right.annotation_store


(** Meet refinements.

    Because the type variables in client code are always the same, we just take the left as an
    optimization *)
let meet_refinements left right =
  let global_resolution = left.global_resolution in
  {
    left with
    annotation_store =
      Refinement.Store.meet ~global_resolution left.annotation_store right.annotation_store;
  }


(** Merge refinements. This means we join type information but preserve new bindings from both sides
    (which causes client code to be strict about type errors but permissive about uninstantiated
    variables)

    Because the type variables in client code are always the same, we just take the left as an
    optimization *)
let outer_join_refinements left right =
  let global_resolution = left.global_resolution in
  {
    left with
    annotation_store =
      Refinement.Store.outer_join ~global_resolution left.annotation_store right.annotation_store;
  }


(** Similar to `outer_join_refinements`, but as a widening operation *)
let outer_widen_refinements ~iteration ~widening_threshold left right =
  let global_resolution = left.global_resolution in
  {
    left with
    annotation_store =
      Refinement.Store.outer_widen
        ~global_resolution
        ~iteration
        ~widening_threshold
        left.annotation_store
        right.annotation_store;
  }


let update_existing_refinements ~old_resolution ~new_resolution =
  {
    old_resolution with
    annotation_store =
      Refinement.Store.update_existing
        ~old_store:old_resolution.annotation_store
        ~new_store:new_resolution.annotation_store;
  }


(** Update the refinements in `old_resolution` to match `new_resolution`, except for locals where
    `filter` applied to the new name + annotation returns false *)
let update_refinements_with_filter ~old_resolution ~new_resolution ~filter =
  {
    old_resolution with
    annotation_store =
      Refinement.Store.update_with_filter
        ~old_store:old_resolution.annotation_store
        ~new_store:new_resolution.annotation_store
        ~filter;
  }


let with_annotation_store resolution ~annotation_store = { resolution with annotation_store }

let parent { parent; _ } = parent

let with_parent resolution ~parent = { resolution with parent }

let is_consistent_with ({ global_resolution; _ } as resolution) =
  GlobalResolution.is_consistent_with
    global_resolution
    ~resolve:(resolve_expression_to_type resolution)


let global_resolution { global_resolution; _ } = global_resolution

let fallback_attribute
    ?(accessed_through_class = false)
    ?(instantiated = None)
    ~resolution
    ~name
    class_name
  =
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
          ~instantiated:(Option.value instantiated ~default:(Type.Primitive class_name))
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
