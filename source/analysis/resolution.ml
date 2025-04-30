(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A Resolution.t value represents all of the type information in a local scope. In Pyre, every
   expression can potentially have a distinct Resolution.t because side effects of evaluating parts
   of an expression can lead to type refinement (for example ternary expressions) or the
   introduction of new names (for example the walrus operator or the placeholder variables in a
   comprehension).

   If you only neeed *global* scope information, then use GlobalResolution.t rather than
   Resolution.t *)

open Core
open Ast
open Pyre

type t = {
  global_resolution: GlobalResolution.t;
  type_info_store: TypeInfo.Store.t;
  type_variables: Type.Variable.Set.t;
  resolve_expression: resolution:t -> Expression.t -> t * TypeInfo.Unit.t;
  resolve_statement: resolution:t -> Statement.t -> resolve_statement_result_t;
  parent: Reference.t option;
}

and resolve_statement_result_t =
  | Unreachable
  | Reachable of {
      resolution: t;
      errors: AnalysisError.t list;
    }

let create ~global_resolution ~type_info_store ~resolve_expression ~resolve_statement ?parent () =
  {
    global_resolution;
    type_info_store;
    type_variables = Type.Variable.Set.empty;
    resolve_expression;
    resolve_statement;
    parent;
  }


let pp format { type_info_store; type_variables; _ } =
  Set.to_list type_variables
  |> List.map ~f:Type.Variable.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Format.fprintf format "%a" TypeInfo.Store.pp type_info_store


let show resolution = Format.asprintf "%a" pp resolution

let is_global { global_resolution; _ } ~reference =
  Reference.delocalize reference |> GlobalResolution.global global_resolution |> Option.is_some


let resolve_expression ({ resolve_expression; _ } as resolution) expression =
  let resolution, type_info = resolve_expression ~resolution expression in
  resolution, TypeInfo.Unit.annotation type_info


let resolve_expression_to_type ({ resolve_expression; _ } as resolution) expression =
  resolve_expression ~resolution expression |> snd |> TypeInfo.Unit.annotation


let resolve_expression_to_type_info ({ resolve_expression; _ } as resolution) expression =
  resolve_expression ~resolution expression |> snd


let resolve_reference ({ resolve_expression; _ } as resolution) reference =
  Expression.from_reference ~location:Location.any reference
  |> resolve_expression ~resolution
  |> snd
  |> TypeInfo.Unit.annotation


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
    { Statement.Assert.test = asserted_expression; message = None; origin = None }
  |> Ast.Node.create_with_default_location
  |> resolve_statement ~resolution
  |> function
  | Unreachable -> None
  | Reachable { resolution; _ } -> Some resolution


let has_nontemporary_type_info ~reference resolution =
  TypeInfo.Store.has_nontemporary_type_info ~name:reference resolution.type_info_store


let new_local ?(temporary = false) resolution ~reference ~type_info =
  {
    resolution with
    type_info_store =
      resolution.type_info_store
      |> TypeInfo.Store.new_as_base ~temporary ~name:reference ~base:type_info;
  }


let refine_local ?(temporary = false) resolution ~reference ~type_info =
  {
    resolution with
    type_info_store =
      resolution.type_info_store
      |> TypeInfo.Store.set_base ~temporary ~name:reference ~base:type_info;
  }


let set_local_with_attributes
    ~wipe_subtree
    ?(temporary = false)
    resolution
    ~name
    ~attribute_path
    ~base_type_info
    ~type_info
  =
  {
    resolution with
    type_info_store =
      resolution.type_info_store
      |> TypeInfo.Store.set_type_info
           ~temporary
           ~wipe_subtree
           ~name
           ~attribute_path
           ~base_type_info
           ~type_info;
  }


let new_local_with_attributes = set_local_with_attributes ~wipe_subtree:true

let refine_local_with_attributes = set_local_with_attributes ~wipe_subtree:false

let get_local ?(global_fallback = true) ~reference { type_info_store; global_resolution; _ } =
  match TypeInfo.Store.get_base ~name:reference type_info_store with
  | Some _ as result -> result
  | None when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.delocalize reference |> global >>| fun { type_info; _ } -> type_info
  | None -> None


let get_local_with_attributes
    ?(global_fallback = true)
    ~name
    ~attribute_path
    { type_info_store; global_resolution; _ }
  =
  match TypeInfo.Store.get_type_info ~name ~attribute_path type_info_store with
  | Some _ as result -> result
  | None when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.(combine name attribute_path |> delocalize)
      |> global
      >>| fun { type_info; _ } -> type_info
  | None -> None


let unset_local
    ({ type_info_store = { type_info; temporary_type_info }; _ } as resolution)
    ~reference
  =
  {
    resolution with
    type_info_store =
      {
        type_info = Reference.Map.Tree.remove type_info reference;
        temporary_type_info = Reference.Map.Tree.remove temporary_type_info reference;
      };
  }


let clear_temporary_type_info ({ type_info_store; _ } as resolution) =
  {
    resolution with
    type_info_store = { type_info_store with temporary_type_info = Reference.Map.Tree.empty };
  }


let resolve_attribute_access resolution ~base_type ~attribute =
  let unique_name = Reference.create "$n" in
  let resolution =
    new_local resolution ~reference:unique_name ~type_info:(TypeInfo.Unit.create_mutable base_type)
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
  let new_local resolution (reference, type_info) = new_local resolution ~reference ~type_info in
  let resolution_with_locals = List.fold ~init:resolution ~f:new_local locals in
  resolve_expression ~resolution:resolution_with_locals expression
  |> snd
  |> TypeInfo.Unit.annotation


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable = Set.mem type_variables variable

let type_variable_name_exists { type_variables; _ } name =
  Set.exists type_variables ~f:(fun tv1 -> String.compare (Type.Variable.name tv1) name = 0)


let all_type_variables_in_scope { type_variables; _ } = Set.to_list type_variables

let type_info_store { type_info_store; _ } = type_info_store

let refinements_equal left right = TypeInfo.Store.equal left.type_info_store right.type_info_store

(** Meet refinements.

    Because the type variables in client code are always the same, we just take the left as an
    optimization *)
let meet_refinements left right =
  let global_resolution = left.global_resolution in
  {
    left with
    type_info_store =
      TypeInfo.Store.meet
        ~type_meet:(GlobalResolution.meet global_resolution)
        left.type_info_store
        right.type_info_store;
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
    type_info_store =
      TypeInfo.Store.outer_join
        ~type_join:(GlobalResolution.join global_resolution)
        left.type_info_store
        right.type_info_store;
  }


(** Similar to `outer_join_refinements`, but as a widening operation *)
let outer_widen_refinements ~iteration ~widening_threshold left right =
  let global_resolution = left.global_resolution in
  {
    left with
    type_info_store =
      TypeInfo.Store.outer_widen
        ~type_join:(GlobalResolution.join_for_branch_merge global_resolution)
        ~iteration
        ~widening_threshold
        left.type_info_store
        right.type_info_store;
  }


let update_existing_refinements ~old_resolution ~new_resolution =
  {
    old_resolution with
    type_info_store =
      TypeInfo.Store.update_existing
        ~old_store:old_resolution.type_info_store
        ~new_store:new_resolution.type_info_store;
  }


(** Update the refinements in `old_resolution` to match `new_resolution`, except for locals where
    `filter` applied to the new name + annotation returns false *)
let update_refinements_with_filter ~old_resolution ~new_resolution ~filter =
  {
    old_resolution with
    type_info_store =
      TypeInfo.Store.update_with_filter
        ~old_store:old_resolution.type_info_store
        ~new_store:new_resolution.type_info_store
        ~filter;
  }


let with_type_info_store resolution ~type_info_store = { resolution with type_info_store }

let parent { parent; _ } = parent

let with_parent resolution ~parent = { resolution with parent }

let resolution_for_statement ~local_annotations ~parent ~statement_key resolution =
  let type_info_store =
    local_annotations
    >>= TypeInfo.ForFunctionBody.ReadOnly.get_precondition ~statement_key
    |> Option.value ~default:TypeInfo.Store.empty
  in
  with_type_info_store ~type_info_store resolution |> with_parent ~parent


let is_consistent_with ({ global_resolution; _ } as resolution) =
  GlobalResolution.is_consistent_with
    global_resolution
    ~resolve:(resolve_expression_to_type resolution)


let global_resolution { global_resolution; _ } = global_resolution

let get_variable resolution =
  let global_resolution = global_resolution resolution in
  GlobalResolution.get_variable global_resolution


let variables resolution name =
  match get_variable resolution name with
  | Some variable -> Some variable
  | _ -> None


let fallback_attribute
    ?(accessed_through_class = false)
    ?(type_for_lookup = None)
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
          global_resolution
          class_name
          ~accessed_through_class:false
          ~transitive:true
          ~type_for_lookup:(Option.value type_for_lookup ~default:(Type.Primitive class_name))
          ~name
    | _ -> None
  in
  let getattr_backup () =
    let fallback =
      GlobalResolution.attribute_from_class_name
        global_resolution
        class_name
        ~accessed_through_class
        ~special_method:true
        ~transitive:true
        ~name:"__getattr__"
        ~type_for_lookup:(Type.Primitive class_name)
    in
    match fallback with
    | Some fallback when AnnotatedAttribute.defined fallback -> (
        let annotation = fallback |> AnnotatedAttribute.annotation |> TypeInfo.Unit.annotation in
        match annotation with
        | Parametric
            {
              name = "BoundMethod";
              arguments =
                [Single (Callable ({ implementation; _ } as callable)); Single self_argument];
            } ->
            let return_annotation =
              match
                GlobalResolution.signature_select
                  global_resolution
                  ~resolve_with_locals:(resolve_expression_to_type_with_locals resolution)
                  ~arguments:
                    [{ expression = None; kind = Positional; resolved = Type.literal_string name }]
                  ~location:Location.any
                  ~callable
                  ~self_argument:(Some self_argument)
              with
              | Found { selected_return_annotation } -> selected_return_annotation
              | NotFound _ -> Type.Callable.Overload.return_annotation implementation
            in
            Some
              (AnnotatedAttribute.create_instantiated
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
                 ~undecorated_signature:None)
        | _ -> None)
    | _ -> None
  in
  match compound_backup with
  | Some backup when AnnotatedAttribute.defined backup -> Some backup
  | _ -> getattr_backup ()
