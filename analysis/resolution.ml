(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast

type t = {
  global_resolution: GlobalResolution.t;
  annotations: Annotation.t Reference.Map.t;
  type_variables: Type.Variable.Set.t;
  resolve: resolution:t -> Expression.t -> Annotation.t;
  resolve_assignment: resolution:t -> Statement.Assign.t -> t;
  parent: Reference.t option;
}

let create ~global_resolution ~annotations ~resolve ~resolve_assignment ?parent () =
  {
    global_resolution;
    annotations;
    type_variables = Type.Variable.Set.empty;
    resolve;
    resolve_assignment;
    parent;
  }


let pp format { annotations; type_variables; _ } =
  let annotation_map_entry (reference, annotation) =
    Format.asprintf "%a -> %a" Reference.pp reference Annotation.pp annotation
  in
  Type.Variable.Set.to_list type_variables
  |> List.map ~f:Type.Variable.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Map.to_alist annotations
  |> List.map ~f:annotation_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Annotations: [%s]"


let show resolution = Format.asprintf "%a" pp resolution

let is_global { global_resolution; _ } ~reference =
  Reference.delocalize reference |> GlobalResolution.global global_resolution |> Option.is_some


let set_local ({ annotations; _ } as resolution) ~reference ~annotation =
  { resolution with annotations = Map.set annotations ~key:reference ~data:annotation }


let get_local ?(global_fallback = true) ~reference { annotations; global_resolution; _ } =
  match Map.find annotations reference with
  | Some result when global_fallback || not (Annotation.is_global result) -> Some result
  | _ when global_fallback ->
      let global = GlobalResolution.global global_resolution in
      Reference.delocalize reference |> global >>| Node.value
  | _ -> None


let unset_local ({ annotations; _ } as resolution) ~reference =
  { resolution with annotations = Map.remove annotations reference }


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Type.Variable.Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable =
  Type.Variable.Set.mem type_variables variable


let all_type_variables_in_scope { type_variables; _ } = Type.Variable.Set.to_list type_variables

let annotations { annotations; _ } = annotations

let with_annotations resolution ~annotations = { resolution with annotations }

let parent { parent; _ } = parent

let with_parent resolution ~parent = { resolution with parent }

let resolve ({ resolve; _ } as resolution) expression =
  resolve ~resolution expression |> Annotation.annotation


let resolve_to_annotation ({ resolve; _ } as resolution) expression = resolve ~resolution expression

let resolve_reference ({ resolve; _ } as resolution) reference =
  Expression.from_reference ~location:Location.Reference.any reference
  |> resolve ~resolution
  |> Annotation.annotation


let resolve_assignment ({ resolve_assignment; _ } as resolution) assignment =
  resolve_assignment ~resolution assignment


let rec weaken_mutable_literals resolution ~expression ~resolved ~expected ~comparator =
  let open Expression in
  match expression with
  | Some { Node.value = Expression.List items; _ } -> (
      match resolved, expected with
      | ( Type.Parametric { name = "list"; parameters = Concrete [actual_item_type] },
          Type.Parametric { name = "list"; parameters = Concrete [expected_item_type] } ) ->
          let weakened_item_type =
            Type.union
              (List.map
                 ~f:(fun item ->
                   weaken_mutable_literals
                     resolution
                     ~expression:(Some item)
                     ~resolved:actual_item_type
                     ~expected:expected_item_type
                     ~comparator)
                 items)
          in
          if comparator ~left:weakened_item_type ~right:expected_item_type then
            expected
          else
            Type.list weakened_item_type
      | _ -> resolved )
  | Some { Node.value = Expression.ListComprehension _; _ } -> (
      match resolved, expected with
      | ( Type.Parametric { name = "list"; parameters = Concrete [actual] },
          Type.Parametric { name = "list"; parameters = Concrete [expected_parameter] } )
        when comparator ~left:actual ~right:expected_parameter ->
          expected
      | _ -> resolved )
  | Some { Node.value = Expression.Set items; _ } -> (
      match resolved, expected with
      | ( Type.Parametric { name = "set"; parameters = Concrete [actual_item_type] },
          Type.Parametric { name = "set"; parameters = Concrete [expected_item_type] } ) ->
          let weakened_item_type =
            Type.union
              (List.map
                 ~f:(fun item ->
                   weaken_mutable_literals
                     resolution
                     ~expression:(Some item)
                     ~resolved:actual_item_type
                     ~expected:expected_item_type
                     ~comparator)
                 items)
          in
          if comparator ~left:weakened_item_type ~right:expected_item_type then
            expected
          else
            Type.set weakened_item_type
      | _ -> resolved )
  | Some { Node.value = Expression.SetComprehension _; _ } -> (
      match resolved, expected with
      | ( Type.Parametric { name = "set"; parameters = Concrete [actual] },
          Type.Parametric { name = "set"; parameters = Concrete [expected_parameter] } )
        when comparator ~left:actual ~right:expected_parameter ->
          expected
      | _ -> resolved )
  | Some { Node.value = Expression.Dictionary { entries; keywords = [] }; _ }
    when Type.is_typed_dictionary expected -> (
      match expected with
      | Type.TypedDictionary { total; fields; _ } ->
          let find_matching_field ~name =
            let matching_name { Type.name = expected_name; _ } = String.equal name expected_name in
            List.find ~f:matching_name
          in
          let resolve_entry { Dictionary.Entry.key; value } =
            let key = resolve resolution key in
            match key with
            | Type.Literal (Type.String name) ->
                let annotation =
                  let resolved = resolve resolution value in
                  let relax { Type.annotation; _ } =
                    if Type.is_dictionary resolved || Type.is_typed_dictionary resolved then
                      weaken_mutable_literals
                        resolution
                        ~expression:(Some value)
                        ~resolved
                        ~expected:annotation
                        ~comparator
                    else if comparator ~left:resolved ~right:annotation then
                      annotation
                    else
                      resolved
                  in
                  find_matching_field fields ~name >>| relax |> Option.value ~default:resolved
                in
                Some { Type.name; annotation }
            | _ -> None
          in
          let add_missing_fields sofar =
            let is_missing { Type.name; _ } = Option.is_none (find_matching_field sofar ~name) in
            sofar @ List.filter fields ~f:is_missing
          in
          List.map entries ~f:resolve_entry
          |> Option.all
          >>| (if total then Fn.id else add_missing_fields)
          >>| Type.TypedDictionary.anonymous ~total
          |> Option.value_map ~default:resolved ~f:(fun typed_dictionary ->
                 if comparator ~left:typed_dictionary ~right:expected then
                   expected
                 else
                   typed_dictionary)
      | _ -> resolved )
  | Some { Node.value = Expression.Dictionary { entries; _ }; _ } ->
      weaken_dictionary_entries resolution ~resolved ~expected ~comparator ~entries
  | Some { Node.value = Expression.DictionaryComprehension _; _ } -> (
      match resolved, expected with
      | ( Type.Parametric { name = "dict"; parameters = Concrete [actual_key; actual_value] },
          Type.Parametric { name = "dict"; parameters = Concrete [expected_key; expected_value] } )
        when comparator ~left:actual_key ~right:expected_key
             && comparator ~left:actual_value ~right:expected_value ->
          expected
      | _ -> resolved )
  | _ -> resolved


and weaken_dictionary_entries resolution ~resolved ~expected ~comparator ~entries =
  match entries with
  | _ -> (
      match resolved, expected with
      | ( Type.Parametric
            { name = "dict"; parameters = Concrete [actual_key_type; actual_value_type] },
          Type.Parametric
            { name = "dict"; parameters = Concrete [expected_key_type; expected_value_type] } ) ->
          let weakened_key_type =
            Type.union
              (List.map
                 ~f:(fun { key; _ } ->
                   weaken_mutable_literals
                     resolution
                     ~expression:(Some key)
                     ~resolved:actual_key_type
                     ~expected:expected_key_type
                     ~comparator)
                 entries)
          in
          let weakened_value_type =
            Type.union
              (List.map
                 ~f:(fun { value; _ } ->
                   weaken_mutable_literals
                     resolution
                     ~expression:(Some value)
                     ~resolved:actual_value_type
                     ~expected:expected_value_type
                     ~comparator)
                 entries)
          in

          (* Note: We don't check for variance because we want {1: 1} to be ok for Dict[float,
             float] even though it gets resolved as Dict[Literal[1], Literal[1]].

             Also, we check the parameter types manually because (comparator
             ~left:weakened_dictionary_type ~right:expected) fails for `Dict[int, A]` and `Dict[int,
             B]. *)
          if
            comparator ~left:weakened_key_type ~right:expected_key_type
            && comparator ~left:weakened_value_type ~right:expected_value_type
          then
            expected
          else
            Type.dictionary ~key:weakened_key_type ~value:weakened_value_type
      | _ -> resolved )


let resolve_mutable_literals ({ global_resolution; _ } as resolution) =
  weaken_mutable_literals
    resolution
    ~comparator:(GlobalResolution.constraints_solution_exists global_resolution)


let is_consistent_with ({ global_resolution; _ } as resolution) left right ~expression =
  let comparator ~left ~right =
    GlobalResolution.constraints_solution_exists global_resolution ~left ~right
  in
  let left =
    weaken_mutable_literals resolution ~expression ~resolved:left ~expected:right ~comparator
  in
  comparator ~left ~right


let global_resolution { global_resolution; _ } = global_resolution
