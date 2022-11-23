(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Annotation
open LatticeOfMaps

module Unit = struct
  type t = {
    base: Annotation.t option;
    attributes: t Identifier.Map.Tree.t;
  }
  [@@deriving eq]

  let empty = { base = None; attributes = IdentifierMap.empty }

  let create base = { empty with base = Some base }

  let create_mutable type_ = create (Annotation.create_mutable type_)

  let top = create (Annotation.create_mutable Type.Top)

  let rec pp format { base; attributes } =
    let attribute_map_entry (identifier, refinement_unit) =
      Format.asprintf "%a -> %a" Identifier.pp identifier pp refinement_unit
    in
    (match base with
    | Some base -> Format.fprintf format "[Base: %a; " Annotation.pp base
    | None -> Format.fprintf format "[Base: (); ");
    Map.Tree.to_alist attributes
    |> List.map ~f:attribute_map_entry
    |> String.concat ~sep:", "
    |> Format.fprintf format "Attributes: [%s]]"


  let show = Format.asprintf "%a" pp

  let find = Identifier.Map.Tree.find

  let base { base; _ } = base

  let set_base refinement_unit ~base = { refinement_unit with base = Some base }

  let set_base_if_none refinement_unit ~base =
    { refinement_unit with base = Option.first_some refinement_unit.base base }


  (** If `attribute_path` is empty, set the base annotation. Otherwise, find the appropriate
      attribute (traversing intermediate units and constructing new ones as needed) and set the base
      there. *)
  let set_annotation ?(wipe_subtree = false) ~attribute_path ~annotation refinement_unit =
    let rec recurse ~annotation ~identifiers ({ attributes; _ } as refinement_unit) =
      match identifiers with
      | [] ->
          if wipe_subtree then
            { empty with base = Some annotation }
          else
            { refinement_unit with base = Some annotation }
      | identifier :: identifiers ->
          {
            refinement_unit with
            attributes =
              attributes
              |> IdentifierMap.set
                   ~key:identifier
                   ~data:
                     (find attributes identifier
                     |> Option.value ~default:empty
                     |> recurse ~annotation ~identifiers);
          }
    in
    recurse ~annotation ~identifiers:(attribute_path |> Reference.as_list) refinement_unit


  (** If `attribute_path` is empty, get the base annotation. Otherwise, find the appropriate
      attribute (traversing intermediate units until we finish or hit a dead end) and return the
      base found there, if any *)
  let get_annotation ~attribute_path refinement_unit =
    let rec recurse { base; attributes } ~identifiers =
      match identifiers with
      | [] -> base
      | identifier :: identifiers -> (
          match find attributes identifier with
          | Some refinement_unit -> recurse refinement_unit ~identifiers
          | None -> None)
    in
    recurse refinement_unit ~identifiers:(attribute_path |> Reference.as_list)


  let rec less_or_equal ~global_resolution ~left ~right =
    let base_less_or_equal left_base right_base =
      match left_base, right_base with
      | Some left, Some right ->
          Annotation.less_or_equal
            ~type_less_or_equal:(GlobalResolution.less_or_equal global_resolution)
            ~left
            ~right
      | None, None -> true (* intermediate refinement units don't require computation *)
      | _ -> false
    in
    let less_or_equal_one = less_or_equal ~global_resolution in
    base_less_or_equal left.base right.base
    && IdentifierMap.less_or_equal ~less_or_equal_one ~left:left.attributes ~right:right.attributes


  let rec join ~global_resolution left right =
    if equal left top || equal right top then
      top
    else
      let should_recurse, base =
        match left.base, right.base with
        | Some left, Some right ->
            ( GlobalResolution.types_are_orderable global_resolution left.annotation right.annotation,
              Some (Annotation.join ~type_join:(GlobalResolution.join global_resolution) left right)
            )
        | None, None ->
            (* you only want to continue the nested join should both attribute trees exist *)
            not (Map.Tree.is_empty left.attributes || Map.Tree.is_empty right.attributes), None
        | _ -> false, None
      in
      let attributes =
        if should_recurse then
          IdentifierMap.join ~join_one:(join ~global_resolution) left.attributes right.attributes
        else
          IdentifierMap.empty
      in
      { base; attributes }


  let rec meet ~global_resolution left right =
    let should_recurse, base =
      match left.base, right.base with
      | Some left, Some right ->
          ( GlobalResolution.types_are_orderable global_resolution left.annotation right.annotation,
            Some (Annotation.meet ~type_meet:(GlobalResolution.meet global_resolution) left right) )
      | None, None ->
          (* you only want to continue the nested meet should at least one attribute tree exists *)
          not (Map.Tree.is_empty left.attributes && Map.Tree.is_empty right.attributes), None
      | _ -> false, None
    in
    let attributes =
      if should_recurse then
        IdentifierMap.meet ~meet_one:(meet ~global_resolution) left.attributes right.attributes
      else
        IdentifierMap.empty
    in
    { base; attributes }


  let widen ~global_resolution ~widening_threshold ~iteration left right =
    if iteration + 1 >= widening_threshold then
      create (Annotation.create_mutable Type.Top)
    else
      join ~global_resolution left right


  let join_annotations ~global_resolution left right =
    let refined =
      join ~global_resolution (create left) (create right)
      |> base
      |> Option.value ~default:(Annotation.create_mutable Type.Bottom)
    in
    { refined with annotation = Type.union [left.annotation; right.annotation] }
end

module Store = struct
  type t = {
    annotations: Unit.t Reference.Map.t;
    temporary_annotations: Unit.t Reference.Map.t;
  }
  [@@deriving eq]

  let empty = { annotations = ReferenceMap.empty; temporary_annotations = ReferenceMap.empty }

  let pp format { annotations; temporary_annotations } =
    let show_annotation (reference, unit) =
      Format.asprintf "%a -> %a" Reference.pp reference Unit.pp unit
    in
    Map.to_alist annotations
    |> List.map ~f:show_annotation
    |> String.concat ~sep:", "
    |> Format.fprintf format "Annotations: [%s]\n";
    Map.to_alist temporary_annotations
    |> List.map ~f:show_annotation
    |> String.concat ~sep:", "
    |> Format.fprintf format "Temporary Annotations: [%s]"


  let show = Format.asprintf "%a" pp

  let has_nontemporary_annotation ~name { annotations; _ } = ReferenceMap.mem annotations name

  let get_unit ?(include_temporary = true) ~name { annotations; temporary_annotations } =
    let temporary =
      if include_temporary then
        ReferenceMap.find temporary_annotations name
      else
        None
    in
    let found =
      match temporary with
      | Some _ -> temporary
      | None -> ReferenceMap.find annotations name
    in
    Option.value ~default:Unit.empty found


  (** Map an operation over what's at a given name. If there's nothing already existing, use
      `empty`.

      The way we handle temporary vs non-temporary is very particular:

      - If `temporary` is true we only apply this to `temporary_annotations`
      - Otherwise, we apply it to `annotations` and also apply it to any *existing* data in
        `temporary_annotations`, but we don't create any new `temporary_annotations`.
      - The idea here is to minimize the amount of duplicated data, but ensure that `annotations`
        and `temporary_annotations` always have a consistent view of (non-temporary) refinements. *)
  let map_over_name ~temporary ~name ~f { annotations; temporary_annotations } =
    let map_over_reference_map ~fallback reference_map =
      match Option.first_some (ReferenceMap.find reference_map name) fallback with
      | Some unit -> ReferenceMap.set ~key:name ~data:(f unit) reference_map
      | None -> reference_map
    in
    if temporary then
      {
        annotations;
        temporary_annotations =
          map_over_reference_map ~fallback:(Some Unit.empty) temporary_annotations;
      }
    else
      {
        annotations = map_over_reference_map ~fallback:(Some Unit.empty) annotations;
        temporary_annotations = map_over_reference_map ~fallback:None temporary_annotations;
      }


  let get_base ~name store = get_unit ~name store |> Unit.base

  let get_annotation ~name ~attribute_path store =
    get_unit ~name store |> Unit.get_annotation ~attribute_path


  let set_base ?(temporary = false) ~name ~base store =
    map_over_name ~temporary ~name ~f:(Unit.set_base ~base) store


  let new_as_base ?(temporary = false) ~name ~base { annotations; temporary_annotations } =
    if temporary then
      {
        annotations;
        temporary_annotations =
          ReferenceMap.set temporary_annotations ~key:name ~data:(Unit.create base);
      }
    else
      {
        annotations = ReferenceMap.set annotations ~key:name ~data:(Unit.create base);
        temporary_annotations = ReferenceMap.remove temporary_annotations name;
      }


  let set_annotation
      ?(temporary = false)
      ?(wipe_subtree = false)
      ~name
      ~attribute_path
      ~base_annotation
      ~annotation
      store
    =
    let set_unit_annotation unit =
      unit
      |> Unit.set_annotation ~wipe_subtree ~attribute_path ~annotation
      |> Unit.set_base_if_none ~base:base_annotation
    in
    map_over_name ~temporary ~name ~f:set_unit_annotation store


  let less_or_equal ~global_resolution ~left ~right =
    let less_or_equal_one = Unit.less_or_equal ~global_resolution in
    ReferenceMap.less_or_equal ~less_or_equal_one ~left:left.annotations ~right:right.annotations
    && ReferenceMap.less_or_equal
         ~less_or_equal_one
         ~left:left.temporary_annotations
         ~right:right.temporary_annotations


  (** Whenever we know for sure that right is pointwise less_or_equal to left, then we can save
      computation by only checking for equality pointwise, which doesn't require type ordering
      operations *)
  let less_or_equal_monotone ~left ~right =
    let less_or_equal_one ~left ~right = Unit.equal left right in
    ReferenceMap.less_or_equal ~less_or_equal_one ~left:left.annotations ~right:right.annotations
    && ReferenceMap.less_or_equal
         ~less_or_equal_one
         ~left:left.temporary_annotations
         ~right:right.temporary_annotations


  let meet ~global_resolution left right =
    let meet_one = Unit.meet ~global_resolution in
    {
      annotations = ReferenceMap.meet ~meet_one left.annotations right.annotations;
      temporary_annotations =
        ReferenceMap.meet ~meet_one left.temporary_annotations right.temporary_annotations;
    }


  (** Use an "outer" join to join or widen stores, which means we are strict about types (a proper
      join) but permissive about variables that might only be instantiated on one side.

      This can be done as either a join or a widen depending whether we set `widening_threshod`,
      which is applied at the `Refinement.Unit` level. *)
  let widen_or_join ~merge_one left right =
    {
      (* Newly-instantiated locals live in `annotations`, so we merge with join *)
      annotations = ReferenceMap.merge_with ~merge_one left.annotations right.annotations;
      (* `temporary_annotations` only has type info, so we do a proper join *)
      temporary_annotations =
        ReferenceMap.join ~join_one:merge_one left.temporary_annotations right.temporary_annotations;
    }


  let outer_join ~global_resolution =
    let merge_one = Unit.join ~global_resolution in
    widen_or_join ~merge_one


  let outer_widen ~global_resolution ~iteration ~widening_threshold =
    let merge_one = Unit.widen ~global_resolution ~iteration ~widening_threshold in
    widen_or_join ~merge_one


  let update_existing ~old_store ~new_store =
    {
      annotations =
        ReferenceMap.update_existing_entries
          ~map_to_update:old_store.annotations
          ~new_map:new_store.annotations;
      temporary_annotations =
        ReferenceMap.update_existing_entries
          ~map_to_update:old_store.temporary_annotations
          ~new_map:new_store.temporary_annotations;
    }


  let update_with_filter ~old_store ~new_store ~filter =
    let update_map old_map new_map =
      let f ~key ~data sofar =
        if Unit.base data |> Option.map ~f:(filter key) |> Option.value ~default:false then
          sofar |> ReferenceMap.set ~key ~data
        else
          sofar
      in
      ReferenceMap.fold ~init:old_map ~f new_map
    in
    {
      annotations = update_map old_store.annotations new_store.annotations;
      temporary_annotations =
        update_map old_store.temporary_annotations new_store.temporary_annotations;
    }
end
