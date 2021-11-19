(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Annotation

module MapLattice = struct
  module type MapSignature = sig
    type key

    type 'data t

    val empty : 'data t

    val set : 'data t -> key:key -> data:'data -> 'data t

    val find : 'data t -> key -> 'data option

    val fold : 'data t -> init:'a -> f:(key:key -> data:'data -> 'a -> 'a) -> 'a

    val fold2
      :  'data t ->
      'data t ->
      init:'a ->
      f:(key:key -> data:[ `Both of 'data * 'data | `Left of 'data | `Right of 'data ] -> 'a -> 'a) ->
      'a
  end

  module Make (Map : MapSignature) = struct
    include Map

    (** The keys of `right` have to be a subset of the keys of `left` for `left` to be less than or
        equal to `right`, since more keys = more restrictions = lower in the lattice *)
    let less_or_equal ~less_or_equal_one ~left ~right =
      let f ~key ~data:right_data sofar =
        sofar
        &&
        match find left key with
        | Some left_data -> less_or_equal_one ~left:left_data ~right:right_data
        | None -> false
      in
      fold right ~init:true ~f


    let join ~join_one left right =
      let f ~key ~data sofar =
        match data with
        | `Both (left, right) -> set sofar ~key ~data:(join_one left right)
        | `Left _
        | `Right _ ->
            sofar
      in
      fold2 left right ~init:empty ~f


    let meet ~meet_one left right =
      let f ~key ~data sofar =
        match data with
        | `Both (left, right) -> set sofar ~key ~data:(meet_one left right)
        | `Left data
        | `Right data ->
            set sofar ~key ~data
      in
      fold2 left right ~init:empty ~f
  end
end

module IdentifierMap = MapLattice.Make (struct
  include Identifier.Map.Tree

  type key = Identifier.t
end)

module ReferenceMap = MapLattice.Make (struct
  include Reference.Map

  type key = Reference.t
end)

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

  let add_attribute_refinement refinement_unit ~reference ~annotation =
    let rec add_attribute_refinement ({ attributes; _ } as refinement_unit) ~annotation ~identifiers
      =
      match identifiers with
      | [] -> { refinement_unit with base = Some annotation }
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
                     |> add_attribute_refinement ~annotation ~identifiers);
          }
    in
    add_attribute_refinement
      refinement_unit
      ~annotation
      ~identifiers:(reference |> Reference.as_list)


  let annotation refinement_unit ~reference =
    let rec annotation { base; attributes } ~identifiers =
      match identifiers with
      | [] -> base
      | identifier :: identifiers -> (
          match find attributes identifier with
          | Some refinement_unit -> annotation refinement_unit ~identifiers
          | None -> None)
    in
    annotation refinement_unit ~identifiers:(reference |> Reference.as_list)


  let rec less_or_equal ~global_resolution left right =
    let annotation_less_or_equal left_base right_base =
      match left_base, right_base with
      | Some left, Some right ->
          Annotation.less_or_equal
            ~type_less_or_equal:(GlobalResolution.less_or_equal global_resolution)
            ~left
            ~right
      | None, None -> true (* intermediate refinement units don't require computation *)
      | _ -> false
    in
    let less_or_equal_one ~left ~right = less_or_equal ~global_resolution left right in
    annotation_less_or_equal left.base right.base
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


  let widen ~global_resolution ~widening_threshold ~previous ~next ~iteration =
    if iteration + 1 >= widening_threshold then
      create (Annotation.create_mutable Type.Top)
    else
      join ~global_resolution previous next
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
    |> Format.fprintf format "Annotations: [%s]";
    Map.to_alist temporary_annotations
    |> List.map ~f:show_annotation
    |> String.concat ~sep:", "
    |> Format.fprintf format "Temporary Annotations: [%s]"


  let show = Format.asprintf "%a" pp

  let less_or_equal ~global_resolution ~left ~right =
    let less_or_equal_one ~left ~right = Unit.less_or_equal ~global_resolution left right in
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
end
