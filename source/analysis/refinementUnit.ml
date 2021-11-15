(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Annotation

type t = {
  base: Annotation.t option;
  attribute_refinements: t Identifier.Map.Tree.t;
}
[@@deriving eq]

let top =
  {
    base = Some (Annotation.create_mutable Type.Top);
    attribute_refinements = Identifier.Map.Tree.empty;
  }


let rec pp format { base; attribute_refinements } =
  let attribute_map_entry (identifier, refinement_unit) =
    Format.asprintf "%a -> %a" Identifier.pp identifier pp refinement_unit
  in
  (match base with
  | Some base -> Format.fprintf format "[Base: %a; " Annotation.pp base
  | None -> Format.fprintf format "[Base: (); ");
  Map.Tree.to_alist attribute_refinements
  |> List.map ~f:attribute_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Attributes: [%s]]"


let show = Format.asprintf "%a" pp

let find = Identifier.Map.Tree.find

let base { base; _ } = base

let create ?base ?(attribute_refinements = Identifier.Map.Tree.empty) () =
  { base; attribute_refinements }


let add_attribute_refinement refinement_unit ~reference ~base =
  let rec add_attribute_refinement
      ({ attribute_refinements; _ } as refinement_unit)
      ~base
      ~attributes
    =
    match attributes with
    | [] -> { refinement_unit with base = Some base }
    | attribute :: attributes ->
        {
          refinement_unit with
          attribute_refinements =
            attribute_refinements
            |> Identifier.Map.Tree.set
                 ~key:attribute
                 ~data:
                   (find attribute_refinements attribute
                   |> Option.value ~default:(create ())
                   |> add_attribute_refinement ~base ~attributes);
        }
  in
  add_attribute_refinement refinement_unit ~base ~attributes:(reference |> Reference.as_list)


let set_base refinement_unit ~base = { refinement_unit with base = Some base }

let annotation refinement_unit ~reference =
  let rec annotation { base; attribute_refinements } ~attributes =
    match attributes with
    | [] -> base
    | attribute :: attributes -> (
        match find attribute_refinements attribute with
        | Some refinement_unit -> annotation refinement_unit ~attributes
        | None -> None)
  in
  annotation refinement_unit ~attributes:(reference |> Reference.as_list)


let less_or_equal
    ~global_resolution
    { base = left_base; attribute_refinements = left_attributes }
    { base = right_base; attribute_refinements = right_attributes }
  =
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
  let attributes_less_or_equal =
    let rec less_or_equal left_attributes right_attributes =
      let compare_refinement_units ~key:_ ~data sofar =
        match data with
        | `Both
            ( { base = left_base; attribute_refinements = left_attributes },
              { base = right_base; attribute_refinements = right_attributes } ) ->
            let inner () = annotation_less_or_equal left_base right_base in
            sofar && inner () && less_or_equal left_attributes right_attributes
        | `Left _
        | `Right _ ->
            (* only compare refinement units which possess the same attributes *)
            false
      in
      Identifier.Map.Tree.fold2
        left_attributes
        right_attributes
        ~init:true
        ~f:compare_refinement_units
    in
    less_or_equal left_attributes right_attributes
  in
  annotation_less_or_equal left_base right_base && attributes_less_or_equal


let join
    ~global_resolution
    ({ base = left_base; attribute_refinements = left_attributes } as left)
    ({ base = right_base; attribute_refinements = right_attributes } as right)
  =
  if equal left top || equal right top then
    top
  else
    let valid_join left_base right_base =
      match left_base, right_base with
      | Some left, Some right ->
          let valid =
            GlobalResolution.less_or_equal
              global_resolution
              ~left:left.annotation
              ~right:right.annotation
            || GlobalResolution.less_or_equal
                 global_resolution
                 ~left:right.annotation
                 ~right:left.annotation
          in
          let base =
            Annotation.join ~type_join:(GlobalResolution.join global_resolution) left right
          in
          valid, Some base
      | None, None ->
          (* you only want to continue the nested join should both attribute trees exist *)
          not (Map.Tree.is_empty left_attributes || Map.Tree.is_empty right_attributes), None
      | _ -> false, None
    in
    let rec create_refinement_unit (valid, base) ~left_attributes ~right_attributes =
      let join left_attributes right_attributes =
        let join_refinement_units ~key ~data sofar =
          match data with
          | `Both
              ( { base = left_base; attribute_refinements = left_attributes },
                { base = right_base; attribute_refinements = right_attributes } ) ->
              valid_join left_base right_base
              |> fun annotation ->
              Identifier.Map.Tree.set
                sofar
                ~key
                ~data:(create_refinement_unit annotation ~left_attributes ~right_attributes)
          | `Left _
          | `Right _ ->
              sofar
        in
        Identifier.Map.Tree.fold2
          left_attributes
          right_attributes
          ~init:Identifier.Map.Tree.empty
          ~f:join_refinement_units
      in
      let attribute_refinements =
        if valid then
          join left_attributes right_attributes
        else
          Identifier.Map.Tree.empty
      in
      create ~attribute_refinements ()
      |> fun refinement_unit ->
      match base with
      | Some base -> set_base refinement_unit ~base
      | _ -> refinement_unit
    in
    valid_join left_base right_base |> create_refinement_unit ~left_attributes ~right_attributes


let meet
    ~global_resolution
    { base = left_base; attribute_refinements = left_attributes }
    { base = right_base; attribute_refinements = right_attributes }
  =
  let valid_meet left_base right_base =
    match left_base, right_base with
    | Some left, Some right ->
        let valid =
          GlobalResolution.less_or_equal
            global_resolution
            ~left:left.annotation
            ~right:right.annotation
          || GlobalResolution.less_or_equal
               global_resolution
               ~left:right.annotation
               ~right:left.annotation
        in
        let base =
          Annotation.meet ~type_meet:(GlobalResolution.meet global_resolution) left right
        in
        valid, Some base
    | None, None ->
        (* you only want to continue the nested meet should both attribute trees exist *)
        not (Map.Tree.is_empty left_attributes || Map.Tree.is_empty right_attributes), None
    | _ -> false, None
  in
  let rec create_refinement_unit (valid, base) ~left_attributes ~right_attributes =
    let meet left_attributes right_attributes =
      let meet_refinement_units ~key ~data sofar =
        match data with
        | `Both
            ( { base = left_base; attribute_refinements = left_attributes },
              { base = right_base; attribute_refinements = right_attributes } ) ->
            valid_meet left_base right_base
            |> fun annotation ->
            Identifier.Map.Tree.set
              sofar
              ~key
              ~data:(create_refinement_unit annotation ~left_attributes ~right_attributes)
        | `Left refinement_unit
        | `Right refinement_unit ->
            Identifier.Map.Tree.set sofar ~key ~data:refinement_unit
      in
      Identifier.Map.Tree.fold2
        left_attributes
        right_attributes
        ~init:Identifier.Map.Tree.empty
        ~f:meet_refinement_units
    in
    let attribute_refinements =
      if valid then
        meet left_attributes right_attributes
      else
        Identifier.Map.Tree.empty
    in
    create ~attribute_refinements ()
    |> fun refinement_unit ->
    match base with
    | Some base -> set_base refinement_unit ~base
    | _ -> refinement_unit
  in
  valid_meet left_base right_base |> create_refinement_unit ~left_attributes ~right_attributes


let widen ~global_resolution ~widening_threshold ~previous ~next ~iteration =
  if iteration + 1 >= widening_threshold then
    create ~base:(Annotation.create_mutable Type.Top) ()
  else
    join ~global_resolution previous next
