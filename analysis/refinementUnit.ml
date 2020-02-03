(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Annotation

type t = {
  base: Annotation.t option;
  attribute_refinements: t Identifier.Map.Tree.t;
}
[@@deriving eq]

let rec pp format { base; attribute_refinements } =
  let attribute_map_entry (identifier, refinement_unit) =
    Format.asprintf "%a -> %a" Identifier.pp identifier pp refinement_unit
  in
  ( match base with
  | Some base -> Format.fprintf format "[Base: %a; " Annotation.pp base
  | None -> Format.fprintf format "[Base: (); " );
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
                   ( find attribute_refinements attribute
                   |> Option.value ~default:(create ())
                   |> add_attribute_refinement ~base ~attributes );
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
        | None -> None )
  in
  annotation refinement_unit ~attributes:(reference |> Reference.as_list)


let refine ~global_resolution { annotation; mutability } refined =
  match mutability with
  | Mutable -> { annotation = refined; mutability }
  | Immutable { original; _ } ->
      let annotation =
        match refined with
        | Type.Top -> refined
        | Type.Bottom -> annotation
        | refined ->
            GlobalResolution.solve_less_or_equal
              global_resolution
              ~constraints:TypeConstraints.empty
              ~left:refined
              ~right:original
            |> List.filter_map ~f:(GlobalResolution.solve_constraints global_resolution)
            |> List.hd
            >>| (fun solution -> TypeConstraints.Solution.instantiate solution refined)
            |> Option.value ~default:annotation
      in
      let refine =
        Type.is_top refined
        || (not (Type.is_unbound refined))
           && GlobalResolution.less_or_equal global_resolution ~left:refined ~right:original
      in
      if refine then
        { annotation = refined; mutability }
      else
        { annotation; mutability }


let less_or_equal
    ~global_resolution
    { base = left_base; attribute_refinements = left_attributes }
    { base = right_base; attribute_refinements = right_attributes }
  =
  let annotation_less_or_equal left_base right_base =
    match left_base, right_base with
    | Some left, Some right ->
        let mutability_less_or_equal =
          match left.mutability, right.mutability with
          | _, Immutable { scope = Global; _ } -> true
          | Immutable { scope = Local; _ }, Immutable { scope = Local; _ }
          | Mutable, Immutable { scope = Local; _ } ->
              true
          | Mutable, Mutable -> true
          | _ -> false
        in
        mutability_less_or_equal
        && GlobalResolution.less_or_equal
             global_resolution
             ~left:left.annotation
             ~right:right.annotation
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
    { base = left_base; attribute_refinements = left_attributes }
    { base = right_base; attribute_refinements = right_attributes }
  =
  let valid_join left_base right_base =
    match left_base, right_base with
    | Some left, Some right ->
        let mutability =
          match left.mutability, right.mutability with
          | ( Immutable ({ scope = Global; final = left_final; _ } as left),
              Immutable ({ scope = Global; final = right_final; _ } as right) ) ->
              Immutable
                {
                  scope = Global;
                  original = GlobalResolution.join global_resolution left.original right.original;
                  final = left_final || right_final;
                }
          | (Immutable { scope = Global; _ } as immutable), _
          | _, (Immutable { scope = Global; _ } as immutable) ->
              immutable
          | ( Immutable ({ scope = Local; final = left_final; _ } as left),
              Immutable ({ scope = Local; final = right_final; _ } as right) ) ->
              Immutable
                {
                  scope = Local;
                  original = GlobalResolution.join global_resolution left.original right.original;
                  final = left_final || right_final;
                }
          | (Immutable { scope = Local; _ } as immutable), _
          | _, (Immutable { scope = Local; _ } as immutable) ->
              immutable
          | _ -> Mutable
        in
        let left, right = left.annotation, right.annotation in
        let base =
          { annotation = GlobalResolution.join global_resolution left right; mutability }
        in
        ( GlobalResolution.less_or_equal global_resolution ~left ~right
          || GlobalResolution.less_or_equal global_resolution ~left:right ~right:left,
          Some base )
    | None, None ->
        (* you only want to continue the nested join should both attribute trees exist *)
        not (Map.Tree.is_empty left_attributes || Map.Tree.is_empty right_attributes), None
    | _ -> false, None
  in
  let rec create_refinement_unit (valid, base) ~left_attributes ~right_attributes =
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
  and join left_attributes right_attributes =
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
  valid_join left_base right_base |> create_refinement_unit ~left_attributes ~right_attributes


let meet ~global_resolution { base = left; _ } { base = right; _ } =
  (* TODO: Add handling for meeting attributes *)
  match left, right with
  | Some left, Some right ->
      let mutability =
        match left.mutability, right.mutability with
        | Mutable, _
        | _, Mutable ->
            Mutable
        | ( Immutable ({ scope = Local; final = left_final; _ } as left),
            Immutable ({ scope = Local; final = right_final; _ } as right) ) ->
            Immutable
              {
                scope = Local;
                original = GlobalResolution.meet global_resolution left.original right.original;
                final = left_final || right_final;
              }
        | (Immutable { scope = Local; _ } as immutable), _
        | _, (Immutable { scope = Local; _ } as immutable) ->
            immutable
        | ( Immutable ({ scope = Global; final = left_final; _ } as left),
            Immutable ({ scope = Global; final = right_final; _ } as right) ) ->
            Immutable
              {
                scope = Global;
                original = GlobalResolution.meet global_resolution left.original right.original;
                final = left_final || right_final;
              }
      in
      create
        ~base:
          {
            annotation = GlobalResolution.meet global_resolution left.annotation right.annotation;
            mutability;
          }
        ()
  | Some left, _ -> create ~base:left ()
  | _, Some right -> create ~base:right ()
  | _ -> create ~base:(Annotation.create Type.Top) ()


let widen ~global_resolution ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    create ~base:{ annotation = Type.Top; mutability = Mutable } ()
  else
    join ~global_resolution previous next
