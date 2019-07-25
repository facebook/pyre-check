(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Annotation

let refine ~resolution { annotation; mutability } refined =
  match mutability with
  | Mutable -> { annotation = refined; mutability }
  | Immutable { original; _ } ->
      let annotation =
        match refined with
        | Type.Top -> refined
        | Type.Bottom -> annotation
        | refined ->
            GlobalResolution.solve_less_or_equal
              resolution
              ~constraints:TypeConstraints.empty
              ~left:refined
              ~right:original
            |> List.filter_map ~f:(GlobalResolution.solve_constraints resolution)
            |> List.hd
            >>| (fun solution -> TypeConstraints.Solution.instantiate solution refined)
            |> Option.value ~default:annotation
      in
      let refine =
        Type.is_top refined
        || (not (Type.is_unbound refined))
           && GlobalResolution.less_or_equal resolution ~left:refined ~right:original
      in
      if refine then
        { annotation = refined; mutability }
      else
        { annotation; mutability }


let less_or_equal ~resolution left right =
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
  && GlobalResolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation


let join ~resolution left right =
  let mutability =
    match left.mutability, right.mutability with
    | Immutable ({ scope = Global; _ } as left), Immutable ({ scope = Global; _ } as right) ->
        Immutable
          {
            scope = Global;
            original = GlobalResolution.join resolution left.original right.original;
            final = false;
          }
    | (Immutable { scope = Global; _ } as immutable), _
    | _, (Immutable { scope = Global; _ } as immutable) ->
        immutable
    | Immutable ({ scope = Local; _ } as left), Immutable ({ scope = Local; _ } as right) ->
        Immutable
          {
            scope = Local;
            original = GlobalResolution.join resolution left.original right.original;
            final = false;
          }
    | (Immutable { scope = Local; _ } as immutable), _
    | _, (Immutable { scope = Local; _ } as immutable) ->
        immutable
    | _ -> Mutable
  in
  { annotation = GlobalResolution.join resolution left.annotation right.annotation; mutability }


let meet ~resolution left right =
  let mutability =
    match left.mutability, right.mutability with
    | Mutable, _
    | _, Mutable ->
        Mutable
    | Immutable ({ scope = Local; _ } as left), Immutable ({ scope = Local; _ } as right) ->
        Immutable
          {
            scope = Local;
            original = GlobalResolution.meet resolution left.original right.original;
            final = false;
          }
    | (Immutable { scope = Local; _ } as immutable), _
    | _, (Immutable { scope = Local; _ } as immutable) ->
        immutable
    | Immutable ({ scope = Global; _ } as left), Immutable ({ scope = Global; _ } as right) ->
        Immutable
          {
            scope = Global;
            original = GlobalResolution.meet resolution left.original right.original;
            final = false;
          }
  in
  { annotation = GlobalResolution.meet resolution left.annotation right.annotation; mutability }


let widen ~resolution ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    { annotation = Type.Top; mutability = Mutable }
  else
    join ~resolution previous next
