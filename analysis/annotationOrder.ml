(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Annotation


let refine ~resolution { annotation; mutability } refined =
  match mutability with
  | Mutable ->
      { annotation = refined; mutability }
  | Immutable { original; _ } ->
      let refine =
        not (Type.is_bottom refined) &&
        Resolution.less_or_equal resolution ~left:refined ~right:original
      in
      if refine then
        { annotation = refined; mutability }
      else
        { annotation; mutability }


let less_or_equal ~resolution left right =
  let mutability_less_or_equal =
    match left.mutability, right.mutability with
    | _, Immutable { scope = Global; _ } ->
        true
    | Immutable { scope = Local; _ }, Immutable { scope = Local; _ }
    | Mutable, Immutable { scope = Local; _ } ->
        true
    | Mutable, Mutable ->
        true
    | _ ->
        false
  in
  mutability_less_or_equal &&
  Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation


let join ~resolution left right =
  let mutability =
    match left.mutability, right.mutability with
    | Immutable ({ scope = Global; _ } as left),
      Immutable ({ scope = Global; _ } as right) ->
        Immutable {
          scope = Global;
          original = Resolution.join resolution left.original right.original;
        }

    | (Immutable { scope = Global; _ } as immutable), _
    | _, (Immutable { scope = Global; _ } as immutable) ->
        immutable

    | Immutable ({ scope = Local; _ } as left),
      Immutable ({ scope = Local; _ } as right) ->
        Immutable {
          scope = Local;
          original = Resolution.join resolution left.original right.original;
        }

    | (Immutable { scope = Local; _ } as immutable), _
    | _, (Immutable { scope = Local; _ } as immutable) ->
        immutable
    | _ ->
        Mutable
  in
  { annotation = Resolution.join resolution left.annotation right.annotation; mutability }


let meet ~resolution left right =
  let mutability =
    match left.mutability, right.mutability with
    | Mutable, _
    | _, Mutable ->
        Mutable

    | Immutable ({ scope = Local; _ } as left),
      Immutable ({ scope = Local; _ } as right) ->
        Immutable {
          scope = Local;
          original = Resolution.meet resolution left.original right.original;
        }

    | (Immutable { scope = Local; _ } as immutable), _
    | _, (Immutable { scope = Local; _ } as immutable) ->
        immutable

    | Immutable ({ scope = Global; _ } as left),
      Immutable ({ scope = Global; _ } as right) ->
        Immutable {
          scope = Global;
          original = Resolution.meet resolution left.original right.original;
        }
  in
  { annotation = Resolution.meet resolution left.annotation right.annotation; mutability }


let widen ~resolution ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    { annotation = Type.Top; mutability = Mutable }
  else
    join ~resolution previous next
