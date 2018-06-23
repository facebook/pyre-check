(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Make(Key: Map.Key)(Element : AbstractDomain.S) = struct
  module ElementMap = Map.Make(Key)
  include (ElementMap : module type of ElementMap with type 'a t := 'a ElementMap.t)
  type t = Element.t ElementMap.t

  let bottom = ElementMap.empty
  let is_bottom d =
    ElementMap.is_empty d ||
    for_all d ~f:Element.is_bottom

  let join x y =
    let merge ~key:_ = function
      | `Both (a, b) -> Some (Element.join a b)
      | `Left e | `Right e -> Some e
    in
    ElementMap.merge ~f:merge x y

  let widen ~iteration ~previous ~next =
    let merge ~key:_ = function
      | `Both (previous, next) -> Some (Element.widen ~iteration ~previous ~next)
      | `Left e | `Right e -> Some e
    in
    ElementMap.merge ~f:merge previous next

  let less_or_equal ~left ~right =
    let find_witness ~key:_ ~data =
      match data with
      | `Both (left, right) ->
          if not (Element.less_or_equal ~left ~right) then raise Exit
      | `Left left ->
          if not (Element.is_bottom left) then
            raise Exit  (* key not in b *)
      | `Right _ -> ()
    in
    try
      ElementMap.iter2 ~f:find_witness left right;
      true
    with
    | Exit -> false

  let show a =
    let sexp_of_element e =
      Element.show e |> String.sexp_of_t
    in
    ElementMap.sexp_of_t sexp_of_element a |> Sexp.to_string
end
