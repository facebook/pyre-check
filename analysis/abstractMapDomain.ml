(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Make(Key: Map.Key)(Element : AbstractDomain.S) = struct
  module Map = struct
    module ElementMap = Map.Make(Key)
    include ElementMap.Tree
  end

  include (Map : module type of Map with type 'a t := 'a Map.t)
  type t = Element.t Map.t
  [@@deriving sexp]

  let bottom = Map.empty
  let is_bottom d =
    Map.is_empty d ||
    for_all d ~f:Element.is_bottom

  let join x y =
    let merge ~key:_ = function
      | `Both (a, b) -> Some (Element.join a b)
      | `Left e | `Right e -> Some e
    in
    Map.merge ~f:merge x y

  let widen ~iteration ~previous ~next =
    let merge ~key:_ = function
      | `Both (previous, next) -> Some (Element.widen ~iteration ~previous ~next)
      | `Left e | `Right e -> Some e
    in
    Map.merge ~f:merge previous next

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
      Map.iter2 ~f:find_witness left right;
      true
    with
    | Exit -> false

  let show map =
    Sexp.to_string [%message (map: Element.t Map.t)]
end
