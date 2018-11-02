(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module type ELEMENT_DOMAIN = sig
  type t
  [@@deriving show, sexp]

  val less_or_equal: left: t -> right: t -> bool
  val join: t -> t -> t

  type group
  [@@deriving compare, sexp]

  val group: t -> group
end


(* A set of abstract elements where set adding will join elements by group *)
module Make(Element : ELEMENT_DOMAIN) : sig
  include AbstractDomain.S

  val add: t -> Element.t -> t
  val elements: t -> Element.t list
  val partition_tf: t -> f: (Element.t -> bool) -> t * t
  val singleton: Element.t -> t
end
= struct
  module Key = struct
    type t = Element.group
    [@@deriving compare, sexp]
  end

  module Map = struct
    module ElementMap = Map.Make(Key)
    include ElementMap.Tree
  end

  include (Map : module type of Map with type 'a t := 'a Map.t)
  type t = Element.t Map.t

  let bottom = Map.empty

  let is_bottom = Map.is_empty

  let join x y =
    let merge ~key:_ = function
      | `Both (a, b) -> Some (Element.join a b)
      | `Left e | `Right e -> Some e
    in
    Map.merge ~f:merge x y

  let widen ~iteration:_ ~previous ~next =
    join previous next

  let less_or_equal ~left ~right =
    let find_witness ~key:_ ~data =
      match data with
      | `Both (left, right) ->
          if not (Element.less_or_equal ~left ~right) then raise Exit
      | `Left _ ->
          raise Exit
      | `Right _ -> ()
    in
    try
      Map.iter2 ~f:find_witness left right;
      true
    with
    | Exit -> false

  let sexp_of_t = Map.sexp_of_t Element.sexp_of_t

  let t_of_sexp = Map.t_of_sexp Element.t_of_sexp

  let show set =
    Map.data set
    |> List.map ~f:Element.show
    |> String.concat ~sep:", "
    |> Format.sprintf "[%s]"

  let add set element =
    let optionally_join = function
      | None -> element
      | Some existing -> Element.join existing element
    in
    let key = Element.group element in
    Map.update set key ~f:optionally_join

  let singleton element =
    add bottom element

  let elements = Map.data

  let partition_tf map ~f =
    let partition_target ~key ~data (true_result, false_result) =
      if f data then
        (Map.set true_result ~key ~data, false_result)
      else
        (true_result, Map.set false_result ~key ~data)
    in
    Map.fold map ~init:(empty, empty) ~f:partition_target
end
