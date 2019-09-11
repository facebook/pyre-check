(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module Annotations = struct
  type t = {
    precondition: Annotation.t Reference.Map.Tree.t;
    postcondition: Annotation.t Reference.Map.Tree.t;
  }
  [@@deriving eq]
  (** Maps a key, unique to each statement for a function CFG, to type annotations. They key is
      computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)

  let pp_state formatter map =
    let pp_element ~key ~data =
      Format.fprintf formatter "\"%a\": \"%a\", " Reference.pp key Annotation.pp data
    in
    Format.fprintf formatter "{";
    Reference.Map.Tree.iteri map ~f:pp_element;
    Format.fprintf formatter "}"


  let pp formatter { precondition; postcondition } =
    Format.fprintf
      formatter
      "{ \"Precondition\": %a, \"Postcondition\": %a}"
      pp_state
      precondition
      pp_state
      postcondition
end

type t = Annotations.t Int.Map.Tree.t [@@deriving eq]

let empty = Int.Map.Tree.empty

let pp formatter map =
  Format.fprintf formatter "{ ";
  let pp_annotation_map ~key ~data = Format.fprintf formatter "%d: %a" key Annotations.pp data in
  Int.Map.Tree.iteri map ~f:pp_annotation_map;
  Format.fprintf formatter " }"


let show map = Format.asprintf "%a" pp map

let set ?(precondition = Reference.Map.empty) ?(postcondition = Reference.Map.empty) ~key map =
  Int.Map.Tree.set
    map
    ~key
    ~data:
      {
        Annotations.precondition = Reference.Map.to_tree precondition;
        postcondition = Reference.Map.to_tree postcondition;
      }


let merge left right =
  let join ~key:_ = function
    | `Left next_resolution
    | `Right next_resolution
    | `Both (_, next_resolution) ->
        Some next_resolution
  in
  Int.Map.Tree.merge ~f:join left right


let get_precondition map key =
  Int.Map.Tree.find map key
  >>| fun { Annotations.precondition; _ } -> Reference.Map.of_tree precondition


let get_postcondition map key =
  Int.Map.Tree.find map key
  >>| fun { Annotations.postcondition; _ } -> Reference.Map.of_tree postcondition
