(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module Annotations = struct
  type t = {
    precondition: RefinementUnit.t Reference.Map.Tree.t;
    postcondition: RefinementUnit.t Reference.Map.Tree.t;
  }
  [@@deriving eq]

  let pp_state formatter map =
    let pp_element ~key ~data =
      Format.fprintf formatter "\"%a\": \"%a\", " Reference.pp key RefinementUnit.pp data
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

(* Maps a key, unique to each statement for a function CFG, to type annotations. The statement key
   is computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)
type t = Annotations.t Int.Table.t

let equal left right = Hashtbl.equal left right Annotations.equal

let empty () = Int.Table.create ()

let pp formatter statements =
  let pp_annotations formatter iterator pp_key map =
    Format.fprintf formatter "{ ";
    let pp_annotation_map ~key ~data =
      Format.fprintf formatter "%a: %a" pp_key key Annotations.pp data
    in
    iterator map ~f:pp_annotation_map;
    Format.fprintf formatter " }"
  in
  pp_annotations formatter Int.Table.iteri Int.pp statements


let show map = Format.asprintf "%a" pp map

let set
    ?(precondition = Reference.Map.empty)
    ?(postcondition = Reference.Map.empty)
    ~key
    local_annotations
  =
  Hashtbl.set
    local_annotations
    ~key
    ~data:
      {
        Annotations.precondition = Reference.Map.to_tree precondition;
        postcondition = Reference.Map.to_tree postcondition;
      }


module ReadOnly = struct
  type t = Annotations.t Int.Map.Tree.t

  let get_precondition local_annotations key =
    Int.Map.Tree.find local_annotations key
    >>| fun { Annotations.precondition; _ } -> Reference.Map.of_tree precondition


  let get_postcondition local_annotations key =
    Int.Map.Tree.find local_annotations key
    >>| fun { Annotations.postcondition; _ } -> Reference.Map.of_tree postcondition
end

let read_only local_annotations = Hashtbl.to_alist local_annotations |> Int.Map.Tree.of_alist_exn
