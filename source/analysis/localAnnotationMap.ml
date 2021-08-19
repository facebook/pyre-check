(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

module Annotations = struct
  type annotation_store = {
    annotations: RefinementUnit.t Reference.Map.Tree.t;
    temporary_annotations: RefinementUnit.t Reference.Map.Tree.t;
  }
  [@@deriving eq]

  type t = {
    precondition: annotation_store;
    postcondition: annotation_store;
  }
  [@@deriving eq]

  let pp_state formatter { annotations; temporary_annotations } =
    let pp_element ~temporary ~key ~data =
      let temporary_suffix = if temporary then "(temp)" else "" in
      Format.fprintf
        formatter
        "\"%a\": \"%a\"%s, "
        Reference.pp
        key
        RefinementUnit.pp
        data
        temporary_suffix
    in
    Format.fprintf formatter "{";
    Reference.Map.Tree.iteri annotations ~f:(pp_element ~temporary:false);
    Reference.Map.Tree.iteri temporary_annotations ~f:(pp_element ~temporary:true);
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

let equal left right = Core.Hashtbl.equal Annotations.equal left right

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
    ?(precondition =
      { Resolution.annotations = Reference.Map.empty; temporary_annotations = Reference.Map.empty })
    ?(postcondition =
      { Resolution.annotations = Reference.Map.empty; temporary_annotations = Reference.Map.empty })
    ~key
    local_annotations
  =
  let convert_to_tree { Resolution.annotations; temporary_annotations } =
    {
      Annotations.annotations = Reference.Map.to_tree annotations;
      temporary_annotations = Reference.Map.to_tree temporary_annotations;
    }
  in
  Hashtbl.set
    local_annotations
    ~key
    ~data:
      {
        Annotations.precondition = convert_to_tree precondition;
        postcondition = convert_to_tree postcondition;
      }


module ReadOnly = struct
  type t = Annotations.t Int.Map.Tree.t

  let convert_to_map { Annotations.annotations; temporary_annotations } =
    {
      Resolution.annotations = Reference.Map.of_tree annotations;
      temporary_annotations = Reference.Map.of_tree temporary_annotations;
    }


  let get_precondition local_annotations key =
    Int.Map.Tree.find local_annotations key
    >>| fun { Annotations.precondition; _ } -> convert_to_map precondition


  let get_postcondition local_annotations key =
    Int.Map.Tree.find local_annotations key
    >>| fun { Annotations.postcondition; _ } -> convert_to_map postcondition
end

let read_only local_annotations = Hashtbl.to_alist local_annotations |> Int.Map.Tree.of_alist_exn
