(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Pyre

module Annotations = struct
  type t = {
    precondition: TypeInfo.Store.t;
    postcondition: TypeInfo.Store.t;
  }
  [@@deriving eq]

  let pp formatter { precondition; postcondition } =
    Format.fprintf
      formatter
      "{ \"Precondition\": %a, \"Postcondition\": %a}"
      TypeInfo.Store.print_as_json
      precondition
      TypeInfo.Store.print_as_json
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
  pp_annotations formatter Hashtbl.iteri Int.pp statements


let show map = Format.asprintf "%a" pp map

let set
    ?(precondition =
      TypeInfo.Store.
        { type_info = Reference.Map.Tree.empty; temporary_type_info = Reference.Map.Tree.empty })
    ?(postcondition =
      TypeInfo.Store.
        { type_info = Reference.Map.Tree.empty; temporary_type_info = Reference.Map.Tree.empty })
    ~statement_key
    local_annotations
  =
  Hashtbl.set local_annotations ~key:statement_key ~data:{ Annotations.precondition; postcondition }


module StatementIdMap = struct
  module StatementId = struct
    type t = int [@@deriving compare, sexp, hash, to_yojson]
  end

  include Map.Make_tree (struct
    include StatementId
    include Comparator.Make (StatementId)
  end)
end

module ReadOnly = struct
  type t = Annotations.t StatementIdMap.t [@@deriving equal]

  let get_precondition local_annotations ~statement_key =
    StatementIdMap.find local_annotations statement_key
    >>| fun { Annotations.precondition; _ } -> precondition


  let get_postcondition local_annotations ~statement_key =
    StatementIdMap.find local_annotations statement_key
    >>| fun { Annotations.postcondition; _ } -> postcondition
end

let read_only local_annotations = Hashtbl.to_alist local_annotations |> StatementIdMap.of_alist_exn
