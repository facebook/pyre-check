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

(* Maps a key, unique to each statement for a function CFG, to type annotations. The statement key
   is computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)
type t = {
  statements: Annotations.t Int.Map.Tree.t;
  expressions: Annotations.t Location.Reference.Map.Tree.t;
}
[@@deriving eq]

let empty = { statements = Int.Map.Tree.empty; expressions = Location.Reference.Map.Tree.empty }

let pp formatter { statements; expressions } =
  let pp_annotations formatter iterator pp_key map =
    Format.fprintf formatter "{ ";
    let pp_annotation_map ~key ~data =
      Format.fprintf formatter "%a: %a" pp_key key Annotations.pp data
    in
    iterator map ~f:pp_annotation_map;
    Format.fprintf formatter " }"
  in
  Format.fprintf formatter "Statements:\n";
  pp_annotations formatter Int.Map.Tree.iteri Int.pp statements;
  Format.fprintf formatter "Expressions:\n";
  pp_annotations formatter Location.Reference.Map.Tree.iteri Location.Reference.pp expressions


let show map = Format.asprintf "%a" pp map

let merge
    { statements = left_statements; expressions = left_expressions }
    { statements = right_statements; expressions = right_expressions }
  =
  let join ~key:_ = function
    | `Left next_resolution
    | `Right next_resolution
    | `Both (_, next_resolution) ->
        Some next_resolution
  in
  {
    statements = Int.Map.Tree.merge ~f:join left_statements right_statements;
    expressions = Location.Reference.Map.Tree.merge ~f:join left_expressions right_expressions;
  }


let set_statement
    ?(precondition = Reference.Map.empty)
    ?(postcondition = Reference.Map.empty)
    ~key
    { statements; expressions }
  =
  let statements =
    Int.Map.Tree.set
      statements
      ~key
      ~data:
        {
          Annotations.precondition = Reference.Map.to_tree precondition;
          postcondition = Reference.Map.to_tree postcondition;
        }
  in
  { statements; expressions }


let set_expression
    ?(precondition = Reference.Map.empty)
    ?(postcondition = Reference.Map.empty)
    ~key
    { statements; expressions }
  =
  let expressions =
    Location.Reference.Map.Tree.set
      expressions
      ~key
      ~data:
        {
          Annotations.precondition = Reference.Map.to_tree precondition;
          postcondition = Reference.Map.to_tree postcondition;
        }
  in
  { statements; expressions }


let get_statement_precondition { statements; _ } key =
  Int.Map.Tree.find statements key
  >>| fun { Annotations.precondition; _ } -> Reference.Map.of_tree precondition


let get_statement_postcondition { statements; _ } key =
  Int.Map.Tree.find statements key
  >>| fun { Annotations.postcondition; _ } -> Reference.Map.of_tree postcondition


let get_expression_precondition { expressions; _ } key =
  Location.Reference.Map.Tree.find expressions key
  >>| fun { Annotations.precondition; _ } -> Reference.Map.of_tree precondition


let get_expression_postcondition { expressions; _ } key =
  Location.Reference.Map.Tree.find expressions key
  >>| fun { Annotations.postcondition; _ } -> Reference.Map.of_tree postcondition
