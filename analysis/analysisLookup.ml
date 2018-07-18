(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Pyre

module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Preprocessing = AnalysisPreprocessing
module Type = AnalysisType
module TypeResolutionSharedMemory = AnalysisTypeResolutionSharedMemory


type approximate_entry = {
  (* Each prefix is stored with the order of its elements reversed to
     reduce the lookup complexity. *)
  reversed_prefix: Access.t;
  annotation: Type.t;
}


type entry =
  | Precise of Type.t
  (* This lists all approximate annotations living at this
     location, in decreasing order of prefix length. *)
  | Approximate of approximate_entry list


type t = entry Location.Reference.Table.t


(** The result state of this visitor is ignored. We need two read-only
    pieces of information to build the location table: the types resolved for
    this statement, and a reference to the (mutable) location table to
    update. *)
module ExpressionVisitor = struct

  type t = Environment.Resolution.t * entry Location.Reference.Table.t

  let expression
      ((resolution, lookup) as state)
      ({ Node.location = expression_location; value = expression_value} as expression) =
    let lookup_of_arguments = function
      | { Node.value = Expression.Access access; _ } ->
          let check_single_access = function
            | Access.Call { Node.value = arguments; _ }  ->
                let check_argument
                    {
                      Argument.value = {
                        Node.location = value_location; _ } as value;
                      name
                    } =
                  let location =
                    match name with
                    | Some { Node.location = { Location.start; _ }; _ } ->
                        { value_location with Location.start }
                    | None ->
                        value_location
                  in
                  try
                    let annotation = Annotated.resolve ~resolution value in
                    if not (Type.is_unknown annotation) then
                      Location.Reference.Table.set lookup ~key:location ~data:(Precise annotation)
                  with AnalysisTypeOrder.Untracked _ ->
                    (* If we cannot resolve the type of this
                       expression, ignore it silently. The
                       construction of the lookup table is not
                       critical. *)
                    ()
                in
                List.iter ~f:check_argument arguments
            | _ ->
                ()
          in
          List.iter ~f:check_single_access access
      | _ ->
          ()
    in

    (* T30816068: we need a better visitor interface that exposes Argument.name *)
    lookup_of_arguments expression;
    let () =
      match expression_value with
      | Expression.Access access ->
          (* Enumerate all prefixes of this access and resolve them separately. *)
          let register_prefix (prefix, prefixes_sofar) element =
            let access = prefix @ [element] in
            let annotation =
              Annotated.resolve
                ~resolution
                (Node.create ~location:expression_location (Expression.Access access))
            in
            let prefixes_sofar =
              if not (Type.is_unknown annotation) then
                (* Optimization: reversing the access makes lookups easier. *)
                { reversed_prefix = List.rev access; annotation } :: prefixes_sofar
              else
                prefixes_sofar
            in
            access, prefixes_sofar
          in
          let _ , prefixes =
            List.fold
              access
              ~init:([], [])
              ~f:register_prefix
          in
          if (List.length prefixes) > 0 then
            Location.Reference.Table.set
              lookup
              ~key:expression_location
              ~data:(Approximate prefixes)
      | _ ->
          try
            let annotation = Annotated.resolve ~resolution expression in
            if not (Type.is_unknown annotation) then
              Location.Reference.Table.set
                lookup
                ~key:expression_location
                ~data:(Precise annotation)
          with AnalysisTypeOrder.Untracked _ ->
            (* If we cannot resolve the type of this expression, ignore it
               silently. The construction of the lookup table is not
               critical. *)
            ()
    in
    state

  let statement state _ =
    state
end


module Visit = Visit.Make(ExpressionVisitor)


let create_of_source environment source =
  let open TypeResolutionSharedMemory in
  let location_lookup = Location.Reference.Table.create () in
  let walk_defines { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let cfg = Cfg.create define in
    let annotation_lookup =
      let fold_annotations map { key; annotations } =
        Int.Map.set map ~key ~data:annotations
      in
      TypeResolutionSharedMemory.get caller
      >>| List.fold ~init:Int.Map.empty ~f:fold_annotations
      |> Option.value ~default:Int.Map.empty
    in
    let walk_cfg ~key:node_id ~data:cfg_node =
      let statements = Cfg.Node.statements cfg_node in
      let walk_statements statement_index statement =
        let annotations =
          Int.Map.find annotation_lookup ([%hash: int * int] (node_id, statement_index))
          |> Option.value ~default:[]
          |> Access.Map.of_alist_exn
        in
        let resolution = Environment.resolution environment ~annotations () in
        Visit.visit (resolution, location_lookup) (Source.create [statement])
        |> ignore
      in
      List.iteri statements ~f:walk_statements
    in
    Int.Table.iteri cfg ~f:walk_cfg
  in
  Preprocessing.defines source
  |> List.iter ~f:walk_defines;
  location_lookup


let refine ~position ~source_text (location, entry) =
  let refine_approximate location entries =
    let find_word_at_position ~position:{ Location.line; column } =
      let word_delimiter _index character =
        (* See `identifier` in parserLexer.mll. *)
        not (Char.is_alphanum character
             || character = '$'
             || character = '?'
             || character = '_')
      in
      let find_word_range text ~column =
        let start_column =
          String.rfindi ~pos:(column - 1) text ~f:word_delimiter
          >>| (fun index -> index + 1)
          |> Option.value ~default:0
        in
        let stop_column =
          String.lfindi ~pos:(column + 1) text ~f:word_delimiter
          >>| (fun index -> index - 1)
          |> Option.value ~default:((String.length text) - 1)
        in

        let start = { position with Location.column = start_column } in
        (* The index of the `stop` field is the first character _after_ the selected word. *)
        let stop = { position with Location.column = stop_column + 1 } in
        (
          { location with Location.start; stop },
          String.sub text ~pos:start_column ~len:(stop_column - start_column + 1)
        )
      in

      let lines = String.split_lines source_text in
      List.nth lines (line - 1)
      >>| find_word_range ~column
    in
    let find_match entries ({ Location.stop = word_stop; _ }, word) =
      let match_reverse_prefix { reversed_prefix; annotation } =
        (* Heuristic: find the access that ends with the word we
           found. However, since we reversed the order of each prefix,
           we look at the first element of the Access.t instead. *)
        let match_first_element first_element =
          if String.equal (Access.show_sanitized [first_element]) word then
            (* We keep the start position of the Access unmodified
               because we are matching with a prefix. *)
            Some ({ location with Location.stop = word_stop }, annotation)
          else
            None
        in
        List.hd reversed_prefix
        >>= match_first_element
      in
      List.find_map
        entries
        ~f:match_reverse_prefix
    in
    let take_longest_access entries =
      (* No word was found, return the longest prefix in the table,
         that corresponds to the whole access. The longest prefix is
         at the front, and it is guarenteed to exist. *)
      List.hd_exn entries
      |> fun { annotation; _ } -> (location, annotation)
    in

    find_word_at_position ~position
    >>= find_match entries
    |> Option.value ~default:(take_longest_access entries)
  in
  match entry with
  | Precise annotation ->
      (location, annotation)
  | Approximate entries ->
      refine_approximate location entries


let get_annotation lookup ~position ~source_text =
  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line } =
    let start_ok = (start_line < line) || (start_line = line && start_column <= column) in
    let stop_ok = (stop_line > line) || (stop_line = line && stop_column > column) in
    start_ok && stop_ok
  in
  let get_best_location position =
    let weight
        {
          Location.start = { Location.column = start_column; line = start_line };
          stop = { Location.column = stop_column; line = stop_line };
          _;
        } =
      (stop_line - start_line) * 1000 + stop_column - start_column
    in
    Hashtbl.to_alist lookup
    |> List.filter ~f:(fun (key, _) -> location_contains_position key position)
    |> List.min_elt ~compare:(fun (location_left, _) (location_right, _) ->
        (weight location_left) - (weight location_right))
  in
  let instantiate_location (location, annotation) =
    Location.instantiate ~lookup:(fun hash -> Ast.AstSharedMemory.get_path ~hash) location,
    annotation
  in
  get_best_location position
  >>| refine ~position ~source_text
  >>| instantiate_location


let get_all_annotations lookup =
  let expand_approximate (location, entry) =
    match entry with
    | Precise annotation ->
        [(location, annotation)]
    | Approximate entries ->
        (* Do not bother with refining locations. This function is
           mostly used for testing. *)
        List.map entries ~f:(fun { annotation; _ } -> (location, annotation))
  in
  Location.Reference.Table.to_alist lookup
  |> List.concat_map ~f:expand_approximate


let get_definition _lookup _position =
  None
