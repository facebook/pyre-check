(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement


type 'entry approximate_entry = {
  (* Each prefix is stored with the order of its elements reversed to
     reduce the lookup complexity. *)
  reversed_prefix: Access.t;
  entry: 'entry;
}


type 'entry complex_entry =
  | Precise of 'entry
  (* This lists all approximate entries living at this location, in
     decreasing order of prefix length. *)
  | Approximate of 'entry approximate_entry list


type annotation_lookup = (Type.t complex_entry) Location.Reference.Table.t


type definition_lookup = (Location.Reference.t complex_entry) Location.Reference.Table.t


type t = {
  annotations_lookup: annotation_lookup;
  definitions_lookup: definition_lookup;
}


(** The result state of this visitor is ignored. We need two read-only
    pieces of information to build the location table: the types resolved for
    this statement, and a reference to the (mutable) location table to
    update. *)
module ExpressionVisitor = struct

  type t = {
    resolution: Resolution.t;
    annotations_lookup: annotation_lookup;
    definitions_lookup: definition_lookup;
  }

  let resolve ~resolution ~expression =
    try
      let annotation = Annotated.resolve ~resolution expression in
      if (Type.is_unknown annotation) or (Type.is_unbound annotation) then
        None
      else
        Some annotation
    with TypeOrder.Untracked _ ->
      None

  let store_lookup ~table ~location ~data =
    if not (Location.equal location Location.Reference.any) then
      Hashtbl.set
        table
        ~key:location
        ~data

  let expression
      ({ resolution; annotations_lookup; definitions_lookup } as state)
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
                  let store_annotation annotation =
                    store_lookup
                      ~table:annotations_lookup
                      ~location
                      ~data:(Precise annotation)
                  in
                  resolve ~resolution ~expression:value
                  >>| store_annotation
                  |> ignore
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
          (* `filter` receives the prefix and current element, should return `Some entry` if one
             should be added for the current prefix+entry, `None` otherwise. *)
          let collect_and_store ~access ~lookup_table ~filter =
            let _, entries =
              let fold_callback (prefix, entries_sofar) element =
                let access = prefix @ [element] in
                let entries_sofar =
                  let add_entry entry =
                    { reversed_prefix = List.rev access; entry } :: entries_sofar
                  in
                  filter ~prefix ~element
                  >>| add_entry
                  |> Option.value ~default:entries_sofar
                in
                access, entries_sofar
              in
              List.fold access ~init:([], []) ~f:fold_callback
            in
            if not (List.is_empty entries) then
              store_lookup
                ~table:lookup_table
                ~location:expression_location
                ~data:(Approximate entries);
          in

          (* Definitions. *)
          let filter_definition ~prefix ~element =
            let find_definition access =
              Resolution.global resolution access
              >>| Node.location
              >>= fun location ->
              if Location.equal location Location.Reference.any then
                None
              else
                Some location
            in

            match find_definition (prefix @ [element]) with
            | Some definition ->
                Some definition
            | None ->
                (* Try and resolve the type of the prefix separately,
                   to see if this is a method in an access chain. *)
                resolve
                  ~resolution
                  ~expression:(Access.expression prefix)
                >>| Type.class_name
                >>| (fun resolved_prefix -> resolved_prefix @ [element])
                >>= find_definition
          in
          collect_and_store ~access ~lookup_table:definitions_lookup ~filter:filter_definition;

          (* Annotations. *)
          let filter_annotation ~prefix ~element =
            let access = prefix @ [element] in
            resolve
              ~resolution
              ~expression:(Node.create ~location:expression_location (Expression.Access access))
          in
          collect_and_store ~access ~lookup_table:annotations_lookup ~filter:filter_annotation

      | _ ->
          let store_annotation annotation =
            store_lookup
              ~table:annotations_lookup
              ~location:expression_location
              ~data:(Precise annotation)
          in
          resolve ~resolution ~expression
          >>| store_annotation
          |> ignore
    in
    state

  let statement
      ({ resolution; annotations_lookup; _ } as state)
      { Node.value = statement_value; _ } =
    match statement_value with
    | Define { parameters; _ } ->
        let extract_parameters { Node.value = { Parameter.annotation; _ }; location } =
          let store_parameter_annotation annotation =
            store_lookup
              ~table:annotations_lookup
              ~location
              ~data:(Precise annotation)
          in
          annotation
          >>= (fun expression -> resolve ~resolution ~expression)
          >>| store_parameter_annotation
          |> ignore
        in
        List.iter ~f:extract_parameters parameters;
        state
    | _ ->
        state
end


module Visit = Visit.Make(ExpressionVisitor)


let create_of_source environment source =
  let annotations_lookup = Location.Reference.Table.create () in
  let definitions_lookup = Location.Reference.Table.create () in
  let walk_defines { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let cfg = Cfg.create define in
    let annotation_lookup =
      ResolutionSharedMemory.get caller
      >>| Int.Map.of_tree
      |> Option.value ~default:Int.Map.empty
    in
    let walk_cfg ~key:node_id ~data:cfg_node =
      let statements = Cfg.Node.statements cfg_node in
      let walk_statements statement_index statement =
        let annotations =
          Map.find annotation_lookup ([%hash: int * int] (node_id, statement_index))
          >>| (fun { ResolutionSharedMemory.precondition; _ } ->
              Access.Map.of_tree precondition)
          |> Option.value ~default:Access.Map.empty
        in
        let resolution = Environment.resolution environment ~annotations () in
        Visit.visit
          { ExpressionVisitor.resolution; annotations_lookup; definitions_lookup }
          (Source.create [statement])
        |> ignore
      in
      List.iteri statements ~f:walk_statements
    in
    Hashtbl.iteri cfg ~f:walk_cfg
  in
  (* TODO(T31738631): remove extract_into_toplevel *)
  Preprocessing.defines ~extract_into_toplevel:true source
  |> List.iter ~f:walk_defines;
  { annotations_lookup; definitions_lookup }


let refine ~position ~source ?(take_default_on_miss = true) (location, entry) =
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
        let length = String.length text in
        let clamp_to_valid_length ~column =
          Int.max 0 (Int.min (length - 1) column)
        in
        let column = clamp_to_valid_length ~column in
        let start_column =
          String.rfindi ~pos:(column - 1) text ~f:word_delimiter
          >>| (fun index -> index + 1)
          |> Option.value ~default:0
        in
        let stop_column =
          let search_column = clamp_to_valid_length ~column:(column + 1) in
          String.lfindi ~pos:search_column text ~f:word_delimiter
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

      let lines = String.split_lines source in
      List.nth lines (line - 1)
      >>| find_word_range ~column
    in
    let find_match entries ({ Location.stop = word_stop; _ }, word) =
      let match_reverse_prefix { reversed_prefix; entry } =
        (* Heuristic: find the access that ends with the word we
           found. However, since we reversed the order of each prefix,
           we look at the first element of the Access.t instead. *)
        let match_first_element first_element =
          if String.equal (Access.show_sanitized [first_element]) word then
            (* We keep the start position of the Access unmodified
               because we are matching with a prefix. *)
            Some ({ location with Location.stop = word_stop }, entry)
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
      |> fun { entry; _ } -> (location, entry)
    in

    let refined =
      find_word_at_position ~position
      >>= find_match entries
    in
    match refined with
    | Some _ ->
        refined
    | None ->
        if take_default_on_miss then
          Some (take_longest_access entries)
        else
          None
  in
  match entry with
  | Precise entry ->
      Some (location, entry)
  | Approximate entries ->
      refine_approximate location entries


let get_best_location lookup_table ~position =
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
  let weight
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      } =
    (stop_line - start_line) * 1000 + stop_column - start_column
  in
  Hashtbl.filter_keys lookup_table ~f:(fun key -> location_contains_position key position)
  |> Hashtbl.to_alist
  |> List.min_elt ~compare:(fun (location_left, _) (location_right, _) ->
      (weight location_left) - (weight location_right))


let get_annotation { annotations_lookup; _ } ~position ~source =
  let instantiate_location (location, annotation) =
    Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location,
    annotation
  in
  get_best_location annotations_lookup ~position
  >>= refine ~position ~source
  >>| instantiate_location


let get_definition { definitions_lookup; _ } ~position ~source =
  get_best_location definitions_lookup ~position
  >>= refine ~position ~source ~take_default_on_miss:false
  >>| snd
  >>| Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash)


let expand_approximate (location, entry) =
  match entry with
  | Precise annotation ->
      [(location, annotation)]
  | Approximate entries ->
      (* Do not bother with refining locations. This function is
         mostly used for testing. *)
      List.map entries ~f:(fun { entry; _ } -> (location, entry))


let get_all_annotations { annotations_lookup; _ } =
  let instantiate_location (location, annotation) =
    Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location,
    annotation
  in
  Hashtbl.to_alist annotations_lookup
  |> List.concat_map ~f:expand_approximate
  |> List.map ~f:instantiate_location


let get_all_definitions { definitions_lookup; _ } =
  Hashtbl.to_alist definitions_lookup
  |> List.concat_map ~f:expand_approximate
