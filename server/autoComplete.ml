(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre

let remove_dot ~cursor_position:{ Location.line; column } source =
  let line_rewriter line_index original_line =
    (* Pyre line number starts at 1. *)
    let line_substring = Substring.of_string original_line in
    if
      Int.equal (line_index + 1) line
      && (not (Int.equal column 0))
      (* Make sure pos and len are within bounds of the line_substring *)
      && String.length original_line - column >= 0
      && column - 1 <= Substring.length line_substring
    then (* Remove DOT to make the source parsable. *)
      let before_dot_segment = Substring.sub ~pos:0 ~len:(column - 1) line_substring in
      let after_dot_segment =
        Substring.sub ~pos:column ~len:(String.length original_line - column) line_substring
      in
      Substring.([before_dot_segment; after_dot_segment] |> concat |> to_string)
    else
      original_line
  in
  source |> String.split_lines |> List.mapi ~f:line_rewriter |> String.concat ~sep:"\n"


let find_module_reference ~cursor_position:{ Location.line; column } source =
  let dot_column = column - 1 in
  source
  |> String.split_lines
  |> (fun lines -> List.nth lines (line - 1))
  (* Starting from right, find the first character that is not part of identifier or dot. This
     should be the start of a module reference. *)
  >>= fun line ->
  let reference_end_position = dot_column - 1 in
  String.rfindi line ~pos:reference_end_position ~f:(fun _ character ->
      character <> '.' && character <> '_' && not (Char.is_alphanum character))
  >>| ( + ) 1 (* Module start is one character after position of the non-identifier character. *)
  >>| fun module_start_column ->
  line
  |> Substring.of_string
  |> Substring.sub ~pos:module_start_column ~len:(dot_column - module_start_column)
  |> Substring.to_string
  |> Reference.create


let get_exported_imports ~ast_environment module_reference =
  AstEnvironment.ReadOnly.get_source ast_environment module_reference
  >>| Source.statements
  >>| List.concat_map ~f:(function
          | { Node.value = Statement.Import { imports; _ }; _ } ->
              List.map imports ~f:(fun import -> import.name)
          | _ -> [])
  >>| Reference.Set.of_list
  |> Option.value ~default:Reference.Set.empty


let get_completion_item ~range ~item_name ~item_type =
  (* Filter away special functions like __len__() *)
  if String.is_prefix ~prefix:"__" item_name then
    None
  else
    let type_string = Type.show_concise item_type in
    let kind =
      let open LanguageServer.Types.CompletionItems.Kind in
      match item_type with
      | Callable _ -> Function
      | _ -> Variable
    in
    let display_text, new_text =
      match item_type with
      | Type.Callable _ ->
          Format.sprintf "%s%s" item_name type_string, Format.sprintf "%s()" item_name
      | _ -> item_name, item_name
    in
    Some
      {
        LanguageServer.Types.CompletionItems.label = display_text;
        kind;
        detail = type_string;
        textEdit = { range; newText = new_text };
      }


let get_class_attributes_list
    ~resolution
    ~cursor_position:{ Location.line; column }
    class_data_list
  =
  let open LanguageServer.Types in
  let position = Position.from_pyre_position ~line ~column in
  let text_edit_range = { Range.start = position; end_ = position } in
  let filter_name_and_type
      {
        Node.value =
          { Annotated.Attribute.name = item_name; annotation = { annotation = item_type; _ }; _ };
        _;
      }
    =
    get_completion_item ~range:text_edit_range ~item_name ~item_type
  in
  let get_attributes_name_and_type { Annotated.Class.class_definition; _ } =
    let attributes = Annotated.Class.attributes class_definition ~resolution in
    attributes |> List.filter_map ~f:filter_name_and_type
  in
  class_data_list |> List.map ~f:get_attributes_name_and_type |> List.concat


let get_module_members_list ~resolution ~cursor_position:{ Location.line; column } module_reference
  =
  let open LanguageServer.Types in
  let position = Position.from_pyre_position ~line ~column in
  let text_edit_range = { Range.start = position; end_ = position } in
  let ast_environment =
    Resolution.global_resolution resolution |> GlobalResolution.ast_environment
  in
  let exported_imports = get_exported_imports ~ast_environment module_reference in
  let get_member_name_and_type member_reference =
    (* We remove members which are exported by importing some other modules. They should not show
       up in autocompletion. *)
    if Reference.Set.mem exported_imports member_reference then
      None
    else
      let fully_qualified_member_reference = Reference.combine module_reference member_reference in
      get_completion_item
        ~range:text_edit_range
        ~item_name:(Reference.last member_reference)
        ~item_type:(Resolution.resolve_reference resolution fully_qualified_member_reference)
  in
  AstEnvironment.ReadOnly.get_wildcard_exports ast_environment module_reference
  >>= fun wildcard_exports -> Some (List.filter_map wildcard_exports ~f:get_member_name_and_type)


let get_completion_items ~state ~configuration ~path ~cursor_position =
  let { State.open_documents; module_tracker; environment; _ } = state in
  match Analysis.ModuleTracker.lookup_path ~configuration module_tracker path with
  | None -> []
  | Some { SourcePath.qualifier; _ } -> (
    match Reference.Table.find open_documents qualifier with
    | None -> []
    | Some content ->
        let content = remove_dot ~cursor_position content in
        (* TODO: Eliminate the filesystem side-effect *)
        let with_dummy_file_and_state ~f ~path ~content ~state =
          (* Fake environment setup: Write a dummy file under the same directory as the target file *)
          let dummy_path =
            (* Intentionally use an illegal & unique Python module name here to make sure nothing
               else gets affected *)
            let dummy_filename =
              let uuid = Uuid_unix.create () in
              Format.sprintf "pyre-autocomplete-%s-%s" (Uuid.to_string uuid) (Path.last path)
            in
            Path.get_directory path
            |> fun root -> Path.create_relative ~root ~relative:dummy_filename
          in
          let dummy_file = File.create ~content dummy_path in
          File.write dummy_file;

          let run () =
            (* Update server state with the newly added dummy file *)
            let state, _ =
              IncrementalCheck.recheck
                ~state
                ~configuration:{ configuration with ignore_dependencies = true }
                [dummy_path]
            in
            (* Perform auto-completion *)
            f dummy_file state
          in
          let cleanup () =
            (* Fake environment cleanup: Remove the dummy file *)
            try
              Sys.remove (Path.absolute (File.path dummy_file));

              (* Trigger another incremental check to revert server state *)
              IncrementalCheck.recheck
                ~state
                ~configuration:{ configuration with ignore_dependencies = true }
                [dummy_path]
              |> ignore
            with
            (* In case the remove operation fails somehow *)
            | Sys_error _ -> ()
          in
          Exn.protect ~f:run ~finally:cleanup
        in
        let get_items file state =
          (* This is the position of the item before DOT *)
          let item_position = { cursor_position with column = cursor_position.column - 2 } in
          let global_resolution = Environment.resolution environment () in
          let resolution = TypeCheck.resolution global_resolution () in
          let global_resolution = Resolution.global_resolution resolution in
          let class_attributes_list =
            LookupCache.find_annotation
              ~state
              ~configuration
              ~path:(File.path file)
              ~position:item_position
            >>| (fun (_, class_type) ->
                  class_type
                  |> Annotated.Class.resolve_class ~resolution:global_resolution
                  |> Option.value ~default:[]
                  |> get_class_attributes_list ~resolution:global_resolution ~cursor_position)
            |> Option.value ~default:[]
          in
          if List.is_empty class_attributes_list then
            (* Find module members only if class attribute completion fails *)
            File.content file
            >>= find_module_reference ~cursor_position
            >>= get_module_members_list ~resolution ~cursor_position
            |> Option.value ~default:[]
          else
            class_attributes_list
        in
        with_dummy_file_and_state ~f:get_items ~path ~content ~state )
