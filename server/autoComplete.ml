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
    if line_index + 1 = line then (* Remove DOT to make the source parsable. *)
      let line_substring = Substring.of_string original_line in
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


let get_exported_imports ~module_reference =
  SharedMemory.Sources.get_for_qualifier module_reference
  >>| Source.statements
  >>| List.concat_map ~f:(function
          | { Node.value = Statement.Import { imports; _ }; _ } ->
              List.map imports ~f:(fun import -> import.name)
          | _ -> [])
  >>| Reference.Set.of_list
  |> Option.value ~default:Reference.Set.empty


let check ~state ~configuration file =
  let state, _ =
    IncrementalCheck.recheck
      ~state
      ~configuration:{ configuration with ignore_dependencies = true }
      ~files:[file]
  in
  state


let get_completion_item ~range ~item_name ~item_type =
  (* Filter away special functions like __len__() *)
  if String.is_prefix ~prefix:"__" item_name then
    None
  else
    let type_string = Type.show_concise item_type in
    let display_text, new_text =
      match item_type with
      | Type.Callable _ ->
          Format.sprintf "%s%s" item_name type_string, Format.sprintf "%s()" item_name
      | _ -> item_name, item_name
    in
    Some
      { LanguageServer.Types.CompletionItems.label = display_text;
        detail = type_string;
        textEdit = { range; newText = new_text }
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
      { Node.value =
          { Annotated.Attribute.name = item_name; annotation = { annotation = item_type; _ }; _ };
        _
      }
    =
    get_completion_item ~range:text_edit_range ~item_name ~item_type
  in
  let get_attributes_name_and_type { Annotated.Class.class_definition; _ } =
    let attributes = Annotated.Class.attributes class_definition ~resolution in
    attributes |> List.filter_map ~f:filter_name_and_type
  in
  class_data_list |> List.map ~f:get_attributes_name_and_type |> List.concat


let get_module_members_list
    ~resolution
    ~cursor_position:{ Location.line; column }
    ~module_reference
    module_definition
  =
  let open LanguageServer.Types in
  let position = Position.from_pyre_position ~line ~column in
  let text_edit_range = { Range.start = position; end_ = position } in
  let exported_imports = get_exported_imports ~module_reference in
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
  List.filter_map (Module.wildcard_exports module_definition) ~f:get_member_name_and_type


let get_completion_items ~state ~configuration ~path ~cursor_position =
  let { State.open_documents; environment; _ } = state in
  let file_option =
    path
    |> Path.Map.find open_documents
    >>| remove_dot ~cursor_position
    >>| fun content -> File.create ~content path
  in
  let state = file_option >>| check ~state ~configuration |> Option.value ~default:state in
  (* This is the position of the item before DOT *)
  let item_position = { cursor_position with column = cursor_position.column - 2 } in
  let resolution = TypeCheck.resolution environment () in
  let class_attributes_list =
    file_option
    >>= (fun file ->
          LookupCache.find_annotation ~state ~configuration ~file ~position:item_position)
    >>| (fun (_, class_type) ->
          class_type
          |> Annotated.Class.resolve_class ~resolution
          |> Option.value ~default:[]
          |> get_class_attributes_list ~resolution ~cursor_position)
    |> Option.value ~default:[]
  in
  if List.is_empty class_attributes_list then
    (* Find module members only if class attribute completion fails *)
    file_option
    >>= File.content
    >>= find_module_reference ~cursor_position
    >>= (fun module_reference ->
          module_reference
          |> Resolution.module_definition resolution
          >>| get_module_members_list ~resolution ~cursor_position ~module_reference)
    |> Option.value ~default:[]
  else
    class_attributes_list
