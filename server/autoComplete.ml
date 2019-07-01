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


let check ~state ~configuration file =
  let state, _ =
    IncrementalCheck.recheck
      ~state
      ~configuration:{ configuration with ignore_dependencies = true }
      ~files:[file]
  in
  state


let get_completion_item_list ~resolution class_data_list =
  let filter_name_and_type
      { Node.value =
          { Annotated.Attribute.name = attribute_name;
            annotation = { annotation = attribute_type; _ };
            _
          };
        _
      }
    =
    (* Filter away special functions like __len__() *)
    if String.is_prefix ~prefix:"__" attribute_name then
      None
    else
      Some
        { LanguageServer.Types.CompletionItems.label = attribute_name;
          detail = Type.show_concise attribute_type
        }
  in
  let get_attributes_name_and_type { Annotated.Class.class_definition; _ } =
    let attributes = Annotated.Class.attributes class_definition ~resolution in
    attributes |> List.filter_map ~f:filter_name_and_type
  in
  class_data_list |> List.map ~f:get_attributes_name_and_type |> List.concat


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
  file_option
  >>= (fun file -> LookupCache.find_annotation ~state ~configuration ~file ~position:item_position)
  >>| (fun (_, class_type) ->
        let resolution = TypeCheck.resolution environment () in
        class_type
        |> Annotated.Class.resolve_class ~resolution
        |> Option.value ~default:[]
        |> get_completion_item_list ~resolution)
  |> Option.value ~default:[]
