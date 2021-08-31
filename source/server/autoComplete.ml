(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

let remove_dot ~cursor_position:{ Location.line; column } source =
  let line_rewriter line_index original_line =
    let original_line_length = String.length original_line in
    if
      (* Pyre line number starts at 1. *)
      Int.equal (line_index + 1) line
      && original_line_length > 0
      && column >= 1
      (* Make sure pos and len are within bounds of the line_substring *)
      && original_line_length - column >= 0
    then (* Remove DOT to make the source parsable. *)
      String.sub original_line ~pos:0 ~len:(column - 1)
      ^ String.sub original_line ~pos:column ~len:(original_line_length - column)
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
      (not (Char.equal character '.'))
      && (not (Char.equal character '_'))
      && not (Char.is_alphanum character))
  >>| ( + ) 1 (* Module start is one character after position of the non-identifier character. *)
  >>| fun module_start_column ->
  line
  |> Substring.of_string
  |> Substring.sub ~pos:module_start_column ~len:(dot_column - module_start_column)
  |> Substring.to_string
  |> Reference.create


let get_completion_item ~range ~item_name ~item_type ~global_resolution =
  (* Filter away special functions like __len__() *)
  if String.is_prefix ~prefix:"__" item_name then
    None
  else
    let type_string = Type.show_concise item_type in
    let kind, display_text, new_text, detail =
      let open LanguageServer.Types.CompletionItems.Kind in
      let handle_callable callable =
        let type_string = Type.show_concise (Callable callable) in
        ( Function,
          Format.sprintf "%s%s" item_name type_string,
          Format.sprintf "%s()" item_name,
          type_string )
      in
      match item_type with
      | Callable callable -> handle_callable callable
      | Parametric { name = "BoundMethod"; _ } ->
          GlobalResolution.attribute_from_annotation
            global_resolution
            ~parent:item_type
            ~name:"__call__"
          >>| Annotated.Attribute.annotation
          >>| Annotation.annotation
          >>= (function
                | Type.Callable callable -> Some (handle_callable callable)
                | _ -> None)
          |> Option.value ~default:(Variable, item_name, item_name, type_string)
      | _ -> Variable, item_name, item_name, type_string
    in
    Some
      {
        LanguageServer.Types.CompletionItems.label = display_text;
        kind;
        detail;
        textEdit = { range; newText = new_text };
      }


let get_class_attributes_list ~resolution ~cursor_position:{ Location.line; column } class_data_list
  =
  let open LanguageServer.Types in
  let position = Position.from_pyre_position ~line ~column in
  let text_edit_range = { Range.start = position; end_ = position } in
  let filter_name_and_type annotation =
    let item_name = Annotated.Attribute.name annotation in
    let item_type =
      GlobalResolution.instantiate_attribute ~resolution ~accessed_through_class:false annotation
      |> Annotated.Attribute.annotation
      |> Annotation.annotation
    in
    get_completion_item ~range:text_edit_range ~item_name ~item_type ~global_resolution:resolution
  in
  let get_attributes_name_and_type { Type.class_name; _ } =
    let attributes = GlobalResolution.attributes class_name ~resolution in
    attributes >>| List.filter_map ~f:filter_name_and_type
  in
  class_data_list |> List.filter_map ~f:get_attributes_name_and_type |> List.concat


let get_module_members_list ~resolution ~cursor_position:{ Location.line; column } module_reference =
  let open LanguageServer.Types in
  let position = Position.from_pyre_position ~line ~column in
  let text_edit_range = { Range.start = position; end_ = position } in
  let global_resolution = Resolution.global_resolution resolution in
  let get_member_name_and_type (member_name, member_export) =
    let fully_qualified_member_reference =
      Reference.create member_name |> Reference.combine module_reference
    in
    let item_type =
      match member_export with
      | Module.Export.(Name Class) ->
          Type.meta (Type.Primitive (Reference.show fully_qualified_member_reference))
      | Module.Export.(Name (Define _ | GlobalVariable)) -> (
          match GlobalResolution.global global_resolution fully_qualified_member_reference with
          | Some { AttributeResolution.Global.annotation; _ } -> Annotation.annotation annotation
          | _ -> Type.Any)
      | Module.Export.(Module _ | NameAlias _) ->
          (* Don't bother with these. *)
          Type.Any
    in
    get_completion_item ~range:text_edit_range ~item_name:member_name ~item_type ~global_resolution
  in
  GlobalResolution.get_module_metadata global_resolution module_reference
  >>= fun module_metadata ->
  Some (List.filter_map (Module.get_all_exports module_metadata) ~f:get_member_name_and_type)


let get_completion_items ~state ~configuration ~path ~cursor_position =
  let { State.open_documents; environment; _ } = state in
  let module_tracker = Analysis.TypeEnvironment.module_tracker environment in
  match ModuleTracker.lookup_path ~configuration module_tracker path with
  | ModuleTracker.PathLookup.NotFound
  | ModuleTracker.PathLookup.ShadowedBy _ ->
      []
  | ModuleTracker.PathLookup.Found { SourcePath.qualifier; _ } -> (
      match Reference.Table.find open_documents qualifier with
      | None -> []
      | Some content ->
          let content = remove_dot ~cursor_position content in
          (* TODO: Eliminate the filesystem side-effect *)
          let with_dummy_file_and_state ~f ~path ~content ~state =
            (* Fake environment setup: Write a dummy file under the same directory as the target
               file *)
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
              let _ =
                IncrementalCheck.recheck_with_state
                  ~state
                  ~configuration:
                    { configuration with Configuration.Analysis.incremental_style = Shallow }
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
                IncrementalCheck.recheck_with_state
                  ~state
                  ~configuration:
                    { configuration with Configuration.Analysis.incremental_style = Shallow }
                  [dummy_path]
                |> ignore
              with
              (* In case the remove operation fails somehow *)
              | Sys_error _ -> ()
            in
            Exn.protect ~f:run ~finally:cleanup
          in
          let get_items file { State.environment; _ } =
            (* This is the position of the item before DOT *)
            let item_position = { cursor_position with column = cursor_position.column - 2 } in
            let global_resolution =
              TypeEnvironment.read_only environment |> TypeEnvironment.ReadOnly.global_resolution
            in
            let resolution =
              TypeCheck.resolution
                global_resolution
                (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                (module TypeCheck.DummyContext)
            in

            let global_resolution = Resolution.global_resolution resolution in
            let class_attributes_list =
              LookupProcessor.find_annotation
                ~environment
                ~configuration
                ~path:(File.path file)
                ~position:item_position
              >>| (fun (_, class_type) ->
                    Type.resolve_class class_type
                    >>| get_class_attributes_list ~resolution:global_resolution ~cursor_position
                    |> Option.value ~default:[])
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
          with_dummy_file_and_state ~f:get_items ~path ~content ~state)
