(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open Analysis

module ServerInternal = struct
  type t = {
    properties: Server.ServerProperties.t;
    state: State.t Server.ExclusiveLock.t;
  }
end

let default_lookup_artifact source_path =
  (* TODO: Add support for Buck path translation. *)
  [SourcePath.raw source_path |> ArtifactPath.create]


let default_lookup_source artifact_path =
  (* TODO: Add support for Buck path translation. *)
  Some (ArtifactPath.raw artifact_path |> SourcePath.create)


let get_overlay ~environment overlay_id =
  match overlay_id with
  | None -> Result.Ok (OverlaidEnvironment.root environment)
  | Some overlay_id -> (
      match OverlaidEnvironment.overlay environment overlay_id with
      | Some overlay -> Result.Ok overlay
      | None -> Result.Error (Response.ErrorKind.OverlayNotFound { overlay_id }))


let get_modules ~module_tracker module_ =
  let modules =
    match module_ with
    | Request.Module.OfName name ->
        let module_name = Ast.Reference.create name in
        if ModuleTracker.ReadOnly.is_module_tracked module_tracker module_name then
          [module_name]
        else
          []
    | Request.Module.OfPath path ->
        let source_path = PyrePath.create_absolute path |> SourcePath.create in
        Server.PathLookup.modules_of_source_path
          source_path
          ~module_tracker
          ~lookup_artifact:default_lookup_artifact
  in
  match modules with
  | [] -> Result.Error (Response.ErrorKind.ModuleNotTracked { module_ })
  | _ -> Result.Ok modules


let get_type_errors_in_overlay ~overlay module_ =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker module_
  >>| fun modules ->
  ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers overlay modules
  |> List.sort ~compare:AnalysisError.compare
  |> List.map
       ~f:
         (Server.RequestHandler.instantiate_error
            ~lookup_source:default_lookup_source
            ~show_error_traces:false
            ~module_tracker)


let handle_get_type_errors ~module_ ~overlay_id { State.environment } =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_type_errors_in_overlay ~overlay module_ >>| fun type_errors -> Response.TypeErrors type_errors


let get_hover_content_for_module ~overlay ~position module_reference =
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  LocationBasedLookup.hover_info_for_position ~type_environment ~module_reference position
  |> Option.map ~f:(fun value -> Response.HoverContent.{ kind = Kind.PlainText; value })


let get_hover_in_overlay ~overlay ~position module_ =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker module_
  >>| List.filter_map ~f:(get_hover_content_for_module ~overlay ~position)


let handle_hover ~module_ ~position ~overlay_id { State.environment } =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_hover_in_overlay ~overlay ~position module_ >>| fun contents -> Response.(Hover { contents })


let get_location_of_definition_for_module ~overlay ~position module_reference =
  let open Option in
  let type_environment = ErrorsEnvironment.ReadOnly.type_environment overlay in
  let module_tracker = TypeEnvironment.ReadOnly.module_tracker type_environment in
  LocationBasedLookup.location_of_definition ~type_environment ~module_reference position
  >>= fun { Ast.Location.WithModule.module_reference; start; stop } ->
  Server.PathLookup.instantiate_path
    ~lookup_source:default_lookup_source
    ~module_tracker
    module_reference
  >>| fun path -> { Response.DefinitionLocation.path; range = { Ast.Location.start; stop } }


let get_location_of_definition_in_overlay ~overlay ~position module_ =
  let open Result in
  let module_tracker = ErrorsEnvironment.ReadOnly.module_tracker overlay in
  get_modules ~module_tracker module_
  >>| List.filter_map ~f:(get_location_of_definition_for_module ~overlay ~position)


let handle_location_of_definition ~module_ ~position ~overlay_id { State.environment } =
  let open Result in
  get_overlay ~environment overlay_id
  >>= fun overlay ->
  get_location_of_definition_in_overlay ~overlay ~position module_
  >>| fun definitions -> Response.(LocationOfDefinition definitions)


let handle_local_update ~module_ ~content ~overlay_id { State.environment } =
  let open Result in
  let module_tracker =
    OverlaidEnvironment.root environment |> ErrorsEnvironment.ReadOnly.module_tracker
  in
  get_modules ~module_tracker module_
  >>| fun modules ->
  let code_updates =
    let code_update = ModuleTracker.Overlay.CodeUpdate.NewCode content in
    let to_update module_name =
      ModuleTracker.ReadOnly.lookup_full_path module_tracker module_name
      |> Option.map ~f:(fun artifact_path -> artifact_path, code_update)
    in
    List.filter_map modules ~f:to_update
  in
  let _ = OverlaidEnvironment.update_overlay_with_code environment ~code_updates overlay_id in
  Response.Ok


let get_artifact_path_event_kind = function
  | Request.FileUpdateEvent.Kind.CreatedOrChanged -> ArtifactPath.Event.Kind.CreatedOrChanged
  | Request.FileUpdateEvent.Kind.Deleted -> ArtifactPath.Event.Kind.Deleted


let get_artifact_path_event { Request.FileUpdateEvent.kind; path } =
  let kind = get_artifact_path_event_kind kind in
  PyrePath.create_absolute path |> ArtifactPath.create |> ArtifactPath.Event.create ~kind


let handle_file_update ~events { State.environment } =
  let artifact_path_events =
    (* TODO: Add support for Buck path translation. *)
    List.map events ~f:get_artifact_path_event
  in
  let () =
    let configuration =
      OverlaidEnvironment.root environment
      |> ErrorsEnvironment.ReadOnly.controls
      |> EnvironmentControls.configuration
    in
    Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
        OverlaidEnvironment.run_update_root environment ~scheduler artifact_path_events)
  in
  Lwt.return_unit


let response_from_result = function
  | Result.Ok response -> response
  | Result.Error kind -> Response.Error kind


let handle_request ~server:{ ServerInternal.properties = _; state } = function
  | Request.Stop -> Server.Stop.stop_waiting_server ()
  | Request.GetTypeErrors { module_; overlay_id } ->
      let f state =
        handle_get_type_errors ~module_ ~overlay_id state |> response_from_result |> Lwt.return
      in
      Server.ExclusiveLock.read state ~f
  | Request.Hover { module_; position; overlay_id } ->
      let f state =
        handle_hover ~module_ ~position ~overlay_id state |> response_from_result |> Lwt.return
      in
      Server.ExclusiveLock.read state ~f
  | Request.LocationOfDefinition { module_; position; overlay_id } ->
      let f state =
        handle_location_of_definition ~module_ ~position ~overlay_id state
        |> response_from_result
        |> Lwt.return
      in
      Server.ExclusiveLock.read state ~f
  | Request.LocalUpdate { module_; content; overlay_id } ->
      let f state =
        handle_local_update ~module_ ~content ~overlay_id state
        |> response_from_result
        |> Lwt.return
      in
      Server.ExclusiveLock.read state ~f
  | Request.FileUpdate events ->
      let f state =
        let%lwt () = handle_file_update ~events state in
        Lwt.return Response.Ok
      in
      Server.ExclusiveLock.read state ~f


let handle_raw_request ~server raw_request =
  match Request.of_string raw_request with
  | Result.Error message -> Lwt.return (Response.Error (Response.ErrorKind.InvalidRequest message))
  | Result.Ok request -> handle_request ~server request
