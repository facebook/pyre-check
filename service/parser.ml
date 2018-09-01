(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser


let parse_source ~configuration ?(show_parser_errors = true) file =
  File.handle ~configuration file
  >>= fun handle ->
  File.lines file
  >>= fun lines ->
  let handle = File.Handle.show handle in
  let metadata = Source.Metadata.parse handle lines in
  try
    let statements = Parser.parse ~handle lines in
    Some (
      Source.create
        ~docstring:(Statement.extract_docstring statements)
        ~metadata
        ~handle
        ~qualifier:(Source.qualifier ~handle)
        statements)
  with
  | Parser.Error error ->
      if show_parser_errors then
        Log.log ~section:`Parser "%s" error;
      None
  | Failure error ->
      Log.error "%s" error;
      None


let parse_modules_job ~configuration ~files =
  let parse file =
    file
    |> parse_source ~configuration ~show_parser_errors:false
    >>| (fun source ->
        let add_module_from_source
            {
              Source.qualifier;
              handle;
              statements;
              metadata = { Source.Metadata.local_mode; _ };
              _;
            } =
          Module.create
            ~qualifier
            ~local_mode
            ~path:handle
            ~stub:(String.is_suffix handle ~suffix:".pyi")
            statements
          |> Ast.SharedMemory.add_module qualifier
        in
        add_module_from_source source)
    |> ignore
  in
  List.iter files ~f:parse


let parse_sources_job ~configuration ~files =
  let parse handles file =
    (file
     |> parse_source ~configuration
     >>= fun source ->
     File.handle ~configuration file
     >>| fun handle ->
     Ast.SharedMemory.add_handle_hash ~handle:(File.Handle.show handle);
     source
     |> Analysis.Preprocessing.preprocess
     |> Plugin.apply_to_ast
     |> Ast.SharedMemory.add_source handle;
     handle :: handles)
    |> Option.value ~default:handles
  in
  List.fold ~init:[] ~f:parse files


let parse_sources ~configuration ~scheduler ~files =
  let handles =
    Scheduler.iter
      scheduler
      ~configuration
      ~f:(fun files -> parse_modules_job ~configuration ~files)
      ~inputs:files;
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:[]
      ~map:(fun _ files -> parse_sources_job ~configuration ~files)
      ~reduce:(fun new_handles processed_handles -> processed_handles @ new_handles)
      ~inputs:files
      ()
  in
  let () =
    let get_qualifier file =
      File.path file
      |> Path.relative
      >>| (fun handle -> Source.qualifier ~handle)
    in
    List.filter_map files ~f:get_qualifier
    |> Ast.SharedMemory.remove_modules
  in
  handles


let log_parse_errors_count ~not_parsed ~description =
  if not_parsed > 0 then
    let hint =
      if not (Log.is_enabled `Parser) then
        " Run with `--show-parse-errors` for more details."
      else
        ""
    in
    Log.warning "Could not parse %d %s%s due to syntax errors!%s"
      not_parsed
      description
      (if not_parsed > 1 then "s" else "")
      hint


let find_stubs
    ~configuration:{ Configuration.local_root; typeshed; search_path; _ } =
  let paths =
    let stubs =
      let typeshed_directories =
        let list_subdirectories typeshed_path =
          let root = Path.absolute typeshed_path in
          if Core.Sys.is_directory root = `Yes then
            match Core.Sys.ls_dir root with
            | entries ->
                let select_directories sofar path =
                  if Core.Sys.is_directory (root ^/ path) = `Yes &&
                     path <> "tests" &&
                     not (String.is_prefix path ~prefix:".")
                  then
                    (Path.create_relative ~root:typeshed_path ~relative:path) :: sofar
                  else
                    sofar
                in
                List.fold ~init:[] ~f:select_directories entries
            | exception Sys_error _ ->
                Log.error "Could not list typeshed directory: `%s`" root;
                []
          else
            begin
              Log.info "Not a typeshed directory: `%s`" root;
              []
            end
        in
        Option.value_map ~default:[] ~f:(fun path -> list_subdirectories path) typeshed
      in
      let stubs root =
        Log.info "Finding type stubs in `%a`..." Path.pp root;
        let is_stub path =
          let is_python_2_stub path =
            String.is_substring ~substring:"/2/" path ||
            String.is_substring ~substring:"/2.7/" path
          in
          String.is_suffix path ~suffix:".pyi" && not (is_python_2_stub path)
        in
        File.list ~filter:is_stub ~root
      in
      List.map ~f:stubs (local_root :: (search_path @ typeshed_directories))
    in
    let modules =
      let modules root =
        Log.info "Finding external sources in `%a`..." Path.pp root;
        File.list ~filter:(String.is_suffix ~suffix:".py") ~root
      in
      List.map ~f:modules search_path
    in

    stubs @ modules
  in
  let _, paths =
    (* If two stub directories contain the same stub, prefer the one that
       appears earlier in the search path. *)
    let filter_interfering_stubs (qualifiers, all_paths) paths =
      let add (qualifiers, all_paths) path =
        match Path.relative path with
        | Some relative ->
            let qualifier = Ast.Source.qualifier ~handle:relative in
            if Set.mem qualifiers qualifier then
              qualifiers, all_paths
            else
              Set.add qualifiers qualifier, path :: all_paths
        | None ->
            qualifiers, all_paths
      in
      List.fold ~f:add ~init:(qualifiers, all_paths) paths
    in
    List.fold ~f:filter_interfering_stubs ~init:(Access.Set.empty, []) paths
  in
  paths


let find_sources ?(filter = fun _ -> true) { Configuration.local_root; _ } =
  let filter path = String.is_suffix ~suffix:".py" path && filter path in
  File.list ~filter ~root:local_root


type result = {
  stubs: File.Handle.t list;
  sources: File.Handle.t list;
}


let parse_all scheduler ~configuration:({ Configuration.local_root; _ } as configuration) =
  let stubs =
    let timer = Timer.start () in
    let stub_paths = find_stubs ~configuration in
    Log.info "Parsing %d stubs and external sources..." (List.length stub_paths);
    let handles =
      parse_sources ~configuration ~scheduler ~files:(List.map ~f:File.create stub_paths)
    in
    let not_parsed = (List.length stub_paths) - (List.length handles) in
    log_parse_errors_count ~not_parsed ~description:"external file";
    Statistics.performance ~name:"stubs parsed" ~timer ();
    handles
  in
  let known_stubs =
    let add_to_known_stubs sofar handle =
      match Ast.SharedMemory.get_source handle with
      | Some { Ast.Source.qualifier; handle; _ } ->
          if Set.mem sofar qualifier then
            Statistics.event ~name:"interfering stub" ~normals:["handle", handle] ();
          Set.add sofar qualifier
      | _ ->
          sofar
    in
    List.fold stubs ~init:Access.Set.empty ~f:add_to_known_stubs
  in
  let sources =
    let filter path =
      let relative =
        Path.get_relative_to_root
          ~root:local_root
          (* We want to filter based on the path of the symlink instead of the path the
             symlink points to. *)
          ~path:(Path.create_absolute ~follow_symbolic_links:false path)
      in
      match relative with
      | Some handle ->
          handle = "__init__.py" ||  (* Analyze top-level `__init__.py`. *)
          not (Set.mem known_stubs (Source.qualifier ~handle))
      | _ ->
          true
    in
    let timer = Timer.start () in
    let paths = find_sources configuration ~filter in
    Log.info "Parsing %d sources in `%a`..." (List.length paths) Path.pp local_root;
    let handles =
      parse_sources ~configuration ~scheduler ~files:(List.map ~f:File.create paths)
    in
    let not_parsed = (List.length paths) - (List.length handles) in
    log_parse_errors_count ~not_parsed ~description:"file";
    Statistics.performance ~name:"sources parsed" ~timer ();
    handles
  in
  { stubs; sources }
