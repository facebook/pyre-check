(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Callable = Interprocedural.Callable
open Core
open Pyre

(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register (struct
  include TaintResult

  let init ~configuration ~environment ~functions:_ =
    (* Parse models *)
    let create_models ~configuration sources =
      let global_resolution = Analysis.Environment.resolution environment () in
      List.fold sources ~init:Callable.Map.empty ~f:(fun models (path, source) ->
          Model.parse
            ~resolution:(Analysis.TypeCheck.resolution global_resolution ())
            ~path
            ~source
            ~configuration
            models)
    in
    let taint = Yojson.Safe.Util.member "taint" configuration in
    let model_directories =
      Yojson.Safe.Util.member "model_directories" taint
      |> Yojson.Safe.Util.to_list
      |> List.map ~f:Yojson.Safe.Util.to_string
    in
    match model_directories with
    | [] -> Callable.Map.empty
    | _ -> (
      try
        let directories = List.map model_directories ~f:Path.create_absolute in
        List.iter directories ~f:(fun directory ->
            if not (Path.is_directory directory) then
              raise
                (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory)));
        let configuration = Configuration.create ~directories in
        Configuration.register configuration;
        let path_and_content file =
          match File.content file with
          | Some content -> Some (File.path file, content)
          | None -> None
        in
        Log.info
          "Finding taint models in `%s`."
          (directories |> List.map ~f:Path.show |> String.concat ~sep:", ");
        directories
        |> List.concat_map ~f:(fun root ->
               Path.list ~file_filter:(String.is_suffix ~suffix:".pysa") ~root ())
        |> List.map ~f:File.create
        |> List.filter_map ~f:path_and_content
        |> create_models ~configuration
      with
      | exn ->
          Log.error "Error getting taint models: %s" (Exn.to_string exn);
          raise exn )


  let analyze ~callable:_ ~environment ~define ~mode existing_model =
    let forward, result = ForwardAnalysis.run ~environment ~define ~existing_model in
    let backward = BackwardAnalysis.run ~environment ~define ~existing_model in
    let model =
      if mode = Normal then
        { forward; backward; mode }
      else
        { empty_model with mode }
    in
    result, model


  let analyze ~callable ~environment ~define ~existing =
    match existing with
    | Some ({ mode = SkipAnalysis; _ } as model) ->
        let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
        [], model
    | Some ({ mode; _ } as model) -> analyze ~callable ~environment ~define ~mode model
    | None -> analyze ~callable ~environment ~define ~mode:Normal empty_model
end)
