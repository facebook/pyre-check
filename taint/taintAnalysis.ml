(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Callable = Interprocedural.Callable

open Core
open Pyre


(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register(struct
    include TaintResult

    let init ~configuration ~environment ~functions:_ =
      (* Parse models *)
      let create_models ~configuration sources =
        List.fold
          sources
          ~init:Callable.Map.empty
          ~f:(fun models source ->
              Model.parse
                ~resolution:(Analysis.TypeCheck.resolution environment ())
                ~source
                ~configuration
                models)
      in
      let taint = Yojson.Safe.Util.member "taint" configuration in
      let model_directory =
        Yojson.Safe.Util.member "model_directory" taint
        |> Yojson.Safe.Util.to_string
      in
      match model_directory with
      | "" ->
          Callable.Map.empty
      | directory ->
          try
            let directory = Path.create_absolute directory in
            if not (Path.is_directory directory) then
              raise
                (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory));
            let configuration =
              let configuration_file = Path.append directory ~element:"taint.config" in
              if Path.file_exists configuration_file then
                try
                  let configuration =
                    File.create configuration_file |> File.content |> Option.value ~default:""
                  in
                  Configuration.parse configuration
                with exn ->
                  Log.error
                    "Error reading taint configuration from %s: %s"
                    (Path.show configuration_file)
                    (Exn.to_string exn);
                  raise exn
              else
                Configuration.default
            in
            Configuration.register configuration;
            Log.info "Finding taint models in %a" Path.pp directory;
            Path.list ~file_filter:(String.is_suffix ~suffix:".pysa") ~root:directory ()
            |> List.map ~f:File.create
            |> List.filter_map ~f:File.content
            |> create_models ~configuration
          with exn ->
            Log.error
              "Error getting taint models: %s" (Exn.to_string exn);
            raise exn

    let analyze ~callable:_ ~environment ~define ~mode existing_model =
      let forward, result = ForwardAnalysis.run ~environment ~define ~existing_model in
      let backward = BackwardAnalysis.run ~environment ~define ~existing_model in
      let model =
        if mode = Normal then
          { forward; backward; mode; }
        else
          { empty_model with mode }
      in
      result, model

    let analyze ~callable ~environment ~define ~existing =
      match existing with
      | Some ({ mode = SkipAnalysis; _ } as model) ->
          let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
          [], model
      | Some ({ mode; _ } as model) ->
          analyze ~callable ~environment ~define ~mode model
      | None ->
          analyze ~callable ~environment ~define ~mode:Normal empty_model

  end)
