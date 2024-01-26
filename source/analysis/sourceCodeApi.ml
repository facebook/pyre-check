(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* SourceCodeApi represents the bottom layer of Pyre: navigating
 * source trees and parsing code.
 *
 * The core of this module represents an abstract API and some convenience functions
 * that can be defined in terms of the primitive functions; in practice Pyre
 * needs cached tables backing this so it is expected that concrete
 * implementations (for example classic dependency-tracked shared memory
 * tables with incremental update) will provide values of this abstract API
 * for downstream use.
 *)

open Core

type t = {
  controls: EnvironmentControls.t;
  module_path_of_qualifier: Ast.Reference.t -> Ast.ModulePath.t option;
  raw_source_of_qualifier: Ast.Reference.t -> Parsing.ParseResult.t option;
}

let create ~controls ~module_path_of_qualifier ~raw_source_of_qualifier =
  { controls; module_path_of_qualifier; raw_source_of_qualifier }


let controls { controls; _ } = controls

let module_path_of_qualifier { module_path_of_qualifier; _ } = module_path_of_qualifier

let relative_path_of_qualifier { module_path_of_qualifier; _ } qualifier =
  module_path_of_qualifier qualifier |> Option.map ~f:Ast.ModulePath.relative


let is_qualifier_tracked { module_path_of_qualifier; _ } qualifier =
  module_path_of_qualifier qualifier |> Option.is_some


let raw_source_of_qualifier { raw_source_of_qualifier; _ } = raw_source_of_qualifier

let processed_source_of_qualifier api =
  AstProcessing.processed_source_of_qualifier ~raw_source_of_qualifier:(raw_source_of_qualifier api)
