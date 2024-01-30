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
 *
 * Implementation notes:
 * - The only reason processed_source_of_qualifier is injected rather than being defined
 *   in terms of Parsing logic and raw_source_of_qualifier is simply because existing
 *   dependency tracking logic requires this in order to skip some dependencies we know
 *   don't have to be tracked.
 *)

open Core

module ModuleLookup = struct
  type t =
    | NotFound
    | Implicit
    | Explicit of Ast.ModulePath.t
end

type t = {
  controls: EnvironmentControls.t;
  look_up_qualifier: Ast.Reference.t -> ModuleLookup.t;
  raw_source_of_qualifier: Ast.Reference.t -> Parsing.ParseResult.t option;
  processed_source_of_qualifier: Ast.Reference.t -> Ast.Source.t option;
}

let create ~controls ~look_up_qualifier ~raw_source_of_qualifier ~processed_source_of_qualifier =
  { controls; look_up_qualifier; raw_source_of_qualifier; processed_source_of_qualifier }


let controls { controls; _ } = controls

let is_qualifier_tracked { look_up_qualifier; _ } qualifier =
  match look_up_qualifier qualifier with
  | ModuleLookup.NotFound -> false
  | ModuleLookup.Implicit
  | ModuleLookup.Explicit _ ->
      true


let module_path_of_qualifier { look_up_qualifier; _ } qualifier =
  match look_up_qualifier qualifier with
  | ModuleLookup.Explicit module_path -> Some module_path
  | ModuleLookup.NotFound
  | ModuleLookup.Implicit ->
      None


let relative_path_of_qualifier api qualifier =
  module_path_of_qualifier api qualifier |> Option.map ~f:Ast.ModulePath.relative


let raw_source_of_qualifier { raw_source_of_qualifier; _ } = raw_source_of_qualifier

let processed_source_of_qualifier { processed_source_of_qualifier; _ } =
  processed_source_of_qualifier
