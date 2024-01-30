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
 * - Pyre only defines `ModulePath` for "explicit" qualifiers, i.e. Python modules that
 *   have an implementation file. As a result, `is_qualifier_tracked`, which should also
 *   support "implicit" qualifiers (i.e. namespace packages with no __init__ file), is
 *   required; it cannot be derived from `module_path_of_qualifier`.
 *)

open Core

type t = {
  controls: EnvironmentControls.t;
  is_qualifier_tracked: Ast.Reference.t -> bool;
  module_path_of_qualifier: Ast.Reference.t -> Ast.ModulePath.t option;
  raw_source_of_qualifier: Ast.Reference.t -> Parsing.ParseResult.t option;
  processed_source_of_qualifier: Ast.Reference.t -> Ast.Source.t option;
}

let create
    ~controls
    ~is_qualifier_tracked
    ~module_path_of_qualifier
    ~raw_source_of_qualifier
    ~processed_source_of_qualifier
  =
  {
    controls;
    is_qualifier_tracked;
    module_path_of_qualifier;
    raw_source_of_qualifier;
    processed_source_of_qualifier;
  }


let controls { controls; _ } = controls

let is_qualifier_tracked { is_qualifier_tracked; _ } = is_qualifier_tracked

let module_path_of_qualifier { module_path_of_qualifier; _ } = module_path_of_qualifier

let relative_path_of_qualifier { module_path_of_qualifier; _ } qualifier =
  module_path_of_qualifier qualifier |> Option.map ~f:Ast.ModulePath.relative


let raw_source_of_qualifier { raw_source_of_qualifier; _ } = raw_source_of_qualifier

let processed_source_of_qualifier { processed_source_of_qualifier; _ } =
  processed_source_of_qualifier
