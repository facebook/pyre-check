(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type result = {
  module_tracker: Analysis.ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: Analysis.TypeEnvironment.t;
  errors: Analysis.Error.t list;
}

val infer : configuration:Configuration.Analysis.t -> unit -> result
