(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

type t = {
  socket_path: Path.t;
  server_configuration: ServerConfiguration.t;
  configuration: Configuration.Analysis.t;
  type_environment: Analysis.TypeEnvironment.t;
  error_table: Analysis.AnalysisError.t list Ast.Reference.Table.t;
}
