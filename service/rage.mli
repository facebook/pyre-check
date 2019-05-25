(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val get_logs
  :  Configuration.Analysis.t ->
  LanguageServer.Types.RageResponse.RageResult.rageItem list
