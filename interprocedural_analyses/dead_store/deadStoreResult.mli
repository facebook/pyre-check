(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

include
  Interprocedural.Result.ANALYSIS_RESULT_WITH_REGISTRATION
    with type result := string
     and type call_model := int
