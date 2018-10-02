(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Statement


val run
  :  environment: (module Environment.Handler)
  -> define: Define.t Node.t
  -> TaintResult.Forward.model * TaintResult.result
