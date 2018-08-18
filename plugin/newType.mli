(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast


(* `typing.NewType` expansion according to PEP484. *)
val transform_ast: Source.t -> Source.t
