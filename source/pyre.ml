(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

exception ParserError of string

module Log = Log

let ( >>| ) = Option.( >>| )

let ( >>= ) = Option.( >>= )
