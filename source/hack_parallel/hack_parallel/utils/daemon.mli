(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type handle = {
  pid: int;
  infd: Unix.file_descr;
  outfd: Unix.file_descr;
}

val fork : ('a -> Unix.file_descr -> Unix.file_descr -> unit) -> 'a -> handle

val kill_and_wait : handle -> unit
