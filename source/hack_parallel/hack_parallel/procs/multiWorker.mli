(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The protocol for a next function is to return a list of elements.
 * It will be called repeatedly until it returns an empty list.
*)
type 'a nextlist = 'a list Hack_bucket.next

val next :
  Worker.t list option ->
  'a list ->
  'a list Hack_bucket.next

(* See definition in Hack_bucket *)
type 'a bucket = 'a Hack_bucket.bucket =
  | Job of 'a
  | Wait
  | Done

val call :
  Worker.t list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) -> neutral:'c ->
  next:'a Hack_bucket.next ->
  'c
