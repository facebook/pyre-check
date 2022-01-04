(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(****************************************************************************)
(* Moduling Making buckets.
 * When we parallelize, we need to create "buckets" of tasks for the
 * workers.
 * Given a list of files, we want to split it up into buckets such that
 * every worker is busy long enough. If the bucket is too big, it hurts
 * load balancing, if it is too small, the overhead in synchronization time
 * hurts *)
(****************************************************************************)

type 'a bucket =
  | Job of 'a
  | Wait
  | Done

type 'a next =
  unit -> 'a bucket

let make_ bucket_size jobs =
  let i = ref 0 in
  fun () ->
    let bucket_size = min (Array.length jobs - !i) bucket_size in
    let result = Array.sub jobs !i bucket_size in
    i := bucket_size + !i;
    Array.to_list result

let make_list ~num_workers jobs =
  let jobs = Array.of_list jobs in
  let bucket_size =
     max 1 (1 + ((Array.length jobs) / num_workers))
  in
  make_ bucket_size jobs

let of_list = function
  | [] -> Done
  | wl -> Job wl

let make ~num_workers jobs =
  let maker = make_list ~num_workers jobs in
  fun () -> of_list (maker ())

type 'a of_n = { work: 'a; bucket: int; total: int }

let make_n_buckets ~buckets ~split =
  let next_bucket = ref 0 in
  fun () ->
    let current = !next_bucket in
    incr next_bucket;
    if (current < buckets) then
      Job { work = split ~bucket:current; bucket = current; total = buckets }
    else
      Done
