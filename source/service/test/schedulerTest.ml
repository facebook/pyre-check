(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module Policy = Scheduler.Policy

let test_policy _ =
  let assert_divide ~number_of_workers ~number_of_tasks ~expected policy =
    let actual = Policy.divide_work policy ~number_of_workers ~number_of_tasks in
    assert_equal ~cmp:Int.equal ~printer:Int.to_string expected actual
  in

  (* Test fixed bucket size policy *)

  (* Preferred chunk size can be satisfied given minimum chunk count *)
  assert_divide
    (Policy.fixed_chunk_size ~minimum_chunks_per_worker:1 ~preferred_chunk_size:10 ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:11;
  (* Preferred chunk size cannot be satisfied given minimum chunk count *)
  assert_divide
    (Policy.fixed_chunk_size ~minimum_chunks_per_worker:3 ~preferred_chunk_size:10 ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:1;
  (* Preferred chunk size cannot be satisfied given minimum chunk count, but a fallback is
     specified. *)
  assert_divide
    (Policy.fixed_chunk_size
       ~minimum_chunks_per_worker:3
       ~preferred_chunk_size:10
       ~minimum_chunk_size:5
       ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:15;
  (* Always try to satisfy preferred chunk size *)
  assert_divide
    (Policy.fixed_chunk_size ~minimum_chunks_per_worker:0 ~preferred_chunk_size:10 ())
    ~number_of_workers:5
    ~number_of_tasks:9
    ~expected:1;

  (* Test fixed bucket count policy *)
  (* Preferred chunk count can be satisfied given minimum chunk size *)
  assert_divide
    (Policy.fixed_chunk_count ~minimum_chunk_size:5 ~preferred_chunks_per_worker:2 ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:10;
  (* Preferred chunk count cannot be satisfied given minimum chunk size *)
  assert_divide
    (Policy.fixed_chunk_count ~minimum_chunk_size:5 ~preferred_chunks_per_worker:5 ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:1;
  (* Preferred chunk size cannot be satisfied given minimum chunk count, but a fallback is
     specified. *)
  assert_divide
    (Policy.fixed_chunk_count
       ~minimum_chunk_size:5
       ~preferred_chunks_per_worker:5
       ~minimum_chunks_per_worker:2
       ())
    ~number_of_workers:5
    ~number_of_tasks:101
    ~expected:21;
  (* Always try to satisfy preferred chunk count *)
  assert_divide
    (Policy.fixed_chunk_count ~minimum_chunk_size:0 ~preferred_chunks_per_worker:5 ())
    ~number_of_workers:5
    ~number_of_tasks:24
    ~expected:25;
  ()


let () = "scheduler" >::: ["policy" >:: test_policy] |> Test.run
