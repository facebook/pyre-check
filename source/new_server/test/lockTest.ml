(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Newserver

let random_shuffle_list ~random_state l =
  let rec do_shuffle = function
    | [] -> []
    | [single] -> [single]
    | list ->
        let before, after = List.partition_tf ~f:(fun _ -> Random.State.bool random_state) list in
        List.rev_append (do_shuffle before) (do_shuffle after)
  in
  do_shuffle l


let test_exclusive_lock _ =
  (* Here's the setup:
   * - We have a state consists of three integers (a, b, c).
   * - A bunch of readers and writers are created to concurrently access the state.
   * - Each writer will try to concurrently increment a, b and c by the same amount. 
   * - Each reader will try to concurrently read a, b and c.
   * - An exclusive lock will be used in such a way that a, b, and c get accessed all together.
   * - If the lock is functioning, then from the reader's perspective, all updates on the 
       writer side will appear atomic. In other words, (b - a) and (c - b) will always be kept
       constant.  
   *)
  let random_seed =
    let () = Random.self_init ~allow_in_tests:true () in
    Random.bits ()
  in
  let assert_int_equal ~expected actual =
    if not (Int.equal expected actual) then (
      (* Dump the random seed for better reproducibility. *)
      Log.dump "Random seed of the run: %d" random_seed;
      let message = Format.sprintf "Expected %d but got %d" expected actual in
      assert_failure message)
  in
  let open Lwt.Infix in
  let lock = ExclusiveLock.create (ref 0, ref 1, ref 2) in
  let create_writer () =
    let update x =
      incr x;
      Lwt.return_unit
    in
    ExclusiveLock.write lock ~f:(fun (a, b, c) ->
        Lwt.join [update a; update b; update c]
        >>= fun () ->
        let new_state = a, b, c in
        Lwt.return (new_state, ()))
  in
  let create_reader () =
    let read x = Lwt.return !x in
    ExclusiveLock.read lock ~f:(fun (a, b, c) ->
        Lwt.both (read a) (Lwt.both (read b) (read c))
        >>= fun (a, (b, c)) ->
        assert_int_equal (b - a) ~expected:1;
        assert_int_equal (c - b) ~expected:1;
        Lwt.return_unit)
  in
  List.append
    (List.init 100 ~f:(fun _ -> create_reader ()))
    (List.init 100 ~f:(fun _ -> create_writer ()))
  |> (* Randomly shuffle the list so we could get potentially different thread interleaving each
        time the test is run. *)
  random_shuffle_list ~random_state:(Random.State.make [| random_seed |])
  |> Lwt.join


let () =
  "lock_test" >::: ["exclusive_lock" >:: OUnitLwt.lwt_wrapper test_exclusive_lock] |> Test.run
