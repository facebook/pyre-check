(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server

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


let assert_optional_int ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: int option]
    ~printer:(fun value -> Sexp.to_string_hum ([%sexp_of: int option] value))
    expected
    actual


let assert_int ~context ~expected actual =
  assert_equal ~ctxt:context ~cmp:Int.equal ~printer:Int.to_string expected actual


let test_lazy_exclusive_lock_basic context =
  let open Lwt.Infix in
  let lock =
    let initialized = ref false in
    let initialize () =
      if !initialized then
        assert_failure "Initialization logic is not expected to be invoked more than once";
      initialized := true;
      Lwt.return 42
    in
    ExclusiveLock.Lazy.create initialize
  in

  Lwt.async (fun () ->
      (* Use `pause` to delay the invocation to `force`, so we have a chance to inspect
         pre-initialization state. *)
      Lwt.pause () >>= fun () -> ExclusiveLock.Lazy.force lock >>= fun _ -> Lwt.return_unit);

  (* Value is not ready before initialization *)
  assert_optional_int ~context (ExclusiveLock.Lazy.unsafe_read lock) ~expected:None;

  (* Read operation would force-initialize the value *)
  ExclusiveLock.Lazy.read lock ~f:(fun x -> Lwt.return (x + 1))
  >>= fun result ->
  assert_int ~context result ~expected:43;

  (* Value is 42 after initialization *)
  assert_optional_int ~context (ExclusiveLock.Lazy.unsafe_read lock) ~expected:(Some 42);

  (* Write (x - 1) into the state, but return (x + 2) *)
  ExclusiveLock.Lazy.write lock ~f:(fun x -> Lwt.return (x - 1, x + 2))
  >>= fun result ->
  assert_int ~context result ~expected:44;

  (* Value is 41 after the write *)
  ExclusiveLock.Lazy.force lock
  >>= fun result ->
  assert_int ~context result ~expected:41;

  Lwt.return_unit


let test_lazy_exclusive_lock_no_reentry_on_initialization context =
  let open Lwt.Infix in
  let mailbox = Lwt_mvar.create_empty () in
  (* The intention here is to set things up in a way such that `reader0` and `reader1` want to force
     the value of `lock` concurrently. This is achieved by having both readers drop a message in the
     mailbox, and insisting that the initializer of the locked value can only proceed if it sees
     both messages in the mailbox.

     If everything goes well, both readers will be able to make progress and terminate eventually.
     But if the lock implementation allows the `initialize` function to be invoked more than once,
     then we are going to get a deadlock here as the additional invocations will be blocked on the
     mailbox forever. *)
  let lock =
    let initialize () =
      Lwt_mvar.take mailbox
      >>= fun first_value ->
      Lwt_mvar.take mailbox
      >>= fun second_value -> Lwt.pause () >>= fun () -> Lwt.return (first_value + second_value)
    in
    ExclusiveLock.Lazy.create initialize
  in
  let reader0 =
    Lwt_mvar.put mailbox 1
    >>= fun () -> ExclusiveLock.Lazy.read lock ~f:(fun x -> Lwt.return (x + 1))
  in
  let reader1 =
    Lwt_mvar.put mailbox 2
    >>= fun () -> ExclusiveLock.Lazy.read lock ~f:(fun x -> Lwt.return (x + 2))
  in
  Lwt.both reader0 reader1
  >>= fun (result0, result1) ->
  assert_int ~context result0 ~expected:4;
  assert_int ~context result1 ~expected:5;

  Lwt.return_unit


let () =
  "lock_test"
  >::: [
         "exclusive_lock" >:: OUnitLwt.lwt_wrapper test_exclusive_lock;
         "lazy_exclusive_lock_basic" >:: OUnitLwt.lwt_wrapper test_lazy_exclusive_lock_basic;
         "lazy_exclusive_lock_no_reentry_on_initialization"
         >:: OUnitLwt.lwt_wrapper test_lazy_exclusive_lock_no_reentry_on_initialization;
       ]
  |> Test.run
