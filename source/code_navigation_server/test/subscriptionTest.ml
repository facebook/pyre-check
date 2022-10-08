(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Subscriptions = CodeNavigationServer.Testing.Subscriptions

let assert_int ~context ~expected actual =
  assert_equal ~ctxt:context ~cmp:Int.equal ~printer:Int.to_string expected actual


let assert_string_option ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string option]
    ~printer:(fun input -> Sexp.to_string_hum ([%sexp_of: string option] input))
    expected
    actual


let test_subscription_registration context =
  let subscriptions = Subscriptions.create () in
  let input_channel0, output_channel0 = Lwt_io.pipe () in
  let input_channel1, output_channel1 = Lwt_io.pipe () in

  let identifier0 = Subscriptions.register subscriptions ~output_channel:output_channel0 in
  let identifier1 = Subscriptions.register subscriptions ~output_channel:output_channel1 in

  assert_bool
    "Identifiers of different subscriptions should be different"
    (not (Subscriptions.Identifier.equal identifier0 identifier1));
  assert_int ~context ~expected:2 (Subscriptions.count subscriptions);

  let%lwt () = Subscriptions.broadcast_raw ~message:(lazy "foo") subscriptions in
  let%lwt input0 = Lwt_io.read_line_opt input_channel0 in
  assert_string_option ~context ~expected:(Some "foo") input0;
  let%lwt input1 = Lwt_io.read_line_opt input_channel1 in
  assert_string_option ~context ~expected:(Some "foo") input1;

  Subscriptions.unregister subscriptions ~identifier:identifier1;
  assert_int ~context ~expected:1 (Subscriptions.count subscriptions);

  let%lwt () = Subscriptions.broadcast_raw ~message:(lazy "bar") subscriptions in
  (* Close both channels so subsequent readlines won't wait for channel closure. *)
  let%lwt () = Lwt_io.close output_channel0 in
  let%lwt () = Lwt_io.close output_channel1 in
  let%lwt input0 = Lwt_io.read_line_opt input_channel0 in
  assert_string_option ~context ~expected:(Some "bar") input0;
  let%lwt input1 = Lwt_io.read_line_opt input_channel1 in
  assert_string_option ~context ~expected:None input1;

  Subscriptions.unregister subscriptions ~identifier:identifier0;

  (* Test message won't be constructed when collection is empty *)
  let sentinel = ref None in
  let message =
    lazy
      (sentinel := Some "derp";
       "derp")
  in
  let%lwt () = Subscriptions.broadcast_raw subscriptions ~message in
  assert_string_option ~context ~expected:None !sentinel;

  Lwt.return_unit


let () =
  "subscription_test"
  >::: ["subscription_registration" >:: OUnitLwt.lwt_wrapper test_subscription_registration]
  |> Test.run
