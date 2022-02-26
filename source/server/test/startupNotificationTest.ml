(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server

let assert_consumed ~log_path expected =
  assert_equal
    ~cmp:[%compare.equal: string option]
    ~printer:(fun consumed -> [%sexp_of: string option] consumed |> Sexp.to_string_hum)
    expected
    (StartupNotification.consume ~log_path ())


let test_basic context =
  assert_consumed ~log_path:(PyrePath.create_absolute "/nonexistent") None;

  let test_root = bracket_tmpdir context |> PyrePath.create_absolute in
  let test_message = "They asked me how well I understood theoretical physics." in
  StartupNotification.produce ~log_path:test_root test_message;
  assert_consumed ~log_path:test_root (Some test_message);
  assert_consumed ~log_path:test_root None;

  let test_root2 = bracket_tmpdir context |> PyrePath.create_absolute in
  let test_message2 = "I said I had a theoretical degree in physics. They said welcome aboard." in
  StartupNotification.produce ~log_path:test_root2 test_message;
  StartupNotification.produce ~log_path:test_root2 test_message2;
  assert_consumed ~log_path:test_root None;
  assert_consumed ~log_path:test_root2 (Some test_message2);
  assert_consumed ~log_path:test_root2 None;
  ()


let () = "startup_notification" >::: ["basic" >:: test_basic] |> Test.run
