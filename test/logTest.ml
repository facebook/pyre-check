(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2


let test_rotate _ =
  let create_file path =
    Out_channel.create path |> Out_channel.close
  in
  let base_log_file = Filename.temp_file "log" "" in
  Sys.remove base_log_file;
  assert_equal (Sys.is_file base_log_file) `No;
  let rotated_file = Log.rotate base_log_file in
  (* The rotational logging gives the path to log to, doesn't create the actual file. *)
  create_file rotated_file;
  assert_equal (Sys.is_file rotated_file) `Yes;
  assert_equal (Sys.is_file base_log_file) `Yes;

  let create_path number =
    Format.sprintf "%s.0000-%d" base_log_file number
  in
  [2; 3; 5; 8; 11]
  |> List.map ~f:create_path
  |> List.iter ~f:create_file;

  assert_equal (Sys.is_file base_log_file) `Yes;

  let actual_path = Log.rotate ~number_to_keep:3 base_log_file in
  assert_equal (Sys.is_file base_log_file) `No;
  assert_equal (Unix.lstat base_log_file).Unix.st_kind Unix.S_LNK;
  assert_equal (Sys.is_file (create_path 11)) `No;
  assert_equal (Sys.is_file (create_path 2)) `No;
  assert_equal (Sys.is_file (create_path 3)) `No;
  assert_equal (Sys.is_file (create_path 5)) `Yes;
  assert_equal (Sys.is_file (create_path 8)) `Yes;
  assert_equal (Sys.is_file actual_path) `No


let () =
  "log">:::[
    "rotate">::test_rotate;
  ]
  |> run_test_tt_main
