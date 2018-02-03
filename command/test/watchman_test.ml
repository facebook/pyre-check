(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Path.AppendOperator


module Parallel = Hack_parallel.Std
module Socket = CommandSocket
module Watchman = CommandWatchman


let start_watchman pid_path () =
  Watchman.run_command true false [] None "." ();
  In_channel.read_all (Path.absolute pid_path) |> Int.of_string


let test_watchman_exists context =
  if not (
      Path.file_exists
        (Path.create_relative
           ~root:(Path.current_working_directory ())
           ~relative:".watchmanconfig"))
  then
    Out_channel.write_all ".watchmanconfig" ~data:"{}";
  let configuration = Configuration.create ~source_root:(Path.current_working_directory ()) () in
  let watchman_root = (Configuration.pyre_root configuration) ^| "watchman" in
  let pid_path = watchman_root ^| "watchman.pid" in
  let lock_path = watchman_root ^| "watchman.lock" in

  let set_up _ =
    Format.pp_set_formatter_out_channel
      Format.err_formatter (Out_channel.create "/dev/null");
    Command_test.start_server () |> ignore;
    let pid = start_watchman pid_path () in
    pid_path, lock_path, pid
  in

  let tear_down (pid_path, lock_path, pid) _ =
    Server.stop "." ();
    Signal.send_i Signal.int (`Pid (Pid.of_int pid));
    Path.remove pid_path;
    Path.remove lock_path;
    Path.remove watchman_root;
  in

  bracket set_up tear_down context |> ignore;

  assert_raises
    (Failure "Watchman client exists (lock is held). Exiting.")
    (Watchman.run_command false false [] None ".");
  Command_test.clean_environment ()


let test_watchman_client context =
  let root = bracket_tmpdir context in
  let create_mock_watchman_response file =
    Format.asprintf
      {|
        {
          "unilateral":true,
          "subscription":"pyre_file_change_subscription",
          "root":"%s",
          "files":["%s"],
          "is_fresh_instance":false,
          "version":"2017-06-20T11:25:02Z",
          "since":"c:1499791693:6173:1:1765",
          "clock":"c:1499791693:6173:1:1768"
        }
      |}
      root
      file
  in
  Format.set_formatter_out_channel (Out_channel.create "/dev/null");
  Command_test.start_server () |> ignore;
  let root = Path.create_absolute root in
  let a = Path.create_relative ~root ~relative:"tmp/a.py" in
  let test = Path.create_relative ~root ~relative:"test.py" in
  let symlinks = Path.Map.of_alist_exn
      [
        a, (Path.create_relative ~root ~relative:"other/c.py");
        test, test
      ]
  in
  let cleanup () =
    Server.stop "." ();
    Command_test.clean_environment ()
  in
  Command_test.protect
    ~f:(fun () ->
        match
          Watchman.process_response
            ~root
            ~watchman_directory:root
            ~symlinks
            (create_mock_watchman_response "tmp/a.py")
        with
        | Some (
            _,
            Protocol.Request.TypeCheckRequest { Protocol.files = [file]; check_dependents = true }) ->
            assert_equal (File.path file |> Path.relative) (Some "other/c.py")
        | _ ->
            assert_failure "Malformed watchman response")
    ~cleanup;
  Command_test.protect
    ~f:(fun () ->
        match
          Watchman.process_response
            ~root
            ~watchman_directory:root
            ~symlinks
            (create_mock_watchman_response "test.py")
        with
        | Some
            (_,
             Protocol.Request.TypeCheckRequest {
               Protocol.files = [file];
               check_dependents = true
             }) ->
            assert_equal (File.path file |> Path.relative) (Some "test.py")
        | _ ->
            assert_failure "Malformed watchman response")
    ~cleanup;
  cleanup ()


let test_different_root context =
  let root = bracket_tmpdir context in
  let watchman_directory = Path.create_absolute root in
  Unix.mkdir (root ^ "/files");
  let root = Path.create_absolute (root ^ "/files") in
  let a = Path.create_relative ~root:watchman_directory ~relative:"files/a.py" in
  let test = Path.create_relative ~root:watchman_directory ~relative:"files/tmp/test.py" in
  let symlinks = Path.Map.of_alist_exn
      [
        a, (Path.create_relative ~root:watchman_directory ~relative:"files/other/c.py");
        test, test
      ]
  in
  let assert_watchman_response_ok file expected_file =
    let mock_watchman_response =
      Format.asprintf
        {|
        {
          "unilateral":true,
          "subscription":"pyre_file_change_subscription",
          "root":"%s",
          "files":["%s"],
          "is_fresh_instance":false,
          "version":"2017-06-20T11:25:02Z",
          "since":"c:1499791693:6173:1:1765",
          "clock":"c:1499791693:6173:1:1768"
        }
      |}
        (Path.absolute watchman_directory)
        file
    in
    match
      Watchman.process_response
        ~root
        ~watchman_directory
        ~symlinks
        mock_watchman_response
    with
    | Some (
        _,
        Protocol.Request.TypeCheckRequest { Protocol.files = [file]; check_dependents = true }) ->
        assert_equal (File.path file |> Path.relative) (Some expected_file)
    | _ ->
        assert_failure "Unexpected watchman response"
  in
  Format.set_formatter_out_channel (Out_channel.create "/dev/null");
  Command_test.start_server () |> ignore;

  let cleanup () =
    Command.run ~argv:["_"] Server.stop_command;
    Server.stop "." ();
    Command_test.clean_environment ()
  in
  Command_test.protect
    ~f:(fun () -> assert_watchman_response_ok "files/a.py" "files/other/c.py")
    ~cleanup;
  Command_test.protect
    ~f:(fun () ->
        assert_watchman_response_ok "files/tmp/test.py" "files/tmp/test.py")
    ~cleanup;
  cleanup ()


let () =
  Command_test.run_command_tests
    "watchman"
    [
      "watchman_exists", test_watchman_exists;
      "watchman_client", test_watchman_client;
      "different_root", test_different_root;
    ]
