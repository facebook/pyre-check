(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Path.AppendOperator
open Server


let start_watchman pid_path () =
  Commands.Watchman.run_command
    ~daemonize:true
    ~verbose:false
    ~sections:[]
    ~local_root:"."
    ~search_path:[]
    ~project_root:(Some ".")
  |> ignore;
  let rec poll () =
    if not (Path.file_exists pid_path) then
      (Unix.nanosleep 0.1 |> ignore; poll ())
    else
      ()
  in
  poll ();
  In_channel.read_all (Path.absolute pid_path) |> Int.of_string


let test_watchman_exists context =
  if not (
      Path.file_exists
        (Path.create_relative
           ~root:(Path.current_working_directory ())
           ~relative:".watchmanconfig"))
  then
    Out_channel.write_all ".watchmanconfig" ~data:"{}";
  let configuration =
    Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
  in
  let watchman_root = (Configuration.Analysis.pyre_root configuration) ^| "watchman" in
  let pid_path = watchman_root ^| "watchman.pid" in
  let lock_path = watchman_root ^| "watchman.lock" in
  let set_up _ =
    Format.pp_set_formatter_out_channel
      Format.err_formatter (Out_channel.create "/dev/null");
    CommandTest.start_server () |> ignore;
    let pid = start_watchman pid_path () in
    pid_path, lock_path, pid
  in

  let tear_down (pid_path, lock_path, pid) _ =
    ignore (Commands.Stop.stop ~local_root:".");
    Signal.send_i Signal.int (`Pid (Pid.of_int pid));
    Path.remove pid_path;
    Path.remove lock_path;
    Path.remove watchman_root;
  in

  bracket set_up tear_down context |> ignore;

  assert_raises
    (Failure "Watchman client exists (lock is held). Exiting.")
    (fun () ->
       Commands.Watchman.run_command
         ~daemonize:false
         ~verbose:false
         ~sections:[]
         ~local_root:"."
         ~search_path:[]
         ~project_root:(Some "."))


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
  CommandTest.start_server () |> ignore;
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
    Commands.Stop.stop ~local_root:"."
    |> ignore
  in
  let configuration =
    Configuration.Analysis.create
      ~local_root:root
      ()
  in
  let watchman_state = {
    Commands.Watchman.configuration;
    watchman_directory = root;
    symlinks;
  }
  in
  CommandTest.protect
    ~f:(fun () ->
        match
          Commands.Watchman.process_response
            watchman_state
            (create_mock_watchman_response "tmp/a.py")
        with
        | Some (
            _,
            Protocol.Request.TypeCheckRequest {
              Protocol.TypeCheckRequest.update_environment_with = [file];
              check = [other_file]
            }) ->
            assert_equal (File.path file |> Path.relative) (Some "other/c.py");
            assert_equal file other_file
        | _ ->
            assert_failure "Malformed watchman response")
    ~cleanup;
  CommandTest.protect
    ~f:(fun () ->
        match
          Commands.Watchman.process_response
            watchman_state
            (create_mock_watchman_response "test.py")
        with
        | Some
            (_,
             Protocol.Request.TypeCheckRequest {
               Protocol.TypeCheckRequest.update_environment_with = [file];
               check = [other_file]
             }) ->
            assert_equal (File.path file |> Path.relative) (Some "test.py");
            assert_equal file other_file
        | _ ->
            assert_failure "Malformed watchman response")
    ~cleanup;
  cleanup ()


let test_different_root context =
  let root = bracket_tmpdir context in
  let watchman_directory = Path.create_absolute root in
  Unix.mkdir (root ^ "/files");
  Unix.mkdir (root ^ "/search");
  Out_channel.write_all ~data:"" (root ^ "/search/stub.py");
  let local_root = Path.create_absolute (root ^ "/files") in
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~search_path:[Path.SearchPath.Root (Path.create_absolute (root ^ "/search"))]
      ()
  in

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
      Commands.Watchman.process_response
        { Commands.Watchman.configuration; watchman_directory; symlinks }
        mock_watchman_response
    with
    | Some (
        _,
        Protocol.Request.TypeCheckRequest {
          Protocol.TypeCheckRequest.update_environment_with = [file];
          check = [other_file];
        }) ->
        assert_equal (File.path file |> Path.relative) (Some expected_file);
        assert_equal file other_file
    | _ ->
        assert_failure "Unexpected watchman response"
  in
  Format.set_formatter_out_channel (Out_channel.create "/dev/null");
  CommandTest.start_server () |> ignore;

  let cleanup () =
    Commands.Stop.stop ~local_root:"."
    |> ignore
  in
  CommandTest.protect
    ~f:(fun () -> assert_watchman_response_ok "files/a.py" "files/other/c.py")
    ~cleanup;
  CommandTest.protect
    ~f:(fun () ->
        assert_watchman_response_ok "files/tmp/test.py" "files/tmp/test.py")
    ~cleanup;
  CommandTest.protect
    ~f:(fun () ->
        assert_watchman_response_ok "search/stub.py" "search/stub.py")
    ~cleanup;

  cleanup ()


let test_watchman_subscription _ =
  let assert_subscription extensions directory expected =
    let expected = Yojson.Safe.from_string expected in
    assert_equal
      ~printer:Yojson.Safe.pretty_to_string
      expected
      (Commands.Watchman.subscription extensions directory)
  in
  assert_subscription
    []
    (Path.create_absolute ~follow_symbolic_links:false "/root")
    {|[
      "subscribe",
      "/root",
      "pyre_file_change_subscription",
      {
        "expression": [
          "allof",
          [ "type", "f" ],
          [ "not", "empty" ],
          [ "anyof", [ "suffix", "py" ], [ "suffix", "pyi" ] ]
        ],
        "fields": [ "name" ]
      }]
    |};
  assert_subscription
    ["cconf"]
    (Path.create_absolute ~follow_symbolic_links:false "/root")
    {|[
      "subscribe",
      "/root",
      "pyre_file_change_subscription",
      {
        "expression": [
          "allof",
          [ "type", "f" ],
          [ "not", "empty" ],
          [ "anyof", [ "suffix", "py" ], [ "suffix", "pyi" ], [ "suffix", "cconf" ] ]
        ],
        "fields": [ "name" ]
      }]
    |}


let () =
  CommandTest.run_command_tests
    "watchman"
    [
      "watchman_exists", test_watchman_exists;
      "watchman_client", test_watchman_client;
      "different_root", test_different_root;
      "watchman_subscription", test_watchman_subscription;
    ]
