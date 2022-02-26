(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Json = Yojson.Safe

let assert_string_equal = assert_equal ~cmp:String.equal ~printer:Fn.id

let assert_int_equal = assert_equal ~cmp:Int.equal ~printer:Int.to_string

let assert_event ?name ?event_type ?timestamp ?tags log_file =
  let event_json = Json.from_file log_file in
  let assert_tag ((key, value), json) =
    match Json.Util.to_list json with
    | [key_json; value_json] ->
        assert_string_equal key (Json.Util.to_string key_json);
        assert_string_equal value (Json.Util.to_string value_json)
    | _ -> failwith "Unexpected tag length"
  in
  Option.iter name ~f:(fun name ->
      event_json |> Json.Util.member "name" |> Json.Util.to_string |> assert_string_equal name);
  Option.iter event_type ~f:(fun event_type ->
      let canonicalized_event_type = event_type |> Json.from_string |> Json.to_string in
      event_json
      |> Json.Util.member "event_type"
      |> Json.to_string
      |> assert_string_equal canonicalized_event_type);
  Option.iter timestamp ~f:(fun timestamp ->
      event_json |> Json.Util.member "timestamp" |> Json.Util.to_int |> assert_int_equal timestamp);
  Option.iter tags ~f:(fun tags ->
      event_json
      |> Json.Util.member "tags"
      |> Json.Util.to_list
      |> List.zip_exn tags
      |> List.iter ~f:assert_tag)


let test_event_format _ =
  let output_name = Filename.temp_file "event_format" "test" in
  Profiling.GlobalState.initialize ~profiling_output:output_name ();
  let assert_event ~name ~event_type ~timestamp ~tags event =
    PyrePath.(remove_if_exists (create_absolute output_name));
    Profiling.log_performance_event event;
    assert_event ~name ~event_type ~timestamp ~tags output_name
  in
  assert_event
    (fun () -> Profiling.Event.create "foo" ~event_type:(Duration 42) ~timestamp:0)
    ~name:"foo"
    ~event_type:"[\"Duration\", 42]"
    ~timestamp:0
    ~tags:[];
  assert_event
    (fun () ->
      Profiling.Event.create "bar" ~event_type:(Duration 24) ~timestamp:1 ~tags:["hello", "world"])
    ~name:"bar"
    ~event_type:"[\"Duration\", 24]"
    ~timestamp:1
    ~tags:["hello", "world"];
  assert_event
    (fun () ->
      Profiling.Event.create
        "baz"
        ~event_type:(Counter (Some "[Luck 7] Ice Cream"))
        ~timestamp:1
        ~tags:["hello", "42"])
    ~name:"baz"
    ~event_type:"[\"Counter\", \"[Luck 7] Ice Cream\"]"
    ~timestamp:1
    ~tags:["hello", "42"];

  Sys.remove output_name;
  ()


let test_event_track _ =
  let output_name = Filename.temp_file "event_track" "test" in
  Profiling.GlobalState.initialize ~profiling_output:output_name ();
  let foo x = Profiling.track_duration_event "foo" ~tags:["hello", "world"] ~f:(fun _ -> x + 1) in
  let y = foo 42 in
  assert_int_equal 43 y;
  assert_event output_name ~name:"foo" ~tags:["hello", "world"];

  Sys.remove output_name;
  ()


let test_memory_profiling _ =
  let output_name = Filename.temp_file "event_track" "test" in
  Profiling.GlobalState.initialize ~memory_profiling_output:output_name ();
  Profiling.track_shared_memory_usage ~name:"foo" ();
  assert_event
    output_name
    ~name:"Shared Memory Usage"
    ~tags:
      [
        "used_heap_size", "0";
        "wasted_heap_size", "0";
        "nonempty_hash_slots", "0";
        "used_hash_slots", "0";
        "used_dependency_slots", "0";
      ];

  Sys.remove output_name;
  ()


let () =
  "profiling"
  >::: [
         "event_format" >:: test_event_format;
         "event_track" >:: test_event_track;
         "memory_profiling" >:: test_memory_profiling;
       ]
  |> Test.run
