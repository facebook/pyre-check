(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open OUnit2
open Test

open Ast

open Pyre


let test_parse_sources_list _ =
  let file =
    File.create
      ~content:(Some "def foo()->int:\n    return 1\n")
      (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
  in
  let handles = ParseService.parse_sources_list (Service.mock ()) [file] in
  let handle = Option.value_exn (File.handle file) in
  assert_equal handles [handle];

  let source = AstSharedMemory.get_source handle in
  assert_equal (Option.is_some source) true;

  let { Source.path; statements; _ } = Option.value_exn source in
  assert_equal path "a.py";
  (match statements with
   | [{ Node.value = Statement.Define { Statement.Define.name; _ }; _ }] ->
       assert_equal name (Instantiated.Access.create_from_identifiers [~~"a"; ~~"foo"])
   | _ -> assert_unreached ())


let () =
  "parser">:::[
    "parse_sources_list">::test_parse_sources_list
  ]
  |> run_test_tt_main
