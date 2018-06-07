(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Pyre
open Test


let configuration = Configuration.create ()


let environment source =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment) [parse source];
  environment


let test_lookup _ =
  let source =
    {|
      def foo(x):
          return 1
      def boo(x):
          return foo(x)
    |}
  in
  let environment = environment source in
  let parsed = parse source in
  let configuration = (Configuration.create ~debug:true ~infer:false ()) in
  let environment = (Environment.handler ~configuration environment) in
  let { TypeCheck.Result.lookup; _ } =
    TypeCheck.check
      configuration
      environment
      mock_call_graph
      parsed
  in
  assert_is_some lookup


let test_lookup_across_files _ =
  let use_source =
    {|
      from define import zoo
      def boo(x):
          return zoo(x)
    |}
  in
  let define_source =
    {|
      def zoo(x):
          return 1
    |}
  in
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.handler ~configuration environment) [
    parse ~qualifier:(Source.qualifier ~path:"use.py") ~path:"use.py" use_source;
    parse ~qualifier:(Source.qualifier ~path:"define.py") ~path:"define.py" define_source;
  ];
  let parsed = parse use_source in
  let configuration = Configuration.create ~debug:true ~infer:false () in
  let environment = Environment.handler ~configuration environment in
  let { TypeCheck.Result.lookup; _ } =
    TypeCheck.check
      configuration
      environment
      mock_call_graph
      parsed
  in
  assert_is_some lookup


let assert_annotation ~lookup ~position ~annotation =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    (Lookup.get_annotation lookup ~position
     >>| (fun (location, annotation) ->
         Format.asprintf "%s/%a" (Location.to_string location) Type.pp annotation))


let test_lookup_call_arguments _ =
  let source =
    {|
      foo(12,
          argname="argval",
          multiline=
          "nextline")
    |}
  in
  let lookup =
    let configuration = Configuration.create ~debug:true ~infer:false () in
    let environment = environment source |> Environment.handler ~configuration in
    let parsed = parse source in
    match TypeCheck.check configuration environment mock_call_graph parsed with
    | { TypeCheck.Result.lookup = Some lookup; _ } -> lookup
    | _ -> failwith "Did not generate lookup table."
  in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "test.py:2:0-2:3/`unknown`";
      "test.py:2:4-2:6/`int`";
      "test.py:3:4-3:20/`str`";
      "test.py:4:4-5:14/`str`";
    ]
    (Hashtbl.to_alist lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (Location.to_string key) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~position:{ Location.line = 2; column = 4 }
    ~annotation:(Some "test.py:2:4-2:6/`int`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "test.py:3:4-3:20/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:(Some "test.py:3:4-3:20/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 19 }
    ~annotation:(Some "test.py:3:4-3:20/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 4; column = 3 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~position:{ Location.line = 4; column = 4 }
    ~annotation:(Some "test.py:4:4-5:14/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 4; column = 13 }
    ~annotation:(Some "test.py:4:4-5:14/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 5; column = 3 }
    ~annotation:(Some "test.py:4:4-5:14/`str`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "test.py:4:4-5:14/`str`")


let test_lookup_pick_narrowest _ =
  let source =
    {|
      def foo(flag: bool, testme: Optional[int]) -> None:
          if flag and (not testme):
              pass
    |}
  in
  let lookup =
    let configuration = Configuration.create ~debug:true ~infer:false () in
    let environment = environment source |> Environment.handler ~configuration in
    let parsed = parse source in
    match TypeCheck.check configuration environment mock_call_graph parsed with
    | { TypeCheck.Result.lookup = Some lookup; _ } -> lookup
    | _ -> failwith "Did not generate lookup table."
  in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "test.py:3:17-3:27/`Optional[int]`";
      "test.py:3:21-3:27/`Optional[int]`";
    ]
    (Hashtbl.to_alist lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (Location.to_string key) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 16 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 17 }
    ~annotation:(Some "test.py:3:17-3:27/`Optional[int]`");
  assert_annotation
    ~lookup
    ~position:{ Location.line = 3; column = 21 }
    ~annotation:(Some "test.py:3:21-3:27/`Optional[int]`")


let () =
  "lookup">:::[
    "lookup">::test_lookup;
    "lookup_across_files">::test_lookup_across_files;
    "lookup_call_arguments">::test_lookup_call_arguments;
    "lookup_pick_narrowest">::test_lookup_pick_narrowest;
  ]
  |> run_test_tt_main
