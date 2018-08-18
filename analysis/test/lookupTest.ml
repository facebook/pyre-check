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


let show_location { Location.path; start; stop } =
  let show_position { Location.line; column } = Format.sprintf "%d:%d" line column in
  Format.sprintf "%s:%s-%s" (path) (show_position start) (show_position stop)


let instantiate =
  let lookup_table =
    Int.Table.of_alist_exn
      [String.hash "test.py", "test.py"; String.hash "builtins.pyi", "builtins.pyi"]
  in
  Location.instantiate ~lookup:(Hashtbl.find lookup_table)


let generate_lookup source =
  let parsed =
    parse
      ~qualifier:(Source.qualifier ~path:"test.py")
      ~path:"test.py" source
    |> Preprocessing.preprocess
  in
  let configuration = Configuration.create ~debug:true ~infer:false () in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [parsed];
  TypeCheck.check configuration environment parsed |> ignore;
  (Lookup.create_of_source environment parsed, trim_extra_indentation source)


let test_lookup _ =
  let source =
    {|
      def foo(x):
          return 1
      def boo(x):
          return foo(x)
    |}
  in
  generate_lookup source |> ignore


let assert_annotation ~lookup ~source ~position ~annotation =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    (Lookup.get_annotation lookup ~source_text:source ~position
     >>| (fun (location, annotation) ->
         Format.asprintf "%s/%a" (show_location location) Type.pp annotation))


let test_lookup_call_arguments _ =
  let source =
    {|
      foo(12,
          argname="argval",
          multiline=
          "nextline")
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "test.py:2:4-2:6/int";
      "test.py:3:12-3:20/str";
      "test.py:3:4-3:20/str";
      "test.py:4:4-5:14/str";
      "test.py:5:4-5:14/str";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 2; column = 4 }
    ~annotation:(Some "test.py:2:4-2:6/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 2; column = 6 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "test.py:3:4-3:20/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:(Some "test.py:3:4-3:20/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 19 }
    ~annotation:(Some "test.py:3:12-3:20/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 20 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 3 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 4 }
    ~annotation:(Some "test.py:4:4-5:14/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 13 }
    ~annotation:(Some "test.py:4:4-5:14/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 14 }
    ~annotation:(Some "test.py:4:4-5:14/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 3 }
    ~annotation:(Some "test.py:4:4-5:14/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "test.py:5:4-5:14/str");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:None


let test_lookup_pick_narrowest _ =
  let source =
    {|
      def foo(flag: bool, testme: Optional[int]) -> None:
          if flag and (not testme):
              pass
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:"\n")
    [
      "test.py:2:14-2:18/typing.Type[bool]";
      "test.py:2:37-2:40/typing.Type[int]";
      "test.py:2:46-2:50/None";
      "test.py:3:17-3:27/bool";
      "test.py:3:7-3:11/bool";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 16 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 17 }
    ~annotation:(Some "test.py:3:17-3:27/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 21 }
    ~annotation:(Some "test.py:3:17-3:27/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 27 }
    ~annotation:None


let test_lookup_class_attributes _ =
  let source =
    {|
      class Foo():
          b: bool
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "*:-1:-1--1:-1/ellipses";
      "test.py:3:4-3:5/bool";
      "test.py:3:4-3:5/typing.Type[test.Foo]";
      "test.py:3:7-3:11/typing.Type[bool]";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "test.py:3:4-3:5/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 5 }
    ~annotation:None


let test_lookup_identifier_accesses _ =
  let source =
    {|
      class A():
          x: int = 12
          def __init__(self, i: int) -> None:
              self.x = i

      def foo() -> int:
          a = A(100)
          return a.x
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:"\n")
    [
      "test.py:3:13-3:15/int";
      "test.py:3:4-3:5/int";
      (* `x` at 3:4-3:5 expands to `test.A.x`. This is the annotation
         for the `test.A` prefix. *)
      "test.py:3:4-3:5/typing.Type[test.A]";
      "test.py:3:7-3:10/typing.Type[int]";
      "test.py:4:26-4:29/typing.Type[int]";
      "test.py:4:34-4:38/None";
      "test.py:5:17-5:18/int";
      "test.py:5:8-5:14/int";
      "test.py:5:8-5:14/test.A";
      "test.py:7:13-7:16/typing.Type[int]";
      "test.py:8:10-8:13/int";
      "test.py:8:8-8:9/test.A";
      (* This is the annotation for `A()` (the function call). *)
      "test.py:8:8-8:9/typing.Type[test.A]";
      "test.py:9:11-9:14/int";
      "test.py:9:11-9:14/test.A";
      "test.py:9:4-9:14/int";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 8 }
    ~annotation:(Some "test.py:5:8-5:12/test.A");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "test.py:5:8-5:14/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 17 }
    ~annotation:(Some "test.py:5:17-5:18/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 9; column = 11 }
    ~annotation:(Some "test.py:9:11-9:12/test.A");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 9; column = 12 }
    ~annotation:(Some "test.py:9:11-9:14/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 9; column = 13 }
    ~annotation:(Some "test.py:9:11-9:14/int")


let test_lookup_unknown_accesses _ =
  let source =
    {|
      def foo() -> None:
          arbitrary["key"] = value
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "test.py:2:13-2:17/None";
      "test.py:3:14-3:19/str";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 23 }
    ~annotation:None


let test_lookup_multiline_accesses _ =
  let source =
    {|
      def foo() -> None:
          if (unknown.
              multiline.
              access):
              pass

      class A():
          x: int = 12

      def bar() -> int:
          a = A()
          return (a.
                  x)

    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:"\n")
    [
      "test.py:11:13-11:16/typing.Type[int]";
      "test.py:12:8-12:9/test.A";
      "test.py:12:8-12:9/typing.Type[test.A]";
      "test.py:13:12-14:13/int";
      "test.py:13:12-14:13/test.A";
      "test.py:13:4-14:13/int";
      "test.py:2:13-2:17/None";
      "test.py:3:8-5:14/bool";
      "test.py:9:13-9:15/int";
      "test.py:9:4-9:5/int";
      "test.py:9:4-9:5/typing.Type[test.A]";
      "test.py:9:7-9:10/typing.Type[int]";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 8 }
    ~annotation:(Some "test.py:3:8-5:14/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 14 }
    ~annotation:(Some "test.py:3:8-5:14/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 15 }
    ~annotation:(Some "test.py:3:8-5:14/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 8 }
    ~annotation:(Some "test.py:3:8-5:14/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 5; column = 8 }
    ~annotation:(Some "test.py:3:8-5:14/bool");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 13; column = 12 }
    ~annotation:(Some "test.py:13:12-13:13/test.A");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 13; column = 13 }
    ~annotation:(Some "test.py:13:12-14:13/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 14; column = 4 }
    ~annotation:(Some "test.py:13:12-14:13/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 14; column = 12 }
    ~annotation:(Some "test.py:13:12-14:13/int");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 14; column = 13 }
    ~annotation:None


let test_lookup_out_of_bounds_accesses _ =
  let source =
    {|
      def foo() -> int:
          arbitrary["key"] = value
          return (
              1
          )
    |}
  in
  let (lookup, source) = generate_lookup source in

  (* Test that no crazy combination crashes the lookup. *)
  let indices = [ -100; -1; 0; 1; 2; 3; 4; 5; 12; 18; 28; 100 ] in
  let indices_product =
    List.concat_map
      indices
      ~f:(fun index_one ->
          List.map indices ~f:(fun index_two -> (index_one, index_two)))
  in

  let test_one (line, column) =
    Lookup.get_annotation
      lookup
      ~source_text:source
      ~position:{ Location.line; column }
    |> ignore
  in
  List.iter indices_product ~f:test_one


let test_lookup_string_annotations _ =
  let source =
    {|
      def foo(
         x: "int",
         y: "str",
      ) -> None:
          pass
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "test.py:3:6-3:11/typing.Type[int]";
      "test.py:4:6-4:11/typing.Type[str]";
      "test.py:5:5-5:9/None";
    ]
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location (instantiate key)) Type.pp data)
     |> List.sort ~compare:String.compare);
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "test.py:3:6-3:11/typing.Type[int]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 7 }
    ~annotation:(Some "test.py:3:6-3:10/typing.Type[int]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 10 }
    ~annotation:(Some "test.py:3:6-3:11/typing.Type[int]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:None;
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 6 }
    ~annotation:(Some "test.py:4:6-4:11/typing.Type[str]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:(Some "test.py:4:6-4:10/typing.Type[str]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 10 }
    ~annotation:(Some "test.py:4:6-4:11/typing.Type[str]");
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 4; column = 11 }
    ~annotation:None

let test_lookup_union_type_resolution _ =
  let source =
    {|
      class A():
          pass

      class B():
          pass

      class C():
          pass

      def foo(condition):
          if condition:
              f = A()
          elif condition > 1:
              f = B()
          else:
              f = C()

          return f
    |}
  in
  let lookup, source = generate_lookup source in
  assert_annotation
    ~lookup
    ~source
    ~position:{ Location.line = 19; column = 11 }
    ~annotation:(Some "test.py:19:11-19:12/typing.Union[test.A, test.B, test.C]")


let assert_definition ~lookup ~position ~definition =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    definition
    (Lookup.get_definition lookup ~position
     >>| show_location)


let test_lookup_definitions _ =
  let source =
    {|
      def getint() -> int:
          return 12

      def takeint(a:int) -> None:
          pass

      def foo(a:int, b:str) -> None:
          pass

      def test() -> None:
          foo(a=getint(), b="one")
          takeint(getint())
    |}
  in
  let lookup, _ = generate_lookup source in

  assert_equal
    ~printer:(String.concat ~sep:"\n")
    [
      "test.py:12:10-12:16 -> test.py:2:0-3:13";
      "test.py:12:4-12:7 -> test.py:8:0-9:8";
      "test.py:13:12-13:18 -> test.py:2:0-3:13";
      "test.py:13:4-13:11 -> test.py:5:0-6:8";
      "test.py:2:16-2:19 -> builtins.pyi:54:0-68:34";
      "test.py:5:14-5:17 -> builtins.pyi:54:0-68:34";
      "test.py:8:10-8:13 -> builtins.pyi:54:0-68:34";
      "test.py:8:17-8:20 -> builtins.pyi:73:0-88:49";
    ]
    (Lookup.get_all_definitions lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s -> %s"
           (show_location (instantiate key))
           (show_location (instantiate data)))
     |> List.sort ~compare:String.compare);
  assert_definition
    ~lookup
    ~position:{ Location.line = 12; column = 0 }
    ~definition:None;
  assert_definition
    ~lookup
    ~position:{ Location.line = 12; column = 4 }
    ~definition:(Some "test.py:8:0-9:8");
  assert_definition
    ~lookup
    ~position:{ Location.line = 12; column = 7 }
    ~definition:None


let () =
  "lookup">:::[
    "lookup">::test_lookup;
    "lookup_call_arguments">::test_lookup_call_arguments;
    "lookup_pick_narrowest">::test_lookup_pick_narrowest;
    "lookup_class_attributes">::test_lookup_class_attributes;
    "lookup_identifier_accesses">::test_lookup_identifier_accesses;
    "lookup_unknown_accesses">::test_lookup_unknown_accesses;
    "lookup_multiline_accesses">::test_lookup_multiline_accesses;
    "lookup_out_of_bounds_accesses">::test_lookup_out_of_bounds_accesses;
    "lookup_string_annotations">::test_lookup_string_annotations;
    "lookup_union_type_resolution">::test_lookup_union_type_resolution;
    "lookup_definitions">::test_lookup_definitions;
  ]
  |> run_test_tt_main
