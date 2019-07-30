(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let configuration = Configuration.Analysis.create ()

let show_location { Location.path; start; stop } =
  Format.asprintf "%s:%a-%a" path Location.pp_position start Location.pp_position stop


let instantiate =
  let lookup_table =
    Reference.Table.of_alist_exn
      [ SourcePath.qualifier_of_relative "test.py", "test.py";
        SourcePath.qualifier_of_relative "builtins.pyi", "builtins.pyi" ]
  in
  Location.instantiate ~lookup:(Hashtbl.find lookup_table)


let generate_lookup source =
  let parsed = parse ~handle:"test.py" source |> Preprocessing.preprocess in
  let configuration = Configuration.Analysis.create ~debug:true ~infer:false () in
  let environment = Test.environment ~configuration () in
  let global_resolution = Environment.resolution environment () in
  Test.populate ~configuration environment [parsed];
  TypeCheck.run ~configuration ~global_resolution ~source:parsed |> ignore;
  let lookup = Lookup.create_of_source global_resolution parsed in
  Environment.purge Service.Environment.shared_handler [Reference.create "test"];

  lookup


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


let assert_annotation_list ~lookup ?(path = "test.py") expected =
  let expected = List.map expected ~f:(fun annotation -> Format.sprintf "%s:%s" path annotation) in
  let list_diff format list = Format.fprintf format "%s\n" (String.concat ~sep:"\n" list) in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~pp_diff:(diff ~print:list_diff)
    expected
    ( Lookup.get_all_annotations lookup
    |> List.map ~f:(fun (key, data) -> Format.asprintf "%s/%a" (show_location key) Type.pp data)
    |> List.sort ~compare:String.compare )


let assert_annotation ~lookup ~path ~position ~annotation =
  let annotation = annotation >>| fun annotation -> Format.sprintf "%s:%s" path annotation in
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    ( Lookup.get_annotation lookup ~position
    >>| fun (location, annotation) ->
    Format.asprintf "%s/%a" (show_location location) Type.pp annotation )


let test_lookup_call_arguments _ =
  let source =
    {|
      foo(12,
          argname="argval",
          multiline=
          "nextline")
    |}
  in
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    [ "2:4-2:6/typing_extensions.Literal[12]";
      "3:12-3:20/typing_extensions.Literal['argval']";
      "3:4-3:20/typing_extensions.Literal['argval']";
      "4:4-5:14/typing_extensions.Literal['nextline']";
      "5:4-5:14/typing_extensions.Literal['nextline']" ];
  assert_annotation
    ~position:{ Location.line = 2; column = 4 }
    ~annotation:(Some "2:4-2:6/typing_extensions.Literal[12]");
  assert_annotation ~position:{ Location.line = 2; column = 6 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 3; column = 3 } ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:20/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:(Some "3:4-3:20/typing_extensions.Literal['argval']");
  assert_annotation
    ~position:{ Location.line = 3; column = 19 }
    ~annotation:(Some "3:12-3:20/typing_extensions.Literal['argval']");
  assert_annotation ~position:{ Location.line = 3; column = 20 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 4; column = 3 } ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 4; column = 4 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 4; column = 13 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 4; column = 14 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 3 }
    ~annotation:(Some "4:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "5:4-5:14/typing_extensions.Literal['nextline']");
  assert_annotation ~position:{ Location.line = 5; column = 14 } ~annotation:None


let test_lookup_pick_narrowest _ =
  let source =
    {|
      def foo(flag: bool, testme: typing.Optional[bool]) -> None:
          if flag and (not testme):
              pass
    |}
  in
  let lookup = generate_lookup source in
  assert_annotation_list
    ~lookup
    [ "2:14-2:18/typing.Type[bool]";
      "2:20-2:26/typing.Optional[bool]";
      "2:28-2:49/typing.Type[typing.Optional[bool]]";
      "2:54-2:58/None";
      "2:8-2:12/bool";
      "3:17-3:27/bool";
      "3:21-3:27/typing.Optional[bool]";
      "3:7-3:11/bool";
      "3:7-3:28/bool" ];
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation ~position:{ Location.line = 3; column = 11 } ~annotation:(Some "3:7-3:28/bool");
  assert_annotation ~position:{ Location.line = 3; column = 16 } ~annotation:(Some "3:7-3:28/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 17 }
    ~annotation:(Some "3:17-3:27/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 21 }
    ~annotation:(Some "3:21-3:27/typing.Optional[bool]");
  assert_annotation ~position:{ Location.line = 3; column = 28 } ~annotation:None


let test_lookup_class_attributes _ =
  let source = {|
      class Foo():
          b: bool
    |} in
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    ["3:11-3:11/typing.Any"; "3:4-3:5/bool"; "3:7-3:11/typing.Type[bool]"];
  assert_annotation ~position:{ Location.line = 3; column = 3 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 3; column = 4 } ~annotation:(Some "3:4-3:5/bool");
  assert_annotation ~position:{ Location.line = 3; column = 5 } ~annotation:None


let test_lookup_comprehensions _ =
  let source = {|a = [x for x in [1.0]]|} in
  assert_annotation_list
    ~lookup:(generate_lookup source)
    [ "1:0-1:1/typing.List[float]";
      "1:16-1:21/typing.List[float]";
      "1:17-1:20/float";
      "1:4-1:22/typing.List[float]" ]


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
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    [ "3:13-3:15/typing_extensions.Literal[12]";
      "3:4-3:5/int";
      "3:7-3:10/typing.Type[int]";
      "4:17-4:21/test.A";
      "4:23-4:24/int";
      "4:26-4:29/typing.Type[int]";
      "4:34-4:38/None";
      "5:17-5:18/int";
      "5:8-5:12/test.A";
      "5:8-5:14/int";
      "7:13-7:16/typing.Type[int]";
      "8:10-8:13/typing_extensions.Literal[100]";
      "8:4-8:5/test.A";
      "8:8-8:14/test.A";
      (* This is the annotation for `A()` (the function call). *)
      "8:8-8:9/typing.Type[test.A]";
      "9:11-9:12/test.A";
      "9:11-9:14/int" ];
  assert_annotation
    ~position:{ Location.line = 5; column = 8 }
    ~annotation:(Some "5:8-5:12/test.A");
  assert_annotation ~position:{ Location.line = 5; column = 13 } ~annotation:(Some "5:8-5:14/int");
  assert_annotation ~position:{ Location.line = 5; column = 14 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 5; column = 17 } ~annotation:(Some "5:17-5:18/int");
  assert_annotation
    ~position:{ Location.line = 9; column = 11 }
    ~annotation:(Some "9:11-9:12/test.A");
  assert_annotation ~position:{ Location.line = 9; column = 12 } ~annotation:(Some "9:11-9:14/int");
  assert_annotation ~position:{ Location.line = 9; column = 13 } ~annotation:(Some "9:11-9:14/int")


let test_lookup_unknown_accesses _ =
  let source = {|
      def foo() -> None:
          arbitrary["key"] = value
    |} in
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list ~lookup ["2:13-2:17/None"; "3:14-3:19/typing_extensions.Literal['key']"];
  assert_annotation ~position:{ Location.line = 3; column = 4 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 3; column = 23 } ~annotation:None


let test_lookup_multiline_accesses _ =
  let source =
    {|
      def foo() -> None:
          if not (unknown.
              multiline.
              access):
              pass

      class A():
          x: int = 12

      def bar() -> int:
          a = A()
          return (a.
                  x)

      def with_blanks() -> int:
          return (A().

                  x)
    |}
  in
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    [ "11:13-11:16/typing.Type[int]";
      "12:4-12:5/test.A";
      "12:8-12:11/test.A";
      "12:8-12:9/typing.Type[test.A]";
      "13:12-13:13/test.A";
      "13:12-14:13/int";
      "16:21-16:24/typing.Type[int]";
      "17:12-17:13/typing.Type[test.A]";
      "17:12-17:15/test.A";
      "17:12-19:13/int";
      "2:13-2:17/None";
      "3:7-5:15/bool";
      "9:13-9:15/typing_extensions.Literal[12]";
      "9:4-9:5/int";
      "9:7-9:10/typing.Type[int]" ];
  assert_annotation ~position:{ Location.line = 3; column = 7 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 3; column = 14 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 3; column = 15 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 4; column = 8 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation ~position:{ Location.line = 5; column = 8 } ~annotation:(Some "3:7-5:15/bool");
  assert_annotation
    ~position:{ Location.line = 13; column = 12 }
    ~annotation:(Some "13:12-13:13/test.A");
  assert_annotation
    ~position:{ Location.line = 13; column = 13 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 4 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 12 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation ~position:{ Location.line = 14; column = 13 } ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 18; column = 0 }
    ~annotation:(Some "17:12-19:13/int")


let test_lookup_out_of_bounds_accesses _ =
  let source =
    {|
      1
      def foo() -> int:
          arbitrary["key"] = value
          return (
              1
          )
    |}
  in
  let lookup = generate_lookup source in
  (* Test that no crazy combination crashes the lookup. *)
  let indices = [-100; -1; 0; 1; 2; 3; 4; 5; 12; 18; 28; 100] in
  let indices_product =
    List.concat_map indices ~f:(fun index_one ->
        List.map indices ~f:(fun index_two -> index_one, index_two))
  in
  let test_one (line, column) =
    Lookup.get_annotation lookup ~position:{ Location.line; column } |> ignore
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
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    [ "3:3-3:4/int";
      "3:6-3:11/typing.Type[int]";
      "4:3-4:4/str";
      "4:6-4:11/typing.Type[str]";
      "5:5-5:9/None" ];
  assert_annotation ~position:{ Location.line = 3; column = 3 } ~annotation:(Some "3:3-3:4/int");
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 7 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 10 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation ~position:{ Location.line = 3; column = 11 } ~annotation:None;
  assert_annotation ~position:{ Location.line = 4; column = 3 } ~annotation:(Some "4:3-4:4/str");
  assert_annotation
    ~position:{ Location.line = 4; column = 6 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 10 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation ~position:{ Location.line = 4; column = 11 } ~annotation:None


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
  assert_annotation
    ~lookup:(generate_lookup source)
    ~path:"test.py"
    ~position:{ Location.line = 19; column = 11 }
    ~annotation:(Some "19:11-19:12/typing.Union[test.A, test.B, test.C]")


let test_lookup_unbound _ =
  let source =
    {|
      def foo(list: typing.List[_T]) -> None:
        a = [x for x in []]
        b = (a[0] if a else a[1])
        c = identity
        d = list
    |}
  in
  let lookup = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~path:"test.py" in
  assert_annotation_list
    ~lookup
    [ "2:14-2:29/typing.Type[typing.List[Variable[_T]]]";
      "2:34-2:38/None";
      "2:8-2:12/typing.List[Variable[_T]]";
      "3:18-3:20/typing.List[Variable[_T]]";
      "3:2-3:3/typing.List[typing.Any]";
      "3:6-3:21/typing.List[typing.Any]";
      "4:15-4:16/typing.List[typing.Any]";
      "4:2-4:3/typing.Any";
      "4:22-4:23/typing.Callable(list.__getitem__)[..., unknown][[[Named(i, int)], \
       typing.Any][[Named(s, slice)], typing.List[typing.Any]]]";
      "4:22-4:26/typing.Any";
      "4:24-4:25/typing_extensions.Literal[1]";
      "4:7-4:11/typing.Any";
      "4:7-4:26/typing.Any";
      "4:7-4:8/typing.Callable(list.__getitem__)[..., unknown][[[Named(i, int)], \
       typing.Any][[Named(s, slice)], typing.List[typing.Any]]]";
      "4:9-4:10/typing_extensions.Literal[0]";
      "5:2-5:3/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "5:6-5:14/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "6:2-6:3/typing.List[Variable[_T]]";
      "6:6-6:10/typing.List[Variable[_T]]" ];
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:21/typing.List[typing.Any]");
  assert_annotation
    ~position:{ Location.line = 3; column = 18 }
    ~annotation:(Some "3:18-3:20/typing.List[Variable[_T]]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:
      (Some
         "4:7-4:8/typing.Callable(list.__getitem__)[..., unknown][[[Named(i, int)], \
          typing.Any][[Named(s, slice)], typing.List[typing.Any]]]");
  assert_annotation
    ~position:{ Location.line = 4; column = 22 }
    ~annotation:
      (Some
         "4:22-4:23/typing.Callable(list.__getitem__)[..., unknown][[[Named(i, int)], \
          typing.Any][[Named(s, slice)], typing.List[typing.Any]]]")


let test_lookup_if_statements _ =
  let source =
    {|
      def foo(flag: bool, list: typing.List[int]) -> None:
          if flag:
              pass
          if not flag:
              pass
          if list:
              pass
          if not list:
              pass
    |}
  in
  let lookup = generate_lookup source in
  assert_annotation_list
    ~lookup
    [ "2:14-2:18/typing.Type[bool]";
      "2:20-2:24/typing.List[int]";
      "2:26-2:42/typing.Type[typing.List[int]]";
      "2:47-2:51/None";
      "2:8-2:12/bool";
      "3:7-3:11/bool";
      "5:11-5:15/bool";
      "5:7-5:15/bool";
      "7:7-7:11/typing.List[int]";
      "9:11-9:15/typing.List[int]";
      "9:7-9:15/bool" ]


let assert_definition_list ~lookup expected =
  let list_diff format list = Format.fprintf format "%s\n" (String.concat ~sep:"\n" list) in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~pp_diff:(diff ~print:list_diff)
    expected
    ( Lookup.get_all_definitions lookup
    |> List.map ~f:(fun (key, data) ->
           Format.asprintf
             "%s -> %s"
             (show_location (instantiate key))
             (show_location (instantiate data)))
    |> List.sort ~compare:String.compare )


let assert_definition ~lookup ~position ~definition =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    definition
    (Lookup.get_definition lookup ~position >>| show_location)


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
  let lookup = generate_lookup source in
  let assert_definition = assert_definition ~lookup in
  assert_definition_list
    ~lookup
    [ "test.py:12:10-12:16 -> test.py:2:0-3:13";
      "test.py:12:4-12:7 -> test.py:8:0-9:8";
      "test.py:13:12-13:18 -> test.py:2:0-3:13";
      "test.py:13:4-13:11 -> test.py:5:0-6:8";
      "test.py:2:16-2:19 -> builtins.pyi:76:0-93:34" ];
  assert_definition ~position:{ Location.line = 12; column = 0 } ~definition:None;
  assert_definition
    ~position:{ Location.line = 12; column = 4 }
    ~definition:(Some "test.py:8:0-9:8");
  assert_definition ~position:{ Location.line = 12; column = 7 } ~definition:None


let test_lookup_definitions_instances _ =
  let source =
    {|
      class X:
          def bar(self) -> None:
              pass

      class Y:
          x: X = X()
          def foo(self) -> X:
              return X()

      def test() -> None:
          x = X()
          x.bar()
          X().bar()
          y = Y()
          y.foo().bar()
          Y().foo().bar()
          y.x.bar()
          Y().x.bar()
    |}
  in
  let lookup = generate_lookup source in
  let assert_definition = assert_definition ~lookup in
  assert_definition_list
    ~lookup
    [ "test.py:12:8-12:9 -> test.py:2:0-4:12";
      "test.py:13:4-13:9 -> test.py:3:4-4:12";
      "test.py:14:4-14:11 -> test.py:3:4-4:12";
      "test.py:14:4-14:5 -> test.py:2:0-4:12";
      "test.py:15:8-15:9 -> test.py:6:0-9:18";
      "test.py:16:4-16:15 -> test.py:3:4-4:12";
      "test.py:16:4-16:9 -> test.py:8:4-9:18";
      "test.py:17:4-17:11 -> test.py:8:4-9:18";
      "test.py:17:4-17:17 -> test.py:3:4-4:12";
      "test.py:17:4-17:5 -> test.py:6:0-9:18";
      "test.py:18:4-18:11 -> test.py:3:4-4:12";
      "test.py:19:4-19:13 -> test.py:3:4-4:12";
      "test.py:19:4-19:5 -> test.py:6:0-9:18";
      "test.py:7:11-7:12 -> test.py:2:0-4:12";
      "test.py:7:4-7:5 -> test.py:6:0-9:18";
      "test.py:8:21-8:22 -> test.py:2:0-4:12";
      "test.py:9:15-9:16 -> test.py:2:0-4:12" ];
  assert_definition
    ~position:{ Location.line = 16; column = 4 }
    ~definition:(Some "test.py:8:4-9:18");
  assert_definition
    ~position:{ Location.line = 16; column = 5 }
    ~definition:(Some "test.py:8:4-9:18");
  assert_definition
    ~position:{ Location.line = 16; column = 6 }
    ~definition:(Some "test.py:8:4-9:18");
  assert_definition
    ~position:{ Location.line = 16; column = 8 }
    ~definition:(Some "test.py:8:4-9:18");
  assert_definition
    ~position:{ Location.line = 16; column = 9 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition
    ~position:{ Location.line = 16; column = 11 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition
    ~position:{ Location.line = 16; column = 12 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition
    ~position:{ Location.line = 16; column = 14 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition ~position:{ Location.line = 16; column = 15 } ~definition:None


let () =
  "lookup"
  >::: [ "lookup" >:: test_lookup;
         "lookup_call_arguments" >:: test_lookup_call_arguments;
         "lookup_pick_narrowest" >:: test_lookup_pick_narrowest;
         "lookup_class_attributes" >:: test_lookup_class_attributes;
         "lookup_identifier_accesses" >:: test_lookup_identifier_accesses;
         "lookup_unknown_accesses" >:: test_lookup_unknown_accesses;
         "lookup_multiline_accesses" >:: test_lookup_multiline_accesses;
         "lookup_out_of_bounds_accesses" >:: test_lookup_out_of_bounds_accesses;
         "lookup_string_annotations" >:: test_lookup_string_annotations;
         "lookup_union_type_resolution" >:: test_lookup_union_type_resolution;
         "lookup_unbound" >:: test_lookup_unbound;
         "lookup_if_statements" >:: test_lookup_if_statements;
         "lookup_comprehensions" >:: test_lookup_comprehensions;
         "lookup_definitions" >:: test_lookup_definitions;
         "lookup_definitions_instances" >:: test_lookup_definitions_instances ]
  |> Test.run
