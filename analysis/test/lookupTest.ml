(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
    Int.Table.of_alist_exn
      [String.hash "test.py", "test.py"; String.hash "builtins.pyi", "builtins.pyi"]
  in
  Location.instantiate ~lookup:(Hashtbl.find lookup_table)


let generate_lookup source =
  let parsed =
    parse
      ~qualifier:(Source.qualifier ~handle:(File.Handle.create "test.py"))
      ~handle:"test.py" source
    |> Preprocessing.preprocess
  in
  let configuration = Configuration.Analysis.create ~debug:true ~infer:false () in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate ~configuration environment [parsed];
  TypeCheck.check ~configuration ~environment ~source:parsed |> ignore;
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


let assert_annotation_list ~lookup ?(path = "test.py") expected =
  let expected = List.map expected ~f:(fun annotation -> Format.sprintf "%s:%s" path annotation) in
  let list_diff format list =
    Format.fprintf format "%s\n" (String.concat ~sep:"\n" list)
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~pp_diff:(diff ~print:list_diff)
    expected
    (Lookup.get_all_annotations lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s/%a" (show_location key) Type.pp data)
     |> List.sort ~compare:String.compare)



let assert_annotation ~lookup ~source ~path ~position ~annotation =
  let annotation =
    annotation
    >>| fun annotation -> Format.sprintf "%s:%s" path annotation
  in
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    annotation
    (Lookup.get_annotation lookup ~source ~position
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
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "2:4-2:6/int";
      "3:12-3:20/str";
      "3:4-3:20/str";
      "4:4-5:14/str";
      "5:4-5:14/str";
    ];
  assert_annotation
    ~position:{ Location.line = 2; column = 4 }
    ~annotation:(Some "2:4-2:6/int");
  assert_annotation
    ~position:{ Location.line = 2; column = 6 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:20/str");
  assert_annotation
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:(Some "3:4-3:20/str");
  assert_annotation
    ~position:{ Location.line = 3; column = 19 }
    ~annotation:(Some "3:12-3:20/str");
  assert_annotation
    ~position:{ Location.line = 3; column = 20 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 4; column = 3 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 4; column = 4 }
    ~annotation:(Some "4:4-5:14/str");
  assert_annotation
    ~position:{ Location.line = 4; column = 13 }
    ~annotation:(Some "4:4-5:14/str");
  assert_annotation
    ~position:{ Location.line = 4; column = 14 }
    ~annotation:(Some "4:4-5:14/str");
  assert_annotation
    ~position:{ Location.line = 5; column = 3 }
    ~annotation:(Some "4:4-5:14/str");
  assert_annotation
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "5:4-5:14/str");
  assert_annotation
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:None


let test_lookup_pick_narrowest _ =
  let source =
    {|
      def foo(flag: bool, testme: typing.Optional[bool]) -> None:
          if flag and (not testme):
              pass
    |}
  in
  let (lookup, source) = generate_lookup source in

  assert_annotation_list
    ~lookup
    [
      "2:14-2:18/typing.Type[bool]";
      "2:20-2:26/typing.Optional[bool]";
      "2:44-2:48/typing.Type[bool]";
      "2:54-2:58/None";
      "2:8-2:12/bool";
      "3:17-3:27/bool";
      "3:21-3:27/typing.Optional[bool]";
      "3:7-3:11/bool";
    ];
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in
  assert_annotation
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 16 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 17 }
    ~annotation:(Some "3:17-3:27/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 21 }
    ~annotation:(Some "3:21-3:27/typing.Optional[bool]");
  assert_annotation
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
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "3:4-3:5/bool";
      "3:4-3:5/typing.Type[test.Foo]";
      "3:7-3:11/typing.Type[bool]";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:(Some "3:4-3:5/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 5 }
    ~annotation:None


let test_lookup_comprehensions _ =
  let source =
    {|a = [x for x in []]|}
  in
  let (lookup, _) = generate_lookup source in
  assert_annotation_list
    ~lookup
    [
      "1:0-1:1/typing.List[]";
      "1:16-1:18/typing.List[]";
      "1:4-1:19/typing.List[]";
    ]


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
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "3:13-3:15/int";
      "3:4-3:5/int";
      (* `x` at 3:4-3:5 expands to `test.A.x`. This is the annotation
         for the `test.A` prefix. *)
      "3:4-3:5/typing.Type[test.A]";
      "3:7-3:10/typing.Type[int]";
      "4:17-4:21/test.A";
      "4:23-4:24/int";
      "4:26-4:29/typing.Type[int]";
      "4:34-4:38/None";
      "5:17-5:18/int";
      "5:8-5:14/int";
      "5:8-5:14/test.A";
      "7:13-7:16/typing.Type[int]";
      "8:10-8:13/int";
      "8:4-8:5/test.A";
      "8:8-8:9/test.A";
      (* This is the annotation for `A()` (the function call). *)
      "8:8-8:9/typing.Type[test.A]";
      "9:11-9:14/int";
      "9:11-9:14/test.A";
    ];
  assert_annotation
    ~position:{ Location.line = 5; column = 8 }
    ~annotation:(Some "5:8-5:12/test.A");
  assert_annotation
    ~position:{ Location.line = 5; column = 13 }
    ~annotation:(Some "5:8-5:14/int");
  assert_annotation
    ~position:{ Location.line = 5; column = 14 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 5; column = 17 }
    ~annotation:(Some "5:17-5:18/int");
  assert_annotation
    ~position:{ Location.line = 9; column = 11 }
    ~annotation:(Some "9:11-9:12/test.A");
  assert_annotation
    ~position:{ Location.line = 9; column = 12 }
    ~annotation:(Some "9:11-9:14/int");
  assert_annotation
    ~position:{ Location.line = 9; column = 13 }
    ~annotation:(Some "9:11-9:14/int")


let test_lookup_unknown_accesses _ =
  let source =
    {|
      def foo() -> None:
          arbitrary["key"] = value
    |}
  in
  let (lookup, source) = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "2:13-2:17/None";
      "3:14-3:19/str";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 4 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 3; column = 23 }
    ~annotation:None


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
  let (lookup, source) = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "11:13-11:16/typing.Type[int]";
      "12:4-12:5/test.A";
      "12:8-12:9/test.A";
      "12:8-12:9/typing.Type[test.A]";
      "13:12-14:13/int";
      "13:12-14:13/test.A";
      "16:21-16:24/typing.Type[int]";
      "17:12-19:13/int";
      "17:12-19:13/test.A";
      "17:12-19:13/typing.Type[test.A]";
      "2:13-2:17/None";
      "3:7-5:14/bool";
      "9:13-9:15/int";
      "9:4-9:5/int";
      "9:4-9:5/typing.Type[test.A]";
      "9:7-9:10/typing.Type[int]";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 7 }
    ~annotation:(Some "3:7-5:14/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 14 }
    ~annotation:(Some "3:7-5:14/bool");
  assert_annotation
    ~position:{ Location.line = 3; column = 15 }
    ~annotation:(Some "3:7-5:14/bool");
  assert_annotation
    ~position:{ Location.line = 4; column = 8 }
    ~annotation:(Some "3:7-5:14/bool");
  assert_annotation
    ~position:{ Location.line = 5; column = 8 }
    ~annotation:(Some "3:7-5:14/bool");
  assert_annotation
    ~position:{ Location.line = 13; column = 12 }
    ~annotation:(Some "13:12-13:13/test.A");
  assert_annotation
    ~position:{ Location.line = 13; column = 13 }
    ~annotation:(Some "13:12-13:13/test.A");
  assert_annotation
    ~position:{ Location.line = 14; column = 4 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 12 }
    ~annotation:(Some "13:12-14:13/int");
  assert_annotation
    ~position:{ Location.line = 14; column = 13 }
    ~annotation:None;
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
      ~source
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
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "3:3-3:4/int";
      "3:6-3:11/typing.Type[int]";
      "4:3-4:4/str";
      "4:6-4:11/typing.Type[str]";
      "5:5-5:9/None";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 3 }
    ~annotation:(Some "3:3-3:4/int");
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 7 }
    ~annotation:(Some "3:6-3:10/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 10 }
    ~annotation:(Some "3:6-3:11/typing.Type[int]");
  assert_annotation
    ~position:{ Location.line = 3; column = 11 }
    ~annotation:None;
  assert_annotation
    ~position:{ Location.line = 4; column = 3 }
    ~annotation:(Some "4:3-4:4/str");
  assert_annotation
    ~position:{ Location.line = 4; column = 6 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:(Some "4:6-4:10/typing.Type[str]");
  assert_annotation
    ~position:{ Location.line = 4; column = 10 }
    ~annotation:(Some "4:6-4:11/typing.Type[str]");
  assert_annotation
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
    ~path:"test.py"
    ~position:{ Location.line = 19; column = 11 }
    ~annotation:(Some "19:11-19:12/typing.Union[test.A, test.B, test.C]")


let test_lookup_unbound _ =
  let source =
    {|
      def foo(list: List[_T]) -> None:
        a = [x for x in []]
        b = (a[0] if a else a[1])
        c = identity
        d = list
    |}
  in
  let lookup, source = generate_lookup source in
  let assert_annotation = assert_annotation ~lookup ~source ~path:"test.py" in

  assert_annotation_list
    ~lookup
    [
      "2:27-2:31/None";
      "2:8-2:12/List[Variable[_T]]";
      "3:18-3:20/typing.List[]";
      "3:2-3:3/typing.List[]";
      "3:6-3:21/typing.List[]";
      "4:15-4:16/typing.List[]";
      "4:22-4:23/typing.Callable(list.__getitem__)[..., unknown]\
       [[[Named(s, slice)], typing.List[]][[Named(i, int)], unknown]]";
      "4:22-4:23/typing.List[]";
      "4:24-4:25/int";
      "4:7-4:8/typing.Callable(list.__getitem__)[..., unknown][[[Named(s, slice)], \
       typing.List[]][[Named(i, int)], unknown]]";
      "4:7-4:8/typing.List[]";
      "4:9-4:10/int";
      "5:2-5:3/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "5:6-5:14/typing.Callable(identity)[[Named(x, Variable[_T])], Variable[_T]]";
      "6:2-6:3/List[Variable[_T]]";
      "6:6-6:10/List[Variable[_T]]";
    ];
  assert_annotation
    ~position:{ Location.line = 3; column = 6 }
    ~annotation:(Some "3:6-3:21/typing.List[]");
  assert_annotation
    ~position:{ Location.line = 3; column = 18 }
    ~annotation:(Some "3:18-3:20/typing.List[]");
  assert_annotation
    ~position:{ Location.line = 4; column = 7 }
    ~annotation:(Some "4:7-4:8/typing.List[]");
  assert_annotation
    ~position:{ Location.line = 4; column = 22 }
    ~annotation:(Some "4:22-4:23/typing.List[]")


let test_lookup_if_statements _ =
  let source =
    {|
      def foo(flag: bool, list: List[int]) -> None:
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
  let lookup, _source = generate_lookup source in
  assert_annotation_list
    ~lookup
    [
      "2:14-2:18/typing.Type[bool]";
      "2:20-2:24/List[int]";
      "2:31-2:34/typing.Type[int]";
      "2:40-2:44/None";
      "2:8-2:12/bool";
      "3:7-3:11/bool";
      "5:11-5:15/bool";
      "5:7-5:15/bool";
      "7:7-7:11/List[int]";
      "9:11-9:15/List[int]";
      "9:7-9:15/bool";
    ]


let assert_definition_list ~lookup expected =
  let list_diff format list =
    Format.fprintf format "%s\n" (String.concat ~sep:"\n" list)
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~pp_diff:(diff ~print:list_diff)
    expected
    (Lookup.get_all_definitions lookup
     |> List.map ~f:(fun (key, data) ->
         Format.asprintf "%s -> %s"
           (show_location (instantiate key))
           (show_location (instantiate data)))
     |> List.sort ~compare:String.compare)


let assert_definition ~lookup ~source ~position ~definition =
  assert_equal
    ~printer:(Option.value ~default:"(none)")
    definition
    (Lookup.get_definition lookup ~source ~position
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
  let lookup, source = generate_lookup source in
  let assert_definition = assert_definition ~lookup ~source in

  assert_definition_list
    ~lookup
    [
      "test.py:12:10-12:16 -> test.py:2:0-3:13";
      "test.py:12:4-12:7 -> test.py:8:0-9:8";
      "test.py:13:12-13:18 -> test.py:2:0-3:13";
      "test.py:13:4-13:11 -> test.py:5:0-6:8";
      "test.py:2:16-2:19 -> builtins.pyi:56:0-70:34";
      "test.py:5:14-5:17 -> builtins.pyi:56:0-70:34";
      "test.py:8:10-8:13 -> builtins.pyi:56:0-70:34";
      "test.py:8:17-8:20 -> builtins.pyi:75:0-90:49";
    ];
  assert_definition
    ~position:{ Location.line = 12; column = 0 }
    ~definition:None;
  assert_definition
    ~position:{ Location.line = 12; column = 4 }
    ~definition:(Some "test.py:8:0-9:8");
  assert_definition
    ~position:{ Location.line = 12; column = 7 }
    ~definition:None


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
  let lookup, source = generate_lookup source in
  let assert_definition = assert_definition ~lookup ~source in

  assert_definition_list
    ~lookup
    [
      "test.py:12:8-12:9 -> test.py:2:0-4:12";
      "test.py:13:4-13:9 -> test.py:3:4-4:12";
      "test.py:14:4-14:11 -> test.py:2:0-4:12";
      "test.py:14:4-14:11 -> test.py:3:4-4:12";
      "test.py:15:8-15:9 -> test.py:6:0-9:16";
      "test.py:16:4-16:15 -> test.py:3:4-4:12";
      "test.py:16:4-16:15 -> test.py:8:4-9:16";
      "test.py:17:4-17:17 -> test.py:3:4-4:12";
      "test.py:17:4-17:17 -> test.py:6:0-9:16";
      "test.py:17:4-17:17 -> test.py:8:4-9:16";
      "test.py:18:4-18:11 -> test.py:3:4-4:12";
      "test.py:19:4-19:13 -> test.py:3:4-4:12";
      "test.py:19:4-19:13 -> test.py:6:0-9:16";
      "test.py:7:11-7:12 -> test.py:2:0-4:12";
      "test.py:7:4-7:5 -> test.py:6:0-9:16";
      "test.py:7:7-7:8 -> test.py:2:0-4:12";
      "test.py:8:21-8:22 -> test.py:2:0-4:12";
      "test.py:9:15-9:16 -> test.py:2:0-4:12";
    ];
  assert_definition
    ~position:{ Location.line = 16; column = 4 }
    ~definition:None;
  assert_definition
    ~position:{ Location.line = 16; column = 5 }
    ~definition:None;
  assert_definition
    ~position:{ Location.line = 16; column = 6 }
    ~definition:(Some "test.py:8:4-9:16");
  assert_definition
    ~position:{ Location.line = 16; column = 8 }
    ~definition:(Some "test.py:8:4-9:16");
  assert_definition
    ~position:{ Location.line = 16; column = 9 }
    ~definition:None;
  assert_definition
    ~position:{ Location.line = 16; column = 11 }
    ~definition:None;
  assert_definition
    ~position:{ Location.line = 16; column = 12 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition
    ~position:{ Location.line = 16; column = 14 }
    ~definition:(Some "test.py:3:4-4:12");
  assert_definition
    ~position:{ Location.line = 16; column = 15 }
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
    "lookup_unbound">::test_lookup_unbound;
    "lookup_if_statements">::test_lookup_if_statements;
    "lookup_comprehensions">::test_lookup_comprehensions;
    "lookup_definitions">::test_lookup_definitions;
    "lookup_definitions_instances">::test_lookup_definitions_instances;
  ]
  |> Test.run
