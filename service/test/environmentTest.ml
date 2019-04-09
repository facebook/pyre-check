(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Ast
open Analysis
open Service.EnvironmentSharedMemory
open Pyre

open Test
open OUnit2

module Handler = Service.Environment.SharedHandler
module DependencyHandler = Handler.DependencyHandler


let assert_keys ~add ~get keys expected =
  let handle = File.Handle.create "dummy.py" in
  DependencyHandler.clear_keys_batch [handle];
  List.iter keys ~f:(add ~handle);
  assert_equal expected (get ~handle)


let test_dependency_keys _ =
  let assert_dependencies references ~expected =
    let references = List.map references ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    assert_keys
      ~add:DependencyHandler.add_dependent_key
      ~get:DependencyHandler.get_dependent_keys
      references
      expected
  in
  assert_dependencies ["a.b.c"; "d"; "e"] ~expected:["e"; "d"; "a.b.c"];
  assert_dependencies ["a.b.c"] ~expected:["a.b.c"]


let test_alias_keys _ =
  let assert_aliases keys ~expected =
    assert_keys
      ~add:DependencyHandler.add_alias_key
      ~get:DependencyHandler.get_alias_keys
      keys
      expected
  in
  assert_aliases [Type.Primitive "first_key"] ~expected:[Type.Primitive "first_key"];
  assert_aliases
    [Type.Primitive "first_key"; Type.Primitive "second_key"]
    ~expected:[Type.Primitive "second_key"; Type.Primitive "first_key"]


let test_class_keys _ =
  let assert_classes keys ~expected =
    assert_keys
      ~add:DependencyHandler.add_class_key
      ~get:DependencyHandler.get_class_keys
      keys
      expected
  in
  assert_classes [Type.Primitive "C"] ~expected:[Type.Primitive "C"];
  assert_classes
    [Type.Primitive "C"; Type.Primitive "D"]
    ~expected:[Type.Primitive "D"; Type.Primitive "C"]


let test_function_keys _ =
  let assert_function_keys keys ~expected =
    let keys = List.map keys ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    assert_keys
      ~add:DependencyHandler.add_function_key
      ~get:DependencyHandler.get_function_keys
      keys
      expected
  in
  assert_function_keys ["f1"] ~expected:["f1"];
  assert_function_keys ["f"; "g"; "class_method"] ~expected:["class_method"; "g"; "f"]


let test_global_keys _ =
  let assert_global_keys keys ~expected =
    let keys = List.map keys ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    assert_keys
      ~add:DependencyHandler.add_global_key
      ~get:DependencyHandler.get_global_keys
      keys
      expected
  in
  assert_global_keys ["a"] ~expected:["a"];
  assert_global_keys ["a"; "b"; "f"] ~expected:["f"; "b"; "a"]


let test_register_modules _ =
  ["a.__name__"; "a.__file__"; "a.__dict__"]
  |> List.map ~f:Reference.create
  |> Service.EnvironmentSharedMemory.Globals.KeySet.of_list
  |> Service.EnvironmentSharedMemory.Globals.remove_batch;

  Handler.register_module
    ~qualifier:(!&"a")
    ~local_mode:Ast.Source.Default
    ~handle:None
    ~stub:false
    ~statements:[];
  let annotation annotation =
    annotation
    |> Annotation.create_immutable ~global:true
    |> Node.create_with_default_location
  in
  assert_equal
    ~printer:(Node.show Annotation.pp)
    (Globals.find_unsafe (!&"a.__name__")) (annotation Type.string);
  assert_equal
    ~printer:(Node.show Annotation.pp)
    (Globals.find_unsafe (!&"a.__file__")) (annotation Type.string);
  assert_equal
    ~printer:(Node.show Annotation.pp)
    (Globals.find_unsafe (!&"a.__dict__"))
    (annotation (Type.dictionary ~key:Type.string ~value:Type.Any))


let test_purge _ =
  Protocols.remove_batch (Protocols.KeySet.singleton SharedMemory.SingletonKey.key);
  Handler.register_protocol ~handle:(File.Handle.create "test.py") (Type.Primitive "MyProtocol");
    Handler.DependencyHandler.add_dependent
    ~handle:(File.Handle.create "test.py")
    (Reference.create "typing");
  assert_equal
    (Some (File.Handle.Set.Tree.singleton (File.Handle.create "test.py")))
    (Handler.DependencyHandler.dependents (!&"typing"));

  assert_equal
    ~printer:(List.to_string ~f:Type.show)
    (Handler.protocols ())
    [Type.Primitive "MyProtocol"];

  Handler.purge [File.Handle.create "test.py"];
  assert_equal
    (Some File.Handle.Set.Tree.empty)
    (Handler.DependencyHandler.dependents (!&"typing"));
  assert_equal
    ~printer:(List.to_string ~f:Type.show)
    (Handler.protocols ())
    []


let test_normalize_dependencies _ =
  let handle = File.Handle.create "dummy.py" in
  DependencyHandler.clear_keys_batch [handle];
  DependencyHandler.add_function_key ~handle (!&"f");
  (* Only keep one copy. *)
  DependencyHandler.add_function_key ~handle (!&"f");
  DependencyHandler.add_function_key ~handle (!&"h");
  DependencyHandler.add_function_key ~handle (!&"g");
  DependencyHandler.normalize [handle];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_function_keys ~handle)
    [!&"f"; !&"g"; !&"h"];

  DependencyHandler.add_global_key ~handle (!&"b");
  DependencyHandler.add_global_key ~handle (!&"c");
  DependencyHandler.add_global_key ~handle (!&"a");
  DependencyHandler.normalize [handle];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_global_keys ~handle) [!&"a"; !&"b"; !&"c"];

  DependencyHandler.add_dependent_key ~handle (!&"first.module");
  DependencyHandler.add_dependent_key ~handle (!&"second.module");
  DependencyHandler.add_dependent_key ~handle (!&"aardvark");
  DependencyHandler.normalize [handle];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_dependent_keys ~handle)
    [!&"aardvark"; !&"first.module"; !&"second.module"];


  DependencyHandler.add_class_key ~handle (Type.Primitive "T1");
  DependencyHandler.add_class_key ~handle (Type.Primitive "T3");
  DependencyHandler.add_class_key ~handle (Type.Primitive "T2");
  DependencyHandler.normalize [handle];
  assert_equal
    ~printer:(List.to_string ~f:Type.show)
    (DependencyHandler.get_class_keys ~handle)
    [Type.Primitive "T1"; Type.Primitive "T2"; Type.Primitive "T3"];

  DependencyHandler.add_alias_key ~handle (Type.Primitive "C_Alias");
  DependencyHandler.add_alias_key ~handle (Type.Primitive "A_Alias");
  DependencyHandler.add_alias_key ~handle (Type.Primitive "B_Alias");
  DependencyHandler.normalize [handle];
  assert_equal
    ~printer:(List.to_string ~f:Type.show)
    (DependencyHandler.get_alias_keys ~handle)
    [Type.Primitive "A_Alias"; Type.Primitive "B_Alias"; Type.Primitive "C_Alias"]


let test_normalize _ =
  TypeOrder.insert (module Handler.TypeOrderHandler) Type.integer;
  TypeOrder.insert (module Handler.TypeOrderHandler) Type.string;
  Protocols.remove_batch (Protocols.KeySet.singleton SharedMemory.SingletonKey.key);
  [Type.Primitive "C"; Type.Primitive "B"; Type.Primitive "A"]
  |> Type.Set.Tree.of_list
  |> Protocols.add SharedMemory.SingletonKey.key;
  let indices =
    let index_of annotation =
      Handler.TypeOrderHandler.find_unsafe (Handler.TypeOrderHandler.indices ()) annotation
    in
    [index_of Type.integer; index_of Type.string]
    |> List.sort ~compare:Int.compare
  in
  Service.Environment.normalize_shared_memory ();
  assert_equal
    (Service.EnvironmentSharedMemory.OrderKeys.get SharedMemory.SingletonKey.key)
    (Some indices);
  assert_equal
    (Protocols.get SharedMemory.SingletonKey.key)
    (Some (Type.Set.Tree.of_list [Type.Primitive "A"; Type.Primitive "B"; Type.Primitive "C"]))


let test_populate context =
  let directory = bracket_tmpdir context in
  let configuration = {
    mock_configuration with Configuration.Analysis.local_root = Path.create_absolute directory;
  }
  in
  Service.Parser.parse_sources
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~preprocessing_state:None
    ~files:[
      File.create
        ~content:(
          {|
            class D: pass
            class C(D): pass
            def foo(): pass
            def bar(): pass
          |}
          |> Test.trim_extra_indentation)
        (Pyre.Path.create_relative ~root:(Path.create_absolute directory) ~relative:"a.py");
    ]
  |> ignore;

  Service.Environment.populate_shared_memory
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~stubs:[]
    ~sources:[File.Handle.create "a.py"];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show) (GlobalKeys.find_unsafe (File.Handle.create "a.py"))
    (List.map ~f:Reference.create ["a.C"; "a.D"; "a.foo"; "a.bar"]);
  let assert_successors name expected_successors =
    let { Resolution.successors; _ } = ClassMetadata.find_unsafe (Type.Primitive name) in
    assert_equal
      ~printer:(List.to_string ~f:Type.show)
      expected_successors
      successors
  in
  assert_successors "a.C" [Type.Primitive "a.D"; Type.Primitive "object"; Type.Any; Type.Top];
  (* Ensure that the memory doesn't get clobbered on a re-write. *)
  Service.Environment.populate
    (module Handler)
    ~configuration
    ~scheduler:(Scheduler.mock ())
    [Option.value_exn (Ast.SharedMemory.Sources.get (File.Handle.create "a.py"))];
  assert_successors "a.C" [Type.Primitive "a.D"; Type.Primitive "object"; Type.Any; Type.Top]


let () =
  "environment">:::[
    "alias_keys">::test_alias_keys;
    "class_keys">::test_class_keys;
    "dependent_keys">::test_dependency_keys;
    "function_keys">::test_function_keys;
    "global_keys">::test_global_keys;
    "normalize_dependencies">::test_normalize_dependencies;
    "normalize">::test_normalize;
    "register_modules">::test_register_modules;
    "populate">::test_populate;
    "purge">::test_purge;
  ]
  |> Test.run
