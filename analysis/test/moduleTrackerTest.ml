(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Ast
open Pyre
open OUnit2
module ModuleTracker = Analysis.ModuleTracker

let touch path = File.create path ~content:"" |> File.write

let create_source_path ~configuration root relative =
  let path = Path.create_relative ~root ~relative in
  SourcePath.create ~configuration path


let create_source_path_exn ~configuration root relative =
  match create_source_path ~configuration root relative with
  | None ->
      let message =
        Format.asprintf "Failed to create source file %s under %a" relative Path.pp root
      in
      assert_failure message
  | Some result -> result


let lookup_exn tracker reference =
  match ModuleTracker.lookup tracker reference with
  | Some source_path -> source_path
  | None ->
      let message =
        Format.asprintf "Cannot find module %a in the module tracker" Reference.pp reference
      in
      assert_failure message


let test_creation context =
  let assert_create_fail ~configuration root relative =
    match create_source_path ~configuration root relative with
    | None -> ()
    | Some _ ->
        let message =
          Format.asprintf
            "Creating source file %s under %a is supposed to fail"
            relative
            Path.pp
            root
        in
        assert_failure message
  in
  let assert_source_path
      ?priority
      ?is_stub
      ?is_external
      ?is_init
      ~configuration
      ~search_root
      ~relative
      ( {
          SourcePath.priority = actual_priority;
          is_stub = actual_is_stub;
          is_external = actual_is_external;
          is_init = actual_is_init;
          _;
        } as source_path )
    =
    let expected_path = Path.create_relative ~root:search_root ~relative in
    let actual_path = SourcePath.full_path ~configuration source_path in
    assert_equal ~cmp:Path.equal ~printer:Path.show expected_path actual_path;
    Option.iter priority ~f:(fun expected_priority ->
        assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_priority actual_priority);
    Option.iter is_stub ~f:(fun expected_is_stub ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_stub actual_is_stub);
    Option.iter is_external ~f:(fun expected_is_external ->
        assert_equal
          ~cmp:Bool.equal
          ~printer:Bool.to_string
          expected_is_external
          actual_is_external);
    Option.iter is_init ~f:(fun expected_is_init ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_init actual_is_init)
  in
  let assert_same_module_greater
      ({ SourcePath.qualifier = left_qualifier; _ } as left)
      ({ SourcePath.qualifier = right_qualifier; _ } as right)
    =
    assert_equal ~cmp:Reference.equal ~printer:Reference.show left_qualifier right_qualifier;
    let compare_result = SourcePath.same_module_compare left right in
    let message =
      Format.asprintf
        "\'%a\' is supposed to be greater than \'%a\'"
        Sexp.pp_hum
        (SourcePath.sexp_of_t left)
        Sexp.pp_hum
        (SourcePath.sexp_of_t right)
    in
    assert_bool message (compare_result > 0)
  in
  let touch root relative = touch (Path.create_relative ~root ~relative) in
  let test_basic () =
    (* SETUP:
     * local_root/a.py
     * local_root/b.py
     * local_root/c.py
     * local_root/c.pyi
     * local_root/d.py
     * local_root/d/__init__.py
     * local_root/e.py
     * external_root/a.py
     * external_root/b.pyi
     * external_root/b/__init__.py
     * external_root/c.py
     * external_root/c.pyi *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root = bracket_tmpdir context |> Path.create_absolute in
    let local_d_path = Path.absolute local_root ^ "/d" in
    let external_b_path = Path.absolute external_root ^ "/b" in
    Sys_utils.mkdir_no_fail local_d_path;
    Sys_utils.mkdir_no_fail external_b_path;
    List.iter ~f:(touch local_root) ["a.py"; "b.py"; "c.py"; "c.pyi"; "d.py"; "e.py"];
    let () =
      let path = local_d_path |> Path.create_absolute in
      touch path "__init__.py"
    in
    List.iter ~f:(touch external_root) ["a.py"; "b.pyi"; "c.py"; "c.pyi"];
    let () =
      let path = external_b_path |> Path.create_absolute in
      touch path "__init__.py"
    in
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~excludes:[".*/thereisnospoon.py"]
        ~search_path:[SearchPath.Root external_root]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    let assert_create_fail = assert_create_fail ~configuration in
    (* Creation test *)
    let local_a = create_exn local_root "a.py" in
    assert_source_path
      local_a
      ~search_root:local_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_b = create_exn local_root "b.py" in
    assert_source_path
      local_b
      ~search_root:local_root
      ~relative:"b.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_c = create_exn local_root "c.py" in
    assert_source_path
      local_c
      ~search_root:local_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_cstub = create_exn local_root "c.pyi" in
    assert_source_path
      local_cstub
      ~search_root:local_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false
      ~is_init:false;
    let local_d = create_exn local_root "d.py" in
    assert_source_path
      local_d
      ~search_root:local_root
      ~relative:"d.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_dinit = create_exn local_root "d/__init__.py" in
    assert_source_path
      local_dinit
      ~search_root:local_root
      ~relative:"d/__init__.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:true;
    let local_e = create_exn local_root "e.py" in
    assert_source_path
      local_e
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let external_a = create_exn external_root "a.py" in
    assert_source_path
      external_a
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_bstub = create_exn external_root "b.pyi" in
    assert_source_path
      external_bstub
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    let external_binit = create_exn external_root "b/__init__.py" in
    assert_source_path
      external_binit
      ~search_root:external_root
      ~relative:"b/__init__.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:true;
    let external_c = create_exn external_root "c.py" in
    assert_source_path
      external_c
      ~search_root:external_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_cstub = create_exn external_root "c.pyi" in
    assert_source_path
      external_cstub
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_create_fail external_root "thereisnospoon.py";
    assert_create_fail external_root "foo/thereisnospoon.py";

    (* Comparison test *)
    assert_same_module_greater external_a local_a;
    assert_same_module_greater external_bstub local_b;
    assert_same_module_greater external_binit local_b;
    assert_same_module_greater external_bstub external_binit;
    assert_same_module_greater local_cstub local_c;
    assert_same_module_greater external_cstub external_c;
    assert_same_module_greater external_cstub local_cstub;
    assert_same_module_greater external_cstub local_c;
    assert_same_module_greater local_cstub external_c;
    assert_same_module_greater external_c local_c;
    assert_same_module_greater local_dinit local_d;

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:local_root
      ~relative:"d/__init__.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false
  in
  let test_search_path_subdirectory () =
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let search_root = bracket_tmpdir context |> Path.create_absolute in
    let search_subdirectory_path = Path.absolute search_root ^ "/sub" in
    Sys_utils.mkdir_no_fail search_subdirectory_path;
    let search_subdirectory = Path.create_absolute search_subdirectory_path in
    touch local_root "a.py";
    touch search_root "b.py";
    touch search_subdirectory "c.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Subdirectory { root = search_root; subdirectory = "sub" }]
        ~filter_directories:[local_root]
        ()
    in
    let assert_path = assert_equal ~cmp:Path.equal ~printer:Path.show in
    assert_create_fail ~configuration search_root "b.py";
    let source_path_a = create_source_path_exn ~configuration local_root "a.py" in
    assert_path
      (Path.create_relative ~root:local_root ~relative:"a.py")
      (SourcePath.full_path ~configuration source_path_a);
    let source_path_b = create_source_path_exn ~configuration search_subdirectory "c.py" in
    assert_path
      (Path.create_relative ~root:search_subdirectory ~relative:"c.py")
      (SourcePath.full_path ~configuration source_path_b)
  in
  let test_priority () =
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root0 = bracket_tmpdir context |> Path.create_absolute in
    let external_root1 = bracket_tmpdir context |> Path.create_absolute in
    let external_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root0) external_paths0;
    let external_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root1) external_paths1;
    let local_paths = ["a.py"; "b.pyi"; "c.py"; "d.py"; "e.pyi"; "f.py"; "g.pyi"] in
    List.iter ~f:(touch local_root) local_paths;
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    (* Creation test *)
    List.iter local_paths ~f:(fun path ->
        assert_source_path
          (create_exn local_root path)
          ~search_root:local_root
          ~relative:path
          ~is_external:false
          ~is_init:false);
    List.iter external_paths0 ~f:(fun path ->
        assert_source_path
          (create_exn external_root0 path)
          ~search_root:external_root0
          ~relative:path
          ~priority:0
          ~is_external:true
          ~is_init:false);
    List.iter external_paths1 ~f:(fun path ->
        assert_source_path
          (create_exn external_root1 path)
          ~search_root:external_root1
          ~relative:path
          ~priority:1
          ~is_external:true
          ~is_init:false);

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:external_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:external_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:external_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:external_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:true
  in
  let test_exclude () =
    (* Test that ${SOURCE_DIRECTORY} gets correctly replaced *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root = bracket_tmpdir context |> Path.create_absolute in
    touch local_root "foo.py";
    touch local_root "bar.py";
    touch local_root "baz.py";
    touch external_root "foo.py";
    touch external_root "bar.py";
    touch external_root "baz.py";

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root external_root]
        ~excludes:["${SOURCE_DIRECTORY}/ba.*"]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    let assert_create_fail = assert_create_fail ~configuration in
    assert_source_path (create_exn local_root "foo.py") ~search_root:local_root ~relative:"foo.py";
    assert_create_fail local_root "bar.py";
    assert_create_fail local_root "baz.py";
    assert_source_path
      (create_exn external_root "foo.py")
      ~search_root:external_root
      ~relative:"foo.py";
    assert_source_path
      (create_exn external_root "bar.py")
      ~search_root:external_root
      ~relative:"bar.py";
    assert_source_path
      (create_exn external_root "baz.py")
      ~search_root:external_root
      ~relative:"baz.py"
  in
  let test_directory_filter () =
    (* SETUP:
     * - all_root is the parent of both local_root and external_root
     * - local_root is the local root
     * - search_root is the root of other search paths
     * - both derp and durp lives under search_root
     * - search_root is whitelisted with filter_directories and durp is blacklisted with ignore_all_errors
     * We want to make sure that the is_external field is correct for this setup. *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let search_root = bracket_tmpdir context |> Path.create_absolute in
    let derp_path = Path.absolute search_root ^ "/derp" in
    Sys_utils.mkdir_no_fail derp_path;
    let durp_path = Path.absolute search_root ^ "/durp" in
    Sys_utils.mkdir_no_fail durp_path;
    let derp = Path.create_absolute derp_path in
    let durp = Path.create_absolute durp_path in
    touch local_root "a.py";
    touch search_root "b.py";
    touch derp "c.py";
    touch durp "d.py";

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root search_root]
        ~filter_directories:[search_root]
        ~ignore_all_errors:[durp]
        ()
    in
    let assert_source_path = assert_source_path ~configuration in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:true;
    assert_source_path
      (create_exn search_root "b.py")
      ~search_root
      ~relative:"b.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "derp/c.py")
      ~search_root
      ~relative:"derp/c.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "durp/d.py")
      ~search_root
      ~relative:"durp/d.py"
      ~is_external:true
  in
  let test_directory_filter2 () =
    (* SETUP: 
     * - local_root is the local root
     * - search_root is the root of other search paths
     * We want to test the case when `ignore_all_errors` contains nonexistent directories. *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let search_root = bracket_tmpdir context |> Path.create_absolute in
    let nonexist_root = Path.create_absolute ~follow_symbolic_links:false "/whosyourdaddy" in
    assert (not (Path.file_exists nonexist_root));

    touch local_root "a.py";
    touch search_root "b.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root search_root]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[search_root; nonexist_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~configuration
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "b.py")
      ~configuration
      ~search_root
      ~relative:"b.py"
      ~is_external:true
  in
  let test_directory_filter3 () =
    (* We want test that filter_directories follows symlinks *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let search_root = bracket_tmpdir context |> Path.create_absolute in
    let link_local_root = bracket_tmpdir context |> Path.create_absolute in
    let link_search_root = bracket_tmpdir context |> Path.create_absolute in
    touch local_root "a.py";
    touch search_root "b.py";
    Unix.symlink
      ~link_name:(Path.create_relative ~root:link_local_root ~relative:"a.py" |> Path.absolute)
      ~target:(Path.create_relative ~root:local_root ~relative:"a.py" |> Path.absolute);
    Unix.symlink
      ~link_name:(Path.create_relative ~root:link_search_root ~relative:"b.py" |> Path.absolute)
      ~target:(Path.create_relative ~root:search_root ~relative:"b.py" |> Path.absolute);

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root link_search_root; SearchPath.Root link_local_root]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[search_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn link_local_root "a.py")
      ~configuration
      ~search_root:link_local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn link_search_root "b.py")
      ~configuration
      ~search_root:link_search_root
      ~relative:"b.py"
      ~is_external:true
  in
  let test_overlapping () =
    (* SETUP:
     * - external_root0 lives under local_root
     * - external_root1 lives under external_root0
     * Module resolution boils down to which root comes first in the search path *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root0_path = Path.absolute local_root ^ "/external0" in
    Sys_utils.mkdir_no_fail external_root0_path;
    let external_root1_path = external_root0_path ^ "/external1" in
    Sys_utils.mkdir_no_fail external_root1_path;
    let external_root0 = Path.create_absolute external_root0_path in
    let external_root1 = Path.create_absolute external_root1_path in
    touch external_root0 "a.py";
    touch external_root1 "a.py";
    touch local_root "a.py";
    touch local_root "b.py";

    let test_external_root_0_before_1 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~search_path:[SearchPath.Root external_root0; SearchPath.Root external_root1]
          ~filter_directories:[local_root]
          ~ignore_all_errors:[external_root0; external_root1]
          ()
      in
      let create_exn = create_source_path_exn ~configuration in
      let assert_source_path = assert_source_path ~configuration in
      assert_source_path
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;

      (* Resolves to external1.a since external_root0 has higher precedence *)
      assert_source_path
        (create_exn external_root1 "a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true
    in
    let test_external_root_1_before_0 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~search_path:[SearchPath.Root external_root1; SearchPath.Root external_root0]
          ~filter_directories:[local_root]
          ~ignore_all_errors:[external_root0; external_root1]
          ()
      in
      let create_exn = create_source_path_exn ~configuration in
      let assert_source_path = assert_source_path ~configuration in
      assert_source_path
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root1 "a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true
    in
    test_external_root_0_before_1 ();
    test_external_root_1_before_0 ()
  in
  let test_overlapping2 () =
    (* SETUP:
     * - stubs_root lives under local_root (project-internal stubs)
     * - venv_root lives outside local_root (project-external stubs)
     *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let stubs_path = Path.absolute local_root ^ "/stubs" in
    Sys_utils.mkdir_no_fail stubs_path;
    let stubs_root = Path.create_absolute stubs_path in
    let venv_root = bracket_tmpdir context |> Path.create_absolute in
    touch local_root "a.py";
    touch local_root "b.pyi";
    touch local_root "c.pyi";
    touch stubs_root "a.pyi";
    touch venv_root "a.pyi";
    touch venv_root "b.pyi";
    touch venv_root "c.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root stubs_root; SearchPath.Root venv_root]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~is_stub:false
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn stubs_root "a.pyi")
      ~search_root:stubs_root
      ~relative:"a.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (create_exn venv_root "a.pyi")
      ~search_root:venv_root
      ~relative:"a.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (create_exn venv_root "b.pyi")
      ~search_root:venv_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (create_exn local_root "b.pyi")
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (create_exn venv_root "c.py")
      ~search_root:venv_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:true;
    assert_source_path
      (create_exn local_root "c.pyi")
      ~search_root:local_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false;

    assert_same_module_greater (create_exn stubs_root "a.pyi") (create_exn venv_root "a.pyi");
    assert_same_module_greater (create_exn stubs_root "a.pyi") (create_exn local_root "a.py");
    assert_same_module_greater (create_exn venv_root "a.pyi") (create_exn local_root "a.py");
    assert_same_module_greater (create_exn venv_root "b.pyi") (create_exn local_root "b.pyi");
    assert_same_module_greater (create_exn local_root "c.pyi") (create_exn venv_root "c.py")
  in
  let test_root_independence () =
    (* We want to test that `ModuleTracker` creation is independent of where the root is located
       at. *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root0 = bracket_tmpdir context |> Path.create_absolute in
    let external_root1 = bracket_tmpdir context |> Path.create_absolute in
    let local_root_copy = bracket_tmpdir context |> Path.create_absolute in
    let external_root0_copy = bracket_tmpdir context |> Path.create_absolute in
    let external_root1_copy = bracket_tmpdir context |> Path.create_absolute in
    let setup local_root external_root0 external_root1 =
      touch local_root "a.py";
      touch local_root "b.pyi";
      touch external_root0 "c.py";
      touch external_root0 "d.pyi";
      touch external_root1 "e.py";
      touch external_root1 "f.pyi";
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ~filter_directories:[local_root; external_root0]
        ()
      |> ModuleTracker.create
      |> ModuleTracker.all_source_paths
      |> List.sort ~compare:SourcePath.compare
    in
    let source_paths_original = setup local_root external_root0 external_root1 in
    let source_paths_copy = setup local_root_copy external_root0_copy external_root1_copy in
    assert_equal
      ~cmp:(List.equal SourcePath.equal)
      ~printer:(List.to_string ~f:(Format.asprintf "%a" SourcePath.pp))
      source_paths_original
      source_paths_copy
  in
  test_basic ();
  test_search_path_subdirectory ();
  test_exclude ();
  test_directory_filter ();
  test_directory_filter2 ();
  test_directory_filter3 ();
  test_priority ();
  test_overlapping ();
  test_overlapping2 ();
  test_root_independence ()


module Root = struct
  type t =
    | Local
    | External
end

module FileSystemEvent = struct
  type kind =
    | Update
    | Remove

  type t = {
    kind: kind;
    root: Root.t;
    relative: string;
  }
end

module ModuleTrackerEvent = struct
  type t =
    | New of {
        root: Root.t;
        relative: string;
      }
    | Delete of string
end

let simulate_filesystem_event kind path =
  begin
    match kind with
    | FileSystemEvent.Update -> touch path
    | FileSystemEvent.Remove -> Path.remove path
  end;
  path


let test_update context =
  let assert_modules ~expected tracker =
    let expected = List.map expected ~f:Reference.create |> List.sort ~compare:Reference.compare in
    let actual =
      ModuleTracker.source_paths tracker
      |> List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
      |> List.sort ~compare:Reference.compare
    in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(List.to_string ~f:Reference.show)
      expected
      actual
  in
  let assert_module_paths ~configuration ~expected tracker =
    let expected = List.sort ~compare:Path.compare expected in
    let actual =
      ModuleTracker.source_paths tracker
      |> List.map ~f:(SourcePath.full_path ~configuration)
      |> List.sort ~compare:Path.compare
    in
    assert_equal
      ~cmp:(List.equal Path.equal)
      ~printer:(List.to_string ~f:Path.show)
      expected
      actual
  in
  let setup_environment () =
    (* SETUP:
     * local_root/a.py
     * local_root/b.py
     * local_root/c.py
     * external_root/a.pyi
     * external_root/b.pyi
     * external_root/b/__init__.pyi
     * external_root/d.py
     *)
    let touch root relative = touch (Path.create_relative ~root ~relative) in
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root = bracket_tmpdir context |> Path.create_absolute in
    let external_b_path = Path.absolute external_root ^ "/b" in
    Sys_utils.mkdir_no_fail external_b_path;
    List.iter ~f:(touch local_root) ["a.py"; "b.py"; "c.py"];
    let () =
      let path = external_b_path |> Path.create_absolute in
      touch path "__init__.pyi"
    in
    List.iter ~f:(touch external_root) ["a.pyi"; "b.pyi"; "d.py"];
    ( local_root,
      external_root,
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root external_root]
        ~filter_directories:[local_root]
        () )
  in
  let test_setup () =
    (* Make sure our setup is sane *)
    let local_root, external_root, configuration = setup_environment () in
    let tracker = ModuleTracker.create configuration in
    assert_modules tracker ~expected:["a"; "b"; "c"; "d"];
    assert_module_paths
      tracker
      ~configuration
      ~expected:
        [ Path.create_relative ~root:external_root ~relative:"a.pyi";
          Path.create_relative ~root:external_root ~relative:"b/__init__.pyi";
          Path.create_relative ~root:local_root ~relative:"c.py";
          Path.create_relative ~root:external_root ~relative:"d.py" ]
  in
  let assert_incremental ~expected events =
    let local_root, external_root, configuration = setup_environment () in
    let tracker = ModuleTracker.create configuration in
    let root_path = function
      | Root.Local -> local_root
      | Root.External -> external_root
    in
    let create_incremental_update_exn = function
      | ModuleTrackerEvent.New { root; relative } ->
          let source_path = create_source_path_exn ~configuration (root_path root) relative in
          ModuleTracker.IncrementalUpdate.New source_path
      | ModuleTrackerEvent.Delete name ->
          ModuleTracker.IncrementalUpdate.Delete (Reference.create name)
    in
    let update_paths =
      List.map events ~f:(fun { FileSystemEvent.kind; root; relative } ->
          Path.create_relative ~root:(root_path root) ~relative |> simulate_filesystem_event kind)
    in
    let expected =
      List.map expected ~f:create_incremental_update_exn
      |> List.sort ~compare:ModuleTracker.IncrementalUpdate.compare
    in
    let actual =
      ModuleTracker.update ~configuration ~paths:update_paths tracker
      |> List.sort ~compare:ModuleTracker.IncrementalUpdate.compare
    in
    (* Check that the computed incremental update is expected *)
    assert_equal
      ~cmp:(List.equal ModuleTracker.IncrementalUpdate.equal)
      ~printer:(fun updates ->
        List.map updates ~f:ModuleTracker.IncrementalUpdate.sexp_of_t
        |> (fun sexps -> Sexp.List sexps)
        |> Format.asprintf "%a" Sexp.pp_hum)
      expected
      actual;

    (* Also check that the module tracker is in a consistent state: we should track exactly the
       same modules and source files after the update as if we build a fresh module tracker from
       scratch. *)
    let fresh_tracker = ModuleTracker.create configuration in
    let expect_modules =
      ModuleTracker.source_paths fresh_tracker
      |> List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
      |> List.map ~f:Reference.show
    in
    assert_modules ~expected:expect_modules tracker;
    let expect_paths =
      ModuleTracker.source_paths fresh_tracker |> List.map ~f:(SourcePath.full_path ~configuration)
    in
    assert_module_paths ~configuration ~expected:expect_paths tracker
  in
  let test_incremental () =
    (* Adding new file for a new module *)
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.py" }]
      [{ FileSystemEvent.kind = Update; root = Local; relative = "e.py" }];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.py" } ];

    (* Adding new shadowing file for an existing module *)
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "c.pyi" }]
      [{ FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" }];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = External; relative = "c.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" };
        { FileSystemEvent.kind = Update; root = External; relative = "c.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = External; relative = "c.pyi" }]
      [ { FileSystemEvent.kind = Update; root = External; relative = "c.pyi" };
        { FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = External; relative = "a/__init__.pyi" }]
      [ { FileSystemEvent.kind = Update; root = External; relative = "a.py" };
        { FileSystemEvent.kind = Update; root = External; relative = "a/__init__.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = External; relative = "a/__init__.pyi" }]
      [ { FileSystemEvent.kind = Update; root = External; relative = "a/__init__.pyi" };
        { FileSystemEvent.kind = Update; root = External; relative = "a.py" } ];

    (* Adding new shadowed file for an existing module *)
    assert_incremental
      ~expected:[]
      [{ FileSystemEvent.kind = Update; root = Local; relative = "b/__init__.py" }];
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Update; root = External; relative = "a.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" } ];
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" };
        { FileSystemEvent.kind = Update; root = External; relative = "a.py" } ];

    (* Removing a module *)
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "c"]
      [{ FileSystemEvent.kind = Remove; root = Local; relative = "c.py" }];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "d"]
      [{ FileSystemEvent.kind = Remove; root = External; relative = "d.py" }];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "a"]
      [ { FileSystemEvent.kind = Remove; root = External; relative = "a.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "a.py" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "a"]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "a.py" };
        { FileSystemEvent.kind = Remove; root = External; relative = "a.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "b"]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "b.py" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b.pyi" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b/__init__.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "b"]
      [ { FileSystemEvent.kind = Remove; root = External; relative = "b/__init__.pyi" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "b.py" } ];

    (* Removing shadowing file for a module *)
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "a.py" }]
      [{ FileSystemEvent.kind = Remove; root = External; relative = "a.pyi" }];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "a.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" };
        { FileSystemEvent.kind = Remove; root = External; relative = "a.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "a.pyi" }]
      [ { FileSystemEvent.kind = Remove; root = External; relative = "a.pyi" };
        { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "b.py" }]
      [ { FileSystemEvent.kind = Remove; root = External; relative = "b.pyi" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b/__init__.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "b.py" }]
      [ { FileSystemEvent.kind = Remove; root = External; relative = "b/__init__.pyi" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b.pyi" } ];

    (* Removing shadowed file for a module *)
    assert_incremental
      ~expected:[]
      [{ FileSystemEvent.kind = Remove; root = Local; relative = "a.py" }];
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "b.py" };
        { FileSystemEvent.kind = Remove; root = External; relative = "b.pyi" } ];
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "a.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" } ];
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "a.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "a.py" } ];

    (* Removing and adding the same file *)
    assert_incremental
      ~expected:[]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "e.py" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "c.py" }]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "c.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "c.py" } ];

    (* Removing and adding the same module *)
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "e.py" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.pyi" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.py" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.py" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "e.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "e.py" }]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "e.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "e.pyi" };
        { FileSystemEvent.kind = Update; root = Local; relative = "e.py" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.New { root = Local; relative = "c.pyi" }]
      [ { FileSystemEvent.kind = Remove; root = Local; relative = "c.py" };
        { FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "c"]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "c.py" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "c.pyi" } ];
    assert_incremental
      ~expected:[ModuleTrackerEvent.Delete "c"]
      [ { FileSystemEvent.kind = Update; root = Local; relative = "c.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "c.pyi" };
        { FileSystemEvent.kind = Remove; root = Local; relative = "c.py" } ]
  in
  test_setup ();
  test_incremental ();
  ()


let () = "environment" >::: ["creation" >:: test_creation; "update" >:: test_update] |> Test.run
