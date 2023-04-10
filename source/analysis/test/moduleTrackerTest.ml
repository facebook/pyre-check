(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open OUnit2
module EnvironmentControls = Analysis.EnvironmentControls
module ModuleTracker = Analysis.ModuleTracker

let content_on_disk = "# contents on disk"

let create_file path = File.create path ~content:content_on_disk |> File.write

let create_module_path ~configuration root relative =
  let path = Test.relative_artifact_path ~root ~relative in
  ModulePath.create ~configuration path


let create_module_path_exn ~configuration root relative =
  match create_module_path ~configuration root relative with
  | None ->
      let message =
        Format.asprintf "Failed to create source file %s under %a" relative PyrePath.pp root
      in
      assert_failure message
  | Some result -> result


let lookup_exn tracker reference =
  match ModuleTracker.ReadOnly.lookup_module_path tracker reference with
  | Some module_path -> module_path
  | None ->
      let message =
        Format.asprintf "Cannot find module %a in the module tracker" Reference.pp reference
      in
      assert_failure message


let create_file root relative = create_file (PyrePath.create_relative ~root ~relative)

let remove_file root relative = PyrePath.create_relative ~root ~relative |> PyrePath.remove

module TestFiles = struct
  type t =
    | File of string
    | Directory of {
        relative: string;
        children: t list;
      }

  let rec create root tree =
    match tree with
    | File relative -> create_file root relative
    | Directory { relative; children } ->
        let path = PyrePath.create_relative ~root ~relative in
        PyrePath.absolute path |> Sys_utils.mkdir_no_fail;
        List.iter ~f:(create path) children
end

let create_test_configuration ~context ~local_tree ~external_tree =
  let local_root, external_root =
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let () = List.iter ~f:(TestFiles.create local_root) local_tree in
    let () = List.iter ~f:(TestFiles.create external_root) external_tree in
    local_root, external_root
  in
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~excludes:[".*/thereisnospoon.py"]
      ~search_paths:[SearchPath.Root external_root]
      ~filter_directories:[local_root]
      ~extensions:
        (List.map
           ~f:Configuration.Extension.create_extension
           [".first"; ".second"; ".third"; ".special$include_suffix_in_module_qualifier"])
      ~track_dependencies:true
      ()
  in
  configuration, external_root


let assert_module_path
    ?priority
    ?is_stub
    ?is_external
    ?is_init
    ~configuration
    ~search_root
    ~relative
    ({
       ModulePath.raw = { priority = actual_priority; _ };
       is_stub = actual_is_stub;
       is_external = actual_is_external;
       is_init = actual_is_init;
       _;
     } as module_path)
  =
  let expected_path = Test.relative_artifact_path ~root:search_root ~relative in
  let actual_path = ModulePath.full_path ~configuration module_path in
  assert_equal
    ~cmp:[%compare.equal: ArtifactPath.t]
    ~printer:ArtifactPath.show
    expected_path
    actual_path;
  Option.iter priority ~f:(fun expected_priority ->
      assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_priority actual_priority);
  Option.iter is_stub ~f:(fun expected_is_stub ->
      assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_stub actual_is_stub);
  Option.iter is_external ~f:(fun expected_is_external ->
      assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_external actual_is_external);
  Option.iter is_init ~f:(fun expected_is_init ->
      assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_init actual_is_init)


let assert_no_module_path ~configuration root relative =
  match create_module_path ~configuration root relative with
  | None -> ()
  | Some _ ->
      let message =
        Format.asprintf
          "Creating source file %s under %a is supposed to fail"
          relative
          PyrePath.pp
          root
      in
      assert_failure message


let assert_same_module_less
    ~configuration
    ({ ModulePath.qualifier = left_qualifier; _ } as left)
    ({ ModulePath.qualifier = right_qualifier; _ } as right)
  =
  assert_equal ~cmp:Reference.equal ~printer:Reference.show left_qualifier right_qualifier;
  let compare_result = ModulePath.same_module_compare ~configuration left right in
  let message =
    Format.asprintf
      "\'%a\' is supposed to be less than \'%a\'"
      Sexp.pp_hum
      (ModulePath.sexp_of_t left)
      Sexp.pp_hum
      (ModulePath.sexp_of_t right)
  in
  assert_bool message (compare_result < 0)


let test_module_path_create context =
  let ({ Configuration.Analysis.local_root; _ } as configuration), external_root =
    create_test_configuration
      ~context
      ~local_tree:
        [
          TestFiles.File "a.py";
          TestFiles.File "b.py";
          TestFiles.File "c.py";
          TestFiles.File "c.pyi";
          TestFiles.File "d.py";
          TestFiles.Directory { relative = "d"; children = [TestFiles.File "__init__.py"] };
          TestFiles.File "e.py";
        ]
      ~external_tree:
        [
          TestFiles.File "a.py";
          TestFiles.File "b.pyi";
          TestFiles.Directory { relative = "b"; children = [TestFiles.File "__init__.py"] };
          TestFiles.File "c.py";
          TestFiles.File "c.pyi";
        ]
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  let assert_same_module_less = assert_same_module_less ~configuration in
  let assert_no_module_path = assert_no_module_path ~configuration in
  (* Creation test *)
  let local_a = create_exn local_root "a.py" in
  assert_module_path
    local_a
    ~search_root:local_root
    ~relative:"a.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let local_b = create_exn local_root "b.py" in
  assert_module_path
    local_b
    ~search_root:local_root
    ~relative:"b.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let local_c = create_exn local_root "c.py" in
  assert_module_path
    local_c
    ~search_root:local_root
    ~relative:"c.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let local_cstub = create_exn local_root "c.pyi" in
  assert_module_path
    local_cstub
    ~search_root:local_root
    ~relative:"c.pyi"
    ~is_stub:true
    ~is_external:false
    ~is_init:false;
  let local_d = create_exn local_root "d.py" in
  assert_module_path
    local_d
    ~search_root:local_root
    ~relative:"d.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let local_dinit = create_exn local_root "d/__init__.py" in
  assert_module_path
    local_dinit
    ~search_root:local_root
    ~relative:"d/__init__.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:true;
  let local_e = create_exn local_root "e.py" in
  assert_module_path
    local_e
    ~search_root:local_root
    ~relative:"e.py"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let local_f = create_exn local_root "f.special" in
  assert_module_path
    local_f
    ~search_root:local_root
    ~relative:"f.special"
    ~is_stub:false
    ~is_external:false
    ~is_init:false;
  let external_a = create_exn external_root "a.py" in
  assert_module_path
    external_a
    ~search_root:external_root
    ~relative:"a.py"
    ~is_stub:false
    ~is_external:true
    ~is_init:false;
  let external_bstub = create_exn external_root "b.pyi" in
  assert_module_path
    external_bstub
    ~search_root:external_root
    ~relative:"b.pyi"
    ~is_stub:true
    ~is_external:true
    ~is_init:false;
  let external_binit = create_exn external_root "b/__init__.py" in
  assert_module_path
    external_binit
    ~search_root:external_root
    ~relative:"b/__init__.py"
    ~is_stub:false
    ~is_external:true
    ~is_init:true;
  let external_c = create_exn external_root "c.py" in
  assert_module_path
    external_c
    ~search_root:external_root
    ~relative:"c.py"
    ~is_stub:false
    ~is_external:true
    ~is_init:false;
  let external_cstub = create_exn external_root "c.pyi" in
  assert_module_path
    external_cstub
    ~search_root:external_root
    ~relative:"c.pyi"
    ~is_stub:true
    ~is_external:true
    ~is_init:false;
  assert_no_module_path external_root "thereisnospoon.py";
  assert_no_module_path external_root "foo/thereisnospoon.py";
  let extension_first = create_exn local_root "dir/a.first" in
  let extension_second = create_exn local_root "dir/a.second" in
  let extension_third = create_exn local_root "dir/a.third" in
  let extension_py = create_exn local_root "dir/a.py" in
  (* Comparison test *)
  assert_same_module_less external_a local_a;
  assert_same_module_less external_bstub local_b;
  assert_same_module_less external_binit local_b;
  assert_same_module_less external_bstub external_binit;
  assert_same_module_less local_cstub local_c;
  assert_same_module_less external_cstub external_c;
  assert_same_module_less external_cstub local_cstub;
  assert_same_module_less external_cstub local_c;
  assert_same_module_less local_cstub external_c;
  assert_same_module_less external_c local_c;
  assert_same_module_less local_dinit local_d;
  assert_same_module_less extension_first extension_second;
  assert_same_module_less extension_first extension_third;
  assert_same_module_less extension_py extension_first;
  ()


let test_module_path_search_path_subdirectory context =
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let search_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let search_subdirectory_path = PyrePath.absolute search_root ^ "/sub" in
  Sys_utils.mkdir_no_fail search_subdirectory_path;
  let search_subdirectory =
    PyrePath.create_absolute ~follow_symbolic_links:true search_subdirectory_path
  in
  create_file local_root "a.py";
  create_file search_root "b.py";
  create_file search_subdirectory "c.py";
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Subdirectory { root = search_root; subdirectory = "sub" }]
      ~filter_directories:[local_root]
      ()
  in
  let assert_path = assert_equal ~cmp:[%compare.equal: ArtifactPath.t] ~printer:ArtifactPath.show in
  assert_no_module_path ~configuration search_root "b.py";
  let module_path_a = create_module_path_exn ~configuration local_root "a.py" in
  assert_path
    (Test.relative_artifact_path ~root:local_root ~relative:"a.py")
    (ModulePath.full_path ~configuration module_path_a);
  let module_path_b = create_module_path_exn ~configuration search_subdirectory "c.py" in
  assert_path
    (Test.relative_artifact_path ~root:search_subdirectory ~relative:"c.py")
    (ModulePath.full_path ~configuration module_path_b)


let test_module_path_exclude context =
  (* Test that ${SOURCE_DIRECTORY} gets correctly replaced *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let external_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  create_file local_root "foo.py";
  create_file local_root "bar.py";
  create_file local_root "baz.py";
  create_file external_root "foo.py";
  create_file external_root "bar.py";
  create_file external_root "baz.py";

  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root external_root]
      ~excludes:["${SOURCE_DIRECTORY}/ba.*"]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  let assert_no_module_path = assert_no_module_path ~configuration in
  assert_module_path (create_exn local_root "foo.py") ~search_root:local_root ~relative:"foo.py";
  assert_no_module_path local_root "bar.py";
  assert_no_module_path local_root "baz.py";
  assert_module_path
    (create_exn external_root "foo.py")
    ~search_root:external_root
    ~relative:"foo.py";
  assert_module_path
    (create_exn external_root "bar.py")
    ~search_root:external_root
    ~relative:"bar.py";
  assert_module_path
    (create_exn external_root "baz.py")
    ~search_root:external_root
    ~relative:"baz.py"


let test_module_path_directory_filter context =
  (* SETUP:
   * - all_root is the parent of both local_root and external_root
   * - local_root is the local root
   * - search_root is the root of other search paths
   * - both derp and durp lives under search_root
   * - search_root is allowlisted with filter_directories and durp is denylisted with ignore_all_errors
   * We want to make sure that the is_external field is correct for this setup. *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let search_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let derp_path = PyrePath.absolute search_root ^ "/derp" in
  Sys_utils.mkdir_no_fail derp_path;
  let durp_path = PyrePath.absolute search_root ^ "/durp" in
  Sys_utils.mkdir_no_fail durp_path;
  let derp = PyrePath.create_absolute ~follow_symbolic_links:true derp_path in
  let durp = PyrePath.create_absolute ~follow_symbolic_links:true durp_path in
  create_file local_root "a.py";
  create_file search_root "b.py";
  create_file derp "c.py";
  create_file durp "d.py";
  create_file local_root "e.py";

  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root search_root]
      ~filter_directories:[search_root]
      ~ignore_all_errors:[durp; PyrePath.create_relative ~root:local_root ~relative:"e.py"]
      ()
  in
  let assert_module_path = assert_module_path ~configuration in
  let create_exn = create_module_path_exn ~configuration in
  assert_module_path
    (create_exn local_root "a.py")
    ~search_root:local_root
    ~relative:"a.py"
    ~is_external:true;
  assert_module_path
    (create_exn search_root "b.py")
    ~search_root
    ~relative:"b.py"
    ~is_external:false;
  assert_module_path
    (create_exn search_root "derp/c.py")
    ~search_root
    ~relative:"derp/c.py"
    ~is_external:false;
  assert_module_path
    (create_exn search_root "durp/d.py")
    ~search_root
    ~relative:"durp/d.py"
    ~is_external:true;
  assert_module_path
    (create_exn local_root "e.py")
    ~search_root:local_root
    ~relative:"e.py"
    ~is_external:true


let test_module_path_directory_filter2 context =
  (* SETUP:
   * - local_root is the local root
   * - search_root is the root of other search paths
   * We want to test the case when `ignore_all_errors` contains nonexistent directories. *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let search_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let nonexist_root = PyrePath.create_absolute "/whosyourdaddy" in
  assert (not (PyrePath.file_exists nonexist_root));

  create_file local_root "a.py";
  create_file search_root "b.py";
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root search_root]
      ~filter_directories:[local_root]
      ~ignore_all_errors:[search_root; nonexist_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  assert_module_path
    (create_exn local_root "a.py")
    ~configuration
    ~search_root:local_root
    ~relative:"a.py"
    ~is_external:false;
  assert_module_path
    (create_exn search_root "b.py")
    ~configuration
    ~search_root
    ~relative:"b.py"
    ~is_external:true


let test_module_path_directory_filter3 context =
  (* We want test that filter_directories follows symlinks *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let search_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let link_local_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let link_search_root =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  create_file local_root "a.py";
  create_file search_root "b.py";
  Core_unix.symlink
    ~link_name:(PyrePath.create_relative ~root:link_local_root ~relative:"a.py" |> PyrePath.absolute)
    ~target:(PyrePath.create_relative ~root:local_root ~relative:"a.py" |> PyrePath.absolute);
  Core_unix.symlink
    ~link_name:
      (PyrePath.create_relative ~root:link_search_root ~relative:"b.py" |> PyrePath.absolute)
    ~target:(PyrePath.create_relative ~root:search_root ~relative:"b.py" |> PyrePath.absolute);

  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root link_search_root; SearchPath.Root link_local_root]
      ~filter_directories:[local_root]
      ~ignore_all_errors:[search_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  assert_module_path
    (create_exn link_local_root "a.py")
    ~configuration
    ~search_root:link_local_root
    ~relative:"a.py"
    ~is_external:false;
  assert_module_path
    (create_exn link_search_root "b.py")
    ~configuration
    ~search_root:link_search_root
    ~relative:"b.py"
    ~is_external:true


let test_module_path_overlapping context =
  (* SETUP:
   * - external_root0 lives under local_root
   * - external_root1 lives under external_root0
   * Module resolution boils down to which root comes first in the search path *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let external_root0_path = PyrePath.absolute local_root ^ "/external0" in
  Sys_utils.mkdir_no_fail external_root0_path;
  let external_root1_path = external_root0_path ^ "/external1" in
  Sys_utils.mkdir_no_fail external_root1_path;
  let external_root0 = PyrePath.create_absolute ~follow_symbolic_links:true external_root0_path in
  let external_root1 = PyrePath.create_absolute ~follow_symbolic_links:true external_root1_path in
  create_file external_root0 "a.py";
  create_file external_root1 "a.py";
  create_file local_root "a.py";
  create_file local_root "b.py";

  let test_external_root_0_before_1 () =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[external_root0; external_root1]
        ()
    in
    let create_exn = create_module_path_exn ~configuration in
    let assert_module_path = assert_module_path ~configuration in
    assert_module_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_module_path
      (create_exn external_root0 "a.py")
      ~search_root:external_root0
      ~relative:"a.py"
      ~is_external:true;
    assert_module_path
      (create_exn local_root "external0/a.py")
      ~search_root:external_root0
      ~relative:"a.py"
      ~is_external:true;

    (* Resolves to external1.a since external_root0 has higher precedence *)
    assert_module_path
      (create_exn external_root1 "a.py")
      ~search_root:external_root0
      ~relative:"external1/a.py"
      ~is_external:true;
    assert_module_path
      (create_exn external_root0 "external1/a.py")
      ~search_root:external_root0
      ~relative:"external1/a.py"
      ~is_external:true;
    assert_module_path
      (create_exn local_root "external0/external1/a.py")
      ~search_root:external_root0
      ~relative:"external1/a.py"
      ~is_external:true
  in
  let test_external_root_1_before_0 () =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root1; SearchPath.Root external_root0]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[external_root0; external_root1]
        ()
    in
    let create_exn = create_module_path_exn ~configuration in
    let assert_module_path = assert_module_path ~configuration in
    assert_module_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_module_path
      (create_exn external_root0 "a.py")
      ~search_root:external_root0
      ~relative:"a.py"
      ~is_external:true;
    assert_module_path
      (create_exn local_root "external0/a.py")
      ~search_root:external_root0
      ~relative:"a.py"
      ~is_external:true;
    assert_module_path
      (create_exn external_root1 "a.py")
      ~search_root:external_root1
      ~relative:"a.py"
      ~is_external:true;
    assert_module_path
      (create_exn external_root0 "external1/a.py")
      ~search_root:external_root1
      ~relative:"a.py"
      ~is_external:true;
    assert_module_path
      (create_exn local_root "external0/external1/a.py")
      ~search_root:external_root1
      ~relative:"a.py"
      ~is_external:true
  in
  let test_local_root_rules_it_all () =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:
          [
            SearchPath.Root local_root;
            SearchPath.Root external_root0;
            SearchPath.Root external_root1;
          ]
        ~filter_directories:[local_root; external_root0; external_root1]
        ()
    in
    let create_exn = create_module_path_exn ~configuration in
    let assert_module_path = assert_module_path ~configuration in
    assert_module_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_module_path
      (create_exn external_root0 "a.py")
      ~search_root:local_root
      ~relative:"external0/a.py"
      ~is_external:false;
    assert_module_path
      (create_exn local_root "external0/a.py")
      ~search_root:local_root
      ~relative:"external0/a.py"
      ~is_external:false;
    assert_module_path
      (create_exn external_root1 "a.py")
      ~search_root:local_root
      ~relative:"external0/external1/a.py"
      ~is_external:false;
    assert_module_path
      (create_exn external_root0 "external1/a.py")
      ~search_root:local_root
      ~relative:"external0/external1/a.py"
      ~is_external:false;
    assert_module_path
      (create_exn local_root "external0/external1/a.py")
      ~search_root:local_root
      ~relative:"external0/external1/a.py"
      ~is_external:false
  in
  test_external_root_0_before_1 ();
  test_external_root_1_before_0 ();
  test_local_root_rules_it_all ();
  ()


let test_module_path_overlapping2 context =
  (* SETUP:
   * - stubs_root lives under local_root (project-internal stubs)
   * - venv_root lives outside local_root (project-external stubs)
   *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let stubs_path = PyrePath.absolute local_root ^ "/stubs" in
  Sys_utils.mkdir_no_fail stubs_path;
  let stubs_root = PyrePath.create_absolute ~follow_symbolic_links:true stubs_path in
  let venv_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  create_file local_root "a.py";
  create_file local_root "b.pyi";
  create_file local_root "c.pyi";
  create_file stubs_root "a.pyi";
  create_file venv_root "a.pyi";
  create_file venv_root "b.pyi";
  create_file venv_root "c.py";
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root stubs_root; SearchPath.Root venv_root]
      ~filter_directories:[local_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  let assert_same_module_less = assert_same_module_less ~configuration in
  assert_module_path
    (create_exn local_root "a.py")
    ~search_root:local_root
    ~is_stub:false
    ~relative:"a.py"
    ~is_external:false;
  assert_module_path
    (create_exn stubs_root "a.pyi")
    ~search_root:stubs_root
    ~relative:"a.pyi"
    ~is_stub:true
    ~is_external:false;
  assert_module_path
    (create_exn venv_root "a.pyi")
    ~search_root:venv_root
    ~relative:"a.pyi"
    ~is_stub:true
    ~is_external:true;
  assert_module_path
    (create_exn venv_root "b.pyi")
    ~search_root:venv_root
    ~relative:"b.pyi"
    ~is_stub:true
    ~is_external:true;
  assert_module_path
    (create_exn local_root "b.pyi")
    ~search_root:local_root
    ~relative:"b.pyi"
    ~is_stub:true
    ~is_external:false;
  assert_module_path
    (create_exn venv_root "c.py")
    ~search_root:venv_root
    ~relative:"c.py"
    ~is_stub:false
    ~is_external:true;
  assert_module_path
    (create_exn local_root "c.pyi")
    ~search_root:local_root
    ~relative:"c.pyi"
    ~is_stub:true
    ~is_external:false;

  assert_same_module_less (create_exn stubs_root "a.pyi") (create_exn venv_root "a.pyi");
  assert_same_module_less (create_exn stubs_root "a.pyi") (create_exn local_root "a.py");
  assert_same_module_less (create_exn venv_root "a.pyi") (create_exn local_root "a.py");
  assert_same_module_less (create_exn venv_root "b.pyi") (create_exn local_root "b.pyi");
  assert_same_module_less (create_exn local_root "c.pyi") (create_exn venv_root "c.py")


let run_lazy_and_nonlazy ~f = List.iter [true; false] ~f

let test_initialization context =
  let ({ Configuration.Analysis.local_root; _ } as configuration), external_root =
    create_test_configuration
      ~context
      ~local_tree:
        [
          TestFiles.File "a.py";
          TestFiles.File "b.py";
          TestFiles.File "c.py";
          TestFiles.File "c.pyi";
          TestFiles.File "d.py";
          TestFiles.Directory { relative = "d"; children = [TestFiles.File "__init__.py"] };
          TestFiles.File "e.py";
          TestFiles.File "f.first";
          TestFiles.File "a.special";
          TestFiles.Directory { relative = "a"; children = [TestFiles.File "b.special"] };
          TestFiles.File "a.b.special";
        ]
      ~external_tree:
        [
          TestFiles.File "a.py";
          TestFiles.File "b.pyi";
          TestFiles.Directory { relative = "b"; children = [TestFiles.File "__init__.py"] };
          TestFiles.File "c.py";
          TestFiles.File "c.pyi";
        ]
  in
  let assert_module_path = assert_module_path ~configuration in
  (* ModuleTracker initialization test *)
  let run_tracker_tests use_lazy_module_tracking =
    let tracker =
      EnvironmentControls.create ~use_lazy_module_tracking configuration
      |> ModuleTracker.create
      |> ModuleTracker.read_only
    in
    assert_module_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:local_root
      ~relative:"d/__init__.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:local_root
      ~relative:"f.first"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "a.special"))
      ~search_root:local_root
      ~relative:"a.special"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "a.b.special"))
      ~search_root:local_root
      ~relative:"a/b.special"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    ()
  in
  run_lazy_and_nonlazy ~f:run_tracker_tests


let test_priority context =
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let external_root0 =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_root1 =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"; "a/b.py"] in
  List.iter ~f:(create_file external_root0) external_paths0;
  let external_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"; "a.b.py"] in
  List.iter ~f:(create_file external_root1) external_paths1;
  let local_paths = ["a.py"; "b.pyi"; "c.py"; "d.py"; "e.pyi"; "f.py"; "g.pyi"] in
  List.iter ~f:(create_file local_root) local_paths;
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
      ~filter_directories:[local_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  (* Make sure ModulePath behavior is as expected *)
  List.iter local_paths ~f:(fun path ->
      assert_module_path
        (create_exn local_root path)
        ~search_root:local_root
        ~relative:path
        ~is_external:false
        ~is_init:false);
  List.iter external_paths0 ~f:(fun path ->
      assert_module_path
        (create_exn external_root0 path)
        ~search_root:external_root0
        ~relative:path
        ~priority:0
        ~is_external:true
        ~is_init:false);
  List.iter external_paths1 ~f:(fun path ->
      assert_module_path
        (create_exn external_root1 path)
        ~search_root:external_root1
        ~relative:path
        ~priority:1
        ~is_external:true
        ~is_init:false);
  (* Test ModuleTracker behavior *)
  let run_tracker_tests use_lazy_module_tracking =
    let tracker =
      EnvironmentControls.create configuration ~use_lazy_module_tracking
      |> ModuleTracker.create
      |> ModuleTracker.read_only
    in
    assert_module_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:external_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:external_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:external_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_module_path
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:external_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:true;
    ()
  in
  run_lazy_and_nonlazy ~f:run_tracker_tests


let test_priority_multi_source_paths context =
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let source_root0_path = PyrePath.absolute local_root ^ "/source0" in
  Sys_utils.mkdir_no_fail source_root0_path;
  let source_root0 = PyrePath.create_absolute source_root0_path in
  let source_root1_path = PyrePath.absolute local_root ^ "/source1" in
  Sys_utils.mkdir_no_fail source_root1_path;
  let source_root1 = PyrePath.create_absolute source_root1_path in
  let source_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"] in
  List.iter ~f:(create_file source_root0) source_paths0;
  let source_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"] in
  List.iter ~f:(create_file source_root1) source_paths1;
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root source_root0; SearchPath.Root source_root1]
      ~filter_directories:[local_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  (* Creation test *)
  List.iter source_paths0 ~f:(fun path ->
      assert_module_path
        (create_exn source_root0 path)
        ~search_root:source_root0
        ~relative:path
        ~priority:0
        ~is_external:false
        ~is_init:false);
  List.iter source_paths1 ~f:(fun path ->
      assert_module_path
        (create_exn source_root1 path)
        ~search_root:source_root1
        ~relative:path
        ~priority:1
        ~is_external:false
        ~is_init:false);

  (* ModuleTracker initialization test *)
  let run_tracker_tests use_lazy_module_tracking =
    let tracker =
      EnvironmentControls.create configuration ~use_lazy_module_tracking
      |> ModuleTracker.create
      |> ModuleTracker.read_only
    in
    assert_module_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:source_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:source_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:source_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:source_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:source_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_module_path
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:source_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:false;
    ()
  in
  run_lazy_and_nonlazy ~f:run_tracker_tests


let test_root_independence context =
  (* We want to test that `ModuleTracker` creation is independent of where the root is located
     at. *)
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  let external_root0 =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_root1 =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let local_root_copy =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_root0_copy =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let external_root1_copy =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let setup local_root external_root0 external_root1 =
    create_file local_root "a.py";
    create_file local_root "b.pyi";
    create_file external_root0 "c.py";
    create_file external_root0 "d.pyi";
    create_file external_root1 "e.py";
    create_file external_root1 "f.pyi";
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
      ~filter_directories:[local_root; external_root0]
      ()
    |> EnvironmentControls.create
    |> ModuleTracker.create
    |> ModuleTracker.all_module_paths
    |> List.sort ~compare:ModulePath.compare
  in
  let module_paths_original = setup local_root external_root0 external_root1 in
  let module_paths_copy = setup local_root_copy external_root0_copy external_root1_copy in
  assert_equal
    ~cmp:(List.equal ModulePath.equal)
    ~printer:(List.to_string ~f:(Format.asprintf "%a" ModulePath.pp))
    module_paths_original
    module_paths_copy;
  ()


let test_hidden_files context =
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  List.iter ~f:Sys_utils.mkdir_no_fail [PyrePath.absolute local_root ^ "/b"];
  List.iter ~f:(create_file local_root) [".a.py"; ".b/c.py"];
  let module_tracker =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~filter_directories:[local_root]
      ()
    |> EnvironmentControls.create
    |> ModuleTracker.create
    |> ModuleTracker.read_only
  in
  assert_equal
    ~cmp:Int.equal
    ~printer:Int.to_string
    0
    (ModuleTracker.ReadOnly.module_paths module_tracker |> List.length);
  ()


let test_hidden_files2 context =
  let local_root =
    let root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
    PyrePath.create_relative ~root ~relative:".a"
  in
  List.iter ~f:(create_file local_root) ["b.py"; ".c.py"; ".d/e.py"];
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root]
      ~filter_directories:[local_root]
      ()
  in
  let create_exn = create_module_path_exn ~configuration in
  let assert_module_path = assert_module_path ~configuration in
  let module_tracker =
    EnvironmentControls.create configuration |> ModuleTracker.create |> ModuleTracker.read_only
  in
  assert_equal
    ~cmp:Int.equal
    ~printer:Int.to_string
    1
    (ModuleTracker.ReadOnly.module_paths module_tracker |> List.length);
  assert_module_path
    (create_exn local_root "b.py")
    ~search_root:local_root
    ~relative:"b.py"
    ~is_stub:false
    ~is_external:false;
  ()


(* This test verifies that module tracker does not respect CPython logic on namespace modules: even
   when there is an explicit package (with an __init__) or a regualar module that should mask
   implicit namespace packages, we expose them. This is intentional - unit tests will trivially
   expose cases where this causes problems at runtime, and it significantly simplifies Pyre's
   incremental update logic. *)
let test_namespace_modules context =
  let root_that_masks =
    bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
  in
  let local_root = bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true in
  List.iter ~f:(create_file root_that_masks) ["package0.py"; "package1/__init__.py"];
  List.iter ~f:(create_file local_root) ["package0/a.py"; "package1/subpackage/b.py"];
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~source_paths:[SearchPath.Root local_root; SearchPath.Root root_that_masks]
      ~filter_directories:[local_root]
      ()
  in
  let module_tracker =
    EnvironmentControls.create configuration |> ModuleTracker.create |> ModuleTracker.read_only
  in
  (* In CPython, only package0 and package1 would be visible, but Pyre also treats package0.a and
     package1.subpackage.b as valid module paths. *)
  assert_equal
    ~cmp:Int.equal
    ~printer:Int.to_string
    4
    (ModuleTracker.ReadOnly.module_paths module_tracker |> List.length);
  ()


let test_stub_package_priority context =
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration
      ~context
      ~local_tree:
        [
          TestFiles.Directory
            { relative = "foo"; children = [TestFiles.File "my_stub.pyi"; TestFiles.File "bar.py"] };
          TestFiles.Directory
            {
              relative = "foo-stubs";
              children = [TestFiles.File "my_stub.pyi"; TestFiles.File "bar.py"];
            };
        ]
      ~external_tree:[]
  in
  let assert_module_path = assert_module_path ~configuration in
  let lazy_module_tracker =
    EnvironmentControls.create configuration ~use_lazy_module_tracking:true
    |> ModuleTracker.create
    |> ModuleTracker.read_only
  in
  (* TODO(T150247738): This is a bug where the lazy tracker doesn't search for paths in the stub
     package directory. *)
  assert_module_path
    (lookup_exn lazy_module_tracker (Reference.create "foo.my_stub"))
    ~search_root:local_root
    ~relative:"foo/my_stub.pyi"
    ~priority:1
    ~is_stub:true
    ~is_external:false;
  assert_module_path
    (lookup_exn lazy_module_tracker (Reference.create "foo.bar"))
    ~search_root:local_root
    ~relative:"foo/bar.py"
    ~priority:1
    ~is_stub:false
    ~is_external:false;
  let eager_module_tracker =
    EnvironmentControls.create configuration ~use_lazy_module_tracking:false
    |> ModuleTracker.create
    |> ModuleTracker.read_only
  in
  assert_module_path
    (lookup_exn eager_module_tracker (Reference.create "foo.my_stub"))
    ~search_root:local_root
    ~relative:"foo-stubs/my_stub.pyi"
    ~priority:1
    ~is_stub:true
    ~is_external:false;
  assert_module_path
    (lookup_exn eager_module_tracker (Reference.create "foo.bar"))
    ~search_root:local_root
    ~relative:"foo-stubs/bar.py"
    ~priority:1
    ~is_stub:false
    ~is_external:false;
  ()


module IncrementalTest = struct
  module FileOperation = struct
    type t =
      | Add
      | Update
      | Remove
      | LeftAlone
  end

  module Event = struct
    type t =
      | NewExplicit of {
          relative: string;
          is_external: bool;
        }
      | NewImplicit of string
      | Delete of string
    [@@deriving sexp, compare]

    let create_new_explicit ?(is_external = false) relative = NewExplicit { relative; is_external }

    let equal = [%compare.equal: t]
  end

  type t = {
    handle: string;
    operation: FileOperation.t;
  }

  open Test

  module LoadingStyle = struct
    type t =
      | NonLazy
      | LazyLookUpQualifiers of Reference.t list

    let look_up_qualifiers = function
      | NonLazy -> []
      | LazyLookUpQualifiers qualifiers -> qualifiers


    let use_lazy_module_tracking = function
      | NonLazy -> false
      | LazyLookUpQualifiers _ -> true
  end

  let assert_incremental
      ?(external_setups = [])
      ?(loading_style = LoadingStyle.NonLazy)
      ~context
      ~expected
      setups
    =
    let get_old_inputs setups =
      List.filter_map setups ~f:(fun { handle; operation } ->
          match operation with
          | FileOperation.Add -> None
          | _ -> Some (handle, ""))
    in
    let update_filesystem_state { Configuration.Analysis.local_root; search_paths; _ } =
      let update_file ~root { handle; operation } =
        let path = PyrePath.create_relative ~root ~relative:handle in
        match operation with
        | Add
        | Update ->
            (* A file is added/updated *)
            File.create path ~content:"" |> File.write;
            Some ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged (ArtifactPath.create path))
        | Remove ->
            (* A file is removed *)
            PyrePath.remove path;
            Some ArtifactPath.Event.(create ~kind:Kind.Deleted (ArtifactPath.create path))
        | LeftAlone -> None
      in
      let paths = List.filter_map setups ~f:(update_file ~root:local_root) in
      let external_paths =
        let external_root = List.hd_exn search_paths |> SearchPath.get_root in
        List.filter_map external_setups ~f:(update_file ~root:external_root)
      in
      List.append external_paths paths
    in
    (* Set up the initial project *)
    let configuration, module_tracker =
      let old_external_sources = get_old_inputs external_setups in
      let old_sources = get_old_inputs setups in
      let project =
        ScratchProject.setup
          ~context
          ~external_sources:old_external_sources
          ~in_memory:false
          ~use_lazy_module_tracking:(LoadingStyle.use_lazy_module_tracking loading_style)
          old_sources
      in
      let configuration = ScratchProject.configuration_of project in
      let module_tracker = ScratchProject.ReadWrite.module_tracker project in
      (* For lazy module tracker tests, we have to populate tables by looking up qualifiers
         explicitly *)
      let () =
        let read_only = ModuleTracker.read_only module_tracker in
        List.map
          (LoadingStyle.look_up_qualifiers loading_style)
          ~f:(ModuleTracker.ReadOnly.is_module_tracked read_only)
        |> ignore
      in
      configuration, module_tracker
    in
    (* Compute the updates *)
    let events = update_filesystem_state configuration in
    let updates = ModuleTracker.update ~events module_tracker in
    let actual =
      let create_event = function
        | ModuleTracker.IncrementalUpdate.NewExplicit
            { ModulePath.raw = { relative; _ }; is_external; _ } ->
            Event.NewExplicit { relative; is_external }
        | ModuleTracker.IncrementalUpdate.NewImplicit qualifier ->
            Event.NewImplicit (Reference.show qualifier)
        | ModuleTracker.IncrementalUpdate.Delete qualifier ->
            Event.Delete (Reference.show qualifier)
      in
      List.map updates ~f:create_event
    in
    (* Check that the computed incremental update is expected *)
    assert_equal
      ~cmp:(List.equal Event.equal)
      ~printer:(fun events -> [%message (events : Event.t list)] |> Sexp.to_string_hum)
      (List.sort ~compare:Event.compare expected)
      (List.sort ~compare:Event.compare actual);

    (if not (LoadingStyle.use_lazy_module_tracking loading_style) then
       (* Also check that the module tracker is in a consistent state: we should track exactly the
          same modules and source files after the update as if we build a fresh module tracker from
          scratch. Skip this in lazy tests, because the `all_module_paths` API is nonlazy-only. *)
       let actual_module_paths =
         ModuleTracker.all_module_paths module_tracker |> List.sort ~compare:ModulePath.compare
       in
       let expected_module_paths =
         EnvironmentControls.create configuration
         |> ModuleTracker.create
         |> ModuleTracker.all_module_paths
         |> List.sort ~compare:ModulePath.compare
       in
       assert_equal
         ~cmp:(List.equal ModulePath.equal)
         ~printer:(fun module_paths ->
           [%message (module_paths : ModulePath.t list)] |> Sexp.to_string_hum)
         expected_module_paths
         actual_module_paths);
    ()
end

let test_update_new_files context =
  let open IncrementalTest in
  let assert_incremental = assert_incremental ~context in
  (* Baseline: no update *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:[{ handle = "b.py"; operation = FileOperation.LeftAlone }]
    ~expected:[];

  (* Adding new file for a new module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Add };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Add };
      { handle = "b.thrift"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.thrift"; operation = FileOperation.Add };
    ]
    ~expected:[];
  (* Adding new shadowing file for an existing module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit ~is_external:true "a.pyi"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:
      [
        { handle = "a.py"; operation = FileOperation.Add };
        { handle = "a.pyi"; operation = FileOperation.LeftAlone };
        { handle = "a/__init__.pyi"; operation = FileOperation.Add };
      ]
    ~expected:[Event.create_new_explicit ~is_external:true "a/__init__.pyi"];
  (* Adding new shadowed file for an existing module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a/__init__.py"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.LeftAlone };
        { handle = "a.py"; operation = FileOperation.Add };
      ]
    ~expected:[];
  (* Test updating conflicting files - ignore based on precedence. *)
  assert_incremental
    [
      { handle = "conflict/a.py"; operation = FileOperation.LeftAlone };
      { handle = "conflict.a.py"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "conflict/a.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit ~is_external:true "conflict/a.py"];
  ()


let test_update_remove_files context =
  let open IncrementalTest in
  let assert_incremental = assert_incremental ~context in
  (* Removing a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    []
    ~external_setups:[{ handle = "a.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = FileOperation.Remove };
      ]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.thrift"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "b.thrift"; operation = FileOperation.Remove };
    ]
    ~expected:[];
  (* Removing shadowing file for a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = FileOperation.Remove };
      ]
    ~expected:[Event.create_new_explicit "a.py"];
  (* Removing shadowed file for a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = LeftAlone };
      ]
    ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];
  ()


let test_update_changed_files context =
  let open IncrementalTest in
  let assert_incremental = assert_incremental ~context in
  (* Update file *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Update }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[];
  assert_incremental [{ handle = "a.thrift"; operation = FileOperation.Update }] ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "b.thrift"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.py"];

  (* Removing and adding the same module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  ()


let test_update_implicits context =
  let open IncrementalTest in
  let assert_incremental = assert_incremental ~context in
  (* Implicit submodule insertion *)
  assert_incremental
    [{ handle = "a/b.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a/b.py"; Event.NewImplicit "a"];
  assert_incremental
    [{ handle = "a/b/c.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.NewImplicit "a.b"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Add };
    ]
    ~expected:
      [
        Event.create_new_explicit "a/b/c.py";
        Event.create_new_explicit "a/b/d.py";
        Event.NewImplicit "a.b";
      ];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.create_new_explicit "a/b/d.py"];

  (* Implicit submodule remove *)
  assert_incremental
    [{ handle = "a/b/c.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a.b.c"; Event.Delete "a.b"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b.c"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b.c"; Event.Delete "a.b"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/d.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.d"; Event.Delete "a"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.LeftAlone };
      { handle = "a/b.pyi"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.create_new_explicit "a/b.py"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b.pyi"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.b"; Event.Delete "a"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/d/e.py"; operation = FileOperation.Remove };
      { handle = "a/d/f.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.d"; Event.Delete "a.d.e"; Event.Delete "a.d.f"];

  (* Implicit submodule add+remove *)
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.create_new_explicit "a/b/d.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Remove };
      { handle = "a/b/e.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.Delete "a.b.d"; Event.Delete "a.b.e"];

  (* NOTE: These tests are likely to change if we want to allow a/b.py to shadow a/b/... *)
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b"; Event.Delete "a"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Add };
    ]
    ~expected:
      [
        Event.Delete "a.b";
        Event.Delete "a";
        Event.create_new_explicit "a/b/c.py";
        Event.create_new_explicit "a/b/d.py";
      ];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Add };
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/b/d.py"; operation = FileOperation.Remove };
    ]
    ~expected:
      [
        Event.Delete "a.b.c";
        Event.Delete "a.b.d";
        Event.NewImplicit "a";
        Event.create_new_explicit "a/b.py";
      ];
  ()


(* The update code is mostly shared between eager and lazy tracking (the only difference is really
   that the lazy tracker skips processing qualifiers that are not yet in its cache). So rather than
   duplicating all the tests - which are a bit slow - we just exercise each known edge case once *)
let test_update_lazy_tracker context =
  let open IncrementalTest in
  let assert_incremental =
    let open Test in
    assert_incremental
      ~context
      ~loading_style:
        (LazyLookUpQualifiers [!&"looked_up"; !&"inner.looked_up"; !&"implicit_looked_up"])
  in
  (* TODO: get rid of this; without it the compiler will not let us include a not-yet-used
     variant. *)
  let _ = LoadingStyle.LazyLookUpQualifiers [] in
  (* Adding new entirely new modules *)
  assert_incremental
    [
      { handle = "looked_up.py"; operation = FileOperation.Add };
      { handle = "inner/looked_up.py"; operation = FileOperation.Add };
      { handle = "not_looked_up.py"; operation = FileOperation.Add };
    ]
    ~expected:
      [Event.create_new_explicit "looked_up.py"; Event.create_new_explicit "inner/looked_up.py"];
  (* Updating modules *)
  assert_incremental
    [
      { handle = "looked_up.py"; operation = FileOperation.Update };
      { handle = "inner/looked_up.py"; operation = FileOperation.Update };
      { handle = "not_looked_up.py"; operation = FileOperation.Update };
    ]
    ~expected:
      [Event.create_new_explicit "looked_up.py"; Event.create_new_explicit "inner/looked_up.py"];
  (* Shadowing existing modules *)
  assert_incremental
    [
      { handle = "looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "looked_up/__init__.py"; operation = FileOperation.Add };
      { handle = "inner/looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "inner/looked_up.pyi"; operation = FileOperation.Add };
      { handle = "not_looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "not_looked_up.pyi"; operation = FileOperation.Add };
    ]
    ~expected:
      [
        Event.create_new_explicit "looked_up/__init__.py";
        Event.create_new_explicit "inner/looked_up.pyi";
      ];
  (* Un-shadowing existing modules *)
  assert_incremental
    [
      { handle = "looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "looked_up/__init__.py"; operation = FileOperation.Remove };
      { handle = "inner/looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "inner/looked_up.pyi"; operation = FileOperation.Remove };
      { handle = "not_looked_up.py"; operation = FileOperation.LeftAlone };
      { handle = "not_looked_up.pyi"; operation = FileOperation.Remove };
    ]
    ~expected:
      [Event.create_new_explicit "looked_up.py"; Event.create_new_explicit "inner/looked_up.py"];
  (* Removing modules *)
  assert_incremental
    [
      { handle = "looked_up.py"; operation = FileOperation.Remove };
      { handle = "inner/looked_up.py"; operation = FileOperation.Remove };
      { handle = "not_looked_up.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "looked_up"; Event.Delete "inner.looked_up"];
  (* New implicits *)
  assert_incremental
    [
      { handle = "implicit_looked_up/submodule.py"; operation = FileOperation.Add };
      { handle = "implicit_not_looked_up/submodule.py"; operation = FileOperation.Add };
    ]
    ~expected:[Event.NewImplicit "implicit_looked_up"];
  (* Removing implicits *)
  assert_incremental
    [
      { handle = "implicit_looked_up/submodule.py"; operation = FileOperation.Remove };
      { handle = "implicit_not_looked_up/submodule.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "implicit_looked_up"];
  ()


let test_invalidate_lazy_tracker_cache__removal context =
  let open Test in
  let open Pyre in
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration
      ~context
      ~local_tree:[TestFiles.File "package/a.py"; TestFiles.File "package/b.py"]
      ~external_tree:[]
  in
  let tracker =
    EnvironmentControls.create configuration ~use_lazy_module_tracking:true |> ModuleTracker.create
  in
  let read_only = ModuleTracker.read_only tracker in
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.a" >>| ModulePath.qualifier)
    (Some !&"package.a");
  (* Remove the entire directory *)
  PyrePath.create_relative ~root:local_root ~relative:"package"
  |> PyrePath.remove_contents_of_directory
  |> ignore;
  let events =
    ["package/a.py"]
    |> List.map ~f:(fun relative ->
           let path = PyrePath.create_relative ~root:local_root ~relative |> ArtifactPath.create in
           ArtifactPath.Event.(create ~kind:Kind.Unknown path))
  in
  (* Verify that the lazy tracker only updates the qualifier we actually looked up *)
  let updates = ModuleTracker.update tracker ~events in
  assert_equal
    ~ctxt:context
    ~printer:[%show: ModuleTracker.IncrementalUpdate.t list]
    [ModuleTracker.IncrementalUpdate.Delete !&"package.a"]
    updates;
  (* Verify that both the previously-read and non-previously-read modules no longer exist if we
     remove the entire directory. *)
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.a" >>| ModulePath.qualifier)
    None;
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.b" >>| ModulePath.qualifier)
    None;
  ()


let test_invalidate_lazy_tracker_cache__add context =
  let open Test in
  let open Pyre in
  (*
   * Case 1: modules in a nested directory
   *)
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration ~context ~local_tree:[TestFiles.File "package/a.py"] ~external_tree:[]
  in
  let tracker =
    EnvironmentControls.create configuration ~use_lazy_module_tracking:true |> ModuleTracker.create
  in
  let read_only = ModuleTracker.read_only tracker in
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.a" >>| ModulePath.qualifier)
    (Some !&"package.a");
  (* Add a second file next to a.py - the cached directory reads won't know about this *)
  let events =
    let path_to_b = PyrePath.create_relative ~root:local_root ~relative:"package/b.py" in
    let () = File.create path_to_b ~content:"" |> File.write in
    [(ArtifactPath.create path_to_b |> ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged))]
  in
  (* Verify that the lazy tracker does not update anything *)
  let updates = ModuleTracker.update tracker ~events in
  assert_equal ~ctxt:context ~printer:[%show: ModuleTracker.IncrementalUpdate.t list] [] updates;
  (* Verify that both the previously-read and non-previously-read modules no longer exist if we
     remove the entire directory. *)
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.a" >>| ModulePath.qualifier)
    (Some !&"package.a");
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"package.b" >>| ModulePath.qualifier)
    (Some !&"package.b");
  (*
   * Case 2: modules in the project top-level
   *)
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration ~context ~local_tree:[TestFiles.File "a.py"] ~external_tree:[]
  in
  let tracker =
    EnvironmentControls.create configuration ~use_lazy_module_tracking:true |> ModuleTracker.create
  in
  let read_only = ModuleTracker.read_only tracker in
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"a" >>| ModulePath.qualifier)
    (Some !&"a");
  (* Add a second file next to a.py - the cached directory reads won't know about this *)
  let events =
    let path_to_b = PyrePath.create_relative ~root:local_root ~relative:"b.py" in
    let () = File.create path_to_b ~content:"" |> File.write in
    [(ArtifactPath.create path_to_b |> ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged))]
  in
  (* Verify that the lazy tracker does not update anything *)
  let updates = ModuleTracker.update tracker ~events in
  assert_equal ~ctxt:context ~printer:[%show: ModuleTracker.IncrementalUpdate.t list] [] updates;
  (* Verify that both the previously-read and non-previously-read modules no longer exist if we
     remove the entire directory. *)
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"a" >>| ModulePath.qualifier)
    (Some !&"a");
  (* TODO(T130802871): this is a bug! We should have gotten a result here. *)
  assert_equal
    ~ctxt:context
    ~printer:[%show: Reference.t option]
    (ModuleTracker.ReadOnly.lookup_module_path read_only !&"b" >>| ModulePath.qualifier)
    (Some !&"b");
  ()


let make_overlay_testing_functions ~context ~configuration ~local_root ~parent_tracker =
  let tracker = ModuleTracker.read_only parent_tracker |> ModuleTracker.Overlay.create in
  let read_only = ModuleTracker.Overlay.read_only tracker in
  let overlay_owns qualifier = ModuleTracker.Overlay.owns_qualifier tracker qualifier in
  let assert_raw_code qualifier expected =
    let actual =
      Option.value_exn (ModuleTracker.ReadOnly.lookup_module_path read_only qualifier)
      |> ModuleTracker.ReadOnly.get_raw_code read_only
      |> Result.ok
      |> Option.value ~default:"Error loading code!"
    in
    assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected actual
  in
  let update_overlay_and_assert_result ~code_updates ~expected =
    let code_updates =
      let relative_to_artifact_path (relative, code) =
        let artifact_path = Test.relative_artifact_path ~root:local_root ~relative in
        artifact_path, code
      in
      List.map code_updates ~f:relative_to_artifact_path
    in
    let incremental_updates =
      ModuleTracker.Overlay.update_overlaid_code tracker ~code_updates
      |> List.sort ~compare:ModuleTracker.IncrementalUpdate.compare
    in
    let expected_incremental_updates =
      let relative_to_incremental_update relative =
        ModuleTracker.IncrementalUpdate.NewExplicit
          (create_module_path_exn ~configuration local_root relative)
      in
      List.map expected ~f:relative_to_incremental_update
      |> List.sort ~compare:ModuleTracker.IncrementalUpdate.compare
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%equal: ModuleTracker.IncrementalUpdate.t list]
      ~printer:[%show: ModuleTracker.IncrementalUpdate.t list]
      expected_incremental_updates
      incremental_updates
  in
  overlay_owns, assert_raw_code, update_overlay_and_assert_result


let test_overlay_basic context =
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration
      ~context
      ~local_tree:
        [TestFiles.File "code.py"; TestFiles.File "has_stub.py"; TestFiles.File "has_stub.pyi"]
      ~external_tree:[]
  in
  let overlay_owns, assert_raw_code, update_overlay_and_assert_result =
    let parent_tracker = EnvironmentControls.create configuration |> ModuleTracker.create in
    make_overlay_testing_functions ~context ~configuration ~local_root ~parent_tracker
  in
  let unsaved_content = "# unsaved_changes" in
  let open Test in
  (* Check read-only behavior prior to any update *)
  overlay_owns !&"code" |> assert_false;
  assert_raw_code !&"code" content_on_disk;
  overlay_owns !&"has_stub" |> assert_false;
  assert_raw_code !&"has_stub" content_on_disk;
  (* Run an update. Both modules should be registered as in the overlay *)
  update_overlay_and_assert_result
    ~code_updates:
      [
        "code.py", ModuleTracker.Overlay.CodeUpdate.NewCode unsaved_content;
        "has_stub.py", ModuleTracker.Overlay.CodeUpdate.NewCode unsaved_content;
      ]
    ~expected:["code.py"; "has_stub.py"];
  (* Check read-only behavior after update; the "has_stub.py" file should be masked by its stub *)
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" unsaved_content;
  overlay_owns !&"has_stub" |> assert_true;
  assert_raw_code !&"has_stub" content_on_disk;
  (* Run an update where we reset code.py to read from disk. It stays owned by the overlay because
     we have no efficient way to clear ownership should match the parent environment *)
  update_overlay_and_assert_result
    ~code_updates:["code.py", ModuleTracker.Overlay.CodeUpdate.ResetCode]
    ~expected:["code.py"];
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" content_on_disk;
  ()


let test_overlay_code_hiding context =
  let ({ Configuration.Analysis.local_root; _ } as configuration), _ =
    create_test_configuration ~context ~local_tree:[TestFiles.File "code.py"] ~external_tree:[]
  in
  let parent_tracker = EnvironmentControls.create configuration |> ModuleTracker.create in
  let overlay_owns, assert_raw_code, update_overlay_and_assert_result =
    make_overlay_testing_functions ~context ~configuration ~local_root ~parent_tracker
  in
  let unsaved_content_0 = "# unsaved_changes - edit 0" in
  let unsaved_content_1 = "# unsaved_changes - edit 1" in
  let update_stub_file_in_parent () =
    let _ =
      ModuleTracker.update
        parent_tracker
        ~events:
          [
            (Test.relative_artifact_path ~root:local_root ~relative:"code.pyi"
            |> ArtifactPath.Event.(create ~kind:Kind.Unknown));
          ]
    in
    ()
  in
  let create_stub_file_and_update_parent () =
    create_file local_root "code.pyi";
    update_stub_file_in_parent ()
  in
  let remove_stub_file_and_update_parent () =
    remove_file local_root "code.pyi";
    update_stub_file_in_parent ()
  in
  let open Test in
  (* Check behavior prior to any update *)
  overlay_owns !&"code" |> assert_false;
  assert_raw_code !&"code" content_on_disk;
  (* Add "code.py" to the overlay, and make sure we pick it up *)
  update_overlay_and_assert_result
    ~code_updates:["code.py", ModuleTracker.Overlay.CodeUpdate.NewCode unsaved_content_0]
    ~expected:["code.py"];
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" unsaved_content_0;
  (* Create a stub "code.pyi" on disk and update the parent
   *
   * Make sure that after this we read content from disk, even though we do still
   * consider the module part of the overlay *)
  create_stub_file_and_update_parent ();
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" content_on_disk;
  (* Update the overlaid code. It should still be shadowed by the parent *)
  update_overlay_and_assert_result
    ~code_updates:["code.py", ModuleTracker.Overlay.CodeUpdate.NewCode unsaved_content_1]
    ~expected:["code.py"];
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" content_on_disk;
  (* Delete the stub file and update the parent
   *
   * The overlay should go back to picking up in-memory code, and the edit that
   * happened while the module was shadowed should be there.
   *)
  remove_stub_file_and_update_parent ();
  overlay_owns !&"code" |> assert_true;
  assert_raw_code !&"code" unsaved_content_1;
  ()


let () =
  "environment"
  >::: [
         "module_path_create" >:: test_module_path_create;
         "module_path_search_path_subdirectory " >:: test_module_path_search_path_subdirectory;
         "module_path_exclude " >:: test_module_path_exclude;
         "module_path_directory_filter " >:: test_module_path_directory_filter;
         "module_path_directory_filter2 " >:: test_module_path_directory_filter2;
         "module_path_directory_filter3 " >:: test_module_path_directory_filter3;
         "module_path_overlapping " >:: test_module_path_overlapping;
         "module_path_overlapping2 " >:: test_module_path_overlapping2;
         "initialization" >:: test_initialization;
         "priority " >:: test_priority;
         "priority_multi_source_paths " >:: test_priority_multi_source_paths;
         "root_independence " >:: test_root_independence;
         "hidden_files " >:: test_hidden_files;
         "hidden_files2 " >:: test_hidden_files2;
         "namespace_modules " >:: test_namespace_modules;
         "stub_package_priority" >:: test_stub_package_priority;
         "update_new_files" >:: test_update_new_files;
         "update_remove_files" >:: test_update_remove_files;
         "update_changed_files" >:: test_update_changed_files;
         "update_implicits" >:: test_update_implicits;
         "update_lazy_tracker" >:: test_update_lazy_tracker;
         "invalidate_lazy_tracker_cache__removal" >:: test_invalidate_lazy_tracker_cache__removal;
         "invalidate_lazy_tracker_cache__add" >:: test_invalidate_lazy_tracker_cache__add;
         "overlay_basic" >:: test_overlay_basic;
         "overlay_code_hiding" >:: test_overlay_code_hiding;
       ]
  |> Test.run
