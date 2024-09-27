(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let test_simple_registration context =
  let assert_registers sources name ~expected_edges =
    let project = ScratchProject.setup sources ~include_typeshed_stubs:false ~context in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let expected_edges =
      Some
        {
          ClassHierarchy.Edges.parents =
            List.map
              ~f:(fun name -> { ClassHierarchy.Target.target = name; arguments = [] })
              expected_edges;
          generic_metadata = ClassHierarchy.GenericMetadata.NotGeneric;
        }
    in
    assert_equal
      ~cmp:[%compare.equal: ClassHierarchy.Edges.t option]
      ~printer:(fun edges -> [%sexp_of: ClassHierarchy.Edges.t option] edges |> Sexp.to_string_hum)
      expected_edges
      (ClassHierarchyEnvironment.ReadOnly.get_edges read_only name)
  in
  assert_registers ["test.py", {|
    class C:
      pass
  |}] "test.C" ~expected_edges:["object"];
  assert_registers
    ["test.py", {|
    class D:
     pass
    class C(D):
      pass
  |}]
    "test.C"
    ~expected_edges:["test.D"];
  ()


let test_parents_and_inferred_generic_base context =
  let assert_registers
      ~expected_parents
      ?(expected_generic_metadata = ClassHierarchy.GenericMetadata.NotGeneric)
      source
      name
    =
    let project = ScratchProject.setup ["test.py", source] ~context ~track_dependencies:true in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let create_target (name, concretes) =
      {
        ClassHierarchy.Target.target = name;
        arguments = List.map concretes ~f:(fun single -> Type.Argument.Single single);
      }
    in

    let expected =
      Some
        {
          ClassHierarchy.Edges.parents = List.map expected_parents ~f:create_target;
          generic_metadata = expected_generic_metadata;
        }
    in
    assert_equal
      ~cmp:[%compare.equal: ClassHierarchy.Edges.t option]
      ~printer:(fun edges -> [%sexp_of: ClassHierarchy.Edges.t option] edges |> Sexp.to_string_hum)
      expected
      (ClassHierarchyEnvironment.ReadOnly.get_edges read_only name)
  in
  assert_registers
    {|
       _T = typing.TypeVar('_T')
       class C:
         pass
     |}
    "test.C"
    ~expected_parents:["object", []];
  assert_registers
    {|
       _T = typing.TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(List[_T]):
         pass
     |}
    "test.C"
    ~expected_parents:["test.List", [Type.variable "test._T"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);
  assert_registers
    {|
       import typing
       _T = typing.TypeVar("_T")
       class List:
         pass
       class C(typing.Generic[_T], List):
         pass
     |}
    "test.C"
    ~expected_parents:["typing.Generic", [Type.variable "test._T"]; "test.List", []]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);
  assert_registers
    {|
       import typing
       _T = typing.TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(typing.Generic[_T], List[_T]):
         pass
     |}
    "test.C"
    ~expected_parents:["test.List", [Type.variable "test._T"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);
  assert_registers
    {|
       import typing
       _T = typing.TypeVar("_T")
       _U = typing.TypeVar("_U")
       class List(typing.Generic[_U]):
         pass
       class C(typing.Generic[_T], List[_T]):
         pass
     |}
    "test.C"
    ~expected_parents:["test.List", [Type.variable "test._T"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);
  assert_registers
    {|
       _T = typing.TypeVar("_T")
       class Iterable(typing.Generic[_T]):
         pass
       class List(Iterable[_T], typing.Generic[_T]):
         pass
     |}
    "test.List"
    ~expected_parents:
      ["test.Iterable", [Type.variable "test._T"]; "typing.Generic", [Type.variable "test._T"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       class Bar(Foo1[T1], Foo2[T2]): pass
     |}
    "test.Bar"
    ~expected_parents:
      ["test.Foo1", [Type.variable "test.T1"]; "test.Foo2", [Type.variable "test.T2"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       class Bar(typing.Generic[T1, T2], Foo1[T1], Foo2[T2]): pass
     |}
    "test.Bar"
    ~expected_parents:
      ["test.Foo1", [Type.variable "test.T1"]; "test.Foo2", [Type.variable "test.T2"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       class Bar(Foo1[T1], Foo2[T2], typing.Generic[T1, T2]): pass
     |}
    "test.Bar"
    ~expected_parents:
      [
        "test.Foo1", [Type.variable "test.T1"];
        "test.Foo2", [Type.variable "test.T2"];
        "typing.Generic", [Type.variable "test.T1"; Type.variable "test.T2"];
      ]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       # Note that Foo1 doesn't have type argument here
       class Bar(typing.Generic[T1, T2], Foo1, Foo2[T2]): pass
     |}
    "test.Bar"
    ~expected_parents:["test.Foo1", []; "test.Foo2", [Type.variable "test.T2"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       class Bar(Foo1[T1], typing.Generic[T1, T2], Foo2[T2]): pass
     |}
    "test.Bar"
    ~expected_parents:
      ["test.Foo1", [Type.variable "test.T1"]; "test.Foo2", [Type.variable "test.T2"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       # Note that Foo2 doesn't have type argument here
       class Bar(Foo1[T1], typing.Generic[T1, T2], Foo2): pass
     |}
    "test.Bar"
    ~expected_parents:
      [
        "test.Foo1", [Type.variable "test.T1"];
        "typing.Generic", [Type.variable "test.T1"; Type.variable "test.T2"];
        "test.Foo2", [];
      ]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T1 = typing.TypeVar("T1")
       T2 = typing.TypeVar("T2")
       class Foo1(typing.Generic[T1]): pass
       class Foo2(typing.Generic[T2]): pass
       class Bar(typing.Generic[T1, T2], Foo1, Foo2): pass
     |}
    "test.Bar"
    ~expected_parents:
      [
        "typing.Generic", [Type.variable "test.T1"; Type.variable "test.T2"];
        "test.Foo1", [];
        "test.Foo2", [];
      ]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
       import typing
       T = typing.TypeVar("T")
       class Foo(typing.Generic[T]): pass
       class Bar(typing.Protocol[T], Generic[T], Foo[T]): pass
     |}
    "test.Bar"
    ~expected_parents:
      ["typing.Protocol", [Type.variable "test.T"]; "test.Foo", [Type.variable "test.T"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test.T";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Dict(typing.Generic[_T1, _T2]):
        pass
      class Foo(Dict[_T1, _T2]): pass
    |}
    "test.Foo"
    ~expected_parents:["test.Dict", [Type.variable "test._T1"; Type.variable "test._T2"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T2";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  assert_registers
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Dict(typing.Generic[_T1, _T2]):
        pass
      class Foo(Dict[_T1, _T1]): pass
    |}
    "test.Foo"
    ~expected_parents:["test.Dict", [Type.variable "test._T1"; Type.variable "test._T1"]]
    ~expected_generic_metadata:
      (ClassHierarchy.GenericMetadata.GenericBase
         [
           Type.GenericParameter.GpTypeVar
             {
               name = "test._T1";
               variance = Type.Record.PreInferenceVariance.P_Invariant;
               constraints = Type.Record.TypeVarConstraints.Unconstrained;
             };
         ]);

  ()


let test_updates context =
  let assert_updates
      ?original_source
      ?new_source
      ~middle_actions
      ~expected_triggers
      ?post_actions
      ()
    =
    Memory.reset_shared_memory ();
    let sources = original_source >>| (fun source -> "test.py", source) |> Option.to_list in
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~track_dependencies:true
        ~in_memory:false
        sources
        ~context
    in
    let configuration = ScratchProject.configuration_of project in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let execute_action = function
      | `Edges (class_name, dependency, expectation) ->
          let expectation =
            Option.map expectation ~f:(fun expectation ->
                {
                  ClassHierarchy.Edges.parents =
                    List.map expectation ~f:(fun name ->
                        { ClassHierarchy.Target.target = name; arguments = [] });
                  generic_metadata = ClassHierarchy.GenericMetadata.NotGeneric;
                })
          in
          ClassHierarchyEnvironment.ReadOnly.get_edges read_only ~dependency class_name
          |> assert_equal
               ~cmp:[%compare.equal: ClassHierarchy.Edges.t option]
               ~printer:(fun edges ->
                 [%sexp_of: ClassHierarchy.Edges.t option] edges |> Sexp.to_string_hum)
               expectation
    in
    List.iter middle_actions ~f:execute_action;
    if Option.is_some original_source then
      ScratchProject.delete_from_local_root project ~relative:"test.py";
    Option.iter new_source ~f:(ScratchProject.add_to_local_root project ~relative:"test.py");
    let update_result =
      let { Configuration.Analysis.local_root; _ } = configuration in
      List.map
        ["test.py", ()]
        ~f:(fun (relative, _) ->
          Test.relative_artifact_path ~root:local_root ~relative
          |> ArtifactPath.Event.(create ~kind:Kind.Unknown))
      |> ScratchProject.update_environment project
      |> ErrorsEnvironment.Testing.UpdateResult.class_hierarchy_environment
    in
    let printer set =
      SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
      |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers =
      SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (ClassHierarchyEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class C:
        pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, None)]
    ();

  (* Class definition changes trigger *)
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class D:
       pass
      class C(D):
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["test.D"])]
    ();

  (* Class attributes do not trigger *)
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class C:
       x: int = 9
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();

  (* Alias changes trigger *)
  assert_updates
    ~original_source:
      {|
      class First:
       pass
      class Second:
       pass
      Alias = First
      class C(Alias):
        pass
    |}
    ~new_source:
      {|
      class First:
       pass
      class Second:
       pass
      Alias = Second
      class C(Alias):
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["test.First"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["test.Second"])]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      class C:
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();
  ()


let ( !! ) concretes = List.map concretes ~f:(fun single -> Type.Argument.Single single)

let test_compute_inferred_generic_base context =
  let assert_inferred_generic ~target source expected =
    let qualifier = Reference.create "test" in
    let project = ScratchProject.setup ~context ["test.py", source] in
    let source =
      SourceCodeApi.source_of_qualifier
        (Test.ScratchProject.get_untracked_source_code_api project)
        qualifier
    in
    let source = Option.value_exn source in
    let { Source.statements; _ } = source in
    let target =
      let target = function
        | {
            Node.location;
            value = Ast.Statement.Statement.Class ({ Statement.Class.name; _ } as definition);
          }
          when String.equal (Reference.show name) target ->
            Some { Node.location; value = definition }
        | _ -> None
      in
      List.find_map ~f:target statements
      |> Option.value_exn
      |> fun { Node.value; _ } -> ClassSummary.create ~qualifier value
    in
    let resolution = Test.ScratchProject.build_global_resolution project in
    let parse_annotation =
      GlobalResolution.parse_annotation ~validation:ValidatePrimitives resolution
    in
    let { ClassSummary.bases = { base_classes; _ }; _ } = target in
    let actual =
      List.map base_classes ~f:parse_annotation |> ClassHierarchyEnvironment.compute_generic_base
    in
    assert_equal
      ~printer:[%show: Expression.t option]
      ~cmp:[%compare.equal: Expression.t option]
      (Option.map expected ~f:Type.expression)
      (Option.map actual ~f:Type.expression)
  in
  assert_inferred_generic
    ~target:"test.C"
    {|
       _T = typing.TypeVar('_T')
       class C:
         pass
     |}
    None;
  assert_inferred_generic
    ~target:"test.C"
    {|
       _T = typing.TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(List[_T]):
         pass
     |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test._T"]));
  assert_inferred_generic
    ~target:"test.List"
    {|
       _T = TypeVar("_T")
       class Iterable(typing.Generic[_T]):
         pass
       class List(Iterable[_T], typing.Generic[_T]):
         pass
     |}
    None;
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Foo(typing.Dict[_T1, _T2]): pass
    |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test._T1"; Type.variable "test._T2"]));
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      _T1 = typing.TypeVar('_T1')
      class Foo(typing.Dict[_T1, _T1]): pass
    |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test._T1"]));
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      TParams = pyre_extensions.ParameterSpecification("TParams")
      class Base(typing.Generic[TParams]): pass
      class Foo(Base[TParams]): pass
    |}
    (Some
       (Type.parametric
          "typing.Generic"
          [
            Type.Argument.CallableParameters
              (Type.Variable.ParamSpec.self_reference
                 (Type.Variable.ParamSpec.create "test.TParams"));
          ]));
  assert_inferred_generic
    ~target:"test.Child"
    {|
      Ts = typing.TypeVarTuple("Ts")

      class Base(typing.Generic[typing.Unpack[Ts]]): ...

      class Child(Base[typing.Unpack[Ts]]): ...
    |}
    (Some
       (Type.parametric
          "typing.Generic"
          [
            Unpacked
              (Type.OrderedTypes.Concatenation.create_unpackable
                 (Type.Variable.TypeVarTuple.create "test.Ts"));
          ]));
  assert_inferred_generic
    ~target:"test.Child"
    {|
      Ts = typing_extensions.TypeVarTuple("Ts")

      class Base(typing.Generic[typing_extensios.Unpack[Ts]]): ...

      class Child(Base[typing_extensions.Unpack[Ts]]): ...
    |}
    (Some
       (Type.parametric
          "typing.Generic"
          [
            Unpacked
              (Type.OrderedTypes.Concatenation.create_unpackable
                 (Type.Variable.TypeVarTuple.create "test.Ts"));
          ]));
  assert_inferred_generic
    ~target:"test.Child"
    {|
      Ts = pyre_extensions.TypeVarTuple("Ts")

      class Base(typing.Generic[pyre_extensions.Unpack[Ts]]): ...

      class Child(Base[pyre_extensions.Unpack[Ts]]): ...
    |}
    (Some
       (Type.parametric
          "typing.Generic"
          [
            Unpacked
              (Type.OrderedTypes.Concatenation.create_unpackable
                 (Type.Variable.TypeVarTuple.create "test.Ts"));
          ]));

  (* We should not sort the generic variables in alphabetical order. *)
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      from typing import Dict, TypeVar

      A = TypeVar("A")
      B = TypeVar("B")

      class Foo(Dict[B, A]): ...
    |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test.B"; Type.variable "test.A"]));
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      from typing import Generic, TypeVar

      A = TypeVar("A", bound=str)
      B = TypeVar("B", bound=int)

      class BaseAB(Generic[A, B]): ...
      class BaseBA(Generic[B, A]): ...

      class Foo(BaseAB[A, B], BaseBA[B, A]): ...
    |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test.A"; Type.variable "test.B"]));
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      from typing import Dict, Generic, TypeVar

      A = TypeVar("A", bound=str)
      B = TypeVar("B", bound=int)

      class BaseAB(Generic[A, B]): ...

      class Foo(BaseAB[Dict[A, B], A]): ...
    |}
    (Some (Type.parametric "typing.Generic" !![Type.variable "test.A"; Type.variable "test.B"]));
  (* This is actually illegal in Python, but Pyre currently lacks the capability to detect it *)
  assert_inferred_generic
    ~target:"test.Foo"
    {|
      from typing import Generic, TypeVar

      class Base: pass
      T = TypeVar("T")
      class Foo(Base, metaclass=Generic[T]): ...
    |}
    None;
  ()


let () =
  Test.sanitized_module_name __MODULE__
  >::: [
         "simple_registration" >:: test_simple_registration;
         "parents_and_inferred_generic_bases" >:: test_parents_and_inferred_generic_base;
         "compute_inferred_generic_base" >:: test_compute_inferred_generic_base;
         "updates" >:: test_updates;
       ]
  |> Test.run
