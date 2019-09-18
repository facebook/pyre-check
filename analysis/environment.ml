(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement

type t = { class_metadata_environment: ClassMetadataEnvironment.ReadOnly.t }

let class_metadata_environment { class_metadata_environment } = class_metadata_environment

let class_hierarchy_environment environment =
  class_metadata_environment environment
  |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment


let alias_environment environment =
  class_hierarchy_environment environment |> ClassHierarchyEnvironment.ReadOnly.alias_environment


module SharedMemory = struct
  (** Values *)
  module FunctionKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Function keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module GlobalKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Global keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module GlobalValue = struct
    type t = GlobalResolution.global

    let prefix = Prefix.make ()

    let description = "Global"

    let unmarshall value = Marshal.from_string value 0

    let compare = GlobalResolution.compare_global
  end

  module UndecoratedFunctionValue = struct
    type t = Type.t Type.Callable.overload [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Undecorated functions"

    let unmarshall value = Marshal.from_string value 0
  end

  (** Shared memory maps *)

  module Globals =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (GlobalValue)
  module UndecoratedFunctions =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (UndecoratedFunctionValue)

  module FunctionKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (FunctionKeyValue)
  (** Keys *)

  module GlobalKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (GlobalKeyValue)
end

module SharedMemoryDependencyHandler = struct
  open SharedMemory

  let add_new_key ~get ~add ~qualifier ~key =
    let existing = get qualifier in
    match existing with
    | None -> add qualifier [key]
    | Some keys -> add qualifier (key :: keys)


  let add_function_key ~qualifier reference =
    add_new_key ~qualifier ~key:reference ~get:FunctionKeys.get ~add:FunctionKeys.add


  let add_global_key ~qualifier global =
    add_new_key ~qualifier ~key:global ~get:GlobalKeys.get ~add:GlobalKeys.add


  let get_function_keys ~qualifier = FunctionKeys.get qualifier |> Option.value ~default:[]

  let get_global_keys ~qualifier = GlobalKeys.get qualifier |> Option.value ~default:[]

  let clear_keys_batch qualifiers =
    FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list qualifiers);
    GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list qualifiers)


  type table_keys = {
    undecorated_signatures: SharedMemory.UndecoratedFunctions.KeySet.t;
    globals: SharedMemory.Globals.KeySet.t;
  }

  let get_all_dependent_table_keys qualifiers =
    let global_keys =
      List.concat_map ~f:(fun qualifier -> get_global_keys ~qualifier) qualifiers
    in
    let function_keys =
      List.concat_map ~f:(fun qualifier -> get_function_keys ~qualifier) qualifiers
    in
    let open SharedMemory in
    {
      undecorated_signatures = UndecoratedFunctions.KeySet.of_list global_keys;
      (* We add a global name for each function definition as well. *)
      globals = Globals.KeySet.of_list (global_keys @ function_keys);
    }


  let find_added_dependent_table_keys qualifiers ~old_keys =
    let new_keys = get_all_dependent_table_keys qualifiers in
    {
      undecorated_signatures =
        UndecoratedFunctions.KeySet.diff
          new_keys.undecorated_signatures
          old_keys.undecorated_signatures;
      globals = Globals.KeySet.diff new_keys.globals old_keys.globals;
    }
end

let unannotated_global_environment environment =
  alias_environment environment |> AliasEnvironment.ReadOnly.unannotated_global_environment


let untracked_class_hierarchy_handler environment =
  let class_hierarchy_environment = class_hierarchy_environment environment in
  ( module struct
    let edges =
      ClassHierarchyEnvironment.ReadOnly.get_edges ?dependency:None class_hierarchy_environment


    let backedges key =
      ClassHierarchyEnvironment.ReadOnly.get_backedges class_hierarchy_environment key
      >>| ClassHierarchy.Target.Set.of_tree


    let contains =
      UnannotatedGlobalEnvironment.ReadOnly.class_exists
        ?dependency:None
        (unannotated_global_environment environment)
  end : ClassHierarchy.Handler )


let ast_environment environment =
  unannotated_global_environment environment
  |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


let resolution_implementation ?dependency environment () =
  let class_metadata_environment = class_metadata_environment environment in
  GlobalResolution.create
    ?dependency
    ~class_metadata_environment
    ~undecorated_signature:(SharedMemory.UndecoratedFunctions.get ?dependency)
    ~global:(SharedMemory.Globals.get ?dependency)
    (module Annotated.Class)


let resolution = resolution_implementation ?dependency:None

let dependency_tracked_resolution environment ~dependency () =
  resolution_implementation ~dependency environment ()


let register_global _ ?qualifier ~reference ~global =
  Option.iter qualifier ~f:(fun qualifier ->
      SharedMemoryDependencyHandler.add_global_key ~qualifier reference);
  SharedMemory.Globals.add reference global


let add_special_globals environment =
  (* Add builtin constants to globals. *)
  let annotation annotation =
    Annotation.create_immutable ~global:true annotation |> Node.create_with_default_location
  in
  register_global
    environment
    ?qualifier:None
    ~reference:(Reference.create "None")
    ~global:(annotation Type.none);
  register_global
    environment
    ?qualifier:None
    ~reference:(Reference.create "...")
    ~global:(annotation Type.Any);
  register_global
    environment
    ?qualifier:None
    ~reference:(Reference.create "__debug__")
    ~global:(annotation Type.bool)


let register_undecorated_functions environment (resolution : GlobalResolution.t) qualifier =
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children = function
      | { Node.value = Statement.Define _; _ } ->
          (* inner functions are not globals *)
          false
      | _ -> true


    let statement _ _ { Node.value; location } =
      let register ~reference ~annotation =
        SharedMemory.UndecoratedFunctions.add reference annotation
      in
      match value with
      | Statement.Class definition -> (
          let annotation =
            AnnotatedClass.create { Node.value = definition; location }
            |> AnnotatedClass.inferred_callable_type ~resolution
          in
          match annotation with
          | Some { Type.Callable.implementation; overloads = []; _ } ->
              register ~reference:definition.Class.name ~annotation:implementation
          | _ -> () )
      | Define ({ Define.signature = { Define.Signature.name; _ }; _ } as define) ->
          if Define.is_overloaded_method define then
            ()
          else
            register
              ~reference:name
              ~annotation:(Annotated.Callable.create_overload ~resolution define)
      | _ -> ()
  end)
  in
  let ast_environment = ast_environment environment in
  AstEnvironment.ReadOnly.get_source ast_environment qualifier |> Option.iter ~f:(Visit.visit ())


let register_values environment (resolution : GlobalResolution.t) qualifier =
  let handle ({ Source.statements; source_path = { SourcePath.qualifier; _ }; _ } as source) =
    let qualified_reference reference =
      let reference =
        let builtins = Reference.create "builtins" in
        if Reference.is_strict_prefix ~prefix:builtins reference then
          Reference.drop_prefix ~prefix:builtins reference
        else
          reference
      in
      Reference.sanitize_qualified reference
    in
    let module CollectCallables = Visit.MakeStatementVisitor (struct
      type t = Type.Callable.t Node.t list Reference.Map.t

      let visit_children = function
        | { Node.value = Statement.Define _; _ } ->
            (* inner functions are not globals *)
            false
        | _ -> true


      let statement { Source.source_path = { SourcePath.qualifier; _ }; _ } callables statement =
        let collect_callable ~name callables callable =
          SharedMemoryDependencyHandler.add_function_key ~qualifier name;

          (* Register callable global. *)
          let change callable = function
            | None -> Some [callable]
            | Some existing -> Some (existing @ [callable])
          in
          Map.change callables name ~f:(change callable)
        in
        match statement with
        | {
         Node.location;
         value = Statement.Define ({ Define.signature = { name; parent; _ }; _ } as define);
        } ->
            let parent =
              if Define.is_class_method define then
                parent
                >>= fun reference -> Some (Type.Primitive (Reference.show reference)) >>| Type.meta
              else
                None
            in
            Annotated.Callable.apply_decorators ~resolution ~location define
            |> (fun overload -> [Define.is_overloaded_method define, overload])
            |> Annotated.Callable.create ~resolution ~parent ~name:(Reference.show name)
            |> Node.create ~location
            |> collect_callable ~name callables
        | _ -> callables
    end)
    in
    let register_callables ~key ~data =
      assert (not (List.is_empty data));
      let location = List.hd_exn data |> Node.location in
      data
      |> List.map ~f:Node.value
      |> Type.Callable.from_overloads
      >>| (fun callable -> Type.Callable callable)
      >>| Annotation.create_immutable ~global:true
      >>| Node.create ~location
      >>| (fun global -> register_global environment ~qualifier ~reference:key ~global)
      |> ignore
    in
    CollectCallables.visit Reference.Map.empty source |> Map.iteri ~f:register_callables;

    (* Register meta annotations for classes. *)
    let module Visit = Visit.MakeStatementVisitor (struct
      type t = unit

      let visit_children _ = true

      let statement { Source.source_path = { SourcePath.qualifier; _ }; _ } _ = function
        | { Node.location; value = Statement.Class { Class.name; _ } } ->
            (* Register meta annotation. *)
            let primitive = Type.Primitive (Reference.show name) in
            let global =
              Annotation.create_immutable
                ~global:true
                ~original:(Some Type.Top)
                (Type.meta primitive)
              |> Node.create ~location
            in
            register_global environment ~qualifier ~reference:(qualified_reference name) ~global
        | _ -> ()
    end)
    in
    Visit.visit () source |> ignore;
    let rec visit statement =
      match statement with
      | { Node.value = Statement.If { If.body; orelse; _ }; _ } ->
          (* TODO(T28732125): Properly take an intersection here. *)
          List.iter ~f:visit body;
          List.iter ~f:visit orelse
      | { Node.value = Assign { Assign.target; annotation; value; _ }; _ } ->
          let explicit = Option.is_some annotation in
          let literal_annotation = GlobalResolution.resolve_literal resolution value in
          let annotation =
            annotation
            >>| Expression.delocalize
            >>| Type.create
                  ~aliases:(AliasEnvironment.ReadOnly.get_alias (alias_environment environment))
            >>= (fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation)
            |> Option.value ~default:literal_annotation
          in
          let rec register_assign ~target ~annotation =
            let register ~location reference annotation =
              let reference = qualified_reference (Reference.combine qualifier reference) in
              (* Don't register attributes or chained accesses as globals *)
              if Reference.length (Reference.drop_prefix ~prefix:qualifier reference) = 1 then
                let register_global global =
                  Node.create ~location global
                  |> fun global -> register_global environment ~qualifier ~reference ~global
                in
                let exists = Option.is_some (SharedMemory.Globals.get reference) in
                if explicit then
                  Annotation.create_immutable ~global:true annotation |> register_global
                else if not exists then
                  (* Treat literal globals as having been explicitly annotated. *)
                  let original =
                    if Type.is_partially_typed annotation then Some Type.Top else None
                  in
                  Annotation.create_immutable ~global:true ~original annotation |> register_global
                else
                  ()
            in
            match target.Node.value, annotation with
            | Name name, _ when Expression.is_simple_name name ->
                register
                  ~location:target.Node.location
                  (Expression.name_to_reference_exn name)
                  annotation
            | Tuple elements, Type.Tuple (Type.Bounded (Concrete parameters))
              when List.length elements = List.length parameters ->
                List.map2_exn
                  ~f:(fun target annotation -> register_assign ~target ~annotation)
                  elements
                  parameters
                |> ignore
            | Tuple elements, Type.Tuple (Type.Unbounded parameter) ->
                List.map ~f:(fun target -> register_assign ~target ~annotation:parameter) elements
                |> ignore
            | Tuple elements, _ ->
                List.map ~f:(fun target -> register_assign ~target ~annotation:Type.Top) elements
                |> ignore
            | _ -> ()
          in
          register_assign ~target ~annotation
      | _ -> ()
    in
    List.iter ~f:visit statements
  in
  let ast_environment = ast_environment environment in
  AstEnvironment.ReadOnly.get_source ast_environment qualifier |> Option.iter ~f:handle


let is_module environment = AstEnvironment.ReadOnly.is_module (ast_environment environment)

let transaction _ ?(only_global_keys = false) ~f () =
  let open SharedMemory in
  if only_global_keys then
    GlobalKeys.LocalChanges.push_stack ()
  else (
    FunctionKeys.LocalChanges.push_stack ();
    GlobalKeys.LocalChanges.push_stack ();
    Globals.LocalChanges.push_stack () );
  let result = f () in
  if only_global_keys then
    GlobalKeys.LocalChanges.commit_all ()
  else (
    FunctionKeys.LocalChanges.commit_all ();
    GlobalKeys.LocalChanges.commit_all ();
    Globals.LocalChanges.commit_all () );
  if only_global_keys then
    GlobalKeys.LocalChanges.pop_stack ()
  else (
    FunctionKeys.LocalChanges.pop_stack ();
    GlobalKeys.LocalChanges.pop_stack ();
    Globals.LocalChanges.pop_stack () );
  result


let check_class_hierarchy_integrity environment =
  let indices =
    UnannotatedGlobalEnvironment.ReadOnly.all_classes (unannotated_global_environment environment)
    |> Type.Primitive.Set.of_list
    |> IndexTracker.indices
    |> IndexTracker.Set.to_list
  in
  ClassHierarchy.check_integrity (untracked_class_hierarchy_handler environment) ~indices


let purge environment ?(debug = false) (qualifiers : Reference.t list) ~update_result:_ =
  let { SharedMemoryDependencyHandler.undecorated_signatures; globals } =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let open SharedMemory in
  (* Must disconnect backedges before deleting edges, since this relies on edges *)
  Globals.remove_batch globals;
  UndecoratedFunctions.remove_batch undecorated_signatures;

  if debug then (* If in debug mode, make sure the ClassHierarchy is still consistent. *)
    check_class_hierarchy_integrity environment


let update_and_compute_dependencies _ qualifiers ~update ~update_result:_ =
  let old_dependent_table_keys =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let { SharedMemoryDependencyHandler.undecorated_signatures; globals } =
    old_dependent_table_keys
  in
  let open SharedMemory in
  (* Backedges are not tracked *)
  SharedMemoryDependencyHandler.clear_keys_batch qualifiers;

  let update, mutation_triggers =
    SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
    |> UndecoratedFunctions.add_to_transaction ~keys:undecorated_signatures
    |> Globals.add_to_transaction ~keys:globals
    |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
  in
  let { SharedMemoryDependencyHandler.undecorated_signatures; globals } =
    SharedMemoryDependencyHandler.find_added_dependent_table_keys
      qualifiers
      ~old_keys:old_dependent_table_keys
  in
  let mutation_and_addition_triggers =
    [
      UndecoratedFunctions.get_all_dependents undecorated_signatures;
      Globals.get_all_dependents globals;
    ]
    |> List.fold ~init:mutation_triggers ~f:SharedMemoryKeys.ReferenceDependencyKey.KeySet.union
  in
  update, mutation_and_addition_triggers


let shared_memory_handler class_metadata_environment = { class_metadata_environment }

let shared_memory_hash_to_key_map ~qualifiers () =
  let extend_map map ~new_map =
    Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
  in
  let open SharedMemory in
  (* Handle-based keys. *)
  let map =
    FunctionKeys.compute_hashes_to_keys ~keys:qualifiers
    |> extend_map ~new_map:(GlobalKeys.compute_hashes_to_keys ~keys:qualifiers)
  in
  (* Globals and undecorated functions. *)
  let map =
    let keys = List.filter_map qualifiers ~f:GlobalKeys.get |> List.concat in
    extend_map map ~new_map:(Globals.compute_hashes_to_keys ~keys)
    |> extend_map ~new_map:(UndecoratedFunctions.compute_hashes_to_keys ~keys)
  in
  map


let serialize_decoded decoded =
  let open SharedMemory in
  match decoded with
  | Globals.Decoded (key, value) ->
      let value = value >>| Node.value >>| Annotation.sexp_of_t >>| Sexp.to_string in
      Some (GlobalValue.description, Reference.show key, value)
  | UndecoratedFunctions.Decoded (key, value) ->
      Some
        ( UndecoratedFunctionValue.description,
          Reference.show key,
          value >>| Type.Callable.show_overload Type.pp )
  | FunctionKeys.Decoded (key, value) ->
      Some
        ( FunctionKeyValue.description,
          Reference.show key,
          value >>| List.to_string ~f:Reference.show )
  | GlobalKeys.Decoded (key, value) ->
      Some
        (GlobalKeyValue.description, Reference.show key, value >>| List.to_string ~f:Reference.show)
  | _ -> None


let decoded_equal first second =
  let open SharedMemory in
  match first, second with
  | Globals.Decoded (_, first), Globals.Decoded (_, second) ->
      Some (Option.equal Annotation.equal (first >>| Node.value) (second >>| Node.value))
  | UndecoratedFunctions.Decoded (_, first), UndecoratedFunctions.Decoded (_, second) ->
      Some (Option.equal (Type.Callable.equal_overload Type.equal) first second)
  | FunctionKeys.Decoded (_, first), FunctionKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | GlobalKeys.Decoded (_, first), GlobalKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | _ -> None


let indices environment =
  unannotated_global_environment environment
  |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  |> Type.Primitive.Set.of_list
  |> IndexTracker.indices
  |> IndexTracker.Set.to_list


let class_hierarchy_json environment =
  ClassHierarchy.to_json
    (untracked_class_hierarchy_handler environment)
    ~indices:(indices environment)


let class_hierarchy_dot environment =
  ClassHierarchy.to_dot
    (untracked_class_hierarchy_handler environment)
    ~indices:(indices environment)
