(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
open Expression
open SharedMemoryKeys

module ResolvedReference = struct
  type export =
    | FromModuleGetattr
    | Exported of Module.Export.Name.t
  [@@deriving sexp, compare, hash]

  type t =
    | Module of Reference.t
    | ModuleAttribute of {
        from: Reference.t;
        name: Identifier.t;
        export: export;
        remaining: Identifier.t list;
      }
    | PlaceholderStub of {
        stub_module: Reference.t;
        remaining: Identifier.t list;
      }
  [@@deriving sexp, compare, hash]
end

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    class_exists: ?dependency:DependencyKey.registered -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
    all_indices: unit -> IndexTracker.t list;
    all_unannotated_globals: unit -> Reference.t list;
    all_defines: unit -> Reference.t list;
    all_defines_in_module: Reference.t -> Reference.t list;
    get_class_definition:
      ?dependency:DependencyKey.registered -> string -> ClassSummary.t Node.t option;
    get_unannotated_global:
      ?dependency:DependencyKey.registered -> Reference.t -> UnannotatedGlobal.t option;
    get_define: ?dependency:DependencyKey.registered -> Reference.t -> FunctionDefinition.t option;
    get_define_body: ?dependency:DependencyKey.registered -> Reference.t -> Define.t Node.t option;
    get_module_metadata: ?dependency:DependencyKey.registered -> Reference.t -> Module.t option;
    module_exists: ?dependency:SharedMemoryKeys.DependencyKey.registered -> Reference.t -> bool;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let unannotated_global_environment = Fn.id

  let class_exists { class_exists; _ } = class_exists

  let all_classes { all_classes; _ } = all_classes ()

  let all_indices { all_indices; _ } = all_indices ()

  let all_defines { all_defines; _ } = all_defines ()

  let all_unannotated_globals { all_unannotated_globals; _ } = all_unannotated_globals ()

  let get_class_definition { get_class_definition; _ } = get_class_definition

  let get_unannotated_global { get_unannotated_global; _ } = get_unannotated_global

  let get_define { get_define; _ } = get_define

  let get_define_body { get_define_body; _ } = get_define_body

  let all_defines_in_module { all_defines_in_module; _ } = all_defines_in_module

  let primitive_name annotation =
    let primitive, _ = Type.split annotation in
    Type.primitive_name primitive


  let is_protocol { get_class_definition; _ } ?dependency annotation =
    primitive_name annotation
    >>= get_class_definition ?dependency
    >>| Node.value
    >>| ClassSummary.is_protocol
    |> Option.value ~default:false


  let contains_untracked read_only ?dependency annotation =
    let is_tracked = class_exists read_only ?dependency in
    List.exists ~f:(fun annotation -> not (is_tracked annotation)) (Type.elements annotation)


  let get_module_metadata { get_module_metadata; _ } = get_module_metadata

  let module_exists { module_exists; _ } = module_exists

  let legacy_resolve_exports read_only ?dependency reference =
    (* Resolve exports. Fixpoint is necessary due to export/module name conflicts: P59503092 *)
    let widening_threshold = 25 in
    let rec resolve_exports_fixpoint ~reference ~visited ~count =
      if Set.mem visited reference || count > widening_threshold then
        reference
      else
        let rec resolve_exports ~lead ~tail =
          match tail with
          | head :: tail ->
              let incremented_lead = lead @ [head] in
              if
                Option.is_some
                  (get_module_metadata
                     ?dependency
                     read_only
                     (Reference.create_from_list incremented_lead))
              then
                resolve_exports ~lead:incremented_lead ~tail
              else
                get_module_metadata ?dependency read_only (Reference.create_from_list lead)
                >>| (fun definition ->
                      match Module.legacy_aliased_export definition (Reference.create head) with
                      | Some export -> Reference.combine export (Reference.create_from_list tail)
                      | _ -> resolve_exports ~lead:(lead @ [head]) ~tail)
                |> Option.value ~default:reference
          | _ -> reference
        in
        match Reference.as_list reference with
        | head :: tail ->
            let exported_reference = resolve_exports ~lead:[head] ~tail in
            if Reference.is_strict_prefix ~prefix:reference exported_reference then
              reference
            else
              resolve_exports_fixpoint
                ~reference:exported_reference
                ~visited:(Set.add visited reference)
                ~count:(count + 1)
        | _ -> reference
    in
    resolve_exports_fixpoint ~reference ~visited:Reference.Set.empty ~count:0


  module ResolveExportItem = struct
    module T = struct
      type t = {
        current_module: Reference.t;
        name: Identifier.t;
      }
      [@@deriving sexp, compare, hash]
    end

    include T
    include Hashable.Make (T)
  end

  let resolve_exports read_only ?dependency ?(from = Reference.empty) reference =
    let visited_set = ResolveExportItem.Hash_set.create () in
    let rec resolve_module_alias ~current_module ~names_to_resolve () =
      match get_module_metadata ?dependency read_only current_module with
      | None ->
          let rec resolve_placeholder_stub sofar = function
            | [] -> None
            | name :: prefixes -> (
                let checked_module = List.rev prefixes |> Reference.create_from_list in
                let sofar = name :: sofar in
                match get_module_metadata ?dependency read_only checked_module with
                | Some module_metadata when Module.empty_stub module_metadata ->
                    Some
                      (ResolvedReference.PlaceholderStub
                         { stub_module = checked_module; remaining = sofar })
                | _ -> resolve_placeholder_stub sofar prefixes)
          in
          (* Make sure none of the parent of `current_module` is placeholder stub *)
          resolve_placeholder_stub names_to_resolve (Reference.as_list current_module |> List.rev)
      | Some module_metadata -> (
          match Module.empty_stub module_metadata with
          | true ->
              Some
                (ResolvedReference.PlaceholderStub
                   { stub_module = current_module; remaining = names_to_resolve })
          | false -> (
              match names_to_resolve with
              | [] -> Some (ResolvedReference.Module current_module)
              | next_name :: rest_names -> (
                  let item = { ResolveExportItem.current_module; name = next_name } in
                  match Hash_set.strict_add visited_set item with
                  | Result.Error _ ->
                      (* Module alias cycle detected. Abort resolution. *)
                      None
                  | Result.Ok _ -> (
                      match Module.get_export module_metadata next_name with
                      | None -> (
                          match Module.get_export module_metadata "__getattr__" with
                          | Some Module.Export.(Name (Define { is_getattr_any = true })) ->
                              Some
                                (ResolvedReference.ModuleAttribute
                                   {
                                     from = current_module;
                                     name = next_name;
                                     export = ResolvedReference.FromModuleGetattr;
                                     remaining = rest_names;
                                   })
                          | _ ->
                              (* We could be hitting an implicit module, or we could be hitting an
                                 explicit module whose name is a prefix of another explicit module.
                                 Keep moving through the current reference chain to make sure we
                                 don't mis-handle those cases. *)
                              resolve_module_alias
                                ~current_module:
                                  (Reference.create next_name |> Reference.combine current_module)
                                ~names_to_resolve:rest_names
                                ())
                      | Some (Module.Export.NameAlias { from; name }) ->
                          if Reference.equal current_module from then
                            (* This could legitimately happen when an __init__ module trying to
                               import its sibling modules *)
                            resolve_module_alias
                              ~current_module:(Reference.create name |> Reference.combine from)
                              ~names_to_resolve:rest_names
                              ()
                          else
                            (* We don't know if `name` refers to a module or not. Move forward on
                               the alias chain. *)
                            resolve_module_alias
                              ~current_module:from
                              ~names_to_resolve:(name :: rest_names)
                              ()
                      | Some (Module.Export.Module name) ->
                          (* `name` is definitely a module. *)
                          resolve_module_alias ~current_module:name ~names_to_resolve:rest_names ()
                      | Some (Module.Export.Name export) ->
                          (* We find a non-module. *)
                          Some
                            (ResolvedReference.ModuleAttribute
                               {
                                 from = current_module;
                                 name = next_name;
                                 export = ResolvedReference.Exported export;
                                 remaining = rest_names;
                               })))))
    in
    resolve_module_alias ~current_module:from ~names_to_resolve:(Reference.as_list reference) ()


  let resolve_decorator_if_matches read_only ?dependency decorator ~target =
    match Decorator.from_expression decorator with
    | None -> None
    | Some ({ Ast.Statement.Decorator.name = { Node.value = name; location }; _ } as decorator) ->
        let resolved_name =
          match resolve_exports read_only ?dependency name with
          | Some (ResolvedReference.ModuleAttribute { from; name; remaining; _ }) ->
              Reference.create_from_list (name :: remaining) |> Reference.combine from
          | _ -> name
        in
        if String.equal (Reference.show resolved_name) target then
          Some { decorator with name = { Node.value = resolved_name; location } }
        else
          None


  let get_decorator
      read_only
      ?dependency
      { Node.value = { ClassSummary.decorators; _ }; _ }
      ~decorator
    =
    List.filter_map
      ~f:(resolve_decorator_if_matches read_only ?dependency ~target:decorator)
      decorators
end

(* The key tracking is necessary because there is no empirical way to determine which classes exist
   for a given class. This "fan-out" necessitates internal tracking. However, this module need not
   be sealed to ensure write only-ness since we're not dependency tracking this, since it's only
   used internally for doing our dependency analysis, and for all_classes, which is only used for
   all-or-nothing operations like validating the class hierarchy and printing it for debugging
   purposes *)
module KeyTracker = struct
  module ClassKeyValue = struct
    type t = Identifier.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module UnannotatedGlobalKeyValue = struct
    type t = Reference.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module FunctionKeyValue = struct
    type t = Reference.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "TypeCheckUnit keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)
  module UnannotatedGlobalKeys =
    Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (UnannotatedGlobalKeyValue)
  module FunctionKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (FunctionKeyValue)

  let get_keys keys =
    ClassKeys.KeySet.of_list keys
    |> ClassKeys.get_batch
    |> ClassKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat


  let get_unannotated_global_keys keys =
    UnannotatedGlobalKeys.KeySet.of_list keys
    |> UnannotatedGlobalKeys.get_batch
    |> UnannotatedGlobalKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat


  let get_define_body_keys keys =
    FunctionKeys.KeySet.of_list keys
    |> FunctionKeys.get_batch
    |> FunctionKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat
end

(* We want to ensure that we are only writing to this table in this phase, not creating internal
   dependencies with self-reads. Accordingly read_only should only be called by downstream clients *)
module WriteOnly : sig
  val set_class_definition : name:string -> definition:ClassSummary.t Node.t -> unit

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    previous_classes_list:string list ->
    previous_unannotated_globals_list:Reference.t list ->
    previous_defines_list:Reference.t list ->
    previous_modules_list:Reference.t list ->
    DependencyKey.Transaction.t

  val get_all_dependents
    :  class_additions:string list ->
    unannotated_global_additions:Reference.t list ->
    define_additions:Reference.t list ->
    DependencyKey.RegisteredSet.t

  val direct_data_purge
    :  previous_classes_list:Type.Primitive.t list ->
    previous_unannotated_globals_list:Reference.t list ->
    previous_defines_list:Reference.t list ->
    previous_modules_list:Reference.t list ->
    unit

  val set_unannotated_global : target:Reference.t -> UnannotatedGlobal.t -> unit

  val set_define : name:Reference.t -> FunctionDefinition.t -> unit

  val set_module_metadata : qualifier:Reference.t -> Module.t -> unit

  val read_only : ast_environment:AstEnvironment.ReadOnly.t -> ReadOnly.t
end = struct
  module ClassValue = struct
    type t = ClassSummary.t Node.t

    let prefix = Prefix.make ()

    let description = "Class"

    let unmarshall value = Marshal.from_string value 0

    let compare = Node.compare ClassSummary.compare
  end

  module ClassDefinitions =
    DependencyTrackedMemory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (DependencyKey)
      (ClassValue)

  module UnannotatedGlobalValue = struct
    type t = UnannotatedGlobal.t

    let prefix = Prefix.make ()

    let description = "UnannotatedGlobal"

    let unmarshall value = Marshal.from_string value 0

    let compare = UnannotatedGlobal.compare
  end

  module UnannotatedGlobals =
    DependencyTrackedMemory.DependencyTrackedTableNoCache
      (SharedMemoryKeys.ReferenceKey)
      (DependencyKey)
      (UnannotatedGlobalValue)

  module FunctionDefinitionValue = struct
    type t = FunctionDefinition.t

    let description = "Define"

    let prefix = Prefix.make ()

    let unmarshall value = Marshal.from_string value 0

    let compare = FunctionDefinition.compare
  end

  module FunctionDefinitions =
    DependencyTrackedMemory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (DependencyKey)
      (FunctionDefinitionValue)

  module ModuleMetadataValue = struct
    type t = Module.t

    let prefix = Prefix.make ()

    let description = "Module"

    let unmarshall value = Marshal.from_string value 0

    let compare = Module.compare
  end

  module ModuleMetadata =
    DependencyTrackedMemory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.DependencyKey)
      (ModuleMetadataValue)

  let set_unannotated_global ~target = UnannotatedGlobals.add target

  let set_class_definition ~name ~definition = ClassDefinitions.write_through name definition

  let set_define ~name definitions = FunctionDefinitions.write_through name definitions

  let set_module_metadata ~qualifier = ModuleMetadata.add qualifier

  let add_to_transaction
      transaction
      ~previous_classes_list
      ~previous_unannotated_globals_list
      ~previous_defines_list
      ~previous_modules_list
    =
    let class_keys = ClassDefinitions.KeySet.of_list previous_classes_list in
    let unannotated_globals_keys =
      UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    in
    let defines_keys = FunctionDefinitions.KeySet.of_list previous_defines_list in
    let module_keys = ModuleMetadata.KeySet.of_list previous_modules_list in
    ClassDefinitions.add_to_transaction ~keys:class_keys transaction
    |> UnannotatedGlobals.add_to_transaction ~keys:unannotated_globals_keys
    |> FunctionDefinitions.add_to_transaction ~keys:defines_keys
    |> ModuleMetadata.add_to_transaction ~keys:module_keys


  let get_all_dependents ~class_additions ~unannotated_global_additions ~define_additions =
    let function_and_class_dependents =
      DependencyKey.RegisteredSet.union
        (ClassDefinitions.KeySet.of_list class_additions |> ClassDefinitions.get_all_dependents)
        (FunctionDefinitions.KeySet.of_list define_additions
        |> FunctionDefinitions.get_all_dependents)
    in
    DependencyKey.RegisteredSet.union
      function_and_class_dependents
      (UnannotatedGlobals.KeySet.of_list unannotated_global_additions
      |> UnannotatedGlobals.get_all_dependents)


  let direct_data_purge
      ~previous_classes_list
      ~previous_unannotated_globals_list
      ~previous_defines_list
      ~previous_modules_list
    =
    ClassDefinitions.KeySet.of_list previous_classes_list |> ClassDefinitions.remove_batch;
    UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    |> UnannotatedGlobals.remove_batch;
    FunctionDefinitions.KeySet.of_list previous_defines_list |> FunctionDefinitions.remove_batch;
    ModuleMetadata.KeySet.of_list previous_modules_list |> ModuleMetadata.remove_batch


  let read_only ~ast_environment =
    let all_classes () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment |> KeyTracker.get_keys
    in
    let all_indices () =
      all_classes ()
      |> Type.Primitive.Set.of_list
      |> IndexTracker.indices
      |> IndexTracker.Set.to_list
    in
    let all_unannotated_globals () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_unannotated_global_keys
    in
    let all_defines () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_define_body_keys
    in
    let class_exists ?dependency name = ClassDefinitions.mem ?dependency name in
    let get_define = FunctionDefinitions.get in
    let get_define_body ?dependency key =
      FunctionDefinitions.get ?dependency key >>= fun { FunctionDefinition.body; _ } -> body
    in
    let all_defines_in_module qualifier = KeyTracker.get_define_body_keys [qualifier] in
    let get_module_metadata ?dependency qualifier =
      let qualifier =
        match Reference.as_list qualifier with
        | ["future"; "builtins"]
        | ["builtins"] ->
            Reference.empty
        | _ -> qualifier
      in
      match ModuleMetadata.get ?dependency qualifier with
      | Some _ as result -> result
      | None -> (
          match AstEnvironment.ReadOnly.is_module_tracked ast_environment qualifier with
          | true -> Some (Module.create_implicit ())
          | false -> None)
    in
    let module_exists ?dependency qualifier =
      let qualifier =
        match Reference.as_list qualifier with
        | ["future"; "builtins"]
        | ["builtins"] ->
            Reference.empty
        | _ -> qualifier
      in
      match ModuleMetadata.mem ?dependency qualifier with
      | true -> true
      | false -> AstEnvironment.ReadOnly.is_module_tracked ast_environment qualifier
    in
    {
      ast_environment;
      ReadOnly.get_class_definition = ClassDefinitions.get;
      all_classes;
      all_indices;
      all_defines;
      class_exists;
      get_unannotated_global = UnannotatedGlobals.get;
      get_define;
      get_define_body;
      all_defines_in_module;
      all_unannotated_globals;
      get_module_metadata;
      module_exists;
    }
end

let missing_builtin_classes, missing_typing_classes, missing_typing_extensions_classes =
  let make ?(bases = []) ?(metaclasses = []) ?(body = []) name =
    let create_base annotation =
      { Call.Argument.name = None; value = Type.expression annotation }
    in
    let create_metaclass annotation =
      {
        Call.Argument.name = Some (Node.create_with_default_location "metaclass");
        value = Type.expression annotation;
      }
    in
    {
      Class.name = Reference.create name;
      base_arguments = List.map bases ~f:create_base @ List.map metaclasses ~f:create_metaclass;
      body;
      decorators = [];
      top_level_unbound_names = [];
    }
    |> Node.create_with_default_location
  in
  let single_unary_generic =
    [Type.parametric "typing.Generic" [Single (Variable (Type.Variable.Unary.create "typing._T"))]]
  in
  let catch_all_generic = [Type.parametric "typing.Generic" [Single Any]] in
  let callable_body =
    [
      Statement.Assign
        {
          target =
            Node.create_with_default_location
              (Expression.Name
                 (Ast.Expression.create_name ~location:Location.any "typing.Callable.__call__"));
          annotation = Some (Type.expression Type.object_primitive);
          value = Node.create_with_default_location (Expression.Constant Constant.NoneLiteral);
        };
    ]
    |> List.map ~f:Node.create_with_default_location
  in
  let make_dunder_get ~parent ~host ~host_type ~return =
    let parent = Reference.create parent in
    Statement.Define
      {
        signature =
          {
            name = Reference.combine parent (Reference.create "__get__");
            parameters =
              [
                Node.create_with_default_location
                  { Ast.Expression.Parameter.name = "self"; value = None; annotation = None };
                Node.create_with_default_location
                  {
                    Ast.Expression.Parameter.name = "host";
                    value = None;
                    annotation = Some (Type.expression host);
                  };
                Node.create_with_default_location
                  {
                    Ast.Expression.Parameter.name = "host_type";
                    value =
                      Some
                        (Node.create_with_default_location
                           (Expression.Constant Constant.NoneLiteral));
                    annotation = Some (Type.expression host_type);
                  };
              ];
            decorators = [];
            return_annotation = Some (Type.expression return);
            async = false;
            generator = false;
            parent = Some parent;
            nesting_define = None;
          };
        captures = [];
        unbound_names = [];
        body = [];
      }
  in
  let classmethod_body =
    (*
     * _T = TypeVar("_T")
     * _S = TypeVar("_S")
     * class ClassMethod(Generic[_T]):
     *   def __get__(self, host: object, host_type: _S = None) -> BoundMethod[_T, _S]: ...
     *)
    [
      make_dunder_get
        ~parent:"typing.ClassMethod"
        ~host:Type.object_primitive
        ~host_type:(Variable (Type.Variable.Unary.create "typing._S"))
        ~return:
          (Type.parametric
             "BoundMethod"
             [
               Single (Variable (Type.Variable.Unary.create "typing._T"));
               Single (Variable (Type.Variable.Unary.create "typing._S"));
             ]);
    ]
    |> List.map ~f:Node.create_with_default_location
  in
  let staticmethod_body =
    (*
     * _T = TypeVar("_T")
     * class StaticMethod(Generic[_T]):
     *   def __get__(self, host: object, host_type: object = None) -> _T: ...
     *)
    [
      make_dunder_get
        ~parent:"typing.StaticMethod"
        ~host:Type.object_primitive
        ~host_type:Type.object_primitive
        ~return:(Variable (Type.Variable.Unary.create "typing._T"));
    ]
    |> List.map ~f:Node.create_with_default_location
  in
  let generic_meta_body =
    [
      Statement.Define
        {
          signature =
            {
              name = Reference.create "typing.GenericMeta.__getitem__";
              parameters =
                [
                  { Parameter.name = "cls"; value = None; annotation = None }
                  |> Node.create_with_default_location;
                  { Parameter.name = "arg"; value = None; annotation = None }
                  |> Node.create_with_default_location;
                ];
              decorators = [];
              return_annotation = None;
              async = false;
              generator = false;
              parent = Some (Reference.create "typing.GenericMeta");
              nesting_define = None;
            };
          captures = [];
          unbound_names = [];
          body = [];
        }
      |> Node.create_with_default_location;
    ]
  in

  let typing_classes =
    [
      make "typing.Optional" ~bases:single_unary_generic;
      make "typing.NoReturn";
      make "typing.Annotated" ~bases:catch_all_generic;
      make "typing.Protocol" ~bases:catch_all_generic;
      make "typing.Callable" ~bases:catch_all_generic ~body:callable_body;
      make "typing.FrozenSet" ~bases:single_unary_generic;
      make "typing.ClassVar" ~bases:single_unary_generic;
      make "typing.Final" ~bases:catch_all_generic;
      make "typing.Literal" ~bases:catch_all_generic;
      make "typing.Union" ~bases:catch_all_generic;
      make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic";
      make "typing.ClassMethod" ~bases:single_unary_generic ~body:classmethod_body;
      make "typing.StaticMethod" ~bases:single_unary_generic ~body:staticmethod_body;
      make "typing.GenericMeta" ~bases:[Primitive "type"] ~body:generic_meta_body;
      make "typing.TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    ]
  in
  let typing_extension_classes =
    [
      make "typing_extensions.Final";
      make "typing_extensions.Literal" ~bases:catch_all_generic;
      make "typing_extensions.Annotated" ~bases:catch_all_generic;
      make "typing_extensions.TypeAlias";
      make "typing_extensions.TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    ]
  in
  let builtin_classes =
    let t_self_expression =
      Expression.Name (Name.Identifier "TSelf") |> Node.create_with_default_location
    in
    [
      make
        ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
        (Type.TypedDictionary.class_name ~total:true);
      make
        ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
        (Type.TypedDictionary.class_name ~total:false);
      (* I think this may be actually covariant, covariant, but I don't think there's any value in
         going out on that limb yet *)
      make
        ~bases:
          [
            Type.parametric
              "typing.Generic"
              [Single (Type.variable "typing._T"); Single (Type.variable "typing._S")];
            Type.Primitive "typing.Callable";
          ]
        "BoundMethod";
    ]
  in
  builtin_classes, typing_classes, typing_extension_classes


let register_class_definitions ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  (* TODO (T57944324): Support checking classes that are nested inside function bodies *)
  let module ClassCollector = Visit.MakeStatementVisitor (struct
    type t = Class.t Node.t list

    let visit_children _ = true

    let statement _ sofar = function
      | { Node.location; value = Statement.Class definition } ->
          { Node.location; value = definition } :: sofar
      | _ -> sofar
  end)
  in
  let classes = ClassCollector.visit [] source in
  let classes =
    match Reference.as_list qualifier with
    | [] -> classes @ missing_builtin_classes
    | ["typing"] -> classes @ missing_typing_classes
    | ["typing_extensions"] -> classes @ missing_typing_extensions_classes
    | _ -> classes
  in
  let register new_annotations { Node.location; value = { Class.name; _ } as definition } =
    let primitive = Reference.show name in
    let definition =
      match primitive with
      | "type" ->
          let value =
            Type.expression (Type.parametric "typing.Generic" [Single (Type.variable "typing._T")])
          in
          { definition with Class.base_arguments = [{ name = None; value }] }
      | _ -> definition
    in
    WriteOnly.set_class_definition
      ~name:primitive
      ~definition:{ Node.location; value = ClassSummary.create ~qualifier definition };
    Set.add new_annotations primitive
  in
  List.fold classes ~init:Type.Primitive.Set.empty ~f:register
  |> Set.to_list
  |> KeyTracker.ClassKeys.add qualifier


let missing_builtin_globals =
  let assign name annotation =
    {
      UnannotatedGlobal.Collector.Result.name;
      unannotated_global =
        UnannotatedGlobal.SimpleAssign
          {
            explicit_annotation = Some (Type.expression annotation);
            target_location = Location.WithModule.any;
            value = Node.create_with_default_location (Expression.Constant Constant.Ellipsis);
          };
    }
  in
  [assign "..." Type.Any; assign "__debug__" Type.bool]


let collect_unannotated_globals ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  let write { UnannotatedGlobal.Collector.Result.name; unannotated_global } =
    let target = Reference.create name |> Reference.combine qualifier in
    WriteOnly.set_unannotated_global ~target unannotated_global;
    target
  in
  let merge_defines unannotated_globals_alist =
    let not_defines, defines =
      List.partition_map unannotated_globals_alist ~f:(function
          | { UnannotatedGlobal.Collector.Result.name; unannotated_global = Define defines } ->
              Either.Second (name, defines)
          | x -> Either.First x)
    in
    let add_to_map sofar (name, defines) =
      let merge_with_existing to_merge = function
        | None -> Some to_merge
        | Some existing -> Some (to_merge @ existing)
      in
      Map.change sofar name ~f:(merge_with_existing defines)
    in
    List.fold defines ~f:add_to_map ~init:Identifier.Map.empty
    |> Identifier.Map.to_alist
    |> List.map ~f:(fun (name, defines) ->
           {
             UnannotatedGlobal.Collector.Result.name;
             unannotated_global = Define (List.rev defines);
           })
    |> fun defines -> List.append defines not_defines
  in
  let drop_classes unannotated_globals =
    let is_not_class = function
      | { UnannotatedGlobal.Collector.Result.unannotated_global = Class; _ } -> false
      | _ -> true
    in
    List.filter unannotated_globals ~f:is_not_class
  in
  let globals = UnannotatedGlobal.Collector.from_source source |> merge_defines |> drop_classes in
  let globals =
    match Reference.as_list qualifier with
    | [] -> globals @ missing_builtin_globals
    | _ -> globals
  in
  globals |> List.map ~f:write |> KeyTracker.UnannotatedGlobalKeys.add qualifier


let collect_defines ({ Source.source_path = { SourcePath.qualifier; is_external; _ }; _ } as source)
  =
  match is_external with
  | true ->
      (* Do not collect function bodies for external sources as they won't get type checked *)
      ()
  | false ->
      let definitions = FunctionDefinition.collect_defines source in
      List.iter definitions ~f:(fun (name, definition) -> WriteOnly.set_define ~name definition);
      KeyTracker.FunctionKeys.add
        qualifier
        (List.map definitions ~f:fst |> List.sort ~compare:Reference.compare)


module UpdateResult = struct
  type t = {
    previous_classes: Type.Primitive.Set.t;
    previous_unannotated_globals: Reference.Set.t;
    previous_defines: Reference.Set.t;
    define_additions: Reference.Set.t;
    triggered_dependencies: DependencyKey.RegisteredSet.t;
    upstream: AstEnvironment.UpdateResult.t;
    read_only: ReadOnly.t;
  }

  type read_only = ReadOnly.t

  let previous_unannotated_globals { previous_unannotated_globals; _ } =
    previous_unannotated_globals


  let previous_defines { previous_defines; _ } = previous_defines

  let previous_classes { previous_classes; _ } = previous_classes

  let define_additions { define_additions; _ } = define_additions

  let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream

  let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
    [triggered_dependencies; AstEnvironment.UpdateResult.triggered_dependencies upstream]


  let unannotated_global_environment_update_result = Fn.id

  let ast_environment_update_result = upstream

  let read_only { read_only; _ } = read_only
end

type t = { ast_environment: AstEnvironment.t }

let create ast_environment = { ast_environment }

let ast_environment { ast_environment } = ast_environment

let read_only { ast_environment } =
  let ast_environment = AstEnvironment.read_only ast_environment in
  WriteOnly.read_only ~ast_environment


let update_this_and_all_preceding_environments { ast_environment } ~scheduler ~configuration trigger
  =
  let upstream = AstEnvironment.update ~configuration ~scheduler ast_environment trigger in
  let ast_environment = AstEnvironment.read_only ast_environment in
  let map sources =
    let register qualifier =
      AstEnvironment.ReadOnly.get_processed_source ~track_dependency:true ast_environment qualifier
      >>| (fun source ->
            WriteOnly.set_module_metadata ~qualifier (Module.create source);
            register_class_definitions source;
            collect_unannotated_globals source;
            collect_defines source)
      |> Option.value ~default:()
    in
    List.iter sources ~f:register
  in
  let modified_qualifiers = AstEnvironment.UpdateResult.invalidated_modules upstream in
  let update () =
    SharedMemoryKeys.DependencyKey.Registry.collected_iter
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~f:map
      ~inputs:modified_qualifiers
  in
  let previous_classes_list = KeyTracker.get_keys modified_qualifiers in
  let previous_classes = Type.Primitive.Set.of_list previous_classes_list in
  let previous_unannotated_globals_list =
    KeyTracker.get_unannotated_global_keys modified_qualifiers
  in
  let previous_unannotated_globals = Reference.Set.of_list previous_unannotated_globals_list in
  let previous_defines_list = KeyTracker.get_define_body_keys modified_qualifiers in
  let previous_defines = Reference.Set.of_list previous_defines_list in
  KeyTracker.ClassKeys.KeySet.of_list modified_qualifiers |> KeyTracker.ClassKeys.remove_batch;
  KeyTracker.UnannotatedGlobalKeys.KeySet.of_list modified_qualifiers
  |> KeyTracker.UnannotatedGlobalKeys.remove_batch;
  KeyTracker.FunctionKeys.KeySet.of_list modified_qualifiers |> KeyTracker.FunctionKeys.remove_batch;
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let define_additions, triggered_dependencies =
        Profiling.track_duration_and_shared_memory_with_dynamic_tags
          "TableUpdate(Unannotated globals)"
          ~f:(fun _ ->
            let (), mutation_triggers =
              DependencyKey.Transaction.empty ~scheduler ~configuration
              |> WriteOnly.add_to_transaction
                   ~previous_classes_list
                   ~previous_unannotated_globals_list
                   ~previous_defines_list
                   ~previous_modules_list:modified_qualifiers
              |> DependencyKey.Transaction.execute ~update
            in
            let current_classes =
              KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
            in
            let current_unannotated_globals =
              KeyTracker.get_unannotated_global_keys modified_qualifiers |> Reference.Set.of_list
            in
            let current_defines =
              KeyTracker.get_define_body_keys modified_qualifiers |> Reference.Set.of_list
            in
            let class_additions = Type.Primitive.Set.diff current_classes previous_classes in
            let unannotated_global_additions =
              Reference.Set.diff current_unannotated_globals previous_unannotated_globals
            in
            let define_additions = Reference.Set.diff current_defines previous_defines in
            let addition_triggers =
              WriteOnly.get_all_dependents
                ~class_additions:(Set.to_list class_additions)
                ~unannotated_global_additions:(Set.to_list unannotated_global_additions)
                ~define_additions:(Set.to_list define_additions)
            in
            let triggered_dependencies =
              DependencyKey.RegisteredSet.union addition_triggers mutation_triggers
            in
            let tags () =
              let triggered_dependencies_size =
                SharedMemoryKeys.DependencyKey.RegisteredSet.cardinal triggered_dependencies
                |> Format.sprintf "%d"
              in
              [
                "phase_name", "Global discovery";
                "number_of_triggered_dependencies", triggered_dependencies_size;
              ]
            in
            { Profiling.result = define_additions, triggered_dependencies; tags })
      in
      {
        UpdateResult.previous_classes;
        previous_unannotated_globals;
        previous_defines;
        define_additions;
        triggered_dependencies;
        upstream;
        read_only = WriteOnly.read_only ~ast_environment;
      }
  | _ ->
      let triggered_dependencies =
        Profiling.track_duration_and_shared_memory
          "LegacyTableUpdate(Unannotated globals)"
          ~tags:["phase_name", "global discovery"]
          ~f:(fun _ ->
            WriteOnly.direct_data_purge
              ~previous_classes_list
              ~previous_unannotated_globals_list
              ~previous_defines_list
              ~previous_modules_list:modified_qualifiers;
            update ();
            DependencyKey.RegisteredSet.empty)
      in
      {
        previous_classes;
        previous_unannotated_globals;
        previous_defines;
        define_additions = Reference.Set.empty;
        triggered_dependencies;
        upstream;
        read_only = WriteOnly.read_only ~ast_environment;
      }
