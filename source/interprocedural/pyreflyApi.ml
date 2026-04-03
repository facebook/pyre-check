(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Module that implements the PyrePysaApi using the results from a pyrefly run with
   --report-pysa. *)

open Core
open Pyre
open Data_structures
open Ast
module Pyre1Api = Analysis.PyrePysaEnvironment
module PysaType = Pyre1Api.PysaType
module AstResult = Pyre1Api.AstResult
module FunctionParameter = Pyre1Api.ModelQueries.FunctionParameter
module FunctionParameters = Pyre1Api.ModelQueries.FunctionParameters
module FunctionSignature = Pyre1Api.ModelQueries.FunctionSignature
module AccessPath = Analysis.TaintAccessPath
module SysInfo = Analysis.PyrePysaEnvironment.SysInfo

(* Types re-exported from PyreflyReport *)
module FormatError = PyreflyReport.FormatError
module Error = PyreflyReport.Error

exception PyreflyFileFormatError = PyreflyReport.PyreflyFileFormatError

module ModulePath = PyreflyReport.ModulePath
module ModuleId = PyreflyReport.ModuleId
module LocalClassId = PyreflyReport.LocalClassId
module GlobalClassId = PyreflyReport.GlobalClassId
module GlobalClassIdSharedMemoryKey = PyreflyReport.GlobalClassIdSharedMemoryKey
module LocalFunctionId = PyreflyReport.LocalFunctionId
module GlobalCallableId = PyreflyReport.GlobalCallableId
module PyreflyTarget = PyreflyReport.PyreflyTarget
module ModuleIdSharedMemoryKey = PyreflyReport.ModuleIdSharedMemoryKey
module GlobalCallableIdSharedMemoryKey = PyreflyReport.GlobalCallableIdSharedMemoryKey
module ModuleQualifier = PyreflyReport.ModuleQualifier
module ModuleQualifierSharedMemoryKey = PyreflyReport.ModuleQualifierSharedMemoryKey
module ModuleInfoFilename = PyreflyReport.ModuleInfoFilename
module ProjectFile = PyreflyReport.ProjectFile
module PyreflyType = PyreflyReport.PyreflyType
module ClassFieldDeclarationKind = PyreflyReport.ClassFieldDeclarationKind
module CapturedVariable = PyreflyReport.CapturedVariable
module ModuleDefinitionsFile = PyreflyReport.ModuleDefinitionsFile
module ModuleTypeOfExpressions = PyreflyReport.ModuleTypeOfExpressions
module ModuleCallGraphs = PyreflyReport.ModuleCallGraphs
module TypeErrors = PyreflyReport.TypeErrors

(* Information about a module, stored in shared memory. *)
module ModuleInfosSharedMemory = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      absolute_source_path: ArtifactPath.t option;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
      pyrefly_info_filename: ModuleInfoFilename.t option;
      sys_info: SysInfo.t;
      is_test: bool; (* Is this a test file? *)
      is_stub: bool; (* Is this a stub file (e.g, `a.pyi`)? *)
      is_internal: bool; (* Is this an internal module (within the project's source directories)? *)
    }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (ModuleQualifierSharedMemoryKey)
      (struct
        type t = Module.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly module infos"
      end)
end

(* List of module qualifiers, stored in shared memory. *)
module QualifiersSharedMemory = struct
  module Value = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      has_source: bool;
      has_info: bool;
    }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
      (Memory.SingletonKey)
      (struct
        type t = Value.t list

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly all modules"
      end)
end

(* Type of expression at a given module qualifier and location, stored in shared memory. *)
module TypeOfExpressionsSharedMemory = struct
  module Key = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      location: Location.t;
    }
    [@@deriving compare, sexp]

    let to_string key = key |> sexp_of_t |> Core.Sexp.to_string
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (Key)
      (struct
        type t = PysaType.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly type of expressions"
      end)
end

(* Unique identifier for a class or define (e.g, `def foo(): ..`) in a module.
 *
 * - In most cases, this will be a dotted path such as `my.module.MyClass.foo`.
 * - Property setters have a suffix `@setter` to be different from property getters.
 * - The code at the top level of a module is considered part of an implicit function called `$toplevel`.
 * - The code within a class scope is considered part of an implicit function called `$class_toplevel`.
 * - The name might have a suffix with an index such as `$1`, `$2` to differentiate multiple definitions
 *   with the same name.
 * - If the name of the class or define might clash with another definition in a different module, we will add a
 *   separator between the module name and local name, such as `my.module#MyClass.foo`. *)
module FullyQualifiedName : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create
    :  module_qualifier:ModuleQualifier.t ->
    local_name:string list ->
    add_module_separator:bool ->
    t

  val create_module_toplevel : module_qualifier:ModuleQualifier.t -> t

  val create_class_toplevel : t -> t

  val to_reference : t -> Reference.t

  (* This is marked `unchecked` because it doesn't actually validate that the reference is a valid
     fully qualifide name. *)
  val from_reference_unchecked : Reference.t -> t

  val last : t -> string

  val prefix : t -> t option

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    (* Same as ModuleQualifier, it is stored as a reference, but it doesn't really make sense
       either. *)
    type t = Reference.t [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let create ~module_qualifier ~local_name ~add_module_separator =
    let () =
      (* Sanity check *)
      if List.exists local_name ~f:(fun s -> String.contains s '#' || String.contains s ':') then
        failwith "unexpected: local name contains an invalid character (`:#`)"
    in
    if not add_module_separator then
      Reference.combine
        (ModuleQualifier.to_reference module_qualifier)
        (Reference.create_from_list local_name)
    else
      Format.asprintf
        "%a#%a"
        Reference.pp
        (ModuleQualifier.to_reference module_qualifier)
        Reference.pp
        (Reference.create_from_list local_name)
      |> Reference.create


  let create_module_toplevel ~module_qualifier =
    create
      ~module_qualifier
      ~local_name:[Ast.Statement.toplevel_define_name]
      ~add_module_separator:false


  let create_class_toplevel name =
    Reference.combine name (Reference.create_from_list [Ast.Statement.class_toplevel_define_name])


  let to_reference = Fn.id

  let from_reference_unchecked = Fn.id

  let last = Reference.last

  let prefix = Reference.prefix

  module Map = Map.Make (T)
end

module TypeshedClass = struct
  type t = {
    sys_info: SysInfo.t;
    global_class_id: GlobalClassId.t;
    fully_qualified_name: FullyQualifiedName.t;
  }
end

module FullyQualifiedNameSharedMemoryKey = struct
  type t = FullyQualifiedName.t [@@deriving compare]

  let to_string key =
    key |> FullyQualifiedName.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string
end

module NameLocation = struct
  type t =
    | DefineName of
        Location.t (* Location of the name AST node, i.e location of `foo` in `def foo():` *)
    | ModuleTopLevel
    | ClassName of
        Location.t (* Location of the class AST node, i.e location of `Foo` in `class Foo:` *)
    | UnknonwnForClassField
  [@@deriving show]
end

module CallableMetadata = struct
  type t = {
    (* TODO(T225700656): This should be ModuleQualifier.t, but it is a Reference.t because it is
       exposed publicly. *)
    module_qualifier: Reference.t;
    name_location: NameLocation.t;
    is_overload: bool;
    is_staticmethod: bool;
    is_classmethod: bool;
    is_property_getter: bool;
    is_property_setter: bool;
    is_toplevel: bool; (* Is this the body of a module? *)
    is_class_toplevel: bool; (* Is this the body of a class? *)
    is_stub_define: bool; (* Is this a stub definition, e.g `def foo(): ...`. *)
    is_def_statement: bool; (* Is this associated with a `def ..` statement? *)
    parent_is_class: bool;
  }
  [@@deriving show]

  let get_method_kind { is_staticmethod; is_classmethod; parent_is_class; _ } =
    let open Analysis.PyrePysaEnvironment in
    if is_staticmethod then
      Some MethodKind.Static
    else if is_classmethod then
      Some MethodKind.Class
    else if parent_is_class then
      Some MethodKind.Instance
    else
      None
end

let assert_shared_memory_key_exists message = function
  | Some value -> value
  | None -> failwith (Format.sprintf "unexpected: %s" (message ()))


let strip_invalid_locations =
  List.filter ~f:(fun { Node.location; _ } -> not (Location.equal Location.any location))


module CallableMetadataSharedMemory = struct
  module Value = struct
    type t = {
      metadata: CallableMetadata.t;
      local_function_id: LocalFunctionId.t;
      (* This is the original name, without the fully qualified suffixes like `$2` or `@setter`. *)
      name: string;
      overridden_base_method: GlobalCallableId.t option;
      defining_class: GlobalClassId.t option;
      (* The list of callees for each decorator *)
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
      captured_variables: CapturedVariable.t list;
    }

    let _unused_fields { name = _; defining_class = _; _ } = ()
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Value.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly callable metadata"
      end)
end

module ClassMetadataSharedMemory = struct
  module Metadata = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      (* Location of the name AST node, i.e location of `Foo` in `class Foo():` *)
      name_location: Location.t;
      local_class_id: LocalClassId.t;
      (* True if this class was synthesized (e.g., from namedtuple), false if from actual `class X:`
         statement *)
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      (* For a given class, its list of immediate parents. Empty if the class has no parents (it is
         implicitly ['object']) *)
      parents: GlobalClassId.t list;
      (* For a given class, its resolved MRO (Method Resolution Order). *)
      mro: ModuleDefinitionsFile.ClassMro.t;
      (* The list of callees for each decorator *)
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving show]

    let _ = pp
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Metadata.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly class metadata"
      end)
end

module ModuleCallablesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = FullyQualifiedName.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly callables in module"
    end)

module ModuleClassesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = FullyQualifiedName.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly classes in module"
    end)

module GlobalVariable = struct
  type t = {
    type_: PysaType.t option;
    location: Location.t;
  }
  [@@deriving equal, compare, show]

  let _ = pp
end

module ModuleGlobalsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = GlobalVariable.t SerializableStringMap.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly global variables in module"
    end)

module ModuleIdToQualifierSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (ModuleIdSharedMemoryKey)
      (struct
        type t = ModuleQualifier.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly module id to module qualified"
      end)

  let get_module_qualifier handle module_id =
    module_id |> get handle |> assert_shared_memory_key_exists (fun () -> "unknown module id")
end

module ClassIdToQualifiedNameSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (GlobalClassIdSharedMemoryKey)
      (struct
        type t = FullyQualifiedName.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly class id to fully qualified name"
      end)

  let get_class_name handle class_id =
    class_id |> get handle |> assert_shared_memory_key_exists (fun () -> "unknown class id")
end

module CallableIdToQualifiedNameSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (GlobalCallableIdSharedMemoryKey)
      (struct
        type t = FullyQualifiedName.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly callable id to fully qualified name"
      end)

  let get_opt = get

  let get handle id =
    get_opt handle id |> assert_shared_memory_key_exists (fun () -> "unknown callable id")
end

module ClassField = struct
  type t = {
    type_: PysaType.t;
    explicit_annotation: string option;
    location: Location.t option;
    declaration_kind: ClassFieldDeclarationKind.t option;
  }
  [@@deriving equal, compare, show]

  let _ = pp
end

module ClassFieldsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = ClassField.t SerializableStringMap.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly class fields"
    end)

module PysaClassSummary = struct
  type t = {
    class_name: FullyQualifiedName.t;
    metadata: ClassMetadataSharedMemory.Metadata.t;
  }
end

module CallableAstSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Statement.Define.t Ast.Node.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly ast of callables"
    end)

(* Define signature of each callable, resulting from parsing the source file. *)
module CallableDefineSignatureSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Statement.Define.Signature.t Ast.Node.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly define signature of callables"
    end)

module CallableParseResultSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = unit AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly callable parse result"
    end)

(* Considered as stub:
 * - Stub functions, i.e when the body is an ellipsis `def foo(): ...`
 * - Functions in a module considered a unit test module
 * - Synthesized functions that we don't have the code for (for instance,
 *   generated `__init__` of a dataclass)
 *)
let is_stub_like_from_metadata
    ~metadata:{ CallableMetadata.is_stub_define; _ }
    ~callable_parse_result_shared_memory
    fully_qualified_name
  =
  is_stub_define
  ||
  let parse_result =
    CallableParseResultSharedMemory.get callable_parse_result_shared_memory fully_qualified_name
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf
             "missing callable parse result: `%a`"
             FullyQualifiedName.pp
             fully_qualified_name)
  in
  match parse_result with
  | AstResult.Some () -> false
  | AstResult.ParseError -> true
  | AstResult.TestFile -> true
  | AstResult.Synthesized -> true
  | AstResult.Pyre1NotFound -> failwith "unreachable"


let captures_from_metadata
    ~callable_id_to_qualified_name_shared_memory
    { CallableMetadataSharedMemory.Value.captured_variables; _ }
  =
  List.map captured_variables ~f:(fun { CapturedVariable.name; outer_function } ->
      AccessPath.CapturedVariable.FromFunction
        {
          name;
          defining_function =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              outer_function
            |> FullyQualifiedName.to_reference;
        })


(* Undecorated signatures of each callable, provided by pyrefly. *)
module CallableUndecoratedSignaturesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = FunctionSignature.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly undecorated signatures of callables"
    end)

module ClassDecoratorsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Ast.Expression.t list AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly class decorators"
    end)

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Reference.t;
      absolute_source_path: ArtifactPath.t option;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
      pyrefly_info_filename: ModuleInfoFilename.t option;
      sys_info: SysInfo.t;
      is_test: bool;
      is_stub: bool;
      is_internal: bool;
    }
    [@@deriving compare, equal, show]

    let from_project
        ~pyrefly_directory
        {
          ProjectFile.Module.module_id;
          module_name;
          absolute_source_path;
          relative_source_path;
          info_filename;
          python_version;
          platform;
          is_test;
          is_interface;
          is_internal;
          _;
        }
      =
      {
        module_id;
        module_name;
        absolute_source_path = ModulePath.artifact_file_path ~pyrefly_directory absolute_source_path;
        relative_source_path;
        pyrefly_info_filename = info_filename;
        sys_info = { SysInfo.python_version; platform = Some platform };
        is_test;
        is_stub = is_interface;
        is_internal;
      }
  end

  type t = {
    pyrefly_directory: PyrePath.t;
    qualifier_to_module_map: Module.t ModuleQualifier.Map.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_shared_memory: QualifiersSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t option;
    callable_metadata_shared_memory: CallableMetadataSharedMemory.t;
    class_metadata_shared_memory: ClassMetadataSharedMemory.t;
    class_fields_shared_memory: ClassFieldsSharedMemory.t;
    class_decorators_shared_memory: ClassDecoratorsSharedMemory.t;
    module_callables_shared_memory: ModuleCallablesSharedMemory.t;
    module_classes_shared_memory: ModuleClassesSharedMemory.t;
    module_globals_shared_memory: ModuleGlobalsSharedMemory.t;
    module_id_to_qualifier_shared_memory: ModuleIdToQualifierSharedMemory.t;
    class_id_to_qualified_name_shared_memory: ClassIdToQualifiedNameSharedMemory.t;
    callable_id_to_qualified_name_shared_memory: CallableIdToQualifiedNameSharedMemory.t;
    callable_ast_shared_memory: CallableAstSharedMemory.t;
    callable_define_signature_shared_memory: CallableDefineSignatureSharedMemory.t;
    callable_parse_result_shared_memory: CallableParseResultSharedMemory.t;
    callable_undecorated_signatures_shared_memory: CallableUndecoratedSignaturesSharedMemory.t;
    all_sys_infos: SysInfo.t list;
    object_classes: TypeshedClass.t list;
    dict_classes: TypeshedClass.t list;
    typing_mapping_classes: TypeshedClass.t list;
  }

  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  let create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules modules =
    let make_unique_qualifiers ~key:module_name ~data:modules =
      match modules with
      | [module_info] ->
          [
            ( ModuleQualifier.create ~path:None module_name,
              Module.from_project ~pyrefly_directory module_info );
          ]
      | _ ->
          (* From a list of modules with the same module name, make unique qualifiers for each
             module, using the path as a prefix *)
          let number_modules = List.length modules in
          let pyre_path_elements path = path |> PyrePath.absolute |> String.split ~on:'/' in
          let module_path_elements = function
            | ModulePath.Filesystem path -> path |> ArtifactPath.raw |> pyre_path_elements
            | ModulePath.Namespace path -> "namespace:/" :: pyre_path_elements path
            | ModulePath.Memory path -> "memory:/" :: pyre_path_elements path
            | ModulePath.BundledTypeshed path -> "typeshed:/" :: pyre_path_elements path
            | ModulePath.BundledTypeshedThirdParty path ->
                "typeshed-third-party:/" :: pyre_path_elements path
          in
          let rec find_shortest_unique_prefix ~prefix_length modules_with_path =
            let map =
              modules_with_path
              |> List.fold ~init:SerializableStringMap.empty ~f:(fun sofar (path, module_info) ->
                     SerializableStringMap.update
                       (List.take path prefix_length |> List.rev |> String.concat ~sep:"/")
                       (function
                         | None -> Some module_info
                         | Some existing -> Some existing)
                       sofar)
            in
            if Int.equal number_modules (SerializableStringMap.cardinal map) then
              SerializableStringMap.to_alist map
            else if prefix_length >= 1000 then
              failwith "Could not make a unique qualifier for a module after 1000 iterations"
            else
              find_shortest_unique_prefix ~prefix_length:(prefix_length + 1) modules_with_path
          in
          modules
          |> List.map ~f:(fun ({ ProjectFile.Module.absolute_source_path; _ } as module_info) ->
                 List.rev (module_path_elements absolute_source_path), module_info)
          |> find_shortest_unique_prefix ~prefix_length:1
          |> List.map ~f:(fun (path, module_info) ->
                 ( ModuleQualifier.create ~path:(Some path) module_name,
                   Module.from_project ~pyrefly_directory module_info ))
    in
    let add_to_module_name_mapping sofar ({ ProjectFile.Module.module_name; _ } as module_info) =
      Map.update sofar module_name ~f:(function
          | None -> [module_info]
          | Some existing -> module_info :: existing)
    in
    (* For every module `a.b.c`, make sure that the module `a` exists. If not, then create an
       implicit empty module `a`. *)
    let add_implicit_top_level_modules qualifier_to_module_map =
      let last_module_id =
        qualifier_to_module_map
        |> Map.data
        |> List.concat
        |> List.fold ~init:(ModuleId.from_int 0) ~f:(fun sofar (_, { Module.module_id; _ }) ->
               ModuleId.max sofar module_id)
      in
      let default_sys_info =
        let _, modules = Map.min_elt_exn qualifier_to_module_map in
        let _, { Module.sys_info; _ } = List.hd_exn modules in
        sys_info
      in
      let add_implicit_module ((qualifier_to_module_map, last_module_id) as sofar) module_name =
        if Reference.length module_name >= 2 then
          let module_head = Option.value_exn (Reference.head module_name) in
          if not (Map.mem qualifier_to_module_map module_head) then
            let last_module_id = ModuleId.increment last_module_id in
            let qualifier_to_module_map =
              Map.add_exn
                qualifier_to_module_map
                ~key:module_head
                ~data:
                  [
                    ( ModuleQualifier.create ~path:None module_head,
                      {
                        Module.module_id = last_module_id;
                        module_name = module_head;
                        absolute_source_path = None;
                        relative_source_path = None;
                        pyrefly_info_filename = None;
                        sys_info = default_sys_info;
                        is_test = false;
                        is_stub = false;
                        is_internal = false;
                      } );
                  ]
            in
            qualifier_to_module_map, last_module_id
          else
            sofar
        else
          sofar
      in
      List.fold
        ~init:(qualifier_to_module_map, last_module_id)
        ~f:add_implicit_module
        (Map.keys qualifier_to_module_map)
      |> fst
    in
    modules
    |> List.fold ~init:Reference.Map.empty ~f:add_to_module_name_mapping
    |> Map.mapi ~f:make_unique_qualifiers
    |> (if add_toplevel_modules then add_implicit_top_level_modules else Fn.id)
    |> Map.data
    |> List.concat
    |> ModuleQualifier.Map.of_alist_exn


  let parse_modules ~pyrefly_directory =
    let timer = Timer.start () in
    let () = Log.info "Parsing module list from pyrefly..." in
    let {
      ProjectFile.modules;
      builtin_module_ids = _;
      object_class_refs;
      dict_class_refs;
      typing_module_ids = _;
      typing_mapping_class_refs;
    }
      =
      let project_file_path_capnp =
        PyrePath.append pyrefly_directory ~element:"pyrefly.pysa.capnp.bin"
      in
      if PyrePath.file_exists project_file_path_capnp then
        PyreflyReportCapnp.ProjectFile.from_path_exn project_file_path_capnp
      else
        PyreflyReportJson.ProjectFile.from_path_exn
          (PyrePath.append pyrefly_directory ~element:"pyrefly.pysa.json")
    in
    let qualifier_to_module_map =
      create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules:true modules
    in
    Log.info "Parsed module list from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed module list from pyrefly"
      ~phase_name:"Parsing module list from pyrefly"
      ~timer
      ~integers:["modules", Map.length qualifier_to_module_map]
      ();
    qualifier_to_module_map, object_class_refs, dict_class_refs, typing_mapping_class_refs


  let write_module_infos_to_shared_memory ~qualifier_to_module_map =
    let timer = Timer.start () in
    let () = Log.info "Writing modules to shared memory..." in
    let module_infos_shared_memory = ModuleInfosSharedMemory.create () in
    let module_id_to_qualifier_shared_memory = ModuleIdToQualifierSharedMemory.create () in
    let () =
      Map.to_alist qualifier_to_module_map
      |> List.iter
           ~f:(fun
                ( qualifier,
                  {
                    Module.module_id;
                    absolute_source_path;
                    relative_source_path;
                    pyrefly_info_filename;
                    sys_info;
                    is_test;
                    is_stub;
                    is_internal;
                    _;
                  } )
              ->
             ModuleInfosSharedMemory.write_around
               module_infos_shared_memory
               qualifier
               {
                 ModuleInfosSharedMemory.Module.module_id;
                 absolute_source_path;
                 relative_source_path;
                 pyrefly_info_filename;
                 sys_info;
                 is_test;
                 is_stub;
                 is_internal;
               };
             ModuleIdToQualifierSharedMemory.write_around
               module_id_to_qualifier_shared_memory
               module_id
               qualifier)
    in
    Log.info "Wrote modules to shared memory: %.3fs" (Timer.stop_in_sec timer);
    module_infos_shared_memory, module_id_to_qualifier_shared_memory


  let parse_source_files
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~module_infos_shared_memory
      ~qualifier_to_module_map
      ~module_callables_shared_memory
      ~module_classes_shared_memory
      ~callable_metadata_shared_memory
      ~class_metadata_shared_memory
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing source files..." in
    let callable_ast_shared_memory = CallableAstSharedMemory.create () in
    let callable_define_signature_shared_memory = CallableDefineSignatureSharedMemory.create () in
    let callable_parse_result_shared_memory = CallableParseResultSharedMemory.create () in
    let class_decorators_shared_memory = ClassDecoratorsSharedMemory.create () in
    let controls =
      Analysis.EnvironmentControls.create
        ~populate_call_graph:false
        ~string_annotation_preserve_location:false
        configuration
    in
    let store_callable_asts callables define_result =
      List.iter callables ~f:(fun callable ->
          CallableAstSharedMemory.add callable_ast_shared_memory callable define_result;
          let signature_result =
            AstResult.map_node ~f:(fun { Statement.Define.signature; _ } -> signature) define_result
          in
          CallableDefineSignatureSharedMemory.add
            callable_define_signature_shared_memory
            callable
            signature_result;
          ();
          CallableParseResultSharedMemory.add
            callable_parse_result_shared_memory
            callable
            (AstResult.map ~f:(fun _ -> ()) define_result))
    in
    let store_class_decorators classes decorator_result =
      List.iter classes ~f:(fun class_name ->
          ClassDecoratorsSharedMemory.add class_decorators_shared_memory class_name decorator_result)
    in
    let collect_callable_asts_from_source ~qualifier ~callables ~source =
      let callables_with_locations, callables_without_locations =
        List.partition_map
          ~f:(fun callable ->
            let {
              CallableMetadataSharedMemory.Value.metadata = { name_location; _ };
              local_function_id;
              _;
            }
              =
              CallableMetadataSharedMemory.get callable_metadata_shared_memory callable
              |> assert_shared_memory_key_exists (fun () ->
                     Format.asprintf
                       "missing callable metadata: `%a`"
                       FullyQualifiedName.pp
                       callable)
            in
            match local_function_id, name_location with
            | LocalFunctionId.Function _, NameLocation.DefineName location ->
                Either.First (location, callable)
            | LocalFunctionId.ModuleTopLevel, _ -> Either.Second (local_function_id, callable)
            | LocalFunctionId.ClassTopLevel _, NameLocation.ClassName location ->
                Either.First (location, callable)
            | LocalFunctionId.ClassField _, NameLocation.UnknonwnForClassField ->
                Either.Second (local_function_id, callable)
            | _ -> failwith "unreachable")
          callables
      in
      let location_to_callable = Location.Map.of_alist_exn callables_with_locations in
      let id_to_callable = LocalFunctionId.Map.of_alist_exn callables_without_locations in
      (* For a given `def ..` statement, we need to find the matching definition provided by
         Pyrefly. *)
      let find_define_callable
          (location_to_callable, id_to_callable, callable_to_define)
          ({
             Node.value = { Statement.Define.signature = { name; parameters; _ }; body; _ };
             location = { Location.start = define_start; stop = define_stop } as define_location;
           } as define)
        =
        if Reference.equal name (Reference.create_from_list [Statement.toplevel_define_name]) then
          let callable = Map.find_exn id_to_callable LocalFunctionId.ModuleTopLevel in
          let id_to_callable = Map.remove id_to_callable LocalFunctionId.ModuleTopLevel in
          let callable_to_define =
            Map.add_exn callable_to_define ~key:callable ~data:(AstResult.Some define)
          in
          location_to_callable, id_to_callable, callable_to_define
        else
          let search_result =
            (* Pyrefly gives us the location of the name AST node, i.e the location of 'foo' in `def
               foo(): ...`, but the Pyre AST does not give us the location of the name, only the
               location of the whole `def foo(): ...` statement. Since we can't match on the exact
               location, we try to find a definition whose location is between the `def` keyword and
               the first parameter or first statement. *)
            let first_parameter_or_statement_position =
              match strip_invalid_locations parameters, strip_invalid_locations body with
              | { Node.location = { Location.start; _ }; _ } :: _, _ -> start
              | _, { Node.location = { Location.start; _ }; _ } :: _ -> start
              | _ -> define_stop
            in
            Map.binary_search_subrange
              location_to_callable
              ~compare:(fun ~key:{ Location.start = location; _ } ~data:_ bound ->
                Location.compare_position location bound)
              ~lower_bound:(Maybe_bound.Excl define_start)
              ~upper_bound:(Maybe_bound.Incl first_parameter_or_statement_position)
            |> Map.to_alist
            |> List.filter ~f:(fun ({ Location.start = name_start; stop = name_stop }, _) ->
                   Location.compare_position define_start name_start < 0
                   && Location.compare_position name_stop first_parameter_or_statement_position <= 0)
          in
          match search_result with
          | [] ->
              (* This definition is not visible by pyrefly. It might be guarded by a `if
                 TYPE_CHECKING` or `if sys.version`. *)
              location_to_callable, id_to_callable, callable_to_define
          | [(location, callable)] ->
              let location_to_callable = Map.remove location_to_callable location in
              let callable_to_define =
                Map.add_exn callable_to_define ~key:callable ~data:(AstResult.Some define)
              in
              location_to_callable, id_to_callable, callable_to_define
          | _ ->
              Format.asprintf
                "Found multiple definitions matching with define `%a` at location %a of module `%a`"
                Reference.pp
                name
                Location.pp
                define_location
                ModuleQualifier.pp
                qualifier
              |> failwith
      in
      (* We create an implicit function containing all statements in the body of each class, called
         the "class top level define". However, some classes are synthesized out of thin air (for
         instance, `X = namedtuple('X')` creates a class `X`). Those won't have a top level define
         in the source. Assign `AstResult.Synthesized` for those. *)
      let add_toplevel_define_for_synthesized_class
          (location_to_callable, id_to_callable, callable_to_define)
          (location, callable)
        =
        if String.equal (FullyQualifiedName.last callable) Statement.class_toplevel_define_name then
          (* We create an implicit function containing all statements in the body of each class,
             called the "class top level define". However, some classes are synthesized out of thin
             air (for instance, `X = namedtuple('X')` creates a class `X`). Those won't have a top
             level define in the source. Let's create a dummy definition for those. *)
          let class_name = Option.value_exn (FullyQualifiedName.prefix callable) in
          let { ClassMetadataSharedMemory.Metadata.is_synthesized; _ } =
            ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
            |> Option.value_exn
                 ~message:"unexpected: class toplevel define on a class that has no metadata info"
          in
          if is_synthesized then
            let location_to_callable = Map.remove location_to_callable location in
            let callable_to_define =
              Map.add_exn callable_to_define ~key:callable ~data:AstResult.Synthesized
            in
            location_to_callable, id_to_callable, callable_to_define
          else
            location_to_callable, id_to_callable, callable_to_define
        else
          location_to_callable, id_to_callable, callable_to_define
      in
      (* Function-like class fields (for instance, the generated `__init__` of dataclasses, or
         function-declared attributes like `foo: Callable[..., int]`) don't have a location. Assign
         `AstResult.Synthesized` for those. *)
      let add_synthesized_class_fields
          (location_to_callable, id_to_callable, callable_to_define)
          (local_function_id, callable)
        =
        if LocalFunctionId.is_class_field local_function_id then
          let id_to_callable = Map.remove id_to_callable local_function_id in
          let callable_to_define =
            Map.add_exn callable_to_define ~key:callable ~data:AstResult.Synthesized
          in
          location_to_callable, id_to_callable, callable_to_define
        else
          location_to_callable, id_to_callable, callable_to_define
      in
      let defines =
        Preprocessing.defines
          ~include_stubs:true
          ~include_nested:true
          ~include_toplevels:true
          ~include_methods:true
          source
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, FullyQualifiedName.Map.empty)
          ~f:find_define_callable
          defines
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, callable_to_define)
          ~f:add_toplevel_define_for_synthesized_class
          (Map.to_alist location_to_callable)
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, callable_to_define)
          ~f:add_synthesized_class_fields
          (Map.to_alist id_to_callable)
      in
      if not (Map.is_empty location_to_callable && Map.is_empty id_to_callable) then
        let id_or_location, callable =
          if not (Map.is_empty location_to_callable) then
            Map.min_elt_exn location_to_callable
            |> fun (location, callable) -> Location.show location, callable
          else
            Map.min_elt_exn id_to_callable
            |> fun (local_function_id, callable) -> LocalFunctionId.show local_function_id, callable
        in
        Format.asprintf
          "Could not find AST of function `%a` at `%s` in module `%a`"
          FullyQualifiedName.pp
          callable
          id_or_location
          ModuleQualifier.pp
          qualifier
        |> failwith
      else
        Map.iteri
          ~f:(fun ~key:callable ~data:define_result ->
            let define_result = AstResult.map ~f:Preprocessing.drop_nested_body define_result in
            CallableAstSharedMemory.add callable_ast_shared_memory callable define_result;
            CallableDefineSignatureSharedMemory.add
              callable_define_signature_shared_memory
              callable
              (AstResult.map_node
                 ~f:(fun { Statement.Define.signature; _ } -> signature)
                 define_result);
            CallableParseResultSharedMemory.add
              callable_parse_result_shared_memory
              callable
              (AstResult.map ~f:(fun _ -> ()) define_result);
            ())
          callable_to_define
    in
    let collect_class_decorators_from_source ~qualifier ~classes ~source =
      let location_to_class =
        classes
        |> List.map ~f:(fun class_name ->
               ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
               |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
               |> fun { ClassMetadataSharedMemory.Metadata.name_location; _ } ->
               name_location, class_name)
        |> Location.Map.of_alist_exn
      in
      (* For a given `class ..` statement, we need to find the matching definition provided by
         Pyrefly. *)
      let find_matching_class_for_ast
          (location_to_class, class_to_statement)
          ({
             Node.value = { Statement.Class.name; base_arguments; body; _ };
             location =
               { Location.start = class_start; stop = class_stop } as class_statement_location;
           } as class_statement)
        =
        (* Pyrefly gives us the location of the name AST node, i.e the location of 'Foo' in `class
           Foo: ...`, but the Pyre AST does not give us the location of the name, only the location
           of the whole `class Foo: ...` statement. Since we can't match on the exact location, we
           try to find a class statement whose location is between the `class` keyword and the first
           base or first statement. *)
        let search_result =
          let first_base_or_statement_position =
            let base_argument_expressions =
              List.map ~f:(fun { Expression.Call.Argument.value; _ } -> value) base_arguments
            in
            match
              strip_invalid_locations base_argument_expressions, strip_invalid_locations body
            with
            | { Node.location = { Location.start; _ }; _ } :: _, _ -> start
            | _, { Node.location = { Location.start; _ }; _ } :: _ -> start
            | _ -> class_stop
          in
          Map.binary_search_subrange
            location_to_class
            ~compare:(fun ~key:{ Location.start = location; stop = _ } ~data:_ bound ->
              Location.compare_position location bound)
            ~lower_bound:(Maybe_bound.Excl class_start)
            ~upper_bound:(Maybe_bound.Incl first_base_or_statement_position)
          |> Map.to_alist
          |> List.filter ~f:(fun ({ Location.start = name_start; stop = name_stop }, _) ->
                 Location.compare_position class_start name_start < 0
                 && Location.compare_position name_stop first_base_or_statement_position <= 0)
        in
        match search_result with
        | [] ->
            (* This definition is not visible by pyrefly. It might be guarded by a `if
               TYPE_CHECKING` or `if sys.version`. *)
            location_to_class, class_to_statement
        | [(location, class_name)] ->
            let location_to_class = Map.remove location_to_class location in
            let class_to_statement =
              Map.add_exn class_to_statement ~key:class_name ~data:class_statement
            in
            location_to_class, class_to_statement
        | _ ->
            Format.asprintf
              "Found multiple class statements matching with class `%a` at location %a of module \
               `%a`"
              Reference.pp
              name
              Location.pp
              class_statement_location
              ModuleQualifier.pp
              qualifier
            |> failwith
      in
      let add_synthesized_class (location_to_class, class_to_statement) (location, class_name) =
        let { ClassMetadataSharedMemory.Metadata.is_synthesized; _ } =
          ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
          |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
        in
        if is_synthesized then
          let class_statement =
            {
              Statement.Class.name = FullyQualifiedName.to_reference class_name;
              base_arguments = [];
              parent = NestingContext.create_toplevel ();
              body = [];
              decorators = [];
              top_level_unbound_names = [];
              type_params = [];
            }
            |> Node.create ~location
          in
          let location_to_class = Map.remove location_to_class location in
          let class_to_statement =
            Map.add_exn class_to_statement ~key:class_name ~data:class_statement
          in
          location_to_class, class_to_statement
        else
          location_to_class, class_to_statement
      in
      let class_statements = Preprocessing.classes source in
      let remaining_classes, class_to_statement =
        List.fold
          ~init:(location_to_class, FullyQualifiedName.Map.empty)
          ~f:find_matching_class_for_ast
          class_statements
      in
      let remaining_classes, class_to_statement =
        List.fold
          ~init:(remaining_classes, class_to_statement)
          ~f:add_synthesized_class
          (Map.to_alist remaining_classes)
      in
      if not (Map.is_empty remaining_classes) then
        let location, class_name = Map.min_elt_exn remaining_classes in
        Format.asprintf
          "Could not find AST of class `%a` at location %a in module `%a`"
          FullyQualifiedName.pp
          class_name
          Location.pp
          location
          ModuleQualifier.pp
          qualifier
        |> failwith
      else
        Map.iteri
          ~f:(fun ~key:class_name ~data:{ Node.value = { Statement.Class.decorators; _ }; _ } ->
            ClassDecoratorsSharedMemory.add
              class_decorators_shared_memory
              class_name
              (AstResult.Some decorators))
          class_to_statement
    in
    let parse_module qualifier =
      let module_info =
        ModuleInfosSharedMemory.get module_infos_shared_memory qualifier
        |> Option.value_exn ~message:"missing module info for qualifier"
      in
      let callables = ModuleCallablesSharedMemory.get module_callables_shared_memory qualifier in
      let classes = ModuleClassesSharedMemory.get module_classes_shared_memory qualifier in
      match module_info, callables, classes with
      | _, None, _ -> ()
      | _, _, None -> ()
      | { ModuleInfosSharedMemory.Module.absolute_source_path = None; _ }, Some _, Some _ ->
          failwith "unexpected: no source path for module with callables"
      | { ModuleInfosSharedMemory.Module.is_test = true; _ }, Some callables, Some classes ->
          let () = store_callable_asts callables AstResult.TestFile in
          let () = store_class_decorators classes AstResult.TestFile in
          ()
      | ( {
            ModuleInfosSharedMemory.Module.absolute_source_path = Some source_path;
            is_test = false;
            _;
          },
          Some callables,
          Some classes ) -> (
          let load_result =
            try Ok (ArtifactPath.raw source_path |> File.create |> File.content_exn) with
            | Sys_error error ->
                Error
                  (Format.asprintf
                     "Cannot open file `%a` due to: %s"
                     ArtifactPath.pp
                     source_path
                     error)
          in
          let pyre1_module_path =
            Ast.ModulePath.create
              ~should_type_check:true
              {
                Ast.ModulePath.Raw.relative = source_path |> ArtifactPath.raw |> PyrePath.absolute;
                priority = 0;
              }
          in
          let open Result.Monad_infix in
          let {
            SysInfo.python_version = { Configuration.PythonVersion.major; minor; micro };
            platform = system_platform;
          }
            =
            module_info.sys_info
          in
          let sys_platform = Option.value system_platform ~default:"linux" in
          let parse_result =
            Analysis.Parsing.parse_result_of_load_result
              ~controls
              ~post_process:false
              pyre1_module_path
              load_result
            >>| Analysis.Preprocessing.replace_version_specific_code
                  ~major_version:major
                  ~minor_version:minor
                  ~micro_version:micro
            >>| Analysis.Preprocessing.replace_platform_specific_code ~sys_platform
            >>| Analysis.Preprocessing.mangle_private_attributes
          in
          match parse_result with
          | Ok ({ Source.module_path; _ } as source) ->
              (* Remove the qualifier created by pyre, it is wrong *)
              let module_path = { module_path with qualifier = Reference.empty } in
              let source = { source with module_path } in
              let () = collect_callable_asts_from_source ~qualifier ~callables ~source in
              let () = collect_class_decorators_from_source ~qualifier ~classes ~source in
              ()
          | Error { Analysis.Parsing.ParseResult.Error.location; message; _ } ->
              let () =
                Log.error
                  "%a:%a: %s"
                  PyrePath.pp
                  (source_path |> ArtifactPath.raw)
                  Location.pp
                  location
                  message
              in
              let () = store_callable_asts callables AstResult.ParseError in
              let () = store_class_decorators classes AstResult.ParseError in
              ())
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseSources
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:4
             ())
    in
    let () =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:()
        ~map:(List.iter ~f:parse_module)
        ~reduce:(fun () () -> ())
        ~inputs:(Map.keys qualifier_to_module_map)
        ()
    in
    Log.info "Parsed source files: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance ~name:"Parsed source files" ~phase_name:"Parsing source files" ~timer ();
    ( callable_ast_shared_memory,
      callable_define_signature_shared_memory,
      callable_parse_result_shared_memory,
      class_decorators_shared_memory )


  (* Logic to assign fully qualified names to classes and defines. *)
  module DefinitionCollector = struct
    module Definition = struct
      type t =
        | Function of ModuleDefinitionsFile.FunctionDefinition.t
        | Class of ModuleDefinitionsFile.ClassDefinition.t
      [@@deriving equal, show]

      let name = function
        | Function { ModuleDefinitionsFile.FunctionDefinition.name; _ } -> name
        | Class { ModuleDefinitionsFile.ClassDefinition.name; _ } -> name
    end

    module ScopeId = struct
      module T = struct
        type t =
          | Location of Location.t
          | Synthesized of LocalFunctionId.t
        [@@deriving compare, equal, show, sexp]

        let _ = pp

        let of_definition = function
          | Definition.Function
              {
                ModuleDefinitionsFile.FunctionDefinition.local_function_id =
                  LocalFunctionId.Function location;
                _;
              } ->
              Location location
          | Definition.Function { ModuleDefinitionsFile.FunctionDefinition.local_function_id; _ } ->
              Synthesized local_function_id
          | Definition.Class { ModuleDefinitionsFile.ClassDefinition.name_location; _ } ->
              Location name_location
      end

      include T
      module Map = Map.Make (T)
    end

    module QualifiedDefinition = struct
      type t = {
        qualified_name: FullyQualifiedName.t;
        local_name: Reference.t; (* a non-unique name, more user-friendly. *)
        definition: Definition.t;
        name_location: NameLocation.t;
      }
    end

    (* Returns a list of path elements for a function or class in the AST. For instance, ['MyClass',
       'foo'] for a method foo in MyClass. *)
    let rec create_local_path ~function_definitions ~class_definitions sofar parent =
      match parent with
      | ModuleDefinitionsFile.ParentScope.TopLevel -> sofar
      | ModuleDefinitionsFile.ParentScope.Class parent_location ->
          let ({ ModuleDefinitionsFile.ClassDefinition.parent; _ } as class_definition) =
            Map.find_exn class_definitions parent_location
          in
          let sofar = Definition.Class class_definition :: sofar in
          create_local_path ~function_definitions ~class_definitions sofar parent
      | ModuleDefinitionsFile.ParentScope.Function parent_location ->
          let ({ ModuleDefinitionsFile.FunctionDefinition.parent; _ } as function_definition) =
            Map.find_exn function_definitions (LocalFunctionId.create_function parent_location)
          in
          let sofar = Definition.Function function_definition :: sofar in
          create_local_path ~function_definitions ~class_definitions sofar parent


    let local_path_of_function
        ~function_definitions
        ~class_definitions
        ({ ModuleDefinitionsFile.FunctionDefinition.parent; _ } as function_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [Definition.Function function_definition]
        parent


    let local_path_of_class
        ~function_definitions
        ~class_definitions
        ({ ModuleDefinitionsFile.ClassDefinition.parent; _ } as class_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [Definition.Class class_definition]
        parent


    (* Represents a tree of definitions, where children of a node are the functions and classes
       nested under that definition. For instance, all methods are nested under their parent
       class. *)
    module Tree = struct
      module Node = struct
        type 'a t = {
          value: 'a;
          children: 'a t ScopeId.Map.t;
        }
      end

      type 'a t = { children: 'a ScopeId.Map.t }

      let empty = { children = ScopeId.Map.empty }

      let add_local_path path { children } =
        let rec add_to_children children = function
          | [] -> children
          | value :: tail ->
              Map.update children (ScopeId.of_definition value) ~f:(function
                  | None -> { value; children = add_to_children ScopeId.Map.empty tail }
                  | Some { Node.value = existing_value; children = existing_children } ->
                      if not (Definition.equal existing_value value) then
                        failwith "Found multiple definitions with the same location"
                      else
                        {
                          value = existing_value;
                          children = add_to_children existing_children tail;
                        })
        in
        { children = add_to_children children path }


      let from_definitions ~function_definitions ~class_definitions =
        let tree =
          Map.fold
            ~init:empty
            ~f:(fun ~key:_ ~data:class_definition sofar ->
              add_local_path
                (local_path_of_class ~function_definitions ~class_definitions class_definition)
                sofar)
            class_definitions
        in
        Map.fold
          ~init:tree
          ~f:(fun ~key:_ ~data:function_definition sofar ->
            add_local_path
              (local_path_of_function ~function_definitions ~class_definitions function_definition)
              sofar)
          function_definitions


      module QualifiedNode = struct
        type t = {
          definition: Definition.t;
          unique_name: string;
          name_overlaps_module: bool;
        }
        [@@deriving show]

        let _ = pp
      end

      let create_qualified_names { children } =
        let rec qualify_children children =
          let name_indices = SerializableStringMap.empty in
          let add_definition (name_indices, sofar) (location, { Node.value = definition; children })
            =
            let name =
              match definition with
              | Definition.Function
                  { ModuleDefinitionsFile.FunctionDefinition.name; is_property_setter; _ } ->
                  (* We need to differentiate property setters from property getters *)
                  let name =
                    if is_property_setter then
                      Format.sprintf "%s@setter" name
                    else
                      name
                  in
                  name
              | Definition.Class { ModuleDefinitionsFile.ClassDefinition.name; _ } -> name
            in
            (* We might find multiple definitions with the same name, for instance:
             * ```
             * def foo(): return
             * def foo(): return
             * ```
             * We assign an unique index to each definition. *)
            let name_indices =
              SerializableStringMap.update
                name
                (function
                  | None -> Some 0
                  | Some index -> Some (index + 1))
                name_indices
            in
            let name =
              match SerializableStringMap.find name name_indices with
              | 0 -> name
              | index -> Format.sprintf "%s$%d" name (index + 1)
            in
            let node =
              {
                Node.value =
                  { QualifiedNode.definition; unique_name = name; name_overlaps_module = false };
                children = qualify_children children;
              }
            in
            name_indices, (location, node) :: sofar
          in
          children
          |> Map.to_alist ~key_order:`Increasing
          |> List.fold ~init:(name_indices, []) ~f:add_definition
          |> snd
          |> ScopeId.Map.of_alist_exn
        in
        { children = qualify_children children }


      (* We might find a definition in a module that would have the same fully qualified name as
       * another definition in another module. For instance:
       *
       * ```
       * # a.py
       * class b:
       *   def c(): ...
       * # a/b.py
       * def c(): ...
       * ```
       *
       * They would both have a fully qualified name `a.b.c`.
       * In those cases, we add a separator `#` in the fully qualified name, between the
       * module and local name, so it would be `a#b.c` instead of `a.b.c`. To detect the name clash, we
       * need to check if our fully qualified name is also a valid module qualifier. *)
      let check_module_overlaps ~module_qualifier ~module_exists { children } =
        let rec set_overlap_flag { Node.value; children } =
          {
            Node.value = { value with QualifiedNode.name_overlaps_module = true };
            children = Map.map ~f:set_overlap_flag children;
          }
        in
        let rec check_node
            ~parent
            { Node.value = { QualifiedNode.unique_name; _ } as value; children }
          =
          let name_overlaps_module =
            FullyQualifiedName.create
              ~module_qualifier
              ~local_name:(List.rev (unique_name :: parent))
              ~add_module_separator:false
            |> FullyQualifiedName.to_reference
            |> ModuleQualifier.from_reference_unchecked
            |> module_exists
          in
          if name_overlaps_module then
            set_overlap_flag { Node.value; children }
          else
            {
              Node.value;
              children = Map.map children ~f:(check_node ~parent:(unique_name :: parent));
            }
        in
        { children = Map.map children ~f:(check_node ~parent:[]) }


      let collect_definitions ~module_qualifier { children } =
        let get_name_location = function
          | Definition.Function { local_function_id = LocalFunctionId.Function location; _ } ->
              NameLocation.DefineName location
          | Definition.Function { local_function_id = LocalFunctionId.ModuleTopLevel; _ } ->
              NameLocation.ModuleTopLevel
          | Definition.Function { local_function_id = LocalFunctionId.ClassTopLevel _; _ } ->
              failwith "unreachable"
          | Definition.Function { local_function_id = LocalFunctionId.ClassField _; _ } ->
              NameLocation.UnknonwnForClassField
          | Definition.Function { local_function_id = LocalFunctionId.FunctionDecoratedTarget _; _ }
            ->
              failwith "unexpected decorated target in function definitions"
          | Definition.Class { name_location; _ } -> NameLocation.ClassName name_location
        in
        let rec add_definition
            sofar
            ~parent_qualified_name
            ~parent_local_name
            {
              Node.value = { QualifiedNode.definition; unique_name; name_overlaps_module };
              children;
            }
          =
          let symbol_name = Definition.name definition in
          let sofar =
            {
              QualifiedDefinition.qualified_name =
                FullyQualifiedName.create
                  ~module_qualifier
                  ~local_name:(List.rev (unique_name :: parent_qualified_name))
                  ~add_module_separator:name_overlaps_module;
              local_name = Reference.create_from_list (List.rev (symbol_name :: parent_local_name));
              name_location = get_name_location definition;
              definition;
            }
            :: sofar
          in
          Map.fold children ~init:sofar ~f:(fun ~key:_ ~data:node sofar ->
              add_definition
                sofar
                ~parent_qualified_name:(unique_name :: parent_qualified_name)
                ~parent_local_name:(symbol_name :: parent_local_name)
                node)
        in
        let definitions =
          Map.fold children ~init:[] ~f:(fun ~key:_ ~data:node sofar ->
              add_definition sofar ~parent_qualified_name:[] ~parent_local_name:[] node)
        in
        List.rev definitions
    end

    let add_toplevel_defines ~module_qualifier definitions =
      let add_toplevel
          sofar
          ({ QualifiedDefinition.definition; qualified_name; local_name; _ } as
          qualified_definition)
        =
        let sofar = qualified_definition :: sofar in
        match definition with
        | Definition.Class
            {
              local_class_id;
              name_location;
              (* Don't add a toplevel define for synthesized classes, such as `MyTuple =
                 namedtuple('MyTuple', ['x', 'y'])` *)
              is_synthesized = false;
              _;
            } ->
            {
              QualifiedDefinition.definition =
                Definition.Function
                  (ModuleDefinitionsFile.FunctionDefinition.create_class_toplevel
                     ~name_location
                     ~local_class_id);
              qualified_name = FullyQualifiedName.create_class_toplevel qualified_name;
              local_name =
                Reference.combine
                  local_name
                  (Reference.create_from_list [Ast.Statement.class_toplevel_define_name]);
              name_location = NameLocation.ClassName name_location;
            }
            :: sofar
        | _ -> sofar
      in
      let definitions = List.fold ~f:add_toplevel ~init:[] definitions in
      let definitions = List.rev definitions in
      {
        QualifiedDefinition.definition =
          Definition.Function (ModuleDefinitionsFile.FunctionDefinition.create_module_toplevel ());
        qualified_name = FullyQualifiedName.create_module_toplevel ~module_qualifier;
        local_name = Reference.create_from_list [Ast.Statement.toplevel_define_name];
        name_location = NameLocation.ModuleTopLevel;
      }
      :: definitions
  end

  module DefinitionCount = struct
    type t = {
      number_callables: int;
      number_classes: int;
    }

    let empty = { number_callables = 0; number_classes = 0 }

    let add
        { number_callables = left_number_callables; number_classes = left_number_classes }
        { number_callables = right_number_callables; number_classes = right_number_classes }
      =
      {
        number_callables = left_number_callables + right_number_callables;
        number_classes = left_number_classes + right_number_classes;
      }
  end

  let collect_classes_and_definitions
      ~scheduler
      ~scheduler_policies
      ~pyrefly_directory
      ~qualifier_to_module_map
      ~module_infos_shared_memory
    =
    let timer = Timer.start () in
    let callable_metadata_shared_memory = CallableMetadataSharedMemory.create () in
    let class_metadata_shared_memory = ClassMetadataSharedMemory.create () in
    let class_fields_shared_memory = ClassFieldsSharedMemory.create () in
    let module_callables_shared_memory = ModuleCallablesSharedMemory.create () in
    let module_classes_shared_memory = ModuleClassesSharedMemory.create () in
    let module_globals_shared_memory = ModuleGlobalsSharedMemory.create () in
    let class_id_to_qualified_name_shared_memory = ClassIdToQualifiedNameSharedMemory.create () in
    let callable_id_to_qualified_name_shared_memory =
      CallableIdToQualifiedNameSharedMemory.create ()
    in
    let callable_undecorated_signatures_shared_memory =
      CallableUndecoratedSignaturesSharedMemory.create ()
    in
    let () = Log.info "Collecting classes and definitions..." in
    (* Collect classes and definitions, assign them fully qualified names, and record undecorated
       signatures. *)
    let collect_definitions_and_assign_names (module_qualifier, pyrefly_info_filename) =
      let {
        ModuleDefinitionsFile.function_definitions;
        class_definitions;
        module_id;
        global_variables;
        _;
      }
        =
        if
          String.is_suffix
            (PyreflyReport.ModuleInfoFilename.raw pyrefly_info_filename)
            ~suffix:".capnp.bin"
        then
          PyreflyReportCapnp.ModuleDefinitionsFile.from_path_exn
            ~pyrefly_directory
            pyrefly_info_filename
        else
          PyreflyReportJson.ModuleDefinitionsFile.from_path_exn
            ~pyrefly_directory
            pyrefly_info_filename
      in
      let definitions =
        DefinitionCollector.Tree.from_definitions ~function_definitions ~class_definitions
        |> DefinitionCollector.Tree.create_qualified_names
        |> DefinitionCollector.Tree.check_module_overlaps
             ~module_qualifier
             ~module_exists:(fun qualifier ->
               ModuleInfosSharedMemory.get module_infos_shared_memory qualifier |> Option.is_some)
        |> DefinitionCollector.Tree.collect_definitions ~module_qualifier
        |> DefinitionCollector.add_toplevel_defines ~module_qualifier
      in
      let store_definition
          (callables, classes)
          { DefinitionCollector.QualifiedDefinition.qualified_name; definition; name_location; _ }
        =
        match definition with
        | Function
            {
              name;
              local_function_id;
              captured_variables;
              is_overload;
              is_staticmethod;
              is_classmethod;
              is_property_getter;
              is_property_setter;
              is_stub;
              is_def_statement;
              is_toplevel;
              is_class_toplevel;
              overridden_base_method;
              defining_class;
              decorator_callees;
              _;
            } ->
            CallableMetadataSharedMemory.write_around
              callable_metadata_shared_memory
              qualified_name
              {
                CallableMetadataSharedMemory.Value.metadata =
                  {
                    module_qualifier = ModuleQualifier.to_reference module_qualifier;
                    name_location;
                    is_overload;
                    is_staticmethod;
                    is_classmethod;
                    is_property_getter;
                    is_property_setter;
                    is_toplevel;
                    is_class_toplevel;
                    is_stub_define = is_stub;
                    is_def_statement;
                    parent_is_class = Option.is_some defining_class;
                  };
                name;
                local_function_id;
                overridden_base_method;
                defining_class;
                decorator_callees;
                captured_variables;
              };
            CallableIdToQualifiedNameSharedMemory.write_around
              callable_id_to_qualified_name_shared_memory
              { GlobalCallableId.module_id; local_function_id }
              qualified_name;
            qualified_name :: callables, classes
        | Class
            {
              name = class_name;
              local_class_id;
              name_location;
              is_synthesized;
              is_dataclass;
              is_named_tuple;
              is_typed_dict;
              fields;
              bases;
              mro;
              decorator_callees;
              _;
            } ->
            ClassMetadataSharedMemory.write_around
              class_metadata_shared_memory
              qualified_name
              {
                ClassMetadataSharedMemory.Metadata.module_qualifier;
                name_location;
                local_class_id;
                is_synthesized;
                is_dataclass;
                is_named_tuple;
                is_typed_dict;
                parents = bases;
                mro;
                decorator_callees;
              };
            let fields =
              fields
              |> List.map
                   ~f:(fun
                        {
                          ModuleDefinitionsFile.PyreflyClassField.name;
                          type_;
                          explicit_annotation;
                          location;
                          declaration_kind;
                        }
                      ->
                     let name =
                       if Identifier.is_private_name name then
                         Identifier.mangle_private_name ~class_name name
                       else
                         name
                     in
                     ( name,
                       {
                         ClassField.type_ = PysaType.from_pyrefly_type type_;
                         explicit_annotation;
                         location;
                         declaration_kind;
                       } ))
              |> SerializableStringMap.of_alist_exn
            in
            ClassFieldsSharedMemory.write_around class_fields_shared_memory qualified_name fields;
            ClassIdToQualifiedNameSharedMemory.write_around
              class_id_to_qualified_name_shared_memory
              { GlobalClassId.module_id; local_class_id }
              qualified_name;
            callables, qualified_name :: classes
      in
      let callables, classes = List.fold definitions ~init:([], []) ~f:store_definition in
      ModuleCallablesSharedMemory.add module_callables_shared_memory module_qualifier callables;
      ModuleClassesSharedMemory.add module_classes_shared_memory module_qualifier classes;
      let global_variables =
        global_variables
        |> List.map ~f:(fun { ModuleDefinitionsFile.PyreflyGlobalVariable.name; type_; location } ->
               name, { GlobalVariable.type_ = type_ >>| PysaType.from_pyrefly_type; location })
        |> SerializableStringMap.of_alist_exn
      in
      ModuleGlobalsSharedMemory.write_around
        module_globals_shared_memory
        module_qualifier
        global_variables;
      (* Record undecorated signatures. *)
      let fold_function_parameters (position, excluded, sofar) = function
        | ModuleDefinitionsFile.FunctionParameter.PosOnly { name; annotation; required } ->
            ( position + 1,
              (match name with
              | Some name -> name :: excluded
              | None -> excluded),
              FunctionParameter.PositionalOnly
                {
                  name;
                  position;
                  annotation = PysaType.from_pyrefly_type annotation;
                  has_default = not required;
                }
              :: sofar )
        | Pos { name; annotation; required } ->
            ( position + 1,
              name :: excluded,
              FunctionParameter.Named
                {
                  name;
                  position;
                  annotation = PysaType.from_pyrefly_type annotation;
                  has_default = not required;
                }
              :: sofar )
        | VarArg { name; annotation = _ } ->
            position + 1, excluded, FunctionParameter.Variable { name; position } :: sofar
        | KwOnly { name; annotation; required } ->
            ( position + 1,
              name :: excluded,
              FunctionParameter.KeywordOnly
                {
                  name;
                  annotation = PysaType.from_pyrefly_type annotation;
                  has_default = not required;
                }
              :: sofar )
        | Kwargs { name; annotation } ->
            ( position + 1,
              [],
              FunctionParameter.Keywords
                { name; annotation = PysaType.from_pyrefly_type annotation; excluded }
              :: sofar )
      in
      let convert_function_signature
          { ModuleDefinitionsFile.FunctionSignature.parameters; return_annotation }
        =
        let parameters =
          match parameters with
          | ModuleDefinitionsFile.FunctionParameters.List parameters ->
              let parameters =
                parameters
                |> List.fold ~init:(0, [], []) ~f:fold_function_parameters
                |> fun (_, _, parameters) -> parameters |> List.rev
              in
              FunctionParameters.List parameters
          | ModuleDefinitionsFile.FunctionParameters.Ellipsis -> FunctionParameters.Ellipsis
          | ModuleDefinitionsFile.FunctionParameters.ParamSpec -> FunctionParameters.ParamSpec
        in
        {
          FunctionSignature.parameters;
          return_annotation = PysaType.from_pyrefly_type return_annotation;
        }
      in
      let toplevel_undecorated_signature =
        {
          FunctionSignature.parameters = FunctionParameters.List [];
          return_annotation =
            PysaType.from_pyrefly_type
              {
                PyreflyType.string = "None";
                scalar_properties = Pyre1Api.ScalarTypeProperties.none;
                class_names = None;
              };
        }
      in
      let get_function_name local_function_id =
        CallableIdToQualifiedNameSharedMemory.get
          callable_id_to_qualified_name_shared_memory
          { GlobalCallableId.module_id; local_function_id }
      in
      let add_function
          ~key:local_function_id
          ~data:{ ModuleDefinitionsFile.FunctionDefinition.undecorated_signatures; _ }
        =
        let qualified_name = get_function_name local_function_id in
        let undecorated_signatures =
          List.map ~f:convert_function_signature undecorated_signatures
        in
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          qualified_name
          undecorated_signatures
      in
      let add_undecorated_signature_for_class
          ~key:_
          ~data:{ ModuleDefinitionsFile.ClassDefinition.local_class_id; _ }
        =
        let class_name =
          ClassIdToQualifiedNameSharedMemory.get_class_name
            class_id_to_qualified_name_shared_memory
            { GlobalClassId.module_id; local_class_id }
        in
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          (FullyQualifiedName.create_class_toplevel class_name)
          [toplevel_undecorated_signature];
        ()
      in
      let () = Map.iteri ~f:add_function function_definitions in
      let () = Map.iteri ~f:add_undecorated_signature_for_class class_definitions in
      let () =
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          (FullyQualifiedName.create_module_toplevel ~module_qualifier)
          [toplevel_undecorated_signature]
      in
      {
        DefinitionCount.number_callables = List.length callables;
        number_classes = List.length classes;
      }
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyCollectDefinitions
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:4
             ())
    in
    let inputs =
      qualifier_to_module_map
      |> Map.to_alist
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_filename; _ }) ->
             match pyrefly_info_filename with
             | Some pyrefly_info_filename -> Some (qualifier, pyrefly_info_filename)
             | None -> None)
    in
    let map modules =
      List.map ~f:collect_definitions_and_assign_names modules
      |> List.fold ~init:DefinitionCount.empty ~f:DefinitionCount.add
    in
    let { DefinitionCount.number_callables; number_classes } =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:DefinitionCount.empty
        ~map
        ~reduce:DefinitionCount.add
        ~inputs
        ()
    in
    Log.info "Collected classes and definitions: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Collected classes and definitions"
      ~phase_name:"Collecting classes and definitions"
      ~timer
      ~integers:["callables", number_callables; "classes", number_classes]
      ();
    ( callable_metadata_shared_memory,
      class_metadata_shared_memory,
      class_fields_shared_memory,
      module_callables_shared_memory,
      module_classes_shared_memory,
      module_globals_shared_memory,
      class_id_to_qualified_name_shared_memory,
      callable_id_to_qualified_name_shared_memory,
      callable_undecorated_signatures_shared_memory )


  let make_typeshed_class
      ~class_id_to_qualified_name_shared_memory
      ~module_id_to_qualifier_shared_memory
      ~module_infos_shared_memory
      global_class_id
    =
    let fully_qualified_name =
      ClassIdToQualifiedNameSharedMemory.get_class_name
        class_id_to_qualified_name_shared_memory
        global_class_id
    in
    let module_qualifier =
      ModuleIdToQualifierSharedMemory.get_module_qualifier
        module_id_to_qualifier_shared_memory
        global_class_id.GlobalClassId.module_id
    in
    let { ModuleInfosSharedMemory.Module.sys_info; _ } =
      ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "unknown module qualifier")
    in
    { TypeshedClass.sys_info; global_class_id; fully_qualified_name }


  let create_from_directory ~scheduler ~scheduler_policies ~configuration pyrefly_directory =
    let qualifier_to_module_map, object_class_refs, dict_class_refs, typing_mapping_class_refs =
      parse_modules ~pyrefly_directory
    in

    let module_infos_shared_memory, module_id_to_qualifier_shared_memory =
      write_module_infos_to_shared_memory ~qualifier_to_module_map
    in

    let qualifiers_shared_memory =
      let handle = QualifiersSharedMemory.create () in
      let qualifiers =
        qualifier_to_module_map
        |> Map.to_alist
        |> List.map
             ~f:(fun (module_qualifier, { Module.absolute_source_path; pyrefly_info_filename; _ })
                ->
               {
                 QualifiersSharedMemory.Value.module_qualifier;
                 has_source = Option.is_some absolute_source_path;
                 has_info = Option.is_some pyrefly_info_filename;
               })
      in
      let () = QualifiersSharedMemory.add handle Memory.SingletonKey.key qualifiers in
      handle
    in

    let ( callable_metadata_shared_memory,
          class_metadata_shared_memory,
          class_fields_shared_memory,
          module_callables_shared_memory,
          module_classes_shared_memory,
          module_globals_shared_memory,
          class_id_to_qualified_name_shared_memory,
          callable_id_to_qualified_name_shared_memory,
          callable_undecorated_signatures_shared_memory )
      =
      collect_classes_and_definitions
        ~scheduler
        ~scheduler_policies
        ~pyrefly_directory
        ~qualifier_to_module_map
        ~module_infos_shared_memory
    in

    let ( callable_ast_shared_memory,
          callable_define_signature_shared_memory,
          callable_parse_result_shared_memory,
          class_decorators_shared_memory )
      =
      parse_source_files
        ~scheduler
        ~scheduler_policies
        ~configuration
        ~module_infos_shared_memory
        ~qualifier_to_module_map
        ~module_callables_shared_memory
        ~module_classes_shared_memory
        ~callable_metadata_shared_memory
        ~class_metadata_shared_memory
    in

    let all_sys_infos =
      qualifier_to_module_map
      |> Map.data
      |> List.fold ~init:SysInfo.Set.empty ~f:(fun set { Module.sys_info; _ } ->
             SysInfo.Set.add sys_info set)
      |> SysInfo.Set.elements
    in

    let make_typeshed_class =
      make_typeshed_class
        ~class_id_to_qualified_name_shared_memory
        ~module_id_to_qualifier_shared_memory
        ~module_infos_shared_memory
    in
    let object_classes = List.map object_class_refs ~f:make_typeshed_class in
    let dict_classes = List.map dict_class_refs ~f:make_typeshed_class in
    let typing_mapping_classes = List.map typing_mapping_class_refs ~f:make_typeshed_class in

    {
      pyrefly_directory;
      qualifier_to_module_map;
      module_infos_shared_memory;
      qualifiers_shared_memory;
      type_of_expressions_shared_memory = None;
      callable_metadata_shared_memory;
      class_metadata_shared_memory;
      class_fields_shared_memory;
      class_decorators_shared_memory;
      module_callables_shared_memory;
      module_classes_shared_memory;
      module_globals_shared_memory;
      module_id_to_qualifier_shared_memory;
      class_id_to_qualified_name_shared_memory;
      callable_id_to_qualified_name_shared_memory;
      callable_ast_shared_memory;
      callable_define_signature_shared_memory;
      callable_parse_result_shared_memory;
      callable_undecorated_signatures_shared_memory;
      all_sys_infos;
      object_classes;
      dict_classes;
      typing_mapping_classes;
    }


  let parse_type_of_expressions
      ({ pyrefly_directory; qualifier_to_module_map; _ } as read_write_api)
      ~scheduler
      ~scheduler_policies
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing type of expressions from pyrefly..." in
    let type_of_expressions_shared_memory = TypeOfExpressionsSharedMemory.create () in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseTypeOfExpressions
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let parse_module_info (module_qualifier, pyrefly_info_filename) =
      let { ModuleTypeOfExpressions.type_of_expression; module_id = _ } =
        if
          String.is_suffix
            (PyreflyReport.ModuleInfoFilename.raw pyrefly_info_filename)
            ~suffix:".capnp.bin"
        then
          PyreflyReportCapnp.ModuleTypeOfExpressions.from_path_exn
            ~pyrefly_directory
            pyrefly_info_filename
        else
          PyreflyReportJson.ModuleTypeOfExpressions.from_path_exn
            ~pyrefly_directory
            pyrefly_info_filename
      in
      List.iter
        type_of_expression
        ~f:(fun { ModuleTypeOfExpressions.TypeAtLocation.location; type_ } ->
          let type_ = PysaType.from_pyrefly_type type_ in
          TypeOfExpressionsSharedMemory.write_around
            type_of_expressions_shared_memory
            { TypeOfExpressionsSharedMemory.Key.module_qualifier; location }
            type_)
    in
    let inputs =
      qualifier_to_module_map
      |> Map.to_alist
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_filename; _ }) ->
             match pyrefly_info_filename with
             | Some pyrefly_info_filename -> Some (qualifier, pyrefly_info_filename)
             | None -> None)
    in
    let () =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:()
        ~map:(List.iter ~f:parse_module_info)
        ~reduce:(fun () () -> ())
        ~inputs
        ()
    in
    Log.info "Parsed type of expressions from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed type of expressions from pyrefly"
      ~phase_name:"Parsing type of expressions from pyrefly"
      ~timer
      ();
    {
      read_write_api with
      type_of_expressions_shared_memory = Some type_of_expressions_shared_memory;
    }


  (* Remove information about the project from the shared memory. This should reduce considerably
     the shared memory **heap** size. However, note that the shared memory does NOT allow removing
     entries from the **hash table**, so all entries are kept. *)
  let cleanup
      {
        pyrefly_directory = _;
        qualifier_to_module_map;
        module_infos_shared_memory;
        qualifiers_shared_memory;
        type_of_expressions_shared_memory = _;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        class_fields_shared_memory;
        class_decorators_shared_memory;
        module_callables_shared_memory;
        module_classes_shared_memory;
        module_globals_shared_memory;
        module_id_to_qualifier_shared_memory;
        class_id_to_qualified_name_shared_memory;
        callable_id_to_qualified_name_shared_memory;
        callable_ast_shared_memory;
        callable_define_signature_shared_memory;
        callable_parse_result_shared_memory;
        callable_undecorated_signatures_shared_memory;
        all_sys_infos = _;
        object_classes = _;
        dict_classes = _;
        typing_mapping_classes = _;
      }
      ~scheduler
    =
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    let cleanup_callable ~module_id callable_name =
      let { CallableMetadataSharedMemory.Value.local_function_id; _ } =
        CallableMetadataSharedMemory.get callable_metadata_shared_memory callable_name
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" FullyQualifiedName.pp callable_name)
      in
      CallableMetadataSharedMemory.remove callable_metadata_shared_memory callable_name;
      CallableAstSharedMemory.remove callable_ast_shared_memory callable_name;
      CallableDefineSignatureSharedMemory.remove
        callable_define_signature_shared_memory
        callable_name;
      CallableParseResultSharedMemory.remove callable_parse_result_shared_memory callable_name;
      CallableUndecoratedSignaturesSharedMemory.remove
        callable_undecorated_signatures_shared_memory
        callable_name;
      CallableIdToQualifiedNameSharedMemory.remove
        callable_id_to_qualified_name_shared_memory
        { GlobalCallableId.module_id; local_function_id };
      ()
    in
    let cleanup_class ~module_id class_name =
      let { ClassMetadataSharedMemory.Metadata.local_class_id; _ } =
        ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
        |> assert_shared_memory_key_exists (fun () -> "missing class metadata")
      in
      ClassMetadataSharedMemory.remove class_metadata_shared_memory class_name;
      ClassFieldsSharedMemory.remove class_fields_shared_memory class_name;
      ClassDecoratorsSharedMemory.remove class_decorators_shared_memory class_name;
      ClassIdToQualifiedNameSharedMemory.remove
        class_id_to_qualified_name_shared_memory
        { GlobalClassId.module_id; local_class_id };
      ()
    in
    let cleanup_module module_qualifier =
      let { ModuleInfosSharedMemory.Module.module_id; pyrefly_info_filename; _ } =
        ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module info")
      in
      ModuleInfosSharedMemory.remove module_infos_shared_memory module_qualifier;
      ModuleIdToQualifierSharedMemory.remove module_id_to_qualifier_shared_memory module_id;
      let () =
        if Option.is_some pyrefly_info_filename then
          ModuleCallablesSharedMemory.get module_callables_shared_memory module_qualifier
          |> assert_shared_memory_key_exists (fun () -> "missing module callables")
          |> List.iter ~f:(cleanup_callable ~module_id)
      in
      let () =
        if Option.is_some pyrefly_info_filename then
          ModuleClassesSharedMemory.get module_classes_shared_memory module_qualifier
          |> assert_shared_memory_key_exists (fun () -> "missing module classes")
          |> List.iter ~f:(cleanup_class ~module_id)
      in
      ModuleCallablesSharedMemory.remove module_callables_shared_memory module_qualifier;
      ModuleClassesSharedMemory.remove module_classes_shared_memory module_qualifier;
      ModuleGlobalsSharedMemory.remove module_globals_shared_memory module_qualifier;
      ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:()
      ~map:(List.iter ~f:cleanup_module)
      ~reduce:(fun () () -> ())
      ~inputs:(Map.keys qualifier_to_module_map)
      ();
    QualifiersSharedMemory.remove qualifiers_shared_memory Memory.SingletonKey.key;
    (* TODO(T225700656): Clean up TypeOfExpressionsSharedMemory (this requires storing all locations
       with a type) *)
    Memory.SharedMemory.collect `aggressive;
    ()
end

(* Read-only API that can be sent to workers. Cheap to copy. *)
module ReadOnly = struct
  type t = {
    pyrefly_directory: PyrePath.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_shared_memory: QualifiersSharedMemory.t;
    callable_metadata_shared_memory: CallableMetadataSharedMemory.t;
    class_metadata_shared_memory: ClassMetadataSharedMemory.t;
    class_fields_shared_memory: ClassFieldsSharedMemory.t;
    class_decorators_shared_memory: ClassDecoratorsSharedMemory.t;
    module_callables_shared_memory: ModuleCallablesSharedMemory.t;
    module_classes_shared_memory: ModuleClassesSharedMemory.t;
    module_globals_shared_memory: ModuleGlobalsSharedMemory.t;
    callable_ast_shared_memory: CallableAstSharedMemory.t;
    callable_define_signature_shared_memory: CallableDefineSignatureSharedMemory.t;
    callable_parse_result_shared_memory: CallableParseResultSharedMemory.t;
    callable_undecorated_signatures_shared_memory: CallableUndecoratedSignaturesSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t option;
    module_id_to_qualifier_shared_memory: ModuleIdToQualifierSharedMemory.t;
    class_id_to_qualified_name_shared_memory: ClassIdToQualifiedNameSharedMemory.t;
    callable_id_to_qualified_name_shared_memory: CallableIdToQualifiedNameSharedMemory.t;
    all_sys_infos: SysInfo.t list;
    object_classes: TypeshedClass.t list;
    dict_classes: TypeshedClass.t list;
    typing_mapping_classes: TypeshedClass.t list;
  }

  let of_read_write_api
      {
        ReadWrite.pyrefly_directory;
        module_infos_shared_memory;
        qualifiers_shared_memory;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        class_fields_shared_memory;
        class_decorators_shared_memory;
        module_callables_shared_memory;
        module_classes_shared_memory;
        module_globals_shared_memory;
        callable_ast_shared_memory;
        callable_define_signature_shared_memory;
        callable_parse_result_shared_memory;
        callable_undecorated_signatures_shared_memory;
        type_of_expressions_shared_memory;
        module_id_to_qualifier_shared_memory;
        class_id_to_qualified_name_shared_memory;
        callable_id_to_qualified_name_shared_memory;
        all_sys_infos;
        object_classes;
        dict_classes;
        typing_mapping_classes;
        _;
      }
    =
    {
      pyrefly_directory;
      module_infos_shared_memory;
      qualifiers_shared_memory;
      callable_metadata_shared_memory;
      class_metadata_shared_memory;
      class_fields_shared_memory;
      class_decorators_shared_memory;
      module_callables_shared_memory;
      module_classes_shared_memory;
      module_globals_shared_memory;
      callable_ast_shared_memory;
      callable_define_signature_shared_memory;
      callable_parse_result_shared_memory;
      callable_undecorated_signatures_shared_memory;
      type_of_expressions_shared_memory;
      module_id_to_qualifier_shared_memory;
      class_id_to_qualified_name_shared_memory;
      callable_id_to_qualified_name_shared_memory;
      all_sys_infos;
      object_classes;
      dict_classes;
      typing_mapping_classes;
    }


  let all_sys_infos { all_sys_infos; _ } = all_sys_infos

  let artifact_path_of_qualifier { module_infos_shared_memory; _ } qualifier =
    if Reference.equal qualifier Analysis.PyrePysaEnvironment.artificial_decorator_define_module
    then
      None
    else
      ModuleInfosSharedMemory.get
        module_infos_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
      |> fun { ModuleInfosSharedMemory.Module.absolute_source_path; _ } -> absolute_source_path


  let absolute_source_path_of_qualifier api qualifier =
    (* TODO(T225700656): We currently return the artifact path, it should be translated back into a
       source path by buck *)
    artifact_path_of_qualifier api qualifier >>| ArtifactPath.raw >>| PyrePath.absolute


  let relative_path_of_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.relative_source_path; _ } -> relative_source_path


  (* Return all qualifiers with source code *)
  let explicit_qualifiers { qualifiers_shared_memory; _ } =
    QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
    |> assert_shared_memory_key_exists (fun () -> "missing qualifiers with source in shared memory")
    |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_source; _ } ->
           Option.some_if has_source (ModuleQualifier.to_reference module_qualifier))


  let is_test_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.is_test; _ } -> is_test


  let is_stub_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.is_stub; _ } -> is_stub


  let is_internal_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.is_internal; _ } -> is_internal


  let get_class_names_for_qualifier
      ({ module_classes_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      ModuleClassesSharedMemory.get
        module_classes_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module classes for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference


  let all_classes { qualifiers_shared_memory; module_classes_shared_memory; _ } ~scheduler =
    let qualifiers =
      QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
      |> assert_shared_memory_key_exists (fun () ->
             "missing qualifiers with source in shared memory")
      |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_info; _ } ->
             Option.some_if has_info module_qualifier)
    in
    let get_class_names_for_qualifier module_qualifier =
      ModuleClassesSharedMemory.get module_classes_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "missing module classes for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference
      |> List.map ~f:Reference.show
    in
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map:(List.concat_map ~f:get_class_names_for_qualifier)
      ~reduce:List.append
      ~inputs:qualifiers
      ()


  let all_global_variables { qualifiers_shared_memory; module_globals_shared_memory; _ } ~scheduler =
    let qualifiers =
      QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
      |> assert_shared_memory_key_exists (fun () ->
             "missing qualifiers with source in shared memory")
      |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_info; _ } ->
             Option.some_if has_info module_qualifier)
    in
    let get_globals_for_qualifier module_qualifier =
      ModuleGlobalsSharedMemory.get module_globals_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "missing module globals for qualifier")
      |> SerializableStringMap.keys
      |> List.map ~f:(Reference.create ~prefix:(ModuleQualifier.to_reference module_qualifier))
    in
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map:(List.concat_map ~f:get_globals_for_qualifier)
      ~reduce:List.append
      ~inputs:qualifiers
      ()


  let get_define_names_for_qualifier
      ({ module_callables_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      ModuleCallablesSharedMemory.get
        module_callables_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference


  let get_qualifier_top_level_define_name _ qualifier =
    let module_qualifier = ModuleQualifier.from_reference_unchecked qualifier in
    FullyQualifiedName.create_module_toplevel ~module_qualifier |> FullyQualifiedName.to_reference


  (* Check if a class name is any variant of "object" *)
  let is_object_class ~object_classes class_name =
    List.exists object_classes ~f:(fun { TypeshedClass.fully_qualified_name; _ } ->
        FullyQualifiedName.equal class_name fully_qualified_name)


  (* Get the SysInfo for a class by looking up its module *)
  let get_sys_info_for_class
      { class_metadata_shared_memory; module_infos_shared_memory; _ }
      class_name
    =
    let { ClassMetadataSharedMemory.Metadata.module_qualifier; _ } =
      ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
      |> assert_shared_memory_key_exists (fun () -> "missing class metadata")
    in
    let { ModuleInfosSharedMemory.Module.sys_info; _ } =
      ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "missing module info")
    in
    sys_info


  (* Find the TypeshedClass matching a given SysInfo *)
  let find_typeshed_class_by_sys_info typeshed_classes sys_info =
    List.find_exn typeshed_classes ~f:(fun { TypeshedClass.sys_info = s; _ } ->
        SysInfo.equal s sys_info)


  (* Get the object_class FullyQualifiedName for a class with the same SysInfo *)
  let get_object_class_for ({ object_classes; _ } as api) class_name =
    let sys_info = get_sys_info_for_class api class_name in
    let { TypeshedClass.fully_qualified_name; _ } =
      find_typeshed_class_by_sys_info object_classes sys_info
    in
    fully_qualified_name


  let class_immediate_parents
      ({ class_metadata_shared_memory; class_id_to_qualified_name_shared_memory; object_classes; _ }
      as api)
      class_name
    =
    (* TOOD(T225700656): Update the API to take a reference and return a reference. *)
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let get_parents_from_class_metadata { ClassMetadataSharedMemory.Metadata.parents; _ } =
      match parents with
      | [] when not (is_object_class ~object_classes class_name) ->
          let object_class = get_object_class_for api class_name in
          [object_class]
      | parents ->
          List.map
            ~f:
              (ClassIdToQualifiedNameSharedMemory.get_class_name
                 class_id_to_qualified_name_shared_memory)
            parents
    in
    ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
    |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    |> get_parents_from_class_metadata
    |> List.map ~f:FullyQualifiedName.to_reference
    |> List.map ~f:Reference.show


  let class_mro
      ({ class_metadata_shared_memory; class_id_to_qualified_name_shared_memory; object_classes; _ }
      as api)
      class_name
    =
    (* TOOD(T225700656): Update the API to take a reference and return a reference. *)
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let get_mro_from_class_metadata { ClassMetadataSharedMemory.Metadata.mro; _ } =
      match mro with
      | _ when is_object_class ~object_classes class_name -> []
      | ModuleDefinitionsFile.ClassMro.Cyclic ->
          (* Failed to resolve the mro because the class hierarchy is cyclic. Fallback to
             [object]. *)
          [get_object_class_for api class_name]
      | ModuleDefinitionsFile.ClassMro.Resolved mro ->
          let mro =
            List.map
              ~f:
                (ClassIdToQualifiedNameSharedMemory.get_class_name
                   class_id_to_qualified_name_shared_memory)
              mro
          in
          (* Pyrefly does not include 'object' in the mro. *)
          let object_class = get_object_class_for api class_name in
          mro @ [object_class]
    in
    ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
    |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    |> get_mro_from_class_metadata
    |> List.map ~f:FullyQualifiedName.to_reference
    |> List.map ~f:Reference.show


  let is_subclass
      { class_metadata_shared_memory; class_id_to_qualified_name_shared_memory; object_classes; _ }
      ~parent
      ~child
    =
    let parent = FullyQualifiedName.from_reference_unchecked (Reference.create parent) in
    let child = FullyQualifiedName.from_reference_unchecked (Reference.create child) in
    if FullyQualifiedName.equal parent child || is_object_class ~object_classes parent then
      true
    else
      let { ClassMetadataSharedMemory.Metadata.mro; _ } =
        ClassMetadataSharedMemory.get class_metadata_shared_memory child
        |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
      in
      match mro with
      | ModuleDefinitionsFile.ClassMro.Cyclic -> false
      | ModuleDefinitionsFile.ClassMro.Resolved mro ->
          List.exists mro ~f:(fun class_id ->
              FullyQualifiedName.equal
                parent
                (ClassIdToQualifiedNameSharedMemory.get_class_name
                   class_id_to_qualified_name_shared_memory
                   class_id))


  let get_callable_metadata_value_opt { callable_metadata_shared_memory; _ } define_name =
    CallableMetadataSharedMemory.get
      callable_metadata_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)


  let get_callable_metadata api define_name =
    get_callable_metadata_value_opt api define_name
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> fun { CallableMetadataSharedMemory.Value.metadata; _ } -> metadata


  let get_callable_metadata_opt api define_name =
    get_callable_metadata_value_opt api define_name
    >>| fun { CallableMetadataSharedMemory.Value.metadata; _ } -> metadata


  let is_stub_like_callable_opt ({ callable_parse_result_shared_memory; _ } as api) define_name =
    get_callable_metadata_opt api define_name
    >>| fun metadata ->
    is_stub_like_from_metadata
      ~metadata
      ~callable_parse_result_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)


  let is_stub_like_callable ({ callable_parse_result_shared_memory; _ } as api) define_name =
    get_callable_metadata_opt api define_name
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> fun metadata ->
    is_stub_like_from_metadata
      ~metadata
      ~callable_parse_result_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)


  let get_overriden_base_method
      { callable_metadata_shared_memory; callable_id_to_qualified_name_shared_memory; _ }
      method_reference
    =
    match method_reference with
    | Analysis.PyrePysaEnvironment.MethodReference.Pyre1 _ -> None
    | Analysis.PyrePysaEnvironment.MethodReference.Pyrefly { define_name; is_property_setter } ->
        CallableMetadataSharedMemory.get
          callable_metadata_shared_memory
          (FullyQualifiedName.from_reference_unchecked define_name)
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
        |> fun { CallableMetadataSharedMemory.Value.overridden_base_method; _ } ->
        overridden_base_method
        >>| CallableIdToQualifiedNameSharedMemory.get callable_id_to_qualified_name_shared_memory
        >>| fun define_name ->
        Analysis.PyrePysaEnvironment.MethodReference.Pyrefly
          { define_name = FullyQualifiedName.to_reference define_name; is_property_setter }


  let get_callable_captures_opt
      ({ callable_id_to_qualified_name_shared_memory; _ } as api)
      define_name
    =
    get_callable_metadata_value_opt api define_name
    >>| captures_from_metadata ~callable_id_to_qualified_name_shared_memory


  let get_callable_captures ({ callable_id_to_qualified_name_shared_memory; _ } as api) define_name =
    get_callable_metadata_value_opt api define_name
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> captures_from_metadata ~callable_id_to_qualified_name_shared_memory


  let get_callable_return_annotations
      { callable_undecorated_signatures_shared_memory; _ }
      ~define_name
      ~define:_
    =
    CallableUndecoratedSignaturesSharedMemory.get
      callable_undecorated_signatures_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> List.map ~f:(fun { FunctionSignature.return_annotation; _ } -> return_annotation)


  let get_callable_parameter_annotations
      { callable_undecorated_signatures_shared_memory; _ }
      ~define_name
      parameters
    =
    let signatures =
      CallableUndecoratedSignaturesSharedMemory.get
        callable_undecorated_signatures_shared_memory
        (FullyQualifiedName.from_reference_unchecked define_name)
      |> assert_shared_memory_key_exists (fun () ->
             Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    in
    let normalize_root = function
      | AccessPath.Root.PositionalParameter { position; positional_only = true; _ } ->
          AccessPath.Root.PositionalParameter { position; positional_only = true; name = "" }
      | AccessPath.Root.StarStarParameter _ ->
          AccessPath.Root.StarStarParameter { excluded = Identifier.SerializableSet.empty }
      | root -> root
    in
    let fold_signature_parameter sofar parameter =
      let root = normalize_root (FunctionParameter.root parameter) in
      match FunctionParameter.annotation parameter with
      | None -> sofar
      | Some annotation ->
          AccessPath.Root.Map.update
            root
            (function
              | None -> Some [annotation]
              | Some existing -> Some (annotation :: existing))
            sofar
    in
    let fold_signatures sofar { FunctionSignature.parameters; _ } =
      match parameters with
      | FunctionParameters.Ellipsis
      | FunctionParameters.ParamSpec ->
          sofar
      | FunctionParameters.List signature_parameters ->
          List.fold ~init:sofar ~f:fold_signature_parameter signature_parameters
    in
    let root_annotations_map =
      List.fold ~init:AccessPath.Root.Map.empty ~f:fold_signatures signatures
    in
    List.map
      parameters
      ~f:(fun ({ AccessPath.NormalizedParameter.root; _ } as normalized_parameter) ->
        let annotations =
          AccessPath.Root.Map.find_opt (normalize_root root) root_annotations_map
          |> Option.value ~default:[]
        in
        normalized_parameter, annotations)


  let get_callable_decorator_callees
      { callable_metadata_shared_memory; callable_id_to_qualified_name_shared_memory; _ }
      define_name
      location
    =
    CallableMetadataSharedMemory.get
      callable_metadata_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> (fun { CallableMetadataSharedMemory.Value.decorator_callees; _ } -> decorator_callees)
    |> Location.SerializableMap.find_opt location
    >>| List.map
          ~f:(CallableIdToQualifiedNameSharedMemory.get callable_id_to_qualified_name_shared_memory)
    >>| List.map ~f:FullyQualifiedName.to_reference


  let get_methods_for_qualifier
      ({ module_callables_shared_memory; callable_metadata_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      let convert_to_method_reference callable =
        callable
        |> CallableMetadataSharedMemory.get callable_metadata_shared_memory
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" FullyQualifiedName.pp callable)
        |> fun { CallableMetadataSharedMemory.Value.metadata = { is_property_setter; _ }; _ } ->
        Analysis.PyrePysaEnvironment.MethodReference.Pyrefly
          { define_name = FullyQualifiedName.to_reference callable; is_property_setter }
      in
      ModuleCallablesSharedMemory.get
        module_callables_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
      |> List.map ~f:convert_to_method_reference


  let get_define_opt { callable_ast_shared_memory; _ } define_name =
    CallableAstSharedMemory.get
      callable_ast_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () -> "missing callable ast")


  let get_callable_signature_opt
      ({
         callable_metadata_shared_memory;
         callable_define_signature_shared_memory;
         callable_parse_result_shared_memory;
         callable_id_to_qualified_name_shared_memory;
         _;
       } as _api)
      define_name
    =
    let fully_qualified_name = FullyQualifiedName.from_reference_unchecked define_name in
    CallableMetadataSharedMemory.get callable_metadata_shared_memory fully_qualified_name
    >>| fun ({ CallableMetadataSharedMemory.Value.metadata; _ } as metadata_value) ->
    let define_signature_result =
      CallableDefineSignatureSharedMemory.get
        callable_define_signature_shared_memory
        fully_qualified_name
      |> assert_shared_memory_key_exists (fun () ->
             Format.asprintf "missing callable define signature: `%a`" Reference.pp define_name)
    in
    let method_kind = CallableMetadata.get_method_kind metadata in
    let captures =
      captures_from_metadata ~callable_id_to_qualified_name_shared_memory metadata_value
    in
    let is_stub_like =
      is_stub_like_from_metadata ~metadata ~callable_parse_result_shared_memory fully_qualified_name
    in
    let define_signature = AstResult.map ~f:Node.value define_signature_result in
    {
      Pyre1Api.CallableSignature.qualifier = metadata.CallableMetadata.module_qualifier;
      location = AstResult.map ~f:Node.location define_signature_result;
      define_name;
      parameters =
        AstResult.map
          ~f:(fun { Statement.Define.Signature.parameters; _ } -> parameters)
          define_signature;
      return_annotation =
        AstResult.map
          ~f:(fun { Statement.Define.Signature.return_annotation; _ } -> return_annotation)
          define_signature;
      decorators =
        AstResult.map
          ~f:(fun { Statement.Define.Signature.decorators; _ } -> decorators)
          define_signature;
      captures;
      method_kind;
      is_stub_like;
    }


  let get_undecorated_signatures { callable_undecorated_signatures_shared_memory; _ } define_name =
    CallableUndecoratedSignaturesSharedMemory.get
      callable_undecorated_signatures_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () -> "missing callable undecorated signature")


  let get_class_summary { class_metadata_shared_memory; _ } class_name =
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let metadata =
      ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
      |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    in
    { PysaClassSummary.class_name; metadata }


  let get_class_decorators_opt { class_decorators_shared_memory; _ } class_name =
    ClassDecoratorsSharedMemory.get
      class_decorators_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing callable ast")


  let get_class_attributes
      { class_fields_shared_memory; _ }
      ~include_generated_attributes:_
      ~only_simple_assignments:_
      class_name
    =
    (* TODO(T225700656): Support include_generated_attributes and only_simple_assignments
       options. *)
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    >>| SerializableStringMap.keys


  let get_class_attribute_inferred_type { class_fields_shared_memory; _ } ~class_name ~attribute =
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
    |> SerializableStringMap.find_opt attribute
    |> assert_shared_memory_key_exists (fun () -> "missing class field")
    |> fun { ClassField.type_; _ } -> type_


  let get_class_attribute_explicit_annotation
      { class_fields_shared_memory; _ }
      ~class_name
      ~attribute
    =
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
    |> SerializableStringMap.find_opt attribute
    |> assert_shared_memory_key_exists (fun () -> "missing class field")
    |> fun { ClassField.explicit_annotation; _ } -> explicit_annotation


  let get_global_inferred_type { module_globals_shared_memory; _ } ~qualifier ~name =
    ModuleGlobalsSharedMemory.get
      module_globals_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module globals")
    |> SerializableStringMap.find_opt name
    |> assert_shared_memory_key_exists (fun () -> "missing global variable")
    |> fun { GlobalVariable.type_; _ } -> type_


  let target_from_define_name api ~override define_name =
    let { CallableMetadata.is_property_setter; parent_is_class; _ } =
      get_callable_metadata api define_name
    in
    let kind =
      if is_property_setter then
        Target.PyreflyPropertySetter
      else
        Target.Normal
    in
    if parent_is_class then
      if override then
        Target.create_override_from_reference ~kind define_name
      else
        Target.create_method_from_reference ~kind define_name
    else
      Target.create_function ~kind define_name


  let instantiate_call_graph
      ({
         module_id_to_qualifier_shared_memory;
         callable_id_to_qualified_name_shared_memory;
         class_id_to_qualified_name_shared_memory;
         _;
       } as api)
      ~overrides_exist
      ~get_overriding_types
      ~global_is_string_literal
      ~attribute_targets
      ~callable
      json_call_graph
    =
    let open ModuleCallGraphs in
    let open CallGraph in
    let instantiate_target ~receiver_class = function
      | PyreflyTarget.Function callable_id ->
          let fully_qualified_name =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              callable_id
          in
          let target =
            target_from_define_name
              api
              ~override:false
              (FullyQualifiedName.to_reference fully_qualified_name)
          in
          [target]
      | PyreflyTarget.Overrides callable_id -> (
          let fully_qualified_name =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              callable_id
          in
          let target =
            target_from_define_name
              api
              ~override:false
              (FullyQualifiedName.to_reference fully_qualified_name)
          in
          let target_method =
            match target with
            | Target.Regular (Target.Regular.Method target_method) -> target_method
            | _ -> failwith "unreachable"
          in
          let declaring_class = target_method.class_name in
          let get_actual_target t =
            if overrides_exist t then
              t
              |> Target.as_regular_exn
              |> Target.Regular.get_corresponding_override_exn
              |> Target.from_regular
            else
              t
          in
          match receiver_class with
          | None -> [target]
          | Some receiver_class when String.equal receiver_class declaring_class ->
              (* Receiver type matches declaring type *)
              [get_actual_target target]
          | Some receiver_class -> (
              let overriding_types = get_overriding_types target in
              match overriding_types with
              | None -> [target]
              | Some overriding_types ->
                  let override_targets =
                    overriding_types
                    |> List.filter ~f:(fun class_name ->
                           is_subclass api ~parent:receiver_class ~child:(Reference.show class_name))
                    |> List.map ~f:(fun class_name ->
                           get_actual_target
                             (Target.create_method
                                ~kind:target_method.kind
                                class_name
                                target_method.method_name))
                  in
                  target :: override_targets))
      | PyreflyTarget.FormatString -> [Target.ArtificialTargets.format_string]
    in
    let instantiate_call_target
        {
          PyreflyCallTarget.target;
          implicit_receiver;
          implicit_dunder_call;
          receiver_class;
          is_class_method;
          is_static_method;
          return_type;
        }
      =
      let receiver_class =
        receiver_class
        >>| ClassIdToQualifiedNameSharedMemory.get_class_name
              class_id_to_qualified_name_shared_memory
        >>| FullyQualifiedName.to_reference
        >>| Ast.Reference.show
      in
      let targets = instantiate_target ~receiver_class target in
      List.map
        ~f:(fun target ->
          {
            CallTarget.target;
            implicit_receiver = PyreflyImplicitReceiver.is_true implicit_receiver;
            implicit_dunder_call;
            index = 0;
            receiver_class;
            is_class_method;
            is_static_method;
            return_type = Some return_type;
          })
        targets
    in
    let instantiate_global_target { PyreflyGlobalVariable.module_id; name } =
      let module_qualifier =
        ModuleIdToQualifierSharedMemory.get_module_qualifier
          module_id_to_qualifier_shared_memory
          module_id
      in
      let object_reference =
        Reference.create ~prefix:(ModuleQualifier.to_reference module_qualifier) name
      in
      let object_target = Target.create_object object_reference in
      if Target.Set.mem object_target attribute_targets || global_is_string_literal object_reference
      then
        Some (CallTarget.create ~return_type:None object_target)
      else
        None
    in
    let instantiate_captured_variable { CapturedVariable.name; outer_function } =
      AccessPath.CapturedVariable.FromFunction
        {
          name;
          defining_function =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              outer_function
            |> FullyQualifiedName.to_reference;
        }
    in
    let instantiate_higher_order_parameter
        { PyreflyHigherOrderParameter.index; call_targets; unresolved }
      =
      {
        CallGraph.HigherOrderParameter.index;
        call_targets = call_targets |> List.map ~f:instantiate_call_target |> List.concat;
        unresolved;
      }
    in
    let instantiate_call_callees
        {
          PyreflyCallCallees.call_targets;
          init_targets;
          new_targets;
          higher_order_parameters;
          unresolved;
        }
      =
      (* TODO(T225700656): Support higher order parameters, shim targets, unresolved calles. *)
      {
        CallCallees.call_targets =
          call_targets |> List.map ~f:instantiate_call_target |> List.concat;
        init_targets = init_targets |> List.map ~f:instantiate_call_target |> List.concat;
        new_targets = new_targets |> List.map ~f:instantiate_call_target |> List.concat;
        decorated_targets = [];
        higher_order_parameters =
          higher_order_parameters
          |> PyreflyHigherOrderParameterMap.data
          |> List.map ~f:instantiate_higher_order_parameter
          |> CallGraph.HigherOrderParameterMap.from_list;
        shim_target = None;
        unresolved;
        recognized_call = CallGraph.CallCallees.RecognizedCall.False;
      }
    in
    let instantiate_identifier_callees
        { PyreflyIdentifierCallees.if_called; global_targets; captured_variables }
      =
      let if_called = instantiate_call_callees if_called in
      let global_targets = List.filter_map ~f:instantiate_global_target global_targets in
      let captured_variables = List.map ~f:instantiate_captured_variable captured_variables in
      { IdentifierCallees.global_targets; captured_variables; if_called }
    in
    let instantiate_attribute_access_callees
        {
          PyreflyAttributeAccessCallees.if_called;
          property_setters;
          property_getters;
          global_targets;
          is_attribute;
        }
      =
      let if_called = instantiate_call_callees if_called in
      let global_targets = List.filter_map ~f:instantiate_global_target global_targets in
      {
        AttributeAccessCallees.property_targets =
          List.append property_setters property_getters
          |> List.map ~f:instantiate_call_target
          |> List.concat;
        global_targets;
        is_attribute;
        if_called;
      }
    in
    let instantiate_define_callees { PyreflyDefineCallees.define_targets } =
      {
        CallGraph.DefineCallees.define_targets =
          define_targets |> List.map ~f:instantiate_call_target |> List.concat;
        decorated_targets = [];
      }
    in
    let instantiate_format_string_artificial_callees
        { PyreflyFormatStringArtificialCallees.targets }
      =
      {
        CallGraph.FormatStringArtificialCallees.targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
      }
    in
    let instantiate_format_string_stringify_callees
        { PyreflyFormatStringStringifyCallees.targets; unresolved = _ }
      =
      {
        CallGraph.FormatStringStringifyCallees.targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
      }
    in
    let instantiate_return_shim_callees { PyreflyReturnShimCallees.targets; arguments } =
      {
        CallGraph.ReturnShimCallees.call_targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
        arguments;
      }
    in
    let instantiate_expression_callees = function
      | PyreflyExpressionCallees.Call callees ->
          ExpressionCallees.Call (instantiate_call_callees callees)
      | PyreflyExpressionCallees.Identifier callees ->
          ExpressionCallees.Identifier (instantiate_identifier_callees callees)
      | PyreflyExpressionCallees.AttributeAccess callees ->
          ExpressionCallees.AttributeAccess (instantiate_attribute_access_callees callees)
      | PyreflyExpressionCallees.Define callees ->
          ExpressionCallees.Define (instantiate_define_callees callees)
      | PyreflyExpressionCallees.FormatStringArtificial callees ->
          ExpressionCallees.FormatStringArtificial
            (instantiate_format_string_artificial_callees callees)
      | PyreflyExpressionCallees.FormatStringStringify callees ->
          ExpressionCallees.FormatStringStringify
            (instantiate_format_string_stringify_callees callees)
      | PyreflyExpressionCallees.Return callees ->
          ExpressionCallees.Return (instantiate_return_shim_callees callees)
    in
    let instantiate_call_edge expression_identifier callees call_graph =
      let callees = instantiate_expression_callees callees in
      DefineCallGraph.add_callees
        ~debug:false
        ~caller:callable
        ~on_existing_callees:DefineCallGraph.OnExistingCallees.Fail
        ~expression_identifier
        ~callees
        ~expression_for_logging:None
        call_graph
    in
    List.fold
      json_call_graph
      ~init:DefineCallGraph.empty
      ~f:(fun call_graph { CallGraphEdge.expression_identifier; callees } ->
        instantiate_call_edge expression_identifier callees call_graph)


  let parse_call_graphs
      ({ pyrefly_directory; callable_metadata_shared_memory; module_infos_shared_memory; _ } as api)
      ~scheduler
      ~scheduler_policies
      ~overrides_exist
      ~get_overriding_types
      ~global_is_string_literal
      ~store_shared_memory
      ~attribute_targets
      ~skip_analysis_targets
      ~definitions
      ~create_dependency_for
      ~redirect_to_decorated
      ~transform_call_graph
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing call graphs from pyrefly..." in
    let module CallableWithId = struct
      type t = {
        callable: Target.t;
        local_function_id: LocalFunctionId.t;
      }
    end
    in
    (* Partition functions per module *)
    let module_definitions_map =
      let fold_definition module_definitions_map callable =
        if Target.should_skip_analysis ~skip_analysis_targets callable then
          module_definitions_map
        else
          let {
            CallableMetadataSharedMemory.Value.metadata = { module_qualifier; _ };
            local_function_id;
            _;
          }
            =
            Target.define_name_exn callable
            |> FullyQualifiedName.from_reference_unchecked
            |> CallableMetadataSharedMemory.get callable_metadata_shared_memory
            |> assert_shared_memory_key_exists (fun () ->
                   Format.asprintf
                     "missing callable metadata: `%a`"
                     Target.pp_pretty_with_kind
                     callable)
          in
          let module_definitions_map =
            Map.update
              module_definitions_map
              (ModuleQualifier.from_reference_unchecked module_qualifier)
              ~f:(fun sofar ->
                { CallableWithId.callable; local_function_id } :: Option.value ~default:[] sofar)
          in
          let module_definitions_map =
            match redirect_to_decorated callable with
            | Some decorated_target ->
                let decorated_function_id =
                  match local_function_id with
                  | LocalFunctionId.Function position ->
                      LocalFunctionId.FunctionDecoratedTarget position
                  | _ ->
                      Format.asprintf
                        "Unexpected local function id for decorated function: `%a`"
                        LocalFunctionId.pp
                        local_function_id
                      |> failwith
                in
                Map.update
                  module_definitions_map
                  (ModuleQualifier.from_reference_unchecked module_qualifier)
                  ~f:(fun sofar ->
                    {
                      CallableWithId.callable = decorated_target;
                      local_function_id = decorated_function_id;
                    }
                    :: Option.value ~default:[] sofar)
            | None -> module_definitions_map
          in
          module_definitions_map
      in
      List.fold ~init:ModuleQualifier.Map.empty ~f:fold_definition definitions
    in
    (* Map-reduce over all modules *)
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseCallGraphs
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let parse_module_call_graphs sofar (module_qualifier, callables) =
      let { ModuleInfosSharedMemory.Module.pyrefly_info_filename; _ } =
        ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module info")
      in
      let pyrefly_info_filename =
        match pyrefly_info_filename with
        | None ->
            Format.asprintf
              "Could not find call graphs for module `%a`: missing pyrefly info file"
              ModuleQualifier.pp
              module_qualifier
            |> failwith
        | Some pyrefly_info_filename -> pyrefly_info_filename
      in
      let { ModuleCallGraphs.call_graphs = function_id_to_call_graph_map; module_id = _ } =
        if
          String.is_suffix
            (PyreflyReport.ModuleInfoFilename.raw pyrefly_info_filename)
            ~suffix:".capnp.bin"
        then
          PyreflyReportCapnp.ModuleCallGraphs.from_path_exn ~pyrefly_directory pyrefly_info_filename
        else
          PyreflyReportJson.ModuleCallGraphs.from_path_exn ~pyrefly_directory pyrefly_info_filename
      in
      List.fold
        ~init:sofar
        ~f:
          (fun (define_call_graphs, whole_program_call_graph)
               { CallableWithId.callable; local_function_id } ->
          match Map.find function_id_to_call_graph_map local_function_id with
          | None ->
              Format.asprintf "Could not find call graph for `%a`" Target.pp callable |> failwith
          | Some call_graph ->
              let call_graph =
                instantiate_call_graph
                  api
                  ~overrides_exist
                  ~get_overriding_types
                  ~global_is_string_literal
                  ~attribute_targets
                  ~callable
                  call_graph
                |> transform_call_graph api callable
              in
              let define_call_graphs =
                if store_shared_memory then
                  CallGraph.SharedMemory.AddOnly.add define_call_graphs callable call_graph
                else
                  define_call_graphs
              in
              let whole_program_call_graph =
                CallGraph.WholeProgramCallGraph.add_or_exn
                  whole_program_call_graph
                  ~callable
                  ~callees:
                    (CallGraph.DefineCallGraph.all_targets
                       ~use_case:create_dependency_for
                       call_graph)
              in
              define_call_graphs, whole_program_call_graph)
        callables
    in
    let reduce
        (left_define_call_graphs, left_whole_program_call_graph)
        (right_define_call_graphs, right_whole_program_call_graph)
      =
      ( CallGraph.SharedMemory.AddOnly.merge_same_handle_disjoint_keys
          ~smaller:left_define_call_graphs
          ~larger:right_define_call_graphs,
        CallGraph.WholeProgramCallGraph.merge_disjoint
          left_whole_program_call_graph
          right_whole_program_call_graph )
    in
    let define_call_graphs = CallGraph.SharedMemory.create () |> CallGraph.SharedMemory.add_only in
    let empty_define_call_graphs = CallGraph.SharedMemory.AddOnly.create_empty define_call_graphs in
    let define_call_graphs, whole_program_call_graph =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:(empty_define_call_graphs, CallGraph.WholeProgramCallGraph.empty)
        ~map:(fun modules_and_callables ->
          List.fold
            modules_and_callables
            ~init:(empty_define_call_graphs, CallGraph.WholeProgramCallGraph.empty)
            ~f:parse_module_call_graphs)
        ~reduce
        ~inputs:(Map.to_alist module_definitions_map)
        ()
    in
    Log.info "Parsed call graphs from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed call graphs from pyrefly"
      ~phase_name:"Parsing call graphs from pyrefly"
      ~timer
      ();
    let define_call_graphs = CallGraph.SharedMemory.from_add_only define_call_graphs in
    { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }


  let parse_type_errors { pyrefly_directory; _ } =
    let path_of_module_path = function
      | ModulePath.Filesystem path -> ArtifactPath.raw path |> PyrePath.absolute
      | Namespace path
      | Memory path
      | BundledTypeshed path
      | BundledTypeshedThirdParty path ->
          PyrePath.absolute path
    in
    let instantiate_error
        {
          TypeErrors.PyreflyError.module_name = _;
          module_path;
          location =
            {
              start = { line = start_line; column = start_column };
              stop = { line = stop_line; column = stop_column };
            };
          kind;
          message;
        }
      =
      {
        Analysis.AnalysisError.Instantiated.line = start_line;
        column = start_column;
        stop_line;
        stop_column;
        path = path_of_module_path module_path;
        code = 0;
        name = kind;
        description = message;
        concise_description = message;
        define = "";
      }
    in
    let { TypeErrors.errors } =
      let errors_capnp_path = PyrePath.append pyrefly_directory ~element:"errors.capnp.bin" in
      if PyrePath.file_exists errors_capnp_path then
        PyreflyReportCapnp.TypeErrors.from_path_exn ~pyrefly_directory
      else
        PyreflyReportJson.TypeErrors.from_path_exn ~pyrefly_directory
    in
    List.map ~f:instantiate_error errors


  let get_type_of_expression { type_of_expressions_shared_memory; _ } ~qualifier ~location =
    match type_of_expressions_shared_memory with
    | None ->
        failwith
          "Using `PyreflyApi.ReadOnly.get_type_of_expression` before calling \
           `parse_type_of_expressions`."
    | Some type_of_expressions_shared_memory ->
        TypeOfExpressionsSharedMemory.get
          type_of_expressions_shared_memory
          {
            TypeOfExpressionsSharedMemory.Key.module_qualifier =
              ModuleQualifier.from_reference_unchecked qualifier;
            location;
          }


  module Type = struct
    let scalar_properties _ pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith "ReadOnly.Type.type_properties: trying to use a pyre1 type with a pyrefly API."
      | Some { Pyre1Api.PyreflyType.scalar_properties; _ } -> scalar_properties


    let get_class_names { class_id_to_qualified_name_shared_memory; _ } pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith "ReadOnly.Type.get_class_names: trying to use a pyre1 type with a pyrefly API."
      | Some { Pyre1Api.PyreflyType.class_names = None; _ } ->
          Analysis.PyrePysaEnvironment.ClassNamesFromType.not_a_class
      | Some { Pyre1Api.PyreflyType.class_names = Some { classes; is_exhaustive }; _ } ->
          let get_class_with_modifiers
              { Pyre1Api.PyreflyType.ClassWithModifiers.module_id; class_id; modifiers }
            =
            let class_name =
              ClassIdToQualifiedNameSharedMemory.get
                class_id_to_qualified_name_shared_memory
                {
                  GlobalClassId.module_id = ModuleId.from_int module_id;
                  local_class_id = LocalClassId.from_int class_id;
                }
              |> assert_shared_memory_key_exists (fun () -> "missing class id")
              |> FullyQualifiedName.to_reference
              |> Reference.show
            in
            { Pyre1Api.ClassWithModifiers.class_name; modifiers }
          in
          {
            Analysis.PyrePysaEnvironment.ClassNamesFromType.classes =
              List.map ~f:get_class_with_modifiers classes;
            is_exhaustive;
          }


    let is_dictionary_or_mapping { dict_classes; typing_mapping_classes; _ } pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith
            "ReadOnly.Type.is_dictionary_or_mapping: trying to use a pyre1 type with a pyrefly API."
      | Some { Pyre1Api.PyreflyType.class_names = None; _ } -> false
      | Some { Pyre1Api.PyreflyType.class_names = Some { classes; _ }; _ } ->
          List.exists
            classes
            ~f:(fun { Pyre1Api.PyreflyType.ClassWithModifiers.module_id; class_id; modifiers } ->
              let global_class_id =
                {
                  GlobalClassId.module_id = ModuleId.from_int module_id;
                  local_class_id = LocalClassId.from_int class_id;
                }
              in
              List.for_all modifiers ~f:(Pyre1Api.TypeModifier.equal Pyre1Api.TypeModifier.Optional)
              && (List.exists dict_classes ~f:(fun { TypeshedClass.global_class_id = id; _ } ->
                      GlobalClassId.equal global_class_id id)
                 || List.exists
                      typing_mapping_classes
                      ~f:(fun { TypeshedClass.global_class_id = id; _ } ->
                        GlobalClassId.equal global_class_id id)))
  end

  module ClassSummary = struct
    let has_custom_new { callable_metadata_shared_memory; _ } { PysaClassSummary.class_name; _ } =
      (* TODO(T225700656): We don't currently store a mapping from class to methods. For now, we use
         a dirty solution, we just check if class_name + __new__ is a valid function name. This
         might not work in tricky cases (if the class name clashes with a module name, for
         instance) *)
      let class_name = FullyQualifiedName.to_reference class_name in
      let method_name =
        Ast.Reference.create ~prefix:class_name "__new__"
        |> FullyQualifiedName.from_reference_unchecked
      in
      CallableMetadataSharedMemory.get callable_metadata_shared_memory method_name
      |> function
      | None -> false
      | Some { CallableMetadataSharedMemory.Value.metadata = { is_def_statement; _ }; _ } ->
          is_def_statement


    let is_dataclass _ { PysaClassSummary.metadata = { is_dataclass; _ }; _ } = is_dataclass

    let is_named_tuple _ { PysaClassSummary.metadata = { is_named_tuple; _ }; _ } = is_named_tuple

    let is_typed_dict _ { PysaClassSummary.metadata = { is_typed_dict; _ }; _ } = is_typed_dict

    let get_ordered_fields_with_predicate
        { class_fields_shared_memory; _ }
        { PysaClassSummary.class_name; _ }
        ~predicate
      =
      let fields =
        ClassFieldsSharedMemory.get class_fields_shared_memory class_name
        |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
      in
      let compare_by_location
          (_, { ClassField.location = left; _ })
          (_, { ClassField.location = right; _ })
        =
        Ast.Location.compare (Option.value_exn left) (Option.value_exn right)
      in
      fields
      |> SerializableStringMap.to_alist
      |> List.filter ~f:(fun (_, field) -> predicate field)
      |> List.filter ~f:(fun (_, { ClassField.location; _ }) -> Option.is_some location)
      |> List.sort ~compare:compare_by_location
      |> List.map ~f:fst


    let dataclass_ordered_attributes api class_summary =
      get_ordered_fields_with_predicate
        api
        class_summary
        ~predicate:(fun { ClassField.declaration_kind; _ } ->
          match declaration_kind with
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation
          | Some ClassFieldDeclarationKind.AssignedInBody ->
              (* Fields may be initialized via assignments. *)
              true
          | _ -> false)


    let typed_dictionary_attributes api class_summary =
      get_ordered_fields_with_predicate
        api
        class_summary
        ~predicate:(fun { ClassField.declaration_kind; _ } ->
          match declaration_kind with
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation -> true
          | _ -> false)


    let named_tuple_attributes api class_summary =
      get_ordered_fields_with_predicate
        api
        class_summary
        ~predicate:(fun { ClassField.declaration_kind; explicit_annotation; _ } ->
          match declaration_kind with
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation -> true
          | Some ClassFieldDeclarationKind.DeclaredWithoutAnnotation -> true
          | Some ClassFieldDeclarationKind.AssignedInBody ->
              (* Fields with default values are classified as AssignedInBody. Only include them if
                 they have an explicit annotation, to distinguish from enum members. *)
              Option.is_some explicit_annotation
          | _ -> false)
  end

  let named_tuple_attributes api class_name =
    if class_name |> get_class_summary api |> ClassSummary.is_named_tuple api then
      let mro = class_name :: class_mro api class_name in
      List.find_map
        ~f:(fun parent ->
          let attributes =
            parent |> get_class_summary api |> ClassSummary.named_tuple_attributes api
          in
          if List.is_empty attributes then
            None
          else
            Some attributes)
        mro
    else
      None
end

(* List of symbols exported from the 'builtins' module. To keep it short, this only contains symbols
   that users might want to annotate. *)
let builtins_symbols =
  String.Set.of_list
    [
      "__build_class__";
      "__import__";
      "abs";
      "aiter";
      "all";
      "anext";
      "any";
      "ascii";
      "BaseException";
      "bin";
      "bool";
      "bytearray";
      "bytes";
      "callable";
      "chr";
      "classmethod";
      "compile";
      "complex";
      "delattr";
      "dict";
      "dir";
      "enumerate";
      "eval";
      "Exception";
      "exec";
      "filter";
      "float";
      "format";
      "frozenset";
      "function";
      "GeneratorExit";
      "getattr";
      "globals";
      "hasattr";
      "hash";
      "hex";
      "id";
      "input";
      "int";
      "isinstance";
      "issubclass";
      "iter";
      "KeyboardInterrupt";
      "len";
      "list";
      "map";
      "max";
      "memoryview";
      "min";
      "next";
      "object";
      "open";
      "ord";
      "pow";
      "print";
      "property";
      "range";
      "repr";
      "reversed";
      "round";
      "set";
      "setattr";
      "slice";
      "sorted";
      "staticmethod";
      "str";
      "sum";
      "super";
      "SystemExit";
      "tuple";
      "type";
      "vars";
      "zip";
    ]


(* Unlike Pyre1, Pyrefly prefixes all builtins symbols with 'builtins.'. This function automatically
   adds 'builtins.' in front of references that are likely builtins symbols. The main use case is
   for taint models in .pysa files. *)
let add_builtins_prefix name =
  if Set.mem builtins_symbols (Reference.first name) then
    Reference.create_from_list ("builtins" :: Reference.as_list name)
  else
    name


let strip_builtins_prefix name =
  match Reference.as_list name with
  | "builtins" :: tail -> Reference.create_from_list tail
  | _ -> name


(* Given a fully qualified name for a function, method, class, attribute or global variable, return
   its 'symbolic' name. This removes any path prefix and suffixes such as `@setter` and `$2`. *)
let target_symbolic_name reference =
  (* We could technically get the proper module name and symbol name using shared memory, but we use
     basic string operations for performance reasons. *)
  let reference = Reference.as_list reference in
  let reference =
    (* First, strip the potential path prefixed to the module name, if any. *)
    if List.exists reference ~f:(fun s -> String.contains s ':') then
      reference
      |> String.concat ~sep:"."
      |> String.rsplit2_exn ~on:':'
      |> snd
      |> String.split ~on:'.'
    else
      reference
  in
  let reference =
    (* If '#' was added to distinguish symbols for different modules, replace it with a dot. *)
    if List.exists reference ~f:(fun s -> String.contains s '#') then
      reference
      |> String.concat ~sep:"."
      |> String.substr_replace_all ~pattern:"#" ~with_:"."
      |> String.split ~on:'.'
    else
      reference
  in
  let reference =
    (* Strip '$2' and '@setter' suffixes. Preserve '$toplevel' and '$class_toplevel' *)
    let strip_suffix name =
      let name =
        if Option.value ~default:(-1) (String.index name '$') >= 1 then
          String.rsplit2_exn name ~on:'$' |> fst
        else
          name
      in
      let name =
        if String.contains name '@' then
          String.rsplit2_exn name ~on:'@' |> fst
        else
          name
      in
      name
    in
    List.map ~f:strip_suffix reference
  in
  Reference.create_from_list reference


module ModelQueries = struct
  module Function = Pyre1Api.ModelQueries.Function
  module Global = Pyre1Api.ModelQueries.Global

  let resolve_qualified_name_to_global
      {
        ReadOnly.module_infos_shared_memory;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        callable_undecorated_signatures_shared_memory;
        class_fields_shared_memory;
        module_globals_shared_memory;
        _;
      }
      ~is_property_getter:_
      ~is_property_setter
      name
    =
    (* TODO(T225700656): For now, we only support looking up symbols in module names that are unique
       (i.e, the module qualifier is not prefixed by the path) *)
    let name =
      if is_property_setter then
        Reference.create (Format.asprintf "%a@setter" Reference.pp name)
      else
        name
    in
    let is_module_qualifier name =
      ModuleInfosSharedMemory.get
        module_infos_shared_memory
        (ModuleQualifier.from_reference_unchecked name)
      |> Option.is_some
    in
    let is_class_name name =
      ClassMetadataSharedMemory.get
        class_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked name)
      |> Option.is_some
    in
    let is_class_attribute name =
      let last_name = Reference.last name in
      Reference.prefix name
      >>| FullyQualifiedName.from_reference_unchecked
      >>= ClassFieldsSharedMemory.get class_fields_shared_memory
      >>| (fun fields -> SerializableStringMap.mem last_name fields)
      |> Option.value ~default:false
    in
    let is_module_global_variable name =
      let last_name = Reference.last name in
      Reference.prefix name
      >>| ModuleQualifier.from_reference_unchecked
      >>= ModuleGlobalsSharedMemory.get module_globals_shared_memory
      >>| (fun globals -> SerializableStringMap.mem last_name globals)
      |> Option.value ~default:false
    in
    (* Check if this is a valid function first. *)
    match
      CallableMetadataSharedMemory.get
        callable_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked name)
    with
    | Some
        {
          CallableMetadataSharedMemory.Value.metadata =
            { is_property_getter; is_property_setter; parent_is_class; _ };
          _;
        } ->
        let undecorated_signatures =
          CallableUndecoratedSignaturesSharedMemory.get
            callable_undecorated_signatures_shared_memory
            (FullyQualifiedName.from_reference_unchecked name)
          |> assert_shared_memory_key_exists (fun () ->
                 "missing undecorated signatures for callable")
        in
        Some
          (Global.Function
             {
               Function.define_name = name;
               imported_name = None;
               undecorated_signatures = Some undecorated_signatures;
               is_property_getter;
               is_property_setter;
               is_method = parent_is_class;
             })
    | None ->
        if is_module_qualifier name then
          Some Global.Module
        else if is_class_name name then
          Some (Global.Class { class_name = Reference.show name })
        else if is_class_attribute name then
          (* Functions might also be considered class attributes by pyrefly, so this should be
             checked last. *)
          Some (Global.ClassAttribute { name })
        else if is_module_global_variable name then
          Some (Global.ModuleGlobal { name })
        else
          None


  let class_method_signatures
      {
        ReadOnly.class_metadata_shared_memory;
        module_callables_shared_memory;
        callable_define_signature_shared_memory;
        _;
      }
      class_name
    =
    match
      ClassMetadataSharedMemory.get
        class_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked class_name)
    with
    | Some { ClassMetadataSharedMemory.Metadata.module_qualifier; _ } ->
        (* TODO(T225700656): We should check if the callable is a method using the parent_is_class
           field. *)
        let is_method_for_class callable_name =
          Reference.equal
            (callable_name
            |> FullyQualifiedName.to_reference
            |> Reference.prefix
            |> Option.value ~default:Reference.empty)
            class_name
          && not
               (String.equal
                  (FullyQualifiedName.last callable_name)
                  Statement.class_toplevel_define_name)
        in
        let add_signature callable_name =
          let signature =
            CallableDefineSignatureSharedMemory.get
              callable_define_signature_shared_memory
              callable_name
            |> assert_shared_memory_key_exists (fun () -> "missing signature for callable")
            |> AstResult.to_option
            >>| Node.value
          in
          FullyQualifiedName.to_reference callable_name, signature
        in
        ModuleCallablesSharedMemory.get module_callables_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
        |> List.filter ~f:is_method_for_class
        |> List.map ~f:add_signature
        |> Option.some
    | None -> None
end

module InContext = struct
  type t =
    | FunctionScope of {
        api: ReadOnly.t;
        module_qualifier: ModuleQualifier.t;
        define_name: Ast.Reference.t;
        call_graph: CallGraph.DefineCallGraph.t;
      }
    | StatementScope of {
        api: ReadOnly.t;
        module_qualifier: ModuleQualifier.t;
        (* Define name of the function. Note that this might be a decorated target, which is not
           visible by pyrefly. *)
        define_name: Ast.Reference.t;
        call_graph: CallGraph.DefineCallGraph.t;
        statement_key: int;
      }

  let create_at_function_scope api ~module_qualifier ~define_name ~call_graph =
    FunctionScope
      {
        api;
        module_qualifier = ModuleQualifier.from_reference_unchecked module_qualifier;
        define_name;
        call_graph;
      }


  let create_at_statement_scope api ~module_qualifier ~define_name ~call_graph ~statement_key =
    StatementScope
      {
        api;
        module_qualifier = ModuleQualifier.from_reference_unchecked module_qualifier;
        define_name;
        statement_key;
        call_graph;
      }


  let pyre_api = function
    | FunctionScope { api; _ } -> api
    | StatementScope { api; _ } -> api


  let define_name = function
    | FunctionScope { define_name; _ } -> define_name
    | StatementScope { define_name; _ } -> define_name


  let call_graph = function
    | FunctionScope { call_graph; _ } -> call_graph
    | StatementScope { call_graph; _ } -> call_graph


  let module_qualifier = function
    | FunctionScope { module_qualifier; _ } -> ModuleQualifier.to_reference module_qualifier
    | StatementScope { module_qualifier; _ } -> ModuleQualifier.to_reference module_qualifier


  let is_global _ ~reference:_ =
    (* TODO(T225700656): Support is_global *)
    failwith "unimplemented: PyreflyApi.InContext.is_global"


  let resolve_reference _ _ =
    (* TODO(T225700656): Support resolve_reference *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_reference"


  let resolve_assignment api _ = api

  let resolve_generators api _ = api

  let resolve_expression_to_type _ _ =
    (* TODO(T225700656): Support resolve_expression_to_type *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_expression_to_type"


  let resolve_attribute_access _ ~base_type:_ ~attribute:_ =
    (* TODO(T225700656): Support resolve_attribute_access *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_attribute_access"


  let fallback_attribute _ ?accessed_through_class:_ ?type_for_lookup:_ ~name:_ _ =
    (* TODO(T225700656): Support fallback_attribute *)
    failwith "unimplemented: PyreflyApi.InContext.fallback_attribute"


  let root_of_identifier pyrefly_in_context ~location ~identifier =
    match
      CallGraph.DefineCallGraph.resolve_identifier
        ~location
        ~identifier
        (call_graph pyrefly_in_context)
    with
    | Some { CallGraph.IdentifierCallees.captured_variables = captured_variable :: _; _ } ->
        AccessPath.Root.CapturedVariable captured_variable
    | _ -> AccessPath.Root.Variable identifier


  let propagate_captured_variable pyrefly_in_context ~defining_function ~name =
    if Reference.equal defining_function (define_name pyrefly_in_context) then
      (* We have reached the function that originated the variable. We should treat it as a regular
         variable now. *)
      AccessPath.Root.Variable name
    else
      AccessPath.Root.CapturedVariable
        (AccessPath.CapturedVariable.FromFunction { name; defining_function })
end

(* Exposed for testing purposes *)
module Testing = struct
  module Module = ReadWrite.Module

  let create_module_qualifiers = ReadWrite.create_module_qualifiers

  module Definition = ReadWrite.DefinitionCollector.Definition
  module QualifiedDefinition = ReadWrite.DefinitionCollector.QualifiedDefinition

  let create_fully_qualified_names
      ~module_qualifier
      ~module_exists
      ~class_definitions
      ~function_definitions
    =
    ReadWrite.DefinitionCollector.Tree.from_definitions ~function_definitions ~class_definitions
    |> ReadWrite.DefinitionCollector.Tree.create_qualified_names
    |> ReadWrite.DefinitionCollector.Tree.check_module_overlaps ~module_qualifier ~module_exists
    |> ReadWrite.DefinitionCollector.Tree.collect_definitions ~module_qualifier
    |> ReadWrite.DefinitionCollector.add_toplevel_defines ~module_qualifier
end
