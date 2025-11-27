(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyrePysaEnvironment provides the Pyre environment interfaces used by the Pysa OCaml codebase,
   including both taint analysis and model queries. *)

open Core
open Pyre
module PyreType = Type

(* Scalar properties of a type (it is a bool/int/float/etc.) *)
module ScalarTypeProperties = struct
  type t = int [@@deriving compare, equal, sexp, hash]

  let boolean_bit = 0b1

  let integer_bit = 0b10

  let float_bit = 0b100

  let enumeration_bit = 0b1000

  let get_bit pattern value = Int.equal (Int.bit_and pattern value) pattern

  let set_bit pattern value = Int.bit_or pattern value

  let is_boolean = get_bit boolean_bit

  let is_integer = get_bit integer_bit

  let is_float = get_bit float_bit

  let is_enumeration = get_bit enumeration_bit

  let set_boolean flag = set_bit (if flag then boolean_bit else 0)

  let set_integer flag = set_bit (if flag then integer_bit else 0)

  let set_float flag = set_bit (if flag then float_bit else 0)

  let set_enumeration flag = set_bit (if flag then enumeration_bit else 0)

  let pp formatter value =
    let add_if condition tag tags =
      if condition then
        tag :: tags
      else
        tags
    in
    []
    |> add_if (is_enumeration value) "enum"
    |> add_if (is_float value) "float"
    |> add_if (is_integer value) "int"
    |> add_if (is_boolean value) "bool"
    |> String.concat ~sep:"|"
    |> Format.fprintf formatter "{%s}"


  let show = Format.asprintf "%a" pp

  let none = 0

  let unknown = none

  let bool = 0 |> set_bit boolean_bit |> set_bit integer_bit |> set_bit float_bit

  let integer = 0 |> set_bit integer_bit |> set_bit float_bit

  let enumeration = set_bit enumeration_bit 0

  let create ~is_boolean ~is_integer ~is_float ~is_enumeration =
    0
    |> set_boolean is_boolean
    |> set_integer is_integer
    |> set_float is_float
    |> set_enumeration is_enumeration
end

(* Result of extracting class names from a type. *)
module ClassNamesFromType = struct
  type t = {
    class_names: string list;
    stripped_coroutine: bool;
    stripped_optional: bool;
    stripped_readonly: bool;
    unbound_type_variable: bool;
    is_exhaustive: bool;
        (* Is there an element (after stripping) that isn't a class name? For instance:
           get_class_name(Union[A, Callable[...])) = { class_names = [A], is_exhaustive = false } *)
  }

  let from_class_name class_name =
    {
      class_names = [class_name];
      stripped_coroutine = false;
      stripped_optional = false;
      stripped_readonly = false;
      unbound_type_variable = false;
      is_exhaustive = true;
    }


  let not_a_class =
    {
      class_names = [];
      stripped_coroutine = false;
      stripped_optional = false;
      stripped_readonly = false;
      unbound_type_variable = false;
      is_exhaustive = false;
    }


  let join left right =
    {
      class_names = List.rev_append left.class_names right.class_names;
      stripped_coroutine = left.stripped_coroutine || right.stripped_coroutine;
      stripped_optional = left.stripped_optional || right.stripped_optional;
      stripped_readonly = left.stripped_readonly || right.stripped_readonly;
      unbound_type_variable = left.unbound_type_variable || right.unbound_type_variable;
      is_exhaustive = left.is_exhaustive && right.is_exhaustive;
    }
end

module PyreflyType = struct
  module ClassNamesFromType = struct
    type t = {
      class_names: (int * int) list;
      stripped_coroutine: bool;
      stripped_optional: bool;
      stripped_readonly: bool;
      unbound_type_variable: bool;
      is_exhaustive: bool;
    }
    [@@deriving equal, compare, show]

    let from_class (module_id, class_id) =
      {
        class_names = [module_id, class_id];
        stripped_coroutine = false;
        stripped_optional = false;
        stripped_readonly = false;
        unbound_type_variable = false;
        is_exhaustive = true;
      }
  end

  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    class_names: ClassNamesFromType.t option;
  }
  [@@deriving equal, compare, show]

  let top =
    { string = "unknown"; scalar_properties = ScalarTypeProperties.none; class_names = None }
end

(* Minimal abstraction for a type, provided from Pyre1 or Pyrefly and used by Pysa. See
   `ReadOnly.Type` for more functions. *)
module PysaType = struct
  (* TODO(T225700656): We currently expose the representation for Pyrefly here instead of exposing
     it in Interprocedural.Pyrefly, because the current module defines other types that depend on
     `PysaType`, such as `FunctionDefinition.t`. The alternative would require to copy/paste all
     these type definitions, which is not ideal. *)
  type t =
    | Pyre1 of PyreType.t
    | Pyrefly of PyreflyType.t
  [@@deriving equal, compare, show]

  let from_pyre1_type annotation = Pyre1 annotation

  let from_pyrefly_type type_ = Pyrefly type_

  let as_pyrefly_type = function
    | Pyrefly type_ -> Some type_
    | Pyre1 _ -> None


  let as_pyre1_type = function
    | Pyre1 type_ -> Some type_
    | Pyrefly _ -> None


  (* Pretty print the type, usually meant for the user *)
  let pp_concise formatter = function
    | Pyre1 type_ -> PyreType.pp_concise formatter type_
    | Pyrefly { PyreflyType.string; _ } ->
        (* Technically, this is the fully qualified representation, but we use it as the concise
           representation for now. *)
        Format.fprintf formatter "%s" string


  let show_fully_qualified = function
    | Pyre1 type_ -> PyreType.show type_
    | Pyrefly { PyreflyType.string; _ } -> string


  let weaken_literals = function
    | Pyre1 type_ -> Pyre1 (Type.weaken_literals type_)
    | Pyrefly type_ -> Pyrefly type_ (* pyrefly already weakens literals before exporting types *)
end

module PyreClassSummary = ClassSummary

(* Abstraction for information about a class, provided from Pyre1 or Pyrefly and used by Pysa. See
   `ReadOnly.ClassSummary` for more functions. *)
module PysaClassSummary = struct
  type nonrec t = PyreClassSummary.t Ast.Node.t

  let find_attribute class_summary attribute_name =
    class_summary
    |> Ast.Node.value
    |> PyreClassSummary.attributes
    |> Ast.Identifier.SerializableMap.find_opt attribute_name


  let get_attributes class_summary =
    class_summary
    |> Ast.Node.value
    |> PyreClassSummary.attributes
    |> Ast.Identifier.SerializableMap.data
end

module AstResult = struct
  type 'a t =
    | Some of 'a
    | ParseError (* callable in a module that failed to parse *)
    | TestFile (* callable in a module marked with is_test = true *)
    | Synthesized (* callable in a synthesized class or function *)
    | Pyre1NotFound (* callable not found - only raised when using pyre1 *)

  let to_option = function
    | ParseError -> None
    | TestFile -> None
    | Synthesized -> None
    | Pyre1NotFound -> None
    | Some ast -> Some ast


  let value_exn ~message = function
    | Some value -> value
    | ParseError -> Format.sprintf "%s (reason: parser error)" message |> failwith
    | TestFile -> Format.sprintf "%s (reason: within a test file)" message |> failwith
    | Synthesized -> Format.sprintf "%s (reason: synthesized function)" message |> failwith
    | Pyre1NotFound -> Format.sprintf "%s (reason: not found)" message |> failwith


  let map ~f = function
    | Some ast -> Some (f ast)
    | ParseError -> ParseError
    | TestFile -> TestFile
    | Synthesized -> Synthesized
    | Pyre1NotFound -> Pyre1NotFound


  let map_node ~f = function
    | Some { Ast.Node.value = ast; location } -> Some { Ast.Node.value = f ast; location }
    | ParseError -> ParseError
    | TestFile -> TestFile
    | Synthesized -> Synthesized
    | Pyre1NotFound -> Pyre1NotFound
end

let absolute_source_path_of_qualifier ~lookup_source read_only_type_environment =
  let source_code_api =
    read_only_type_environment |> TypeEnvironment.ReadOnly.get_untracked_source_code_api
  in
  SourcePaths.absolute_source_path_of_qualifier ~lookup_source ~source_code_api


(* Private helper module for creating a TypeEnvironment the way Pysa wants to. *)
module PysaTypeEnvironment = struct
  let create ~configuration ~decorator_configuration =
    let configuration =
      (* In order to get an accurate call graph and type information, we need to ensure that we
         schedule a type check for external files. *)
      (* TODO(T180476103) Remove the need for this by using explicit_qualifiers, and delete this
         flag from the configuration + environment logic. *)
      { configuration with Configuration.Analysis.analyze_external_sources = true }
    in
    let () = DecoratorPreprocessing.setup_preprocessing decorator_configuration in
    let errors_environment =
      EnvironmentControls.create
        ~populate_call_graph:false
        ~string_annotation_preserve_location:false
        configuration
      |> ErrorsEnvironment.create_with_ast_environment
    in
    ErrorsEnvironment.AssumeDownstreamNeverNeedsUpdates.type_environment errors_environment


  let qualifiers_and_definitions ~scheduler ~scheduler_policies type_environment =
    Log.info "Starting type checking...";
    PyreProfiling.track_shared_memory_usage ~name:"Before legacy type check" ();
    let qualifiers =
      TypeEnvironment.AssumeGlobalModuleListing.global_module_paths_api type_environment
      |> GlobalModulePathsApi.type_check_qualifiers
    in
    Log.info "Found %d modules" (List.length qualifiers);
    let definitions =
      TypeEnvironment.collect_definitions ~scheduler ~scheduler_policies type_environment qualifiers
    in
    Log.info "Found %d functions" (List.length definitions);
    qualifiers, definitions


  let populate ~scheduler ~scheduler_policies type_environment definitions =
    let () =
      TypeEnvironment.populate_for_definitions
        ~scheduler
        ~scheduler_policies
        type_environment
        definitions
    in
    Statistics.event
      ~section:`Memory
      ~name:"shared memory size post-typecheck"
      ~integers:["size", Memory.heap_size ()]
      ();
    PyreProfiling.track_shared_memory_usage ~name:"After legacy type check" ();
    ()
end

(* Api used in the top-level code of `pyre analyze`, where we need read-write access in order to
   perform operations like loading / saving the Pysa cache cache and scheduling type analysis in
   parallel. *)
module ReadWrite = struct
  type t = { type_environment: TypeEnvironment.t }

  (* Constructors *)

  (* Note: this function assumes that Cache.ml logic has initialized shared memory (which is global,
     and loading is purely side-effect based) from a cache dump already. *)
  let load_from_cache ~configuration =
    let type_environment =
      let controls = EnvironmentControls.create configuration in
      TypeEnvironment.AssumeAstEnvironment.load_without_dependency_keys controls
    in
    { type_environment }


  let create_with_cold_start
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~decorator_configuration
      ~skip_type_checking_callables
      ~callback_with_qualifiers_and_definitions
    =
    let type_environment = PysaTypeEnvironment.create ~configuration ~decorator_configuration in
    let qualifiers, definitions =
      PysaTypeEnvironment.qualifiers_and_definitions ~scheduler ~scheduler_policies type_environment
    in
    let () =
      callback_with_qualifiers_and_definitions
        (TypeEnvironment.read_only type_environment |> absolute_source_path_of_qualifier)
        qualifiers
        definitions
    in
    let definitions =
      List.filter
        ~f:(fun define_name ->
          not (Ast.Reference.SerializableSet.mem define_name skip_type_checking_callables))
        definitions
    in
    PysaTypeEnvironment.populate ~scheduler ~scheduler_policies type_environment definitions;
    { type_environment }


  (* Helpers to access underlying environment and configuration *)

  let read_write_type_environment { type_environment } = type_environment

  let read_write_module_tracker api =
    read_write_type_environment api
    |> TypeEnvironment.AssumeAstEnvironment.ast_environment
    |> AstEnvironment.module_tracker


  let type_environment { type_environment } = TypeEnvironment.read_only type_environment

  let configuration api =
    type_environment api |> TypeEnvironment.ReadOnly.controls |> EnvironmentControls.configuration


  (* Interface used in cache management *)

  let module_paths api = read_write_module_tracker api |> ModuleTracker.module_paths

  (* Reload module paths from disk, ignoring any caches. Used by Pysa's cache invalidation logic. *)
  let module_paths_from_disk api =
    configuration api
    |> EnvironmentControls.create
    |> ModuleTracker.create
    |> ModuleTracker.module_paths


  let all_module_paths api = read_write_module_tracker api |> ModuleTracker.all_module_paths

  let artifact_path_of_module_path api module_path =
    let configuration = configuration api in
    ArtifactPaths.artifact_path_of_module_path ~configuration module_path


  let save api =
    read_write_type_environment api
    |> TypeEnvironment.AssumeAstEnvironment.store_without_dependency_keys


  (* Aggressively shrink shared memory by dropping all raw sources *)
  let purge_sources_from_shared_memory api =
    Log.info "Purging source files from shared memory...";
    let timer = Timer.start () in
    let ast_environment =
      read_write_type_environment api |> TypeEnvironment.AssumeAstEnvironment.ast_environment
    in
    let qualifiers =
      read_write_type_environment api
      |> TypeEnvironment.AssumeGlobalModuleListing.global_module_paths_api
      |> GlobalModulePathsApi.explicit_qualifiers
    in
    AstEnvironment.remove_sources ast_environment qualifiers;
    Memory.SharedMemory.collect `aggressive;
    Statistics.performance
      ~name:"Purged source files from shared memory"
      ~phase_name:"Purging source files from shared memory"
      ~timer
      ();
    ()
end

module MethodInQualifier = struct
  type t = {
    class_name: Ast.Reference.t;
    method_name: string;
    is_property_setter: bool;
  }
end

module ReadOnly = struct
  type t = {
    type_environment: TypeEnvironment.ReadOnly.t;
    global_module_paths_api: GlobalModulePathsApi.t;
  }

  let of_read_write_api { ReadWrite.type_environment } =
    {
      type_environment = TypeEnvironment.read_only type_environment;
      global_module_paths_api =
        TypeEnvironment.AssumeGlobalModuleListing.global_module_paths_api type_environment;
    }


  let create ~type_environment ~global_module_paths_api =
    { type_environment; global_module_paths_api }


  let type_environment { type_environment; _ } = type_environment

  let global_module_paths_api { global_module_paths_api; _ } = global_module_paths_api

  let global_resolution api = type_environment api |> TypeEnvironment.ReadOnly.global_resolution

  let unannotated_global_environment api =
    type_environment api |> TypeEnvironment.ReadOnly.unannotated_global_environment


  let source_code_api api =
    type_environment api |> TypeEnvironment.ReadOnly.get_untracked_source_code_api


  let contextless_resolution api =
    TypeCheck.resolution
      (global_resolution api)
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)


  (* Interface to get source paths; used when dumping stats from Pysa *)

  let absolute_source_path_of_qualifier ~lookup_source api =
    type_environment api |> absolute_source_path_of_qualifier ~lookup_source


  let explicit_qualifiers api =
    global_module_paths_api api |> GlobalModulePathsApi.explicit_qualifiers


  let parse_annotation api = global_resolution api |> GlobalResolution.parse_annotation

  let get_class_summary api = global_resolution api |> GlobalResolution.get_class_summary

  let get_class_decorators_opt api class_name =
    match get_class_summary api class_name with
    | Some { Ast.Node.value = { PyreClassSummary.decorators; _ }; _ } -> AstResult.Some decorators
    | None -> AstResult.Pyre1NotFound


  let get_class_attributes api ~include_generated_attributes ~only_simple_assignments class_name =
    match get_class_summary api class_name with
    | Some { Ast.Node.value = class_summary; _ } ->
        let attributes = PyreClassSummary.attributes ~include_generated_attributes class_summary in
        let constructor_attributes = PyreClassSummary.constructor_attributes class_summary in
        let all_attributes =
          Ast.Identifier.SerializableMap.union
            (fun _ x _ -> Some x)
            attributes
            constructor_attributes
        in
        let get_attribute attribute_name attribute accumulator =
          if not only_simple_assignments then
            attribute_name :: accumulator
          else
            match Ast.Node.value attribute with
            | { PyreClassSummary.Attribute.kind = Simple _; _ } -> attribute_name :: accumulator
            | _ -> accumulator
        in
        Some (Ast.Identifier.SerializableMap.fold get_attribute all_attributes [])
    | None -> None


  let get_class_attribute_annotation api ~include_generated_attributes ~class_name ~attribute =
    let get_annotation = function
      | { PyreClassSummary.Attribute.kind = Simple { PyreClassSummary.Attribute.annotation; _ }; _ }
        ->
          annotation
      | _ -> None
    in
    get_class_summary api class_name
    >>| Ast.Node.value
    >>= fun class_summary ->
    match
      PyreClassSummary.constructor_attributes class_summary
      |> Ast.Identifier.SerializableMap.find_opt attribute
      >>| Ast.Node.value
      >>| get_annotation
    with
    | Some annotation -> annotation
    | None ->
        PyreClassSummary.attributes ~include_generated_attributes class_summary
        |> Ast.Identifier.SerializableMap.find_opt attribute
        >>| Ast.Node.value
        >>= get_annotation


  let get_unannotated_global api =
    unannotated_global_environment api
    |> UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global


  let get_global_annotation api reference =
    match get_unannotated_global api reference with
    | Some (SimpleAssign { explicit_annotation; _ }) -> explicit_annotation
    | _ -> None


  let class_immediate_parents api = global_resolution api |> GlobalResolution.immediate_parents

  let class_mro api = global_resolution api |> GlobalResolution.successors

  let get_define_names_for_qualifier api =
    (* In Pysa, all qualifiers are "in project" so this is always safe to use. *)
    global_resolution api |> GlobalResolution.get_define_names_for_qualifier_in_project


  let get_qualifier_top_level_define_name _ qualifier =
    Ast.Reference.create ~prefix:qualifier Ast.Statement.toplevel_define_name


  let parse_reference api = global_resolution api |> GlobalResolution.parse_reference

  let module_exists api = global_resolution api |> GlobalResolution.module_exists

  let class_exists api = global_resolution api |> GlobalResolution.class_exists

  let get_define_body api =
    (* In Pysa, all qualifiers are "in project" so this is always safe to use. *)
    global_resolution api |> GlobalResolution.get_define_body_in_project


  let get_callable_captures api define_name =
    match get_define_body api define_name with
    | Some { Ast.Node.value = { Ast.Statement.Define.captures; _ }; _ } ->
        List.map ~f:(fun capture -> capture.Ast.Statement.Define.Capture.name) captures
    | _ -> []


  let get_callable_return_annotations
      api
      ~define_name:_
      ~define:{ Ast.Statement.Define.signature = { return_annotation; _ }; _ }
    =
    match return_annotation with
    | Some return_annotation -> [PysaType.from_pyre1_type (parse_annotation api return_annotation)]
    | None -> []


  let get_callable_parameter_annotations api ~define_name:_ parameters =
    let attach_annotation
        ({
           TaintAccessPath.NormalizedParameter.original =
             { Ast.Node.value = { Ast.Expression.Parameter.annotation; _ }; _ };
           _;
         } as parameter)
      =
      let annotation =
        annotation
        >>| parse_annotation api
        >>| PysaType.from_pyre1_type
        >>| List.return
        |> Option.value ~default:[]
      in
      parameter, annotation
    in
    List.map parameters ~f:attach_annotation


  let resolve_define api = global_resolution api |> GlobalResolution.resolve_define

  let resolve_define_undecorated api =
    global_resolution api |> GlobalResolution.resolve_define_undecorated


  let global api = global_resolution api |> GlobalResolution.global

  let get_variable api = GlobalResolution.get_variable (global_resolution api)

  let get_overriden_base_method api ~class_name ~method_name =
    let ancestor =
      GlobalResolution.overrides
        (global_resolution api)
        (Ast.Reference.show class_name)
        ~name:method_name
    in
    let open Core.Option.Monad_infix in
    ancestor
    >>= fun ancestor ->
    let parent_annotation = AnnotatedAttribute.parent ancestor in
    let ancestor_parent =
      Type.Primitive parent_annotation
      |> Type.expression
      |> Ast.Expression.show
      |> Ast.Reference.create
    in
    (* This special case exists only for `type`. Our override lookup for a class C first looks at
       the regular MRO. If that fails, it looks for Type[C]'s MRO. However, when C is type, this
       causes a cycle to get registered. *)
    if Ast.Reference.equal ancestor_parent class_name then
      None
    else
      Some (Ast.Reference.create ~prefix:ancestor_parent method_name)


  let annotation_parser api = global_resolution api |> GlobalResolution.annotation_parser

  let less_or_equal api = global_resolution api |> GlobalResolution.less_or_equal

  let resolve_exports api = global_resolution api |> GlobalResolution.resolve_exports

  let location_of_global api = global_resolution api |> GlobalResolution.location_of_global

  let get_function_definition api =
    (* In Pysa, all qualifiers are "in project" so this is always safe to use. *)
    global_resolution api |> GlobalResolution.get_function_definition_in_project


  let attribute_from_class_name api =
    global_resolution api |> GlobalResolution.attribute_from_class_name


  let has_transitive_successor api =
    global_resolution api |> GlobalResolution.has_transitive_successor


  (* Check whether `successor` extends `predecessor`.
   * Returns false on untracked types.
   * Returns `reflexive` if `predecessor` and `successor` are equal. *)
  let has_transitive_successor_ignoring_untracked api ~reflexive ~predecessor ~successor =
    if String.equal predecessor successor then
      reflexive
    else
      try has_transitive_successor api ~successor predecessor with
      | ClassHierarchy.Untracked untracked_type ->
          Log.warning
            "Found untracked type `%s` when checking whether `%s` is a subclass of `%s`. This \
             could lead to false negatives."
            untracked_type
            successor
            predecessor;
          false


  let exists_matching_class_decorator api =
    unannotated_global_environment api
    |> UnannotatedGlobalEnvironment.ReadOnly.exists_matching_class_decorator


  let generic_parameters_as_variables api =
    global_resolution api |> GlobalResolution.generic_parameters_as_variables


  let source_of_qualifier api = source_code_api api |> SourceCodeApi.source_of_qualifier

  (* There isn't a great way of testing whether a file only contains tests in Python.
   * We currently use the following heuristics:
   * - If a class inherits from `unittest.TestCase`, we assume this is a test file.
   * - If `pytest` is imported and at least one function starts with `test_`, we assume this is a test file.
   *)
  let source_is_unit_test api ~source =
    let open Ast in
    let open Statement in
    let is_unittest () =
      let is_unittest_class { Node.value = { Class.name; _ }; _ } =
        try
          has_transitive_successor api ~successor:"unittest.case.TestCase" (Reference.show name)
        with
        | ClassHierarchy.Untracked _ -> false
      in
      List.exists (Preprocessing.classes source) ~f:is_unittest_class
    in
    let is_pytest () =
      let imports_pytest () =
        let has_pytest_prefix = Reference.is_prefix ~prefix:(Reference.create "pytest") in
        let is_pytest_import { Node.value; _ } =
          match value with
          | Statement.Import { from = Some { Node.value; _ }; _ } when has_pytest_prefix value ->
              true
          | Statement.Import { imports; _ }
            when List.exists imports ~f:(fun { Node.value = { name; _ }; _ } ->
                     has_pytest_prefix name) ->
              true
          | _ -> false
        in
        List.exists source.statements ~f:is_pytest_import
      in
      let has_test_function () =
        let is_test_function { Node.value = { Define.signature = { name; _ }; _ }; _ } =
          Reference.last name |> String.is_prefix ~prefix:"test_"
        in
        List.exists (Preprocessing.defines source) ~f:is_test_function
      in
      imports_pytest () && has_test_function ()
    in
    is_unittest () || is_pytest ()


  let get_class_names_for_qualifier api ~exclude_test_modules qualifier =
    match source_of_qualifier api qualifier with
    | None -> []
    | Some source ->
        if exclude_test_modules && source_is_unit_test api ~source then
          []
        else
          Preprocessing.classes source
          |> List.map ~f:(fun { Ast.Node.value = { Ast.Statement.Class.name; _ }; _ } -> name)


  let relative_path_of_qualifier api =
    source_code_api api |> SourceCodeApi.relative_path_of_qualifier


  let decorated_define api define =
    AnnotatedDefine.create define
    |> AnnotatedDefine.decorate ~resolution:(global_resolution api)
    |> AnnotatedDefine.define


  let named_tuple_attributes api receiver_class =
    let global_resolution = global_resolution api in
    if NamedTuple.is_named_tuple ~global_resolution ~annotation:(PyreType.Primitive receiver_class)
    then
      NamedTuple.field_names_from_class_name ~global_resolution receiver_class
    else
      None


  let resolve_expression_to_type_info api =
    contextless_resolution api |> Resolution.resolve_expression_to_type_info


  let all_classes api =
    unannotated_global_environment api
    |> UnannotatedGlobalEnvironment.ReadOnly.GlobalApis.all_classes
         ~global_module_paths_api:(global_module_paths_api api)


  let all_unannotated_globals api =
    unannotated_global_environment api
    |> UnannotatedGlobalEnvironment.ReadOnly.GlobalApis.all_unannotated_globals
         ~global_module_paths_api:(global_module_paths_api api)


  module Type = struct
    (* Returns whether the type is an int, float, bool or enum, after stripping Optional and
       Awaitable. *)
    let scalar_properties api = function
      | PysaType.Pyrefly _ -> failwith "cannot use a pyrefly type with the pyre API"
      | PysaType.Pyre1 annotation -> (
          let matches_at_leaves ~f annotation =
            let rec matches_at_leaves ~f annotation =
              match annotation with
              | Type.Any
              | Type.Bottom ->
                  false
              | Type.Union [Type.NoneType; annotation]
              | Type.Union [annotation; Type.NoneType]
              | Type.Parametric { name = "typing.Awaitable"; arguments = [Single annotation] } ->
                  matches_at_leaves ~f annotation
              | Type.Tuple (Concatenation concatenation) ->
                  Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
                  >>| (fun element -> matches_at_leaves ~f element)
                  |> Option.value ~default:(f annotation)
              | Type.Tuple (Type.OrderedTypes.Concrete annotations) ->
                  List.for_all annotations ~f:(matches_at_leaves ~f)
              | annotation -> f annotation
            in
            matches_at_leaves ~f annotation
          in
          try
            let is_boolean =
              matches_at_leaves annotation ~f:(fun left -> less_or_equal api ~left ~right:Type.bool)
            in
            let is_integer =
              matches_at_leaves annotation ~f:(fun left ->
                  less_or_equal api ~left ~right:Type.integer)
            in
            let is_float =
              matches_at_leaves annotation ~f:(fun left ->
                  less_or_equal api ~left ~right:Type.float)
            in
            let is_enumeration =
              matches_at_leaves annotation ~f:(fun left ->
                  less_or_equal api ~left ~right:Type.enumeration)
            in
            ScalarTypeProperties.create ~is_boolean ~is_integer ~is_float ~is_enumeration
          with
          | ClassHierarchy.Untracked untracked_type ->
              Log.warning
                "Found untracked type `%s` when checking the return type `%a` of a call. The \
                 return type will NOT be considered a scalar, which could lead to missing \
                 breadcrumbs."
                untracked_type
                Type.pp
                annotation;
              ScalarTypeProperties.none)


    (* Return a list of fully qualified class names that this type refers to, after
     * stripping Optional, ReadOnly and TypeVar.
     *
     * For instance:
     * Union[int, str] -> [int, str]
     * Optional[int] -> [int]
     * List[int] -> [List]
     * List[Dict[str, str]] -> [List]
     *)
    let get_class_names _ = function
      | PysaType.Pyrefly _ -> failwith "cannot use a pyrefly type with the pyre API"
      | PysaType.Pyre1 annotation ->
          let rec extract_class_names = function
            | Type.Bottom
            | Type.Top
            | Type.Any
            | Type.Literal _
            | Type.Callable _
            | Type.NoneType
            | Type.TypeOperation _
            | Type.Variable _
            | Type.RecursiveType _
            | Type.ParamSpecComponent _
            | Type.PyreReadOnly _ ->
                ClassNamesFromType.not_a_class
            | Type.Tuple _ -> ClassNamesFromType.from_class_name "tuple"
            | Type.Primitive class_name
            | Type.Parametric { name = class_name; _ } ->
                ClassNamesFromType.from_class_name class_name
            | Type.Union [] -> failwith "unreachable"
            | Type.Union (head :: tail) ->
                List.fold
                  ~init:(extract_class_names head)
                  ~f:(fun sofar annotation ->
                    ClassNamesFromType.join (extract_class_names annotation) sofar)
                  tail
          in
          let annotation, stripped_coroutine =
            match Type.coroutine_value annotation with
            | Some annotation -> annotation, true
            | None -> annotation, false
          in
          let annotation, stripped_optional =
            match Type.optional_value annotation with
            | Some annotation -> annotation, true
            | None -> annotation, false
          in
          let annotation, stripped_readonly =
            if Type.PyreReadOnly.is_readonly annotation then
              Type.PyreReadOnly.strip_readonly annotation, true
            else
              annotation, false
          in
          let annotation, unbound_type_variable =
            match annotation with
            | Type.Variable { constraints = Type.Record.TypeVarConstraints.Bound bound; _ } ->
                bound, true
            | annotation -> annotation, false
          in
          let class_name_result = extract_class_names annotation in
          {
            class_name_result with
            ClassNamesFromType.stripped_coroutine;
            stripped_optional;
            stripped_readonly;
            unbound_type_variable;
          }


    let is_dictionary_or_mapping _ = function
      | PysaType.Pyrefly _ -> failwith "cannot use a pyrefly type with the pyre API"
      | PysaType.Pyre1 annotation -> Type.is_dictionary_or_mapping annotation
  end

  module ClassSummary = struct
    let has_custom_new _ class_summary =
      class_summary
      |> Ast.Node.value
      |> PyreClassSummary.attributes ~include_generated_attributes:false ~in_test:false
      |> Ast.Identifier.SerializableMap.mem "__new__"


    let is_dataclass api class_summary =
      exists_matching_class_decorator
        api
        ~names:["dataclasses.dataclass"; "dataclass"]
        class_summary


    let is_named_tuple api { Ast.Node.value = { PyreClassSummary.name; _ }; _ } =
      has_transitive_successor_ignoring_untracked
        api
        ~reflexive:false
        ~predecessor:(Ast.Reference.show name)
        ~successor:"typing.NamedTuple"


    let is_typed_dict api { Ast.Node.value = { PyreClassSummary.name; _ }; _ } =
      has_transitive_successor_ignoring_untracked
        api
        ~reflexive:false
        ~predecessor:(Ast.Reference.show name)
        ~successor:"TypedDictionary"
      || has_transitive_successor_ignoring_untracked
           api
           ~reflexive:false
           ~predecessor:(Ast.Reference.show name)
           ~successor:"NonTotalTypedDictionary"


    let dataclass_ordered_attributes _ class_summary =
      let compare_by_location left right =
        Ast.Location.compare (Ast.Node.location left) (Ast.Node.location right)
      in
      class_summary
      |> Ast.Node.value
      |> PyreClassSummary.attributes ~include_generated_attributes:false ~in_test:false
      |> Ast.Identifier.SerializableMap.bindings
      |> List.unzip
      |> snd
      |> List.sort ~compare:compare_by_location
      |> List.map ~f:(fun { Ast.Node.value = { PyreClassSummary.Attribute.name; _ }; _ } -> name)


    let typed_dictionary_attributes api { Ast.Node.value = { ClassSummary.name; _ }; _ } =
      GlobalResolution.get_typed_dictionary
        (global_resolution api)
        (PyreType.Primitive (Ast.Reference.show name))
      >>| (fun { PyreType.TypedDictionary.fields; _ } -> fields)
      >>| List.map ~f:(fun { PyreType.TypedDictionary.name; required = _; _ } -> name)
      |> Option.value ~default:[]
  end

  let get_methods_for_qualifier api ~exclude_test_modules qualifier =
    let timer = Timer.start () in
    let extract_methods_from_class
        { Ast.Node.value = { Ast.Statement.Class.body; name = class_name; _ }; _ }
      =
      let methods =
        List.filter_map body ~f:(function
            | { Ast.Node.value = Ast.Statement.Statement.Define define; _ } ->
                Some
                  {
                    MethodInQualifier.class_name;
                    method_name = Ast.Statement.Define.unqualified_name define;
                    is_property_setter = Ast.Statement.Define.is_property_setter define;
                  }
            | _ -> None)
      in
      methods
    in
    let classes =
      match source_of_qualifier api qualifier with
      | Some source when (not exclude_test_modules) || not (source_is_unit_test api ~source) ->
          Preprocessing.classes source
      | _ -> []
    in
    let results = List.concat_map ~f:extract_methods_from_class classes in
    Statistics.performance
      ~randomly_log_every:1000
      ~always_log_time_threshold:1.0 (* Seconds *)
      ~name:"Fetched all methods for a qualifier"
      ~section:`DependencyGraph
      ~timer
      ();
    results
end

(* This module represents the API Pysa uses when it needs to interact with Pyre inside of a context
   with a non-global resolution. Any Pysa code that has to traverse what is in essence a typed AST
   (the actual mechanism is to use fixpoint states keyed on statement ids) will rely on this.

   The global `pyre_api` is packaged alongside the resolution for convenience because it is always
   sensible to ask for global information inside of a traversal (this is analagous to how Pyre
   allows you to extract a `GlobalResolution.t` from a `Resolution.t`). *)
module InContext = struct
  type t = {
    pyre_api: ReadOnly.t;
    resolution: Resolution.t;
    module_qualifier: Ast.Reference.t;
    define_name: Ast.Reference.t;
  }

  let create_at_function_scope pyre_api ~module_qualifier ~define_name =
    {
      pyre_api;
      resolution = ReadOnly.contextless_resolution pyre_api;
      module_qualifier;
      define_name;
    }


  let create_at_statement_scope pyre_api ~module_qualifier ~define_name ~define ~statement_key =
    let {
      Ast.Node.value = { Ast.Statement.Define.signature = { legacy_parent; _ }; _ };
      location = define_location;
    }
      =
      define
    in
    let pyre_define_name =
      FunctionDefinition.qualified_name_of_define
        ~module_name:module_qualifier
        (Ast.Node.value define)
    in
    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations
        (ReadOnly.type_environment pyre_api)
        pyre_define_name
        define_location
    in
    let resolution =
      TypeCheck.resolution_at_key
        ~global_resolution:(ReadOnly.global_resolution pyre_api)
        ~local_annotations
        ~parent:legacy_parent
        ~statement_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in
    { pyre_api; resolution; module_qualifier; define_name }


  let pyre_api { pyre_api; _ } = pyre_api

  let is_global { resolution; _ } = Resolution.is_global resolution

  let resolve_reference { resolution; _ } = Resolution.resolve_reference resolution

  let resolve_assignment ({ resolution; _ } as pyre_in_context) assign =
    { pyre_in_context with resolution = Resolution.resolve_assignment resolution assign }


  let resolve_expression_to_type { resolution; _ } =
    Resolution.resolve_expression_to_type resolution


  let resolve_attribute_access { resolution; _ } = Resolution.resolve_attribute_access resolution

  let fallback_attribute { resolution; _ } = Resolution.fallback_attribute ~resolution

  let resolve_generators pyre_in_context generators =
    let resolve_generator pyre_in_context generator =
      resolve_assignment pyre_in_context (Ast.Statement.Statement.generator_assignment generator)
    in
    List.fold generators ~init:pyre_in_context ~f:resolve_generator


  let module_qualifier { module_qualifier; _ } = module_qualifier

  let define_name { define_name; _ } = define_name
end

module ModelQueries = struct
  module FunctionParameter = struct
    type t =
      | PositionalOnly of {
          name: string option;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Named of {
          name: string;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | KeywordOnly of {
          name: string;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Variable of {
          name: string option;
          position: int;
        }
      | Keywords of {
          name: string option;
          annotation: PysaType.t;
          excluded: string list;
        }
    [@@deriving equal, compare, show]

    let root = function
      | PositionalOnly { name; position; _ } ->
          let name =
            match name with
            | Some name -> name
            | None -> Format.sprintf "__arg%d" position
          in
          TaintAccessPath.Root.PositionalParameter { position; name; positional_only = true }
      | Named { name; position; _ } ->
          TaintAccessPath.Root.PositionalParameter { position; name; positional_only = false }
      | KeywordOnly { name; _ } -> TaintAccessPath.Root.NamedParameter { name }
      | Variable { position; _ } -> TaintAccessPath.Root.StarParameter { position }
      | Keywords { excluded; _ } -> TaintAccessPath.Root.StarStarParameter { excluded }


    let annotation = function
      | PositionalOnly { annotation; _ } -> Some annotation
      | Named { annotation; _ } -> Some annotation
      | KeywordOnly { annotation; _ } -> Some annotation
      | Variable _ -> None
      | Keywords { annotation; _ } -> Some annotation


    let name = function
      | PositionalOnly { name; _ } -> name
      | Named { name; _ } -> Some name
      | KeywordOnly { name; _ } -> Some name
      | Variable { name; _ } -> name
      | Keywords { name; _ } -> name


    let has_default = function
      | PositionalOnly { has_default; _ }
      | Named { has_default; _ }
      | KeywordOnly { has_default; _ } ->
          has_default
      | _ -> false
  end

  module FunctionParameters = struct
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, compare, show]
  end

  module FunctionSignature = struct
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PysaType.t;
    }
    [@@deriving equal, compare, show]

    let toplevel =
      {
        parameters = FunctionParameters.List [];
        return_annotation = PysaType.from_pyre1_type PyreType.NoneType;
      }


    let from_overload { PyreType.Callable.parameters; annotation } =
      let fold_parameters (position, excluded, sofar) = function
        | PyreType.Callable.CallableParamType.PositionalOnly { index; annotation; default } ->
            ( position + 1,
              excluded,
              FunctionParameter.PositionalOnly
                {
                  name = None;
                  position = index;
                  annotation = PysaType.from_pyre1_type annotation;
                  has_default = default;
                }
              :: sofar )
        | PyreType.Callable.CallableParamType.Named { name; annotation; default } ->
            let name = Ast.Identifier.sanitized name in
            ( position + 1,
              name :: excluded,
              FunctionParameter.Named
                {
                  name;
                  position;
                  annotation = PysaType.from_pyre1_type annotation;
                  has_default = default;
                }
              :: sofar )
        | PyreType.Callable.CallableParamType.KeywordOnly { name; annotation; default } ->
            let name = Ast.Identifier.sanitized name in
            ( position + 1,
              name :: excluded,
              FunctionParameter.KeywordOnly
                { name; annotation = PysaType.from_pyre1_type annotation; has_default = default }
              :: sofar )
        | PyreType.Callable.CallableParamType.Variable _ ->
            position + 1, excluded, FunctionParameter.Variable { name = None; position } :: sofar
        | PyreType.Callable.CallableParamType.Keywords annotation ->
            ( position + 1,
              [],
              FunctionParameter.Keywords
                { name = None; annotation = PysaType.from_pyre1_type annotation; excluded }
              :: sofar )
      in
      let parameters =
        match parameters with
        | Defined parameters ->
            let parameters =
              parameters
              |> List.fold ~init:(0, [], []) ~f:fold_parameters
              |> (fun (_, _, parameters) -> parameters)
              |> List.rev
            in
            FunctionParameters.List parameters
        | Undefined -> FunctionParameters.Ellipsis
        | FromParamSpec _ -> FunctionParameters.ParamSpec
      in
      { parameters; return_annotation = PysaType.from_pyre1_type annotation }


    let from_callable_type { PyreType.Callable.implementation; overloads; _ } =
      List.map ~f:from_overload (implementation :: overloads)


    let from_pyre1_ast ~pyre_api ~parameters ~return_annotation =
      let fold_parameters
          (position, excluded, sofar)
          {
            TaintAccessPath.NormalizedParameter.root;
            qualified_name;
            original =
              {
                Ast.Node.value = { Ast.Expression.Parameter.annotation; value = default_value; _ };
                _;
              };
          }
        =
        let get_annotation () =
          annotation
          >>| ReadOnly.parse_annotation pyre_api
          |> Option.value ~default:Type.Top
          |> PysaType.from_pyre1_type
        in
        match root with
        | TaintAccessPath.Root.PositionalParameter { position; name; positional_only } ->
            ( position + 1,
              name :: excluded,
              (if positional_only then
                 FunctionParameter.PositionalOnly
                   {
                     name = Some name;
                     position;
                     annotation = get_annotation ();
                     has_default = Option.is_some default_value;
                   }
              else
                FunctionParameter.Named
                  {
                    name;
                    position;
                    annotation = get_annotation ();
                    has_default = Option.is_some default_value;
                  })
              :: sofar )
        | TaintAccessPath.Root.NamedParameter { name } ->
            ( position + 1,
              name :: excluded,
              FunctionParameter.KeywordOnly
                { name; annotation = get_annotation (); has_default = Option.is_some default_value }
              :: sofar )
        | TaintAccessPath.Root.StarParameter { position } ->
            let name = Ast.Identifier.sanitized qualified_name in
            ( position + 1,
              excluded,
              FunctionParameter.Variable { name = Some name; position } :: sofar )
        | TaintAccessPath.Root.StarStarParameter { excluded } ->
            let name = Ast.Identifier.sanitized qualified_name in
            ( position + 1,
              excluded,
              FunctionParameter.Keywords
                { name = Some name; annotation = get_annotation (); excluded }
              :: sofar )
        | _ -> failwith "unsupported root"
      in
      let parameters =
        parameters
        |> TaintAccessPath.normalize_parameters
        |> List.fold ~init:(0, [], []) ~f:fold_parameters
        |> (fun (_, _, parameters) -> parameters)
        |> List.rev
      in
      let return_annotation =
        return_annotation
        >>| ReadOnly.parse_annotation pyre_api
        |> Option.value ~default:Type.Top
        |> PysaType.from_pyre1_type
      in
      { parameters = FunctionParameters.List parameters; return_annotation }
  end

  module Function = struct
    type t = {
      define_name: Ast.Reference.t;
      (* If the user-provided name is a re-export, this is the original name. *)
      imported_name: Ast.Reference.t option;
      (* Signature of the function, ignoring all decorators. None when unknown. *)
      (* Note that functions with `@overload` have multiple signatures. *)
      undecorated_signatures: FunctionSignature.t list option;
      is_property_getter: bool;
      is_property_setter: bool;
      is_method: bool;
    }
    [@@deriving show]
  end

  module Global = struct
    type t =
      | Class of { class_name: string }
      | Module
      (* function or method *)
      | Function of Function.t
      (* non-callable class attribute. *)
      | ClassAttribute of { name: Ast.Reference.t }
      (* non-callable module global variable. *)
      | ModuleGlobal of { name: Ast.Reference.t }
      (* class attribute exists, but type is unknown. *)
      | UnknownClassAttribute of { name: Ast.Reference.t }
      (* module global exists, but type is unknown. *)
      | UnknownModuleGlobal of { name: Ast.Reference.t }
    [@@deriving show]
  end

  let containing_source read_only reference =
    let rec qualifier ~found ~lead ~tail =
      match tail with
      | head :: (_ :: _ as tail) ->
          let new_lead = Ast.Reference.create ~prefix:lead head in
          if ReadOnly.module_exists read_only new_lead then
            qualifier ~found:new_lead ~lead:new_lead ~tail
          else
            qualifier ~found ~lead:new_lead ~tail
      | _ -> found
    in
    qualifier
      ~found:Ast.Reference.empty
      ~lead:Ast.Reference.empty
      ~tail:(Ast.Reference.as_list reference)
    |> ReadOnly.source_of_qualifier read_only


  module ReferenceTableCache (Type : sig
    type t
  end) =
  struct
    let cache : Type.t Ast.Reference.Table.t = Ast.Reference.Table.create ()

    let set key value = Hashtbl.set cache ~key ~data:value

    let get = Hashtbl.find cache

    let invalidate () = Hashtbl.clear cache
  end

  module ClassMethodSignatureCache = ReferenceTableCache (struct
    type t = (Ast.Reference.t * Ast.Statement.Define.Signature.t option) list option
  end)

  let class_method_signatures read_only reference =
    (* The current implementation relies on iterating through the AST, which means it doesn't handle
       synthesized methods. If possible, this shouldn't be used. *)
    match ClassMethodSignatureCache.get reference with
    | Some result -> result
    | None ->
        let open Option in
        let get_method_signature = function
          | {
              Ast.Node.value =
                Ast.Statement.Statement.Define { signature = { name; _ } as signature; _ };
              _;
            } ->
              (* TODO(T199841372) Pysa should not be assuming that a Define name in the raw AST is
                 fully qualified. *)
              Some (name, Some signature)
          | _ -> None
        in
        let result =
          containing_source read_only reference
          >>| Preprocessing.classes
          >>| List.filter ~f:(fun { Ast.Node.value = { Ast.Statement.Class.name; _ }; _ } ->
                  Ast.Reference.equal reference name)
          (* Prefer earlier definitions. *)
          >>| List.rev
          >>= List.hd
          >>| (fun { Ast.Node.value = { Ast.Statement.Class.body = statements; _ }; _ } ->
                statements)
          >>| List.filter_map ~f:get_method_signature
        in
        ClassMethodSignatureCache.set reference result;
        result


  (* Find a method definition matching the given predicate. *)
  let find_method_definitions read_only ?(predicate = fun _ -> true) name =
    let get_matching_define = function
      | define_name, Some signature when Ast.Reference.equal define_name name && predicate signature
        ->
          let parser = ReadOnly.annotation_parser read_only in
          let generic_parameters_as_variables =
            ReadOnly.generic_parameters_as_variables read_only
          in
          AnnotatedDefine.Callable.create_overload_without_applying_decorators
            ~parser
            ~generic_parameters_as_variables
            signature
          |> Option.some
      | _ -> None
    in
    Ast.Reference.prefix name
    >>= class_method_signatures read_only
    >>| List.filter_map ~f:get_matching_define
    |> Option.value ~default:[]


  let property_decorators =
    Set.union Recognized.property_decorators Recognized.classproperty_decorators


  (* This is a very specific Pysa API used for dealing with model verification: it - determines what
     a fully qualified name means, where qualification handles not only module name prepending but
     nesting of classes, functions, and methods/attributes - if the meaning is not a class or
     module, it returns the type. For callable types, it uses the undecorated signature rather than
     the decorated signature.

     This logic used to live inside of `modelVerifier`, but it is extremely invasive to Pyre
     internals so we need to extract it if we want to be able to work toward a well-defined
     interface. *)
  let resolve_qualified_name_to_global read_only ~is_property_getter ~is_property_setter name =
    let get_callable_type = function
      | PyreType.Callable t -> t
      | _ -> failwith "expected callable type"
    in
    let get_imported_name { PyreType.Callable.kind; _ } =
      match kind with
      | Named name -> Some name
      | _ -> None
    in
    let name_end = Ast.Reference.last name in
    if Ast.Identifier.equal name_end Ast.Statement.toplevel_define_name then
      if
        name
        |> Ast.Reference.prefix
        >>| ReadOnly.module_exists read_only
        |> Option.value ~default:false
      then
        Some
          (Global.Function
             {
               Function.define_name = name;
               imported_name = None;
               undecorated_signatures = Some [FunctionSignature.toplevel];
               is_property_getter = false;
               is_property_setter = false;
               is_method = false;
             })
      else
        None
    else if Ast.Identifier.equal name_end Ast.Statement.class_toplevel_define_name then
      if
        name
        |> Ast.Reference.prefix
        >>| Ast.Reference.show
        >>| ReadOnly.class_exists read_only
        |> Option.value ~default:false
      then
        Some
          (Global.Function
             {
               Function.define_name = name;
               imported_name = None;
               undecorated_signatures = Some [FunctionSignature.toplevel];
               is_property_getter = false;
               is_property_setter = false;
               is_method = true;
             })
      else
        None
    else if is_property_getter then
      let predicate define =
        Set.exists property_decorators ~f:(Ast.Statement.Define.Signature.has_decorator define)
      in
      find_method_definitions read_only ~predicate name
      |> List.hd
      >>| fun callable ->
      Global.Function
        {
          Function.define_name = name;
          imported_name = None;
          undecorated_signatures = Some [FunctionSignature.from_overload callable];
          is_property_setter;
          is_property_getter;
          is_method = true;
        }
    else if is_property_setter then
      find_method_definitions
        read_only
        ~predicate:Ast.Statement.Define.Signature.is_property_setter
        name
      |> List.hd
      >>| fun callable ->
      Global.Function
        {
          Function.define_name = name;
          imported_name = None;
          undecorated_signatures = Some [FunctionSignature.from_overload callable];
          is_property_getter;
          is_property_setter;
          is_method = true;
        }
    else (* Resolve undecorated functions. *)
      let class_summary =
        Ast.Reference.prefix name
        >>| ReadOnly.parse_reference read_only
        >>| PyreType.split
        >>| fst
        >>= PyreType.primitive_name
        >>= ReadOnly.get_class_summary read_only
      in
      let maybe_signature_of_function =
        match ReadOnly.global read_only name with
        | Some { AttributeResolution.Global.undecorated_signature = Some signature; _ } ->
            Some signature
        | _ ->
            ReadOnly.get_define_body read_only name
            >>| Ast.Node.value
            |> (function
                 | Some
                     ({
                        Ast.Statement.Define.signature =
                          { parent = Ast.NestingContext.Function _; _ };
                        _;
                      } as define) ->
                     Some
                       (ReadOnly.resolve_define_undecorated
                          ~callable_name:(Some name)
                          ~implementation:(Some define.signature)
                          ~overloads:[]
                          ~scoped_type_variables:None
                          read_only)
                 | _ -> None)
            |> Option.map ~f:(fun { AnnotatedAttribute.undecorated_signature = signature; _ } ->
                   signature)
      in
      match maybe_signature_of_function with
      | Some signature ->
          Some
            (Global.Function
               {
                 Function.define_name = name;
                 imported_name = get_imported_name signature;
                 undecorated_signatures = Some (FunctionSignature.from_callable_type signature);
                 is_property_getter = false;
                 is_property_setter = false;
                 is_method = Option.is_some class_summary;
               })
      | None -> (
          (* Resolve undecorated methods. *)
          match find_method_definitions read_only name with
          | [callable] ->
              Some
                (Global.Function
                   {
                     define_name = name;
                     imported_name = None;
                     undecorated_signatures = Some [FunctionSignature.from_overload callable];
                     is_property_getter = false;
                     is_property_setter = false;
                     is_method = Option.is_some class_summary;
                   })
          | first :: _ :: _ as overloads ->
              (* Note that we use the first overload as the base implementation, which might be
                 unsound. *)
              Some
                (Global.Function
                   {
                     define_name = name;
                     imported_name = None;
                     undecorated_signatures =
                       Some
                         (PyreType.Callable.create
                            ~overloads
                            ~parameters:first.parameters
                            ~annotation:first.annotation
                            ()
                         |> get_callable_type
                         |> FunctionSignature.from_callable_type);
                     is_property_getter = false;
                     is_property_setter = false;
                     is_method = Option.is_some class_summary;
                   })
          | [] -> (
              (* Fall back for anything else. *)
              let annotation =
                Ast.Expression.from_reference
                  name
                  ~location:Ast.Location.any
                  ~create_origin:(fun _ -> None)
                |> ReadOnly.resolve_expression_to_type_info read_only
              in
              match TypeInfo.Unit.annotation annotation with
              | PyreType.Parametric { name = "type"; _ }
                when ReadOnly.class_exists read_only (Ast.Reference.show name) ->
                  Some (Global.Class { class_name = Ast.Reference.show name })
              | PyreType.Top when ReadOnly.module_exists read_only name -> Some Global.Module
              | PyreType.Top when not (TypeInfo.Unit.is_immutable annotation) ->
                  (* FIXME: We are relying on the fact that nonexistent functions & attributes
                     resolve to mutable annotation, while existing ones resolve to immutable
                     annotation. This is fragile! *)
                  None
              | PyreType.Parametric
                  {
                    name = "BoundMethod";
                    arguments = [PyreType.Argument.Single (PyreType.Callable t); _];
                  } ->
                  Some
                    (Global.Function
                       {
                         define_name = name;
                         imported_name = get_imported_name t;
                         undecorated_signatures = Some (FunctionSignature.from_callable_type t);
                         is_property_getter = false;
                         is_property_setter = false;
                         is_method = Option.is_some class_summary;
                       })
              | PyreType.Callable t ->
                  Some
                    (Global.Function
                       {
                         define_name = name;
                         imported_name = get_imported_name t;
                         undecorated_signatures = Some (FunctionSignature.from_callable_type t);
                         is_property_getter = false;
                         is_property_setter = false;
                         is_method = Option.is_some class_summary;
                       })
              | PyreType.Top
              | PyreType.Any ->
                  if Option.is_some class_summary then
                    Some (Global.UnknownClassAttribute { name })
                  else
                    Some (Global.UnknownModuleGlobal { name })
              | _ ->
                  if Option.is_some class_summary then
                    Some (Global.ClassAttribute { name })
                  else
                    Some (Global.ModuleGlobal { name })))


  let invalidate_cache = ClassMethodSignatureCache.invalidate
end
