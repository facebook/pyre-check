(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServerDependencies = Dependencies
module ModuleTracker = Analysis.ModuleTracker
open Ast
open Analysis
open State
open Configuration.Server
open Protocol
open Request
open Pyre

exception IncorrectParameters of Type.t

exception MissingFunction of Reference.t

let errors_of_path ~configuration ~state:{ State.module_tracker; errors; _ } path =
  ModuleTracker.lookup_path ~configuration module_tracker path
  >>= (fun { SourcePath.qualifier; _ } -> Hashtbl.find errors qualifier)
  |> Option.value ~default:[]


let instantiate_error ~configuration ~state:{ State.ast_environment; _ } error =
  let ast_environment = AstEnvironment.read_only ast_environment in
  Error.instantiate
    ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
    error


type response = {
  state: State.t;
  response: Protocol.response option;
}

module AnnotationEdit = struct
  type t = {
    new_text: string;
    range: LanguageServer.Types.Range.t;
    title: string;
  }

  let range { range; _ } = range

  let new_text { new_text; _ } = new_text

  let title { title; _ } = title

  let is_replacement_edit kind =
    match kind with
    | Error.IncompatibleVariableType _
    | Error.IncompatibleReturnType _ ->
        true
    | _ -> false


  let create_range ~error:{ Error.kind = error_kind; location; _ } ~file =
    let token =
      match error_kind with
      | Error.MissingReturnAnnotation _ -> Some "):"
      | Error.MissingAttributeAnnotation { missing_annotation = { name; _ }; _ }
      | Error.MissingParameterAnnotation { name; _ }
      | Error.MissingGlobalAnnotation { name; _ } ->
          Some (Format.asprintf "%a" Reference.pp_sanitized name)
      | Error.IncompatibleReturnType { mismatch = { expected; _ }; _ } ->
          Some (Format.asprintf " -> %s" (Type.show expected))
      | Error.IncompatibleVariableType { name; mismatch = { expected; _ }; _ } ->
          Some (Format.asprintf "%a: %s" Reference.pp_sanitized name (Type.show expected))
      | _ -> None
    in
    let start_line =
      let line =
        match error_kind with
        | Error.IncompatibleReturnType { define_location; _ } -> Location.line define_location
        | Error.IncompatibleVariableType { declare_location; _ } -> Location.line declare_location
        | _ -> Location.line location
      in
      line - 1
    in
    let get_range lines token =
      List.findi lines ~f:(fun _ line -> Option.is_some (String.substr_index line ~pattern:token))
      >>| (fun (index, line) ->
            let position =
              {
                LanguageServer.Types.Position.line = index + start_line;
                character = String.substr_index_exn line ~pattern:token + 1;
              }
            in
            let end_ =
              match error_kind with
              | Error.IncompatibleVariableType _
              | Error.IncompatibleReturnType _
              | Error.MissingGlobalAnnotation _
              | Error.MissingAttributeAnnotation _ ->
                  let { LanguageServer.Types.Position.character; _ } = position in
                  { position with character = character + String.length token }
              | _ -> position
            in
            Some { LanguageServer.Types.Range.start = position; end_ })
      |> Option.value ~default:None
    in
    let lines = File.lines file in
    match token, lines with
    | Some token, Some lines ->
        let _, lines = List.split_n lines start_line in
        get_range lines token
    | _, _ -> None


  let create ~file ~error =
    error
    >>| (fun ({ Error.kind; _ } as error) ->
          let format_type annotation =
            Type.weaken_literals annotation |> Type.infer_transform |> Type.show
          in
          let new_text =
            match kind with
            | Error.MissingReturnAnnotation { annotation = Some annotation; _ } ->
                Some (" -> " ^ format_type annotation)
            | Error.MissingAttributeAnnotation
                { missing_annotation = { annotation = Some annotation; _ }; _ }
            | Error.MissingParameterAnnotation { annotation = Some annotation; _ }
            | Error.MissingGlobalAnnotation { annotation = Some annotation; _ } ->
                Some (": " ^ format_type annotation)
            | Error.IncompatibleReturnType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf "-> %s:" @@ format_type annotation)
            | Error.IncompatibleVariableType { mismatch = { actual = annotation; _ }; _ } ->
                Some (Format.asprintf ": %s " @@ format_type annotation)
            | _ -> None
          in
          let range = create_range ~error ~file in
          let title =
            if is_replacement_edit kind then
              "Fix annotation"
            else
              "Add annotation"
          in
          match range, new_text with
          | Some range, Some new_text -> Some { new_text; range; title }
          | _, _ -> None)
    |> Option.value ~default:None
end

let process_client_shutdown_request ~state ~id =
  let open LanguageServer.Protocol in
  let response =
    ShutdownResponse.default id |> ShutdownResponse.to_yojson |> Yojson.Safe.to_string
  in
  { state; response = Some (LanguageServerProtocolResponse response) }


let process_type_query_request
    ~state:({ State.module_tracker; environment; _ } as state)
    ~configuration
    ~request
  =
  let process_request () =
    let global_resolution = TypeEnvironment.global_resolution environment in
    let order = GlobalResolution.class_hierarchy global_resolution in
    let resolution = TypeCheck.resolution global_resolution () in
    let parse_and_validate
        ?(unknown_is_top = false)
        ?(fill_missing_type_parameters_with_any = false)
        expression
      =
      let annotation =
        (* Return untracked so we can specifically message the user about them. *)
        GlobalResolution.parse_annotation
          ~allow_untracked:true
          ~allow_invalid_type_parameters:true
          global_resolution
          expression
      in
      let annotation =
        if unknown_is_top then
          let constraints = function
            | Type.Primitive "unknown" -> Some Type.Top
            | _ -> None
          in
          Type.instantiate annotation ~constraints
        else
          annotation
      in
      let annotation =
        if fill_missing_type_parameters_with_any && Type.is_primitive annotation then
          let generics =
            GlobalResolution.class_definition global_resolution annotation
            >>| Annotated.Class.create
            >>| GlobalResolution.generics ~resolution:global_resolution
          in
          match generics, annotation with
          | Some (Type.OrderedTypes.Concrete generics), Type.Primitive primitive
            when not (List.is_empty generics) ->
              Type.Parametric
                {
                  name = primitive;
                  parameters = Type.OrderedTypes.Concrete (List.map generics ~f:(fun _ -> Type.Any));
                }
          | _ -> annotation
        else
          annotation
      in
      if ClassHierarchy.is_instantiated order annotation then
        let mismatches, _ =
          GlobalResolution.check_invalid_type_parameters global_resolution annotation
        in
        if List.is_empty mismatches then
          annotation
        else
          raise (IncorrectParameters annotation)
      else
        raise (ClassHierarchy.Untracked annotation)
    in
    let global_environment = TypeEnvironment.global_environment environment in
    let class_metadata_environment =
      GlobalResolution.class_metadata_environment global_resolution
    in
    let class_hierarchy_environment =
      GlobalResolution.class_hierarchy_environment global_resolution
    in
    let alias_environment = GlobalResolution.alias_environment global_resolution in
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    match request with
    | TypeQuery.RunCheck { check_name; paths } ->
        let source_paths =
          List.filter_map
            paths
            ~f:(Analysis.ModuleTracker.lookup_path ~configuration module_tracker)
        in
        let errors =
          IncrementalStaticAnalysis.run_additional_check
            ~configuration
            ~environment
            ~source_paths
            ~check:check_name
        in
        TypeQuery.Response (TypeQuery.Errors errors)
    | TypeQuery.Attributes annotation ->
        let to_attribute { Node.value = { Annotated.Class.Attribute.name; annotation; _ }; _ } =
          { TypeQuery.name; annotation }
        in
        parse_and_validate (Expression.from_reference ~location:Location.Reference.any annotation)
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attributes ~resolution:global_resolution
        >>| List.map ~f:to_attribute
        >>| (fun attributes -> TypeQuery.Response (TypeQuery.FoundAttributes attributes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Reference.show annotation)))
    | TypeQuery.Callees caller ->
        TypeQuery.Response
          (TypeQuery.Callees
             (Callgraph.get ~caller |> List.map ~f:(fun { Callgraph.callee; _ } -> callee)))
    | TypeQuery.CalleesWithLocation caller ->
        let instantiate =
          Location.instantiate
            ~lookup:
              (AstEnvironment.ReadOnly.get_real_path_relative
                 ~configuration
                 (AstEnvironment.read_only state.ast_environment))
        in
        let callees =
          Callgraph.get ~caller
          |> List.map ~f:(fun { Callgraph.callee; locations } ->
                 { TypeQuery.callee; locations = List.map locations ~f:instantiate })
        in
        TypeQuery.Response (TypeQuery.CalleesWithLocation callees)
    | TypeQuery.ComputeHashesToKeys ->
        (* Type order. *)
        let extend_map map ~new_map =
          Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        (* Environments *)
        let map =
          List.fold
            [
              ClassMetadataEnvironment.ReadOnly.hash_to_key_map class_metadata_environment;
              ClassHierarchyEnvironment.ReadOnly.hash_to_key_map class_hierarchy_environment;
              AliasEnvironment.ReadOnly.hash_to_key_map alias_environment;
              UnannotatedGlobalEnvironment.ReadOnly.hash_to_key_map unannotated_global_environment;
            ]
            ~f:(fun sofar new_map -> extend_map sofar ~new_map)
            ~init:(AnnotatedGlobalEnvironment.ReadOnly.hash_to_key_map global_environment)
        in
        (* AST shared memory. *)
        let map =
          map |> extend_map ~new_map:(AstEnvironment.shared_memory_hash_to_key_map qualifiers)
        in
        (* TODO (T56904923): Track the CallGraph table consistency *)
        map
        (* TODO (T57043920): Track TypeEnvironment consistency *)
        |> Map.to_alist
        |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
        |> List.map ~f:(fun (hash, key) -> { TypeQuery.hash; key })
        |> fun response -> TypeQuery.Response (TypeQuery.FoundKeyMapping response)
    | TypeQuery.CoverageInFile path ->
        let default =
          TypeQuery.Error (Format.asprintf "Not able to get lookups in `%a`" Path.pp path)
        in
        let map_to_coverage (location, annotation) =
          let coverage =
            if Type.is_partially_typed annotation then
              TypeQuery.Partial
            else if Type.is_untyped annotation then
              TypeQuery.Untyped
            else
              TypeQuery.Typed
          in
          { location; TypeQuery.coverage }
        in
        LookupCache.find_all_annotations ~state ~configuration ~path
        >>| List.map ~f:map_to_coverage
        >>| (fun list -> TypeQuery.Response (TypeQuery.CoverageAtLocations list))
        |> Option.value ~default
    | TypeQuery.DecodeOcamlValues values ->
        let decode key value =
          let key, value = Base64.decode key, Base64.decode value in
          match key, value with
          | Ok key, Ok value -> (
              match Memory.decode ~key ~value with
              | Ok decoded -> Some decoded
              | _ -> None )
          | _ -> None
        in
        let serialize_decoded decoded =
          match decoded with
          | Analysis.Callgraph.SharedMemory.Decoded (key, value) ->
              let show { Callgraph.callee; locations } =
                Format.asprintf
                  "%s: [%s]"
                  (Callgraph.show_callee callee)
                  (List.map locations ~f:Location.Reference.show |> String.concat ~sep:", ")
              in
              Some
                ( Callgraph.CalleeValue.description,
                  Reference.show key,
                  value >>| List.map ~f:show >>| String.concat ~sep:"," )
          | _ ->
              List.find_map
                [
                  AnnotatedGlobalEnvironment.ReadOnly.serialize_decoded global_environment;
                  ClassMetadataEnvironment.ReadOnly.serialize_decoded class_metadata_environment;
                  ClassHierarchyEnvironment.ReadOnly.serialize_decoded class_hierarchy_environment;
                  AliasEnvironment.ReadOnly.serialize_decoded alias_environment;
                  UnannotatedGlobalEnvironment.ReadOnly.serialize_decoded
                    unannotated_global_environment;
                  AstEnvironment.serialize_decoded;
                ]
                ~f:(fun serialize -> serialize decoded)
        in
        let build_response { TypeQuery.decoded; undecodable_keys } = function
          | TypeQuery.SerializedValue { serialized_key; serialized_value } -> (
              let serialized = decode serialized_key serialized_value >>= serialize_decoded in
              match serialized with
              | Some (kind, key, value) ->
                  let decoded_value =
                    TypeQuery.DecodedValue
                      { serialized_key; kind; actual_key = key; actual_value = value }
                  in
                  { TypeQuery.decoded = decoded_value :: decoded; undecodable_keys }
              | None -> { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys }
              )
          | TypeQuery.SerializedPair
              { serialized_key; first_serialized_value; second_serialized_value } -> (
              let first_decoded = decode serialized_key first_serialized_value in
              let second_decoded = decode serialized_key second_serialized_value in
              match first_decoded, second_decoded with
              | Some first, Some second -> (
                  let equal =
                    List.find_map
                      [
                        AnnotatedGlobalEnvironment.ReadOnly.decoded_equal global_environment;
                        ClassMetadataEnvironment.ReadOnly.decoded_equal class_metadata_environment;
                        ClassHierarchyEnvironment.ReadOnly.decoded_equal class_hierarchy_environment;
                        AliasEnvironment.ReadOnly.decoded_equal alias_environment;
                        UnannotatedGlobalEnvironment.ReadOnly.decoded_equal
                          unannotated_global_environment;
                        AstEnvironment.decoded_equal;
                      ]
                      ~f:(fun decoded_equal -> decoded_equal first second)
                    |> Option.value ~default:false
                  in
                  match serialize_decoded first, serialize_decoded second with
                  | Some (kind, key, first_value), Some (_, _, second_value) ->
                      let value =
                        TypeQuery.DecodedPair
                          {
                            serialized_key;
                            kind;
                            actual_key = key;
                            first_value;
                            second_value;
                            equal;
                          }
                      in
                      { TypeQuery.decoded = value :: decoded; undecodable_keys }
                  | _ ->
                      { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys } )
              | _ -> { TypeQuery.decoded; undecodable_keys = serialized_key :: undecodable_keys } )
        in
        let decoded =
          List.fold values ~init:{ TypeQuery.decoded = []; undecodable_keys = [] } ~f:build_response
        in
        TypeQuery.Response (TypeQuery.Decoded decoded)
    | TypeQuery.Defines module_names ->
        let ast_environment = TypeEnvironment.ast_environment environment in
        let defines_of_module module_name =
          match AstEnvironment.ReadOnly.get_source ast_environment module_name with
          | Some definition ->
              let defines =
                Preprocessing.defines
                  ~include_stubs:true
                  ~include_nested:true
                  ~include_methods:true
                  definition
              in
              let represent
                  {
                    Node.value =
                      { Statement.Define.signature = { name; return_annotation; parameters; _ }; _ };
                    _;
                  }
                =
                let represent_parameter
                    { Node.value = { Expression.Parameter.name; annotation; _ }; _ }
                  =
                  {
                    TypeQuery.parameter_name = Identifier.sanitized name;
                    parameter_annotation = annotation;
                  }
                in
                {
                  TypeQuery.define_name = name;
                  parameters = List.map parameters ~f:represent_parameter;
                  return_annotation;
                }
              in
              List.map defines ~f:represent
          | None -> []
        in
        List.concat_map module_names ~f:defines_of_module
        |> fun defines -> TypeQuery.Response (TypeQuery.FoundDefines defines)
    | TypeQuery.DumpCallGraph ->
        let get_callgraph module_qualifier =
          let callees
              {
                Node.value =
                  {
                    Statement.Define.signature = { Statement.Define.Signature.name = caller; _ };
                    _;
                  };
                _;
              }
            =
            let instantiate =
              Location.instantiate
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative
                     ~configuration
                     (AstEnvironment.read_only state.ast_environment))
            in
            Callgraph.get ~caller
            |> List.map ~f:(fun { Callgraph.callee; locations } ->
                   { TypeQuery.callee; locations = List.map locations ~f:instantiate })
            |> fun callees -> { Protocol.TypeQuery.caller; callees }
          in
          let ast_environment = Analysis.AstEnvironment.read_only state.ast_environment in
          AstEnvironment.ReadOnly.get_source ast_environment module_qualifier
          >>| Preprocessing.defines ~include_toplevels:false ~include_stubs:false
          >>| List.map ~f:callees
          |> Option.value ~default:[]
        in
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        TypeQuery.Response (TypeQuery.Callgraph (List.concat_map qualifiers ~f:get_callgraph))
    | TypeQuery.DumpClassHierarchy ->
        let global_environment = TypeEnvironment.global_environment environment in
        let resolution = GlobalResolution.create global_environment in
        let class_hierarchy_json =
          let indices =
            Analysis.UnannotatedGlobalEnvironment.ReadOnly.all_indices
              unannotated_global_environment
          in
          ClassHierarchy.to_json (GlobalResolution.class_hierarchy resolution) ~indices
        in
        TypeQuery.Response (TypeQuery.ClassHierarchy class_hierarchy_json)
    | TypeQuery.DumpDependencies path ->
        let () =
          match ModuleTracker.lookup_path ~configuration module_tracker path with
          | None -> ()
          | Some { SourcePath.qualifier; _ } ->
              let ast_environment =
                TypeEnvironment.global_environment environment
                |> AnnotatedGlobalEnvironment.ReadOnly.ast_environment
              in
              let legacy_dependency_tracker = Dependencies.create ast_environment in
              Path.create_relative
                ~root:(Configuration.Analysis.log_directory configuration)
                ~relative:"dependencies.dot"
              |> File.create ~content:(Dependencies.to_dot legacy_dependency_tracker ~qualifier)
              |> File.write
        in
        TypeQuery.Response (TypeQuery.Success "Dependencies dumped.")
    | TypeQuery.DumpMemoryToSqlite path ->
        let path = Path.absolute path in
        let () =
          try Unix.unlink path with
          | Unix.Unix_error _ -> ()
        in
        let timer = Timer.start () in
        (* Normalize the environment for comparison. *)
        let qualifiers = ModuleTracker.tracked_explicit_modules module_tracker in
        let ast_environment = TypeEnvironment.ast_environment environment in
        let legacy_dependency_tracker = Dependencies.create ast_environment in
        Dependencies.normalize legacy_dependency_tracker qualifiers;
        Memory.SharedMemory.save_table_sqlite path |> ignore;
        let { Memory.SharedMemory.used_slots; _ } = Memory.SharedMemory.hash_stats () in
        Log.info
          "Dumped %d slots in %.2f seconds to %s"
          used_slots
          (Timer.stop timer |> Time.Span.to_sec)
          path;
        TypeQuery.Response (TypeQuery.Path (Path.create_absolute path))
    | TypeQuery.IsCompatibleWith (left, right) ->
        (* We need a special version of parse_and_validate to handle the "unknown" type that
           Monkeycheck may send us *)
        let left = parse_and_validate ~unknown_is_top:true left in
        let right = parse_and_validate ~unknown_is_top:true right in
        let right =
          match Type.coroutine_value right with
          | None -> right
          | Some unwrapped -> unwrapped
        in
        GlobalResolution.is_compatible_with global_resolution ~left ~right
        |> fun result ->
        TypeQuery.Response (TypeQuery.Compatibility { actual = left; expected = right; result })
    | TypeQuery.Join (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.join global_resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.LessOrEqual (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.less_or_equal global_resolution ~left ~right
        |> fun response -> TypeQuery.Response (TypeQuery.Boolean response)
    | TypeQuery.Meet (left, right) ->
        let left = parse_and_validate left in
        let right = parse_and_validate right in
        GlobalResolution.meet global_resolution left right
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.Methods annotation ->
        let to_method = function
          | {
              Node.value =
                {
                  Annotated.Attribute.annotation =
                    Callable
                      {
                        implementation = { annotation; parameters = Defined parameters; _ };
                        kind = Named name;
                        _;
                      };
                  _;
                };
              _;
            } ->
              let parameters =
                parameters
                |> List.filter_map ~f:Type.Callable.Parameter.annotation
                |> fun parameters -> Type.Primitive "self" :: parameters
              in
              let return_annotation = annotation in
              Some { TypeQuery.name = Reference.last name; parameters; return_annotation }
          | _ -> None
        in
        let parsed_annotation =
          parse_and_validate ~fill_missing_type_parameters_with_any:true annotation
        in
        parsed_annotation
        |> Type.split
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attributes
              ~instantiated:parsed_annotation
              ~resolution:global_resolution
        >>| List.filter_map ~f:to_method
        >>| (fun methods -> TypeQuery.Response (TypeQuery.FoundMethods methods))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Expression.show annotation)))
    | TypeQuery.NormalizeType expression ->
        parse_and_validate expression
        |> fun annotation -> TypeQuery.Response (TypeQuery.Type annotation)
    | TypeQuery.PathOfModule module_name ->
        ModuleTracker.lookup_source_path module_tracker module_name
        >>= (fun source_path ->
              let path = SourcePath.full_path ~configuration source_path |> Path.absolute in
              Some (TypeQuery.Response (TypeQuery.FoundPath path)))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No path found for module `%s`" (Reference.show module_name)))
    | TypeQuery.SaveServerState path ->
        let path = Path.absolute path in
        Log.info "Saving server state into `%s`" path;
        Memory.save_shared_memory ~path;
        TypeQuery.Response (TypeQuery.Success (Format.sprintf "Saved state."))
    | TypeQuery.Signature function_names -> (
        let get_signatures function_name =
          let keep_known_annotation annotation =
            match annotation with
            | Type.Top -> None
            | _ -> Some annotation
          in
          match
            UnannotatedGlobalEnvironment.ReadOnly.get_define_body
              unannotated_global_environment
              function_name
          with
          | Some { Node.location; value = { Statement.Define.signature; _ } } -> (
              let parser = GlobalResolution.annotation_parser global_resolution in
              let { Type.Callable.annotation; parameters; _ } =
                Node.create signature ~location
                |> Analysis.Annotated.Callable.create_overload ~parser
              in
              match parameters with
              | Type.Callable.Defined parameters ->
                  let format parameter =
                    match parameter with
                    | Type.Callable.Parameter.Named { name; annotation; _ } ->
                        let name = Identifier.sanitized name in
                        Some
                          {
                            TypeQuery.parameter_name = name;
                            annotation = keep_known_annotation annotation;
                          }
                    | _ -> None
                  in
                  let parameters = List.filter_map ~f:format parameters in
                  {
                    TypeQuery.return_type = keep_known_annotation annotation;
                    parameters;
                    function_name = Reference.show function_name;
                  }
              | _ -> raise (MissingFunction function_name) )
          | None -> raise (MissingFunction function_name)
        in
        try
          TypeQuery.Response (TypeQuery.FoundSignature (List.map function_names ~f:get_signatures))
        with
        | MissingFunction function_name ->
            TypeQuery.Error
              (Format.sprintf "No signature found for %s" (Reference.show function_name)) )
    | TypeQuery.Superclasses annotation ->
        parse_and_validate annotation
        |> GlobalResolution.class_definition global_resolution
        >>| Annotated.Class.create
        >>| GlobalResolution.superclasses ~resolution:global_resolution
        >>| List.map ~f:Annotated.Class.annotation
        >>| (fun classes -> TypeQuery.Response (TypeQuery.Superclasses classes))
        |> Option.value
             ~default:
               (TypeQuery.Error
                  (Format.sprintf "No class definition found for %s" (Expression.show annotation)))
    | TypeQuery.Type expression -> (
        let define =
          Statement.Define.create_toplevel ~qualifier:None ~statements:[]
          |> Node.create_with_default_location
        in
        let module State = TypeCheck.State (struct
          let debug = false

          let define = define

          module Builder = Callgraph.NullBuilder
        end)
        in
        let state = State.create ~resolution () in
        let { State.state; resolved = annotation; _ } =
          State.forward_expression ~state ~expression
        in
        match State.errors state with
        | [] -> TypeQuery.Response (TypeQuery.Type annotation)
        | errors ->
            let descriptions =
              let lookup reference =
                let ast_environment =
                  TypeEnvironment.global_environment environment
                  |> AnnotatedGlobalEnvironment.ReadOnly.ast_environment
                in
                AstEnvironment.ReadOnly.get_real_path_relative
                  ~configuration
                  ast_environment
                  reference
              in
              errors
              |> List.map ~f:(Analysis.Error.instantiate ~lookup)
              |> List.map ~f:(Analysis.Error.Instantiated.description ~show_error_traces:false)
              |> String.concat ~sep:", "
            in
            TypeQuery.Error (Format.sprintf "Expression had errors: %s" descriptions) )
    | TypeQuery.TypeAtPosition { path; position } ->
        let default =
          TypeQuery.Error
            (Format.asprintf
               "Not able to get lookup at %a:%a"
               Path.pp
               path
               Location.pp_position
               position)
        in
        LookupCache.find_annotation ~state ~configuration ~path ~position
        >>| (fun (location, annotation) ->
              TypeQuery.Response (TypeQuery.TypeAtLocation { TypeQuery.location; annotation }))
        |> Option.value ~default
    | TypeQuery.TypesInFiles paths ->
        let annotations = LookupCache.find_all_annotations_batch ~state ~configuration ~paths in
        let create_result = function
          | { LookupCache.path; types_by_location = Some types } ->
              `Fst { TypeQuery.path; types = List.map ~f:TypeQuery.create_type_at_location types }
          | { LookupCache.path; _ } -> `Snd path
        in
        let results, errors = List.partition_map ~f:create_result annotations in
        if List.is_empty errors then
          TypeQuery.Response (TypeQuery.TypesByFile results)
        else
          let paths =
            List.fold
              ~init:""
              ~f:(fun sofar path -> Format.asprintf "%s\n\t`%a`" sofar PyrePath.pp path)
              errors
          in
          TypeQuery.Error (Format.asprintf "Not able to get lookups in: %s" paths)
    | TypeQuery.ValidateTaintModels path -> (
        try
          let directories =
            match path with
            | Some path -> [path]
            | None -> configuration.Configuration.Analysis.taint_models_directories
          in
          let configuration = Taint.TaintConfiguration.create ~rule_filter:None ~directories in
          let create_models sources =
            let create_model (path, source) =
              Taint.Model.parse
                ~resolution:(TypeCheck.resolution global_resolution ())
                ~path
                ~verify:true
                ~source
                ~configuration
                Interprocedural.Callable.Map.empty
              |> ignore
            in
            List.iter sources ~f:create_model
          in
          Taint.Model.get_model_sources ~directories |> create_models;
          TypeQuery.Response
            (TypeQuery.Success
               (Format.asprintf
                  "Models in `%s` are valid."
                  (directories |> List.map ~f:Path.show |> String.concat ~sep:", ")))
        with
        | error -> TypeQuery.Error (Exn.to_string error) )
  in
  let response =
    try process_request () with
    | ClassHierarchy.Untracked untracked ->
        let untracked_response =
          Format.asprintf "Type `%a` was not found in the type order." Type.pp untracked
        in
        TypeQuery.Error untracked_response
    | IncorrectParameters untracked ->
        let untracked_response =
          Format.asprintf "Type `%a` has the wrong number of parameters." Type.pp untracked
        in
        TypeQuery.Error untracked_response
  in
  { state; response = Some (TypeQueryResponse response) }


let process_type_check_request ~state ~configuration paths =
  let state, response = IncrementalCheck.recheck_with_state ~state ~configuration paths in
  let response = List.map response ~f:(instantiate_error ~configuration ~state) in
  { state; response = Some (TypeCheckResponse response) }


let process_display_type_errors_request ~state ~configuration paths =
  let errors =
    let { errors; _ } = state in
    match paths with
    | [] -> Hashtbl.data errors |> List.concat |> List.sort ~compare:Error.compare
    | _ -> List.concat_map ~f:(errors_of_path ~configuration ~state) paths
  in
  let errors = List.map errors ~f:(instantiate_error ~configuration ~state) in
  { state; response = Some (TypeCheckResponse errors) }


let process_get_definition_request
    ~state:({ State.environment; _ } as state)
    ~configuration
    ~request:{ DefinitionRequest.id; path; position }
  =
  let response =
    let ast_environment = TypeEnvironment.ast_environment environment in
    let open LanguageServer.Protocol in
    let response =
      match LookupCache.find_definition ~state ~configuration path position with
      | None -> TextDocumentDefinitionResponse.create_empty ~id
      | Some { Location.start; stop; path } -> (
          match AstEnvironment.ReadOnly.get_source_path ast_environment path with
          | None -> TextDocumentDefinitionResponse.create_empty ~id
          | Some source_path ->
              let path = SourcePath.full_path ~configuration source_path in
              TextDocumentDefinitionResponse.create ~id ~start ~stop ~path )
    in
    TextDocumentDefinitionResponse.to_yojson response
    |> Yojson.Safe.to_string
    |> (fun response -> LanguageServerProtocolResponse response)
    |> Option.some
  in
  { state; response }


let rec process
    ~state:({ State.module_tracker; connections; _ } as state)
    ~configuration:({ configuration; _ } as server_configuration)
    ~request
  =
  let { Configuration.Features.go_to_definition; click_to_fix; hover } =
    Configuration.Analysis.features configuration
  in
  let timer = Timer.start () in
  let log_request_error ~error =
    Statistics.event
      ~section:`Error
      ~name:"request error"
      ~normals:["request", Request.show request; "error", error]
      ~flush:true
      ()
  in
  let result =
    try
      match request with
      | TypeCheckRequest paths ->
          SharedMem.collect `aggressive;
          process_type_check_request ~state ~configuration paths
      | StopRequest ->
          Mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop ~reason:"explicit request" ~configuration:server_configuration)
      | TypeQueryRequest request -> process_type_query_request ~state ~configuration ~request
      | DisplayTypeErrors paths ->
          let configuration = { configuration with include_hints = true } in
          process_display_type_errors_request ~state ~configuration paths
      | LanguageServerProtocolRequest request ->
          RequestParser.parse_and_translate
            ~configuration
            ~state
            ~request:(Yojson.Safe.from_string request)
          >>| (fun request -> process ~state ~configuration:server_configuration ~request)
          |> Option.value ~default:{ state; response = None }
      | ClientShutdownRequest id -> process_client_shutdown_request ~state ~id
      | ClientExitRequest client ->
          Log.log ~section:`Server "Stopping %s client" (show_client client);
          { state; response = Some (ClientExitResponse client) }
      | RageRequest id ->
          let response =
            let items = Service.Rage.get_logs configuration in
            LanguageServer.Protocol.RageResponse.create ~items ~id
            |> LanguageServer.Protocol.RageResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | GetDefinitionRequest { DefinitionRequest.id; _ } when not go_to_definition ->
          let response =
            LanguageServer.Protocol.TextDocumentDefinitionResponse.create_empty ~id
            |> LanguageServer.Protocol.TextDocumentDefinitionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | GetDefinitionRequest request ->
          process_get_definition_request ~state ~configuration ~request
      | CompletionRequest { CompletionRequest.id; path; position = cursor_position; _ } ->
          let completion_items =
            AutoComplete.get_completion_items ~state ~configuration ~path ~cursor_position
          in
          let response =
            LanguageServer.Protocol.CompletionResponse.create ~id ~items:completion_items
            |> LanguageServer.Protocol.CompletionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> fun response -> Some (LanguageServerProtocolResponse response)
          in
          { state; response }
      | HoverRequest { DefinitionRequest.id; _ } when not hover ->
          let response =
            LanguageServer.Protocol.HoverResponse.create_empty ~id
            |> LanguageServer.Protocol.HoverResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | HoverRequest { DefinitionRequest.id; path; position } ->
          let response =
            let open LanguageServer.Protocol in
            let result =
              LookupCache.find_annotation ~state ~configuration ~path ~position
              >>| fun (location, annotation) ->
              { HoverResponse.location; contents = Type.show_for_hover annotation }
            in
            HoverResponse.create ~id ~result
            |> HoverResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | CodeActionRequest { id; _ } when not click_to_fix ->
          let response =
            LanguageServer.Protocol.CodeActionResponse.create_empty ~id
            |> LanguageServer.Protocol.CodeActionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | CodeActionRequest { id; diagnostics; uri; path } ->
          let is_range_equal_location
              { LanguageServer.Types.Range.start = range_start; end_ }
              { Location.start = location_start; stop; _ }
            =
            let compare_position
                { LanguageServer.Types.Position.line = range_line; character }
                { Location.line = location_line; column }
              =
              Int.equal (range_line + 1) location_line && Int.equal character column
            in
            compare_position range_start location_start && compare_position end_ stop
          in
          let response =
            let open LanguageServer.Protocol in
            let { Configuration.Server.server_uuid; _ } = server_configuration in
            let code_actions =
              diagnostics
              |> List.filter_map
                   ~f:(fun (LanguageServer.Types.Diagnostic.{ range; _ } as diagnostic) ->
                     let error =
                       List.find
                         (errors_of_path ~configuration ~state path)
                         ~f:(fun { location; _ } -> is_range_equal_location range location)
                     in
                     AnnotationEdit.create ~file:(File.create path) ~error
                     >>| (fun edit ->
                           Some
                             {
                               LanguageServer.Types.CodeAction.diagnostics = Some [diagnostic];
                               command =
                                 Some
                                   {
                                     title = "Fix it";
                                     command = "add_pyre_annotation_" ^ server_uuid;
                                     arguments =
                                       [
                                         {
                                           range = AnnotationEdit.range edit;
                                           newText = AnnotationEdit.new_text edit;
                                           uri;
                                         };
                                       ];
                                   };
                               title = AnnotationEdit.title edit;
                               kind = Some "refactor.rewrite";
                             })
                     |> Option.value ~default:None)
            in
            CodeActionResponse.create ~id ~code_actions
            |> CodeActionResponse.to_yojson
            |> Yojson.Safe.to_string
            |> (fun response -> LanguageServerProtocolResponse response)
            |> Option.some
          in
          { state; response }
      | ExecuteCommandRequest { arguments; id } ->
          let response =
            List.hd arguments
            >>| (fun { uri; newText; range } ->
                  let edit =
                    {
                      LanguageServer.Types.WorkspaceEdit.changes =
                        Some { uri; textEdit = [{ newText; range }] };
                    }
                  in
                  LanguageServer.Protocol.ApplyWorkspaceEdit.create ~id edit
                  |> LanguageServer.Protocol.ApplyWorkspaceEdit.to_yojson
                  |> Yojson.Safe.to_string
                  |> (fun response -> LanguageServerProtocolResponse response)
                  |> Option.some)
            |> Option.value ~default:None
          in
          { state; response }
      | OpenDocument path ->
          (* Make sure cache is fresh. We might not have received a close notification. *)
          LookupCache.evict_path ~state ~configuration path;

          (* Make sure the IDE flushes its state about this file, by sending back all the errors for
             this file. *)
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker path with
            | Some { SourcePath.qualifier; _ } ->
                Reference.Table.set
                  open_documents
                  ~key:qualifier
                  ~data:(File.create path |> File.content |> Option.value ~default:"")
            | _ -> ()
          in
          (* We do not recheck dependencies because nothing changes when we open a document, and we
             do the type checking here just to make type checking resolution appear in shared
             memory. *)
          process_type_check_request
            ~state
            ~configuration:{ configuration with Configuration.Analysis.incremental_style = Shallow }
            [path]
      | CloseDocument path ->
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker path with
            | Some { SourcePath.qualifier; _ } -> Reference.Table.remove open_documents qualifier
            | _ -> ()
          in
          LookupCache.evict_path ~state ~configuration path;
          { state; response = None }
      | DocumentChange file ->
          (* On change, update open document's content but do not trigger recheck. *)
          let { State.open_documents; _ } = state in
          let _ =
            match ModuleTracker.lookup_path ~configuration module_tracker (File.path file) with
            | Some { SourcePath.qualifier; _ } ->
                Reference.Table.set
                  open_documents
                  ~key:qualifier
                  ~data:(File.content file |> Option.value ~default:"")
            | _ -> ()
          in
          { state; response = None }
      | SaveDocument path ->
          ( if Random.bool () then
              let { Configuration.Analysis.local_root; filter_directories; project_root; _ } =
                configuration
              in
              let { Configuration.Server.server_uuid; _ } = server_configuration in
              Telemetry.send_telemetry () ~f:(fun _ ->
                  Telemetry.create_update_message
                    ~local_root
                    ~project_root
                    ~filter_directories
                    ~server_uuid) );

          (* On save, evict entries from the lookup cache. The updated source will be picked up at
             the next lookup (if any). *)
          LookupCache.evict_path ~state ~configuration path;
          let check_on_save =
            Mutex.critical_section connections.lock ~f:(fun () ->
                let { file_notifiers; _ } = !(connections.connections) in
                List.is_empty file_notifiers)
          in
          if check_on_save then
            let configuration = { configuration with include_hints = true } in
            process_type_check_request ~state ~configuration [path]
          else (
            Log.log ~section:`Server "Explicitly ignoring didSave request";
            { state; response = None } )
      | ShowStatusRequest { message; shortMessage; type_; _ } ->
          let update_function =
            let open LanguageServer.Types in
            match ShowMessageParameters.fromMessageTypeNumber type_ with
            | ShowMessageParameters.InfoMessage -> StatusUpdate.information
            | _ -> StatusUpdate.warning
          in
          update_function ~message ~state ~short_message:shortMessage;

          { state; response = None }
      | TypeCoverageRequest { path; id } ->
          let response =
            LookupCache.find_all_annotations ~state ~configuration ~path
            >>| fun location_types ->
            let types = List.map location_types ~f:snd in
            let { Coverage.full; partial; untyped; _ } = Coverage.aggregate_over_types types in
            let total = Float.of_int (full + partial + untyped) in
            let covered_percent =
              if total > 0.0 then
                Int.of_float (Float.of_int full /. total *. 100.0)
              else
                0
            in
            LanguageServer.Protocol.TypeCoverageResponse.create ~id ~covered_percent
            |> LanguageServer.Protocol.TypeCoverageResponse.to_yojson
            |> Yojson.Safe.to_string
            |> fun response -> LanguageServerProtocolResponse response
          in
          { state; response }
      | GetServerUuid ->
          let { Configuration.Server.server_uuid; _ } = server_configuration in
          { state; response = Some (ServerUuidResponse server_uuid) }
      (* Requests that cannot be fulfilled here. *)
      | ClientConnectionRequest _ ->
          Log.warning "Explicitly ignoring ClientConnectionRequest request";
          { state; response = None }
    with
    | Unix.Unix_error (kind, name, parameters) ->
        Log.log_unix_error (kind, name, parameters);
        log_request_error
          ~error:(Format.sprintf "Unix error %s: %s(%s)" (Unix.error_message kind) name parameters);
        { state; response = None }
    | Analysis.ClassHierarchy.Untracked annotation ->
        log_request_error ~error:(Format.sprintf "Untracked %s" (Type.show annotation));
        { state; response = None }
    | uncaught_exception ->
        let should_stop =
          match request with
          | HoverRequest _
          | GetDefinitionRequest _ ->
              false
          | _ -> true
        in
        Statistics.log_exception uncaught_exception ~fatal:should_stop ~origin:"server";
        if should_stop then
          Mutex.critical_section connections.lock ~f:(fun () ->
              Operations.stop ~reason:"uncaught exception" ~configuration:server_configuration);
        { state; response = None }
  in
  Statistics.performance
    ~name:"server request"
    ~timer
    ~normals:["request kind", Request.name request]
    ();
  result
