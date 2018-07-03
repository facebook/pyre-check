(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Pyre

module EnvironmentSharedMemory = ServiceEnvironmentSharedMemory

open EnvironmentSharedMemory
open ServiceIgnoreSharedMemory


let populate
    (module Handler: Environment.Handler)
    ~configuration
    ?(source_root = Path.current_working_directory ())
    ?(check_dependency_exists = true)
    sources =
  (* Yikes... *)
  Handler.register_alias
    ~path:"typing.py"
    ~key:(Type.primitive "typing.DefaultDict")
    ~data:(Type.primitive "collections.defaultdict");
  Handler.register_alias
    ~path:"builtins.py"
    ~key:(Type.primitive "None")
    ~data:(Type.Optional Type.Bottom);
  (* This is broken in typeshed:
     https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
  Handler.register_alias
    ~path:"builtins.py"
    ~key:(Type.primitive "PathLike")
    ~data:(Type.primitive "_PathLike");

  List.iter ~f:(Environment.register_module (module Handler)) sources;

  let all_annotations =
    List.fold
      ~init:Type.Set.empty
      ~f:(fun annotations source ->
          Set.union annotations (Environment.register_class_definitions (module Handler) source))
      sources
    |> Set.to_list
  in
  Environment.register_aliases (module Handler) sources;

  List.iter
    ~f:(Environment.register_dependencies ~source_root ~check_dependency_exists (module Handler))
    sources;
  (* Build type order. *)
  List.iter ~f:(Environment.connect_type_order (module Handler)) sources;
  TypeOrder.deduplicate (module Handler.TypeOrderHandler) ~annotations:all_annotations;

  TypeOrder.connect_annotations_to_top
    (module Handler.TypeOrderHandler)
    ~configuration
    ~top:Type.Object
    all_annotations;
  TypeOrder.remove_extra_edges
    (module Handler.TypeOrderHandler)
    ~bottom:Type.Bottom
    ~top:Type.Object
    all_annotations;

  List.iter ~f:(Environment.register_functions (module Handler)) sources;
  List.iter ~f:(Environment.register_globals (module Handler)) sources;
  (* TODO(T30713406): Merge with class registration. *)
  List.iter ~f:Handler.refine_class_definition all_annotations


let build
    ((module Handler: Environment.Handler) as handler)
    ~configuration:({ Configuration.source_root; _ } as configuration)
    ~stubs
    ~sources =
  Log.info "Building type environment...";
  (* This grabs all sources from shared memory. It is unavoidable: Environment
     must be built sequentially until we find a way to build the environment in
     parallel. *)
  let get_sources =
    List.fold
      ~init:[]
      ~f:(fun handles path ->
          match AstSharedMemory.get_source path with
          | Some handle -> handle :: handles
          | None -> handles)
  in
  let timer = Timer.start () in
  let stubs = get_sources stubs in
  populate ~configuration ~source_root handler stubs;
  Statistics.performance ~name:"stub environment built" ~timer ~configuration ();

  let timer = Timer.start () in
  let sources =
    (* If a stub matching a path's qualifier already exists, we shouldn't override. *)
    let should_keep { Source.path; qualifier; _ } =
      Handler.module_definition qualifier
      >>= Module.path
      |> Option.value ~default:path
      |> String.equal path
    in
    let sources = get_sources sources in
    List.filter ~f:should_keep sources
  in
  populate ~configuration ~source_root handler sources;
  Statistics.performance ~name:"full environment built" ~timer ~configuration ();

  if Log.is_enabled `Dotty then
    begin
      let type_order_file =
        Path.create_relative
          ~root:(Configuration.pyre_root configuration)
          ~relative:"type_order.dot"
      in
      let (module Handler: Environment.Handler) = handler in
      Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
      File.create
        ~content:(Some (TypeOrder.to_dot (module Handler.TypeOrderHandler)))
        type_order_file
      |> File.write
    end


let in_process_handler ~configuration ~stubs ~sources =
  let environment = Environment.Builder.create ~configuration () in
  let ((module Handler: Environment.Handler) as handler) =
    Environment.handler
      environment
      ~configuration
  in
  build handler ~configuration ~stubs ~sources;
  Log.log ~section:`Environment "%a" Environment.Builder.pp environment;
  Environment.infer_protocols ~handler ~configuration;
  TypeOrder.check_integrity (module Handler.TypeOrderHandler);
  handler


(** First dumps environment to shared memory, then exposes through
    Environment_handler *)
let shared_memory_handler
    ~configuration:({ Configuration.sections; verbose; infer; _ } as configuration)
    ~stubs
    ~sources =
  let add_to_shared_memory
      {
        Environment.function_definitions;
        class_definitions;
        protocols;
        modules;
        aliases;
        globals;
        order;
        dependencies = {
          Dependencies.index = {
            Dependencies.function_keys;
            class_keys;
            alias_keys;
            global_keys;
            dependent_keys;
          };
          dependents;
        };
      } =
    Log.info "Adding environment information to shared memory...";
    (* Writing through the caches because we are doing a
       batch-add. Especially while still adding amounts of data that
       exceed the cache size, the time spent doing cache bookkeeping
       is wasted. *)
    let timer = Timer.start () in
    let add_table f = Hashtbl.iteri ~f:(fun ~key ~data -> f key data) in
    let add_type_order { TypeOrder.edges; backedges; indices; annotations } =
      add_table OrderEdges.write_through edges;
      let add_backedge key targets =
        let targets = TypeOrder.Target.Set.of_list targets in
        OrderBackedges.write_through key (Set.to_list targets)
      in
      add_table add_backedge backedges;

      add_table OrderIndices.write_through indices;
      add_table OrderAnnotations.write_through annotations;
      OrderKeys.write_through "Order" (Hashtbl.keys annotations);
    in
    add_type_order order;

    add_table FunctionDefinitions.write_through function_definitions;
    add_table ClassDefinitions.write_through class_definitions;
    add_table Aliases.write_through aliases;
    add_table Globals.write_through globals;
    add_table Dependents.write_through dependents;
    add_table FunctionKeys.write_through (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.write_through (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.write_through (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.write_through (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.write_through (Hashtbl.map ~f:Hash_set.to_list dependent_keys);

    Protocols.write_through "Protocols" (Hash_set.to_list protocols);
    add_table AstSharedMemory.add_module modules;
    Statistics.performance ~configuration ~name:"Added environment to shared memory" ~timer ()
  in

  Log.initialize ~verbose ~sections;
  let environment = Environment.Builder.create ~configuration () in
  let ((module Handler: Environment.Handler) as shared_handler) =
    (module struct
      let function_definitions =
        FunctionDefinitions.get

      let class_definition =
        ClassDefinitions.get

      let protocols () =
        Protocols.get "Protocols"
        |> Option.value ~default:[]

      let register_module ~qualifier ~local_mode ~path ~stub ~statements =
        let is_registered_empty_stub =
          AstSharedMemory.get_module qualifier
          >>| Module.empty_stub
          |> Option.value ~default:false
        in
        if not is_registered_empty_stub then
          begin
            AstSharedMemory.remove_modules [qualifier];
            AstSharedMemory.add_module
              qualifier
              (Module.create ~qualifier ~local_mode ?path ~stub statements)
          end

      let is_module access =
        AstSharedMemory.in_modules access

      let module_definition access =
        AstSharedMemory.get_module access

      let in_class_definition_keys annotation =
        ClassDefinitions.mem annotation

      let aliases =
        Aliases.get

      let globals =
        Globals.get

      let dependencies =
        Dependents.get

      module DependencyHandler = (struct
        let add_new_key ~get ~add ~path ~key =
          match get path with
          | None -> add path [key]
          | Some keys -> add path (key :: keys)

        let add_function_key ~path access =
          add_new_key ~path ~key:access ~get:FunctionKeys.get ~add:FunctionKeys.add

        let add_class_key ~path class_type =
          add_new_key ~path ~key:class_type ~get:ClassKeys.get ~add:ClassKeys.add

        let add_alias_key ~path alias =
          add_new_key ~path ~key:alias ~get:AliasKeys.get ~add:AliasKeys.add

        let add_global_key ~path global =
          add_new_key ~path ~key:global ~get:GlobalKeys.get ~add:GlobalKeys.add

        let add_dependent_key ~path dependent =
          add_new_key ~path ~key:dependent ~get:DependentKeys.get ~add:DependentKeys.add

        let add_dependent ~path dependent =
          add_dependent_key ~path dependent;
          match Dependents.get dependent with
          | None -> Dependents.add dependent [path]
          | Some dependencies -> Dependents.add dependent (path :: dependencies)

        let get_function_keys ~path = FunctionKeys.get path |> Option.value ~default:[]
        let get_class_keys ~path = ClassKeys.get path |> Option.value ~default:[]
        let get_alias_keys ~path = AliasKeys.get path |> Option.value ~default:[]
        let get_global_keys ~path = GlobalKeys.get path |> Option.value ~default:[]
        let get_dependent_keys ~path = DependentKeys.get path |> Option.value ~default:[]

        let clear_keys_batch paths =
          FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list paths);
          ClassKeys.remove_batch (ClassKeys.KeySet.of_list paths);
          AliasKeys.remove_batch (AliasKeys.KeySet.of_list paths);
          GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list paths);
          DependentKeys.remove_batch (DependentKeys.KeySet.of_list paths)

        let dependents = Dependents.get
      end: Dependencies.Handler)

      module TypeOrderHandler = struct
        type ('key, 'value) lookup = {
          get: 'key -> 'value option;
          set: 'key -> 'value -> unit;
        }

        let edges () = {
          get = OrderEdges.get;
          set = (fun key value ->
              OrderEdges.remove_batch (OrderEdges.KeySet.singleton key);
              OrderEdges.add key value);
        }

        let backedges () = {
          get = OrderBackedges.get;
          set =
            (fun key value ->
               OrderBackedges.remove_batch (OrderBackedges.KeySet.singleton key);
               OrderBackedges.add key value);
        }

        let indices () = {
          get = OrderIndices.get;
          set =
            (fun key value ->
               OrderIndices.remove_batch (OrderIndices.KeySet.singleton key);
               OrderIndices.add key value);
        }

        let annotations () = {
          get = OrderAnnotations.get;
          set =
            (fun key value ->
               OrderAnnotations.remove_batch (OrderAnnotations.KeySet.singleton key);
               OrderAnnotations.add key value);
        }

        let find { get; _ } key = get key

        let find_unsafe { get; _ } key = Option.value_exn (get key)

        let contains { get; _ } key = Option.is_some (get key)

        let set { set; _ } ~key ~data =
          set key data

        let length _ =
          (OrderKeys.get "Order"
           >>| List.length)
          |> Option.value ~default:0

        let add_key key =
          match OrderKeys.get "Order" with
          | None -> OrderKeys.add "Order" [key]
          | Some keys ->
              OrderKeys.remove_batch (OrderKeys.KeySet.singleton "Order");
              OrderKeys.add "Order" (key :: keys)

        let keys () =
          Option.value ~default:[] (OrderKeys.get "Order")

        let show () =
          let keys =
            keys ()
            |> List.sort ~compare:Int.compare
          in
          let serialized_keys = List.to_string ~f:Int.to_string keys in
          let serialized_annotations =
            let serialize_annotation key =
              find (annotations ()) key
              >>| (fun annotation -> Format.asprintf "%d->%a\n" key Type.pp annotation)
            in
            List.filter_map ~f:serialize_annotation keys
            |> String.concat
          in
          let serialized_edges edges =
            let edges_of_key key =
              let show_successor { TypeOrder.Target.target = successor; _ } =
                Option.value_exn (find (annotations ()) successor)
                |> Type.show
              in
              Option.value ~default:[] (find edges key)
              |> List.to_string ~f:show_successor
            in
            List.to_string ~f:(fun key -> Format.asprintf "%d -> %s\n" key (edges_of_key key)) keys
          in
          Format.asprintf "Keys:\n%s\nAnnotations:\n%s\nEdges:\n%s\nBackedges:\n%s\n"
            serialized_keys
            serialized_annotations
            (serialized_edges (edges ()))
            (serialized_edges (backedges ()))
      end

      let register_definition
          ~path
          ?name_override
          ({ Node.location; value = { Statement.Define.name; _ }; _ } as definition) =
        let name = Option.value ~default:name name_override in
        DependencyHandler.add_function_key ~path name;
        let annotation =
          Annotation.create_immutable ~global:true Type.Top
          |> Node.create ~location
        in
        Globals.remove_batch (Globals.KeySet.singleton name);
        Globals.add name annotation;

        if infer then
          begin
            let definitions =
              match FunctionDefinitions.get name with
              | Some definitions ->
                  definition :: definitions
              | None ->
                  [definition]
            in
            FunctionDefinitions.remove_batch (FunctionDefinitions.KeySet.singleton name);
            FunctionDefinitions.add name definitions
          end

      let refine_class_definition _ = ()

      let register_dependency ~path ~dependency =
        Log.log
          ~section:`Dependencies
          "Adding dependency from %s to %s"
          dependency
          path;
        DependencyHandler.add_dependent ~path dependency


      let register_global ~path ~access ~global =
        DependencyHandler.add_global_key ~path access;
        Globals.remove_batch (Globals.KeySet.singleton access);
        Globals.add access global


      let connect_definition =
        let add_class_definition ~primitive ~definition =
          let definition =
            match ClassDefinitions.get primitive with
            | Some ({
                Environment.class_definition = { Node.location; value = preexisting };
                _;
              } as representation) ->
                {
                  representation with
                  Environment.class_definition = {
                    Node.location;
                    value = Statement.Class.update preexisting ~definition:(Node.value definition);
                  };
                }
            | _ ->
                {
                  Environment.class_definition = definition;
                  methods = [];
                  explicit_attributes = Statement.Access.SerializableMap.empty;
                  implicit_attributes = Statement.Access.SerializableMap.empty;
                  is_test = false;
                }
          in
          ClassDefinitions.remove_batch (ClassDefinitions.KeySet.singleton primitive);
          ClassDefinitions.add primitive definition
        in
        Environment.connect_definition
          ~order:(module TypeOrderHandler: TypeOrder.Handler)
          ~configuration
          ~aliases:Aliases.get
          ~add_class_definition
          ~add_class_key:(DependencyHandler.add_class_key)
          ~add_protocol:(fun protocol ->
              let protocols = Protocols.get "Protocols" |> Option.value ~default:[] in
              Protocols.remove_batch (Protocols.KeySet.singleton "Protocols");
              Protocols.add "Protocols" (protocol :: protocols))

      let register_alias ~path ~key ~data =
        DependencyHandler.add_alias_key ~path key;
        Aliases.remove_batch (Aliases.KeySet.singleton key);
        Aliases.add key data

      let purge handles =
        let paths = List.map ~f:File.Handle.show handles in
        let purge_dependents keys =
          let remove_path dependents =
            List.filter
              ~f:(fun dependent -> not (List.mem paths dependent ~equal:String.equal))
              dependents
          in
          List.iter
            ~f:(fun key -> Dependents.get key >>| remove_path >>| Dependents.add key |> ignore)
            keys;
          DependentKeys.remove_batch (DependentKeys.KeySet.of_list paths)
        in
        List.concat_map ~f:(fun path -> DependencyHandler.get_function_keys ~path) paths
        |> fun keys ->
        begin
          FunctionDefinitions.remove_batch (FunctionDefinitions.KeySet.of_list keys);
          (* We add a global name for each function definition as well. *)
          Globals.remove_batch (Globals.KeySet.of_list keys);
        end;

        List.concat_map ~f:(fun path -> DependencyHandler.get_class_keys ~path) paths
        |> fun keys -> ClassDefinitions.remove_batch (ClassDefinitions.KeySet.of_list keys);

        List.concat_map ~f:(fun path -> DependencyHandler.get_alias_keys ~path) paths
        |> fun keys -> Aliases.remove_batch (Aliases.KeySet.of_list keys);

        List.concat_map ~f:(fun path -> DependencyHandler.get_global_keys ~path) paths
        |> fun keys -> Globals.remove_batch (Globals.KeySet.of_list keys);

        List.concat_map ~f:(fun path -> DependencyHandler.get_dependent_keys ~path) paths
        |> purge_dependents;

        DependencyHandler.clear_keys_batch paths;
        List.map ~f:(fun path -> Ast.Source.qualifier ~path) paths
        |> AstSharedMemory.remove_modules

      let mode path = ErrorModes.get path
    end: Environment.Handler)
  in
  begin
    match Sys.getenv "PYRE_USE_SHARED_MEMORY" with
    | Some "1" ->
        add_to_shared_memory environment;
        build shared_handler ~configuration ~stubs ~sources
    | _ ->
        let handler = Environment.handler ~configuration environment in
        build handler ~configuration ~stubs ~sources;
        add_to_shared_memory environment
  end;
  let heap_size =
    EnvironmentSharedMemory.SharedMemory.heap_size ()
    |> Float.of_int
    |> (fun size -> size /. 1.0e6)
    |> Int.of_float
  in
  Statistics.event ~name:"shared memory size" ~integers:["size", heap_size] ~configuration ();

  Environment.infer_protocols ~handler:shared_handler ~configuration;
  TypeOrder.check_integrity (module Handler.TypeOrderHandler);

  shared_handler
