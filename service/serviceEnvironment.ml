(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Environment
open Pyre

module Scheduler = ServiceScheduler
module AstSharedMemory = ServiceAstSharedMemory
module EnvironmentSharedMemory = ServiceEnvironmentSharedMemory

open EnvironmentSharedMemory
open ServiceIgnoreSharedMemory


let build
    ((module Handler: Handler) as handler)
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
  Environment.populate ~configuration ~source_root handler stubs;
  Statistics.performance ~name:"stub environment built" ~timer ~configuration ();

  let timer = Timer.start () in
  let sources =
    (* If a stub matching a path's qualifier already exists, we shouldn't override. *)
    let should_keep { Ast.Source.path; qualifier; _ } =
      Handler.module_definition qualifier
      >>= Ast.Module.path
      |> Option.value ~default:path
      |> String.equal path
    in
    let sources = get_sources sources in
    List.filter ~f:should_keep sources
  in
  Environment.populate ~configuration ~source_root handler sources;
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


let infer_protocols
    ~scheduler
    ~handler:((module Handler: Environment.Handler) as handler)
    ~configuration:({ Configuration.sections; verbose; _ } as configuration) =
  let module Edge = TypeOrder.Edge in
  Log.info "Inferring protocol implementations...";
  let timer = Timer.start () in

  let map _ protocols =
    List.fold
      ~init:Edge.Set.empty
      ~f:(fun edges protocol ->
          Log.initialize ~verbose ~sections;
          Environment.infer_implementations handler ~protocol
          |> Set.union edges)
      protocols
  in
  let protocols =
    (* Skip useless protocols for better performance. *)
    let skip_protocol protocol =
      match Handler.class_definition protocol with
      | Some protocol_definition ->
          let protocol_definition = Annotated.Class.create protocol_definition in
          let whitelisted = ["typing.Hashable"] in
          let name = Annotated.Class.name protocol_definition |> Ast.Expression.Access.show in
          List.is_empty (Annotated.Class.methods protocol_definition) ||
          List.mem ~equal:String.equal whitelisted name
      | _ ->
          true
    in
    List.filter ~f:(fun protocol -> not (skip_protocol protocol)) (Handler.protocols ())
  in
  Scheduler.map_reduce
    scheduler
    ~init:Edge.Set.empty
    ~map
    ~reduce:Set.union
    protocols
  |> Set.iter ~f:(fun { Edge.source; target } ->
      TypeOrder.connect
        (module Handler.TypeOrderHandler)
        ~configuration
        ~add_backedge:true
        ~predecessor:source
        ~successor:target);

  TypeOrder.check_integrity (module Handler.TypeOrderHandler);

  Statistics.performance ~name:"inferred protocol implementations" ~timer ~configuration ()


let in_process_handler scheduler ~configuration ~stubs ~sources =
  let environment = Environment.Builder.create ~configuration () in
  let handler = Environment.handler ~configuration environment in
  build handler ~configuration ~stubs ~sources;
  Log.log ~section:`Environment "%a" Environment.Builder.pp environment;
  infer_protocols ~scheduler ~handler ~configuration;
  handler


(** First dumps environment to shared memory, then exposes through
    Environment_handler *)
let shared_memory_handler
    scheduler
    ~configuration:({ Configuration.sections; verbose; infer; _ } as configuration)
    ~stubs
    ~sources =
  let add_to_shared_memory
      {
        function_definitions;
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
    let timer = Timer.start () in
    let add_table f = Hashtbl.iteri ~f:(fun ~key ~data -> f key data) in
    let add_type_order { TypeOrder.edges; backedges; indices; annotations } =
      add_table OrderEdges.add edges;
      let add_backedge key targets =
        let targets = TypeOrder.Target.Set.of_list targets in
        OrderBackedges.add key (Set.to_list targets)
      in
      add_table add_backedge backedges;

      add_table OrderIndices.add indices;
      add_table OrderAnnotations.add annotations;
      OrderKeys.add "Order" (Hashtbl.keys annotations);
    in
    add_type_order order;

    add_table FunctionDefinitions.add function_definitions;
    add_table ClassDefinitions.add class_definitions;
    add_table Aliases.add aliases;
    add_table Globals.add globals;
    add_table Dependents.add dependents;
    add_table FunctionKeys.add (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.add (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.add (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.add (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.add (Hashtbl.map ~f:Hash_set.to_list dependent_keys);

    Protocols.add "Protocols" (Hash_set.to_list protocols);
    add_table Modules.add modules;
    Statistics.performance ~configuration ~name:"Added environment to shared memory" ~timer ()
  in

  Log.initialize ~verbose ~sections;
  let environment = Environment.Builder.create ~configuration () in
  let shared_handler =
    (module struct
      let function_definitions =
        FunctionDefinitions.get

      let class_definition =
        ClassDefinitions.get

      let protocols () =
        Protocols.get "Protocols"
        |> Option.value ~default:[]

      let register_module ~qualifier ~path ~stub ~statements =
        let is_registered_empty_stub =
          Modules.get qualifier
          >>| Ast.Module.empty_stub
          |> Option.value ~default:false
        in
        if not is_registered_empty_stub then
          begin
            Modules.remove_batch (Modules.KeySet.singleton qualifier);
            Modules.add qualifier (Ast.Module.create ~qualifier ?path ~stub statements)
          end

      let is_module access =
        Modules.mem access

      let module_definition access =
        Modules.get access

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
          ({ Ast.Node.location; value = { Ast.Statement.Define.name; _ }; _ } as definition) =
        let name = Option.value ~default:name name_override in
        DependencyHandler.add_function_key ~path name;
        let annotation =
          Annotation.create_immutable ~global:true Type.Top
          |> Ast.Node.create ~location
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
            let open Ast in
            match ClassDefinitions.get primitive with
            | Some { Node.location; value = preexisting } ->
                {
                  Node.location;
                  value = Statement.Class.update preexisting ~definition:(Node.value definition);
                }
            | _ ->
                definition
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
          |> Modules.KeySet.of_list
          |> Modules.remove_batch

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

  infer_protocols ~scheduler ~handler:shared_handler ~configuration;

  shared_handler
