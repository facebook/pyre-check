(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Environment
open EnvironmentSharedMemory
open Pyre


let build ~configuration:({ Configuration.source_root; _ } as configuration) ~stubs ~sources =
  Log.info "Building type environment...";
  let environment = Environment.Builder.create ~configuration () in
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
  Environment.populate ~configuration ~source_root (reader ~configuration environment) stubs;
  Statistics.performance ~name:"stub environment built" ~timer ~configuration ();

  let timer = Timer.start () in
  let sources = get_sources sources in
  Environment.populate ~configuration ~source_root (reader ~configuration environment) sources;
  Statistics.performance ~name:"full environment built" ~timer ~configuration ();

  Log.log ~section:`Environment "%a" Environment.Builder.pp environment;
  Log.info "%s" (Environment.Builder.statistics environment);
  if Log.is_enabled `Dotty then
    begin
      let type_order_file =
        Path.create_relative
          ~root:(Configuration.pyre_root configuration)
          ~relative:"type_order.dot"
      in
      let (module Reader: Environment.Reader) = (reader ~configuration environment) in
      Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
      File.create
        ~content:(Some (TypeOrder.to_dot (module Reader.TypeOrderReader)))
        type_order_file
      |> File.write
    end;

  environment


let infer_protocols
    ~service
    ~reader:((module Reader: Environment.Reader) as reader)
    ~configuration:({ Configuration.sections; verbose; _ } as configuration) =
  let module Edge = TypeOrder.Edge in
  Log.info "Inferring protocol implementations...";
  let timer = Timer.start () in

  let map _ protocols =
    List.fold
      ~init:Edge.Set.empty
      ~f:(fun edges protocol ->
          Log.initialize ~verbose ~sections;
          Environment.infer_implementations reader ~protocol
          |> Set.union edges)
      protocols
  in
  Service.map_reduce
    service
    ~init:Edge.Set.empty
    ~map
    ~reduce:Set.union
    (Hash_set.to_list Reader.protocols)
  |> Set.iter ~f:(fun { Edge.source; target } ->
      TypeOrder.connect
        (module Reader.TypeOrderReader)
        ~configuration
        ~add_backedge:true
        ~predecessor:source
        ~successor:target);

  TypeOrder.check_integrity (module Reader.TypeOrderReader);

  Statistics.performance ~name:"inferred protocol implementations" ~timer ~configuration ()


let in_process_reader service ~configuration ~stubs ~sources =
  let reader =
    build ~configuration ~stubs ~sources
    |> Environment.reader ~configuration
  in
  infer_protocols ~service ~reader ~configuration;
  reader


(** First dumps environment to shared memory, then exposes through
    Environment_reader *)
let shared_memory_reader
    service
    ~configuration:({ Configuration.sections; verbose; _ } as configuration)
    ~stubs
    ~sources =
  let add_to_shared_memory
      {
        function_definitions;
        class_definitions;
        protocols;
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

    let add_table f = Hashtbl.iteri ~f:(fun ~key ~data -> f key data) in
    let add_type_order { TypeOrder.edges; backedges; indices; annotations } =
      add_table OrderEdges.add edges;
      add_table OrderBackedges.add backedges;
      add_table OrderIndices.add indices;
      add_table OrderAnnotations.add annotations;
      OrderKeys.add "Order" (Hashtbl.keys annotations);
    in
    add_type_order order;

    let add_function_without_body key functions =
      let strip { Ast.Node.location; value } =
        { Ast.Node.location; value = Ast.Statement.Define.strip value }
      in
      FunctionDefinitions.add key (List.map ~f:strip functions)
    in
    add_table add_function_without_body function_definitions;
    let add_class_without_function_bodies key { Ast.Node.location; value } =
      ClassDefinitions.add key { Ast.Node.location; value = Ast.Statement.Class.strip value }
    in
    add_table add_class_without_function_bodies class_definitions;
    add_table Aliases.add aliases;
    add_table Globals.add globals;
    add_table Dependents.add dependents;
    add_table FunctionKeys.add (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.add (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.add (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.add (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.add (Hashtbl.map ~f:Hash_set.to_list dependent_keys);
    ClassDefinitionsKeys.add "ClassDefinitionsKeys" (Type.Table.keys class_definitions);

    Protocols.add "Protocols" (Hash_set.to_list protocols);
  in

  Log.initialize ~verbose ~sections;
  let environment = build ~configuration ~stubs ~sources in
  add_to_shared_memory environment;

  let heap_size =
    EnvironmentSharedMemory.SharedMemory.heap_size ()
    |> Float.of_int
    |> (fun size -> size /. 1.0e6)
    |> Int.of_float
  in
  Statistics.event ~name:"shared memory size" ~integers:["size", heap_size] ~configuration ();

  let reader =
    (module struct
      let function_definitions =
        FunctionDefinitions.get

      let class_definition =
        ClassDefinitions.get

      let protocols =
        Protocols.get "Protocols"
        >>| Type.Hash_set.of_list
        |> Option.value ~default:(Type.Hash_set.create ())

      let in_class_definition_keys annotation =
        let keys = ClassDefinitionsKeys.find_unsafe "ClassDefinitionsKeys" in
        List.mem keys annotation ~equal:Type.equal

      let aliases =
        Aliases.get

      let globals =
        Globals.get

      let dependencies =
        Dependents.get

      module DependencyReader = (struct
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

        let clear_all_keys ~path =
          FunctionKeys.remove_batch (FunctionKeys.KeySet.singleton path);
          ClassKeys.remove_batch (ClassKeys.KeySet.singleton path);
          AliasKeys.remove_batch (AliasKeys.KeySet.singleton path);
          GlobalKeys.remove_batch (GlobalKeys.KeySet.singleton path);
          DependentKeys.remove_batch (DependentKeys.KeySet.singleton path)

        let dependents = Dependents.get
      end: Dependencies.Reader)

      module TypeOrderReader = struct
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

        let show () = ""

        let add_key key =
          match OrderKeys.get "Order" with
          | None -> OrderKeys.add "Order" [key]
          | Some keys -> OrderKeys.add "Order" (key :: keys)

        let keys () =
          Option.value ~default:[] (OrderKeys.get "Order")
      end

      let register_definition
          ~path
          ?name_override
          ({ Ast.Node.value = { Ast.Statement.Define.name; _ }; _ } as definition) =
        let name = Option.value ~default:name name_override in
        DependencyReader.add_function_key ~path name;
        let definitions =
          match FunctionDefinitions.get name with
          | Some definitions ->
              definition :: definitions
          | None ->
              [definition] in
        FunctionDefinitions.add name definitions


      let register_dependency ~path ~dependency =
        Log.log
          ~section:`Dependencies
          "Adding dependency from %s to %s"
          dependency
          path;
        DependencyReader.add_dependent ~path dependency


      let register_global ~path ~key ~data =
        DependencyReader.add_global_key ~path key;
        Globals.add key data


      let register_type =
        let protocols =
          Protocols.get "Protocols"
          |> Option.value ~default:[]
        in
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
          ClassDefinitions.add primitive definition
        in
        Environment.register_type
          ~order:(module TypeOrderReader: TypeOrder.Reader)
          ~configuration
          ~aliases:Aliases.get
          ~add_class_definition
          ~add_class_key:(DependencyReader.add_class_key)
          ~add_protocol:(fun protocol -> Protocols.add "Protocols" (protocol :: protocols))
          ~register_global

      let register_alias ~path ~key ~data =
        DependencyReader.add_alias_key ~path key;
        Aliases.add key data

      let purge handle =
        let path = File.Handle.show handle in
        let purge_dependents keys =
          let remove_path dependents =
            List.filter ~f:(fun dependent -> not (String.equal dependent path)) dependents
          in
          List.iter
            ~f:(fun key -> Dependents.get key >>| remove_path >>| Dependents.add key |> ignore)
            keys;
          DependentKeys.remove_batch (DependentKeys.KeySet.singleton path)
        in
        DependencyReader.get_function_keys ~path
        |> fun keys -> FunctionDefinitions.remove_batch (FunctionDefinitions.KeySet.of_list keys);

        DependencyReader.get_class_keys ~path
        |> fun keys -> ClassDefinitions.remove_batch (ClassDefinitions.KeySet.of_list keys);

        DependencyReader.get_alias_keys ~path
        |> fun keys -> Aliases.remove_batch (Aliases.KeySet.of_list keys);

        DependencyReader.get_global_keys ~path
        |> fun keys -> Globals.remove_batch (Globals.KeySet.of_list keys);

        DependencyReader.get_dependent_keys ~path |> purge_dependents;

        DependencyReader.clear_all_keys ~path;
    end: Environment.Reader)
  in

  infer_protocols ~service ~reader ~configuration;

  reader


let repopulate
    (module Reader: Environment.Reader)
    ~configuration:({ Configuration.source_root; _ } as configuration)
    ~handles =
  Log.log
    ~section:`Debug
    "Repopulating the environment with %s"
    (List.to_string ~f:File.Handle.show handles);
  let repopulate_path handle =
    Reader.purge handle;
    match AstSharedMemory.get_source handle with
    | Some source -> [source]
    | None -> []
  in
  List.concat_map ~f:repopulate_path handles
  |> Environment.populate ~configuration ~source_root (module Reader: Environment.Reader)
