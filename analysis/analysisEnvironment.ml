(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Dependencies = AnalysisDependencies
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type class_representation = {
  class_definition: Class.t Node.t;
  explicit_attributes: Attribute.t Access.SerializableMap.t;
  implicit_attributes: Attribute.t Access.SerializableMap.t;
  is_test: bool;
  methods: Type.t list;
}

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: class_representation Type.Table.t;
  protocols: Type.Hash_set.t;
  modules: Module.t Access.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Access.Table.t;
  dependencies: Dependencies.t;
}

module type Handler = sig
  val register_definition
    :  path: string
    -> ?name_override: Access.t
    -> (Define.t Node.t)
    -> unit
  val register_dependency: path: string -> dependency: string -> unit
  val register_global: path: string -> access: Access.t -> global: Resolution.global -> unit
  val connect_definition
    :  path: string
    -> resolution: Resolution.t
    -> predecessor: Type.t
    -> name: Access.t
    -> definition: (Class.t Node.t) option
    -> (Type.t * Type.t list)
  val refine_class_definition: Type.t -> unit
  val register_alias: path: string -> key: Type.t -> data: Type.t -> unit
  val purge: File.Handle.t list -> unit

  val function_definitions: Access.t -> (Define.t Node.t) list option
  val class_definition: Type.t -> class_representation option
  val protocols: unit -> Type.t list

  val register_module
    :  qualifier: Access.t
    -> local_mode: Source.mode
    -> path: string option
    -> stub: bool
    -> statements: Statement.t list
    -> unit

  val is_module: Access.t -> bool
  val module_definition: Access.t -> Module.t option

  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: string -> string list option

  val mode: string -> Source.mode option

  module DependencyHandler: Dependencies.Handler

  module TypeOrderHandler: TypeOrder.Handler
end


let connect_definition
    ~order
    ~aliases
    ~add_class_definition
    ~add_class_key
    ~add_protocol =
  let rec connect_definition ~path ~resolution ~predecessor ~name ~definition =
    let connect ~predecessor ~successor ~parameters =
      let annotations_tracked =
        let (module Handler: TypeOrder.Handler) = order in
        Handler.contains (Handler.indices ()) predecessor &&
        Handler.contains (Handler.indices ()) successor
      in
      let primitive_cycle =
        (* Primitive cycles can be introduced by meta-programming. *)
        Type.equal predecessor successor
      in
      let cycle_with_top =
        match predecessor, successor with
        | Type.Top, _ -> true
        | Type.Object, successor when not (Type.equal successor Type.Top) -> true
        | _ -> false
      in
      if annotations_tracked && not primitive_cycle && not cycle_with_top then
        TypeOrder.connect order ~predecessor ~successor ~parameters
    in

    let annotation =
      Resolution.parse_annotation
        resolution
        (Node.create_with_default_location (Access name))
    in
    let primitive, parameters = Type.split annotation in
    connect ~predecessor ~successor:primitive ~parameters;

    (* Handle definition. *)
    begin
      match definition with
      | Some ({ Node.value = { Class.name; bases; _ } as definition; _ } as definition_node)
        when not (Type.equal primitive Type.Object) || Access.show name = "object" ->
          add_class_key ~path primitive;
          let annotated = Annotated.Class.create definition_node in

          (* Register protocols. *)
          if Annotated.Class.is_protocol annotated then
            add_protocol primitive;

          (* Register normal annotations. *)
          add_class_definition ~primitive ~definition:definition_node;
          if List.length definition.Class.bases > 0 then
            begin
              let register_supertype base =
                let qualified_name =
                  let value = Expression.delocalize base.Argument.value in
                  match Node.value value with
                  | Access access ->
                      let primitive, _ =
                        Type.create ~aliases value
                        |> Type.split
                      in
                      if not (TypeOrder.contains order primitive) &&
                         not (Type.equal primitive Type.Top) then
                        begin
                          Log.log
                            ~section:`Environment
                            "Superclass annotation %a is missing"
                            Type.pp
                            primitive;
                          None
                        end
                      else
                        Some access
                  | _ ->
                      None in
                let super_annotation, parameters =
                  match qualified_name with
                  | Some name ->
                      connect_definition
                        ~path
                        ~resolution
                        ~predecessor:annotation
                        ~name
                        ~definition:None
                  | None ->
                      Type.Object, []
                in
                connect ~predecessor:primitive ~successor:super_annotation ~parameters
              in
              let bases =
                let inferred_base =
                  Annotated.Class.inferred_generic_base
                    (Annotated.Class.create definition_node)
                    ~resolution
                in
                inferred_base @ bases
              in
              List.iter bases ~f:register_supertype
            end
          else
            connect ~predecessor:primitive ~successor:Type.Object ~parameters:[]
      | _ ->
          ()
    end;
    primitive, parameters
  in connect_definition


let handler
    {
      function_definitions;
      class_definitions;
      protocols;
      modules;
      order;
      aliases;
      globals;
      dependencies;
    }
    ~configuration:{ Configuration.infer; _ } =
  let (module DependencyHandler: Dependencies.Handler) = Dependencies.handler dependencies in

  (module struct
    module TypeOrderHandler = (val TypeOrder.handler order: TypeOrder.Handler)
    module DependencyHandler = DependencyHandler

    let register_definition
        ~path
        ?name_override
        ({ Node.value = { Define.name; _  }; _ } as definition) =
      let name = Option.value ~default:name name_override in
      DependencyHandler.add_function_key ~path name;
      if infer then
        let definitions =
          match Hashtbl.find function_definitions name with
          | Some definitions ->
              definition::definitions
          | None ->
              [definition]
        in
        Hashtbl.set ~key:name ~data:definitions function_definitions


    let register_dependency ~path ~dependency =
      Log.log
        ~section:`Dependencies
        "Adding dependency from %s to %s"
        dependency
        path;
      DependencyHandler.add_dependent ~path dependency


    let register_global ~path ~access ~global =
      DependencyHandler.add_global_key ~path access;
      Hashtbl.set ~key:access ~data:global globals


    let connect_definition =
      let add_class_definition ~primitive ~definition =
        let definition =
          match Hashtbl.find class_definitions primitive with
          | Some ({
              class_definition = { Node.location; value = preexisting };
              _;
            } as class_representation) ->
              {
                class_representation with
                class_definition = {
                  Node.location;
                  value = Class.update preexisting ~definition:(Node.value definition);
                };
              }
          | _ ->
              {
                class_definition = definition;
                methods = [];
                explicit_attributes = Access.SerializableMap.empty;
                implicit_attributes = Access.SerializableMap.empty;
                is_test = false;
              }
        in
        Hashtbl.set class_definitions ~key:primitive ~data:definition
      in
      connect_definition
        ~order:(TypeOrder.handler order)
        ~aliases:(Hashtbl.find aliases)
        ~add_class_definition
        ~add_class_key:DependencyHandler.add_class_key
        ~add_protocol:(Hash_set.add protocols)


    let refine_class_definition annotation =
      let refine { class_definition = { Node.location; value = class_definition }; _ } =
        let in_test =
          let is_unit_test { class_definition = { Node.value = { Class.name; _ }; _ }; _ } =
            Access.equal name (Access.create "unittest.TestCase")
          in
          let successors =
            TypeOrder.successors (module TypeOrderHandler) annotation
            |> List.filter_map ~f:(Hashtbl.find class_definitions)
          in
          List.exists ~f:is_unit_test successors
        in
        let explicit_attributes = Class.explicitly_assigned_attributes class_definition in
        let implicit_attributes = Class.implicit_attributes ~in_test class_definition in
        Hashtbl.set
          class_definitions
          ~key:annotation
          ~data:{
            class_definition = { Node.location; value = class_definition };
            is_test = in_test;
            explicit_attributes;
            implicit_attributes;
            methods = [];
          }
      in
      Hashtbl.find class_definitions annotation
      >>| refine
      |> ignore


    let register_alias ~path ~key ~data =
      DependencyHandler.add_alias_key ~path key;
      Hashtbl.set ~key ~data aliases


    let purge handles =
      let paths = List.map ~f:File.Handle.show handles in

      let purge_table_given_keys table keys =
        List.iter ~f:(fun key -> Hashtbl.remove table key) keys
      in
      (* Dependents are handled differently from other keys, because in each other
       * instance, the path is the only one adding entries to the key. However, we can have
       *  both a.py and b.py import c.py, and thus have c.py in its keys. Therefore, when
       * purging a.py, we need to take care not to remove the c -> b dependent relationship. *)
      let purge_dependents keys =
        let remove_path dependents =
          List.filter
            ~f:(fun dependent -> not (List.mem paths dependent ~equal:String.equal))
            dependents
        in
        let dependents = dependencies.Dependencies.dependents in
        List.iter
          ~f:(fun key ->
              Hashtbl.find dependents key
              >>| remove_path
              >>| (fun dependents_list -> Hashtbl.set ~key ~data:dependents_list dependents)
              |> ignore)
          keys
      in
      List.concat_map ~f:(fun path -> DependencyHandler.get_function_keys ~path) paths
      |> purge_table_given_keys function_definitions;
      List.concat_map ~f:(fun path -> DependencyHandler.get_class_keys ~path) paths
      |> purge_table_given_keys class_definitions;
      List.concat_map ~f:(fun path -> DependencyHandler.get_alias_keys ~path) paths
      |> purge_table_given_keys aliases;
      List.concat_map ~f:(fun path -> DependencyHandler.get_global_keys ~path) paths
      |> purge_table_given_keys globals;
      List.concat_map ~f:(fun path -> DependencyHandler.get_dependent_keys ~path) paths
      |> purge_dependents;
      DependencyHandler.clear_keys_batch paths;
      List.map ~f:(fun path -> Source.qualifier ~path) paths
      |> List.iter ~f:(Hashtbl.remove modules)


    let function_definitions =
      Hashtbl.find function_definitions

    let class_definition =
      Hashtbl.find class_definitions

    let protocols () =
      Hash_set.to_list protocols

    let register_module ~qualifier ~local_mode ~path ~stub ~statements =
      let is_registered_empty_stub =
        Hashtbl.find modules qualifier
        >>| Module.empty_stub
        |> Option.value ~default:false
      in

      let string =
        Annotation.create_immutable ~global:true Type.string
        |> Node.create_with_default_location
      in
      Hashtbl.set globals ~key:(qualifier @ (Access.create "__file__")) ~data:string;
      Hashtbl.set globals ~key:(qualifier @ (Access.create "__name__")) ~data:string;

      if not is_registered_empty_stub then
        Hashtbl.set
          ~key:qualifier
          ~data:(Module.create ~qualifier ~local_mode ?path ~stub statements)
          modules

    let is_module access =
      Hashtbl.mem modules access

    let module_definition access =
      Hashtbl.find modules access

    let in_class_definition_keys =
      Hashtbl.mem class_definitions

    let aliases =
      Hashtbl.find aliases

    let globals =
      Hashtbl.find globals

    let dependencies =
      DependencyHandler.dependents

    let mode _ =
      None
  end: Handler)


let resolution (module Handler: Handler) ?(annotations = Access.Map.empty) () =
  let parse_annotation = Type.create ~aliases:Handler.aliases in

  let class_definition annotation =
    let primitive, _ = Type.split annotation in
    match Handler.class_definition primitive with
    | Some { class_definition; _ } ->
        Some class_definition
    | None ->
        None
  in

  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  Resolution.create
    ~annotations
    ~order
    ~resolve:
      (fun ~resolution expression ->
         Annotated.resolve
           ~resolution
           expression)
    ~resolve_literal:Annotated.resolve_literal
    ~parse_annotation
    ~global:Handler.globals
    ~module_definition:Handler.module_definition
    ~class_definition
    ()


let dependencies (module Handler: Handler) =
  Handler.dependencies


let register_module
    (module Handler: Handler)
    { Source.qualifier; path; statements; metadata = { Source.Metadata.local_mode; _ }; _ } =
  let rec register_submodules = function
    | [] ->
        ()
    | (_ :: tail) as reversed ->
        let qualifier = List.rev reversed in
        if not (Handler.is_module qualifier) then
          Handler.register_module ~path:None ~qualifier ~local_mode ~stub:false ~statements:[];
        register_submodules tail
  in
  Handler.register_module
    ~qualifier
    ~local_mode
    ~path:(Some path)
    ~stub:(String.is_suffix path ~suffix:".pyi")
    ~statements;
  if List.length qualifier > 1 then
    register_submodules (List.rev qualifier |> List.tl_exn)


let register_class_definitions (module Handler: Handler) source =
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = Type.Set.t

      let statement_keep_recursing _ = Transform.Recurse

      let statement _ new_annotations = function
        | { Node.value = Class { Class.name; _ }; _ }
        | { Node.value = Stub (Stub.Class { Class.name; _ }); _ } ->
            let primitive, _ =
              Type.create ~aliases:Handler.aliases (Node.create_with_default_location (Access name))
              |> Type.split
            in
            if not (TypeOrder.contains order primitive) then
              begin
                TypeOrder.insert order primitive;
                Set.add new_annotations primitive
              end
            else
              new_annotations
        | _ ->
            new_annotations
    end)
  in
  Visit.visit Type.Set.empty source


let register_aliases (module Handler: Handler) sources =
  Type.Cache.disable ();
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let collect_aliases { Source.path; statements; qualifier; _ } =
    let rec visit_statement ~qualifier ?(in_class_body = false) aliases { Node.value; _ } =
      match value with
      | Assign { Assign.target; annotation = None; value = Some value; _ } ->
          let target =
            let access =
              let access =
                Expression.access target
                |> Access.delocalize_qualified
              in
              if in_class_body then
                access
              else
                qualifier @ access
            in
            { target with Node.value = Access access }
          in
          let value = Expression.delocalize value in

          let value_annotation = Type.create ~aliases:Handler.aliases value in
          let target_annotation = Type.create ~aliases:Handler.aliases target in
          if not (Type.equal target_annotation Type.Top ||
                  Type.equal value_annotation Type.Top ||
                  Type.equal value_annotation target_annotation) then
            (path, target, value) :: aliases
          else
            aliases
      | Class { Class.name; body; _ } ->
          List.fold body ~init:aliases ~f:(visit_statement ~qualifier:name ~in_class_body:true)
      | Import { Import.from = Some from; imports = [{ Import.name; _ }]  }
        when Access.show name = "*" ->
          let exports =
            Handler.module_definition from
            >>| Module.wildcard_exports
            |> Option.value ~default:[]
          in
          let add_alias aliases export =
            let value = Node.create_with_default_location (Access (from @ export)) in
            let alias = Node.create_with_default_location (Access (qualifier @ export)) in
            (path, alias, value) :: aliases
          in
          List.fold exports ~init:aliases ~f:add_alias
      | Import { Import.from = Some from; imports } ->
          let from =
            match Access.show from with
            | "builtins" -> []
            | _ -> from
          in
          let import_to_alias { Import.name; alias } =
            let qualified_name =
              match alias with
              | None ->
                  Node.create_with_default_location (Access (qualifier @ name))
              | Some alias ->
                  Node.create_with_default_location (Access (qualifier @ alias))
            in
            [
              path,
              qualified_name,
              Node.create_with_default_location (Access (from @ name));
            ]
          in
          List.rev_append (List.concat_map ~f:import_to_alias imports) aliases
      | _ ->
          aliases
    in
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements
  in
  let rec resolve_aliases unresolved =
    if List.is_empty unresolved then
      ()
    else
      let register_alias (any_changed, unresolved) (path, target, value) =
        let target_annotation = Type.create ~aliases:Handler.aliases target in
        let value_annotation =
          match Type.create ~aliases:Handler.aliases value with
          | Type.Variable variable ->
              let name =
                Expression.access target
                |> Access.show
                |> Identifier.create
              in
              Type.Variable { variable with Type.variable = name }
          | annotation ->
              annotation
        in

        let rec tracked annotation =
          match annotation with
          | Type.Primitive _
          | Type.Parametric _ ->
              let primitive, _ = Type.split annotation in
              let access =
                Type.expression primitive
                |> Expression.access
              in
              if Module.from_empty_stub ~access ~module_definition:Handler.module_definition then
                Some Type.Object
              else if TypeOrder.contains order primitive then
                Some annotation
              else
                None

          | Type.Callable _ ->
              Some annotation


          | Type.Tuple (Type.Bounded annotations) ->
              let tracked = List.filter_map annotations ~f:tracked in
              if List.length tracked = List.length annotations then
                Some (Type.Tuple (Type.Bounded tracked))
              else
                None
          | Type.Tuple (Type.Unbounded annotation) ->
              tracked annotation
              >>| fun annotation -> Type.Tuple (Type.Unbounded annotation)

          | Type.Union annotations ->
              let tracked = List.filter_map annotations ~f:tracked in
              if List.length tracked = List.length annotations then
                Some (Type.union tracked)
              else
                None

          | Type.Optional Type.Bottom ->
              Some annotation
          | Type.Optional annotation ->
              tracked annotation
              >>| Type.optional

          | Type.Object
          | Type.Variable _ ->
              Some annotation

          | Type.Top
          | Type.Bottom ->
              None
        in

        let target_primitive, _ = Type.split target_annotation in
        match not (TypeOrder.contains order target_primitive), tracked value_annotation with
        | true, Some value_annotation ->
            Handler.register_alias ~path ~key:target_annotation ~data:value_annotation;
            (true, unresolved)
        | _ ->
            (any_changed, (path, target, value) :: unresolved)
      in
      let (any_changed, unresolved) = List.fold ~init:(false, []) ~f:register_alias unresolved in
      if any_changed then
        resolve_aliases unresolved
      else
        let show_unresolved (path, target, value) =
          Log.debug
            "Unresolved alias %s:%a <- %a"
            path
            Expression.pp target
            Expression.pp value
        in
        List.iter ~f:show_unresolved unresolved
  in
  List.concat_map ~f:collect_aliases sources
  |> resolve_aliases;
  Type.Cache.enable ()


let register_globals
    (module Handler: Handler)
    ({ Source.path; qualifier; statements; _ } as source) =
  let resolution = resolution (module Handler: Handler) ~annotations:Access.Map.empty () in

  let qualified_access access =
    let access =
      match access with
      | (Access.Identifier builtins) :: tail when Identifier.show builtins = "builtins" -> tail
      | _ -> access
    in
    Access.delocalize_qualified access
  in

  (* Register meta annotations for classes. *)
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let statement_keep_recursing _ = Transform.Recurse

      let statement { Source.path; _ } _ = function
        | { Node.location; value = Class { Class.name; _ } }
        | { Node.location; value = Stub (Stub.Class { Class.name; _ }) } ->
            (* Register meta annotation. *)
            let primitive, _ =
              Node.create ~location (Access name)
              |> Resolution.parse_annotation resolution
              |> Type.split
            in
            let global =
              Annotation.create_immutable
                ~global:true
                ~original:(Some Type.Top)
                (Type.meta primitive)
              |> Node.create ~location
            in
            Handler.register_global ~path ~access:(qualified_access name) ~global
        | _ ->
            ()
    end)
  in
  Visit.visit () source
  |> ignore;

  let rec visit statement =
    match statement with
    | { Node.value = If { If.body; orelse; _ }; _ } ->
        (* TODO(T28732125): Properly take an intersection here. *)
        List.iter ~f:visit body;
        List.iter ~f:visit orelse
    | _ ->
        let global =
          match statement with
          | {
            Node.location;
            value = Assign {
                Assign.target;
                annotation = None;
                value = Some value;
                _;
              };
          } ->
              begin
                try
                  match target.Node.value, (Resolution.resolve_literal resolution value) with
                  | Access access, annotation ->
                      let global =
                        Annotation.create_immutable
                          ~global:true
                          ~original:(Some Type.Top)
                          annotation
                        |> Node.create ~location
                      in
                      Some (access, global)
                  | _ ->
                      None
                with _ ->
                (* TODO(T19628746): joins are not sound when building the environment. *)
                match target.Node.value with
                | Access access ->
                    (* If we have a global of the form x = os.path.join('a', 'b'), still add x. *)
                    let global =
                      Annotation.create_immutable ~global:true Type.Top
                      |> Node.create ~location
                    in
                    Some (access, global)
                | _ ->
                    None
              end
          | {
            Node.location;
            value = Assign {
                Assign.target = { Node.value = Access access; _ };
                annotation = Some annotation;
                _;
              };
          }
          | {
            Node.location;
            value = Stub (Stub.Assign {
                Assign.target = { Node.value = Access access; _ };
                annotation = Some annotation;
                _;
              });
          } ->
              let global =
                Annotation.create_immutable
                  ~global:true
                  (Type.create ~aliases:Handler.aliases annotation)
                |> Node.create ~location
              in
              Some (access, global)
          | _ ->
              None
        in
        global
        >>| (fun (access, global) ->
            let access = qualified_access (qualifier @ access) in
            Handler.register_global ~path ~access ~global)
        |> ignore
  in
  List.iter ~f:visit statements


let connect_type_order (module Handler: Handler) source =
  let resolution = resolution (module Handler) () in

  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let statement_keep_recursing _ = Transform.Recurse

      let statement { Source.path; _ } _ = function
        | { Node.location; value = Class ({ Class.name; _ } as definition) }
        | { Node.location; value = Stub (Stub.Class ({ Class.name; _ } as definition)) } ->
            Handler.connect_definition
              ~path
              ~resolution
              ~predecessor:Type.Bottom
              ~name
              ~definition:(Some (Node.create ~location definition))
            |> ignore;
        | _ ->
            ()
    end)
  in
  Visit.visit () source
  |> ignore


let register_dependencies
    ?(source_root = Path.current_working_directory ())
    ?(check_dependency_exists = true)
    (module Handler: Handler)
    source =

  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let statement_keep_recursing _ = Transform.Recurse

      let statement { Source.path; _ } _ = function
        | { Node.value = Import { Import.from; imports }; _ } ->
            let imports =
              let path_of_import access =
                let path =
                  let path =
                    Handler.module_definition access
                    >>= Module.path
                    >>| fun relative ->
                    Path.create_relative ~root:source_root ~relative
                  in
                  match path with
                  | Some path ->
                      path
                  | None ->
                      let show_identifier = function
                        | Access.Identifier identifier ->
                            Identifier.show identifier
                        | access -> Access.show_access Expression.pp access
                      in
                      Format.sprintf "%s.py"
                        (access
                         |> List.map ~f:show_identifier
                         |> List.fold ~init:(Path.absolute source_root) ~f:(^/))
                      |> (fun relative -> Path.create_relative ~root:source_root ~relative)
                in
                if Path.file_exists path || not check_dependency_exists then
                  Path.relative path
                else
                  begin
                    Log.log
                      ~section:`Dependencies
                      "Import path not found in %s"
                      (Path.absolute path);
                    None
                  end
              in
              let import_accesses =
                match from with
                (* If analyzing from x import y, only add x to the dependencies.
                   Otherwise, add all dependencies. *)
                | None -> imports |> List.map ~f:(fun { Import.name; _ } -> name)
                | Some base_module -> [base_module]
              in
              List.filter_map ~f:path_of_import import_accesses
            in
            List.iter
              ~f:(fun dependency -> Handler.register_dependency ~path ~dependency)
              imports
        | _ ->
            ()
    end)
  in
  Visit.visit () source


let register_functions (module Handler: Handler) ({ Source.path; _ } as source) =
  let resolution = resolution (module Handler: Handler) ~annotations:Access.Map.empty () in

  let module CollectCallables = Visit.MakeStatementVisitor(struct
      type t = ((Type.Callable.t Node.t) list) Access.Map.t

      let statement_keep_recursing _ =
        Transform.Recurse

      let statement _ callables statement =
        let collect_callable
            ~location
            callables
            ({ Define.name; _ } as define) =
          Handler.register_definition ~path ~name_override:name (Node.create ~location define);

          (* Register callable global. *)
          let callable =
            Annotated.Callable.create [define] ~resolution
            |> Node.create ~location
          in
          let change callable = function
            | None -> Some [callable]
            | Some existing -> Some (callable :: existing)
          in
          Map.change callables name ~f:(change callable)
        in
        match statement with
        | { Node.location; value = Class definition }
        | { Node.location; value = Stub (Stub.Class definition) } ->
            (* Register constructors. *)
            Node.create ~location definition
            |> Annotated.Class.create
            |> Annotated.Class.constructors ~resolution
            |> List.fold
              ~init:callables
              ~f:(collect_callable ~location)

        | { Node.location; value = Define define }
        | { Node.location; value = Stub (Stub.Define define) }
          when not (Define.is_constructor define) ->
            Annotated.Callable.apply_decorators ~resolution ~define
            |> Annotated.Define.create
            |> Annotated.Define.define
            |> collect_callable ~location callables

        | _ ->
            callables
    end)
  in

  let register_callables ~key ~data =
    assert (not (List.is_empty data));
    let location =
      List.hd_exn data
      |> Node.location
    in
    Type.Callable.from_overloads (List.map ~f:Node.value data)
    >>| (fun callable -> Type.Callable callable)
    >>| Annotation.create_immutable ~global:true
    >>| Node.create ~location
    >>| (fun global -> Handler.register_global ~path ~access:key ~global)
    |> ignore
  in

  CollectCallables.visit Access.Map.empty source
  |> Map.iteri ~f:register_callables


let infer_implementations (module Handler: Handler) ~implementing_classes ~protocol =
  let module Edge = TypeOrder.Edge in
  let resolution = resolution (module Handler: Handler) ~annotations:Access.Map.empty () in
  let open Annotated in

  Resolution.class_definition resolution protocol
  >>| (fun protocol_definition ->
      let protocol_definition = Class.create protocol_definition in
      let implementations =
        let classes_to_analyze =
          (* Get all implementing classes. *)
          let names =
            Class.methods protocol_definition
            |> List.map ~f:Class.Method.name
            |> List.map ~f:(fun access -> [List.last_exn access])
          in
          if List.is_empty names then
            let annotations = Handler.TypeOrderHandler.annotations () in
            Handler.TypeOrderHandler.keys ()
            |> List.map ~f:(Handler.TypeOrderHandler.find_unsafe annotations)
          else
            let get_implementing_methods method_name =
              implementing_classes ~method_name
              |> Option.value ~default:[]
              |> List.map ~f:(fun class_name -> Type.primitive (Statement.Access.show class_name))
            in
            List.concat_map ~f:get_implementing_methods names
            |> List.dedup_and_sort ~compare:Type.compare
        in
        let implements annotation =
          Handler.class_definition annotation
          >>| (fun { class_definition; _ } -> class_definition)
          >>| Class.create
          >>| (fun definition ->
              not (Class.is_protocol definition) &&
              Class.implements ~protocol:protocol_definition definition)
          |> Option.value ~default:false
        in
        List.filter ~f:implements classes_to_analyze
      in

      (* Get edges to protocol. *)
      let edges =
        let add_edge sofar source =
          (* Even if `object` technically implements a protocol, do not add cyclic edge. *)
          if Type.equal source protocol || Type.equal source Type.Object then
            sofar
          else
            Set.add sofar { Edge.source; target = protocol }
        in
        List.fold ~init:Edge.Set.empty ~f:add_edge implementations
      in
      Log.log
        ~section:`Protocols
        "Found implementations for protocol %a: %s"
        Type.pp protocol
        (List.map ~f:Type.show implementations |> String.concat ~sep:", ");
      edges)
  |> Option.value ~default:Edge.Set.empty


let infer_protocol_edges ~handler:((module Handler: Handler) as handler) ~classes_to_infer =
  let module Edge = TypeOrder.Edge in
  Log.info "Inferring protocol implementations...";
  let protocols =
    (* Skip useless protocols for better performance. *)
    let skip_protocol protocol =
      match Handler.class_definition protocol with
      | Some { class_definition = protocol_definition; _ } ->
          let protocol_definition = Annotated.Class.create protocol_definition in
          let whitelisted = ["typing.Hashable"] in
          let name = Annotated.Class.name protocol_definition |> Expression.Access.show in
          List.is_empty (Annotated.Class.methods protocol_definition) ||
          List.mem ~equal:String.equal whitelisted name
      | _ ->
          true
    in
    List.filter ~f:(fun protocol -> not (skip_protocol protocol)) (Handler.protocols ())
  in
  let implementing_classes =
    let methods_to_implementing_classes =
      let open Statement in
      let protocol_methods =
        let names_of_methods protocol =
          Handler.class_definition protocol
          >>| (fun { class_definition; _ } -> class_definition)
          >>| Annotated.Class.create
          >>| Annotated.Class.methods
          >>| List.map ~f:Annotated.Class.Method.name
          >>| List.map ~f:(fun name -> [List.last_exn name])
          |> Option.value ~default:[]
        in
        List.concat_map ~f:names_of_methods protocols
        |> Access.Set.of_list
      in
      let annotations = Handler.TypeOrderHandler.annotations () in
      let add_type_methods methods_to_implementing_classes index =
        let class_definition =
          Handler.TypeOrderHandler.find_unsafe annotations index
          |> Handler.class_definition
        in
        match class_definition with
        | None ->
            methods_to_implementing_classes
        | Some { class_definition = { Node.value = { Class.name = class_name; body; _ }; _ }; _ } ->
            (* TODO(T30499509): Rely on existing class defines instead of repeating work. *)
            let add_method methods_to_implementing_classes { Node.value = statement; _ } =
              match statement with
              | Stub (Stub.Define { Define.name; _ })
              | Define { Define.name;  _ } ->
                  let method_name = [List.last_exn name] in
                  if Set.mem protocol_methods method_name then
                    let classes =
                      match Map.find methods_to_implementing_classes method_name with
                      | Some classes -> class_name :: classes
                      | None -> [class_name]
                    in
                    Map.set methods_to_implementing_classes ~key:method_name ~data:classes
                  else
                    methods_to_implementing_classes
              | _ ->
                  methods_to_implementing_classes
            in
            List.fold body ~f:add_method ~init:methods_to_implementing_classes
      in
      List.fold classes_to_infer ~f:add_type_methods ~init:Access.Map.empty
    in
    fun ~method_name -> Map.find methods_to_implementing_classes method_name
  in
  let add_protocol_edges edges protocol =
    infer_implementations handler ~implementing_classes ~protocol
    |> Set.union edges
  in
  List.fold ~init:TypeOrder.Edge.Set.empty ~f:add_protocol_edges protocols


let infer_protocols ?classes_to_infer ~handler:((module Handler: Handler) as handler) () =
  let timer = Timer.start () in
  let classes_to_infer =
    match classes_to_infer with
    | Some classes ->
        List.filter_map
          classes
          ~f:(Handler.TypeOrderHandler.find (Handler.TypeOrderHandler.indices ()))
    | None ->
        Handler.TypeOrderHandler.keys ()
  in
  infer_protocol_edges ~handler ~classes_to_infer
  |> Set.iter ~f:(fun { TypeOrder.Edge.source; target } ->
      TypeOrder.connect
        (module Handler.TypeOrderHandler)
        ~predecessor:source
        ~successor:target);

  Statistics.performance ~name:"inferred protocol implementations" ~timer ()


module Builder = struct
  let create () =
    let function_definitions = Access.Table.create () in
    let class_definitions = Type.Table.create () in
    let protocols = Type.Hash_set.create () in
    let modules = Access.Table.create () in
    let order = TypeOrder.Builder.default () in
    let aliases = Type.Table.create () in
    let globals = Access.Table.create () in
    let dependencies = Dependencies.create () in

    (* Register dummy module for `builtins`. *)
    let builtins = Access.create "builtins" in
    Hashtbl.set
      modules
      ~key:builtins
      ~data:(
        Ast.Module.create
          ~qualifier:builtins
          ~local_mode:Ast.Module.Source.PlaceholderStub
          ~stub:true
          []);

    (* Add `None` constant to globals. *)
    let annotation annotation =
      Annotation.create_immutable ~global:true annotation
      |> Node.create_with_default_location
    in
    Hashtbl.set globals ~key:(Access.create "None") ~data:(annotation Type.none);
    Hashtbl.set globals
      ~key:[Access.Identifier (Identifier.create "...")]
      ~data:(annotation Type.Object);

    (* Add classes for `typing.Optional` and `typing.Undeclared` that are currently not encoded
       in the stubs. *)
    let add_special_class ~name ~bases ~body =
      let definition =
        {
          Class.name = Access.create name;
          bases;
          body;
          decorators = [];
          docstring = None;
        }
      in
      Hashtbl.set
        ~key:(Type.primitive name)
        ~data:{
          class_definition = Node.create_with_default_location definition;
          methods = [];
          explicit_attributes = Access.SerializableMap.empty;
          implicit_attributes = Access.SerializableMap.empty;
          is_test = false;
        }
        class_definitions;
    in
    List.iter
      ~f:(fun (name, bases, body) -> add_special_class ~name ~bases ~body)
      [
        "typing.Optional", [], [];
        "typing.Undeclared", [], [];
        "typing.NoReturn", [], [];
        "typing.Type",
        [
          {
            Argument.name = None;
            value = Type.parametric "typing.Generic" [Type.variable "typing._T"] |> Type.expression
          };
        ],
        [];
        "typing.Generic",
        [],
        [
          Define {
            Define.name = Access.create "typing.Generic.__getitem__";
            parameters = [
              { Parameter.name = Identifier.create "*args"; value = None; annotation = None}
              |> Node.create_with_default_location;
            ];
            body = [];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            generated = true;
            parent = Some (Access.create "typing.Generic");
          }
          |> Node.create_with_default_location
        ];
      ];

    {
      function_definitions;
      class_definitions;
      protocols;
      modules;
      order;
      aliases;
      globals;
      dependencies;
    }


  let copy
      {
        function_definitions;
        class_definitions;
        protocols;
        modules;
        order;
        aliases;
        globals;
        dependencies;
      } =
    {
      function_definitions = Hashtbl.copy function_definitions;
      class_definitions = Hashtbl.copy class_definitions;
      protocols = Hash_set.copy protocols;
      modules = Hashtbl.copy modules;
      order = TypeOrder.Builder.copy order;
      aliases = Hashtbl.copy aliases;
      globals = Hashtbl.copy globals;
      dependencies = Dependencies.copy dependencies;
    }


  let statistics
      {
        function_definitions;
        class_definitions;
        protocols;
        globals;
        _;
      } =
    Format.asprintf
      "Found %d functions, %d classes, %d protocols, and %d globals"
      (Hashtbl.length function_definitions)
      (Hashtbl.length class_definitions)
      (Hash_set.length protocols)
      (Hashtbl.length globals)


  let pp format { function_definitions; order; aliases; globals; _ } =
    let functions =
      Hashtbl.keys function_definitions
      |> List.map ~f:(fun access -> "  " ^  (Format.asprintf "%a" Access.pp access))
      |> String.concat ~sep:"\n" in
    let aliases =
      let alias (key, data) =
        Format.asprintf
          "  %a -> %a"
          Type.pp key
          Type.pp data in
      Hashtbl.to_alist aliases
      |> List.map ~f:alias
      |> String.concat ~sep:"\n" in
    let globals =
      let global (key, { Node.value; _ }) =
        Format.asprintf
          "  %a -> %a"
          Access.pp key
          Annotation.pp value
      in
      Hashtbl.to_alist globals
      |> List.map ~f:global
      |> String.concat ~sep:"\n" in
    Format.fprintf
      format
      "Environment:\nOrder:\n%a\nFunctions:\n%s\nAliases:\n%s\nGlobals:\n%s"
      TypeOrder.pp order
      functions
      aliases
      globals


  let show environment =
    Format.asprintf "%a" pp environment
end
