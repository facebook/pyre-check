(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


type t = {
  class_definitions: (Class.t Node.t) Identifier.Table.t;
  class_metadata: Resolution.class_metadata Identifier.Table.t;
  protocols: Identifier.Hash_set.t;
  modules: Module.t Reference.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Reference.Table.t;
  dependencies: Dependencies.t;
}

module type Handler = sig
  val register_dependency: handle: File.Handle.t -> dependency: Reference.t -> unit
  val register_global
    :  handle: File.Handle.t
    -> reference: Reference.t
    -> global: Resolution.global
    -> unit
  val set_class_definition: name: Identifier.t -> definition: Class.t Node.t -> unit
  val register_class_metadata: Identifier.t -> unit
  val register_alias: handle: File.Handle.t -> key: Type.t -> data: Type.t -> unit
  val purge: ?debug: bool -> File.Handle.t list -> unit

  val class_definition: Identifier.t -> Class.t Node.t option
  val class_metadata: Identifier.t -> Resolution.class_metadata option

  val register_protocol: handle: File.Handle.t -> Identifier.t -> unit
  val protocols: unit -> Identifier.t list

  val register_module
    :  qualifier: Reference.t
    -> local_mode: Source.mode
    -> handle: File.Handle.t option
    -> stub: bool
    -> statements: Statement.t list
    -> unit

  val is_module: Reference.t -> bool
  val module_definition: Reference.t -> Module.t option

  val in_class_definition_keys: Identifier.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Reference.t -> Resolution.global option
  val dependencies: Reference.t -> File.Handle.Set.Tree.t option

  val local_mode: File.Handle.t -> Source.mode option

  val transaction: f: (unit -> 'a) -> unit -> 'a

  module DependencyHandler: Dependencies.Handler

  module TypeOrderHandler: TypeOrder.Handler
end


let connect_definition
    ~resolution
    ~definition:({ Node.value = { Class.name; bases; _ }; _ } as definition) =
  let (module Handler: TypeOrder.Handler) = Resolution.order resolution in
  let annotated = Annotated.Class.create definition in
  (* We have to split the type here due to our built-in aliasing. Namely, the "list" and "dict"
     classes get expanded into parametric types of List[Any] and Dict[Any, Any]. *)
  let connect ~predecessor ~successor ~parameters =
    let annotations_tracked =
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
      | Type.Any, successor when not (Type.equal successor Type.Top) -> true
      | _ -> false
    in
    if annotations_tracked && not primitive_cycle && not cycle_with_top then
      TypeOrder.connect (module Handler) ~predecessor ~successor ~parameters
  in
  let primitive, _ =
    Annotated.Class.annotation ~resolution annotated
    |> Type.split
  in
  connect ~predecessor:Type.Bottom ~successor:primitive ~parameters:[];
  begin
    match Annotated.Class.inferred_callable_type annotated ~resolution with
    | Some callable ->
        connect
          ~predecessor:primitive
          ~successor:(Type.Primitive "typing.Callable")
          ~parameters:[callable]
    | None ->
        ()
  end;
  if not (Type.equal primitive Type.object_primitive) ||
     String.equal (Reference.show name) "object" then
    (* Register normal annotations. *)
    let register_supertype { Expression.Call.Argument.value; _ } =
      let value = Expression.delocalize value in
      match Node.value value with
      | Access (SimpleAccess name) ->
          let supertype, parameters =
            (* While building environment, allow untracked to parse into primitives *)
            Resolution.parse_annotation
              ~allow_untracked:true
              ~allow_invalid_type_parameters:true
              resolution
              value
            |> Type.split
          in
          if not (TypeOrder.contains (module Handler) supertype) &&
             not (Type.equal supertype Type.Top) then
            Log.log
              ~section:`Environment
              "Superclass annotation %a is missing"
              Type.pp
              supertype
          else if Type.equal supertype Type.Top then
            Statistics.event
              ~name:"superclass of top"
              ~section:`Environment
              ~normals:["unresolved name", Access.show name]
              ()
          else
            connect ~predecessor:primitive ~successor:supertype ~parameters
      | _ ->
          ()
    in
    let inferred_base = Annotated.Class.inferred_generic_base annotated ~resolution in
    inferred_base @ bases
    (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
    |> List.filter ~f:(fun { Expression.Call.Argument.name; _ } -> Option.is_none name)
    |> List.iter ~f:register_supertype
  else
    ()


let handler
    {
      class_definitions;
      class_metadata;
      protocols;
      modules;
      order;
      aliases;
      globals;
      dependencies;
    } =
  let (module DependencyHandler: Dependencies.Handler) = Dependencies.handler dependencies in

  (module struct
    module TypeOrderHandler = (val TypeOrder.handler order: TypeOrder.Handler)
    module DependencyHandler = DependencyHandler


    let register_dependency ~handle ~dependency =
      Log.log
        ~section:`Dependencies
        "Adding dependency from %a to %a"
        Reference.pp dependency
        File.Handle.pp handle;
      DependencyHandler.add_dependent ~handle dependency


    let register_global ~handle ~reference ~global =
      DependencyHandler.add_global_key ~handle reference;
      Hashtbl.set ~key:reference ~data:global globals


    let set_class_definition ~name ~definition =
      let definition =
        match Hashtbl.find class_definitions name with
        | Some { Node.location; value = preexisting } ->
            {
              Node.location;
              value = Class.update preexisting ~definition:(Node.value definition);
            };
        | _ ->
            definition
      in
      Hashtbl.set class_definitions ~key:name ~data:definition


    let register_class_metadata class_name =
      let successors = TypeOrder.successors (module TypeOrderHandler) class_name in
      let in_test =
        let is_unit_test { Node.value = definition; _ } =
          Class.is_unit_test definition
        in
        List.filter_map successors ~f:(Hashtbl.find class_definitions)
        |> List.exists ~f:is_unit_test
      in
      Hashtbl.set
        class_metadata
        ~key:class_name
        ~data:{
          is_test = in_test;
          successors;
        }


    let register_alias ~handle ~key ~data =
      DependencyHandler.add_alias_key ~handle key;
      Hashtbl.set ~key ~data aliases


    let purge ?(debug = false) handles =
      let purge_table_given_keys table keys =
        List.iter ~f:(fun key -> Hashtbl.remove table key) keys
      in
      (* Dependents are handled differently from other keys, because in each other
       * instance, the path is the only one adding entries to the key. However, we can have
       *  both a.py and b.py import c.py, and thus have c.py in its keys. Therefore, when
       * purging a.py, we need to take care not to remove the c -> b dependent relationship. *)
      let purge_dependents keys =
        let remove_paths entry =
          match entry with
          | Some set ->
              Set.filter
                ~f:(fun dependent -> not (List.mem handles dependent ~equal:File.Handle.equal))
                set
              |> Option.some
          | None ->
              None
        in
        List.iter
          ~f:(fun key -> Hashtbl.change dependencies.Dependencies.dependents key ~f:remove_paths)
          keys
      in
      List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
      |> purge_table_given_keys class_definitions;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_alias_keys ~handle) handles
      |> purge_table_given_keys aliases;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_global_keys ~handle) handles
      |> purge_table_given_keys globals;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_dependent_keys ~handle) handles
      |> purge_dependents;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_protocol_keys ~handle) handles
      |> List.iter ~f:(Hash_set.remove protocols);
      DependencyHandler.clear_keys_batch handles;
      List.map handles ~f:(fun handle -> Source.qualifier ~handle)
      |> List.iter ~f:(Hashtbl.remove modules);

      SharedMem.collect `aggressive;
      if debug then
        TypeOrder.check_integrity (TypeOrder.handler order)


    let class_definition =
      Hashtbl.find class_definitions

    let class_metadata =
      Hashtbl.find class_metadata

    let register_protocol ~handle protocol =
      DependencyHandler.add_protocol_key ~handle protocol;
      Hash_set.add protocols protocol

    let protocols () =
      Hash_set.to_list protocols

    let register_module ~qualifier ~local_mode ~handle ~stub ~statements =
      let is_registered_empty_stub =
        Hashtbl.find modules qualifier
        >>| Module.empty_stub
        |> Option.value ~default:false
      in

      let string =
        Annotation.create_immutable ~global:true Type.string
        |> Node.create_with_default_location
      in
      let global_key = Reference.create ~prefix:qualifier in
      Hashtbl.set globals ~key:(global_key "__file__") ~data:string;
      Hashtbl.set globals ~key:(global_key "__name__") ~data:string;
      let dictionary_annotation =
        Type.dictionary ~key:Type.string ~value:Type.Any
        |> Annotation.create_immutable ~global:true
        |> Node.create_with_default_location
      in
      Hashtbl.set globals
        ~key:(global_key "__dict__")
        ~data:dictionary_annotation;

      if not is_registered_empty_stub then
        Hashtbl.set
          ~key:qualifier
          ~data:(
            Module.create
              ~qualifier
              ~local_mode
              ?handle
              ~stub
              statements)
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

    let local_mode _ =
      None

    let transaction ~f () = f ()
  end: Handler)


let dependencies (module Handler: Handler) =
  Handler.dependencies


let register_module
    (module Handler: Handler)
    {
      Source.qualifier;
      handle;
      statements;
      metadata = { Source.Metadata.local_mode; _ };
      _;
    } =
  let rec register_submodules = function
    | None ->
        ()
    | Some qualifier ->
        if not (Handler.is_module qualifier) then
          Handler.register_module
            ~handle:None
            ~qualifier
            ~local_mode
            ~stub:false
            ~statements:[];
        register_submodules (Reference.prefix qualifier)
  in
  Handler.register_module
    ~qualifier
    ~local_mode
    ~handle:(Some handle)
    ~stub:(File.Handle.is_stub handle)
    ~statements;
  if Reference.length qualifier > 1 then
    Reference.prefix qualifier
    |> register_submodules


let register_class_definitions (module Handler: Handler) source =
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = Type.Set.t

      let visit_children _ =
        true

      let statement { Source.handle; _ } new_annotations = function
        | { Node.location; value = Class ({ Class.name; _ } as definition); } ->
            let name = Reference.show name in
            let primitive = Type.Primitive name in
            Handler.DependencyHandler.add_class_key ~handle name;
            let annotated = Annotated.Class.create { Node.location; value = definition } in

            if Annotated.Class.is_protocol annotated then
              begin
                Handler.register_protocol ~handle name
              end;

            Handler.set_class_definition
              ~name
              ~definition:{ Node.location; value = definition };
            if not (TypeOrder.contains order primitive) then
              TypeOrder.insert order primitive;
            Set.add new_annotations primitive
        | _ ->
            new_annotations
    end)
  in
  Visit.visit Type.Set.empty source


let register_aliases (module Handler: Handler) sources =
  Type.Cache.disable ();
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let collect_aliases { Source.handle; statements; qualifier; _ } =
    let rec visit_statement ~qualifier ?(in_class_body = false) aliases { Node.value; _ } =
      match value with
      | Assign {
          Assign.target = { Node.value = Access (SimpleAccess target); _ };
          annotation;
          value;
          _;
        } ->
          let target =
            let access = Access.delocalize_qualified target in
            if in_class_body then
              access
            else
              Reference.access qualifier @ access
          in
          let target_annotation =
            Type.create
              ~aliases:Handler.aliases
              (Node.create (Access (SimpleAccess target)) ~location:(Node.location value))
          in
          begin
            match Node.value value, annotation with
            | _, Some {
                Node.value =
                  Access
                    (SimpleAccess [
                        Access.Identifier "typing";
                        Access.Identifier "Type";
                        Access.Identifier "__getitem__";
                        Access.Call {
                          Node.value = [{
                              Argument.value = ({
                                  Node.value = Access (SimpleAccess [
                                      Access.Identifier "mypy_extensions";
                                      Access.Identifier "TypedDict";
                                      Access.Identifier "__getitem__";
                                      Access.Call _;
                                    ]);
                                  _;
                                } as annotation);
                              _;
                            }];
                          _;
                        };
                      ]);
                _;
              } ->
                if not (Type.equal target_annotation Type.Top) then
                  (handle, Reference.from_access target, annotation) :: aliases
                else
                  aliases
            | _, Some ({
                Node.value =
                  Access
                    (SimpleAccess[
                        Access.Identifier "typing";
                        Access.Identifier "Any";
                      ]);
                _;
              } as annotation) ->
                if not (Type.equal target_annotation Type.Top) then
                  (handle, Reference.from_access target, annotation) :: aliases
                else
                  aliases
            | Access _, None ->
                let value = Expression.delocalize value in
                let value_annotation = Type.create ~aliases:Handler.aliases value in
                if not (Type.equal target_annotation Type.Top ||
                        Type.equal value_annotation Type.Top ||
                        Type.equal value_annotation target_annotation) then
                  (handle, Reference.from_access target, value) :: aliases
                else
                  aliases
            | _ ->
                aliases
          end
      | Class { Class.name; body; _ } ->
          List.fold
            body
            ~init:aliases
            ~f:(visit_statement ~qualifier:name ~in_class_body:true)
      | Import { Import.from = Some _; imports = [{ Import.name; _ }] }
        when Reference.show name = "*" ->
          (* Don't register x.* as an alias when a user writes `from x import *`. *)
          aliases
      | Import {
          Import.from = Some from;
          imports;
        } ->
          let from =
            match Reference.show from with
            | "future.builtins"
            | "builtins" -> Reference.empty
            | _ -> from
          in
          let import_to_alias { Import.name; alias } =
            let qualified_name =
              match alias with
              | None -> Reference.combine qualifier name
              | Some alias -> Reference.combine qualifier alias
            in
            let original_name = Reference.combine from name in
            match Reference.as_list qualified_name, Reference.as_list original_name with
            | single_identifier :: [], typing :: [identifier]
              when String.equal typing "typing" && String.equal single_identifier identifier ->
                (* builtins has a bare qualifier. Don't export bare aliases from typing. *)
                []
            | _ ->
                [handle, qualified_name, Reference.expression original_name]
          in
          List.rev_append (List.concat_map ~f:import_to_alias imports) aliases
      | _ ->
          aliases
    in
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements
  in
  let register_alias (any_changed, unresolved) (handle, target, value) =
    let target_annotation =
      Type.create
        (Reference.expression ~location:(Node.location value) target)
        ~aliases:Handler.aliases
    in
    let value_annotation =
      match Type.create ~aliases:Handler.aliases value with
      | Type.Variable variable ->
          Type.Variable { variable with variable = Reference.show target }
      | annotation ->
          annotation
    in
    let target_primitive, _ = Type.split target_annotation in
    let module TrackedTransform = Type.Transform.Make(struct
        type state = bool

        let visit_children_before _ = function
          | Type.Optional Bottom -> false
          | _ -> true

        let visit_children_after = false

        let visit sofar annotation =
          let new_state, transformed_annotation =
            match annotation with
            | Type.Parametric { name = primitive; _ }
            | Primitive primitive ->
                let reference =
                  match Node.value (Type.expression (Type.Primitive primitive)) with
                  | Expression.Access (SimpleAccess name) ->
                      Reference.from_access name
                  | _ ->
                      Reference.create "typing.Any"
                in
                let module_definition = Handler.module_definition in
                if Module.from_empty_stub ~reference ~module_definition then
                  sofar, Type.Any
                else if TypeOrder.contains order (Primitive primitive) then
                  sofar, annotation
                else
                  false, annotation
            | Bottom
            | Top ->
                false, annotation
            | _ ->
                sofar, annotation
          in
          { Type.Transform.transformed_annotation; new_state }
      end)
    in
    let all_valid, value_annotation = TrackedTransform.visit true value_annotation in
    if all_valid && not (TypeOrder.contains order target_primitive) then
      begin
        Handler.register_alias ~handle ~key:target_annotation ~data:value_annotation;
        (true, unresolved)
      end
    else
      (any_changed, (handle, target, value) :: unresolved)
  in
  let rec resolve_aliases unresolved =
    if List.is_empty unresolved then
      ()
    else
      let (any_changed, unresolved) = List.fold ~init:(false, []) ~f:register_alias unresolved in
      if any_changed then
        resolve_aliases unresolved
      else
        let show_unresolved (handle, target, value) =
          Log.debug
            "Unresolved alias %a:%a <- %a"
            File.Handle.pp handle
            Reference.pp target
            Expression.pp value
        in
        List.iter ~f:show_unresolved unresolved
  in
  List.concat_map ~f:collect_aliases sources
  |> resolve_aliases;
  Type.Cache.enable ()


let register_values
    (module Handler: Handler)
    resolution
    ({ Source.handle; qualifier; statements; _ } as source) =
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

  let module CollectCallables = Visit.MakeStatementVisitor(struct
      type t = ((Type.Callable.t Node.t) list) Access.Map.t

      let visit_children _ =
        true

      let statement { Source.handle; _ } callables statement =
        let collect_callable
            ~parent
            ~location
            callables
            ({ Define.signature = { name; _ }; _ } as define) =

          Handler.DependencyHandler.add_function_key ~handle name;

          (* Register callable global. *)
          let callable =
            (* Only omit `self` for class methods in the environment. When accessed globally,
               Instance methods require the calling class to be passed in. *)
            let parent =
              if Define.is_class_method define then
                parent
                >>| Resolution.parse_reference ~allow_untracked:true resolution
                >>| Type.meta
              else
                None
            in
            Annotated.Callable.create ~parent [define] ~resolution
            |> Node.create ~location
          in
          let change callable = function
            | None -> Some [callable]
            | Some existing -> Some (existing @ [callable])
          in
          Map.change callables (Reference.access name) ~f:(change callable)
        in
        match statement with
        | {
          Node.location;
          value = Define ({ Statement.Define.signature = { parent; _ }; _ } as define)
        } ->
            Annotated.Callable.apply_decorators ~resolution ~define
            |> collect_callable ~parent ~location callables

        | _ ->
            callables
    end)
  in

  let register_callables handle ~key ~data =
    assert (not (List.is_empty data));
    let location =
      List.hd_exn data
      |> Node.location
    in
    data
    |> List.map ~f:Node.value
    |> Type.Callable.from_overloads
    >>| (fun callable -> Type.Callable callable)
    >>| Annotation.create_immutable ~global:true
    >>| Node.create ~location
    >>| (fun global ->
        Handler.register_global ~handle ~reference:(Reference.from_access key) ~global)
    |> ignore
  in
  CollectCallables.visit Access.Map.empty source
  |> Map.iteri ~f:(register_callables handle);
  (* Register meta annotations for classes. *)
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let visit_children _ =
        true

      let statement { Source.handle; _ } _ = function
        | { Node.location; value = Class { Class.name; _ } } ->
            (* Register meta annotation. *)
            let primitive, _ =
              Resolution.parse_reference ~allow_untracked:true resolution name
              |> Type.split
            in
            let global =
              Annotation.create_immutable
                ~global:true
                ~original:(Some Type.Top)
                (Type.meta primitive)
              |> Node.create ~location
            in
            Handler.register_global
              ~handle
              ~reference:(qualified_reference name)
              ~global
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
    | { Node.value = Assign { Assign.target; annotation; value; _ }; _ } ->
        let annotation, explicit =
          match annotation with
          | Some ({ Node.location; value } as annotation) ->
              let annotation =
                match value with
                | Access (SimpleAccess access) ->
                    (* Local names don't make sense when used globally. *)
                    Access.delocalize access
                    |> Access.expression ~location
                | _ ->
                    annotation
              in
              Type.create ~aliases:Handler.aliases annotation, true
          | None ->
              let annotation =
                try
                  Resolution.resolve_literal resolution value
                with _ ->
                  Type.Top
              in
              annotation, false
        in
        let rec register_assign ~target ~annotation =
          let register ~location reference annotation =
            let reference = qualified_reference (Reference.combine qualifier reference) in
            (* Don't register attributes or chained accesses as globals *)
            if Reference.length (Reference.drop_prefix ~prefix:qualifier reference) = 1 then
              let register_global global =
                Node.create ~location global
                |> (fun global -> Handler.register_global ~handle ~reference ~global)
              in
              let exists = Option.is_some (Handler.globals reference) in
              if explicit then
                Annotation.create_immutable ~global:true annotation
                |> register_global
              else if not exists then
                (* Treat literal globals as having been explicitly annotated. *)
                let original =
                  if Type.is_partially_typed annotation then (Some Type.Top) else None
                in
                Annotation.create_immutable ~global:true ~original annotation
                |> register_global
              else
                ()
          in
          match target.Node.value, annotation with
          | Access (SimpleAccess access), _ ->
              register ~location:target.Node.location (Reference.from_access access) annotation
          | Tuple elements, Type.Tuple (Type.Bounded parameters)
            when List.length elements = List.length parameters ->
              List.map2_exn
                ~f:(fun target annotation -> register_assign ~target ~annotation)
                elements
                parameters
              |> ignore
          | Tuple elements, Type.Tuple (Type.Unbounded parameter) ->
              List.map ~f:(fun target -> register_assign ~target ~annotation:parameter) elements
              |> ignore
          | _ ->
              ()
        in
        register_assign ~target ~annotation
    | _ ->
        ()
  in
  List.iter ~f:visit statements


let connect_type_order (module Handler: Handler) resolution source =
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let visit_children _ =
        true

      let statement _ _ = function
        | { Node.location; value = Class definition } ->
            connect_definition
              ~resolution
              ~definition:(Node.create ~location definition)
        | _ ->
            ()
    end)
  in
  Visit.visit () source
  |> ignore


let register_dependencies (module Handler: Handler) source =
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let visit_children _ =
        true

      let statement { Source.handle; _ } _ = function
        | { Node.value = Import { Import.from; imports }; _ } ->
            let imports =
              let imports =
                match from with
                (* If analyzing from x import y, only add x to the dependencies.
                   Otherwise, add all dependencies. *)
                | None ->
                    imports
                    |> List.map ~f:(fun { Import.name; _ } -> name)
                | Some base_module -> [base_module]
              in
              let qualify_builtins import =
                match Reference.single import with
                | Some "builtins" -> Reference.empty
                | _ -> import
              in
              List.map imports ~f:qualify_builtins
            in
            List.iter
              ~f:(fun dependency -> Handler.register_dependency ~handle ~dependency)
              imports
        | _ ->
            ()
    end)
  in
  Visit.visit () source


let infer_implementations (module Handler: Handler) resolution ~implementing_classes ~protocol =
  let module Edge = TypeOrder.Edge in
  let open Annotated in

  Resolution.class_definition resolution protocol
  >>| (fun protocol_definition ->
      let protocol_definition = Class.create protocol_definition in
      let implementations =
        let classes_to_analyze =
          (* Get all implementing classes. *)
          let names =
            Class.methods protocol_definition ~resolution
            |> List.map ~f:Class.Method.name
          in
          if List.is_empty names then
            let annotations = Handler.TypeOrderHandler.annotations () in
            Handler.TypeOrderHandler.keys ()
            |> List.filter_map ~f:(Handler.TypeOrderHandler.find annotations)
          else
            let get_implementing_methods method_name =
              implementing_classes ~method_name
              |> Option.value ~default:[]
              |> List.map ~f:(fun class_name -> Type.Primitive (Reference.show class_name))
            in
            List.concat_map ~f:get_implementing_methods names
            |> List.dedup_and_sort ~compare:Type.compare
        in
        let validate definition =
          (* TODO(T40726328): temporary workaround until proxying implemented *)
          Option.some_if
            (
              not (Class.is_protocol definition) ||
              (
                Reference.equal
                  (Class.name protocol_definition) (Reference.create "typing.Sized") &&
                (
                  Reference.equal
                    (Class.name definition) (Reference.create "typing._Collection") ||
                  Reference.equal
                    (Class.name definition) (Reference.create "typing.Collection")
                )
              )
            )
            definition
        in
        let implements (annotation: Type.t) =
          annotation
          |> Type.primitive_name
          >>= Handler.class_definition
          >>| Class.create
          >>= validate
          >>| Class.implements ~resolution ~protocol:protocol_definition
          >>= function
          | TypeOrder.Implements { parameters } -> Some (annotation, parameters)
          | DoesNotImplement -> None
        in
        List.filter_map ~f:implements classes_to_analyze
      in

      (* Get edges to protocol. *)
      let edges =
        let add_edge sofar (source, parameters) =
          (* Even if `object` technically implements a protocol, do not add cyclic edge. *)
          if Type.equal source protocol || Type.equal source Type.object_primitive then
            sofar
          else
            Set.add sofar { Edge.source; target = protocol; parameters }
        in
        List.fold ~init:Edge.Set.empty ~f:add_edge implementations
      in
      Log.log
        ~section:`Protocols
        "Found implementations for protocol %a: %s"
        Type.pp protocol
        (List.map ~f:fst implementations |> List.map ~f:Type.show |> String.concat ~sep:", ");
      edges)
  |> Option.value ~default:Edge.Set.empty


let infer_protocol_edges
    ~handler:((module Handler: Handler) as handler)
    resolution
    ~classes_to_infer =
  let module Edge = TypeOrder.Edge in
  Log.info "Inferring protocol implementations...";
  let protocols =
    (* Skip useless protocols for better performance. *)
    let skip_protocol protocol =
      match Handler.class_definition protocol with
      | Some protocol_definition ->
          let protocol_definition = Annotated.Class.create protocol_definition in
          List.is_empty (Annotated.Class.methods protocol_definition ~resolution)
      | _ ->
          true
    in
    List.filter ~f:(fun protocol -> not (skip_protocol protocol)) (Handler.protocols ())
  in
  let implementing_classes =
    (* TODO(T40727281): This ignores transitive methods, for perf reasons *)
    let methods_to_implementing_classes =
      let open Statement in
      let protocol_methods =
        let names_of_methods protocol =
          protocol
          |> Handler.class_definition
          >>| Annotated.Class.create
          >>| Annotated.Class.methods ~resolution
          >>| List.map ~f:Annotated.Class.Method.name
          |> Option.value ~default:[]
        in
        List.concat_map ~f:names_of_methods protocols
        |> Identifier.Set.of_list
      in
      let annotations = Handler.TypeOrderHandler.annotations () in
      let add_type_methods methods_to_implementing_classes index =
        let class_definition =
          Handler.TypeOrderHandler.find annotations index
          >>= Type.primitive_name
          >>= Handler.class_definition
        in
        match class_definition with
        | None ->
            methods_to_implementing_classes
        | Some { Node.value = { Class.name = class_name; body; _ }; _ } ->
            (* TODO(T30499509): Rely on existing class defines instead of repeating work. *)
            let add_method methods_to_implementing_classes { Node.value = statement; _ } =
              match statement with
              | Define define ->
                  let method_name = Define.unqualified_name define in
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
      List.fold classes_to_infer ~f:add_type_methods ~init:Identifier.Map.empty
    in
    fun ~method_name -> Map.find methods_to_implementing_classes method_name
  in
  let add_protocol_edges edges protocol =
    let protocol = Type.Primitive protocol in
    infer_implementations handler resolution ~implementing_classes ~protocol
    |> Set.union edges
  in
  List.fold ~init:TypeOrder.Edge.Set.empty ~f:add_protocol_edges protocols


let infer_protocols
    ?classes_to_infer
    ~handler:((module Handler: Handler) as handler)
    resolution
    () =
  let timer = Timer.start () in
  let classes_to_infer =
    match classes_to_infer with
    | Some classes ->
        let f name =
          Handler.TypeOrderHandler.find
            (Handler.TypeOrderHandler.indices ())
            (Type.Primitive name)
        in
        List.filter_map classes ~f
    | None ->
        Handler.TypeOrderHandler.keys ()
  in
  infer_protocol_edges ~handler resolution ~classes_to_infer
  |> Set.iter ~f:(fun { TypeOrder.Edge.source; target; parameters } ->
      TypeOrder.connect
        (module Handler.TypeOrderHandler)
        ~parameters
        ~predecessor:source
        ~successor:target);

  Statistics.performance ~name:"inferred protocol implementations" ~timer ()


let propagate_nested_classes (module Handler: Handler) resolution annotation =
  let propagate
      { Node.location; value = { Statement.Class.name; _ } as definition }
      successors
    =
    let handle =
      Location.instantiate ~lookup:(fun hash -> SharedMemory.Handles.get ~hash) location
      |> fun { Location.path; _ } -> File.Handle.create path
    in
    let nested_class_names { Statement.Class.name; body; _ } =
      let extract_classes = function
        | { Node.value = Class { name = nested_name; _ }; _ } ->
            Some (Reference.drop_prefix nested_name ~prefix:name, nested_name)
        | _ ->
            None
      in
      List.filter_map ~f:extract_classes body
    in
    let create_alias added_sofar (stripped_name, full_name) =
      let alias = Reference.combine name stripped_name in
      if List.exists added_sofar ~f:(Reference.equal alias) then
        added_sofar
      else
        begin
          let primitive name = Type.Primitive (Reference.show name) in
          Handler.register_alias ~handle ~key:(primitive alias) ~data:(primitive full_name);
          alias :: added_sofar
        end
    in
    let own_nested_classes =
      nested_class_names definition
      |> List.map ~f:snd
    in
    successors
    |> List.filter_map ~f:(fun name -> Resolution.class_definition resolution (Type.Primitive name))
    |> List.map ~f:Node.value
    |> List.concat_map ~f:nested_class_names
    |> List.fold ~f:create_alias ~init:own_nested_classes
  in
  (annotation
   |> Type.primitive_name
   >>= fun name -> Handler.class_definition name
   >>= fun definition -> Handler.class_metadata name
   >>| (fun { Resolution.successors; _ } -> propagate definition successors))
  |> ignore


let built_in_annotations =
  [ Type.Primitive "TypedDictionary"; Type.Primitive "NonTotalTypedDictionary" ]
  |> Type.Set.of_list


module Builder = struct
  let create () =
    let class_definitions = Identifier.Table.create () in
    let class_metadata = Identifier.Table.create () in
    let protocols = Identifier.Hash_set.create () in
    let modules = Reference.Table.create () in
    let order = TypeOrder.Builder.default () in
    let aliases = Type.Table.create () in
    let globals = Reference.Table.create () in
    let dependencies = Dependencies.create () in

    (* Register dummy module for `builtins` and `future.builtins`. *)
    let builtins = Reference.create "builtins" in
    Hashtbl.set
      modules
      ~key:builtins
      ~data:(
        Ast.Module.create
          ~qualifier:builtins
          ~local_mode:Ast.Source.PlaceholderStub
          ~stub:true
          []);
    let future_builtins = Reference.create "future.builtins" in
    Hashtbl.set
      modules
      ~key:future_builtins
      ~data:(
        Ast.Module.create
          ~qualifier:future_builtins
          ~local_mode:Ast.Source.PlaceholderStub
          ~stub:true
          []);

    (* Add `None` constant to globals. *)
    let annotation annotation =
      Annotation.create_immutable ~global:true annotation
      |> Node.create_with_default_location
    in
    Hashtbl.set globals ~key:(Reference.create "None") ~data:(annotation Type.none);
    Hashtbl.set globals
      ~key:(Reference.create "...")
      ~data:(annotation Type.Any);

    (* Add classes for `typing.Optional` and `typing.Undeclared` that are currently not encoded
       in the stubs. *)
    let add_special_class ~name ~bases ~body =
      let definition =
        {
          Class.name = Reference.create name;
          bases;
          body;
          decorators = [];
          docstring = None;
        }
      in
      let successors =
        let successor { Expression.Call.Argument.value; _ } =
          Type.create value ~aliases:(fun _ -> None)
          |> Type.split
          |> fst
          |> Type.primitive_name
        in
        (List.filter_map bases ~f:successor) @ ["object"]
      in
      Hashtbl.set
        ~key:name
        ~data:{
          Resolution.successors;
          is_test = false;
        }
        class_metadata;
      Hashtbl.set
        ~key:name
        ~data:(Node.create_with_default_location definition)
        class_definitions;
    in
    let t_self_expression =
      Access (SimpleAccess ([Identifier "TSelf"]))
      |> Node.create_with_default_location
    in
    List.iter
      ~f:(fun (name, bases, body) -> add_special_class ~name ~bases ~body)
      [
        "None", [], [];
        "typing.Optional", [], [];
        "typing.Undeclared", [], [];
        "typing.NoReturn", [], [];
        "typing.Type",
        [
          {
            Expression.Call.Argument.name = None;
            value = Type.parametric "typing.Generic" [Type.variable "typing._T"] |> Type.expression
          };
        ],
        [];
        "typing.Generic",
        [],
        [
          Define {
            signature = {
              name = Reference.create "typing.Generic.__getitem__";
              parameters = [
                { Parameter.name = "*args"; value = None; annotation = None}
                |> Node.create_with_default_location;
              ];
              decorators = [];
              docstring = None;
              return_annotation = None;
              async = false;
              parent = Some (Reference.create "typing.Generic");
            };
            body = [];
          }
          |> Node.create_with_default_location
        ];
        "TypedDictionary",
        [
          {
            Expression.Call.Argument.name = None;
            value =
              (Type.parametric "typing.Mapping" [Type.string; Type.Any])
              |> Type.expression
          };
        ],
        Type.TypedDictionary.defines ~t_self_expression ~total:true;
        "NonTotalTypedDictionary",
        [
          {
            Expression.Call.Argument.name = None;
            value =
              (Type.Primitive "TypedDictionary")
              |> Type.expression
          };
        ],
        Type.TypedDictionary.defines ~t_self_expression ~total:false;
      ];
    (* Register hardcoded aliases. *)
    Hashtbl.set
      aliases
      ~key:(Type.Primitive "typing.DefaultDict")
      ~data:(Type.Primitive "collections.defaultdict");
    Hashtbl.set
      aliases
      ~key:(Type.Primitive "None")
      ~data:(Type.Optional Type.Bottom);
    (* This is broken in typeshed:
       https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
    Hashtbl.set
      aliases
      ~key:(Type.Primitive "PathLike")
      ~data:(Type.Primitive "_PathLike");
    Hashtbl.set
      aliases
      ~key:(Type.Primitive "TSelf")
      ~data:(Type.variable "_PathLike");

    TypeOrder.insert
      (TypeOrder.handler order)
      (Type.Primitive "typing_extensions.Literal");


    {
      class_definitions;
      class_metadata;
      protocols;
      modules;
      order;
      aliases;
      globals;
      dependencies;
    }


  let copy
      {
        class_definitions;
        class_metadata;
        protocols;
        modules;
        order;
        aliases;
        globals;
        dependencies;
      } =
    {
      class_definitions = Hashtbl.copy class_definitions;
      class_metadata = Hashtbl.copy class_metadata;
      protocols = Hash_set.copy protocols;
      modules = Hashtbl.copy modules;
      order = TypeOrder.Builder.copy order;
      aliases = Hashtbl.copy aliases;
      globals = Hashtbl.copy globals;
      dependencies = Dependencies.copy dependencies;
    }


  let statistics
      {
        class_definitions;
        protocols;
        globals;
        _;
      } =
    Format.asprintf
      "Found %d classes, %d protocols, and %d globals"
      (Hashtbl.length class_definitions)
      (Hash_set.length protocols)
      (Hashtbl.length globals)


  let pp format { order; aliases; globals; _ } =
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
          Reference.pp key
          Annotation.pp value
      in
      Hashtbl.to_alist globals
      |> List.map ~f:global
      |> String.concat ~sep:"\n" in
    Format.fprintf
      format
      "Environment:\nOrder:\n%a\nAliases:\n%s\nGlobals:\n%s"
      TypeOrder.pp order
      aliases
      globals


  let show environment =
    Format.asprintf "%a" pp environment
end
