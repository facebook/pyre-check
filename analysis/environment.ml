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
  modules: Module.t Reference.Table.t;
  order: TypeOrder.t;
  aliases: Type.t Identifier.Table.t;
  globals: Resolution.global Reference.Table.t;
  dependencies: Dependencies.t;
  undecorated_functions: (Type.t Type.Callable.overload) Reference.Table.t;
}

module type Handler = sig
  val register_dependency: handle: File.Handle.t -> dependency: Reference.t -> unit
  val register_global
    :  handle: File.Handle.t
    -> reference: Reference.t
    -> global: Resolution.global
    -> unit
  val register_undecorated_function
    :  reference: Reference.t
    -> annotation: Type.t Type.Callable.overload
    -> unit
  val set_class_definition: name: Identifier.t -> definition: Class.t Node.t -> unit
  val register_class_metadata: Identifier.t -> unit
  val register_alias: handle: File.Handle.t -> key: Identifier.t -> data: Type.t -> unit
  val purge: ?debug: bool -> File.Handle.t list -> unit

  val class_definition: ?convert: bool -> Identifier.t -> Class.t Node.t option
  val class_metadata: Identifier.t -> Resolution.class_metadata option

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
  val aliases: Identifier.t -> Type.t option
  val globals: Reference.t -> Resolution.global option
  val undecorated_signature: Reference.t -> Type.t Type.Callable.overload option
  val dependencies: Reference.t -> Reference.Set.Tree.t option

  val local_mode: File.Handle.t -> Source.mode option

  val transaction: f: (unit -> 'a) -> unit -> 'a

  module DependencyHandler: Dependencies.Handler

  module TypeOrderHandler: TypeOrder.Handler
end


module UnresolvedAlias = struct
  type t = { handle: File.Handle.t; target: Reference.t; value: Expression.expression_t }
  [@@deriving sexp, compare, hash]
end


module ResolvedAlias = struct
  type t = { handle: File.Handle.t; name: Type.primitive; annotation: Type.t }
  [@@deriving sexp, compare, hash]

  let register (module Handler: Handler) { handle; name; annotation } =
    Handler.register_alias ~handle ~key:name ~data:annotation
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
      | Type.Any, successor when not (Type.is_top successor) -> true
      | _ -> false
    in
    if annotations_tracked && not primitive_cycle && not cycle_with_top then
      TypeOrder.connect (module Handler) ~predecessor ~successor ~parameters
  in
  let primitive = Type.Primitive (Reference.show name) in
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
  (* Register normal annotations. *)
  let register_supertype { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    match Node.value value with
    | Call _
    | Name _ ->
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
           not (Type.is_top supertype) then
          Log.log
            ~section:`Environment
            "Superclass annotation %a is missing"
            Type.pp
            supertype
        else if Type.is_top supertype then
          Statistics.event
            ~name:"superclass of top"
            ~section:`Environment
            ~normals:["unresolved name", Expression.show value]
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


let handler
    {
      class_definitions;
      class_metadata;
      modules;
      order;
      aliases;
      globals;
      dependencies;
      undecorated_functions;
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


    let register_undecorated_function ~reference ~annotation =
      Hashtbl.set undecorated_functions ~key:reference ~data:annotation


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
      let is_final =
        Hashtbl.find class_definitions class_name
        >>| (fun { Node.value = definition; _ } -> Class.is_final definition)
        |> Option.value ~default:false
      in
      Hashtbl.set
        class_metadata
        ~key:class_name
        ~data:{
          is_test = in_test;
          successors;
          is_final;
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
        let qualifiers =
          List.map handles ~f:(fun handle -> Source.qualifier ~handle)
          |> Reference.Set.of_list
        in
        let remove_paths entry =
          entry
          >>| fun entry -> Set.diff entry qualifiers
        in
        List.iter
          ~f:(fun key -> Hashtbl.change dependencies.Dependencies.dependents key ~f:remove_paths)
          keys
      in
      List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
      |> purge_table_given_keys class_definitions;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_alias_keys ~handle) handles
      |> purge_table_given_keys aliases;
      let global_keys =
        List.concat_map ~f:(fun handle -> DependencyHandler.get_global_keys ~handle) handles
      in
      purge_table_given_keys globals global_keys;
      purge_table_given_keys undecorated_functions global_keys;
      List.concat_map ~f:(fun handle -> DependencyHandler.get_dependent_keys ~handle) handles
      |> purge_dependents;
      DependencyHandler.clear_keys_batch handles;
      List.map handles ~f:(fun handle -> Source.qualifier ~handle)
      |> List.iter ~f:(Hashtbl.remove modules);

      SharedMem.collect `aggressive;
      if debug then
        TypeOrder.check_integrity (TypeOrder.handler order)


    let class_definition ?(convert = false) annotation =
      match Hashtbl.find class_definitions annotation with
      | Some { Node.location; value } when convert ->
          { Node.location; value = Statement.Class value }
          |> Statement.convert
          |> (function
              | { Node.location; value = Statement.Class value } -> { Node.location; value }
              | _ -> failwith "Impossible.")
          |> Option.some
      | result ->
          result

    let class_metadata =
      Hashtbl.find class_metadata

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

    let undecorated_signature =
      Hashtbl.find undecorated_functions

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


let collect_aliases (module Handler: Handler) { Source.handle; statements; qualifier; _ } =
  let rec visit_statement ~qualifier ?(in_class_body = false) aliases { Node.value; _ } =
    match value with
    | Assign {
        Assign.target = { Node.value = Name target; _ };
        annotation;
        value;
        _;
      } when Expression.is_simple_name target ->
        let target =
          let target =
            Reference.from_name_exn target
            |> Reference.sanitize_qualified
          in
          if in_class_body then target else Reference.combine qualifier target
        in
        let target_annotation =
          Type.create ~aliases:Handler.aliases (Reference.expression target)
        in
        begin
          match Node.value value, annotation with
          | _, Some {
              Node.value =
                Call {
                  callee = {
                    Node.value = Name (Name.Attribute {
                        base = {
                          Node.value = Name (Name.Attribute {
                              base = { Node.value = Name (Name.Identifier "typing"); _ };
                              attribute = "Type";
                            });
                          _;
                        };
                        attribute = "__getitem__";
                      });
                    _;
                  };
                  arguments = [{
                      Call.Argument.value = {
                        Node.value = Call {
                            callee = {
                              Node.value = Name (Name.Attribute {
                                  base = {
                                    Node.value = Name (Name.Attribute {
                                        base = {
                                          Node.value = Name (Name.Identifier "mypy_extensions");
                                          _;
                                        };
                                        attribute = "TypedDict";
                                      });
                                    _;
                                  };
                                  attribute = "__getitem__";
                                });
                              _;
                            };
                            _;
                          };
                        _;
                      };
                      _;
                    }];
                };
              _;
            } ->
              if not (Type.is_top target_annotation) then
                { UnresolvedAlias.handle; target; value } :: aliases
              else
                aliases
          | _, Some ({
              Node.value =
                Name (Name.Attribute {
                    base = { Node.value = Name (Name.Identifier "typing"); _ };
                    attribute = "Any";
                  });
              _;
            } as value) ->
              if not (Type.is_top target_annotation) then
                { UnresolvedAlias.handle; target; value } :: aliases
              else
                aliases
          | Call _, None
          | Name _, None ->
              let value = Expression.delocalize value in
              let value_annotation = Type.create ~aliases:Handler.aliases value in
              if not (Type.is_top target_annotation ||
                      Type.is_top value_annotation ||
                      Type.equal value_annotation target_annotation) then
                { UnresolvedAlias.handle; target; value } :: aliases
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
              [{
                UnresolvedAlias.handle;
                target = qualified_name;
                value = Reference.expression original_name
              }]
        in
        List.rev_append (List.concat_map ~f:import_to_alias imports) aliases
    | _ ->
        aliases
  in
  List.fold ~init:[] ~f:(visit_statement ~qualifier) statements


let resolve_alias (module Handler: Handler) { UnresolvedAlias.handle; target; value } =
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let target_primitive_name = Reference.show target in
  let value_annotation =
    match Type.create ~aliases:Handler.aliases value with
    | Type.Variable variable ->
        Type.Variable { variable with variable = Reference.show target }
    | annotation ->
        annotation
  in
  let dependencies = String.Hash_set.create () in
  let module TrackedTransform = Type.Transform.Make(struct
      type state = unit

      let visit_children_before _ = function
        | Type.Optional Bottom -> false
        | _ -> true

      let visit_children_after = false

      let visit _ annotation =
        let new_state, transformed_annotation =
          match annotation with
          | Type.Parametric { name = primitive; _ }
          | Primitive primitive ->
              let reference =
                match Node.value (Type.expression (Type.Primitive primitive)) with
                | Expression.Name name when Expression.is_simple_name name ->
                    Reference.from_name_exn name
                | _ ->
                    Reference.create "typing.Any"
              in
              let module_definition = Handler.module_definition in
              if Module.from_empty_stub ~reference ~module_definition then
                (), Type.Any
              else if TypeOrder.contains order (Primitive primitive) then
                (), annotation
              else
                let _ = Hash_set.add dependencies primitive in
                (), annotation
          | _ ->
              (), annotation
        in
        { Type.Transform.transformed_annotation; new_state }
    end)
  in
  let _, annotation = TrackedTransform.visit () value_annotation in
  if Hash_set.is_empty dependencies then
    Result.Ok { ResolvedAlias.handle; name = target_primitive_name; annotation = annotation }
  else
    Result.Error (Hash_set.to_list dependencies)


let register_aliases (module Handler: Handler) sources =
  Type.Cache.disable ();
  let register_aliases unresolved =
    let resolution_dependency = String.Table.create () in
    let worklist = Queue.create () in
    Queue.enqueue_all worklist unresolved;

    let rec fixpoint () =
      match Queue.dequeue worklist with
      | None -> ()
      | Some unresolved ->
          let _ =
            match resolve_alias (module Handler) unresolved with
            | Result.Error dependencies ->
                let add_dependency =
                  let update_dependency = function
                    | None ->
                        [unresolved]
                    | Some entries ->
                        unresolved :: entries
                  in
                  String.Table.update resolution_dependency ~f:update_dependency
                in
                List.iter dependencies ~f:add_dependency
            | Result.Ok ({ ResolvedAlias.name; _ } as resolved) ->
                ResolvedAlias.register (module Handler) resolved;
                match Hashtbl.find resolution_dependency name with
                | Some entries ->
                    Queue.enqueue_all worklist entries;
                    Hashtbl.remove resolution_dependency name
                | None ->
                    ()
          in
          fixpoint ()
    in
    fixpoint ()
  in
  List.concat_map ~f:(collect_aliases (module Handler)) sources
  |> register_aliases;
  Type.Cache.enable ()


let register_undecorated_functions
    (module Handler: Handler)
    resolution
    source =
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let visit_children _ =
        true

      let statement _ _ statement =
        match statement with
        | {
          Node.value =
            Define ({ Define.signature = { Statement.Define.name; _ }; _ } as define);
          _;
        } ->
            if Define.is_overloaded_method define then
              ()
            else
              Handler.register_undecorated_function
                ~reference:name
                ~annotation:(Annotated.Callable.create_overload ~resolution ~define)
        | _ ->
            ()
    end)
  in
  Visit.visit () source


let register_values
    (module Handler: Handler)
    resolution
    ({ Source.handle; statements; qualifier; _ } as source) =
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
      type t = ((Type.Callable.t Node.t) list) Reference.Map.t

      let visit_children _ =
        true

      let statement { Source.handle; _ } callables statement =
        let collect_callable
            ~name
            callables
            callable =

          Handler.DependencyHandler.add_function_key ~handle name;
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
          value = Define ({ Statement.Define.signature = { name; parent; _ }; _ } as define)
        } ->
            let parent =
              if Define.is_class_method define then
                parent
                >>= fun reference -> Some (Type.Primitive (Reference.show reference))
                >>| Type.meta
              else
                None
            in
            Annotated.Callable.apply_decorators ~resolution ~define
            |> (fun overload -> [Define.is_overloaded_method define, overload])
            |> Annotated.Callable.create ~parent ~name:(Reference.show name)
            |> Node.create ~location
            |> collect_callable ~name callables

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
        Handler.register_global ~handle ~reference:key ~global)
    |> ignore
  in
  CollectCallables.visit Reference.Map.empty source
  |> Map.iteri ~f:(register_callables handle);
  (* Register meta annotations for classes. *)
  let module Visit = Visit.MakeStatementVisitor(struct
      type t = unit

      let visit_children _ =
        true

      let statement { Source.handle; _ } _ = function
        | { Node.location; value = Class { Class.name; _ } } ->
            (* Register meta annotation. *)
            let primitive = Type.Primitive (Reference.show name) in
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
          | Some ({ Node.value; _ } as annotation) ->
              let annotation =
                match value with
                | Name name when Expression.is_simple_name name ->
                    Expression.delocalize annotation
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
          | Name name, _ when Expression.is_simple_name name ->
              register ~location:target.Node.location (Reference.from_name_exn name) annotation
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
          | Tuple elements, _ ->
              List.map ~f:(fun target -> register_assign ~target ~annotation:Type.Top) elements
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
            List.iter ~f:(fun dependency -> Handler.register_dependency ~handle ~dependency) imports
        | _ ->
            ()
    end)
  in
  Visit.visit () source


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
          Handler.register_alias ~handle ~key:(Reference.show alias) ~data:(primitive full_name);
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
    let modules = Reference.Table.create () in
    let order = TypeOrder.Builder.default () in
    let aliases = Identifier.Table.create () in
    let globals = Reference.Table.create () in
    let dependencies = Dependencies.create () in
    let undecorated_functions = Reference.Table.create () in

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
        let create_argument annotation =
          {
            Expression.Call.Argument.name = None;
            value = Type.expression annotation;
          }
        in
        {
          Class.name = Reference.create name;
          bases = (List.map bases ~f:create_argument);
          body;
          decorators = [];
          docstring = None;
        }
      in
      let successors =
        let successor annotation =
          annotation
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
          is_final = false;
        }
        class_metadata;
      Hashtbl.set
        ~key:name
        ~data:(Node.create_with_default_location definition)
        class_definitions;
    in
    let t_self_expression =
      Name (Name.Identifier "TSelf")
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
          Type.parametric "typing.Generic" [Type.variable "typing._T"]
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
        [Type.parametric "typing.Mapping" [Type.string; Type.Any]],
        Type.TypedDictionary.defines ~t_self_expression ~total:true;
        "NonTotalTypedDictionary",
        [Type.Primitive "TypedDictionary"],
        Type.TypedDictionary.defines ~t_self_expression ~total:false;
      ];
    (* Register hardcoded aliases. *)
    Hashtbl.set
      aliases
      ~key:"typing.DefaultDict"
      ~data:(Type.Primitive "collections.defaultdict");
    Hashtbl.set
      aliases
      ~key:"None"
      ~data:(Type.Optional Type.Bottom);
    (* This is broken in typeshed:
       https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
    Hashtbl.set
      aliases
      ~key:"PathLike"
      ~data:(Type.Primitive "_PathLike");
    Hashtbl.set
      aliases
      ~key:"TSelf"
      ~data:(Type.variable "_PathLike");

    TypeOrder.insert
      (TypeOrder.handler order)
      (Type.Primitive "typing_extensions.Literal");


    {
      class_definitions;
      class_metadata;
      modules;
      order;
      aliases;
      globals;
      dependencies;
      undecorated_functions;
    }


  let copy
      {
        class_definitions;
        class_metadata;
        modules;
        order;
        aliases;
        globals;
        dependencies;
        undecorated_functions;
      } =
    {
      class_definitions = Hashtbl.copy class_definitions;
      class_metadata = Hashtbl.copy class_metadata;
      modules = Hashtbl.copy modules;
      order = TypeOrder.Builder.copy order;
      aliases = Hashtbl.copy aliases;
      globals = Hashtbl.copy globals;
      dependencies = Dependencies.copy dependencies;
      undecorated_functions = Hashtbl.copy undecorated_functions;
    }


  let statistics
      {
        class_definitions;
        globals;
        _;
      } =
    Format.asprintf
      "Found %d classes, and %d globals"
      (Hashtbl.length class_definitions)
      (Hashtbl.length globals)


  let pp format { order; aliases; globals; _ } =
    let aliases =
      let alias (key, data) =
        Format.asprintf
          "  %a -> %a"
          Identifier.pp key
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
