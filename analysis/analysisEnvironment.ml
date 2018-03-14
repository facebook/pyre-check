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
module Signature = AnalysisSignature
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: (Class.t Node.t) Type.Table.t;
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
  val register_global: path: string -> key: Access.t -> data:Resolution.global -> unit
  val register_type
    :  path: string
    -> Type.t
    -> Access.t
    -> (Class.t Node.t) option
    -> (Type.t * Type.t list)
  val register_alias: path: string -> key: Type.t -> data: Type.t -> unit
  val purge: File.Handle.t -> unit

  val function_definitions: Access.t -> (Define.t Node.t) list option
  val class_definition: Type.t -> (Class.t Node.t) option
  val protocols: unit -> Type.t list

  val register_module: qualifier: Access.t -> statements: Statement.t list -> unit
  val is_module: Access.t -> bool

  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: string -> string list option

  module DependencyHandler: Dependencies.Handler

  module TypeOrderHandler: TypeOrder.Handler
end

let register_type
    ~order
    ~configuration
    ~aliases
    ~add_class_definition
    ~add_class_key
    ~add_protocol
    ~register_global =
  let rec register_type ~path subtype name definition =
    let annotation =
      Type.create
        ~aliases
        (Node.create_with_default_location (Access name))
    in
    let primitive, parameters = Type.split annotation in
    let (module Handler: TypeOrder.Handler) = order in
    if Handler.contains (Handler.indices ()) subtype &&
       Handler.contains (Handler.indices ()) primitive &&
       not (Type.equal subtype primitive) then
      TypeOrder.connect
        order
        ~add_backedge:true
        ~configuration
        ~predecessor:subtype
        ~successor:primitive
        ~parameters;
    (* Register meta annotation. *)
    register_global
      ~path
      ~key:name
      ~data:{
        Resolution.annotation =
          Annotation.create_immutable
            ~global:true
            ~original:(Some Type.Top)
            (Type.meta primitive);
        location = Location.any;
      };

    (* Handle definition. *)
    begin
      match definition with
      | Some ({ Node.value = { Class.bases; _ } as definition; _ } as definition_node) ->
          add_class_key ~path primitive;
          let annotated = Annotated.Class.create definition_node in

          (* Register protocols. *)
          if Annotated.Class.is_protocol annotated then
            add_protocol primitive;

          (* Register normal annotations. *)
          add_class_definition ~primitive ~definition:definition_node;
          if List.length definition.Class.bases > 0 then
            begin
              let register_supertype name =
                let qualified_name =
                  match name.Argument.value.Node.value with
                  | Access access ->
                      let primitive, _ =
                        Type.create ~aliases name.Argument.value
                        |> Type.split in
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
                      register_type ~path annotation name None
                  | None ->
                      Type.Object, [] in
                if not (Type.equal primitive super_annotation) &&
                   not (Type.equal primitive Type.Top) then
                  (* Meta-programming can introduce cycles. *)
                  TypeOrder.connect
                    order
                    ~add_backedge:true
                    ~configuration
                    ~predecessor:primitive
                    ~successor:super_annotation
                    ~parameters
                else
                  Log.debug
                    "Trivial cycle found: %a -> %a"
                    Type.pp primitive
                    Type.pp super_annotation in
              let bases =
                let inferred_base =
                  Annotated.Class.inferred_generic_base
                    ~aliases
                    (Annotated.Class.create definition_node)
                in
                inferred_base @ bases
              in
              List.iter bases ~f:register_supertype
            end
          else if not (Type.equal primitive Type.Object) &&
                  not (Type.equal primitive Type.Top) then
            TypeOrder.connect
              order
              ~add_backedge:true
              ~configuration
              ~predecessor:primitive
              ~successor:Type.Object;
      | _ ->
          ()
    end;
    primitive, parameters
  in register_type


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
    ~configuration =
  let (module DependencyHandler: Dependencies.Handler) =
    Dependencies.handler dependencies
  in
  (module struct
    let register_definition
        ~path
        ?name_override
        ({ Node.location; value = { Define.name; _ }; _ } as definition) =
      let name = Option.value ~default:name name_override in
      DependencyHandler.add_function_key ~path name;
      let annotation =
        {
          Resolution.annotation =
            (Annotation.create_immutable ~global:true Type.Top);
          location;
        }
      in
      Hashtbl.set ~key:name ~data:annotation globals;
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


    let register_global ~path ~key ~data =
      DependencyHandler.add_global_key ~path key;
      Hashtbl.set ~key ~data globals


    let register_type =
      let add_class_definition ~primitive ~definition =
        let definition =
          match Hashtbl.find class_definitions primitive with
          | Some { Node.location; value = preexisting } ->
              {
                Node.location;
                value = Class.update preexisting ~definition:(Node.value definition);
              }
          | _ ->
              definition
        in
        Hashtbl.set class_definitions ~key:primitive ~data:definition
      in
      register_type
        ~order:(TypeOrder.handler order)
        ~configuration
        ~aliases:(Hashtbl.find aliases)
        ~add_class_definition
        ~add_class_key:DependencyHandler.add_class_key
        ~add_protocol:(Hash_set.add protocols)
        ~register_global


    let register_alias ~path ~key ~data =
      DependencyHandler.add_alias_key ~path key;
      Hashtbl.set ~key ~data aliases


    let purge handle =
      let path = File.Handle.show handle in

      let purge_table_given_keys table keys =
        List.iter ~f:(fun key -> Hashtbl.remove table key) keys
      in
      (* Dependents are handled differently from other keys, because in each other
       * instance, the path is the only one adding entries to the key. However, we can have
       *  both a.py and b.py import c.py, and thus have c.py in its keys. Therefore, when
       * purging a.py, we need to take care not to remove the c -> b dependent relationship. *)
      let purge_dependents keys =
        let remove_path dependents =
          List.filter ~f:(fun dependent -> not (String.equal dependent path)) dependents
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
      DependencyHandler.get_function_keys ~path |> purge_table_given_keys function_definitions;
      DependencyHandler.get_class_keys ~path |> purge_table_given_keys class_definitions;
      DependencyHandler.get_alias_keys ~path |> purge_table_given_keys aliases;
      DependencyHandler.get_global_keys ~path |> purge_table_given_keys globals;
      DependencyHandler.get_dependent_keys ~path |> purge_dependents;
      DependencyHandler.clear_all_keys ~path


    let function_definitions =
      Hashtbl.find function_definitions

    let class_definition =
      Hashtbl.find class_definitions

    let protocols () =
      Hash_set.to_list protocols

    let register_module ~qualifier ~statements =
      Hashtbl.set ~key:qualifier ~data:(Module.create statements) modules

    let is_module access =
      Hashtbl.mem modules access

    let in_class_definition_keys =
      Hashtbl.mem class_definitions

    let aliases =
      Hashtbl.find aliases

    let globals =
      Hashtbl.find globals

    let dependencies =
      DependencyHandler.dependents

    module TypeOrderHandler =
      (val TypeOrder.handler order: TypeOrder.Handler)

    module DependencyHandler = DependencyHandler
  end: Handler)


let resolution
    (module Handler: Handler)
    ?(annotations = Access.Map.empty)
    () =
  let parse_annotation = Type.create ~aliases:Handler.aliases in

  let instantiate
      ({ Node.location; value = { Define.parameters; return_annotation; _ } as define })
      ~constraints =
    let instantiate_aliases ?(widen = false) expression =
      expression
      >>| fun expression ->
      let annotation = parse_annotation expression in
      Type.instantiate ~widen ~constraints:(Map.find constraints) annotation
      |> Type.expression in
    let resolve_parameter parameter_node =
      let { Parameter.annotation; _ } as parameter = Node.value parameter_node in
      {
        parameter_node with
        Node.value =
          {
            parameter with
            Parameter.annotation = instantiate_aliases ~widen:true annotation
          };
      } in
    {
      Node.value = {
        define with
        Define.parameters = List.map ~f:resolve_parameter parameters;
        return_annotation = instantiate_aliases return_annotation;
      };
      location;
    }
  in

  let instantiate_signature call arguments definitions =
    let insert_implicit_arguments { Node.value = define; _ } =
      let arguments = List.map ~f:Node.value arguments in
      if Define.is_class_method define ||
         Define.is_method define ||
         (Define.is_constructor define &&
          not (Call.is_explicit_constructor_call call)) then
        let self_or_class_argument =
          Signature.Normal {
            Signature.annotation = Type.Object;
            value = Node.create_with_default_location (Access (Access.create "self_or_class"));
          }
        in
        self_or_class_argument :: arguments
      else
        arguments
    in

    let inferred_constraints parameters arguments return_annotation =
      let rec inferred_constraints parameters arguments constraints =
        match parameters, arguments with
        | { Node.value = { Parameter.annotation = Some expression; _ }; _ } :: parameters,
          (Signature.Normal { Signature.annotation = argument; value }) :: arguments ->
            let parameter = parse_annotation expression in
            let rec update_constraints constraints parameter argument =
              match parameter, argument with
              | Type.Optional parameter, Type.Optional argument ->
                  update_constraints constraints parameter argument
              | Type.Parametric { Type.parameters = [variable]; _ }, _
                when Type.is_meta parameter ->
                  (* From `typing.Type[_T]` and the actual argument extract constraint
                     `_T` -> `type(argument)`. *)
                  let annotation = parse_annotation value in
                  Some (Map.set constraints ~key:variable ~data:annotation)
              | Type.Parametric { Type.parameters = parameters; name },
                _ ->
                  let arguments =
                    let primitive, _ = Type.split parameter in
                    (* This is for performance - having Type.Bottom is common when we don't know
                       the parameters being passed into a signature, which causes a full traversal
                       of the type order. *)
                    let argument =
                      if Type.equal argument Type.Bottom then
                        Type.Object
                      else
                        argument
                    in
                    TypeOrder.instantiate_parameters
                      (module Handler.TypeOrderHandler)
                      ~source:argument ~target:primitive
                    |> Option.value ~default:[]
                  in

                  if List.length arguments = List.length parameters then
                    begin
                      let propagate constraints parameter argument =
                        constraints
                        >>= fun constraints ->
                        update_constraints constraints parameter argument
                      in
                      List.fold2_exn
                        ~init:(Some constraints)
                        ~f:propagate
                        parameters
                        arguments
                      >>| fun constraints ->
                      let arguments =
                        let parameters =
                          let keep_if_variable sofar parameter argument =
                            match parameter with
                            | Type.Variable _ -> argument::sofar
                            | _ -> parameter::sofar
                          in
                          List.fold2_exn ~init:[] ~f:keep_if_variable parameters arguments
                          |> List.rev
                        in
                        Type.Parametric { Type.parameters; name }
                      in
                      Map.set
                        ~key:parameter
                        ~data:arguments
                        constraints
                    end
                  else
                    begin
                      match parameter with
                      | Type.Parametric { Type.parameters = [variable]; _ }
                        when Type.is_meta parameter ->
                          Some (Map.set constraints ~key:variable ~data:(parse_annotation value))
                      | _ ->
                          Some constraints
                    end
              | _, Type.Top ->
                  (* Don't constrain based on unknown arguments. *)
                  Some constraints
              | Type.Variable { Type.constraints = type_constraints; _ }, _ ->
                  if type_constraints = [] ||
                     List.mem type_constraints argument ~equal:Type.equal then
                    begin
                      match Map.find constraints parameter with
                      | Some existing
                        when not (Type.equal argument existing) ->
                          (* Don't do joins. *)
                          None
                      | _ ->
                          Some argument
                    end
                    >>| fun argument ->
                    Map.set constraints ~key:parameter ~data:argument
                  else
                    Some constraints
              | Type.Union union, _ ->
                  List.fold
                    ~f:(fun constraints annotation ->
                        constraints >>=
                        (fun constraints -> update_constraints constraints annotation argument))
                    ~init:(Some constraints)
                    union
              | _ ->
                  Some constraints in
            update_constraints constraints parameter argument
            >>= inferred_constraints parameters arguments
        | _ :: parameters, _ :: arguments ->
            inferred_constraints parameters arguments constraints
        | _ ->
            Some constraints
      in
      let return_constraints constraints =
        (* Map unresolved constraints in return type to `Bottom`. This is a heuristic to deal with
           not fully resolved collection parameters, e.g. empty lists. *)
        let update_to_bottom constraints variable =
          match Map.find constraints variable with
          | Some _ -> constraints
          | None -> Map.set ~key:variable ~data:Type.Bottom constraints
        in
        return_annotation
        >>| parse_annotation
        >>= (function
            | Type.Variable _ -> None  (* Never return bottom as return type. *)
            | annotation -> Some (Type.variables annotation))
        >>| List.fold ~init:constraints ~f:update_to_bottom
        |> Option.value ~default:constraints
      in
      inferred_constraints parameters arguments Type.Map.empty
      >>| return_constraints
    in

    let sufficient_arguments_provided parameters arguments =
      let rec sufficient_arguments_provided parameters arguments =
        let is_starred_argument = function
          | Signature.Normal _ -> false
          | Signature.Starred _ -> true
        in
        let is_keyword_or_variable_argument_parameter name =
          Identifier.show name
          |> String.is_prefix ~prefix:"*"
        in
        match parameters, arguments with
        | _ :: parameters, argument :: arguments ->
            if is_starred_argument argument then
              true
            else
              sufficient_arguments_provided parameters arguments
        | { Node.value = { Parameter.name; value = None; _ }; _} :: _, []
          when not (is_keyword_or_variable_argument_parameter name) ->
            false
        | _ ->
            true
      in
      sufficient_arguments_provided parameters arguments
    in

    let sufficient_variables_resolved parameters =
      let variable_resolved { Node.value = { Parameter.annotation; _ }; _ } =
        (annotation
         >>| parse_annotation
         >>| Type.is_resolved)
        |> Option.value ~default:true
      in
      List.take parameters (List.length arguments)
      |> List.for_all ~f:variable_resolved
    in

    let instantiate_signature ({
        Node.value = { Define.parameters; return_annotation; _ };
        location;
      } as define) =
      (* Add implicit arguments. *)
      let arguments = insert_implicit_arguments define in

      (* Infer constraints. *)
      inferred_constraints parameters arguments return_annotation

      (* Apply constraints to formal parameters. *)
      >>| (fun constraints -> instantiate define ~constraints, constraints)

      (* Check additional constraints. *)
      >>= (fun ({ Node.value = { Define.parameters; _ } as instantiated; _ }, constraints) ->
          if sufficient_arguments_provided parameters arguments &&
             sufficient_variables_resolved parameters then
            Some { Signature.constraints; instantiated; location }
          else
            None)
    in
    List.filter_map ~f:instantiate_signature definitions
  in

  let function_signature qualifier call arguments =
    let name =
      begin
        match call.Expression.Call.name with
        | { Node.value = Access access; _ } ->
            qualifier @ access
        | _ ->
            []
      end
    in
    Handler.function_definitions name
    >>| instantiate_signature call arguments
    |> Option.value ~default:[]
  in

  let class_definition annotation =
    let primitive, _ = Type.split annotation in
    Handler.class_definition primitive
  in

  let method_signature ~resolution annotation call arguments =
    let definitions annotation =
      let primitive, _ = Type.split annotation in
      let name =
        match Type.expression primitive, call.Expression.Call.name with
        | { Node.value = Access qualifier; _ },
          { Node.value = Access access; _ } ->
            qualifier @ access
        | _ ->
            [] in

      (* Instantiating type variables constrained by the type. E.g. for a generic class

         class List(typing.Generic[_T]):
          def append(self, data: _T): ...

         we know that if we have an instantiated type `List[int]` the variable `_T` in `append` will
         be instantiated to `int` too. *)
      let constraints =
        (class_definition primitive
         >>| (fun definition ->
             Annotated.Class.create definition
             |> Annotated.Class.constraints ~instantiated:annotation ~resolution))
        |> Option.value ~default:Type.Map.empty
      in
      Handler.function_definitions name
      >>| List.map ~f:(instantiate ~constraints)
    in

    let successors =
      let primitive, _ = Type.split annotation in
      if Handler.in_class_definition_keys primitive then
        TypeOrder.successors (module Handler.TypeOrderHandler) annotation
      else
        []
    in
    (annotation :: successors)
    |> List.find_map ~f:definitions
    >>| instantiate_signature call arguments
    |> Option.value ~default:[]
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
    ~parse_annotation
    ~global:Handler.globals
    ~is_module:Handler.is_module
    ~class_definition
    ~function_signature
    ~method_signature


let dependencies (module Handler: Handler) =
  Handler.dependencies


let register_module (module Handler: Handler) { Source.qualifier; statements; _ } =
  let rec register_submodules = function
    | [] ->
        ()
    | (_ :: tail) as reversed ->
        let qualifier = List.rev reversed in
        if not (Handler.is_module qualifier) then
          Handler.register_module ~qualifier ~statements:[];
        register_submodules tail
  in
  Handler.register_module ~qualifier ~statements;
  if List.length qualifier > 1 then
    register_submodules (List.rev qualifier |> List.tl_exn)


let register_class_definitions (module Handler: Handler) source =
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let module Visit = Visit.Make(struct
      type t = unit

      let expression _ _ =
        ()

      let statement _ = function
        | { Node.value = Class { Class.name; _ }; _ }
        | { Node.value = Stub (Stub.Class { Class.name; _ }); _ } ->
            let primitive, _ =
              Type.create ~aliases:Handler.aliases (Node.create_with_default_location (Access name))
              |> Type.split
            in
            if not (TypeOrder.contains order primitive) then
              TypeOrder.insert order primitive;
        | _ ->
            ()
    end)
  in
  Visit.visit () source


let register_aliases (module Handler: Handler) sources =
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let collect_aliases { Source.path; statements; qualifier; _ } =
    let visit_statement aliases { Node.value; _ } =
      match value with
      | Assign {
          Assign.target;
          annotation = None;
          compound = None;
          value = Some value;
          _;
        } ->
          let value_annotation = Type.create ~aliases:Handler.aliases value in
          let target_annotation = Type.create ~aliases:Handler.aliases target in
          if not (Type.equal target_annotation Type.Top ||
                  Type.equal value_annotation Type.Top ||
                  Type.equal value_annotation target_annotation) then
            (path, target, value) :: aliases
          else
            aliases
      | Import { Import.from = Some from; imports } ->
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
    List.fold ~init:[] ~f:visit_statement statements
  in
  let rec resolve_aliases unresolved =
    if List.is_empty unresolved then
      ()
    else
      let register_alias (any_changed, unresolved) (path, target, value) =
        let target_annotation = Type.create ~aliases:Handler.aliases target in
        let value_annotation = Type.create ~aliases:Handler.aliases value in
        let rec annotation_in_order annotation =
          match annotation with
          | Type.Primitive _
          | Type.Parametric _ ->
              let primitive, _ = Type.split annotation in
              TypeOrder.contains order primitive

          | Type.Tuple (Type.Bounded annotations)
          | Type.Union annotations ->
              List.for_all ~f:annotation_in_order annotations

          | Type.Tuple (Type.Unbounded annotation) ->
              annotation_in_order annotation

          | Type.Optional annotation ->
              annotation_in_order annotation

          | Type.Object
          | Type.Variable _ ->
              true

          | Type.Bottom
          | Type.Top ->
              false
        in
        let primitive, _ = Type.split target_annotation in
        if not (TypeOrder.contains order primitive) &&
           annotation_in_order value_annotation then
          begin
            Handler.register_alias ~path ~key:target_annotation ~data:value_annotation;
            (true, unresolved)
          end
        else
          (any_changed, (path, target, value) :: unresolved)
      in
      let (any_changed, unresolved) = List.fold ~init:(false, []) ~f:register_alias unresolved in
      if any_changed then
        resolve_aliases unresolved
      else
        let show_unresolved (path, target, value) =
          Log.debug
            "Unresolved alias %s:%s <- %s"
            path
            (Expression.show target)
            (Expression.show value)
        in
        List.iter ~f:show_unresolved unresolved
  in
  List.concat_map ~f:collect_aliases sources
  |> resolve_aliases

let connect_type_order
    (module Handler: Handler)
    ?(source_root = Path.current_working_directory ())
    ?(check_dependency_exists = true)
    source =
  let path = source.Source.path in
  let parse_annotation = Type.create ~aliases:(Handler.aliases) in
  let resolution =
    resolution
      (module Handler: Handler)
      ~annotations:Access.Map.empty ()
  in

  (* Visit everything. *)
  let module Visit = Visit.Make(struct
      type t = unit

      let expression _ _ =
        ()

      let statement _ = function
        | { Node.location; value = Class definition }
        | { Node.location; value = Stub (Stub.Class definition) } ->
            (* Register constructors. *)
            let constructors =
              Annotated.Class.create (Node.create ~location definition)
              |> Annotated.Class.constructors ~resolution
            in
            List.iter
              ~f:(fun constructor ->
                  Handler.register_definition
                    ~path
                    { Node.value = constructor; location })
              constructors;

            Handler.register_type
              ~path
              Type.Bottom
              definition.Class.name
              (Some (Node.create ~location definition))
            |> ignore;

            (* Handle enumeration constants. *)
            let enumeration { Argument.value; _ } =
              match parse_annotation value with
              | Type.Primitive identifier
                when String.Set.mem
                    Recognized.enumeration_classes
                    (Identifier.show identifier) ->
                  Some (Access.create (Identifier.show identifier))
              | _ ->
                  None
            in
            List.find_map ~f:enumeration definition.Class.bases
            >>| (fun enumeration ->
                (* Register generated constructor. *)
                Handler.register_definition
                  ~path
                  {
                    Node.location;
                    value = {
                      Define.name = enumeration @ (Access.create "__init__");
                      parameters = [Parameter.create ~name:(Identifier.create "a") ()];
                      body = [];
                      decorators = [];
                      docstring = None;
                      return_annotation = Some {
                          Node.location;
                          value = Access enumeration;
                        };
                      async = false;
                      generated = true;
                      parent = Some enumeration;
                    };
                  })
            |> ignore

        | { Node.value = Define definition; location }
        | { Node.value = Stub (Stub.Define definition); location } ->
            let definition =
              Annotated.Define.apply_decorators ~resolution (Annotated.Define.create definition)
              |> Annotated.Define.define
            in
            if Define.is_method definition then
              let parent = Option.value_exn definition.Define.parent in
              Handler.register_definition
                ~path
                ~name_override:(parent @ definition.Define.name)
                { Node.value = definition; location }
            else
              Handler.register_definition ~path { Node.value = definition; location }
        | { Node.value = Import { Import.from; imports }; _ } ->
            let imports =
              let path_of_import access =
                let show_identifier = function
                  | Expression.Access.Identifier identifier ->
                      Identifier.show identifier
                  | access -> Expression.Access.show_access Expression.pp access
                in
                let relative =
                  Format.sprintf "%s.py"
                    (access
                     |> List.map ~f:show_identifier
                     |> List.fold ~init:(Path.absolute source_root) ~f:(^/))
                in
                if (not check_dependency_exists) || Sys.is_file relative = `Yes then
                  Path.create_relative ~root:source_root ~relative
                  |> Path.relative
                else
                  begin
                    Log.log ~section:`Dependencies "Import path %s not found in %s" relative path;
                    None
                  end
              in
              let import_accesses =
                match from with
                (* If analyzing from x import y, only add x to the dependencies.
                 * Otherwise, add all dependencies. *)
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
  Visit.visit () source;

  (* Visit toplevel statements. *)
  let visit = function
    | {
      Node.value = Assign {
          Assign.target;
          annotation = None;
          compound = None;
          value = Some value;
          _;
        };
      location;
    } ->
        (try
           match target.Node.value, (Resolution.resolve resolution value)
           with
           | Access access, annotation ->
               Handler.register_global
                 ~path
                 ~key:access
                 ~data:{
                   Resolution.annotation =
                     (Annotation.create_immutable
                        ~global:true
                        ~original:(Some Type.Top)
                        annotation);
                   location;
                 }
           | _ -> ()
         with _ ->
           (* TODO(T19628746): joins are not sound when building the environment. *)
           ())
    | {
      Node.value = Assign {
          Assign.target = { Node.value = Access access; _ };
          annotation = Some annotation;
          compound = None;
          _;
        };
      location;
    }
    | {
      Node.value = Stub (Stub.Assign {
          Assign.target = { Node.value = Access access; _ };
          annotation = Some annotation;
          compound = None;
          _;
        });
      location;
    } ->
        Handler.register_global
          ~path
          ~key:access
          ~data:{
            Resolution.annotation =
              (Annotation.create_immutable ~global:true (parse_annotation annotation));
            location;
          }
    | _ ->
        ()
  in
  List.iter ~f:visit source.Source.statements

let populate
    (module Handler: Handler)
    ~configuration
    ?(source_root = Path.current_working_directory ())
    ?(check_integrity = true)
    ?(check_dependency_exists = true)
    sources =

  let add_aliases aliases =
    List.iter
      ~f:(fun (path, target, value) -> Handler.register_alias ~path ~key:target ~data:value)
      aliases
  in
  add_aliases [
    "typing.py",
    Type.Primitive (Identifier.create "typing.DefaultDict"),
    Type.Primitive (Identifier.create "collections.defaultdict");
  ];

  List.iter ~f:(register_module (module Handler)) sources;
  List.iter ~f:(register_class_definitions (module Handler)) sources;
  Type.TypeCache.disable ();
  register_aliases (module Handler) sources;
  Type.TypeCache.enable ();
  List.iter ~f:(connect_type_order ~source_root ~check_dependency_exists (module Handler)) sources;
  TypeOrder.connect_annotations_to_top
    (module Handler.TypeOrderHandler)
    ~configuration
    ~bottom:Type.Bottom
    ~top:Type.Object;
  TypeOrder.remove_extra_edges
    (module Handler.TypeOrderHandler)
    ~bottom:Type.Bottom
    ~top:Type.Object;
  if check_integrity then
    TypeOrder.check_integrity (module Handler.TypeOrderHandler)


let infer_implementations (module Handler: Handler) ~protocol =
  let module Edge = TypeOrder.Edge in
  let resolution = resolution (module Handler) () in

  Resolution.class_definition resolution protocol
  >>| (fun protocol_definition ->
      let open Annotated in
      let protocol_definition = Class.create protocol_definition in
      (* Get all implementing classes. *)
      let implementations =
        let implements annotation =
          Handler.class_definition annotation
          >>| Class.create
          >>| (fun definition ->
              not (Class.is_protocol definition) &&
              Class.implements ~protocol:protocol_definition definition)
          |> Option.value ~default:false
        in
        TypeOrder.greatest (module Handler.TypeOrderHandler) ~matches:implements
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


module Builder = struct
  let create ~configuration () =
    let function_definitions = Access.Table.create () in
    let class_definitions = Type.Table.create () in
    let protocols = Type.Hash_set.create () in
    let modules = Access.Table.create () in
    let order = TypeOrder.Builder.default ~configuration () in
    let aliases = Type.Table.create () in
    let globals = Access.Table.create () in
    let dependencies = Dependencies.create () in

    (* Add classes for `typing.Optional` and `typing.Unbound` that are currently not encoded in the
       stubs. *)
    let add_special_class name =
      let definition =
        {
          Class.name = Access.create name;
          bases = [];
          body = [];
          decorators = [];
          docstring = None;
        }
      in
      Hashtbl.set
        ~key:(Type.primitive name)
        ~data:(Node.create_with_default_location definition)
        class_definitions;
    in
    List.iter ~f:add_special_class ["typing.Optional"; "typing.Unbound"];

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
      let global (key, { Resolution.annotation; _ }) =
        Format.asprintf
          "  %a -> %a"
          Access.pp key
          Annotation.pp annotation in
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
