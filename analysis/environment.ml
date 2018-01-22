(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

type t = {
  function_definitions: ((Define.t Node.t) list) Access.Table.t;
  class_definitions: (Class.t Node.t) Type.Table.t;
  protocols: Type.Hash_set.t;
  order: TypeOrder.t;
  aliases: Type.t Type.Table.t;
  globals: Resolution.global Access.Table.t;
  dependencies: Dependencies.t;
}

module type Reader = sig
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
  val protocols: Type.Hash_set.t
  val in_class_definition_keys: Type.t -> bool
  val aliases: Type.t -> Type.t option
  val globals: Access.t -> Resolution.global option
  val dependencies: string -> string list option

  module DependencyReader: Dependencies.Reader

  module TypeOrderReader: TypeOrder.Reader
end

let register_type
    ~order
    ~aliases
    ~add_class_definition
    ~add_class_key
    ~add_protocol
    ~register_global =
  let rec register_type ~path subtype name definition =
    let annotation =
      Type.create
        ~aliases
        (Node.create (Access name))
    in
    let primitive, parameters = Type.split annotation in
    let (module Reader: TypeOrder.Reader) = order in
    if Reader.contains (Reader.indices ()) subtype &&
       Reader.contains (Reader.indices ()) primitive &&
       not (Type.equal subtype primitive) then
      TypeOrder.connect order ~predecessor:subtype ~successor:primitive ~parameters;
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
      | Some ({ Node.value = definition; _ } as definition_node) ->
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
                            "Inserting missing annotation %a"
                            Type.pp
                            primitive;
                          TypeOrder.insert order primitive
                        end;
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
                    ~predecessor:primitive
                    ~successor:super_annotation
                    ~parameters
                else
                  Log.debug
                    "Trivial cycle found: %a -> %a"
                    Type.pp primitive
                    Type.pp super_annotation in
              List.iter definition.Class.bases ~f:register_supertype
            end
          else if not (Type.equal primitive Type.Object) &&
                  not (Type.equal primitive Type.Top) then
            TypeOrder.connect
              order
              ~predecessor:primitive
              ~successor:Type.Object;
      | _ ->
          ()
    end;
    primitive, parameters
  in register_type


let reader
    {
      function_definitions;
      class_definitions;
      protocols;
      order;
      aliases;
      globals;
      dependencies;
    } =
  let (module DependencyReader: Dependencies.Reader) =
    Dependencies.reader dependencies
  in
  (module struct
    let register_definition
        ~path
        ?name_override
        ({ Node.value = { Define.name; _ }; _ } as definition) =
      let name = Option.value ~default:name name_override in
      DependencyReader.add_function_key ~path name;
      let definitions =
        match Hashtbl.find function_definitions name with
        | Some definitions ->
            definition::definitions
        | None ->
            [definition] in
      Hashtbl.set function_definitions ~key:name ~data:definitions


    let register_dependency ~path ~dependency =
      Log.log
        ~section:`Dependencies
        "Adding dependency from %s to %s"
        dependency
        path;
      DependencyReader.add_dependent ~path dependency


    let register_global ~path ~key ~data =
      DependencyReader.add_global_key ~path key;
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
        ~order:(TypeOrder.reader order)
        ~aliases:(Hashtbl.find aliases)
        ~add_class_definition
        ~add_class_key:DependencyReader.add_class_key
        ~add_protocol:(Hash_set.add protocols)
        ~register_global


    let register_alias ~path ~key ~data =
      DependencyReader.add_alias_key ~path key;
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
      DependencyReader.get_function_keys ~path |> purge_table_given_keys function_definitions;
      DependencyReader.get_class_keys ~path |> purge_table_given_keys class_definitions;
      DependencyReader.get_alias_keys ~path |> purge_table_given_keys aliases;
      DependencyReader.get_global_keys ~path |> purge_table_given_keys globals;
      DependencyReader.get_dependent_keys ~path |> purge_dependents;
      DependencyReader.clear_all_keys ~path


    let function_definitions =
      Hashtbl.find function_definitions

    let class_definition =
      Hashtbl.find class_definitions

    let protocols =
      protocols

    let in_class_definition_keys =
      Hashtbl.mem class_definitions

    let aliases =
      Hashtbl.find aliases

    let globals =
      Hashtbl.find globals

    let dependencies =
      DependencyReader.dependents

    module TypeOrderReader =
      (val TypeOrder.reader order: TypeOrder.Reader)

    module DependencyReader = DependencyReader
  end: Reader)


let resolution
    (module Reader: Reader)
    ?(annotations = Access.Map.empty)
    () =
  let parse_annotation = Type.create ~aliases:Reader.aliases in

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
            value = Node.create (Access (Access.create "self_or_class"));
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
                  Some (Map.add constraints ~key:variable ~data:annotation)
              | Type.Parametric { Type.parameters = parameters; name },
                _ ->
                  let arguments =
                    let primitive, _ = Type.split parameter in
                    TypeOrder.instantiate_parameters
                      (module Reader.TypeOrderReader)
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
                      Map.add
                        ~key:parameter
                        ~data:arguments
                        constraints
                    end
                  else
                    begin
                      match parameter with
                      | Type.Parametric { Type.parameters = [variable]; _ }
                        when Type.is_meta parameter ->
                          Some (Map.add constraints ~key:variable ~data:(parse_annotation value))
                      | _ ->
                          Some constraints
                    end
              | _, Type.Top ->
                  (* Don't constrain based on unknown arguments. *)
                  Some constraints
              | Type.Variable { Type.constraints = type_constraints; _ }, _ ->
                  if type_constraints = [] ||
                     List.mem type_constraints argument ~equal:Type.equal then
                    (match Map.find constraints parameter with
                     | Some existing
                       when not (Type.equal argument existing) ->
                         (* Don't do joins. *)
                         None
                     | _ ->
                         Some argument)
                    >>| fun argument ->
                    Map.add constraints ~key:parameter ~data:argument
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
          | None -> Map.add ~key:variable ~data:Type.Bottom constraints
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
      (match call.Expression.Call.name with
       | { Node.value = Access access; _ } ->
           qualifier @ access
       | _ ->
           [])
    in
    Reader.function_definitions name
    >>| instantiate_signature call arguments
    |> Option.value ~default:[]
  in

  let class_definition annotation =
    let primitive, _ = Type.split annotation in
    Reader.class_definition primitive
  in

  let method_signature ~resolution annotation call arguments =
    let definitions annotation =
      let primitive, parameters = Type.split annotation in
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
        class_definition primitive
        >>| (fun definition ->
            Annotated.Class.create definition
            |> Annotated.Class.generics ~resolution)
        >>| (fun generics ->
            match List.zip generics parameters with
            | Some zipped ->
                (* Don't instantiate Bottom. *)
                List.filter
                  ~f:(fun (_, parameter) -> not (Type.equal parameter Type.Bottom))
                  zipped
                |> Type.Map.of_alist_exn
            | None ->
                Type.Map.empty)
        |> Option.value ~default:Type.Map.empty in
      Reader.function_definitions name
      >>| List.map ~f:(instantiate ~constraints)
    in

    let successors =
      let primitive, _ = Type.split annotation in
      if Reader.in_class_definition_keys primitive then
        TypeOrder.successors (module Reader.TypeOrderReader) annotation
      else
        []
    in
    (annotation :: successors)
    |> List.find_map ~f:definitions
    >>| instantiate_signature call arguments
    |> Option.value ~default:[]
  in

  let order = (module Reader.TypeOrderReader : TypeOrder.Reader) in
  Resolution.create
    ~annotations
    ~order
    ~resolve:
      (fun ~resolution expression ->
         Annotated.resolve
           ~resolution
           expression)
    ~parse_annotation
    ~global:Reader.globals
    ~class_definition
    ~function_signature
    ~method_signature


let dependencies (module Reader: Reader) =
  Reader.dependencies


let register_class_definitions (module Reader: Reader) source =
  let order = (module Reader.TypeOrderReader : TypeOrder.Reader) in
  let module Visit = Visit.Make(struct
      type t = unit

      let expression _ _ =
        ()

      let statement _ = function
        | { Node.value = Class { Class.name; _ }; _ }
        | { Node.value = Stub (Stub.Class { Class.name; _ }); _ } ->
            let primitive, _ =
              Type.create ~aliases:Reader.aliases (Node.create (Access name))
              |> Type.split
            in
            if not (TypeOrder.contains order primitive) then
              TypeOrder.insert order primitive;
        | _ ->
            ()
    end)
  in
  Visit.visit () source


let register_aliases (module Reader: Reader) sources =
  let order = (module Reader.TypeOrderReader : TypeOrder.Reader) in
  let collect_aliases { Source.path; statements; _ } =
    let rec visit_statement aliases { Node.value; _ } =
      match value with
      | Assign {
          Assign.target;
          annotation = None;
          compound = None;
          value = Some value;
          _;
        } ->
          let value_annotation = Type.create ~aliases:Reader.aliases value in
          let target_annotation = Type.create ~aliases:Reader.aliases target in
          if not (Type.equal target_annotation Type.Top ||
                  Type.equal value_annotation Type.Top ||
                  Type.equal value_annotation target_annotation) then
            (path, target, value) :: aliases
          else
            aliases
      | If { If.body; orelse; _ } ->
          let aliases = List.fold ~init:aliases ~f:visit_statement body in
          let aliases = List.fold ~init:aliases ~f:visit_statement orelse in
          aliases
      | Import _ ->
          (* TODO(T25119940): Handle aliases here. *)
          aliases
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
        let target_annotation = Type.create ~aliases:Reader.aliases target in
        let value_annotation = Type.create ~aliases:Reader.aliases value in
        let rec annotation_in_order annotation =
          match annotation with
          | Type.Primitive _
          | Type.Parametric _ ->
              let primitive, _ = Type.split annotation in
              Option.is_some (TypeOrder.find order primitive)

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
        if Option.is_none (TypeOrder.find order primitive) &&
           annotation_in_order value_annotation then
          begin
            Reader.register_alias ~path ~key:target_annotation ~data:value_annotation;
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
    (module Reader: Reader)
    ?(source_root = Path.current_working_directory ())
    ?(check_dependency_exists = true)
    source =
  let path = source.Source.path in
  let parse_annotation = Type.create ~aliases:(Reader.aliases) in
  let resolution =
    resolution
      (module Reader: Reader)
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
                  Reader.register_definition
                    ~path
                    { Node.value = constructor; location })
              constructors;

            Reader.register_type
              ~path
              Type.Bottom
              definition.Class.name
              (Some (Node.create ~location definition))
            |> ignore;

            (* Handle enumeration constants. *)
            let enumeration { Argument.value; _ } =
              let enumerations =
                (* Custom enumeration classes. *)
                [
                  "enum.Enum";
                  "enum.IntEnum";
                  "util.enum.Enum";
                  "util.enum.IntEnum";
                  "util.enum.StringEnum";
                ]
              in
              match parse_annotation value with
              | Type.Primitive identifier
                when List.mem ~equal:String.equal enumerations (Identifier.show identifier) ->
                  Some (Access.create (Identifier.show identifier))
              | _ ->
                  None
            in
            List.find_map ~f:enumeration definition.Class.bases
            >>| (fun enumeration ->
                (* Register generated constructor. *)
                Reader.register_definition
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
            if Define.is_method definition then
              let parent = Option.value_exn definition.Define.parent in
              Reader.register_definition
                ~path
                ~name_override:(parent @ definition.Define.name)
                { Node.value = definition; location }
            else
              Reader.register_definition ~path { Node.value = definition; location }
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
                    Log.log ~section:`Dependencies "Import path %s not found" path;
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
              ~f:(fun dependency -> Reader.register_dependency ~path ~dependency)
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
               Reader.register_global
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
        Reader.register_global
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
    (module Reader: Reader)
    ?(source_root = Path.current_working_directory ())
    ?(check_dependency_exists = true)
    sources =
  (* TODO(T19628746) Handle type aliases when building the environment instead of relying on this
     hack. *)
  let brute_force_type_aliases () =
    let aliased_classes =
      [
        "collections.Callable", "typing.Callable";
        "collections.Container", "typing.Container";
        "collections.Hashable", "typing.Hashable";
        "collections.Iterable", "typing.Iterable";
        "collections.Iterator", "typing.Iterator";
        "collections.Sized", "typing.Sized";
        "collections.Generator", "typing.Generator";
        "collections.ByteString", "typing.ByteString";
        "collections.Reversible", "typing.Reversible";
        "collections.Mapping", "typing.Mapping";
        "collections.MappingView", "typing.MappingView";
        "collections.ItemsView", "typing.ItemsView";
        "collections.KeysView", "typing.KeysView";
        "collections.ValuesView", "typing.ValuesView";
        "collections.MutableMapping", "typing.MutableMapping";
        "collections.Sequence", "typing.Sequence";
        "collections.MutableSequence", "typing.MutableSequence";
        "collections.MutableSet", "typing.MutableSet";
        "collections.AbstractSet", "typing.Set";
        "util.enum.IntEnum", "enum.IntEnum";
      ]
      |> List.map ~f:(fun (alias, actual) ->
          Type.Primitive (Identifier.create alias),
          Type.Primitive (Identifier.create actual))
    in
    List.iter
      ~f:(fun (alias, actual) -> Reader.register_alias ~path:"" ~key:alias ~data:actual)
      aliased_classes
  in
  brute_force_type_aliases ();

  List.iter ~f:(register_class_definitions (module Reader)) sources;
  register_aliases (module Reader) sources;
  List.iter ~f:(connect_type_order ~source_root ~check_dependency_exists (module Reader)) sources;

  TypeOrder.complete (module Reader.TypeOrderReader) ~bottom:Type.Bottom ~top:Type.Object;
  TypeOrder.check_integrity (module Reader.TypeOrderReader)


let infer_implementations (module Reader: Reader) ~protocol =
  let module Edge = TypeOrder.Edge in
  let resolution = resolution (module Reader) () in

  Resolution.class_definition resolution protocol
  >>| (fun protocol_definition ->
      let open Annotated in
      let protocol_definition = Class.create protocol_definition in

      (* Skip useless protocols for better performance. *)
      let skip_protocol =
        let whitelisted = ["typing.Hashable"] in
        let name = Class.name protocol_definition |> Expression.Access.show in
        List.is_empty (Class.methods protocol_definition) ||
        List.mem ~equal:String.equal whitelisted name
      in

      if skip_protocol then
        Edge.Set.empty
      else
        begin
          (* Get all implementing classes. *)
          let implementations =
            let implements annotation =
              Resolution.class_definition resolution annotation
              >>| Class.create
              >>| (fun definition ->
                  not (Class.is_protocol definition) &&
                  Class.implements ~resolution ~protocol:protocol_definition definition)
              |> Option.value ~default:false
            in
            TypeOrder.greatest (module Reader.TypeOrderReader) ~matches:implements
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
          edges
        end)
  |> Option.value ~default:Edge.Set.empty


module Builder = struct
  let create () =
    let function_definitions = Access.Table.create () in
    let class_definitions = Type.Table.create () in
    let protocols = Type.Hash_set.create () in
    let order = TypeOrder.Builder.default () in
    let aliases = Type.Table.create () in
    let globals = Access.Table.create () in
    let dependencies = Dependencies.create () in
    { function_definitions; class_definitions; protocols; order; aliases; globals; dependencies }


  let copy
      {
        function_definitions;
        class_definitions;
        protocols;
        order;
        aliases;
        globals;
        dependencies;
      } =
    {
      function_definitions = Hashtbl.copy function_definitions;
      class_definitions = Hashtbl.copy class_definitions;
      protocols = Hash_set.copy protocols;
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
