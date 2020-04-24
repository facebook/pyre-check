(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement
open Expression
open SharedMemoryKeys

type unannotated_global =
  | SimpleAssign of {
      explicit_annotation: Expression.t option;
      value: Expression.t;
      target_location: Location.t;
    }
  | TupleAssign of {
      value: Expression.t;
      target_location: Location.t;
      index: int;
      total_length: int;
    }
  | Imported of Reference.t
  | Define of Define.Signature.t Node.t list
[@@deriving compare, show, equal, sexp]

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    class_exists: ?dependency:dependency -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
    all_indices: unit -> IndexTracker.t list;
    all_unannotated_globals: unit -> Reference.t list;
    all_defines: unit -> Reference.t list;
    all_defines_in_module: Reference.t -> Reference.t list;
    get_class_definition: ?dependency:dependency -> string -> ClassSummary.t Node.t option;
    get_unannotated_global: ?dependency:dependency -> Reference.t -> unannotated_global option;
    get_define: ?dependency:dependency -> Reference.t -> FunctionDefinition.t option;
    get_define_body: ?dependency:dependency -> Reference.t -> Define.t Node.t option;
    hash_to_key_map: unit -> string String.Map.t;
    serialize_decoded: Memory.decodable -> (string * string * string option) option;
    decoded_equal: Memory.decodable -> Memory.decodable -> bool option;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let unannotated_global_environment = Fn.id

  let class_exists { class_exists; _ } = class_exists

  let all_classes { all_classes; _ } = all_classes ()

  let all_indices { all_indices; _ } = all_indices ()

  let all_defines { all_defines; _ } = all_defines ()

  let all_unannotated_globals { all_unannotated_globals; _ } = all_unannotated_globals ()

  let get_class_definition { get_class_definition; _ } = get_class_definition

  let get_unannotated_global { get_unannotated_global; _ } = get_unannotated_global

  let get_define { get_define; _ } = get_define

  let get_define_body { get_define_body; _ } = get_define_body

  let all_defines_in_module { all_defines_in_module; _ } = all_defines_in_module

  let hash_to_key_map { hash_to_key_map; _ } = hash_to_key_map ()

  let serialize_decoded { serialize_decoded; _ } = serialize_decoded

  let decoded_equal { decoded_equal; _ } = decoded_equal

  let primitive_name annotation =
    let primitive, _ = Type.split annotation in
    Type.primitive_name primitive


  let is_protocol { get_class_definition; _ } ?dependency annotation =
    primitive_name annotation
    >>= get_class_definition ?dependency
    >>| Node.value
    >>| ClassSummary.is_protocol
    |> Option.value ~default:false


  let contains_untracked read_only ?dependency annotation =
    let is_tracked = class_exists read_only ?dependency in
    List.exists ~f:(fun annotation -> not (is_tracked annotation)) (Type.elements annotation)
end

(* The key tracking is necessary because there is no empirical way to determine which classes exist
   for a given class. This "fan-out" necessitates internal tracking. However, this module need not
   be sealed to ensure write only-ness since we're not dependency tracking this, since it's only
   used internally for doing our dependency analysis, and for all_classes, which is only used for
   all-or-nothing operations like validating the class hierarchy and printing it for debugging
   purposes *)
module KeyTracker = struct
  module ClassKeyValue = struct
    type t = Identifier.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module UnannotatedGlobalKeyValue = struct
    type t = Reference.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module FunctionKeyValue = struct
    type t = Reference.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "TypeCheckUnit keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)
  module UnannotatedGlobalKeys =
    Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (UnannotatedGlobalKeyValue)
  module FunctionKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (FunctionKeyValue)

  let get_keys keys =
    ClassKeys.KeySet.of_list keys
    |> ClassKeys.get_batch
    |> ClassKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat


  let get_unannotated_global_keys keys =
    UnannotatedGlobalKeys.KeySet.of_list keys
    |> UnannotatedGlobalKeys.get_batch
    |> UnannotatedGlobalKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat


  let get_define_body_keys keys =
    FunctionKeys.KeySet.of_list keys
    |> FunctionKeys.get_batch
    |> FunctionKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat
end

(* We want to ensure that we are only writing to this table in this phase, not creating internal
   dependencies with self-reads. Accordingly read_only should only be called by downstream clients *)
module WriteOnly : sig
  val set_class_definition : name:string -> definition:ClassSummary.t Node.t -> unit

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    previous_classes_list:string list ->
    previous_unannotated_globals_list:Reference.t list ->
    previous_defines_list:Reference.t list ->
    DependencyKey.Transaction.t

  val get_all_dependents
    :  class_additions:string list ->
    unannotated_global_additions:Reference.t list ->
    define_additions:Reference.t list ->
    DependencyKey.KeySet.t

  val direct_data_purge
    :  previous_classes_list:Type.Primitive.t list ->
    previous_unannotated_globals_list:Reference.t list ->
    previous_defines_list:Reference.t list ->
    unit

  val set_unannotated_global : target:Reference.t -> unannotated_global -> unit

  val set_define : name:Reference.t -> FunctionDefinition.t -> unit

  val read_only : ast_environment:AstEnvironment.ReadOnly.t -> ReadOnly.t
end = struct
  module ClassValue = struct
    type t = ClassSummary.t Node.t

    let prefix = Prefix.make ()

    let description = "Class"

    let unmarshall value = Marshal.from_string value 0

    let compare = Node.compare ClassSummary.compare
  end

  module ClassDefinitions =
    DependencyTrackedMemory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (DependencyKey)
      (ClassValue)

  module UnannotatedGlobalValue = struct
    type t = unannotated_global

    let prefix = Prefix.make ()

    let description = "UnannotatedGlobal"

    let unmarshall value = Marshal.from_string value 0

    let compare = compare_unannotated_global
  end

  module UnannotatedGlobals =
    DependencyTrackedMemory.DependencyTrackedTableNoCache
      (SharedMemoryKeys.ReferenceKey)
      (DependencyKey)
      (UnannotatedGlobalValue)

  module FunctionDefinitionValue = struct
    type t = FunctionDefinition.t

    let description = "Define"

    let prefix = Prefix.make ()

    let unmarshall value = Marshal.from_string value 0

    let compare = FunctionDefinition.compare
  end

  module FunctionDefinitions =
    DependencyTrackedMemory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (DependencyKey)
      (FunctionDefinitionValue)

  let set_unannotated_global ~target = UnannotatedGlobals.add target

  let set_class_definition ~name ~definition = ClassDefinitions.write_through name definition

  let set_define ~name definitions = FunctionDefinitions.write_through name definitions

  let add_to_transaction
      transaction
      ~previous_classes_list
      ~previous_unannotated_globals_list
      ~previous_defines_list
    =
    let class_keys = ClassDefinitions.KeySet.of_list previous_classes_list in
    let unannotated_globals_keys =
      UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    in
    let defines_keys = FunctionDefinitions.KeySet.of_list previous_defines_list in
    ClassDefinitions.add_to_transaction ~keys:class_keys transaction
    |> UnannotatedGlobals.add_to_transaction ~keys:unannotated_globals_keys
    |> FunctionDefinitions.add_to_transaction ~keys:defines_keys


  let get_all_dependents ~class_additions ~unannotated_global_additions ~define_additions =
    let function_and_class_dependents =
      DependencyKey.KeySet.union
        (ClassDefinitions.KeySet.of_list class_additions |> ClassDefinitions.get_all_dependents)
        ( FunctionDefinitions.KeySet.of_list define_additions
        |> FunctionDefinitions.get_all_dependents )
    in
    DependencyKey.KeySet.union
      function_and_class_dependents
      ( UnannotatedGlobals.KeySet.of_list unannotated_global_additions
      |> UnannotatedGlobals.get_all_dependents )


  let direct_data_purge
      ~previous_classes_list
      ~previous_unannotated_globals_list
      ~previous_defines_list
    =
    ClassDefinitions.KeySet.of_list previous_classes_list |> ClassDefinitions.remove_batch;
    UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    |> UnannotatedGlobals.remove_batch;
    FunctionDefinitions.KeySet.of_list previous_defines_list |> FunctionDefinitions.remove_batch


  let read_only ~ast_environment =
    let all_classes () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment |> KeyTracker.get_keys
    in
    let all_indices () =
      all_classes ()
      |> Type.Primitive.Set.of_list
      |> IndexTracker.indices
      |> IndexTracker.Set.to_list
    in
    let all_unannotated_globals () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_unannotated_global_keys
    in
    let all_defines () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_define_body_keys
    in
    let class_exists ?dependency name = ClassDefinitions.mem ?dependency name in
    let hash_to_key_map () =
      let extend_map map ~new_map =
        Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
      in
      ClassDefinitions.compute_hashes_to_keys ~keys:(all_classes ())
      |> extend_map
           ~new_map:(UnannotatedGlobals.compute_hashes_to_keys ~keys:(all_unannotated_globals ()))
      |> extend_map ~new_map:(FunctionDefinitions.compute_hashes_to_keys ~keys:(all_defines ()))
    in
    let serialize_decoded = function
      | ClassDefinitions.Decoded (key, value) ->
          let value = value >>| Node.value >>| ClassSummary.show in
          Some (ClassValue.description, key, value)
      | UnannotatedGlobals.Decoded (key, value) ->
          let value = value >>| show_unannotated_global in
          Some (UnannotatedGlobalValue.description, Reference.show key, value)
      | FunctionDefinitions.Decoded (key, value) ->
          let value =
            value >>| fun value -> Sexp.to_string_hum [%message (value : FunctionDefinition.t)]
          in
          Some (FunctionDefinitionValue.description, Reference.show key, value)
      | _ -> None
    in
    let decoded_equal first second =
      match first, second with
      | ClassDefinitions.Decoded (_, first), ClassDefinitions.Decoded (_, second) ->
          Some (Option.equal (Node.equal ClassSummary.equal) first second)
      | UnannotatedGlobals.Decoded (_, first), UnannotatedGlobals.Decoded (_, second) ->
          Some (Option.equal equal_unannotated_global first second)
      | FunctionDefinitions.Decoded (_, first), FunctionDefinitions.Decoded (_, second) ->
          let node_equal left right = Int.equal 0 (FunctionDefinitionValue.compare left right) in
          Some (Option.equal node_equal first second)
      | _ -> None
    in
    let get_define = FunctionDefinitions.get in
    let get_define_body ?dependency key =
      FunctionDefinitions.get ?dependency key >>= fun { FunctionDefinition.body; _ } -> body
    in
    let all_defines_in_module qualifier = KeyTracker.get_define_body_keys [qualifier] in
    {
      ast_environment;
      ReadOnly.get_class_definition = ClassDefinitions.get;
      all_classes;
      all_indices;
      all_defines;
      class_exists;
      get_unannotated_global = UnannotatedGlobals.get;
      get_define;
      get_define_body;
      all_defines_in_module;
      all_unannotated_globals;
      hash_to_key_map;
      serialize_decoded;
      decoded_equal;
    }
end

let missing_builtin_classes, missing_typing_classes, missing_typing_extensions_classes =
  let make ?(bases = []) ?(metaclasses = []) ?(body = []) name =
    let create_base annotation =
      { Call.Argument.name = None; value = Type.expression annotation }
    in
    let create_metaclass annotation =
      {
        Call.Argument.name = Some (Node.create_with_default_location "metaclass");
        value = Type.expression annotation;
      }
    in
    {
      Class.name = Node.create_with_default_location (Reference.create name);
      bases = List.map bases ~f:create_base @ List.map metaclasses ~f:create_metaclass;
      body;
      decorators = [];
    }
    |> Node.create_with_default_location
  in
  let single_unary_generic =
    [Type.parametric "typing.Generic" [Single (Variable (Type.Variable.Unary.create "typing._T"))]]
  in
  let catch_all_generic = [Type.parametric "typing.Generic" [Group Any]] in
  let callable_body =
    [
      Statement.Assign
        {
          target =
            Node.create_with_default_location
              (Expression.Name
                 (Ast.Expression.create_name ~location:Location.any "typing.Callable.__call__"));
          annotation = Some (Type.expression Type.object_primitive);
          value =
            Node.create_with_default_location
              (Expression.Name (Ast.Expression.create_name ~location:Location.any "None"));
          parent = Some (Reference.create "typing.Callable");
        };
    ]
    |> List.map ~f:Node.create_with_default_location
  in
  let make_dunder_get ~parent ~host ~host_type ~return =
    let parent = Reference.create parent in
    Statement.Define
      {
        signature =
          {
            name =
              Node.create_with_default_location
                (Reference.combine parent (Reference.create "__get__"));
            parameters =
              [
                Node.create_with_default_location
                  { Ast.Expression.Parameter.name = "self"; value = None; annotation = None };
                Node.create_with_default_location
                  {
                    Ast.Expression.Parameter.name = "host";
                    value = None;
                    annotation = Some (Type.expression host);
                  };
                Node.create_with_default_location
                  {
                    Ast.Expression.Parameter.name = "host_type";
                    value =
                      Some
                        (Node.create_with_default_location
                           (Expression.Name
                              (Ast.Expression.create_name ~location:Location.any "None")));
                    annotation = Some (Type.expression host_type);
                  };
              ];
            decorators = [];
            return_annotation = Some (Type.expression return);
            async = false;
            generator = false;
            parent = Some parent;
            nesting_define = None;
          };
        captures = [];
        body = [];
      }
  in
  let classmethod_body =
    (*
     * _T = TypeVar("_T")
     * _S = TypeVar("_S")
     * class ClassMethod(Generic[_T]):
     *   def __get__(self, host: object, host_type: _S = None) -> BoundMethod[_T, _S]: ...
     *)
    [
      make_dunder_get
        ~parent:"typing.ClassMethod"
        ~host:Type.object_primitive
        ~host_type:(Variable (Type.Variable.Unary.create "typing._S"))
        ~return:
          (Type.Parametric
             {
               name = "BoundMethod";
               parameters =
                 [
                   Single (Variable (Type.Variable.Unary.create "typing._T"));
                   Single (Variable (Type.Variable.Unary.create "typing._S"));
                 ];
             });
    ]
    |> List.map ~f:Node.create_with_default_location
  in
  let staticmethod_body =
    (*
     * _T = TypeVar("_T")
     * class StaticMethod(Generic[_T]):
     *   def __get__(self, host: object, host_type: object = None) -> _T: ...
     *)
    [
      make_dunder_get
        ~parent:"typing.StaticMethod"
        ~host:Type.object_primitive
        ~host_type:Type.object_primitive
        ~return:(Variable (Type.Variable.Unary.create "typing._T"));
    ]
    |> List.map ~f:Node.create_with_default_location
  in

  let typing_classes =
    [
      make "typing.Optional" ~bases:single_unary_generic;
      make "typing.Undeclared";
      make "typing.NoReturn";
      make "typing.Annotated" ~bases:catch_all_generic;
      make "typing.Protocol" ~bases:catch_all_generic;
      make "typing.Callable" ~bases:catch_all_generic ~body:callable_body;
      make "typing.FrozenSet" ~bases:single_unary_generic;
      make "typing.ClassVar" ~bases:single_unary_generic;
      make "typing.Final" ~bases:catch_all_generic;
      make "typing.Union" ~bases:catch_all_generic;
      make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic";
      make "typing.ClassMethod" ~bases:single_unary_generic ~body:classmethod_body;
      make "typing.StaticMethod" ~bases:single_unary_generic ~body:staticmethod_body;
    ]
  in
  let typing_extension_classes =
    [
      make "typing_extensions.Final";
      make "typing_extensions.Literal" ~bases:catch_all_generic;
      make "typing_extensions.Annotated" ~bases:catch_all_generic;
    ]
  in
  let builtin_classes =
    let t_self_expression =
      Expression.Name (Name.Identifier "TSelf") |> Node.create_with_default_location
    in
    [
      make
        ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
        (Type.TypedDictionary.class_name ~total:true);
      make
        ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
        (Type.TypedDictionary.class_name ~total:false);
      (* I think this may be actually covariant, covariant, but I don't think there's any value in
         going out on that limb yet *)
      make
        ~bases:
          [
            Type.parametric
              "typing.Generic"
              [Single (Type.variable "typing._T"); Single (Type.variable "typing._S")];
            Type.Primitive "typing.Callable";
          ]
        "BoundMethod";
    ]
  in
  builtin_classes, typing_classes, typing_extension_classes


let register_class_definitions ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  (* TODO (T57944324): Support checking classes that are nested inside function bodies *)
  let module ClassCollector = Visit.MakeStatementVisitor (struct
    type t = Class.t Node.t list

    let visit_children _ = true

    let statement _ sofar = function
      | { Node.location; value = Statement.Class definition } ->
          { Node.location; value = definition } :: sofar
      | _ -> sofar
  end)
  in
  let classes = ClassCollector.visit [] source in
  let classes =
    match Reference.as_list qualifier with
    | [] -> classes @ missing_builtin_classes
    | ["typing"] -> classes @ missing_typing_classes
    | ["typing_extensions"] -> classes @ missing_typing_extensions_classes
    | _ -> classes
  in
  let register new_annotations { Node.location; value = { Class.name; _ } as definition } =
    let primitive = Reference.show (Node.value name) in
    let definition =
      match primitive with
      | "type" ->
          let value =
            Type.expression (Type.parametric "typing.Generic" [Single (Type.variable "typing._T")])
          in
          { definition with Class.bases = [{ name = None; value }] }
      | "typing.GenericMeta" ->
          let body =
            [
              Statement.Define
                {
                  signature =
                    {
                      name =
                        Reference.create "typing.GenericMeta.__getitem__"
                        |> Node.create_with_default_location;
                      parameters =
                        [
                          { Parameter.name = "cls"; value = None; annotation = None }
                          |> Node.create_with_default_location;
                          { Parameter.name = "arg"; value = None; annotation = None }
                          |> Node.create_with_default_location;
                        ];
                      decorators = [];
                      return_annotation = None;
                      async = false;
                      generator = false;
                      parent = Some (Reference.create "typing.GenericMeta");
                      nesting_define = None;
                    };
                  captures = [];
                  body = [];
                }
              |> Node.create_with_default_location;
            ]
          in
          { definition with body }
      | _ -> definition
    in
    WriteOnly.set_class_definition
      ~name:primitive
      ~definition:{ Node.location; value = ClassSummary.create definition };
    Set.add new_annotations primitive
  in
  List.fold classes ~init:Type.Primitive.Set.empty ~f:register
  |> Set.to_list
  |> KeyTracker.ClassKeys.add qualifier


let missing_builtin_globals =
  let assign name annotation =
    ( Reference.create name,
      SimpleAssign
        {
          explicit_annotation = Some (Type.expression annotation);
          target_location = Location.any;
          value = Node.create_with_default_location Expression.Ellipsis;
        } )
  in
  [assign "None" Type.none; assign "..." Type.Any; assign "__debug__" Type.bool]


let collect_unannotated_globals { Source.statements; source_path = { SourcePath.qualifier; _ }; _ } =
  let rec visit_statement ~qualifier globals { Node.value; location } =
    let qualified_name target =
      let target = name_to_reference_exn target |> Reference.sanitize_qualified in
      Option.some_if (Reference.length target = 1) (Reference.combine qualifier target)
    in
    match value with
    | Statement.Assign
        {
          Assign.target = { Node.value = Name target; location = target_location };
          annotation;
          value;
          _;
        }
      when is_simple_name target ->
        qualified_name target
        >>| (fun qualified ->
              (qualified, SimpleAssign { explicit_annotation = annotation; value; target_location })
              :: globals)
        |> Option.value ~default:globals
    | Statement.Assign { Assign.target = { Node.value = Tuple elements; _ }; value; _ } ->
        let valid =
          let total_length = List.length elements in
          let is_simple_name index = function
            | { Node.value = Expression.Name name; location = target_location }
              when is_simple_name name ->
                qualified_name name
                >>| fun name -> name, TupleAssign { value; target_location; index; total_length }
            | _ -> None
          in
          List.mapi elements ~f:is_simple_name
        in
        (Option.all valid |> Option.value ~default:[]) @ globals
    | Import { Import.from = Some _; imports = [{ Import.name = { Node.value = name; _ }; _ }] }
      when String.equal (Reference.show name) "*" ->
        (* Don't register x.* as a global when a user writes `from x import *`. *)
        globals
    | Import { Import.from; imports } ->
        let from =
          match from >>| Node.value >>| Reference.show with
          | None
          | Some "future.builtins"
          | Some "builtins" ->
              Reference.empty
          | Some from -> Reference.create from
        in
        let import_to_global { Import.name = { Node.value = name; _ }; alias } =
          let qualified_name =
            match alias with
            | None -> Reference.combine qualifier name
            | Some { Node.value = alias; _ } -> Reference.combine qualifier alias
          in
          let original_name = Reference.combine from name in
          qualified_name, Imported original_name
        in
        List.rev_append (List.map ~f:import_to_global imports) globals
    | Define { Define.signature = { Define.Signature.name; _ } as signature; _ } ->
        (Node.value name, Define [Node.create signature ~location]) :: globals
    | If { If.body; orelse; _ } ->
        (* TODO(T28732125): Properly take an intersection here. *)
        List.fold ~init:globals ~f:(visit_statement ~qualifier) (body @ orelse)
    | Try { Try.body; handlers; orelse; finally } ->
        let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) body in
        let globals =
          let handlers_statements =
            List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
          in
          List.fold ~init:globals ~f:(visit_statement ~qualifier) handlers_statements
        in
        let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) orelse in
        List.fold ~init:globals ~f:(visit_statement ~qualifier) finally
    | _ -> globals
  in
  let write (target, o) =
    WriteOnly.set_unannotated_global ~target o;
    target
  in
  let merge_defines unannotated_globals_alist =
    let not_defines, defines =
      List.partition_map unannotated_globals_alist ~f:(function
          | name, Define defines -> `Snd (name, defines)
          | x -> `Fst x)
    in
    let add_to_map sofar (name, defines) =
      let merge_with_existing to_merge = function
        | None -> Some to_merge
        | Some existing -> Some (to_merge @ existing)
      in
      Map.change sofar name ~f:(merge_with_existing defines)
    in
    List.fold defines ~f:add_to_map ~init:Reference.Map.empty
    |> Reference.Map.map ~f:(fun x -> Define x)
    |> Reference.Map.to_alist
    |> List.append not_defines
  in
  let globals =
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements |> merge_defines |> List.rev
  in
  let globals =
    match Reference.as_list qualifier with
    | [] -> globals @ missing_builtin_globals
    | _ -> globals
  in
  globals |> List.map ~f:write |> KeyTracker.UnannotatedGlobalKeys.add qualifier


let collect_defines ({ Source.source_path = { SourcePath.qualifier; is_external; _ }; _ } as source)
  =
  match is_external with
  | true ->
      (* Do not collect function bodies for external sources as they won't get type checked *)
      ()
  | false ->
      let definitions = FunctionDefinition.collect_defines source in
      List.iter definitions ~f:(fun (name, definition) -> WriteOnly.set_define ~name definition);
      KeyTracker.FunctionKeys.add
        qualifier
        (List.map definitions ~f:fst |> List.sort ~compare:Reference.compare)


module UpdateResult = struct
  type t = {
    previous_classes: Type.Primitive.Set.t;
    previous_unannotated_globals: Reference.Set.t;
    previous_defines: Reference.Set.t;
    define_additions: Reference.Set.t;
    triggered_dependencies: DependencyKey.KeySet.t;
    upstream: AstEnvironment.UpdateResult.t;
    read_only: ReadOnly.t;
  }

  type read_only = ReadOnly.t

  let previous_unannotated_globals { previous_unannotated_globals; _ } =
    previous_unannotated_globals


  let previous_defines { previous_defines; _ } = previous_defines

  let previous_classes { previous_classes; _ } = previous_classes

  let define_additions { define_additions; _ } = define_additions

  let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream

  let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
    [triggered_dependencies; AstEnvironment.UpdateResult.triggered_dependencies upstream]


  let unannotated_global_environment_update_result = Fn.id

  let read_only { read_only; _ } = read_only
end

let update_this_and_all_preceding_environments
    ast_environment
    ~scheduler
    ~configuration
    ~ast_environment_update_result:upstream
    modified_qualifiers
  =
  let map sources =
    let register qualifier =
      AstEnvironment.ReadOnly.get_source ast_environment qualifier
      >>| (fun source ->
            register_class_definitions source;
            collect_unannotated_globals source;
            collect_defines source)
      |> Option.value ~default:()
    in
    List.iter sources ~f:register
  in
  let modified_qualifiers = Set.to_list modified_qualifiers in
  let update () =
    Scheduler.iter
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~configuration
      ~f:map
      ~inputs:modified_qualifiers
  in
  let previous_classes_list = KeyTracker.get_keys modified_qualifiers in
  let previous_classes = Type.Primitive.Set.of_list previous_classes_list in
  let previous_unannotated_globals_list =
    KeyTracker.get_unannotated_global_keys modified_qualifiers
  in
  let previous_unannotated_globals = Reference.Set.of_list previous_unannotated_globals_list in
  let previous_defines_list = KeyTracker.get_define_body_keys modified_qualifiers in
  let previous_defines = Reference.Set.of_list previous_defines_list in
  KeyTracker.ClassKeys.KeySet.of_list modified_qualifiers |> KeyTracker.ClassKeys.remove_batch;
  KeyTracker.UnannotatedGlobalKeys.KeySet.of_list modified_qualifiers
  |> KeyTracker.UnannotatedGlobalKeys.remove_batch;
  KeyTracker.FunctionKeys.KeySet.of_list modified_qualifiers |> KeyTracker.FunctionKeys.remove_batch;
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let define_additions, triggered_dependencies =
        Profiling.track_duration_and_shared_memory_with_dynamic_tags
          "TableUpdate(Unannotated globals)"
          ~f:(fun _ ->
            let (), mutation_triggers =
              DependencyKey.Transaction.empty ~scheduler ~configuration
              |> WriteOnly.add_to_transaction
                   ~previous_classes_list
                   ~previous_unannotated_globals_list
                   ~previous_defines_list
              |> DependencyKey.Transaction.execute ~update
            in
            let current_classes =
              KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
            in
            let current_unannotated_globals =
              KeyTracker.get_unannotated_global_keys modified_qualifiers |> Reference.Set.of_list
            in
            let current_defines =
              KeyTracker.get_define_body_keys modified_qualifiers |> Reference.Set.of_list
            in
            let class_additions = Type.Primitive.Set.diff current_classes previous_classes in
            let unannotated_global_additions =
              Reference.Set.diff current_unannotated_globals previous_unannotated_globals
            in
            let define_additions = Reference.Set.diff current_defines previous_defines in
            let addition_triggers =
              WriteOnly.get_all_dependents
                ~class_additions:(Set.to_list class_additions)
                ~unannotated_global_additions:(Set.to_list unannotated_global_additions)
                ~define_additions:(Set.to_list define_additions)
            in
            let triggered_dependencies =
              DependencyKey.KeySet.union addition_triggers mutation_triggers
            in
            let tags () =
              let triggered_dependencies_size =
                SharedMemoryKeys.DependencyKey.KeySet.cardinal triggered_dependencies
                |> Format.sprintf "%d"
              in
              [
                "phase_name", "Global discovery";
                "number_of_triggered_dependencies", triggered_dependencies_size;
              ]
            in
            { Profiling.result = define_additions, triggered_dependencies; tags })
      in
      {
        UpdateResult.previous_classes;
        previous_unannotated_globals;
        previous_defines;
        define_additions;
        triggered_dependencies;
        upstream;
        read_only = WriteOnly.read_only ~ast_environment;
      }
  | _ ->
      let triggered_dependencies =
        Profiling.track_duration_and_shared_memory
          "LegacyTableUpdate(Unannotated globals)"
          ~tags:["phase_name", "global discovery"]
          ~f:(fun _ ->
            WriteOnly.direct_data_purge
              ~previous_classes_list
              ~previous_unannotated_globals_list
              ~previous_defines_list;
            update ();
            DependencyKey.KeySet.empty)
      in
      {
        previous_classes;
        previous_unannotated_globals;
        previous_defines;
        define_additions = Reference.Set.empty;
        triggered_dependencies;
        upstream;
        read_only = WriteOnly.read_only ~ast_environment;
      }
