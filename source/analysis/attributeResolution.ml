(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AttributeResolution: layer of the environment stack
 * - upstream: ClassSuccessorMetadataEnvironment
 * - downstream: AnnotatedGlobalEnvironment
 *
 * Unlike most other layers, attribute resolution combines business
 * logic and caching, and it actually internally has 4 different layers
 * with their own cache tables. In upstream -> downstream order they are
 * - MetaclassCache
 *   - key: type name as a string
 *   - value: Type.t option, the metaclass of a class (if any)
 * - AttributeCache
 *   - key: AttributeTableKey, a combination of a Type.t and
 *     some flags that determine which attributes exist in different contexts
 *   - value: UninstantiatedAttributeTable.t, a table mapping names (strings)
 *     to UninstantiatedAnnotation.t AnnotatedAttribute.t, which has
 *     - information about annotations, visibility, and other factors that
 *       determine how attributes may be used in typing as well
 *     - possible problems detected while resolving the attribute, which
 *       can later be turned into type errors
 * - GlobalAnnotationCache
 *   - key: name of a global, as a Reference.t
 *   - value: Global.t option; this has type information needed in type
 *     checking as well as possible problems resolving
 *
 * The implementation of all of this is implemented using object-oriented
 * programming becasue this allows us to define the logic in ordinary functions
 * and add caches via inheritance, following the idea described as "Open
 * recursion" in RealWorldOcaml: https://dev.realworldocaml.org/objects.html.
 *)

open Core
open Pyre
open Ast
open Statement
open CycleDetection
open ClassSummary
open SignatureSelection

let map_find = Map.find

let compose = function
  | Type.Record.Variance.Bivariant, x
  | x, Type.Record.Variance.Bivariant ->
      x
  | Invariant, _
  | _, Invariant ->
      Invariant
  | Covariant, Covariant
  | Contravariant, Contravariant ->
      Covariant
  | _ -> Contravariant


let inv v = compose (v, Contravariant)

let union = function
  | Type.Record.Variance.Bivariant, x
  | x, Type.Record.Variance.Bivariant ->
      x
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  | _ -> Invariant


module VarianceVisitor = struct
  type injectivity = bool [@@deriving compare]

  type t_param = string * Type.Record.Variance.t * injectivity [@@deriving compare]

  type t_param_array = t_param array [@@deriving compare]

  type variance_env = (string, t_param array) Stdlib.Hashtbl.t

  let on_class ~class_name ~on_edge ~on_var ~to_list ~get_base_class_types =
    let handle_ordered_types ordered_types on_type variance inj =
      match ordered_types with
      | Type.Record.OrderedTypes.Concrete concretes ->
          List.iter ~f:(fun ty -> on_type ~variance ~inj ~typ:ty) concretes
      | Type.Record.OrderedTypes.Concatenation c -> begin
          match Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation c with
          | Some annotation -> on_type ~variance ~inj ~typ:annotation
          | None -> ()
        end
    in
    let param_to_types p =
      match p with
      | Type.Record.Callable.Defined defined ->
          List.filter_map ~f:(fun x -> Type.Record.Callable.CallableParamType.annotation x) defined
      | FromParamSpec { head; _ } -> head
      | Type.Record.Callable.Undefined -> []
    in
    let arguments_to_types (arguments : Type.Argument.t list) =
      List.fold
        ~f:(fun acc x ->
          match x with
          | Type.Argument.Single t -> t :: acc
          | CallableParameters parameters -> param_to_types parameters @ acc
          | Unpacked unpackable -> (
              let ordered_types =
                Type.OrderedTypes.Concatenation.create_from_unpackable unpackable
              in
              match
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation ordered_types
              with
              | Some annotation -> annotation :: acc
              | None -> acc))
        arguments
        ~init:[]
    in

    let rec on_type ~variance ~inj ~typ =
      match typ with
      | Type.Variable v ->
          (* TODO: understand how to implement on_var *)
          on_var v.name variance inj
      | Type.Parametric { name; arguments } ->
          let params : t_param array = on_edge name in
          let arguments_to_types = arguments_to_types arguments in
          for i = 0 to Array.length params - 1 do
            let ty = List.nth arguments_to_types i in
            match ty with
            | Some ty ->
                let _, variance', inj' = params.(i) in
                on_type ~variance:(compose (variance, variance')) ~inj:inj' ~typ:ty
            | None -> ()
          done
      | Type.Union types -> List.iter ~f:(fun ty -> on_type ~variance ~inj ~typ:ty) types
      | Type.PyreReadOnly typ -> on_type ~variance ~inj ~typ
      | Type.Callable { implementation; overloads; _ } ->
          let process (currerent_signature : Type.t Callable.overload) =
            let return_type = currerent_signature.annotation in

            let safe_tl lst =
              match lst with
              | [] -> []
              | _ :: tl -> tl
            in
            let params_types = param_to_types currerent_signature.parameters in
            (* TODO: For methods, the first parameter is just the type of the self argument and
               should not be considered. *)
            on_type ~variance ~inj ~typ:return_type;
            List.iter
              ~f:(fun ty -> on_type ~variance:(inv variance) ~inj ~typ:ty)
              (safe_tl params_types)
          in
          process implementation;
          List.iter ~f:process overloads
      | Type.Tuple ordered_types -> handle_ordered_types ordered_types on_type variance inj
      | Type.TypeOperation (Compose ordered_types) ->
          handle_ordered_types ordered_types on_type variance inj
      (* Recursive types cannot currently be generic. So we do nothing for variance. *)
      | Type.RecursiveType _ -> ()
      | _ -> ()
    in
    List.iter
      ~f:(fun x ->
        let element = AnnotatedAttribute.uninstantiated_annotation x in
        if not (String.equal (AnnotatedAttribute.name x) "__init__") then begin
          match element with
          | { AnnotatedAttribute.UninstantiatedAnnotation.kind = Attribute annotation; _ } ->
              let visibility = AnnotatedAttribute.visibility x in
              let variance =
                match visibility with
                | ReadOnly _ -> Type.Record.Variance.Covariant
                | ReadWrite ->
                    if AnnotatedAttribute.is_private_field x then
                      Covariant
                    else
                      Invariant
              in
              on_type ~variance ~inj:true ~typ:annotation
          | {
           AnnotatedAttribute.UninstantiatedAnnotation.kind =
             DecoratedMethod { undecorated_signature; _ };
           _;
          } ->
              on_type ~variance:Covariant ~inj:true ~typ:(Type.Callable undecorated_signature)
          | { AnnotatedAttribute.UninstantiatedAnnotation.kind = Property { getter; setter }; _ }
            -> (
              let call_on_type_co value =
                match value with
                | Some typ -> on_type ~variance:Covariant ~inj:true ~typ
                | _ -> ()
              in
              let call_on_type_contra value =
                match value with
                | Some typ -> on_type ~variance:Contravariant ~inj:true ~typ
                | _ -> ()
              in
              call_on_type_co getter.self;
              call_on_type_co getter.value;

              match setter with
              | Some setter ->
                  call_on_type_co setter.self;
                  call_on_type_contra setter.value
              | _ -> ())
        end)
      (to_list class_name);
    List.iter
      ~f:(fun typ -> on_type ~variance:Covariant ~inj:true ~typ)
      (get_base_class_types class_name)
end

(* a helper function which converts type variables to a string -> type variable map *)
let scoped_type_variables_as_map scoped_type_variables_list =
  let empty_string_map = Identifier.Map.empty in
  let update_map map key value =
    match Map.add ~key ~data:value map with
    | `Ok new_map -> new_map
    | `Duplicate -> map
  in
  let named_values = List.map ~f:(fun x -> Type.Variable.name x, x) scoped_type_variables_list in
  let create_mapping_from_type_variable_assoc_list =
    List.fold_left
      ~f:(fun map (key, value) -> update_map map key value)
      ~init:empty_string_map
      named_values
  in
  match named_values with
  | [] -> None
  | _ -> Some create_mapping_from_type_variable_assoc_list


(* A helper function which merges two maps by flatten the inner and outer scope, shadowing outer
   vars on collisions *)
let merge_scoped_type_variables ~inner_scope_type_variables ~outer_scope_type_variables =
  match inner_scope_type_variables, outer_scope_type_variables with
  | Some m1, Some m2 ->
      Some
        (Map.merge m1 m2 ~f:(fun ~key:_ -> function
           | `Left v1 -> Some v1
           | `Right v2 -> Some v2
           | `Both (v1, _) -> Some v1))
  | None, Some m2 -> Some m2
  | Some m1, _ -> Some m1
  | None, None -> None


module Queries = struct
  type t = {
    controls: EnvironmentControls.t;
    resolve_exports: ?from:Ast.Reference.t -> Ast.Reference.t -> ResolvedReference.t option;
    is_protocol: Type.t -> bool;
    get_unannotated_global: Ast.Reference.t -> Module.UnannotatedGlobal.t option;
    get_class_summary: string -> ClassSummary.t Ast.Node.t option;
    first_matching_class_decorator:
      names:string list -> ClassSummary.t Ast.Node.t -> Ast.Statement.Decorator.t option;
    exists_matching_class_decorator: names:string list -> ClassSummary.t Ast.Node.t -> bool;
    class_exists: string -> bool;
    parse_annotation_without_sanitizing_type_arguments:
      ?modify_aliases:(?replace_unbound_parameters_with_any:bool -> Type.t -> Type.t) ->
      variables:(string -> Type.Variable.t option) ->
      ?allow_untracked:bool ->
      Ast.Expression.t ->
      Type.t;
    param_spec_from_vararg_annotations:
      args_annotation:Ast.Expression.t ->
      kwargs_annotation:Ast.Expression.t ->
      unit ->
      Type.Variable.ParamSpec.t option;
    class_hierarchy: unit -> (module ClassHierarchy.Handler);
    generic_parameters_as_variables:
      ?empty_for_nongeneric:bool -> Type.Primitive.t -> Type.Variable.t list option;
    successors: Type.Primitive.t -> string list;
    get_class_metadata: Type.Primitive.t -> ClassSuccessorMetadataEnvironment.class_metadata option;
    is_typed_dictionary: Type.Primitive.t -> bool;
    extends_enum: Type.Primitive.t -> bool;
    has_transitive_successor: successor:Type.Primitive.t -> Type.Primitive.t -> bool;
    least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t option;
    get_variable: string -> Type.Variable.t option;
  }

  let class_summary_for_outer_type { get_class_summary; _ } annotation =
    Type.split annotation |> fst |> Type.primitive_name >>= get_class_summary
end

module Global = struct
  type t = {
    type_info: TypeInfo.Unit.t;
    undecorated_signature: Type.Callable.t option;
    problem: AnnotatedAttribute.problem option;
  }
  [@@deriving show, compare, sexp]
end

type resolved_define = (Type.t, AnnotatedAttribute.problem) Result.t

(* Given a publically-visible define (one not nested in another define - this includes module-level
   functions, methods, and methods of nested classes), get its qualified name.

   TODO(T199841372) This depends on qualification; we cannot use the new
   `FunctionDefinition.qualified_name_of_signature` because Pyre currently looks up public globals
   in a way that isn't aware of modules; we need to re-wire lookups on a module-aware key before we
   can stop relying on qualification here. *)
let callable_name_of_public ~implementation ~overloads =
  match implementation, overloads with
  | Some { Define.Signature.name; _ }, _
  | _, { Define.Signature.name; _ } :: _ ->
      Some name
  | None, [] -> None


let create_uninstantiated_method ?(accessed_via_metaclass = false) callable =
  {
    AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass;
    kind = Attribute (Callable callable);
  }


module UninstantiatedAttributeTable = struct
  type element = AnnotatedAttribute.UninstantiatedAnnotation.t AnnotatedAttribute.t
  [@@deriving compare]

  type table = (string, element) Stdlib.Hashtbl.t

  type t = {
    attributes: table;
    names: string list ref;
  }

  let create () = { attributes = Stdlib.Hashtbl.create 15; names = ref [] }

  let add { attributes; names } attribute =
    let name = AnnotatedAttribute.name attribute in
    if Stdlib.Hashtbl.mem attributes name then
      ()
    else (
      Stdlib.Hashtbl.add attributes name attribute;
      names := name :: !names)


  let mark_as_implicitly_initialized_if_uninitialized { attributes; _ } name =
    let is_uninitialized attribute =
      match AnnotatedAttribute.initialized attribute with
      | NotInitialized -> true
      | _ -> false
    in
    match Stdlib.Hashtbl.find_opt attributes name with
    | Some attribute when is_uninitialized attribute ->
        AnnotatedAttribute.with_initialized ~initialized:OnlyOnInstance attribute
        |> Stdlib.Hashtbl.replace attributes name
    | _ -> ()


  let lookup_name { attributes; _ } = Stdlib.Hashtbl.find_opt attributes

  let to_list { attributes; names } = List.rev_map !names ~f:(Stdlib.Hashtbl.find attributes)

  let compare ({ names = left_names; _ } as left) ({ names = right_names; _ } as right) =
    let left_names = !left_names in
    let right_names = !right_names in
    match List.compare String.compare left_names right_names with
    | 0 ->
        let rec compare_elements = function
          | [] -> 0
          | name :: names -> (
              match
                Option.compare compare_element (lookup_name left name) (lookup_name right name)
              with
              | 0 -> compare_elements names
              | nonzero -> nonzero)
        in
        compare_elements left_names
    | nonzero -> nonzero
end

(* These modules get included at the bottom of this file, they're just here for aesthetic
   purposes *)
module TypeParameterValidationTypes = struct
  type generic_type_problems =
    | IncorrectNumberOfParameters of {
        actual: int;
        expected: int;
        can_accept_more_parameters: bool;
      }
    | ViolateConstraints of {
        actual: Type.t;
        expected: Type.Variable.TypeVar.t;
      }
    | UnexpectedKind of {
        actual: Type.Argument.t;
        expected: Type.Variable.t;
      }
  [@@deriving compare, sexp, show, hash]

  type type_parameters_mismatch = {
    name: string;
    kind: generic_type_problems;
  }
  [@@deriving compare, sexp, show, hash]
end

type type_validation_policy =
  | NoValidation
  | ValidatePrimitives
  | ValidatePrimitivesAndTypeParameters
[@@deriving compare, sexp, show, hash]

let class_name { Node.value = { ClassSummary.name; _ }; _ } = name

module ParsingValidation = struct
  (** The environment controls can specify whether it wants to perform validation on annotated
      attributes where its class fails lookup. One example in which a class fails lookup is when a
      class is defined and imported in a missing file.

      The downstream result of performing validation on missing classes is that we will treat that
      attribute as having type Any or unknown, but if we skip that validation, we will preserve the
      class information of that attribute.

      This function extracts the value of the validation from the environment controls and returns a
      corresponding validation type. **)
  let parse_annotation_validation_kind controls =
    let no_validation_on_class_lookup_failure =
      EnvironmentControls.no_validation_on_class_lookup_failure controls
    in
    if no_validation_on_class_lookup_failure then
      NoValidation
    else
      ValidatePrimitivesAndTypeParameters
end

(* This function mutably updates an UninstantiatedAttributeTable.t if a class has any dataclass
   transforms (including @dataclass itself) applied to it. *)
let apply_dataclass_transforms_to_table
    ~queries:(Queries.{ get_class_summary; successors; _ } as queries)
    ~definition
    create_uninstantiated_attribute
    instantiate_attribute
    class_name
    table
    ~scoped_type_variables
  =
  let open Expression in
  let { Node.value = { ClassSummary.name; _ }; _ } = definition in
  let generate_attributes ~options =
    let already_in_table name =
      UninstantiatedAttributeTable.lookup_name table name |> Option.is_some
    in
    let make_attribute ~annotation ~attribute_name =
      AnnotatedAttribute.create_uninstantiated
        ~uninstantiated_annotation:
          {
            AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass = false;
            kind = Attribute annotation;
          }
        ~abstract:false
        ~async_property:false
        ~class_variable:false
        ~defined:true
        ~initialized:OnClass
        ~name:attribute_name
        ~parent:(Reference.show name)
        ~visibility:ReadWrite
        ~property:false
        ~undecorated_signature:None
    in
    let make_method ~parameters ~annotation ~attribute_name =
      let parameters =
        {
          Type.Callable.CallableParamType.name = "$parameter$self";
          annotation = Type.Primitive (Reference.show name);
          default = false;
        }
        :: parameters
      in
      let callable =
        {
          Type.Callable.kind = Named (Reference.combine name (Reference.create attribute_name));
          overloads = [];
          implementation =
            { annotation; parameters = Defined (Type.Callable.CallableParamType.create parameters) };
        }
      in
      AnnotatedAttribute.create_uninstantiated
        ~uninstantiated_annotation:
          {
            AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass = false;
            kind = Attribute (Callable callable);
          }
        ~abstract:false
        ~async_property:false
        ~class_variable:false
        ~defined:true
        ~initialized:OnClass
        ~name:attribute_name
        ~parent:(Reference.show name)
        ~visibility:ReadWrite
        ~property:false
        ~undecorated_signature:(Some callable)
    in
    match options definition with
    | None -> []
    | Some
        {
          DataclassOptions.init;
          repr;
          eq;
          order;
          match_args;
          field_specifiers;
          keyword_only = class_level_keyword_only;
          has_slots;
          frozen;
          _;
        } ->
        let is_class_var attribute =
          match Node.value attribute with
          | {
           Attribute.kind =
             Attribute.Simple
               {
                 annotation =
                   Some
                     {
                       Node.value =
                         Expression.Subscript
                           {
                             base =
                               {
                                 Node.value =
                                   Name
                                     ( Name.Attribute
                                         {
                                           attribute = "ClassVar";
                                           base =
                                             { Node.value = Name (Name.Identifier "typing"); _ };
                                           _;
                                         }
                                     | Name.Identifier "typing.ClassVar" );
                                 _;
                               };
                             _;
                           };
                       _;
                     };
                 _;
               };
           _;
          } ->
              false
          | _ -> true
        in
        let get_table_from_class_summary ({ Node.value = class_summary; _ } as parent) =
          let create attribute : AnnotatedAttribute.uninstantiated * Expression.t =
            let value =
              match attribute with
              | { Node.value = { Attribute.kind = Simple { values = { value; _ } :: _; _ }; _ }; _ }
                ->
                  value
              | { Node.location; _ } ->
                  Node.create (Expression.Constant Constant.Ellipsis) ~location
            in
            ( create_uninstantiated_attribute
                ~scoped_type_variables
                ~parent
                ?defined:None
                ~accessed_via_metaclass:false
                (Node.value attribute),
              value )
          in
          let compare_by_location left right =
            Ast.Location.compare (Node.location left) (Node.location right)
          in
          ClassSummary.attributes ~include_generated_attributes:false ~in_test:false class_summary
          |> Identifier.SerializableMap.bindings
          |> List.unzip
          |> snd
          |> List.filter ~f:is_class_var
          |> List.sort ~compare:compare_by_location
          |> List.map ~f:create
        in
        let attribute_tables =
          (Reference.show name
          |> successors
          |> List.filter_map ~f:get_class_summary
          |> List.filter ~f:(fun definition -> options definition |> Option.is_some)
          |> List.rev
          |> List.map ~f:get_table_from_class_summary)
          @ [get_table_from_class_summary definition]
        in
        let extract_dataclass_field_arguments (_, value) =
          match value with
          | { Node.value = Expression.Call { callee; arguments; _ }; _ } ->
              Option.some_if
                (List.exists field_specifiers ~f:(fun field_specifier ->
                     Int.equal
                       (Ast.Expression.location_insensitive_compare callee field_specifier)
                       0))
                arguments
          | _ -> None
        in
        let init_not_disabled attribute =
          let is_disable_init { Call.Argument.name; value = { Node.value; _ } } =
            match name, value with
            | Some { Node.value = parameter_name; _ }, Expression.Constant Constant.False
              when String.equal "init" (Identifier.sanitized parameter_name) ->
                true
            | _ -> false
          in
          match extract_dataclass_field_arguments attribute with
          | Some arguments -> not (List.exists arguments ~f:is_disable_init)
          | _ -> true
        in
        let extract_init_value (attribute, value) =
          let initialized = AnnotatedAttribute.initialized attribute in
          let get_default_value { Call.Argument.name; value } =
            name
            >>| Node.value
            >>| Identifier.sanitized
            >>= function
            | "default" -> Some value
            | "default_factory"
            | "factory" ->
                let { Node.location; _ } = value in
                Some
                  {
                    Node.value =
                      Expression.Call
                        {
                          Call.callee = value;
                          arguments = [];
                          origin = Some { Node.location; value = Origin.DataclassImplicitDefault };
                        };
                    location;
                  }
            | _ -> None
          in
          match initialized with
          | NotInitialized -> None
          | _ -> (
              match extract_dataclass_field_arguments (attribute, value) with
              | Some arguments -> List.find_map arguments ~f:get_default_value
              | _ -> Some value)
        in
        let get_is_field_level_keyword_only (attribute, value) =
          let initialized = AnnotatedAttribute.initialized attribute in
          let get_is_keyword_only { Call.Argument.name; value } =
            name
            >>| Node.value
            >>| Identifier.sanitized
            >>= function
            | "kw_only" -> (
                match value with
                | { Node.location = _; Node.value = Expression.Constant Constant.True } -> Some true
                | _ -> Some false)
            | _ -> None
          in
          match initialized with
          | NotInitialized -> None
          | _ -> (
              match extract_dataclass_field_arguments (attribute, value) with
              | Some arguments -> List.find_map arguments ~f:get_is_keyword_only
              | _ -> None)
        in
        let split_parameters_by_keyword_only parameters =
          let keyword_only, not_keyword_only =
            List.partition_tf parameters ~f:(function
                | { DataclassOptions.name = _; annotation = _; default = _; keyword_only } ->
                keyword_only)
          in
          let dataclass_constructor_to_named { DataclassOptions.name; annotation; default; _ }
              : Type.t Callable.CallableParamType.named
            =
            { name; annotation; default }
          in
          let keyword_only_named = List.map keyword_only ~f:dataclass_constructor_to_named in
          let not_keyword_only_named =
            List.map not_keyword_only ~f:dataclass_constructor_to_named
          in
          match keyword_only_named, not_keyword_only_named with
          | [], not_keyword_only -> not_keyword_only
          | keyword_only, not_keyword_only ->
              not_keyword_only
              @ [Type.Callable.CallableParamType.dummy_star_parameter]
              @ keyword_only
        in
        (* A central method that processes parameters that abstracts over constructing methods. It
           specializes on __init__ and __match_args__. *)
        let process_parameters ~implicitly_initialize process_potential_initvar_annotation =
          let collect_parameters parameters (attribute, value) =
            (* Parameters must be annotated attributes *)
            let original_annotation =
              instantiate_attribute attribute
              |> AnnotatedAttribute.annotation
              |> TypeInfo.Unit.original
            in
            let annotation, is_initvar_for_init =
              process_potential_initvar_annotation original_annotation
            in
            match AnnotatedAttribute.name attribute with
            | name when not (Type.contains_unknown annotation) ->
                if implicitly_initialize && not is_initvar_for_init then
                  UninstantiatedAttributeTable.mark_as_implicitly_initialized_if_uninitialized
                    table
                    name;
                let name = "$parameter$" ^ name in
                let init_value = extract_init_value (attribute, value) in
                let keyword_only =
                  match get_is_field_level_keyword_only (attribute, value) with
                  | Some value -> value
                  | None -> class_level_keyword_only
                in
                let rec override_existing_parameters
                    (unchecked_parameters :
                      Type.t DataclassOptions.dataclass_constructor_parameter list)
                  =
                  match unchecked_parameters with
                  | [] ->
                      [
                        {
                          DataclassOptions.name;
                          annotation;
                          default = Option.is_some init_value;
                          keyword_only;
                        };
                      ]
                  | { name = old_name; default = old_default; _ } :: tail
                    when Identifier.equal old_name name ->
                      {
                        name;
                        annotation;
                        default = Option.is_some init_value || old_default;
                        keyword_only;
                      }
                      :: tail
                  | head :: tail -> head :: override_existing_parameters tail
                in
                override_existing_parameters parameters
            | _ -> parameters
          in
          attribute_tables
          |> List.map ~f:(List.filter ~f:init_not_disabled)
          |> List.fold ~init:[] ~f:(fun parameters ->
                 List.fold ~init:parameters ~f:collect_parameters)
          |> split_parameters_by_keyword_only
        in
        (* generate parameters for __match_args__. InitVar parameters should be excluded. *)
        let match_parameters ~implicitly_initialize =
          process_parameters ~implicitly_initialize (fun original_annotation ->
              original_annotation, false)
        in
        (* generate parameters for __init__. InitVar parameters should be included in the parameter
           list but not initialized *)
        let init_parameters ~implicitly_initialize =
          process_parameters ~implicitly_initialize (fun original_annotation ->
              match original_annotation with
              | Type.Parametric
                  { name = "dataclasses.InitVar"; arguments = [Single single_parameter] } ->
                  single_parameter, true
              | _ -> original_annotation, false)
        in
        (* Override the attribute table with respect to frozen attributes *)
        let handle_frozen_attributes table frozen =
          let frozen_attributes attribute name =
            if frozen then begin
              let frozen_visibility =
                AnnotatedAttribute.ReadOnly (AnnotatedAttribute.Refinable { overridable = true })
              in
              let frozen_attribute =
                AnnotatedAttribute.with_visibility attribute ~visibility:frozen_visibility
              in
              Stdlib.Hashtbl.replace table name frozen_attribute
            end
          in
          Stdlib.Hashtbl.iter (fun name attribute -> frozen_attributes attribute name) table
        in
        handle_frozen_attributes table.attributes frozen;
        (* We are unable to use init_parameters because slots items can have different values
         * for ancestors and we do not want the dummy star argument.
         * TODO(T130663259) Inaccurate for ancestors *)
        let slots_items =
          if has_slots then
            attribute_tables
            |> List.concat
            |> List.map ~f:(fun (attribute, _) -> AnnotatedAttribute.name attribute)
            |> List.dedup_and_sort ~compare:Identifier.compare
          else
            []
        in
        let methods =
          if init && not (already_in_table "__init__") then
            [
              make_method
                ~parameters:(init_parameters ~implicitly_initialize:true)
                ~annotation:Type.none
                ~attribute_name:"__init__";
            ]
          else
            []
        in
        let methods =
          if repr && not (already_in_table "__repr__") then
            let new_method =
              make_method ~parameters:[] ~annotation:Type.string ~attribute_name:"__repr__"
            in
            new_method :: methods
          else
            methods
        in
        let add_order_method methods name =
          let annotation =
            match name with
            | "__eq__" -> Type.object_primitive
            | _ -> Type.Primitive class_name
          in
          if not (already_in_table name) then
            make_method
              ~parameters:[{ name = "$parameter$o"; annotation; default = false }]
              ~annotation:Type.bool
              ~attribute_name:name
            :: methods
          else
            methods
        in
        let methods =
          if eq then
            add_order_method methods "__eq__"
          else
            methods
        in
        let methods =
          if order then
            ["__lt__"; "__le__"; "__gt__"; "__ge__"] |> List.fold ~init:methods ~f:add_order_method
          else
            methods
        in
        let methods =
          if match_args && not (already_in_table "__match_args__") then
            let parameter_name { Callable.CallableParamType.name; _ } = Identifier.sanitized name in
            let params = match_parameters ~implicitly_initialize:false in
            let init_parameter_names = List.map ~f:parameter_name params in
            let literal_string_value_type name = Type.Literal (String (LiteralValue name)) in
            let annotation =
              Type.tuple (List.map ~f:literal_string_value_type init_parameter_names)
            in
            make_attribute ~annotation ~attribute_name:"__match_args__" :: methods
          else
            methods
        in
        let methods =
          if (not (List.is_empty slots_items)) && not (already_in_table "__slots__") then
            make_attribute
              ~annotation:(Type.tuple (List.map slots_items ~f:(fun _ -> Type.string)))
              ~attribute_name:"__slots__"
            :: methods
          else
            methods
        in
        methods
  in
  let dataclass_attributes () =
    (* TODO (T43210531): Warn about inconsistent annotations
     * TODO (T131540506): Decouple dataclass options from other options *)
    let Queries.{ first_matching_class_decorator; _ } = queries in

    generate_attributes
      ~options:(DataclassOptions.dataclass_options ~first_matching_class_decorator)
  in
  let attrs_attributes () =
    (* TODO (T41039225): Add support for other methods
     * TODO (T129741558): support type annotations in attr *)
    let Queries.{ first_matching_class_decorator; _ } = queries in
    generate_attributes ~options:(DataclassOptions.attrs_attributes ~first_matching_class_decorator)
  in
  let dataclass_transform_attributes () =
    let Queries.{ get_unannotated_global; _ } = queries in
    generate_attributes
      ~options:
        (DataclassOptions.options_from_custom_dataclass_transform_decorator ~get_unannotated_global)
  in
  let dataclass_transform_class_attributes () =
    generate_attributes
      ~options:
        (DataclassOptions.options_from_custom_dataclass_transform_base_class_or_metaclass
           ~get_class_summary
           ~successors)
  in
  dataclass_attributes ()
  @ attrs_attributes ()
  @ dataclass_transform_attributes ()
  @ dataclass_transform_class_attributes ()
  |> List.iter ~f:(UninstantiatedAttributeTable.add table)


let partial_apply_self { Type.Callable.implementation; overloads; _ } ~order ~self_type =
  let open Type.Callable in
  let implementation, overloads =
    match implementation, overloads with
    | { Type.Callable.parameters = Defined (Named { annotation; _ } :: _); _ }, _ -> (
        let solution =
          try
            TypeOrder.OrderedConstraintsSet.add_and_simplify
              ConstraintsSet.empty
              ~new_constraint:(LessOrEqual { left = self_type; right = annotation })
              ~order
            |> TypeOrder.OrderedConstraintsSet.solve ~order
            |> Option.value ~default:TypeConstraints.Solution.empty
          with
          | ClassHierarchy.Untracked _ -> TypeConstraints.Solution.empty
        in
        let instantiated =
          TypeConstraints.Solution.instantiate
            solution
            (Type.Callable { kind = Anonymous; implementation; overloads })
        in
        match instantiated with
        | Type.Callable { implementation; overloads; _ } -> implementation, overloads
        | _ -> implementation, overloads)
    | _ -> implementation, overloads
  in
  let drop_self { Type.Callable.annotation; parameters } =
    let parameters =
      match parameters with
      | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
      | FromParamSpec { head = _ :: head; variable } -> FromParamSpec { head; variable }
      | _ -> parameters
    in
    { Type.Callable.annotation; parameters }
  in
  {
    Type.Callable.kind = Anonymous;
    implementation = drop_self implementation;
    overloads = List.map overloads ~f:drop_self;
  }


let callable_call_special_cases
    ~type_for_lookup
    ~class_name
    ~attribute_name
    ~order
    ~accessed_through_class
  =
  match type_for_lookup, class_name, attribute_name, accessed_through_class with
  | Some (Type.Callable _), "typing.Callable", "__call__", false -> type_for_lookup
  | ( Some
        (Parametric
          { name = "BoundMethod"; arguments = [Single (Callable callable); Single self_type] }),
      "typing.Callable",
      "__call__",
      false ) ->
      let order = order () in
      partial_apply_self callable ~order ~self_type
      |> fun callable -> Type.Callable callable |> Option.some
  | _ -> None


class base ~queries:(Queries.{ controls; get_class_summary; class_hierarchy; _ } as queries) =
  object (self)
    (* Given a `Type.t`, recursively search for parameteric forms with invalid
     * type arguments. If we find such arguments:
     * - (validate) record the problems in a
     *   `TypeParameterValidationTypes.type_parameters_mismatch list`
     * - (sanitize) convert any problematic arguments to gradual forms (e.g.
     *   `Any` for unary type parameters, `...` for param specs)
     *
     * The `parse_annotation` code relies on (sanitize), and `typeCheck.ml`
     * relies on (validate) to eventually produce user-facing errors on bad
     * annotations.
     *)
    method validate_and_sanitize_type_arguments
        ?(replace_unbound_parameters_with_any = true)
        ~cycle_detections
        annotation =
      let Queries.{ generic_parameters_as_variables; _ } = queries in
      let open TypeParameterValidationTypes in
      let module InvalidTypeParametersTransform = Type.VisitWithTransform.Make (struct
        type state = type_parameters_mismatch list

        let visit_children_before _ _ = false

        let visit_children_after = true

        let visit sofar annotation =
          let transformed_annotation, new_state =
            let generics_for_name name =
              match name with
              | "type"
              | "typing.Type"
              | "typing.ClassVar"
              | "typing.Iterator"
              | "Optional"
              | "typing.Final"
              | "typing_extensions.Final"
              | "typing.Optional" ->
                  [Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "T")]
              | "typing.Annotated"
              | "typing_extensions.Annotated" ->
                  [
                    Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "T1");
                    Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "T2");
                    Type.Variable.TypeVarTupleVariable (Type.Variable.TypeVarTuple.create "Ts");
                  ]
              | "typing.Callable" ->
                  [
                    Type.Variable.ParamSpecVariable (Type.Variable.ParamSpec.create "Ps");
                    Type.Variable.TypeVarVariable (Type.Variable.TypeVar.create "R");
                  ]
              | _ -> generic_parameters_as_variables name |> Option.value ~default:[]
            in
            let invalid_type_arguments ~name ~given =
              let generics = generics_for_name name in
              match
                Type.Variable.zip_variables_with_arguments_including_mismatches
                  ~arguments:given
                  generics
              with
              | Some [] -> Type.Primitive name, sofar
              | Some paired ->
                  let check_argument { Type.Variable.variable_pair; received_argument } =
                    match variable_pair, received_argument with
                    | Type.Variable.TypeVarPair (unary, given), Type.Argument.Single _ ->
                        let invalid =
                          let order = self#full_order ~cycle_detections in
                          TypeOrder.OrderedConstraints.add_lower_bound
                            TypeConstraints.empty
                            ~order
                            ~pair:variable_pair
                          >>| TypeOrder.OrderedConstraints.add_upper_bound
                                ~order
                                ~pair:variable_pair
                          |> Option.is_none
                        in
                        if invalid then
                          ( [Type.Argument.Single Type.Any],
                            Some
                              {
                                name;
                                kind = ViolateConstraints { expected = unary; actual = given };
                              } )
                        else
                          [Type.Argument.Single given], None
                    | ParamSpecPair (_, given), (CallableParameters _ | Single (Primitive "...")) ->
                        [CallableParameters given], None
                    | TypeVarTuplePair (_, given), Single (Tuple _) ->
                        Type.OrderedTypes.to_arguments given, None
                    | Type.Variable.TypeVarPair (unary, given), _ ->
                        ( [Single given],
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = Type.Variable.TypeVarVariable unary;
                                    actual = received_argument;
                                  };
                            } )
                    | ParamSpecPair (param_spec, given), _ ->
                        ( [CallableParameters given],
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = ParamSpecVariable param_spec;
                                    actual = received_argument;
                                  };
                            } )
                    | TypeVarTuplePair (type_var_tuple, given), _ ->
                        ( Type.OrderedTypes.to_arguments given,
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = TypeVarTupleVariable type_var_tuple;
                                    actual = received_argument;
                                  };
                            } )
                  in
                  List.map paired ~f:check_argument
                  |> List.unzip
                  |> fun (list_of_arguments, errors) ->
                  ( Type.parametric name (List.concat list_of_arguments),
                    List.filter_map errors ~f:Fn.id @ sofar )
              | None when not replace_unbound_parameters_with_any ->
                  (* There is *exactly* one place in Pyre where
                     `replace_unbound_parameters_with_any` would be set to `false`, which is when we
                     are resolving a parametric invocation of a type alias.

                     In this case, if the alias itself is structually invalid (so that the zip
                     fails), then we treat the alias "as if" it were a stand in for the target
                     directly, with the same type vars.

                     The purpose of this is to handle aliases that are bare names of generic
                     targets, e.g. `MyDictAlias = dict`. Aliases like this are implicitly generic;
                     they have no type variables but they inherit the generic structure of their
                     target, and the `Type.create` code wants to resolve the type arguments of the
                     alias as if they were type arguments of the target itself, e.g. treat
                     `MyDictAlias[int, str]` as if it were `dict[int, str]`.

                     That scenario hits this branch, because when we zip the type arguments of the
                     bare `dict` (an empty list) against the type parameters of `dict` (which has
                     two type parameters) the zip fails. So if the
                     `replace_unbound_parameters_with_any` flag is set, we just return the
                     parameterized target `dict[K, V]`, which allows `Type.create` to match `int` to
                     `K` and `str` to `V`.

                     Note that because we are doing this implicitly via a zip failure rather than
                     explicitly when the alias is a primitive form, we will also treat a
                     structurally invalid alias the same way. In other words, `OopsDictAlias =
                     dict[str, int, float]` will follow exactly the same code path as `MyDictAlias =
                     dict`. This is probably unintended. *)
                  Type.parametric name (List.map generics ~f:Type.Variable.to_argument), sofar
              | None ->
                  let annotation, expected_parameter_count, can_accept_more_parameters =
                    match name with
                    | "typing.Callable" ->
                        Type.Callable.create ~annotation:Type.Any (), List.length generics, false
                    | "tuple" ->
                        (* Note: this case can only be hit by a bare `tuple` or `typing.Tuple`
                           because the generic form gets converted to Type.Tuple as part of
                           Type.create, and this logic is only run against `Primitive` and
                           `Parametric` run against `Type.Tuple`. *)
                        ( Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Any),
                          List.length generics,
                          true )
                    | _ ->
                        let is_tuple_variadic = function
                          | Type.Variable.TypeVarTupleVariable _ -> true
                          | _ -> false
                        in
                        let annotation =
                          Type.parametric
                            name
                            (List.concat_map generics ~f:(function
                                | Type.Variable.TypeVarVariable _ -> [Type.Argument.Single Type.Any]
                                | ParamSpecVariable _ -> [CallableParameters Undefined]
                                | TypeVarTupleVariable _ ->
                                    Type.OrderedTypes.to_arguments Type.Variable.TypeVarTuple.any))
                        in
                        ( annotation,
                          List.filter generics ~f:(fun x -> not (is_tuple_variadic x))
                          |> List.length,
                          List.exists generics ~f:is_tuple_variadic )
                  in
                  let mismatch =
                    {
                      name;
                      kind =
                        IncorrectNumberOfParameters
                          {
                            actual = List.length given;
                            expected = expected_parameter_count;
                            can_accept_more_parameters;
                          };
                    }
                  in
                  annotation, mismatch :: sofar
            in
            match annotation with
            | Type.Primitive ("typing.Final" | "typing_extensions.Final") -> annotation, sofar
            | Type.Primitive name -> invalid_type_arguments ~name ~given:[]
            (* natural variadics *)
            | Type.Parametric { name = "typing.Protocol"; _ }
            | Type.Parametric { name = "typing.Generic"; _ } ->
                annotation, sofar
            | Type.Parametric { name; arguments } -> invalid_type_arguments ~name ~given:arguments
            | _ -> annotation, sofar
          in
          { Type.VisitWithTransform.transformed_annotation; new_state }
      end)
      in
      InvalidTypeParametersTransform.visit [] annotation

    (* Convert a type expression of type `Expression.t` to a `Type.t`.
     *
     * In the process several sanitization phases happen:
     * - TypeAliasEnvironment will verify that all types map to valid classes
     *   (Pyre incorrectly assumes all types are classes; we use `missingFromStubs.ml`
     *   to fabricate classes for special forms that aren't really classes).
     * - TypeAliasEnvironment will expand type aliases eagerly.
     * - The `validate_and_sanitize_type_arguments` transform will convert invalid
     *   type arguments (which can be invalid because of arity mismatches or because
     *   they fail to meet constraints) to gradual forms.
     *)
    method parse_annotation
        ~cycle_detections
        ?(validation = ParsingValidation.parse_annotation_validation_kind controls)
        ~(scoped_type_variables : Type.Variable.t Identifier.Map.t option)
        expression =
      let { Queries.parse_annotation_without_sanitizing_type_arguments; get_variable; _ } =
        queries
      in
      let variables_before_modify =
        match scoped_type_variables with
        | None -> get_variable
        | Some variable_map -> (
            fun variable_name ->
              let local_scope_variable = map_find variable_map variable_name in
              match local_scope_variable with
              | None -> get_variable variable_name
              | Some variable -> Some variable)
      in
      let modify_variables ?replace_unbound_parameters_with_any = function
        | Type.Variable.TypeVarVariable variable ->
            let visited_variable =
              self#validate_and_sanitize_type_arguments
                ?replace_unbound_parameters_with_any
                (Variable variable)
                ~cycle_detections
              |> snd
            in
            begin
              match visited_variable with
              | Variable variable -> Type.Variable.TypeVarVariable variable
              | _ ->
                  failwith
                    "Impossible: validate_and_sanitize_type_arguments received a variable variant \
                     as input and output a different variant."
            end
        | result -> result
      in
      let variables ?replace_unbound_parameters_with_any name =
        variables_before_modify name >>| modify_variables ?replace_unbound_parameters_with_any
      in
      let modify_aliases ?replace_unbound_parameters_with_any = function
        | alias ->
            self#validate_and_sanitize_type_arguments
              ?replace_unbound_parameters_with_any
              alias
              ~cycle_detections
            |> snd
      in
      let allow_untracked =
        match validation with
        | NoValidation -> true
        | ValidatePrimitives
        | ValidatePrimitivesAndTypeParameters ->
            false
      in
      let annotation =
        parse_annotation_without_sanitizing_type_arguments
          ~modify_aliases
          ~variables
          ~allow_untracked
          expression
      in
      let result =
        match validation with
        | ValidatePrimitivesAndTypeParameters ->
            self#validate_and_sanitize_type_arguments annotation ~cycle_detections |> snd
        | NoValidation
        | ValidatePrimitives ->
            annotation
      in
      result

    (* Given a define signature (which arrives in the form of an overloads list plus
     * an optional implementation - the implementation is optional for stubs), produce
     * an `AnnotatedAttribute.decorated_method` value that tracks:
     * - the type of the raw function signature without resolving decorators
     * - all of the decorators that are consistently applied (which are resolved later)
     * - an `AnnotatedAttribute.problem` whenever decorators are inconsistent across overloads
     *
     * How this relates to other parts of AttributeResolution and TypeCheck:
     * - uninstantiated attribute tables store method types in this form, which allows
     *   us to defer the expensive work of resolving and applying decorators and gives
     *   much better cache performance.
     * - Decorators can modify the signature; that logic is handled downstream in the
     *   `apply_decorators` function.
     * - The `resolve_define` function combines both steps into one; this is what
     *   is used both for resolving globals and inside of TypeCheck to get the types
     *   of locally-defined functions.
     *)
    method resolve_define_undecorated
        ~cycle_detections
        ~callable_name
        ~(implementation : Ast.Statement.Define.Signature.t option)
        ~overloads
        ~scoped_type_variables:outer_scope_type_variables =
      let Queries.
            {
              param_spec_from_vararg_annotations;
              generic_parameters_as_variables;
              parse_annotation_without_sanitizing_type_arguments;
              _;
            }
        =
        queries
      in
      let parse (signature : Define.Signature.t) =
        (* merge the parameters on top of the existing scope (ex. classes) *)
        let scoped_type_variables =
          (* collect our local type variables from our function defintion *)
          let local_type_parameters =
            let type_param_names =
              List.map
                ~f:
                  (Type.Variable.of_ast_type_param
                     ~create_type:
                       (parse_annotation_without_sanitizing_type_arguments
                          ?modify_aliases:None
                          ?allow_untracked:None
                          ~variables:Type.resolved_empty_variables))
                signature.type_params
            in
            type_param_names
          in
          let local_scoped_type_variables = scoped_type_variables_as_map local_type_parameters in
          merge_scoped_type_variables
            ~inner_scope_type_variables:local_scoped_type_variables
            ~outer_scope_type_variables
        in
        (* We modify the original param_spec_from_vararg_annotations from TypeAliasEnvironment to a
           version which accounts for scoped type variables. Here, we are turning
           scoped_type_variables to a get_variable and if we don't find a result, we revert to the
           original version of param_spec_from_vararg_annotations, which will use the global
           scope. *)
        let param_spec_from_vararg_annotations ~args_annotation ~kwargs_annotation =
          let local_get_variable =
            match scoped_type_variables with
            | Some variable_map -> (
                fun variable_name ->
                  let local_scope_variable = map_find variable_map variable_name in
                  match local_scope_variable with
                  | None -> None
                  | Some variable -> Some variable)
            | _ -> fun _ -> None
          in
          let get_param_spec variable_name =
            match local_get_variable variable_name with
            | Some (Type.Variable.ParamSpecVariable name) -> Some name
            | _ -> None
          in
          let res =
            Type.Variable.ParamSpec.of_component_annotations
              ~get_param_spec
              ~args_annotation:(Expression.delocalize args_annotation)
              ~kwargs_annotation:(Expression.delocalize kwargs_annotation)
          in
          match res with
          | Some res -> Some res
          | _ -> param_spec_from_vararg_annotations ~args_annotation ~kwargs_annotation ()
        in
        let parser =
          {
            AnnotatedCallable.parse_annotation =
              self#parse_annotation ~cycle_detections ~scoped_type_variables;
            param_spec_from_vararg_annotations;
          }
        in
        AnnotatedCallable.create_overload_without_applying_decorators
          ~parser
          ~generic_parameters_as_variables
          signature
      in
      let kind =
        match callable_name with
        | Some name -> Callable.Named name
        | None -> Callable.Anonymous
      in
      let undefined_overload =
        { Type.Callable.annotation = Type.Top; parameters = Type.Callable.Undefined }
      in
      let parsed_overloads, parsed_implementation, decorators =
        match overloads, implementation with
        | ( ({ Ast.Statement.Define.Signature.decorators = head_decorators; _ } as overload) :: tail,
            _ ) ->
            let filter_overload_decorator =
              let is_not_overload_decorator decorator =
                not
                  (Ast.Statement.Define.Signature.is_overloaded_function
                     { overload with Ast.Statement.Define.Signature.decorators = [decorator] })
              in
              List.filter ~f:is_not_overload_decorator
            in
            let enforce_equality ~parsed ~current sofar =
              let equal left right =
                Int.equal (Ast.Expression.location_insensitive_compare left right) 0
              in
              if List.equal equal sofar (filter_overload_decorator current) then
                Ok sofar
              else
                Error (AnnotatedAttribute.DifferingDecorators { offender = parsed })
            in
            let reversed_parsed_overloads, decorators =
              let collect
                  (reversed_parsed_overloads, decorators_sofar)
                  ({ Define.Signature.decorators = current; _ } as overload)
                =
                let parsed = parse overload in
                ( parsed :: reversed_parsed_overloads,
                  Result.bind decorators_sofar ~f:(enforce_equality ~parsed ~current) )
              in
              List.fold
                tail
                ~f:collect
                ~init:([parse overload], Result.Ok (filter_overload_decorator head_decorators))
            in
            let parsed_implementation, decorators =
              match implementation with
              | Some ({ Ast.Statement.Define.Signature.decorators = current; _ } as implementation)
                ->
                  let parsed = parse implementation in
                  Some parsed, Result.bind decorators ~f:(enforce_equality ~parsed ~current)
              | None -> None, decorators
            in
            List.rev reversed_parsed_overloads, parsed_implementation, decorators
        | [], Some { decorators; _ } -> [], implementation >>| parse, Result.Ok decorators
        | [], None -> [], None, Ok []
      in
      let undecorated_signature =
        {
          Type.Callable.implementation =
            parsed_implementation |> Option.value ~default:undefined_overload;
          overloads = parsed_overloads;
          kind;
        }
      in
      { AnnotatedAttribute.undecorated_signature; decorators }

    (* Given the name of a class, return the metaclass as a `Type.t`.
     *
     * This requires a specific search order, see
     * https://docs.python.org/3/reference/datamodel.html#determining-the-appropriate-metaclass
     *
     * This method is mostly self-explanatory but Pyre's handling of `GenericMeta`, which hasn't
     * even existed in the runtime for several years, is special: in `missingFromStubs.ml` we
     * make up `GenericMeta` as a fake metaclass, and then we treat a class as having this
     * metaclass only if `Generic` is a *direct* parent of the current class, without checking
     * all ancestors as we would for normal metaclass resolution.
     *)
    method metaclass ~cycle_detections target =
      let Queries.{ get_class_summary; _ } = queries in
      let type_params =
        match get_class_summary target with
        | None -> []
        | Some class_summary -> class_summary.value.type_params
      in
      let rec handle
          ({ Node.value = { ClassSummary.bases = { base_classes; metaclass; _ }; _ }; _ } as
          original)
        =
        let open Expression in
        let parse_annotation =
          self#parse_annotation ~cycle_detections ?validation:None ~scoped_type_variables:None
        in
        let metaclass_candidates =
          let explicit_metaclass = metaclass >>| parse_annotation in
          let metaclass_of_bases =
            let explicit_bases =
              let base_to_class base_expression =
                delocalize base_expression |> parse_annotation |> Type.split |> fst
              in
              base_classes
              |> List.map ~f:base_to_class
              |> List.filter_map ~f:(Queries.class_summary_for_outer_type queries)
              |> List.filter ~f:(fun base_class ->
                     not ([%compare.equal: ClassSummary.t Node.t] base_class original))
            in
            let filter_generic_meta base_metaclasses =
              if
                List.exists
                  ~f:(fun base ->
                    Reference.equal (Reference.create "typing.Generic") (class_name base))
                  explicit_bases
              then
                base_metaclasses
              else
                List.filter
                  ~f:(fun metaclass ->
                    not (Type.equal (Type.Primitive "typing.GenericMeta") metaclass))
                  base_metaclasses
            in
            explicit_bases |> List.map ~f:handle |> filter_generic_meta
          in
          match explicit_metaclass with
          | Some metaclass -> metaclass :: metaclass_of_bases
          | None -> metaclass_of_bases
        in
        let metaclass_candidates =
          match type_params with
          | [] -> metaclass_candidates
          | _ -> Type.Primitive "typing.GenericMeta" :: metaclass_candidates
        in
        match metaclass_candidates with
        | [] -> Type.Primitive "type"
        | first :: candidates -> (
            let order = self#full_order ~cycle_detections in
            let candidate = List.fold candidates ~init:first ~f:(TypeOrder.meet order) in
            match candidate with
            | Type.Bottom ->
                (* If we get Bottom here, we don't have a "most derived metaclass", so default to
                   one. *)
                first
            | _ -> candidate)
      in
      get_class_summary target >>| handle

    (* Create an uninstantiated attribute. This operation essentially
     * "lifts" the ClassSummary.Attribute.t type to the type level, producing
     * an `AnnotatedAttribute.uninstantiated` value.
     *
     * Note that for methods, decorators are not yet applied in the resulting
     * type, we track the callable type of the undecorated signature but defer
     * applying decorators until instantiation.
     *)
    method create_uninstantiated_attribute
        ~scoped_type_variables
        ~cycle_detections
        ~parent
        ?(defined = true)
        ~accessed_via_metaclass
        ({ Attribute.name = attribute_name; kind } as attribute) =
      let Queries.{ exists_matching_class_decorator; successors; extends_enum; _ } = queries in
      let { Node.value = { ClassSummary.name = parent_name; _ }; _ } = parent in
      let parent_name = Reference.show parent_name in
      let class_annotation = Type.Primitive parent_name in
      let is_enum =
        let metaclass_extends_enummeta class_name =
          match self#metaclass ~cycle_detections class_name with
          | Some metaclass_type ->
              let metaclass_name = Type.class_name metaclass_type |> Reference.show_sanitized in
              let metaclass_superclasses = successors metaclass_name |> String.Set.of_list in
              not
                (Set.is_empty
                   (Set.inter
                      (String.Set.of_list ["enum.EnumMeta"; "enum.EnumType"])
                      metaclass_superclasses))
          | _ -> false
        in
        (not (Set.mem Recognized.enumeration_classes (Type.show class_annotation)))
        && (metaclass_extends_enummeta parent_name || extends_enum parent_name)
      in
      let annotation, class_variable, visibility, undecorated_signature =
        match kind with
        | Simple { annotation; values; toplevel; _ } ->
            let value = List.hd values >>| fun { value; _ } -> value in
            let parsed_annotation =
              annotation >>| self#parse_annotation ~cycle_detections ~scoped_type_variables
            in
            (* Account for class attributes. *)
            let annotation, final, class_variable =
              parsed_annotation
              >>| (fun annotation ->
                    let process_class_variable annotation =
                      match Type.class_variable_value annotation with
                      | Some annotation -> true, annotation
                      | None -> false, annotation
                    in
                    match Type.final_value annotation with
                    | `NoArgument -> None, true, false
                    | `NotFinal ->
                        let is_class_variable, annotation = process_class_variable annotation in
                        Some annotation, false, is_class_variable
                    | `Ok annotation ->
                        let is_class_variable, annotation = process_class_variable annotation in
                        Some annotation, true, is_class_variable)
              |> Option.value ~default:(None, false, false)
            in
            let visibility =
              if final then
                AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
              else
                ReadWrite
            in
            (* Try resolve to tuple of string literal types for __match_args__ *)
            let annotation =
              let open Expression in
              match attribute_name, annotation, value with
              | "__match_args__", None, Some { Node.value = Expression.Tuple elements; _ } ->
                  let string_literal_value_to_type = function
                    | {
                        Node.value =
                          Expression.Constant
                            (Constant.String { StringLiteral.kind = String; value });
                        _;
                      } ->
                        Some (Type.Literal (String (LiteralValue value)))
                    | _ -> None
                  in
                  List.map elements ~f:string_literal_value_to_type |> Option.all >>| Type.tuple
              | _ -> annotation
            in
            let annotation, visibility =
              match annotation, value with
              | _, Some value when is_enum && Attribute.may_be_enum_member attribute ->
                  let literal_value_annotation =
                    self#resolve_literal ~scoped_type_variables ~cycle_detections value
                  in
                  let is_enum_member =
                    match literal_value_annotation with
                    | Type.Primitive "enum.nonmember" -> false
                    | _ -> true
                  in
                  if is_enum_member then
                    ( Type.Literal
                        (Type.EnumerationMember
                           { enumeration_type = class_annotation; member_name = attribute_name }),
                      AnnotatedAttribute.ReadOnly (Refinable { overridable = true }) )
                  else if (not (Type.is_partially_typed literal_value_annotation)) && toplevel then
                    Option.value annotation ~default:literal_value_annotation, visibility
                  else
                    Option.value annotation ~default:Type.Top, visibility
              | Some annotation, _ -> annotation, visibility
              | None, Some value ->
                  let literal_value_annotation =
                    self#resolve_literal ~scoped_type_variables ~cycle_detections value
                  in
                  let is_dataclass_attribute =
                    exists_matching_class_decorator
                      ~names:["dataclasses.dataclass"; "dataclass"]
                      parent
                  in
                  if
                    (not (Type.is_partially_typed literal_value_annotation))
                    && (not is_dataclass_attribute)
                    && toplevel
                  then (* Treat literal attributes as having been explicitly annotated. *)
                    literal_value_annotation, visibility
                  else
                    Type.Top, visibility
              | _ -> Type.Top, visibility
            in
            ( AnnotatedAttribute.UninstantiatedAnnotation.Attribute annotation,
              class_variable,
              visibility,
              None )
        | Method { signatures; final; _ } ->
            (* Handle Callables *)
            let visibility =
              if final then
                AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
              else
                ReadWrite
            in
            let callable, undecorated_signature =
              let overloads =
                let create_overload define =
                  Define.Signature.is_overloaded_function define, define
                in
                List.map signatures ~f:create_overload
              in
              let implementation, overloads =
                let to_signature (implementation, overloads) (is_overload, signature) =
                  if is_overload then
                    implementation, signature :: overloads
                  else
                    Some signature, overloads
                in
                List.fold ~init:(None, []) ~f:to_signature overloads
              in
              let callable_name = callable_name_of_public ~implementation ~overloads in
              let { AnnotatedAttribute.undecorated_signature; decorators } =
                self#resolve_define_undecorated
                  ~callable_name
                  ~implementation
                  ~overloads
                  ~cycle_detections
                  ~scoped_type_variables
              in
              ( AnnotatedAttribute.UninstantiatedAnnotation.DecoratedMethod
                  { undecorated_signature; decorators },
                undecorated_signature )
            in
            (* If the method is decorated with @enum.member, it's an enum member so we should infer
               a literal type *)
            let callable, visibility =
              if is_enum && Attribute.may_be_enum_member attribute then
                ( AnnotatedAttribute.UninstantiatedAnnotation.Attribute
                    (Type.Literal
                       (Type.EnumerationMember
                          { enumeration_type = class_annotation; member_name = attribute_name })),
                  AnnotatedAttribute.ReadOnly (Refinable { overridable = true }) )
              else
                callable, visibility
            in
            callable, false, visibility, Some undecorated_signature
        | Property { kind; _ } -> (
            let parse_annotation_option annotation =
              annotation >>| self#parse_annotation ~cycle_detections ~scoped_type_variables
            in
            match kind with
            | ReadWrite
                {
                  getter = { self = getter_self_annotation; return = getter_annotation; _ };
                  setter = { self = setter_self_annotation; value = setter_annotation; _ };
                } ->
                let getter_annotation = parse_annotation_option getter_annotation in
                let setter_annotation = parse_annotation_option setter_annotation in
                ( AnnotatedAttribute.UninstantiatedAnnotation.Property
                    {
                      getter =
                        {
                          self = parse_annotation_option getter_self_annotation;
                          value = getter_annotation;
                        };
                      setter =
                        Some
                          {
                            self = parse_annotation_option setter_self_annotation;
                            value = setter_annotation;
                          };
                    },
                  false,
                  ReadWrite,
                  None )
            | ReadOnly { getter = { self = self_annotation; return = getter_annotation; _ } } ->
                let annotation = parse_annotation_option getter_annotation in
                ( AnnotatedAttribute.UninstantiatedAnnotation.Property
                    {
                      getter =
                        { self = parse_annotation_option self_annotation; value = annotation };
                      setter = None;
                    },
                  false,
                  ReadOnly Unrefinable,
                  None ))
      in
      let initialized =
        match kind with
        | Simple { nested_class = true; _ } -> AnnotatedAttribute.OnClass
        | Simple { values; _ } ->
            List.hd values
            >>| (function
                  | {
                      Attribute.value = { Node.value = Constant Expression.Constant.Ellipsis; _ };
                      _;
                    } ->
                      AnnotatedAttribute.OnlyOnInstance
                  | { Attribute.origin = Explicit; _ } -> OnClass
                  | { origin = Implicit; _ } -> OnlyOnInstance)
            |> Option.value ~default:AnnotatedAttribute.NotInitialized
        | Method _
        | Property { class_property = true; _ } ->
            OnClass
        | Property { class_property = false; _ } -> OnlyOnInstance
      in
      AnnotatedAttribute.create_uninstantiated
        ~uninstantiated_annotation:
          { AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation }
        ~visibility
        ~abstract:
          (match kind with
          | Method { signatures; _ } ->
              List.exists signatures ~f:Define.Signature.is_abstract_method
          | _ -> false)
        ~async_property:
          (match kind with
          | Property { async; _ } -> async
          | _ -> false)
        ~class_variable
        ~defined
        ~initialized
        ~name:attribute_name
        ~parent:parent_name
        ~property:
          (match kind with
          | Property _ -> true
          | _ -> false)
        ~undecorated_signature

    (* Special-case attribute table factory used as a helper in
     * single_uninstantiated_attribute_table.
     *)
    method sqlalchemy_attribute_table
        ~cycle_detections
        ~include_generated_attributes
        ~in_test
        ~accessed_via_metaclass
        ({ Node.value = { ClassSummary.name = parent_name; _ }; _ } as parent) =
      let { Queries.get_class_summary; successors; _ } = queries in
      let class_name = Reference.show parent_name in
      let unannotated_attributes
          ~include_generated_attributes
          ~in_test
          ({ Node.value = class_summary; _ } as parent)
        =
        let attributes =
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> Identifier.SerializableMap.bindings
          |> List.map ~f:(fun (_, attribute) -> attribute)
        in
        let unannotated_attribute { Node.value = attribute; _ } =
          self#create_uninstantiated_attribute
            ~scoped_type_variables:None
            ~cycle_detections
            ~parent
            ?defined:(Some true)
            ~accessed_via_metaclass
            attribute
        in
        List.map attributes ~f:unannotated_attribute
      in
      let add_constructor table =
        let successor_definitions = successors class_name |> List.filter_map ~f:get_class_summary in
        let name_annotation_pairs =
          let name_annotation_pair attribute =
            let name = AnnotatedAttribute.name attribute in
            if
              Expression.is_dunder_attribute name
              || AnnotatedAttribute.is_mangled_private_field attribute
            then
              None
            else
              let annotation =
                self#instantiate_attribute
                  ~cycle_detections
                  ~accessed_through_class:false
                  ~accessed_through_readonly:false
                  ?type_for_lookup:None
                  ?apply_descriptors:None
                  attribute
                |> AnnotatedAttribute.annotation
                |> TypeInfo.Unit.annotation
              in
              Some (name, annotation)
          in
          parent :: successor_definitions
          |> List.concat_map
               ~f:(unannotated_attributes ~include_generated_attributes:false ~in_test:false)
          |> List.filter_map ~f:name_annotation_pair
          (* Pick the overriding attribute. *)
          |> Identifier.Map.of_alist_reduce ~f:(fun first _ -> first)
          |> Map.to_alist
        in
        let parameters =
          let keyword_only_parameter (name, annotation) =
            Type.Record.Callable.CallableParamType.KeywordOnly
              { name = Format.asprintf "$parameter$%s" name; annotation; default = true }
          in
          let self_parameter =
            Type.Callable.CallableParamType.Named
              { name = "$parameter$self"; annotation = Type.Primitive class_name; default = false }
          in
          List.map ~f:keyword_only_parameter name_annotation_pairs
          |> fun parameters -> Type.Record.Callable.Defined (self_parameter :: parameters)
        in
        let constructor =
          {
            Type.Callable.kind = Named (Reference.create ~prefix:parent_name "__init__");
            implementation = { annotation = Type.none; parameters };
            overloads = [];
          }
        in
        AnnotatedAttribute.create_uninstantiated
          ~abstract:false
          ~uninstantiated_annotation:(create_uninstantiated_method constructor)
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name:"__init__"
          ~parent:class_name
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:(Some constructor)
        |> UninstantiatedAttributeTable.add table
      in
      let add_special_attribute ~name ~annotation table =
        AnnotatedAttribute.create_uninstantiated
          ~abstract:false
          ~uninstantiated_annotation:
            {
              AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass = false;
              kind = AnnotatedAttribute.UninstantiatedAnnotation.Attribute annotation;
            }
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name
          ~parent:class_name
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:None
        |> UninstantiatedAttributeTable.add table
      in
      let table = UninstantiatedAttributeTable.create () in
      unannotated_attributes ~include_generated_attributes ~in_test parent
      |> List.iter ~f:(UninstantiatedAttributeTable.add table);
      if include_generated_attributes then
        add_constructor table;
      add_special_attribute
        ~name:"metadata"
        ~annotation:(Type.Primitive "sqlalchemy.sql.schema.MetaData")
        table;
      add_special_attribute
        ~name:"__table__"
        ~annotation:(Type.Primitive "sqlalchemy.sql.schema.Table")
        table;
      table

    (* Special-case attribute table factory used as a helper in
     * single_uninstantiated_attribute_table.
     *
     * This creates overloaded method signatures that approximate the behavior
     * of TypedDictionary, mostly using helpers defined in the
     * `Type.TypedDictionary` module.
     *
     * These methods are only sufficient for type checking some kinds of code involving
     * TypedDictionary, often we still have to special-case the logic.
     *)
    method typed_dictionary_special_methods_table
        ~cycle_detections
        ~include_generated_attributes
        ~in_test
        ~accessed_via_metaclass
        ~class_name
        ({ Node.value = { ClassSummary.name; _ }; _ } as parent_definition) =
      let Queries.{ is_typed_dictionary; get_class_summary; successors; _ } = queries in
      let table = UninstantiatedAttributeTable.create () in
      let add_special_methods () =
        let successor_definitions =
          Reference.show name |> successors |> List.filter_map ~f:get_class_summary
        in
        let base_typed_dictionary_definition fields =
          let total = Type.TypedDictionary.are_fields_total fields in
          match get_class_summary (Type.TypedDictionary.class_name ~total) with
          | Some definition -> definition
          | None -> failwith "Expected to find TypedDictionary"
        in
        let typed_dictionary_definitions =
          List.filter
            (parent_definition :: successor_definitions)
            ~f:(fun { Node.value = { ClassSummary.name; _ }; _ } ->
              is_typed_dictionary (Reference.show name))
        in
        let get_field_attributes
            ~include_generated_attributes
            { Node.value = { bases = { ClassSummary.base_classes; _ }; _ } as class_summary; _ }
          =
          let has_non_total_typed_dictionary_base_class =
            List.exists base_classes ~f:(fun base_expression ->
                String.equal
                  (Expression.show base_expression)
                  (Type.TypedDictionary.class_name ~total:false))
          in
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> Identifier.SerializableMap.bindings
          |> List.map ~f:(fun (_, field_attribute) ->
                 ( self#create_uninstantiated_attribute
                     ~scoped_type_variables:None
                     ~cycle_detections
                     ~parent:parent_definition
                     ?defined:(Some true)
                     ~accessed_via_metaclass
                     (Node.value field_attribute),
                   has_non_total_typed_dictionary_base_class ))
        in
        let attribute_to_typed_dictionary_field
            (attribute, has_non_total_typed_dictionary_base_class)
          =
          match AnnotatedAttribute.uninstantiated_annotation attribute with
          | { AnnotatedAttribute.UninstantiatedAnnotation.kind = Attribute annotation; _ } ->
              Some
                (Type.TypedDictionary.create_field
                   ~annotation
                   ~has_non_total_typed_dictionary_base_class
                   (AnnotatedAttribute.name attribute))
          | _ -> None
        in
        let keep_last_declarations fields =
          List.map fields ~f:(fun (field : Type.TypedDictionary.typed_dictionary_field) ->
              field.name, field)
          |> Map.of_alist_multi (module String)
          |> Map.to_alist
          |> List.map ~f:(fun (_, fields) -> List.last_exn fields)
        in
        let fields =
          List.rev typed_dictionary_definitions
          |> List.concat_map ~f:(get_field_attributes ~include_generated_attributes:false)
          |> List.filter_map ~f:attribute_to_typed_dictionary_field
          |> keep_last_declarations
        in
        let overload_method (attribute, _) =
          match AnnotatedAttribute.uninstantiated_annotation attribute with
          | {
              AnnotatedAttribute.UninstantiatedAnnotation.kind =
                ( Attribute (Callable callable)
                | DecoratedMethod { undecorated_signature = callable; decorators = _ } );
              _;
            } as uninstantiated_annotation ->
              let overloaded_callable overloads =
                {
                  callable with
                  Type.Callable.implementation = { annotation = Type.Top; parameters = Undefined };
                  overloads;
                }
              in
              Type.TypedDictionary.special_overloads
                ~class_name
                ~fields
                ~method_name:(AnnotatedAttribute.name attribute)
              >>| overloaded_callable
              >>| fun callable ->
              AnnotatedAttribute.with_uninstantiated_annotation
                ~uninstantiated_annotation:
                  {
                    uninstantiated_annotation with
                    AnnotatedAttribute.UninstantiatedAnnotation.kind = Attribute (Callable callable);
                  }
                attribute
              |> AnnotatedAttribute.with_undecorated_signature
                   ~undecorated_signature:(Some callable)
          | _ -> None
        in
        let constructor =
          let constructor = Type.TypedDictionary.constructor ~name:class_name ~fields in
          constructor
          |> create_uninstantiated_method
          |> fun uninstantiated_annotation ->
          AnnotatedAttribute.create_uninstantiated
            ~uninstantiated_annotation
            ~abstract:false
            ~async_property:false
            ~class_variable:false
            ~defined:true
            ~initialized:OnClass
            ~name:"__init__"
            ~parent:class_name
            ~visibility:ReadWrite
            ~property:false
            ~undecorated_signature:(Some constructor)
        in
        let all_special_methods =
          constructor
          :: (base_typed_dictionary_definition fields
             |> get_field_attributes ~include_generated_attributes:true
             |> List.filter_map ~f:overload_method)
        in
        List.iter ~f:(UninstantiatedAttributeTable.add table) all_special_methods
      in
      if include_generated_attributes then add_special_methods ();
      table

    (* Create an UninstantiatedAttributeTable value with all of the uninstantiated
     * attributes defined on the body of a class.
     *
     * The output of this function is generally cached in the `AttributeCache`
     * environment layer, which is critical for performance.
     *
     * Note that in the standard case this does *not* contain any inherited attributes;
     * we usually have to traverse a lazy sequence of these tables to find an attribute
     * on a class.
     *
     * The granularity is usually the same as ClassSummary, which is why caching this
     * table turns out to work well for performance.
     *
     * Dataclasses are heavily special-cased via the
     * `apply_dataclass_transforms_to_table` helper, which also flattens fields
     * defined in all dataclass ancestors (so the granularity of this table for
     * dataclasses is substantially different than normal classes).
     *)
    method single_uninstantiated_attribute_table
        ~cycle_detections
        ~include_generated_attributes
        ~accessed_via_metaclass
        class_name =
      let Queries.{ get_class_summary; get_class_metadata; generic_parameters_as_variables; _ } =
        queries
      in
      let handle ({ Node.value = class_summary; _ } as parent) ~in_test =
        let table = UninstantiatedAttributeTable.create () in
        (* collect generic parameters (which we initially added in our class hierarchy environment)
           for this class *)
        let class_parameters =
          match generic_parameters_as_variables class_name with
          | Some parameter_list -> parameter_list
          | _ -> []
        in
        let scoped_type_variables = scoped_type_variables_as_map class_parameters in
        let add_actual () =
          let collect_attributes attribute =
            let created_attribute =
              self#create_uninstantiated_attribute
                ~scoped_type_variables
                (Node.value attribute)
                ~cycle_detections
                ~parent
                ~accessed_via_metaclass
            in
            let is_not_init_var attribute =
              match AnnotatedAttribute.uninstantiated_annotation attribute with
              | {
               AnnotatedAttribute.UninstantiatedAnnotation.kind =
                 Attribute (Type.Parametric { name = "dataclasses.InitVar"; _ });
               _;
              } ->
                  false
              | _ -> true
            in
            if is_not_init_var created_attribute then
              UninstantiatedAttributeTable.add table created_attribute;
            ()
          in
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> fun attribute_map ->
          Identifier.SerializableMap.iter (fun _ data -> collect_attributes data) attribute_map
        in
        add_actual ();
        let () =
          if include_generated_attributes then
            apply_dataclass_transforms_to_table
              ~queries
              ~definition:parent
              (self#create_uninstantiated_attribute ~cycle_detections)
              (self#instantiate_attribute
                 ~cycle_detections
                 ?type_for_lookup:None
                 ~accessed_through_class:false
                 ~accessed_through_readonly:false
                   (* TODO(T65806273): Right now we're just ignoring `__set__`s on dataclass
                      attributes. This avoids needing to explicitly break the loop that would
                      otherwise result or to somehow separate these results from the main set of
                      attributes *)
                 ~apply_descriptors:false)
              class_name
              table
              ~scoped_type_variables
        in
        table
      in
      match get_class_summary class_name, get_class_metadata class_name with
      | Some definition, Some { is_typed_dictionary; is_test = in_test; successors = Some _; _ } ->
          let is_declarative_sqlalchemy_class () =
            Option.equal
              Type.equal
              (self#metaclass ~cycle_detections class_name)
              (Some (Type.Primitive "sqlalchemy.ext.declarative.api.DeclarativeMeta"))
          in
          let table =
            if is_typed_dictionary then
              self#typed_dictionary_special_methods_table
                ~cycle_detections
                ~include_generated_attributes
                ~in_test
                ~accessed_via_metaclass
                ~class_name
                definition
            else if is_declarative_sqlalchemy_class () then
              self#sqlalchemy_attribute_table
                ~cycle_detections
                ~include_generated_attributes
                ~in_test
                ~accessed_via_metaclass
                definition
            else
              handle definition ~in_test
          in
          Some table
      | _ -> None

    (* Get all the uninstantiated attribute tables for a class.
     *
     * This normally produces a Sequence.t (which is lazy) of the uninstantiated attribute table
     * for each class in the method resolution order (MRO) of the class with name `class_name`,
     * plus potentially the metaclass hierarchy as well.
     *
     * There are a few flags controlling behavior; in particular
     * - if `transistive` is false we don't traverse the MRO, only the current
     *   class.
     * - if `accessed_through_class` and `special_method` are both set, then we will skip straight
     *   to the metaclass hierarchy.
     *
     * Note that the behavior of jumping straight to the metaclass hierarchy
     * on special methods is consistent with the runtime, see more details at
     * https://docs.python.org/3/reference/datamodel.html.
     *)
    method uninstantiated_attribute_tables
        ~cycle_detections
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ~special_method
        class_name =
      let Queries.{ successors; get_class_metadata; _ } = queries in
      let handle { ClassSuccessorMetadataEnvironment.successors = the_successors; _ } =
        let get_table ~accessed_via_metaclass =
          self#single_uninstantiated_attribute_table
            ~cycle_detections
            ~include_generated_attributes
            ~accessed_via_metaclass
        in
        let normal_tables =
          let normal_hierarchy =
            (* Pass over normal class hierarchy. *)
            if accessed_through_class && special_method then
              []
            else if transitive then
              class_name :: Option.value the_successors ~default:[]
            else
              [class_name]
          in
          Sequence.of_list normal_hierarchy
          |> Sequence.filter_map ~f:(get_table ~accessed_via_metaclass:false)
        in
        let metaclass_tables =
          (* We don't want to have to find our metaclass/it's parents if we successfully find the
             attribute in one of our actual parents *)
          lazy
            begin
              let metaclass_hierarchy =
                (* Class over meta hierarchy if necessary. *)
                if accessed_through_class then
                  self#metaclass ~cycle_detections class_name
                  >>| Type.split
                  >>| fst
                  >>= Type.primitive_name
                  >>| (fun metaclass -> metaclass :: successors metaclass)
                  |> Option.value ~default:[]
                else
                  []
              in
              metaclass_hierarchy
              |> Sequence.of_list
              |> Sequence.filter_map ~f:(get_table ~accessed_via_metaclass:true)
            end
        in
        Sequence.append normal_tables (Sequence.of_lazy metaclass_tables)
      in
      get_class_metadata class_name >>| handle

    (* Collect all of the uninstantiated attributes from all the individual
     * uninstantiated attribute tables into one big list.
     *
     * This operation is expensive and should be avoided when type checking
     * dependent code; it should only be used when performing once-per-class
     * checks (e.g. in typeCheck.ml when checking the body of a class).
     *)
    method uninstantiated_attributes
        ~cycle_detections
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ?(special_method = false)
        class_name =
      let collect sofar table =
        let add ((sofar_list, sofar_set) as sofar) attribute =
          let name = AnnotatedAttribute.name attribute in
          if Set.mem sofar_set name then
            sofar
          else
            attribute :: sofar_list, Set.add sofar_set name
        in
        UninstantiatedAttributeTable.to_list table |> List.fold ~f:add ~init:sofar
      in
      self#uninstantiated_attribute_tables
        ~cycle_detections
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ~special_method
        class_name
      >>| Sequence.fold ~f:collect ~init:([], Identifier.Set.empty)
      >>| fst
      >>| List.rev

    (* Take a type `current_type` and the name `source_type_name` of an ancestor class (potentially a
     * metaclass) of that type, compute the constraints on the type parameters of the source
     * type (expressed as `Type.Variable.t` values).
     *
     * These constraints represent the substitution needed to take an uninstantiated
     * attribute coming from the source type's attribute table (which will be expressed in terms of the
     * parameters of that type, as variables) and instantiate it for `current_type`.
     *
     * The implementation has two steps:
     * - take the source type and parameterize it by its own parameters, as arguments
     * - solve for (parameterized source type) <: (current_type)
     *)
    method constraints_for_instantiate ~cycle_detections ~source_type_name ~current_type () =
      let Queries.{ generic_parameters_as_variables; _ } = queries in
      let parameters_as_arguments =
        generic_parameters_as_variables source_type_name
        >>| List.map ~f:Type.Variable.to_argument
        |> Option.value ~default:[]
      in
      if List.is_empty parameters_as_arguments then
        TypeConstraints.Solution.empty
      else
        let right = Type.parametric source_type_name parameters_as_arguments in
        match current_type, right with
        | Type.Primitive name, Parametric { name = right_name; _ } when String.equal name right_name
          ->
            (* TODO(T42259381) This special case is only necessary because constructor calls
               attributes with an "instantiated" type of a bare parametric, which will fill with
               Anys *)
            TypeConstraints.Solution.empty
        | _ ->
            let order = self#full_order ~cycle_detections in
            TypeOrder.OrderedConstraintsSet.add_and_simplify
              ConstraintsSet.empty
              ~new_constraint:(LessOrEqual { left = current_type; right })
              ~order
            |> TypeOrder.OrderedConstraintsSet.solve ~order
            (* TODO(T39598018): error in this case somehow, something must be wrong *)
            |> Option.value ~default:TypeConstraints.Solution.empty

    (* This is a helper method - used exclusively in `instantiate_attribute` - to deal
     * with the complexities of accessing the `__call__` method on a value whose type
     * is some class object `type[Xyz]`.
     *
     * This logic is complex because there are both constructors (`__new__`) and
     * initializers (`__init__`) to consider, and we also have to handle unwrapping bound
     * methods.
     *)
    method constructor ~cycle_detections class_name ~type_for_lookup =
      let Queries.{ generic_parameters_as_variables; successors; _ } = queries in
      let return_annotation =
        let generics =
          generic_parameters_as_variables class_name
          >>| List.map ~f:Type.Variable.to_argument
          |> Option.value ~default:[]
        in
        (* Tuples are special. *)
        if String.equal class_name "tuple" then
          match generics with
          | [Single tuple_variable] ->
              Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation tuple_variable)
          | _ -> Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Any)
        else
          let backup = Type.parametric class_name generics in
          match type_for_lookup, generics with
          | _, [] -> type_for_lookup
          | Type.Primitive instantiated_name, _ when String.equal instantiated_name class_name ->
              backup
          | Type.Parametric { arguments; name = instantiated_name }, generics
            when String.equal instantiated_name class_name
                 && List.length arguments <> List.length generics ->
              backup
          | _ -> type_for_lookup
      in
      let definitions = class_name :: successors class_name in
      let definition_index parent =
        parent
        |> (fun class_annotation ->
             List.findi definitions ~f:(fun _ annotation ->
                 Type.equal (Primitive annotation) class_annotation))
        >>| fst
        |> Option.value ~default:Int.max_value
      in
      let signature_index_and_parent ~name =
        let signature, parent_name =
          match
            self#attribute
              ~cycle_detections
              ~transitive:true
              ~accessed_through_class:false
              ~accessed_through_readonly:false
              ~include_generated_attributes:true
              ?special_method:None
              ?type_for_lookup:(Some return_annotation)
              ~attribute_name:name
              class_name
          with
          | Some attribute ->
              ( AnnotatedAttribute.annotation attribute |> TypeInfo.Unit.annotation,
                AnnotatedAttribute.parent attribute )
          | None -> Type.Top, class_name
        in
        signature, definition_index (Type.Primitive parent_name), parent_name
      in
      let constructor_signature, constructor_index, _ =
        signature_index_and_parent ~name:"__init__"
      in
      let new_signature, new_index, new_parent_name =
        let new_signature, new_index, new_parent_name =
          signature_index_and_parent ~name:"__new__"
        in
        ( Type.parametric
            "BoundMethod"
            [Single new_signature; Single (Type.class_type type_for_lookup)],
          new_index,
          new_parent_name )
      in
      let signature, with_return =
        let replace_return_type_for_degenerate_cases callable_return_type =
          let is_instance_of_current_class =
            Reference.equal (Type.class_name callable_return_type) (Reference.create class_name)
          in
          let should_ignore_return_type =
            (* If the class inherits `__new__` from a parent class, replace the return type with the
               child type. Otherwise, it would be returning `Base[T1, T2]` instead of `Child[...]`.
               Note that this will result in the same return type for all `__new__` overloads but it
               seems like the best we can do in an ambiguous situation.

               If the user has erroneously marked the `__new__` method as returning something other
               than an instance of the class, such as `None`, replace the return type with the
               synthesized return type, such as `Base[T1, T2]`. *)
            (not (String.equal class_name new_parent_name)) || not is_instance_of_current_class
          in
          if should_ignore_return_type then
            return_annotation
          else
            callable_return_type
        in
        if new_index < constructor_index then
          new_signature, Type.Callable.map_annotation ~f:replace_return_type_for_degenerate_cases
        else
          constructor_signature, Type.Callable.with_return_annotation ~annotation:return_annotation
      in
      match signature with
      | Type.Callable callable -> Type.Callable (with_return callable)
      | Parametric
          { name = "BoundMethod"; arguments = [Single (Callable callable); Single self_type] } ->
          Parametric
            {
              name = "BoundMethod";
              arguments = [Single (Callable (with_return callable)); Single self_type];
            }
      | _ -> signature

    (* Given an `AnnotatedAttribute.uninstantiated` value `attribute`, compute an instantiated attribute.
     *
     * In many cases, this will be called with `type_for_lookup` set to an instantiated `Type.t`
     * that allows us to substitute tyep variables in the attribute for the result of
     * applying type arguments. If no `type_for_lookup` is provided, we assume we are in the body
     * of the attribute's parent class, and use the type variables as given. *)
    method instantiate_attribute
        ~cycle_detections
        ~accessed_through_class
        ~accessed_through_readonly
        ?type_for_lookup
        ?(apply_descriptors = true)
        attribute =
      let Queries.
            {
              get_class_summary;
              exists_matching_class_decorator;
              generic_parameters_as_variables;
              _;
            }
        =
        queries
      in
      let make_annotation_readonly = function
        | `Attribute annotation -> `Attribute (Type.PyreReadOnly.create annotation)
        | `Property (getter, setter) ->
            let make_property_annotation_readonly
                { AnnotatedAttribute.UninstantiatedAnnotation.self; value }
              =
              {
                AnnotatedAttribute.UninstantiatedAnnotation.self = self >>| Type.PyreReadOnly.create;
                value = value >>| Type.PyreReadOnly.create;
              }
            in
            `Property
              ( make_property_annotation_readonly getter,
                setter >>| make_property_annotation_readonly )
      in
      let get_attribute = self#attribute in
      let class_name = AnnotatedAttribute.parent attribute in
      let attribute_name = AnnotatedAttribute.name attribute in
      let { AnnotatedAttribute.UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation } =
        AnnotatedAttribute.uninstantiated_annotation attribute
      in
      let accessed_through_class = accessed_through_class && not accessed_via_metaclass in
      let annotation, uninstantiated_annotation, problem =
        match annotation with
        | DecoratedMethod { undecorated_signature; decorators } ->
            let annotation, problem =
              match decorators with
              | Error problem -> Type.Any, Some problem
              | Ok decorators -> (
                  match
                    self#apply_decorators
                      ~cycle_detections
                      ~scoped_type_variables:None
                      undecorated_signature
                      decorators
                  with
                  | Error problem -> Type.Any, Some problem
                  | Ok resolved ->
                      let annotation =
                        match attribute_name, resolved with
                        (* these names are only magic-ed into being ClassMethods/StaticMethods if
                           they're "plain functions". We can't capture that in the type system, so
                           we approximate with Callable *)
                        | "__new__", Type.Callable _ ->
                            Type.parametric "typing.StaticMethod" [Single resolved]
                        | "__init_subclass__", Callable _
                        | "__class_getitem__", Callable _ ->
                            Type.parametric "typing.ClassMethod" [Single resolved]
                        | _ -> resolved
                      in
                      annotation, None)
            in
            `Attribute annotation, Some annotation, problem
        | Attribute annotation -> `Attribute annotation, Some annotation, None
        | Property { getter; setter } -> `Property (getter, setter), None, None
      in
      let annotation =
        if accessed_through_readonly then make_annotation_readonly annotation else annotation
      in
      let annotation =
        match type_for_lookup with
        | None -> annotation
        | Some type_for_lookup -> (
            let solution =
              self#constraints_for_instantiate
                ~source_type_name:class_name
                ~current_type:type_for_lookup
                ~cycle_detections
                ()
            in
            let instantiate annotation = TypeConstraints.Solution.instantiate solution annotation in
            match annotation with
            | `Attribute annotation -> `Attribute (instantiate annotation)
            | `Property (getter, setter) ->
                let instantiate_property_annotation
                    { AnnotatedAttribute.UninstantiatedAnnotation.self; value }
                  =
                  {
                    AnnotatedAttribute.UninstantiatedAnnotation.self = self >>| instantiate;
                    value = value >>| instantiate;
                  }
                in
                `Property
                  ( instantiate_property_annotation getter,
                    setter >>| instantiate_property_annotation ))
      in
      let ({ ConstraintsSet.get_named_tuple_fields; _ } as order) =
        self#full_order ~cycle_detections
      in
      let annotation, original =
        let type_for_lookup =
          match type_for_lookup with
          | Some type_for_lookup -> type_for_lookup
          | None -> Type.Primitive class_name
        in
        let type_for_lookup =
          if accessed_via_metaclass then Type.class_type type_for_lookup else type_for_lookup
        in
        let special_case_methods callable =
          (* Certain callables' types can't be expressed directly and need to be special cased *)
          let self_parameter =
            Type.Callable.CallableParamType.Named
              { name = "self"; annotation = Type.Top; default = false }
          in
          let tuple_getitem_overloads members =
            let { Type.Callable.overloads; _ } = callable in
            let overload index member =
              {
                Type.Callable.annotation = member;
                parameters =
                  Defined
                    [
                      self_parameter;
                      Named { name = "x"; annotation = Type.literal_integer index; default = false };
                    ];
              }
            in
            let overloads =
              List.mapi ~f:overload members
              @ List.map2_exn
                  ~f:overload
                  (List.init ~f:(fun x -> -x - 1) (List.length members))
                  (List.rev members)
              @ overloads
            in
            Type.Callable { callable with overloads }
          in
          match type_for_lookup, attribute_name, class_name with
          | Type.Tuple (Concrete members), "__getitem__", _ -> tuple_getitem_overloads members
          | ( Parametric { name = "type"; arguments = [Single (Type.Primitive name)] },
              "__getitem__",
              "typing.GenericMeta" ) ->
              let implementation, overloads =
                let generics = generic_parameters_as_variables name |> Option.value ~default:[] in
                let create_parameter annotation =
                  Type.Callable.CallableParamType.PositionalOnly
                    { index = 0; annotation; default = false }
                in
                let synthetic =
                  Type.Variable
                    (Type.Variable.TypeVar.create "$synthetic_attribute_resolution_variable")
                in
                match name with
                (* This can't be expressed without IntVars, StrVars, and corresponding TypeVarTuple
                   variants of them *)
                | "typing_extensions.Literal"
                | "typing.Literal"
                (* TODO:(T60535947) We can't do the Map[Ts, type] -> X[Ts] trick here because we
                   don't yet support Union[Ts] *)
                | "typing.Union" ->
                    ( { Type.Callable.annotation = Type.class_type Type.Any; parameters = Undefined },
                      [] )
                | "typing.Callable" ->
                    ( {
                        Type.Callable.annotation =
                          Type.class_type (Type.Callable.create ~annotation:synthetic ());
                        parameters =
                          Defined
                            [
                              self_parameter;
                              create_parameter
                                (Type.Tuple (Concrete [Type.Any; Type.class_type synthetic]));
                            ];
                      },
                      [] )
                | _ -> (
                    let overload parameter =
                      let generics = List.map generics ~f:Type.Variable.to_argument in
                      match name, generics with
                      | "typing.Optional", [Single generic] ->
                          {
                            Type.Callable.annotation = Type.class_type (Type.optional generic);
                            parameters = Defined [self_parameter; parameter];
                          }
                      | _ ->
                          {
                            Type.Callable.annotation =
                              Type.class_type (Type.parametric name generics);
                            parameters = Defined [self_parameter; parameter];
                          }
                    in
                    match generics with
                    | [TypeVarVariable generic] ->
                        overload (create_parameter (Type.class_type (Variable generic))), []
                    | _ ->
                        (* To support the value `GenericFoo[int, str]`, we need `class
                           GenericFoo[T1, T2]` to have:

                           `def __getitem__(cls, __x: Tuple[Type[T1], Type[T2]] ) -> GenericFoo[T1,
                           T2]`. *)
                        let meta_type_and_return_type = function
                          | Type.Variable.TypeVarVariable single ->
                              ( Type.class_type (Variable single),
                                Type.Argument.Single (Type.Variable single) )
                          | ParamSpecVariable _ ->
                              (* TODO:(T60536033) We'd really like to take FiniteList[Ts], but
                                 without that we can't actually return the correct metatype, which
                                 is a bummer *)
                              Type.Any, Type.Argument.CallableParameters Undefined
                          | TypeVarTupleVariable _ -> Type.Any, Single Any
                        in
                        let meta_types, return_parameters =
                          List.map generics ~f:meta_type_and_return_type |> List.unzip
                        in
                        ( {
                            Type.Callable.annotation =
                              Type.class_type (Type.parametric name return_parameters);
                            parameters =
                              Defined [self_parameter; create_parameter (Type.tuple meta_types)];
                          },
                          [] ))
              in
              Type.Callable { callable with implementation; overloads }
          | Parametric { name = "type"; arguments = [Single meta_argument] }, "__call__", "type"
            when accessed_via_metaclass ->
              let get_constructor { Type.type_for_lookup; accessed_through_class; class_name; _ } =
                if accessed_through_class then (* Type[Type[X]] is invalid *)
                  None
                else
                  Some (self#constructor ~cycle_detections class_name ~type_for_lookup)
              in
              Type.class_attribute_lookups_for_type meta_argument
              >>| List.map ~f:get_constructor
              >>= Option.all
              >>| Type.union
              |> Option.value ~default:(Type.Callable callable)
          | _, "__getitem__", _ -> (
              match get_named_tuple_fields type_for_lookup with
              | Some members -> tuple_getitem_overloads members
              | None -> Type.Callable callable)
          | _ -> Type.Callable callable
        in
        match annotation with
        | `Property (getter_annotation, setter_annotation) -> (
            (* Special case properties with type variables. *)
            let solve_property
                {
                  AnnotatedAttribute.UninstantiatedAnnotation.self = self_annotation;
                  value = value_annotation;
                }
              =
              match value_annotation with
              | None -> Type.Top
              | Some annotation -> (
                  let constraints =
                    match self_annotation with
                    | Some annotation ->
                        TypeOrder.OrderedConstraintsSet.add_and_simplify
                          ConstraintsSet.empty
                          ~new_constraint:
                            (LessOrEqual { left = type_for_lookup; right = annotation })
                          ~order
                    | None -> ConstraintsSet.empty
                  in
                  match TypeOrder.OrderedConstraintsSet.solve ~order constraints with
                  | Some solution -> TypeConstraints.Solution.instantiate solution annotation
                  | None -> Type.Top)
            in
            match setter_annotation with
            | Some setter_annotation ->
                solve_property getter_annotation, solve_property setter_annotation
            | None ->
                let annotation = solve_property getter_annotation in
                annotation, annotation)
        | `Attribute annotation -> (
            let annotation =
              match annotation with
              | Type.Callable callable -> special_case_methods callable
              | other -> other
            in
            let order () = order in
            let special =
              callable_call_special_cases
                ~type_for_lookup:(Some type_for_lookup)
                ~class_name
                ~attribute_name
                ~order
                ~accessed_through_class
              >>| fun callable -> callable, callable
            in
            match special with
            | Some special -> special
            | None
              when [%compare.equal: AnnotatedAttribute.initialized]
                     (AnnotatedAttribute.initialized attribute)
                     OnClass
                   && apply_descriptors -> (
                let call_dunder_get (descriptor, callable) =
                  let selection_result =
                    self#signature_select
                      ~cycle_detections
                      ~arguments:
                        [
                          { Argument.kind = Positional; expression = None; resolved = descriptor };
                          {
                            Argument.kind = Positional;
                            expression = None;
                            resolved =
                              (if accessed_through_class then Type.none else type_for_lookup);
                          };
                          {
                            Argument.kind = Positional;
                            expression = None;
                            resolved = Type.class_type type_for_lookup;
                          };
                        ]
                      ~resolve_with_locals:(fun ~locals:_ _ -> Type.object_primitive)
                      ~location:Location.any
                      ~callable
                      ~self_argument:None
                      ~skip_marking_escapees:true
                  in
                  match selection_result with
                  | SignatureSelectionTypes.NotFound _ -> None
                  | Found { selected_return_annotation = return } -> Some return
                in
                let invert_dunder_set (descriptor, callable) ~order =
                  let synthetic = Type.Variable.TypeVar.create "$synthetic_dunder_set_variable" in
                  let right =
                    Type.Callable.create
                      ~annotation:Type.none
                      ~parameters:
                        (Defined
                           [
                             PositionalOnly { index = 0; annotation = descriptor; default = false };
                             PositionalOnly
                               { index = 1; annotation = type_for_lookup; default = false };
                             PositionalOnly
                               { index = 2; annotation = Variable synthetic; default = false };
                           ])
                      ()
                  in
                  TypeOrder.OrderedConstraintsSet.add_and_simplify
                    ConstraintsSet.empty
                    ~new_constraint:(LessOrEqual { left = Type.Callable callable; right })
                    ~order
                  |> TypeOrder.OrderedConstraintsSet.solve ~order
                  >>= fun solution ->
                  TypeConstraints.Solution.instantiate_single_type_var solution synthetic
                in
                let function_dunder_get callable =
                  let is_dataclass_attribute =
                    match get_class_summary class_name with
                    | Some class_summary ->
                        exists_matching_class_decorator
                          ~names:["dataclasses.dataclass"; "dataclass"]
                          class_summary
                    | _ -> false
                  in
                  let has_self_or_cls_param { Type.Callable.implementation = { parameters; _ }; _ } =
                    match parameters with
                    | Defined (Named { name = "$parameter$self"; _ } :: _) -> true
                    | Defined (Named { name = "$parameter$cls"; _ } :: _) -> true
                    | Defined
                        (PositionalOnly { index = 0; annotation = Type.Primitive class_; _ } :: _)
                      when String.equal class_ class_name ->
                        true
                    | _ -> false
                  in
                  if
                    accessed_through_class
                    || (is_dataclass_attribute && not (has_self_or_cls_param callable))
                  then
                    if accessed_through_readonly then
                      Type.PyreReadOnly.create (Type.Callable callable)
                    else
                      Type.Callable callable
                  else
                    let bound_self_type =
                      if accessed_through_readonly then
                        Type.PyreReadOnly.create type_for_lookup
                      else
                        type_for_lookup
                    in
                    Type.parametric
                      "BoundMethod"
                      [Single (Callable callable); Single bound_self_type]
                in
                let get_descriptor_method
                    { Type.type_for_lookup; accessed_through_class; class_name; _ }
                    ~kind
                  =
                  if accessed_through_class then
                    (* descriptor methods are statically looked up on the class (in this case
                       `type`), not on the instance. `type` is not a descriptor. *)
                    `NotDescriptor (Type.class_type type_for_lookup)
                  else
                    match type_for_lookup with
                    | Callable callable -> (
                        match kind with
                        | `DunderGet ->
                            (* We unsoundly assume all callables are callables with the `function`
                               `__get__` *)
                            `HadDescriptor (function_dunder_get callable)
                        | `DunderSet -> `NotDescriptor type_for_lookup)
                    | _ -> (
                        let attribute =
                          let attribute_name =
                            match kind with
                            | `DunderGet -> "__get__"
                            | `DunderSet -> "__set__"
                          in
                          (* descriptor methods are statically looked up on the class, and are not
                             themselves subject to description *)
                          get_attribute
                            ~cycle_detections
                            ~transitive:true
                            ~accessed_through_class:true
                            ~accessed_through_readonly:false
                            ~include_generated_attributes:true
                            ?special_method:None
                            ?type_for_lookup:(Some type_for_lookup)
                            ?apply_descriptors:(Some false)
                            ~attribute_name
                            class_name
                          >>| AnnotatedAttribute.annotation
                          >>| TypeInfo.Unit.annotation
                        in
                        match attribute with
                        | None -> `NotDescriptor type_for_lookup
                        | Some (Type.Callable callable) ->
                            let extracted =
                              match kind with
                              | `DunderGet -> call_dunder_get (type_for_lookup, callable)
                              | `DunderSet ->
                                  invert_dunder_set ~order:(order ()) (type_for_lookup, callable)
                            in
                            extracted
                            >>| (fun extracted -> `HadDescriptor extracted)
                            |> Option.value ~default:`FailedToExtract
                        | Some _ ->
                            (* In theory we could support `__get__`s or `__set__`s that are not just
                               Callables, but for now lets just ignore that *)
                            `DescriptorNotACallable)
                in
                match Type.class_attribute_lookups_for_type annotation with
                | None ->
                    (* This means we have a type that can't be `Type.split`, (most of) which aren't
                       descriptors, so we should be usually safe to just ignore. In general we
                       should fix class_attribute_lookups_for_type to always return something. *)
                    annotation, annotation
                | Some elements ->
                    let collect x =
                      let partitioner = function
                        | `NotDescriptor element -> `Fst element
                        | `HadDescriptor element -> `Snd element
                        (* Every descriptor should accept all hosts (and all host types) as a matter
                           of Liskov substitutibility with `object`. This means we need to error on
                           these invalid definitions (T65807232), and not on usages *)
                        | `FailedToExtract
                        | `DescriptorNotACallable ->
                            `Trd ()
                      in
                      match List.partition3_map x ~f:partitioner with
                      | _, _, _ :: _ ->
                          (* If we have broken descriptor methods we should error on them, not their
                             usages *)
                          Type.Any
                      | _, [], _ ->
                          (* If none of the components are descriptors, we don't need to worry about
                             re-unioning together the components we split apart, we can just give
                             back the original type *)
                          annotation
                      | normal, had_descriptors, _ -> Type.union (normal @ had_descriptors)
                    in
                    let elements_and_get_results =
                      List.map elements ~f:(fun element ->
                          element, get_descriptor_method element ~kind:`DunderGet)
                    in
                    let get_type = List.unzip elements_and_get_results |> snd |> collect in
                    let set_type =
                      if accessed_through_class then
                        annotation
                      else
                        let process (element, get_result) =
                          match get_descriptor_method element ~kind:`DunderSet, get_result with
                          | `NotDescriptor _, `HadDescriptor element ->
                              (* non-data descriptors set type should be their get type *)
                              `HadDescriptor element
                          | other, _ -> other
                        in
                        List.map elements_and_get_results ~f:process |> collect
                    in
                    get_type, set_type)
            | None -> annotation, annotation)
      in
      AnnotatedAttribute.instantiate
        attribute
        ~annotation
        ~original_annotation:original
        ~uninstantiated_annotation
        ~problem

    (* Given the name of an attribute on a class `class_name`, fetch an
     * `AnnotatedAttribute.instantiated`.
     *
     * There are many flags controlling behavior; the most important are:
     * - `type_for_lookup`: this will control the instantiation of the attribute; if it
     *   comes from a generic class and we are looking it up on an instantiated type,
     *   we will substitute type variables accordingly.
     * - `transitive`: if set to false, we will only search the uninstantiated attribute
     *   table of `class_name`, we won't look upward in the MRO
     *
     * The implementation proceeds as follows:
     * - special case some callable scenarios to handle `BoundMethod` and the fact
     *   that many types are callable in various ways.
     * - otherwise, create a lazy sequence of the uninstantiated tables for classes
     *   in the MRO, take the first hit, and instantiate it with `instantiate_attribute`.
     *)
    method attribute
        ~cycle_detections
        ~transitive
        ~accessed_through_class
        ~accessed_through_readonly
        ~include_generated_attributes
        ?(special_method = false)
        ?type_for_lookup
        ?apply_descriptors
        ~attribute_name
        class_name =
      let order () = self#full_order ~cycle_detections in
      match
        callable_call_special_cases
          ~type_for_lookup
          ~class_name
          ~attribute_name
          ~accessed_through_class
          ~order
      with
      | Some callable ->
          AnnotatedAttribute.create_instantiated
            ~annotation:callable
            ~original_annotation:callable
            ~uninstantiated_annotation:None
            ~visibility:ReadWrite
            ~abstract:false
            ~async_property:false
            ~class_variable:false
            ~defined:true
            ~initialized:OnClass
            ~name:"__call__"
            ~parent:"typing.Callable"
            ~property:false
            ~undecorated_signature:None
          |> Option.some
      | None ->
          self#uninstantiated_attribute_tables
            ~cycle_detections
            ~transitive
            ~accessed_through_class
            ~include_generated_attributes
            ~special_method
            class_name
          >>= Sequence.find_map ~f:(fun table ->
                  UninstantiatedAttributeTable.lookup_name table attribute_name)
          >>| self#instantiate_attribute
                ~cycle_detections
                ~accessed_through_class
                ~accessed_through_readonly
                ?type_for_lookup
                ?apply_descriptors

    (* Given a Type.t that might represent a typed dictionary, return a `Type.TypedDictionary.t`
     * if it is. Return `None` if the type is not a typed dictionary.
     *
     * The implementation relies on the synthesized constructor method to
     * determine the typed dictionary fields.
     *)
    method get_typed_dictionary ~cycle_detections annotation =
      let Queries.{ is_typed_dictionary; _ } = queries in
      match annotation with
      | Type.Primitive class_name when is_typed_dictionary class_name ->
          let fields =
            let get_attribute name =
              self#attribute
                ~cycle_detections
                ~transitive:false
                ~accessed_through_class:true
                ~accessed_through_readonly:false
                ~include_generated_attributes:true
                ~type_for_lookup:(Type.class_type annotation)
                ~special_method:false
                ~attribute_name:name
                class_name
              >>| AnnotatedAttribute.annotation
              >>| TypeInfo.Unit.annotation
            in
            (* __init__ tells us the name, value type, and requiredness of each field. *)
            let constructor =
              get_attribute "__init__"
              >>= function
              | Type.Callable callable -> Some callable
              | _ -> None
            in
            (* For readonly fields, we set the value type in the corresponding __setitem__ overload
               to Never, so we can use __setitem__ to reconstruct readonlyness. *)
            let readonlyness =
              get_attribute "__setitem__"
              >>= function
              | Type.Callable { overloads; _ } ->
                  let field_is_readonly { Callable.parameters; _ } =
                    match (parameters : Type.t Callable.record_parameters) with
                    | Defined
                        [
                          _;
                          Named { annotation = Literal (String (LiteralValue field_name)); _ };
                          Named { annotation = value_type; _ };
                        ] ->
                        Some (field_name, Type.is_noreturn_or_never value_type)
                    | _ -> None
                  in
                  Some
                    (List.filter_map ~f:field_is_readonly overloads
                    |> Map.of_alist_reduce (module String) ~f:(fun _ readonly -> readonly))
              | _ -> None
            in
            match constructor, readonlyness with
            | Some constructor, Some readonlyness ->
                Type.TypedDictionary.fields_from_constructor constructor readonlyness
            | _ -> None
          in
          fields >>| fun fields -> { Type.TypedDictionary.fields; name = class_name }
      | _ -> None

    (* We will expose this interface as the variance map *)
    method variance_map ~cycle_detections ~class_name ~parameters =
      let contains_bivariant = ref false in
      let convert_pre_to_post_one_param ~generic_type_param =
        let from_pre_to_post variance =
          match variance with
          | Type.Record.PreInferenceVariance.P_Covariant -> Type.Record.Variance.Covariant
          | Type.Record.PreInferenceVariance.P_Contravariant -> Type.Record.Variance.Contravariant
          | Type.Record.PreInferenceVariance.P_Invariant -> Type.Record.Variance.Invariant
          | Type.Record.PreInferenceVariance.P_Undefined ->
              contains_bivariant := true;
              Type.Record.Variance.Bivariant
        in

        let find_variance =
          match generic_type_param with
          | Type.GenericParameter.GpTypeVar { variance; _ } -> from_pre_to_post variance
          | _ -> Invariant
        in
        find_variance
      in
      let default_variance_and_inj gp =
        let variance = convert_pre_to_post_one_param ~generic_type_param:gp in
        let inj =
          match variance with
          | Type.Record.Variance.Bivariant -> false
          | _ -> true
        in
        variance, inj
      in
      let from_gp_to_decl gp =
        match gp with
        | Type.GenericParameter.GpTypeVar { name; _ } as gp ->
            let variance, inj = default_variance_and_inj gp in

            Some (name, variance, inj)
        | _ -> None
      in
      let convert_all_params parameters =
        let add_to_lookup so_far = function
          | Type.GenericParameter.GpTypeVar { name; _ } as gp ->
              let variance = convert_pre_to_post_one_param ~generic_type_param:gp in
              Map.set so_far ~key:name ~data:variance
          | Type.GenericParameter.GpParamSpec { name } -> Map.set so_far ~key:name ~data:Invariant
          | Type.GenericParameter.GpTypeVarTuple { name } ->
              Map.set so_far ~key:name ~data:Invariant
        in
        List.fold parameters ~f:add_to_lookup ~init:Identifier.Map.empty
      in
      let post_inference_initial = convert_all_params parameters in

      if not !contains_bivariant then
        post_inference_initial
      else
        let class_parameters = List.map ~f:Type.GenericParameter.to_variable parameters in
        let scoped_type_variables = scoped_type_variables_as_map class_parameters in
        let parse_annotation =
          self#parse_annotation ~cycle_detections ?validation:None ~scoped_type_variables
        in
        (* Creates a function from generic type parameters for a given class to post variance
           inference *)
        let environment : VarianceVisitor.variance_env = Stdlib.Hashtbl.create 0 in
        let params_from_gp ~class_name =
          let class_hierarchy_handler = class_hierarchy () in
          let generic_parameters = ClassHierarchy.generic_parameters class_hierarchy_handler in

          let parameters =
            match generic_parameters class_name with
            | Some parameters -> parameters
            | _ -> []
          in
          let params =
            Array.of_list
              (List.map
                 ~f:(fun param ->
                   Type.GenericParameter.parameter_name param, Type.Record.Variance.Bivariant, false)
                 parameters)
          in
          List.iteri
            ~f:(fun i param ->
              match from_gp_to_decl param with
              | Some (name, variance, inj) -> params.(i) <- name, variance, inj
              | None -> ())
            parameters;
          params
        in

        let to_list class_name =
          let get_table class_name =
            self#single_uninstantiated_attribute_table
              ~cycle_detections
              ~include_generated_attributes:true
              ~accessed_via_metaclass:false
              class_name
          in

          match get_table class_name with
          | Some table -> UninstantiatedAttributeTable.to_list table
          | None -> []
        in

        let get_base_class_types class_name =
          match get_class_summary class_name with
          | Some { Node.value = { ClassSummary.bases = { base_classes; _ }; _ }; _ } ->
              List.map ~f:parse_annotation base_classes
          | None -> []
        in

        let rec loop class_name =
          match Stdlib.Hashtbl.find_opt environment class_name with
          | Some params -> params
          | None ->
              let params = params_from_gp ~class_name in
              Stdlib.Hashtbl.add environment class_name params;
              let on_var _name _variance _inj = () in
              let on_edge = loop in
              VarianceVisitor.on_class ~class_name ~on_edge ~on_var ~to_list ~get_base_class_types;

              params
        in
        ignore (loop class_name);

        let rec fixpoint env =
          let environment' : VarianceVisitor.variance_env = Stdlib.Hashtbl.create 0 in
          let changed = ref false in
          Stdlib.Hashtbl.iter
            (fun class_name params ->
              let params' = Array.copy params in
              Stdlib.Hashtbl.add environment' class_name params';
              let on_var name variance inj =
                Array.iteri
                  ~f:(fun i (n, variance', inj') ->
                    if String.equal n name then
                      params'.(i) <- name, union (variance, variance'), inj || inj')
                  params'
              in

              let on_edge class_name = Stdlib.Hashtbl.find env class_name in

              VarianceVisitor.on_class ~class_name ~on_edge ~on_var ~to_list ~get_base_class_types;

              changed := !changed || VarianceVisitor.compare_t_param_array params params' = 1)
            env;
          if !changed then
            fixpoint environment'
          else
            environment'
        in

        let environment = fixpoint environment in

        let rec to_map = function
          | [] -> Identifier.Map.empty
          | (name, variance, inj) :: rest ->
              let inferred_variance =
                match Map.find post_inference_initial name with
                | Some Type.Record.Variance.Bivariant -> (
                    match variance, inj with
                    | _, false -> Type.Record.Variance.Bivariant
                    | Type.Record.Variance.Bivariant, _ -> Bivariant
                    | Covariant, _ -> Covariant
                    | Contravariant, _ -> Contravariant
                    | Invariant, _ -> Invariant)
                | Some res -> res
                | None -> failwith "Impossible. Class name must be present in initial variance map"
              in
              let map = to_map rest in
              Map.set ~key:name ~data:inferred_variance map
        in

        let class_variances =
          match Stdlib.Hashtbl.find_opt environment class_name with
          | Some params -> to_map (Array.to_list params)
          | None -> failwith "class_name is always present in the map"
        in

        class_variances

    (* Construct a ConstraintsSet.order representing the lattice of types for a
     * project. The order object is just a record of callbacks providing access to
     * shared memory for all cases where type order and constraint solving operations
     * require information about the codebase.
     *
     * Note that the assumption that we can create such an object without scope
     * is the root cause for why Pyre has to put classes nested inside functions
     * (which are not actually global) into the global symbol table.
     *)
    method full_order ~cycle_detections =
      let Queries.
            {
              is_protocol;
              class_hierarchy;
              has_transitive_successor;
              least_upper_bound;
              get_class_summary;
              successors;
              _;
            }
        =
        queries
      in
      let resolve class_type =
        match Type.class_attribute_lookups_for_type class_type with
        | None -> None
        | Some [] -> None
        | Some [resolved] -> Some resolved
        | Some (_ :: _) ->
            (* These come from calling attributes on Unions, which are handled by
               solve_less_or_equal indirectly by breaking apart the union before doing the
               instantiate_protocol_parameters. Therefore, there is no reason to deal with joining
               the attributes together here *)
            None
      in
      let attribute class_type ~cycle_detections ~name =
        resolve class_type
        >>= fun {
                  Type.type_for_lookup;
                  accessed_through_class;
                  class_name;
                  accessed_through_readonly;
                } ->
        self#attribute
          ~cycle_detections
          ~transitive:true
          ~accessed_through_class
          ~accessed_through_readonly
          ~include_generated_attributes:true
          ?special_method:None
          ~attribute_name:name
          ~type_for_lookup
          class_name
      in
      let instantiated_attributes class_type ~cycle_detections =
        resolve class_type
        >>= fun {
                  Type.type_for_lookup;
                  accessed_through_class;
                  class_name;
                  accessed_through_readonly;
                } ->
        self#uninstantiated_attributes
          ~cycle_detections
          ~transitive:true
          ~accessed_through_class
          ~include_generated_attributes:true
          ?special_method:None
          class_name
        >>| List.map
              ~f:
                (self#instantiate_attribute
                   ~cycle_detections
                   ~type_for_lookup
                   ~accessed_through_class
                   ~accessed_through_readonly
                   ?apply_descriptors:None)
      in
      let class_hierarchy_handler = class_hierarchy () in
      let metaclass class_name ~cycle_detections = self#metaclass class_name ~cycle_detections in
      let get_named_tuple_fields class_type =
        resolve class_type
        >>= fun { class_name; _ } ->
        (let successors = successors class_name in
         if List.exists ~f:(Identifier.equal "typing.NamedTuple") successors then
           List.find_map
             ~f:(fun class_name ->
               get_class_summary class_name
               >>| Node.value
               >>= ClassSummary.fields_tuple_value
               >>| List.map ~f:(fun name ->
                       attribute class_type ~cycle_detections ~name
                       >>| AnnotatedAttribute.annotation
                       >>| TypeInfo.Unit.annotation)
               >>| Option.all)
             (class_name :: successors)
         else
           None)
        |> Option.value ~default:None
      in
      {
        ConstraintsSet.class_hierarchy =
          {
            instantiate_successors_parameters =
              ClassHierarchy.instantiate_successors_parameters class_hierarchy_handler;
            has_transitive_successor;
            generic_parameters = ClassHierarchy.generic_parameters class_hierarchy_handler;
            least_upper_bound;
          };
        attribute;
        instantiated_attributes;
        is_protocol;
        cycle_detections;
        get_typed_dictionary = self#get_typed_dictionary ~cycle_detections;
        get_named_tuple_fields;
        metaclass;
        variance_map = self#variance_map ~cycle_detections;
      }

    (* Given an expression, produce a Type.t where literal type information - that is,
     * type information we can infer directly from the expression structure, including
     * constructor calls, is resolved.
     *
     * By "type information we can infer directly" I mean that in addition to
     * constructor calls we handle things like constant values, container literals,
     * comprehensions, await expressions, etc.
     *
     * This function is what determines scenarios where Pyre is able to automatically
     * infer global variable and class attribute types (Pyre does less inference in
     * these cases than normal type checking, because it does not track scope across
     * control flow).
     *
     * Any situation Pyre cannot resolve as a literal results in a `Type.Any`.
     *)
    method resolve_literal ~cycle_detections ~scoped_type_variables expression =
      let Queries.{ generic_parameters_as_variables; get_unannotated_global; _ } = queries in
      let open Ast.Expression in
      let is_concrete_class class_type =
        class_type
        |> Queries.class_summary_for_outer_type queries
        >>| (fun { Node.value = { name; _ }; _ } -> Reference.show name)
        >>= generic_parameters_as_variables ~empty_for_nongeneric:true
        >>| List.is_empty
      in
      let fully_specified_type = function
        | { Node.value = Expression.Name name; _ } as annotation when is_simple_name name ->
            let class_type =
              self#parse_annotation ~cycle_detections annotation ~scoped_type_variables
            in
            if
              Type.is_none class_type || is_concrete_class class_type |> Option.value ~default:false
            then
              Some class_type
            else
              None
        | { Node.value = Subscript { base = { Node.value = Name generic_name; _ }; _ }; _ } as
          annotation
          when is_simple_name generic_name ->
            let class_type =
              self#parse_annotation ~cycle_detections annotation ~scoped_type_variables
            in
            if is_concrete_class class_type >>| not |> Option.value ~default:false then
              Some class_type
            else
              None
        | _ -> None
      in
      let resolve_name expression =
        if has_identifier_base expression then
          match fully_specified_type expression with
          | Some annotation ->
              if Type.is_none annotation then
                Type.none
              else
                Type.class_type annotation
          | None -> Type.Any
        else
          Type.Any
      in
      let order = self#full_order ~cycle_detections in
      match Node.value expression with
      | Expression.Await expression ->
          self#resolve_literal ~cycle_detections expression ~scoped_type_variables
          |> Type.awaitable_value
          |> Option.value ~default:Type.Any
      | BooleanOperator { BooleanOperator.left; right; _ } ->
          let annotation =
            TypeOrder.join
              order
              (self#resolve_literal ~cycle_detections ~scoped_type_variables left)
              (self#resolve_literal ~cycle_detections ~scoped_type_variables right)
          in
          if Type.is_concrete annotation then annotation else Type.Any
      | Call { callee; _ } ->
          (* Constructor on concrete class. *)
          Option.value (fully_specified_type callee) ~default:Type.Any
      | Subscript { base; _ } -> (
          match fully_specified_type expression with
          | Some annotation ->
              (* Literal generic type, e.g. global = List[int] *)
              Type.class_type annotation
          | None ->
              (* Constructor on concrete class or fully specified generic,
               * e.g. global = GenericClass[int](x, y) or global = ConcreteClass(x) *)
              Option.value (fully_specified_type base) ~default:Type.Any)
      | Constant Constant.NoneLiteral -> Type.Any
      | Constant (Constant.Complex _) -> Type.complex
      | Constant (Constant.False | Constant.True) -> Type.bool
      | Constant (Constant.Float _) -> Type.float
      | Constant (Constant.Integer _) -> Type.integer
      | Constant (Constant.String { StringLiteral.kind; _ }) -> (
          match kind with
          | StringLiteral.Bytes -> Type.bytes
          | _ -> Type.string)
      | FormatString _ -> Type.string
      | Name name when is_simple_name name -> (
          let reference = name_to_reference_exn name in
          match get_unannotated_global reference with
          | Some (Module.UnannotatedGlobal.Define signatures) ->
              let decorated =
                List.map signatures ~f:(fun { signature; _ } -> signature)
                |> List.partition_tf ~f:Define.Signature.is_overloaded_function
                |> fun (overloads, implementations) ->
                self#resolve_define
                  ~cycle_detections
                  ~callable_name:(Some reference)
                  ~implementation:(List.last implementations)
                  ~overloads
                  ~scoped_type_variables
              in
              Result.ok decorated |> Option.value ~default:Type.Any
          | _ -> resolve_name expression)
      | Name _ -> resolve_name expression
      | Dictionary entries when Dictionary.has_no_keywords entries ->
          let key_annotation, value_annotation =
            let join_entry (key_annotation, value_annotation) entry =
              let open Dictionary.Entry in
              match entry with
              | KeyValue { key; value } ->
                  ( TypeOrder.join
                      order
                      key_annotation
                      (self#resolve_literal ~cycle_detections ~scoped_type_variables key),
                    TypeOrder.join
                      order
                      value_annotation
                      (self#resolve_literal ~cycle_detections ~scoped_type_variables value) )
              | Splat _ -> key_annotation, value_annotation
            in
            List.fold ~init:(Type.Bottom, Type.Bottom) ~f:join_entry entries
          in
          let key = if Type.is_concrete key_annotation then key_annotation else Type.Any in
          let value = if Type.is_concrete value_annotation then value_annotation else Type.Any in
          Type.dictionary ~key ~value
      | Dictionary _ -> Type.dictionary ~key:Type.Any ~value:Type.Any
      | List elements ->
          let argument =
            let join sofar element =
              TypeOrder.join
                order
                sofar
                (self#resolve_literal ~cycle_detections ~scoped_type_variables element)
            in
            List.fold ~init:Type.Bottom ~f:join elements
          in
          if Type.is_concrete argument then Type.list argument else Type.list Type.Any
      | Set elements ->
          let argument =
            let join sofar element =
              TypeOrder.join
                order
                sofar
                (self#resolve_literal ~cycle_detections ~scoped_type_variables element)
            in
            List.fold ~init:Type.Bottom ~f:join elements
          in
          if Type.is_concrete argument then Type.set argument else Type.set Type.Any
      | Ternary { Ternary.target; alternative; _ } ->
          let annotation =
            TypeOrder.join
              order
              (self#resolve_literal ~cycle_detections ~scoped_type_variables target)
              (self#resolve_literal ~cycle_detections ~scoped_type_variables alternative)
          in
          if Type.is_concrete annotation then annotation else Type.Any
      | Tuple elements ->
          Type.tuple
            (List.map elements ~f:(self#resolve_literal ~cycle_detections ~scoped_type_variables))
      | Expression.Yield _ -> Type.yield Type.Any
      | _ -> Type.Any

    (* Given a Type.Callable.t representing the type of a bare function signature,
     * apply decorators to get the decorated type.
     *
     * We return both the resulting type, which may be transformed by the decorators,
     * and a list of "problems" that we encountered when trying to apply decorators
     * which can occur for various reasons ranging from not being able to find the
     * decorator at all to a signature selection error checking the decorator call.
     *
     * The type output is used in three main places:
     * - as part of instantiating an attribute, to get decorated method types
     * - in `global_annotation` to get the decorated type of global functions
     * - within `typeCheck.ml` to get the decorated type of inner, nested defines
     *
     * The problems are converted into `AnalysisError` values in typeCheck.ml, if
     * the module where this decorator is called is type checked.
     *)
    method apply_decorators
        ~cycle_detections
        ~scoped_type_variables:outer_scope_type_variables
        undecorated_signature
        decorators =
      let Queries.{ resolve_exports; _ } = queries in
      let apply_decorator argument (index, decorator) =
        let make_error reason =
          Result.Error (AnnotatedAttribute.InvalidDecorator { index; reason })
        in
        match Decorator.from_expression decorator with
        | None -> make_error CouldNotResolve
        | Some { Decorator.name; arguments } -> (
            let name = Node.value name |> Reference.delocalize in
            let decorator = resolve_exports name in
            let simple_decorator_name =
              match decorator with
              | Some (ModuleAttribute { from; name; remaining; _ }) ->
                  Reference.create_from_list (Reference.as_list from @ (name :: remaining))
                  |> Reference.show
              | _ -> Reference.show name
            in
            match simple_decorator_name, argument with
            | ( ("click.decorators.pass_context" | "click.decorators.pass_obj"),
                Type.Callable callable ) ->
                (* Suppress caller/callee parameter matching by altering the click entry point to
                   have a generic parameter list. *)
                let parameters =
                  Type.Callable.Defined
                    [
                      Type.Callable.CallableParamType.Variable (Concrete Type.Any);
                      Type.Callable.CallableParamType.Keywords Type.Any;
                    ]
                in
                Type.Callable (Type.Callable.map_parameters callable ~f:(fun _ -> parameters))
                |> Result.return
            | name, Callable callable
              when String.equal name "contextlib.asynccontextmanager"
                   || Set.mem Recognized.asyncio_contextmanager_decorators name ->
                let process_overload ({ Type.Callable.annotation; _ } as overload) =
                  let joined =
                    let order = self#full_order ~cycle_detections in
                    try TypeOrder.join order annotation (Type.async_iterator Type.Bottom) with
                    | ClassHierarchy.Untracked _ ->
                        (* create_overload gets called when building the environment, which is
                           unsound and can raise. *)
                        Type.Any
                  in
                  if Type.is_async_iterator joined then
                    {
                      overload with
                      Type.Callable.annotation =
                        Type.parametric
                          "typing.AsyncContextManager"
                          [Single (Type.single_argument joined)];
                    }
                  else
                    overload
                in
                let {
                  Type.Callable.implementation = old_implementation;
                  overloads = old_overloads;
                  _;
                }
                  =
                  callable
                in
                Type.Callable
                  {
                    callable with
                    implementation = process_overload old_implementation;
                    overloads = List.map old_overloads ~f:process_overload;
                  }
                |> Result.return
            | name, callable when String.is_suffix name ~suffix:".validator" ->
                (* TODO(T70606997): We should be type checking attr validators properly. *)
                Result.return callable
            | "contextlib.contextmanager", Callable callable ->
                let process_overload ({ Type.Callable.annotation; _ } as overload) =
                  let joined =
                    let order = self#full_order ~cycle_detections in
                    try TypeOrder.join order annotation (Type.iterator Type.Bottom) with
                    | ClassHierarchy.Untracked _ ->
                        (* create_overload gets called when building the environment, which is
                           unsound and can raise. *)
                        Type.Any
                  in
                  if Type.is_iterator joined then
                    {
                      overload with
                      Type.Callable.annotation =
                        Type.parametric
                          "contextlib._GeneratorContextManager"
                          [Single (Type.single_argument joined)];
                    }
                  else
                    overload
                in
                let {
                  Type.Callable.implementation = old_implementation;
                  overloads = old_overloads;
                  _;
                }
                  =
                  callable
                in
                Type.Callable
                  {
                    callable with
                    implementation = process_overload old_implementation;
                    overloads = List.map old_overloads ~f:process_overload;
                  }
                |> Result.return
            | name, argument when Set.mem Decorators.special_decorators name ->
                Decorators.apply ~argument ~name |> Result.return
            | name, _ when Set.mem Recognized.classmethod_decorators name ->
                (* TODO (T67024249): convert these to just normal stubs *)
                Type.parametric "typing.ClassMethod" [Single argument] |> Result.return
            | "staticmethod", _ ->
                Type.parametric "typing.StaticMethod" [Single argument] |> Result.return
            | _ -> (
                let { decorators_being_resolved; _ } = cycle_detections in
                if
                  CycleDetection.DecoratorsBeingResolved.not_a_decorator
                    decorators_being_resolved
                    ~candidate:name
                then
                  make_error CouldNotResolve
                else
                  let cycle_detections =
                    {
                      cycle_detections with
                      decorators_being_resolved =
                        CycleDetection.DecoratorsBeingResolved.add
                          decorators_being_resolved
                          ~assume_is_not_a_decorator:name;
                    }
                  in
                  let resolve_attribute_access ?special_method base ~attribute_name =
                    let access
                        {
                          Type.type_for_lookup;
                          accessed_through_class;
                          class_name;
                          accessed_through_readonly;
                        }
                      =
                      self#attribute
                        ~cycle_detections
                        ~transitive:true
                        ~accessed_through_class
                        ~accessed_through_readonly
                        ~include_generated_attributes:true
                        ?special_method
                        ~attribute_name
                        ~type_for_lookup
                        class_name
                    in
                    let join_all = function
                      | head :: tail ->
                          let order = self#full_order ~cycle_detections in
                          List.fold tail ~init:head ~f:(TypeOrder.join order) |> Option.some
                      | [] -> None
                    in
                    Type.class_attribute_lookups_for_type base
                    >>| List.map ~f:access
                    >>= Option.all
                    >>| List.map ~f:AnnotatedAttribute.annotation
                    >>| List.map ~f:TypeInfo.Unit.annotation
                    >>= join_all
                  in
                  let resolver = function
                    | ResolvedReference.Module _ -> None
                    | ModuleAttribute { from; name; remaining; _ } ->
                        let rec resolve_remaining base ~remaining =
                          match remaining with
                          | [] -> Some base
                          | attribute_name :: remaining ->
                              resolve_attribute_access base ~attribute_name
                              >>= resolve_remaining ~remaining
                        in
                        Reference.create_from_list [name]
                        |> Reference.combine from
                        |> self#global_annotation ~cycle_detections
                        >>| (fun { Global.type_info = { annotation; _ }; _ } -> annotation)
                        >>= resolve_remaining ~remaining
                  in
                  let extract_callable = function
                    | Type.Callable callable -> Some callable
                    | other -> (
                        match
                          resolve_attribute_access
                            other
                            ~attribute_name:"__call__"
                            ~special_method:true
                        with
                        | None -> None
                        | Some (Type.Callable callable) -> Some callable
                        | Some other -> (
                            (* We potentially need to go specifically two layers in order to support
                               when name resolves to Type[X], which has a __call__ of its
                               constructor that is itself a BoundMethod, which has a Callable
                               __call__ *)
                            match
                              resolve_attribute_access
                                other
                                ~attribute_name:"__call__"
                                ~special_method:true
                            with
                            | Some (Callable callable) -> Some callable
                            | _ -> None))
                  in
                  let apply_arguments_to_decorator_factory ~factory_callable ~arguments =
                    let arguments =
                      let resolve argument_index argument =
                        let expression, kind = Ast.Expression.Call.Argument.unpack argument in
                        let make_matched_argument resolved =
                          { Argument.kind; expression = Some expression; resolved }
                        in
                        let error = AnnotatedAttribute.CouldNotResolveArgument { argument_index } in
                        match expression with
                        | {
                         Node.value = Expression.Expression.Constant Expression.Constant.NoneLiteral;
                         _;
                        } ->
                            Ok (make_matched_argument Type.NoneType)
                        | { Node.value = Expression.Expression.Name name; _ } ->
                            Expression.name_to_reference name
                            >>| Reference.delocalize
                            >>= resolve_exports
                            >>= resolver
                            >>| make_matched_argument
                            |> Result.of_option
                                 ~error:
                                   (AnnotatedAttribute.InvalidDecorator { index; reason = error })
                        | expression ->
                            let resolved =
                              self#resolve_literal
                                ~cycle_detections
                                ~scoped_type_variables:outer_scope_type_variables
                                expression
                            in
                            if Type.is_untyped resolved || Type.contains_unknown resolved then
                              make_error error
                            else
                              Ok (make_matched_argument resolved)
                      in
                      List.mapi arguments ~f:resolve |> Result.all
                    in
                    let select arguments =
                      self#signature_select
                        ~cycle_detections
                        ~resolve_with_locals:(fun ~locals:_ _ -> Type.Top)
                        ~arguments
                        ~location:Location.any
                        ~callable:factory_callable
                        ~self_argument:None
                        ~skip_marking_escapees:false
                    in
                    let extract = function
                      | SignatureSelectionTypes.Found { selected_return_annotation; _ } ->
                          Result.Ok selected_return_annotation
                      | NotFound { reason; _ } ->
                          make_error
                            (FactorySignatureSelectionFailed { reason; callable = factory_callable })
                    in
                    Result.map arguments ~f:select |> Result.bind ~f:extract
                  in
                  let resolved_decorator =
                    match decorator >>= resolver with
                    | None -> make_error CouldNotResolve
                    | Some Any -> Ok Type.Any
                    | Some fetched -> (
                        match arguments with
                        | None -> Ok fetched
                        | Some arguments -> (
                            match extract_callable fetched with
                            | None -> make_error (NonCallableDecoratorFactory fetched)
                            | Some factory_callable ->
                                apply_arguments_to_decorator_factory ~factory_callable ~arguments))
                  in
                  match resolved_decorator with
                  | Error error -> Result.Error error
                  | Ok Any -> Ok Any
                  | Ok resolved_decorator -> (
                      match extract_callable resolved_decorator with
                      | None -> make_error (NonCallableDecorator resolved_decorator)
                      | Some callable -> (
                          match
                            self#signature_select
                              ~cycle_detections
                              ~resolve_with_locals:(fun ~locals:_ _ -> Type.object_primitive)
                              ~arguments:
                                [{ kind = Positional; expression = None; resolved = argument }]
                              ~location:Location.any
                              ~callable
                              ~self_argument:None
                              ~skip_marking_escapees:false
                          with
                          | SignatureSelectionTypes.Found { selected_return_annotation; _ } ->
                              Ok selected_return_annotation
                          | NotFound { reason; _ } ->
                              make_error (ApplicationFailed { reason; callable })))))
      in
      let kind = undecorated_signature.Type.Callable.kind in
      let applied =
        List.mapi decorators ~f:(fun index decorator -> index, decorator)
        |> List.rev
        |> List.fold_result ~init:(Type.Callable undecorated_signature) ~f:apply_decorator
      in
      (* If the decorator preserves the function's type signature, preserve the function name. This
         leads to better error messages, since we can print the function's name instead of
         considering it an "anonymous call". *)
      let should_preserve_function_name ~undecorated_signature ~kind callable =
        (* Some decorators expect and return `Callable[P, Awaitable[T]]`. But the return type for
           `async def` is `Coroutine[_, _, X]`, which means that the signatures are slightly
           different before and after decorating. Ignore the difference. *)
        let replace_coroutine_with_awaitable return_type =
          Type.coroutine_value return_type >>| Type.awaitable |> Option.value ~default:return_type
        in
        let signature_with_awaitable_return_type =
          Type.Callable.map_annotation undecorated_signature ~f:replace_coroutine_with_awaitable
        in
        Type.Callable.equal { callable with kind } undecorated_signature
        || Type.Callable.equal { callable with kind } signature_with_awaitable_return_type
      in
      match applied with
      | Result.Ok (Type.Callable callable)
        when should_preserve_function_name ~undecorated_signature ~kind callable ->
          Ok (Type.Callable { callable with kind })
      | Result.Ok
          (Type.Parametric
            {
              name = ("typing.ClassMethod" | "typing.StaticMethod") as parametric_name;
              arguments = [Single (Type.Callable callable)];
            })
        when should_preserve_function_name ~undecorated_signature ~kind callable ->
          Ok (Type.parametric parametric_name [Single (Type.Callable { callable with kind })])
      | other -> other

    (* Resolve a define signature to a Type.t, accounting for decorators.
     *
     * Implementation just pipelines `resolve_define_undecorated` with `apply_decorators`;
     * these stages are separated so that uninstantiated attributes can defer decorator
     * handling which is important for cache coherence.
     *)
    method resolve_define
        ~cycle_detections
        ~callable_name
        ~implementation
        ~overloads
        ~scoped_type_variables:outer_scope_type_variables =
      let { AnnotatedAttribute.undecorated_signature; decorators } =
        self#resolve_define_undecorated
          ~cycle_detections
          ~callable_name
          ~implementation
          ~overloads
          ~scoped_type_variables:outer_scope_type_variables
      in
      let apply_decorators =
        self#apply_decorators
          ~cycle_detections
          ~scoped_type_variables:outer_scope_type_variables
          undecorated_signature
      in
      Result.bind decorators ~f:apply_decorators

    (* Given a call whose callee has type `callable` with arguments `arguments`, resolve the
     * call which either produces a `Type.t` for the return, or information about why
     * resolution failed.
     *
     * The implementation chains the `SignatureSelection` functions
     * `select_closest_signature_for_function_call` to pick an overload
     * and `instantiate_return_annotation`, injecting callbacks to global symbol
     * tables as needed.
     *)
    method signature_select
        ~cycle_detections
        ~resolve_with_locals
        ~arguments
        ~location
        ~callable
        ~self_argument
        ~skip_marking_escapees =
      let order = self#full_order ~cycle_detections in
      SignatureSelection.select_closest_signature_for_function_call
        ~order
        ~resolve_with_locals
        ~resolve_mutable_literals:(self#resolve_mutable_literals ~cycle_detections)
        ~get_typed_dictionary:(self#get_typed_dictionary ~cycle_detections)
        ~arguments
        ~location
        ~callable
        ~self_argument
      >>| SignatureSelection.instantiate_return_annotation ~skip_marking_escapees ~order
      |> Option.value ~default:(SignatureSelection.default_instantiated_return_annotation callable)

    (* Wrap `weaken_mutable_literals` with callbacks powering constraint solving
     * and typed dictionary finding with global symbol tables.
     *)
    method resolve_mutable_literals ~cycle_detections ~resolve =
      WeakenMutableLiterals.weaken_mutable_literals
        ~resolve
        ~get_typed_dictionary:(self#get_typed_dictionary ~cycle_detections)
        ~comparator:(self#constraints_solution_exists ~cycle_detections)

    (* Use constraint solving to determine whether `left` is a gradual subtype of `right`.
     *
     * For the most part this is just the constraint set operations
     * `add_and_simplify` |> `solve`, with callbacks to global symbol tables.
     *
     * The `get_typed_dictinary_override` argument is used by `weaken_mutable_literals`
     * to handle recursive typed dictionaries; all other callsites pass `fun _ -> None`.
     *)
    method constraints_solution_exists ~cycle_detections ~get_typed_dictionary_override ~left ~right
        =
      let ({ ConstraintsSet.get_typed_dictionary; _ } as order) =
        self#full_order ~cycle_detections
      in
      let order =
        {
          order with
          get_typed_dictionary =
            (fun annotation ->
              Option.first_some
                (get_typed_dictionary_override annotation)
                (get_typed_dictionary annotation));
        }
      in
      TypeOrder.OrderedConstraintsSet.add_and_simplify
        ConstraintsSet.empty
        ~new_constraint:(LessOrEqual { left; right })
        ~order
      |> TypeOrder.OrderedConstraintsSet.solve ~order
      |> Option.is_some

    (* Given the fully-qualified name of a global (i.e. a top-level name of a module),
     * produce a Global.t indicating its type and any problems we encountered (in particular,
     * failure to resolve decorators) finding that type.
     *
     * This computation is at the core of Pyre's primary global symbol table that powers
     * everything except class attribute lookups. It can be thought of as lifting
     * the globals from `UnannotatedGlobalEnvironment` to the type level.
     *)
    method global_annotation ~cycle_detections name =
      let Queries.{ class_exists; get_unannotated_global; _ } = queries in
      let process_unannotated_global global =
        let produce_assignment_global ~is_explicit ~is_final annotation =
          let original =
            if is_explicit then
              None
            else if
              (* Treat literal globals as having been explicitly annotated. *)
              Type.is_partially_typed annotation
            then
              Some Type.Top
            else
              None
          in
          TypeInfo.Unit.create_immutable ~final:is_final ~original annotation
        in
        match global with
        | Module.UnannotatedGlobal.Define signatures ->
            let overloads, implementation =
              List.map signatures ~f:(fun { signature; _ } -> signature)
              |> List.partition_tf ~f:Define.Signature.is_overloaded_function
              |> fun (overloads, implementations) -> overloads, List.last implementations
            in
            let callable_name = callable_name_of_public ~implementation ~overloads in
            let { AnnotatedAttribute.undecorated_signature; decorators } =
              self#resolve_define_undecorated
                ~callable_name
                ~implementation
                ~scoped_type_variables:None
                ~overloads
                ~cycle_detections
            in
            let decorated =
              Result.bind
                decorators
                ~f:
                  (self#apply_decorators
                     ~scoped_type_variables:None
                     ~cycle_detections
                     undecorated_signature)
            in
            let type_info =
              Result.ok decorated
              |> Option.value ~default:Type.Any
              |> TypeInfo.Unit.create_immutable
            in
            Some
              {
                Global.type_info;
                undecorated_signature = Some undecorated_signature;
                problem = Result.error decorated;
              }
        | SimpleAssign
            {
              explicit_annotation = None;
              value =
                Some
                  {
                    Node.value =
                      Call
                        {
                          callee =
                            {
                              value =
                                Name
                                  (Attribute
                                    {
                                      base = { Node.value = Name (Identifier "typing"); _ };
                                      attribute = "TypeAlias";
                                      _;
                                    });
                              _;
                            };
                          _;
                        };
                    _;
                  };
              target_location = location;
            } ->
            let location = Location.strip_module location in
            Ast.Expression.Expression.Name (Expression.create_name_from_reference ~location name)
            |> Node.create ~location
            |> self#parse_annotation
                 ~validation:ValidatePrimitives
                 ~cycle_detections
                 ~scoped_type_variables:None
            |> Type.class_type
            |> TypeInfo.Unit.create_immutable
            |> fun type_info ->
            Some { Global.type_info; undecorated_signature = None; problem = None }
        | SimpleAssign { explicit_annotation; value; _ } -> (
            let explicit_annotation =
              explicit_annotation
              >>| self#parse_annotation ~cycle_detections ~scoped_type_variables:None
              >>= fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation
            in
            let annotation, is_explicit, is_final =
              match explicit_annotation with
              | None ->
                  ( value >>| self#resolve_literal ~cycle_detections ~scoped_type_variables:None,
                    false,
                    false )
              | Some explicit -> (
                  match Type.final_value explicit with
                  | `Ok final_value -> Some final_value, true, true
                  | `NoArgument ->
                      ( value >>| self#resolve_literal ~cycle_detections ~scoped_type_variables:None,
                        false,
                        true )
                  | `NotFinal -> Some explicit, true, false)
            in
            match annotation with
            | Some annotation ->
                produce_assignment_global ~is_explicit ~is_final annotation
                |> fun type_info ->
                Some { Global.type_info; undecorated_signature = None; problem = None }
            | _ -> None)
        | TupleAssign { value = Some value; index; total_length; _ } ->
            let extracted =
              match self#resolve_literal ~cycle_detections ~scoped_type_variables:None value with
              | Type.Tuple (Concrete arguments) when List.length arguments = total_length ->
                  List.nth arguments index
                  (* This should always be Some, but I don't think its worth being fragile here *)
                  |> Option.value ~default:Type.Top
              | Type.Tuple (Concatenation concatenation) ->
                  Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
                  |> Option.value ~default:Type.Top
              | _ -> Type.Top
            in
            produce_assignment_global ~is_explicit:false ~is_final:false extracted
            |> fun type_info ->
            Some { Global.type_info; undecorated_signature = None; problem = None }
        | _ -> None
      in
      let class_lookup = Reference.show name |> class_exists in
      if class_lookup then
        let primitive = Type.Primitive (Reference.show name) in
        TypeInfo.Unit.create_immutable (Type.class_type primitive)
        |> fun type_info -> Some { Global.type_info; undecorated_signature = None; problem = None }
      else
        get_unannotated_global name
        >>= fun global ->
        let timer = Timer.start () in
        let result = process_unannotated_global global in
        Statistics.performance
          ~flush:false
          ~randomly_log_every:500
          ~always_log_time_threshold:1.0 (* Seconds *)
          ~section:`Check
          ~name:"SingleGlobalTypeCheck"
          ~timer
          ~normals:["name", Reference.show name; "request kind", "SingleGlobalTypeCheck"]
          ();
        result
  end

let empty_cycle_detections =
  {
    decorators_being_resolved = DecoratorsBeingResolved.empty;
    assumed_callable_types = AssumedCallableTypes.empty;
    assumed_recursive_instantiations = AssumedRecursiveInstantiations.empty;
  }


module OutgoingDataComputation = struct
  module Queries = struct
    type t = { global_annotation: Reference.t -> Global.t option }
  end

  let global Queries.{ global_annotation; _ } reference =
    match Reference.last reference with
    | "__doc__"
    | "__file__"
    | "__name__"
    | "__package__" ->
        let type_info = TypeInfo.Unit.create_immutable Type.string in
        Some { Global.type_info; undecorated_signature = None; problem = None }
    | "__path__" ->
        let type_info = Type.list Type.string |> TypeInfo.Unit.create_immutable in
        Some { Global.type_info; undecorated_signature = None; problem = None }
    | "__dict__" ->
        let type_info =
          Type.dictionary ~key:Type.string ~value:Type.Any |> TypeInfo.Unit.create_immutable
        in
        Some { type_info; undecorated_signature = None; problem = None }
    | _ -> global_annotation reference
end

let class_hierarchy_environment class_metadata_environment =
  ClassSuccessorMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment


let alias_environment class_metadata_environment =
  ClassHierarchyEnvironment.ReadOnly.alias_environment
    (class_hierarchy_environment class_metadata_environment)


let unannotated_global_environment class_metadata_environment =
  alias_environment class_metadata_environment
  |> TypeAliasEnvironment.ReadOnly.unannotated_global_environment


let create_queries ~class_metadata_environment ~dependency =
  Queries.
    {
      controls =
        ClassSuccessorMetadataEnvironment.MetadataReadOnly.controls class_metadata_environment;
      resolve_exports =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.resolve_exports ?dependency;
      is_protocol =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.is_protocol ?dependency;
      get_unannotated_global =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global ?dependency;
      get_class_summary =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.get_class_summary ?dependency;
      first_matching_class_decorator =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.first_matching_class_decorator ?dependency;
      exists_matching_class_decorator =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.exists_matching_class_decorator ?dependency;
      class_exists =
        unannotated_global_environment class_metadata_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.class_exists ?dependency;
      parse_annotation_without_sanitizing_type_arguments =
        alias_environment class_metadata_environment
        |> TypeAliasEnvironment.ReadOnly.parse_annotation_without_sanitizing_type_arguments
             ?dependency;
      param_spec_from_vararg_annotations =
        alias_environment class_metadata_environment
        |> TypeAliasEnvironment.ReadOnly.param_spec_from_vararg_annotations ?dependency;
      generic_parameters_as_variables =
        class_hierarchy_environment class_metadata_environment
        |> ClassHierarchyEnvironment.ReadOnly.generic_parameters_as_variables ?dependency;
      class_hierarchy =
        (fun () ->
          class_hierarchy_environment class_metadata_environment
          |> ClassHierarchyEnvironment.ReadOnly.class_hierarchy ?dependency);
      successors =
        ClassSuccessorMetadataEnvironment.ReadOnly.successors ?dependency class_metadata_environment;
      least_upper_bound =
        ClassSuccessorMetadataEnvironment.ReadOnly.least_upper_bound
          ?dependency
          class_metadata_environment;
      is_typed_dictionary =
        ClassSuccessorMetadataEnvironment.ReadOnly.is_class_typed_dictionary
          ?dependency
          class_metadata_environment;
      extends_enum =
        ClassSuccessorMetadataEnvironment.ReadOnly.does_class_extend_enum
          ?dependency
          class_metadata_environment;
      has_transitive_successor =
        ClassSuccessorMetadataEnvironment.ReadOnly.has_transitive_successor
          ?dependency
          class_metadata_environment;
      get_class_metadata =
        ClassSuccessorMetadataEnvironment.ReadOnly.get_class_metadata
          ?dependency
          class_metadata_environment;
      get_variable =
        alias_environment class_metadata_environment
        |> TypeAliasEnvironment.ReadOnly.get_variable ?dependency;
    }


module MetaclassCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = ClassSuccessorMetadataEnvironment

    module Key = struct
      type t = string [@@deriving compare, show, sexp, hash]

      let to_string = show

      let from_string = Fn.id
    end

    module Value = struct
      type t = Type.t option [@@deriving equal]

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "metaclasses"
    end

    module KeySet = String.Set
    module HashableKey = String

    let lazy_incremental = false

    let produce_value class_metadata_environment key ~dependency =
      let queries = create_queries ~class_metadata_environment ~dependency in
      let implementation = new base ~queries in
      implementation#metaclass key ~cycle_detections:empty_cycle_detections


    let filter_upstream_dependency = function
      | SharedMemoryKeys.Metaclass key -> Some key
      | _ -> None


    let trigger_to_dependency key = SharedMemoryKeys.Metaclass key

    let overlay_owns_key source_code_overlay =
      SourceCodeIncrementalApi.Overlay.owns_identifier source_code_overlay
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    class with_parse_annotation_and_metaclass_caches dependency read_only =
      object
        inherit
          base
            ~queries:
              (create_queries
                 ~class_metadata_environment:(upstream_environment read_only)
                 ~dependency)

        method! metaclass ~cycle_detections:_ key = get read_only ?dependency key
      end
  end
end

module AttributeCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = MetaclassCache
    module Key = SharedMemoryKeys.AttributeTableKey

    module Value = struct
      type t = UninstantiatedAttributeTable.t option [@@deriving compare]

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "attributes"

      let equal = Memory.equal_from_compare compare
    end

    module KeySet = SharedMemoryKeys.AttributeTableKey.Set
    module HashableKey = SharedMemoryKeys.AttributeTableKey

    let lazy_incremental = true

    let produce_value
        metaclass_cache
        {
          SharedMemoryKeys.AttributeTableKey.include_generated_attributes;
          accessed_via_metaclass;
          name;
        }
        ~dependency
      =
      let implementation_with_cached_parse_annotation =
        new MetaclassCache.ReadOnly.with_parse_annotation_and_metaclass_caches
          dependency
          metaclass_cache
      in
      implementation_with_cached_parse_annotation#single_uninstantiated_attribute_table
        ~include_generated_attributes
        ~accessed_via_metaclass
        ~cycle_detections:empty_cycle_detections
        name


    let filter_upstream_dependency = function
      | SharedMemoryKeys.AttributeTable key -> Some key
      | _ -> None


    let trigger_to_dependency key = SharedMemoryKeys.AttributeTable key

    let overlay_owns_key source_code_overlay { SharedMemoryKeys.AttributeTableKey.name; _ } =
      SourceCodeIncrementalApi.Overlay.owns_identifier source_code_overlay name
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    let metaclass_cache = upstream_environment

    let cached_single_uninstantiated_attribute_table
        read_only
        dependency
        ~include_generated_attributes
        ~accessed_via_metaclass
        name
      =
      get
        read_only
        ?dependency
        {
          SharedMemoryKeys.AttributeTableKey.include_generated_attributes;
          accessed_via_metaclass;
          name;
        }


    class with_parse_annotation_metaclass_and_attribute_caches dependency read_only =
      object
        inherit
          MetaclassCache.ReadOnly.with_parse_annotation_and_metaclass_caches
            dependency
            (metaclass_cache read_only)

        method! single_uninstantiated_attribute_table ~cycle_detections:_ =
          cached_single_uninstantiated_attribute_table read_only dependency
      end
  end
end

module GlobalAnnotationCache = struct
  module Cache = Environment.EnvironmentTable.WithCache (struct
    let show_key = Reference.show

    let overlay_owns_key source_code_overlay =
      SourceCodeIncrementalApi.Overlay.owns_reference source_code_overlay


    module PreviousEnvironment = AttributeCache
    module Key = SharedMemoryKeys.ReferenceKey

    module Value = struct
      type t = Global.t option

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "Global"

      let equal = Memory.equal_from_compare (Option.compare Global.compare)
    end

    type trigger = Reference.t [@@deriving sexp, compare]

    let convert_trigger = Fn.id

    let key_to_trigger = Fn.id

    module TriggerSet = Reference.Set

    let lazy_incremental = false

    let produce_value attribute_cache key ~dependency =
      let implementation_with_preceding_caches =
        new AttributeCache.ReadOnly.with_parse_annotation_metaclass_and_attribute_caches
          dependency
          attribute_cache
      in
      implementation_with_preceding_caches#global_annotation
        ~cycle_detections:empty_cycle_detections
        key


    let filter_upstream_dependency = function
      | SharedMemoryKeys.AnnotateGlobal name -> Some name
      | _ -> None


    let trigger_to_dependency name = SharedMemoryKeys.AnnotateGlobal name

    let equal_value = Option.equal [%compare.equal: Global.t]
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    let attribute_cache = upstream_environment

    class with_all_caches dependency read_only =
      object
        inherit
          AttributeCache.ReadOnly.with_parse_annotation_metaclass_and_attribute_caches
            dependency
            (attribute_cache read_only)

        method! global_annotation ~cycle_detections:_ = get read_only ?dependency
      end
  end
end

module PreviousEnvironment = ClassSuccessorMetadataEnvironment
include GlobalAnnotationCache

module ReadOnly = struct
  include GlobalAnnotationCache.ReadOnly

  let attribute_cache = upstream_environment

  let metaclass_cache read_only =
    attribute_cache read_only |> AttributeCache.ReadOnly.upstream_environment


  let class_metadata_environment read_only =
    metaclass_cache read_only |> MetaclassCache.ReadOnly.upstream_environment


  class with_uninstantiated_attributes_cache dependency read_only =
    object
      inherit
        base
          ~queries:
            (create_queries
               ~class_metadata_environment:(class_metadata_environment read_only)
               ~dependency)

      method! single_uninstantiated_attribute_table ~cycle_detections:_ =
        AttributeCache.ReadOnly.cached_single_uninstantiated_attribute_table
          (attribute_cache read_only)
          dependency
    end

  let add_all_caches_and_empty_cycle_detections f read_only ?dependency =
    new GlobalAnnotationCache.ReadOnly.with_all_caches dependency read_only
    |> f
    |> fun method_ -> method_ ~cycle_detections:empty_cycle_detections


  let variance_map read_only =
    let queries =
      create_queries
        ~dependency:None
        ~class_metadata_environment:(class_metadata_environment read_only)
    in
    let implementation = new base ~queries in
    implementation#variance_map ~cycle_detections:empty_cycle_detections


  let instantiate_attribute =
    add_all_caches_and_empty_cycle_detections (fun o ->
        o#instantiate_attribute ?apply_descriptors:None)


  let attribute =
    add_all_caches_and_empty_cycle_detections (fun o -> o#attribute ?apply_descriptors:None)


  let uninstantiated_attributes =
    add_all_caches_and_empty_cycle_detections (fun o -> o#uninstantiated_attributes)


  let validate_and_sanitize_type_arguments =
    add_all_caches_and_empty_cycle_detections (fun o ->
        o#validate_and_sanitize_type_arguments ~replace_unbound_parameters_with_any:true)


  let parse_annotation read_only ?dependency ~scoped_type_variables =
    let attributes_cached_but_not_annotations =
      new with_uninstantiated_attributes_cache dependency read_only
    in
    attributes_cached_but_not_annotations#parse_annotation
      ~cycle_detections:empty_cycle_detections
      ~scoped_type_variables


  let metaclass = add_all_caches_and_empty_cycle_detections (fun o -> o#metaclass)

  let resolve_define = add_all_caches_and_empty_cycle_detections (fun o -> o#resolve_define)

  let resolve_define_undecorated =
    add_all_caches_and_empty_cycle_detections (fun o -> o#resolve_define_undecorated)


  let resolve_mutable_literals =
    add_all_caches_and_empty_cycle_detections (fun o -> o#resolve_mutable_literals)


  let constraints_solution_exists =
    add_all_caches_and_empty_cycle_detections (fun o -> o#constraints_solution_exists)


  let full_order ?dependency read_only =
    let implementation = new with_all_caches dependency read_only in
    implementation#full_order ~cycle_detections:empty_cycle_detections


  let get_typed_dictionary =
    add_all_caches_and_empty_cycle_detections (fun o -> o#get_typed_dictionary)


  let signature_select =
    add_all_caches_and_empty_cycle_detections (fun o ->
        o#signature_select ~skip_marking_escapees:false)


  let global_annotation = add_all_caches_and_empty_cycle_detections (fun o -> o#global_annotation)

  let global read_only ?dependency reference =
    OutgoingDataComputation.global
      OutgoingDataComputation.Queries.
        { global_annotation = global_annotation ?dependency read_only }
      reference


  module Testing = struct
    let constraints_for_instantiate =
      add_all_caches_and_empty_cycle_detections (fun o -> o#constraints_for_instantiate)
  end
end

module AttributeReadOnly = ReadOnly
include TypeParameterValidationTypes

module AssumeDownstreamNeverNeedsUpdates = struct
  let upstream environment =
    GlobalAnnotationCache.AssumeDownstreamNeverNeedsUpdates.upstream environment
    |> AttributeCache.AssumeDownstreamNeverNeedsUpdates.upstream
    |> MetaclassCache.AssumeDownstreamNeverNeedsUpdates.upstream
end

module Testing = struct
  module ReadOnly = struct
    let upstream environment =
      GlobalAnnotationCache.Testing.ReadOnly.upstream environment
      |> AttributeCache.Testing.ReadOnly.upstream
      |> MetaclassCache.Testing.ReadOnly.upstream
  end

  module UpdateResult = struct
    let upstream update_result =
      GlobalAnnotationCache.Testing.UpdateResult.upstream update_result
      |> AttributeCache.Testing.UpdateResult.upstream
      |> MetaclassCache.Testing.UpdateResult.upstream
  end
end
