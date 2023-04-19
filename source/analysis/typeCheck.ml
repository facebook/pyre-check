(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Typecheck is the main file used for typechecking within Pyre. It contains methods for resolving
   the entire fixpoint including type refinement. In Pyre, we mainly resolve forward, meaning we
   recompute types in a resolution as we move forward in the control flow graph. In every reachable
   node, we will save type errors for displaying at the end. *)

open Core
open Pyre
open Ast
open Expression
open Statement
module StatementDefine = Define
module Error = AnalysisError

type class_name_and_is_abstract_and_is_protocol = {
  class_name: string;
  is_abstract: bool;
  is_protocol: bool;
}

type exit_state_of_define = {
  resolution: Resolution.t;
  errors: Error.t list;
  local_annotations: LocalAnnotationMap.t option;
  callees: Callgraph.callee_with_locations list option;
}

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let set error_map ~statement_key ~errors = Int.Table.set error_map ~key:statement_key ~data:errors

  let append error_map ~statement_key ~error =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val constraint_solving_style : Configuration.Analysis.constraint_solving_style

  val define : Define.t Node.t

  (* Where to store local annotations during the fixpoint. `None` discards them. *)
  val resolution_fixpoint : LocalAnnotationMap.t option

  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create : resolution:Resolution.t -> t

  val unreachable : t

  val resolution : t -> Resolution.t option

  val initial : resolution:Resolution.t -> t

  val parse_and_check_annotation
    :  ?bind_variables:bool ->
    resolution:Resolution.t ->
    Expression.t ->
    Error.t list * Type.t

  include Fixpoint.State with type t := t
end

let error_and_location_from_typed_dictionary_mismatch
    { Node.value = mismatch; location = define_location }
  =
  let mismatch =
    match mismatch with
    | WeakenMutableLiterals.FieldTypeMismatch { field_name; expected_type; actual_type; class_name }
      ->
        Error.FieldTypeMismatch { field_name; expected_type; actual_type; class_name }
    | MissingRequiredField { field_name; class_name } ->
        Error.MissingRequiredField { field_name; class_name }
    | UndefinedField { field_name; class_name } -> Error.UndefinedField { field_name; class_name }
  in
  define_location, Error.TypedDictionaryInitializationError mismatch


let incompatible_annotation_with_attribute_error ~define ~explicit ~original_annotation attribute =
  match original_annotation, AnnotatedAttribute.initialized attribute with
  | Some original, AnnotatedAttribute.OnClass when explicit && Define.is_constructor define ->
      let class_annotation = AnnotatedAttribute.annotation attribute |> Annotation.annotation in
      if not (Type.equal original class_annotation) then
        Some
          Error.(
            InconsistentConstructorAnnotation { attribute_annotation = original; class_annotation })
      else
        None
  | _ -> None


(* Return true if the mismatch between `actual` and `expected` is due to readonlyness.

   We check this by stripping any `ReadOnly` types in both and checking if they are compatible. *)
let is_readonlyness_mismatch ~global_resolution ~actual ~expected =
  (Type.ReadOnly.contains_readonly actual || Type.ReadOnly.contains_readonly expected)
  && GlobalResolution.less_or_equal
       global_resolution
       ~left:(Type.ReadOnly.strip_readonly actual)
       ~right:(Type.ReadOnly.strip_readonly expected)


let errors_from_not_found
    ?(callee_base_expression = None)
    ~callable
    ~self_argument
    ~reason
    ~global_resolution
    ?original_target
    ?callee_expression
    ~arguments
    ()
  =
  let callee = Type.Callable.name callable in
  match reason with
  | SignatureSelectionTypes.AbstractClassInstantiation { class_name; abstract_methods } ->
      [
        ( None,
          Error.InvalidClassInstantiation
            (Error.AbstractClassInstantiation { class_name; abstract_methods }) );
      ]
  | CallingParameterVariadicTypeVariable -> [None, Error.NotCallable (Type.Callable callable)]
  | InvalidKeywordArgument { Node.location; value = { expression; annotation } } ->
      [
        ( Some location,
          Error.InvalidArgument
            (Error.Keyword { expression; annotation; require_string_keys = true }) );
      ]
  | InvalidVariableArgument { Node.location; value = { expression; annotation } } ->
      [Some location, Error.InvalidArgument (Error.ConcreteVariable { expression; annotation })]
  | Mismatches mismatches ->
      let convert_to_error = function
        | SignatureSelectionTypes.Mismatch
            { Node.value = { SignatureSelectionTypes.actual; expected; name; position }; location }
          ->
            let typed_dictionary_error
                ~mismatch
                ~method_name
                ~position
                { Type.Record.TypedDictionary.fields; name = typed_dictionary_name }
              =
              if
                Type.TypedDictionary.is_special_mismatch
                  ~class_name:typed_dictionary_name
                  ~method_name
                  ~position
                  ~total:(Type.TypedDictionary.are_fields_total fields)
              then
                match actual with
                | Type.Literal (Type.String (Type.LiteralValue field_name)) ->
                    let required_field_exists =
                      List.exists
                        ~f:(fun { Type.Record.TypedDictionary.name; required; _ } ->
                          String.equal name field_name && required)
                        fields
                    in
                    if required_field_exists then
                      Error.TypedDictionaryInvalidOperation
                        { typed_dictionary_name; field_name; method_name; mismatch }
                      |> Option.some
                    else
                      Error.TypedDictionaryKeyNotFound
                        { typed_dictionary_name; missing_key = field_name }
                      |> Option.some
                | Type.Primitive "str" ->
                    Error.TypedDictionaryAccessWithNonLiteral
                      (List.map fields ~f:(fun { name; _ } -> name))
                    |> Option.some
                | _ -> None
              else
                match method_name, arguments with
                | ( "__setitem__",
                    Some
                      ({
                         AttributeResolution.Argument.expression =
                           Some
                             {
                               Node.value = Constant (Constant.String { value = field_name; _ });
                               _;
                             };
                         _;
                       }
                      :: _) ) ->
                    Error.TypedDictionaryInvalidOperation
                      { typed_dictionary_name; field_name; method_name; mismatch }
                    |> Option.some
                | _ -> None
            in
            let mismatch =
              Error.create_mismatch ~resolution:global_resolution ~actual ~expected ~covariant:true
            in
            let is_mutating_method_on_readonly self_argument_type =
              Int.equal
                position
                AttributeResolution.SignatureSelection.reserved_position_for_self_argument
              && Type.ReadOnly.is_readonly self_argument_type
            in
            let default_location_and_error =
              match callee, self_argument, callee_base_expression with
              | Some method_name, Some self_argument_type, Some self_argument
                when is_mutating_method_on_readonly self_argument_type ->
                  ( Node.location self_argument,
                    Error.ReadOnlynessMismatch
                      (CallingMutatingMethodOnReadOnly
                         { self_argument; self_argument_type; method_name }) )
              | _ ->
                  if Type.is_primitive_string actual && Type.is_literal_string expected then
                    location, Error.NonLiteralString { name; position; callee }
                  else if is_readonlyness_mismatch ~global_resolution ~actual ~expected then
                    ( location,
                      Error.ReadOnlynessMismatch
                        (IncompatibleParameterType
                           { keyword_argument_name = name; position; callee; mismatch }) )
                  else
                    ( location,
                      Error.IncompatibleParameterType
                        { keyword_argument_name = name; position; callee; mismatch } )
            in
            let location, kind =
              match self_argument, callee >>| Reference.last with
              | Some self_annotation, Some callee_name when is_operator callee_name -> (
                  let is_uninverted = Option.equal Type.equal self_argument original_target in
                  let operator_symbol =
                    if is_uninverted then
                      operator_name_to_symbol callee_name
                    else
                      callee_name |> inverse_operator >>= operator_name_to_symbol
                  in
                  match operator_symbol, callee_expression >>| Node.value with
                  | Some operator_name, Some (Expression.Name (Attribute { special = true; _ })) ->
                      let left_operand, right_operand =
                        if is_uninverted then
                          self_annotation, actual
                        else
                          actual, self_annotation
                      in
                      ( location,
                        Error.UnsupportedOperand
                          (Binary { operator_name; left_operand; right_operand }) )
                  | _ -> default_location_and_error)
              | Some (Type.Primitive _ as annotation), Some method_name ->
                  GlobalResolution.get_typed_dictionary ~resolution:global_resolution annotation
                  >>= typed_dictionary_error ~mismatch ~method_name ~position
                  >>| (fun kind -> location, kind)
                  |> Option.value ~default:default_location_and_error
              | _ -> default_location_and_error
            in
            [Some location, kind]
        | MismatchWithUnpackableType { variable; mismatch } ->
            [
              ( None,
                Error.InvalidArgument (VariableArgumentsWithUnpackableType { variable; mismatch }) );
            ]
      in
      List.concat_map mismatches ~f:convert_to_error
  | MissingArgument parameter -> [None, Error.MissingArgument { callee; parameter }]
  | MutuallyRecursiveTypeVariables -> [None, Error.MutuallyRecursiveTypeVariables callee]
  | ProtocolInstantiation class_name ->
      [None, Error.InvalidClassInstantiation (ProtocolInstantiation class_name)]
  | TooManyArguments { expected; provided } ->
      [None, Error.TooManyArguments { callee; expected; provided }]
  | TypedDictionaryInitializationError mismatches ->
      List.map mismatches ~f:(fun mismatch ->
          error_and_location_from_typed_dictionary_mismatch mismatch)
      |> List.map ~f:(fun (location, error) -> Some location, error)
  | UnexpectedKeyword name -> [None, Error.UnexpectedKeyword { callee; name }]


let incompatible_variable_type_error_kind
    ~global_resolution
    ~declare_location
    ({ Error.mismatch = { expected; actual; _ }; _ } as incompatible_type)
  =
  if is_readonlyness_mismatch ~global_resolution ~actual ~expected then
    Error.ReadOnlynessMismatch (IncompatibleVariableType { incompatible_type; declare_location })
  else
    Error.IncompatibleVariableType { incompatible_type; declare_location }


let rec unpack_callable_and_self_argument ~signature_select ~global_resolution input =
  let get_call_attribute parent =
    GlobalResolution.attribute_from_annotation global_resolution ~parent ~name:"__call__"
    >>| Annotated.Attribute.annotation
    >>| Annotation.annotation
  in
  match input with
  | Type.Callable callable -> Some { TypeOperation.callable; self_argument = None }
  | Type.TypeOperation (Compose (Concrete annotations)) ->
      List.map annotations ~f:(fun input ->
          get_call_attribute input
          (* TODO (T96555096): Fix potential infinite loop *)
          >>= unpack_callable_and_self_argument ~signature_select ~global_resolution)
      |> Option.all
      >>= TypeOperation.TypeOperation.Compose.compose_list ~signature_select
  | Any ->
      Some
        {
          callable =
            {
              kind = Anonymous;
              implementation = { annotation = Type.Any; parameters = Undefined };
              overloads = [];
            };
          self_argument = None;
        }
  | Parametric { name = "BoundMethod"; parameters = [Single callable; Single self_argument] } -> (
      let self_argument = Some self_argument in
      match callable with
      | Callable callable -> Some { TypeOperation.callable; self_argument }
      | complex -> (
          (* We do two layers since almost all callable classes have a BoundMethod __call__ which we
             need to unwrap. We can't go arbitrarily deep since it would be possible to loop, and
             its not worth building in a new assumption system just for this. We can't use a
             constraint/protocol solve if we want to extract overloads, leaving us with this *)
          get_call_attribute complex
          >>= get_call_attribute
          >>= function
          | Callable callable -> Some { TypeOperation.callable; self_argument }
          | _ -> None))
  | _ -> None


module State (Context : Context) = struct
  type partitioned = {
    consistent_with_boundary: Type.t;
    not_consistent_with_boundary: Type.t option;
  }

  (* None means the state in unreachable *)
  and t =
    | Unreachable
    | Value of Resolution.t

  let pp format = function
    | Unreachable -> Format.fprintf format "  <UNREACHABLE STATE>\n"
    | Value resolution ->
        let global_resolution = Resolution.global_resolution resolution in
        let expected =
          let parser = GlobalResolution.annotation_parser global_resolution in
          let { Node.value = { Define.signature; _ }; _ } = Context.define in
          Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
        in
        let errors =
          let error_to_string error =
            let error =
              let lookup =
                GlobalResolution.module_tracker global_resolution
                |> ModuleTracker.ReadOnly.lookup_relative_path
              in
              Error.instantiate ~show_error_traces:true ~lookup error
            in
            Format.asprintf
              "    %a -> %s"
              Location.WithPath.pp
              (Error.Instantiated.location error)
              (Error.Instantiated.description error)
          in
          Context.error_map
          >>| LocalErrorMap.all_errors
          >>| List.map ~f:error_to_string
          |> Option.value ~default:[]
          |> String.concat ~sep:"\n"
        in
        Format.fprintf
          format
          "  Expected return: %a\n  Resolution:\n%a\n  Errors:\n%s\n"
          Type.pp
          expected
          Resolution.pp
          resolution
          errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    match left, right with
    | Unreachable, Unreachable -> true
    | Value left_resolution, Value right_resolution ->
        Resolution.refinements_equal left_resolution right_resolution
    | _, _ -> false


  let create ~resolution = Value resolution

  let unreachable = Unreachable

  let bottom = Unreachable

  let emit_error ~errors ~location ~kind =
    Error.create
      ~location:(Location.with_module ~module_reference:Context.qualifier location)
      ~kind
      ~define:Context.define
    :: errors


  let emit_typed_dictionary_errors ~errors mismatches =
    let emit_error errors mismatch =
      let location, kind = error_and_location_from_typed_dictionary_mismatch mismatch in
      emit_error ~errors ~location ~kind
    in
    List.fold mismatches ~f:emit_error ~init:errors


  let add_invalid_type_parameters_errors ~resolution ~location ~errors annotation =
    let mismatches, annotation =
      GlobalResolution.check_invalid_type_parameters resolution annotation
    in
    let add_error errors mismatch =
      match annotation with
      (* Ignore errors from synthetic Self type when it is Generic without the proper bound *)
      | Type.Variable variable
        when Preprocessing.SelfType.is_synthetic_type_variable variable.variable ->
          errors
      | _ -> emit_error ~errors ~location ~kind:(Error.InvalidTypeParameters mismatch)
    in
    List.fold mismatches ~f:add_error ~init:errors, annotation


  let get_untracked_annotation_errors ~resolution ~location annotation =
    let add_untracked_errors errors =
      let is_untracked_name class_name =
        match class_name with
        | "..." -> false
        | _ -> not (GlobalResolution.is_tracked resolution class_name)
      in
      let untracked =
        List.filter (Type.elements annotation) ~f:is_untracked_name
        |> List.dedup_and_sort ~compare:String.compare
      in
      List.fold untracked ~init:errors ~f:(fun errors name ->
          emit_error ~errors ~location ~kind:(Error.UndefinedType (Primitive name)))
    in
    let add_literal_value_errors errors =
      (* Literal enum class names will later be parsed as types, so we must validate them when
         checking for untracked annotations. In error messaging, assume these are arbitrary
         non-literal expressions. *)
      let literals =
        let is_literal_enumeration = function
          | Type.Literal (Type.EnumerationMember _) -> true
          | _ -> false
        in
        Type.collect annotation ~predicate:is_literal_enumeration
      in
      let add_literal_error errors literal =
        match literal with
        | Type.Literal
            (Type.EnumerationMember
              { enumeration_type = Type.Primitive enumeration_name; member_name })
          when not (GlobalResolution.is_tracked resolution enumeration_name) ->
            emit_error
              ~errors
              ~location
              ~kind:
                (Error.InvalidType
                   (InvalidLiteral (Reference.create (enumeration_name ^ "." ^ member_name))))
        | _ -> errors
      in
      List.fold literals ~init:errors ~f:add_literal_error
    in
    add_untracked_errors [] |> add_literal_value_errors


  let parse_and_check_annotation
      ?(bind_variables = true)
      ~resolution
      ({ Node.location; _ } as expression)
    =
    let global_resolution = Resolution.global_resolution resolution in
    let check_and_correct_annotation ~resolution ~location ~annotation errors =
      let check_invalid_variables resolution variable =
        if not (Resolution.type_variable_exists resolution ~variable) then
          let origin =
            if Define.is_toplevel (Node.value Context.define) then
              Error.Toplevel
            else if Define.is_class_toplevel (Node.value Context.define) then
              Error.ClassToplevel
            else
              Error.Define
          in
          Error.InvalidTypeVariable { annotation = variable; origin } |> Option.some
        else
          None
      in
      let all_primitives_and_variables_are_valid, errors =
        let errors, no_untracked =
          let untracked_annotation_errors =
            get_untracked_annotation_errors ~resolution:global_resolution ~location annotation
          in
          List.append errors untracked_annotation_errors, List.is_empty untracked_annotation_errors
        in
        let invalid_variable_error_kinds =
          Type.Variable.all_free_variables annotation
          |> List.filter_map ~f:(check_invalid_variables resolution)
        in
        ( no_untracked && List.is_empty invalid_variable_error_kinds,
          List.fold invalid_variable_error_kinds ~init:errors ~f:(fun errors kind ->
              emit_error ~errors ~location ~kind) )
      in
      if all_primitives_and_variables_are_valid then
        add_invalid_type_parameters_errors
          annotation
          ~resolution:global_resolution
          ~location
          ~errors
      else
        errors, Type.Top
    in
    let annotation =
      GlobalResolution.parse_annotation ~validation:NoValidation global_resolution expression
    in
    let errors =
      match annotation with
      | Type.Callable { implementation = { annotation = Type.Top; _ }; _ } ->
          emit_error
            ~errors:[]
            ~location
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    {
                      annotation = Type.Primitive (Expression.show expression);
                      expected = "`Callable[[<parameters>], <return type>]`";
                    }))
      | _ when Type.contains_unknown annotation ->
          emit_error
            ~errors:[]
            ~location
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    { annotation = Type.Primitive (Expression.show expression); expected = "" }))
      | _ -> []
    in
    let errors, annotation =
      check_and_correct_annotation errors ~resolution ~location ~annotation
    in
    let annotation =
      if bind_variables then Type.Variable.mark_all_variables_as_bound annotation else annotation
    in
    errors, annotation


  let resolution = function
    | Unreachable -> None
    | Value resolution -> Some resolution


  let resolution_or_default ~default = function
    | Unreachable -> default
    | Value resolution -> resolution


  let less_or_equal ~left ~right =
    match left, right with
    | Unreachable, _ -> true
    | _, Unreachable -> false
    | Value left_resolution, Value right_resolution ->
        Refinement.Store.less_or_equal_monotone
          ~left:(Resolution.annotation_store left_resolution)
          ~right:(Resolution.annotation_store right_resolution)


  let widening_threshold = 3

  let add_fixpoint_threshold_reached_error () =
    let define = Context.define in
    let { Node.value = define_value; location = define_location } = define in
    match StatementDefine.is_toplevel define_value with
    | true ->
        (* Avoid emitting errors on top-level defines, which are generally unsuppressable. *)
        ()
    | false ->
        let kind =
          let define = StatementDefine.name define_value in
          AnalysisError.AnalysisFailure (FixpointThresholdReached { define })
        in
        let location = Location.with_module ~module_reference:Context.qualifier define_location in
        let error = AnalysisError.create ~location ~kind ~define in
        let statement_key = [%hash: int * int] (Cfg.entry_index, 0) in
        let (_ : unit option) = Context.error_map >>| LocalErrorMap.append ~statement_key ~error in
        ()


  let widen ~previous ~next ~iteration =
    match previous, next with
    | Unreachable, _ -> next
    | _, Unreachable -> previous
    | Value previous_resolution, Value next_resolution ->
        if iteration + 1 >= widening_threshold then
          add_fixpoint_threshold_reached_error ();
        Value
          (Resolution.outer_widen_refinements
             ~iteration
             ~widening_threshold
             previous_resolution
             next_resolution)


  let join left right = widen ~previous:left ~next:right ~iteration:0

  module Resolved = struct
    type base =
      | Class of Type.t
      | Instance of Type.t
      | Super of Type.t
    [@@deriving show]

    type t = {
      resolution: Resolution.t;
      errors: Error.t list;
      resolved: Type.t;
      resolved_annotation: Annotation.t option;
      base: base option;
    }
    [@@deriving show]

    let _ = show

    let resolved_base_type = function
      | Some (Class base_type)
      | Some (Instance base_type)
      | Some (Super base_type) ->
          Some base_type
      | None -> None
  end

  module Callee = struct
    type base = {
      expression: Expression.t;
      resolved_base: Type.t;
    }

    type attribute = {
      name: Identifier.t;
      resolved: Type.t;
    }

    type callee_attribute = {
      base: base;
      attribute: attribute;
      expression: Expression.t;
    }

    type t =
      | Attribute of callee_attribute
      | NonAttribute of {
          expression: Expression.t;
          resolved: Type.t;
        }

    let resolved = function
      | Attribute { attribute = { resolved; _ }; _ }
      | NonAttribute { resolved; _ } ->
          resolved


    let expression = function
      | Attribute { expression; _ }
      | NonAttribute { expression; _ } ->
          expression


    let base_expression = function
      | Attribute { base = { expression; _ }; _ } -> Some expression
      | NonAttribute _ -> None
  end

  module CallableApplicationData = struct
    type ('return_annotation, 'arguments) callable_data = {
      callable: TypeOperation.callable_and_self_argument;
      arguments: 'arguments;
      is_inverted_operator: bool;
      selected_return_annotation: 'return_annotation;
    }

    type ('return_annotation, 'arguments) t =
      | KnownCallable of ('return_annotation, 'arguments) callable_data
      | UnknownCallableAttribute of {
          callable_attribute: Callee.callee_attribute;
          arguments: 'arguments;
        }

    let unknown_callable_attribute_before_application callable_attribute =
      UnknownCallableAttribute { callable_attribute; arguments = () }


    let known_callable_before_application callable =
      KnownCallable
        { callable; is_inverted_operator = false; arguments = (); selected_return_annotation = () }
  end

  let type_of_signature ~resolution signature =
    let global_resolution = Resolution.global_resolution resolution in
    match
      GlobalResolution.resolve_define
        ~resolution:global_resolution
        ~implementation:(Some signature)
        ~overloads:[]
    with
    | { decorated = Ok (Type.Callable callable); _ } ->
        Type.Callable { callable with kind = Anonymous }
    | { decorated = Ok other; _ } -> other
    | { decorated = Error _; _ } -> Any


  let type_of_parent ~global_resolution parent =
    let parent_name = Reference.show parent in
    let parent_type = Type.Primitive parent_name in
    let variables = GlobalResolution.variables global_resolution parent_name in
    match variables with
    | None
    | Some [] ->
        parent_type
    | Some variables ->
        List.map variables ~f:Type.Variable.to_parameter |> Type.parametric parent_name
    | exception _ -> parent_type


  let instantiate_path ~global_resolution location =
    let lookup =
      GlobalResolution.module_tracker global_resolution
      |> ModuleTracker.ReadOnly.lookup_relative_path
    in
    let location = Location.with_module ~module_reference:Context.qualifier location in
    Location.WithModule.instantiate ~lookup location


  let define_signature =
    let { Node.value = { Define.signature; _ }; _ } = Context.define in
    signature


  let return_annotation ~global_resolution =
    let signature = define_signature in
    let annotation : Type.t =
      let parser = GlobalResolution.annotation_parser global_resolution in
      Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
    in
    let annotation =
      match annotation with
      | Type.Parametric { name = "typing.TypeGuard" | "typing_extensions.TypeGuard"; _ } ->
          Type.bool
      | _ when signature.async && not signature.generator ->
          Type.coroutine_value annotation |> Option.value ~default:Type.Top
      | _ -> annotation
    in
    Type.Variable.mark_all_variables_as_bound annotation


  let is_toplevel_module_reference ~global_resolution name =
    name
    |> GlobalResolution.resolve_exports global_resolution
    >>= UnannotatedGlobalEnvironment.ResolvedReference.as_module_toplevel_reference
    |> Option.is_some


  let resolve_reference_annotation ~resolution reference =
    let local_annotation = Resolution.get_local resolution ~reference in
    match local_annotation, Reference.prefix reference with
    | Some annotation, _ -> Some annotation
    | None, Some qualifier -> (
        (* Fallback to use a __getattr__ callable as defined by PEP 484. *)
        let getattr =
          Resolution.get_local
            resolution
            ~reference:(Reference.create ~prefix:qualifier "__getattr__")
          >>| Annotation.annotation
        in
        let correct_getattr_arity signature =
          Type.Callable.Overload.parameters signature
          >>| (fun parameters -> List.length parameters == 1)
          |> Option.value ~default:false
        in
        let create_annotation signature =
          Annotation.create_immutable
            ~original:(Some Type.Top)
            (Type.Callable.Overload.return_annotation signature)
        in
        match getattr with
        | Some (Callable { overloads = [signature]; _ }) when correct_getattr_arity signature ->
            Some (create_annotation signature)
        | Some (Callable { implementation = signature; _ }) when correct_getattr_arity signature ->
            Some (create_annotation signature)
        | _ -> None)
    | _ -> None


  let forward_reference ~resolution ~location ~errors reference =
    let global_resolution = Resolution.global_resolution resolution in
    let reference = GlobalResolution.legacy_resolve_exports global_resolution reference in
    let annotation = resolve_reference_annotation ~resolution reference in
    match annotation with
    | Some annotation ->
        {
          Resolved.resolution;
          errors;
          resolved = Annotation.annotation annotation;
          resolved_annotation = Some annotation;
          base = None;
        }
    | None ->
        let errors =
          if
            (not (GlobalResolution.module_exists global_resolution reference))
            && not (GlobalResolution.is_suppressed_module global_resolution reference)
          then
            match Reference.prefix reference with
            | Some qualifier when not (Reference.is_empty qualifier) ->
                if GlobalResolution.module_exists global_resolution qualifier then
                  let origin =
                    let module_tracker = GlobalResolution.module_tracker global_resolution in
                    match ModuleTracker.ReadOnly.lookup_module_path module_tracker qualifier with
                    | Some module_path -> Error.ExplicitModule module_path
                    | None -> Error.ImplicitModule qualifier
                  in
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.UndefinedAttribute
                         { attribute = Reference.last reference; origin = Error.Module origin })
                else
                  errors
            | _ -> errors
          else
            errors
        in
        { resolution; errors; resolved = Type.Top; resolved_annotation = None; base = None }


  type partition_name_result_t = {
    name: Reference.t;
    attribute_path: Reference.t;
    base_annotation: Annotation.t option;
  }

  let partition_name ~resolution name =
    let global_resolution = Resolution.global_resolution resolution in
    let reference = Ast.Expression.name_to_reference_exn name in
    match Reference.as_list reference with
    | [] -> { name = Reference.empty; attribute_path = Reference.empty; base_annotation = None }
    | head :: tail ->
        let base, attribute_list =
          match GlobalResolution.resolve_exports global_resolution reference with
          | Some (UnannotatedGlobalEnvironment.ResolvedReference.Module _) -> reference, []
          | Some
              (UnannotatedGlobalEnvironment.ResolvedReference.PlaceholderStub
                { stub_module; remaining }) ->
              stub_module, remaining
          | Some
              (UnannotatedGlobalEnvironment.ResolvedReference.ModuleAttribute
                { from; name; remaining; _ }) ->
              Reference.create ~prefix:from name, remaining
          | None -> Reference.create head, tail
        in
        let rec partition_attribute base attribute_list =
          let base_type =
            base
            |> GlobalResolution.legacy_resolve_exports global_resolution
            |> resolve_reference_annotation ~resolution
            >>| Annotation.annotation
            |> Option.value ~default:Type.Top
          in
          if Type.is_untyped base_type then
            match attribute_list with
            | [] -> Reference.create head, tail, None
            | attribute :: attribute_list ->
                partition_attribute Reference.(attribute |> create |> combine base) attribute_list
          else
            base, attribute_list, Some (Annotation.create_mutable base_type)
        in
        partition_attribute base attribute_list
        |> fun (base, attributes, annotation) ->
        {
          name = base;
          attribute_path = Reference.create_from_list attributes;
          base_annotation = annotation;
        }


  let rec validate_return expression ~resolution ~errors ~location ~actual ~is_implicit =
    let global_resolution = Resolution.global_resolution resolution in
    let { Node.location = define_location; value = define } = Context.define in
    let { Define.Signature.async; generator; return_annotation = return_annotation_expression; _ } =
      define.signature
    in
    let return_annotation = return_annotation ~global_resolution in
    (* We weaken type inference of mutable literals for assignments and returns to get around the
       invariance of containers when we can prove that casting to a supertype is safe. *)
    let actual =
      GlobalResolution.resolve_mutable_literals
        global_resolution
        ~resolve:(resolve_expression_type ~resolution)
        ~expression
        ~resolved:actual
        ~expected:return_annotation
    in
    let check_incompatible_return actual errors =
      if
        Define.has_return_annotation define
        && (not
              (GlobalResolution.constraints_solution_exists
                 global_resolution
                 ~left:actual
                 ~right:return_annotation))
        && (not (Define.is_abstract_method define))
        && (not (Define.is_overloaded_function define))
        && (not (Type.is_none actual && async && generator))
        && not (Type.is_noreturn_or_never actual && Type.is_noreturn_or_never return_annotation)
      then
        let rec check_unimplemented = function
          | [
              { Node.value = Statement.Pass; _ };
              { Node.value = Statement.Return { Return.expression = None; _ }; _ };
            ] ->
              true
          | {
              Node.value =
                Statement.Expression { Node.value = Expression.Constant (Constant.String _); _ };
              _;
            }
            :: tail ->
              check_unimplemented tail
          | _ -> false
        in
        let kind =
          let mismatch =
            Error.create_mismatch
              ~resolution:global_resolution
              ~actual
              ~expected:return_annotation
              ~covariant:true
          in
          if is_readonlyness_mismatch ~global_resolution ~actual ~expected:return_annotation then
            Error.ReadOnlynessMismatch (IncompatibleReturnType { mismatch; define_location })
          else
            Error.IncompatibleReturnType
              {
                mismatch;
                is_implicit;
                is_unimplemented = check_unimplemented define.body;
                define_location;
              }
        in
        emit_error ~errors ~location ~kind
      else
        errors
    in
    let check_missing_return actual errors =
      let contains_literal_any =
        return_annotation_expression >>| Type.expression_contains_any |> Option.value ~default:false
      in
      if
        (not (Define.has_return_annotation define))
        || (contains_literal_any && Type.contains_prohibited_any return_annotation)
      then
        let given_annotation =
          Option.some_if (Define.has_return_annotation define) return_annotation
        in
        emit_error
          ~errors
          ~location:define_location
          ~kind:
            (Error.MissingReturnAnnotation
               {
                 name = Reference.create "$return_annotation";
                 annotation = Some actual;
                 given_annotation;
                 evidence_locations = [instantiate_path ~global_resolution location];
                 thrown_at_source = true;
               })
      else
        errors
    in
    match actual with
    | { resolved = actual; typed_dictionary_errors = [] } ->
        check_incompatible_return actual errors |> check_missing_return actual
    | { typed_dictionary_errors; _ } -> emit_typed_dictionary_errors ~errors typed_dictionary_errors


  (** Resolves types by moving forward through nodes in the CFG starting at an expression. *)
  and forward_expression ~resolution { Node.location; value } =
    let global_resolution = Resolution.global_resolution resolution in
    let forward_entry ~resolution ~errors ~entry:{ Dictionary.Entry.key; value } =
      let { Resolved.resolution; resolved = key_resolved; errors = key_errors; _ } =
        forward_expression ~resolution key
      in
      let { Resolved.resolution; resolved = value_resolved; errors = value_errors; _ } =
        forward_expression ~resolution value
      in
      ( Type.weaken_literals key_resolved,
        Type.weaken_literals value_resolved,
        resolution,
        List.concat [key_errors; value_errors; errors] )
    in
    let forward_generator
        ~resolution
        ~errors
        ~generator:({ Comprehension.Generator.conditions; _ } as generator)
      =
      (* Propagate the target type information. *)
      let resolution, errors =
        let iterator_resolution, iterator_errors =
          let post_resolution, errors =
            let { Assign.target; annotation; value } = Statement.generator_assignment generator in
            forward_assignment ~resolution ~location ~target ~annotation ~value
          in
          resolution_or_default post_resolution ~default:resolution, errors
        in
        let iterator_errors =
          (* Don't throw Incompatible Variable errors on the generated iterator assign; we are
             temporarily minting a variable in a new scope and old annotations should be ignored. *)
          let is_not_assignment_error = function
            | { Error.kind = Error.IncompatibleVariableType _; _ } -> false
            | _ -> true
          in
          List.filter ~f:is_not_assignment_error iterator_errors
        in
        (* We want the resolution in the iterator assignment -- they will become useful in
           `forward_comprehension`. Don't worry about annotation store pollutions as we will throw
           away generator-local variables there. *)
        iterator_resolution, List.append iterator_errors errors
      in
      List.fold conditions ~init:(resolution, errors) ~f:(fun (resolution, errors) condition ->
          let resolution, new_errors =
            let post_resolution, errors = forward_assert ~resolution condition in
            resolution_or_default post_resolution ~default:resolution, errors
          in
          resolution, List.append new_errors errors)
    in
    let forward_comprehension ~resolution ~errors ~element ~generators =
      let resolved, errors =
        List.fold
          generators
          ~f:(fun (resolution, errors) generator ->
            forward_generator ~resolution ~errors ~generator)
          ~init:(resolution, errors)
        |> fun (resolution, errors) ->
        let { Resolved.resolved; errors = element_errors; _ } =
          forward_expression ~resolution element
        in
        resolved, List.append element_errors errors
      in
      (* Discard generator-local variables. *)
      {
        Resolved.resolution;
        resolved = Type.weaken_literals resolved;
        resolved_annotation = None;
        base = None;
        errors;
      }
    in
    let forward_elements ~resolution ~errors ~elements =
      let forward_element { Resolved.resolution; resolved; errors; _ } expression =
        match Node.value expression with
        | Expression.Starred (Starred.Once expression) ->
            let { Resolved.resolution; resolved = new_resolved; errors = new_errors; _ } =
              forward_expression ~resolution expression
            in
            let parameter =
              new_resolved
              |> GlobalResolution.type_of_iteration_value ~global_resolution
              |> Option.value ~default:Type.Any
            in
            {
              Resolved.resolution;
              resolved = GlobalResolution.join global_resolution resolved parameter;
              errors = List.append new_errors errors;
              resolved_annotation = None;
              base = None;
            }
        | _ ->
            let { Resolved.resolution; resolved = new_resolved; errors = new_errors; _ } =
              forward_expression ~resolution expression
            in
            {
              resolution;
              resolved = GlobalResolution.join global_resolution resolved new_resolved;
              errors = List.append new_errors errors;
              resolved_annotation = None;
              base = None;
            }
      in
      let correct_bottom { Resolved.resolution; resolved; errors; _ } =
        let resolved =
          if Type.is_unbound resolved then
            Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            resolved
        in
        { Resolved.resolution; errors; resolved; resolved_annotation = None; base = None }
      in
      List.fold
        elements
        ~init:
          {
            Resolved.resolution;
            errors;
            resolved = Type.Bottom;
            resolved_annotation = None;
            base = None;
          }
        ~f:forward_element
      |> (fun { Resolved.resolution; errors; resolved; _ } ->
           {
             Resolved.resolution;
             errors;
             resolved = Type.weaken_literals resolved;
             resolved_annotation = None;
             base = None;
           })
      |> correct_bottom
    in
    let resolve_attribute_access
        ~base_resolved:
          { Resolved.resolution; errors; resolved = resolved_base; base = super_base; _ }
        ~base
        ~special
        ~attribute
        ~has_default
      =
      let name = Name.Attribute { base; special; attribute } in
      let reference = name_to_reference name in
      let access_as_attribute () =
        let find_attribute
            ({ Type.instantiated; accessed_through_class; class_name; accessed_through_readonly } as
            class_data)
          =
          let name = attribute in
          match
            GlobalResolution.attribute_from_class_name
              class_name
              ~transitive:true
              ~accessed_through_class
              ~accessed_through_readonly
              ~special_method:special
              ~resolution:global_resolution
              ~name
              ~instantiated
          with
          | Some attribute ->
              let attribute =
                if not (Annotated.Attribute.defined attribute) then
                  Resolution.fallback_attribute
                    class_name
                    ~instantiated:(Some resolved_base)
                    ~accessed_through_class
                    ~resolution
                    ~name
                  |> Option.value ~default:attribute
                else
                  attribute
              in
              let undefined_target =
                if Annotated.Attribute.defined attribute then
                  None
                else
                  Some instantiated
              in
              (* Collect @property's in the call graph. *)
              Some (class_data, attribute, undefined_target)
          | None -> None
        in
        let module_path_of_parent_module class_type =
          let module_tracker = GlobalResolution.module_tracker global_resolution in
          GlobalResolution.class_summary global_resolution class_type
          >>| Node.value
          >>= fun { ClassSummary.qualifier; _ } ->
          ModuleTracker.ReadOnly.lookup_module_path module_tracker qualifier
        in
        match
          Type.class_data_for_attribute_lookup resolved_base
          >>| List.map ~f:find_attribute
          >>= Option.all
        with
        | None ->
            let errors =
              if has_default then
                errors
              else
                emit_error
                  ~errors
                  ~location
                  ~kind:
                    (Error.UndefinedAttribute
                       {
                         attribute;
                         origin =
                           Error.Class
                             {
                               class_origin = ClassType resolved_base;
                               parent_module_path = module_path_of_parent_module resolved_base;
                             };
                       })
            in
            {
              Resolved.resolution;
              errors;
              resolved = Type.Top;
              resolved_annotation = None;
              base = None;
            }
        | Some [] ->
            {
              Resolved.resolution;
              errors;
              resolved = Type.Top;
              resolved_annotation = None;
              base = None;
            }
        | Some (head_attribute_info :: tail_attributes_info) ->
            let add_attributes_to_context attribute_info =
              let get_instantiated { Type.instantiated; _ } = instantiated in
              let attributes_with_instantiated =
                List.map attribute_info ~f:(fun (class_data, attribute, _) ->
                    attribute, get_instantiated class_data)
              in
              Context.Builder.add_property_callees
                ~global_resolution
                ~resolved_base
                ~attributes:attributes_with_instantiated
                ~location
                ~qualifier:Context.qualifier
                ~name:attribute
            in
            add_attributes_to_context (head_attribute_info :: tail_attributes_info);

            let _, head_attribute, _ = head_attribute_info in
            let _, tail_attributes, _ = List.unzip3 tail_attributes_info in

            let errors =
              let attribute_name, target =
                match
                  List.find
                    (head_attribute_info :: tail_attributes_info)
                    ~f:(fun (_, _, undefined_target) -> Option.is_some undefined_target)
                with
                | Some (_, attribute, Some target) ->
                    AnnotatedAttribute.public_name attribute, Some target
                | Some (_, attribute, _) -> AnnotatedAttribute.public_name attribute, None
                | _ -> attribute, None
              in
              match target with
              | Some target ->
                  if has_default then
                    errors
                  else if Option.is_some (inverse_operator attribute) then
                    (* Defer any missing attribute error until the inverse operator has been
                       typechecked. *)
                    errors
                  else
                    let class_origin =
                      match resolved_base with
                      | Type.Union [Type.NoneType; _]
                      | Union [_; Type.NoneType] ->
                          Error.ClassType target
                      | Union unions ->
                          List.findi ~f:(fun _ element -> Type.equal element target) unions
                          >>| (fun (index, _) -> Error.ClassInUnion { unions; index })
                          |> Option.value ~default:(Error.ClassType target)
                      | _ -> Error.ClassType target
                    in
                    emit_error
                      ~errors
                      ~location
                      ~kind:
                        (Error.UndefinedAttribute
                           {
                             attribute = attribute_name;
                             origin =
                               Error.Class
                                 {
                                   class_origin;
                                   parent_module_path = module_path_of_parent_module target;
                                 };
                           })
              | _ -> errors
            in

            let head_annotation = Annotated.Attribute.annotation head_attribute in
            let tail_annotations = List.map ~f:Annotated.Attribute.annotation tail_attributes in

            let resolved_annotation =
              let apply_local_override global_annotation =
                let local_override =
                  reference
                  >>= fun reference ->
                  let { name; attribute_path; _ } =
                    partition_name
                      ~resolution
                      (create_name_from_reference ~location:Location.any reference)
                  in
                  Resolution.get_local_with_attributes
                    resolution
                    ~name
                    ~attribute_path
                    ~global_fallback:(Type.is_meta (Annotation.annotation global_annotation))
                in
                match local_override with
                | Some local_annotation -> local_annotation
                | None -> global_annotation
              in
              tail_annotations
              |> Algorithms.fold_balanced
                   ~f:(Refinement.Unit.join_annotations ~global_resolution)
                   ~init:head_annotation
              |> apply_local_override
            in
            {
              resolution;
              errors;
              resolved = Annotation.annotation resolved_annotation;
              resolved_annotation = Some resolved_annotation;
              base = None;
            }
      in
      let resolved =
        match resolved_base with
        (* Global or local. *)
        | Type.Top ->
            reference
            >>| forward_reference ~resolution ~location ~errors
            |> Option.value
                 ~default:
                   {
                     Resolved.resolution;
                     errors;
                     resolved = Type.Top;
                     resolved_annotation = None;
                     base = None;
                   }
        (* TODO(T63892020): We need to fix up qualification so nested classes and functions are just
           normal locals rather than attributes of the enclosing function, which they really are
           not *)
        | Type.Parametric { name = "BoundMethod"; _ }
        | Type.Callable _ -> (
            let resolved =
              reference >>= fun reference -> Resolution.get_local resolution ~reference
            in
            match resolved with
            | Some annotation ->
                {
                  resolution;
                  errors;
                  resolved = Annotation.annotation annotation;
                  resolved_annotation = Some annotation;
                  base = None;
                }
            | None -> access_as_attribute ())
        | _ ->
            (* Attribute access. *)
            access_as_attribute ()
      in
      let base =
        match super_base with
        | Some (Resolved.Super _) -> super_base
        | _ ->
            let is_global_meta =
              let is_global () =
                match base with
                | { Node.value = Name name; _ } ->
                    name_to_identifiers name
                    >>| Reference.create_from_list
                    >>= GlobalResolution.global global_resolution
                    |> Option.is_some
                | _ -> false
              in
              Type.is_meta resolved_base && is_global ()
            in
            if is_global_meta then
              Some (Resolved.Class resolved_base)
            else
              Some (Resolved.Instance resolved_base)
      in
      { resolved with base }
    in
    let forward_call ~resolution ~errors ~target ~dynamic ~callee ~arguments =
      let open CallableApplicationData in
      let unpack_callable_and_self_argument =
        unpack_callable_and_self_argument
          ~signature_select:
            (GlobalResolution.signature_select
               ~global_resolution
               ~resolve_with_locals:(resolve_expression_type_with_locals ~resolution))
          ~global_resolution
      in
      let find_method ~parent ~name ~special_method =
        GlobalResolution.attribute_from_annotation global_resolution ~parent ~name ~special_method
        >>| Annotated.Attribute.annotation
        >>| Annotation.annotation
        >>= unpack_callable_and_self_argument
      in
      let callable_from_type resolved =
        match unpack_callable_and_self_argument resolved with
        | Some unpacked -> Some unpacked
        | _ -> find_method ~parent:resolved ~name:"__call__" ~special_method:true
      in
      let inverse_operator_callable
          ~callee_attribute:
            ({ Callee.base = { expression; resolved_base }; attribute = { name; _ }; _ } as
            callee_attribute)
        = function
        | [{ AttributeResolution.Argument.resolved; _ }] as arguments ->
            let found_inverse_operator =
              inverse_operator name
              >>= (fun name -> find_method ~parent:resolved ~name ~special_method:false)
              >>= fun found_callable ->
              if Type.is_any resolved_base || Type.is_unbound resolved_base then
                None
              else
                let inverted_arguments =
                  [
                    {
                      AttributeResolution.Argument.expression = Some expression;
                      resolved = resolved_base;
                      kind = Positional;
                    };
                  ]
                in
                Some
                  (KnownCallable
                     {
                       callable = found_callable;
                       arguments = inverted_arguments;
                       is_inverted_operator = true;
                       selected_return_annotation = ();
                     })
            in
            Option.first_some
              found_inverse_operator
              (Some (UnknownCallableAttribute { callable_attribute = callee_attribute; arguments }))
        | _ -> None
      in
      let rec get_callables callee =
        match callee with
        | Callee.Attribute ({ attribute = { resolved; _ }; _ } as callee_attribute)
          when Type.is_top resolved ->
            Some [unknown_callable_attribute_before_application callee_attribute]
        | Callee.Attribute { attribute = { resolved; _ }; _ }
        | Callee.NonAttribute { resolved; _ } -> (
            match resolved with
            | Type.Union annotations ->
                List.map annotations ~f:(fun annotation ->
                    callable_from_type annotation
                    >>| fun callable -> known_callable_before_application callable)
                |> Option.all
            | Type.Variable ({ constraints = Type.Variable.Explicit _; _ } as explicit) ->
                let upper_bound = Type.Variable.Unary.upper_bound explicit in
                let callee =
                  match callee with
                  | Callee.Attribute { attribute; base; expression } ->
                      Callee.Attribute
                        { base; attribute = { attribute with resolved = upper_bound }; expression }
                  | Callee.NonAttribute callee ->
                      Callee.NonAttribute { callee with resolved = upper_bound }
                in
                get_callables callee
            | Type.Variable { constraints = Type.Variable.Bound parent; _ } ->
                let callee =
                  match callee with
                  | Callee.Attribute { attribute; base; expression } ->
                      Callee.Attribute
                        { base; attribute = { attribute with resolved = parent }; expression }
                  | Callee.NonAttribute callee ->
                      Callee.NonAttribute { callee with resolved = parent }
                in
                get_callables callee
            | annotation ->
                callable_from_type annotation
                >>| fun callable -> [known_callable_before_application callable])
      in
      let return_annotation_with_callable_and_self
          ~resolution
          ({
             callable = { TypeOperation.callable; _ };
             arguments;
             is_inverted_operator;
             selected_return_annotation;
           } as callable_data)
        =
        match selected_return_annotation, callable with
        | SignatureSelectionTypes.NotFound _, _ -> (
            match callee, callable, arguments with
            | ( Callee.Attribute { base = { expression; resolved_base }; _ },
                { Type.Callable.kind = Type.Callable.Named name; _ },
                [{ AttributeResolution.Argument.resolved; _ }] )
              when not is_inverted_operator ->
                inverse_operator (Reference.last name)
                >>= (fun name -> find_method ~parent:resolved ~name ~special_method:false)
                >>| (fun ({ TypeOperation.callable; self_argument } as
                         unpacked_callable_and_self_argument) ->
                      let arguments =
                        [
                          {
                            AttributeResolution.Argument.expression = Some expression;
                            kind = Positional;
                            resolved = resolved_base;
                          };
                        ]
                      in
                      {
                        callable_data with
                        selected_return_annotation =
                          GlobalResolution.signature_select
                            ~arguments
                            ~global_resolution:(Resolution.global_resolution resolution)
                            ~resolve_with_locals:(resolve_expression_type_with_locals ~resolution)
                            ~callable
                            ~self_argument;
                        (* Make sure we emit errors against the inverse function, not the
                           original *)
                        callable = unpacked_callable_and_self_argument;
                      })
                |> Option.value ~default:{ callable_data with selected_return_annotation }
            | _ -> { callable_data with selected_return_annotation })
        | ( (Found { selected_return_annotation; _ } as found_return_annotation),
            { kind = Named access; _ } )
          when String.equal "__init__" (Reference.last access) ->
            Type.split selected_return_annotation
            |> fst
            |> Type.primitive_name
            >>| (function
                  | class_name ->
                      let abstract_methods =
                        GlobalResolution.attributes
                          ~transitive:true
                          class_name
                          ~resolution:global_resolution
                        >>| List.filter ~f:AnnotatedAttribute.abstract
                        |> Option.value ~default:[]
                        |> List.map ~f:Annotated.Attribute.name
                      in
                      if not (List.is_empty abstract_methods) then
                        SignatureSelectionTypes.NotFound
                          {
                            closest_return_annotation = selected_return_annotation;
                            reason =
                              Some
                                (AbstractClassInstantiation
                                   { class_name = Reference.create class_name; abstract_methods });
                          }
                      else if GlobalResolution.is_protocol global_resolution (Primitive class_name)
                      then
                        NotFound
                          {
                            closest_return_annotation = selected_return_annotation;
                            reason = Some (ProtocolInstantiation (Reference.create class_name));
                          }
                      else
                        found_return_annotation)
            |> Option.value ~default:found_return_annotation
            |> fun selected_return_annotation -> { callable_data with selected_return_annotation }
        | _ -> { callable_data with selected_return_annotation }
      in
      let extract_found_not_found_unknown_attribute = function
        | KnownCallable
            {
              selected_return_annotation =
                SignatureSelectionTypes.Found { selected_return_annotation };
              _;
            } ->
            `Fst selected_return_annotation
        | KnownCallable _ as not_found -> `Snd not_found
        | UnknownCallableAttribute _ as unknown_callable_attribute ->
            `Trd unknown_callable_attribute
      in
      let resolved_for_bad_callable ~resolution ~errors undefined_attributes =
        (* When an operator does not exist on the left operand but its inverse exists on the right
           operand, the missing attribute error would not have been thrown for the original
           operator. Build up the original error in case the inverse operator does not typecheck. *)
        let potential_missing_operator_error undefined_attributes =
          match target, callee with
          | Some target, Callee.Attribute { attribute = { name; resolved }; _ }
            when Type.is_top resolved
                 && Option.is_some (inverse_operator name)
                 && (not (Type.is_any target))
                 && not (Type.is_unbound target) -> (
              match undefined_attributes, operator_name_to_symbol name with
              | ( [
                    UnknownCallableAttribute
                      { arguments = [{ AttributeResolution.Argument.resolved; _ }]; _ };
                  ],
                  Some operator_name ) ->
                  Some
                    (Error.UnsupportedOperand
                       (Binary { operator_name; left_operand = target; right_operand = resolved }))
              | _ ->
                  let class_module =
                    let module_tracker = GlobalResolution.module_tracker global_resolution in
                    GlobalResolution.class_summary global_resolution target
                    >>| Node.value
                    >>= fun { ClassSummary.qualifier; _ } ->
                    ModuleTracker.ReadOnly.lookup_module_path module_tracker qualifier
                  in
                  Some
                    (Error.UndefinedAttribute
                       {
                         attribute = name;
                         origin =
                           Error.Class
                             { class_origin = ClassType target; parent_module_path = class_module };
                       }))
          | _ -> None
        in
        let errors =
          let resolved_callee = Callee.resolved callee in
          match resolved_callee, potential_missing_operator_error undefined_attributes with
          | Type.Top, Some kind -> emit_error ~errors ~location ~kind
          | Parametric { name = "type"; parameters = [Single Any] }, _
          | Parametric { name = "BoundMethod"; parameters = [Single Any; _] }, _
          | Type.Any, _
          | Type.Top, _ ->
              errors
          | _ -> emit_error ~errors ~location ~kind:(Error.NotCallable resolved_callee)
        in
        {
          Resolved.resolution;
          errors;
          resolved = Type.Any;
          resolved_annotation = None;
          base = None;
        }
      in
      let join_return_annotations
          ~resolution
          ~errors
          (found_return_annotations, not_found_return_annotations)
        =
        match found_return_annotations, not_found_return_annotations with
        | head :: tail, [] ->
            Some
              {
                Resolved.resolution;
                errors;
                resolved = List.fold ~f:(GlobalResolution.join global_resolution) ~init:head tail;
                resolved_annotation = None;
                base = None;
              }
        | ( _,
            KnownCallable
              {
                selected_return_annotation =
                  SignatureSelectionTypes.NotFound
                    { closest_return_annotation; reason = Some reason };
                callable = unpacked_callable_and_self_argument;
                arguments;
                _;
              }
            :: _ ) ->
            (* For a union of callables, we prioritize mismatched signatures even if some of the
               callables matched correctly. *)
            let errors =
              let error_kinds =
                let { TypeOperation.callable; self_argument } =
                  unpacked_callable_and_self_argument
                in
                errors_from_not_found
                  ~reason
                  ~callable
                  ~self_argument
                  ~global_resolution
                  ?original_target:target
                  ~callee_expression:(Callee.expression callee)
                  ~callee_base_expression:(Callee.base_expression callee)
                  ~arguments:(Some arguments)
                  ()
              in
              let emit errors (more_specific_error_location, kind) =
                let location = Option.value more_specific_error_location ~default:location in
                emit_error ~errors ~location ~kind
              in
              List.fold error_kinds ~init:errors ~f:emit
            in
            Some
              {
                resolution;
                errors;
                resolved = closest_return_annotation;
                resolved_annotation = None;
                base = None;
              }
        | _ -> None
      in
      let check_for_error ({ Resolved.resolved; errors; _ } as input) =
        let is_broadcast_error = function
          | Type.Parametric
              {
                name = "pyre_extensions.BroadcastError";
                parameters = [Type.Parameter.Single _; Type.Parameter.Single _];
              } ->
              true
          | _ -> false
        in
        match Type.collect resolved ~predicate:is_broadcast_error with
        | [] -> input
        | broadcast_errors ->
            let new_errors =
              List.fold broadcast_errors ~init:errors ~f:(fun current_errors error_type ->
                  match error_type with
                  | Type.Parametric
                      {
                        name = "pyre_extensions.BroadcastError";
                        parameters =
                          [Type.Parameter.Single left_type; Type.Parameter.Single right_type];
                      } ->
                      emit_error
                        ~errors:current_errors
                        ~location
                        ~kind:
                          (Error.BroadcastError
                             {
                               expression = { location; value };
                               left = left_type;
                               right = right_type;
                             })
                  | _ -> current_errors)
            in

            { input with resolved = Type.Any; errors = new_errors }
      in
      let select_return_annotation_bidirectional_inference
          ({
             callable =
               {
                 TypeOperation.callable =
                   { Type.Callable.implementation = { parameters; _ }; overloads; _ } as callable;
                 self_argument;
               };
             _;
           } as callable_data)
        =
        let callable_data_with_return_annotation, resolution, errors =
          match parameters, overloads with
          | Type.Callable.Defined record_parameters, [] ->
              let resolution, errors, reversed_arguments =
                let forward_argument (resolution, errors, reversed_arguments) argument =
                  let expression, kind = Ast.Expression.Call.Argument.unpack argument in
                  forward_expression ~resolution expression
                  |> fun { resolution; errors = new_errors; resolved; _ } ->
                  ( resolution,
                    List.append new_errors errors,
                    { AttributeResolution.Argument.kind; expression = Some expression; resolved }
                    :: reversed_arguments )
                in
                List.fold arguments ~f:forward_argument ~init:(resolution, errors, [])
              in
              let arguments = List.rev reversed_arguments in
              let open AttributeResolution.SignatureSelection in
              prepare_arguments_for_signature_selection ~self_argument arguments
              |> get_parameter_argument_mapping
                   ~all_parameters:parameters
                   ~parameters:record_parameters
                   ~self_argument
              |> check_arguments_against_parameters
                   ~order:(GlobalResolution.full_order global_resolution)
                   ~resolve_mutable_literals:
                     (GlobalResolution.resolve_mutable_literals global_resolution)
                   ~resolve_with_locals:(resolve_expression_type_with_locals ~resolution)
                   ~callable
              |> instantiate_return_annotation
                   ~order:(GlobalResolution.full_order global_resolution)
              |> fun selected_return_annotation ->
              { callable_data with arguments; selected_return_annotation }, resolution, errors
          | _ ->
              let resolution, errors, reversed_arguments =
                let forward_argument (resolution, errors, reversed_arguments) argument =
                  let expression, kind = Ast.Expression.Call.Argument.unpack argument in
                  forward_expression ~resolution expression
                  |> fun { resolution; errors = new_errors; resolved; _ } ->
                  ( resolution,
                    List.append new_errors errors,
                    { AttributeResolution.Argument.kind; expression = Some expression; resolved }
                    :: reversed_arguments )
                in
                List.fold arguments ~f:forward_argument ~init:(resolution, errors, [])
              in
              let arguments = List.rev reversed_arguments in
              let selected_return_annotation =
                GlobalResolution.signature_select
                  ~global_resolution
                  ~resolve_with_locals:(resolve_expression_type_with_locals ~resolution)
                  ~arguments
                  ~callable
                  ~self_argument
              in
              { callable_data with arguments; selected_return_annotation }, resolution, errors
        in
        let callable_data_with_selected_return_annotation =
          return_annotation_with_callable_and_self ~resolution callable_data_with_return_annotation
        in
        [KnownCallable callable_data_with_selected_return_annotation], resolution, errors
      in
      let callables_with_selected_return_annotations, resolution, errors =
        let callable_data_list = get_callables callee |> Option.value ~default:[] in
        match callable_data_list, Context.constraint_solving_style with
        | [KnownCallable callable_data], Configuration.Analysis.ExpressionLevel ->
            select_return_annotation_bidirectional_inference callable_data
        | callable_data_list, _ ->
            let resolution, errors, reversed_arguments =
              let forward_argument (resolution, errors, reversed_arguments) argument =
                let expression, kind = Ast.Expression.Call.Argument.unpack argument in
                forward_expression ~resolution expression
                |> fun { resolution; errors = new_errors; resolved; _ } ->
                ( resolution,
                  List.append new_errors errors,
                  { AttributeResolution.Argument.kind; expression = Some expression; resolved }
                  :: reversed_arguments )
              in
              List.fold arguments ~f:forward_argument ~init:(resolution, errors, [])
            in
            let arguments = List.rev reversed_arguments in
            let callable_data_list =
              List.filter_map callable_data_list ~f:(function
                  | UnknownCallableAttribute { callable_attribute; _ } ->
                      inverse_operator_callable ~callee_attribute:callable_attribute arguments
                  | KnownCallable callable_data ->
                      Some (KnownCallable { callable_data with arguments }))
            in
            let select_annotation_for_known_callable = function
              | KnownCallable
                  ({ callable = { TypeOperation.callable; self_argument }; arguments; _ } as
                  callable_data) ->
                  let selected_return_annotation =
                    GlobalResolution.signature_select
                      ~global_resolution
                      ~resolve_with_locals:(resolve_expression_type_with_locals ~resolution)
                      ~arguments
                      ~callable
                      ~self_argument
                  in
                  KnownCallable
                    (return_annotation_with_callable_and_self
                       ~resolution
                       { callable_data with selected_return_annotation })
              | UnknownCallableAttribute other -> UnknownCallableAttribute other
            in
            List.map callable_data_list ~f:select_annotation_for_known_callable, resolution, errors
      in
      Context.Builder.add_callee
        ~global_resolution
        ~target
        ~callables:
          (List.filter_map callables_with_selected_return_annotations ~f:(function
              | KnownCallable { callable = { TypeOperation.callable; _ }; _ } -> Some callable
              | _ -> None))
        ~arguments
        ~dynamic
        ~qualifier:Context.qualifier
        ~callee_type:(Callee.resolved callee)
        ~callee:(Callee.expression callee);
      let found_return_annotations, not_found_return_annotations, undefined_attributes =
        List.partition3_map
          callables_with_selected_return_annotations
          ~f:extract_found_not_found_unknown_attribute
      in
      join_return_annotations
        ~resolution
        ~errors
        (found_return_annotations, not_found_return_annotations)
      |> (function
           | Some resolved -> resolved
           | None -> resolved_for_bad_callable ~resolution ~errors undefined_attributes)
      |> check_for_error
    in
    match value with
    | Await expression -> (
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution expression
        in
        match resolved with
        | Type.Any ->
            {
              Resolved.resolution;
              resolved = Type.Any;
              errors;
              resolved_annotation = None;
              base = None;
            }
        | _ -> (
            match
              GlobalResolution.extract_type_parameters
                global_resolution
                ~target:"typing.Awaitable"
                ~source:resolved
            with
            | Some [awaited_type] ->
                {
                  resolution;
                  resolved = awaited_type;
                  errors;
                  resolved_annotation = None;
                  base = None;
                }
            | _ ->
                let errors =
                  emit_error ~errors ~location ~kind:(Error.IncompatibleAwaitableType resolved)
                in
                { resolution; resolved = Type.Any; errors; resolved_annotation = None; base = None }
            ))
    | BooleanOperator { BooleanOperator.left; operator; right } -> (
        let {
          Resolved.resolution = resolution_left;
          resolved = resolved_left;
          errors = errors_left;
          _;
        }
          =
          forward_expression ~resolution left
        in
        let left_assume =
          match operator with
          | BooleanOperator.And -> left
          | BooleanOperator.Or -> normalize (negate left)
        in
        match refine_resolution_for_assert ~resolution:resolution_left left_assume with
        | Unreachable ->
            {
              Resolved.resolution = resolution_left;
              resolved = resolved_left;
              errors = errors_left;
              resolved_annotation = None;
              base = None;
            }
        | Value refined_resolution -> (
            let forward_right resolved_left =
              let {
                Resolved.resolution = resolution_right;
                resolved = resolved_right;
                errors = errors_right;
                _;
              }
                =
                forward_expression ~resolution:refined_resolution right
              in
              let resolved =
                match resolved_left with
                | None -> resolved_right
                | Some resolved_left ->
                    GlobalResolution.join global_resolution resolved_left resolved_right
              in
              {
                Resolved.resolution =
                  Resolution.outer_join_refinements resolution_left resolution_right;
                errors = List.append errors_left errors_right;
                resolved;
                resolved_annotation = None;
                base = None;
              }
            in
            match resolved_left, operator with
            | resolved_left, BooleanOperator.And when Type.is_falsy resolved_left ->
                (* false_expression and b has the same type as false_expression *)
                {
                  resolution = resolution_left;
                  errors = errors_left;
                  resolved = resolved_left;
                  resolved_annotation = None;
                  base = None;
                }
            | resolved_left, BooleanOperator.Or when Type.is_truthy resolved_left ->
                (* true_expression or b has the same type as true_expression *)
                {
                  resolution = resolution_left;
                  errors = errors_left;
                  resolved = resolved_left;
                  resolved_annotation = None;
                  base = None;
                }
            | resolved_left, BooleanOperator.Or when Type.is_falsy resolved_left ->
                (* false_expression or b has the same type as b *)
                forward_right None
            | resolved_left, BooleanOperator.And when Type.is_truthy resolved_left ->
                (* true_expression and b has the same type as b *)
                forward_right None
            | Type.Union parameters, BooleanOperator.Or ->
                (* If type_of(a) = Union[A, None], then type_of(a or b) = Union[A, type_of(b) under
                   assumption type_of(a) = None] *)
                let not_none_left =
                  Type.union
                    (List.filter parameters ~f:(fun parameter -> not (Type.is_none parameter)))
                in
                forward_right (Some not_none_left)
            | _, _ -> forward_right (Some resolved_left)))
    | Call { callee = { Node.value = Name (Name.Identifier "super"); _ } as callee; arguments } -> (
        let metadata =
          Resolution.parent resolution
          >>| (fun parent -> Type.Primitive (Reference.show parent))
          >>= GlobalResolution.class_metadata global_resolution
        in
        (* Resolve `super()` calls. *)
        let superclass { ClassMetadataEnvironment.successors; extends_placeholder_stub_class; _ } =
          if extends_placeholder_stub_class then
            None
          else
            List.find successors ~f:(GlobalResolution.class_exists global_resolution)
        in
        match metadata >>= superclass with
        | Some superclass ->
            let resolved = Type.Primitive superclass in
            {
              resolution;
              errors = [];
              resolved;
              resolved_annotation = None;
              base = Some (Super resolved);
            }
        | None ->
            let { Resolved.resolved; _ } = forward_expression ~resolution callee in
            forward_call
              ~resolution
              ~errors:[]
              ~target:None
              ~callee:(Callee.NonAttribute { expression = callee; resolved })
              ~dynamic:false
              ~arguments)
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "type"); _ };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        (* Resolve `type()` calls. *)
        let resolved = resolve_expression_type ~resolution value |> Type.meta in
        { resolution; errors = []; resolved; resolved_annotation = None; base = None }
    | Call { callee = { Node.location; value = Name (Name.Identifier "reveal_locals") }; _ } ->
        (* Special case reveal_locals(). *)
        let from_annotation (reference, unit) =
          let name = reference in
          let annotation =
            Option.value ~default:(Annotation.create_mutable Type.Any) (Refinement.Unit.base unit)
          in
          { Error.name; annotation }
        in
        let annotations =
          Reference.Map.Tree.to_alist (Resolution.annotation_store resolution).annotations
        in
        let temporary_annotations =
          Reference.Map.Tree.to_alist (Resolution.annotation_store resolution).temporary_annotations
        in
        let revealed_locals = List.map ~f:from_annotation (temporary_annotations @ annotations) in
        let errors = emit_error ~errors:[] ~location ~kind:(Error.RevealedLocals revealed_locals) in
        { resolution; errors; resolved = Type.none; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.location; value = Name (Name.Identifier "reveal_type") };
          arguments = { Call.Argument.value; _ } :: remainder;
        } ->
        (* Special case reveal_type(). *)
        let qualify =
          match remainder with
          | [
           {
             Call.Argument.name = Some { Node.value = name; _ };
             value = { Node.value = Constant Constant.True; _ };
           };
          ]
            when Identifier.equal name "$parameter$qualify" ->
              true
          | _ -> false
        in
        let errors =
          emit_error
            ~errors:[]
            ~location
            ~kind:
              (Error.RevealedType
                 { expression = value; annotation = resolve_expression ~resolution value; qualify })
        in
        { resolution; errors; resolved = Type.none; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.location; value = Name name };
          arguments = [{ Call.Argument.value = cast_annotation; _ }; { Call.Argument.value; _ }];
        }
      when Option.equal
             Reference.equal
             (name_to_reference name)
             (Some (Reference.create "typing.cast"))
           || Option.equal
                Reference.equal
                (name_to_reference name)
                (Some (Reference.create "pyre_extensions.safe_cast")) ->
        let contains_literal_any = Type.expression_contains_any cast_annotation in
        let errors, cast_annotation = parse_and_check_annotation ~resolution cast_annotation in
        let resolution, resolved, errors =
          let { Resolved.resolution; resolved; errors = value_errors; _ } =
            forward_expression ~resolution value
          in
          resolution, resolved, List.append value_errors errors
        in
        let errors =
          if contains_literal_any then
            emit_error
              ~errors
              ~location
              ~kind:
                (Error.ProhibitedAny
                   {
                     missing_annotation =
                       {
                         Error.name = name_to_reference_exn name;
                         annotation = None;
                         given_annotation = Some cast_annotation;
                         evidence_locations = [];
                         thrown_at_source = true;
                       };
                     annotation_kind = Annotation;
                   })
          else if Type.equal cast_annotation resolved then
            emit_error ~errors ~location ~kind:(Error.RedundantCast resolved)
          else if
            Reference.equal
              (name_to_reference_exn name)
              (Reference.create "pyre_extensions.safe_cast")
            && GlobalResolution.less_or_equal
                 global_resolution
                 ~left:cast_annotation
                 ~right:resolved
          then
            emit_error
              ~errors
              ~location
              ~kind:(Error.UnsafeCast { expression = value; annotation = cast_annotation })
          else
            errors
        in
        { resolution; errors; resolved = cast_annotation; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "isinstance"); _ } as callee;
          arguments =
            [{ Call.Argument.value = expression; _ }; { Call.Argument.value = annotations; _ }] as
            arguments;
        } ->
        let { Resolved.resolved; _ } = forward_expression ~resolution callee in
        let callables =
          match resolved with
          | Type.Callable callable -> [callable]
          | _ -> []
        in
        Context.Builder.add_callee
          ~global_resolution
          ~target:None
          ~callables
          ~arguments
          ~dynamic:false
          ~qualifier:Context.qualifier
          ~callee_type:resolved
          ~callee;

        (* Be angelic and compute errors using the typeshed annotation for isinstance. *)

        (* We special case type inference for `isinstance` in asserted, and the typeshed stubs are
           imprecise (doesn't correctly declare the arguments as a recursive tuple. *)
        let resolution, errors =
          let { Resolved.resolution; errors; _ } = forward_expression ~resolution expression in
          let resolution, errors, annotations =
            let rec collect_types (state, errors, collected) = function
              | { Node.value = Expression.Tuple annotations; _ } ->
                  List.fold annotations ~init:(state, errors, collected) ~f:collect_types
              | expression ->
                  let { Resolved.resolution; resolved; errors = expression_errors; _ } =
                    forward_expression ~resolution expression
                  in
                  let new_annotations =
                    match resolved with
                    | Type.Tuple (Concrete annotations) ->
                        List.map annotations ~f:(fun annotation ->
                            annotation, Node.location expression)
                    | Type.Tuple (Concatenation concatenation) ->
                        Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
                          concatenation
                        >>| (fun element_annotation ->
                              [element_annotation, Node.location expression])
                        |> Option.value ~default:[resolved, Node.location expression]
                    | annotation -> [annotation, Node.location expression]
                  in
                  resolution, List.append expression_errors errors, new_annotations @ collected
            in
            collect_types (resolution, errors, []) annotations
          in
          let add_incompatible_non_meta_error errors (non_meta, location) =
            emit_error
              ~errors
              ~location
              ~kind:
                (Error.IncompatibleParameterType
                   {
                     keyword_argument_name = None;
                     position = 2;
                     callee = Some (Reference.create "isinstance");
                     mismatch =
                       {
                         Error.actual = non_meta;
                         expected =
                           Type.union
                             [
                               Type.meta Type.Any;
                               Type.Tuple
                                 (Type.OrderedTypes.create_unbounded_concatenation
                                    (Type.meta Type.Any));
                             ];
                         due_to_invariance = false;
                       };
                   })
          in
          let rec is_compatible annotation =
            match annotation with
            | _ when Type.is_meta annotation or Type.is_untyped annotation -> true
            | Type.Primitive "typing._Alias" -> true
            | Type.Tuple (Concatenation concatenation) ->
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
                >>| (fun annotation -> Type.is_meta annotation)
                |> Option.value ~default:false
            | Type.Tuple (Type.OrderedTypes.Concrete annotations) ->
                List.for_all ~f:Type.is_meta annotations
            | Type.Union annotations -> List.for_all annotations ~f:is_compatible
            | _ -> false
          in
          let errors =
            List.find annotations ~f:(fun (annotation, _) -> not (is_compatible annotation))
            >>| add_incompatible_non_meta_error errors
            |> Option.value ~default:errors
          in
          resolution, errors
        in
        { resolution; errors; resolved = Type.bool; resolved_annotation = None; base = None }
    | Call
        {
          callee =
            {
              Node.value =
                Name
                  (Name.Attribute
                    { attribute = ("assertIsNotNone" | "assertTrue") as attribute; base; _ });
              _;
            } as callee;
          arguments =
            ( [{ Call.Argument.value = expression; _ }]
            | [{ Call.Argument.value = expression; _ }; _] ) as arguments;
        } ->
        let resolution, resolved_callee, errors, resolved_base =
          let resolution, assume_errors =
            let post_resolution, errors = forward_assert ~resolution expression in
            resolution_or_default post_resolution ~default:resolution, errors
          in
          let { Resolved.resolution; resolved; errors = callee_errors; base = resolved_base; _ } =
            forward_expression ~resolution callee
          in
          resolution, resolved, List.append assume_errors callee_errors, resolved_base
        in
        forward_call
          ~resolution
          ~errors
          ~target:None
          ~dynamic:true
          ~callee:
            (Callee.Attribute
               {
                 base =
                   {
                     expression = base;
                     resolved_base =
                       Resolved.resolved_base_type resolved_base |> Option.value ~default:Type.Top;
                   };
                 attribute = { name = attribute; resolved = resolved_callee };
                 expression = callee;
               })
          ~arguments
    | Call
        {
          callee =
            {
              Node.value = Name (Name.Attribute { attribute = "assertFalse" as attribute; base; _ });
              _;
            } as callee;
          arguments =
            ( [{ Call.Argument.value = expression; _ }]
            | [{ Call.Argument.value = expression; _ }; _] ) as arguments;
        } ->
        let resolution, resolved_callee, errors, resolved_base =
          let resolution, assume_errors =
            let post_resolution, errors = forward_assert ~resolution (negate expression) in
            resolution_or_default post_resolution ~default:resolution, errors
          in
          let { Resolved.resolution; resolved; errors = callee_errors; base = resolved_base; _ } =
            forward_expression ~resolution callee
          in
          resolution, resolved, List.append assume_errors callee_errors, resolved_base
        in
        forward_call
          ~resolution
          ~errors
          ~target:None
          ~dynamic:true
          ~callee:
            (Callee.Attribute
               {
                 base =
                   {
                     expression = base;
                     resolved_base =
                       Resolved.resolved_base_type resolved_base |> Option.value ~default:Type.Top;
                   };
                 attribute = { name = attribute; resolved = resolved_callee };
                 expression = callee;
               })
          ~arguments
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "getattr"); _ };
          arguments =
            { Call.Argument.value = base; _ }
            :: { Call.Argument.value = attribute_expression; _ }
            :: (([] | [_]) as default_argument);
        } -> (
        let ({ Resolved.errors; resolution; _ } as base_resolved) =
          forward_expression ~resolution base
        in
        let resolution, errors, attribute_resolved =
          forward_expression ~resolution attribute_expression
          |> fun { resolution; errors = attribute_errors; resolved = attribute_resolved; _ } ->
          resolution, List.append attribute_errors errors, attribute_resolved
        in
        let resolution, errors, has_default =
          match default_argument with
          | [{ Call.Argument.value = default_expression; _ }] ->
              forward_expression ~resolution default_expression
              |> fun { resolution; errors = default_errors; _ } ->
              resolution, List.append default_errors errors, true
          | _ -> resolution, errors, false
        in
        match attribute_resolved with
        | Type.Literal (String (LiteralValue attribute)) ->
            resolve_attribute_access
              ~base_resolved:{ base_resolved with Resolved.resolution; errors }
              ~base
              ~special:false
              ~attribute
              ~has_default
        | _ ->
            {
              Resolved.resolution;
              errors;
              resolved = Type.Any;
              base = None;
              resolved_annotation = None;
            })
    | Call call ->
        let { Call.callee; arguments } = AnnotatedCall.redirect_special_calls ~resolution call in
        let {
          Resolved.errors = callee_errors;
          resolved = resolved_callee;
          base = resolved_base;
          resolution = callee_resolution;
          _;
        }
          =
          forward_expression ~resolution callee
        in
        let { Resolved.resolution = updated_resolution; resolved; errors = updated_errors; _ } =
          let target_and_dynamic resolved_callee =
            if Type.is_meta resolved_callee then
              Some (Type.single_parameter resolved_callee), false
            else
              match resolved_base with
              | Some (Resolved.Instance resolved) when not (Type.is_top resolved) ->
                  Some resolved, true
              | Some (Resolved.Class resolved) when not (Type.is_top resolved) ->
                  Some resolved, false
              | Some (Resolved.Super resolved) when not (Type.is_top resolved) ->
                  Some resolved, false
              | _ -> None, false
          in
          let create_callee resolved =
            match Node.value callee with
            | Expression.Name (Name.Attribute { base; attribute; _ }) ->
                Callee.Attribute
                  {
                    base =
                      {
                        expression = base;
                        resolved_base =
                          Resolved.resolved_base_type resolved_base
                          |> Option.value ~default:Type.Top;
                      };
                    attribute = { name = attribute; resolved = resolved_callee };
                    expression = callee;
                  }
            | _ -> Callee.NonAttribute { expression = callee; resolved }
          in
          match resolved_callee with
          | Type.Parametric { name = "type"; parameters = [Single (Type.Union resolved_callees)] }
            ->
              let forward_inner_callable (resolution, errors, annotations) inner_resolved_callee =
                let target, dynamic = target_and_dynamic inner_resolved_callee in
                forward_call
                  ~resolution
                  ~errors:[]
                  ~target
                  ~dynamic
                  ~callee:(create_callee inner_resolved_callee)
                  ~arguments
                |> fun { resolution; resolved; errors = new_errors; _ } ->
                resolution, List.append new_errors errors, resolved :: annotations
              in
              let resolution, errors, return_annotations =
                List.fold_left
                  ~f:forward_inner_callable
                  ~init:(callee_resolution, callee_errors, [])
                  (List.map ~f:Type.meta resolved_callees)
              in
              {
                resolution;
                errors;
                resolved = Type.union return_annotations;
                resolved_annotation = None;
                base = None;
              }
          | _ ->
              let target, dynamic = target_and_dynamic resolved_callee in
              forward_call
                ~resolution:callee_resolution
                ~errors:callee_errors
                ~target
                ~dynamic
                ~callee:(create_callee resolved_callee)
                ~arguments
        in
        {
          resolution = Resolution.clear_temporary_annotations updated_resolution;
          errors = updated_errors;
          resolved;
          resolved_annotation = None;
          base = None;
        }
    | ComparisonOperator
        { ComparisonOperator.left; right; operator = ComparisonOperator.In as operator }
    | ComparisonOperator
        { ComparisonOperator.left; right; operator = ComparisonOperator.NotIn as operator } ->
        let resolve_in_call
            (resolution, errors, joined_annotation)
            { Type.instantiated; class_name; accessed_through_class; _ }
          =
          let resolve_method
              ?(accessed_through_class = false)
              ?(special_method = false)
              class_name
              instantiated
              name
            =
            GlobalResolution.attribute_from_class_name
              ~transitive:true
              ~accessed_through_class
              class_name
              ~resolution:global_resolution
              ~special_method
              ~name
              ~instantiated
            >>| Annotated.Attribute.annotation
            >>| Annotation.annotation
            >>= function
            | Type.Top -> None
            | annotation -> Some annotation
          in
          let { Resolved.resolution; resolved; errors; _ } =
            match
              resolve_method
                ~accessed_through_class
                ~special_method:true
                class_name
                instantiated
                "__contains__"
            with
            | Some resolved ->
                let callee =
                  let { Resolved.resolved = resolved_base; _ } =
                    forward_expression ~resolution right
                  in
                  Callee.Attribute
                    {
                      base = { expression = right; resolved_base };
                      attribute = { name = "__contains__"; resolved };
                      expression =
                        {
                          Node.location;
                          value =
                            Expression.Name
                              (Name.Attribute
                                 { base = right; attribute = "__contains__"; special = true });
                        };
                    }
                in
                forward_call
                  ~resolution
                  ~errors
                  ~target:(Some instantiated)
                  ~dynamic:true
                  ~callee
                  ~arguments:[{ Call.Argument.name = None; value = left }]
            | None -> (
                match
                  resolve_method
                    ~accessed_through_class
                    ~special_method:true
                    class_name
                    instantiated
                    "__iter__"
                with
                | Some iter_callable ->
                    let create_callee resolved =
                      Callee.NonAttribute
                        {
                          expression =
                            {
                              Node.location;
                              value =
                                Expression.Name
                                  (Name.Attribute
                                     { base = right; attribute = "__iter__"; special = true });
                            };
                          resolved;
                        }
                    in
                    (* Since we can't call forward_expression with the general type (we don't have a
                       good way of saying "synthetic expression with type T", simulate what happens
                       ourselves. *)
                    let forward_method
                        ~method_name
                        ~arguments
                        { Resolved.resolution; resolved = parent; errors; _ }
                      =
                      Type.split parent
                      |> fst
                      |> Type.primitive_name
                      >>= (fun class_name -> resolve_method class_name parent method_name)
                      >>| fun callable ->
                      forward_call
                        ~dynamic:true
                        ~resolution
                        ~errors
                        ~target:(Some parent)
                        ~callee:(create_callee callable)
                        ~arguments:
                          (List.map arguments ~f:(fun value -> { Call.Argument.name = None; value }))
                    in
                    forward_call
                      ~dynamic:true
                      ~resolution
                      ~errors
                      ~target:(Some instantiated)
                      ~callee:(create_callee iter_callable)
                      ~arguments:[]
                    |> forward_method ~method_name:"__next__" ~arguments:[]
                    >>= forward_method ~method_name:"__eq__" ~arguments:[left]
                    |> Option.value
                         ~default:
                           {
                             Resolved.resolution;
                             errors;
                             resolved = Type.Top;
                             resolved_annotation = None;
                             base = None;
                           }
                | None -> (
                    let getitem_attribute =
                      {
                        Node.location;
                        value =
                          Expression.Name
                            (Name.Attribute
                               { base = right; attribute = "__getitem__"; special = true });
                      }
                    in
                    let call =
                      let getitem =
                        {
                          Node.location;
                          value =
                            Expression.Call
                              {
                                callee = getitem_attribute;
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value =
                                        {
                                          Node.location;
                                          value = Expression.Constant (Constant.Integer 0);
                                        };
                                    };
                                  ];
                              };
                        }
                      in
                      {
                        Node.location;
                        value =
                          Expression.Call
                            {
                              callee =
                                {
                                  Node.location;
                                  value =
                                    Name
                                      (Name.Attribute
                                         { base = getitem; attribute = "__eq__"; special = true });
                                };
                              arguments = [{ Call.Argument.name = None; value = left }];
                            };
                      }
                    in
                    let ({ Resolved.resolved; _ } as getitem_resolution) =
                      forward_expression ~resolution getitem_attribute
                    in
                    let is_valid_getitem = function
                      | Type.Parametric
                          {
                            name = "BoundMethod";
                            parameters =
                              [
                                Single
                                  (Type.Callable
                                    {
                                      implementation =
                                        { parameters = Defined (_ :: index_parameter :: _); _ };
                                      _;
                                    });
                                _;
                              ];
                          }
                      | Type.Callable
                          {
                            implementation = { parameters = Defined (_ :: index_parameter :: _); _ };
                            _;
                          }
                        when GlobalResolution.less_or_equal
                               global_resolution
                               ~left:Type.integer
                               ~right:
                                 (Type.Callable.Parameter.annotation index_parameter
                                 |> Option.value ~default:Type.Bottom) ->
                          true
                      | _ -> false
                    in
                    match resolved with
                    | Type.Union elements when List.for_all ~f:is_valid_getitem elements ->
                        forward_expression ~resolution call
                    | _ when is_valid_getitem resolved -> forward_expression ~resolution call
                    | _ ->
                        let errors =
                          let { Resolved.resolved; _ } = forward_expression ~resolution right in
                          emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.UnsupportedOperand
                                 (Unary
                                    {
                                      operator_name =
                                        Format.asprintf
                                          "%a"
                                          ComparisonOperator.pp_comparison_operator
                                          operator;
                                      operand = resolved;
                                    }))
                        in
                        { getitem_resolution with Resolved.resolved = Type.Any; errors }))
          in
          resolution, errors, GlobalResolution.join global_resolution joined_annotation resolved
        in
        let { Resolved.resolution; resolved; errors; _ } = forward_expression ~resolution right in
        let resolution, errors, resolved =
          (* We should really error here if class_data_for_attribute_lookup fails *)
          Type.class_data_for_attribute_lookup resolved
          >>| List.fold ~f:resolve_in_call ~init:(resolution, errors, Type.Bottom)
          |> Option.value ~default:(resolution, errors, Type.Bottom)
        in
        let resolved = if Type.equal resolved Type.Bottom then Type.Top else resolved in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | ComparisonOperator ({ ComparisonOperator.left; right; _ } as operator) -> (
        let operator = { operator with left } in
        match ComparisonOperator.override ~location operator with
        | Some expression ->
            let resolved = forward_expression ~resolution expression in
            { resolved with errors = resolved.errors }
        | None ->
            forward_expression ~resolution left
            |> (fun { Resolved.resolution; errors = left_errors; _ } ->
                 let { Resolved.resolution; errors = right_errors; _ } =
                   forward_expression ~resolution right
                 in
                 resolution, List.append left_errors right_errors)
            |> fun (resolution, errors) ->
            {
              Resolved.resolution;
              errors;
              resolved = Type.bool;
              resolved_annotation = None;
              base = None;
            })
    | Constant (Constant.Complex _) ->
        {
          resolution;
          errors = [];
          resolved = Type.complex;
          resolved_annotation = None;
          base = None;
        }
    | Dictionary { Dictionary.entries; keywords } ->
        let key, value, resolution, errors =
          let forward_entry (key, value, resolution, errors) entry =
            let new_key, new_value, resolution, errors = forward_entry ~resolution ~errors ~entry in
            ( GlobalResolution.join global_resolution key new_key,
              GlobalResolution.join global_resolution value new_value,
              resolution,
              errors )
          in
          List.fold entries ~f:forward_entry ~init:(Type.Bottom, Type.Bottom, resolution, [])
        in
        let key =
          if List.is_empty keywords && Type.is_unbound key then
            Type.variable "_KT" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            key
        in
        let value =
          if List.is_empty keywords && Type.is_unbound value then
            Type.variable "_VT" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            value
        in
        let resolved_key_and_value, resolution, errors =
          let forward_keyword (resolved, resolution, errors) keyword =
            match resolved with
            | None -> resolved, resolution, errors
            | Some (key, value) -> (
                let { Resolved.resolution; resolved = source; errors = new_errors; _ } =
                  forward_expression ~resolution keyword
                in
                let errors = List.append new_errors errors in
                match source with
                | Top
                | Bottom
                | Any ->
                    None, resolution, errors
                | _ -> (
                    match
                      GlobalResolution.extract_type_parameters
                        global_resolution
                        ~source
                        ~target:"typing.Mapping"
                    with
                    | Some [new_key; new_value] ->
                        ( Some
                            ( GlobalResolution.join global_resolution key new_key,
                              GlobalResolution.join global_resolution value new_value ),
                          resolution,
                          errors )
                    | _ ->
                        let errors =
                          emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.InvalidArgument
                                 (Error.Keyword
                                    {
                                      expression = Some keyword;
                                      annotation = source;
                                      require_string_keys = false;
                                    }))
                        in
                        None, resolution, errors))
          in
          List.fold keywords ~f:forward_keyword ~init:(Some (key, value), resolution, errors)
        in
        let resolved =
          resolved_key_and_value
          >>| (fun (key, value) -> Type.dictionary ~key ~value)
          |> Option.value ~default:Type.Top
        in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | DictionaryComprehension { Comprehension.element; generators } ->
        let key, value, _, errors =
          List.fold
            generators
            ~f:(fun (resolution, errors) generator ->
              forward_generator ~resolution ~errors ~generator)
            ~init:(resolution, [])
          |> fun (resolution, errors) -> forward_entry ~resolution ~errors ~entry:element
        in
        (* Discard generator-local variables. *)
        {
          resolution;
          errors;
          resolved = Type.dictionary ~key ~value;
          resolved_annotation = None;
          base = None;
        }
    | Constant Constant.Ellipsis ->
        { resolution; errors = []; resolved = Type.Any; resolved_annotation = None; base = None }
    | Constant Constant.False ->
        {
          resolution;
          errors = [];
          resolved = Type.Literal (Type.Boolean false);
          resolved_annotation = None;
          base = None;
        }
    | Constant (Constant.Float _) ->
        { resolution; errors = []; resolved = Type.float; resolved_annotation = None; base = None }
    | Constant (Constant.Integer literal) ->
        {
          resolution;
          errors = [];
          resolved = Type.literal_integer literal;
          resolved_annotation = None;
          base = None;
        }
    | Constant (Constant.BigInteger _) ->
        {
          resolution;
          errors = [];
          resolved = Type.integer;
          resolved_annotation = None;
          base = None;
        }
    | Constant (Constant.String { StringLiteral.kind = StringLiteral.Bytes; value }) ->
        {
          resolution;
          errors = [];
          resolved = Type.literal_bytes value;
          resolved_annotation = None;
          base = None;
        }
    | Constant (Constant.String { StringLiteral.kind = StringLiteral.String; value }) ->
        {
          resolution;
          errors = [];
          resolved = Type.literal_string value;
          resolved_annotation = None;
          base = None;
        }
    | Constant Constant.True ->
        {
          resolution;
          errors = [];
          resolved = Type.Literal (Type.Boolean true);
          resolved_annotation = None;
          base = None;
        }
    | FormatString substrings ->
        let forward_substring ((resolution, has_non_literal, errors) as sofar) = function
          | Substring.Literal _ -> sofar
          | Substring.Format expression ->
              forward_expression ~resolution expression
              |> fun { resolution; errors = new_errors; resolved; _ } ->
              let has_non_literal =
                match resolved with
                | Type.Literal _ -> has_non_literal
                | _ -> true
              in
              resolution, has_non_literal, List.append new_errors errors
        in
        let resolution, has_non_literal, errors =
          List.fold substrings ~f:forward_substring ~init:(resolution, false, [])
        in
        let resolved = if has_non_literal then Type.string else Type.literal_any_string in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | Generator { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        let has_async_generator = List.exists ~f:(fun generator -> generator.async) generators in
        let has_await =
          match Node.value element with
          | Expression.Await _ -> true
          | _ -> false
        in
        let generator =
          if has_async_generator || has_await then
            Type.async_generator ~yield_type:resolved ()
          else
            Type.generator_expression resolved
        in
        { resolution; errors; resolved = generator; resolved_annotation = None; base = None }
    | Lambda { Lambda.body; parameters } ->
        let resolution_with_parameters =
          let add_parameter resolution { Node.value = { Parameter.name; _ }; _ } =
            let name = String.chop_prefix name ~prefix:"*" |> Option.value ~default:name in
            Resolution.new_local
              resolution
              ~reference:(Reference.create name)
              ~annotation:(Annotation.create_mutable Type.Any)
          in
          List.fold ~f:add_parameter ~init:resolution parameters
        in
        let { Resolved.resolved; errors; _ } =
          forward_expression ~resolution:resolution_with_parameters body
        in
        (* Judgement call, many more people want to pass in `lambda: 0` to `defaultdict` than want
           to write a function that take in callables with literal return types. If you really want
           that behavior you can always write a real inner function with a literal return type *)
        let resolved = Type.weaken_literals resolved in
        let create_parameter { Node.value = { Parameter.name; value; _ }; _ } =
          { Type.Callable.Parameter.name; annotation = Type.Any; default = Option.is_some value }
        in
        let parameters =
          List.map parameters ~f:create_parameter
          |> Type.Callable.Parameter.create
          |> fun parameters -> Type.Callable.Defined parameters
        in
        {
          resolution;
          errors;
          resolved = Type.Callable.create ~parameters ~annotation:resolved ();
          resolved_annotation = None;
          base = None;
        }
    | List elements ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_elements ~resolution ~errors:[] ~elements
        in
        {
          resolution;
          errors;
          resolved = Type.list resolved;
          resolved_annotation = None;
          base = None;
        }
    | ListComprehension { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        {
          resolution;
          errors;
          resolved = Type.list resolved;
          resolved_annotation = None;
          base = None;
        }
    | Name (Name.Identifier identifier) ->
        forward_reference ~resolution ~location ~errors:[] (Reference.create identifier)
    | Name (Name.Attribute { base; attribute; special } as name) -> (
        (*
         * Attribute accesses are recursively resolved by stripping mames off
         * of the end and then trying
         * to resolve the prefix. For example, `foo().bar` will be stripped to
         * `foo()` first, then once that type is resolved we'll look up the
         * `bar` attribute.
         *
         * But to handle lazy module tracking, which requires us to only
         * support limited cases of implicit namespace modules, we also want
         * to check whether the reference can be directly resolved to a type
         * (for example `pandas.core.DataFrame`) and short-circuit the
         * recursion in that case.
         *
         * Our method for doing this is to decide whether a name can be
         * directly looked up, short ciruiting the recursion, by using
         * - `name_to_reference`, which produces a reference when a `Name`
         *    corresponds to a plain reference (for example `foo().bar` does
         *    not but `pandas.core.DataFrame` does, and
         * - `resolve_exports`, which does a principled syntax-based lookup
         *    to see if a name makes sense as a module top-level name
         *)
        match name_to_reference name with
        | Some module_reference
          when is_toplevel_module_reference ~global_resolution module_reference ->
            (* TODO(T125828725) Use the resolved name coming from resolve_exports, rather than
               throwing away that name and falling back to legacy_resolve_exports.

               This requires either using better qualification architecture or refactors of existing
               python code that relies on the legacy behaviror in the presence of ambiguous
               fully-qualified names. *)
            forward_reference ~resolution ~location ~errors:[] module_reference
        | _ ->
            let ({ Resolved.errors; resolved = resolved_base; _ } as base_resolved) =
              forward_expression ~resolution base
            in
            let errors, resolved_base =
              if Type.Variable.contains_escaped_free_variable resolved_base then
                let errors =
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.IncompleteType
                         {
                           target = base;
                           annotation = resolved_base;
                           attempted_action = Error.AttributeAccess attribute;
                         })
                in
                errors, Type.Variable.convert_all_escaped_free_variables_to_anys resolved_base
              else
                errors, resolved_base
            in
            resolve_attribute_access
              ~base_resolved:{ base_resolved with errors; resolved = resolved_base }
              ~base
              ~special
              ~attribute
              ~has_default:false)
    | Constant Constant.NoneLiteral ->
        {
          resolution;
          errors = [];
          resolved = Type.NoneType;
          resolved_annotation = None;
          base = None;
        }
    | Set elements ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_elements ~resolution ~errors:[] ~elements
        in
        {
          resolution;
          errors;
          resolved = Type.set resolved;
          resolved_annotation = None;
          base = None;
        }
    | SetComprehension { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        {
          resolution;
          errors;
          resolved = Type.set resolved;
          resolved_annotation = None;
          base = None;
        }
    | Starred starred ->
        let resolved =
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              forward_expression ~resolution expression
        in
        { resolved with resolved = Type.Top; resolved_annotation = None; base = None }
    | Ternary { Ternary.target; test; alternative } ->
        let test_errors =
          let { Resolved.errors; _ } = forward_expression ~resolution test in
          errors
        in
        let target_resolved, target_errors =
          let post_resolution = refine_resolution_for_assert ~resolution test in
          let resolution = resolution_or_default post_resolution ~default:resolution in
          let { Resolved.resolved; errors; _ } = forward_expression ~resolution target in
          resolved, errors
        in
        let alternative_resolved, alternative_errors =
          let post_resolution =
            refine_resolution_for_assert ~resolution (normalize (negate test))
          in
          let resolution = resolution_or_default post_resolution ~default:resolution in
          let { Resolved.resolved; errors; _ } = forward_expression ~resolution alternative in
          resolved, errors
        in
        let resolved =
          (* Joining Literals as their union is currently too expensive, so we do it only for
             ternary expressions. *)
          match target_resolved, alternative_resolved with
          | Type.Literal (Type.String Type.AnyLiteral), Type.Literal (Type.String _)
          | Type.Literal (Type.String _), Type.Literal (Type.String Type.AnyLiteral) ->
              Type.Literal (Type.String Type.AnyLiteral)
          | Type.Literal (Type.Boolean _), Type.Literal (Type.Boolean _)
          | Type.Literal (Type.Integer _), Type.Literal (Type.Integer _)
          | Type.Literal (Type.String _), Type.Literal (Type.String _)
          | Type.Literal (Type.EnumerationMember _), Type.Literal (Type.EnumerationMember _) ->
              Type.union [target_resolved; alternative_resolved]
          | _ -> GlobalResolution.join global_resolution target_resolved alternative_resolved
        in
        let errors = List.concat [test_errors; target_errors; alternative_errors] in
        (* The resolution is local to the ternary expression and should not be propagated out. *)
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | Tuple elements ->
        let resolution, errors, resolved_elements =
          let forward_element (resolution, errors, resolved) expression =
            let resolution, new_errors, resolved_element =
              match expression with
              | { Node.value = Expression.Starred (Starred.Once expression); _ } ->
                  let { Resolved.resolution; resolved = resolved_element; errors = new_errors; _ } =
                    forward_expression ~resolution expression
                  in
                  let new_errors, ordered_type =
                    match resolved_element with
                    | Type.Tuple ordered_type -> new_errors, ordered_type
                    | Type.Any ->
                        new_errors, Type.OrderedTypes.create_unbounded_concatenation Type.Any
                    | _ -> (
                        match
                          GlobalResolution.type_of_iteration_value
                            ~global_resolution
                            resolved_element
                        with
                        | Some element_type ->
                            ( new_errors,
                              Type.OrderedTypes.create_unbounded_concatenation element_type )
                        | None ->
                            ( emit_error
                                ~errors:new_errors
                                ~location
                                ~kind:
                                  (Error.TupleConcatenationError
                                     (UnpackingNonIterable { annotation = resolved_element })),
                              Type.OrderedTypes.create_unbounded_concatenation Type.Any ))
                  in
                  resolution, new_errors, ordered_type
              | _ ->
                  let { Resolved.resolution; resolved = resolved_element; errors = new_errors; _ } =
                    forward_expression ~resolution expression
                  in
                  resolution, new_errors, Type.OrderedTypes.Concrete [resolved_element]
            in
            resolution, List.append new_errors errors, resolved_element :: resolved
          in
          List.fold elements ~f:forward_element ~init:(resolution, [], [])
        in
        let resolved, errors =
          let resolved_elements = List.rev resolved_elements in
          let concatenated_elements =
            let concatenate sofar next =
              sofar >>= fun left -> Type.OrderedTypes.concatenate ~left ~right:next
            in
            List.fold resolved_elements ~f:concatenate ~init:(Some (Type.OrderedTypes.Concrete []))
          in
          match concatenated_elements with
          | Some concatenated_elements -> Type.Tuple concatenated_elements, errors
          | None ->
              let variadic_expressions =
                match List.zip elements resolved_elements with
                | Ok pairs ->
                    List.filter_map pairs ~f:(function
                        | expression, Type.OrderedTypes.Concatenation _ -> Some expression
                        | _ -> None)
                | Unequal_lengths -> elements
              in
              ( Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Any),
                emit_error
                  ~errors
                  ~location
                  ~kind:(Error.TupleConcatenationError (MultipleVariadics { variadic_expressions }))
              )
        in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | UnaryOperator ({ UnaryOperator.operand; _ } as operator) -> (
        match UnaryOperator.override operator with
        | Some expression -> forward_expression ~resolution expression
        | None ->
            let resolved = forward_expression ~resolution operand in
            { resolved with resolved = Type.bool; resolved_annotation = None; base = None })
    | WalrusOperator { value; target } ->
        let resolution, errors =
          let post_resolution, errors =
            forward_assignment ~resolution ~location ~target ~value ~annotation:None
          in
          resolution_or_default post_resolution ~default:resolution, errors
        in
        let resolved = forward_expression ~resolution value in
        { resolved with errors = List.append errors resolved.errors }
    | Expression.Yield yielded ->
        let { Resolved.resolution; resolved = yield_type; errors; _ } =
          match yielded with
          | Some expression ->
              let { Resolved.resolution; resolved; errors; _ } =
                forward_expression ~resolution expression
              in
              { resolution; errors; resolved; resolved_annotation = None; base = None }
          | None ->
              {
                resolution;
                errors = [];
                resolved = Type.none;
                resolved_annotation = None;
                base = None;
              }
        in
        let actual =
          if define_signature.async then
            Type.async_generator ~yield_type ()
          else
            Type.generator ~yield_type ()
        in
        let errors =
          validate_return ~location None ~resolution ~errors ~actual ~is_implicit:false
        in
        let send_type, _ =
          return_annotation ~global_resolution
          |> GlobalResolution.type_of_generator_send_and_return ~global_resolution
        in
        { resolution; errors; resolved = send_type; resolved_annotation = None; base = None }
    | Expression.YieldFrom yielded_from ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution yielded_from
        in
        let yield_type =
          resolved
          |> GlobalResolution.type_of_iteration_value ~global_resolution
          |> Option.value ~default:Type.Any
        in
        let send_type, subgenerator_return_type =
          GlobalResolution.type_of_generator_send_and_return ~global_resolution resolved
        in
        let actual =
          if define_signature.async then
            Type.async_generator ~yield_type ~send_type ()
          else
            Type.generator ~yield_type ~send_type ()
        in
        let errors =
          validate_return ~location None ~resolution ~errors ~actual ~is_implicit:false
        in
        {
          resolution;
          errors;
          resolved = subgenerator_return_type;
          resolved_annotation = None;
          base = None;
        }


  (* Since the control flow graph has already preprocessed assert statements (see
     [this](https://www.internalfb.com/intern/graphviz/?paste=65053708) for an example), type
     refinment happens here based on these asserts. *)
  and refine_resolution_for_assert ~resolution test =
    let global_resolution = Resolution.global_resolution resolution in
    let annotation_less_or_equal =
      Annotation.less_or_equal
        ~type_less_or_equal:(GlobalResolution.less_or_equal global_resolution)
    in
    let parse_refinement_annotation annotation =
      let parse_meta annotation =
        match parse_and_check_annotation ~resolution annotation |> snd with
        | Type.Top -> (
            (* Try to resolve meta-types given as expressions. *)
            match resolve_expression_type ~resolution annotation with
            | annotation when Type.is_meta annotation -> Type.single_parameter annotation
            | Type.Tuple (Concrete elements) when List.for_all ~f:Type.is_meta elements ->
                List.map ~f:Type.single_parameter elements |> Type.union
            | Type.Tuple (Concatenation concatenation) ->
                Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
                >>= (fun element ->
                      if Type.is_meta element then
                        Some (Type.single_parameter element)
                      else
                        None)
                |> Option.value ~default:Type.Top
            | _ -> Type.Top)
        | annotation -> annotation
      in
      match annotation with
      | { Node.value = Expression.Tuple elements; _ } ->
          List.map ~f:parse_meta elements |> fun elements -> Type.Union elements
      | _ -> parse_meta annotation
    in
    (* TODO(T131546670) replace with meet *)
    let partition annotation ~boundary =
      let consistent_with_boundary, not_consistent_with_boundary =
        let unfolded_annotation =
          match annotation with
          | Type.RecursiveType recursive_type ->
              Type.RecursiveType.unfold_recursive_type recursive_type
          | _ -> annotation
        in
        let extract_union_members = function
          | Type.Union parameters -> parameters
          | annotation -> [annotation]
        in
        extract_union_members unfolded_annotation
        |> List.partition_tf ~f:(fun left ->
               Type.ReadOnly.unpack_readonly left
               |> Option.value ~default:left
               |> fun left ->
               Resolution.is_consistent_with resolution left boundary ~expression:None)
      in
      let not_consistent_with_boundary =
        if List.is_empty not_consistent_with_boundary then
          None
        else
          Some (Type.union not_consistent_with_boundary)
      in
      let consistent_with_boundary = Type.union consistent_with_boundary in
      { consistent_with_boundary; not_consistent_with_boundary }
    in
    let is_temporary_refinement name =
      let rec refinable_annotation name =
        let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
        match
          ( Resolution.get_local_with_attributes
              ~global_fallback:false
              ~name:partitioned_name
              ~attribute_path
              resolution,
            name )
        with
        | Some local_annotation, _ -> Some local_annotation
        | _, Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ } -> (
            let attribute =
              refinable_annotation base
              >>| Annotation.annotation
              >>= fun parent ->
              Type.split parent
              |> fst
              |> Type.primitive_name
              >>= GlobalResolution.attribute_from_class_name
                    ~resolution:global_resolution
                    ~name:attribute
                    ~instantiated:parent
                    ~transitive:true
            in
            match
              ( attribute >>| AnnotatedAttribute.visibility,
                attribute >>| AnnotatedAttribute.defined,
                attribute >>| AnnotatedAttribute.annotation )
            with
            | Some (ReadOnly (Refinable _)), Some true, Some annotation -> Some annotation
            | _ -> None)
        | _ -> None
      in
      Option.is_none (refinable_annotation name)
    in
    let rec existing_annotation name =
      let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
      match
        ( Resolution.get_local_with_attributes
            ~global_fallback:true
            ~name:partitioned_name
            ~attribute_path
            resolution,
          name )
      with
      | Some annotation, _ -> Some annotation
      | _, Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ } -> (
          let attribute =
            existing_annotation base
            >>| Annotation.annotation
            >>= fun parent ->
            Type.split parent
            |> fst
            |> Type.primitive_name
            >>= GlobalResolution.attribute_from_class_name
                  ~resolution:global_resolution
                  ~name:attribute
                  ~instantiated:parent
                  ~transitive:true
          in
          match attribute with
          | Some attribute when AnnotatedAttribute.defined attribute ->
              Some (AnnotatedAttribute.annotation attribute)
          | _ -> None)
      | _ -> None
    in
    let refine_local ~name annotation =
      let { name = partitioned_name; attribute_path; base_annotation } =
        partition_name ~resolution name
      in
      Resolution.refine_local_with_attributes
        ~temporary:(is_temporary_refinement name)
        resolution
        ~name:partitioned_name
        ~attribute_path
        ~base_annotation
        ~annotation
    in
    match Node.value test with
    (* Explicit asserting falsy values. *)
    | Expression.Constant Constant.(False | NoneLiteral)
    | Expression.Constant (Constant.Integer 0)
    | Expression.Constant (Constant.Float 0.0)
    | Expression.Constant (Constant.Complex 0.0)
    | Expression.Constant (Constant.String { StringLiteral.value = ""; _ })
    | Expression.List []
    | Expression.Tuple []
    | Expression.Dictionary { Dictionary.entries = []; keywords = [] } ->
        Unreachable
    (* Type is the same as `annotation_expression` *)
    | ComparisonOperator
        {
          left =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "type"); _ };
                    arguments =
                      [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
                  };
              _;
            };
          operator = ComparisonOperator.Is;
          right = annotation_expression;
        }
    | ComparisonOperator
        {
          left =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "type"); _ };
                    arguments =
                      [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
                  };
              _;
            };
          operator = ComparisonOperator.Equals;
          right = annotation_expression;
        }
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
          arguments =
            [
              { Call.Argument.name = None; value = { Node.value = Name name; _ } };
              { Call.Argument.name = None; value = annotation_expression };
            ];
        }
      when is_simple_name name ->
        let parsed_annotation = parse_refinement_annotation annotation_expression in
        let resolution =
          let refinement_unnecessary existing_annotation =
            annotation_less_or_equal
              ~left:existing_annotation
              ~right:(Annotation.create_mutable parsed_annotation)
            && (not (Type.equal (Annotation.annotation existing_annotation) Type.Bottom))
            && not (Type.equal (Annotation.annotation existing_annotation) Type.Any)
          in
          match existing_annotation name with
          | Some _ when Type.is_any parsed_annotation -> Value resolution
          | None -> Value resolution
          | Some existing_annotation when refinement_unnecessary existing_annotation ->
              Value resolution
          (* Clarify Anys if possible *)
          | Some existing_annotation
            when Type.equal (Annotation.annotation existing_annotation) Type.Any ->
              Value (Annotation.create_mutable parsed_annotation |> refine_local ~name)
          | Some existing_annotation ->
              let existing_type = Annotation.annotation existing_annotation in
              let { consistent_with_boundary; _ } =
                partition existing_type ~boundary:parsed_annotation
              in
              if not (Type.is_unbound consistent_with_boundary) then
                Value (Annotation.create_mutable consistent_with_boundary |> refine_local ~name)
              else if
                GlobalResolution.less_or_equal_either_way
                  global_resolution
                  existing_type
                  parsed_annotation
              then
                (* If we have `isinstance(x, Child)` where `x` is of type `Base`, then it is sound
                   to refine `x` to `Child` because the runtime will not enter the branch unless `x`
                   is an instance of `Child`. *)
                let refined_type =
                  if
                    (not (Type.is_top existing_type))
                    && GlobalResolution.less_or_equal
                         global_resolution
                         ~left:(Type.ReadOnly.create parsed_annotation)
                         ~right:existing_type
                  then
                    (* In the case where `x` is `ReadOnly[Base]`, we need to refine it to
                       `ReadOnly[Child]`. Otherwise, the code could modify the object. *)
                    Type.ReadOnly.create parsed_annotation
                  else
                    parsed_annotation
                in
                Value (Annotation.create_mutable refined_type |> refine_local ~name)
              else
                Unreachable
        in
        resolution
    (* Type is *not* the same as `annotation_expression` *)
    | ComparisonOperator
        {
          left =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "type"); _ };
                    arguments = [{ Call.Argument.name = None; value }];
                  };
              _;
            };
          operator = ComparisonOperator.IsNot;
          right = annotation_expression;
        }
    | ComparisonOperator
        {
          left =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "type"); _ };
                    arguments = [{ Call.Argument.name = None; value }];
                  };
              _;
            };
          operator = ComparisonOperator.NotEquals;
          right = annotation_expression;
        }
    | UnaryOperator
        {
          UnaryOperator.operator = UnaryOperator.Not;
          operand =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
                    arguments =
                      [
                        { Call.Argument.name = None; value };
                        { Call.Argument.name = None; value = annotation_expression };
                      ];
                  };
              _;
            };
        } -> (
        let resolve_non_instance ~boundary name =
          let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
          match
            Resolution.get_local_with_attributes resolution ~name:partitioned_name ~attribute_path
          with
          | Some { annotation = previous_annotation; _ } ->
              let { not_consistent_with_boundary; _ } = partition previous_annotation ~boundary in
              not_consistent_with_boundary
              >>| Annotation.create_mutable
              >>| refine_local ~name
              |> Option.value ~default:resolution
          | _ -> resolution
        in
        let boundary = parse_refinement_annotation annotation_expression in
        let is_consistent_with_boundary =
          if Type.contains_unknown boundary || Type.is_any boundary then
            false
          else
            let { Resolved.resolved; _ } = forward_expression ~resolution value in
            (not (Type.is_unbound resolved))
            && (not (Type.contains_unknown resolved))
            && (not (Type.is_any resolved))
            && GlobalResolution.less_or_equal global_resolution ~left:resolved ~right:boundary
        in
        match is_consistent_with_boundary, value with
        | true, _ -> Unreachable
        | _, { Node.value = Name name; _ } when is_simple_name name ->
            Value (resolve_non_instance ~boundary name)
        | _ -> Value resolution)
    (* Is callable *)
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "callable"); _ };
          arguments = [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
        }
      when is_simple_name name -> (
        match existing_annotation name with
        | Some existing_annotation ->
            let callable =
              Type.Callable.create ~parameters:Undefined ~annotation:Type.object_primitive ()
            in
            let existing_type = Annotation.annotation existing_annotation in
            let { consistent_with_boundary; _ } = partition existing_type ~boundary:callable in
            (* Check for supertypes of callable: e.g. object is only returned on
               not_consistent_with_boundary in partition *)
            if GlobalResolution.less_or_equal global_resolution ~left:callable ~right:existing_type
            then
              Value (Annotation.create_mutable callable |> refine_local ~name)
            else if not (Type.is_unbound consistent_with_boundary) then
              Value (Annotation.create_mutable consistent_with_boundary |> refine_local ~name)
            else
              Unreachable
        | _ -> Value resolution)
    (* Is not callable *)
    | UnaryOperator
        {
          UnaryOperator.operator = UnaryOperator.Not;
          operand =
            {
              Node.value =
                Call
                  {
                    callee = { Node.value = Name (Name.Identifier "callable"); _ };
                    arguments =
                      [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
                  };
              _;
            };
        }
      when is_simple_name name -> (
        match existing_annotation name with
        | Some existing_annotation -> (
            let callable =
              Type.Callable.create ~parameters:Undefined ~annotation:Type.object_primitive ()
            in
            let existing_type = Annotation.annotation existing_annotation in
            let { not_consistent_with_boundary; _ } = partition existing_type ~boundary:callable in
            match not_consistent_with_boundary with
            | Some type_ -> Value (Annotation.create_mutable type_ |> refine_local ~name)
            | None when Type.is_any existing_type ->
                Value (Annotation.create_mutable existing_type |> refine_local ~name)
            | None -> Unreachable)
        | _ -> Value resolution)
    (* Is typeddict *)
    | Call
        {
          callee =
            {
              Node.value =
                Name
                  (Name.Attribute
                    {
                      base = { Node.location = _; value = Name (Name.Identifier "typing") };
                      attribute = "is_typeddict";
                      special = false;
                    });
              _;
            };
          arguments = [{ value = { Node.value = Name name; _ }; _ }];
        }
      when is_simple_name name -> (
        match existing_annotation name with
        | Some existing_annotation -> (
            match Annotation.annotation existing_annotation with
            | Type.Parametric { name = "type"; parameters = [Single typed_dictionary] } ->
                if
                  Type.is_any typed_dictionary
                  or GlobalResolution.is_typed_dictionary
                       ~resolution:global_resolution
                       typed_dictionary
                then
                  Value resolution
                else
                  Unreachable
            | Type.Any -> Value resolution
            | _ -> Unreachable)
        | _ -> Value resolution)
    (* Is not typeddict *)
    | UnaryOperator
        {
          UnaryOperator.operator = UnaryOperator.Not;
          operand =
            {
              Node.value =
                Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                base =
                                  { Node.location = _; value = Name (Name.Identifier "typing") };
                                attribute = "is_typeddict";
                                special = false;
                              });
                        _;
                      };
                    arguments = [{ value = { Node.value = Name name; _ }; _ }];
                  };
              _;
            };
          _;
        }
      when is_simple_name name -> (
        match existing_annotation name with
        | Some existing_annotation -> (
            match Annotation.annotation existing_annotation with
            | Type.Parametric { name = "type"; parameters = [Single typed_dictionary] } ->
                if
                  GlobalResolution.is_typed_dictionary
                    ~resolution:global_resolution
                    typed_dictionary
                then
                  Unreachable
                else
                  Value resolution
            | _ -> Value resolution)
        | _ -> Value resolution)
    (* `is` and `in` refinement *)
    | ComparisonOperator
        {
          ComparisonOperator.left = { Node.value = Name name; _ };
          operator = ComparisonOperator.Is;
          right;
        }
      when is_simple_name name -> (
        let { Resolved.resolved = refined; _ } = forward_expression ~resolution right in
        let refined = Annotation.create_mutable refined in
        match existing_annotation name with
        | Some previous ->
            if annotation_less_or_equal ~left:refined ~right:previous then
              Value (refine_local ~name refined)
            else
              (* Keeping previous state, since it is more refined. *)
              (* TODO: once T38750424 is done, we should really return bottom if previous is not <=
                 refined and refined is not <= previous, as this is an obvious contradiction. *)
              Value resolution
        | None -> Value resolution)
    | ComparisonOperator
        {
          ComparisonOperator.left = { Node.value = Name name; _ };
          operator = ComparisonOperator.In;
          right;
        }
      when is_simple_name name -> (
        let reference = name_to_reference_exn name in
        let { Resolved.resolved; _ } = forward_expression ~resolution right in
        match
          GlobalResolution.extract_type_parameters
            global_resolution
            ~target:"typing.Iterable"
            ~source:resolved
        with
        | Some [element_type] -> (
            let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
            let annotation =
              Resolution.get_local_with_attributes
                ~global_fallback:false
                ~name:partitioned_name
                ~attribute_path
                resolution
            in
            match annotation with
            | Some previous ->
                let refined =
                  if Annotation.is_immutable previous then
                    Annotation.create_immutable
                      ~original:(Some (Annotation.original previous))
                      element_type
                  else
                    Annotation.create_mutable element_type
                in
                if annotation_less_or_equal ~left:refined ~right:previous then
                  Value (refine_local ~name refined)
                else (* Keeping previous state, since it is more refined. *)
                  Value resolution
            | None when not (Resolution.is_global resolution ~reference) ->
                let resolution = refine_local ~name (Annotation.create_mutable element_type) in
                Value resolution
            | _ -> Value resolution)
        | _ -> Value resolution)
    (* Not-none checks (including ones that work over containers) *)
    | ComparisonOperator
        {
          ComparisonOperator.left;
          operator = ComparisonOperator.IsNot;
          right = { Node.value = Constant Constant.NoneLiteral; _ };
        } ->
        refine_resolution_for_assert ~resolution left
    | Name name when is_simple_name name -> (
        match existing_annotation name with
        | Some { Annotation.annotation = Type.NoneType; _ } -> Unreachable
        | Some ({ Annotation.annotation = Type.Union parameters; _ } as annotation) ->
            let refined_annotation =
              List.filter parameters ~f:(fun parameter -> not (Type.is_none parameter))
            in
            let resolution =
              refine_local
                ~name
                { annotation with Annotation.annotation = Type.union refined_annotation }
            in
            Value resolution
        | _ -> Value resolution)
    | ComparisonOperator
        {
          ComparisonOperator.left = { Node.value = Constant Constant.NoneLiteral; _ };
          operator = ComparisonOperator.NotIn;
          right = { Node.value = Name name; _ };
        }
      when is_simple_name name -> (
        let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
        let annotation =
          Resolution.get_local_with_attributes
            ~global_fallback:false
            ~name:partitioned_name
            ~attribute_path
            resolution
        in
        match annotation with
        | Some annotation -> (
            match Annotation.annotation annotation with
            | Type.Parametric
                {
                  name = "list";
                  parameters =
                    [Single (Type.Union ([Type.NoneType; parameter] | [parameter; Type.NoneType]))];
                } ->
                let resolution =
                  refine_local ~name { annotation with Annotation.annotation = Type.list parameter }
                in
                Value resolution
            | _ -> Value resolution)
        | _ -> Value resolution)
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "all"); _ };
          arguments = [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
        }
      when is_simple_name name ->
        let resolution =
          let { name = partitioned_name; attribute_path; _ } = partition_name ~resolution name in
          match
            Resolution.get_local_with_attributes resolution ~name:partitioned_name ~attribute_path
          with
          | Some
              {
                Annotation.annotation =
                  Type.Parametric
                    { name = parametric_name; parameters = [Single (Type.Union parameters)] } as
                  annotation;
                _;
              }
            when GlobalResolution.less_or_equal
                   global_resolution
                   ~left:annotation
                   ~right:(Type.iterable (Type.Union parameters)) ->
              let parameters =
                List.filter parameters ~f:(fun parameter -> not (Type.is_none parameter))
              in
              refine_local
                ~name
                (Annotation.create_mutable
                   (Type.parametric parametric_name [Single (Type.union parameters)]))
          | _ -> resolution
        in
        Value resolution
    (* TypeGuard support *)
    | Call
        { arguments = { Call.Argument.name = None; value = { Node.value = Name name; _ } } :: _; _ }
      when is_simple_name name -> (
        let { Annotation.annotation = callee_type; _ } = resolve_expression ~resolution test in
        match Type.typeguard_annotation callee_type with
        | Some guard_type ->
            let resolution = refine_local ~name (Annotation.create_mutable guard_type) in
            Value resolution
        | None -> Value resolution)
    (* Compound assertions *)
    | WalrusOperator { target; _ } -> refine_resolution_for_assert ~resolution target
    | BooleanOperator { BooleanOperator.left; operator; right } -> (
        match operator with
        | BooleanOperator.And ->
            let left_state = refine_resolution_for_assert ~resolution left in
            let right_state =
              left_state
              |> function
              | Unreachable -> Unreachable
              | Value resolution -> refine_resolution_for_assert ~resolution right
            in
            let state =
              match left_state, right_state with
              | Unreachable, _ -> Unreachable
              | _, Unreachable -> Unreachable
              | Value left_resolution, Value right_resolution ->
                  Value (Resolution.meet_refinements left_resolution right_resolution)
            in
            state
        | BooleanOperator.Or ->
            let update resolution expression =
              refine_resolution_for_assert ~resolution expression
              |> function
              | Value post_resolution -> post_resolution
              | Unreachable -> resolution
            in
            let left_resolution = update resolution left in
            let right_resolution =
              update resolution (normalize (negate left))
              |> fun resolution -> update resolution right
            in
            Value (Resolution.outer_join_refinements left_resolution right_resolution))
    (* Everything else has no refinement *)
    | _ -> Value resolution


  and forward_assert ?(origin = Assert.Origin.Assertion) ~resolution test =
    let { Resolved.resolution; errors; _ } = forward_expression ~resolution test in
    let resolution = refine_resolution_for_assert ~resolution test in
    (* Ignore type errors from the [assert (not foo)] in the else-branch because it's the same [foo]
       as in the true-branch. This duplication of errors is normally ok because the errors get
       deduplicated in the error map and give one final error. However, it leads to two separate
       errors for [a < b] and [a >= b] (negation of <) since their error messages are different. So,
       ignore all else-branch assertion errors. *)
    let errors =
      match origin with
      | Assert.Origin.If { true_branch = false; _ }
      | Assert.Origin.While { true_branch = false; _ } ->
          []
      | _ -> errors
    in
    resolution, errors


  and forward_assignment ~resolution ~location ~target ~annotation ~value =
    let { Node.value = { Define.signature = { parent; _ }; _ } as define; _ } = Context.define in
    let global_resolution = Resolution.global_resolution resolution in
    let errors, is_final, original_annotation =
      match annotation with
      | None -> [], false, None
      | Some annotation ->
          let annotation_errors, parsed_annotation =
            parse_and_check_annotation ~resolution annotation
          in
          let final_annotation, is_final =
            match Type.final_value parsed_annotation with
            | `Ok final_annotation -> Some final_annotation, true
            | `NoParameter -> None, true
            | `NotFinal -> Some parsed_annotation, false
          in
          let unwrap_class_variable annotation =
            Type.class_variable_value annotation |> Option.value ~default:annotation
          in
          annotation_errors, is_final, Option.map final_annotation ~f:unwrap_class_variable
    in
    match Node.value target with
    | Expression.Name (Name.Identifier _)
      when delocalize target
           |> Expression.show
           |> GlobalResolution.aliases global_resolution
           |> Option.is_some ->
        (* The statement has been recognized as a type alias definition instead of an actual value
           assignment. *)
        let parsed =
          GlobalResolution.parse_annotation ~validation:NoValidation global_resolution value
        in

        (* TODO(T35601774): We need to suppress subscript related errors on generic classes. *)
        let add_annotation_errors errors =
          add_invalid_type_parameters_errors ~resolution:global_resolution ~location ~errors parsed
          |> fun (errors, _) ->
          let errors =
            List.append
              errors
              (get_untracked_annotation_errors ~resolution:global_resolution ~location parsed)
          in
          errors
        in
        let add_type_variable_errors errors =
          match parsed with
          | Variable variable when Type.Variable.Unary.contains_subvariable variable ->
              emit_error
                ~errors
                ~location
                ~kind:
                  (AnalysisError.InvalidType
                     (AnalysisError.NestedTypeVariables (Type.Variable.Unary variable)))
          | Variable { constraints = Explicit [explicit]; _ } ->
              emit_error
                ~errors
                ~location
                ~kind:(AnalysisError.InvalidType (AnalysisError.SingleExplicit explicit))
          | _ -> errors
        in
        let add_prohibited_any_errors errors =
          let reference =
            match target.value with
            | Expression.Name (Name.Identifier identifier) -> Reference.create identifier
            | _ -> failwith "not possible"
          in
          let annotation_kind =
            match parsed with
            | Variable _ -> Error.TypeVariable
            | _ -> Error.TypeAlias
          in
          if Type.expression_contains_any value && Type.contains_prohibited_any parsed then
            emit_error
              ~errors
              ~location
              ~kind:
                (Error.ProhibitedAny
                   {
                     missing_annotation =
                       {
                         Error.name = reference;
                         annotation = None;
                         given_annotation = Some parsed;
                         evidence_locations = [instantiate_path ~global_resolution target.location];
                         thrown_at_source = true;
                       };
                     annotation_kind;
                   })
          else
            errors
        in
        ( Value resolution,
          add_annotation_errors errors |> add_type_variable_errors |> add_prohibited_any_errors )
    | _ ->
        (* Processing actual value assignments. *)
        let resolution, errors, resolved_value =
          let { Resolved.resolution; errors = new_errors; resolved; _ } =
            forward_expression ~resolution value
          in
          resolution, List.append new_errors errors, resolved
        in
        let guide =
          (* This is the annotation determining how we recursively break up the assignment. *)
          match original_annotation with
          | Some annotation when not (Type.contains_unknown annotation) -> annotation
          | _ -> resolved_value
        in
        let explicit = Option.is_some annotation in
        let rec forward_assign
            ~resolution
            ~errors
            ~target:({ Node.location; value = target_value } as target)
            ~guide
            ~resolved_value
            expression
          =
          let uniform_sequence_parameter annotation =
            let unbounded_annotation =
              match annotation with
              | Type.Tuple (Concatenation concatenation) ->
                  Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
              | _ -> None
            in
            match unbounded_annotation with
            | Some annotation -> annotation
            | None -> (
                match
                  GlobalResolution.extract_type_parameters
                    global_resolution
                    ~target:"typing.Iterable"
                    ~source:annotation
                with
                | Some [element_type] -> element_type
                | _ -> Type.Any)
          in
          let nonuniform_sequence_parameters expected_size annotation =
            match annotation with
            | Type.Tuple (Concrete parameters) -> Some parameters
            | annotation when NamedTuple.is_named_tuple ~global_resolution ~annotation ->
                NamedTuple.field_annotations ~global_resolution annotation
            | annotation ->
                let parameters_from_getitem () =
                  (* Simulate __getitem__ in the fallback. *)
                  let synthetic = "$getitem_host" in
                  let resolution =
                    Resolution.new_local
                      resolution
                      ~reference:(Reference.create synthetic)
                      ~annotation:(Annotation.create_mutable annotation)
                  in
                  let getitem_type =
                    let callee =
                      let base =
                        Node.create_with_default_location
                          (Expression.Name (Name.Identifier synthetic))
                      in
                      Node.create_with_default_location
                        (Expression.Name
                           (Name.Attribute { base; attribute = "__getitem__"; special = true }))
                    in

                    Resolution.resolve_expression_to_type
                      resolution
                      (Node.create_with_default_location
                         (Expression.Call
                            {
                              callee;
                              arguments =
                                [
                                  {
                                    Call.Argument.value =
                                      Node.create_with_default_location
                                        (Expression.Constant (Constant.Integer 0));
                                    name = None;
                                  };
                                ];
                            }))
                  in
                  match getitem_type with
                  | Type.Top
                  | Type.Any ->
                      None
                  | getitem_annotation ->
                      Some (List.init ~f:(fun _ -> getitem_annotation) expected_size)
                in
                Option.first_some
                  (Type.type_parameters_for_bounded_tuple_union annotation)
                  (parameters_from_getitem ())
          in
          let is_uniform_sequence annotation =
            match annotation with
            | Type.Tuple (Concatenation concatenation)
              when Type.OrderedTypes.Concatenation.is_fully_unbounded concatenation ->
                true
            (* Bounded tuples subclass iterable, but should be handled in the nonuniform case. *)
            | Type.Tuple _ -> false
            | Type.Union (Type.Tuple _ :: _)
              when Option.is_some (Type.type_parameters_for_bounded_tuple_union annotation) ->
                false
            | _ ->
                (not (NamedTuple.is_named_tuple ~global_resolution ~annotation))
                && Option.is_some
                     (GlobalResolution.type_of_iteration_value ~global_resolution annotation)
          in
          match target_value with
          | Expression.Name name -> (
              let inner_assignment resolution errors resolved_base =
                let reference, attribute, target_annotation =
                  match resolved_base with
                  | `Identifier identifier ->
                      let reference = Reference.create identifier in

                      ( Some reference,
                        None,
                        from_reference ~location:Location.any reference
                        |> resolve_expression ~resolution )
                  | `Attribute ({ Name.Attribute.base; attribute; _ }, resolved) ->
                      let name = attribute in
                      let parent, accessed_through_class, accessed_through_readonly =
                        match Type.ReadOnly.unpack_readonly resolved, Type.is_meta resolved with
                        | Some resolved, _ -> resolved, false, true
                        | None, true -> Type.single_parameter resolved, true, false
                        | _ -> resolved, false, false
                      in
                      let parent_class_name = Type.split parent |> fst |> Type.primitive_name in
                      let reference =
                        match base with
                        | { Node.value = Name name; _ } when is_simple_name name ->
                            Some (Reference.create ~prefix:(name_to_reference_exn name) attribute)
                        | _ ->
                            parent_class_name
                            >>| Reference.create
                            >>| fun prefix -> Reference.create ~prefix attribute
                      in
                      let attribute =
                        parent_class_name
                        >>= GlobalResolution.attribute_from_class_name
                              ~resolution:global_resolution
                              ~name:attribute
                              ~instantiated:parent
                              ~transitive:true
                              ~accessed_through_class
                              ~accessed_through_readonly
                        >>| fun annotated -> annotated, attribute
                      in
                      let target_annotation =
                        match attribute with
                        | Some (attribute, _) -> AnnotatedAttribute.annotation attribute
                        | _ ->
                            (* The reason why we need to do resolve_expression on the entire target
                               again is to deal with imported globals. To fix it, we ought to stop
                               representing imported globals as `Expression.Name.Attribute`. *)
                            resolve_expression ~resolution target
                      in
                      begin
                        match attribute with
                        | Some (attribute, _)
                          when AnnotatedAttribute.property attribute
                               && AnnotatedAttribute.(
                                    [%compare.equal: visibility] (visibility attribute) ReadWrite)
                          ->
                            Context.Builder.add_property_setter_callees
                              ~attribute
                              ~instantiated_parent:parent
                              ~name
                              ~location:
                                (Location.with_module ~module_reference:Context.qualifier location)
                        | _ -> ()
                      end;
                      reference, attribute, target_annotation
                in
                let expected, is_immutable =
                  match original_annotation, target_annotation with
                  | Some original, _ when not (Type.is_type_alias original) -> original, true
                  | _, target_annotation when Annotation.is_immutable target_annotation ->
                      Annotation.original target_annotation, true
                  | _ -> Type.Top, false
                in
                let find_getattr parent =
                  let attribute =
                    match Type.class_data_for_attribute_lookup parent with
                    | Some [{ instantiated; class_name; _ }] ->
                        GlobalResolution.attribute_from_class_name
                          class_name
                          ~accessed_through_class:false
                          ~transitive:true
                          ~resolution:global_resolution
                          ~name:"__getattr__"
                          ~instantiated
                    | _ -> None
                  in
                  match attribute with
                  | Some attribute when Annotated.Attribute.defined attribute -> (
                      match Annotated.Attribute.annotation attribute |> Annotation.annotation with
                      | Type.Parametric
                          { name = "BoundMethod"; parameters = [Single (Callable _); _] }
                      | Type.Callable _ ->
                          Some attribute
                      | _ -> None)
                  | _ -> None
                in
                let check_errors ~name_reference errors resolved =
                  match reference with
                  | Some reference ->
                      let check_assignment_compatibility errors =
                        let is_valid_enumeration_assignment =
                          let parent_annotation =
                            match parent with
                            | None -> Type.Top
                            | Some reference -> Type.Primitive (Reference.show reference)
                          in
                          let compatible =
                            if explicit then
                              GlobalResolution.less_or_equal
                                global_resolution
                                ~left:expected
                                ~right:resolved
                            else
                              true
                          in
                          GlobalResolution.less_or_equal
                            global_resolution
                            ~left:parent_annotation
                            ~right:Type.enumeration
                          && compatible
                        in
                        let is_incompatible =
                          let expression_is_ellipses =
                            match expression with
                            | Some { Node.value = Expression.Constant Constant.Ellipsis; _ } -> true
                            | _ -> false
                          in
                          is_immutable
                          && (not expression_is_ellipses)
                          && (not
                                (GlobalResolution.constraints_solution_exists
                                   global_resolution
                                   ~left:resolved
                                   ~right:expected))
                          && not is_valid_enumeration_assignment
                        in
                        let open Annotated in
                        match attribute with
                        | Some (attribute, name) when is_incompatible ->
                            Error.IncompatibleAttributeType
                              {
                                parent = Primitive (Attribute.parent attribute);
                                incompatible_type =
                                  {
                                    Error.name = Reference.create name;
                                    mismatch =
                                      Error.create_mismatch
                                        ~resolution:global_resolution
                                        ~actual:resolved
                                        ~expected
                                        ~covariant:true;
                                  };
                              }
                            |> fun kind -> emit_error ~errors ~location ~kind
                        | None when is_incompatible ->
                            incompatible_variable_type_error_kind
                              ~global_resolution
                              ~declare_location:(instantiate_path ~global_resolution location)
                              {
                                Error.name = reference;
                                mismatch =
                                  Error.create_mismatch
                                    ~resolution:global_resolution
                                    ~actual:resolved
                                    ~expected
                                    ~covariant:true;
                              }
                            |> fun kind -> emit_error ~errors ~location ~kind
                        | _ -> errors
                      in
                      let check_assign_class_variable_on_instance errors =
                        match
                          ( resolved_base,
                            attribute >>| fst >>| Annotated.Attribute.class_variable,
                            attribute >>| fst >>| Annotated.Attribute.name )
                        with
                        | `Attribute (_, parent), Some true, Some class_variable
                          when Option.is_none original_annotation && not (Type.is_meta parent) ->
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.InvalidAssignment
                                   (ClassVariable { class_name = Type.show parent; class_variable }))
                        | _ -> errors
                      in
                      let check_final_is_outermost_qualifier errors =
                        original_annotation
                        >>| (fun annotation ->
                              if Type.contains_final annotation then
                                emit_error
                                  ~errors
                                  ~location
                                  ~kind:(Error.InvalidType (FinalNested annotation))
                              else
                                errors)
                        |> Option.value ~default:errors
                      in
                      let check_undefined_attribute_target errors =
                        match resolved_base, attribute with
                        | `Attribute (_, parent), Some (attribute, _)
                          when not (Annotated.Attribute.defined attribute) ->
                            let is_meta_typed_dictionary =
                              Type.is_meta parent
                              && GlobalResolution.is_typed_dictionary
                                   ~resolution:global_resolution
                                   (Type.single_parameter parent)
                            in
                            let is_getattr_returning_any_defined =
                              match
                                find_getattr parent
                                >>| AnnotatedAttribute.annotation
                                >>| Annotation.annotation
                              with
                              | Some
                                  (Type.Parametric
                                    {
                                      name = "BoundMethod";
                                      parameters =
                                        [
                                          Single (Callable { implementation = { annotation; _ }; _ });
                                          _;
                                        ];
                                    })
                              | Some (Type.Callable { implementation = { annotation; _ }; _ }) ->
                                  Type.is_any annotation
                              | _ -> false
                            in
                            if is_meta_typed_dictionary || is_getattr_returning_any_defined then
                              (* Ignore the error from the attribute declaration `Movie.name = ...`,
                                 which would raise an error because `name` was removed as an
                                 attribute from the TypedDictionary. *)
                              errors
                            else
                              (* TODO(T64156088): To catch errors against the implicit call to a
                                 custom definition of `__setattr__`, we should run signature select
                                 against the value type. *)
                              let parent_module_path =
                                let module_tracker =
                                  GlobalResolution.module_tracker global_resolution
                                in
                                GlobalResolution.class_summary global_resolution parent
                                >>| Node.value
                                >>= fun { ClassSummary.qualifier; _ } ->
                                ModuleTracker.ReadOnly.lookup_module_path module_tracker qualifier
                              in
                              emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.UndefinedAttribute
                                     {
                                       attribute = AnnotatedAttribute.public_name attribute;
                                       origin =
                                         Error.Class
                                           { class_origin = ClassType parent; parent_module_path };
                                     })
                        | _ -> errors
                      in
                      let check_nested_explicit_type_alias errors =
                        match name, original_annotation with
                        | Name.Identifier identifier, Some annotation
                          when Type.is_type_alias annotation && not (Define.is_toplevel define) ->
                            emit_error
                              ~errors
                              ~location
                              ~kind:(Error.InvalidType (NestedAlias identifier))
                        | _ -> errors
                      in
                      let check_enumeration_literal errors =
                        original_annotation
                        >>| emit_invalid_enumeration_literal_errors ~resolution ~location ~errors
                        |> Option.value ~default:errors
                      in
                      let check_previously_annotated errors =
                        match name with
                        | Name.Identifier identifier ->
                            let is_defined =
                              Option.is_some
                                (Resolution.get_local
                                   ~global_fallback:true
                                   ~reference:(Reference.create identifier)
                                   resolution)
                            in
                            let is_reannotation_with_same_type =
                              (* TODO(T77219514): special casing for re-annotation in loops can be
                                 removed when fixpoint is gone *)
                              Annotation.is_immutable target_annotation
                              && Type.equal expected (Annotation.original target_annotation)
                            in
                            if
                              explicit
                              && (not (Define.is_toplevel define))
                              && is_defined
                              && not is_reannotation_with_same_type
                            then
                              emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.IllegalAnnotationTarget { target; kind = Reassignment })
                            else
                              errors
                        | _ -> errors
                      in
                      let check_assignment_to_readonly_type errors =
                        let is_readonly_attribute =
                          target_annotation |> Annotation.annotation |> Type.ReadOnly.is_readonly
                        in
                        match attribute, resolved_base with
                        | Some (_, attribute_name), `Attribute (_, resolved_base_type)
                          when is_readonly_attribute
                               && Type.ReadOnly.is_readonly resolved_base_type
                               && not (Define.is_class_toplevel define) ->
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.ReadOnlynessMismatch
                                   (AssigningToReadOnlyAttribute { attribute_name }))
                        | _ -> errors
                      in
                      let errors =
                        let modifying_read_only_error =
                          let is_locally_initialized =
                            name_reference
                            >>| (fun reference ->
                                  Resolution.has_nontemporary_annotation ~reference resolution)
                            |> Option.value ~default:false
                          in
                          match attribute, original_annotation with
                          | None, _ when is_locally_initialized || not explicit ->
                              Option.some_if
                                (Annotation.is_final target_annotation)
                                (AnalysisError.FinalAttribute reference)
                          | None, _ -> None
                          | Some _, Some _ ->
                              (* We presume assignments to annotated targets are valid re:
                                 Finality *)
                              None
                          | Some (attribute, _), None -> (
                              let open AnnotatedAttribute in
                              match
                                visibility attribute, property attribute, initialized attribute
                              with
                              | ReadOnly _, false, OnlyOnInstance when Define.is_constructor define
                                ->
                                  None
                              | ReadOnly _, false, OnClass when Define.is_class_toplevel define ->
                                  None
                              | ReadOnly _, false, _ ->
                                  Some (AnalysisError.FinalAttribute reference)
                              | ReadOnly _, true, _ -> Some (ReadOnly reference)
                              | _ -> None)
                        in
                        match modifying_read_only_error with
                        | Some error ->
                            emit_error ~errors ~location ~kind:(Error.InvalidAssignment error)
                        | None ->
                            (* Check compatibility only when we're not already erroring about Final
                               reassignment. *)
                            check_assignment_compatibility errors
                      in
                      check_assign_class_variable_on_instance errors
                      |> check_final_is_outermost_qualifier
                      |> check_undefined_attribute_target
                      |> check_nested_explicit_type_alias
                      |> check_enumeration_literal
                      |> check_previously_annotated
                      |> check_assignment_to_readonly_type
                  | _ -> errors
                in
                let check_for_missing_annotations errors resolved =
                  let insufficiently_annotated, thrown_at_source =
                    let is_reassignment =
                      (* Special-casing re-use of typed parameters as attributes *)
                      match name, Node.value value with
                      | ( Name.Attribute
                            { base = { Node.value = Name (Name.Identifier self); _ }; attribute; _ },
                          Name _ )
                        when String.equal (Identifier.sanitized self) "self" ->
                          let sanitized =
                            Ast.Transform.sanitize_expression value |> Expression.show
                          in
                          is_immutable
                          && (not (Type.contains_unknown expected))
                          && (String.equal attribute sanitized
                             || String.equal attribute ("_" ^ sanitized))
                      | _ -> false
                    in
                    match annotation with
                    | Some annotation when Type.expression_contains_any annotation ->
                        original_annotation
                        >>| Type.contains_prohibited_any
                        |> Option.value ~default:false
                        |> fun insufficient -> insufficient, true
                    | None when is_immutable && not is_reassignment ->
                        let thrown_at_source =
                          match define, attribute with
                          | _, None -> Define.is_toplevel define
                          | ( { StatementDefine.signature = { parent = Some parent; _ }; _ },
                              Some (attribute, _) ) ->
                              Type.Primitive.equal
                                (Reference.show parent)
                                (AnnotatedAttribute.parent attribute)
                              && (Define.is_class_toplevel define || Define.is_constructor define)
                          | _ -> false
                        in
                        ( Type.equal expected Type.Top || Type.contains_prohibited_any expected,
                          thrown_at_source )
                    | _ -> false, false
                  in
                  let actual_annotation, evidence_locations =
                    if Type.equal resolved Type.Top then
                      None, []
                    else
                      Some resolved, [instantiate_path ~global_resolution location]
                  in
                  let is_illegal_attribute_annotation attribute =
                    let attribute_parent = AnnotatedAttribute.parent attribute in
                    let parent_annotation =
                      match parent with
                      | None -> Type.Top
                      | Some reference -> Type.Primitive (Reference.show reference)
                    in
                    explicit
                    (* [Movie.items: int] would raise an error because [Mapping] also has
                       [items]. *)
                    && (not
                          (GlobalResolution.is_typed_dictionary
                             ~resolution:global_resolution
                             parent_annotation))
                    && not (Type.equal parent_annotation (Primitive attribute_parent))
                  in
                  let parent_class =
                    match resolved_base with
                    | `Attribute (_, base_type) -> Type.class_data_for_attribute_lookup base_type
                    | _ -> None
                  in
                  match name, parent_class with
                  | Name.Identifier identifier, _ ->
                      let reference = Reference.create identifier in
                      if Resolution.is_global ~reference resolution && insufficiently_annotated then
                        let global_location =
                          Reference.delocalize reference
                          |> GlobalResolution.global_location global_resolution
                          >>| Location.strip_module
                          |> Option.value ~default:location
                        in
                        ( emit_error
                            ~errors
                            ~location:global_location
                            ~kind:
                              (Error.MissingGlobalAnnotation
                                 {
                                   Error.name = reference;
                                   annotation = actual_annotation;
                                   given_annotation = Option.some_if is_immutable expected;
                                   evidence_locations;
                                   thrown_at_source;
                                 }),
                          true )
                      else if explicit && insufficiently_annotated then
                        ( emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.ProhibitedAny
                                 {
                                   missing_annotation =
                                     {
                                       Error.name = reference;
                                       annotation = actual_annotation;
                                       given_annotation = Option.some_if is_immutable expected;
                                       evidence_locations;
                                       thrown_at_source = true;
                                     };
                                   annotation_kind = Annotation;
                                 }),
                          true )
                      else
                        errors, true
                  | Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ }, None
                    when is_simple_name base && insufficiently_annotated ->
                      (* Module *)
                      let reference = name_to_reference_exn base in
                      if
                        explicit && not (GlobalResolution.module_exists global_resolution reference)
                      then
                        ( emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.ProhibitedAny
                                 {
                                   missing_annotation =
                                     {
                                       Error.name = Reference.create ~prefix:reference attribute;
                                       annotation = actual_annotation;
                                       given_annotation = Option.some_if is_immutable expected;
                                       evidence_locations;
                                       thrown_at_source = true;
                                     };
                                   annotation_kind = Annotation;
                                 }),
                          true )
                      else
                        errors, true
                  | ( Name.Attribute { attribute; _ },
                      Some ({ Type.instantiated; accessed_through_class; class_name; _ } :: _) )
                    -> (
                      (* Instance *)
                      let reference = Reference.create attribute in
                      let attribute =
                        GlobalResolution.attribute_from_class_name
                          ~resolution:global_resolution
                          ~name:attribute
                          ~instantiated
                          ~accessed_through_class
                          ~transitive:true
                          class_name
                      in
                      match attribute with
                      | Some attribute -> (
                          if is_illegal_attribute_annotation attribute then
                            (* Non-self attributes may not be annotated. *)
                            ( emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.IllegalAnnotationTarget
                                     { target; kind = InvalidExpression }),
                              false )
                          else if
                            Annotated.Attribute.defined attribute
                            && (not (Annotated.Attribute.property attribute))
                            && insufficiently_annotated
                          then
                            ( emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.MissingAttributeAnnotation
                                     {
                                       parent = Primitive (Annotated.Attribute.parent attribute);
                                       missing_annotation =
                                         {
                                           Error.name = reference;
                                           annotation = actual_annotation;
                                           given_annotation = Option.some_if is_immutable expected;
                                           evidence_locations;
                                           thrown_at_source;
                                         };
                                     }),
                              true )
                          else if insufficiently_annotated && explicit then
                            ( emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.ProhibitedAny
                                     {
                                       missing_annotation =
                                         {
                                           Error.name = reference;
                                           annotation = actual_annotation;
                                           given_annotation = Option.some_if is_immutable expected;
                                           evidence_locations;
                                           thrown_at_source = true;
                                         };
                                       annotation_kind = Annotation;
                                     }),
                              true )
                          else
                            match
                              incompatible_annotation_with_attribute_error
                                ~define
                                ~explicit
                                ~original_annotation
                                attribute
                            with
                            | Some inconsistent_constructor_annotation ->
                                ( emit_error
                                    ~errors
                                    ~location
                                    ~kind:
                                      (Error.IllegalAnnotationTarget
                                         { target; kind = inconsistent_constructor_annotation }),
                                  false )
                            | None -> errors, true)
                      | None ->
                          if
                            insufficiently_annotated
                            && GlobalResolution.is_typed_dictionary
                                 ~resolution:global_resolution
                                 (Type.Primitive class_name)
                          then
                            ( emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.ProhibitedAny
                                     {
                                       missing_annotation =
                                         {
                                           Error.name = reference;
                                           annotation = actual_annotation;
                                           given_annotation = Option.some_if is_immutable expected;
                                           evidence_locations;
                                           thrown_at_source = true;
                                         };
                                       annotation_kind = Annotation;
                                     }),
                              true )
                          else
                            errors, true)
                  | _ ->
                      if explicit then
                        ( emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.IllegalAnnotationTarget { target; kind = InvalidExpression }),
                          false )
                      else
                        errors, true
                in
                let propagate_annotations
                    ~errors
                    ~is_valid_annotation
                    ~resolved_value_weakened
                    name_reference
                  =
                  let is_global =
                    name_reference
                    >>| (fun reference -> Resolution.is_global resolution ~reference)
                    |> Option.value ~default:false
                  in
                  let is_not_local = is_global && not (Define.is_toplevel Context.define.value) in
                  let refine_annotation annotation refined =
                    GlobalResolution.refine ~global_resolution annotation refined
                  in
                  let annotation =
                    (* Do not refine targets explicitly annotated as 'Any' to allow for escape
                       hatch *)
                    (* Do not refine targets with invariance mismatch as we cannot keep the inferred
                       type up to date for mutable containers *)
                    let invariance_mismatch =
                      GlobalResolution.is_invariance_mismatch
                        global_resolution
                        ~right:expected
                        ~left:resolved_value
                    in
                    if explicit && is_valid_annotation then
                      let guide_annotation = Annotation.create_immutable ~final:is_final guide in
                      if
                        Type.is_concrete resolved_value
                        && (not (Type.is_any guide))
                        && not invariance_mismatch
                      then
                        refine_annotation guide_annotation resolved_value
                      else
                        guide_annotation
                    else if is_immutable then
                      if Type.is_any (Annotation.original target_annotation) || invariance_mismatch
                      then
                        target_annotation
                      else
                        refine_annotation target_annotation guide
                    else
                      Annotation.create_mutable guide
                  in
                  let errors, annotation =
                    if
                      (not explicit)
                      && Type.Variable.contains_escaped_free_variable
                           (Annotation.annotation annotation)
                    then
                      let kind =
                        Error.IncompleteType
                          {
                            target = { Node.location; value = target_value };
                            annotation = resolved_value_weakened;
                            attempted_action = Naming;
                          }
                      in
                      let converted =
                        Type.Variable.convert_all_escaped_free_variables_to_anys
                          (Annotation.annotation annotation)
                      in
                      emit_error ~errors ~location ~kind, { annotation with annotation = converted }
                    else
                      errors, annotation
                  in
                  let resolution =
                    match name with
                    | Identifier identifier ->
                        Resolution.new_local
                          resolution
                          ~temporary:is_not_local
                          ~reference:(Reference.create identifier)
                          ~annotation
                    | Attribute _ as name when is_simple_name name -> (
                        match resolved_base, attribute with
                        | `Attribute (_, parent), Some (attribute, _)
                          when not
                                 (Annotated.Attribute.property attribute
                                 || Option.is_some (find_getattr parent)) ->
                            let { name; attribute_path; base_annotation } =
                              partition_name ~resolution name
                            in
                            Resolution.new_local_with_attributes
                              ~temporary:(is_not_local || Annotated.Attribute.defined attribute)
                              resolution
                              ~name
                              ~attribute_path
                              ~base_annotation
                              ~annotation
                        | _ -> resolution)
                    | _ -> resolution
                  in
                  resolution, errors
                in
                let resolved_value_weakened =
                  GlobalResolution.resolve_mutable_literals
                    global_resolution
                    ~resolve:(resolve_expression_type ~resolution)
                    ~expression
                    ~resolved:resolved_value
                    ~expected
                in
                let name_reference =
                  match name with
                  | Identifier identifier -> Reference.create identifier |> Option.some
                  | Attribute _ as name when is_simple_name name ->
                      name_to_reference_exn name |> Option.some
                  | _ -> None
                in
                match resolved_value_weakened with
                | { resolved = resolved_value_weakened; typed_dictionary_errors = [] } ->
                    let errors = check_errors ~name_reference errors resolved_value_weakened in
                    let errors, is_valid_annotation =
                      check_for_missing_annotations errors resolved_value_weakened
                    in
                    propagate_annotations
                      ~errors
                      ~is_valid_annotation
                      ~resolved_value_weakened
                      name_reference
                | { typed_dictionary_errors; _ } ->
                    propagate_annotations
                      ~errors:(emit_typed_dictionary_errors ~errors typed_dictionary_errors)
                      ~is_valid_annotation:false
                      ~resolved_value_weakened:Type.Top
                      name_reference
              in
              let resolved_base =
                match name with
                | Name.Identifier identifier -> `Identifier identifier
                | Name.Attribute attribute ->
                    let resolved = resolve_expression_type ~resolution attribute.base in
                    `Attribute (attribute, resolved)
              in
              match resolved_base with
              | `Attribute (attribute, Type.Union types) ->
                  (* Union[A,B].attr is valid iff A.attr and B.attr is valid

                     TODO(T130377746): Use `Type.class_data_for_attribute_lookup` here to avoid
                     duplicating the logic of how to figure out the attribute type for various
                     types. Right now, we're duplicating some of the logic (for unions) but missing
                     others. We're also hackily extracting `accessed_through_class` later on by
                     checking if the top-level type is `Type[...]` instead of doing it for all
                     possible elements of a union, etc. *)
                  let propagate (resolution, errors) t =
                    inner_assignment resolution errors (`Attribute (attribute, t))
                  in
                  let _, errors = List.fold types ~init:(resolution, errors) ~f:propagate in
                  (* We process type as union again to populate resolution *)
                  propagate (resolution, errors) (Union types)
              | resolved -> inner_assignment resolution errors resolved)
          | List elements
          | Tuple elements
            when is_uniform_sequence guide ->
              let propagate (resolution, errors) element =
                match Node.value element with
                | Expression.Starred (Starred.Once target) ->
                    let guide = uniform_sequence_parameter guide |> Type.list in
                    let resolved_value = uniform_sequence_parameter resolved_value |> Type.list in
                    forward_assign ~resolution ~errors ~target ~guide ~resolved_value None
                | _ ->
                    let guide = uniform_sequence_parameter guide in
                    let resolved_value = uniform_sequence_parameter resolved_value in
                    forward_assign ~resolution ~errors ~target:element ~guide ~resolved_value None
              in
              List.fold elements ~init:(resolution, errors) ~f:propagate
          | List elements
          | Tuple elements ->
              let left, starred, right =
                let is_starred { Node.value; _ } =
                  match value with
                  | Expression.Starred (Starred.Once _) -> true
                  | _ -> false
                in
                let left, tail =
                  List.split_while elements ~f:(fun element -> not (is_starred element))
                in
                let starred, right =
                  let starred, right = List.split_while tail ~f:is_starred in
                  let starred =
                    match starred with
                    | [{ Node.value = Starred (Starred.Once starred); _ }] -> [starred]
                    | _ -> []
                  in
                  starred, right
                in
                left, starred, right
              in
              let assignees = left @ starred @ right in
              let errors, annotations =
                match guide with
                | Type.Any -> errors, List.map assignees ~f:(fun _ -> Type.Any)
                | Type.Top -> errors, List.map assignees ~f:(fun _ -> Type.Any)
                | _ -> (
                    match nonuniform_sequence_parameters (List.length assignees) guide with
                    | None ->
                        let errors =
                          emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.Unpack
                                 {
                                   expected_count = List.length assignees;
                                   unpack_problem = UnacceptableType guide;
                                 })
                        in
                        errors, List.map assignees ~f:(fun _ -> Type.Any)
                    | Some annotations ->
                        let annotations =
                          let has_starred_assignee = not (List.is_empty starred) in
                          let left, tail = List.split_n annotations (List.length left) in
                          let starred, right =
                            List.split_n tail (List.length tail - List.length right)
                          in
                          let starred =
                            if not (List.is_empty starred) then
                              let annotation =
                                List.fold
                                  starred
                                  ~init:Type.Bottom
                                  ~f:(GlobalResolution.join global_resolution)
                                |> Type.list
                              in
                              [annotation]
                            else if has_starred_assignee then
                              [Type.tuple []]
                            else
                              []
                          in
                          left @ starred @ right
                        in
                        if List.length annotations <> List.length assignees then
                          let errors =
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.Unpack
                                   {
                                     expected_count = List.length assignees;
                                     unpack_problem = CountMismatch (List.length annotations);
                                   })
                          in
                          errors, List.map assignees ~f:(fun _ -> Type.Any)
                        else
                          errors, annotations)
              in
              List.zip_exn assignees annotations
              |> List.fold
                   ~init:(resolution, errors)
                   ~f:(fun (resolution, errors) (target, guide) ->
                     forward_assign ~resolution ~errors ~target ~guide ~resolved_value:guide None)
          | _ ->
              if Option.is_some annotation then
                ( resolution,
                  emit_error
                    ~errors
                    ~location
                    ~kind:(Error.IllegalAnnotationTarget { target; kind = InvalidExpression }) )
              else
                resolution, errors
        in
        let resolution, errors =
          forward_assign ~resolution ~errors ~target ~guide ~resolved_value (Some value)
        in
        Value resolution, errors


  and resolve_expression ~resolution expression =
    forward_expression ~resolution expression
    |> fun { Resolved.resolved; resolved_annotation; _ } ->
    resolved_annotation |> Option.value ~default:(Annotation.create_mutable resolved)


  and resolve_expression_type ~resolution expression =
    resolve_expression ~resolution expression |> Annotation.annotation


  and resolve_expression_type_with_locals ~resolution ~locals expression =
    let new_local resolution (reference, annotation) =
      Resolution.new_local resolution ~reference ~annotation
    in
    let resolution_with_locals = List.fold ~init:resolution ~f:new_local locals in
    resolve_expression ~resolution:resolution_with_locals expression |> Annotation.annotation


  and resolve_reference_type ~resolution reference =
    from_reference ~location:Location.any reference |> resolve_expression_type ~resolution


  and emit_invalid_enumeration_literal_errors ~resolution ~location ~errors annotation =
    let invalid_enumeration_literals =
      let is_invalid_enumeration_member = function
        | Type.Literal (Type.EnumerationMember { enumeration_type; member_name }) ->
            let global_resolution = Resolution.global_resolution resolution in
            let is_enumeration =
              GlobalResolution.class_exists global_resolution (Type.show enumeration_type)
              && GlobalResolution.less_or_equal
                   global_resolution
                   ~left:enumeration_type
                   ~right:Type.enumeration
            in
            let is_member_of_enumeration =
              let literal_expression =
                Node.create
                  ~location
                  (Expression.Name
                     (Attribute
                        {
                          base = Type.expression enumeration_type;
                          attribute = member_name;
                          special = false;
                        }))
              in
              let { Resolved.resolved = resolved_member_type; _ } =
                forward_expression ~resolution literal_expression
              in
              GlobalResolution.less_or_equal
                global_resolution
                ~left:resolved_member_type
                ~right:enumeration_type
            in
            not (is_enumeration && is_member_of_enumeration)
        | _ -> false
      in
      Type.collect annotation ~predicate:is_invalid_enumeration_member
    in
    List.fold invalid_enumeration_literals ~init:errors ~f:(fun errors annotation ->
        emit_error
          ~errors
          ~location
          ~kind:(Error.InvalidType (InvalidType { annotation; expected = "an Enum member" })))


  let forward_statement ~resolution ~statement:{ Node.location; value } =
    let global_resolution = Resolution.global_resolution resolution in
    let validate_return = validate_return ~location in
    match value with
    | Statement.Assign { Assign.target; annotation; value } ->
        forward_assignment ~resolution ~location ~target ~annotation ~value
    | Assert { Assert.test; origin; message } ->
        let message_errors =
          Option.value
            ~default:[]
            (message >>| forward_expression ~resolution >>| fun { Resolved.errors; _ } -> errors)
        in
        let resolution, errors = forward_assert ~resolution ~origin test in
        resolution, message_errors @ errors
    | Delete expressions ->
        let process_expression (resolution, errors_sofar) expression =
          let { Resolved.resolution; errors; _ } = forward_expression ~resolution expression in
          let resolution =
            match Node.value expression with
            | Name (Identifier identifier) ->
                Resolution.unset_local resolution ~reference:(Reference.create identifier)
            | _ -> resolution
          in
          resolution, List.append errors errors_sofar
        in
        let resolution, errors =
          List.fold expressions ~init:(resolution, []) ~f:process_expression
        in
        Value resolution, errors
    | Expression
        { Node.value = Call { callee; arguments = { Call.Argument.value = test; _ } :: _ }; _ }
      when Core.Set.mem Recognized.assert_functions (Expression.show callee) ->
        forward_assert ~resolution test
    | Expression expression ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution expression
        in
        if Type.is_noreturn_or_never resolved then
          Unreachable, errors
        else
          Value resolution, errors
    | Raise { Raise.expression = Some expression; _ } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution expression
        in
        let expected = Type.Primitive "BaseException" in
        let actual =
          if Type.is_meta resolved then
            Type.single_parameter resolved
          else
            resolved
        in
        let errors =
          if GlobalResolution.less_or_equal global_resolution ~left:actual ~right:expected then
            errors
          else
            emit_error
              ~errors
              ~location
              ~kind:(Error.InvalidException { expression; annotation = resolved })
        in
        Value resolution, errors
    | Raise _ -> Value resolution, []
    | Return { Return.expression; is_implicit } ->
        let { Resolved.resolution; resolved = return_type; errors; _ } =
          Option.value_map
            expression
            ~default:
              {
                Resolved.resolution;
                errors = [];
                resolved = Type.none;
                resolved_annotation = None;
                base = None;
              }
            ~f:(fun expression -> forward_expression ~resolution expression)
        in
        let actual =
          if define_signature.generator && not define_signature.async then
            Type.generator ~return_type ()
          else
            return_type
        in
        Value resolution, validate_return expression ~resolution ~errors ~actual ~is_implicit
    | Define { signature = { Define.Signature.name; _ } as signature; _ } ->
        let resolution =
          if Reference.is_local name then
            type_of_signature ~resolution signature
            |> Type.Variable.mark_all_variables_as_bound
                 ~specific:(Resolution.all_type_variables_in_scope resolution)
            |> Annotation.create_mutable
            |> fun annotation -> Resolution.new_local resolution ~reference:name ~annotation
          else
            resolution
        in
        Value resolution, []
    | Import { Import.from; imports } ->
        let undefined_imports =
          match from with
          | None ->
              List.filter_map imports ~f:(fun { Node.value = { Import.name; _ }; _ } ->
                  match GlobalResolution.module_exists global_resolution name with
                  | true -> None
                  | false -> (
                      match GlobalResolution.is_suppressed_module global_resolution name with
                      | true -> None
                      | false -> Some (Error.UndefinedModule name)))
          | Some { Node.value = from; _ } -> (
              match GlobalResolution.get_module_metadata global_resolution from with
              | None ->
                  if GlobalResolution.is_suppressed_module global_resolution from then
                    []
                  else
                    [Error.UndefinedModule from]
              | Some module_metadata ->
                  let module_tracker = GlobalResolution.module_tracker global_resolution in
                  List.filter_map
                    imports
                    ~f:(fun { Node.value = { Import.name = name_reference; _ }; _ } ->
                      let name = Reference.show name_reference in
                      match Module.get_export module_metadata name with
                      | Some _ ->
                          (* `name` is defined inside the module. *)
                          None
                      | None -> (
                          match Module.get_export module_metadata "__getattr__" with
                          | Some Module.Export.(Name (Define { is_getattr_any = true })) ->
                              (* The current module has `__getattr__: str -> Any` defined. *)
                              None
                          | _ ->
                              if
                                (* `name` is a submodule of the current package. *)
                                GlobalResolution.module_exists
                                  global_resolution
                                  (Reference.combine from name_reference)
                                || (* The current module is descendant of a placeholder-stub
                                      module. *)
                                GlobalResolution.is_suppressed_module global_resolution from
                              then
                                None
                              else
                                let origin_module =
                                  match
                                    ModuleTracker.ReadOnly.lookup_module_path module_tracker from
                                  with
                                  | Some source_path -> Error.ExplicitModule source_path
                                  | None -> Error.ImplicitModule from
                                in
                                Some (Error.UndefinedName { from = origin_module; name }))))
        in
        ( Value resolution,
          List.fold undefined_imports ~init:[] ~f:(fun errors undefined_import ->
              emit_error ~errors ~location ~kind:(Error.UndefinedImport undefined_import)) )
    | Class class_statement ->
        (* Check that variance isn't widened on inheritence. Don't check for other errors. Nested
           classes and functions are analyzed separately. *)
        let check_base errors base =
          let check_pair errors extended actual =
            match extended, actual with
            | ( Type.Variable { Type.Record.Variable.RecordUnary.variance = left; _ },
                Type.Variable { Type.Record.Variable.RecordUnary.variance = right; _ } ) -> (
                match left, right with
                | Type.Variable.Covariant, Type.Variable.Invariant
                | Type.Variable.Contravariant, Type.Variable.Invariant
                | Type.Variable.Covariant, Type.Variable.Contravariant
                | Type.Variable.Contravariant, Type.Variable.Covariant ->
                    emit_error
                      ~errors
                      ~location
                      ~kind:
                        (Error.InvalidTypeVariance
                           { annotation = extended; origin = Error.Inheritance actual })
                | _ -> errors)
            | _, _ -> errors
          in
          let check_duplicate_typevars errors parameters base =
            let rec get_duplicate_typevars parameters duplicates =
              match parameters with
              | parameter :: rest when List.exists ~f:(Type.Variable.equal parameter) rest ->
                  get_duplicate_typevars rest (parameter :: duplicates)
              | _ -> duplicates
            in
            let emit_duplicate_errors errors variable =
              emit_error ~errors ~location ~kind:(Error.DuplicateTypeVariables { variable; base })
            in
            List.fold_left
              ~f:emit_duplicate_errors
              ~init:errors
              (get_duplicate_typevars (Type.Variable.all_free_variables parameters) [])
          in
          match GlobalResolution.parse_annotation global_resolution base with
          | Type.Parametric { name; _ } as parametric when String.equal name "typing.Generic" ->
              check_duplicate_typevars errors parametric GenericBase
          | Type.Parametric { name; parameters = extended_parameters } as parametric ->
              let errors =
                if String.equal name "typing.Protocol" then
                  check_duplicate_typevars errors parametric ProtocolBase
                else
                  errors
              in
              Type.Parameter.all_singles extended_parameters
              >>| (fun extended_parameters ->
                    let actual_parameters =
                      GlobalResolution.variables global_resolution name
                      >>= Type.Variable.all_unary
                      >>| List.map ~f:(fun unary -> Type.Variable unary)
                      |> Option.value ~default:[]
                    in
                    match
                      List.fold2 extended_parameters actual_parameters ~init:errors ~f:check_pair
                    with
                    | Ok errors -> errors
                    | Unequal_lengths -> errors)
              |> Option.value ~default:errors
          | _ -> errors
        in
        Value resolution, List.fold (Class.base_classes class_statement) ~f:check_base ~init:[]
    | For _
    | If _
    | Match _
    | Try _
    | With _
    | While _ ->
        (* Check happens implicitly in the resulting control flow. *)
        Value resolution, []
    | Break
    | Continue
    | Global _
    | Nonlocal _
    | Pass ->
        Value resolution, []


  let initial ~resolution =
    let global_resolution = Resolution.global_resolution resolution in
    let {
      Node.location;
      value =
        {
          Define.signature =
            {
              name;
              parent;
              parameters;
              return_annotation;
              decorators;
              async;
              nesting_define;
              generator;
              _;
            } as signature;
          captures;
          unbound_names;
          _;
        } as define;
    }
      =
      Context.define
    in
    let check_decorators resolution errors =
      let check_final_decorator errors =
        if Option.is_none parent && Define.is_final_method define then
          emit_error
            ~errors
            ~location
            ~kind:(Error.InvalidInheritance (NonMethodFunction "typing.final"))
        else
          errors
      in
      let check_override_decorator errors =
        if StatementDefine.is_override_method define then
          match define with
          | { Ast.Statement.Define.signature = { parent = Some parent; _ }; _ } -> (
              let possibly_overridden_attribute =
                GlobalResolution.overrides
                  (Reference.show parent)
                  ~resolution:global_resolution
                  ~name:(StatementDefine.unqualified_name define)
              in
              match possibly_overridden_attribute with
              | Some _ -> errors
              | None ->
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.InvalidOverride
                         { parent = Reference.show parent; decorator = NothingOverridden }))
          | { Ast.Statement.Define.signature = { parent = None; _ }; _ } ->
              emit_error
                ~errors
                ~location
                ~kind:(Error.InvalidOverride { parent = ""; decorator = IllegalOverrideDecorator })
        else
          errors
      in
      let check_decorator errors decorator =
        let get_allowlisted decorator =
          match Decorator.from_expression decorator with
          | None -> None
          | Some decorator ->
              let has_suffix
                  { Ast.Statement.Decorator.name = { Node.value = name; _ }; arguments }
                  suffix
                =
                Option.is_none arguments && String.equal (Reference.last name) suffix
              in
              let is_property_derivative decorator =
                has_suffix decorator "setter"
                || has_suffix decorator "getter"
                || has_suffix decorator "deleter"
              in
              let is_attr_validator decorator = has_suffix decorator "validator" in
              let is_click_derivative decorator = has_suffix decorator "command" in
              (* TODO (T41383196): Properly deal with @property and @click *)
              Option.some_if
                (is_property_derivative decorator
                || is_click_derivative decorator
                || is_attr_validator decorator)
                decorator
        in
        match get_allowlisted decorator with
        | Some { Ast.Statement.Decorator.name = { Node.value = decorator_name; _ }; _ } -> (
            match Reference.as_list decorator_name |> List.rev with
            | "setter" :: decorated_property_name :: _ ->
                if String.equal (Reference.last name) decorated_property_name then
                  errors
                else
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.InvalidDecoration
                         (Error.SetterNameMismatch
                            {
                              name = decorator_name;
                              actual = decorated_property_name;
                              expected = Reference.last name;
                            }))
            | _ -> errors)
        | None ->
            let { Resolved.errors = decorator_errors; _ } =
              forward_expression ~resolution decorator
            in
            List.append decorator_errors errors
      in
      List.fold decorators ~init:errors ~f:check_decorator
      |> check_final_decorator
      |> check_override_decorator
    in
    let check_unbound_names errors =
      let add_unbound_name_error errors { Define.NameAccess.name; location } =
        match GlobalResolution.get_module_metadata global_resolution Reference.empty with
        | Some module_metadata when Option.is_some (Module.get_export module_metadata name) ->
            (* Do not error on names defined in empty qualifier space, e.g. custom builtins. *)
            errors
        | _ -> emit_error ~errors ~location ~kind:(AnalysisError.UnboundName name)
      in
      List.fold unbound_names ~init:errors ~f:add_unbound_name_error
    in
    let check_return_annotation resolution errors =
      let add_missing_return_error ~errors annotation =
        let return_annotation =
          let annotation =
            let parser = GlobalResolution.annotation_parser global_resolution in
            Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
          in
          if async && not generator then
            Type.coroutine_value annotation |> Option.value ~default:Type.Top
          else
            annotation
        in
        let return_annotation = Type.Variable.mark_all_variables_as_bound return_annotation in
        let contains_literal_any =
          Type.contains_prohibited_any return_annotation
          && annotation >>| Type.expression_contains_any |> Option.value ~default:false
        in
        if
          ((not (Define.is_toplevel define)) && not (Define.is_class_toplevel define))
          && not (Option.is_some annotation)
          || contains_literal_any
        then
          emit_error
            ~errors
            ~location
            ~kind:
              (Error.MissingReturnAnnotation
                 {
                   name = Reference.create "$return_annotation";
                   annotation = None;
                   given_annotation =
                     Option.some_if (Define.has_return_annotation define) return_annotation;
                   evidence_locations = [];
                   thrown_at_source = true;
                 })
        else
          errors
      in
      let add_variance_error ~errors annotation =
        match annotation with
        | Type.Variable variable when Type.Variable.Unary.is_contravariant variable ->
            emit_error
              ~errors
              ~location
              ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Return })
        | _ -> errors
      in
      let add_async_generator_error ~errors annotation =
        if async && generator then
          let async_generator_type =
            Type.parametric "typing.AsyncGenerator" [Single Type.Any; Single Type.Any]
          in
          if
            GlobalResolution.less_or_equal
              ~left:async_generator_type
              ~right:annotation
              global_resolution
          then
            errors
          else
            emit_error
              ~errors
              ~location
              ~kind:(Error.IncompatibleAsyncGeneratorReturnType annotation)
        else
          errors
      in
      let errors = add_missing_return_error ~errors return_annotation in
      match return_annotation with
      | None -> errors
      | Some return_annotation ->
          let annotation_errors, annotation =
            parse_and_check_annotation ~resolution return_annotation
          in
          List.append annotation_errors errors
          |> fun errors ->
          add_async_generator_error ~errors annotation
          |> fun errors -> add_variance_error ~errors annotation
    in
    let add_capture_annotations ~outer_scope_type_variables resolution errors =
      let process_signature ({ Define.Signature.name; _ } as signature) =
        if Reference.is_local name then
          type_of_signature ~resolution signature
          |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_type_variables
          |> Annotation.create_mutable
          |> fun annotation -> Resolution.new_local resolution ~reference:name ~annotation
        else
          resolution
      in
      let process_capture (resolution, errors) { Define.Capture.name; kind } =
        let resolution, errors, type_ =
          match kind with
          | Define.Capture.Kind.Annotation None ->
              ( resolution,
                emit_error ~errors ~location ~kind:(Error.MissingCaptureAnnotation name),
                Type.Any )
          | Define.Capture.Kind.Annotation (Some annotation_expression) ->
              let annotation_errors, annotation =
                parse_and_check_annotation ~resolution annotation_expression
              in
              resolution, List.append annotation_errors errors, annotation
          | Define.Capture.Kind.DefineSignature signature ->
              ( resolution,
                errors,
                type_of_signature ~resolution signature
                |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_type_variables )
          | Define.Capture.Kind.Self parent ->
              resolution, errors, type_of_parent ~global_resolution parent
          | Define.Capture.Kind.ClassSelf parent ->
              resolution, errors, type_of_parent ~global_resolution parent |> Type.meta
        in
        let type_ =
          let is_readonly_entrypoint_function =
            decorators
            |> List.map ~f:Expression.show
            |> String.Set.of_list
            |> Set.inter Recognized.readonly_entrypoint_decorators
            |> Set.is_empty
            |> not
          in
          if is_readonly_entrypoint_function then
            type_ |> Type.ReadOnly.create |> Annotation.create_immutable
          else
            Annotation.create_immutable type_
        in
        let resolution =
          let reference = Reference.create name in
          Resolution.new_local resolution ~reference ~annotation:type_
        in
        resolution, errors
      in
      let resolution = process_signature signature in
      List.fold captures ~init:(resolution, errors) ~f:process_capture
    in
    let check_parameter_annotations resolution errors =
      let make_parameter_name name =
        name
        |> String.filter ~f:(function
               | '*' -> false
               | _ -> true)
        |> Reference.create
      in
      let check_parameter
          index
          (new_resolution, errors)
          { Node.location; value = { Parameter.name; value; annotation } }
        =
        let add_incompatible_variable_error ~errors annotation default =
          if
            Type.is_any default
            || GlobalResolution.less_or_equal global_resolution ~left:default ~right:annotation
            || GlobalResolution.constraints_solution_exists
                 global_resolution
                 ~left:default
                 ~right:annotation
          then
            errors
          else
            incompatible_variable_type_error_kind
              ~global_resolution
              ~declare_location:(instantiate_path ~global_resolution location)
              {
                Error.name = Reference.create name;
                mismatch =
                  Error.create_mismatch
                    ~resolution:global_resolution
                    ~expected:annotation
                    ~actual:default
                    ~covariant:true;
              }
            |> fun kind -> emit_error ~errors ~location ~kind
        in
        let add_missing_parameter_annotation_error ~errors ~given_annotation annotation =
          let name = name |> Identifier.sanitized in
          let is_dunder_new_method_for_named_tuple =
            Define.is_method define
            && Reference.is_suffix ~suffix:(Reference.create ".__new__") define.signature.name
            && Option.value_map
                 ~default:false
                 ~f:(name_is ~name:"typing.NamedTuple")
                 return_annotation
          in
          if
            String.equal name "*"
            || String.is_prefix ~prefix:"_" name
            || Option.is_some given_annotation
               && (String.is_prefix ~prefix:"**" name || String.is_prefix ~prefix:"*" name)
            || is_dunder_new_method_for_named_tuple
            || String.equal name "/"
          then
            errors
          else
            emit_error
              ~errors
              ~location
              ~kind:
                (Error.MissingParameterAnnotation
                   {
                     name = Reference.create name;
                     annotation;
                     given_annotation;
                     evidence_locations = [];
                     thrown_at_source = true;
                   })
        in
        let add_final_parameter_annotation_error ~errors =
          emit_error ~errors ~location ~kind:(Error.InvalidType (FinalParameter name))
        in
        let add_variance_error errors annotation =
          match annotation with
          | Type.Variable variable
            when (not (Define.is_constructor define)) && Type.Variable.Unary.is_covariant variable
            ->
              emit_error
                ~errors
                ~location
                ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Parameter })
          | _ -> errors
        in
        let parse_as_unary () =
          let errors, annotation =
            match index, parent with
            | 0, Some parent
            (* __new__ does not require an annotation for __cls__, even though it is a static
               method. *)
              when not
                     (Define.is_class_toplevel define
                     || Define.is_static_method define
                        && not (String.equal (Define.unqualified_name define) "__new__")) -> (
                let resolved, is_class_method =
                  let parent_annotation = type_of_parent ~global_resolution parent in
                  if Define.is_class_method define || Define.is_class_property define then
                    (* First parameter of a method is a class object. *)
                    Type.meta parent_annotation, true
                  else (* First parameter of a method is the callee object. *)
                    parent_annotation, false
                in
                match annotation with
                | Some annotation ->
                    let errors, annotation =
                      let annotation_errors, annotation =
                        parse_and_check_annotation ~resolution ~bind_variables:false annotation
                      in
                      List.append annotation_errors errors, annotation
                    in
                    let enforce_here =
                      let is_literal_classmethod decorator =
                        match Decorator.from_expression decorator with
                        | None -> false
                        | Some { Decorator.name = { Node.value = name; _ }; _ } -> (
                            match Reference.as_list name with
                            | ["classmethod"] -> true
                            | _ -> false)
                      in
                      match List.rev decorators with
                      | [] -> true
                      | last :: _ when is_literal_classmethod last -> true
                      | _ :: _ -> false
                    in
                    let errors =
                      if enforce_here then
                        let name = Identifier.sanitized name in
                        let kind =
                          let compatible =
                            GlobalResolution.constraints_solution_exists
                              global_resolution
                              ~left:resolved
                              ~right:annotation
                          in
                          if compatible then
                            None
                          else if
                            (is_class_method && String.equal name "cls")
                            || ((not is_class_method) && String.equal name "self")
                          then
                            (* Assume the user incorrectly tried to type the implicit parameter *)
                            Some
                              (Error.InvalidMethodSignature { annotation = Some annotation; name })
                          else (* Assume the user forgot to specify the implicit parameter *)
                            Some
                              (Error.InvalidMethodSignature
                                 {
                                   annotation = None;
                                   name = (if is_class_method then "cls" else "self");
                                 })
                        in
                        match kind with
                        | Some kind -> emit_error ~errors ~location ~kind
                        | None -> errors
                      else
                        errors
                    in
                    errors, Annotation.create_mutable annotation
                | None -> errors, Annotation.create_mutable resolved)
            | _ -> (
                let errors, parsed_annotation =
                  match annotation with
                  | None -> errors, None
                  | Some annotation ->
                      let anntation_errors, annotation =
                        parse_and_check_annotation ~resolution ~bind_variables:false annotation
                      in
                      let errors = List.append anntation_errors errors in
                      let errors = add_variance_error errors annotation in
                      errors, Some annotation
                in
                let contains_prohibited_any parsed_annotation =
                  let contains_literal_any =
                    annotation >>| Type.expression_contains_any |> Option.value ~default:false
                  in
                  contains_literal_any && Type.contains_prohibited_any parsed_annotation
                in
                let value_annotation, errors =
                  match value with
                  | Some value ->
                      let { Resolved.resolved; errors = value_errors; _ } =
                        forward_expression ~resolution value
                      in
                      Some resolved, value_errors @ errors
                  | None -> None, errors
                in
                let errors =
                  match parsed_annotation, value_annotation with
                  | Some annotation, Some value_annotation ->
                      add_incompatible_variable_error ~errors annotation value_annotation
                  | _ -> errors
                in
                match parsed_annotation, value_annotation with
                | Some annotation, Some _ when Type.contains_final annotation ->
                    ( add_final_parameter_annotation_error ~errors,
                      Annotation.create_immutable annotation )
                | Some annotation, Some value_annotation when contains_prohibited_any annotation ->
                    ( add_missing_parameter_annotation_error
                        ~errors
                        ~given_annotation:(Some annotation)
                        (Some value_annotation),
                      Annotation.create_immutable annotation )
                | Some annotation, _ when Type.contains_final annotation ->
                    ( add_final_parameter_annotation_error ~errors,
                      Annotation.create_immutable annotation )
                | Some annotation, None when contains_prohibited_any annotation ->
                    ( add_missing_parameter_annotation_error
                        ~errors
                        ~given_annotation:(Some annotation)
                        None,
                      Annotation.create_immutable annotation )
                | Some annotation, _ ->
                    let errors =
                      emit_invalid_enumeration_literal_errors
                        ~resolution
                        ~location
                        ~errors
                        annotation
                    in
                    errors, Annotation.create_immutable annotation
                | None, Some value_annotation ->
                    ( add_missing_parameter_annotation_error
                        ~errors
                        ~given_annotation:None
                        (Some value_annotation),
                      Annotation.create_mutable Type.Any )
                | None, None ->
                    ( add_missing_parameter_annotation_error ~errors ~given_annotation:None None,
                      Annotation.create_mutable Type.Any ))
          in
          let apply_starred_annotations annotation =
            if String.is_prefix ~prefix:"**" name then
              Type.dictionary ~key:Type.string ~value:annotation
            else if String.is_prefix ~prefix:"*" name then
              Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation annotation)
            else
              annotation
          in
          let transform type_ =
            Type.Variable.mark_all_variables_as_bound type_ |> apply_starred_annotations
          in
          errors, Annotation.transform_types ~f:transform annotation
        in
        let errors, annotation =
          if String.is_prefix ~prefix:"*" name && not (String.is_prefix ~prefix:"**" name) then
            annotation
            >>= Type.OrderedTypes.concatenation_from_unpack_expression
                  ~parse_annotation:(GlobalResolution.parse_annotation global_resolution)
            >>| (fun concatenation ->
                  Type.Tuple (Concatenation concatenation)
                  |> Type.Variable.mark_all_variables_as_bound
                  |> Annotation.create_mutable
                  |> fun annotation -> errors, annotation)
            |> Option.value ~default:(parse_as_unary ())
          else
            parse_as_unary ()
        in
        ( Resolution.new_local ~reference:(make_parameter_name name) ~annotation new_resolution,
          errors )
      in
      let number_of_stars name = Identifier.split_star name |> fst |> String.length in
      match List.rev parameters, parent with
      | [], Some _ when not (Define.is_class_toplevel define || Define.is_static_method define) ->
          let errors =
            let name =
              if Define.is_class_method define || Define.is_class_property define then
                "cls"
              else
                "self"
            in
            emit_error
              ~errors
              ~location
              ~kind:(Error.InvalidMethodSignature { annotation = None; name })
          in
          resolution, errors
      | ( {
            Node.value = { name = second_name; value = None; annotation = Some second_annotation };
            _;
          }
          :: {
               Node.value = { name = first_name; value = None; annotation = Some first_annotation };
               _;
             }
          :: reversed_head,
          _ )
        when number_of_stars first_name = 1 && number_of_stars second_name = 2 -> (
          match
            GlobalResolution.parse_as_parameter_specification_instance_annotation
              global_resolution
              ~variable_parameter_annotation:first_annotation
              ~keywords_parameter_annotation:second_annotation
          with
          | Some variable ->
              let add_annotations_to_resolution
                  {
                    Type.Variable.Variadic.Parameters.Components.positional_component;
                    keyword_component;
                  }
                =
                resolution
                |> Resolution.new_local
                     ~reference:(make_parameter_name first_name)
                     ~annotation:(Annotation.create_mutable positional_component)
                |> Resolution.new_local
                     ~reference:(make_parameter_name second_name)
                     ~annotation:(Annotation.create_mutable keyword_component)
              in
              if Resolution.type_variable_exists resolution ~variable:(ParameterVariadic variable)
              then
                let new_resolution =
                  Type.Variable.Variadic.Parameters.mark_as_bound variable
                  |> Type.Variable.Variadic.Parameters.decompose
                  |> add_annotations_to_resolution
                in
                List.rev reversed_head
                |> List.foldi ~init:(new_resolution, errors) ~f:check_parameter
              else
                let errors =
                  let origin =
                    if Define.is_toplevel (Node.value Context.define) then
                      Error.Toplevel
                    else if Define.is_class_toplevel (Node.value Context.define) then
                      Error.ClassToplevel
                    else
                      Error.Define
                  in
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.InvalidTypeVariable { annotation = ParameterVariadic variable; origin })
                in
                ( add_annotations_to_resolution
                    { positional_component = Top; keyword_component = Top },
                  errors )
          | None -> List.foldi ~init:(resolution, errors) ~f:check_parameter parameters)
      | _ -> List.foldi ~init:(resolution, errors) ~f:check_parameter parameters
    in
    let check_base_annotations resolution errors =
      let current_class_name = parent >>| Reference.show in
      let is_current_class_typed_dictionary =
        current_class_name
        >>| (fun class_name ->
              GlobalResolution.is_typed_dictionary
                ~resolution:global_resolution
                (Primitive class_name))
        |> Option.value ~default:false
      in
      if Define.is_class_toplevel define then
        let open Annotated in
        let check_base old_errors base =
          let annotation_errors, parsed = parse_and_check_annotation ~resolution base in
          let errors = List.append annotation_errors old_errors in
          match parsed with
          | Type.Parametric { name = "type"; parameters = [Single Type.Any] } ->
              (* Inheriting from type makes you a metaclass, and we don't want to
               * suggest that instead you need to use typing.Type[Something] *)
              old_errors
          | Primitive base_name ->
              if
                is_current_class_typed_dictionary
                && not
                     (GlobalResolution.is_typed_dictionary
                        ~resolution:global_resolution
                        (Primitive base_name)
                     || Type.TypedDictionary.is_builtin_typed_dictionary_class base_name)
              then
                emit_error
                  ~errors
                  ~location:(Node.location base)
                  ~kind:
                    (InvalidInheritance
                       (UninheritableType
                          { annotation = parsed; is_parent_class_typed_dictionary = true }))
              else
                errors
          | Top
          (* There's some other problem we already errored on *)
          | Parametric _
          | Tuple _ ->
              errors
          | Any when GlobalResolution.base_is_from_placeholder_stub global_resolution base -> errors
          | annotation ->
              emit_error
                ~errors
                ~location:(Node.location base)
                ~kind:
                  (InvalidInheritance
                     (UninheritableType { annotation; is_parent_class_typed_dictionary = false }))
        in
        let bases =
          Node.create define ~location
          |> Define.create
          |> Define.parent_definition ~resolution:global_resolution
          >>| Node.value
          >>| ClassSummary.base_classes
          |> Option.value ~default:[]
        in
        let errors = List.fold ~init:errors ~f:check_base bases in
        if is_current_class_typed_dictionary then
          let open Type.Record.TypedDictionary in
          let superclass_pairs_with_same_field_name =
            let field_name_to_successor_fields_map =
              let get_typed_dictionary_fields class_name =
                GlobalResolution.get_typed_dictionary
                  ~resolution:global_resolution
                  (Type.Primitive class_name)
                >>| (fun { fields; _ } -> fields)
                |> Option.value ~default:[]
              in
              let get_successor_map_entries successor_name =
                get_typed_dictionary_fields successor_name
                |> List.map ~f:(fun ({ name; annotation = _; _ } as field) ->
                       name, (successor_name, field))
              in
              let base_classes =
                current_class_name
                >>| GlobalResolution.immediate_parents ~resolution:global_resolution
                |> Option.value ~default:[]
              in
              List.concat_map base_classes ~f:get_successor_map_entries
              |> Map.of_alist_multi (module String)
            in
            let all_pairs items =
              List.cartesian_product items items
              |> List.filter ~f:(fun ((class_name1, _), (class_name2, _)) ->
                     String.compare class_name1 class_name2 < 0)
            in
            Map.data field_name_to_successor_fields_map |> List.concat_map ~f:all_pairs
          in
          let emit_requiredness_error
              errors
              ((class_name1, { name; required = required1; _ }), (class_name2, _))
            =
            let mismatch =
              if required1 then
                Error.RequirednessMismatch
                  {
                    required_field_class = class_name1;
                    non_required_field_class = class_name2;
                    field_name = name;
                  }
              else
                Error.RequirednessMismatch
                  {
                    required_field_class = class_name2;
                    non_required_field_class = class_name1;
                    field_name = name;
                  }
            in
            emit_error
              ~errors
              ~location
              ~kind:(InvalidInheritance (TypedDictionarySuperclassCollision mismatch))
          in
          let emit_type_mismatch_error
              errors
              ( (class_name1, { name = field_name; annotation = annotation1; _ }),
                (class_name2, { annotation = annotation2; _ }) )
            =
            emit_error
              ~errors
              ~location
              ~kind:
                (InvalidInheritance
                   (TypedDictionarySuperclassCollision
                      (Error.TypeMismatch
                         {
                           field_name;
                           annotation_and_parent1 =
                             { annotation = annotation1; parent = class_name1 };
                           annotation_and_parent2 =
                             { annotation = annotation2; parent = class_name2 };
                         })))
          in
          let errors =
            List.filter superclass_pairs_with_same_field_name ~f:(fun ((_, field1), (_, field2)) ->
                Type.TypedDictionary.same_name_different_requiredness field1 field2)
            |> List.fold ~init:errors ~f:emit_requiredness_error
          in
          let errors =
            List.filter superclass_pairs_with_same_field_name ~f:(fun ((_, field1), (_, field2)) ->
                Type.TypedDictionary.same_name_different_annotation field1 field2)
            |> List.fold ~init:errors ~f:emit_type_mismatch_error
          in
          errors
        else
          errors
      else
        errors
    in
    let check_init_subclass_call resolution errors =
      let init_subclass_arguments =
        Node.create define ~location
        |> Annotated.Define.create
        |> Annotated.Define.parent_definition ~resolution:global_resolution
        >>| Node.value
        >>| (fun { ClassSummary.bases = { init_subclass_arguments; _ }; _ } ->
              init_subclass_arguments)
        |> Option.value ~default:[]
      in
      let init_subclass_parent =
        let find_init_subclass parent_class =
          GlobalResolution.attribute_from_class_name
            ~resolution:global_resolution
            ~transitive:false
            ~accessed_through_class:true
            ~name:"__init_subclass__"
            ~instantiated:(Type.Primitive parent_class)
            parent_class
          >>= fun attribute ->
          Option.some_if
            (AnnotatedAttribute.defined attribute
            && String.equal (AnnotatedAttribute.parent attribute) parent_class)
            attribute
          >>| AnnotatedAttribute.parent
        in
        parent
        >>| Reference.show
        >>| GlobalResolution.successors ~resolution:global_resolution
        >>= List.find_map ~f:find_init_subclass
      in
      match init_subclass_parent with
      | Some parent ->
          let implicit_call =
            Expression.Call
              {
                callee =
                  {
                    Node.location;
                    value =
                      Name
                        (Name.Attribute
                           {
                             base =
                               Expression.Name (create_name ~location parent)
                               |> Node.create ~location;
                             attribute = "__init_subclass__";
                             special = false;
                           });
                  };
                arguments = init_subclass_arguments;
              }
            |> Node.create ~location
          in
          let { Resolved.errors = init_subclass_errors; _ } =
            forward_expression ~resolution implicit_call
          in
          init_subclass_errors @ errors
      | None -> errors
    in
    let check_behavioral_subtyping resolution errors =
      let is_allowlisted_dunder_method define =
        let allowlist =
          String.Set.of_list
            [
              "__call__";
              "__delattr__";
              "__delitem__";
              "__eq__";
              "__getitem__";
              "__ne__";
              "__setattr__";
              "__setitem__";
              "__sizeof__";
            ]
        in
        String.Set.mem allowlist (Define.unqualified_name define)
      in
      try
        if
          Define.is_constructor define
          || Define.is_overloaded_function define
          || is_allowlisted_dunder_method define
        then
          errors
        else
          let open Annotated in
          begin
            match define with
            | { Ast.Statement.Define.signature = { parent = Some parent; decorators; _ }; _ } -> (
                GlobalResolution.overrides
                  (Reference.show parent)
                  ~resolution:global_resolution
                  ~name:(StatementDefine.unqualified_name define)
                >>| fun overridden_attribute ->
                let errors =
                  match AnnotatedAttribute.visibility overridden_attribute with
                  | ReadOnly (Refinable { overridable = false }) ->
                      let parent = overridden_attribute |> Attribute.parent in
                      emit_error
                        ~errors
                        ~location
                        ~kind:(Error.InvalidOverride { parent; decorator = Final })
                  | _ -> errors
                in
                let errors =
                  if
                    not
                      (Bool.equal
                         (Attribute.static overridden_attribute)
                         (StatementDefine.is_static_method define))
                  then
                    let parent = overridden_attribute |> Attribute.parent in
                    let decorator =
                      if Attribute.static overridden_attribute then
                        Error.StaticSuper
                      else
                        Error.StaticOverride
                    in
                    emit_error ~errors ~location ~kind:(Error.InvalidOverride { parent; decorator })
                  else
                    errors
                in
                (* Check strengthening of postcondition. *)
                let overridden_base_attribute_annotation =
                  Annotation.annotation (Attribute.annotation overridden_attribute)
                in
                match overridden_base_attribute_annotation with
                | Type.Parametric
                    {
                      name = "BoundMethod";
                      parameters = [Single (Type.Callable { implementation; kind; _ }); _];
                    }
                | Type.Callable { Type.Callable.implementation; kind; _ } ->
                    let original_implementation =
                      resolve_reference_type ~resolution name
                      |> function
                      | Type.Callable { Type.Callable.implementation = original_implementation; _ }
                      | Type.Parametric
                          {
                            parameters =
                              [
                                Single
                                  (Type.Callable { implementation = original_implementation; _ });
                                _;
                              ];
                            _;
                          } ->
                          original_implementation
                      | annotation -> raise (ClassHierarchy.Untracked (Type.show annotation))
                    in
                    let errors =
                      let expected = Type.Callable.Overload.return_annotation implementation in
                      let actual =
                        Type.Callable.Overload.return_annotation original_implementation
                      in
                      if
                        Type.Variable.all_variables_are_resolved expected
                        && not
                             (GlobalResolution.less_or_equal
                                global_resolution
                                ~left:actual
                                ~right:expected)
                      then
                        emit_error
                          ~errors
                          ~location
                          ~kind:
                            (Error.InconsistentOverride
                               {
                                 overridden_method = StatementDefine.unqualified_name define;
                                 parent = Attribute.parent overridden_attribute |> Reference.create;
                                 override_kind = Method;
                                 override =
                                   Error.WeakenedPostcondition
                                     (Error.create_mismatch
                                        ~resolution:global_resolution
                                        ~actual
                                        ~expected
                                        ~covariant:false);
                               })
                      else
                        errors
                    in
                    (* Check weakening of precondition. *)
                    let overriding_parameters =
                      let parameter_annotations
                          { StatementDefine.signature = { parameters; _ }; _ }
                          ~resolution
                        =
                        let element { Node.value = { Parameter.name; annotation; value; _ }; _ } =
                          let annotation =
                            annotation
                            >>| (fun annotation ->
                                  GlobalResolution.parse_annotation resolution annotation)
                            |> Option.value ~default:Type.Top
                          in
                          name, annotation, value
                        in
                        List.map parameters ~f:element
                      in
                      parameter_annotations define ~resolution:global_resolution
                      |> List.map ~f:(fun (name, annotation, value) ->
                             let default = Option.is_some value in
                             { Type.Callable.Parameter.name; annotation; default })
                      |> Type.Callable.Parameter.create
                    in
                    let validate_match ~errors ~index ~overridden_parameter ~expected = function
                      | Some actual -> (
                          let is_compatible =
                            let expected = Type.Variable.mark_all_variables_as_bound expected in
                            GlobalResolution.constraints_solution_exists
                              global_resolution
                              ~left:expected
                              ~right:actual
                          in
                          let is_self_or_class_parameter =
                            index = 0 && not (StatementDefine.is_static_method define)
                          in
                          try
                            if
                              (not (Type.is_top expected))
                              && (not is_compatible)
                              && not is_self_or_class_parameter
                            then
                              emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.InconsistentOverride
                                     {
                                       overridden_method = StatementDefine.unqualified_name define;
                                       parent =
                                         Attribute.parent overridden_attribute |> Reference.create;
                                       override_kind = Method;
                                       override =
                                         Error.StrengthenedPrecondition
                                           (Error.Found
                                              (Error.create_mismatch
                                                 ~resolution:global_resolution
                                                 ~actual
                                                 ~expected
                                                 ~covariant:false));
                                     })
                            else
                              errors
                          with
                          | ClassHierarchy.Untracked _ ->
                              (* TODO(T27409168): Error here. *)
                              errors)
                      | None ->
                          let has_keyword_and_anonymous_starred_parameters =
                            List.exists overriding_parameters ~f:(function
                                | Keywords _ -> true
                                | _ -> false)
                            && List.exists overriding_parameters ~f:(function
                                   | Variable _ -> true
                                   | _ -> false)
                          in
                          if has_keyword_and_anonymous_starred_parameters then
                            errors
                          else
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.InconsistentOverride
                                   {
                                     overridden_method = StatementDefine.unqualified_name define;
                                     override_kind = Method;
                                     parent =
                                       Attribute.parent overridden_attribute |> Reference.create;
                                     override =
                                       Error.StrengthenedPrecondition
                                         (Error.NotFound
                                            {
                                              parameter = overridden_parameter;
                                              parameter_exists_in_overridden_signature = true;
                                            });
                                   })
                    in
                    let check_parameter index errors = function
                      | `Both (overridden_parameter, overriding_parameter) -> (
                          match
                            ( Type.Callable.RecordParameter.annotation overridden_parameter,
                              Type.Callable.RecordParameter.annotation overriding_parameter )
                          with
                          | Some expected, Some actual ->
                              validate_match
                                ~errors
                                ~index
                                ~overridden_parameter
                                ~expected
                                (Some actual)
                          | None, _
                          | _, None ->
                              (* TODO(T53997072): There is no reasonable way to compare Variable
                                 (Concatenation _). For now, let's just ignore this. *)
                              errors)
                      | `Left overridden_parameter -> (
                          match Type.Callable.RecordParameter.annotation overridden_parameter with
                          | Some expected ->
                              validate_match ~errors ~index ~overridden_parameter ~expected None
                          | None -> errors)
                      | `Right overriding_parameter ->
                          let is_args_kwargs_or_has_default =
                            match overriding_parameter with
                            | Type.Callable.RecordParameter.Keywords _ -> true
                            | Variable _ -> true
                            | Named { default = has_default; _ } -> has_default
                            | KeywordOnly { default = has_default; _ } -> has_default
                            | PositionalOnly { default = has_default; _ } -> has_default
                          in
                          (* TODO(T150016653): Figure out how to handle decorators and clean up
                             hardcoding. This is a yucky hack. *)
                          let allowlisted_non_modifying_decorators =
                            [
                              Reference.create "staticmethod";
                              Reference.create "classmethod";
                              Reference.create "override";
                            ]
                          in
                          let is_equal_to_decorator_ref name ref =
                            [%compare.equal: Reference.t option]
                              (name_to_reference name)
                              (Option.some ref)
                          in
                          let is_non_modifying_decorator = function
                            | { Node.value = decorator; _ } -> (
                                match decorator with
                                | Expression.Name decorator_name ->
                                    List.exists
                                      ~f:(is_equal_to_decorator_ref decorator_name)
                                      allowlisted_non_modifying_decorators
                                | _ -> false)
                          in
                          let are_overriding_function_possibly_changed =
                            not (List.for_all ~f:is_non_modifying_decorator decorators)
                          in
                          let are_overridden_function_args_possibly_changed =
                            match kind with
                            | Anonymous -> true
                            | Named function_name -> (
                                let definition =
                                  GlobalResolution.define_body global_resolution function_name
                                in
                                match definition with
                                | None -> false
                                | Some
                                    {
                                      Node.value =
                                        { StatementDefine.signature = { decorators; _ }; _ };
                                      _;
                                    } ->
                                    not (List.for_all ~f:is_non_modifying_decorator decorators))
                          in

                          if
                            is_args_kwargs_or_has_default
                            || are_overriding_function_possibly_changed
                            || are_overridden_function_args_possibly_changed
                          then
                            errors
                          else
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.InconsistentOverride
                                   {
                                     overridden_method = StatementDefine.unqualified_name define;
                                     parent =
                                       Attribute.parent overridden_attribute |> Reference.create;
                                     override_kind = Method;
                                     override =
                                       Error.StrengthenedPrecondition
                                         (Error.NotFound
                                            {
                                              parameter = overriding_parameter;
                                              parameter_exists_in_overridden_signature = false;
                                            });
                                   })
                    in
                    let overridden_parameters =
                      Type.Callable.Overload.parameters implementation |> Option.value ~default:[]
                    in
                    Type.Callable.Parameter.zip overridden_parameters overriding_parameters
                    |> List.foldi ~init:errors ~f:check_parameter
                | _ -> errors)
            | _ -> None
          end
          |> Option.value ~default:errors
      with
      | ClassHierarchy.Untracked _ -> errors
    in
    let check_constructor_return errors =
      if not (Define.is_constructor define) then
        errors
      else
        match return_annotation with
        | Some ({ Node.location; _ } as annotation) -> (
            match define with
            | { Define.signature = { Define.Signature.name; _ }; _ }
              when String.equal (Reference.last name) "__new__" ->
                (* TODO(T45018328): Error here. `__new__` is a special undecorated static method,
                   and we really ought to be checking its return type against typing.Type[Cls]. *)
                errors
            | _ ->
                let annotation = GlobalResolution.parse_annotation global_resolution annotation in
                if Type.is_none annotation then
                  errors
                else
                  emit_error
                    ~errors
                    ~location
                    ~kind:(Error.IncompatibleConstructorAnnotation annotation))
        | _ -> errors
    in
    let outer_scope_type_variables, current_scope_type_variables =
      let type_variables_of_class class_name =
        let unarize unary =
          let fix_invalid_parameters_in_bounds unary =
            match
              GlobalResolution.check_invalid_type_parameters global_resolution (Type.Variable unary)
            with
            | _, Type.Variable unary -> unary
            | _ -> failwith "did not transform"
          in
          fix_invalid_parameters_in_bounds unary |> fun unary -> Type.Variable.Unary unary
        in
        let extract = function
          | Type.Variable.Unary unary -> unarize unary
          | ParameterVariadic variable -> ParameterVariadic variable
          | TupleVariadic variable -> TupleVariadic variable
        in
        Reference.show class_name
        |> GlobalResolution.variables global_resolution
        >>| List.map ~f:extract
        |> Option.value ~default:[]
      in
      let type_variables_of_define signature =
        let parser =
          GlobalResolution.annotation_parser ~allow_invalid_type_parameters:true global_resolution
        in
        let variables = GlobalResolution.variables global_resolution in
        let define_variables =
          AnnotatedCallable.create_overload_without_applying_decorators ~parser ~variables signature
          |> (fun { parameters; _ } -> Type.Callable.create ~parameters ~annotation:Type.Top ())
          |> Type.Variable.all_free_variables
          |> List.dedup_and_sort ~compare:Type.Variable.compare
        in
        let parent_variables =
          let { Define.Signature.parent; _ } = signature in
          (* PEP484 specifies that scope of the type variables of the outer class doesn't cover the
             inner one. We are able to inspect only 1 level of nesting class as a result. *)
          Option.value_map parent ~f:type_variables_of_class ~default:[]
        in
        List.append parent_variables define_variables
      in
      match Define.is_class_toplevel define with
      | true ->
          let class_name = Option.value_exn parent in
          [], type_variables_of_class class_name
      | false ->
          let define_variables = type_variables_of_define signature in
          let nesting_define_variables =
            let rec walk_nesting_define sofar = function
              | None -> sofar
              | Some define_name -> (
                  (* TODO (T57339384): This operation should only depend on the signature, not the
                     body *)
                  match GlobalResolution.define_body global_resolution define_name with
                  | None -> sofar
                  | Some
                      {
                        Node.value =
                          {
                            Define.signature = { Define.Signature.nesting_define; _ } as signature;
                            _;
                          };
                        _;
                      } ->
                      let sofar = List.rev_append (type_variables_of_define signature) sofar in
                      walk_nesting_define sofar nesting_define)
            in
            walk_nesting_define [] nesting_define
          in
          nesting_define_variables, define_variables
    in
    let resolution, errors =
      let resolution =
        List.append current_scope_type_variables outer_scope_type_variables
        |> List.fold ~init:resolution ~f:(fun resolution variable ->
               Resolution.add_type_variable resolution ~variable)
      in
      let resolution = Resolution.with_parent resolution ~parent in
      let resolution, errors = add_capture_annotations ~outer_scope_type_variables resolution [] in
      let resolution, errors = check_parameter_annotations resolution errors in
      let errors =
        check_unbound_names errors
        |> check_return_annotation resolution
        |> check_decorators resolution
        |> check_base_annotations resolution
        |> check_init_subclass_call resolution
        |> check_behavioral_subtyping resolution
        |> check_constructor_return
      in
      resolution, errors
    in
    let state =
      let postcondition = Resolution.annotation_store resolution in
      let statement_key = [%hash: int * int] (Cfg.entry_index, 0) in
      let (_ : unit option) =
        Context.resolution_fixpoint >>| LocalAnnotationMap.set ~statement_key ~postcondition
      in
      let (_ : unit option) = Context.error_map >>| LocalErrorMap.set ~statement_key ~errors in
      Value resolution
    in
    state


  let forward ~statement_key state ~statement =
    match state with
    | Unreachable -> state
    | Value resolution ->
        let post_resolution, errors = forward_statement ~resolution ~statement in
        let () =
          let (_ : unit option) = Context.error_map >>| LocalErrorMap.set ~statement_key ~errors in
          match post_resolution with
          | Unreachable -> ()
          | Value post_resolution ->
              let precondition = Resolution.annotation_store resolution in
              let postcondition = Resolution.annotation_store post_resolution in
              let (_ : unit option) =
                Context.resolution_fixpoint
                >>| LocalAnnotationMap.set ~statement_key ~precondition ~postcondition
              in
              ()
        in
        post_resolution


  let backward ~statement_key:_ state ~statement:_ = state
end

module CheckResult = struct
  type t = {
    errors: Error.t list option;
    local_annotations: LocalAnnotationMap.ReadOnly.t option;
  }

  let errors { errors; _ } = errors

  let local_annotations { local_annotations; _ } = local_annotations

  let equal
      { errors = errors0; local_annotations = local_annotations0 }
      { errors = errors1; local_annotations = local_annotations1 }
    =
    [%compare.equal: Error.t list option] errors0 errors1
    && [%equal: LocalAnnotationMap.ReadOnly.t option] local_annotations0 local_annotations1
end

module DummyContext = struct
  let qualifier = Reference.empty

  let debug = false

  let constraint_solving_style = Configuration.Analysis.default_constraint_solving_style

  let define =
    Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
    |> Node.create_with_default_location


  let resolution_fixpoint = None

  let error_map = None

  module Builder = Callgraph.NullBuilder
end

let resolution
    global_resolution
    ?(annotation_store = Refinement.Store.empty)
    (module Context : Context)
  =
  let module State = State (Context) in
  let resolve_expression ~resolution expression =
    State.forward_expression ~resolution expression
    |> fun { State.Resolved.resolved; resolved_annotation; resolution = new_resolution; _ } ->
    ( new_resolution,
      resolved_annotation |> Option.value ~default:(Annotation.create_mutable resolved) )
  in
  let resolve_statement ~resolution statement =
    State.forward_statement ~resolution ~statement
    |> fun (resolution, errors) ->
    match resolution with
    | Unreachable -> Resolution.Unreachable
    | Value resolution -> Resolution.Reachable { resolution; errors }
  in
  Resolution.create ~global_resolution ~annotation_store ~resolve_expression ~resolve_statement ()


let resolution_with_key ~global_resolution ~local_annotations ~parent ~statement_key context =
  resolution global_resolution context
  |> Resolution.resolution_for_statement ~local_annotations ~parent ~statement_key


let emit_errors_on_exit (module Context : Context) ~errors_sofar ~resolution () =
  let ({ Node.value = { Define.signature = { name; _ } as signature; _ } as define; location } as
      define_node)
    =
    Context.define
  in
  let global_resolution = Resolution.global_resolution resolution in
  let class_initialization_errors errors =
    let check_protocol_properties definition errors =
      if ClassSummary.is_protocol definition then
        let private_protocol_property_errors =
          GlobalResolution.attributes
            ~transitive:false
            ~include_generated_attributes:true
            ~resolution:global_resolution
            (Reference.show (ClassSummary.name definition))
          >>| List.filter ~f:(fun attribute -> AnnotatedAttribute.is_private attribute)
          >>| List.map ~f:(fun attribute ->
                  Error.create
                    ~location:(Location.with_module ~module_reference:Context.qualifier location)
                    ~kind:
                      (Error.PrivateProtocolProperty
                         {
                           name = AnnotatedAttribute.public_name attribute;
                           parent = Type.Primitive (ClassSummary.name definition |> Reference.show);
                         })
                    ~define:Context.define)
          |> Option.value ~default:[]
        in
        private_protocol_property_errors @ errors
      else
        errors
    in
    (* Ensure all attributes are instantiated. *)
    let check_attribute_initialization definition errors =
      if
        (not (ClassSummary.is_protocol definition))
        && (not (ClassSummary.is_abstract definition))
        && not
             (GlobalResolution.is_typed_dictionary
                ~resolution:global_resolution
                (Type.Primitive (Reference.show (ClassSummary.name definition))))
      then
        let unimplemented_errors =
          let uninitialized_attributes =
            let add_uninitialized ({ class_name; _ } as name_and_metadata) attribute_map =
              let attributes =
                GlobalResolution.attributes
                  ~include_generated_attributes:true
                  ~resolution:global_resolution
                  class_name
                |> Option.value ~default:[]
              in
              let is_uninitialized attribute =
                match Annotated.Attribute.initialized attribute with
                | NotInitialized -> true
                | _ -> false
              in
              let add_to_map sofar attribute =
                let annotation =
                  GlobalResolution.instantiate_attribute
                    ~resolution:global_resolution
                    ~accessed_through_class:false
                    ~accessed_through_readonly:false
                    ?instantiated:None
                    attribute
                  |> Annotated.Attribute.annotation
                  |> Annotation.annotation
                in
                let name = Annotated.Attribute.name attribute in
                match String.Map.add sofar ~key:name ~data:(annotation, name_and_metadata) with
                | `Ok map -> map
                | `Duplicate -> sofar
              in
              List.filter attributes ~f:is_uninitialized
              |> List.fold ~init:attribute_map ~f:add_to_map
            in
            let remove_initialized { class_name; _ } attribute_map =
              let attributes =
                GlobalResolution.attributes
                  ~transitive:true
                  ~include_generated_attributes:true
                  ~resolution:global_resolution
                  class_name
                |> Option.value ~default:[]
              in
              let is_initialized attribute =
                (* TODO(T54083014): Don't error on properties overriding attributes, even if they
                   are read-only and therefore not marked as initialized on the attribute object. We
                   should error in the future that this is an inconsistent override. *)
                match Annotated.Attribute.initialized attribute with
                | NotInitialized -> Annotated.Attribute.property attribute
                | _ -> true
              in

              List.filter attributes ~f:is_initialized
              |> List.map ~f:AnnotatedAttribute.name
              |> List.fold ~init:attribute_map ~f:Map.remove
            in
            if ClassSummary.is_abstract definition then
              []
            else
              let abstract_superclasses, concrete_superclasses, _superclasses_missing_metadata =
                let { ClassSummary.name; _ } = definition in
                let class_name = Reference.show name in
                let is_protocol_or_abstract class_name =
                  match
                    GlobalResolution.class_metadata global_resolution (Primitive class_name)
                  with
                  | Some { is_protocol; is_abstract; _ } when is_protocol || is_abstract ->
                      `Fst { class_name; is_abstract; is_protocol }
                  | Some { is_protocol; is_abstract; _ } ->
                      `Snd { class_name; is_abstract; is_protocol }
                  | None -> `Trd ()
                in
                GlobalResolution.successors class_name ~resolution:global_resolution
                |> List.partition3_map ~f:is_protocol_or_abstract
              in
              let name_and_metadata =
                let { ClassSummary.name; _ } = definition in
                {
                  class_name = Reference.show name;
                  is_abstract = ClassSummary.is_abstract definition;
                  is_protocol = ClassSummary.is_protocol definition;
                }
              in
              List.cons name_and_metadata abstract_superclasses
              |> List.fold_right ~init:String.Map.empty ~f:add_uninitialized
              |> (fun attribute_map ->
                   List.fold_right
                     ~init:attribute_map
                     ~f:remove_initialized
                     (List.cons name_and_metadata concrete_superclasses))
              |> String.Map.to_alist
          in
          uninitialized_attributes
          |> List.filter_map
               ~f:(fun
                    ( name,
                      (annotation, { class_name = original_class_name; is_protocol; is_abstract })
                    )
                  ->
                 let parent = Type.Primitive (ClassSummary.name definition |> Reference.show) in
                 let expected = annotation in
                 if Type.is_top expected then
                   None
                 else
                   let error_kind =
                     if is_protocol then
                       Error.Protocol (Reference.create original_class_name)
                     else if is_abstract then
                       Error.Abstract (Reference.create original_class_name)
                     else if
                       GlobalResolution.less_or_equal
                         global_resolution
                         ~left:parent
                         ~right:Type.enumeration
                     then
                       Error.Enumeration
                     else
                       Error.Class
                   in
                   Some
                     (Error.create
                        ~location:
                          (Location.with_module ~module_reference:Context.qualifier location)
                        ~kind:
                          (Error.UninitializedAttribute
                             {
                               name;
                               parent;
                               mismatch =
                                 { Error.expected; actual = expected; due_to_invariance = false };
                               kind = error_kind;
                             })
                        ~define:Context.define))
        in
        unimplemented_errors @ errors
      else
        errors
    in
    if Define.is_class_toplevel define then
      let check_bases errors =
        let open Annotated in
        let is_final errors expression_value =
          let add_error { ClassMetadataEnvironment.is_final; _ } =
            if is_final then
              let error =
                Error.create
                  ~location:(Location.with_module ~module_reference:Context.qualifier location)
                  ~kind:(Error.InvalidInheritance (ClassName (Expression.show expression_value)))
                  ~define:Context.define
              in
              error :: errors
            else
              errors
          in
          match expression_value with
          | { Node.value = Name name; _ } when is_simple_name name ->
              let reference = name_to_reference_exn name in
              GlobalResolution.class_metadata
                global_resolution
                (Type.Primitive (Reference.show reference))
              >>| add_error
              |> Option.value ~default:errors
          | _ -> errors
        in
        Define.parent_definition ~resolution:global_resolution (Define.create define_node)
        >>| Node.value
        >>| ClassSummary.base_classes
        >>| List.fold ~init:errors ~f:is_final
        |> Option.value ~default:errors
      in
      let check_protocol definition errors = check_protocol_properties definition errors in
      let check_overrides class_summary errors =
        let attributes = ClassSummary.attributes ~include_generated_attributes:true class_summary in

        let override_errors_for_typed_dictionary class_name =
          let open Type.Record.TypedDictionary in
          let get_typed_dictionary_fields class_name =
            GlobalResolution.get_typed_dictionary
              ~resolution:global_resolution
              (Type.Primitive class_name)
            >>| (fun typed_dictionary -> typed_dictionary.fields)
            |> Option.value ~default:[]
          in
          let field_name_to_successor_fields_map =
            let get_successor_map_entries successor_name =
              get_typed_dictionary_fields successor_name
              |> List.map ~f:(fun (field : Type.t typed_dictionary_field) ->
                     field.name, (successor_name, field))
            in
            GlobalResolution.successors ~resolution:global_resolution class_name
            |> List.concat_map ~f:get_successor_map_entries
            |> Map.of_alist_multi (module String)
          in
          let colliding_successor_fields (field : Type.t typed_dictionary_field) =
            let matching_successors =
              Map.find_multi field_name_to_successor_fields_map field.name
            in
            let is_inherited_field =
              List.exists matching_successors ~f:(fun (_, successor_field) ->
                  [%equal: Type.t typed_dictionary_field] field successor_field)
            in
            if is_inherited_field then
              []
            else
              List.map matching_successors ~f:(fun (successor_name, successor_field) ->
                  field, (successor_name, successor_field))
          in
          let wrongly_overriding_fields =
            get_typed_dictionary_fields class_name |> List.concat_map ~f:colliding_successor_fields
          in
          let create_override_error
              ((field : Type.t typed_dictionary_field), (successor_name, successor_field))
            =
            let kind =
              Error.InconsistentOverride
                {
                  overridden_method = field.name;
                  parent = Reference.create successor_name;
                  override_kind = Attribute;
                  override =
                    Error.WeakenedPostcondition
                      {
                        actual = field.annotation;
                        expected = successor_field.annotation;
                        due_to_invariance = false;
                      };
                }
            in
            let location =
              Identifier.SerializableMap.find_opt class_name attributes
              >>| Node.location
              |> Option.value ~default:location
            in
            Error.create
              ~location:(Location.with_module ~module_reference:Context.qualifier location)
              ~kind
              ~define:Context.define
          in
          List.map wrongly_overriding_fields ~f:create_override_error
        in
        let override_errors =
          let open Annotated in
          let class_name = ClassSummary.name class_summary |> Reference.show in
          if
            GlobalResolution.is_typed_dictionary
              ~resolution:global_resolution
              (Type.Primitive class_name)
          then
            override_errors_for_typed_dictionary class_name
          else
            GlobalResolution.attributes
              ~include_generated_attributes:false
              ~resolution:global_resolution
              class_name
            >>| List.filter_map ~f:(fun attribute ->
                    (* `accessed_through_class` is true here because it is ture in
                       GlobalResolution.overrides. This mostly works, but we might fail to uncover
                       some incompatible overrides that only appear in instance access.
                       TODO(T146994981) we should check for both attribute access patterns. *)
                    let annotation =
                      GlobalResolution.instantiate_attribute
                        ~accessed_through_class:true
                        ~accessed_through_readonly:false
                        ~resolution:global_resolution
                        ?instantiated:None
                        attribute
                      |> Annotated.Attribute.annotation
                      |> Annotation.annotation
                    in
                    let name = Annotated.Attribute.name attribute in
                    let actual = annotation in
                    let check_override overridden_attribute =
                      let annotation =
                        Annotated.Attribute.annotation overridden_attribute |> Annotation.annotation
                      in
                      let name = Annotated.Attribute.name overridden_attribute in
                      let visibility = Annotated.Attribute.visibility overridden_attribute in
                      let expected = annotation in
                      let overridable =
                        match visibility with
                        | ReadOnly (Refinable { overridable }) -> overridable
                        | _ -> true
                      in
                      if
                        Annotated.Attribute.is_private overridden_attribute
                        || (GlobalResolution.less_or_equal
                              global_resolution
                              ~left:actual
                              ~right:expected
                           || Type.is_top actual
                           || Type.contains_variable actual)
                           && overridable
                      then (* TODO(T53997072): Support type variable instantiation for overrides. *)
                        None
                      else
                        let kind =
                          if not overridable then
                            Error.InvalidAssignment (FinalAttribute (Reference.create name))
                          else
                            Error.InconsistentOverride
                              {
                                overridden_method = name;
                                parent = Attribute.parent overridden_attribute |> Reference.create;
                                override_kind = Attribute;
                                override =
                                  Error.WeakenedPostcondition
                                    (Error.create_mismatch
                                       ~resolution:global_resolution
                                       ~actual
                                       ~expected
                                       ~covariant:false);
                              }
                        in
                        let location =
                          Identifier.SerializableMap.find_opt name attributes
                          >>| Node.location
                          |> Option.value ~default:location
                        in
                        Some
                          (Error.create
                             ~location:
                               (Location.with_module ~module_reference:Context.qualifier location)
                             ~kind
                             ~define:Context.define)
                    in
                    GlobalResolution.overrides ~resolution:global_resolution ~name class_name
                    >>| check_override
                    |> Option.value ~default:None)
            |> Option.value ~default:[]
        in
        override_errors @ errors
      in
      let name = Reference.prefix name >>| Reference.show |> Option.value ~default:"" in
      GlobalResolution.class_summary global_resolution (Type.Primitive name)
      >>| Node.value
      >>| (fun definition ->
            errors
            |> check_bases
            |> check_protocol definition
            |> check_attribute_initialization definition
            |> check_overrides definition)
      |> Option.value ~default:errors
    else
      errors
  in
  let overload_errors errors =
    let parser =
      GlobalResolution.annotation_parser ~allow_invalid_type_parameters:true global_resolution
    in
    let variables = GlobalResolution.variables global_resolution in
    let ({ Type.Callable.annotation = current_overload_annotation; _ } as current_overload) =
      AnnotatedCallable.create_overload_without_applying_decorators ~parser ~variables signature
    in
    let handle ~undecorated_signature ~problem =
      let overload_to_callable overload =
        Type.Callable
          {
            implementation = { overload with annotation = Type.Any };
            kind = Anonymous;
            overloads = [];
          }
      in
      let is_scapegoat =
        match undecorated_signature with
        | {
         Type.Callable.implementation = { annotation = Type.Top; parameters = Undefined };
         overloads;
         _;
        } ->
            (* if there is no implementation we blame the first overload *)
            List.hd overloads
            >>| Type.Callable.equal_overload Type.equal current_overload
            |> Option.value ~default:false
        | _ ->
            (* otherwise blame the implementation *)
            not (Define.is_overloaded_function define)
      in
      let check_implementation_exists errors =
        let { Type.Callable.implementation; _ } = undecorated_signature in
        if
          Define.is_overloaded_function define && Type.Callable.Overload.is_undefined implementation
        then
          let error =
            Error.create
              ~location:(Location.with_module ~module_reference:Context.qualifier location)
              ~kind:(Error.MissingOverloadImplementation name)
              ~define:Context.define
          in
          error :: errors
        else
          errors
      in
      let check_compatible_return_types errors =
        let {
          Type.Callable.implementation =
            { annotation = implementation_annotation; _ } as implementation;
          _;
        }
          =
          undecorated_signature
        in
        if Define.is_overloaded_function define then
          let errors_sofar =
            if
              Resolution.is_consistent_with
                resolution
                current_overload_annotation
                implementation_annotation
                ~expression:None
            then
              errors
            else
              let error =
                Error.create
                  ~location:(Location.with_module ~module_reference:Context.qualifier location)
                  ~kind:
                    (Error.IncompatibleOverload
                       (ReturnType
                          {
                            implementation_annotation;
                            overload_annotation = current_overload_annotation;
                            name;
                          }))
                  ~define:Context.define
              in
              error :: errors
          in
          if
            not
              (GlobalResolution.less_or_equal
                 global_resolution
                 ~right:(overload_to_callable current_overload)
                 ~left:(overload_to_callable implementation))
          then
            let error =
              Error.create
                ~location:(Location.with_module ~module_reference:Context.qualifier location)
                ~define:Context.define
                ~kind:(Error.IncompatibleOverload (Parameters { name; location }))
            in
            error :: errors_sofar
          else
            errors_sofar
        else
          errors
      in
      let check_unmatched_overloads errors =
        let { Type.Callable.overloads; _ } = undecorated_signature in
        if Define.is_overloaded_function define then
          let preceding, following_and_including =
            List.split_while overloads ~f:(fun other ->
                not (Type.Callable.equal_overload Type.equal other current_overload))
          in
          if List.is_empty following_and_including then
            errors
          else
            let right = overload_to_callable current_overload in
            List.find preceding ~f:(fun preceder ->
                GlobalResolution.less_or_equal
                  global_resolution
                  ~left:(overload_to_callable preceder)
                  ~right)
            >>| (fun matching_overload ->
                  Error.create
                    ~location:(Location.with_module ~module_reference:Context.qualifier location)
                    ~define:Context.define
                    ~kind:
                      (Error.IncompatibleOverload
                         (Unmatchable { name; unmatched_location = location; matching_overload })))
            >>| (fun error -> error :: errors)
            |> Option.value ~default:errors
        else
          errors
      in
      let check_differing_decorators errors =
        match problem with
        | Some (AnnotatedAttribute.DifferingDecorators { offender })
          when Type.Callable.equal_overload Type.equal current_overload offender ->
            let error =
              Error.create
                ~location:(Location.with_module ~module_reference:Context.qualifier location)
                ~define:Context.define
                ~kind:(Error.IncompatibleOverload DifferingDecorators)
            in
            error :: errors
        | _ -> errors
      in
      let overload_decorator_misplaced =
        match signature with
        | { decorators = _ :: (_ :: _ as tail_decorators); _ } ->
            let is_overload_decorator decorator =
              Ast.Statement.Define.Signature.is_overloaded_function
                { signature with decorators = [decorator] }
            in
            List.exists tail_decorators ~f:is_overload_decorator
        | _ -> false
      in
      let check_misplaced_overload_decorator errors =
        if overload_decorator_misplaced then
          let error =
            Error.create
              ~location:(Location.with_module ~module_reference:Context.qualifier location)
              ~define:Context.define
              ~kind:(Error.IncompatibleOverload MisplacedOverloadDecorator)
          in
          error :: errors
        else
          errors
      in
      let check_invalid_decorator errors =
        match problem with
        | Some (AnnotatedAttribute.InvalidDecorator { index; reason })
          when is_scapegoat && not overload_decorator_misplaced ->
            let adjusted_index =
              if Define.is_overloaded_function define then
                index + 1
              else
                index
            in
            let add_error decorator =
              let make_error ~location reason =
                let error =
                  Error.create
                    ~location:(Location.with_module ~module_reference:Context.qualifier location)
                    ~define:Context.define
                    ~kind:(Error.InvalidDecoration reason)
                in
                error :: errors
              in
              let extract_error ~reason ~callable =
                let convert reason =
                  errors_from_not_found
                    ~callable
                    ~self_argument:None
                    ~reason
                    ~global_resolution
                    ?original_target:None
                    ?callee_expression:None
                    ~arguments:None
                    ()
                in
                reason >>| convert >>= List.hd >>| fun (_, kind) -> kind
              in
              match Decorator.from_expression decorator with
              | None ->
                  let { Node.location; _ } = decorator in
                  make_error ~location (CouldNotResolve decorator)
              | Some { Decorator.name = { Node.location; value = name }; arguments } -> (
                  match reason with
                  | CouldNotResolve -> make_error ~location (CouldNotResolve decorator)
                  | CouldNotResolveArgument { argument_index } ->
                      let add_error argument =
                        let argument, _ = Ast.Expression.Call.Argument.unpack argument in
                        make_error ~location (CouldNotResolveArgument { name; argument })
                      in
                      arguments
                      >>= (fun arguments -> List.nth arguments argument_index)
                      >>| add_error
                      |> Option.value ~default:errors
                  | NonCallableDecoratorFactory resolved ->
                      make_error
                        ~location
                        (NonCallableDecoratorFactory { name; annotation = resolved })
                  | NonCallableDecorator result ->
                      make_error
                        ~location
                        (NonCallableDecorator
                           { name; has_arguments = Option.is_some arguments; annotation = result })
                  | FactorySignatureSelectionFailed { reason; callable } ->
                      make_error
                        ~location
                        (DecoratorFactoryFailedToApply
                           { name; reason = extract_error ~reason ~callable })
                  | ApplicationFailed { reason; callable } ->
                      make_error
                        ~location
                        (ApplicationFailed
                           {
                             name;
                             has_arguments = Option.is_some arguments;
                             reason = extract_error ~reason ~callable;
                           }))
            in

            let { StatementDefine.Signature.decorators; _ } = signature in
            List.nth decorators adjusted_index >>| add_error |> Option.value ~default:errors
        | _ -> errors
      in
      errors
      |> check_implementation_exists
      |> check_compatible_return_types
      |> check_unmatched_overloads
      |> check_differing_decorators
      |> check_misplaced_overload_decorator
      |> check_invalid_decorator
    in
    match GlobalResolution.global global_resolution name with
    | Some { undecorated_signature = Some undecorated_signature; problem; _ } ->
        handle ~undecorated_signature ~problem
    | _ -> (
        let attribute =
          Reference.prefix name
          >>| Reference.show
          >>= GlobalResolution.attribute_from_class_name
                ~resolution:global_resolution
                ~name:(Reference.last name)
                ~instantiated:Top
        in
        match
          attribute
          >>| fun attribute -> attribute, AnnotatedAttribute.undecorated_signature attribute
        with
        | Some (attribute, Some undecorated_signature) ->
            handle ~undecorated_signature ~problem:(AnnotatedAttribute.problem attribute)
        | _ -> errors)
  in

  class_initialization_errors errors_sofar |> overload_errors


let filter_errors (module Context : Context) ~global_resolution errors =
  if Context.debug then
    errors
  else
    Error.filter ~resolution:global_resolution errors


let exit_state ~resolution (module Context : Context) =
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let initial = State.initial ~resolution in
  let { Node.value = { Define.signature = { Define.Signature.name; _ }; _ } as define; _ } =
    Context.define
  in
  let global_resolution = Resolution.global_resolution resolution in
  if Define.is_stub define then
    let resolution = Option.value_exn (State.resolution initial) in
    let errors_sofar =
      Option.value_exn
        ~message:"analysis context has no error map"
        (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
    in
    {
      resolution;
      errors =
        emit_errors_on_exit (module Context) ~errors_sofar ~resolution ()
        |> filter_errors (module Context) ~global_resolution;
      local_annotations = None;
      callees = None;
    }
  else (
    Log.log ~section:`Check "Checking %a" Reference.pp name;
    Context.Builder.initialize ();
    let cfg = Cfg.create define in
    let fixpoint = Fixpoint.forward ~cfg ~initial in
    let exit = Fixpoint.exit fixpoint in
    (* debugging logic for pyre_dump / pyre_dump_locations / pyre_dump_cfg *)
    if Define.dump_locations define then
      Log.dump
        "AST of %a with Locations:\n----\n%s\n----"
        Reference.pp
        name
        (Define.show_json define);
    if Define.dump define then (
      Log.dump "AST of %a:\n----%a\n----" Reference.pp name Define.pp define;
      Option.iter exit ~f:(Log.dump "Exit state:\n%a" State.pp));
    (if Define.dump_cfg define then
       let precondition { Fixpoint.preconditions; _ } id =
         match Hashtbl.find preconditions id with
         | Some (State.Value exit_resolution) ->
             Resolution.annotation_store exit_resolution |> Refinement.Store.show
         | _ -> ""
       in
       Log.dump
         "CFG for %a in dot syntax for graphviz:\n----\n%s\n----"
         Reference.pp
         name
         (Cfg.to_dot ~precondition:(precondition fixpoint) ~single_line:true cfg));

    let callees = Context.Builder.get_all_callees () in
    let local_annotations =
      Option.value_exn
        ~message:"analysis context has no resolution fixpoint"
        Context.resolution_fixpoint
    in
    let errors =
      Option.value_exn
        ~message:"analysis context has no error map"
        (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
    in
    let resolution, errors =
      match exit with
      | None -> resolution, errors
      | Some post_state ->
          let resolution = State.resolution_or_default post_state ~default:resolution in
          ( resolution,
            emit_errors_on_exit (module Context) ~errors_sofar:errors ~resolution ()
            |> filter_errors (module Context) ~global_resolution )
    in
    { resolution; errors; local_annotations = Some local_annotations; callees = Some callees })


let compute_local_annotations ~global_environment name =
  let global_resolution = GlobalResolution.create global_environment in
  let exit_state_of_define define_node =
    let resolution = resolution global_resolution (module DummyContext) in
    let module Context = struct
      (* Doesn't matter what the qualifier is since we won't be using it *)
      let qualifier = Reference.empty

      let debug = false

      let constraint_solving_style = Configuration.Analysis.default_constraint_solving_style

      let define = define_node

      let resolution_fixpoint = Some (LocalAnnotationMap.empty ())

      let error_map = Some (LocalErrorMap.empty ())

      module Builder = Callgraph.NullBuilder
    end
    in
    exit_state ~resolution (module Context)
  in
  GlobalResolution.define_body global_resolution name
  >>| exit_state_of_define
  >>= (fun { local_annotations; _ } -> local_annotations)
  >>| LocalAnnotationMap.read_only


let errors_from_other_analyses
    ~include_unawaited_awaitable_errors
    ~resolution
    ~local_annotations
    ~qualifier
    ({ Node.value = define; _ } as define_node)
  =
  let uninitialized_local_errors =
    if Define.is_toplevel define then
      []
    else
      UninitializedLocalCheck.check_define ~qualifier define_node
  in
  let unawaited_awaitable_errors =
    if include_unawaited_awaitable_errors && not (Define.is_toplevel define) then
      UnawaitedAwaitableCheck.check_define
        ~resolution
        ~local_annotations:(local_annotations >>| LocalAnnotationMap.read_only)
        ~qualifier
        define_node
    else
      []
  in
  uninitialized_local_errors @ unawaited_awaitable_errors


let check_define
    ~type_check_controls:
      {
        EnvironmentControls.TypeCheckControls.constraint_solving_style;
        include_type_errors;
        include_local_annotations;
        debug;
        include_unawaited_awaitable_errors;
        _;
      }
    ~call_graph_builder:(module Builder : Callgraph.Builder)
    ~resolution
    ~qualifier
    ({ Node.location; value = { Define.signature = { name; _ }; _ } as define } as define_node)
  =
  let errors, local_annotations, callees =
    try
      let module Context = struct
        let qualifier = qualifier

        let debug = debug

        let constraint_solving_style = constraint_solving_style

        let define = define_node

        let resolution_fixpoint = Some (LocalAnnotationMap.empty ())

        let error_map = Some (LocalErrorMap.empty ())

        module Builder = Builder
      end
      in
      let { errors = type_errors; local_annotations; callees; _ } =
        exit_state ~resolution (module Context)
      in
      let errors =
        if include_type_errors then
          Some
            (errors_from_other_analyses
               ~include_unawaited_awaitable_errors
               ~resolution
               ~local_annotations
               ~qualifier
               define_node
            @ type_errors)
        else
          None
      in
      errors, local_annotations, callees
    with
    | ClassHierarchy.Untracked annotation ->
        Statistics.event
          ~name:"undefined type"
          ~integers:[]
          ~normals:
            ["module", Reference.show qualifier; "define", Reference.show name; "type", annotation]
          ();
        if Define.dump define then
          Log.dump "Analysis crashed because of untracked type `%s`." (Log.Color.red annotation);
        let undefined_error =
          Error.create
            ~location:(Location.with_module ~module_reference:qualifier location)
            ~kind:(Error.AnalysisFailure (UnexpectedUndefinedType annotation))
            ~define:define_node
        in
        Some [undefined_error], None, None
  in
  (if not (Define.is_overloaded_function define) then
     let caller =
       if Define.is_property_setter define then
         Callgraph.PropertySetterCaller name
       else
         Callgraph.FunctionCaller name
     in
     Option.iter callees ~f:(fun callees -> Callgraph.set ~caller ~callees));
  let local_annotations =
    if include_local_annotations then
      Some
        (Option.value local_annotations ~default:(LocalAnnotationMap.empty ())
        |> LocalAnnotationMap.read_only)
    else
      None
  in
  { CheckResult.errors; local_annotations }


let check_function_definition
    ~type_check_controls
    ~call_graph_builder
    ~resolution
    ~name
    { FunctionDefinition.body; siblings; qualifier }
  =
  let timer = Timer.start () in

  let check_define = check_define ~type_check_controls ~resolution ~qualifier ~call_graph_builder in
  let sibling_bodies = List.map siblings ~f:(fun { FunctionDefinition.Sibling.body; _ } -> body) in
  let sibling_results = List.map sibling_bodies ~f:(fun define_node -> check_define define_node) in
  let result =
    let aggregate_errors results =
      List.map results ~f:CheckResult.errors
      |> List.fold ~init:(Some []) ~f:(Option.map2 ~f:List.append)
    in
    match body with
    | None -> { CheckResult.errors = aggregate_errors sibling_results; local_annotations = None }
    | Some define_node ->
        let ({ CheckResult.local_annotations; _ } as body_result) = check_define define_node in
        { errors = aggregate_errors (body_result :: sibling_results); local_annotations }
  in

  let number_of_lines =
    let bodies =
      match body with
      | None -> sibling_bodies
      | Some body -> body :: sibling_bodies
    in
    List.fold bodies ~init:0 ~f:(fun sofar body -> sofar + Node.number_of_lines body)
  in
  Statistics.performance
    ~flush:false
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~section:`Check
    ~name:"SingleDefineTypeCheck"
    ~timer
    ~normals:["name", Reference.show name; "request kind", "SingleDefineTypeCheck"]
    ~integers:["number of lines", number_of_lines]
    ();
  result


let check_define_by_name
    ~type_check_controls
    ~call_graph_builder
    ~global_environment
    ~dependency
    name
  =
  let global_resolution = GlobalResolution.create global_environment ?dependency in
  (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
  let resolution = resolution global_resolution (module DummyContext) in
  GlobalResolution.function_definition global_resolution name
  >>| check_function_definition ~type_check_controls ~call_graph_builder ~resolution ~name
