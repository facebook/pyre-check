(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Expression
open Statement
module Error = AnalysisError
module TypeResolution = Resolution

module Resolution = struct
  include
    Abstract.MapDomain.Make
      (struct
        include Reference

        let name = "Reference"

        let absence_implicitly_maps_to_bottom = false
      end)
      (Abstract.SimpleDomain.Make (ReadOnlyness))
end

module Resolved = struct
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]

  let resolved { resolved; _ } = resolved
end

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let set ~statement_key ~errors error_map = Int.Table.set error_map ~key:statement_key ~data:errors

  let append ~statement_key ~error error_map =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val type_resolution_for_statement
    :  global_resolution:GlobalResolution.t ->
    local_annotations:LocalAnnotationMap.ReadOnly.t option ->
    parent:Reference.t option ->
    statement_key:int ->
    TypeResolution.t
end

type callable_data_for_function_call = {
  (* The signature for the function call, after selecting among any overloads. *)
  selected_signature: Type.t Type.Callable.overload;
  (* The return type for the function call, after selecting among any overloads and instantiating
     any type variables. *)
  instantiated_return_type: Type.t;
  function_name: Reference.t option;
  self_readonlyness: ReadOnlyness.t option;
}
[@@deriving compare, show, sexp]

(* Return information about callables that `callee_type` could resolve to in a function call.

   For example, if it is a union of callables, then return the individual callables.

   TODO(T130377746): Use the type checking analysis to get the signature that will be selected by
   the function call arguments. This is determined by the type checking analysis, since
   overload-selection depends on the argument types, not their readonlyness. *)
let callable_data_list_for_callee ?self_readonlyness callee_type =
  match callee_type with
  | Type.Callable
      ({ implementation = { annotation; _ } as selected_signature; overloads = []; _ } as callable)
    ->
      [
        {
          selected_signature;
          instantiated_return_type = annotation;
          function_name = Type.Callable.name callable;
          self_readonlyness;
        };
      ]
  | Parametric
      {
        name = "BoundMethod";
        parameters =
          [
            Single
              (Type.Callable
                ({ implementation = { annotation; _ } as selected_signature; overloads = []; _ } as
                callable));
            Single _self_type;
          ];
      } ->
      [
        {
          selected_signature;
          instantiated_return_type = annotation;
          function_name = Type.Callable.name callable;
          self_readonlyness;
        };
      ]
  | _ ->
      (* TODO(T130377746): Extract other types of callables, such as unions. *)
      []


(* Return a mapping of each parameter to the arguments that will be assigned to it in the function
   call `callable(self_argument, arguments)`. *)
let get_parameter_argument_mapping ~self_argument ~arguments callable =
  let open AttributeResolution in
  match callable with
  | { Type.Callable.parameters = Defined parameters as all_parameters; _ } ->
      SignatureSelection.prepare_arguments_for_signature_selection ~self_argument arguments
      |> SignatureSelection.get_parameter_argument_mapping
           ~parameters
           ~all_parameters
           ~self_argument
      |> Option.some
  | _ ->
      (* TODO(T130377746): Handle non-Defined callables. *)
      None


module State (Context : Context) = struct
  include Resolution

  let create_error ~location kind =
    Error.create
      ~location:(Location.with_module ~module_reference:Context.qualifier location)
      ~kind
      ~define:Context.define


  let add_error ~location ~kind errors = create_error ~location kind :: errors

  let instantiate_path ~global_resolution location =
    let lookup =
      GlobalResolution.module_tracker global_resolution
      |> ModuleTracker.ReadOnly.lookup_relative_path
    in
    let location = Location.with_module ~module_reference:Context.qualifier location in
    Location.WithModule.instantiate ~lookup location


  let widen ~previous ~next ~iteration = widen ~prev:previous ~next ~iteration

  (* Emit errors for arguments that are not compatible with their assigned parameters. *)
  let check_arguments_against_parameters ~function_name parameter_argument_mapping =
    let open Type.Callable in
    let open AttributeResolution in
    let open ReadOnlyness in
    let check_non_variadic_parameter ~parameter_annotation = function
      | Default ->
          (* Assume that any errors about the default parameter value not matching the parameter
             type are emitted when initially checking the define. So, no need to emit an error at
             the function call site. *)
          None
      | MatchedArgument
          { argument = { resolved = actual_readonlyness; position; kind; expression }; _ } ->
          let expected_readonlyness = ReadOnlyness.of_type parameter_annotation in
          if not (less_or_equal ~left:actual_readonlyness ~right:expected_readonlyness) then
            let name =
              match kind with
              | Named name -> Some name
              | _ -> None
            in
            let argument_location =
              expression >>| Node.location |> Option.value ~default:Location.any
            in
            create_error
              ~location:(name >>| Node.location |> Option.value ~default:argument_location)
              (Error.ReadOnlynessMismatch
                 (IncompatibleParameterType
                    {
                      keyword_argument_name = name >>| Node.value;
                      position;
                      callee = function_name;
                      mismatch = { actual = actual_readonlyness; expected = expected_readonlyness };
                    }))
            |> Option.some
          else
            None
    in
    let check_arguments_and_update_signature_match ~parameter ~arguments errors_so_far =
      match parameter, arguments with
      | Parameter.KeywordOnly { annotation = parameter_annotation; _ }, arguments
      | Parameter.PositionalOnly { annotation = parameter_annotation; _ }, arguments
      | Parameter.Keywords parameter_annotation, arguments
      | Parameter.Named { annotation = parameter_annotation; _ }, arguments ->
          List.filter_map arguments ~f:(check_non_variadic_parameter ~parameter_annotation)
          @ errors_so_far
      | _ ->
          (* TODO(T130377746): Handle other kinds of parameters. *)
          errors_so_far
    in
    Map.fold
      ~init:[]
      ~f:(fun ~key ~data ->
        check_arguments_and_update_signature_match ~parameter:key ~arguments:data)
      parameter_argument_mapping


  let rec forward_call ~type_resolution ~resolution ~callee arguments =
    let forward_arguments_in_order ~resolution arguments =
      let forward_argument (resolution, errors, reversed_arguments) argument =
        let expression, kind = Ast.Expression.Call.Argument.unpack argument in
        forward_expression ~type_resolution ~resolution expression
        |> fun { Resolved.resolution; errors = new_errors; resolved } ->
        ( resolution,
          List.append new_errors errors,
          { AttributeResolution.Argument.kind; expression = Some expression; resolved }
          :: reversed_arguments )
      in
      List.fold arguments ~f:forward_argument ~init:(resolution, [], [])
      |> fun (resolution, errors, reversed_arguments) ->
      resolution, errors, List.rev reversed_arguments
    in
    let errors_for_callable ~arguments { selected_signature; function_name; self_readonlyness; _ } =
      get_parameter_argument_mapping ~self_argument:self_readonlyness ~arguments selected_signature
      >>| (fun { AttributeResolution.ParameterArgumentMapping.parameter_argument_mapping; _ } ->
            parameter_argument_mapping)
      >>| check_arguments_against_parameters ~function_name
      |> Option.value ~default:[]
    in
    let resolution, base_errors, callable_data_list =
      (* TODO(T130377746): Handle first-class callables, since they would not be of the form
         `x.my_method()`. *)
      match TypeResolution.resolve_expression_to_type type_resolution callee, callee with
      | ( (Parametric { name = "BoundMethod"; parameters = [Single (Type.Callable _); Single _] } as
          callee_type),
          { Node.value = Expression.Name (Name.Attribute { base; _ }); _ } ) ->
          let { Resolved.resolved; resolution; errors } =
            forward_expression ~type_resolution ~resolution base
          in
          resolution, errors, callable_data_list_for_callee ~self_readonlyness:resolved callee_type
      | callee_type, _ -> resolution, [], callable_data_list_for_callee callee_type
    in
    let resolution, argument_errors, arguments = forward_arguments_in_order ~resolution arguments in
    let return_type =
      List.fold
        callable_data_list
        ~init:ReadOnlyness.bottom
        ~f:(fun sofar { instantiated_return_type; _ } ->
          ReadOnlyness.of_type instantiated_return_type |> ReadOnlyness.join sofar)
    in
    {
      Resolved.errors =
        List.concat_map callable_data_list ~f:(errors_for_callable ~arguments)
        @ base_errors
        @ argument_errors;
      resolution;
      resolved = return_type;
    }


  and forward_expression ~type_resolution ~resolution ({ Node.value; _ } as expression) =
    let open ReadOnlyness in
    match value with
    | Expression.Constant _ ->
        (* Even though constants are technically readonly, marking them as readonly would emit
           spurious errors when assigning them to mutable variables. So, mark them as mutable. *)
        { Resolved.resolution; errors = []; resolved = Mutable }
    | Expression.Name (Name.Identifier identifier) ->
        {
          Resolved.resolution;
          errors = [];
          resolved =
            Resolution.get_opt (Reference.create identifier) resolution
            |> Option.value ~default:Mutable;
        }
    | Name (Name.Attribute { base; _ }) ->
        let { Resolved.errors; resolved = resolved_base; resolution } =
          forward_expression ~type_resolution ~resolution base
        in
        let resolved =
          match resolved_base with
          | ReadOnly ->
              (* If the base expression is readonly, the attribute is automatically readonly. *)
              ReadOnly
          | Mutable ->
              TypeResolution.resolve_expression_to_type type_resolution expression
              |> ReadOnlyness.of_type
        in
        { Resolved.resolved; errors; resolution }
    | Call
        {
          callee = { Node.location; value = Name (Name.Identifier "reveal_type") };
          arguments = { Call.Argument.value; _ } :: _;
        } ->
        let error =
          Error.ReadOnlynessMismatch
            (RevealedType
               {
                 expression = value;
                 readonlyness =
                   forward_expression ~type_resolution ~resolution value |> Resolved.resolved;
               })
          |> create_error ~location
        in
        { Resolved.resolved = Mutable; errors = [error]; resolution }
    | Call { callee; arguments } -> forward_call ~type_resolution ~resolution ~callee arguments
    | FormatString substrings ->
        let forward_substring ((resolution, errors) as sofar) = function
          | Substring.Literal _ -> sofar
          | Substring.Format expression ->
              let { Resolved.resolution; errors = new_errors; _ } =
                forward_expression ~type_resolution ~resolution expression
              in
              resolution, List.append new_errors errors
        in
        let resolution, errors = List.fold substrings ~f:forward_substring ~init:(resolution, []) in
        (* Format strings can be assigned to either readonly or mutable variables. So, their
           readonlyness is bottom (i.e., Mutable). *)
        { Resolved.resolved = Mutable; errors; resolution }
    | _ ->
        (* TODO(T130377746): Actually handle other expressions. *)
        { Resolved.resolved = Mutable; errors = []; resolution }


  let forward_assignment
      ~type_resolution
      ~resolution
      ~location
      ~target:({ Node.value = target; location = target_location } as target_expression)
      ~annotation
      ~value
    =
    (* TODO(T130377746): Handle other Assign targets. *)
    match target with
    | Expression.Name target -> (
        match name_to_reference target with
        | Some name ->
            let { Resolved.resolved = actual_readonlyness; errors; resolution } =
              forward_expression ~type_resolution ~resolution value
            in
            let expected_readonlyness =
              annotation
              >>| GlobalResolution.parse_annotation Context.global_resolution
              >>| ReadOnlyness.of_type
            in
            let resolution =
              Resolution.set
                resolution
                ~key:name
                ~data:(Option.value ~default:actual_readonlyness expected_readonlyness)
            in
            let errors =
              match expected_readonlyness with
              | Some expected_readonlyness
                when not
                       (ReadOnlyness.less_or_equal
                          ~left:actual_readonlyness
                          ~right:expected_readonlyness) ->
                  add_error
                    ~location
                    ~kind:
                      (Error.ReadOnlynessMismatch
                         (IncompatibleVariableType
                            {
                              incompatible_type =
                                {
                                  name;
                                  mismatch =
                                    {
                                      actual = actual_readonlyness;
                                      expected = expected_readonlyness;
                                    };
                                };
                              declare_location =
                                instantiate_path
                                  ~global_resolution:Context.global_resolution
                                  target_location;
                            }))
                    errors
              | _ -> errors
            in
            let assignment_errors, resolution =
              let { Resolved.resolved = target_readonlyness; errors = target_errors; resolution } =
                forward_expression ~type_resolution ~resolution target_expression
              in
              match target, target_readonlyness with
              | Name.Attribute { attribute; _ }, ReadOnly
                when not (Define.is_class_toplevel (Node.value Context.define)) ->
                  ( add_error
                      ~location:target_location
                      ~kind:
                        (Error.ReadOnlynessMismatch
                           (AssigningToReadOnlyAttribute { attribute_name = attribute }))
                      target_errors,
                    resolution )
              | _ -> target_errors, resolution
            in
            resolution, assignment_errors @ errors
        | None -> resolution, [])
    | _ -> resolution, []


  let forward_statement ~type_resolution ~state ~statement:{ Node.value; location } =
    let resolution = state in
    match value with
    | Statement.Assign { Assign.target; annotation; value } ->
        forward_assignment ~type_resolution ~resolution ~location ~target ~annotation ~value
    | Expression expression ->
        let { Resolved.errors; resolution; _ } =
          forward_expression ~type_resolution ~resolution expression
        in
        resolution, errors
    | _ -> state, []


  let check_parameter_annotations ~type_resolution ~resolution parameters =
    let make_parameter_name name =
      name
      (* Hack: The qualifier name will be `$parameter$*args`, so we just filter stars. *)
      |> String.filter ~f:(function
             | '*' -> false
             | _ -> true)
      |> Reference.create
    in
    let add_parameter_as_local_variable
        (resolution_with_parameters, errors)
        { Node.location; value = { Parameter.name; value; annotation } }
      =
      let annotation_readonlyness =
        annotation
        >>| GlobalResolution.parse_annotation
              (TypeResolution.global_resolution type_resolution)
              ~validation:NoValidation
        >>| ReadOnlyness.of_type
      in
      let value_readonlyness, errors =
        match value with
        | Some value ->
            let { Resolved.resolved; errors = value_errors; _ } =
              (* NOTE: We use the original `resolution`, because `resolution_with_parameters` has
                 the earlier parameters in scope. We don't want to consider them in scope when
                 resolving the default value. *)
              forward_expression ~type_resolution ~resolution value
            in
            Some resolved, value_errors @ errors
        | None -> None, errors
      in
      let errors =
        match annotation_readonlyness, value_readonlyness with
        | Some annotation_readonlyness, Some value_readonlyness
          when not
                 (ReadOnlyness.less_or_equal
                    ~left:value_readonlyness
                    ~right:annotation_readonlyness) ->
            add_error
              ~location
              ~kind:
                (Error.ReadOnlynessMismatch
                   (IncompatibleVariableType
                      {
                        incompatible_type =
                          {
                            name = Reference.create name;
                            mismatch =
                              { actual = value_readonlyness; expected = annotation_readonlyness };
                          };
                        declare_location =
                          instantiate_path ~global_resolution:Context.global_resolution location;
                      }))
              errors
        | _ -> errors
      in
      ( Resolution.set
          resolution_with_parameters
          ~key:(make_parameter_name name)
          ~data:(Option.value ~default:ReadOnlyness.bottom annotation_readonlyness),
        errors )
    in
    List.fold ~init:(resolution, []) ~f:add_parameter_as_local_variable parameters


  let initial =
    let { Node.value = { Define.signature = { parent; parameters; _ }; _ }; _ } = Context.define in
    let dummy_statement_key = 0 in
    let type_resolution =
      Context.type_resolution_for_statement
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key:dummy_statement_key
    in
    check_parameter_annotations ~type_resolution ~resolution:(Resolution.of_list []) parameters


  let forward ~statement_key state ~statement =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let type_resolution =
      Context.type_resolution_for_statement
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
    in
    let new_state, errors = forward_statement ~type_resolution ~state ~statement in
    let () =
      let _ = Context.error_map >>| LocalErrorMap.set ~statement_key ~errors in
      ()
    in
    new_state


  let backward ~statement_key:_ _state ~statement:_ =
    failwith "Not implementing this for readonly analysis"
end

let readonly_errors_for_define
    ~type_resolution_for_statement
    ~global_resolution
    ~local_annotations
    ~qualifier
    define
  =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = global_resolution

    let error_map = Some (LocalErrorMap.empty ())

    let local_annotations = local_annotations

    let type_resolution_for_statement = type_resolution_for_statement
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Node.value define |> Cfg.create in
  let resolution, initial_errors = State.initial in
  let _state = Fixpoint.forward ~cfg ~initial:resolution |> Fixpoint.exit in
  let errors =
    Option.value_exn
      ~message:"no error map found in the analysis context"
      (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
  in
  initial_errors @ errors
