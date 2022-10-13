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
end

type callable_data_for_function_call = {
  (* The signature for the function call, after selecting among any overloads. *)
  selected_signature: Type.t Type.Callable.overload;
  (* The return type for the function call, after selecting among any overloads and instantiating
     any type variables. *)
  instantiated_return_type: Type.t;
  function_name: Reference.t option;
}
[@@deriving compare, show, sexp]

(* Return information about callables that `callee_type` could resolve to in a function call.

   For example, if it is a union of callables, then return the individual callables.

   TODO(T130377746): Use the type checking analysis to get the signature that will be selected by
   the function call arguments. This is determined by the type checking analysis, since
   overload-selection depends on the argument types, not their readonlyness. *)
let callable_data_list_for_callee callee_type =
  match callee_type with
  | Type.Callable
      ({ implementation = { annotation; _ } as selected_signature; overloads = []; _ } as callable)
    ->
      [
        {
          selected_signature;
          instantiated_return_type = annotation;
          function_name = Type.Callable.name callable;
        };
      ]
  | _ ->
      (* TODO(T130377746): Extract other types of callables, such as for methods and unions. *)
      []


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
                      name = name >>| Node.value;
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


  let forward_expression ~type_resolution:_ ~resolution { Node.value; _ } =
    let open ReadOnlyness in
    match value with
    | Expression.Constant _ ->
        { Resolved.resolution; errors = []; resolved = ReadOnlyness.ReadOnly }
    | Expression.Name (Name.Identifier identifier) ->
        {
          Resolved.resolution;
          errors = [];
          resolved =
            Resolution.get_opt (Reference.create identifier) resolution
            |> Option.value ~default:Mutable;
        }
    | _ -> failwith "TODO(T130377746)"


  let forward_assignment
      ~type_resolution
      ~resolution
      ~location
      ~target:{ Node.value = target; location = target_location }
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
            resolution, errors
        | None -> resolution, [])
    | _ -> resolution, []


  let forward_statement ~type_resolution ~state ~statement:{ Node.value; location } =
    let resolution = state in
    match value with
    | Statement.Assign { Assign.target; annotation; value } ->
        forward_assignment ~type_resolution ~resolution ~location ~target ~annotation ~value
    | _ -> state, []


  let initial = Resolution.of_list []

  let forward ~statement_key state ~statement =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let type_resolution =
      TypeCheck.resolution_with_key
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
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

let readonly_errors_for_define ~type_environment ~qualifier define =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment

    let error_map = Some (LocalErrorMap.empty ())

    let local_annotations =
      TypeEnvironment.TypeEnvironmentReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Node.value define |> Cfg.create in
  let _state = Fixpoint.forward ~cfg ~initial:State.initial |> Fixpoint.exit in
  Option.value_exn
    ~message:"no error map found in the analysis context"
    (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
