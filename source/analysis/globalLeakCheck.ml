(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Per-function analysis that determines whether a global's been written to. *)

open Core
open Ast
open Expression
open Statement
open Pyre
module Error = AnalysisError

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let append error_map ~statement_key ~error =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val error_map : LocalErrorMap.t
end

module State (Context : Context) = struct
  type t = unit [@@deriving show]

  let less_or_equal ~left:_ ~right:_ = true

  let join _ _ = ()

  let widen ~previous ~next ~iteration:_ = join previous next

  let errors () = Context.error_map |> LocalErrorMap.all_errors

  let known_mutation_methods =
    String.Set.of_list
      [
        "list.append";
        "list.insert";
        "list.extend";
        "dict.setdefault";
        "dict.update";
        "set.add";
        "set.update";
        "set.intersection_update";
        "set.difference_update";
        "set.symmetric_difference_update";
      ]


  let is_known_mutation_method ~resolution expression identifier =
    let is_blocklisted_method () =
      let expression_type =
        Resolution.resolve_expression_to_type resolution expression
        |> Type.class_name
        |> Reference.show
      in
      String.Set.mem known_mutation_methods (expression_type ^ "." ^ identifier)
    in
    String.equal identifier "__setitem__"
    or String.equal identifier "__setattr__"
    or is_blocklisted_method ()


  let rec forward_expression
      ~error_on_global_target
      ~resolution
      ?(is_mutable_expression = false)
      { Node.value; location }
    =
    let forward_expression ?(is_mutable_expression = is_mutable_expression) =
      forward_expression ~error_on_global_target ~is_mutable_expression ~resolution
    in
    let forward_generator { Comprehension.Generator.target; iterator; conditions; _ } =
      forward_expression target;
      forward_expression iterator;
      List.iter ~f:forward_expression conditions
    in
    match value with
    (* interesting cases *)
    | Expression.Name (Name.Identifier target) ->
        if is_mutable_expression then
          error_on_global_target ~location target
    | Name (Name.Attribute { base; attribute; _ }) ->
        let is_mutable_expression =
          is_mutable_expression or is_known_mutation_method ~resolution base attribute
        in
        forward_expression ~is_mutable_expression base
    | Call { callee; arguments } ->
        forward_expression callee;
        List.iter ~f:(fun { value; _ } -> forward_expression value) arguments
    | Expression.Constant _
    | Yield None ->
        ()
    | Await expression
    | Yield (Some expression)
    | YieldFrom expression
    | UnaryOperator { operand = expression; _ }
    | Starred (Once expression)
    | Starred (Twice expression) ->
        forward_expression expression
    | List expressions
    | Set expressions
    | Tuple expressions ->
        List.iter ~f:forward_expression expressions
    | BooleanOperator { left; right; _ }
    | ComparisonOperator { left; right; _ } ->
        forward_expression left;
        forward_expression right
    | WalrusOperator { target; value } ->
        forward_assignment_target ~error_on_global_target ~resolution target;
        forward_expression value
    | Dictionary { entries; keywords } ->
        let forward_entries { Dictionary.Entry.key; value } =
          forward_expression key;
          forward_expression value
        in
        List.iter ~f:forward_entries entries;
        List.iter ~f:forward_expression keywords
    | DictionaryComprehension { element = { key; value }; generators } ->
        forward_expression key;
        forward_expression value;
        List.iter ~f:forward_generator generators
    | Generator { element; generators }
    | ListComprehension { element; generators }
    | SetComprehension { element; generators } ->
        forward_expression element;
        List.iter ~f:forward_generator generators
    | FormatString substrings ->
        let forward_format_string = function
          | Substring.Format expression -> forward_expression expression
          | _ -> ()
        in
        List.iter ~f:forward_format_string substrings
    | Lambda { parameters; body } ->
        let forward_parameters { Node.value = { Parameter.value; _ }; _ } =
          Option.iter ~f:forward_expression value
        in
        List.iter ~f:forward_parameters parameters;
        forward_expression body
    | Ternary { target; test; alternative } ->
        forward_expression test;
        forward_expression target;
        forward_expression alternative


  and forward_assignment_target
      ~error_on_global_target
      ~resolution
      ({ Node.value; location } as expression)
    =
    let forward_assignment_target = forward_assignment_target ~error_on_global_target ~resolution in
    match value with
    | Expression.Name (Name.Identifier target) -> error_on_global_target ~location target
    | Name (Name.Attribute { base; _ }) -> forward_assignment_target base
    | Call _ ->
        forward_expression
          ~resolution
          ~error_on_global_target
          ~is_mutable_expression:true
          expression
    | Constant _
    | UnaryOperator _
    | Await _
    | Yield _
    | Starred (Twice _)
    | YieldFrom _
    | Set _
    | Dictionary _
    | DictionaryComprehension _
    | Generator _
    | ListComprehension _
    | SetComprehension _
    | FormatString _
    | Lambda _
    | Ternary _
    | WalrusOperator _ ->
        ()
    | Starred (Once expression) -> forward_assignment_target expression
    | List expressions
    | Tuple expressions ->
        List.iter ~f:forward_assignment_target expressions
    | BooleanOperator { left; right; _ }
    | ComparisonOperator { left; right; _ } ->
        forward_assignment_target left;
        forward_assignment_target right


  let forward ~statement_key state ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let resolution =
      TypeCheck.resolution_with_key
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in
    let error_on_global_target ~location target =
      let reference = Reference.create target |> Reference.delocalize in
      let is_global = Resolution.is_global resolution ~reference in
      if is_global then
        let error =
          Error.create
            ~location:(Location.with_module ~module_reference:Context.qualifier location)
            ~kind:(Error.GlobalLeak { global_name = reference })
            ~define:Context.define
        in
        LocalErrorMap.append Context.error_map ~statement_key ~error
      else
        ()
    in
    let forward_expression = forward_expression ~resolution ~error_on_global_target in
    match value with
    | Statement.Assert _ -> ()
    | Assign { target; value; _ } ->
        forward_assignment_target ~resolution ~error_on_global_target target;
        forward_expression value
    | Expression expression -> forward_expression expression
    | Raise { expression; from } ->
        Option.iter ~f:forward_expression expression;
        Option.iter ~f:forward_expression from
    | Return { expression = Some expression; _ } -> forward_expression expression
    | Delete _ -> ()
    | Return _ -> ()
    (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
    | If _
    | Class _
    | Define _
    | For _
    | Match _
    | While _
    | With _
    | Try _ ->
        state
    (* Trivial cases. *)
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state


  let backward ~statement_key:_ _ ~statement:_ = ()

  let bottom = ()

  let initial ~global_resolution:_ _ = ()
end

let global_leak_errors ~type_environment ~qualifier define =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in

  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = global_resolution

    let local_annotations =
      TypeEnvironment.TypeEnvironmentReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)


    let error_map = LocalErrorMap.empty ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  Fixpoint.forward ~cfg ~initial:(State.initial ~global_resolution (Node.value define))
  |> Fixpoint.exit
  >>| State.errors
  |> Option.value ~default:[]


(* TODO: Use this function to limit by entrypoints in the future. *)
let should_run_analysis ~type_environment:_ _ = true

let check_define ~type_environment ~qualifier define =
  if should_run_analysis ~type_environment define then
    global_leak_errors ~type_environment ~qualifier define
  else
    []
