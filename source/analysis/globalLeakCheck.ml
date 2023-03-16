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

  val is_global : resolution:Resolution.t -> Reference.t -> bool
end

module State (Context : Context) = struct
  type t = unit [@@deriving show]

  let less_or_equal ~left:_ ~right:_ = true

  let join _ _ = ()

  let widen ~previous ~next ~iteration:_ = join previous next

  let errors () = Context.error_map |> LocalErrorMap.all_errors

  let mutation_methods_and_types =
    String.Map.of_alist_exn
      [
        "list", String.Set.of_list ["append"; "insert"; "extend"];
        "dict", String.Set.of_list ["setdefault"; "update"];
        ( "set",
          String.Set.of_list
            [
              "add";
              "update";
              "intersection_update";
              "difference_update";
              "symmetric_difference_update";
            ] );
      ]


  let mutation_methods = String.Map.data mutation_methods_and_types |> String.Set.union_list

  let is_known_mutation_method ~resolution expression identifier =
    let is_blocklisted_method () =
      let expression_type = Resolution.resolve_expression_to_type resolution expression in
      match expression_type with
      | Type.Parametric { name; _ } ->
          String.Map.find mutation_methods_and_types name
          >>| (fun methods -> String.Set.mem methods identifier)
          |> Option.value ~default:false
      | Type.Top
      | Type.Any ->
          String.Set.mem mutation_methods identifier
      | _ -> false
    in
    String.equal identifier "__setitem__"
    || String.equal identifier "__setattr__"
    || is_blocklisted_method ()


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
    | Expression.Name (Name.Identifier target as name) ->
        if is_mutable_expression && not (Scope.Builtins.mem target) then
          ignore (error_on_global_target ~location name)
    | Name (Name.Attribute { base; attribute; _ } as name) ->
        let error_emitted = is_mutable_expression && error_on_global_target ~location name in
        if not error_emitted then
          let is_mutable_expression =
            is_mutable_expression || is_known_mutation_method ~resolution base attribute
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
        ignore (forward_assignment_target ~error_on_global_target ~resolution target);
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
    | Expression.Name (Name.Identifier target as name) ->
        if not (Scope.Builtins.mem target) then
          ignore (error_on_global_target ~location name)
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


  and forward_assert ~resolution ~error_on_global_target ?(origin = Assert.Origin.Assertion) test =
    (* Ignore global errors from the [assert (not foo)] in the else-branch because it's the same
       [foo] as in the true-branch. We can either ignore it here or de-duplicate it in the error
       map. We ignore it here instead. *)
    match origin with
    | Assert.Origin.If { true_branch = false; _ }
    | Assert.Origin.While { true_branch = false; _ } ->
        ()
    | _ -> forward_expression ~resolution ~error_on_global_target test


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
      match Ast.Expression.name_to_reference target with
      | None -> false
      | Some reference ->
          let rec get_module_qualifier qualifier =
            let module_tracker = GlobalResolution.module_tracker Context.global_resolution in
            let qualifier_prefix = Reference.prefix qualifier in
            match
              ModuleTracker.ReadOnly.is_module_tracked module_tracker qualifier, qualifier_prefix
            with
            (* we couldn't find a module from the given qualifier *)
            | _, None -> Reference.empty
            (* we found the module *)
            | true, _ -> qualifier
            (* we haven't found a module yet *)
            | _, Some prefix -> get_module_qualifier prefix
          in
          let is_global = Context.is_global ~resolution reference in
          (if is_global then
             let target_type = Resolution.resolve_reference resolution reference in
             let delocalized_reference = Reference.delocalize reference in
             let error =
               Error.create
                 ~location:
                   (Location.with_module
                      ~module_reference:
                        (get_module_qualifier (Context.qualifier |> Reference.delocalize))
                      location)
                 ~kind:
                   (Error.GlobalLeak
                      { global_name = delocalized_reference; global_type = target_type })
                 ~define:Context.define
             in
             LocalErrorMap.append Context.error_map ~statement_key ~error);
          is_global
    in
    let forward_expression = forward_expression ~resolution ~error_on_global_target in
    match value with
    | Statement.Assert { test; origin; _ } ->
        forward_assert ~resolution ~error_on_global_target ~origin test
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
  let scope = Scope.Scope.of_define (Node.value define) in

  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = global_resolution

    let local_annotations =
      TypeEnvironment.TypeEnvironmentReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)


    let error_map = LocalErrorMap.empty ()

    let is_global ~resolution reference =
      let reference = Reference.delocalize reference in
      let is_global_in_scope () =
        scope
        >>| (fun { Scope.Scope.globals; _ } ->
              let sanitized_identifier = Identifier.sanitized (Reference.last reference) in
              Identifier.Set.mem globals sanitized_identifier)
        |> Option.value ~default:false
      in
      (* We're using `Resolution.is_global` to detect global reads on references, even if the
         `global` keyword isn't used within the callable. `Scope.globals` is used here as a backup,
         for the case where the global keyword is used but `Resolution.is_global` fails to determine
         if the reference is a global. *)
      Resolution.is_global resolution ~reference || is_global_in_scope ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  Fixpoint.forward ~cfg ~initial:(State.initial ~global_resolution (Node.value define))
  |> Fixpoint.exit
  >>| State.errors
  |> Option.value ~default:[]


let check_qualifier ~type_environment qualifier =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  match GlobalResolution.define_body global_resolution qualifier with
  | Some define -> Some (global_leak_errors ~type_environment ~qualifier define)
  | None ->
      (* assume the target is a nested definition and see if we can find it by performing name
         mangling *)
      Reference.prefix qualifier
      >>| (fun prefix ->
            let qualifier =
              Preprocessing.qualify_local_identifier ~qualifier:prefix (Reference.last qualifier)
              |> Reference.create
            in
            GlobalResolution.define_body global_resolution qualifier
            >>| global_leak_errors ~type_environment ~qualifier)
      |> Option.join
