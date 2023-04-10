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

  val get_non_builtin_global_reference
    :  resolution:Resolution.t ->
    Ast.Expression.Name.t ->
    Reference.t option
end

module State (Context : Context) = struct
  type t = unit [@@deriving show]

  type reachable_global = {
    global: Reference.t;
    expression_type: Type.t;
  }

  type leaked_global = {
    reference: Reference.t;
    location: Location.t;
  }

  type result = {
    (* represents the list of globals from the current expression and sub-expression that will be
       mutated if a wrapping statement or expression is a known mutation (i.e. the globals that will
       be mutated if the wrapping expression is a known mutable method or wrapping statement is an
       assignment) *)
    reachable_globals: reachable_global list;
    (* represents the list of reachable globals and their locations that have been confirmed to
       result in a mutation *)
    errors: leaked_global list;
  }

  let append_errors_for_globals ~location globals errors =
    List.map ~f:(fun { global; _ } -> { reference = global; location }) globals @ errors


  let empty_result = { reachable_globals = []; errors = [] }

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


  let rec forward_expression ~resolution ({ Node.value; location } as expression) =
    let forward_expression = forward_expression ~resolution in
    let forward_generator { Comprehension.Generator.target; iterator; conditions; _ } =
      let { errors = target_errors; _ } = forward_expression target in
      let { errors = iterator_errors; _ } = forward_expression iterator in
      let condition_errors =
        List.concat_map
          ~f:(fun expression ->
            let { errors; _ } = forward_expression expression in
            errors)
          conditions
      in
      target_errors @ iterator_errors @ condition_errors
    in
    let append_errors_for_globals = append_errors_for_globals ~location in
    let expression_type () = Resolution.resolve_expression_to_type resolution expression in
    match value with
    (* interesting cases *)
    | Expression.Name (Name.Identifier _ as name) ->
        Context.get_non_builtin_global_reference ~resolution name
        >>| (fun global ->
              {
                reachable_globals = [{ global; expression_type = expression_type () }];
                errors = [];
              })
        |> Option.value ~default:empty_result
    | Name (Name.Attribute { base; attribute; _ } as name) ->
        let ({ reachable_globals; errors } as sub_expression_result) = forward_expression base in
        let reachable_globals =
          Context.get_non_builtin_global_reference ~resolution name
          >>| (fun global -> { global; expression_type = expression_type () } :: reachable_globals)
          |> Option.value ~default:reachable_globals
        in
        if is_known_mutation_method ~resolution base attribute then
          { reachable_globals = []; errors = append_errors_for_globals reachable_globals errors }
        else
          { sub_expression_result with reachable_globals }
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { attribute = "__setattr__"; _ }); _ };
          arguments = [{ Call.Argument.value = object_; _ }; _; { Call.Argument.value; _ }];
        }
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "setattr"); _ };
          arguments = [{ Call.Argument.value = object_; _ }; _; { Call.Argument.value; _ }];
        } ->
        (* Adds special casing for `<anything>.__setattr__(...)` and `setattr(...)` to error if the
           first argument (the object) has a reachable global or a mutation occurs in the third
           argument (the value). These need to be special cased since we want to error on specific
           arguments having reachable globals rather than the callee of the expression. *)
        let { reachable_globals; errors } = forward_expression object_ in
        (* TODO (T142189949): forward reachable globals for value if assigning globals is
           disallowed *)
        let { errors = value_errors; _ } = forward_expression value in
        {
          empty_result with
          errors = append_errors_for_globals reachable_globals (value_errors @ errors);
        }
    | Call { callee; arguments } ->
        let { errors; reachable_globals } = forward_expression callee in
        let reachable_globals =
          match callee with
          | { Node.value = Expression.Name (Name.Attribute { attribute = "__getitem__"; _ }); _ } ->
              reachable_globals
          (* if this call is `__getitem__`, assume a mutation on the return value mutates reachable
             globals (i.e. `my_global[5].append(3)` would be a mutation) *)
          | _ -> (
              let resolved_expression_type = expression_type () in
              match Type.extract_meta resolved_expression_type with
              | Some class_name ->
                  (* if this expression (the result of the call) returns a class reference/type,
                     then treat it as a global (i.e. `get_class().x = 5` for `def get_class() ->
                     Type[MyClass]: ...` is a global mutation) *)
                  [
                    {
                      global = Type.class_name class_name;
                      expression_type = resolved_expression_type;
                    };
                  ]
              | _ -> [])
        in
        let get_errors_from_forward_expression { Call.Argument.value; _ } =
          let { errors; _ } = forward_expression value in
          errors
        in
        List.concat_map ~f:get_errors_from_forward_expression arguments
        |> fun argument_errors -> { errors = argument_errors @ errors; reachable_globals }
    | Expression.Constant _
    | Yield None ->
        empty_result
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
        let errors =
          List.concat_map
            ~f:(fun expression ->
              let { errors; _ } = forward_expression expression in
              errors)
            expressions
        in
        { empty_result with errors }
    | BooleanOperator { left; right; _ }
    | ComparisonOperator { left; right; _ } ->
        let { errors = left_errors; _ } = forward_expression left in
        let { errors = right_errors; _ } = forward_expression right in
        { empty_result with errors = left_errors @ right_errors }
    | WalrusOperator { target; value } ->
        let { reachable_globals; errors } = forward_assignment_target ~resolution target in
        let { reachable_globals = value_globals; errors = value_errors } =
          forward_expression value
        in
        (* We keep the value_globals as reachable globals since they can immediately be written to
           outside of the walrus expression, causing a global leak. *)
        {
          reachable_globals = value_globals;
          errors = append_errors_for_globals reachable_globals (value_errors @ errors);
        }
    | Dictionary { entries; keywords } ->
        let forward_entries { Dictionary.Entry.key; value } =
          let { errors = key_errors; _ } = forward_expression key in
          let { errors = value_errors; _ } = forward_expression value in
          key_errors @ value_errors
        in
        let entry_errors = List.concat_map ~f:forward_entries entries in
        let keyword_errors =
          List.concat_map
            ~f:(fun expression ->
              let { errors; _ } = forward_expression expression in
              errors)
            keywords
        in
        { empty_result with errors = entry_errors @ keyword_errors }
    | DictionaryComprehension { element = { key; value }; generators } ->
        let { errors = key_errors; _ } = forward_expression key in
        let { errors = value_errors; _ } = forward_expression value in
        let generator_errors = List.concat_map ~f:forward_generator generators in
        { empty_result with errors = key_errors @ value_errors @ generator_errors }
    | Generator { element; generators }
    | ListComprehension { element; generators }
    | SetComprehension { element; generators } ->
        let { errors = element_errors; _ } = forward_expression element in
        let generator_errors = List.concat_map ~f:forward_generator generators in
        { empty_result with errors = element_errors @ generator_errors }
    | FormatString substrings ->
        let forward_format_string = function
          | Substring.Format expression ->
              let { errors; _ } = forward_expression expression in
              errors
          | _ -> []
        in
        let errors = List.concat_map ~f:forward_format_string substrings in
        { empty_result with errors }
    | Lambda { parameters; body } ->
        let forward_parameters { Node.value = { Parameter.value; _ }; _ } =
          value
          >>| (fun expression -> forward_expression expression |> fun { errors; _ } -> errors)
          |> Option.value ~default:[]
        in
        let parameter_errors = List.concat_map ~f:forward_parameters parameters in
        let { errors = body_errors; _ } = forward_expression body in
        { empty_result with errors = body_errors @ parameter_errors }
    | Ternary { target; test; alternative } ->
        let { errors = test_errors; _ } = forward_expression test in
        let { reachable_globals = target_globals; errors = target_errors } =
          forward_expression target
        in
        let { reachable_globals = alternative_globals; errors = alternative_errors } =
          forward_expression alternative
        in
        {
          reachable_globals = target_globals @ alternative_globals;
          errors = test_errors @ target_errors @ alternative_errors;
        }


  and forward_assignment_target ~resolution ({ Node.value; _ } as expression) =
    let forward_assignment_target = forward_assignment_target ~resolution in
    let expression_type () = Resolution.resolve_expression_to_type resolution expression in
    match value with
    | Expression.Name (Name.Identifier _ as name) ->
        Context.get_non_builtin_global_reference ~resolution name
        >>| (fun global ->
              {
                reachable_globals = [{ global; expression_type = expression_type () }];
                errors = [];
              })
        |> Option.value ~default:empty_result
    | Name (Name.Attribute { base; _ } as name) ->
        let ({ reachable_globals = base_globals; _ } as base_result) =
          forward_assignment_target base
        in
        Context.get_non_builtin_global_reference ~resolution name
        >>| (fun global ->
              {
                base_result with
                reachable_globals = { global; expression_type = expression_type () } :: base_globals;
              })
        |> Option.value ~default:base_result
    | Call _ -> forward_expression ~resolution expression
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
    | BooleanOperator _
    | ComparisonOperator _
    | Ternary _
    | WalrusOperator _ ->
        empty_result
    | Starred (Once expression) -> forward_assignment_target expression
    | List expressions
    | Tuple expressions ->
        let fold_sub_expression_targets { reachable_globals; errors } expression =
          let { reachable_globals = expression_globals; errors = expression_errors } =
            forward_assignment_target expression
          in
          {
            reachable_globals = reachable_globals @ expression_globals;
            errors = expression_errors @ errors;
          }
        in
        List.fold ~init:empty_result ~f:fold_sub_expression_targets expressions


  and forward_assert ~resolution ?(origin = Assert.Origin.Assertion) test =
    (* Ignore global errors from the [assert (not foo)] in the else-branch because it's the same
       [foo] as in the true-branch. We can either ignore it here or de-duplicate it in the error
       map. We ignore it here instead. *)
    match origin with
    | Assert.Origin.If { true_branch = false; _ }
    | Assert.Origin.While { true_branch = false; _ } ->
        empty_result
    | _ -> forward_expression ~resolution test


  let forward ~statement_key _ ~statement:{ Node.value; location } =
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
    let prepare_globals_for_errors = append_errors_for_globals ~location in
    let module_reference =
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
      get_module_qualifier (Context.qualifier |> Reference.delocalize)
    in
    let emit_error_for_global { reference; location } =
      let target_type = Resolution.resolve_reference resolution reference in
      let delocalized_reference = Reference.delocalize reference in
      let error =
        Error.create
          ~location:(Location.with_module ~module_reference location)
          ~kind:
            (Error.GlobalLeak { global_name = delocalized_reference; global_type = target_type })
          ~define:Context.define
      in
      LocalErrorMap.append Context.error_map ~statement_key ~error
    in
    let resulting_errors =
      match value with
      | Statement.Assert { test; origin; _ } ->
          let { errors; _ } = forward_assert ~resolution ~origin test in
          errors
      | Assign { target; value; _ } ->
          let { reachable_globals; errors } = forward_assignment_target ~resolution target in
          let { errors = value_errors; _ } = forward_expression ~resolution value in
          prepare_globals_for_errors reachable_globals (value_errors @ errors)
      | Expression expression ->
          let { errors; _ } = forward_expression ~resolution expression in
          errors
      | Raise { expression; from } ->
          let get_errors expression =
            expression
            >>| (fun expression ->
                  forward_expression ~resolution expression |> fun { errors; _ } -> errors)
            |> Option.value ~default:[]
          in
          get_errors expression @ get_errors from
      | Return { expression = Some expression; _ } ->
          let { errors; reachable_globals } = forward_expression ~resolution expression in
          let reachable_globals =
            let is_safe_global { expression_type; _ } = not (Type.is_meta expression_type) in
            List.filter ~f:is_safe_global reachable_globals
          in
          prepare_globals_for_errors reachable_globals errors
      | Delete _
      | Return _ ->
          []
      (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
      | If _
      | Class _
      | Define _
      | For _
      | Match _
      | While _
      | With _
      | Try _ ->
          []
      (* Trivial cases. *)
      | Break
      | Continue
      | Global _
      | Import _
      | Nonlocal _
      | Pass ->
          []
    in
    List.iter ~f:emit_error_for_global resulting_errors


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
      if Resolution.is_global resolution ~reference || is_global_in_scope () then
        Some reference
      else
        None


    let get_non_builtin_global_reference ~resolution name =
      match name with
      | Name.Identifier target
      | Name.Attribute { attribute = target; _ } ->
          if Scope.Builtins.mem target then
            None
          else
            Ast.Expression.name_to_reference name >>| is_global ~resolution |> Option.join
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
