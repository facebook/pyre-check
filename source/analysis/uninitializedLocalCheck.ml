(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
open Expression
module Error = AnalysisError

let name = "UninitializedLocal"

module NameAccessSet = Set.Make (Define.NameAccess)

module AccessCollector = struct
  let rec from_expression collected { Node.value; location = expression_location } =
    let open Expression in
    let from_entry collected { Dictionary.Entry.key; value } =
      let collected = from_expression collected key in
      from_expression collected value
    in
    match value with
    (* Lambdas are speical -- they bind their own names, which we want to exclude *)
    | Lambda { Lambda.parameters; body } ->
        let collected =
          let from_parameter collected { Node.value = { Parameter.value; _ }; _ } =
            Option.value_map value ~f:(from_expression collected) ~default:collected
          in
          List.fold parameters ~init:collected ~f:from_parameter
        in
        let bound_names =
          List.map parameters ~f:(fun { Node.value = { Parameter.name; _ }; _ } ->
              Identifier.split_star name |> snd)
          |> Identifier.Set.of_list
        in
        let names_in_body = from_expression NameAccessSet.empty body in
        let unbound_names_in_body =
          Set.filter names_in_body ~f:(fun { Define.NameAccess.name; _ } ->
              not (Identifier.Set.mem bound_names name))
        in
        Set.union unbound_names_in_body collected
    | Name (Name.Identifier identifier) ->
        (* For simple names, add them to the result *)
        Set.add collected { Define.NameAccess.name = identifier; location = expression_location }
    | Name (Name.Attribute _) ->
        (* For attribute access, we currently skip *)
        collected
    (* The rest is boilerplates to make sure that expressions are visited recursively *)
    | Await await -> from_expression collected await
    | BooleanOperator { BooleanOperator.left; right; _ }
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let collected = from_expression collected left in
        from_expression collected right
    | Call { Call.callee; arguments } ->
        let collected = from_expression collected callee in
        List.fold arguments ~init:collected ~f:(fun collected { Call.Argument.value; _ } ->
            from_expression collected value)
    | Dictionary { Dictionary.entries; keywords } ->
        let collected = List.fold entries ~init:collected ~f:from_entry in
        List.fold keywords ~init:collected ~f:from_expression
    | DictionaryComprehension comprehension -> from_comprehension from_entry collected comprehension
    | Generator comprehension
    | ListComprehension comprehension
    | SetComprehension comprehension ->
        from_comprehension from_expression collected comprehension
    | List expressions
    | Set expressions
    | Tuple expressions ->
        List.fold expressions ~init:collected ~f:from_expression
    | FormatString substrings ->
        let from_substring sofar = function
          | Substring.Literal _ -> sofar
          | Substring.Format expression -> from_expression sofar expression
        in
        List.fold substrings ~init:collected ~f:from_substring
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        from_expression collected expression
    | Ternary { Ternary.target; test; alternative } ->
        let collected = from_expression collected target in
        let collected = from_expression collected test in
        from_expression collected alternative
    | UnaryOperator { UnaryOperator.operand; _ } -> from_expression collected operand
    | WalrusOperator { WalrusOperator.value; _ } -> from_expression collected value
    | Yield yield -> Option.value_map yield ~default:collected ~f:(from_expression collected)
    | YieldFrom yield -> from_expression collected yield
    | Constant _ -> collected


  (* Generators are as special as lambdas -- they bind their own names, which we want to exclude *)
  and from_comprehension :
        'a.
        (NameAccessSet.t -> 'a -> NameAccessSet.t) ->
        NameAccessSet.t ->
        'a Comprehension.t ->
        NameAccessSet.t
    =
   fun from_element collected { Comprehension.element; generators } ->
    let remove_bound_names ~bound_names =
      Set.filter ~f:(fun { Define.NameAccess.name; _ } -> not (Identifier.Set.mem bound_names name))
    in
    let bound_names, collected =
      let from_generator
          (bound_names, accesses_sofar)
          { Comprehension.Generator.target; iterator; conditions; _ }
        =
        let iterator_accesses =
          from_expression NameAccessSet.empty iterator |> remove_bound_names ~bound_names
        in
        let bound_names =
          let add_bound_name bound_names { Define.NameAccess.name; _ } = Set.add bound_names name in
          from_expression NameAccessSet.empty target
          |> NameAccessSet.fold ~init:bound_names ~f:add_bound_name
        in
        let condition_accesses =
          List.fold conditions ~init:NameAccessSet.empty ~f:from_expression
          |> remove_bound_names ~bound_names
        in
        ( bound_names,
          NameAccessSet.union_list [accesses_sofar; iterator_accesses; condition_accesses] )
      in
      List.fold generators ~init:(Identifier.Set.empty, collected) ~f:from_generator
    in
    let element_accesses =
      from_element NameAccessSet.empty element |> remove_bound_names ~bound_names
    in
    NameAccessSet.union collected element_accesses
end

let extract_reads_expression expression =
  let name_access_to_identifier_node { Define.NameAccess.name; location } =
    { Node.value = name; location }
  in
  AccessCollector.from_expression NameAccessSet.empty expression
  |> NameAccessSet.to_list
  |> List.map ~f:name_access_to_identifier_node


let extract_reads_statement { Node.value; _ } =
  let expressions =
    match value with
    | Statement.Assign { Assign.value = expression; _ }
    | Expression expression
    | If { If.test = expression; _ }
    | While { While.test = expression; _ } ->
        [expression]
    | Delete expressions -> expressions
    | Assert { Assert.test; message; _ } -> [test] @ Option.to_list message
    | For { For.target; iterator; _ } -> [target; iterator]
    | Raise { Raise.expression; from } -> Option.to_list expression @ Option.to_list from
    | Return { Return.expression; _ } -> Option.to_list expression
    | With { With.items; _ } -> items |> List.map ~f:(fun (value, _) -> value)
    | Break
    | Class _
    | Continue
    | Define _
    | Global _
    | Import _
    (* TODO(T107105911): Handle access for match statement. *)
    | Match _
    | Nonlocal _
    | Pass
    | Try _ ->
        []
  in
  expressions |> List.concat_map ~f:extract_reads_expression


module InitializedVariables = Identifier.Set

module type Context = sig
  val fixpoint_post_statement : (Statement.t * InitializedVariables.t) Int.Table.t
end

module State (Context : Context) = struct
  type t =
    | Bottom
    | Value of InitializedVariables.t

  let show = function
    | Bottom -> "[]"
    | Value state ->
        state |> InitializedVariables.elements |> String.concat ~sep:", " |> Format.sprintf "[%s]"


  let bottom = Bottom

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define:{ Node.value = { Define.signature; _ }; _ } =
    signature.parameters
    |> Scope.Binding.of_parameters []
    |> List.map ~f:Scope.Binding.name
    |> List.map ~f:Identifier.sanitized
    |> InitializedVariables.of_list
    |> fun value -> Value value


  let errors ~qualifier ~define _ =
    let emit_error { Node.value; location } =
      Error.create
        ~location:(Location.with_module ~qualifier location)
        ~kind:(Error.UninitializedLocal value)
        ~define
    in
    let all_locals =
      let { Scope.Scope.bindings; globals; nonlocals; _ } =
        Scope.Scope.of_define_exn define.value
      in
      (* Santitization is needed to remove (some) scope information that is (sometimes, but not
         consistently) added into the identifiers themselves (e.g. $local_test?f$y). *)
      let locals =
        Identifier.Map.keys bindings |> List.map ~f:Identifier.sanitized |> Identifier.Set.of_list
      in
      (* This operation needs to be repeated as Scope doesn't know about qualification, and hence
         doesn't remove all globals and nonlocals from bindings *)
      let globals = Identifier.Set.map ~f:Identifier.sanitized globals in
      let nonlocals = Identifier.Set.map ~f:Identifier.sanitized nonlocals in
      Identifier.Set.diff (Identifier.Set.diff locals globals) nonlocals
    in
    let in_local_scope { Node.value = identifier; _ } =
      identifier |> Identifier.sanitized |> Identifier.Set.mem all_locals
    in
    let uninitialized_usage (statement, initialized) =
      let is_uninitialized { Node.value = identifier; _ } =
        not (InitializedVariables.mem initialized (Identifier.sanitized identifier))
      in
      extract_reads_statement statement |> List.filter ~f:is_uninitialized
    in
    Int.Table.data Context.fixpoint_post_statement
    |> List.map ~f:uninitialized_usage
    |> List.concat
    |> List.filter ~f:in_local_scope
    |> List.map ~f:emit_error


  let less_or_equal ~left ~right =
    match left, right with
    | Value left, Value right -> InitializedVariables.is_subset right ~of_:left
    | Value _, Bottom -> false
    | Bottom, Value _ -> true
    | Bottom, Bottom -> true


  let join left right =
    match left, right with
    | Value left, Value right -> Value (InitializedVariables.inter left right)
    | Value left, Bottom -> Value left
    | Bottom, Value right -> Value right
    | Bottom, Bottom -> Bottom


  let widen ~previous ~next ~iteration:_ = join previous next

  let forward ~statement_key state ~statement =
    match state with
    | Bottom -> Bottom
    | Value state ->
        let new_state =
          Scope.Binding.of_statement [] statement
          |> List.map ~f:Scope.Binding.name
          |> List.map ~f:Identifier.sanitized
          |> InitializedVariables.of_list
          |> InitializedVariables.union state
        in
        Hashtbl.set Context.fixpoint_post_statement ~key:statement_key ~data:(statement, new_state);
        Value new_state


  let backward ~statement_key:_ _ ~statement:_ = failwith "Not implemented"
end

let run_on_define ~qualifier define =
  let module Context = struct
    let fixpoint_post_statement = Int.Table.create ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  let fixpoint = Fixpoint.forward ~cfg ~initial:(State.initial ~define) in
  Fixpoint.exit fixpoint >>| State.errors ~qualifier ~define |> Option.value ~default:[]


let run
    ~configuration:_
    ~environment:_
    ~source:({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:false
  |> List.map ~f:(run_on_define ~qualifier)
  |> List.concat
