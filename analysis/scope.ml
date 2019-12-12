(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module Binding = struct
  module Kind = struct
    module Star = struct
      type t =
        | Once
        | Twice
      [@@deriving sexp, compare, hash]
    end

    type t =
      | AssignTarget of Expression.t option
      | ClassName
      | ComprehensionTarget
      | DefineName of Statement.Define.Signature.t
      | ExceptTarget of Expression.t option
      | ForTarget
      | ImportName
      | ParameterName of {
          index: int;
          annotation: Expression.t option;
          star: Star.t option;
        }
      | WithTarget
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    name: Identifier.t;
    location: Location.t;
  }
  [@@deriving sexp, compare, hash]

  let rec of_unannotated_target ~kind { Node.value = target; location } =
    let open Expression in
    match target with
    | Expression.Name (Name.Identifier name) -> [{ name; kind; location }]
    | Tuple elements
    | List elements ->
        (* Tuple or list cannot be annotated. *)
        List.concat_map elements ~f:(of_unannotated_target ~kind)
    | _ -> []


  let rec of_statement { Node.value = statement; location } =
    (* TODO (T53600647): Support walrus operator. *)
    let open Statement in
    match statement with
    | Statement.Assign
        {
          Assign.target =
            { Node.value = Expression.Expression.Name (Expression.Name.Identifier name); location };
          annotation = Some annotation;
          _;
        } ->
        [{ name; kind = Kind.AssignTarget (Some annotation); location }]
    | Statement.Assign { Assign.target; _ } ->
        of_unannotated_target ~kind:(Kind.AssignTarget None) target
    | Statement.Class { Class.name = { Node.value = name; _ }; _ } ->
        [{ kind = Kind.ClassName; name = Reference.show name; location }]
    | Statement.Define { Define.signature = { name = { Node.value = name; _ }; _ } as signature; _ }
      ->
        [{ kind = Kind.DefineName signature; name = Reference.show name; location }]
    | Statement.For { For.target; body; orelse; _ } ->
        let target_names = of_unannotated_target ~kind:Kind.ForTarget target in
        let body_names = of_statements body in
        let orelse_names = of_statements orelse in
        List.concat [target_names; body_names; orelse_names]
    | Statement.If { If.body; orelse; _ } -> List.append (of_statements body) (of_statements orelse)
    | Statement.Import { Import.imports; _ } ->
        let binding_of_import { Import.alias; name } =
          (* TODO: Track the location of import name. *)
          match alias with
          | None ->
              let name = Reference.show name in
              if String.is_prefix name ~prefix:"_" then
                None
              else
                Some { kind = Kind.ImportName; name; location }
          | Some alias -> Some { kind = Kind.ImportName; name = Reference.show alias; location }
        in
        List.filter_map imports ~f:binding_of_import
    | Statement.Try { Try.body; handlers; orelse; finally } ->
        let bindings_of_handler { Try.Handler.name; kind; body } =
          let name_binding =
            Option.map name ~f:(fun name ->
                (* TODO: Track the location of handler name. *)
                { kind = Kind.ExceptTarget kind; name; location })
          in
          let bindings_in_body = of_statements body in
          List.append (Option.to_list name_binding) bindings_in_body
        in
        let body_names = of_statements body in
        let handler_names = List.concat_map handlers ~f:bindings_of_handler in
        let orelse_names = of_statements orelse in
        let finally_names = of_statements finally in
        List.concat [body_names; handler_names; orelse_names; finally_names]
    | Statement.While { While.body; orelse; _ } ->
        List.append (of_statements body) (of_statements orelse)
    | Statement.With { With.items; body; _ } ->
        let bindings_of_item (_, alias) =
          Option.value_map alias ~f:(of_unannotated_target ~kind:Kind.WithTarget) ~default:[]
        in
        let item_names = List.concat_map items ~f:bindings_of_item in
        let body_names = of_statements body in
        List.append item_names body_names
    | Assert _
    | Break
    | Continue
    | Delete _
    | Expression _
    | Global _
    | Nonlocal _
    | Pass
    | Raise _
    | Return _
    | Yield _
    | YieldFrom _ ->
        []


  and of_statements statements = List.concat_map statements ~f:of_statement

  let of_parameter index { Node.value = { Expression.Parameter.name; annotation; _ }; location } =
    let star, name =
      let star, name = Identifier.split_star name in
      let star =
        match star with
        | "**" -> Some Kind.Star.Twice
        | "*" -> Some Kind.Star.Once
        | _ -> None
      in
      star, name
    in
    { kind = Kind.ParameterName { index; star; annotation }; name; location }


  let of_generator { Expression.Comprehension.Generator.target; _ } =
    of_unannotated_target ~kind:Kind.ComprehensionTarget target
end

let rec globals_of_statement { Node.value; _ } =
  let open Statement in
  match value with
  | Statement.Global globals -> globals
  | For { For.body; orelse; _ }
  | If { If.body; orelse; _ }
  | While { While.body; orelse; _ } ->
      globals_of_statements (List.append body orelse)
  | Try { Try.body; handlers; orelse; finally } ->
      let statements =
        let handler_statements =
          List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
        in
        List.concat [body; handler_statements; orelse; finally]
      in
      globals_of_statements statements
  | With { With.body; _ } -> globals_of_statements body
  | Assign _
  | Assert _
  | Break
  | Class _
  | Continue
  | Define _
  | Delete _
  | Expression _
  | Import _
  | Nonlocal _
  | Pass
  | Raise _
  | Return _
  | Yield _
  | YieldFrom _ ->
      []


and globals_of_statements statements = List.concat_map statements ~f:globals_of_statement

let rec nonlocals_of_statement { Node.value; _ } =
  let open Statement in
  match value with
  | Statement.Nonlocal nonlocals -> nonlocals
  | For { For.body; orelse; _ }
  | If { If.body; orelse; _ }
  | While { While.body; orelse; _ } ->
      nonlocals_of_statements (List.append body orelse)
  | Try { Try.body; handlers; orelse; finally } ->
      let statements =
        let handler_statements =
          List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
        in
        List.concat [body; handler_statements; orelse; finally]
      in
      nonlocals_of_statements statements
  | With { With.body; _ } -> nonlocals_of_statements body
  | Assign _
  | Assert _
  | Break
  | Class _
  | Continue
  | Define _
  | Delete _
  | Expression _
  | Global _
  | Import _
  | Pass
  | Raise _
  | Return _
  | Yield _
  | YieldFrom _ ->
      []


and nonlocals_of_statements statements = List.concat_map statements ~f:nonlocals_of_statement

module Scope = struct
  module Kind = struct
    type t =
      | Module
      | Define of Statement.Define.Signature.t
      | Lambda
      | Comprehension
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    globals: Identifier.Set.t;
    nonlocals: Identifier.Set.t;
    bindings: Binding.t Identifier.Map.t;
  }
  [@@deriving sexp, compare]

  let create_map ~globals ~nonlocals =
    List.fold ~init:Identifier.Map.empty ~f:(fun sofar ({ Binding.name; _ } as binding) ->
        match Identifier.Set.mem globals name || Identifier.Set.mem nonlocals name with
        | true ->
            (* Global and nonlocal declarations take priority. *)
            sofar
        | false -> (
            (* First binding wins. *)
            match Map.add sofar ~key:name ~data:binding with
            | `Ok result -> result
            | `Duplicate -> sofar ))


  let of_define { Statement.Define.signature; body; _ } =
    let open Statement in
    if Define.Signature.is_toplevel signature then
      None
    else
      let globals = globals_of_statements body |> Identifier.Set.of_list in
      let nonlocals = nonlocals_of_statements body |> Identifier.Set.of_list in
      let parameter_bindings =
        let { Define.Signature.parameters; _ } = signature in
        List.mapi parameters ~f:Binding.of_parameter
      in
      let body_bindings = Binding.of_statements body in
      Some
        {
          kind = Kind.Define signature;
          globals;
          nonlocals;
          bindings = create_map ~globals ~nonlocals (List.append parameter_bindings body_bindings);
        }


  let of_define_exn define =
    match of_define define with
    | None ->
        let message =
          Format.asprintf
            "Cannot create a scope for define %a"
            Reference.pp
            (Node.value (Statement.Define.name define))
        in
        failwith message
    | Some result -> result


  let of_expression { Node.value; _ } =
    let open Expression in
    (* There's no way to declare globals or nonlocals in expression scope. *)
    let globals = Identifier.Set.empty in
    let nonlocals = Identifier.Set.empty in
    match value with
    | Expression.Lambda { Lambda.parameters; _ } ->
        Some
          {
            kind = Kind.Lambda;
            globals;
            nonlocals;
            bindings =
              List.mapi parameters ~f:Binding.of_parameter |> create_map ~globals ~nonlocals;
          }
    | DictionaryComprehension { Comprehension.generators; _ }
    | Generator { Comprehension.generators; _ }
    | ListComprehension { Comprehension.generators; _ }
    | SetComprehension { Comprehension.generators; _ } ->
        Some
          {
            kind = Kind.Comprehension;
            globals;
            nonlocals;
            bindings =
              List.concat_map generators ~f:Binding.of_generator |> create_map ~globals ~nonlocals;
          }
    | _ -> None


  let of_expression_exn expression =
    match of_expression expression with
    | None ->
        let message =
          Format.asprintf "Cannot create a scope for expression %a" Expression.pp expression
        in
        failwith message
    | Some result -> result


  let of_source { Source.statements; _ } =
    let globals =
      (* Global statement has no effect at the module level. *)
      Identifier.Set.empty
    in
    let nonlocals =
      (* Nonlocal statement is illegal at the module level. *)
      Identifier.Set.empty
    in
    {
      kind = Kind.Module;
      globals;
      nonlocals;
      bindings = Binding.of_statements statements |> create_map ~globals ~nonlocals;
    }


  let lookup_bindings { bindings; _ } = Map.find bindings

  module Lookup = struct
    type t =
      | Binding of Binding.t
      | Global
      | Nonlocal
    [@@deriving sexp, compare, hash]
  end

  let lookup { globals; nonlocals; bindings; _ } name =
    match Set.mem globals name with
    | true -> Some Lookup.Global
    | false -> (
        match Set.mem nonlocals name with
        | true -> Some Lookup.Nonlocal
        | false -> Map.find bindings name >>| fun binding -> Lookup.Binding binding )
end

module Access = struct
  module Locality = struct
    type t =
      | Local
      | Nonlocal
      | Global
    [@@deriving sexp, compare, hash]
  end

  module Kind = struct
    type t =
      | CurrentScope
      | OuterScope of Locality.t
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    binding: Binding.t;
    scope: Scope.t;
  }
  [@@deriving sexp, compare]
end

module ScopeStack = struct
  type t = {
    global: Scope.t;
    locals: Scope.t list;
  }

  let create source = { global = Scope.of_source source; locals = [] }

  let global_scope { global; _ } = global

  let current_scope { locals; global } =
    match locals with
    | [] -> global
    | scope :: _ -> scope


  let lookup { locals; global } name =
    let rec lookup_outer_scopes ~exclude_global = function
      | [] ->
          if exclude_global then
            None
          else
            Scope.lookup_bindings global name >>| fun binding -> global, binding
      | scope :: rest -> (
          match Scope.lookup scope name with
          | Some (Scope.Lookup.Binding binding) -> Some (scope, binding)
          | Some Scope.Lookup.Global -> lookup_outer_scopes ~exclude_global []
          | _ -> lookup_outer_scopes ~exclude_global rest )
    in
    match locals with
    | [] ->
        (* No need to deal with nested scopes if there's none *)
        Scope.lookup_bindings global name
        >>| fun binding -> { Access.binding; scope = global; kind = Access.Kind.CurrentScope }
    | current_scope :: rest -> (
        match Scope.lookup current_scope name with
        | Some (Scope.Lookup.Binding binding) ->
            (* Binding found in the current scope *)
            Some { Access.binding; scope = current_scope; kind = Access.Kind.CurrentScope }
        | Some Scope.Lookup.Global ->
            Scope.lookup_bindings global name
            >>| fun binding ->
            { Access.binding; scope = global; kind = Access.(Kind.OuterScope Locality.Global) }
        | Some Scope.Lookup.Nonlocal ->
            lookup_outer_scopes ~exclude_global:true rest
            >>| fun (scope, binding) ->
            { Access.binding; scope; kind = Access.(Kind.OuterScope Locality.Nonlocal) }
        | None ->
            lookup_outer_scopes ~exclude_global:false rest
            >>| fun (scope, binding) ->
            { Access.binding; scope; kind = Access.(Kind.OuterScope Locality.Local) } )


  let extend ~with_ { global; locals } = { global; locals = with_ :: locals }
end
