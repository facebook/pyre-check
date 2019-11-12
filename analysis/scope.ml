(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module Binding = struct
  module Kind = struct
    type t =
      | AssignTarget
      | ClassName
      | ComprehensionTarget
      | DefineName
      | ExceptTarget
      | ForTarget
      | ImportName
      | ParameterName
      | WithTarget
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    name: Identifier.t;
    location: Location.t;
    annotation: Expression.t option;
  }
  [@@deriving sexp, compare, hash]

  let rec of_target ~kind ~annotation { Node.value = target; location } =
    let open Expression in
    let create_without_annotation = List.concat_map ~f:(of_target ~kind ~annotation:None) in
    match target with
    | Expression.Name (Name.Identifier name) -> [{ name; kind; location; annotation }]
    | Tuple elements
    | List elements ->
        (* Tuple or list cannot be annotated. *)
        create_without_annotation elements
    | _ -> []


  let rec of_statement { Node.value = statement; location } =
    (* TODO (T53600647): Support walrus operator. *)
    let open Statement in
    match statement with
    | Statement.Assign { Assign.target; annotation; _ } ->
        of_target ~kind:Kind.AssignTarget ~annotation target
    | Statement.Class { Class.name; _ } ->
        [{ kind = Kind.ClassName; name = Reference.show name; location; annotation = None }]
    | Statement.Define { Define.signature = { name; _ }; _ } ->
        [{ kind = Kind.DefineName; name = Reference.show name; location; annotation = None }]
    | Statement.For { For.target; body; orelse; _ } ->
        let target_names = of_target ~kind:Kind.ForTarget ~annotation:None target in
        let body_names = of_statements body in
        let orelse_names = of_statements orelse in
        List.concat [target_names; body_names; orelse_names]
    | Statement.If { If.body; orelse; _ } ->
        List.append (of_statements body) (of_statements orelse)
    | Statement.Import { Import.imports; _ } ->
        let binding_of_import { Import.alias; name } =
          (* TODO: Track the location of import name. *)
          match alias with
          | None ->
              let name = Reference.show name in
              if String.is_prefix name ~prefix:"_" then
                None
              else
                Some { kind = Kind.ImportName; name; location; annotation = None }
          | Some alias ->
              Some
                {
                  kind = Kind.ImportName;
                  name = Reference.show alias;
                  location;
                  annotation = None;
                }
        in
        List.filter_map imports ~f:binding_of_import
    | Statement.Try { Try.body; handlers; orelse; finally } ->
        let bindings_of_handler { Try.Handler.name; kind; body } =
          let name_binding =
            Option.map name ~f:(fun name ->
                (* TODO: Track the location of handler name. *)
                { kind = Kind.ExceptTarget; name; location; annotation = kind })
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
          Option.value_map alias ~f:(of_target ~kind:Kind.WithTarget ~annotation:None) ~default:[]
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

  let of_parameter { Node.value = { Expression.Parameter.name; annotation; _ }; location } =
    { kind = Kind.ParameterName; name; location; annotation }


  let of_generator { Expression.Comprehension.Generator.target; _ } =
    of_target ~kind:Kind.ComprehensionTarget ~annotation:None target
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
      | Define
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


  let of_define { Statement.Define.signature; body } =
    let open Statement in
    if Define.Signature.is_toplevel signature then
      None
    else
      let globals = globals_of_statements body |> Identifier.Set.of_list in
      let nonlocals = nonlocals_of_statements body |> Identifier.Set.of_list in
      let parameter_bindings =
        let { Define.Signature.parameters; _ } = signature in
        List.map parameters ~f:Binding.of_parameter
      in
      let body_bindings = Binding.of_statements body in
      Some
        {
          kind = Kind.Define;
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
            (Statement.Define.name define)
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
              List.map parameters ~f:Binding.of_parameter |> create_map ~globals ~nonlocals;
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
end
