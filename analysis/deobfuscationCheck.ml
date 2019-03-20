(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


module Error = AnalysisError


let name =
  "Deobfuscation"


module NestedDefines = struct
  type 'state nested = {
    nested_define: Define.t;
    state: 'state;
  }


  and 'state t = ('state nested) Location.Reference.Map.t


  let initial =
    Location.Reference.Map.empty


  let nested_defines nested_defines =
    Map.data nested_defines


  let update_nested_defines nested_defines ~statement ~state =
    match statement with
    | { Node.location; value = Define nested_define } ->
        Map.set nested_defines ~key:location ~data:{ nested_define; state }
    | _ ->
        nested_defines
end


module type Context = sig
  val configuration: Configuration.Analysis.t
  val environment: (module Environment.Handler)
  val transformations: (Statement.t list) Location.Reference.Table.t
end


module ConstantPropagationState(Context: Context) = struct
  type constant =
    | Constant of Expression.t
    | Top


  and t = {
    constants: constant Access.Map.t;
    define: Define.t;
    nested_defines: t NestedDefines.t;
  }


  let show { constants; _ } =
    let print_entry (access, constant) =
      let pp_constant format = function
        | Constant expression -> Format.fprintf format "Constant %a" Expression.pp expression
        | Top -> Format.fprintf format "Top"
      in
      Format.asprintf
        "%a -> %a"
        Access.pp access
        pp_constant constant
    in
    Map.to_alist constants
    |> List.map ~f:print_entry
    |> String.concat ~sep:", "


  let pp format state =
    Format.fprintf format "%s" (show state)


  let initial ~state ~define =
    let constants =
      match state with
      | Some { constants; _ } -> constants
      | _ -> Access.Map.empty
    in
    { constants; define; nested_defines = NestedDefines.initial }


  let nested_defines { nested_defines; _ } =
    nested_defines


  let less_or_equal ~left:{ constants = left; _ } ~right:{ constants = right; _ } =
    let less_or_equal (access, constant) =
      match constant, Map.find right access with
      | _, Some Top -> true
      | Constant left, Some (Constant right) when Expression.equal left right -> true
      | _ -> false
    in
    Map.to_alist left
    |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Both (Constant left, Constant right) when Expression.equal left right ->
          Some (Constant left)
      | _ ->
          Some Top
    in
    { left with constants = Map.merge left.constants right.constants ~f:merge }


  let widen ~previous ~next ~iteration:_ =
    join previous next


  let forward
      ?key
      ({ constants; define = { Define.name; parent; _ }; nested_defines } as state)
      ~statement =
    let resolution =
      TypeCheck.resolution_with_key
        ~environment:Context.environment
        ~parent:(parent >>| Reference.expression)
        ~access:(Reference.expression name)
        ~key
    in

    (* Update transformations. *)
    let transformed =
      let transform_statement statement =
        let module Transform =
          Transform.Make(struct
            type t = unit
            let expression _ expression =
              match Node.value expression with
              | Access (SimpleAccess access) ->
                  begin
                    let rec transform ~lead ~tail =
                      match tail with
                      | head :: tail ->
                          begin
                            let lead = lead @ [head] in
                            match Map.find constants lead with
                            | Some (Constant { Node.value = Access (SimpleAccess access); _ }) ->
                                Access (SimpleAccess (access @ tail))
                            | Some (Constant expression) ->
                                begin
                                  match tail with
                                  | [] ->
                                      Node.value expression
                                  | tail ->
                                      Access.combine expression tail
                                      |> fun access -> Access access
                                end
                            | _ ->
                                transform ~lead ~tail
                          end
                      | _ ->
                          Access (SimpleAccess lead)
                    in
                    let value = transform ~lead:[] ~tail:access in
                    Node.create value ~location:(Node.location expression)
                  end
              | _ ->
                  expression

            let transform_children _ _ =
              true

            let statement _ statement =
              (), [statement]
          end)
        in
        Source.create [statement]
        |> Transform.transform ()
        |> Transform.source
        |> Source.statements
        |> function
        | [statement] -> statement
        | _ -> failwith "Could not transform statement"
      in
      let transform_expression expression =
        Statement.Expression expression
        |> Node.create_with_default_location
        |> transform_statement
        |> function
        | { Node.value = Statement.Expression value; _ } -> value
        | _ -> failwith "Could not extract expression"
      in
      match Node.value statement with
      | Assign ({ value; _ } as assign) ->
          (* Do not update left hand side of assignment. *)
          let value = transform_expression value in
          { statement with Node.value = Assign { assign with value }}
      | Assert {
          Assert.origin =
            Assert.If {
              statement = { Node.location; value = If ({ If.test; _ } as conditional) };
              true_branch = true;
            };
          _;
        } ->
          let transformed_test = transform_expression test in
          if not (Expression.equal test transformed_test) then
            { Node.location; value = If { conditional with If.test = transformed_test } }
          else
            statement
      | _ ->
          transform_statement statement
    in
    if not (Statement.equal statement transformed) then
      Hashtbl.set Context.transformations ~key:(Node.location transformed) ~data:[transformed];

    (* Find new constants. *)
    let constants =
      match Node.value transformed with
      | Assign {
          target = { Node.value = Access (SimpleAccess access); _ };
          value = expression;
          _;
        }  ->
          let propagate =
            let is_literal =
              match Node.value expression with
              | Integer _ | String _ | True | False
              | Access (SimpleAccess [Access.Identifier "None"]) -> true
              | _ -> false
            in
            let is_callable =
              Resolution.resolve resolution expression
              |> (fun annotation -> Type.is_callable annotation || Type.is_meta annotation)
            in
            let is_global_constant =
              match Node.value expression with
              | Access (SimpleAccess access) ->
                  Str.string_match (Str.regexp ".*\\.[A-Z_0-9]+$") (Access.show access) 0
              | _ ->
                  false
            in
            is_literal || is_callable || is_global_constant
          in
          if propagate then
            Map.set constants ~key:access ~data:(Constant expression)
          else
            Map.remove constants access
      | _ ->
          constants
    in
    let state = { state with constants } in

    let nested_defines = NestedDefines.update_nested_defines nested_defines ~statement ~state in
    { state with nested_defines }


  let backward ?key:_ _ ~statement:_ =
    failwith "Not implemented"
end


module UnusedStoreState (Context: Context) = struct
  type t = {
    unused: Location.Reference.Set.t Access.Map.t;
    define: Define.t;
    nested_defines: t NestedDefines.t;
  }


  let show { unused; _ } =
    Map.keys unused
    |> List.map ~f:Access.show
    |> String.concat ~sep:", "


  let pp format state =
    Format.fprintf format "%s" (show state)


  let initial ~state:_ ~define =
    { unused = Access.Map.empty; define; nested_defines = NestedDefines.initial }


  let nested_defines { nested_defines; _ } =
    nested_defines


  let less_or_equal ~left:{ unused = left; _ } ~right:{ unused = right; _ } =
    let less_or_equal (access, location) =
      match location, Map.find right access with
      | left, Some right -> Set.is_subset left ~of_:right
      | _ -> false
    in
    Map.to_alist left
    |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Both (left, right) -> Some (Set.union left right)
      | `Left left -> Some left
      | `Right right -> Some right
    in
    { left with unused = Map.merge left.unused right.unused ~f:merge }


  let widen ~previous ~next ~iteration:_ =
    join previous next


  let forward
      ?key:_
      ({ unused; nested_defines; _ } as state)
      ~statement:({ Node.location; value } as statement) =
    (* Remove used accesses from transformation map. *)
    let unused =
      let used_accesses =
        Visit.collect_accesses statement
        |> List.map ~f:Node.value
        |> List.filter_map
          ~f:(function | Access.SimpleAccess (head :: _) -> Some [head] | _ -> None)
      in
      let used_locations =
        used_accesses
        |> List.filter_map ~f:(Map.find unused)
        |> List.fold ~f:Set.union ~init:Location.Reference.Set.empty
        |> Set.to_list
      in
      List.iter used_locations ~f:(Hashtbl.remove Context.transformations);
      List.fold used_accesses ~f:Map.remove ~init:unused
    in

    (* Add assignments to transformation map. *)
    let unused =
      match value with
      | Assign { target = { Node.value = Access (SimpleAccess access); _ }; _ }  ->
          let update = function
            | Some existing -> Set.add existing location
            | None -> Location.Reference.Set.of_list [location]
          in
          Hashtbl.set Context.transformations ~key:location ~data:[];
          Map.update unused access ~f:update
      | _ ->
          unused
    in
    let state = { state with unused } in

    let nested_defines = NestedDefines.update_nested_defines nested_defines ~statement ~state in
    { state with nested_defines }


  let backward ?key:_ _ ~statement:_ =
    failwith "Not implemented"
end


module type State = sig
  type t

  include Fixpoint.State with type t := t

  val initial: state: t option -> define: Define.t -> t
  val nested_defines: t -> t NestedDefines.t
end


(* Lol functors... *)
module Scheduler (State: State) (Context: Context) = struct
  let run ({ Source.qualifier; statements; _ } as source) =
    Hashtbl.clear Context.transformations;

    let module Fixpoint = Fixpoint.Make(State) in

    let rec run ~state ~define =
      Fixpoint.forward ~cfg:(Cfg.create define) ~initial:(State.initial ~state ~define)
      |> Fixpoint.exit
      >>| (fun state ->
          State.nested_defines state
          |> Map.data
          |> List.iter
            ~f:(fun { NestedDefines.nested_define; state } ->
                run ~state:(Some state) ~define:nested_define))
      |> ignore
    in
    let qualifier = Some (Reference.from_access qualifier) in
    let define = Define.create_toplevel ~qualifier ~statements in
    run ~state:None ~define;

    let module Transform =
      Transform.MakeStatementTransformer(struct
        type t = unit
        let statement _ statement =
          let transformed =
            let transformed =
              Hashtbl.find Context.transformations (Node.location statement)
              |> Option.value ~default:[statement]
            in
            match statement, transformed with
            | { Node.value = If conditional; _ }, [{ Node.value = If { If.test; _ }; _ }] ->
                (* Don't undo work we've done in the body of the conditional. *)
                [{ statement with Node.value = If { conditional with If.test } }]
            | _ ->
                transformed
          in
          (), transformed
      end)
    in
    Transform.transform () source
    |> Transform.source
end


let run
    ~configuration
    ~environment
    ~source:({ Source.qualifier; statements; handle; _ } as source) =
  let module Context =
    (struct
      let configuration = configuration
      let environment = environment
      let transformations = Location.Reference.Table.create ()
    end)
  in

  (* Constant propagation. *)
  let source =
    let module State = ConstantPropagationState(Context) in
    let module ConstantPropagationScheduler = Scheduler(State)(Context) in
    ConstantPropagationScheduler.run source
  in

  (* Dead store elimination. *)
  let source =
    let module State = UnusedStoreState (Context) in
    let module DeadStoreEliminationScheduler = Scheduler (State) (Context) in
    DeadStoreEliminationScheduler.run source
  in

  (* Rename obfuscated variables. *)
  let source =
    let last_identifier = ref "" in
    let generate_identifier () =
      let identifier =
        let bump character =
          Char.to_int character
          |> (+) 1
          |> Char.of_int_exn
        in
        let to_string reversed_list =
          List.rev reversed_list
          |> String.of_char_list
        in
        match String.to_list_rev !last_identifier with
        | [] ->
            "a"
        | ('z' :: _) as characters ->
            let next_identifier =
              let next_identifier =
                let rec next_identifier = function
                  | 'z' :: tail -> 'a' :: (next_identifier tail)
                  | character :: tail -> (bump character) :: tail
                  | [] -> []
                in
                next_identifier characters
              in
              if List.for_all next_identifier ~f:(Char.equal 'a') then
                'a' :: next_identifier
              else
                next_identifier
            in
            next_identifier
            |> to_string
        | character :: tail ->
            (bump character) :: tail
            |> to_string
      in
      last_identifier := identifier;
      identifier
    in
    let replacements = String.Table.create () in
    let sanitize_access access =
      match List.rev access with
      | Access.Identifier identifier :: tail
        when String.length (Identifier.sanitized identifier) > 15 ->
          let identifier =
            let replacement = generate_identifier () in
            Hashtbl.set replacements ~key:identifier ~data:replacement;
            replacement
          in
          Access.Identifier identifier :: tail
          |> List.rev
      | _ ->
          access
    in
    let sanitize_reference reference =
      let last = Reference.show (Reference.last reference) in
      if String.length last > 15 then
        let replacement = generate_identifier () in
        Hashtbl.set replacements ~key:last ~data:replacement;
        Reference.create ?prefix:(Reference.prefix reference) replacement
      else
        reference
    in
    let sanitize_identifier identifier =
      if String.length (Identifier.sanitized identifier) > 15 then
        begin
          let replacement = generate_identifier () in
          Hashtbl.set replacements ~key:identifier ~data:replacement;
          replacement
        end
      else
        identifier
    in

    let module ScopeTransform =
      Transform.MakeStatementTransformer(struct
        type t = unit

        let statement state statement =
          let transformed =
            let value =
              match Node.value statement with
              | Define ({ Define.name; parameters; body; _ } as define) ->
                  (* Scope parameters to the function. *)
                  let names = String.Hash_set.create () in
                  let scope_name identifier =
                    let qualifier =
                      Reference.show name
                      |> String.substr_replace_all ~pattern:"." ~with_:"?"
                    in
                    identifier
                    |> String.chop_prefix_exn ~prefix:"$parameter"
                    |> Format.asprintf "$parameter_%s%s" qualifier
                  in
                  let parameters  =
                    let parameter ({ Node.value = { Parameter.name; _ } as parameter; _ } as node) =
                      Hash_set.add names name;
                      { node with Node.value = { parameter with Parameter.name = scope_name name } }
                    in
                    List.map parameters ~f:parameter
                  in
                  let body =
                    let module Transform =
                      Transform.Make(struct
                        type t = unit

                        let expression _ expression =
                          let value =
                            match Node.value expression with
                            | Access (SimpleAccess access) ->
                                let element = function
                                  | Access.Identifier name when Hash_set.mem names name ->
                                      Access.Identifier (scope_name name)
                                  | element ->
                                      element
                                in
                                Access (SimpleAccess (List.map access ~f:element))
                            | value ->
                                value
                          in
                          { expression with Node.value }

                        let transform_children _ _ =
                          true

                        let statement _ statement =
                          (), [statement]
                      end)
                    in
                    Source.create body
                    |> Transform.transform ()
                    |> Transform.source
                    |> Source.statements
                  in

                  (* Sanitize. *)
                  let parameters =
                    let sanitize_parameter
                        ({ Node.value = { Parameter.name; _ } as parameter; _ } as node) =
                      let name = sanitize_identifier name in
                      { node with Node.value = { parameter with Parameter.name} }
                    in
                    List.map parameters ~f:sanitize_parameter
                  in
                  Define { define with Define.name = sanitize_reference name; parameters; body }
              | For ({
                  For.target = { Node.value = Access (SimpleAccess access); _ } as target;
                  _;
                } as block) ->
                  let target =
                    { target with Node.value = Access (SimpleAccess (sanitize_access access)) }
                  in
                  For { block with For.target }
              | Global globals ->
                  Global (List.map globals ~f:sanitize_identifier)
              | value ->
                  value
            in
            { statement with Node.value }
          in
          state, [transformed]
      end)
    in
    let module Transform =
      Transform.Make(struct
        type t = unit

        let expression _ expression =
          let value =
            match Node.value expression with
            | Access (SimpleAccess access) ->
                let sanitize = function
                  | Access.Identifier identifier ->
                      let identifier =
                        match Hashtbl.find replacements identifier with
                        | Some replacement -> replacement
                        | None -> identifier
                      in
                      Access.Identifier identifier
                  | access ->
                      access
                in
                Access (SimpleAccess (List.map access ~f:sanitize))
            | value ->
                value
          in
          { expression with Node.value }

        let transform_children _ _ =
          true

        let statement state statement =
          let transformed =
            let value =
              match Node.value statement with
              | Assign ({ target; _; } as assign) ->
                  let rec sanitize_target target =
                    match target with
                    | { Node.value = Access (SimpleAccess access); _ } as target ->
                        { target with Node.value = Access (SimpleAccess (sanitize_access access)) }
                    | { Node.value = Tuple targets; _ } as tuple ->
                        { tuple with Node.value = Tuple (List.map targets ~f:sanitize_target) }
                    | _ ->
                        target
                  in
                  Assign { assign with Assign.target = sanitize_target target }
              | value ->
                  value
            in
            { statement with Node.value }
          in
          state, [transformed]
      end)
    in
    ScopeTransform.transform () source
    |> ScopeTransform.source
    |> Transform.transform ()
    |> Transform.source
  in

  (* Dequalify. *)
  let source =
    let module Transform =
      Transform.Make(struct
        type t = unit

        let dequalify_access access =
          let sanitized = Access.sanitized access in
          let has_qualifier_as_prefix =
            List.is_prefix
              sanitized
              ~prefix:qualifier
              ~equal:(Access.equal_access Expression.equal)
          in
          if has_qualifier_as_prefix then
            List.drop sanitized (List.length qualifier)
          else
            sanitized

        let dequalify_reference reference =
          let sanitized = Reference.sanitized reference in
          let qualifier = Reference.from_access qualifier in
          if Reference.is_strict_prefix ~prefix:qualifier sanitized then
            Reference.drop_prefix ~prefix:qualifier sanitized
          else
            sanitized

        let expression _ expression =
          let value =
            match Node.value expression with
            | Access (SimpleAccess access) -> Access (SimpleAccess (dequalify_access access))
            | value -> value
          in
          { expression with Node.value }

        let transform_children _ _ =
          true

        let statement _ statement =
          let transformed =
            let value =
              match Node.value statement with
              | Define ({ Define.name; parameters; _ } as define) ->
                  let parameters =
                    let sanitize_parameter =
                      let sanitize_parameter ({ Parameter.name; _ } as parameter) =
                        { parameter with Parameter.name = Identifier.sanitized name }
                      in
                      Node.map ~f:sanitize_parameter
                    in
                    List.map parameters ~f:sanitize_parameter;
                  in
                  Define { define with Define.name = dequalify_reference name; parameters }
              | Try ({ Try.handlers; _ } as block) ->
                  let handlers =
                    let sanitize_handler ({ Try.name; _ } as handler) =
                      { handler with Try.name = name >>| Identifier.sanitized }
                    in
                    List.map handlers ~f:sanitize_handler
                  in
                  Try { block with Try.handlers }
              | value ->
                  value
            in
            { statement with Node.value }
          in
          (), [transformed]
      end)
    in
    Transform.transform () source
    |> Transform.source
  in

  (* Fix up AST. *)
  let source =
    let module Transform =
      Transform.MakeStatementTransformer(struct
        type t = unit

        let statement _ statement =
          let transformed =
            let fix_statement_list = function
              | [] -> [Node.create_with_default_location Pass]
              | statements -> statements
            in
            let value =
              match Node.value statement with
              | If ({ If.body; _ } as conditional) ->
                  If { conditional with If.body = fix_statement_list body }
              | Define ({ Define.body; _ } as define) ->
                  let body =
                    let remove_docstring = function
                      | { Node.value = Expression { Node.value = String _; _ }; _ } :: tail -> tail
                      | statements -> statements
                    in
                    fix_statement_list body
                    |> remove_docstring
                  in
                  Define { define with Define.body; docstring = None }
              | value ->
                  value
            in
            { statement with Node.value }
          in
          (), [transformed]
      end)
    in
    Transform.transform () source
    |> Transform.source
  in

  (* Create error. *)
  let location = Location.Reference.create_with_handle ~handle in
  let define =
    Define.create_toplevel ~qualifier:(Some (Reference.from_access qualifier)) ~statements
  in
  [
    Error.create
      ~location
      ~kind:(Error.Deobfuscation source)
      ~define:(Node.create define ~location);
  ]
