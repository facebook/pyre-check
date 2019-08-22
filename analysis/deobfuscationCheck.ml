(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement
open CustomAnalysis
module Error = AnalysisError

let name = "Deobfuscation"

module type Context = sig
  val global_resolution : GlobalResolution.t

  val transformations : Statement.t list Location.Reference.Table.t

  val errors : LivenessCheck.ErrorMap.t
end

module ConstantPropagationState (Context : Context) = struct
  type constant =
    | Constant of Expression.t
    | Top

  and t = {
    constants: constant Reference.Map.t;
    define: Define.t;
    nested_defines: t NestedDefines.t;
  }

  let show { constants; _ } =
    let print_entry (reference, constant) =
      let pp_constant format = function
        | Constant expression -> Format.fprintf format "Constant %a" Expression.pp expression
        | Top -> Format.fprintf format "Top"
      in
      Format.asprintf "%a -> %a" Reference.pp reference pp_constant constant
    in
    Map.to_alist constants |> List.map ~f:print_entry |> String.concat ~sep:", "


  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~state ~lookup:_ ~define =
    let constants =
      match state with
      | Some { constants; _ } -> constants
      | _ -> Reference.Map.empty
    in
    { constants; define = define.Node.value; nested_defines = NestedDefines.initial }


  let nested_defines { nested_defines; _ } = nested_defines

  let less_or_equal ~left:{ constants = left; _ } ~right:{ constants = right; _ } =
    let less_or_equal (reference, constant) =
      match constant, Map.find right reference with
      | _, Some Top -> true
      | Constant left, Some (Constant right) when Expression.equal left right -> true
      | _ -> false
    in
    Map.to_alist left |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Both (Constant left, Constant right) when Expression.equal left right ->
          Some (Constant left)
      | _ -> Some Top
    in
    { left with constants = Map.merge left.constants right.constants ~f:merge }


  let widen ~previous ~next ~iteration:_ = join previous next

  let update_transformations _ = ()

  let forward
      ?key
      ( { constants; define = { Define.signature = { name; parent; _ }; _ }; nested_defines } as
      state )
      ~statement
    =
    let resolution =
      TypeCheck.resolution_with_key ~global_resolution:Context.global_resolution ~parent ~name ~key
    in
    (* Update transformations. *)
    let transformed =
      let transform_statement statement =
        let module Transform = Transform.Make (struct
          type t = unit

          let transform_expression_children _ _ = true

          let expression _ expression =
            match Node.value expression with
            | Name name when Expression.is_simple_name name ->
                let rec transform { Node.value; location } =
                  let get_constant location reference =
                    match Map.find constants reference with
                    | Some (Constant expression) -> { expression with Node.location }, true
                    | _ ->
                        ( Node.create
                            ~location
                            (Name (Expression.create_name_from_reference ~location reference)),
                          false )
                  in
                  match value with
                  | Name (Name.Identifier identifier) ->
                      get_constant location (Reference.create identifier)
                  | Name (Name.Attribute { base; attribute; special } as name) ->
                      let base, transformed = transform base in
                      if transformed then
                        ( Node.create ~location (Name (Name.Attribute { base; attribute; special })),
                          transformed )
                      else
                        get_constant location (Expression.name_to_reference_exn name)
                  | _ -> { Node.value; location }, false
                in
                transform expression |> fst
            | _ -> expression


          let transform_children _ _ = true

          let statement _ statement = (), [statement]
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
          { statement with Node.value = Assign { assign with value } }
      | Assert
          {
            Assert.origin =
              Assert.If
                {
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
      | _ -> transform_statement statement
    in
    if not (Statement.equal statement transformed) then
      Hashtbl.set Context.transformations ~key:(Node.location transformed) ~data:[transformed];

    (* Find new constants. *)
    let constants =
      match Node.value transformed with
      | Assign { target = { Node.value = Name name; _ }; value = expression; _ }
        when Expression.is_simple_name name ->
          let propagate =
            let is_literal =
              match Node.value expression with
              | Integer _
              | String _
              | True
              | False
              | Name (Name.Identifier "None") ->
                  true
              | _ -> false
            in
            let is_callable =
              Resolution.resolve resolution expression
              |> fun annotation -> Type.is_callable annotation || Type.is_meta annotation
            in
            let is_global_constant =
              match Node.value expression with
              | Name name -> Expression.is_simple_name name
              | _ -> false
            in
            is_literal || is_callable || is_global_constant
          in
          let reference = Expression.name_to_reference_exn name in
          if propagate then
            Map.set constants ~key:reference ~data:(Constant expression)
          else
            Map.remove constants reference
      | _ -> constants
    in
    let state = { state with constants } in
    let nested_defines = NestedDefines.update_nested_defines nested_defines ~statement ~state in
    { state with nested_defines }


  let backward ?key:_ state ~statement:_ = state
end

module UnusedStoreState (Context : Context) = struct
  include LivenessCheck.State (Context)

  let nested_defines { define; _ } =
    let add_nested nested_defines { Node.location; value = define_value } =
      Map.set
        nested_defines
        ~key:location
        ~data:
          {
            NestedDefines.nested_define = define_value;
            state = initial ~lookup:(LivenessCheck.NestedDefineLookup.Table.create ()) ~define;
          }
    in
    List.fold ~init:NestedDefines.initial ~f:add_nested (nested_defines_deep_to_shallow define)


  let initial ~state:_ = initial

  let update_transformations state =
    let add_transformation { Error.location; _ } =
      Hashtbl.set Context.transformations ~key:location ~data:[]
    in
    List.iter ~f:add_transformation (errors state)
end

module type State = sig
  type t

  include Fixpoint.State with type t := t

  val initial
    :  state:t option ->
    lookup:t LivenessCheck.NestedDefineLookup.t ->
    define:Define.t Node.t ->
    t

  val nested_defines : t -> t NestedDefines.t

  val update_transformations : t -> unit
end

(* Lol functors... *)
module Scheduler (State : State) (Context : Context) = struct
  let run source =
    Hashtbl.clear Context.transformations;
    let module Fixpoint = Fixpoint.Make (State) in
    let lookup = LivenessCheck.NestedDefineLookup.Table.create () in
    let rec run ~state ~define =
      let cfg = Cfg.create define.Node.value in
      Fixpoint.forward ~cfg ~initial:(State.initial ~state ~lookup ~define)
      |> Fixpoint.exit
      >>| (fun state -> Fixpoint.backward ~cfg ~initial:state)
      >>= Fixpoint.entry
      >>| (fun state ->
            State.update_transformations state;
            State.nested_defines state
            |> Map.iteri ~f:(fun ~key ~data:{ NestedDefines.nested_define; state } ->
                   run ~state:(Some state) ~define:{ Node.location = key; value = nested_define }))
      |> ignore
    in
    let define = Source.top_level_define_node source in
    run ~state:None ~define;
    let module Transform = Transform.MakeStatementTransformer (struct
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
          | _ -> transformed
        in
        (), transformed
    end)
    in
    Transform.transform () source |> Transform.source
end

let run ~configuration:_ ~global_resolution ~source:({ Source.qualifier; _ } as source) =
  let module Context = struct
    let global_resolution = global_resolution

    let transformations = Location.Reference.Table.create ()

    let errors = LivenessCheck.ErrorMap.Table.create ()
  end
  in
  (* Constant propagation. *)
  let source =
    let module State = ConstantPropagationState (Context) in
    let module ConstantPropagationScheduler = Scheduler (State) (Context) in
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
        let bump character = Char.to_int character |> ( + ) 1 |> Char.of_int_exn in
        let to_string reversed_list = List.rev reversed_list |> String.of_char_list in
        match String.to_list_rev !last_identifier with
        | [] -> "a"
        | 'z' :: _ as characters ->
            let next_identifier =
              let next_identifier =
                let rec next_identifier = function
                  | 'z' :: tail -> 'a' :: next_identifier tail
                  | character :: tail -> bump character :: tail
                  | [] -> []
                in
                next_identifier characters
              in
              if List.for_all next_identifier ~f:(Char.equal 'a') then
                'a' :: next_identifier
              else
                next_identifier
            in
            next_identifier |> to_string
        | character :: tail -> bump character :: tail |> to_string
      in
      last_identifier := identifier;
      identifier
    in
    let replacements = String.Table.create () in
    let sanitize_name simple_name =
      let replace identifier =
        let replacement = generate_identifier () in
        Hashtbl.set replacements ~key:identifier ~data:replacement;
        replacement
      in
      match simple_name with
      | Name.Identifier identifier when String.length (Identifier.sanitized identifier) > 15 ->
          let replacement = replace identifier in
          Name.Identifier replacement
      | Name.Attribute ({ attribute = identifier; _ } as name)
        when String.length (Identifier.sanitized identifier) > 15 ->
          let replacement = replace identifier in
          Name.Attribute { name with attribute = replacement }
      | name -> name
    in
    let sanitize_reference reference =
      let last = Reference.last reference in
      if String.length last > 15 then (
        let replacement = generate_identifier () in
        Hashtbl.set replacements ~key:last ~data:replacement;
        Reference.create ?prefix:(Reference.prefix reference) replacement )
      else
        reference
    in
    let sanitize_identifier identifier =
      if String.length (Identifier.sanitized identifier) > 15 then (
        let replacement = generate_identifier () in
        Hashtbl.set replacements ~key:identifier ~data:replacement;
        replacement )
      else
        identifier
    in
    let module ScopeTransform = Transform.MakeStatementTransformer (struct
      type t = unit

      let statement state statement =
        let transformed =
          let value =
            match Node.value statement with
            | Define ({ Define.signature = { name; parameters; _ }; body } as define) ->
                (* Scope parameters to the function. *)
                let names = String.Hash_set.create () in
                let scope_name identifier =
                  let qualifier =
                    Reference.show name |> String.substr_replace_all ~pattern:"." ~with_:"?"
                  in
                  identifier
                  |> String.chop_prefix_exn ~prefix:"$parameter"
                  |> Format.asprintf "$parameter_%s%s" qualifier
                in
                let parameters =
                  let parameter ({ Node.value = { Parameter.name; _ } as parameter; _ } as node) =
                    Hash_set.add names name;
                    { node with Node.value = { parameter with Parameter.name = scope_name name } }
                  in
                  List.map parameters ~f:parameter
                in
                let body =
                  let module Transform = Transform.Make (struct
                    type t = unit

                    let transform_expression_children _ _ = true

                    let expression _ expression =
                      let value =
                        match Node.value expression with
                        | Name name when Expression.is_simple_name name ->
                            let rec convert name =
                              let convert_identifier identifier =
                                if Hash_set.mem names identifier then
                                  scope_name identifier
                                else
                                  identifier
                              in
                              match name with
                              | Name.Identifier identifier ->
                                  Name.Identifier (convert_identifier identifier)
                              | Name.Attribute
                                  {
                                    base = { Node.location; value = Name name };
                                    attribute;
                                    special;
                                  } ->
                                  Name.Attribute
                                    {
                                      base = { Node.location; value = Name (convert name) };
                                      attribute = convert_identifier attribute;
                                      special;
                                    }
                              | Name.Attribute { base; attribute; special } ->
                                  Name.Attribute
                                    { base; attribute = convert_identifier attribute; special }
                            in
                            Name (convert name)
                        | value -> value
                      in
                      { expression with Node.value }


                    let transform_children _ _ = true

                    let statement _ statement = (), [statement]
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
                      ({ Node.value = { Parameter.name; _ } as parameter; _ } as node)
                    =
                    let name = sanitize_identifier name in
                    { node with Node.value = { parameter with Parameter.name } }
                  in
                  List.map parameters ~f:sanitize_parameter
                in
                let signature =
                  { define.signature with name = sanitize_reference name; parameters }
                in
                Define { signature; body }
            | For ({ For.target = { Node.value = Name name; _ } as target; _ } as block)
              when Expression.is_simple_name name ->
                let target = { target with Node.value = Name (sanitize_name name) } in
                For { block with For.target }
            | Global globals -> Global (List.map globals ~f:sanitize_identifier)
            | value -> value
          in
          { statement with Node.value }
        in
        state, [transformed]
    end)
    in
    let module Transform = Transform.Make (struct
      type t = unit

      let transform_expression_children _ _ = true

      let expression _ expression =
        let rec sanitize { Node.location; value } =
          match value with
          | Name (Name.Attribute { base; attribute; special }) ->
              let base = sanitize base in
              let value =
                match Hashtbl.find replacements attribute with
                | Some replacement ->
                    Name (Name.Attribute { base; attribute = replacement; special })
                | None -> Name (Name.Attribute { base; attribute; special })
              in
              { Node.location; value }
          | Name (Name.Identifier identifier) ->
              let value =
                match Hashtbl.find replacements identifier with
                | Some replacement -> Name (Name.Identifier replacement)
                | None -> Name (Name.Identifier identifier)
              in
              { Node.location; value }
          | Call { callee; arguments } ->
              { Node.location; value = Call { callee = sanitize callee; arguments } }
          | _ -> { Node.location; value }
        in
        sanitize expression


      let transform_children _ _ = true

      let statement state statement =
        let transformed =
          let value =
            match Node.value statement with
            | Assign ({ target; _ } as assign) ->
                let rec sanitize_target target =
                  match target with
                  | { Node.value = Name name; _ } as target when Expression.is_simple_name name ->
                      { target with Node.value = Name (sanitize_name name) }
                  | { Node.value = Tuple targets; _ } as tuple ->
                      { tuple with Node.value = Tuple (List.map targets ~f:sanitize_target) }
                  | _ -> target
                in
                Assign { assign with Assign.target = sanitize_target target }
            | value -> value
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
    let module Transform = Transform.Make (struct
      type t = unit

      let dequalify_reference reference =
        if Reference.is_strict_prefix ~prefix:qualifier reference then
          Reference.drop_prefix ~prefix:qualifier reference
        else
          reference


      let transform_expression_children _ _ = true

      let expression _ expression =
        let rec dequalify { Node.location; value } =
          match value with
          | Name name when Expression.is_simple_name name ->
              Expression.name_to_reference_exn name
              |> dequalify_reference
              |> Expression.create_name_from_reference ~location
              |> fun name -> Name name |> Node.create ~location
          | Name (Name.Attribute ({ base; _ } as name)) ->
              Name (Name.Attribute { name with base = dequalify base }) |> Node.create ~location
          | Call { callee; arguments } ->
              Call { callee = dequalify callee; arguments } |> Node.create ~location
          | _ -> { Node.value; location }
        in
        Expression.sanitized expression |> dequalify


      let transform_children _ _ = true

      let statement _ statement =
        let transformed =
          let value =
            match Node.value statement with
            | Define ({ Define.signature = { name; parameters; _ }; _ } as define) ->
                let parameters =
                  let sanitize_parameter =
                    let sanitize_parameter ({ Parameter.name; _ } as parameter) =
                      { parameter with Parameter.name = Identifier.sanitized name }
                    in
                    Node.map ~f:sanitize_parameter
                  in
                  List.map parameters ~f:sanitize_parameter
                in
                let signature =
                  { define.signature with name = dequalify_reference name; parameters }
                in
                Define { define with signature }
            | Try ({ Try.handlers; _ } as block) ->
                let handlers =
                  let sanitize_handler ({ Try.name; _ } as handler) =
                    { handler with Try.name = name >>| Identifier.sanitized }
                  in
                  List.map handlers ~f:sanitize_handler
                in
                Try { block with Try.handlers }
            | value -> value
          in
          { statement with Node.value }
        in
        (), [transformed]
    end)
    in
    Transform.transform () source |> Transform.source
  in
  (* Fix up AST. *)
  let source =
    let module Transform = Transform.MakeStatementTransformer (struct
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
                  fix_statement_list body |> remove_docstring
                in
                let signature = { define.signature with docstring = None } in
                Define { signature; body }
            | value -> value
          in
          { statement with Node.value }
        in
        (), [transformed]
    end)
    in
    Transform.transform () source |> Transform.source
  in
  (* Create error. *)
  let { Node.location; value = define } = Source.top_level_define_node source in
  [Error.create ~location ~kind:(Error.Deobfuscation source) ~define:(Node.create define ~location)]
