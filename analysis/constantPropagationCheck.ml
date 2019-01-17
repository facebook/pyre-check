(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement


module Error = AnalysisError


let name =
  "ConstantPropagation"


module type Context = sig
  val configuration: Configuration.Analysis.t
  val define: Define.t Node.t
  val environment: (module Environment.Handler)
  val transformations: Statement.t Location.Reference.Table.t
end


module State (Context: Context) = struct
  type constant =
    | Constant of Expression.t
    | Top
  [@@deriving show]


  type t = {
    constants: constant Access.Map.t;
  }


  let show { constants; _ } =
    let print_entry (access, constant) =
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


  let initial =
    { constants = Access.Map.empty }


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


  let forward ?key ({ constants; _ } as state) ~statement =
    let { Node.value = { Define.name; parent;_ }; _ } = Context.define in
    let resolution =
      TypeCheck.resolution_with_key
        ~environment:Context.environment
        ~parent
        ~access:name
        ~key
    in

    (* Update transformations. *)
    let transformed =
      let transform statement =
        let module Transform =
          Transform.Make(struct
            type t = unit
            let expression _ expression =
              match Node.value expression with
              | Access access ->
                  begin
                    Map.find constants access
                    |> function
                    | Some (Constant expression) ->
                        expression
                    | _ ->
                        expression
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
      match Node.value statement with
      | Assign ({ value; _ } as assign) ->
          (* Do not update left hand side of assignment. *)
          let value =
            Statement.Expression value
            |> Node.create_with_default_location
            |> transform
            |> function
            | { Node.value = Statement.Expression value; _ } -> value
            | _ -> failwith "Could not extract expression"
          in
          { statement with Node.value = Assign { assign with value }}
      | _ ->
          transform statement
    in
    Hashtbl.set Context.transformations ~key:(Node.location transformed) ~data:transformed;

    (* Find new constants. *)
    let constants =
      match Node.value transformed with
      | Assign { target = { Node.value = Access access; _ }; value = expression; _ }  ->
          let propagate =
            let is_literal =
              match Node.value expression with
              | Integer _ | String _ | True | False -> true
              | _ -> false
            in
            let is_callable =
              Resolution.resolve resolution expression
              |> Type.is_callable
            in
            is_literal || is_callable
          in
          if propagate then
            Map.set constants ~key:access ~data:(Constant expression)
          else
            Map.remove constants access
      | _ ->
          constants
    in

    { state with constants }


  let backward ?key:_ _ ~statement:_ =
    failwith "Not implemented"
end


let run
    ~configuration
    ~environment
    ~source:({ Source.qualifier; statements; handle; _ } as source) =
  let define =
    (* TODO(T38205782): run this on all defines. Limiting this to the toplevel for now. *)
    Define.create_toplevel ~qualifier ~statements
    |> Node.create ~location:(Location.Reference.create_with_handle ~handle)
  in
  let module Context =
  struct
    let configuration = configuration
    let define = define
    let environment = environment
    let transformations = Location.Reference.Table.create ()
  end
  in

  (* Collect transformations. *)
  let module State = State(Context) in
  let module Fixpoint = Fixpoint.Make(State) in
  Fixpoint.forward
    ~cfg:(Cfg.create (Node.value define))
    ~initial:State.initial
  |> ignore;

  (* Apply transformations. *)
  let source =
    let module Transform =
      Transform.MakeStatementTransformer(struct
        type t = unit
        let statement _ statement =
          let transformed =
            Hashtbl.find Context.transformations (Node.location statement)
            |> Option.value ~default:statement
          in
          (), [transformed]
      end)
    in
    Transform.transform () source
    |> Transform.source
  in
  [
    Error.create
      ~location:(Node.location define)
      ~kind:(Error.ConstantPropagation source)
      ~define:define;
  ]
