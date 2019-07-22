open Core
open Ast
open Statement
open Pyre
module Error = AnalysisError

let name = "ImmutableCollection"

module type Context = sig
  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t
end

module State (Context : Context) = struct
  module Element = struct
    type t = Location.t

    let pp = Location.pp
  end

  type t = Element.t Reference.Map.t

  let show state =
    Map.to_alist state
    |> List.map ~f:(fun (reference, element) ->
           Format.asprintf "%a -> %a" Reference.pp reference Element.pp element)
    |> String.concat ~sep:", "


  let pp format state = Format.fprintf format "%s" (show state)

  let initial = Reference.Map.empty

  let errors =
    let emit_error ~key:_ ~data:_ errors =
      (* TODO (T44181093): Create a new error type for this check *)
      errors
    in
    Reference.Map.fold ~init:[] ~f:emit_error


  let less_or_equal ~left ~right =
    let less_or_equal (reference, _) = Map.mem right reference in
    Map.to_alist left |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Left state
      | `Right state ->
          Some state
      (* It does not matter which one we pick as long as we are consistent. *)
      | `Both (left, _) -> Some left
    in
    Map.merge left right ~f:merge


  let widen ~previous ~next ~iteration:_ = join previous next

  let forward ?key:_ state ~statement:_ = state

  let backward ?key:_ _ ~statement:_ =
    (* TODO (T44181093): Remove the ignores *)
    ignore Context.define;
    ignore Context.global_resolution;
    failwith "Not implemented"
end

let run ~configuration:_ ~global_resolution ~source =
  let check define =
    let module Context = struct
      let define = define

      let global_resolution = global_resolution
    end
    in
    let module State = State (Context) in
    let module Fixpoint = Fixpoint.Make (State) in
    Fixpoint.forward ~cfg:(Cfg.create (Node.value define)) ~initial:State.initial
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check |> List.concat
