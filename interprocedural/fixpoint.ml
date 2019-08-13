(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Result = InterproceduralResult
module SharedMemory = Memory

module Epoch = struct
  type t = int [@@deriving show]

  let predefined = 0

  let initial = 1
end

type step = {
  epoch: Epoch.t;
  iteration: int;
}
[@@deriving show]

type state = {
  is_partial: bool;
  (* Whether to reanalyze this and its callers. *)
  model: Result.model_t;
  (* Model to use at call sites. *)
  result: Result.result_t; (* The result of the analysis. *)
}

module SharedModels =
  SharedMemory.WithCache.Make
    (Callable.Key)
    (struct
      type t = Result.model_t

      let prefix = Prefix.make ()

      let description = "InterproceduralFixpointModel"

      let unmarshall value = Marshal.from_string value 0
    end)

module SharedResults =
  SharedMemory.WithCache.Make
    (Callable.Key)
    (struct
      type t = Result.result_t

      let prefix = Prefix.make ()

      let description = "InterproceduralFixpointResults"

      let unmarshall value = Marshal.from_string value 0
    end)

type meta_data = {
  is_partial: bool;
  step: step;
}

(* Caches the fixpoint state (is_partial) of a call model. *)
module SharedFixpoint =
  SharedMemory.WithCache.Make
    (Callable.Key)
    (struct
      type t = meta_data

      let prefix = Prefix.make ()

      let description = "InterproceduralFixpointMetadata"

      let unmarshall value = Marshal.from_string value 0
    end)

module KeySet = SharedModels.KeySet
module KeyMap = SharedModels.KeyMap

let get_new_model callable =
  let callable = (callable :> Callable.t) in
  SharedModels.get callable


let get_old_model callable =
  let callable = (callable :> Callable.t) in
  SharedModels.get_old callable


let get_model callable =
  let callable = (callable :> Callable.t) in
  match get_new_model callable with
  | Some _ as model -> model
  | None -> get_old_model callable


let get_result callable =
  let callable = (callable :> Callable.t) in
  SharedResults.get callable |> Option.value ~default:Result.empty_result


let get_is_partial callable =
  let callable = (callable :> Callable.t) in
  match SharedFixpoint.get callable with
  | Some { is_partial; _ } -> is_partial
  | None -> (
    match SharedFixpoint.get_old callable with
    | None -> true
    | Some { is_partial; _ } -> is_partial )


let get_meta_data callable =
  let callable = (callable :> Callable.t) in
  match SharedFixpoint.get callable with
  | Some _ as meta_data -> meta_data
  | None -> SharedFixpoint.get_old callable


let has_model callable =
  let key = (callable :> Callable.t) in
  SharedModels.mem key


let meta_data_to_string { is_partial; step = { epoch; iteration } } =
  Format.sprintf "{ partial: %b; epoch: %d; iteration: %d }" is_partial epoch iteration


let add_state step callable state =
  let callable = (callable :> Callable.t) in
  (* Separate diagnostics from state to speed up lookups, and cache fixpoint state separately. *)
  let () = SharedModels.add callable state.model in
  (* Skip result writing unless necessary (e.g. overrides don't have results) *)
  let () =
    match callable with
    | #Callable.target_with_result -> SharedResults.add callable state.result
    | _ -> ()
  in
  SharedFixpoint.add callable { is_partial = state.is_partial; step }


let add_predefined epoch callable model =
  let callable = (callable :> Callable.t) in
  let () = SharedModels.add callable model in
  let step = { epoch; iteration = 0 } in
  SharedFixpoint.add callable { is_partial = false; step }


let get_new_models = SharedModels.get_batch

let get_new_results = SharedResults.get_batch

let oldify callable_set =
  SharedModels.oldify_batch callable_set;
  SharedFixpoint.oldify_batch callable_set;

  (* Old results are never looked up, so remove them. *)
  SharedResults.remove_batch callable_set


let remove_new callable_set =
  SharedModels.remove_batch callable_set;
  SharedFixpoint.remove_batch callable_set;
  SharedResults.remove_batch callable_set


let remove_old callable_set =
  SharedModels.remove_old_batch callable_set;
  SharedFixpoint.remove_old_batch callable_set


(* No old results. *)

let is_initial_iteration { epoch = _; iteration } = iteration = 0
