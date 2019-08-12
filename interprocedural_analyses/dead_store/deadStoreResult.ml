(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Interprocedural

module ResultArgument = struct
  type result = string

  type call_model = int [@@deriving show]

  let name = "liveness"

  let empty_model = 0

  let obscure_model = -1

  let get_errors _ = []

  let join ~iteration:_ a b = a + b

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next = next <= previous

  let externalize ~environment:_ callable result_option model =
    let result_json =
      match result_option with
      | None -> `Null
      | Some result -> `String result
    in
    [ `Assoc
        [ "analysis", `String name;
          "name", `String (Callable.show callable);
          "model", `Int model;
          "result", result_json ] ]


  let metadata () = `Assoc ["codes", `List [`String "A"]]

  let strip_for_callsite model = model
end

include Interprocedural.Result.Make (ResultArgument)
