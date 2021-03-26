(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Interprocedural

module ResultArgument = struct
  type result = string list

  type call_model = string [@@deriving show]

  let name = "type_inference"

  let empty_model = "empty_model"

  let obscure_model = "obscure_model"

  let get_errors _ = []

  let join ~iteration:_ left right = left ^ " " ^ right

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous:_ ~next:_ = true

  let externalize ~filename_lookup:_ callable result_option model =
    let result_json =
      match result_option with
      | None -> `Null
      | Some result -> `List (List.map ~f:(fun x -> `String x) result)
    in
    [
      `Assoc
        [
          "analysis", `String name;
          "name", `String (Callable.show callable);
          "model", `String model;
          "result", result_json;
        ];
    ]


  let metadata () = `Assoc []

  let statistics () = `Assoc []

  let strip_for_callsite model = model
end

include Interprocedural.Result.Make (ResultArgument)
