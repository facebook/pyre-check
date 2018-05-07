(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Analysis
open Ast
open Expression


let test_register_overload _ =
  Service.Scheduler.mock () |> ignore;

  let (module Handler: CallGraph.Handler) = Service.CallGraph.shared_memory_handler () in
  Handler.register_overload ~access:(Access.create "one") ~overload:(Access.create "one");
  Handler.register_overload ~access:(Access.create "one") ~overload:(Access.create "two");
  Handler.register_overload ~access:(Access.create "two") ~overload:(Access.create "three")


let () =
  "callGraph">:::[
    "register_overload">::test_register_overload;
  ]
  |> run_test_tt_main
