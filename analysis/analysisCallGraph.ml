(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


(* Dummy implementation for plumbing. Do not use! *)
type t = {
  overloads: Access.t Access.Table.t;
  callers: Access.t list;
  call_edges: Access.t Access.Table.t;
}


let create () =
  {
    overloads = Access.Table.create ();
    callers = [];
    call_edges = Access.Table.create ();
  }


module type Handler = sig
  (* Module providing interface to access data structure. *)
  val register_overload: access: Access.t -> overload: Access.t -> unit
  val register_caller: path: string -> caller: Access.t -> unit
  val register_call_edge: caller: Access.t -> callee: Access.t -> unit
  val callers: path: string -> Access.t list option
  val callees: caller: Access.t -> Access.t list option
end


let handler { overloads; _ }: (module Handler) =
  (* Creates handler for in-memory storage. *)
  (module struct
    let register_overload ~access ~overload =
      (* Blindly overrides previously registered overload... *)
      Hashtbl.set overloads ~key:access ~data:overload

    (* Stubs *)
    let register_caller ~path:_ ~caller:_ =
      ()

    let register_call_edge ~caller:_ ~callee:_ =
      ()

    let callers ~path:_ =
      None

    let callees ~caller:_ =
      None
  end)

let stub () = create () |> handler
