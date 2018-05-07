(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


(* Dummy implementation for plumbing. Do not use! *)
type t = {
  overloads: Access.t Access.Table.t;
}


let create () =
  { overloads = Access.Table.create () }


module type Handler = sig
  (* Module providing interface to access data structure. *)
  val register_overload: access: Access.t -> overload: Access.t -> unit
end


let handler { overloads }: (module Handler) =
  (* Creates handler for in-memory storage. *)
  (module struct
    let register_overload ~access ~overload =
      (* Blindly overrides previously registered overload... *)
      Hashtbl.set overloads ~key:access ~data:overload
  end)
