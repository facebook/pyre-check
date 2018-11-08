(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Expression
open Analysis
open Pyre

module SharedMemory = Memory


type real_target = [ `RealTarget of string ]
[@@deriving show, sexp, compare, hash, eq]


type override_target = [ `OverrideTarget of string ]
[@@deriving show, sexp, compare, hash, eq]


type t = [ real_target | override_target ]
[@@deriving show, sexp, compare, hash, eq]


type target_with_stored_result = real_target


let create_real access =
  `RealTarget (Access.show access)


let create_override access =
  `OverrideTarget (Access.show access)


let create { Node.value = Define.{ name; _ }; _ } =
  create_real name


let get_real_access = function
  | `RealTarget name -> Access.create name


let get_override_access = function
  | `OverrideTarget name -> Access.create name


module Key = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let hash = hash
  let hash_fold_t = hash_fold_t
  let to_string = show
  let compare = compare
end


module RealKey = struct
  type t = real_target
  let to_string = show_real_target
  let compare = compare_real_target
end


module FileOfDefinition = SharedMemory.WithCache (RealKey)
    (struct
      type t = File.Handle.t
      let prefix = Prefix.make ()
      let description = "File of definition"
    end)


module Set = Caml.Set.Make(Key)


let add_definition callable handle =
  FileOfDefinition.add callable handle


let define_matches search { Node.value = { Define.name; _ } ; _ } =
  search = Access.show name


let get_definition (`RealTarget name as callable) =
  FileOfDefinition.get callable
  >>= Ast.SharedMemory.Sources.get
  >>| Preprocessing.defines
  >>= List.find ~f:(define_matches name)


let show = function
  | `RealTarget target -> Format.sprintf "%s (real)" target
  | `OverrideTarget target -> Format.sprintf "%s (override)" target


let external_target_name = function
  | `RealTarget target -> target
  | `OverrideTarget target -> Format.sprintf "O{%s}" target


let raw_to_string callable =
  sexp_of_t callable
  |> Sexp.to_string

let compare target1 target2 =
  let target1 = (target1 :> t) in
  let target2 = (target2 :> t) in
  compare target1 target2


module Map = Map.Make(Key)
module Hashable = Hashable.Make(Key)
