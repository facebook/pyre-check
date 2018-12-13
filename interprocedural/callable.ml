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


type method_name = {
  class_name: string;
  method_name: string;
}
[@@deriving show, sexp, compare, hash, eq]


type function_target = [ `Function of string ]
[@@deriving show, sexp, compare, hash, eq]


type method_target = [ `Method of method_name ]
[@@deriving show, sexp, compare, hash, eq]


type real_target = [ function_target | method_target ]
[@@deriving show, sexp, compare, hash, eq]


type override_target = [ `OverrideTarget of method_name ]
[@@deriving show, sexp, compare, hash, eq]


(* Technically not a callable, but we store models of some fields/globals,
   E.g. os.environ, or HttpRequest.GET
*)
type object_target = [ `Object of string ]
[@@deriving show, sexp, compare, hash, eq]


type non_override_target = [ real_target | object_target]
[@@deriving show, sexp, compare, hash, eq]


type t = [ non_override_target | override_target ]
[@@deriving show, sexp, compare, hash, eq]


type target_with_result = real_target


let create_function access =
  `Function (Access.show access)


let unqualified_method_name method_access =
  match Access.last method_access with
  | Some (Access.Identifier name) -> Identifier.show name
  | _ ->
      Format.asprintf "Bad method name %a" Access.pp method_access
      |> failwith


let create_method access =
  `Method {
    class_name = Access.show (Access.prefix access);
    method_name = unqualified_method_name access;
  }


let create_override access =
  `OverrideTarget {
    class_name = Access.show (Access.prefix access);
    method_name = unqualified_method_name access;
  }


let create { Node.value = define; _ } =
  match define.Define.parent with
  | Some _ ->
      create_method define.name
  | None ->
      create_function define.name


let create_derived_override override ~at_type =
  match override with
  | `OverrideTarget { method_name; _ } ->
      `OverrideTarget { class_name = Access.show at_type; method_name }


let create_object access =
  `Object (Access.show access)


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
  [@@deriving sexp, hash]
  let to_string = show_real_target
  let compare = compare_real_target
end


module OverrideKey = struct
  type t = override_target
  [@@deriving sexp, hash]
  let to_string = show_override_target
  let compare = compare_override_target
end


(* Maps global function names and class names to their defining file.
   Note, global functions and class names cannot clash. *)
module FileOfDefinition = SharedMemory.WithCache (String)
    (struct
      type t = File.Handle.t
      let prefix = Prefix.make ()
      let description = "File of global function or class"
    end)


module Set = Caml.Set.Make(Key)
module OverrideSet = Caml.Set.Make(OverrideKey)


let add_function_definition function_name handle =
  FileOfDefinition.add (Access.show function_name) handle


let add_class_definition class_name handle =
  FileOfDefinition.add (Access.show class_name) handle


let define_matches search { Node.value = { Define.name; _ } ; _ } =
  search = Access.show name


let class_matches search { Node.value = { Class.name; _ } ; _ } =
  search = Access.show name


let get_definition = function
  | `Function name ->
      FileOfDefinition.get name
      >>= Ast.SharedMemory.Sources.get
      >>| Preprocessing.defines
      >>= List.find ~f:(define_matches name)
  | `Method { class_name; method_name; } ->
      FileOfDefinition.get class_name
      >>= Ast.SharedMemory.Sources.get
      >>| Preprocessing.classes
      >>= List.find ~f:(class_matches class_name)
      >>| Node.value
      >>= Class.find_define ~method_name:(Identifier.create method_name)


let get_override_access = function
  | `OverrideTarget { class_name; method_name } ->
      (Access.create class_name) @ (Access.create method_name)


let get_method_access = function
  | `Method { class_name; method_name } ->
      (Access.create class_name) @ (Access.create method_name)


let get_corresponding_method = function
  | `OverrideTarget method_name ->
      `Method method_name


let get_corresponding_override = function
  | `Method method_name ->
      `OverrideTarget method_name


let show = function
  | `Function target ->
      Format.sprintf "%s (fun)" target
  | `Method { class_name; method_name } ->
      Format.sprintf "%s::%s (method)" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "%s::%s (override)" class_name method_name
  | `Object name ->
      Format.sprintf "%s (object)" name


let external_target_name = function
  | `Function target ->
      target
  | `Method { class_name; method_name } ->
      Format.sprintf "%s.%s" class_name method_name
  | `OverrideTarget { class_name; method_name } ->
      Format.sprintf "Ovr{%s::%s}" class_name method_name
  | `Object name ->
      Format.sprintf "Obj{%s}" name


let raw_to_string callable =
  sexp_of_t callable
  |> Sexp.to_string


let compare target1 target2 =
  let target1 = (target1 :> t) in
  let target2 = (target2 :> t) in
  compare target1 target2


module RealMap = Map.Make(RealKey)
module Map = Map.Make(Key)
module Hashable = Hashable.Make(Key)
