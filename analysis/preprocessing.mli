(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


(* Resolve imports and fully qualify names. *)
val qualify: Source.t -> Source.t

val expand_optional_assigns: Source.t -> Source.t

val expand_operators: Source.t -> Source.t

(* Transform returns to make them more amenable for analysis. *)
val return_access: Access.t
val expand_returns: Source.t -> Source.t

(* Tranforms for loops to handle variable typing inside *)
val expand_for_loop: Source.t -> Source.t

(* Transform `except (E1, ... En) as e` into `... e: typing.Union[E1, ..., En]`. *)
val expand_excepts: Source.t -> Source.t

(* Transforms assignments to ternaries to handle tests *)
val expand_ternary_assign: Source.t -> Source.t

(* List of function definitions in a source. *)
val defines
  :  Source.t
  -> Statement.Define.t Node.t list

(* List of class definitions in a source. *)
val classes
  :  Source.t
  -> (Statement.t Statement.Class.t) Node.t list

(* Creates a map used for dequalification from imports in the source *)
val dequalify_map: Source.t -> Access.t Access.Map.t

(* Most of the above. *)
val preprocess: Source.t -> Source.t
