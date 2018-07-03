(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


val expand_string_annotations: Source.t -> Source.t

val expand_format_string: Source.t -> Source.t

(* Resolve imports and fully qualify names. *)
val qualify: Source.t -> Source.t

(* Resolves sys.version_info related checks at parse time. *)
val replace_version_specific_code: Source.t -> Source.t

val expand_type_checking_imports: Source.t -> Source.t
val expand_wildcard_imports: Source.t -> Source.t

(* Transform returns to make them more amenable for analysis. *)
val return_access: Access.t
val expand_returns: Source.t -> Source.t

(* Transforms assignments to ternaries to handle tests *)
val expand_ternary_assign: Source.t -> Source.t

(* List of function definitions in a source. *)
val defines
  :  ?include_stubs: bool
  -> Source.t
  -> Statement.Define.t Node.t list

(* List of class definitions in a source. *)
val classes
  :  Source.t
  -> Statement.Class.t Node.t list

(* Creates a map used for dequalification from imports in the source *)
val dequalify_map: Source.t -> Access.t Access.Map.t

(* Most of the above. *)
val preprocess: Source.t -> Source.t
