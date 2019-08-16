(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

val expand_relative_imports : Source.t -> Source.t

val expand_string_annotations : Source.t -> Source.t

val expand_format_string : Source.t -> Source.t

(* Resolve imports and fully qualify names. *)
val qualify : Source.t -> Source.t

(* Resolves sys.version_info related checks at parse time. *)
val replace_version_specific_code : Source.t -> Source.t

(* Uses heuristics to pick platform-specific code at parse time. *)
val replace_platform_specific_code : Source.t -> Source.t

val expand_type_checking_imports : Source.t -> Source.t

(* Add implicit returns when a function doesn't have one. *)
val expand_implicit_returns : Source.t -> Source.t

val replace_mypy_extensions_stub : Source.t -> Source.t

val expand_typed_dictionary_declarations : Source.t -> Source.t

(* List of function definitions in a source. include_toplevels copies all definitions into a
   toplevel definition. *)
val defines
  :  ?include_stubs:bool ->
  ?include_nested:bool ->
  ?include_toplevels:bool ->
  Source.t ->
  Statement.Define.t Node.t list

(* `count_defines source` should be the same as `List.length (defines ~include_stubs:true
   ~include_nested:true ~include_toplevels:true source)` except it's more efficient *)
val count_defines : Source.t -> int

(* List of class definitions in a source. *)
val classes : Source.t -> Statement.Class.t Node.t list

(* Creates a map used for dequalification from imports in the source *)
val dequalify_map : Source.t -> Reference.t Reference.Map.t

(* Steps that may affect wildcard imports *)
val preprocess_phase0 : Source.t -> Source.t

(* Steps that does not affect wildcard imports *)
val preprocess_phase1 : Source.t -> Source.t

(* Phase0 followed by Phase1 *)
val preprocess : Source.t -> Source.t
