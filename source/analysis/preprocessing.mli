(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val expand_relative_imports : Source.t -> Source.t

val expand_import_python_calls : Source.t -> Source.t

val expand_string_annotations : Source.t -> Source.t

val expand_strings_in_annotation_expression : Expression.t -> Expression.t

val qualify_local_identifier : qualifier:Reference.t -> Identifier.t -> Identifier.t

val replace_union_shorthand_in_annotation_expression : Expression.t -> Expression.t

(* Resolve imports and fully qualify names. *)
val qualify : Source.t -> Source.t

(* Resolves sys.version_info related checks at parse time. *)
val replace_version_specific_code
  :  major_version:int ->
  minor_version:int ->
  micro_version:int ->
  Source.t ->
  Source.t

(* Uses heuristics to pick platform-specific code at parse time. *)
val replace_platform_specific_code : Source.t -> Source.t

val expand_type_checking_imports : Source.t -> Source.t

(* Add implicit returns when a function doesn't have one. *)
val expand_implicit_returns : Source.t -> Source.t

val replace_lazy_import : ?is_lazy_import:(Expression.t -> bool) -> Source.t -> Source.t

val replace_mypy_extensions_stub : Source.t -> Source.t

val expand_typed_dictionary_declarations : Source.t -> Source.t

val expand_named_tuples : Source.t -> Source.t

val expand_new_types : Source.t -> Source.t

val expand_sqlalchemy_declarative_base : Source.t -> Source.t

val populate_nesting_defines : Source.t -> Source.t

val populate_captures : Source.t -> Source.t

val populate_unbound_names : Source.t -> Source.t

val replace_union_shorthand : Source.t -> Source.t

val mangle_private_attributes : Source.t -> Source.t

val inline_six_metaclass : Source.t -> Source.t

val expand_pytorch_register_buffer : Source.t -> Source.t

(* List of function definitions in a source. include_toplevels copies all definitions into a
   toplevel definition. *)
val defines
  :  ?include_stubs:bool ->
  ?include_nested:bool ->
  ?include_toplevels:bool ->
  ?include_methods:bool ->
  Source.t ->
  Statement.Define.t Node.t list

(* `count_defines source` should be the same as `List.length (defines ~include_stubs:true
   ~include_nested:true ~include_toplevels:true source)` except it's more efficient *)
val count_defines : Source.t -> int

(* List of class definitions in a source. *)
val classes : Source.t -> Statement.Class.t Node.t list

(* Creates a map used for dequalification from imports in the source *)
val dequalify_map : Source.t -> Reference.t Reference.Map.t

(* Steps that may affect wildcard imports, excluding version-specific code replacement *)
val preprocess_phase0 : Source.t -> Source.t

(* Steps that does not affect wildcard imports *)
val preprocess_phase1 : Source.t -> Source.t

(* Phase0 followed by Phase1 *)
val preprocess : Source.t -> Source.t

(* Following are exposed for testing only *)

module type QualifyContext = sig
  val source_relative : string

  val source_qualifier : Reference.t
end

module Qualify (Context : QualifyContext) : sig
  type alias = {
    name: Reference.t;
    qualifier: Reference.t;
    is_forward_reference: bool;
  }

  type scope = {
    qualifier: Reference.t;
    aliases: alias Reference.Map.t;
    immutables: Reference.Set.t;
    locals: Reference.Set.t;
    use_forward_references: bool;
    is_top_level: bool;
    skip: Location.Set.t;
    is_in_function: bool;
    is_in_class: bool;
  }

  val qualify_statement : qualify_assign:bool -> scope:scope -> Statement.t -> scope * Statement.t

  val qualify_match_case : scope:scope -> Statement.Match.Case.t -> scope * Statement.Match.Case.t

  val qualify_pattern : scope:scope -> Statement.Match.Pattern.t -> Statement.Match.Pattern.t
end

module SelfType : sig
  val expand_self_type : Source.t -> Source.t
end
