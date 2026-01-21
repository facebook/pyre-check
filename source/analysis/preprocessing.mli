(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val expand_relative_imports : Source.t -> Source.t

val expand_import_python_calls : Source.t -> Source.t

val expand_string_annotations : preserve_original_location:bool -> Source.t -> Source.t

val expand_strings_in_annotation_expression
  :  preserve_original_location:bool ->
  Expression.t ->
  Expression.t

val get_qualified_local_identifier : qualifier:Reference.t -> Identifier.t -> Identifier.t

val get_unqualified_local_identifier : Identifier.t -> (Reference.t * string) option

val get_qualified_parameter : Identifier.t -> Identifier.t

val get_unqualified_parameter : Identifier.t -> string option

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
val replace_platform_specific_code : sys_platform:string -> Source.t -> Source.t

val expand_type_checking_imports : Source.t -> Source.t

(* Add implicit returns when a function doesn't have one. *)
val expand_implicit_returns : Source.t -> Source.t

val replace_lazy_import : ?is_lazy_import:(Reference.t -> bool) -> Source.t -> Source.t

val expand_typed_dictionary_declarations : Source.t -> Source.t

val expand_named_tuples : Source.t -> Source.t

val expand_new_types : Source.t -> Source.t

val expand_sqlalchemy_declarative_base : Source.t -> Source.t

val populate_captures : Source.t -> Source.t

val populate_unbound_names : Source.t -> Source.t

val replace_union_shorthand : Source.t -> Source.t

val mangle_private_attributes : Source.t -> Source.t

val inline_six_metaclass : Source.t -> Source.t

val expand_pytorch_register_buffer : Source.t -> Source.t

val add_dataclass_keyword_only_specifiers : Source.t -> Source.t

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

(* List of global variable assignments in a source. *)
val toplevel_assigns : Source.t -> Statement.Assign.t Node.t list

(* Turn tuple assignments `a, b = c, d` into multiple sequential assignments `a = c` and `b = d`.
   Warning, in general this is unsound due to the left values being evaluated before being assigned
   to the right identifiers, though it is safe if the left values do not access the right
   identifiers*)
val toplevel_expand_tuple_assign : Statement.Assign.t Node.t -> Statement.Assign.t Node.t list

(* Creates a map used for dequalification from imports in the source *)
val dequalify_map : Source.t -> Reference.t Reference.Map.t

(* Steps that may affect wildcard imports, excluding version-specific code replacement *)
val preprocess_before_wildcards : Source.t -> Source.t

(* Steps that do not affect wildcard imports *)
val preprocess_after_wildcards : string_annotation_preserve_location:bool -> Source.t -> Source.t

(* All preprocessing steps, skipping wildcard expansion *)
val preprocess_no_wildcards : string_annotation_preserve_location:bool -> Source.t -> Source.t

(* Following are exposed for testing only *)

module Qualify : sig
  type alias = { name: Reference.t }

  type scope = {
    module_name: Reference.t;
    parent: NestingContext.t;
    aliases: alias Core.String.Map.t;
    locals: Core.String.Set.t;
  }

  val qualify_statement : scope:scope -> Statement.t -> Statement.t

  val qualify_match_case : scope:scope -> Statement.Match.Case.t -> Statement.Match.Case.t

  val qualify_pattern : scope:scope -> Statement.Match.Pattern.t -> Statement.Match.Pattern.t
end

module SelfType : sig
  val is_synthetic_type_variable : string -> bool

  val expand_self_type : Source.t -> Source.t
end

val expand_enum_functional_syntax : Source.t -> Source.t

val drop_nested_body : Statement.Define.t Node.t -> Statement.Define.t Node.t
