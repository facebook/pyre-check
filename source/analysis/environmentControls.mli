(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type raw_code = string

type t

val create
  :  ?populate_call_graph:bool ->
  ?in_memory_sources:(Ast.ModulePath.t * string) list ->
  ?no_validation_on_class_lookup_failure:bool ->
  ?use_lazy_module_tracking:bool ->
  ?string_annotation_preserve_location:bool ->
  Configuration.Analysis.t ->
  t

val create_for_overlay : t -> t

val configuration : t -> Configuration.Analysis.t

val track_dependencies : t -> bool

val debug : t -> bool

val in_memory_sources : t -> (Ast.ModulePath.t * string) list option

val no_validation_on_class_lookup_failure : t -> bool

val assert_allow_updates : t -> unit

val populate_call_graph : t -> bool

val use_lazy_module_tracking : t -> bool

val string_annotation_preserve_location : t -> bool

module TypeCheckControls : sig
  type t = {
    include_type_errors: bool;
    include_local_annotations: bool;
    include_readonly_errors: bool;
    include_strict_override_errors: bool;
    include_unawaited_awaitable_errors: bool;
    debug: bool;
    include_suppressed_errors: bool;
    no_validation_on_class_lookup_failure: bool;
  }
end

val type_check_controls : t -> TypeCheckControls.t
