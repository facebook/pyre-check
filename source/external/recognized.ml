(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This contains the OSS values for global constants that differ from the Meta-internal values. *)

open Core

let classmethod_decorators = String.Set.of_list ["classmethod"]

let property_decorators =
  String.Set.of_list
    [
      "abc.abstractproperty";
      "functools.cached_property";
      "__property__";
      "property";
      "builtins.property";
      "_magic_enum_attr";
    ]


let classproperty_decorators = String.Set.of_list ["__classproperty__"]

let ignored_decorators_for_higher_order = []

let enumeration_classes = String.Set.of_list ["enum.Enum"; "enum.IntEnum"]

let assert_functions =
  String.Set.of_list [(* We need a way to test this feature, right? *) "foo.pyretestassert"]


let lazy_import_functions = String.Set.of_list ["lazy_import.lazy_import"]

let allowlisted_callable_class_decorators = String.Set.of_list ["functools._lru_cache_wrapper"]

let readonly_entrypoint_decorators =
  String.Set.of_list ["readonly_stubs_for_testing.readonly_entrypoint"]


let readonly_modules_to_ignore = String.Set.of_list ["readonly_module_to_ignore"]

let classes_safe_to_coerce_readonly_to_mutable =
  String.Set.of_list
    [
      "int";
      "bool";
      "float";
      "readonly_stubs_for_testing.MySafeReadOnlyClass";
      "readonly_stubs_for_testing.MySafeReadOnlyInt";
      "readonly_stubs_for_testing.MySafeReadOnlyIdType";
    ]


(* Tuples of decorators. For any tuple (x, y), it means if a callable is decorated by x, then find
   all callables that are decorated with y in the return type of the callable. *)
let graphql_decorators =
  [
    "graphqlserver.types.graphql_root_field", "graphqlserver.types.graphql_field";
    "test.decorator_1", "test.decorator_2" (* For testing only *);
    ( "graphql_callees.entrypoint_decorator",
      "graphql_callees.method_decorator" (* For testing only *) );
  ]
