(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let asyncio_contextmanager_decorators = String.Set.of_list ["contextlib.asynccontextmanager"]

let classmethod_decorators = String.Set.of_list ["classmethod"]

let property_decorators =
  String.Set.of_list
    [
      "abc.abstractproperty";
      "functools.cached_property";
      "__property__";
      "property";
      "builtins.property";
    ]


let classproperty_decorators = String.Set.of_list ["pyre_extensions.classproperty"]

let enumeration_classes = String.Set.of_list ["enum.Enum"; "enum.IntEnum"]

let assert_functions =
  String.Set.of_list [(* We need a way to test this feature, right? *) "foo.pyretestassert"]


let lazy_import_functions = String.Set.of_list ["lazy_import.lazy_import"]

let allowlisted_callable_class_decorators = String.Set.of_list ["functools._lru_cache_wrapper"]
