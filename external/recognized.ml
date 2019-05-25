(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

let asyncio_contextmanager_decorators = String.Set.of_list ["contextlib.asynccontextmanager"]

let classmethod_decorators = String.Set.of_list ["classmethod"]

let property_decorators = String.Set.of_list ["abc.abstractproperty"; "__property__"; "property"]

let classproperty_decorators = String.Set.of_list ["pyre_extensions.classproperty"]

let enumeration_classes = String.Set.of_list ["enum.Enum"; "enum.IntEnum"]

let assert_functions =
  String.Set.of_list
    [(* We need a way to test this feature, right? *) "pyretestassert"; "foo.pyretestassert"]
