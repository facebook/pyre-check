(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


let property_decorators =
  String.Set.of_list [
    "abc.abstractproperty";
    "property";
  ]


let enumeration_classes =
  String.Set.of_list [
    "enum.Enum";
    "enum.IntEnum";
  ]
