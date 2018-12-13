(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val add_overrides: ancestor: Callable.t -> children: Callable.t list -> unit
val get_overrides: ancestor: Callable.t -> Callable.t list option
