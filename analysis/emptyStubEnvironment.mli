(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
module EmptyStubReadOnly : sig
  include Environment.ReadOnly

  val from_empty_stub : t -> ?dependency:SharedMemoryKeys.DependencyKey.t -> Ast.Reference.t -> bool

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
end

include Environment.S with module ReadOnly = EmptyStubReadOnly

module PreviousEnvironment = UnannotatedGlobalEnvironment
