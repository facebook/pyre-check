(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type CONFIG = sig
  val route: 'a AbstractDomain.part -> [ `Left | `Right | `Both ]
end


module Make
    (Config : CONFIG)
    (Left : AbstractDomain.S)
    (Right : AbstractDomain.S) : sig
  include AbstractDomain.S

  val make: Left.t -> Right.t -> t
  val get: t -> Left.t * Right.t
end
