(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module implements exclusive lock, a generic and high-level synchronization primitive.

    A mutex lock can be used to grant exclusive access of a given state in a concurrent program. It
    enforces that one Lwt thread never enters a given critical section while a concurrent Lwt thread
    is already accessing the same data. Such functionality is very useful for a concurrent Pyre
    server, where we want to be held back on responding to client read requests while an incremental
    update is being actively processed in the background. *)

type 'a t
(** An exclusive lock that holds a state of type 'a. Semantically it has almost the same behavior as
    'a ref, except that accesses to the underlying storage must go through the provided APIs that
    offers lock protection. *)

val create : 'a -> 'a t
(** [create s] creates an exclusive lock with its initial state set to [s]. *)

val read : f:('a -> 'b Lwt.t) -> 'a t -> 'b Lwt.t
(** [read ~f exclusive_lock] will try to lock [exclusive_lock] first, then invoke [f] with the
    containing state of [exclusive_lock], and finally unlock [exclusive_lock] once [f] returns. The
    return value of [f] will be used as the return value of the entire [read] operation.

    If [exclusive_lock] is already locked by another Lwt thread, this API will wait until the
    locking thread releases that lock. If there are multiple threads competing on the same lock, the
    lock will be granted in the same order in which they attempt to grab the lock. *)

val write : f:('a -> ('a * 'b) Lwt.t) -> 'a t -> 'b Lwt.t
(** [write ~f exclusive_lock] will try to lock [exclusive_lock] first, then invoke [f] with the
    containing state of [exclusive_lock], update the containing state of [exclusive_lock], and
    finally unlock [exclusive_lock]. The return type of [f] is expected to be a pair that contains
    both the new state that [exclusive_lock] will take, and some additional info that will be
    forwarded as the return value of the entire [write] operation.

    If [exclusive_lock] is already locked by another Lwt thread, this API will wait until the
    locking thread releases that lock. If there are multiple threads competing on the same lock, the
    lock will be granted in the same order in which they attempt to grab the lock. *)

val unsafe_read : 'a t -> 'a
(** [unsafe_read exclusive_lock] returns the containing state of [exclusive_lock] directly.

    WARNING: This function is unsafe since it allows its caller to get access to the locked state
    without obtaining the lock first. Such a blatant violation of the locking protocol would defeat
    the point of using a lock in the first place. Only use this API if you have very strict
    non-blocking requirement and very loose consistency requirements. *)

(** This module implements additional laziness on top of {!type: ExclusiveLock.t}.

    Functionally, a value of ['a ExclusiveLock.Lazy.t] is no different from a
    ['a Lwt.t Lazy.t ExclusiveLock.t]. But this module provides nicer APIs so its clients do not
    need to explicitly wait on the result of {!Lazy.force} themselves. *)
module Lazy : sig
  type 'a t
  (** An exclusive lock that holds a (lazily-initialized) state of type ['a]. Semantically it has
      almost the same behavior as ['a ExclusiveLock.t], except that the state itself does not have
      to be available at creation time. *)

  val create : (unit -> 'a Lwt.t) -> 'a t
  (** [create f] creates a lazy exclusive lock, where [f] is the "initializer" of the state. Upon
      creation, the lock is considered to hold an "uninitialized" state. The first time operations
      like [force], [read], and [write] gets invoked, [f ()] will be called and waited on, after
      which the lock is considered to be "initialized". *)

  val force : 'a t -> 'a Lwt.t
  (** [force lazy_lock] will try to force-initialize the state of the lock. If [lazy_lock] is
      already initialized, the state will be returned directly. Otherwise, the initializer that was
      provided in the {!create} function will be invoked and waited on first.

      Note that the lock itself will be held while initialization is in-progress. If [force] is
      invoked concurrently on an uninitialized state, only the first invocation will go through, and
      the rest will only get the already-initialized state. *)

  val read : f:('a -> 'b Lwt.t) -> 'a t -> 'b Lwt.t
  (** [read ~f lazy_lock] will try to lock [lazy_lock] first, then invoke [f] with the containing
      state of [lazy_lock], and finally unlock [lazy_lock] once [f] returns. If the containing state
      of [lazy_lock] is not yet initialized, a {!force} operation will be performed first before [f]
      is called. The return value of [f] will be used as the return value of the entire [read]
      operation. *)

  val write : f:('a -> ('a * 'b) Lwt.t) -> 'a t -> 'b Lwt.t
  (** [write ~f lazy_lock] will try to lock [lazy_lock] first, then invoke [f] with the containing
      state of [lazy_lock], update the containing state of [lazy_lock], and finally unlock
      [lazy_lock]. If the containing state of [lazy_lock] is not yet initialized, a {!force}
      operation will be performed first before [f] is called. The return type of [f] is expected to
      be a pair that contains both the new state that [lazy_lock] will take, and some additional
      info that will be forwarded as the return value of the entire [write] operation. *)

  val unsafe_read : 'a t -> 'a option
  (** If [lazy_lock] is uninitialized, [unsafe_read lazy_lock] returns [None]. Otherwise,
      [unsafe_read] returns a [Some] of the containing state of [lazy_lock] directly without any
      blocking. *)
end
