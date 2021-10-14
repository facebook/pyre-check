(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
