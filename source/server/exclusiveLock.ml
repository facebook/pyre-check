(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lwt.Infix

module T = struct
  type 'a t = {
    mutex: Lwt_mutex.t;
    value: 'a ref;
  }

  let create initial_value = { mutex = Lwt_mutex.create (); value = ref initial_value }

  let read ~f { mutex; value } = Lwt_mutex.with_lock mutex (fun () -> f !value)

  let write ~f { mutex; value } =
    Lwt_mutex.with_lock mutex (fun () ->
        f !value
        >>= fun (new_value, result) ->
        value := new_value;
        Lwt.return result)


  let unsafe_read { value; _ } = !value
end

module Lazy = struct
  module Value = struct
    type 'a t =
      | Uninitialized of (unit -> 'a Lwt.t)
      | Initialized of 'a

    let force = function
      | Uninitialized initialize -> initialize ()
      | Initialized value -> Lwt.return value
  end

  type 'a t = 'a Value.t T.t

  let create initialize = T.create (Value.Uninitialized initialize)

  let write ~f =
    let f value =
      Value.force value
      >>= fun forced_value ->
      f forced_value >>= fun (new_value, result) -> Lwt.return (Value.Initialized new_value, result)
    in
    T.write ~f


  let read ~f =
    let f value = f value >>= fun result -> Lwt.return (value, result) in
    write ~f


  let force lock = read ~f:Lwt.return lock

  let unsafe_read lock =
    match T.unsafe_read lock with
    | Value.Uninitialized _ -> None
    | Value.Initialized value -> Some value
end

include T
