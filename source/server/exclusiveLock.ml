(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t = {
  mutex: Lwt_mutex.t;
  value: 'a ref;
}

let create initial_value = { mutex = Lwt_mutex.create (); value = ref initial_value }

let read ~f { mutex; value } = Lwt_mutex.with_lock mutex (fun () -> f !value)

let write ~f { mutex; value } =
  Lwt_mutex.with_lock mutex (fun () ->
      let open Lwt.Infix in
      f !value
      >>= fun (new_value, result) ->
      value := new_value;
      Lwt.return result)


let unsafe_read { value; _ } = !value
