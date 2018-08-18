(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre

let disable () = ()


let sample
    ?(system_time = 0.0)
    ?(integers = [])
    ?(normals = [])
    ?(metadata = false)
    () =
  ignore system_time;
  ignore integers;
  ignore normals;
  ignore metadata;
  ""


let flush () = ()


let performance
    ?(flush = false)
    ?(randomly_log_every = 1)
    ?(section = `Performance)
    ~name
    ~timer
    ?(integers = [])
    ?(normals = [])
    () =
  let seconds = Timer.stop timer in
  Log.log ~section "%s: %fs" (String.capitalize_ascii name) seconds;
  ignore flush;
  ignore randomly_log_every;
  ignore integers;
  ignore normals;
  ignore section


let coverage ?(flush = false) ~coverage:_ ?(normals = []) () =
  ignore flush;
  ignore normals


let event ?(flush = false) ?(section = `Event) ~name:_ ?(integers = []) ?(normals = []) () =
  ignore flush;
  ignore section;
  ignore integers;
  ignore normals
