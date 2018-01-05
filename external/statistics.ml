(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


let disable () = ()


let sample ?(system_time = 0.0) ?(integers = []) ?(normals = []) () =
  ignore system_time;
  ignore integers;
  ignore normals;
  ""


let flush () = ()


let performance
    ?(flush = false)
    ?(randomly_log_every = 1)
    ~name:_
    ~timer:_
    ~root:_
    ?(normals = [])
    () =
  ignore flush;
  ignore randomly_log_every;
  ignore normals


let coverage ?(flush = false) ~coverage:_ ~root:_ ?(normals = []) () =
  ignore flush;
  ignore normals


let event ?(flush = false) ~name:_ ~root:_ ?(integers = []) ?(normals = []) () =
  ignore flush;
  ignore integers;
  ignore normals
