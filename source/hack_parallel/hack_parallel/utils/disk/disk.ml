(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Stub = struct
  let cat = TestDisk.get
end

include (val (if Injector_config.use_test_stubbing
              then (module Stub : Disk_sig.S)
              else (module RealDisk : Disk_sig.S)
             ))
