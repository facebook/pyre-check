(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Invalid_Int_Size_Exception
exception Payload_Size_Too_Large_Exception
exception Malformed_Preamble_Exception
exception Writing_Preamble_Exception
exception Writing_Payload_Exception
exception Reading_Preamble_Exception
exception Reading_Payload_Exception

type remote_exception_data = {
  message : string;
  stack : string;
}

val to_fd_with_preamble: Unix.file_descr -> 'a -> unit
val from_fd_with_preamble: Unix.file_descr -> 'a
