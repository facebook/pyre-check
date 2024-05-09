(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

type handle = {
  pid: int;
  infd: Unix.file_descr;
  outfd: Unix.file_descr;
}

let fork (f : _ -> _ -> _ -> 'a) param =
  let parent_in, child_out = Unix.pipe () in
  let child_in, parent_out = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close parent_in;
    Unix.close parent_out;
    f param child_in child_out;
    exit 0
  | pid ->
    Unix.close child_in;
    Unix.close child_out;
    { pid; infd = parent_in; outfd = parent_out }

let kill_and_wait { pid; infd; outfd } =
  Unix.close infd;
  Unix.close outfd;
  Unix.kill pid Sys.sigkill;
  let rec waitpid () =
    try ignore (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ()
  in
  waitpid ()
