(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)


type 'a in_channel = Stdlib.in_channel
type 'a out_channel = Stdlib.out_channel

type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel

type ('in_, 'out) handle = {
  channels : ('in_, 'out) channel_pair;
  pid : int;
}

let to_channel :
  'a out_channel -> ?flags:Marshal.extern_flags list -> ?flush:bool ->
  'a -> unit =
  fun oc ?(flags = []) ?flush:(should_flush=true) v ->
  Marshal.to_channel oc v flags;
  if should_flush then flush oc

let from_channel : 'a in_channel -> 'a = fun ic ->
  input_value ic

let flush : 'a out_channel -> unit = Stdlib.flush

let descr_of_in_channel : 'a in_channel -> Unix.file_descr =
  Unix.descr_of_in_channel

let descr_of_out_channel : 'a out_channel -> Unix.file_descr =
  Unix.descr_of_out_channel

let cast_in ic = ic
let cast_out oc = oc

let setup_channels () =
      let parent_in, child_out = Unix.pipe () in
      let child_in, parent_out = Unix.pipe () in
      (* Close descriptors on exec so they are not leaked. *)
      Unix.set_close_on_exec parent_in;
      Unix.set_close_on_exec parent_out;
      (parent_in, child_out), (child_in, parent_out)

let make_pipe (descr_in, descr_out)  =
  let ic = Unix.in_channel_of_descr descr_in in
  let oc = Unix.out_channel_of_descr descr_out in
  ic, oc

let close_pipe (ch_in, ch_out) =
    close_in ch_in;
    close_out ch_out

(* This only works on Unix, and should be avoided as far as possible. Use
 * Daemon.spawn instead. *)
let fork
    (type param)
    (f : param -> ('a, 'b) channel_pair -> unit)
    (param : param) : ('b, 'a) handle =
  let (parent_in, child_out), (child_in, parent_out) = setup_channels () in
  let (parent_in, child_out) = make_pipe (parent_in, child_out) in
  let (child_in, parent_out) = make_pipe (child_in, parent_out) in
  match Unix.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      (try
         ignore(Unix.setsid());
         close_pipe (parent_in, parent_out);
         f param (child_in, child_out);
         exit 0
       with e ->
         prerr_endline (Printexc.to_string e);
         Printexc.print_backtrace stderr;
         exit 1)
  | pid -> (* parent *)
      close_pipe (child_in, child_out);
      { channels = parent_in, parent_out; pid }

let close { channels = (ic, oc); _ } =
  close_in ic;
  close_out oc

let kill h =
  close h;
  Sys_utils.terminate_process h.pid

let kill_and_wait h =
  kill h;
  let rec ensure_waitpid pid =
    (* `waitpid` may be interrupted by other signals.
     * When this happens, we will get an EINTR error in which case the wait
     * operation needs to be retried.
     * For some reason, the EINTR issue happens more frequently on MacOS than
     * on Linux... *)
    try
      Unix.waitpid [] pid |> ignore
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      ensure_waitpid pid
  in
  ensure_waitpid h.pid

let close_out = close_out

let output_string = output_string

let flush = flush

let close_in = close_in

let input_char ic = input_char ic

let input_value ic = input_value ic
