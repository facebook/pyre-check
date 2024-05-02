(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)


type 'a in_channel = Timeout.in_channel
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

let from_channel : ?timeout:Timeout.t -> 'a in_channel -> 'a = fun ?timeout ic ->
  Timeout.input_value ?timeout ic

let flush : 'a out_channel -> unit = Stdlib.flush

let descr_of_in_channel : 'a in_channel -> Unix.file_descr =
  Timeout.descr_of_in_channel

let descr_of_out_channel : 'a out_channel -> Unix.file_descr =
  Unix.descr_of_out_channel

let cast_in ic = ic
let cast_out oc = oc

let fd_of_path path =
  Sys_utils.with_umask 0o111 begin fun () ->
    Sys_utils.mkdir_no_fail (Filename.dirname path);
    Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o666
  end

let null_fd () = fd_of_path Sys_utils.null_path

let setup_channels () =
      let parent_in, child_out = Unix.pipe () in
      let child_in, parent_out = Unix.pipe () in
      (* Close descriptors on exec so they are not leaked. *)
      Unix.set_close_on_exec parent_in;
      Unix.set_close_on_exec parent_out;
      (parent_in, child_out), (child_in, parent_out)

let make_pipe (descr_in, descr_out)  =
  let ic = Timeout.in_channel_of_descr descr_in in
  let oc = Unix.out_channel_of_descr descr_out in
  ic, oc

let close_pipe (ch_in, ch_out) =
    Timeout.close_in ch_in;
    close_out ch_out

(* This only works on Unix, and should be avoided as far as possible. Use
 * Daemon.spawn instead. *)
let fork
    (type param)
    (log_stdout, log_stderr)
    (f : param -> ('a, 'b) channel_pair -> unit)
    (param : param) : ('b, 'a) handle =
  let (parent_in, child_out), (child_in, parent_out) = setup_channels () in
  let (parent_in, child_out) = make_pipe (parent_in, child_out) in
  let (child_in, parent_out) = make_pipe (child_in, parent_out) in
  match Fork.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      (try
         ignore(Unix.setsid());
         close_pipe (parent_in, parent_out);
         Sys_utils.with_umask 0o111 begin fun () ->
           let fd = null_fd () in
           Unix.dup2 fd Unix.stdin;
           Unix.close fd;
         end;
         Unix.dup2 log_stdout Unix.stdout;
         Unix.dup2 log_stderr Unix.stderr;
         if log_stdout <> Unix.stdout then Unix.close log_stdout;
         if log_stderr <> Unix.stderr && log_stderr <> log_stdout then
           Unix.close log_stderr;
         f param (child_in, child_out);
         exit 0
       with e ->
         prerr_endline (Printexc.to_string e);
         Printexc.print_backtrace stderr;
         exit 1)
  | pid -> (* parent *)
      close_pipe (child_in, child_out);
      { channels = parent_in, parent_out; pid }

(* for testing code *)
let devnull () =
  let ic = Timeout.open_in "/dev/null" in
  let oc = open_out "/dev/null" in
  {channels = ic, oc; pid = 0}

let close { channels = (ic, oc); _ } =
  Timeout.close_in ic;
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

let close_in = Timeout.close_in

let input_char ic = Timeout.input_char ic

let input_value ic = Timeout.input_value ic
