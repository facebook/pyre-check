(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module T = struct
  type t = {
    path: PyrePath.t;
    content: string option;
  }
  [@@deriving compare, show, sexp, hash]

  let create ?content path = { path; content }

  let path { path; _ } = path

  let content { path; content } =
    match content with
    | Some content -> Some content
    | None -> (
        try Some (In_channel.read_all (PyrePath.absolute path)) with
        | Sys_error _ -> None)


  let content_exn { path; content } =
    match content with
    | Some content -> content
    | None -> In_channel.read_all (PyrePath.absolute path)


  let lines file = content file >>| String.split ~on:'\n'

  let lines_exn file = content_exn file |> String.split ~on:'\n'

  let hash file = lines file >>| fun lines -> [%hash: string list] lines

  let existing_directories = String.Table.create ()

  let write { path; content } =
    let path = PyrePath.absolute path in
    let make_directories () =
      let directory = Filename.dirname path in
      if not (Hashtbl.mem existing_directories directory) then
        match Core.Sys.is_directory directory with
        | `Yes -> Hashtbl.set existing_directories ~key:directory ~data:()
        | _ -> (
            try
              Core.Unix.mkdir_p directory;
              Hashtbl.set existing_directories ~key:directory ~data:()
            with
            | Sys_error _ -> Log.info "Could not create directory `%s`" directory)
    in
    make_directories ();
    match content with
    | Some content -> Core.Out_channel.write_all ~data:content path |> ignore
    | None -> Log.error "No contents to write to `%s`" path


  let append ~lines path =
    let append_lines out_channel = Out_channel.output_lines out_channel lines in
    Out_channel.with_file
      ~append:true
      ~fail_if_exists:false
      ~f:append_lines
      (PyrePath.absolute path)
end

include T
module Set = Set.Make (T)
