(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre

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

let write { path; content } =
  (* TODO: Make a variant of the `write` API that writes directly to file without ensuring parent
     dir exists. *)
  PyrePath.ensure_parent_directory_exists path |> ignore;
  match content with
  | Some content -> Core.Out_channel.write_all ~data:content (PyrePath.absolute path) |> ignore
  | None -> Log.error "No contents to write to `%a`" PyrePath.pp path


let append ~lines path =
  let append_lines out_channel = Out_channel.output_lines out_channel lines in
  Out_channel.with_file ~append:true ~fail_if_exists:false ~f:append_lines (PyrePath.absolute path)
