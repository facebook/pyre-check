(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = Target.t list Target.Map.t

let to_alist = Target.Map.to_alist

let dump ~path graph =
  let module Buffer = Caml.Buffer in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "{\n";
  let remove_trailing_comma () =
    Buffer.truncate buffer (Buffer.length buffer - 2);
    Buffer.add_string buffer "\n"
  in
  let add_edges ~key:source ~data:targets =
    let add_edge target = Format.asprintf "    \"%s\",\n" target |> Buffer.add_string buffer in
    if not (List.is_empty targets) then (
      Format.asprintf "  \"%s\": [\n" (Target.external_name source) |> Buffer.add_string buffer;
      List.map targets ~f:Target.external_name
      |> List.sort ~compare:String.compare
      |> List.iter ~f:add_edge;
      remove_trailing_comma ();
      Buffer.add_string buffer "  ],\n")
  in
  Target.Map.iteri graph ~f:add_edges;
  remove_trailing_comma ();
  Buffer.add_string buffer "}";

  (* Write to file. *)
  path |> File.create ~content:(Buffer.contents buffer) |> File.write


let pp formatter edges =
  let pp_edge (callable, data) =
    let targets =
      List.map data ~f:Target.show_pretty_with_kind
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:" "
    in
    Format.fprintf formatter "%a -> [%s]\n" Target.pp_pretty_with_kind callable targets
  in
  let compare (left, _) (right, _) =
    String.compare (Target.show_internal left) (Target.show_internal right)
  in
  Target.Map.to_alist edges |> List.sort ~compare |> List.iter ~f:pp_edge
