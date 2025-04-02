(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TargetGraph: represents a mapping from a target to a list of targets. *)

open Core

type t = Target.t list Target.Map.Tree.t

let dump ~path graph =
  let module Buffer = Stdlib.Buffer in
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
  Target.Map.Tree.iteri graph ~f:add_edges;
  remove_trailing_comma ();
  Buffer.add_string buffer "}";

  (* Write to file. *)
  path |> File.create ~content:(Buffer.contents buffer) |> File.write


let show_target = Target.show_pretty_with_kind

let compare_target left right = String.compare (show_target left) (show_target right)

let to_alist ~sorted edges =
  let edges = Target.Map.Tree.to_alist edges in
  if sorted then
    List.sort ~compare:(fun (left, _) (right, _) -> compare_target left right) edges
    |> List.map ~f:(fun (key, value) -> key, List.sort ~compare:compare_target value)
  else
    edges


let pp formatter edges =
  let pp_edge (callable, callees) =
    let targets = List.map ~f:show_target callees |> String.concat ~sep:" " in
    Format.fprintf formatter "%s -> [%s]\n" (show_target callable) targets
  in
  edges |> to_alist ~sorted:true |> List.iter ~f:pp_edge


let to_json ~skip_empty_callees ~sorted edges =
  let callees_to_json callees =
    `List (List.map ~f:(fun target -> `String (show_target target)) callees)
  in
  to_alist ~sorted edges
  |> List.filter_map ~f:(fun (caller, callees) ->
         if skip_empty_callees && List.is_empty callees then
           None
         else
           Some (show_target caller, callees_to_json callees))
  |> fun list -> `Assoc list
