(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = { load: string -> (string, string) Result.t }

let create ?(load = fun _ -> Result.Error "`load` interface is not implemented yet") () = { load }

let create_for_testing sources_alist =
  {
    load =
      (fun path ->
        match List.Assoc.find sources_alist ~equal:String.equal path with
        | None ->
            let message = Stdlib.Format.sprintf "File not included in file loader alist: %s" path in
            Result.Error message
        | Some content -> Result.Ok content);
  }


let create_from_sourcedb_lookup ~root { Sourcedb.Lookup.get_source; get_dependency } =
  let lookup_path relative_path =
    match get_source relative_path with
    | Some _ as result -> result
    | None -> get_dependency relative_path
  in
  let load relative_artifact_path =
    match lookup_path relative_artifact_path with
    | None ->
        let message =
          Stdlib.Format.sprintf "Cannot find entry `%s` in sourcedb" relative_artifact_path
        in
        Result.Error message
    | Some relative_source_path -> (
        let source_path = PyrePath.create_relative ~root ~relative:relative_source_path in
        try Result.Ok (File.create source_path |> File.content_exn) with
        | Sys_error error ->
            let message =
              Stdlib.Format.asprintf
                "Cannot open file `%a` due to: %s"
                PyrePath.pp
                source_path
                error
            in
            Result.Error message)
  in
  { load }
