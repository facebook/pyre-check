(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Lwt.Infix

module Queried = struct
  type t = {
    bucket: string;
    path: string;
    target: PyrePath.t;
    changed_files: PyrePath.t list;
    (* For informational purpose only *)
    commit_id: string option;
  }
  [@@deriving sexp, compare]
end

module Fetched = struct
  type t = {
    path: PyrePath.t;
    changed_files: PyrePath.t list;
  }
  [@@deriving sexp, compare]
end

module Setting = struct
  type t = {
    watchman_root: PyrePath.t;
    watchman_filter: Watchman.Filter.t;
    watchman_connection: Watchman.Raw.Connection.t;
    project_name: string;
    project_metadata: string option;
    critical_files: CriticalFile.t list;
    target: PyrePath.t;
  }
end

exception SavedStateQueryFailure of string

let query_exn
    {
      Setting.watchman_root;
      watchman_filter;
      watchman_connection;
      project_name;
      project_metadata;
      critical_files;
      target;
    }
  =
  let process_watchman_response { Watchman.SinceQuery.Response.relative_paths; saved_state } =
    match saved_state with
    | None ->
        raise (SavedStateQueryFailure "Watchman did not send back any saved-state information")
    | Some { Watchman.SinceQuery.Response.SavedState.bucket; path; commit_id } -> (
        let changed_files =
          List.map relative_paths ~f:(fun relative ->
              PyrePath.create_relative ~root:watchman_root ~relative)
        in
        match CriticalFile.find critical_files ~within:changed_files with
        | Some critical_file ->
            let message =
              Format.asprintf
                "Watchman detects changes in critical file `%a`"
                PyrePath.pp
                critical_file
            in
            raise (SavedStateQueryFailure message)
        | None -> { Queried.bucket; path; changed_files; target; commit_id })
  in
  Log.info "Querying watchman for a saved state";
  Watchman.SinceQuery.
    {
      root = watchman_root;
      filter = watchman_filter;
      since =
        Since.SourceControlAware
          {
            mergebase_with = "master";
            saved_state =
              Some { Since.SavedState.storage = "manifold"; project_name; project_metadata };
          };
    }
  |> Watchman.SinceQuery.query_exn ~connection:watchman_connection
  >>= fun response -> Lwt.return (process_watchman_response response)


let query setting =
  let on_exception exn =
    let message = Format.sprintf "Saved state query failed: %s" (Exn.to_string exn) in
    Lwt.return (Result.Error message)
  in
  Lwt.catch
    (fun () -> query_exn setting >>= fun queried -> Lwt.return (Result.Ok queried))
    on_exception


let fetch_exn { Queried.bucket; path; target; changed_files; commit_id } =
  let () =
    match commit_id with
    | None -> Log.info "Downloading saved state..."
    | Some commit_id -> Log.info "Downloading saved state at commit %s..." commit_id
  in
  FetchSavedState.fetch ~bucket ~path ~target ()
  >>= fun () -> Lwt.return { Fetched.path = target; changed_files }


let fetch queried =
  Lwt.catch
    (fun () -> fetch_exn queried >>= fun fetched -> Lwt.return (Result.Ok fetched))
    (fun exn ->
      let message = Format.sprintf "Saved state fetching failed: %s" (Exn.to_string exn) in
      Lwt.return (Result.Error message))


let query_and_fetch_exn setting = query_exn setting >>= fetch_exn

let query_and_fetch setting =
  Lwt.catch
    (fun () -> query_and_fetch_exn setting >>= fun fetched -> Lwt.return (Result.Ok fetched))
    (fun exn ->
      let message = Format.sprintf "Saved state query&fetching failed: %s" (Exn.to_string exn) in
      Lwt.return (Result.Error message))
