(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Kind = struct
  type t =
    | Watchman
    | BuckInternal
    | BuckUser
    | Pyre
    | Unknown
  [@@deriving sexp, compare, hash, to_yojson]
end

let kind_and_message_from_exception = function
  | Buck.Raw.BuckError { buck_command; arguments; description; exit_code; additional_logs } ->
      (* Buck exit code >=10 are considered internal: https://buck.build/command/exit_codes.html *)
      let kind =
        match exit_code with
        | Some exit_code when exit_code < 10 -> Kind.BuckUser
        | _ -> Kind.BuckInternal
      in
      let reproduce_message =
        if Buck.Raw.ArgumentList.length arguments <= 20 then
          [
            Format.sprintf
              "To reproduce this error, run `%s`."
              (Buck.Raw.ArgumentList.to_buck_command ~buck_command arguments);
          ]
        else
          []
      in
      let additional_messages =
        if List.is_empty additional_logs then
          []
        else
          "Here are the last few lines of Buck log:"
          :: "  ..." :: List.map additional_logs ~f:(String.( ^ ) " ")
      in
      ( kind,
        Format.sprintf
          "Cannot build the project: %s.\n%s"
          description
          (String.concat ~sep:"\n" (List.append reproduce_message additional_messages)) )
  | Buck.Interface.JsonError message ->
      ( Kind.Pyre,
        Format.sprintf "Cannot build the project because Buck returns malformed JSON: %s" message )
  | Buck.Builder.LinkTreeConstructionError message ->
      ( Kind.Pyre,
        Format.sprintf
          "Cannot build the project because Pyre encounters a fatal error while constructing a \
           link tree: %s"
          message )
  | ChecksumMap.LoadError message ->
      ( Kind.Pyre,
        Format.sprintf
          "Cannot build the project because Pyre encounters a fatal error while loading external \
           wheel: %s"
          message )
  | Watchman.ConnectionError message ->
      Kind.Watchman, Format.sprintf "Watchman connection error: %s" message
  | Watchman.SubscriptionError message ->
      Kind.Watchman, Format.sprintf "Watchman subscription error: %s" message
  | Watchman.QueryError message -> Kind.Watchman, Format.sprintf "Watchman query error: %s" message
  | Core_unix.Unix_error (Core_unix.EADDRINUSE, _, _) ->
      ( Kind.Pyre,
        "A Pyre server is already running for the current project. Use `pyre stop` to stop it \
         before starting another one." )
  | _ -> Kind.Unknown, Printexc.get_backtrace ()
