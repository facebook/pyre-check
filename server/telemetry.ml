(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Message = struct
  type message =
    | Base of {
        root: string;
        hash: string;
      }
    | Update of {
        path: string;
        content: string;
      }

  type t = {
    uuid: string;
    message: message;
  }

  let to_normals { uuid; message } =
    match message with
    | Base { root; hash } -> ["kind", "base"; "uuid", uuid; "root", root; "hash", hash]
    | Update { path; content } ->
        ["kind", "update"; "uuid", uuid; "path", path; "content", content]
end

let budget = ref 0

let reset_budget ?(value = 6) () = budget := value

let send_telemetry ~f () =
  match !budget with
  | x when x <= 0 -> ()
  | current_budget -> (
    try
      f () |> Message.to_normals |> Statistics.server_telemetry;
      budget := current_budget - 1
    with
    | _ -> () )
