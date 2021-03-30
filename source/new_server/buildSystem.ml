(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Path = Pyre.Path

type t = {
  update: Path.t list -> Path.t list Lwt.t;
  cleanup: unit -> unit Lwt.t;
  lookup_source: Path.t -> Path.t option;
  lookup_artifact: Path.t -> Path.t list;
}

let update { update; _ } = update

let cleanup { cleanup; _ } = cleanup ()

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

let create_for_testing
    ?(update = fun _ -> Lwt.return [])
    ?(cleanup = fun () -> Lwt.return_unit)
    ?(lookup_source = fun path -> Some path)
    ?(lookup_artifact = fun path -> [path])
    ()
  =
  { update; cleanup; lookup_source; lookup_artifact }


module Initializer = struct
  type build_system = t

  type t = { initialize: unit -> build_system Lwt.t }

  let run { initialize } = initialize ()

  let null = { initialize = (fun () -> Lwt.return (create_for_testing ())) }

  let buck _ = { initialize = (fun () -> failwith "not implemented yet") }

  let create_for_testing ~initialize () = { initialize }
end
