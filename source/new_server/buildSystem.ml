(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Path = Pyre.Path

type t = {
  initialize: unit -> unit Lwt.t;
  update: Path.t list -> Path.t list Lwt.t;
  cleanup: unit -> unit Lwt.t;
  lookup_source: Path.t -> Path.t option;
  lookup_artifact: Path.t -> Path.t list;
}

let initialize { initialize; _ } = initialize ()

let update { update; _ } = update

let cleanup { cleanup; _ } = cleanup ()

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

let create_for_testing
    ?(initialize = fun () -> Lwt.return_unit)
    ?(update = fun _ -> Lwt.return [])
    ?(cleanup = fun () -> Lwt.return_unit)
    ?(lookup_source = fun path -> Some path)
    ?(lookup_artifact = fun path -> [path])
    ()
  =
  { initialize; update; cleanup; lookup_source; lookup_artifact }


let null = create_for_testing ()

let buck _ = failwith "not implemented yet"
