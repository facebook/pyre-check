(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

let ( !+ ) descriptor = Unix.File_descr.of_int descriptor

let connections persistent_clients =
  let persistent_clients =
    List.map persistent_clients ~f:(fun (descriptor, failures) -> !+descriptor, failures)
    |> Network.Socket.Map.of_alist_exn
  in
  { Server.State.lock = Mutex.create ();
    connections =
      ref
        { Server.State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
          json_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
          persistent_clients;
          file_notifiers = []
        }
  }


module TrackedWrites = struct
  (* Type inference fails us for top-level polymorphic types :( *)
  let writes : Server.Protocol.response list Unix.File_descr.Table.t =
    Unix.File_descr.Table.create ()


  let closed = Unix.File_descr.Hash_set.create ()

  (* Used to simulate socket failures. *)
  let failing = Unix.File_descr.Hash_set.create ()

  let write socket response =
    if Hash_set.mem failing socket then
      raise (Unix.Unix_error (Unix.EPIPE, "", ""));
    Hashtbl.add_multi writes ~key:socket ~data:response


  let close socket = Hash_set.add closed socket

  let fail_writes_for socket = Hash_set.add failing socket

  let clear () =
    Hashtbl.clear writes;
    Hash_set.clear closed;
    Hash_set.clear failing
end

let test_broadcast_response _ =
  let module Connections = Server.Connections.Make (TrackedWrites) in
  let response content = Server.Protocol.LanguageServerProtocolResponse content in
  Connections.broadcast_response ~connections:(connections []) ~response:(response "1");

  (* Clients need to be present before writes can happen. *)
  TrackedWrites.clear ();
  assert_equal 0 (Hashtbl.length TrackedWrites.writes);

  (* Simple case - broadcasting to a single socket. *)
  TrackedWrites.clear ();
  Connections.broadcast_response ~connections:(connections [42, 0]) ~response:(response "1");
  assert_equal [response "1"] (Hashtbl.find_exn TrackedWrites.writes !+42);

  (* Broadcasting to two descriptors. *)
  TrackedWrites.clear ();
  Connections.broadcast_response ~connections:(connections [42, 0; 43, 0]) ~response:(response "1");
  assert_equal [response "1"] (Hashtbl.find_exn TrackedWrites.writes !+42);
  assert_equal [response "1"] (Hashtbl.find_exn TrackedWrites.writes !+43);

  (* Fail writes for one of the sockets. *)
  TrackedWrites.clear ();
  TrackedWrites.fail_writes_for !+42;
  Connections.broadcast_response ~connections:(connections [42, 4; 43, 0]) ~response:(response "1");
  assert_equal false (Hashtbl.mem TrackedWrites.writes !+42);
  assert_equal [response "1"] (Hashtbl.find_exn TrackedWrites.writes !+43);
  assert_equal true (Hash_set.mem TrackedWrites.closed !+42)


let test_write_to_persistent_client _ =
  let module Connections = Server.Connections.Make (TrackedWrites) in
  let response content = Server.Protocol.LanguageServerProtocolResponse content in
  (* Successful case - write to a known persistent client. *)
  TrackedWrites.clear ();
  Connections.write_to_persistent_client
    ~connections:(connections [42, 0])
    ~socket:!+42
    ~response:(response "1");
  assert_equal [response "1"] (Hashtbl.find_exn TrackedWrites.writes !+42);

  (* Multiple writes. *)
  TrackedWrites.clear ();
  Connections.write_to_persistent_client
    ~connections:(connections [42, 0])
    ~socket:!+42
    ~response:(response "1");
  Connections.write_to_persistent_client
    ~connections:(connections [42, 0])
    ~socket:!+42
    ~response:(response "2");
  assert_equal [response "2"; response "1"] (Hashtbl.find_exn TrackedWrites.writes !+42);

  (* The socket must be tracked in order to write. *)
  TrackedWrites.clear ();
  Connections.write_to_persistent_client
    ~connections:(connections [])
    ~socket:!+42
    ~response:(response "1");
  assert_equal false (Hashtbl.mem TrackedWrites.writes !+42)


let () =
  "connections"
  >::: [ "broadcast_response" >:: test_broadcast_response;
         "write_to_persistent_client" >:: test_write_to_persistent_client ]
  |> Test.run
