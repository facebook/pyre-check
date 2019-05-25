(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

(* The kind of a particular analysis result. Extensible by registering new analyses. *)
type 'a kind = ..

(* Since extensible variants are not properly serialized/deserialized, we can't store them in the
   heap. So we use a 1:1 mapping from 'a kind constants to 'a storable_kind, which are simply the
   variant tag. Outside this module 'a storable_kind is abstract. *)
type 'a storable_kind = int

let get_kind_name kind = Obj.extension_constructor kind |> Obj.extension_name

let assert_constant_constructor (type a) (kind : a kind) =
  if Obj.tag (Obj.repr kind) <> Obj.object_tag then
    get_kind_name kind |> Format.sprintf "Non-constant result kind not allowed: %s" |> failwith


let cast (type a) (kind : a kind) =
  try Obj.extension_constructor kind |> Obj.extension_id with
  | exn -> raise exn


type abstract = Abstract : 'a storable_kind -> abstract

let abstract k = Abstract k

type ('a, 'b) equality_witness =
  | Equal : ('a, 'a) equality_witness
  | Distinct : ('a, 'b) equality_witness

let are_equal (type a b) (a : a storable_kind) (b : b storable_kind) : (a, b) equality_witness =
  if a = b then
    Obj.magic Equal
  else
    Distinct


module Map = Caml.Map.Make (struct
  type t = abstract

  let compare = Pervasives.compare
end)

(* Maps analysis name to the analysis kind. *)
let analysis_map : (string * abstract) list ref = ref []

let register_option abstract_kind name = analysis_map := (name, abstract_kind) :: !analysis_map

let analysis_by_name analysis_name =
  let rec find = function
    | [] -> None
    | (name, kind) :: _ when name = analysis_name -> Some kind
    | _ :: rest -> find rest
  in
  find !analysis_map


(* Registers kinds and maps them to their name. *)
let kinds : string Map.t ref = ref Map.empty

let show kind =
  try Map.find kind !kinds with
  | _ -> failwith "unregistered kind"


let assert_unique_kind (type a) (kind : a kind) abstract_kind =
  if Map.mem abstract_kind !kinds then
    get_kind_name kind |> Format.sprintf "Duplicate registration of analysis %s" |> failwith


let register (type a) (kind : a kind) ~name =
  let () = assert_constant_constructor kind in
  let kind_num = cast kind in
  let abstract_kind = Abstract kind_num in
  let () = assert_unique_kind kind abstract_kind in
  let () = kinds := Map.add abstract_kind name !kinds in
  let () = register_option abstract_kind name in
  abstract_kind
