(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Sexplib.Conv

type t = Identifier.t list [@@deriving compare, eq, sexp, hash, to_yojson]

let create ?prefix name =
  let name =
    if String.equal name "" then
      []
    else if String.equal name "..." then
      [name]
    else
      String.split ~on:'.' name
  in
  match prefix with
  | None
  | Some [""] ->
      name
  | Some prefix -> prefix @ name


let pp format reference = reference |> String.concat ~sep:"." |> Format.fprintf format "%s"

let show reference = Format.asprintf "%a" pp reference

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module SerializableMap = SerializableMap.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

include Hashable.Make (struct
  type nonrec t = t

  let compare = compare

  let hash = hash

  let hash_fold_t = hash_fold_t

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module Key = struct
  type nonrec t = t

  let to_string = show

  let compare = compare

  type out = t

  let from_string name = create name
end

let empty = []

let create_from_list names = names

let as_list reference = reference

let combine prefix suffix =
  match prefix with
  | [""] -> suffix
  | _ -> prefix @ suffix


let delocalize reference =
  match reference with
  | head :: tail when String.is_prefix ~prefix:"$local_" head ->
      let qualifier =
        let local_qualifier_pattern = Str.regexp "^\\$local_\\([a-zA-Z_0-9\\?]+\\)\\$" in
        if Str.string_match local_qualifier_pattern head 0 then
          Str.matched_group 1 head |> String.substr_replace_all ~pattern:"?" ~with_:"." |> create
        else (
          Log.debug "Unable to extract qualifier from %s" head;
          [] )
      in
      qualifier @ [Identifier.sanitized head] @ tail
  | _ -> reference


let is_local reference =
  match reference with
  | head :: _ when String.is_prefix ~prefix:"$local_" head -> true
  | _ -> false


let sanitized reference = List.map ~f:Identifier.sanitized reference

let sanitize_qualified reference =
  List.rev reference
  |> (function
       | head :: tail -> sanitized [head] @ tail
       | reference -> reference)
  |> List.rev


let equal_sanitized left right = equal (sanitized left) (sanitized right)

let pp_sanitized format reference =
  sanitized reference |> String.concat ~sep:"." |> Format.fprintf format "%s"


let show_sanitized reference = Format.asprintf "%a" pp_sanitized reference

let single = function
  | [single] -> Some single
  | _ -> None


let length = List.length

let reverse = List.rev

let is_empty = List.is_empty

let rec is_prefix ~prefix reference =
  match prefix, reference with
  | [], _ -> true
  | "" :: prefix, _ -> is_prefix ~prefix reference
  | prefix_head :: prefix, head :: reference when String.equal prefix_head head ->
      is_prefix ~prefix reference
  | _ -> false


let is_suffix ~suffix reference = is_prefix ~prefix:(List.rev suffix) (List.rev reference)

let rec is_strict_prefix ~prefix reference =
  match prefix, reference with
  | [], _ :: _ -> true
  | "" :: prefix, _ -> is_strict_prefix ~prefix reference
  | prefix_head :: prefix, head :: reference when String.equal prefix_head head ->
      is_strict_prefix ~prefix reference
  | _ -> false


let drop_prefix ~prefix reference =
  let rec strip stripped reference prefix =
    match prefix, reference with
    | _, [_] when not stripped -> None
    | prefix_head :: prefix_tail, head :: tail when prefix_head = head ->
        strip true tail prefix_tail
    | [], reference -> Some reference
    | _ -> None
  in
  strip false reference prefix |> Option.value ~default:reference


let prefix reference =
  match List.rev reference with
  | [] -> None
  | _ :: prefix_reversed -> Some (List.rev prefix_reversed)


let last = function
  | [] -> ""
  | reference -> List.last_exn reference
