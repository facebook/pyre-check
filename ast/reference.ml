(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Conv

open Expression

type t = Identifier.t list
[@@deriving compare, eq, sexp, hash]


module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


module SerializableMap = SerializableMap.Make(struct
    type nonrec t = t
    let compare = compare
  end)


module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


include Hashable.Make(struct
    type nonrec t = t
    let compare = compare
    let hash = hash
    let hash_fold_t = hash_fold_t
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let pp format reference =
  reference
  |> String.concat ~sep:"."
  |> Format.fprintf format "%s"


let show reference =
  Format.asprintf "%a" pp reference


let empty =
  []


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
  | Some prefix ->
      prefix @ name


let create_from_list names =
  names


let as_list reference =
  reference


let combine prefix suffix =
  match prefix with
  | [""] -> suffix
  | _ -> prefix @ suffix


let from_access access =
  Access.show access |> create


let access =
  List.map ~f:(fun identifier -> Access.Identifier identifier)


let from_name name =
  let rec get_reversed_identifiers = function
    | Name.Identifier identifier ->
        [identifier]
    | Name.Attribute { base = { Node.value = Name base; _ }; attribute } ->
        attribute :: (get_reversed_identifiers base)
    | _ ->
        failwith "Cannot convert expression with non-identifiers to reference."
  in
  get_reversed_identifiers name
  |> List.rev
  |> create_from_list


let name reference =
  let rec create = function
    | [] ->
        failwith "Reference cannot be empty."
    | identifier :: [] ->
        Name (Name.Identifier identifier)
        |> Node.create_with_default_location
    | identifier :: rest ->
        Name (Name.Attribute {
          base = create rest;
          attribute = identifier;
        })
        |> Node.create_with_default_location
  in
  match create (List.rev reference) with
  | { Node.value = Name name; _ } -> name
  | _ -> failwith "Impossible."


let expression ?(location = Location.Reference.any) reference =
  access reference
  |> Access.expression ~location


let new_expression ?(location = Location.Reference.any) reference =
  name reference
  |> (fun name -> Name name)
  |> Node.create ~location


let delocalize reference =
  match reference with
  | head :: tail when String.is_prefix ~prefix:"$local_" head ->
      let qualifier =
        let local_qualifier_pattern = Str.regexp "^\\$local_\\([a-zA-Z_0-9\\?]+\\)\\$" in
        if Str.string_match local_qualifier_pattern head 0 then
          Str.matched_group 1 head
          |> String.substr_replace_all ~pattern:"?" ~with_:"."
          |> create
        else
          begin
            Log.debug "Unable to extract qualifier from %s" head;
            []
          end
      in
      qualifier @ [Identifier.sanitized head] @ tail
  | _ ->
      reference


let sanitized reference =
  List.map ~f:Identifier.sanitized reference


let sanitize_qualified reference =
  List.rev reference
  |> (function
      | head :: tail -> sanitized [head] @ tail
      | reference -> reference)
  |> List.rev


let equal_sanitized left right =
  equal (sanitized left) (sanitized right)


let pp_sanitized format reference =
  sanitized reference
  |> String.concat ~sep:"."
  |> Format.fprintf format "%s"


let show_sanitized reference =
  Format.asprintf "%a" pp_sanitized reference


let single = function
  | [single] -> Some single
  | _ -> None


let length =
  List.length


let reverse =
  List.rev


let is_empty =
  List.is_empty


let rec is_prefix ~prefix reference =
  match prefix, reference with
  | [], _ -> true
  | prefix_head :: prefix, _ when prefix_head = "" ->
      is_prefix ~prefix reference
  | prefix_head :: prefix, head :: reference when prefix_head = head ->
      is_prefix ~prefix reference
  | _ ->
      false


let is_suffix ~suffix reference =
  is_prefix ~prefix:(List.rev suffix) (List.rev reference)


let rec is_strict_prefix ~prefix reference =
  match prefix, reference with
  | [], _ :: _ ->
      true
  | prefix_head :: prefix, _ when prefix_head = "" ->
      is_strict_prefix ~prefix reference
  | prefix_head :: prefix, head :: reference when prefix_head = head ->
      is_strict_prefix ~prefix reference
  | _ ->
      false


let drop_prefix ~prefix reference =
  let rec strip stripped reference prefix =
    match prefix, reference with
    | _, [_] when not stripped ->
        None
    | prefix_head :: prefix_tail, head :: tail when prefix_head = head ->
        strip true tail prefix_tail
    | [], reference ->
        Some reference
    | _ ->
        None
  in
  strip false reference prefix
  |> Option.value ~default:reference


let prefix reference =
  match List.rev reference with
  | [] ->
      None
  | _ :: prefix_reversed ->
      Some (List.rev prefix_reversed)


let last = function
  | [] -> ""
  | reference -> List.last_exn reference
