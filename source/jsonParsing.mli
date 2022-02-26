(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** A small helper module to convert {!Yojson.Safe.t} into corresponding OCaml data types. *)

val to_bool_with_default : ?default:bool -> Yojson.Safe.t -> bool
(** [to_bool_with_default ?default json] tries to convert [json] into a boolean value.

    - If [json] is indeed a boolean, return the corresponding boolean.
    - If [json] is [null] and [default] is specified, return [default].
    - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)

val to_int_with_default : ?default:int -> Yojson.Safe.t -> int
(** [to_int_with_default ?default json] tries to convert [json] into a integer value.

    - If [json] is indeed an int, return the corresponding int.
    - If [json] is [null] and [default] is specified, return [default].
    - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)

val to_string_with_default : ?default:string -> Yojson.Safe.t -> string
(** [to_string_with_default ?default json] tries to convert [json] into a string value.

    - If [json] is indeed an string, return the corresponding string.
    - If [json] is [null] and [default] is specified, return [default].
    - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)

val to_path : Yojson.Safe.t -> PyrePath.t
(** [to_path json] tries to convert [json] into a {!PyrePath.t} if [json] is a string value.
    Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)

val bool_member : ?default:bool -> string -> Yojson.Safe.t -> bool
(** [bool_member ?default name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
      boolean and return it.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val int_member : ?default:int -> string -> Yojson.Safe.t -> int
(** [int_member ?default name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
      int and return it.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val string_member : ?default:string -> string -> Yojson.Safe.t -> string
(** [string_member ?default name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented as string, return it.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val path_member : string -> Yojson.Safe.t -> PyrePath.t
(** [path_member ?default name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
      {!PyrePath.t} and return it.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val list_member : ?default:'a list -> f:(Yojson.Safe.t -> 'a) -> string -> Yojson.Safe.t -> 'a list
(** [list_member ?default ~f name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary, [name] is presented, and the corresponding value is a list,
      then convert each element of the list using `f` and return the result list.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val string_list_member : ?default:string list -> string -> Yojson.Safe.t -> string list
(** [string_list_member ?default ~f name json] tries to look up key [name] in [json] as a
    dictionary.

    - If [json] is indeed a dictionary, [name] is presented, convert the corresponding value to a
      list of string.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val path_list_member : ?default:PyrePath.t list -> string -> Yojson.Safe.t -> PyrePath.t list
(** [path_list_member ?default ~f name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary, [name] is presented, convert the corresponding value to a
      list of {!PyrePath.t}.
    - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
      specified.
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val optional_string_member : string -> Yojson.Safe.t -> string option
(** [optional_string_member name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to a
      string and return it.
    - If [json] is indeed a directory but [name] is not presented, return [None].
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val optional_int_member : string -> Yojson.Safe.t -> int option
(** [optional_int_member name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to a
      int and return it.
    - If [json] is indeed a directory but [name] is not presented, return [None].
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)

val optional_path_member : string -> Yojson.Safe.t -> PyrePath.t option
(** [optional_path_member name json] tries to look up key [name] in [json] as a dictionary.

    - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to a
      {!PyrePath.t} and return it.
    - If [json] is indeed a directory but [name] is not presented, return [None].
    - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
