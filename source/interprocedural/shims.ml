(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Shims: defines data structures to represent shimming from one function call
 * to another. This is usually used to properly handle functions with behavior
 * that are too complicated for the taint analysis.
 *)

open Core
open Ast
open Expression

module IdentifiedCallee = struct
  type t =
    | FunctoolsPartial
    | MultiprocessingProcess
    | PromoteQueue
    | ApiClient
    | WeatherDatatype of string
  [@@deriving show]
end

(* Represents how a specific call should be shimmed to another call, as a syntactic
   transformation. *)
module ShimArgumentMapping = struct
  (* Type to represent the callee or argument of a shimmed call, in terms of the original call. *)
  module Target = struct
    type t =
      | Callee (* original callee *)
      | Argument of { index: int } (* original argument at this index *)
      | GetAttributeBase of {
          attribute: string;
          inner: t;
        }
      (* If the target expression for `inner` is `foo.bar`, then the new target is `foo`. *)
      (* For instance, when shimming `f(x.a, b)` then `GetAttributeBase(Argument 0)` is `x` *)
      | AppendAttribute of {
          attribute: string;
          inner: t;
        }
      (* If the target expression for `inner` is `foo`, then the new target is `foo.attribute`. *)
      | GetTupleElement of {
          index: int;
          inner: t;
        }
      (* If the target expression for `inner` is `(a, b, c)`, then the new target is the element at
         the given index. *)
      | GetListElement of {
          index: int;
          inner: t;
        }
      (* If the target expression for `inner` is `[a, b, c]`, then the new target is the element at
         the given index. *)
      | GetDictEntryValue of {
          index: int;
          key: string;
          inner: t;
        }
      (* If the target expression for `inner` is `{a: b, c: d, ..}`, then the new target is the
         value at the given key. *)
      | GetCallArgument of {
          index: int;
          inner: t;
        }
      | Constant of Constant.t
      | Reference of Reference.t
    [@@deriving equal, show { with_path = false }]

    let rec to_json = function
      | Callee -> `Assoc ["kind", `String "callee"]
      | Argument { index } -> `Assoc ["kind", `String "argument"; "index", `Int index]
      | GetAttributeBase { attribute; inner } ->
          `Assoc
            [
              "kind", `String "get-attribute-base";
              "inner", to_json inner;
              "attribute", `String attribute;
            ]
      | AppendAttribute { attribute; inner } ->
          `Assoc
            [
              "kind", `String "append-attribute";
              "inner", to_json inner;
              "attribute", `String attribute;
            ]
      | GetTupleElement { index; inner } ->
          `Assoc ["kind", `String "get-tuple-element"; "inner", to_json inner; "index", `Int index]
      | GetListElement { index; inner } ->
          `Assoc ["kind", `String "get-list-element"; "inner", to_json inner; "index", `Int index]
      | GetDictEntryValue { index; key; inner } ->
          `Assoc
            [
              "kind", `String "get-dict-entry-value";
              "inner", to_json inner;
              "index", `Int index;
              "key", `String key;
            ]
      | GetCallArgument { index; inner } ->
          `Assoc ["kind", `String "get-call-argument"; "inner", to_json inner; "index", `Int index]
      | Constant constant ->
          `Assoc ["kind", `String "constant"; "value", `String (Constant.show constant)]
      | Reference reference ->
          `Assoc ["kind", `String "reference"; "value", `String (Reference.show reference)]
  end

  module Argument = struct
    type t = {
      name: Identifier.t option;
      value: Target.t;
    }
    [@@deriving equal, show { with_path = false }]

    let to_json { name; value } =
      let bindings =
        match name with
        | Some name -> ["name", `String name]
        | None -> []
      in
      `Assoc (("value", Target.to_json value) :: bindings)
  end

  type t = {
    identifier: string;
    callee: Target.t;
    arguments: Argument.t list;
  }
  [@@deriving equal, show { with_path = false }]

  let create_artificial_call
      ~call_location
      { Call.callee = call_callee; arguments = call_arguments; origin = call_origin }
      { identifier; callee = shim_callee; arguments = shim_arguments }
    =
    let open Core.Result in
    let rec from_target = function
      | Target.Callee -> Ok call_callee
      | Target.Argument { index } -> (
          match List.nth call_arguments index with
          | Some { Call.Argument.value; _ } -> Ok value
          | None -> Error (Format.asprintf "invalid argument index: %d" index))
      | Target.GetAttributeBase { inner; attribute = get_attribute } -> (
          from_target inner
          >>= function
          | {
              Node.value = Expression.Name (Name.Attribute { Name.Attribute.base; attribute; _ });
              _;
            }
            when String.equal attribute get_attribute ->
              Ok base
          | expression ->
              Error
                (Format.asprintf
                   "expected attribute access of %s, got `%a`"
                   get_attribute
                   Expression.pp
                   expression))
      | Target.AppendAttribute { attribute; inner } ->
          from_target inner
          >>= fun ({ Node.location; _ } as inner) ->
          let origin =
            Some (Origin.create ?base:(origin inner) ~location (Origin.PysaCallRedirect identifier))
          in
          Ok
            (Expression.Name (Name.Attribute { Name.Attribute.base = inner; attribute; origin })
            |> Node.create ~location)
      | Target.GetTupleElement { index; inner } -> (
          from_target inner
          >>= function
          | { Node.value = Expression.Tuple items; _ } -> (
              match List.nth items index with
              | Some element -> Ok element
              | None -> Error (Format.asprintf "invalid index in tuple: %d" index))
          | expression ->
              Error (Format.asprintf "expected tuple expression, got `%a`" Expression.pp expression)
          )
      | Target.GetListElement { index; inner } -> (
          from_target inner
          >>= function
          | { Node.value = Expression.List items; _ } -> (
              match List.nth items index with
              | Some element -> Ok element
              | None -> Error (Format.asprintf "invalid index in list: %d" index))
          | expression ->
              Error (Format.asprintf "expected list expression, got `%a`" Expression.pp expression))
      | Target.GetDictEntryValue { index; key = _; inner } -> (
          from_target inner
          >>= function
          | { Node.value = Expression.Dictionary entries; _ } -> (
              let open Dictionary.Entry in
              match List.nth entries index with
              | Some (KeyValue KeyValue.{ key = _; value }) -> Ok value
              | _ -> Error (Format.asprintf "invalid index in dict entries: %d" index))
          | expression ->
              Error (Format.asprintf "expected dict expression, got `%a`" Expression.pp expression))
      | Target.GetCallArgument { index; inner } -> (
          from_target inner
          >>= function
          | { Node.value = Expression.Call { Call.arguments; _ }; _ } -> (
              match List.nth arguments index with
              | Some { Call.Argument.value; _ } -> Ok value
              | None -> Error (Format.asprintf "invalid argument index: %d" index))
          | expression ->
              Error (Format.asprintf "expected call expression, got `%a`" Expression.pp expression))
      | Target.Constant constant ->
          Ok (Node.create_with_default_location (Expression.Constant constant))
      | Target.Reference reference ->
          Ok
            (Ast.Expression.from_reference
               ~location:call_location
               ~create_origin:(fun attributes ->
                 Some
                   (Origin.create
                      ?base:call_origin
                      ~location:call_location
                      (Origin.PysaCallRedirect
                         (Format.sprintf "%s:%s" identifier (String.concat ~sep:"." attributes)))))
               reference)
    in
    from_target shim_callee
    >>= fun callee ->
    List.map
      ~f:(fun { Argument.name; value } ->
        from_target value
        >>| fun value ->
        { Call.Argument.value; name = Option.map ~f:Node.create_with_default_location name })
      shim_arguments
    |> Result.all
    >>| fun arguments ->
    let origin =
      Some
        (Origin.create
           ?base:call_origin
           ~location:call_location
           (Origin.PysaCallRedirect identifier))
    in
    { Call.callee; arguments; origin }


  let to_json { identifier; callee; arguments } =
    `Assoc
      [
        "identifier", `String identifier;
        "callee", Target.to_json callee;
        "arguments", `List (List.map arguments ~f:Argument.to_json);
      ]
end
