(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

module IdentifiedCallee : sig
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
module ShimArgumentMapping : sig
  module Target : sig
    (* Type to represent the callee or argument of a shimmed call, in terms of the original call. *)
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
      | StaticMethod of {
          class_name: Reference.t;
          method_name: string;
        }
    [@@deriving equal, show { with_path = false }]

    val to_json : t -> Yojson.Safe.t
  end

  module Argument : sig
    type t = {
      name: Identifier.t option;
      value: Target.t;
    }
    [@@deriving equal, show { with_path = false }]

    val to_json : t -> Yojson.Safe.t
  end

  type t = {
    identifier: string;
    callee: Target.t;
    arguments: Argument.t list;
    discard_higher_order_parameters: bool;
  }
  [@@deriving equal, show { with_path = false }]

  val create_artificial_call : call_location:Location.t -> Call.t -> t -> (Call.t, string) result

  val to_json : t -> Yojson.Safe.t
end
