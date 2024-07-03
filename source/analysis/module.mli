(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module UnannotatedGlobal : sig
  type define_signature = {
    signature: Ast.Statement.Define.Signature.t;
    location: Ast.Location.WithModule.t;
  }
  [@@deriving sexp, equal, compare]

  type import =
    | ImportModule of {
        target: Ast.Reference.t;
        implicit_alias: bool;
      }
    | ImportFrom of {
        from: Ast.Reference.t;
        target: Ast.Identifier.t;
        implicit_alias: bool;
      }
  [@@deriving sexp, equal, compare]

  type t =
    | SimpleAssign of {
        explicit_annotation: Ast.Expression.t option;
        value: Ast.Expression.t option;
        target_location: Ast.Location.WithModule.t;
      }
    | TupleAssign of {
        value: Ast.Expression.t option;
        target_location: Ast.Location.WithModule.t;
        index: int;
        total_length: int;
      }
    | Imported of import
    | Define of define_signature list
    | Class
  [@@deriving sexp, equal, compare]

  (* Only exposed in the API for testing purposes *)
  val raw_alist_of_source : Ast.Source.t -> (Ast.Identifier.t * t) list
end

module Export : sig
  module Name : sig
    type t =
      | Class
      | Define of { is_getattr_any: bool }
      | GlobalVariable
    [@@deriving sexp, equal, compare, hash, show]
  end

  type t =
    | NameAlias of {
        from: Ast.Reference.t;
        name: Ast.Identifier.t;
      }
    | Module of Ast.Reference.t
    | Name of Name.t
  [@@deriving sexp, equal, compare, hash]
end

module Metadata : sig
  type t [@@deriving sexp, show, equal, compare]

  val empty_stub : t -> bool

  val create : Ast.Source.t -> t

  val create_implicit : ?empty_stub:bool -> unit -> t

  val get_export : t -> Ast.Identifier.t -> Export.t option

  val get_all_exports : t -> (Ast.Identifier.t * Export.t) list

  val is_implicit : t -> bool

  (* Exposed for testing only *)
  val create_for_testing : stub:bool -> t
end

module Components : sig
  type t = {
    module_metadata: Metadata.t;
    class_summaries: ClassSummary.t Ast.Node.t Ast.Identifier.Map.Tree.t;
    unannotated_globals: UnannotatedGlobal.t Ast.Identifier.Map.Tree.t;
    function_definitions: FunctionDefinition.t Ast.Reference.Map.Tree.t;
  }
  [@@deriving equal, sexp]

  val of_source : Ast.Source.t -> t

  val implicit_module : unit -> t
end

val wildcard_exports_of_raw_source : Ast.Source.t -> Ast.Identifier.t list
