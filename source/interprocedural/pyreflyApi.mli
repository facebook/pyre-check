(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Module that implements the PyrePysaApi using the results from a pyrefly run with
   --report-pysa. *)

open Core

module FormatError : sig
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
  [@@deriving show]
end

module Error : sig
  type t =
    | InvalidJsonError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite : sig
  type t

  val create_from_directory
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    decorator_configuration:Analysis.DecoratorPreprocessing.Configuration.t ->
    PyrePath.t ->
    t
end

module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  val absolute_source_path_of_qualifier : t -> Ast.Reference.t -> string option

  (* Return all qualifiers with source code *)
  val explicit_qualifiers : t -> Ast.Reference.t list

  val source_of_qualifier : t -> Ast.Reference.t -> Ast.Source.t option
end

(* Exposed for testing purposes *)
module ModuleId : sig
  type t [@@deriving compare, equal, show]

  val from_int : int -> t
end

(* Exposed for testing purposes *)
module ModulePath : sig
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
  [@@deriving compare, equal, show]
end

(* Exposed for testing purposes *)
module ModuleInfoPath : sig
  type t [@@deriving compare, equal, show]

  val create : string -> t
end

(* Exposed for testing purposes *)
module ProjectFile : sig
  module Module : sig
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      module_path: ModulePath.t;
      info_path: ModuleInfoPath.t option;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, show]

  val create : path:string option -> Ast.Reference.t -> t

  val from_reference_unchecked : Ast.Reference.t -> t

  module Map : Map.S with type Key.t = t
end

(* Exposed for testing purposes *)
module Testing : sig
  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  val create_module_qualifiers
    :  ProjectFile.Module.t list ->
    ProjectFile.Module.t ModuleQualifier.Map.t
end
