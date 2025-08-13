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

module CallableMetadata : sig
  type t = {
    module_qualifier: Ast.Reference.t;
    name_location: Ast.Location.t;
    is_overload: bool;
    is_staticmethod: bool;
    is_classmethod: bool;
    is_property_getter: bool;
    is_property_setter: bool;
    is_toplevel: bool;
    is_class_toplevel: bool;
    is_stub: bool; (* Is this a stub definition, e.g `def foo(): ...` *)
    parent_is_class: bool;
  }
  [@@deriving show]
end

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

(* Read-only API that can be sent to workers. Cheap to copy. *)
module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  (* Return all qualifiers with source code *)
  val explicit_qualifiers : t -> Ast.Reference.t list

  val artifact_path_of_qualifier : t -> Ast.Reference.t -> ArtifactPath.t option

  val absolute_source_path_of_qualifier : t -> Ast.Reference.t -> string option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val get_define_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val class_immediate_parents : t -> string -> string list

  val get_callable_metadata : t -> Ast.Reference.t -> CallableMetadata.t

  (* Is this a stub module, i.e a `.pyi` file. *)
  val is_stub_qualifier : t -> Ast.Reference.t -> bool

  val get_define_opt : t -> Ast.Reference.t -> Ast.Statement.Define.t Ast.Node.t option
end

(* Exposed for testing purposes *)
module ModuleId : sig
  type t [@@deriving compare, equal, show]

  val from_int : int -> t
end

(* Exposed for testing purposes *)
module LocalClassId : sig
  type t [@@deriving compare, equal, show]

  val from_int : int -> t
end

(* Exposed for testing purposes *)
module GlobalClassId : sig
  type t [@@deriving compare, equal, show]
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
      is_test: bool;
      is_interface: bool;
      is_init: bool;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ModuleInfoFile : sig
  module ParentScope : sig
    type t =
      | TopLevel
      | Class of Ast.Location.t
      | Function of Ast.Location.t
    [@@deriving equal, show]
  end

  module FunctionDefinition : sig
    type t = {
      name: string;
      parent: ParentScope.t;
      is_overload: bool;
      is_staticmethod: bool;
      is_classmethod: bool;
      is_property_getter: bool;
      is_property_setter: bool;
      is_stub: bool;
      is_toplevel: bool;
      is_class_toplevel: bool;
    }
    [@@deriving equal, show]
  end

  module ClassDefinition : sig
    type t = {
      name: string;
      parent: ParentScope.t;
      local_class_id: LocalClassId.t;
      bases: GlobalClassId.t list;
      is_synthesized: bool;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, show]

  val create : path:string option -> Ast.Reference.t -> t

  val from_reference_unchecked : Ast.Reference.t -> t

  val to_reference : t -> Ast.Reference.t

  module Map : Map.S with type Key.t = t
end

(* Exposed for testing purposes *)
module FullyQualifiedName : sig
  type t [@@deriving compare, equal, show]

  val to_reference : t -> Ast.Reference.t
end

(* Exposed for testing purposes *)
module Testing : sig
  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  val create_module_qualifiers
    :  ProjectFile.Module.t list ->
    ProjectFile.Module.t ModuleQualifier.Map.t

  module Definition : sig
    type t =
      | Function of ModuleInfoFile.FunctionDefinition.t
      | Class of ModuleInfoFile.ClassDefinition.t
    [@@deriving equal, show]
  end

  module QualifiedDefinition : sig
    type t = {
      qualified_name: FullyQualifiedName.t;
      local_name: Ast.Reference.t; (* a non-unique name, more user-friendly. *)
      definition: Definition.t; (* class or def *)
      name_location: Ast.Location.t;
    }
  end

  val create_fully_qualified_names
    :  module_qualifier:ModuleQualifier.t ->
    module_exists:(ModuleQualifier.t -> bool) ->
    class_definitions:ModuleInfoFile.ClassDefinition.t Ast.Location.Map.t ->
    function_definitions:ModuleInfoFile.FunctionDefinition.t Ast.Location.Map.t ->
    QualifiedDefinition.t list
end
