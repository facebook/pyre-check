(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* JSON parsing functions for pyrefly report files. *)

module JsonUtil : sig
  val read_json_file_exn : PyrePath.t -> Yojson.Safe.t

  val get_string_member : Yojson.Safe.t -> string -> (string, PyreflyReport.FormatError.t) result

  val as_list : Yojson.Safe.t -> (Yojson.Safe.t list, PyreflyReport.FormatError.t) result

  val as_int : Yojson.Safe.t -> (int, PyreflyReport.FormatError.t) result

  val as_string : Yojson.Safe.t -> (string, PyreflyReport.FormatError.t) result

  val get_optional_string_member
    :  Yojson.Safe.t ->
    string ->
    (string option, PyreflyReport.FormatError.t) result

  val get_optional_bool_member
    :  default:bool ->
    Yojson.Safe.t ->
    string ->
    (bool, PyreflyReport.FormatError.t) result

  val get_optional_list_member
    :  Yojson.Safe.t ->
    string ->
    (Yojson.Safe.t list, PyreflyReport.FormatError.t) result

  val get_int_member : Yojson.Safe.t -> string -> (int, PyreflyReport.FormatError.t) result

  val get_optional_member : Yojson.Safe.t -> string -> Yojson.Safe.t option

  val check_format_version
    :  expected:int ->
    Yojson.Safe.t ->
    (unit, PyreflyReport.FormatError.t) result

  val get_member : Yojson.Safe.t -> string -> (Yojson.Safe.t, PyreflyReport.FormatError.t) result

  val get_object_member
    :  Yojson.Safe.t ->
    string ->
    ((string * Yojson.Safe.t) list, PyreflyReport.FormatError.t) result

  val get_optional_object_member
    :  Yojson.Safe.t ->
    string ->
    ((string * Yojson.Safe.t) list, PyreflyReport.FormatError.t) result

  val get_list_member
    :  Yojson.Safe.t ->
    string ->
    (Yojson.Safe.t list, PyreflyReport.FormatError.t) result

  val check_object
    :  Yojson.Safe.t ->
    ((string * Yojson.Safe.t) list, PyreflyReport.FormatError.t) result
end

module ModulePath : sig
  val from_json : Yojson.Safe.t -> (PyreflyReport.ModulePath.t, PyreflyReport.FormatError.t) result
end

module GlobalClassId : sig
  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.GlobalClassId.t, PyreflyReport.FormatError.t) result

  val from_optional_json
    :  Yojson.Safe.t option ->
    (PyreflyReport.GlobalClassId.t option, PyreflyReport.FormatError.t) result
end

module GlobalCallableId : sig
  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.GlobalCallableId.t, PyreflyReport.FormatError.t) result

  val from_optional_json
    :  Yojson.Safe.t option ->
    (PyreflyReport.GlobalCallableId.t option, PyreflyReport.FormatError.t) result
end

module PyreflyTarget : sig
  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.PyreflyTarget.t, PyreflyReport.FormatError.t) result
end

val parse_scalar_type_properties
  :  Yojson.Safe.t ->
  (Analysis.PyrePysaEnvironment.ScalarTypeProperties.t, PyreflyReport.FormatError.t) result

module ProjectFile : sig
  module Module : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ProjectFile.Module.t, PyreflyReport.FormatError.t) result
  end

  val from_json : Yojson.Safe.t -> (PyreflyReport.ProjectFile.t, PyreflyReport.FormatError.t) result

  val from_path_exn : PyrePath.t -> PyreflyReport.ProjectFile.t
end

module PyreflyType : sig
  val from_json : Yojson.Safe.t -> (PyreflyReport.PyreflyType.t, PyreflyReport.FormatError.t) result
end

module CapturedVariable : sig
  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.CapturedVariable.t, PyreflyReport.FormatError.t) result
end

module ModuleDefinitionsFile : sig
  module ParentScope : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.ParentScope.t, PyreflyReport.FormatError.t) result
  end

  module FunctionParameter : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.FunctionParameter.t, PyreflyReport.FormatError.t) result
  end

  module FunctionParameters : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.FunctionParameters.t, PyreflyReport.FormatError.t) result
  end

  module FunctionSignature : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.FunctionSignature.t, PyreflyReport.FormatError.t) result
  end

  val parse_decorator_callees
    :  (string * Yojson.Safe.t) list ->
    ( PyreflyReport.GlobalCallableId.t list Ast.Location.SerializableMap.t,
      PyreflyReport.FormatError.t )
    result

  module FunctionDefinition : sig
    val from_json
      :  local_function_id:PyreflyReport.LocalFunctionId.t ->
      Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.FunctionDefinition.t, PyreflyReport.FormatError.t) result
  end

  module ClassMro : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.ClassMro.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyClassField : sig
    val from_json
      :  name:string ->
      Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.PyreflyClassField.t, PyreflyReport.FormatError.t) result
  end

  module ClassDefinition : sig
    val from_json
      :  name_location:Ast.Location.t ->
      Yojson.Safe.t ->
      (PyreflyReport.ModuleDefinitionsFile.ClassDefinition.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyGlobalVariable : sig
    val from_json
      :  name:string ->
      Yojson.Safe.t ->
      ( PyreflyReport.ModuleDefinitionsFile.PyreflyGlobalVariable.t,
        PyreflyReport.FormatError.t )
      result
  end

  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.ModuleDefinitionsFile.t, PyreflyReport.FormatError.t) result

  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleDefinitionsFile.t
end

module ModuleTypeOfExpressions : sig
  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.ModuleTypeOfExpressions.t, PyreflyReport.FormatError.t) result

  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleTypeOfExpressions.t
end

module ModuleCallGraphs : sig
  module PyreflyImplicitReceiver : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyCallTarget : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyCallTarget.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyUnresolved : sig
    val from_json : Yojson.Safe.t -> (CallGraph.Unresolved.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyHigherOrderParameter : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyHigherOrderParameter.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyHigherOrderParameterMap : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyHigherOrderParameterMap.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyGlobalVariable : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyGlobalVariable.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyCallCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyCallCallees.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyAttributeAccessCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyAttributeAccessCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyIdentifierCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyIdentifierCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyDefineCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyDefineCallees.t, PyreflyReport.FormatError.t) result
  end

  module PyreflyFormatStringArtificialCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyFormatStringArtificialCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyFormatStringStringifyCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyFormatStringStringifyCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyReturnShimCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyReturnShimCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyExpressionCallees : sig
    val from_json
      :  Yojson.Safe.t ->
      ( PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.t,
        PyreflyReport.FormatError.t )
      result
  end

  module PyreflyCallGraph : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.ModuleCallGraphs.PyreflyCallGraph.t, PyreflyReport.FormatError.t) result
  end

  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.ModuleCallGraphs.t, PyreflyReport.FormatError.t) result

  val from_path_exn
    :  pyrefly_directory:PyrePath.t ->
    PyreflyReport.ModuleInfoFilename.t ->
    PyreflyReport.ModuleCallGraphs.t
end

module TypeErrors : sig
  module PyreflyError : sig
    val from_json
      :  Yojson.Safe.t ->
      (PyreflyReport.TypeErrors.PyreflyError.t, PyreflyReport.FormatError.t) result
  end

  val from_json
    :  Yojson.Safe.t ->
    (PyreflyReport.TypeErrors.PyreflyError.t list, PyreflyReport.FormatError.t) result

  val from_path_exn : pyrefly_directory:PyrePath.t -> PyreflyReport.TypeErrors.t
end
