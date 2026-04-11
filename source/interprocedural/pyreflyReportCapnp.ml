(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Cap'n Proto parsing functions for pyrefly report files. *)

open Core
open Pyre
open Ast
module ScalarTypeProperties = Analysis.PyrePysaEnvironment.ScalarTypeProperties
module PyreflyTypeRep = Analysis.PyrePysaEnvironment.PyreflyType
module CapnpGenerated = PyreflyReportCapnpGenerated.Make (Capnp.BytesMessage)
module CapnpReader = CapnpGenerated.Reader

let read_message_from_file_exn path =
  let path_string = PyrePath.absolute path in
  try
    let contents = In_channel.read_all path_string in
    let stream = Capnp.Codecs.FramedStream.of_string ~compression:`None contents in
    match Capnp.Codecs.FramedStream.get_next_frame stream with
    | Result.Ok message -> message
    | Result.Error _ ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.InvalidCapnpError "Failed to decode capnp frame" })
  with
  | Sys_error message ->
      raise
        (PyreflyReport.PyreflyFileFormatError { path; error = PyreflyReport.Error.IOError message })


let read_optional_field reader has_fn get_fn parse_fn =
  if has_fn reader then Some (parse_fn (get_fn reader)) else None


let read_optional_string reader has_fn get_fn = read_optional_field reader has_fn get_fn Fun.id

let read_location reader =
  let line = CapnpReader.PysaLocation.line_get_int_exn reader in
  let col = CapnpReader.PysaLocation.col_get_int_exn reader in
  let end_line = CapnpReader.PysaLocation.end_line_get_int_exn reader in
  let end_col = CapnpReader.PysaLocation.end_col_get_int_exn reader in
  PyreflyReport.fixup_location
    {
      Location.start = { Location.line; column = col };
      stop = { Location.line = end_line; column = end_col };
    }


let read_source_path reader =
  match CapnpReader.SourcePath.get reader with
  | CapnpReader.SourcePath.FileSystem path ->
      PyreflyReport.ModulePath.Filesystem (path |> PyrePath.create_absolute |> ArtifactPath.create)
  | CapnpReader.SourcePath.Namespace path ->
      PyreflyReport.ModulePath.Namespace (PyrePath.create_absolute path)
  | CapnpReader.SourcePath.Memory path ->
      PyreflyReport.ModulePath.Memory (PyrePath.create_absolute path)
  | CapnpReader.SourcePath.BundledTypeshed path ->
      PyreflyReport.ModulePath.BundledTypeshed (PyrePath.create_absolute path)
  | CapnpReader.SourcePath.BundledTypeshedThirdParty path ->
      PyreflyReport.ModulePath.BundledTypeshedThirdParty (PyrePath.create_absolute path)
  | CapnpReader.SourcePath.BundledThirdParty path ->
      PyreflyReport.ModulePath.BundledTypeshed (PyrePath.create_absolute path)
  | CapnpReader.SourcePath.Undefined _ -> failwith "Unknown SourcePath variant in capnp"


let read_class_ref reader =
  let module_id =
    PyreflyReport.ModuleId.from_int (CapnpReader.ClassRef.module_id_get_int_exn reader)
  in
  let local_class_id =
    PyreflyReport.LocalClassId.from_int (CapnpReader.ClassRef.class_id_get_int_exn reader)
  in
  { PyreflyReport.GlobalClassId.module_id; local_class_id }


let read_optional_class_ref reader has_fn get_fn =
  read_optional_field reader has_fn get_fn read_class_ref


let read_function_ref reader =
  let module_id =
    PyreflyReport.ModuleId.from_int (CapnpReader.FunctionRef.module_id_get_int_exn reader)
  in
  let local_function_id =
    match
      PyreflyReport.LocalFunctionId.from_string (CapnpReader.FunctionRef.function_id_get reader)
    with
    | Ok id -> id
    | Error error ->
        Format.asprintf "Invalid function id: %a" PyreflyReport.FormatError.pp error |> failwith
  in
  { PyreflyReport.GlobalCallableId.module_id; local_function_id }


let read_optional_function_ref reader has_fn get_fn =
  read_optional_field reader has_fn get_fn read_function_ref


let read_scalar_type_properties reader =
  let is_boolean = CapnpReader.ScalarTypeProperties.is_bool_get reader in
  let is_integer = CapnpReader.ScalarTypeProperties.is_int_get reader in
  let is_float = CapnpReader.ScalarTypeProperties.is_float_get reader in
  let is_enumeration = CapnpReader.ScalarTypeProperties.is_enum_get reader in
  ScalarTypeProperties.create
    ~is_boolean
    ~is_integer
      (* TODO(T225700656): pyre1 considers integers to be valid floats. We preserve that behavior
         for now. *)
    ~is_float:(is_float || is_integer)
    ~is_enumeration


let read_type_modifier = function
  | CapnpReader.TypeModifier.Optional -> Some Analysis.PyrePysaEnvironment.TypeModifier.Optional
  | CapnpReader.TypeModifier.Coroutine -> Some Analysis.PyrePysaEnvironment.TypeModifier.Coroutine
  | CapnpReader.TypeModifier.Awaitable -> Some Analysis.PyrePysaEnvironment.TypeModifier.Awaitable
  | CapnpReader.TypeModifier.TypeVariableBound ->
      Some Analysis.PyrePysaEnvironment.TypeModifier.TypeVariableBound
  | CapnpReader.TypeModifier.TypeVariableConstraint ->
      Some Analysis.PyrePysaEnvironment.TypeModifier.TypeVariableConstraint
  | CapnpReader.TypeModifier.Type -> Some Analysis.PyrePysaEnvironment.TypeModifier.Type
  | CapnpReader.TypeModifier.Undefined _ -> None


let read_class_with_modifiers reader =
  let class_ref = CapnpReader.ClassWithModifiers.class_get reader |> read_class_ref in
  let modifiers =
    CapnpReader.ClassWithModifiers.modifiers_get_list reader
    |> List.filter_map ~f:read_type_modifier
  in
  {
    PyreflyTypeRep.ClassWithModifiers.module_id = PyreflyReport.ModuleId.to_int class_ref.module_id;
    class_id = PyreflyReport.LocalClassId.to_int class_ref.local_class_id;
    modifiers;
  }


let read_class_names_from_type reader =
  let classes =
    CapnpReader.ClassNamesFromType.classes_get_list reader |> List.map ~f:read_class_with_modifiers
  in
  let is_exhaustive = CapnpReader.ClassNamesFromType.is_exhaustive_get reader in
  { PyreflyTypeRep.ClassNamesFromType.classes; is_exhaustive }


let read_pysa_type reader =
  let string = CapnpReader.PysaType.string_get reader in
  let scalar_properties =
    if CapnpReader.PysaType.has_scalar_type_properties reader then
      read_scalar_type_properties (CapnpReader.PysaType.scalar_type_properties_get reader)
    else
      ScalarTypeProperties.none
  in
  let class_names =
    read_optional_field
      reader
      CapnpReader.PysaType.has_class_names
      CapnpReader.PysaType.class_names_get
      read_class_names_from_type
  in
  { PyreflyTypeRep.string; scalar_properties; class_names }


let read_scope_parent reader =
  match CapnpReader.ScopeParent.get reader with
  | CapnpReader.ScopeParent.TopLevel -> PyreflyReport.ModuleDefinitionsFile.ParentScope.TopLevel
  | CapnpReader.ScopeParent.Function func_def_index ->
      PyreflyReport.ModuleDefinitionsFile.ParentScope.Function
        (PyreflyReport.FuncDefIndex.from_int (Stdint.Uint32.to_int func_def_index))
  | CapnpReader.ScopeParent.Class class_id ->
      PyreflyReport.ModuleDefinitionsFile.ParentScope.Class
        (PyreflyReport.LocalClassId.from_int (Stdint.Uint32.to_int class_id))
  | CapnpReader.ScopeParent.Undefined _ -> failwith "Unknown ScopeParent variant in capnp"


let read_captured_variable reader =
  let name = CapnpReader.CapturedVariableRef.name_get reader in
  let outer_function =
    read_function_ref (CapnpReader.CapturedVariableRef.outer_function_get reader)
  in
  { PyreflyReport.CapturedVariable.name; outer_function }


let read_target reader =
  match CapnpReader.Target.get reader with
  | CapnpReader.Target.Function function_ref ->
      PyreflyReport.PyreflyTarget.Function (read_function_ref function_ref)
  | CapnpReader.Target.Overrides function_ref ->
      PyreflyReport.PyreflyTarget.Overrides (read_function_ref function_ref)
  | CapnpReader.Target.FormatString -> PyreflyReport.PyreflyTarget.FormatString
  | CapnpReader.Target.Undefined _ -> failwith "Unknown Target variant in capnp"


let read_implicit_receiver = function
  | CapnpReader.ImplicitReceiver.TrueWithClassReceiver ->
      PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.TrueWithClassReceiver
  | CapnpReader.ImplicitReceiver.TrueWithObjectReceiver ->
      PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.TrueWithObjectReceiver
  | CapnpReader.ImplicitReceiver.False ->
      PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.False
  | CapnpReader.ImplicitReceiver.Undefined _ -> failwith "Unknown ImplicitReceiver variant in capnp"


let read_unresolved_reason = function
  | CapnpReader.UnresolvedReason.LambdaArgument -> CallGraph.Unresolved.LambdaArgument
  | CapnpReader.UnresolvedReason.UnexpectedPyreflyTarget ->
      CallGraph.Unresolved.UnexpectedPyreflyTarget
  | CapnpReader.UnresolvedReason.EmptyPyreflyCallTarget ->
      CallGraph.Unresolved.EmptyPyreflyCallTarget
  | CapnpReader.UnresolvedReason.UnknownClassField -> CallGraph.Unresolved.UnknownClassField
  | CapnpReader.UnresolvedReason.ClassFieldOnlyExistInObject ->
      CallGraph.Unresolved.ClassFieldOnlyExistInObject
  | CapnpReader.UnresolvedReason.UnsupportedFunctionTarget ->
      CallGraph.Unresolved.UnsupportedFunctionTarget
  | CapnpReader.UnresolvedReason.UnexpectedDefiningClass ->
      CallGraph.Unresolved.UnexpectedDefiningClass
  | CapnpReader.UnresolvedReason.UnexpectedInitMethod -> CallGraph.Unresolved.UnexpectedInitMethod
  | CapnpReader.UnresolvedReason.UnexpectedNewMethod -> CallGraph.Unresolved.UnexpectedNewMethod
  | CapnpReader.UnresolvedReason.UnexpectedCalleeExpression ->
      CallGraph.Unresolved.UnexpectedCalleeExpression
  | CapnpReader.UnresolvedReason.UnresolvedMagicDunderAttr ->
      CallGraph.Unresolved.UnresolvedMagicDunderAttr
  | CapnpReader.UnresolvedReason.UnresolvedMagicDunderAttrDueToNoBase ->
      CallGraph.Unresolved.UnresolvedMagicDunderAttrDueToNoBase
  | CapnpReader.UnresolvedReason.UnresolvedMagicDunderAttrDueToNoAttribute ->
      CallGraph.Unresolved.UnresolvedMagicDunderAttrDueToNoAttribute
  | CapnpReader.UnresolvedReason.Mixed -> CallGraph.Unresolved.Mixed
  | CapnpReader.UnresolvedReason.Undefined _ -> failwith "Unknown UnresolvedReason variant in capnp"


let read_unresolved reader =
  match CapnpReader.Unresolved.get reader with
  | CapnpReader.Unresolved.False -> CallGraph.Unresolved.False
  | CapnpReader.Unresolved.True reason -> CallGraph.Unresolved.True (read_unresolved_reason reason)
  | CapnpReader.Unresolved.Undefined _ -> failwith "Unknown Unresolved variant in capnp"


let read_call_target reader =
  let target = read_target (CapnpReader.PysaCallTarget.target_get reader) in
  let implicit_receiver =
    read_implicit_receiver (CapnpReader.PysaCallTarget.implicit_receiver_get reader)
  in
  let implicit_dunder_call = CapnpReader.PysaCallTarget.implicit_dunder_call_get reader in
  let receiver_class =
    read_optional_class_ref
      reader
      CapnpReader.PysaCallTarget.has_receiver_class
      CapnpReader.PysaCallTarget.receiver_class_get
  in
  let is_class_method = CapnpReader.PysaCallTarget.is_class_method_get reader in
  let is_static_method = CapnpReader.PysaCallTarget.is_static_method_get reader in
  let return_type =
    if CapnpReader.PysaCallTarget.has_return_type reader then
      read_scalar_type_properties (CapnpReader.PysaCallTarget.return_type_get reader)
    else
      ScalarTypeProperties.none
  in
  {
    PyreflyReport.ModuleCallGraphs.PyreflyCallTarget.target;
    implicit_receiver;
    implicit_dunder_call;
    receiver_class;
    is_class_method;
    is_static_method;
    return_type;
  }


let read_decorator_callees reader_list =
  List.map reader_list ~f:(fun reader ->
      let location = read_location (CapnpReader.DecoratorCallee.location_get reader) in
      let targets =
        CapnpReader.DecoratorCallee.targets_get_list reader
        |> List.map ~f:(fun target_reader ->
               match read_target target_reader with
               | PyreflyReport.PyreflyTarget.Function global_callable_id
               | PyreflyReport.PyreflyTarget.Overrides global_callable_id ->
                   global_callable_id
               | target ->
                   Format.asprintf
                     "Unexpected type of decorator callee: `%a`"
                     PyreflyReport.PyreflyTarget.pp
                     target
                   |> failwith)
      in
      location, targets)
  |> Location.SerializableMap.of_alist_exn


let read_function_parameter reader =
  match CapnpReader.FunctionParameter.get reader with
  | CapnpReader.FunctionParameter.PosOnly param ->
      let name =
        read_optional_string
          param
          CapnpReader.FunctionParameter.PosOnlyParam.has_name
          CapnpReader.FunctionParameter.PosOnlyParam.name_get
      in
      let annotation =
        read_pysa_type (CapnpReader.FunctionParameter.PosOnlyParam.annotation_get param)
      in
      let required = CapnpReader.FunctionParameter.PosOnlyParam.required_get param in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameter.PosOnly { name; annotation; required }
  | CapnpReader.FunctionParameter.Pos param ->
      let name = CapnpReader.FunctionParameter.PosParam.name_get param in
      let annotation =
        read_pysa_type (CapnpReader.FunctionParameter.PosParam.annotation_get param)
      in
      let required = CapnpReader.FunctionParameter.PosParam.required_get param in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameter.Pos { name; annotation; required }
  | CapnpReader.FunctionParameter.VarArg param ->
      let name =
        read_optional_string
          param
          CapnpReader.FunctionParameter.VarArgParam.has_name
          CapnpReader.FunctionParameter.VarArgParam.name_get
      in
      let annotation =
        read_pysa_type (CapnpReader.FunctionParameter.VarArgParam.annotation_get param)
      in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameter.VarArg { name; annotation }
  | CapnpReader.FunctionParameter.KwOnly param ->
      let name = CapnpReader.FunctionParameter.KwOnlyParam.name_get param in
      let annotation =
        read_pysa_type (CapnpReader.FunctionParameter.KwOnlyParam.annotation_get param)
      in
      let required = CapnpReader.FunctionParameter.KwOnlyParam.required_get param in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameter.KwOnly { name; annotation; required }
  | CapnpReader.FunctionParameter.Kwargs param ->
      let name =
        read_optional_string
          param
          CapnpReader.FunctionParameter.KwargsParam.has_name
          CapnpReader.FunctionParameter.KwargsParam.name_get
      in
      let annotation =
        read_pysa_type (CapnpReader.FunctionParameter.KwargsParam.annotation_get param)
      in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameter.Kwargs { name; annotation }
  | CapnpReader.FunctionParameter.Undefined _ ->
      failwith "Unknown FunctionParameter variant in capnp"


let read_function_parameters reader =
  match CapnpReader.FunctionParameters.get reader with
  | CapnpReader.FunctionParameters.List params ->
      let parameters = Capnp.Array.to_list params |> List.map ~f:read_function_parameter in
      PyreflyReport.ModuleDefinitionsFile.FunctionParameters.List parameters
  | CapnpReader.FunctionParameters.Ellipsis ->
      PyreflyReport.ModuleDefinitionsFile.FunctionParameters.Ellipsis
  | CapnpReader.FunctionParameters.ParamSpec ->
      PyreflyReport.ModuleDefinitionsFile.FunctionParameters.ParamSpec
  | CapnpReader.FunctionParameters.Undefined _ ->
      failwith "Unknown FunctionParameters variant in capnp"


let read_function_signature reader =
  let parameters = read_function_parameters (CapnpReader.FunctionSignature.parameters_get reader) in
  let return_annotation =
    read_pysa_type (CapnpReader.FunctionSignature.return_annotation_get reader)
  in
  { PyreflyReport.ModuleDefinitionsFile.FunctionSignature.parameters; return_annotation }


let read_class_field_declaration_kind = function
  | CapnpReader.PysaClassFieldDeclaration.None -> None
  | CapnpReader.PysaClassFieldDeclaration.DeclaredByAnnotation ->
      Some PyreflyReport.ClassFieldDeclarationKind.DeclaredByAnnotation
  | CapnpReader.PysaClassFieldDeclaration.DeclaredWithoutAnnotation ->
      Some PyreflyReport.ClassFieldDeclarationKind.DeclaredWithoutAnnotation
  | CapnpReader.PysaClassFieldDeclaration.AssignedInBody ->
      Some PyreflyReport.ClassFieldDeclarationKind.AssignedInBody
  | CapnpReader.PysaClassFieldDeclaration.DefinedWithoutAssign ->
      Some PyreflyReport.ClassFieldDeclarationKind.DefinedWithoutAssign
  | CapnpReader.PysaClassFieldDeclaration.DefinedInMethod ->
      Some PyreflyReport.ClassFieldDeclarationKind.DefinedInMethod
  | CapnpReader.PysaClassFieldDeclaration.Undefined _ -> None


let read_class_field reader =
  let name = CapnpReader.PysaClassField.name_get reader in
  let type_ = read_pysa_type (CapnpReader.PysaClassField.type_get reader) in
  let explicit_annotation =
    read_optional_string
      reader
      CapnpReader.PysaClassField.has_explicit_annotation
      CapnpReader.PysaClassField.explicit_annotation_get
  in
  let location =
    read_optional_field
      reader
      CapnpReader.PysaClassField.has_location
      CapnpReader.PysaClassField.location_get
      read_location
  in
  let declaration_kind =
    read_class_field_declaration_kind (CapnpReader.PysaClassField.declaration_kind_get reader)
  in
  {
    PyreflyReport.ModuleDefinitionsFile.PyreflyClassField.name;
    type_;
    explicit_annotation;
    location;
    declaration_kind;
  }


let read_class_mro reader =
  match CapnpReader.PysaClassMro.get reader with
  | CapnpReader.PysaClassMro.Resolved classes ->
      let classes = Capnp.Array.to_list classes |> List.map ~f:read_class_ref in
      PyreflyReport.ModuleDefinitionsFile.ClassMro.Resolved classes
  | CapnpReader.PysaClassMro.Cyclic -> PyreflyReport.ModuleDefinitionsFile.ClassMro.Cyclic
  | CapnpReader.PysaClassMro.Undefined _ -> failwith "Unknown ClassMro variant in capnp"


let read_class_definition reader =
  let name_location = read_location (CapnpReader.ClassDefinition.name_location_get reader) in
  let class_id = CapnpReader.ClassDefinition.class_id_get_int_exn reader in
  let local_class_id = PyreflyReport.LocalClassId.from_int class_id in
  let name = CapnpReader.ClassDefinition.name_get reader in
  let parent = read_scope_parent (CapnpReader.ClassDefinition.parent_get reader) in
  let bases = CapnpReader.ClassDefinition.bases_get_list reader |> List.map ~f:read_class_ref in
  let mro = read_class_mro (CapnpReader.ClassDefinition.mro_get reader) in
  let is_synthesized = CapnpReader.ClassDefinition.is_synthesized_get reader in
  let is_dataclass = CapnpReader.ClassDefinition.is_dataclass_get reader in
  let is_named_tuple = CapnpReader.ClassDefinition.is_named_tuple_get reader in
  let is_typed_dict = CapnpReader.ClassDefinition.is_typed_dict_get reader in
  let fields = CapnpReader.ClassDefinition.fields_get_list reader |> List.map ~f:read_class_field in
  let decorator_callees =
    read_decorator_callees (CapnpReader.ClassDefinition.decorator_callees_get_list reader)
  in
  ( local_class_id,
    {
      PyreflyReport.ModuleDefinitionsFile.ClassDefinition.name;
      local_class_id;
      name_location;
      parent;
      bases;
      mro;
      is_synthesized;
      is_dataclass;
      is_named_tuple;
      is_typed_dict;
      fields;
      decorator_callees;
    } )


let read_function_definition reader =
  let function_id_string = CapnpReader.FunctionDefinition.function_id_get reader in
  let local_function_id =
    match PyreflyReport.LocalFunctionId.from_string function_id_string with
    | Ok id -> id
    | Error error ->
        Format.asprintf "Invalid function id: %a" PyreflyReport.FormatError.pp error |> failwith
  in
  let name = CapnpReader.FunctionDefinition.name_get reader in
  let parent = read_scope_parent (CapnpReader.FunctionDefinition.parent_get reader) in
  let undecorated_signatures =
    CapnpReader.FunctionDefinition.undecorated_signatures_get_list reader
    |> List.map ~f:read_function_signature
  in
  let captured_variables =
    CapnpReader.FunctionDefinition.captured_variables_get_list reader
    |> List.map ~f:read_captured_variable
  in
  let is_overload = CapnpReader.FunctionDefinition.is_overload_get reader in
  let is_staticmethod = CapnpReader.FunctionDefinition.is_staticmethod_get reader in
  let is_classmethod = CapnpReader.FunctionDefinition.is_classmethod_get reader in
  let is_property_getter = CapnpReader.FunctionDefinition.is_property_getter_get reader in
  let is_property_setter = CapnpReader.FunctionDefinition.is_property_setter_get reader in
  let is_stub = CapnpReader.FunctionDefinition.is_stub_get reader in
  let is_def_statement = CapnpReader.FunctionDefinition.is_def_statement_get reader in
  let overridden_base_method =
    read_optional_function_ref
      reader
      CapnpReader.FunctionDefinition.has_overridden_base_method
      CapnpReader.FunctionDefinition.overridden_base_method_get
  in
  let defining_class =
    read_optional_class_ref
      reader
      CapnpReader.FunctionDefinition.has_defining_class
      CapnpReader.FunctionDefinition.defining_class_get
  in
  let name_location =
    read_optional_field
      reader
      CapnpReader.FunctionDefinition.has_define_name_location
      CapnpReader.FunctionDefinition.define_name_location_get
      read_location
  in
  let decorator_callees =
    read_decorator_callees (CapnpReader.FunctionDefinition.decorator_callees_get_list reader)
  in
  ( local_function_id,
    {
      PyreflyReport.ModuleDefinitionsFile.FunctionDefinition.name;
      name_location;
      local_function_id;
      parent;
      undecorated_signatures;
      captured_variables;
      is_overload;
      is_staticmethod;
      is_classmethod;
      is_property_getter;
      is_property_setter;
      is_stub;
      is_def_statement;
      is_toplevel = false;
      is_class_toplevel = false;
      overridden_base_method;
      defining_class;
      decorator_callees;
    } )


let read_global_variable reader =
  let name = CapnpReader.GlobalVariable.name_get reader in
  let type_ =
    if CapnpReader.GlobalVariable.has_type reader then
      Some (read_pysa_type (CapnpReader.GlobalVariable.type_get reader))
    else
      None
  in
  let location = read_location (CapnpReader.GlobalVariable.location_get reader) in
  { PyreflyReport.ModuleDefinitionsFile.PyreflyGlobalVariable.name; type_; location }


(* Call graph readers *)

let read_higher_order_parameter reader =
  let index = CapnpReader.HigherOrderParameter.index_get_int_exn reader in
  let call_targets =
    CapnpReader.HigherOrderParameter.call_targets_get_list reader |> List.map ~f:read_call_target
  in
  let unresolved =
    if CapnpReader.HigherOrderParameter.has_unresolved reader then
      read_unresolved (CapnpReader.HigherOrderParameter.unresolved_get reader)
    else
      CallGraph.Unresolved.False
  in
  { PyreflyReport.ModuleCallGraphs.PyreflyHigherOrderParameter.index; call_targets; unresolved }


let read_call_callees reader =
  let call_targets =
    CapnpReader.CallCallees.call_targets_get_list reader |> List.map ~f:read_call_target
  in
  let init_targets =
    CapnpReader.CallCallees.init_targets_get_list reader |> List.map ~f:read_call_target
  in
  let new_targets =
    CapnpReader.CallCallees.new_targets_get_list reader |> List.map ~f:read_call_target
  in
  let higher_order_parameters =
    CapnpReader.CallCallees.higher_order_parameters_get_list reader
    |> List.map ~f:read_higher_order_parameter
  in
  let unresolved =
    if CapnpReader.CallCallees.has_unresolved reader then
      read_unresolved (CapnpReader.CallCallees.unresolved_get reader)
    else
      CallGraph.Unresolved.False
  in
  {
    PyreflyReport.ModuleCallGraphs.PyreflyCallCallees.call_targets;
    init_targets;
    new_targets;
    higher_order_parameters;
    unresolved;
  }


let read_call_graph_global_variable reader =
  let module_id =
    PyreflyReport.ModuleId.from_int (CapnpReader.GlobalVariableRef.module_id_get_int_exn reader)
  in
  let name = CapnpReader.GlobalVariableRef.name_get reader in
  { PyreflyReport.ModuleCallGraphs.PyreflyGlobalVariable.module_id; name }


let read_attribute_access_callees reader =
  let if_called = read_call_callees (CapnpReader.AttributeAccessCallees.if_called_get reader) in
  let property_setters =
    CapnpReader.AttributeAccessCallees.property_setters_get_list reader
    |> List.map ~f:read_call_target
  in
  let property_getters =
    CapnpReader.AttributeAccessCallees.property_getters_get_list reader
    |> List.map ~f:read_call_target
  in
  let global_targets =
    CapnpReader.AttributeAccessCallees.global_targets_get_list reader
    |> List.map ~f:read_call_graph_global_variable
  in
  let is_attribute = CapnpReader.AttributeAccessCallees.is_attribute_get reader in
  {
    PyreflyReport.ModuleCallGraphs.PyreflyAttributeAccessCallees.if_called;
    property_setters;
    property_getters;
    global_targets;
    is_attribute;
  }


let read_identifier_callees reader =
  let if_called = read_call_callees (CapnpReader.IdentifierCallees.if_called_get reader) in
  let global_targets =
    CapnpReader.IdentifierCallees.global_targets_get_list reader
    |> List.map ~f:read_call_graph_global_variable
  in
  let captured_variables =
    CapnpReader.IdentifierCallees.captured_variables_get_list reader
    |> List.map ~f:read_captured_variable
  in
  {
    PyreflyReport.ModuleCallGraphs.PyreflyIdentifierCallees.if_called;
    global_targets;
    captured_variables;
  }


let read_define_callees reader =
  let define_targets =
    CapnpReader.DefineCallees.define_targets_get_list reader |> List.map ~f:read_call_target
  in
  { PyreflyReport.ModuleCallGraphs.PyreflyDefineCallees.define_targets }


let read_format_string_artificial_callees reader =
  let targets =
    CapnpReader.FormatStringArtificialCallees.targets_get_list reader
    |> List.map ~f:read_call_target
  in
  { PyreflyReport.ModuleCallGraphs.PyreflyFormatStringArtificialCallees.targets }


let read_format_string_stringify_callees reader =
  let targets =
    CapnpReader.FormatStringStringifyCallees.targets_get_list reader |> List.map ~f:read_call_target
  in
  let unresolved =
    if CapnpReader.FormatStringStringifyCallees.has_unresolved reader then
      read_unresolved (CapnpReader.FormatStringStringifyCallees.unresolved_get reader)
    else
      CallGraph.Unresolved.False
  in
  { PyreflyReport.ModuleCallGraphs.PyreflyFormatStringStringifyCallees.targets; unresolved }


let read_return_shim_argument_mapping = function
  | CapnpReader.ReturnShimArgumentMapping.ReturnExpression ->
      CallGraph.ReturnShimCallees.ReturnExpression
  | CapnpReader.ReturnShimArgumentMapping.ReturnExpressionElement ->
      CallGraph.ReturnShimCallees.ReturnExpressionElement
  | CapnpReader.ReturnShimArgumentMapping.Undefined _ ->
      failwith "Unknown ReturnShimArgumentMapping variant in capnp"


let read_return_shim_callees reader =
  let targets =
    CapnpReader.ReturnShimCallees.targets_get_list reader |> List.map ~f:read_call_target
  in
  let arguments =
    CapnpReader.ReturnShimCallees.arguments_get_list reader
    |> List.map ~f:read_return_shim_argument_mapping
  in
  { PyreflyReport.ModuleCallGraphs.PyreflyReturnShimCallees.targets; arguments }


let read_expression_callees reader =
  match CapnpReader.ExpressionCallees.get reader with
  | CapnpReader.ExpressionCallees.Call call_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Call (read_call_callees call_callees)
  | CapnpReader.ExpressionCallees.Identifier identifier_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Identifier
        (read_identifier_callees identifier_callees)
  | CapnpReader.ExpressionCallees.AttributeAccess attribute_access_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.AttributeAccess
        (read_attribute_access_callees attribute_access_callees)
  | CapnpReader.ExpressionCallees.Define define_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Define
        (read_define_callees define_callees)
  | CapnpReader.ExpressionCallees.FormatStringArtificial format_string_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.FormatStringArtificial
        (read_format_string_artificial_callees format_string_callees)
  | CapnpReader.ExpressionCallees.FormatStringStringify format_string_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.FormatStringStringify
        (read_format_string_stringify_callees format_string_callees)
  | CapnpReader.ExpressionCallees.Return return_callees ->
      PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Return
        (read_return_shim_callees return_callees)
  | CapnpReader.ExpressionCallees.Undefined _ ->
      failwith "Unknown ExpressionCallees variant in capnp"


let read_call_graph_entry reader =
  let expression_id_string = CapnpReader.CallGraphEntry.expression_id_get reader in
  let expression_identifier =
    match ExpressionIdentifier.from_json_key expression_id_string with
    | Ok id -> ExpressionIdentifier.map_location ~f:PyreflyReport.fixup_location id
    | Error error -> failwith error
  in
  let callees = read_expression_callees (CapnpReader.CallGraphEntry.callees_get reader) in
  { PyreflyReport.ModuleCallGraphs.CallGraphEdge.expression_identifier; callees }


let read_function_call_graph reader =
  let function_id_string = CapnpReader.FunctionCallGraph.function_id_get reader in
  let function_id =
    match PyreflyReport.LocalFunctionId.from_string function_id_string with
    | Ok id -> id
    | Error error ->
        Format.asprintf "Invalid function id: %a" PyreflyReport.FormatError.pp error |> failwith
  in
  let call_graph =
    CapnpReader.FunctionCallGraph.entries_get_list reader |> List.map ~f:read_call_graph_entry
  in
  function_id, call_graph


(* Top-level file readers *)

module ProjectFile = struct
  let read_module reader =
    let module_id =
      PyreflyReport.ModuleId.from_int (CapnpReader.PysaProjectModule.module_id_get_int_exn reader)
    in
    let module_name = Reference.create (CapnpReader.PysaProjectModule.module_name_get reader) in
    let absolute_source_path =
      read_source_path (CapnpReader.PysaProjectModule.source_path_get reader)
    in
    let relative_source_path =
      read_optional_string
        reader
        CapnpReader.PysaProjectModule.has_relative_source_path
        CapnpReader.PysaProjectModule.relative_source_path_get
    in
    let info_filename =
      read_optional_string
        reader
        CapnpReader.PysaProjectModule.has_info_filename
        CapnpReader.PysaProjectModule.info_filename_get
      |> Option.map ~f:PyreflyReport.ModuleInfoFilename.create
    in
    let python_version_string = CapnpReader.PysaProjectModule.python_version_get reader in
    let python_version =
      match Configuration.PythonVersion.from_string python_version_string with
      | Ok version -> version
      | Error error -> failwith error
    in
    let platform = CapnpReader.PysaProjectModule.platform_get reader in
    let is_test = CapnpReader.PysaProjectModule.is_test_get reader in
    let is_interface = CapnpReader.PysaProjectModule.is_interface_get reader in
    let is_init = CapnpReader.PysaProjectModule.is_init_get reader in
    let is_internal = CapnpReader.PysaProjectModule.is_internal_get reader in
    {
      PyreflyReport.ProjectFile.Module.module_id;
      module_name;
      absolute_source_path;
      relative_source_path;
      info_filename;
      python_version;
      platform;
      is_test;
      is_interface;
      is_init;
      is_internal;
    }


  let from_path_exn path =
    let () = Log.debug "Parsing pyrefly project file `%a` (capnp)" PyrePath.pp path in
    let message = read_message_from_file_exn path in
    let reader = CapnpReader.ProjectFile.of_message message in
    let modules = CapnpReader.ProjectFile.modules_get_list reader |> List.map ~f:read_module in
    let builtin_module_ids =
      CapnpReader.ProjectFile.builtin_module_ids_get_list reader
      |> List.map ~f:(fun id -> PyreflyReport.ModuleId.from_int (Stdint.Uint32.to_int id))
    in
    let object_class_refs =
      CapnpReader.ProjectFile.object_class_refs_get_list reader |> List.map ~f:read_class_ref
    in
    let dict_class_refs =
      CapnpReader.ProjectFile.dict_class_refs_get_list reader |> List.map ~f:read_class_ref
    in
    let typing_module_ids =
      CapnpReader.ProjectFile.typing_module_ids_get_list reader
      |> List.map ~f:(fun id -> PyreflyReport.ModuleId.from_int (Stdint.Uint32.to_int id))
    in
    let typing_mapping_class_refs =
      CapnpReader.ProjectFile.typing_mapping_class_refs_get_list reader
      |> List.map ~f:read_class_ref
    in
    {
      PyreflyReport.ProjectFile.modules;
      builtin_module_ids;
      object_class_refs;
      dict_class_refs;
      typing_module_ids;
      typing_mapping_class_refs;
    }
end

module ModuleDefinitionsFile = struct
  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"definitions"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module definitions file %a (capnp)" PyrePath.pp path in
    let message = read_message_from_file_exn path in
    let reader = CapnpReader.ModuleDefinitions.of_message message in
    let module_id =
      PyreflyReport.ModuleId.from_int (CapnpReader.ModuleDefinitions.module_id_get_int_exn reader)
    in
    let function_definitions =
      CapnpReader.ModuleDefinitions.function_definitions_get_list reader
      |> List.map ~f:read_function_definition
      |> PyreflyReport.LocalFunctionId.Map.of_alist_exn
    in
    let class_definitions =
      CapnpReader.ModuleDefinitions.class_definitions_get_list reader
      |> List.map ~f:read_class_definition
      |> PyreflyReport.LocalClassId.Map.of_alist_exn
    in
    let global_variables =
      CapnpReader.ModuleDefinitions.global_variables_get_list reader
      |> List.map ~f:read_global_variable
    in
    {
      PyreflyReport.ModuleDefinitionsFile.module_id;
      function_definitions;
      class_definitions;
      global_variables;
    }
end

module ModuleTypeOfExpressions = struct
  let read_function_type_of_expressions reader =
    let function_id_string = CapnpReader.FunctionTypeOfExpressions.function_id_get reader in
    let function_id =
      match PyreflyReport.LocalFunctionId.from_string function_id_string with
      | Ok id -> id
      | Error error ->
          Format.asprintf "Invalid function id: %a" PyreflyReport.FormatError.pp error |> failwith
    in
    let types =
      CapnpReader.FunctionTypeOfExpressions.types_get_list reader
      |> List.map ~f:read_pysa_type
      |> Array.of_list
    in
    let locations =
      CapnpReader.FunctionTypeOfExpressions.locations_get_list reader
      |> List.map ~f:(fun entry ->
             let location = read_location (CapnpReader.LocationTypeIdEntry.location_get entry) in
             let type_id = CapnpReader.LocationTypeIdEntry.type_id_get_int_exn entry in
             {
               PyreflyReport.ModuleTypeOfExpressions.TypeAtLocation.location;
               type_ = PyreflyReport.ModuleTypeOfExpressions.LocalTypeId.of_int type_id;
             })
    in
    {
      PyreflyReport.ModuleTypeOfExpressions.FunctionTypeOfExpressions.function_id;
      types;
      locations;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"type_of_expressions"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () =
      Log.debug "Parsing pyrefly module type-of-expressions file %a (capnp)" PyrePath.pp path
    in
    let message = read_message_from_file_exn path in
    let reader = CapnpReader.ModuleTypeOfExpressions.of_message message in
    let module_id =
      PyreflyReport.ModuleId.from_int
        (CapnpReader.ModuleTypeOfExpressions.module_id_get_int_exn reader)
    in
    let functions =
      CapnpReader.ModuleTypeOfExpressions.functions_get_list reader
      |> List.map ~f:read_function_type_of_expressions
    in
    { PyreflyReport.ModuleTypeOfExpressions.module_id; functions }
end

module ModuleCallGraphs = struct
  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"call_graphs"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module call-graphs file %a (capnp)" PyrePath.pp path in
    let message = read_message_from_file_exn path in
    let reader = CapnpReader.ModuleCallGraphs.of_message message in
    let module_id =
      PyreflyReport.ModuleId.from_int (CapnpReader.ModuleCallGraphs.module_id_get_int_exn reader)
    in
    let call_graphs =
      CapnpReader.ModuleCallGraphs.call_graphs_get_list reader
      |> List.map ~f:read_function_call_graph
      |> PyreflyReport.LocalFunctionId.Map.of_alist_exn
    in
    { PyreflyReport.ModuleCallGraphs.module_id; call_graphs }
end

module TypeErrors = struct
  let from_path_exn ~pyrefly_directory =
    let path = pyrefly_directory |> PyrePath.append ~element:"errors.capnp.bin" in
    let () = Log.debug "Parsing pyrefly type errors file %a (capnp)" PyrePath.pp path in
    let message = read_message_from_file_exn path in
    let reader = CapnpReader.TypeErrors.of_message message in
    let errors =
      CapnpReader.TypeErrors.errors_get_list reader
      |> List.map ~f:(fun error_reader ->
             let module_name = CapnpReader.TypeError.module_name_get error_reader in
             let module_path =
               read_source_path (CapnpReader.TypeError.module_path_get error_reader)
             in
             let location = read_location (CapnpReader.TypeError.location_get error_reader) in
             let kind = CapnpReader.TypeError.kind_get error_reader in
             let message = CapnpReader.TypeError.message_get error_reader in
             {
               PyreflyReport.TypeErrors.PyreflyError.module_name;
               module_path;
               location;
               kind;
               message;
             })
    in
    { PyreflyReport.TypeErrors.errors }
end
