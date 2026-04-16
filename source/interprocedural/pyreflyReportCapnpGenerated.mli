(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 * @generated
 * @codegen-command: cd ~/fbsource/fbcode/tools/pyre && python3 facebook/scripts/generate_pyrefly_report_capnp.py
 * @codegen-source: fbcode/pyrefly/pyrefly/lib/report/pysa/pysa_report.capnp
 * DO NOT EDIT. *)
[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t

  module ReturnShimArgumentMapping_16744047642608040181 : sig
    type t =
      | ReturnExpression
      | ReturnExpressionElement
      | Undefined of int
  end
  module UnresolvedReason_16256959910610618162 : sig
    type t =
      | LambdaArgument
      | UnexpectedPyreflyTarget
      | EmptyPyreflyCallTarget
      | UnknownClassField
      | ClassFieldOnlyExistInObject
      | UnsupportedFunctionTarget
      | UnexpectedDefiningClass
      | UnexpectedInitMethod
      | UnexpectedNewMethod
      | UnexpectedCalleeExpression
      | UnresolvedMagicDunderAttr
      | UnresolvedMagicDunderAttrDueToNoBase
      | UnresolvedMagicDunderAttrDueToNoAttribute
      | Mixed
      | Undefined of int
  end
  module PysaClassFieldDeclaration_17132881886155188463 : sig
    type t =
      | None
      | DeclaredByAnnotation
      | DeclaredWithoutAnnotation
      | AssignedInBody
      | DefinedWithoutAssign
      | DefinedInMethod
      | Undefined of int
  end
  module ImplicitReceiver_16434162114200234704 : sig
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False
      | Undefined of int
  end
  module TypeModifier_17523487963418047075 : sig
    type t =
      | Optional
      | Coroutine
      | Awaitable
      | TypeVariableBound
      | TypeVariableConstraint
      | Type
      | Undefined of int
  end

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module SourcePath : sig
      type struct_t = [`SourcePath_c9789635145f607d]
      type t = struct_t reader_t
      type unnamed_union_t =
        | FileSystem of string
        | Namespace of string
        | Memory of string
        | BundledTypeshed of string
        | BundledTypeshedThirdParty of string
        | BundledThirdParty of string
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PysaLocation : sig
      type struct_t = [`PysaLocation_c309c969f3e58a60]
      type t = struct_t reader_t
      val line_get : t -> Stdint.Uint32.t
      val line_get_int_exn : t -> int
      val col_get : t -> Stdint.Uint32.t
      val col_get_int_exn : t -> int
      val end_line_get : t -> Stdint.Uint32.t
      val end_line_get_int_exn : t -> int
      val end_col_get : t -> Stdint.Uint32.t
      val end_col_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ClassRef : sig
      type struct_t = [`ClassRef_de5e9ff6d0dd25c7]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val class_id_get : t -> Stdint.Uint32.t
      val class_id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionRef : sig
      type struct_t = [`FunctionRef_80115bb31006b8bb]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module GlobalVariableRef : sig
      type struct_t = [`GlobalVariableRef_f70734c08fbb8800]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_name : t -> bool
      val name_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module TypeModifier : sig
      type t = TypeModifier_17523487963418047075.t =
        | Optional
        | Coroutine
        | Awaitable
        | TypeVariableBound
        | TypeVariableConstraint
        | Type
        | Undefined of int
    end
    module ClassWithModifiers : sig
      type struct_t = [`ClassWithModifiers_e15f82ade37d4236]
      type t = struct_t reader_t
      val has_class : t -> bool
      val class_get : t -> ClassRef.t
      val class_get_pipelined : struct_t MessageWrapper.StructRef.t -> ClassRef.struct_t MessageWrapper.StructRef.t
      val has_modifiers : t -> bool
      val modifiers_get : t -> (ro, TypeModifier.t, array_t) Capnp.Array.t
      val modifiers_get_list : t -> TypeModifier.t list
      val modifiers_get_array : t -> TypeModifier.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ClassNamesFromType : sig
      type struct_t = [`ClassNamesFromType_a0584aac2e97e83c]
      type t = struct_t reader_t
      val has_classes : t -> bool
      val classes_get : t -> (ro, ClassWithModifiers.t, array_t) Capnp.Array.t
      val classes_get_list : t -> ClassWithModifiers.t list
      val classes_get_array : t -> ClassWithModifiers.t array
      val is_exhaustive_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ScalarTypeProperties : sig
      type struct_t = [`ScalarTypeProperties_d79ace8daba1babf]
      type t = struct_t reader_t
      val is_bool_get : t -> bool
      val is_int_get : t -> bool
      val is_float_get : t -> bool
      val is_enum_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PysaType : sig
      type struct_t = [`PysaType_d28d95e8ed282312]
      type t = struct_t reader_t
      val has_string : t -> bool
      val string_get : t -> string
      val has_scalar_type_properties : t -> bool
      val scalar_type_properties_get : t -> ScalarTypeProperties.t
      val scalar_type_properties_get_pipelined : struct_t MessageWrapper.StructRef.t -> ScalarTypeProperties.struct_t MessageWrapper.StructRef.t
      val has_class_names : t -> bool
      val class_names_get : t -> ClassNamesFromType.t
      val class_names_get_pipelined : struct_t MessageWrapper.StructRef.t -> ClassNamesFromType.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ScopeParent : sig
      type struct_t = [`ScopeParent_de2463e0e757468e]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Function of Stdint.Uint32.t
        | Class of Stdint.Uint32.t
        | TopLevel
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionParameter : sig
      type struct_t = [`FunctionParameter_a1b2fce98392735b]
      type t = struct_t reader_t
      module PosOnlyParam : sig
        type struct_t = [`PosOnlyParam_f44b7078e9979a56]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
        val required_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module PosParam : sig
        type struct_t = [`PosParam_cb05f6c652a9cccf]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
        val required_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module VarArgParam : sig
        type struct_t = [`VarArgParam_987ff4fc98295c57]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module KwOnlyParam : sig
        type struct_t = [`KwOnlyParam_9ceff6d824c01469]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
        val required_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module KwargsParam : sig
        type struct_t = [`KwargsParam_914731ff31eab123]
        type t = struct_t reader_t
        val has_name : t -> bool
        val name_get : t -> string
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      type unnamed_union_t =
        | PosOnly of [`PosOnlyParam_f44b7078e9979a56] reader_t
        | Pos of [`PosParam_cb05f6c652a9cccf] reader_t
        | VarArg of [`VarArgParam_987ff4fc98295c57] reader_t
        | KwOnly of [`KwOnlyParam_9ceff6d824c01469] reader_t
        | Kwargs of [`KwargsParam_914731ff31eab123] reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionParameters : sig
      type struct_t = [`FunctionParameters_a7c6a3610ea670c8]
      type t = struct_t reader_t
      type unnamed_union_t =
        | List of (ro, FunctionParameter.t, array_t) Capnp.Array.t
        | Ellipsis
        | ParamSpec
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionSignature : sig
      type struct_t = [`FunctionSignature_be11da038b7eb7d9]
      type t = struct_t reader_t
      val has_parameters : t -> bool
      val parameters_get : t -> FunctionParameters.t
      val parameters_get_pipelined : struct_t MessageWrapper.StructRef.t -> FunctionParameters.struct_t MessageWrapper.StructRef.t
      val has_return_annotation : t -> bool
      val return_annotation_get : t -> PysaType.t
      val return_annotation_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionBaseDefinition : sig
      type struct_t = [`FunctionBaseDefinition_876cdc721c603758]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_get_pipelined : struct_t MessageWrapper.StructRef.t -> ScopeParent.struct_t MessageWrapper.StructRef.t
      val is_overload_get : t -> bool
      val is_staticmethod_get : t -> bool
      val is_classmethod_get : t -> bool
      val is_property_getter_get : t -> bool
      val is_property_setter_get : t -> bool
      val is_stub_get : t -> bool
      val is_def_statement_get : t -> bool
      val has_defining_class : t -> bool
      val defining_class_get : t -> ClassRef.t
      val defining_class_get_pipelined : struct_t MessageWrapper.StructRef.t -> ClassRef.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module CapturedVariableRef : sig
      type struct_t = [`CapturedVariableRef_94805e8830d6bb4e]
      type t = struct_t reader_t
      val has_outer_function : t -> bool
      val outer_function_get : t -> FunctionRef.t
      val outer_function_get_pipelined : struct_t MessageWrapper.StructRef.t -> FunctionRef.struct_t MessageWrapper.StructRef.t
      val has_name : t -> bool
      val name_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Target : sig
      type struct_t = [`Target_ff8266cc34db19d3]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Function of FunctionRef.t
        | Overrides of FunctionRef.t
        | FormatString
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ImplicitReceiver : sig
      type t = ImplicitReceiver_16434162114200234704.t =
        | TrueWithClassReceiver
        | TrueWithObjectReceiver
        | False
        | Undefined of int
    end
    module PysaCallTarget : sig
      type struct_t = [`PysaCallTarget_cf1542da5a3f29e1]
      type t = struct_t reader_t
      val has_target : t -> bool
      val target_get : t -> Target.t
      val target_get_pipelined : struct_t MessageWrapper.StructRef.t -> Target.struct_t MessageWrapper.StructRef.t
      val implicit_receiver_get : t -> ImplicitReceiver.t
      val implicit_dunder_call_get : t -> bool
      val has_receiver_class : t -> bool
      val receiver_class_get : t -> ClassRef.t
      val receiver_class_get_pipelined : struct_t MessageWrapper.StructRef.t -> ClassRef.struct_t MessageWrapper.StructRef.t
      val is_class_method_get : t -> bool
      val is_static_method_get : t -> bool
      val has_return_type : t -> bool
      val return_type_get : t -> ScalarTypeProperties.t
      val return_type_get_pipelined : struct_t MessageWrapper.StructRef.t -> ScalarTypeProperties.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module DecoratorCallee : sig
      type struct_t = [`DecoratorCallee_bc083e8c7ab91017]
      type t = struct_t reader_t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val has_targets : t -> bool
      val targets_get : t -> (ro, Target.t, array_t) Capnp.Array.t
      val targets_get_list : t -> Target.t list
      val targets_get_array : t -> Target.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionDefinition : sig
      type struct_t = [`FunctionDefinition_ac2bccd871436411]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val has_define_name_location : t -> bool
      val define_name_location_get : t -> PysaLocation.t
      val define_name_location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_get_pipelined : struct_t MessageWrapper.StructRef.t -> ScopeParent.struct_t MessageWrapper.StructRef.t
      val is_overload_get : t -> bool
      val is_staticmethod_get : t -> bool
      val is_classmethod_get : t -> bool
      val is_property_getter_get : t -> bool
      val is_property_setter_get : t -> bool
      val is_stub_get : t -> bool
      val is_def_statement_get : t -> bool
      val has_defining_class : t -> bool
      val defining_class_get : t -> ClassRef.t
      val defining_class_get_pipelined : struct_t MessageWrapper.StructRef.t -> ClassRef.struct_t MessageWrapper.StructRef.t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val has_undecorated_signatures : t -> bool
      val undecorated_signatures_get : t -> (ro, FunctionSignature.t, array_t) Capnp.Array.t
      val undecorated_signatures_get_list : t -> FunctionSignature.t list
      val undecorated_signatures_get_array : t -> FunctionSignature.t array
      val has_captured_variables : t -> bool
      val captured_variables_get : t -> (ro, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_get_list : t -> CapturedVariableRef.t list
      val captured_variables_get_array : t -> CapturedVariableRef.t array
      val has_decorator_callees : t -> bool
      val decorator_callees_get : t -> (ro, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_get_list : t -> DecoratorCallee.t list
      val decorator_callees_get_array : t -> DecoratorCallee.t array
      val has_overridden_base_method : t -> bool
      val overridden_base_method_get : t -> FunctionRef.t
      val overridden_base_method_get_pipelined : struct_t MessageWrapper.StructRef.t -> FunctionRef.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PysaClassFieldDeclaration : sig
      type t = PysaClassFieldDeclaration_17132881886155188463.t =
        | None
        | DeclaredByAnnotation
        | DeclaredWithoutAnnotation
        | AssignedInBody
        | DefinedWithoutAssign
        | DefinedInMethod
        | Undefined of int
    end
    module PysaClassField : sig
      type struct_t = [`PysaClassField_f94ec0e6aa9a5e25]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val has_type : t -> bool
      val type_get : t -> PysaType.t
      val type_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
      val has_explicit_annotation : t -> bool
      val explicit_annotation_get : t -> string
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val declaration_kind_get : t -> PysaClassFieldDeclaration.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PysaClassMro : sig
      type struct_t = [`PysaClassMro_addaafc52d26dffb]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Resolved of (ro, ClassRef.t, array_t) Capnp.Array.t
        | Cyclic
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ClassDefinition : sig
      type struct_t = [`ClassDefinition_f802b9f88052bf19]
      type t = struct_t reader_t
      val class_id_get : t -> Stdint.Uint32.t
      val class_id_get_int_exn : t -> int
      val has_name : t -> bool
      val name_get : t -> string
      val has_name_location : t -> bool
      val name_location_get : t -> PysaLocation.t
      val name_location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val has_bases : t -> bool
      val bases_get : t -> (ro, ClassRef.t, array_t) Capnp.Array.t
      val bases_get_list : t -> ClassRef.t list
      val bases_get_array : t -> ClassRef.t array
      val has_mro : t -> bool
      val mro_get : t -> PysaClassMro.t
      val mro_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaClassMro.struct_t MessageWrapper.StructRef.t
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_get_pipelined : struct_t MessageWrapper.StructRef.t -> ScopeParent.struct_t MessageWrapper.StructRef.t
      val is_synthesized_get : t -> bool
      val is_dataclass_get : t -> bool
      val is_named_tuple_get : t -> bool
      val is_typed_dict_get : t -> bool
      val has_fields : t -> bool
      val fields_get : t -> (ro, PysaClassField.t, array_t) Capnp.Array.t
      val fields_get_list : t -> PysaClassField.t list
      val fields_get_array : t -> PysaClassField.t array
      val has_decorator_callees : t -> bool
      val decorator_callees_get : t -> (ro, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_get_list : t -> DecoratorCallee.t list
      val decorator_callees_get_array : t -> DecoratorCallee.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module GlobalVariable : sig
      type struct_t = [`GlobalVariable_d7ecbd0f4024046b]
      type t = struct_t reader_t
      val has_name : t -> bool
      val name_get : t -> string
      val has_type : t -> bool
      val type_get : t -> PysaType.t
      val type_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaType.struct_t MessageWrapper.StructRef.t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module UnresolvedReason : sig
      type t = UnresolvedReason_16256959910610618162.t =
        | LambdaArgument
        | UnexpectedPyreflyTarget
        | EmptyPyreflyCallTarget
        | UnknownClassField
        | ClassFieldOnlyExistInObject
        | UnsupportedFunctionTarget
        | UnexpectedDefiningClass
        | UnexpectedInitMethod
        | UnexpectedNewMethod
        | UnexpectedCalleeExpression
        | UnresolvedMagicDunderAttr
        | UnresolvedMagicDunderAttrDueToNoBase
        | UnresolvedMagicDunderAttrDueToNoAttribute
        | Mixed
        | Undefined of int
    end
    module Unresolved : sig
      type struct_t = [`Unresolved_f5be7abed98173b3]
      type t = struct_t reader_t
      type unnamed_union_t =
        | False
        | True of UnresolvedReason.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module HigherOrderParameter : sig
      type struct_t = [`HigherOrderParameter_9f08f2a4f21fb93d]
      type t = struct_t reader_t
      val index_get : t -> Stdint.Uint32.t
      val index_get_int_exn : t -> int
      val has_call_targets : t -> bool
      val call_targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_get_list : t -> PysaCallTarget.t list
      val call_targets_get_array : t -> PysaCallTarget.t array
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_get_pipelined : struct_t MessageWrapper.StructRef.t -> Unresolved.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module CallCallees : sig
      type struct_t = [`CallCallees_fa97c29dc67a9655]
      type t = struct_t reader_t
      val has_call_targets : t -> bool
      val call_targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_get_list : t -> PysaCallTarget.t list
      val call_targets_get_array : t -> PysaCallTarget.t array
      val has_init_targets : t -> bool
      val init_targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val init_targets_get_list : t -> PysaCallTarget.t list
      val init_targets_get_array : t -> PysaCallTarget.t array
      val has_new_targets : t -> bool
      val new_targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val new_targets_get_list : t -> PysaCallTarget.t list
      val new_targets_get_array : t -> PysaCallTarget.t array
      val has_higher_order_parameters : t -> bool
      val higher_order_parameters_get : t -> (ro, HigherOrderParameter.t, array_t) Capnp.Array.t
      val higher_order_parameters_get_list : t -> HigherOrderParameter.t list
      val higher_order_parameters_get_array : t -> HigherOrderParameter.t array
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_get_pipelined : struct_t MessageWrapper.StructRef.t -> Unresolved.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module AttributeAccessCallees : sig
      type struct_t = [`AttributeAccessCallees_da03cbf714d87201]
      type t = struct_t reader_t
      val has_if_called : t -> bool
      val if_called_get : t -> CallCallees.t
      val if_called_get_pipelined : struct_t MessageWrapper.StructRef.t -> CallCallees.struct_t MessageWrapper.StructRef.t
      val has_property_setters : t -> bool
      val property_setters_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_setters_get_list : t -> PysaCallTarget.t list
      val property_setters_get_array : t -> PysaCallTarget.t array
      val has_property_getters : t -> bool
      val property_getters_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_getters_get_list : t -> PysaCallTarget.t list
      val property_getters_get_array : t -> PysaCallTarget.t array
      val has_global_targets : t -> bool
      val global_targets_get : t -> (ro, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_get_list : t -> GlobalVariableRef.t list
      val global_targets_get_array : t -> GlobalVariableRef.t array
      val is_attribute_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module IdentifierCallees : sig
      type struct_t = [`IdentifierCallees_a5f0dc0068285f29]
      type t = struct_t reader_t
      val has_if_called : t -> bool
      val if_called_get : t -> CallCallees.t
      val if_called_get_pipelined : struct_t MessageWrapper.StructRef.t -> CallCallees.struct_t MessageWrapper.StructRef.t
      val has_global_targets : t -> bool
      val global_targets_get : t -> (ro, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_get_list : t -> GlobalVariableRef.t list
      val global_targets_get_array : t -> GlobalVariableRef.t array
      val has_captured_variables : t -> bool
      val captured_variables_get : t -> (ro, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_get_list : t -> CapturedVariableRef.t list
      val captured_variables_get_array : t -> CapturedVariableRef.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module DefineCallees : sig
      type struct_t = [`DefineCallees_b379211649ffdce7]
      type t = struct_t reader_t
      val has_define_targets : t -> bool
      val define_targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val define_targets_get_list : t -> PysaCallTarget.t list
      val define_targets_get_array : t -> PysaCallTarget.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FormatStringArtificialCallees : sig
      type struct_t = [`FormatStringArtificialCallees_a05d8efd266e9543]
      type t = struct_t reader_t
      val has_targets : t -> bool
      val targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FormatStringStringifyCallees : sig
      type struct_t = [`FormatStringStringifyCallees_c57214d24c99a6b3]
      type t = struct_t reader_t
      val has_targets : t -> bool
      val targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_get_pipelined : struct_t MessageWrapper.StructRef.t -> Unresolved.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ReturnShimArgumentMapping : sig
      type t = ReturnShimArgumentMapping_16744047642608040181.t =
        | ReturnExpression
        | ReturnExpressionElement
        | Undefined of int
    end
    module ReturnShimCallees : sig
      type struct_t = [`ReturnShimCallees_a830c3d4e1910d6d]
      type t = struct_t reader_t
      val has_targets : t -> bool
      val targets_get : t -> (ro, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val has_arguments : t -> bool
      val arguments_get : t -> (ro, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val arguments_get_list : t -> ReturnShimArgumentMapping.t list
      val arguments_get_array : t -> ReturnShimArgumentMapping.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ExpressionCallees : sig
      type struct_t = [`ExpressionCallees_9c207ddf98bd8cdf]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Call of CallCallees.t
        | Identifier of IdentifierCallees.t
        | AttributeAccess of AttributeAccessCallees.t
        | Define of DefineCallees.t
        | FormatStringArtificial of FormatStringArtificialCallees.t
        | FormatStringStringify of FormatStringStringifyCallees.t
        | Return of ReturnShimCallees.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module CallGraphEntry : sig
      type struct_t = [`CallGraphEntry_d918afd531f118f8]
      type t = struct_t reader_t
      val has_expression_id : t -> bool
      val expression_id_get : t -> string
      val has_callees : t -> bool
      val callees_get : t -> ExpressionCallees.t
      val callees_get_pipelined : struct_t MessageWrapper.StructRef.t -> ExpressionCallees.struct_t MessageWrapper.StructRef.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionCallGraph : sig
      type struct_t = [`FunctionCallGraph_e54ecff47ce9d92a]
      type t = struct_t reader_t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val has_entries : t -> bool
      val entries_get : t -> (ro, CallGraphEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> CallGraphEntry.t list
      val entries_get_array : t -> CallGraphEntry.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PysaProjectModule : sig
      type struct_t = [`PysaProjectModule_898e79c185b3c64b]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_get_pipelined : struct_t MessageWrapper.StructRef.t -> SourcePath.struct_t MessageWrapper.StructRef.t
      val has_relative_source_path : t -> bool
      val relative_source_path_get : t -> string
      val has_info_filename : t -> bool
      val info_filename_get : t -> string
      val has_python_version : t -> bool
      val python_version_get : t -> string
      val has_platform : t -> bool
      val platform_get : t -> string
      val is_test_get : t -> bool
      val is_interface_get : t -> bool
      val is_init_get : t -> bool
      val is_internal_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ProjectFile : sig
      type struct_t = [`ProjectFile_fb60e377cc6c9cae]
      type t = struct_t reader_t
      val has_modules : t -> bool
      val modules_get : t -> (ro, PysaProjectModule.t, array_t) Capnp.Array.t
      val modules_get_list : t -> PysaProjectModule.t list
      val modules_get_array : t -> PysaProjectModule.t array
      val has_builtin_module_ids : t -> bool
      val builtin_module_ids_get : t -> (ro, Stdint.Uint32.t, array_t) Capnp.Array.t
      val builtin_module_ids_get_list : t -> Stdint.Uint32.t list
      val builtin_module_ids_get_array : t -> Stdint.Uint32.t array
      val has_object_class_refs : t -> bool
      val object_class_refs_get : t -> (ro, ClassRef.t, array_t) Capnp.Array.t
      val object_class_refs_get_list : t -> ClassRef.t list
      val object_class_refs_get_array : t -> ClassRef.t array
      val has_dict_class_refs : t -> bool
      val dict_class_refs_get : t -> (ro, ClassRef.t, array_t) Capnp.Array.t
      val dict_class_refs_get_list : t -> ClassRef.t list
      val dict_class_refs_get_array : t -> ClassRef.t array
      val has_typing_module_ids : t -> bool
      val typing_module_ids_get : t -> (ro, Stdint.Uint32.t, array_t) Capnp.Array.t
      val typing_module_ids_get_list : t -> Stdint.Uint32.t list
      val typing_module_ids_get_array : t -> Stdint.Uint32.t array
      val has_typing_mapping_class_refs : t -> bool
      val typing_mapping_class_refs_get : t -> (ro, ClassRef.t, array_t) Capnp.Array.t
      val typing_mapping_class_refs_get_list : t -> ClassRef.t list
      val typing_mapping_class_refs_get_array : t -> ClassRef.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ModuleDefinitions : sig
      type struct_t = [`ModuleDefinitions_c7923485cc475b88]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_get_pipelined : struct_t MessageWrapper.StructRef.t -> SourcePath.struct_t MessageWrapper.StructRef.t
      val has_function_definitions : t -> bool
      val function_definitions_get : t -> (ro, FunctionDefinition.t, array_t) Capnp.Array.t
      val function_definitions_get_list : t -> FunctionDefinition.t list
      val function_definitions_get_array : t -> FunctionDefinition.t array
      val has_class_definitions : t -> bool
      val class_definitions_get : t -> (ro, ClassDefinition.t, array_t) Capnp.Array.t
      val class_definitions_get_list : t -> ClassDefinition.t list
      val class_definitions_get_array : t -> ClassDefinition.t array
      val has_global_variables : t -> bool
      val global_variables_get : t -> (ro, GlobalVariable.t, array_t) Capnp.Array.t
      val global_variables_get_list : t -> GlobalVariable.t list
      val global_variables_get_array : t -> GlobalVariable.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module LocationTypeIdEntry : sig
      type struct_t = [`LocationTypeIdEntry_ec97908cde450b07]
      type t = struct_t reader_t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val type_id_get : t -> Stdint.Uint32.t
      val type_id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module FunctionTypeOfExpressions : sig
      type struct_t = [`FunctionTypeOfExpressions_e454c787c4578ae9]
      type t = struct_t reader_t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val has_types : t -> bool
      val types_get : t -> (ro, PysaType.t, array_t) Capnp.Array.t
      val types_get_list : t -> PysaType.t list
      val types_get_array : t -> PysaType.t array
      val has_locations : t -> bool
      val locations_get : t -> (ro, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val locations_get_list : t -> LocationTypeIdEntry.t list
      val locations_get_array : t -> LocationTypeIdEntry.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ModuleTypeOfExpressions : sig
      type struct_t = [`ModuleTypeOfExpressions_cfca7d3ae9f723a7]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_get_pipelined : struct_t MessageWrapper.StructRef.t -> SourcePath.struct_t MessageWrapper.StructRef.t
      val has_functions : t -> bool
      val functions_get : t -> (ro, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val functions_get_list : t -> FunctionTypeOfExpressions.t list
      val functions_get_array : t -> FunctionTypeOfExpressions.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ModuleCallGraphs : sig
      type struct_t = [`ModuleCallGraphs_c11251973766f6c6]
      type t = struct_t reader_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_get_pipelined : struct_t MessageWrapper.StructRef.t -> SourcePath.struct_t MessageWrapper.StructRef.t
      val has_call_graphs : t -> bool
      val call_graphs_get : t -> (ro, FunctionCallGraph.t, array_t) Capnp.Array.t
      val call_graphs_get_list : t -> FunctionCallGraph.t list
      val call_graphs_get_array : t -> FunctionCallGraph.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module TypeError : sig
      type struct_t = [`TypeError_cf833e7f4687d6b7]
      type t = struct_t reader_t
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val has_module_path : t -> bool
      val module_path_get : t -> SourcePath.t
      val module_path_get_pipelined : struct_t MessageWrapper.StructRef.t -> SourcePath.struct_t MessageWrapper.StructRef.t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_get_pipelined : struct_t MessageWrapper.StructRef.t -> PysaLocation.struct_t MessageWrapper.StructRef.t
      val has_kind : t -> bool
      val kind_get : t -> string
      val has_message : t -> bool
      val message_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module TypeErrors : sig
      type struct_t = [`TypeErrors_c2e0d691270c45f9]
      type t = struct_t reader_t
      val has_errors : t -> bool
      val errors_get : t -> (ro, TypeError.t, array_t) Capnp.Array.t
      val errors_get_list : t -> TypeError.t list
      val errors_get_array : t -> TypeError.t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module SourcePath : sig
      type struct_t = [`SourcePath_c9789635145f607d]
      type t = struct_t builder_t
      type unnamed_union_t =
        | FileSystem of string
        | Namespace of string
        | Memory of string
        | BundledTypeshed of string
        | BundledTypeshedThirdParty of string
        | BundledThirdParty of string
        | Undefined of int
      val get : t -> unnamed_union_t
      val file_system_set : t -> string -> unit
      val namespace_set : t -> string -> unit
      val memory_set : t -> string -> unit
      val bundled_typeshed_set : t -> string -> unit
      val bundled_typeshed_third_party_set : t -> string -> unit
      val bundled_third_party_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PysaLocation : sig
      type struct_t = [`PysaLocation_c309c969f3e58a60]
      type t = struct_t builder_t
      val line_get : t -> Stdint.Uint32.t
      val line_get_int_exn : t -> int
      val line_set : t -> Stdint.Uint32.t -> unit
      val line_set_int_exn : t -> int -> unit
      val col_get : t -> Stdint.Uint32.t
      val col_get_int_exn : t -> int
      val col_set : t -> Stdint.Uint32.t -> unit
      val col_set_int_exn : t -> int -> unit
      val end_line_get : t -> Stdint.Uint32.t
      val end_line_get_int_exn : t -> int
      val end_line_set : t -> Stdint.Uint32.t -> unit
      val end_line_set_int_exn : t -> int -> unit
      val end_col_get : t -> Stdint.Uint32.t
      val end_col_get_int_exn : t -> int
      val end_col_set : t -> Stdint.Uint32.t -> unit
      val end_col_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ClassRef : sig
      type struct_t = [`ClassRef_de5e9ff6d0dd25c7]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val class_id_get : t -> Stdint.Uint32.t
      val class_id_get_int_exn : t -> int
      val class_id_set : t -> Stdint.Uint32.t -> unit
      val class_id_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionRef : sig
      type struct_t = [`FunctionRef_80115bb31006b8bb]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val function_id_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module GlobalVariableRef : sig
      type struct_t = [`GlobalVariableRef_f70734c08fbb8800]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module TypeModifier : sig
      type t = TypeModifier_17523487963418047075.t =
        | Optional
        | Coroutine
        | Awaitable
        | TypeVariableBound
        | TypeVariableConstraint
        | Type
        | Undefined of int
    end
    module ClassWithModifiers : sig
      type struct_t = [`ClassWithModifiers_e15f82ade37d4236]
      type t = struct_t builder_t
      val has_class : t -> bool
      val class_get : t -> ClassRef.t
      val class_set_reader : t -> ClassRef.struct_t reader_t -> ClassRef.t
      val class_set_builder : t -> ClassRef.t -> ClassRef.t
      val class_init : t -> ClassRef.t
      val has_modifiers : t -> bool
      val modifiers_get : t -> (rw, TypeModifier.t, array_t) Capnp.Array.t
      val modifiers_get_list : t -> TypeModifier.t list
      val modifiers_get_array : t -> TypeModifier.t array
      val modifiers_set : t -> (rw, TypeModifier.t, array_t) Capnp.Array.t -> (rw, TypeModifier.t, array_t) Capnp.Array.t
      val modifiers_set_list : t -> TypeModifier.t list -> (rw, TypeModifier.t, array_t) Capnp.Array.t
      val modifiers_set_array : t -> TypeModifier.t array -> (rw, TypeModifier.t, array_t) Capnp.Array.t
      val modifiers_init : t -> int -> (rw, TypeModifier.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ClassNamesFromType : sig
      type struct_t = [`ClassNamesFromType_a0584aac2e97e83c]
      type t = struct_t builder_t
      val has_classes : t -> bool
      val classes_get : t -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t
      val classes_get_list : t -> ClassWithModifiers.t list
      val classes_get_array : t -> ClassWithModifiers.t array
      val classes_set : t -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t
      val classes_set_list : t -> ClassWithModifiers.t list -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t
      val classes_set_array : t -> ClassWithModifiers.t array -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t
      val classes_init : t -> int -> (rw, ClassWithModifiers.t, array_t) Capnp.Array.t
      val is_exhaustive_get : t -> bool
      val is_exhaustive_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ScalarTypeProperties : sig
      type struct_t = [`ScalarTypeProperties_d79ace8daba1babf]
      type t = struct_t builder_t
      val is_bool_get : t -> bool
      val is_bool_set : t -> bool -> unit
      val is_int_get : t -> bool
      val is_int_set : t -> bool -> unit
      val is_float_get : t -> bool
      val is_float_set : t -> bool -> unit
      val is_enum_get : t -> bool
      val is_enum_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PysaType : sig
      type struct_t = [`PysaType_d28d95e8ed282312]
      type t = struct_t builder_t
      val has_string : t -> bool
      val string_get : t -> string
      val string_set : t -> string -> unit
      val has_scalar_type_properties : t -> bool
      val scalar_type_properties_get : t -> ScalarTypeProperties.t
      val scalar_type_properties_set_reader : t -> ScalarTypeProperties.struct_t reader_t -> ScalarTypeProperties.t
      val scalar_type_properties_set_builder : t -> ScalarTypeProperties.t -> ScalarTypeProperties.t
      val scalar_type_properties_init : t -> ScalarTypeProperties.t
      val has_class_names : t -> bool
      val class_names_get : t -> ClassNamesFromType.t
      val class_names_set_reader : t -> ClassNamesFromType.struct_t reader_t -> ClassNamesFromType.t
      val class_names_set_builder : t -> ClassNamesFromType.t -> ClassNamesFromType.t
      val class_names_init : t -> ClassNamesFromType.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ScopeParent : sig
      type struct_t = [`ScopeParent_de2463e0e757468e]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Function of Stdint.Uint32.t
        | Class of Stdint.Uint32.t
        | TopLevel
        | Undefined of int
      val get : t -> unnamed_union_t
      val function_set : t -> Stdint.Uint32.t -> unit
      val function_set_int_exn : t -> int -> unit
      val class_set : t -> Stdint.Uint32.t -> unit
      val class_set_int_exn : t -> int -> unit
      val top_level_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionParameter : sig
      type struct_t = [`FunctionParameter_a1b2fce98392735b]
      type t = struct_t builder_t
      module PosOnlyParam : sig
        type struct_t = [`PosOnlyParam_f44b7078e9979a56]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
        val annotation_set_builder : t -> PysaType.t -> PysaType.t
        val annotation_init : t -> PysaType.t
        val required_get : t -> bool
        val required_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module PosParam : sig
        type struct_t = [`PosParam_cb05f6c652a9cccf]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
        val annotation_set_builder : t -> PysaType.t -> PysaType.t
        val annotation_init : t -> PysaType.t
        val required_get : t -> bool
        val required_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module VarArgParam : sig
        type struct_t = [`VarArgParam_987ff4fc98295c57]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
        val annotation_set_builder : t -> PysaType.t -> PysaType.t
        val annotation_init : t -> PysaType.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module KwOnlyParam : sig
        type struct_t = [`KwOnlyParam_9ceff6d824c01469]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
        val annotation_set_builder : t -> PysaType.t -> PysaType.t
        val annotation_init : t -> PysaType.t
        val required_get : t -> bool
        val required_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module KwargsParam : sig
        type struct_t = [`KwargsParam_914731ff31eab123]
        type t = struct_t builder_t
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val has_annotation : t -> bool
        val annotation_get : t -> PysaType.t
        val annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
        val annotation_set_builder : t -> PysaType.t -> PysaType.t
        val annotation_init : t -> PysaType.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      type unnamed_union_t =
        | PosOnly of [`PosOnlyParam_f44b7078e9979a56] builder_t
        | Pos of [`PosParam_cb05f6c652a9cccf] builder_t
        | VarArg of [`VarArgParam_987ff4fc98295c57] builder_t
        | KwOnly of [`KwOnlyParam_9ceff6d824c01469] builder_t
        | Kwargs of [`KwargsParam_914731ff31eab123] builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val pos_only_set_reader : t -> [`PosOnlyParam_f44b7078e9979a56] reader_t -> [`PosOnlyParam_f44b7078e9979a56] builder_t
      val pos_only_set_builder : t -> [`PosOnlyParam_f44b7078e9979a56] builder_t -> [`PosOnlyParam_f44b7078e9979a56] builder_t
      val pos_only_init : t -> [`PosOnlyParam_f44b7078e9979a56] builder_t
      val pos_set_reader : t -> [`PosParam_cb05f6c652a9cccf] reader_t -> [`PosParam_cb05f6c652a9cccf] builder_t
      val pos_set_builder : t -> [`PosParam_cb05f6c652a9cccf] builder_t -> [`PosParam_cb05f6c652a9cccf] builder_t
      val pos_init : t -> [`PosParam_cb05f6c652a9cccf] builder_t
      val var_arg_set_reader : t -> [`VarArgParam_987ff4fc98295c57] reader_t -> [`VarArgParam_987ff4fc98295c57] builder_t
      val var_arg_set_builder : t -> [`VarArgParam_987ff4fc98295c57] builder_t -> [`VarArgParam_987ff4fc98295c57] builder_t
      val var_arg_init : t -> [`VarArgParam_987ff4fc98295c57] builder_t
      val kw_only_set_reader : t -> [`KwOnlyParam_9ceff6d824c01469] reader_t -> [`KwOnlyParam_9ceff6d824c01469] builder_t
      val kw_only_set_builder : t -> [`KwOnlyParam_9ceff6d824c01469] builder_t -> [`KwOnlyParam_9ceff6d824c01469] builder_t
      val kw_only_init : t -> [`KwOnlyParam_9ceff6d824c01469] builder_t
      val kwargs_set_reader : t -> [`KwargsParam_914731ff31eab123] reader_t -> [`KwargsParam_914731ff31eab123] builder_t
      val kwargs_set_builder : t -> [`KwargsParam_914731ff31eab123] builder_t -> [`KwargsParam_914731ff31eab123] builder_t
      val kwargs_init : t -> [`KwargsParam_914731ff31eab123] builder_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionParameters : sig
      type struct_t = [`FunctionParameters_a7c6a3610ea670c8]
      type t = struct_t builder_t
      type unnamed_union_t =
        | List of (rw, FunctionParameter.t, array_t) Capnp.Array.t
        | Ellipsis
        | ParamSpec
        | Undefined of int
      val get : t -> unnamed_union_t
      val list_set : t -> (rw, FunctionParameter.t, array_t) Capnp.Array.t -> (rw, FunctionParameter.t, array_t) Capnp.Array.t
      val list_set_list : t -> FunctionParameter.t list -> (rw, FunctionParameter.t, array_t) Capnp.Array.t
      val list_set_array : t -> FunctionParameter.t array -> (rw, FunctionParameter.t, array_t) Capnp.Array.t
      val list_init : t -> int -> (rw, FunctionParameter.t, array_t) Capnp.Array.t
      val ellipsis_set : t -> unit
      val param_spec_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionSignature : sig
      type struct_t = [`FunctionSignature_be11da038b7eb7d9]
      type t = struct_t builder_t
      val has_parameters : t -> bool
      val parameters_get : t -> FunctionParameters.t
      val parameters_set_reader : t -> FunctionParameters.struct_t reader_t -> FunctionParameters.t
      val parameters_set_builder : t -> FunctionParameters.t -> FunctionParameters.t
      val parameters_init : t -> FunctionParameters.t
      val has_return_annotation : t -> bool
      val return_annotation_get : t -> PysaType.t
      val return_annotation_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
      val return_annotation_set_builder : t -> PysaType.t -> PysaType.t
      val return_annotation_init : t -> PysaType.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionBaseDefinition : sig
      type struct_t = [`FunctionBaseDefinition_876cdc721c603758]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_set_reader : t -> ScopeParent.struct_t reader_t -> ScopeParent.t
      val parent_set_builder : t -> ScopeParent.t -> ScopeParent.t
      val parent_init : t -> ScopeParent.t
      val is_overload_get : t -> bool
      val is_overload_set : t -> bool -> unit
      val is_staticmethod_get : t -> bool
      val is_staticmethod_set : t -> bool -> unit
      val is_classmethod_get : t -> bool
      val is_classmethod_set : t -> bool -> unit
      val is_property_getter_get : t -> bool
      val is_property_getter_set : t -> bool -> unit
      val is_property_setter_get : t -> bool
      val is_property_setter_set : t -> bool -> unit
      val is_stub_get : t -> bool
      val is_stub_set : t -> bool -> unit
      val is_def_statement_get : t -> bool
      val is_def_statement_set : t -> bool -> unit
      val has_defining_class : t -> bool
      val defining_class_get : t -> ClassRef.t
      val defining_class_set_reader : t -> ClassRef.struct_t reader_t -> ClassRef.t
      val defining_class_set_builder : t -> ClassRef.t -> ClassRef.t
      val defining_class_init : t -> ClassRef.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module CapturedVariableRef : sig
      type struct_t = [`CapturedVariableRef_94805e8830d6bb4e]
      type t = struct_t builder_t
      val has_outer_function : t -> bool
      val outer_function_get : t -> FunctionRef.t
      val outer_function_set_reader : t -> FunctionRef.struct_t reader_t -> FunctionRef.t
      val outer_function_set_builder : t -> FunctionRef.t -> FunctionRef.t
      val outer_function_init : t -> FunctionRef.t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Target : sig
      type struct_t = [`Target_ff8266cc34db19d3]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Function of FunctionRef.t
        | Overrides of FunctionRef.t
        | FormatString
        | Undefined of int
      val get : t -> unnamed_union_t
      val function_set_reader : t -> FunctionRef.struct_t reader_t -> FunctionRef.t
      val function_set_builder : t -> FunctionRef.t -> FunctionRef.t
      val function_init : t -> FunctionRef.t
      val overrides_set_reader : t -> FunctionRef.struct_t reader_t -> FunctionRef.t
      val overrides_set_builder : t -> FunctionRef.t -> FunctionRef.t
      val overrides_init : t -> FunctionRef.t
      val format_string_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ImplicitReceiver : sig
      type t = ImplicitReceiver_16434162114200234704.t =
        | TrueWithClassReceiver
        | TrueWithObjectReceiver
        | False
        | Undefined of int
    end
    module PysaCallTarget : sig
      type struct_t = [`PysaCallTarget_cf1542da5a3f29e1]
      type t = struct_t builder_t
      val has_target : t -> bool
      val target_get : t -> Target.t
      val target_set_reader : t -> Target.struct_t reader_t -> Target.t
      val target_set_builder : t -> Target.t -> Target.t
      val target_init : t -> Target.t
      val implicit_receiver_get : t -> ImplicitReceiver.t
      val implicit_receiver_set : t -> ImplicitReceiver.t -> unit
      val implicit_receiver_set_unsafe : t -> ImplicitReceiver.t -> unit
      val implicit_dunder_call_get : t -> bool
      val implicit_dunder_call_set : t -> bool -> unit
      val has_receiver_class : t -> bool
      val receiver_class_get : t -> ClassRef.t
      val receiver_class_set_reader : t -> ClassRef.struct_t reader_t -> ClassRef.t
      val receiver_class_set_builder : t -> ClassRef.t -> ClassRef.t
      val receiver_class_init : t -> ClassRef.t
      val is_class_method_get : t -> bool
      val is_class_method_set : t -> bool -> unit
      val is_static_method_get : t -> bool
      val is_static_method_set : t -> bool -> unit
      val has_return_type : t -> bool
      val return_type_get : t -> ScalarTypeProperties.t
      val return_type_set_reader : t -> ScalarTypeProperties.struct_t reader_t -> ScalarTypeProperties.t
      val return_type_set_builder : t -> ScalarTypeProperties.t -> ScalarTypeProperties.t
      val return_type_init : t -> ScalarTypeProperties.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module DecoratorCallee : sig
      type struct_t = [`DecoratorCallee_bc083e8c7ab91017]
      type t = struct_t builder_t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val location_init : t -> PysaLocation.t
      val has_targets : t -> bool
      val targets_get : t -> (rw, Target.t, array_t) Capnp.Array.t
      val targets_get_list : t -> Target.t list
      val targets_get_array : t -> Target.t array
      val targets_set : t -> (rw, Target.t, array_t) Capnp.Array.t -> (rw, Target.t, array_t) Capnp.Array.t
      val targets_set_list : t -> Target.t list -> (rw, Target.t, array_t) Capnp.Array.t
      val targets_set_array : t -> Target.t array -> (rw, Target.t, array_t) Capnp.Array.t
      val targets_init : t -> int -> (rw, Target.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionDefinition : sig
      type struct_t = [`FunctionDefinition_ac2bccd871436411]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val has_define_name_location : t -> bool
      val define_name_location_get : t -> PysaLocation.t
      val define_name_location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val define_name_location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val define_name_location_init : t -> PysaLocation.t
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_set_reader : t -> ScopeParent.struct_t reader_t -> ScopeParent.t
      val parent_set_builder : t -> ScopeParent.t -> ScopeParent.t
      val parent_init : t -> ScopeParent.t
      val is_overload_get : t -> bool
      val is_overload_set : t -> bool -> unit
      val is_staticmethod_get : t -> bool
      val is_staticmethod_set : t -> bool -> unit
      val is_classmethod_get : t -> bool
      val is_classmethod_set : t -> bool -> unit
      val is_property_getter_get : t -> bool
      val is_property_getter_set : t -> bool -> unit
      val is_property_setter_get : t -> bool
      val is_property_setter_set : t -> bool -> unit
      val is_stub_get : t -> bool
      val is_stub_set : t -> bool -> unit
      val is_def_statement_get : t -> bool
      val is_def_statement_set : t -> bool -> unit
      val has_defining_class : t -> bool
      val defining_class_get : t -> ClassRef.t
      val defining_class_set_reader : t -> ClassRef.struct_t reader_t -> ClassRef.t
      val defining_class_set_builder : t -> ClassRef.t -> ClassRef.t
      val defining_class_init : t -> ClassRef.t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val function_id_set : t -> string -> unit
      val has_undecorated_signatures : t -> bool
      val undecorated_signatures_get : t -> (rw, FunctionSignature.t, array_t) Capnp.Array.t
      val undecorated_signatures_get_list : t -> FunctionSignature.t list
      val undecorated_signatures_get_array : t -> FunctionSignature.t array
      val undecorated_signatures_set : t -> (rw, FunctionSignature.t, array_t) Capnp.Array.t -> (rw, FunctionSignature.t, array_t) Capnp.Array.t
      val undecorated_signatures_set_list : t -> FunctionSignature.t list -> (rw, FunctionSignature.t, array_t) Capnp.Array.t
      val undecorated_signatures_set_array : t -> FunctionSignature.t array -> (rw, FunctionSignature.t, array_t) Capnp.Array.t
      val undecorated_signatures_init : t -> int -> (rw, FunctionSignature.t, array_t) Capnp.Array.t
      val has_captured_variables : t -> bool
      val captured_variables_get : t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_get_list : t -> CapturedVariableRef.t list
      val captured_variables_get_array : t -> CapturedVariableRef.t array
      val captured_variables_set : t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_set_list : t -> CapturedVariableRef.t list -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_set_array : t -> CapturedVariableRef.t array -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_init : t -> int -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val has_decorator_callees : t -> bool
      val decorator_callees_get : t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_get_list : t -> DecoratorCallee.t list
      val decorator_callees_get_array : t -> DecoratorCallee.t array
      val decorator_callees_set : t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_set_list : t -> DecoratorCallee.t list -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_set_array : t -> DecoratorCallee.t array -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_init : t -> int -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val has_overridden_base_method : t -> bool
      val overridden_base_method_get : t -> FunctionRef.t
      val overridden_base_method_set_reader : t -> FunctionRef.struct_t reader_t -> FunctionRef.t
      val overridden_base_method_set_builder : t -> FunctionRef.t -> FunctionRef.t
      val overridden_base_method_init : t -> FunctionRef.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PysaClassFieldDeclaration : sig
      type t = PysaClassFieldDeclaration_17132881886155188463.t =
        | None
        | DeclaredByAnnotation
        | DeclaredWithoutAnnotation
        | AssignedInBody
        | DefinedWithoutAssign
        | DefinedInMethod
        | Undefined of int
    end
    module PysaClassField : sig
      type struct_t = [`PysaClassField_f94ec0e6aa9a5e25]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val has_type : t -> bool
      val type_get : t -> PysaType.t
      val type_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
      val type_set_builder : t -> PysaType.t -> PysaType.t
      val type_init : t -> PysaType.t
      val has_explicit_annotation : t -> bool
      val explicit_annotation_get : t -> string
      val explicit_annotation_set : t -> string -> unit
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val location_init : t -> PysaLocation.t
      val declaration_kind_get : t -> PysaClassFieldDeclaration.t
      val declaration_kind_set : t -> PysaClassFieldDeclaration.t -> unit
      val declaration_kind_set_unsafe : t -> PysaClassFieldDeclaration.t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PysaClassMro : sig
      type struct_t = [`PysaClassMro_addaafc52d26dffb]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Resolved of (rw, ClassRef.t, array_t) Capnp.Array.t
        | Cyclic
        | Undefined of int
      val get : t -> unnamed_union_t
      val resolved_set : t -> (rw, ClassRef.t, array_t) Capnp.Array.t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val resolved_set_list : t -> ClassRef.t list -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val resolved_set_array : t -> ClassRef.t array -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val resolved_init : t -> int -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val cyclic_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ClassDefinition : sig
      type struct_t = [`ClassDefinition_f802b9f88052bf19]
      type t = struct_t builder_t
      val class_id_get : t -> Stdint.Uint32.t
      val class_id_get_int_exn : t -> int
      val class_id_set : t -> Stdint.Uint32.t -> unit
      val class_id_set_int_exn : t -> int -> unit
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val has_name_location : t -> bool
      val name_location_get : t -> PysaLocation.t
      val name_location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val name_location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val name_location_init : t -> PysaLocation.t
      val has_bases : t -> bool
      val bases_get : t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val bases_get_list : t -> ClassRef.t list
      val bases_get_array : t -> ClassRef.t array
      val bases_set : t -> (rw, ClassRef.t, array_t) Capnp.Array.t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val bases_set_list : t -> ClassRef.t list -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val bases_set_array : t -> ClassRef.t array -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val bases_init : t -> int -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val has_mro : t -> bool
      val mro_get : t -> PysaClassMro.t
      val mro_set_reader : t -> PysaClassMro.struct_t reader_t -> PysaClassMro.t
      val mro_set_builder : t -> PysaClassMro.t -> PysaClassMro.t
      val mro_init : t -> PysaClassMro.t
      val has_parent : t -> bool
      val parent_get : t -> ScopeParent.t
      val parent_set_reader : t -> ScopeParent.struct_t reader_t -> ScopeParent.t
      val parent_set_builder : t -> ScopeParent.t -> ScopeParent.t
      val parent_init : t -> ScopeParent.t
      val is_synthesized_get : t -> bool
      val is_synthesized_set : t -> bool -> unit
      val is_dataclass_get : t -> bool
      val is_dataclass_set : t -> bool -> unit
      val is_named_tuple_get : t -> bool
      val is_named_tuple_set : t -> bool -> unit
      val is_typed_dict_get : t -> bool
      val is_typed_dict_set : t -> bool -> unit
      val has_fields : t -> bool
      val fields_get : t -> (rw, PysaClassField.t, array_t) Capnp.Array.t
      val fields_get_list : t -> PysaClassField.t list
      val fields_get_array : t -> PysaClassField.t array
      val fields_set : t -> (rw, PysaClassField.t, array_t) Capnp.Array.t -> (rw, PysaClassField.t, array_t) Capnp.Array.t
      val fields_set_list : t -> PysaClassField.t list -> (rw, PysaClassField.t, array_t) Capnp.Array.t
      val fields_set_array : t -> PysaClassField.t array -> (rw, PysaClassField.t, array_t) Capnp.Array.t
      val fields_init : t -> int -> (rw, PysaClassField.t, array_t) Capnp.Array.t
      val has_decorator_callees : t -> bool
      val decorator_callees_get : t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_get_list : t -> DecoratorCallee.t list
      val decorator_callees_get_array : t -> DecoratorCallee.t array
      val decorator_callees_set : t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_set_list : t -> DecoratorCallee.t list -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_set_array : t -> DecoratorCallee.t array -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val decorator_callees_init : t -> int -> (rw, DecoratorCallee.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module GlobalVariable : sig
      type struct_t = [`GlobalVariable_d7ecbd0f4024046b]
      type t = struct_t builder_t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val has_type : t -> bool
      val type_get : t -> PysaType.t
      val type_set_reader : t -> PysaType.struct_t reader_t -> PysaType.t
      val type_set_builder : t -> PysaType.t -> PysaType.t
      val type_init : t -> PysaType.t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val location_init : t -> PysaLocation.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module UnresolvedReason : sig
      type t = UnresolvedReason_16256959910610618162.t =
        | LambdaArgument
        | UnexpectedPyreflyTarget
        | EmptyPyreflyCallTarget
        | UnknownClassField
        | ClassFieldOnlyExistInObject
        | UnsupportedFunctionTarget
        | UnexpectedDefiningClass
        | UnexpectedInitMethod
        | UnexpectedNewMethod
        | UnexpectedCalleeExpression
        | UnresolvedMagicDunderAttr
        | UnresolvedMagicDunderAttrDueToNoBase
        | UnresolvedMagicDunderAttrDueToNoAttribute
        | Mixed
        | Undefined of int
    end
    module Unresolved : sig
      type struct_t = [`Unresolved_f5be7abed98173b3]
      type t = struct_t builder_t
      type unnamed_union_t =
        | False
        | True of UnresolvedReason.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val false_set : t -> unit
      val true_set : t -> UnresolvedReason.t -> unit
      val true_set_unsafe : t -> UnresolvedReason.t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module HigherOrderParameter : sig
      type struct_t = [`HigherOrderParameter_9f08f2a4f21fb93d]
      type t = struct_t builder_t
      val index_get : t -> Stdint.Uint32.t
      val index_get_int_exn : t -> int
      val index_set : t -> Stdint.Uint32.t -> unit
      val index_set_int_exn : t -> int -> unit
      val has_call_targets : t -> bool
      val call_targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_get_list : t -> PysaCallTarget.t list
      val call_targets_get_array : t -> PysaCallTarget.t array
      val call_targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_set_reader : t -> Unresolved.struct_t reader_t -> Unresolved.t
      val unresolved_set_builder : t -> Unresolved.t -> Unresolved.t
      val unresolved_init : t -> Unresolved.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module CallCallees : sig
      type struct_t = [`CallCallees_fa97c29dc67a9655]
      type t = struct_t builder_t
      val has_call_targets : t -> bool
      val call_targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_get_list : t -> PysaCallTarget.t list
      val call_targets_get_array : t -> PysaCallTarget.t array
      val call_targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val call_targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_init_targets : t -> bool
      val init_targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val init_targets_get_list : t -> PysaCallTarget.t list
      val init_targets_get_array : t -> PysaCallTarget.t array
      val init_targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val init_targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val init_targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val init_targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_new_targets : t -> bool
      val new_targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val new_targets_get_list : t -> PysaCallTarget.t list
      val new_targets_get_array : t -> PysaCallTarget.t array
      val new_targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val new_targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val new_targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val new_targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_higher_order_parameters : t -> bool
      val higher_order_parameters_get : t -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t
      val higher_order_parameters_get_list : t -> HigherOrderParameter.t list
      val higher_order_parameters_get_array : t -> HigherOrderParameter.t array
      val higher_order_parameters_set : t -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t
      val higher_order_parameters_set_list : t -> HigherOrderParameter.t list -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t
      val higher_order_parameters_set_array : t -> HigherOrderParameter.t array -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t
      val higher_order_parameters_init : t -> int -> (rw, HigherOrderParameter.t, array_t) Capnp.Array.t
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_set_reader : t -> Unresolved.struct_t reader_t -> Unresolved.t
      val unresolved_set_builder : t -> Unresolved.t -> Unresolved.t
      val unresolved_init : t -> Unresolved.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module AttributeAccessCallees : sig
      type struct_t = [`AttributeAccessCallees_da03cbf714d87201]
      type t = struct_t builder_t
      val has_if_called : t -> bool
      val if_called_get : t -> CallCallees.t
      val if_called_set_reader : t -> CallCallees.struct_t reader_t -> CallCallees.t
      val if_called_set_builder : t -> CallCallees.t -> CallCallees.t
      val if_called_init : t -> CallCallees.t
      val has_property_setters : t -> bool
      val property_setters_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_setters_get_list : t -> PysaCallTarget.t list
      val property_setters_get_array : t -> PysaCallTarget.t array
      val property_setters_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_setters_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_setters_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_setters_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_property_getters : t -> bool
      val property_getters_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_getters_get_list : t -> PysaCallTarget.t list
      val property_getters_get_array : t -> PysaCallTarget.t array
      val property_getters_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_getters_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_getters_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val property_getters_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_global_targets : t -> bool
      val global_targets_get : t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_get_list : t -> GlobalVariableRef.t list
      val global_targets_get_array : t -> GlobalVariableRef.t array
      val global_targets_set : t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_set_list : t -> GlobalVariableRef.t list -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_set_array : t -> GlobalVariableRef.t array -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_init : t -> int -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val is_attribute_get : t -> bool
      val is_attribute_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module IdentifierCallees : sig
      type struct_t = [`IdentifierCallees_a5f0dc0068285f29]
      type t = struct_t builder_t
      val has_if_called : t -> bool
      val if_called_get : t -> CallCallees.t
      val if_called_set_reader : t -> CallCallees.struct_t reader_t -> CallCallees.t
      val if_called_set_builder : t -> CallCallees.t -> CallCallees.t
      val if_called_init : t -> CallCallees.t
      val has_global_targets : t -> bool
      val global_targets_get : t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_get_list : t -> GlobalVariableRef.t list
      val global_targets_get_array : t -> GlobalVariableRef.t array
      val global_targets_set : t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_set_list : t -> GlobalVariableRef.t list -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_set_array : t -> GlobalVariableRef.t array -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val global_targets_init : t -> int -> (rw, GlobalVariableRef.t, array_t) Capnp.Array.t
      val has_captured_variables : t -> bool
      val captured_variables_get : t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_get_list : t -> CapturedVariableRef.t list
      val captured_variables_get_array : t -> CapturedVariableRef.t array
      val captured_variables_set : t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_set_list : t -> CapturedVariableRef.t list -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_set_array : t -> CapturedVariableRef.t array -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val captured_variables_init : t -> int -> (rw, CapturedVariableRef.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module DefineCallees : sig
      type struct_t = [`DefineCallees_b379211649ffdce7]
      type t = struct_t builder_t
      val has_define_targets : t -> bool
      val define_targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val define_targets_get_list : t -> PysaCallTarget.t list
      val define_targets_get_array : t -> PysaCallTarget.t array
      val define_targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val define_targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val define_targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val define_targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FormatStringArtificialCallees : sig
      type struct_t = [`FormatStringArtificialCallees_a05d8efd266e9543]
      type t = struct_t builder_t
      val has_targets : t -> bool
      val targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FormatStringStringifyCallees : sig
      type struct_t = [`FormatStringStringifyCallees_c57214d24c99a6b3]
      type t = struct_t builder_t
      val has_targets : t -> bool
      val targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_unresolved : t -> bool
      val unresolved_get : t -> Unresolved.t
      val unresolved_set_reader : t -> Unresolved.struct_t reader_t -> Unresolved.t
      val unresolved_set_builder : t -> Unresolved.t -> Unresolved.t
      val unresolved_init : t -> Unresolved.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ReturnShimArgumentMapping : sig
      type t = ReturnShimArgumentMapping_16744047642608040181.t =
        | ReturnExpression
        | ReturnExpressionElement
        | Undefined of int
    end
    module ReturnShimCallees : sig
      type struct_t = [`ReturnShimCallees_a830c3d4e1910d6d]
      type t = struct_t builder_t
      val has_targets : t -> bool
      val targets_get : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_get_list : t -> PysaCallTarget.t list
      val targets_get_array : t -> PysaCallTarget.t array
      val targets_set : t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_list : t -> PysaCallTarget.t list -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_set_array : t -> PysaCallTarget.t array -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val targets_init : t -> int -> (rw, PysaCallTarget.t, array_t) Capnp.Array.t
      val has_arguments : t -> bool
      val arguments_get : t -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val arguments_get_list : t -> ReturnShimArgumentMapping.t list
      val arguments_get_array : t -> ReturnShimArgumentMapping.t array
      val arguments_set : t -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val arguments_set_list : t -> ReturnShimArgumentMapping.t list -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val arguments_set_array : t -> ReturnShimArgumentMapping.t array -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val arguments_init : t -> int -> (rw, ReturnShimArgumentMapping.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ExpressionCallees : sig
      type struct_t = [`ExpressionCallees_9c207ddf98bd8cdf]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Call of CallCallees.t
        | Identifier of IdentifierCallees.t
        | AttributeAccess of AttributeAccessCallees.t
        | Define of DefineCallees.t
        | FormatStringArtificial of FormatStringArtificialCallees.t
        | FormatStringStringify of FormatStringStringifyCallees.t
        | Return of ReturnShimCallees.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val call_set_reader : t -> CallCallees.struct_t reader_t -> CallCallees.t
      val call_set_builder : t -> CallCallees.t -> CallCallees.t
      val call_init : t -> CallCallees.t
      val identifier_set_reader : t -> IdentifierCallees.struct_t reader_t -> IdentifierCallees.t
      val identifier_set_builder : t -> IdentifierCallees.t -> IdentifierCallees.t
      val identifier_init : t -> IdentifierCallees.t
      val attribute_access_set_reader : t -> AttributeAccessCallees.struct_t reader_t -> AttributeAccessCallees.t
      val attribute_access_set_builder : t -> AttributeAccessCallees.t -> AttributeAccessCallees.t
      val attribute_access_init : t -> AttributeAccessCallees.t
      val define_set_reader : t -> DefineCallees.struct_t reader_t -> DefineCallees.t
      val define_set_builder : t -> DefineCallees.t -> DefineCallees.t
      val define_init : t -> DefineCallees.t
      val format_string_artificial_set_reader : t -> FormatStringArtificialCallees.struct_t reader_t -> FormatStringArtificialCallees.t
      val format_string_artificial_set_builder : t -> FormatStringArtificialCallees.t -> FormatStringArtificialCallees.t
      val format_string_artificial_init : t -> FormatStringArtificialCallees.t
      val format_string_stringify_set_reader : t -> FormatStringStringifyCallees.struct_t reader_t -> FormatStringStringifyCallees.t
      val format_string_stringify_set_builder : t -> FormatStringStringifyCallees.t -> FormatStringStringifyCallees.t
      val format_string_stringify_init : t -> FormatStringStringifyCallees.t
      val return_set_reader : t -> ReturnShimCallees.struct_t reader_t -> ReturnShimCallees.t
      val return_set_builder : t -> ReturnShimCallees.t -> ReturnShimCallees.t
      val return_init : t -> ReturnShimCallees.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module CallGraphEntry : sig
      type struct_t = [`CallGraphEntry_d918afd531f118f8]
      type t = struct_t builder_t
      val has_expression_id : t -> bool
      val expression_id_get : t -> string
      val expression_id_set : t -> string -> unit
      val has_callees : t -> bool
      val callees_get : t -> ExpressionCallees.t
      val callees_set_reader : t -> ExpressionCallees.struct_t reader_t -> ExpressionCallees.t
      val callees_set_builder : t -> ExpressionCallees.t -> ExpressionCallees.t
      val callees_init : t -> ExpressionCallees.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionCallGraph : sig
      type struct_t = [`FunctionCallGraph_e54ecff47ce9d92a]
      type t = struct_t builder_t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val function_id_set : t -> string -> unit
      val has_entries : t -> bool
      val entries_get : t -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> CallGraphEntry.t list
      val entries_get_array : t -> CallGraphEntry.t array
      val entries_set : t -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t
      val entries_set_list : t -> CallGraphEntry.t list -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t
      val entries_set_array : t -> CallGraphEntry.t array -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t
      val entries_init : t -> int -> (rw, CallGraphEntry.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PysaProjectModule : sig
      type struct_t = [`PysaProjectModule_898e79c185b3c64b]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val module_name_set : t -> string -> unit
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_set_reader : t -> SourcePath.struct_t reader_t -> SourcePath.t
      val source_path_set_builder : t -> SourcePath.t -> SourcePath.t
      val source_path_init : t -> SourcePath.t
      val has_relative_source_path : t -> bool
      val relative_source_path_get : t -> string
      val relative_source_path_set : t -> string -> unit
      val has_info_filename : t -> bool
      val info_filename_get : t -> string
      val info_filename_set : t -> string -> unit
      val has_python_version : t -> bool
      val python_version_get : t -> string
      val python_version_set : t -> string -> unit
      val has_platform : t -> bool
      val platform_get : t -> string
      val platform_set : t -> string -> unit
      val is_test_get : t -> bool
      val is_test_set : t -> bool -> unit
      val is_interface_get : t -> bool
      val is_interface_set : t -> bool -> unit
      val is_init_get : t -> bool
      val is_init_set : t -> bool -> unit
      val is_internal_get : t -> bool
      val is_internal_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ProjectFile : sig
      type struct_t = [`ProjectFile_fb60e377cc6c9cae]
      type t = struct_t builder_t
      val has_modules : t -> bool
      val modules_get : t -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t
      val modules_get_list : t -> PysaProjectModule.t list
      val modules_get_array : t -> PysaProjectModule.t array
      val modules_set : t -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t
      val modules_set_list : t -> PysaProjectModule.t list -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t
      val modules_set_array : t -> PysaProjectModule.t array -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t
      val modules_init : t -> int -> (rw, PysaProjectModule.t, array_t) Capnp.Array.t
      val has_builtin_module_ids : t -> bool
      val builtin_module_ids_get : t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val builtin_module_ids_get_list : t -> Stdint.Uint32.t list
      val builtin_module_ids_get_array : t -> Stdint.Uint32.t array
      val builtin_module_ids_set : t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val builtin_module_ids_set_list : t -> Stdint.Uint32.t list -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val builtin_module_ids_set_array : t -> Stdint.Uint32.t array -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val builtin_module_ids_init : t -> int -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val has_object_class_refs : t -> bool
      val object_class_refs_get : t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val object_class_refs_get_list : t -> ClassRef.t list
      val object_class_refs_get_array : t -> ClassRef.t array
      val object_class_refs_set : t -> (rw, ClassRef.t, array_t) Capnp.Array.t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val object_class_refs_set_list : t -> ClassRef.t list -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val object_class_refs_set_array : t -> ClassRef.t array -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val object_class_refs_init : t -> int -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val has_dict_class_refs : t -> bool
      val dict_class_refs_get : t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val dict_class_refs_get_list : t -> ClassRef.t list
      val dict_class_refs_get_array : t -> ClassRef.t array
      val dict_class_refs_set : t -> (rw, ClassRef.t, array_t) Capnp.Array.t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val dict_class_refs_set_list : t -> ClassRef.t list -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val dict_class_refs_set_array : t -> ClassRef.t array -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val dict_class_refs_init : t -> int -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val has_typing_module_ids : t -> bool
      val typing_module_ids_get : t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val typing_module_ids_get_list : t -> Stdint.Uint32.t list
      val typing_module_ids_get_array : t -> Stdint.Uint32.t array
      val typing_module_ids_set : t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val typing_module_ids_set_list : t -> Stdint.Uint32.t list -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val typing_module_ids_set_array : t -> Stdint.Uint32.t array -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val typing_module_ids_init : t -> int -> (rw, Stdint.Uint32.t, array_t) Capnp.Array.t
      val has_typing_mapping_class_refs : t -> bool
      val typing_mapping_class_refs_get : t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val typing_mapping_class_refs_get_list : t -> ClassRef.t list
      val typing_mapping_class_refs_get_array : t -> ClassRef.t array
      val typing_mapping_class_refs_set : t -> (rw, ClassRef.t, array_t) Capnp.Array.t -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val typing_mapping_class_refs_set_list : t -> ClassRef.t list -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val typing_mapping_class_refs_set_array : t -> ClassRef.t array -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val typing_mapping_class_refs_init : t -> int -> (rw, ClassRef.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ModuleDefinitions : sig
      type struct_t = [`ModuleDefinitions_c7923485cc475b88]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val module_name_set : t -> string -> unit
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_set_reader : t -> SourcePath.struct_t reader_t -> SourcePath.t
      val source_path_set_builder : t -> SourcePath.t -> SourcePath.t
      val source_path_init : t -> SourcePath.t
      val has_function_definitions : t -> bool
      val function_definitions_get : t -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t
      val function_definitions_get_list : t -> FunctionDefinition.t list
      val function_definitions_get_array : t -> FunctionDefinition.t array
      val function_definitions_set : t -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t
      val function_definitions_set_list : t -> FunctionDefinition.t list -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t
      val function_definitions_set_array : t -> FunctionDefinition.t array -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t
      val function_definitions_init : t -> int -> (rw, FunctionDefinition.t, array_t) Capnp.Array.t
      val has_class_definitions : t -> bool
      val class_definitions_get : t -> (rw, ClassDefinition.t, array_t) Capnp.Array.t
      val class_definitions_get_list : t -> ClassDefinition.t list
      val class_definitions_get_array : t -> ClassDefinition.t array
      val class_definitions_set : t -> (rw, ClassDefinition.t, array_t) Capnp.Array.t -> (rw, ClassDefinition.t, array_t) Capnp.Array.t
      val class_definitions_set_list : t -> ClassDefinition.t list -> (rw, ClassDefinition.t, array_t) Capnp.Array.t
      val class_definitions_set_array : t -> ClassDefinition.t array -> (rw, ClassDefinition.t, array_t) Capnp.Array.t
      val class_definitions_init : t -> int -> (rw, ClassDefinition.t, array_t) Capnp.Array.t
      val has_global_variables : t -> bool
      val global_variables_get : t -> (rw, GlobalVariable.t, array_t) Capnp.Array.t
      val global_variables_get_list : t -> GlobalVariable.t list
      val global_variables_get_array : t -> GlobalVariable.t array
      val global_variables_set : t -> (rw, GlobalVariable.t, array_t) Capnp.Array.t -> (rw, GlobalVariable.t, array_t) Capnp.Array.t
      val global_variables_set_list : t -> GlobalVariable.t list -> (rw, GlobalVariable.t, array_t) Capnp.Array.t
      val global_variables_set_array : t -> GlobalVariable.t array -> (rw, GlobalVariable.t, array_t) Capnp.Array.t
      val global_variables_init : t -> int -> (rw, GlobalVariable.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module LocationTypeIdEntry : sig
      type struct_t = [`LocationTypeIdEntry_ec97908cde450b07]
      type t = struct_t builder_t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val location_init : t -> PysaLocation.t
      val type_id_get : t -> Stdint.Uint32.t
      val type_id_get_int_exn : t -> int
      val type_id_set : t -> Stdint.Uint32.t -> unit
      val type_id_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module FunctionTypeOfExpressions : sig
      type struct_t = [`FunctionTypeOfExpressions_e454c787c4578ae9]
      type t = struct_t builder_t
      val has_function_id : t -> bool
      val function_id_get : t -> string
      val function_id_set : t -> string -> unit
      val has_types : t -> bool
      val types_get : t -> (rw, PysaType.t, array_t) Capnp.Array.t
      val types_get_list : t -> PysaType.t list
      val types_get_array : t -> PysaType.t array
      val types_set : t -> (rw, PysaType.t, array_t) Capnp.Array.t -> (rw, PysaType.t, array_t) Capnp.Array.t
      val types_set_list : t -> PysaType.t list -> (rw, PysaType.t, array_t) Capnp.Array.t
      val types_set_array : t -> PysaType.t array -> (rw, PysaType.t, array_t) Capnp.Array.t
      val types_init : t -> int -> (rw, PysaType.t, array_t) Capnp.Array.t
      val has_locations : t -> bool
      val locations_get : t -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val locations_get_list : t -> LocationTypeIdEntry.t list
      val locations_get_array : t -> LocationTypeIdEntry.t array
      val locations_set : t -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val locations_set_list : t -> LocationTypeIdEntry.t list -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val locations_set_array : t -> LocationTypeIdEntry.t array -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val locations_init : t -> int -> (rw, LocationTypeIdEntry.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ModuleTypeOfExpressions : sig
      type struct_t = [`ModuleTypeOfExpressions_cfca7d3ae9f723a7]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val module_name_set : t -> string -> unit
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_set_reader : t -> SourcePath.struct_t reader_t -> SourcePath.t
      val source_path_set_builder : t -> SourcePath.t -> SourcePath.t
      val source_path_init : t -> SourcePath.t
      val has_functions : t -> bool
      val functions_get : t -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val functions_get_list : t -> FunctionTypeOfExpressions.t list
      val functions_get_array : t -> FunctionTypeOfExpressions.t array
      val functions_set : t -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val functions_set_list : t -> FunctionTypeOfExpressions.t list -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val functions_set_array : t -> FunctionTypeOfExpressions.t array -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val functions_init : t -> int -> (rw, FunctionTypeOfExpressions.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ModuleCallGraphs : sig
      type struct_t = [`ModuleCallGraphs_c11251973766f6c6]
      type t = struct_t builder_t
      val module_id_get : t -> Stdint.Uint32.t
      val module_id_get_int_exn : t -> int
      val module_id_set : t -> Stdint.Uint32.t -> unit
      val module_id_set_int_exn : t -> int -> unit
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val module_name_set : t -> string -> unit
      val has_source_path : t -> bool
      val source_path_get : t -> SourcePath.t
      val source_path_set_reader : t -> SourcePath.struct_t reader_t -> SourcePath.t
      val source_path_set_builder : t -> SourcePath.t -> SourcePath.t
      val source_path_init : t -> SourcePath.t
      val has_call_graphs : t -> bool
      val call_graphs_get : t -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t
      val call_graphs_get_list : t -> FunctionCallGraph.t list
      val call_graphs_get_array : t -> FunctionCallGraph.t array
      val call_graphs_set : t -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t
      val call_graphs_set_list : t -> FunctionCallGraph.t list -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t
      val call_graphs_set_array : t -> FunctionCallGraph.t array -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t
      val call_graphs_init : t -> int -> (rw, FunctionCallGraph.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module TypeError : sig
      type struct_t = [`TypeError_cf833e7f4687d6b7]
      type t = struct_t builder_t
      val has_module_name : t -> bool
      val module_name_get : t -> string
      val module_name_set : t -> string -> unit
      val has_module_path : t -> bool
      val module_path_get : t -> SourcePath.t
      val module_path_set_reader : t -> SourcePath.struct_t reader_t -> SourcePath.t
      val module_path_set_builder : t -> SourcePath.t -> SourcePath.t
      val module_path_init : t -> SourcePath.t
      val has_location : t -> bool
      val location_get : t -> PysaLocation.t
      val location_set_reader : t -> PysaLocation.struct_t reader_t -> PysaLocation.t
      val location_set_builder : t -> PysaLocation.t -> PysaLocation.t
      val location_init : t -> PysaLocation.t
      val has_kind : t -> bool
      val kind_get : t -> string
      val kind_set : t -> string -> unit
      val has_message : t -> bool
      val message_get : t -> string
      val message_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module TypeErrors : sig
      type struct_t = [`TypeErrors_c2e0d691270c45f9]
      type t = struct_t builder_t
      val has_errors : t -> bool
      val errors_get : t -> (rw, TypeError.t, array_t) Capnp.Array.t
      val errors_get_list : t -> TypeError.t list
      val errors_get_array : t -> TypeError.t array
      val errors_set : t -> (rw, TypeError.t, array_t) Capnp.Array.t -> (rw, TypeError.t, array_t) Capnp.Array.t
      val errors_set_list : t -> TypeError.t list -> (rw, TypeError.t, array_t) Capnp.Array.t
      val errors_set_array : t -> TypeError.t array -> (rw, TypeError.t, array_t) Capnp.Array.t
      val errors_init : t -> int -> (rw, TypeError.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
  end

  module Service : sig
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
