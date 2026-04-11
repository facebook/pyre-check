(* @generated
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

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make(MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module ReturnShimArgumentMapping_16744047642608040181 = struct
    type t =
      | ReturnExpression
      | ReturnExpressionElement
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> ReturnExpression
      | 1 -> ReturnExpressionElement
      | v -> Undefined v
    let encode_safe enum = match enum with
      | ReturnExpression -> 0
      | ReturnExpressionElement -> 1
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | ReturnExpression -> 0
      | ReturnExpressionElement -> 1
      | Undefined x -> x
  end
  module UnresolvedReason_16256959910610618162 = struct
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
    let decode u16 = match u16 with
      | 0 -> LambdaArgument
      | 1 -> UnexpectedPyreflyTarget
      | 2 -> EmptyPyreflyCallTarget
      | 3 -> UnknownClassField
      | 4 -> ClassFieldOnlyExistInObject
      | 5 -> UnsupportedFunctionTarget
      | 6 -> UnexpectedDefiningClass
      | 7 -> UnexpectedInitMethod
      | 8 -> UnexpectedNewMethod
      | 9 -> UnexpectedCalleeExpression
      | 10 -> UnresolvedMagicDunderAttr
      | 11 -> UnresolvedMagicDunderAttrDueToNoBase
      | 12 -> UnresolvedMagicDunderAttrDueToNoAttribute
      | 13 -> Mixed
      | v -> Undefined v
    let encode_safe enum = match enum with
      | LambdaArgument -> 0
      | UnexpectedPyreflyTarget -> 1
      | EmptyPyreflyCallTarget -> 2
      | UnknownClassField -> 3
      | ClassFieldOnlyExistInObject -> 4
      | UnsupportedFunctionTarget -> 5
      | UnexpectedDefiningClass -> 6
      | UnexpectedInitMethod -> 7
      | UnexpectedNewMethod -> 8
      | UnexpectedCalleeExpression -> 9
      | UnresolvedMagicDunderAttr -> 10
      | UnresolvedMagicDunderAttrDueToNoBase -> 11
      | UnresolvedMagicDunderAttrDueToNoAttribute -> 12
      | Mixed -> 13
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | LambdaArgument -> 0
      | UnexpectedPyreflyTarget -> 1
      | EmptyPyreflyCallTarget -> 2
      | UnknownClassField -> 3
      | ClassFieldOnlyExistInObject -> 4
      | UnsupportedFunctionTarget -> 5
      | UnexpectedDefiningClass -> 6
      | UnexpectedInitMethod -> 7
      | UnexpectedNewMethod -> 8
      | UnexpectedCalleeExpression -> 9
      | UnresolvedMagicDunderAttr -> 10
      | UnresolvedMagicDunderAttrDueToNoBase -> 11
      | UnresolvedMagicDunderAttrDueToNoAttribute -> 12
      | Mixed -> 13
      | Undefined x -> x
  end
  module PysaClassFieldDeclaration_17132881886155188463 = struct
    type t =
      | None
      | DeclaredByAnnotation
      | DeclaredWithoutAnnotation
      | AssignedInBody
      | DefinedWithoutAssign
      | DefinedInMethod
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> None
      | 1 -> DeclaredByAnnotation
      | 2 -> DeclaredWithoutAnnotation
      | 3 -> AssignedInBody
      | 4 -> DefinedWithoutAssign
      | 5 -> DefinedInMethod
      | v -> Undefined v
    let encode_safe enum = match enum with
      | None -> 0
      | DeclaredByAnnotation -> 1
      | DeclaredWithoutAnnotation -> 2
      | AssignedInBody -> 3
      | DefinedWithoutAssign -> 4
      | DefinedInMethod -> 5
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | None -> 0
      | DeclaredByAnnotation -> 1
      | DeclaredWithoutAnnotation -> 2
      | AssignedInBody -> 3
      | DefinedWithoutAssign -> 4
      | DefinedInMethod -> 5
      | Undefined x -> x
  end
  module ImplicitReceiver_16434162114200234704 = struct
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> TrueWithClassReceiver
      | 1 -> TrueWithObjectReceiver
      | 2 -> False
      | v -> Undefined v
    let encode_safe enum = match enum with
      | TrueWithClassReceiver -> 0
      | TrueWithObjectReceiver -> 1
      | False -> 2
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | TrueWithClassReceiver -> 0
      | TrueWithObjectReceiver -> 1
      | False -> 2
      | Undefined x -> x
  end
  module TypeModifier_17523487963418047075 = struct
    type t =
      | Optional
      | Coroutine
      | Awaitable
      | TypeVariableBound
      | TypeVariableConstraint
      | Type
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> Optional
      | 1 -> Coroutine
      | 2 -> Awaitable
      | 3 -> TypeVariableBound
      | 4 -> TypeVariableConstraint
      | 5 -> Type
      | v -> Undefined v
    let encode_safe enum = match enum with
      | Optional -> 0
      | Coroutine -> 1
      | Awaitable -> 2
      | TypeVariableBound -> 3
      | TypeVariableConstraint -> 4
      | Type -> 5
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | Optional -> 0
      | Coroutine -> 1
      | Awaitable -> 2
      | TypeVariableBound -> 3
      | TypeVariableConstraint -> 4
      | Type -> 5
      | Undefined x -> x
  end
  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module SourcePath = struct
      type struct_t = [`SourcePath_c9789635145f607d]
      type t = struct_t reader_t
      let has_file_system x =
        RA_.has_field x 0
      let file_system_get x =
        RA_.get_text ~default:"" x 0
      let has_namespace x =
        RA_.has_field x 0
      let namespace_get x =
        RA_.get_text ~default:"" x 0
      let has_memory x =
        RA_.has_field x 0
      let memory_get x =
        RA_.get_text ~default:"" x 0
      let has_bundled_typeshed x =
        RA_.has_field x 0
      let bundled_typeshed_get x =
        RA_.get_text ~default:"" x 0
      let has_bundled_typeshed_third_party x =
        RA_.has_field x 0
      let bundled_typeshed_third_party_get x =
        RA_.get_text ~default:"" x 0
      let has_bundled_third_party x =
        RA_.has_field x 0
      let bundled_third_party_get x =
        RA_.get_text ~default:"" x 0
      type unnamed_union_t =
        | FileSystem of string
        | Namespace of string
        | Memory of string
        | BundledTypeshed of string
        | BundledTypeshedThirdParty of string
        | BundledThirdParty of string
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> FileSystem (file_system_get x)
        | 1 -> Namespace (namespace_get x)
        | 2 -> Memory (memory_get x)
        | 3 -> BundledTypeshed (bundled_typeshed_get x)
        | 4 -> BundledTypeshedThirdParty (bundled_typeshed_third_party_get x)
        | 5 -> BundledThirdParty (bundled_third_party_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PysaLocation = struct
      type struct_t = [`PysaLocation_c309c969f3e58a60]
      type t = struct_t reader_t
      let line_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let line_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (line_get x)
      let col_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let col_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (col_get x)
      let end_line_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 8
      let end_line_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (end_line_get x)
      let end_col_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 12
      let end_col_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (end_col_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ClassRef = struct
      type struct_t = [`ClassRef_de5e9ff6d0dd25c7]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let class_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let class_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionRef = struct
      type struct_t = [`FunctionRef_80115bb31006b8bb]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_function_id x =
        RA_.has_field x 0
      let function_id_get x =
        RA_.get_text ~default:"" x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module GlobalVariableRef = struct
      type struct_t = [`GlobalVariableRef_f70734c08fbb8800]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module TypeModifier = struct
      type t = TypeModifier_17523487963418047075.t =
        | Optional
        | Coroutine
        | Awaitable
        | TypeVariableBound
        | TypeVariableConstraint
        | Type
        | Undefined of int
    end
    module ClassWithModifiers = struct
      type struct_t = [`ClassWithModifiers_e15f82ade37d4236]
      type t = struct_t reader_t
      let has_class x =
        RA_.has_field x 0
      let class_get x =
        RA_.get_struct x 0
      let class_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_modifiers x =
        RA_.has_field x 1
      let modifiers_get x =
        let slice_decoder slice =
          TypeModifier_17523487963418047075.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        RA_.get_list (RA_.ListDecoders.Bytes2 slice_decoder) x 1
      let modifiers_get_list x =
        Capnp.Array.to_list (modifiers_get x)
      let modifiers_get_array x =
        Capnp.Array.to_array (modifiers_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ClassNamesFromType = struct
      type struct_t = [`ClassNamesFromType_a0584aac2e97e83c]
      type t = struct_t reader_t
      let has_classes x =
        RA_.has_field x 0
      let classes_get x = 
        RA_.get_struct_list x 0
      let classes_get_list x =
        Capnp.Array.to_list (classes_get x)
      let classes_get_array x =
        Capnp.Array.to_array (classes_get x)
      let is_exhaustive_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ScalarTypeProperties = struct
      type struct_t = [`ScalarTypeProperties_d79ace8daba1babf]
      type t = struct_t reader_t
      let is_bool_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_int_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_float_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_enum_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PysaType = struct
      type struct_t = [`PysaType_d28d95e8ed282312]
      type t = struct_t reader_t
      let has_string x =
        RA_.has_field x 0
      let string_get x =
        RA_.get_text ~default:"" x 0
      let has_scalar_type_properties x =
        RA_.has_field x 1
      let scalar_type_properties_get x =
        RA_.get_struct x 1
      let scalar_type_properties_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_class_names x =
        RA_.has_field x 2
      let class_names_get x =
        RA_.get_struct x 2
      let class_names_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ScopeParent = struct
      type struct_t = [`ScopeParent_de2463e0e757468e]
      type t = struct_t reader_t
      let function_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let function_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (function_get x)
      let class_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let class_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_get x)
      let top_level_get x = ()
      type unnamed_union_t =
        | Function of Stdint.Uint32.t
        | Class of Stdint.Uint32.t
        | TopLevel
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 4 with
        | 0 -> Function (function_get x)
        | 1 -> Class (class_get x)
        | 2 -> TopLevel
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionParameter = struct
      type struct_t = [`FunctionParameter_a1b2fce98392735b]
      type t = struct_t reader_t
      module PosOnlyParam = struct
        type struct_t = [`PosOnlyParam_f44b7078e9979a56]
        type t = struct_t reader_t
        let has_name x =
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let has_annotation x =
          RA_.has_field x 1
        let annotation_get x =
          RA_.get_struct x 1
        let annotation_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 1
        let required_get x =
          RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module PosParam = struct
        type struct_t = [`PosParam_cb05f6c652a9cccf]
        type t = struct_t reader_t
        let has_name x =
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let has_annotation x =
          RA_.has_field x 1
        let annotation_get x =
          RA_.get_struct x 1
        let annotation_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 1
        let required_get x =
          RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module VarArgParam = struct
        type struct_t = [`VarArgParam_987ff4fc98295c57]
        type t = struct_t reader_t
        let has_name x =
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let has_annotation x =
          RA_.has_field x 1
        let annotation_get x =
          RA_.get_struct x 1
        let annotation_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 1
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module KwOnlyParam = struct
        type struct_t = [`KwOnlyParam_9ceff6d824c01469]
        type t = struct_t reader_t
        let has_name x =
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let has_annotation x =
          RA_.has_field x 1
        let annotation_get x =
          RA_.get_struct x 1
        let annotation_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 1
        let required_get x =
          RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module KwargsParam = struct
        type struct_t = [`KwargsParam_914731ff31eab123]
        type t = struct_t reader_t
        let has_name x =
          RA_.has_field x 0
        let name_get x =
          RA_.get_text ~default:"" x 0
        let has_annotation x =
          RA_.has_field x 1
        let annotation_get x =
          RA_.get_struct x 1
        let annotation_get_pipelined x =
          MessageWrapper.Untyped.struct_field x 1
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let has_pos_only x =
        RA_.has_field x 0
      let pos_only_get x =
        RA_.get_struct x 0
      let pos_only_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_pos x =
        RA_.has_field x 0
      let pos_get x =
        RA_.get_struct x 0
      let pos_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_var_arg x =
        RA_.has_field x 0
      let var_arg_get x =
        RA_.get_struct x 0
      let var_arg_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_kw_only x =
        RA_.has_field x 0
      let kw_only_get x =
        RA_.get_struct x 0
      let kw_only_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_kwargs x =
        RA_.has_field x 0
      let kwargs_get x =
        RA_.get_struct x 0
      let kwargs_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | PosOnly of [`PosOnlyParam_f44b7078e9979a56] reader_t
        | Pos of [`PosParam_cb05f6c652a9cccf] reader_t
        | VarArg of [`VarArgParam_987ff4fc98295c57] reader_t
        | KwOnly of [`KwOnlyParam_9ceff6d824c01469] reader_t
        | Kwargs of [`KwargsParam_914731ff31eab123] reader_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> PosOnly (pos_only_get x)
        | 1 -> Pos (pos_get x)
        | 2 -> VarArg (var_arg_get x)
        | 3 -> KwOnly (kw_only_get x)
        | 4 -> Kwargs (kwargs_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionParameters = struct
      type struct_t = [`FunctionParameters_a7c6a3610ea670c8]
      type t = struct_t reader_t
      let has_list x =
        RA_.has_field x 0
      let list_get x = 
        RA_.get_struct_list x 0
      let list_get_list x =
        Capnp.Array.to_list (list_get x)
      let list_get_array x =
        Capnp.Array.to_array (list_get x)
      let ellipsis_get x = ()
      let param_spec_get x = ()
      type unnamed_union_t =
        | List of (ro, FunctionParameter.t, array_t) Capnp.Array.t
        | Ellipsis
        | ParamSpec
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> List (list_get x)
        | 1 -> Ellipsis
        | 2 -> ParamSpec
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionSignature = struct
      type struct_t = [`FunctionSignature_be11da038b7eb7d9]
      type t = struct_t reader_t
      let has_parameters x =
        RA_.has_field x 0
      let parameters_get x =
        RA_.get_struct x 0
      let parameters_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_return_annotation x =
        RA_.has_field x 1
      let return_annotation_get x =
        RA_.get_struct x 1
      let return_annotation_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionBaseDefinition = struct
      type struct_t = [`FunctionBaseDefinition_876cdc721c603758]
      type t = struct_t reader_t
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let has_parent x =
        RA_.has_field x 1
      let parent_get x =
        RA_.get_struct x 1
      let parent_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let is_overload_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_staticmethod_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_classmethod_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_property_getter_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let is_property_setter_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4
      let is_stub_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5
      let is_def_statement_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6
      let has_defining_class x =
        RA_.has_field x 2
      let defining_class_get x =
        RA_.get_struct x 2
      let defining_class_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module CapturedVariableRef = struct
      type struct_t = [`CapturedVariableRef_94805e8830d6bb4e]
      type t = struct_t reader_t
      let has_outer_function x =
        RA_.has_field x 0
      let outer_function_get x =
        RA_.get_struct x 0
      let outer_function_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_name x =
        RA_.has_field x 1
      let name_get x =
        RA_.get_text ~default:"" x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Target = struct
      type struct_t = [`Target_ff8266cc34db19d3]
      type t = struct_t reader_t
      let has_function x =
        RA_.has_field x 0
      let function_get x =
        RA_.get_struct x 0
      let function_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_overrides x =
        RA_.has_field x 0
      let overrides_get x =
        RA_.get_struct x 0
      let overrides_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let format_string_get x = ()
      type unnamed_union_t =
        | Function of FunctionRef.t
        | Overrides of FunctionRef.t
        | FormatString
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Function (function_get x)
        | 1 -> Overrides (overrides_get x)
        | 2 -> FormatString
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ImplicitReceiver = struct
      type t = ImplicitReceiver_16434162114200234704.t =
        | TrueWithClassReceiver
        | TrueWithObjectReceiver
        | False
        | Undefined of int
    end
    module PysaCallTarget = struct
      type struct_t = [`PysaCallTarget_cf1542da5a3f29e1]
      type t = struct_t reader_t
      let has_target x =
        RA_.has_field x 0
      let target_get x =
        RA_.get_struct x 0
      let target_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let implicit_receiver_get x =
        let discr = RA_.get_uint16 ~default:0 x 0 in
        ImplicitReceiver_16434162114200234704.decode discr
      let implicit_dunder_call_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let has_receiver_class x =
        RA_.has_field x 1
      let receiver_class_get x =
        RA_.get_struct x 1
      let receiver_class_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let is_class_method_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1
      let is_static_method_get x =
        RA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2
      let has_return_type x =
        RA_.has_field x 2
      let return_type_get x =
        RA_.get_struct x 2
      let return_type_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module DecoratorCallee = struct
      type struct_t = [`DecoratorCallee_bc083e8c7ab91017]
      type t = struct_t reader_t
      let has_location x =
        RA_.has_field x 0
      let location_get x =
        RA_.get_struct x 0
      let location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_targets x =
        RA_.has_field x 1
      let targets_get x = 
        RA_.get_struct_list x 1
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionDefinition = struct
      type struct_t = [`FunctionDefinition_ac2bccd871436411]
      type t = struct_t reader_t
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let has_define_name_location x =
        RA_.has_field x 8
      let define_name_location_get x =
        RA_.get_struct x 8
      let define_name_location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 8
      let has_parent x =
        RA_.has_field x 1
      let parent_get x =
        RA_.get_struct x 1
      let parent_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let is_overload_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_staticmethod_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_classmethod_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_property_getter_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let is_property_setter_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4
      let is_stub_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5
      let is_def_statement_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6
      let has_defining_class x =
        RA_.has_field x 2
      let defining_class_get x =
        RA_.get_struct x 2
      let defining_class_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let has_function_id x =
        RA_.has_field x 3
      let function_id_get x =
        RA_.get_text ~default:"" x 3
      let has_undecorated_signatures x =
        RA_.has_field x 4
      let undecorated_signatures_get x = 
        RA_.get_struct_list x 4
      let undecorated_signatures_get_list x =
        Capnp.Array.to_list (undecorated_signatures_get x)
      let undecorated_signatures_get_array x =
        Capnp.Array.to_array (undecorated_signatures_get x)
      let has_captured_variables x =
        RA_.has_field x 5
      let captured_variables_get x = 
        RA_.get_struct_list x 5
      let captured_variables_get_list x =
        Capnp.Array.to_list (captured_variables_get x)
      let captured_variables_get_array x =
        Capnp.Array.to_array (captured_variables_get x)
      let has_decorator_callees x =
        RA_.has_field x 6
      let decorator_callees_get x = 
        RA_.get_struct_list x 6
      let decorator_callees_get_list x =
        Capnp.Array.to_list (decorator_callees_get x)
      let decorator_callees_get_array x =
        Capnp.Array.to_array (decorator_callees_get x)
      let has_overridden_base_method x =
        RA_.has_field x 7
      let overridden_base_method_get x =
        RA_.get_struct x 7
      let overridden_base_method_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 7
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PysaClassFieldDeclaration = struct
      type t = PysaClassFieldDeclaration_17132881886155188463.t =
        | None
        | DeclaredByAnnotation
        | DeclaredWithoutAnnotation
        | AssignedInBody
        | DefinedWithoutAssign
        | DefinedInMethod
        | Undefined of int
    end
    module PysaClassField = struct
      type struct_t = [`PysaClassField_f94ec0e6aa9a5e25]
      type t = struct_t reader_t
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let has_type x =
        RA_.has_field x 1
      let type_get x =
        RA_.get_struct x 1
      let type_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_explicit_annotation x =
        RA_.has_field x 2
      let explicit_annotation_get x =
        RA_.get_text ~default:"" x 2
      let has_location x =
        RA_.has_field x 3
      let location_get x =
        RA_.get_struct x 3
      let location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 3
      let declaration_kind_get x =
        let discr = RA_.get_uint16 ~default:0 x 0 in
        PysaClassFieldDeclaration_17132881886155188463.decode discr
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PysaClassMro = struct
      type struct_t = [`PysaClassMro_addaafc52d26dffb]
      type t = struct_t reader_t
      let has_resolved x =
        RA_.has_field x 0
      let resolved_get x = 
        RA_.get_struct_list x 0
      let resolved_get_list x =
        Capnp.Array.to_list (resolved_get x)
      let resolved_get_array x =
        Capnp.Array.to_array (resolved_get x)
      let cyclic_get x = ()
      type unnamed_union_t =
        | Resolved of (ro, ClassRef.t, array_t) Capnp.Array.t
        | Cyclic
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Resolved (resolved_get x)
        | 1 -> Cyclic
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ClassDefinition = struct
      type struct_t = [`ClassDefinition_f802b9f88052bf19]
      type t = struct_t reader_t
      let class_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let class_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_id_get x)
      let has_name x =
        RA_.has_field x 1
      let name_get x =
        RA_.get_text ~default:"" x 1
      let has_name_location x =
        RA_.has_field x 0
      let name_location_get x =
        RA_.get_struct x 0
      let name_location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_bases x =
        RA_.has_field x 2
      let bases_get x = 
        RA_.get_struct_list x 2
      let bases_get_list x =
        Capnp.Array.to_list (bases_get x)
      let bases_get_array x =
        Capnp.Array.to_array (bases_get x)
      let has_mro x =
        RA_.has_field x 3
      let mro_get x =
        RA_.get_struct x 3
      let mro_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 3
      let has_parent x =
        RA_.has_field x 4
      let parent_get x =
        RA_.get_struct x 4
      let parent_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 4
      let is_synthesized_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let is_dataclass_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1
      let is_named_tuple_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2
      let is_typed_dict_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3
      let has_fields x =
        RA_.has_field x 5
      let fields_get x = 
        RA_.get_struct_list x 5
      let fields_get_list x =
        Capnp.Array.to_list (fields_get x)
      let fields_get_array x =
        Capnp.Array.to_array (fields_get x)
      let has_decorator_callees x =
        RA_.has_field x 6
      let decorator_callees_get x = 
        RA_.get_struct_list x 6
      let decorator_callees_get_list x =
        Capnp.Array.to_list (decorator_callees_get x)
      let decorator_callees_get_array x =
        Capnp.Array.to_array (decorator_callees_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module GlobalVariable = struct
      type struct_t = [`GlobalVariable_d7ecbd0f4024046b]
      type t = struct_t reader_t
      let has_name x =
        RA_.has_field x 0
      let name_get x =
        RA_.get_text ~default:"" x 0
      let has_type x =
        RA_.has_field x 1
      let type_get x =
        RA_.get_struct x 1
      let type_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_location x =
        RA_.has_field x 2
      let location_get x =
        RA_.get_struct x 2
      let location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module UnresolvedReason = struct
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
    module Unresolved = struct
      type struct_t = [`Unresolved_f5be7abed98173b3]
      type t = struct_t reader_t
      let false_get x = ()
      let true_get x =
        let discr = RA_.get_uint16 ~default:0 x 2 in
        UnresolvedReason_16256959910610618162.decode discr
      type unnamed_union_t =
        | False
        | True of UnresolvedReason.t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> False
        | 1 -> True (true_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module HigherOrderParameter = struct
      type struct_t = [`HigherOrderParameter_9f08f2a4f21fb93d]
      type t = struct_t reader_t
      let index_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let index_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (index_get x)
      let has_call_targets x =
        RA_.has_field x 0
      let call_targets_get x = 
        RA_.get_struct_list x 0
      let call_targets_get_list x =
        Capnp.Array.to_list (call_targets_get x)
      let call_targets_get_array x =
        Capnp.Array.to_array (call_targets_get x)
      let has_unresolved x =
        RA_.has_field x 1
      let unresolved_get x =
        RA_.get_struct x 1
      let unresolved_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module CallCallees = struct
      type struct_t = [`CallCallees_fa97c29dc67a9655]
      type t = struct_t reader_t
      let has_call_targets x =
        RA_.has_field x 0
      let call_targets_get x = 
        RA_.get_struct_list x 0
      let call_targets_get_list x =
        Capnp.Array.to_list (call_targets_get x)
      let call_targets_get_array x =
        Capnp.Array.to_array (call_targets_get x)
      let has_init_targets x =
        RA_.has_field x 1
      let init_targets_get x = 
        RA_.get_struct_list x 1
      let init_targets_get_list x =
        Capnp.Array.to_list (init_targets_get x)
      let init_targets_get_array x =
        Capnp.Array.to_array (init_targets_get x)
      let has_new_targets x =
        RA_.has_field x 2
      let new_targets_get x = 
        RA_.get_struct_list x 2
      let new_targets_get_list x =
        Capnp.Array.to_list (new_targets_get x)
      let new_targets_get_array x =
        Capnp.Array.to_array (new_targets_get x)
      let has_higher_order_parameters x =
        RA_.has_field x 3
      let higher_order_parameters_get x = 
        RA_.get_struct_list x 3
      let higher_order_parameters_get_list x =
        Capnp.Array.to_list (higher_order_parameters_get x)
      let higher_order_parameters_get_array x =
        Capnp.Array.to_array (higher_order_parameters_get x)
      let has_unresolved x =
        RA_.has_field x 4
      let unresolved_get x =
        RA_.get_struct x 4
      let unresolved_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 4
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module AttributeAccessCallees = struct
      type struct_t = [`AttributeAccessCallees_da03cbf714d87201]
      type t = struct_t reader_t
      let has_if_called x =
        RA_.has_field x 0
      let if_called_get x =
        RA_.get_struct x 0
      let if_called_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_property_setters x =
        RA_.has_field x 1
      let property_setters_get x = 
        RA_.get_struct_list x 1
      let property_setters_get_list x =
        Capnp.Array.to_list (property_setters_get x)
      let property_setters_get_array x =
        Capnp.Array.to_array (property_setters_get x)
      let has_property_getters x =
        RA_.has_field x 2
      let property_getters_get x = 
        RA_.get_struct_list x 2
      let property_getters_get_list x =
        Capnp.Array.to_list (property_getters_get x)
      let property_getters_get_array x =
        Capnp.Array.to_array (property_getters_get x)
      let has_global_targets x =
        RA_.has_field x 3
      let global_targets_get x = 
        RA_.get_struct_list x 3
      let global_targets_get_list x =
        Capnp.Array.to_list (global_targets_get x)
      let global_targets_get_array x =
        Capnp.Array.to_array (global_targets_get x)
      let is_attribute_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module IdentifierCallees = struct
      type struct_t = [`IdentifierCallees_a5f0dc0068285f29]
      type t = struct_t reader_t
      let has_if_called x =
        RA_.has_field x 0
      let if_called_get x =
        RA_.get_struct x 0
      let if_called_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_global_targets x =
        RA_.has_field x 1
      let global_targets_get x = 
        RA_.get_struct_list x 1
      let global_targets_get_list x =
        Capnp.Array.to_list (global_targets_get x)
      let global_targets_get_array x =
        Capnp.Array.to_array (global_targets_get x)
      let has_captured_variables x =
        RA_.has_field x 2
      let captured_variables_get x = 
        RA_.get_struct_list x 2
      let captured_variables_get_list x =
        Capnp.Array.to_list (captured_variables_get x)
      let captured_variables_get_array x =
        Capnp.Array.to_array (captured_variables_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module DefineCallees = struct
      type struct_t = [`DefineCallees_b379211649ffdce7]
      type t = struct_t reader_t
      let has_define_targets x =
        RA_.has_field x 0
      let define_targets_get x = 
        RA_.get_struct_list x 0
      let define_targets_get_list x =
        Capnp.Array.to_list (define_targets_get x)
      let define_targets_get_array x =
        Capnp.Array.to_array (define_targets_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FormatStringArtificialCallees = struct
      type struct_t = [`FormatStringArtificialCallees_a05d8efd266e9543]
      type t = struct_t reader_t
      let has_targets x =
        RA_.has_field x 0
      let targets_get x = 
        RA_.get_struct_list x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FormatStringStringifyCallees = struct
      type struct_t = [`FormatStringStringifyCallees_c57214d24c99a6b3]
      type t = struct_t reader_t
      let has_targets x =
        RA_.has_field x 0
      let targets_get x = 
        RA_.get_struct_list x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let has_unresolved x =
        RA_.has_field x 1
      let unresolved_get x =
        RA_.get_struct x 1
      let unresolved_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ReturnShimArgumentMapping = struct
      type t = ReturnShimArgumentMapping_16744047642608040181.t =
        | ReturnExpression
        | ReturnExpressionElement
        | Undefined of int
    end
    module ReturnShimCallees = struct
      type struct_t = [`ReturnShimCallees_a830c3d4e1910d6d]
      type t = struct_t reader_t
      let has_targets x =
        RA_.has_field x 0
      let targets_get x = 
        RA_.get_struct_list x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let has_arguments x =
        RA_.has_field x 1
      let arguments_get x =
        let slice_decoder slice =
          ReturnShimArgumentMapping_16744047642608040181.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        RA_.get_list (RA_.ListDecoders.Bytes2 slice_decoder) x 1
      let arguments_get_list x =
        Capnp.Array.to_list (arguments_get x)
      let arguments_get_array x =
        Capnp.Array.to_array (arguments_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ExpressionCallees = struct
      type struct_t = [`ExpressionCallees_9c207ddf98bd8cdf]
      type t = struct_t reader_t
      let has_call x =
        RA_.has_field x 0
      let call_get x =
        RA_.get_struct x 0
      let call_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_identifier x =
        RA_.has_field x 0
      let identifier_get x =
        RA_.get_struct x 0
      let identifier_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_attribute_access x =
        RA_.has_field x 0
      let attribute_access_get x =
        RA_.get_struct x 0
      let attribute_access_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_define x =
        RA_.has_field x 0
      let define_get x =
        RA_.get_struct x 0
      let define_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_format_string_artificial x =
        RA_.has_field x 0
      let format_string_artificial_get x =
        RA_.get_struct x 0
      let format_string_artificial_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_format_string_stringify x =
        RA_.has_field x 0
      let format_string_stringify_get x =
        RA_.get_struct x 0
      let format_string_stringify_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_return x =
        RA_.has_field x 0
      let return_get x =
        RA_.get_struct x 0
      let return_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | Call of CallCallees.t
        | Identifier of IdentifierCallees.t
        | AttributeAccess of AttributeAccessCallees.t
        | Define of DefineCallees.t
        | FormatStringArtificial of FormatStringArtificialCallees.t
        | FormatStringStringify of FormatStringStringifyCallees.t
        | Return of ReturnShimCallees.t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Call (call_get x)
        | 1 -> Identifier (identifier_get x)
        | 2 -> AttributeAccess (attribute_access_get x)
        | 3 -> Define (define_get x)
        | 4 -> FormatStringArtificial (format_string_artificial_get x)
        | 5 -> FormatStringStringify (format_string_stringify_get x)
        | 6 -> Return (return_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module CallGraphEntry = struct
      type struct_t = [`CallGraphEntry_d918afd531f118f8]
      type t = struct_t reader_t
      let has_expression_id x =
        RA_.has_field x 0
      let expression_id_get x =
        RA_.get_text ~default:"" x 0
      let has_callees x =
        RA_.has_field x 1
      let callees_get x =
        RA_.get_struct x 1
      let callees_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionCallGraph = struct
      type struct_t = [`FunctionCallGraph_e54ecff47ce9d92a]
      type t = struct_t reader_t
      let has_function_id x =
        RA_.has_field x 0
      let function_id_get x =
        RA_.get_text ~default:"" x 0
      let has_entries x =
        RA_.has_field x 1
      let entries_get x = 
        RA_.get_struct_list x 1
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PysaProjectModule = struct
      type struct_t = [`PysaProjectModule_898e79c185b3c64b]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_module_name x =
        RA_.has_field x 0
      let module_name_get x =
        RA_.get_text ~default:"" x 0
      let has_source_path x =
        RA_.has_field x 1
      let source_path_get x =
        RA_.get_struct x 1
      let source_path_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_relative_source_path x =
        RA_.has_field x 2
      let relative_source_path_get x =
        RA_.get_text ~default:"" x 2
      let has_info_filename x =
        RA_.has_field x 3
      let info_filename_get x =
        RA_.get_text ~default:"" x 3
      let has_python_version x =
        RA_.has_field x 4
      let python_version_get x =
        RA_.get_text ~default:"" x 4
      let has_platform x =
        RA_.has_field x 5
      let platform_get x =
        RA_.get_text ~default:"" x 5
      let is_test_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let is_interface_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1
      let is_init_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2
      let is_internal_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ProjectFile = struct
      type struct_t = [`ProjectFile_fb60e377cc6c9cae]
      type t = struct_t reader_t
      let has_modules x =
        RA_.has_field x 0
      let modules_get x = 
        RA_.get_struct_list x 0
      let modules_get_list x =
        Capnp.Array.to_list (modules_get x)
      let modules_get_array x =
        Capnp.Array.to_array (modules_get x)
      let has_builtin_module_ids x =
        RA_.has_field x 1
      let builtin_module_ids_get x =
        RA_.get_uint32_list x 1
      let builtin_module_ids_get_list x =
        Capnp.Array.to_list (builtin_module_ids_get x)
      let builtin_module_ids_get_array x =
        Capnp.Array.to_array (builtin_module_ids_get x)
      let has_object_class_refs x =
        RA_.has_field x 2
      let object_class_refs_get x = 
        RA_.get_struct_list x 2
      let object_class_refs_get_list x =
        Capnp.Array.to_list (object_class_refs_get x)
      let object_class_refs_get_array x =
        Capnp.Array.to_array (object_class_refs_get x)
      let has_dict_class_refs x =
        RA_.has_field x 3
      let dict_class_refs_get x = 
        RA_.get_struct_list x 3
      let dict_class_refs_get_list x =
        Capnp.Array.to_list (dict_class_refs_get x)
      let dict_class_refs_get_array x =
        Capnp.Array.to_array (dict_class_refs_get x)
      let has_typing_module_ids x =
        RA_.has_field x 4
      let typing_module_ids_get x =
        RA_.get_uint32_list x 4
      let typing_module_ids_get_list x =
        Capnp.Array.to_list (typing_module_ids_get x)
      let typing_module_ids_get_array x =
        Capnp.Array.to_array (typing_module_ids_get x)
      let has_typing_mapping_class_refs x =
        RA_.has_field x 5
      let typing_mapping_class_refs_get x = 
        RA_.get_struct_list x 5
      let typing_mapping_class_refs_get_list x =
        Capnp.Array.to_list (typing_mapping_class_refs_get x)
      let typing_mapping_class_refs_get_array x =
        Capnp.Array.to_array (typing_mapping_class_refs_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ModuleDefinitions = struct
      type struct_t = [`ModuleDefinitions_c7923485cc475b88]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_module_name x =
        RA_.has_field x 0
      let module_name_get x =
        RA_.get_text ~default:"" x 0
      let has_source_path x =
        RA_.has_field x 1
      let source_path_get x =
        RA_.get_struct x 1
      let source_path_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_function_definitions x =
        RA_.has_field x 2
      let function_definitions_get x = 
        RA_.get_struct_list x 2
      let function_definitions_get_list x =
        Capnp.Array.to_list (function_definitions_get x)
      let function_definitions_get_array x =
        Capnp.Array.to_array (function_definitions_get x)
      let has_class_definitions x =
        RA_.has_field x 3
      let class_definitions_get x = 
        RA_.get_struct_list x 3
      let class_definitions_get_list x =
        Capnp.Array.to_list (class_definitions_get x)
      let class_definitions_get_array x =
        Capnp.Array.to_array (class_definitions_get x)
      let has_global_variables x =
        RA_.has_field x 4
      let global_variables_get x = 
        RA_.get_struct_list x 4
      let global_variables_get_list x =
        Capnp.Array.to_list (global_variables_get x)
      let global_variables_get_array x =
        Capnp.Array.to_array (global_variables_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module LocationTypeIdEntry = struct
      type struct_t = [`LocationTypeIdEntry_ec97908cde450b07]
      type t = struct_t reader_t
      let has_location x =
        RA_.has_field x 0
      let location_get x =
        RA_.get_struct x 0
      let location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let type_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let type_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (type_id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module FunctionTypeOfExpressions = struct
      type struct_t = [`FunctionTypeOfExpressions_e454c787c4578ae9]
      type t = struct_t reader_t
      let has_function_id x =
        RA_.has_field x 0
      let function_id_get x =
        RA_.get_text ~default:"" x 0
      let has_types x =
        RA_.has_field x 1
      let types_get x = 
        RA_.get_struct_list x 1
      let types_get_list x =
        Capnp.Array.to_list (types_get x)
      let types_get_array x =
        Capnp.Array.to_array (types_get x)
      let has_locations x =
        RA_.has_field x 2
      let locations_get x = 
        RA_.get_struct_list x 2
      let locations_get_list x =
        Capnp.Array.to_list (locations_get x)
      let locations_get_array x =
        Capnp.Array.to_array (locations_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ModuleTypeOfExpressions = struct
      type struct_t = [`ModuleTypeOfExpressions_cfca7d3ae9f723a7]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_module_name x =
        RA_.has_field x 0
      let module_name_get x =
        RA_.get_text ~default:"" x 0
      let has_source_path x =
        RA_.has_field x 1
      let source_path_get x =
        RA_.get_struct x 1
      let source_path_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_functions x =
        RA_.has_field x 2
      let functions_get x = 
        RA_.get_struct_list x 2
      let functions_get_list x =
        Capnp.Array.to_list (functions_get x)
      let functions_get_array x =
        Capnp.Array.to_array (functions_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ModuleCallGraphs = struct
      type struct_t = [`ModuleCallGraphs_c11251973766f6c6]
      type t = struct_t reader_t
      let module_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let has_module_name x =
        RA_.has_field x 0
      let module_name_get x =
        RA_.get_text ~default:"" x 0
      let has_source_path x =
        RA_.has_field x 1
      let source_path_get x =
        RA_.get_struct x 1
      let source_path_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_call_graphs x =
        RA_.has_field x 2
      let call_graphs_get x = 
        RA_.get_struct_list x 2
      let call_graphs_get_list x =
        Capnp.Array.to_list (call_graphs_get x)
      let call_graphs_get_array x =
        Capnp.Array.to_array (call_graphs_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module TypeError = struct
      type struct_t = [`TypeError_cf833e7f4687d6b7]
      type t = struct_t reader_t
      let has_module_name x =
        RA_.has_field x 0
      let module_name_get x =
        RA_.get_text ~default:"" x 0
      let has_module_path x =
        RA_.has_field x 1
      let module_path_get x =
        RA_.get_struct x 1
      let module_path_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let has_location x =
        RA_.has_field x 2
      let location_get x =
        RA_.get_struct x 2
      let location_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 2
      let has_kind x =
        RA_.has_field x 3
      let kind_get x =
        RA_.get_text ~default:"" x 3
      let has_message x =
        RA_.has_field x 4
      let message_get x =
        RA_.get_text ~default:"" x 4
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module TypeErrors = struct
      type struct_t = [`TypeErrors_c2e0d691270c45f9]
      type t = struct_t reader_t
      let has_errors x =
        RA_.has_field x 0
      let errors_get x = 
        RA_.get_struct_list x 0
      let errors_get_list x =
        Capnp.Array.to_list (errors_get x)
      let errors_get_array x =
        Capnp.Array.to_array (errors_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module SourcePath = struct
      type struct_t = [`SourcePath_c9789635145f607d]
      type t = struct_t builder_t
      let has_file_system x =
        BA_.has_field x 0
      let file_system_get x =
        BA_.get_text ~default:"" x 0
      let file_system_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let has_namespace x =
        BA_.has_field x 0
      let namespace_get x =
        BA_.get_text ~default:"" x 0
      let namespace_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let has_memory x =
        BA_.has_field x 0
      let memory_get x =
        BA_.get_text ~default:"" x 0
      let memory_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
      let has_bundled_typeshed x =
        BA_.has_field x 0
      let bundled_typeshed_get x =
        BA_.get_text ~default:"" x 0
      let bundled_typeshed_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 v
      let has_bundled_typeshed_third_party x =
        BA_.has_field x 0
      let bundled_typeshed_third_party_get x =
        BA_.get_text ~default:"" x 0
      let bundled_typeshed_third_party_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let has_bundled_third_party x =
        BA_.has_field x 0
      let bundled_third_party_get x =
        BA_.get_text ~default:"" x 0
      let bundled_third_party_set x v =
        BA_.set_text ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 v
      type unnamed_union_t =
        | FileSystem of string
        | Namespace of string
        | Memory of string
        | BundledTypeshed of string
        | BundledTypeshedThirdParty of string
        | BundledThirdParty of string
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> FileSystem (file_system_get x)
        | 1 -> Namespace (namespace_get x)
        | 2 -> Memory (memory_get x)
        | 3 -> BundledTypeshed (bundled_typeshed_get x)
        | 4 -> BundledTypeshedThirdParty (bundled_typeshed_third_party_get x)
        | 5 -> BundledThirdParty (bundled_third_party_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module PysaLocation = struct
      type struct_t = [`PysaLocation_c309c969f3e58a60]
      type t = struct_t builder_t
      let line_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let line_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (line_get x)
      let line_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let line_set_int_exn x v = line_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let col_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let col_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (col_get x)
      let col_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 4 v
      let col_set_int_exn x v = col_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let end_line_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 8
      let end_line_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (end_line_get x)
      let end_line_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 8 v
      let end_line_set_int_exn x v = end_line_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let end_col_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 12
      let end_col_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (end_col_get x)
      let end_col_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 12 v
      let end_col_set_int_exn x v = end_col_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:0
    end
    module ClassRef = struct
      type struct_t = [`ClassRef_de5e9ff6d0dd25c7]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let class_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let class_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_id_get x)
      let class_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 4 v
      let class_id_set_int_exn x v = class_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module FunctionRef = struct
      type struct_t = [`FunctionRef_80115bb31006b8bb]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_function_id x =
        BA_.has_field x 0
      let function_id_get x =
        BA_.get_text ~default:"" x 0
      let function_id_set x v =
        BA_.set_text x 0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module GlobalVariableRef = struct
      type struct_t = [`GlobalVariableRef_f70734c08fbb8800]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module TypeModifier = struct
      type t = TypeModifier_17523487963418047075.t =
        | Optional
        | Coroutine
        | Awaitable
        | TypeVariableBound
        | TypeVariableConstraint
        | Type
        | Undefined of int
    end
    module ClassWithModifiers = struct
      type struct_t = [`ClassWithModifiers_e15f82ade37d4236]
      type t = struct_t builder_t
      let has_class x =
        BA_.has_field x 0
      let class_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 0
      let class_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 0 v
      let class_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 0 (Some v)
      let class_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 0
      let has_modifiers x =
        BA_.has_field x 1
      let modifiers_get x =
        let slice_decoder slice =
          TypeModifier_17523487963418047075.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (TypeModifier_17523487963418047075.encode_safe v)
        in
        BA_.get_list
          ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1
      let modifiers_get_list x =
        Capnp.Array.to_list (modifiers_get x)
      let modifiers_get_array x =
        Capnp.Array.to_array (modifiers_get x)
      let modifiers_set x v =
        let slice_decoder slice =
          TypeModifier_17523487963418047075.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (TypeModifier_17523487963418047075.encode_safe v)
        in
        BA_.set_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1 v
      let modifiers_init x n =
        let slice_decoder slice =
          TypeModifier_17523487963418047075.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (TypeModifier_17523487963418047075.encode_safe v)
        in
        BA_.init_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1 n
      let modifiers_set_list x v =
        let builder = modifiers_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let modifiers_set_array x v =
        let builder = modifiers_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module ClassNamesFromType = struct
      type struct_t = [`ClassNamesFromType_a0584aac2e97e83c]
      type t = struct_t builder_t
      let has_classes x =
        BA_.has_field x 0
      let classes_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 0
      let classes_get_list x =
        Capnp.Array.to_list (classes_get x)
      let classes_get_array x =
        Capnp.Array.to_array (classes_get x)
      let classes_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 0 v
      let classes_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 0 n
      let classes_set_list x v =
        let builder = classes_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let classes_set_array x v =
        let builder = classes_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let is_exhaustive_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_exhaustive_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module ScalarTypeProperties = struct
      type struct_t = [`ScalarTypeProperties_d79ace8daba1babf]
      type t = struct_t builder_t
      let is_bool_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_bool_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let is_int_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_int_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
      let is_float_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_float_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2 v
      let is_enum_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let is_enum_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module PysaType = struct
      type struct_t = [`PysaType_d28d95e8ed282312]
      type t = struct_t builder_t
      let has_string x =
        BA_.has_field x 0
      let string_get x =
        BA_.get_text ~default:"" x 0
      let string_set x v =
        BA_.set_text x 0 v
      let has_scalar_type_properties x =
        BA_.has_field x 1
      let scalar_type_properties_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let scalar_type_properties_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let scalar_type_properties_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let scalar_type_properties_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let has_class_names x =
        BA_.has_field x 2
      let class_names_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 2
      let class_names_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 2 v
      let class_names_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 2 (Some v)
      let class_names_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 2
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:3
    end
    module ScopeParent = struct
      type struct_t = [`ScopeParent_de2463e0e757468e]
      type t = struct_t builder_t
      let function_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let function_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (function_get x)
      let function_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
      let function_set_int_exn x v = function_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let class_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let class_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_get x)
      let class_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
      let class_set_int_exn x v = class_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let top_level_get x = ()
      let top_level_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=4} x
      type unnamed_union_t =
        | Function of Stdint.Uint32.t
        | Class of Stdint.Uint32.t
        | TopLevel
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 4 with
        | 0 -> Function (function_get x)
        | 1 -> Class (class_get x)
        | 2 -> TopLevel
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module FunctionParameter = struct
      type struct_t = [`FunctionParameter_a1b2fce98392735b]
      type t = struct_t builder_t
      module PosOnlyParam = struct
        type struct_t = [`PosOnlyParam_f44b7078e9979a56]
        type t = struct_t builder_t
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let has_annotation x =
          BA_.has_field x 1
        let annotation_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
        let annotation_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
        let annotation_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
        let annotation_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
        let required_get x =
          BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let required_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
      end
      module PosParam = struct
        type struct_t = [`PosParam_cb05f6c652a9cccf]
        type t = struct_t builder_t
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let has_annotation x =
          BA_.has_field x 1
        let annotation_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
        let annotation_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
        let annotation_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
        let annotation_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
        let required_get x =
          BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let required_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
      end
      module VarArgParam = struct
        type struct_t = [`VarArgParam_987ff4fc98295c57]
        type t = struct_t builder_t
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let has_annotation x =
          BA_.has_field x 1
        let annotation_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
        let annotation_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
        let annotation_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
        let annotation_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
        let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
      end
      module KwOnlyParam = struct
        type struct_t = [`KwOnlyParam_9ceff6d824c01469]
        type t = struct_t builder_t
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let has_annotation x =
          BA_.has_field x 1
        let annotation_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
        let annotation_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
        let annotation_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
        let annotation_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
        let required_get x =
          BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
        let required_set x v =
          BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
      end
      module KwargsParam = struct
        type struct_t = [`KwargsParam_914731ff31eab123]
        type t = struct_t builder_t
        let has_name x =
          BA_.has_field x 0
        let name_get x =
          BA_.get_text ~default:"" x 0
        let name_set x v =
          BA_.set_text x 0 v
        let has_annotation x =
          BA_.has_field x 1
        let annotation_get x =
          BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
        let annotation_set_reader x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
        let annotation_set_builder x v =
          BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
        let annotation_init x =
          BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
        let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
      end
      let has_pos_only x =
        BA_.has_field x 0
      let pos_only_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let pos_only_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let pos_only_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let pos_only_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let has_pos x =
        BA_.has_field x 0
      let pos_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let pos_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let pos_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let pos_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
      let has_var_arg x =
        BA_.has_field x 0
      let var_arg_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let var_arg_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
      let var_arg_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let var_arg_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0
      let has_kw_only x =
        BA_.has_field x 0
      let kw_only_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let kw_only_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 v
      let kw_only_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let kw_only_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0
      let has_kwargs x =
        BA_.has_field x 0
      let kwargs_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let kwargs_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let kwargs_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let kwargs_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0
      type unnamed_union_t =
        | PosOnly of [`PosOnlyParam_f44b7078e9979a56] builder_t
        | Pos of [`PosParam_cb05f6c652a9cccf] builder_t
        | VarArg of [`VarArgParam_987ff4fc98295c57] builder_t
        | KwOnly of [`KwOnlyParam_9ceff6d824c01469] builder_t
        | Kwargs of [`KwargsParam_914731ff31eab123] builder_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> PosOnly (pos_only_get x)
        | 1 -> Pos (pos_get x)
        | 2 -> VarArg (var_arg_get x)
        | 3 -> KwOnly (kw_only_get x)
        | 4 -> Kwargs (kwargs_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module FunctionParameters = struct
      type struct_t = [`FunctionParameters_a7c6a3610ea670c8]
      type t = struct_t builder_t
      let has_list x =
        BA_.has_field x 0
      let list_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 0
      let list_get_list x =
        Capnp.Array.to_list (list_get x)
      let list_get_array x =
        Capnp.Array.to_array (list_get x)
      let list_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let list_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 n
      let list_set_list x v =
        let builder = list_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let list_set_array x v =
        let builder = list_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let ellipsis_get x = ()
      let ellipsis_set x =
        BA_.set_void ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x
      let param_spec_get x = ()
      let param_spec_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x
      type unnamed_union_t =
        | List of (rw, FunctionParameter.t, array_t) Capnp.Array.t
        | Ellipsis
        | ParamSpec
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> List (list_get x)
        | 1 -> Ellipsis
        | 2 -> ParamSpec
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module FunctionSignature = struct
      type struct_t = [`FunctionSignature_be11da038b7eb7d9]
      type t = struct_t builder_t
      let has_parameters x =
        BA_.has_field x 0
      let parameters_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let parameters_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let parameters_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let parameters_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let has_return_annotation x =
        BA_.has_field x 1
      let return_annotation_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
      let return_annotation_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
      let return_annotation_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
      let return_annotation_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module FunctionBaseDefinition = struct
      type struct_t = [`FunctionBaseDefinition_876cdc721c603758]
      type t = struct_t builder_t
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let has_parent x =
        BA_.has_field x 1
      let parent_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let parent_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let parent_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let parent_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let is_overload_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_overload_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let is_staticmethod_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_staticmethod_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
      let is_classmethod_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_classmethod_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2 v
      let is_property_getter_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let is_property_getter_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3 v
      let is_property_setter_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4
      let is_property_setter_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4 v
      let is_stub_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5
      let is_stub_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5 v
      let is_def_statement_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6
      let is_def_statement_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6 v
      let has_defining_class x =
        BA_.has_field x 2
      let defining_class_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 2
      let defining_class_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 v
      let defining_class_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 (Some v)
      let defining_class_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 2
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module CapturedVariableRef = struct
      type struct_t = [`CapturedVariableRef_94805e8830d6bb4e]
      type t = struct_t builder_t
      let has_outer_function x =
        BA_.has_field x 0
      let outer_function_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let outer_function_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let outer_function_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let outer_function_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let has_name x =
        BA_.has_field x 1
      let name_get x =
        BA_.get_text ~default:"" x 1
      let name_set x v =
        BA_.set_text x 1 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module Target = struct
      type struct_t = [`Target_ff8266cc34db19d3]
      type t = struct_t builder_t
      let has_function x =
        BA_.has_field x 0
      let function_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let function_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let function_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let function_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let has_overrides x =
        BA_.has_field x 0
      let overrides_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let overrides_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let overrides_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let overrides_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
      let format_string_get x = ()
      let format_string_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x
      type unnamed_union_t =
        | Function of FunctionRef.t
        | Overrides of FunctionRef.t
        | FormatString
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Function (function_get x)
        | 1 -> Overrides (overrides_get x)
        | 2 -> FormatString
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module ImplicitReceiver = struct
      type t = ImplicitReceiver_16434162114200234704.t =
        | TrueWithClassReceiver
        | TrueWithObjectReceiver
        | False
        | Undefined of int
    end
    module PysaCallTarget = struct
      type struct_t = [`PysaCallTarget_cf1542da5a3f29e1]
      type t = struct_t builder_t
      let has_target x =
        BA_.has_field x 0
      let target_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let target_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let target_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let target_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let implicit_receiver_get x =
        let discr = BA_.get_uint16 ~default:0 x 0 in
        ImplicitReceiver_16434162114200234704.decode discr
      let implicit_receiver_set x e =
        BA_.set_uint16 ~default:0 x 0 (ImplicitReceiver_16434162114200234704.encode_safe e)
      let implicit_receiver_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 0 (ImplicitReceiver_16434162114200234704.encode_unsafe e)
      let implicit_dunder_call_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0
      let implicit_dunder_call_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:0 v
      let has_receiver_class x =
        BA_.has_field x 1
      let receiver_class_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let receiver_class_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let receiver_class_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let receiver_class_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let is_class_method_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1
      let is_class_method_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:1 v
      let is_static_method_get x =
        BA_.get_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2
      let is_static_method_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:2 ~bit_ofs:2 v
      let has_return_type x =
        BA_.has_field x 2
      let return_type_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 2
      let return_type_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 v
      let return_type_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 (Some v)
      let return_type_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 2
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module DecoratorCallee = struct
      type struct_t = [`DecoratorCallee_bc083e8c7ab91017]
      type t = struct_t builder_t
      let has_location x =
        BA_.has_field x 0
      let location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 0
      let location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 v
      let location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 (Some v)
      let location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 0
      let has_targets x =
        BA_.has_field x 1
      let targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 v
      let targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n
      let targets_set_list x v =
        let builder = targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let targets_set_array x v =
        let builder = targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module FunctionDefinition = struct
      type struct_t = [`FunctionDefinition_ac2bccd871436411]
      type t = struct_t builder_t
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let has_define_name_location x =
        BA_.has_field x 8
      let define_name_location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 8
      let define_name_location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 8 v
      let define_name_location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 8 (Some v)
      let define_name_location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 8
      let has_parent x =
        BA_.has_field x 1
      let parent_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let parent_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let parent_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let parent_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let is_overload_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_overload_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let is_staticmethod_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1
      let is_staticmethod_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:1 v
      let is_classmethod_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2
      let is_classmethod_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:2 v
      let is_property_getter_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3
      let is_property_getter_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:3 v
      let is_property_setter_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4
      let is_property_setter_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:4 v
      let is_stub_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5
      let is_stub_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:5 v
      let is_def_statement_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6
      let is_def_statement_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:6 v
      let has_defining_class x =
        BA_.has_field x 2
      let defining_class_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 2
      let defining_class_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 v
      let defining_class_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 2 (Some v)
      let defining_class_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 2
      let has_function_id x =
        BA_.has_field x 3
      let function_id_get x =
        BA_.get_text ~default:"" x 3
      let function_id_set x v =
        BA_.set_text x 3 v
      let has_undecorated_signatures x =
        BA_.has_field x 4
      let undecorated_signatures_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 4
      let undecorated_signatures_get_list x =
        Capnp.Array.to_list (undecorated_signatures_get x)
      let undecorated_signatures_get_array x =
        Capnp.Array.to_array (undecorated_signatures_get x)
      let undecorated_signatures_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 4 v
      let undecorated_signatures_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 4 n
      let undecorated_signatures_set_list x v =
        let builder = undecorated_signatures_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let undecorated_signatures_set_array x v =
        let builder = undecorated_signatures_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_captured_variables x =
        BA_.has_field x 5
      let captured_variables_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 5
      let captured_variables_get_list x =
        Capnp.Array.to_list (captured_variables_get x)
      let captured_variables_get_array x =
        Capnp.Array.to_array (captured_variables_get x)
      let captured_variables_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 5 v
      let captured_variables_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 5 n
      let captured_variables_set_list x v =
        let builder = captured_variables_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let captured_variables_set_array x v =
        let builder = captured_variables_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_decorator_callees x =
        BA_.has_field x 6
      let decorator_callees_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 6
      let decorator_callees_get_list x =
        Capnp.Array.to_list (decorator_callees_get x)
      let decorator_callees_get_array x =
        Capnp.Array.to_array (decorator_callees_get x)
      let decorator_callees_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 6 v
      let decorator_callees_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 6 n
      let decorator_callees_set_list x v =
        let builder = decorator_callees_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let decorator_callees_set_array x v =
        let builder = decorator_callees_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_overridden_base_method x =
        BA_.has_field x 7
      let overridden_base_method_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 7
      let overridden_base_method_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 7 v
      let overridden_base_method_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 7 (Some v)
      let overridden_base_method_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 7
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:9 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:9 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:9
    end
    module PysaClassFieldDeclaration = struct
      type t = PysaClassFieldDeclaration_17132881886155188463.t =
        | None
        | DeclaredByAnnotation
        | DeclaredWithoutAnnotation
        | AssignedInBody
        | DefinedWithoutAssign
        | DefinedInMethod
        | Undefined of int
    end
    module PysaClassField = struct
      type struct_t = [`PysaClassField_f94ec0e6aa9a5e25]
      type t = struct_t builder_t
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let has_type x =
        BA_.has_field x 1
      let type_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
      let type_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
      let type_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
      let type_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
      let has_explicit_annotation x =
        BA_.has_field x 2
      let explicit_annotation_get x =
        BA_.get_text ~default:"" x 2
      let explicit_annotation_set x v =
        BA_.set_text x 2 v
      let has_location x =
        BA_.has_field x 3
      let location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 3
      let location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 3 v
      let location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 3 (Some v)
      let location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 3
      let declaration_kind_get x =
        let discr = BA_.get_uint16 ~default:0 x 0 in
        PysaClassFieldDeclaration_17132881886155188463.decode discr
      let declaration_kind_set x e =
        BA_.set_uint16 ~default:0 x 0 (PysaClassFieldDeclaration_17132881886155188463.encode_safe e)
      let declaration_kind_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 0 (PysaClassFieldDeclaration_17132881886155188463.encode_unsafe e)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:4 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:4 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:4
    end
    module PysaClassMro = struct
      type struct_t = [`PysaClassMro_addaafc52d26dffb]
      type t = struct_t builder_t
      let has_resolved x =
        BA_.has_field x 0
      let resolved_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 0
      let resolved_get_list x =
        Capnp.Array.to_list (resolved_get x)
      let resolved_get_array x =
        Capnp.Array.to_array (resolved_get x)
      let resolved_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let resolved_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 n
      let resolved_set_list x v =
        let builder = resolved_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let resolved_set_array x v =
        let builder = resolved_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let cyclic_get x = ()
      let cyclic_set x =
        BA_.set_void ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x
      type unnamed_union_t =
        | Resolved of (rw, ClassRef.t, array_t) Capnp.Array.t
        | Cyclic
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Resolved (resolved_get x)
        | 1 -> Cyclic
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module ClassDefinition = struct
      type struct_t = [`ClassDefinition_f802b9f88052bf19]
      type t = struct_t builder_t
      let class_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let class_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (class_id_get x)
      let class_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let class_id_set_int_exn x v = class_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_name x =
        BA_.has_field x 1
      let name_get x =
        BA_.get_text ~default:"" x 1
      let name_set x v =
        BA_.set_text x 1 v
      let has_name_location x =
        BA_.has_field x 0
      let name_location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 0
      let name_location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 v
      let name_location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 (Some v)
      let name_location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 0
      let has_bases x =
        BA_.has_field x 2
      let bases_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 2
      let bases_get_list x =
        Capnp.Array.to_list (bases_get x)
      let bases_get_array x =
        Capnp.Array.to_array (bases_get x)
      let bases_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 x 2 v
      let bases_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 x 2 n
      let bases_set_list x v =
        let builder = bases_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let bases_set_array x v =
        let builder = bases_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_mro x =
        BA_.has_field x 3
      let mro_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 3
      let mro_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 3 v
      let mro_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 3 (Some v)
      let mro_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 3
      let has_parent x =
        BA_.has_field x 4
      let parent_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 4
      let parent_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 4 v
      let parent_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 4 (Some v)
      let parent_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 4
      let is_synthesized_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let is_synthesized_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0 v
      let is_dataclass_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1
      let is_dataclass_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1 v
      let is_named_tuple_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2
      let is_named_tuple_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2 v
      let is_typed_dict_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3
      let is_typed_dict_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3 v
      let has_fields x =
        BA_.has_field x 5
      let fields_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:4 x 5
      let fields_get_list x =
        Capnp.Array.to_list (fields_get x)
      let fields_get_array x =
        Capnp.Array.to_array (fields_get x)
      let fields_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:4 x 5 v
      let fields_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:4 x 5 n
      let fields_set_list x v =
        let builder = fields_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let fields_set_array x v =
        let builder = fields_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_decorator_callees x =
        BA_.has_field x 6
      let decorator_callees_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 6
      let decorator_callees_get_list x =
        Capnp.Array.to_list (decorator_callees_get x)
      let decorator_callees_get_array x =
        Capnp.Array.to_array (decorator_callees_get x)
      let decorator_callees_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 6 v
      let decorator_callees_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 6 n
      let decorator_callees_set_list x v =
        let builder = decorator_callees_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let decorator_callees_set_array x v =
        let builder = decorator_callees_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:7 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:7 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:7
    end
    module GlobalVariable = struct
      type struct_t = [`GlobalVariable_d7ecbd0f4024046b]
      type t = struct_t builder_t
      let has_name x =
        BA_.has_field x 0
      let name_get x =
        BA_.get_text ~default:"" x 0
      let name_set x v =
        BA_.set_text x 0 v
      let has_type x =
        BA_.has_field x 1
      let type_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:3 x 1
      let type_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 v
      let type_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 x 1 (Some v)
      let type_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:3 x 1
      let has_location x =
        BA_.has_field x 2
      let location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 2
      let location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 2 v
      let location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 2 (Some v)
      let location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 2
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:3
    end
    module UnresolvedReason = struct
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
    module Unresolved = struct
      type struct_t = [`Unresolved_f5be7abed98173b3]
      type t = struct_t builder_t
      let false_get x = ()
      let false_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
      let true_get x =
        let discr = BA_.get_uint16 ~default:0 x 2 in
        UnresolvedReason_16256959910610618162.decode discr
      let true_set x e =
        BA_.set_uint16 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} ~default:0 x 2 (UnresolvedReason_16256959910610618162.encode_safe e)
      let true_set_unsafe x e =
        BA_.set_uint16 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} ~default:0 x 2 (UnresolvedReason_16256959910610618162.encode_unsafe e)
      type unnamed_union_t =
        | False
        | True of UnresolvedReason.t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> False
        | 1 -> True (true_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module HigherOrderParameter = struct
      type struct_t = [`HigherOrderParameter_9f08f2a4f21fb93d]
      type t = struct_t builder_t
      let index_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let index_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (index_get x)
      let index_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let index_set_int_exn x v = index_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_call_targets x =
        BA_.has_field x 0
      let call_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let call_targets_get_list x =
        Capnp.Array.to_list (call_targets_get x)
      let call_targets_get_array x =
        Capnp.Array.to_array (call_targets_get x)
      let call_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let call_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let call_targets_set_list x v =
        let builder = call_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let call_targets_set_array x v =
        let builder = call_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_unresolved x =
        BA_.has_field x 1
      let unresolved_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let unresolved_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let unresolved_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let unresolved_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
    end
    module CallCallees = struct
      type struct_t = [`CallCallees_fa97c29dc67a9655]
      type t = struct_t builder_t
      let has_call_targets x =
        BA_.has_field x 0
      let call_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let call_targets_get_list x =
        Capnp.Array.to_list (call_targets_get x)
      let call_targets_get_array x =
        Capnp.Array.to_array (call_targets_get x)
      let call_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let call_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let call_targets_set_list x v =
        let builder = call_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let call_targets_set_array x v =
        let builder = call_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_init_targets x =
        BA_.has_field x 1
      let init_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 1
      let init_targets_get_list x =
        Capnp.Array.to_list (init_targets_get x)
      let init_targets_get_array x =
        Capnp.Array.to_array (init_targets_get x)
      let init_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 1 v
      let init_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 1 n
      let init_targets_set_list x v =
        let builder = init_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let init_targets_set_array x v =
        let builder = init_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_new_targets x =
        BA_.has_field x 2
      let new_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 2
      let new_targets_get_list x =
        Capnp.Array.to_list (new_targets_get x)
      let new_targets_get_array x =
        Capnp.Array.to_array (new_targets_get x)
      let new_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 2 v
      let new_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 2 n
      let new_targets_set_list x v =
        let builder = new_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let new_targets_set_array x v =
        let builder = new_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_higher_order_parameters x =
        BA_.has_field x 3
      let higher_order_parameters_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:2 x 3
      let higher_order_parameters_get_list x =
        Capnp.Array.to_list (higher_order_parameters_get x)
      let higher_order_parameters_get_array x =
        Capnp.Array.to_array (higher_order_parameters_get x)
      let higher_order_parameters_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:2 x 3 v
      let higher_order_parameters_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:2 x 3 n
      let higher_order_parameters_set_list x v =
        let builder = higher_order_parameters_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let higher_order_parameters_set_array x v =
        let builder = higher_order_parameters_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_unresolved x =
        BA_.has_field x 4
      let unresolved_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 4
      let unresolved_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 4 v
      let unresolved_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 4 (Some v)
      let unresolved_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 4
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:5 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:5
    end
    module AttributeAccessCallees = struct
      type struct_t = [`AttributeAccessCallees_da03cbf714d87201]
      type t = struct_t builder_t
      let has_if_called x =
        BA_.has_field x 0
      let if_called_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:5 x 0
      let if_called_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 x 0 v
      let if_called_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 x 0 (Some v)
      let if_called_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:5 x 0
      let has_property_setters x =
        BA_.has_field x 1
      let property_setters_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 1
      let property_setters_get_list x =
        Capnp.Array.to_list (property_setters_get x)
      let property_setters_get_array x =
        Capnp.Array.to_array (property_setters_get x)
      let property_setters_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 1 v
      let property_setters_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 1 n
      let property_setters_set_list x v =
        let builder = property_setters_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let property_setters_set_array x v =
        let builder = property_setters_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_property_getters x =
        BA_.has_field x 2
      let property_getters_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 2
      let property_getters_get_list x =
        Capnp.Array.to_list (property_getters_get x)
      let property_getters_get_array x =
        Capnp.Array.to_array (property_getters_get x)
      let property_getters_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 2 v
      let property_getters_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 2 n
      let property_getters_set_list x v =
        let builder = property_getters_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let property_getters_set_array x v =
        let builder = property_getters_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_global_targets x =
        BA_.has_field x 3
      let global_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 3
      let global_targets_get_list x =
        Capnp.Array.to_list (global_targets_get x)
      let global_targets_get_array x =
        Capnp.Array.to_array (global_targets_get x)
      let global_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 3 v
      let global_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 3 n
      let global_targets_set_list x v =
        let builder = global_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let global_targets_set_array x v =
        let builder = global_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let is_attribute_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let is_attribute_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:4 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:4 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:4
    end
    module IdentifierCallees = struct
      type struct_t = [`IdentifierCallees_a5f0dc0068285f29]
      type t = struct_t builder_t
      let has_if_called x =
        BA_.has_field x 0
      let if_called_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:5 x 0
      let if_called_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 x 0 v
      let if_called_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 x 0 (Some v)
      let if_called_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:5 x 0
      let has_global_targets x =
        BA_.has_field x 1
      let global_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1
      let global_targets_get_list x =
        Capnp.Array.to_list (global_targets_get x)
      let global_targets_get_array x =
        Capnp.Array.to_array (global_targets_get x)
      let global_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 v
      let global_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n
      let global_targets_set_list x v =
        let builder = global_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let global_targets_set_array x v =
        let builder = global_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_captured_variables x =
        BA_.has_field x 2
      let captured_variables_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 2
      let captured_variables_get_list x =
        Capnp.Array.to_list (captured_variables_get x)
      let captured_variables_get_array x =
        Capnp.Array.to_array (captured_variables_get x)
      let captured_variables_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 2 v
      let captured_variables_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 2 n
      let captured_variables_set_list x v =
        let builder = captured_variables_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let captured_variables_set_array x v =
        let builder = captured_variables_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:3
    end
    module DefineCallees = struct
      type struct_t = [`DefineCallees_b379211649ffdce7]
      type t = struct_t builder_t
      let has_define_targets x =
        BA_.has_field x 0
      let define_targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let define_targets_get_list x =
        Capnp.Array.to_list (define_targets_get x)
      let define_targets_get_array x =
        Capnp.Array.to_array (define_targets_get x)
      let define_targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let define_targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let define_targets_set_list x v =
        let builder = define_targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let define_targets_set_array x v =
        let builder = define_targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
    end
    module FormatStringArtificialCallees = struct
      type struct_t = [`FormatStringArtificialCallees_a05d8efd266e9543]
      type t = struct_t builder_t
      let has_targets x =
        BA_.has_field x 0
      let targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let targets_set_list x v =
        let builder = targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let targets_set_array x v =
        let builder = targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
    end
    module FormatStringStringifyCallees = struct
      type struct_t = [`FormatStringStringifyCallees_c57214d24c99a6b3]
      type t = struct_t builder_t
      let has_targets x =
        BA_.has_field x 0
      let targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let targets_set_list x v =
        let builder = targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let targets_set_array x v =
        let builder = targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_unresolved x =
        BA_.has_field x 1
      let unresolved_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 1
      let unresolved_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 v
      let unresolved_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 x 1 (Some v)
      let unresolved_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 x 1
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module ReturnShimArgumentMapping = struct
      type t = ReturnShimArgumentMapping_16744047642608040181.t =
        | ReturnExpression
        | ReturnExpressionElement
        | Undefined of int
    end
    module ReturnShimCallees = struct
      type struct_t = [`ReturnShimCallees_a830c3d4e1910d6d]
      type t = struct_t builder_t
      let has_targets x =
        BA_.has_field x 0
      let targets_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:3 x 0
      let targets_get_list x =
        Capnp.Array.to_list (targets_get x)
      let targets_get_array x =
        Capnp.Array.to_array (targets_get x)
      let targets_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:3 x 0 v
      let targets_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:3 x 0 n
      let targets_set_list x v =
        let builder = targets_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let targets_set_array x v =
        let builder = targets_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_arguments x =
        BA_.has_field x 1
      let arguments_get x =
        let slice_decoder slice =
          ReturnShimArgumentMapping_16744047642608040181.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (ReturnShimArgumentMapping_16744047642608040181.encode_safe v)
        in
        BA_.get_list
          ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1
      let arguments_get_list x =
        Capnp.Array.to_list (arguments_get x)
      let arguments_get_array x =
        Capnp.Array.to_array (arguments_get x)
      let arguments_set x v =
        let slice_decoder slice =
          ReturnShimArgumentMapping_16744047642608040181.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (ReturnShimArgumentMapping_16744047642608040181.encode_safe v)
        in
        BA_.set_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1 v
      let arguments_init x n =
        let slice_decoder slice =
          ReturnShimArgumentMapping_16744047642608040181.decode (MessageWrapper.Slice.get_uint16 slice 0)
        in
        let slice_encoder v slice =
          MessageWrapper.Slice.set_uint16 slice 0 (ReturnShimArgumentMapping_16744047642608040181.encode_safe v)
        in
        BA_.init_list ~storage_type:Capnp.Runtime.ListStorageType.Bytes2
          ~codecs:(BA_.NC.ListCodecs.Bytes2 (slice_decoder, slice_encoder))
          x 1 n
      let arguments_set_list x v =
        let builder = arguments_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let arguments_set_array x v =
        let builder = arguments_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module ExpressionCallees = struct
      type struct_t = [`ExpressionCallees_9c207ddf98bd8cdf]
      type t = struct_t builder_t
      let has_call x =
        BA_.has_field x 0
      let call_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:5 x 0
      let call_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let call_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let call_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:5 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let has_identifier x =
        BA_.has_field x 0
      let identifier_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:3 x 0
      let identifier_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let identifier_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:3 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let identifier_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:3 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
      let has_attribute_access x =
        BA_.has_field x 0
      let attribute_access_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:4 x 0
      let attribute_access_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:4 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
      let attribute_access_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:4 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let attribute_access_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:4 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0
      let has_define x =
        BA_.has_field x 0
      let define_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:1 x 0
      let define_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 v
      let define_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let define_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0
      let has_format_string_artificial x =
        BA_.has_field x 0
      let format_string_artificial_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:1 x 0
      let format_string_artificial_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let format_string_artificial_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let format_string_artificial_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0
      let has_format_string_stringify x =
        BA_.has_field x 0
      let format_string_stringify_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let format_string_stringify_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 v
      let format_string_stringify_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let format_string_stringify_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0
      let has_return x =
        BA_.has_field x 0
      let return_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let return_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0 v
      let return_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let return_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0
      type unnamed_union_t =
        | Call of CallCallees.t
        | Identifier of IdentifierCallees.t
        | AttributeAccess of AttributeAccessCallees.t
        | Define of DefineCallees.t
        | FormatStringArtificial of FormatStringArtificialCallees.t
        | FormatStringStringify of FormatStringStringifyCallees.t
        | Return of ReturnShimCallees.t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Call (call_get x)
        | 1 -> Identifier (identifier_get x)
        | 2 -> AttributeAccess (attribute_access_get x)
        | 3 -> Define (define_get x)
        | 4 -> FormatStringArtificial (format_string_artificial_get x)
        | 5 -> FormatStringStringify (format_string_stringify_get x)
        | 6 -> Return (return_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module CallGraphEntry = struct
      type struct_t = [`CallGraphEntry_d918afd531f118f8]
      type t = struct_t builder_t
      let has_expression_id x =
        BA_.has_field x 0
      let expression_id_get x =
        BA_.get_text ~default:"" x 0
      let expression_id_set x v =
        BA_.set_text x 0 v
      let has_callees x =
        BA_.has_field x 1
      let callees_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let callees_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let callees_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let callees_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module FunctionCallGraph = struct
      type struct_t = [`FunctionCallGraph_e54ecff47ce9d92a]
      type t = struct_t builder_t
      let has_function_id x =
        BA_.has_field x 0
      let function_id_get x =
        BA_.get_text ~default:"" x 0
      let function_id_set x v =
        BA_.set_text x 0 v
      let has_entries x =
        BA_.has_field x 1
      let entries_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 1
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let entries_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 1 v
      let entries_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 1 n
      let entries_set_list x v =
        let builder = entries_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let entries_set_array x v =
        let builder = entries_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module PysaProjectModule = struct
      type struct_t = [`PysaProjectModule_898e79c185b3c64b]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_module_name x =
        BA_.has_field x 0
      let module_name_get x =
        BA_.get_text ~default:"" x 0
      let module_name_set x v =
        BA_.set_text x 0 v
      let has_source_path x =
        BA_.has_field x 1
      let source_path_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let source_path_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let source_path_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let source_path_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let has_relative_source_path x =
        BA_.has_field x 2
      let relative_source_path_get x =
        BA_.get_text ~default:"" x 2
      let relative_source_path_set x v =
        BA_.set_text x 2 v
      let has_info_filename x =
        BA_.has_field x 3
      let info_filename_get x =
        BA_.get_text ~default:"" x 3
      let info_filename_set x v =
        BA_.set_text x 3 v
      let has_python_version x =
        BA_.has_field x 4
      let python_version_get x =
        BA_.get_text ~default:"" x 4
      let python_version_set x v =
        BA_.set_text x 4 v
      let has_platform x =
        BA_.has_field x 5
      let platform_get x =
        BA_.get_text ~default:"" x 5
      let platform_set x v =
        BA_.set_text x 5 v
      let is_test_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let is_test_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0 v
      let is_interface_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1
      let is_interface_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:1 v
      let is_init_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2
      let is_init_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:2 v
      let is_internal_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3
      let is_internal_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:3 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:6 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:6 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:6
    end
    module ProjectFile = struct
      type struct_t = [`ProjectFile_fb60e377cc6c9cae]
      type t = struct_t builder_t
      let has_modules x =
        BA_.has_field x 0
      let modules_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:6 x 0
      let modules_get_list x =
        Capnp.Array.to_list (modules_get x)
      let modules_get_array x =
        Capnp.Array.to_array (modules_get x)
      let modules_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:6 x 0 v
      let modules_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:6 x 0 n
      let modules_set_list x v =
        let builder = modules_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let modules_set_array x v =
        let builder = modules_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_builtin_module_ids x =
        BA_.has_field x 1
      let builtin_module_ids_get x =
        BA_.get_uint32_list x 1
      let builtin_module_ids_get_list x =
        Capnp.Array.to_list (builtin_module_ids_get x)
      let builtin_module_ids_get_array x =
        Capnp.Array.to_array (builtin_module_ids_get x)
      let builtin_module_ids_set x v =
        BA_.set_uint32_list x 1 v
      let builtin_module_ids_init x n =
        BA_.init_uint32_list x 1 n
      let builtin_module_ids_set_list x v =
        let builder = builtin_module_ids_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let builtin_module_ids_set_array x v =
        let builder = builtin_module_ids_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_object_class_refs x =
        BA_.has_field x 2
      let object_class_refs_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 2
      let object_class_refs_get_list x =
        Capnp.Array.to_list (object_class_refs_get x)
      let object_class_refs_get_array x =
        Capnp.Array.to_array (object_class_refs_get x)
      let object_class_refs_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 x 2 v
      let object_class_refs_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 x 2 n
      let object_class_refs_set_list x v =
        let builder = object_class_refs_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let object_class_refs_set_array x v =
        let builder = object_class_refs_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_dict_class_refs x =
        BA_.has_field x 3
      let dict_class_refs_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 3
      let dict_class_refs_get_list x =
        Capnp.Array.to_list (dict_class_refs_get x)
      let dict_class_refs_get_array x =
        Capnp.Array.to_array (dict_class_refs_get x)
      let dict_class_refs_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 x 3 v
      let dict_class_refs_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 x 3 n
      let dict_class_refs_set_list x v =
        let builder = dict_class_refs_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let dict_class_refs_set_array x v =
        let builder = dict_class_refs_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_typing_module_ids x =
        BA_.has_field x 4
      let typing_module_ids_get x =
        BA_.get_uint32_list x 4
      let typing_module_ids_get_list x =
        Capnp.Array.to_list (typing_module_ids_get x)
      let typing_module_ids_get_array x =
        Capnp.Array.to_array (typing_module_ids_get x)
      let typing_module_ids_set x v =
        BA_.set_uint32_list x 4 v
      let typing_module_ids_init x n =
        BA_.init_uint32_list x 4 n
      let typing_module_ids_set_list x v =
        let builder = typing_module_ids_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let typing_module_ids_set_array x v =
        let builder = typing_module_ids_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_typing_mapping_class_refs x =
        BA_.has_field x 5
      let typing_mapping_class_refs_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 5
      let typing_mapping_class_refs_get_list x =
        Capnp.Array.to_list (typing_mapping_class_refs_get x)
      let typing_mapping_class_refs_get_array x =
        Capnp.Array.to_array (typing_mapping_class_refs_get x)
      let typing_mapping_class_refs_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 x 5 v
      let typing_mapping_class_refs_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 x 5 n
      let typing_mapping_class_refs_set_list x v =
        let builder = typing_mapping_class_refs_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let typing_mapping_class_refs_set_array x v =
        let builder = typing_mapping_class_refs_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:6 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:6 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:6
    end
    module ModuleDefinitions = struct
      type struct_t = [`ModuleDefinitions_c7923485cc475b88]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_module_name x =
        BA_.has_field x 0
      let module_name_get x =
        BA_.get_text ~default:"" x 0
      let module_name_set x v =
        BA_.set_text x 0 v
      let has_source_path x =
        BA_.has_field x 1
      let source_path_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let source_path_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let source_path_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let source_path_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let has_function_definitions x =
        BA_.has_field x 2
      let function_definitions_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:9 x 2
      let function_definitions_get_list x =
        Capnp.Array.to_list (function_definitions_get x)
      let function_definitions_get_array x =
        Capnp.Array.to_array (function_definitions_get x)
      let function_definitions_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:9 x 2 v
      let function_definitions_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:9 x 2 n
      let function_definitions_set_list x v =
        let builder = function_definitions_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let function_definitions_set_array x v =
        let builder = function_definitions_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_class_definitions x =
        BA_.has_field x 3
      let class_definitions_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:7 x 3
      let class_definitions_get_list x =
        Capnp.Array.to_list (class_definitions_get x)
      let class_definitions_get_array x =
        Capnp.Array.to_array (class_definitions_get x)
      let class_definitions_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:7 x 3 v
      let class_definitions_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:7 x 3 n
      let class_definitions_set_list x v =
        let builder = class_definitions_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let class_definitions_set_array x v =
        let builder = class_definitions_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_global_variables x =
        BA_.has_field x 4
      let global_variables_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:3 x 4
      let global_variables_get_list x =
        Capnp.Array.to_list (global_variables_get x)
      let global_variables_get_array x =
        Capnp.Array.to_array (global_variables_get x)
      let global_variables_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:3 x 4 v
      let global_variables_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:3 x 4 n
      let global_variables_set_list x v =
        let builder = global_variables_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let global_variables_set_array x v =
        let builder = global_variables_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:5 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:5
    end
    module LocationTypeIdEntry = struct
      type struct_t = [`LocationTypeIdEntry_ec97908cde450b07]
      type t = struct_t builder_t
      let has_location x =
        BA_.has_field x 0
      let location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 0
      let location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 v
      let location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 0 (Some v)
      let location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 0
      let type_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let type_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (type_id_get x)
      let type_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let type_id_set_int_exn x v = type_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module FunctionTypeOfExpressions = struct
      type struct_t = [`FunctionTypeOfExpressions_e454c787c4578ae9]
      type t = struct_t builder_t
      let has_function_id x =
        BA_.has_field x 0
      let function_id_get x =
        BA_.get_text ~default:"" x 0
      let function_id_set x v =
        BA_.set_text x 0 v
      let has_types x =
        BA_.has_field x 1
      let types_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:3 x 1
      let types_get_list x =
        Capnp.Array.to_list (types_get x)
      let types_get_array x =
        Capnp.Array.to_array (types_get x)
      let types_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:3 x 1 v
      let types_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:3 x 1 n
      let types_set_list x v =
        let builder = types_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let types_set_array x v =
        let builder = types_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let has_locations x =
        BA_.has_field x 2
      let locations_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 2
      let locations_get_list x =
        Capnp.Array.to_list (locations_get x)
      let locations_get_array x =
        Capnp.Array.to_array (locations_get x)
      let locations_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 2 v
      let locations_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 2 n
      let locations_set_list x v =
        let builder = locations_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let locations_set_array x v =
        let builder = locations_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:3
    end
    module ModuleTypeOfExpressions = struct
      type struct_t = [`ModuleTypeOfExpressions_cfca7d3ae9f723a7]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_module_name x =
        BA_.has_field x 0
      let module_name_get x =
        BA_.get_text ~default:"" x 0
      let module_name_set x v =
        BA_.set_text x 0 v
      let has_source_path x =
        BA_.has_field x 1
      let source_path_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let source_path_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let source_path_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let source_path_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let has_functions x =
        BA_.has_field x 2
      let functions_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:3 x 2
      let functions_get_list x =
        Capnp.Array.to_list (functions_get x)
      let functions_get_array x =
        Capnp.Array.to_array (functions_get x)
      let functions_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:3 x 2 v
      let functions_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:3 x 2 n
      let functions_set_list x v =
        let builder = functions_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let functions_set_array x v =
        let builder = functions_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module ModuleCallGraphs = struct
      type struct_t = [`ModuleCallGraphs_c11251973766f6c6]
      type t = struct_t builder_t
      let module_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let module_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (module_id_get x)
      let module_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let module_id_set_int_exn x v = module_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_module_name x =
        BA_.has_field x 0
      let module_name_get x =
        BA_.get_text ~default:"" x 0
      let module_name_set x v =
        BA_.set_text x 0 v
      let has_source_path x =
        BA_.has_field x 1
      let source_path_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let source_path_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let source_path_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let source_path_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let has_call_graphs x =
        BA_.has_field x 2
      let call_graphs_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:2 x 2
      let call_graphs_get_list x =
        Capnp.Array.to_list (call_graphs_get x)
      let call_graphs_get_array x =
        Capnp.Array.to_array (call_graphs_get x)
      let call_graphs_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:2 x 2 v
      let call_graphs_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:2 x 2 n
      let call_graphs_set_list x v =
        let builder = call_graphs_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let call_graphs_set_array x v =
        let builder = call_graphs_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:3
    end
    module TypeError = struct
      type struct_t = [`TypeError_cf833e7f4687d6b7]
      type t = struct_t builder_t
      let has_module_name x =
        BA_.has_field x 0
      let module_name_get x =
        BA_.get_text ~default:"" x 0
      let module_name_set x v =
        BA_.set_text x 0 v
      let has_module_path x =
        BA_.has_field x 1
      let module_path_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 1
      let module_path_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 v
      let module_path_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 1 (Some v)
      let module_path_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 1
      let has_location x =
        BA_.has_field x 2
      let location_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 2
      let location_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 2 v
      let location_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 x 2 (Some v)
      let location_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 x 2
      let has_kind x =
        BA_.has_field x 3
      let kind_get x =
        BA_.get_text ~default:"" x 3
      let kind_set x v =
        BA_.set_text x 3 v
      let has_message x =
        BA_.has_field x 4
      let message_get x =
        BA_.get_text ~default:"" x 4
      let message_set x v =
        BA_.set_text x 4 v
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:5 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:5 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:5
    end
    module TypeErrors = struct
      type struct_t = [`TypeErrors_c2e0d691270c45f9]
      type t = struct_t builder_t
      let has_errors x =
        BA_.has_field x 0
      let errors_get x = 
        BA_.get_struct_list ~data_words:0 ~pointer_words:5 x 0
      let errors_get_list x =
        Capnp.Array.to_list (errors_get x)
      let errors_get_array x =
        Capnp.Array.to_array (errors_get x)
      let errors_set x v =
        BA_.set_struct_list ~data_words:0 ~pointer_words:5 x 0 v
      let errors_init x n =
        BA_.init_struct_list ~data_words:0 ~pointer_words:5 x 0 n
      let errors_set_list x v =
        let builder = errors_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let errors_set_array x v =
        let builder = errors_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
    end
  end

  module Client = struct
  end

  module Service = struct
  end
  module MessageWrapper = MessageWrapper
end

module Make(M:Capnp.MessageSig.S) = MakeRPC(Capnp.RPC.None(M))
