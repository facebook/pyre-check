(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* MissingTypeStubs:
 *
 * This module is mostly a data module that fills in the gaps from missing typeshed stubs.
 * It adds type lattice and attribute information form for values annotated as
 * `x: _SpecialForm = ...` in typeshed, and is used by the UnannotatedGlobalEnvironment.
 * Implementers need to write raw qualified ASTs.
 *)

open Core
open Ast
open Statement
open Expression

let make_class ?(bases = []) ?(metaclasses = []) ?(body = []) ~in_module name =
  let name = Reference.combine in_module (Reference.create name) in
  let create_base annotation = { Call.Argument.name = None; value = Type.expression annotation } in
  let create_metaclass annotation =
    {
      Call.Argument.name = Some (Node.create_with_default_location "metaclass");
      value = Type.expression annotation;
    }
  in
  {
    Class.name;
    base_arguments = List.map bases ~f:create_base @ List.map metaclasses ~f:create_metaclass;
    parent = NestingContext.create_toplevel ();
    body;
    decorators = [];
    top_level_unbound_names = [];
    type_params = [];
  }
  |> Node.create_with_default_location


let single_unary_generic =
  [
    Type.parametric "typing.Generic" [Single (Variable (Type.Variable.TypeVar.create "typing._T"))];
    Type.Primitive "_SpecialForm";
  ]


let catch_all_generic =
  [Type.parametric "typing.Generic" [Single Any]; Type.Primitive "_SpecialForm"]


let callable_body =
  [
    Statement.Assign
      {
        target =
          Node.create_with_default_location
            (Expression.Name
               (Ast.Expression.create_name
                  ~location:Location.any
                  ~create_origin:(fun _ -> None)
                  "typing.Callable.__call__"));
        annotation = Some (Type.expression Type.object_primitive);
        value = Some (Node.create_with_default_location (Expression.Constant Constant.NoneLiteral));
      };
    Statement.Assign
      {
        target =
          Node.create_with_default_location
            (Expression.Name
               (Ast.Expression.create_name
                  ~location:Location.any
                  ~create_origin:(fun _ -> None)
                  "typing.Callable.__qualname__"));
        annotation = Some (Type.expression Type.string);
        value =
          Some
            (Node.create_with_default_location
               (Expression.Constant
                  (Constant.String { StringLiteral.kind = StringLiteral.String; value = "" })));
      };
  ]
  |> List.map ~f:Node.create_with_default_location


let make_dunder_get ~in_module ~parent ~host ~host_type ~return =
  let prefix = NestingContext.to_qualifier ~module_name:in_module parent in
  Statement.Define
    {
      signature =
        {
          name = Reference.create ~prefix "__get__";
          parameters =
            [
              Node.create_with_default_location
                { Ast.Expression.Parameter.name = "self"; value = None; annotation = None };
              Node.create_with_default_location
                {
                  Ast.Expression.Parameter.name = "host";
                  value = None;
                  annotation = Some (Type.expression host);
                };
              Node.create_with_default_location
                {
                  Ast.Expression.Parameter.name = "host_type";
                  value =
                    Some
                      (Node.create_with_default_location (Expression.Constant Constant.NoneLiteral));
                  annotation = Some (Type.expression host_type);
                };
            ];
          decorators = [];
          return_annotation = Some (Type.expression return);
          async = false;
          generator = false;
          parent;
          legacy_parent = Some prefix;
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [];
    }


let classmethod_body =
  (*
   * _T = TypeVar("_T")
   * _S = TypeVar("_S")
   * class ClassMethod(Generic[_T]):
   *   def __get__(self, host: object, host_type: _S = None) -> BoundMethod[_T, _S]: ...
   *)
  let parent = NestingContext.(create_class ~parent:(create_toplevel ()) "ClassMethod") in
  [
    make_dunder_get
      ~in_module:(Reference.create "typing")
      ~parent
      ~host:Type.object_primitive
      ~host_type:(Variable (Type.Variable.TypeVar.create "typing._S"))
      ~return:
        (Type.parametric
           "BoundMethod"
           [
             Single (Variable (Type.Variable.TypeVar.create "typing._T"));
             Single (Variable (Type.Variable.TypeVar.create "typing._S"));
           ]);
  ]
  |> List.map ~f:Node.create_with_default_location


let staticmethod_body =
  (*
   * _T = TypeVar("_T")
   * class StaticMethod(Generic[_T]):
   *   def __get__(self, host: object, host_type: object = None) -> _T: ...
   *)
  let parent = NestingContext.(create_class ~parent:(create_toplevel ()) "StaticMethod") in
  [
    make_dunder_get
      ~in_module:(Reference.create "typing")
      ~parent
      ~host:Type.object_primitive
      ~host_type:Type.object_primitive
      ~return:(Variable (Type.Variable.TypeVar.create "typing._T"));
  ]
  |> List.map ~f:Node.create_with_default_location


let generic_meta_body =
  let module_name = Reference.create "typing" in
  let parent = NestingContext.(create_class ~parent:(create_toplevel ()) "GenericMeta") in
  let prefix = NestingContext.to_qualifier ~module_name parent in
  [
    Statement.Define
      {
        signature =
          {
            name = Reference.create ~prefix "__getitem__";
            parameters =
              [
                { Parameter.name = "cls"; value = None; annotation = None }
                |> Node.create_with_default_location;
                { Parameter.name = "arg"; value = None; annotation = None }
                |> Node.create_with_default_location;
              ];
            decorators = [];
            return_annotation = None;
            async = false;
            generator = false;
            parent;
            legacy_parent = Some prefix;
            type_params = [];
          };
        captures = [];
        unbound_names = [];
        body = [];
      }
    |> Node.create_with_default_location;
  ]


let missing_builtin_classes =
  let t_self_expression =
    Expression.Name (Name.Identifier "TSelf") |> Node.create_with_default_location
  in
  let in_module = Reference.empty in
  [
    make_class
      ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
      ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
      ~in_module
      (Type.TypedDictionary.class_name ~total:true);
    make_class
      ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
      ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
      ~in_module
      (Type.TypedDictionary.class_name ~total:false);
    (* I think this may be actually covariant, covariant, but I don't think there's any value in
       going out on that limb yet *)
    make_class
      ~bases:
        [
          Type.Primitive "typing.Callable";
          Type.parametric
            "typing.Generic"
            [Single (Type.variable "typing._T"); Single (Type.variable "typing._S")];
        ]
      ~in_module
      "BoundMethod";
  ]


let missing_typing_classes =
  let in_module = Reference.create "typing" in
  [
    make_class ~in_module "Optional" ~bases:single_unary_generic;
    make_class ~in_module "NoReturn";
    make_class ~in_module "Never";
    (* Although~in_module Any is represented by a special variant in our type system, we still
       create a definiti~in_module on for it so it can be used as a base class *)
    make_class ~in_module "Any";
    make_class ~in_module "Annotated" ~bases:catch_all_generic;
    make_class ~in_module "Protocol" ~bases:catch_all_generic;
    make_class ~in_module "Callable" ~bases:catch_all_generic ~body:callable_body;
    make_class ~in_module "FrozenSet" ~bases:single_unary_generic;
    make_class ~in_module "ClassVar" ~bases:single_unary_generic;
    make_class ~in_module "Final" ~bases:catch_all_generic;
    make_class ~in_module "Literal" ~bases:catch_all_generic;
    make_class ~in_module "Union" ~bases:catch_all_generic;
    make_class ~in_module "Generic" ~metaclasses:[Primitive "typing.GenericMeta"];
    make_class ~in_module "ClassMethod" ~bases:single_unary_generic ~body:classmethod_body;
    make_class ~in_module "StaticMethod" ~bases:single_unary_generic ~body:staticmethod_body;
    make_class ~in_module "GenericMeta" ~bases:[Primitive "type"] ~body:generic_meta_body;
    make_class ~in_module "TypeAlias";
    (* Note: ReadOnly is actually covariant. *)
    make_class ~in_module "ReadOnly" ~bases:single_unary_generic;
    make_class ~in_module "Required" ~bases:single_unary_generic;
    make_class ~in_module "NotRequired" ~bases:single_unary_generic;
    (* Note: TypeGuard is actually covariant; we hardcode this in AttributeResolution. *)
    make_class ~in_module "TypeIs" ~bases:(Type.bool :: single_unary_generic);
    make_class ~in_module "TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    make_class ~in_module "Unpack" ~bases:single_unary_generic;
  ]


let missing_typing_extensions_classes =
  let in_module = Reference.create "typing_extensions" in
  [
    make_class ~in_module "Final";
    make_class ~in_module "Literal" ~bases:catch_all_generic;
    make_class ~in_module "Annotated" ~bases:catch_all_generic;
    make_class ~in_module "TypeAlias";
    make_class ~in_module "Never";
    (* Note: ReadOnly is actually covariant. *)
    make_class ~in_module "ReadOnly" ~bases:single_unary_generic;
    make_class ~in_module "Required" ~bases:single_unary_generic;
    make_class ~in_module "NotRequired" ~bases:single_unary_generic;
    (* Note: TypeGuard is actually covariant; we hardcode this in AttributeResolution. *)
    make_class ~in_module "TypeIs" ~bases:(Type.bool :: single_unary_generic);
    make_class ~in_module "TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    make_class ~in_module "Unpack" ~bases:single_unary_generic;
  ]
