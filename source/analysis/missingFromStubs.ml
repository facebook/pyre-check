(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Expression

let missing_builtin_globals =
  let assign name annotation =
    {
      UnannotatedGlobal.Collector.Result.name;
      unannotated_global =
        UnannotatedGlobal.SimpleAssign
          {
            explicit_annotation = Some (Type.expression annotation);
            target_location = Location.WithModule.any;
            value = Node.create_with_default_location (Expression.Constant Constant.Ellipsis);
          };
    }
  in
  [assign "..." Type.Any; assign "__debug__" Type.bool]


let make ?(bases = []) ?(metaclasses = []) ?(body = []) name =
  let create_base annotation = { Call.Argument.name = None; value = Type.expression annotation } in
  let create_metaclass annotation =
    {
      Call.Argument.name = Some (Node.create_with_default_location "metaclass");
      value = Type.expression annotation;
    }
  in
  {
    Class.name = Reference.create name;
    base_arguments = List.map bases ~f:create_base @ List.map metaclasses ~f:create_metaclass;
    body;
    decorators = [];
    top_level_unbound_names = [];
  }
  |> Node.create_with_default_location


let single_unary_generic =
  [Type.parametric "typing.Generic" [Single (Variable (Type.Variable.Unary.create "typing._T"))]]


let catch_all_generic = [Type.parametric "typing.Generic" [Single Any]]

let callable_body =
  [
    Statement.Assign
      {
        target =
          Node.create_with_default_location
            (Expression.Name
               (Ast.Expression.create_name ~location:Location.any "typing.Callable.__call__"));
        annotation = Some (Type.expression Type.object_primitive);
        value = Node.create_with_default_location (Expression.Constant Constant.NoneLiteral);
      };
  ]
  |> List.map ~f:Node.create_with_default_location


let make_dunder_get ~parent ~host ~host_type ~return =
  let parent = Reference.create parent in
  Statement.Define
    {
      signature =
        {
          name = Reference.combine parent (Reference.create "__get__");
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
          parent = Some parent;
          nesting_define = None;
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
  [
    make_dunder_get
      ~parent:"typing.ClassMethod"
      ~host:Type.object_primitive
      ~host_type:(Variable (Type.Variable.Unary.create "typing._S"))
      ~return:
        (Type.parametric
           "BoundMethod"
           [
             Single (Variable (Type.Variable.Unary.create "typing._T"));
             Single (Variable (Type.Variable.Unary.create "typing._S"));
           ]);
  ]
  |> List.map ~f:Node.create_with_default_location


let staticmethod_body =
  (*
   * _T = TypeVar("_T")
   * class StaticMethod(Generic[_T]):
   *   def __get__(self, host: object, host_type: object = None) -> _T: ...
   *)
  [
    make_dunder_get
      ~parent:"typing.StaticMethod"
      ~host:Type.object_primitive
      ~host_type:Type.object_primitive
      ~return:(Variable (Type.Variable.Unary.create "typing._T"));
  ]
  |> List.map ~f:Node.create_with_default_location


let generic_meta_body =
  [
    Statement.Define
      {
        signature =
          {
            name = Reference.create "typing.GenericMeta.__getitem__";
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
            parent = Some (Reference.create "typing.GenericMeta");
            nesting_define = None;
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
  [
    make
      ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
      ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
      (Type.TypedDictionary.class_name ~total:true);
    make
      ~bases:[Type.parametric "typing.Mapping" [Single Type.string; Single Type.object_primitive]]
      ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
      (Type.TypedDictionary.class_name ~total:false);
    (* I think this may be actually covariant, covariant, but I don't think there's any value in
       going out on that limb yet *)
    make
      ~bases:
        [
          Type.parametric
            "typing.Generic"
            [Single (Type.variable "typing._T"); Single (Type.variable "typing._S")];
          Type.Primitive "typing.Callable";
        ]
      "BoundMethod";
  ]


let missing_typing_classes =
  [
    make "typing.Optional" ~bases:single_unary_generic;
    make "typing.NoReturn";
    make "typing.Annotated" ~bases:catch_all_generic;
    make "typing.Protocol" ~bases:catch_all_generic;
    make "typing.Callable" ~bases:catch_all_generic ~body:callable_body;
    make "typing.FrozenSet" ~bases:single_unary_generic;
    make "typing.ClassVar" ~bases:single_unary_generic;
    make "typing.Final" ~bases:catch_all_generic;
    make "typing.Literal" ~bases:catch_all_generic;
    make "typing.Union" ~bases:catch_all_generic;
    make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic";
    make "typing.ClassMethod" ~bases:single_unary_generic ~body:classmethod_body;
    make "typing.StaticMethod" ~bases:single_unary_generic ~body:staticmethod_body;
    make "typing.GenericMeta" ~bases:[Primitive "type"] ~body:generic_meta_body;
    make "typing.TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    make "typing.Required" ~bases:single_unary_generic;
    make "typing.NotRequired" ~bases:single_unary_generic;
  ]


let missing_typing_extensions_classes =
  [
    make "typing_extensions.Final";
    make "typing_extensions.Literal" ~bases:catch_all_generic;
    make "typing_extensions.Annotated" ~bases:catch_all_generic;
    make "typing_extensions.TypeAlias";
    make "typing_extensions.TypeGuard" ~bases:(Type.bool :: single_unary_generic);
    make "typing_extensions.Required" ~bases:single_unary_generic;
    make "typing_extensions.NotRequired" ~bases:single_unary_generic;
  ]
