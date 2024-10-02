(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test
open ClassHierarchy

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Argument.Single single)

let assert_method_resolution_order (module Handler : Handler) annotation expected =
  let get_parents = ClassHierarchy.parents_of (module Handler) in
  assert_equal
    ~cmp:[%compare.equal: string list]
    ~printer:(fun names -> [%sexp_of: string list] names |> Sexp.to_string_hum)
    expected
    (method_resolution_order_linearize annotation ~get_parents |> Result.ok |> Option.value_exn)


(* Test matching the example in
 * https://web.archive.org/web/20241001181736/https://en.wikipedia.org/wiki/C3_linearization
 *
 * This is a convenient test since the article walks through algorithm execution in detail.
 *
 * class O
 * class A extends O
 * class B extends O
 * class C extends O
 * class D extends O
 * class E extends O
 * class K1 extends C, A, B
 * class K2 extends B, D, E
 * class K3 extends A, D
 * class Z extends K1, K3, K2
 *
 * MRO(Z) = [Z, K1, C, K3, A, K2, B, D, E, O]
 *
 * (Note: the order of `connect` is reversed relative to source code order because
 * it prepends)
 *)
let test_wikipedia_example _ =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "O";
  insert order "A";
  connect order ~predecessor:"A" ~successor:"O";
  insert order "B";
  connect order ~predecessor:"B" ~successor:"O";
  insert order "C";
  connect order ~predecessor:"C" ~successor:"O";
  insert order "D";
  connect order ~predecessor:"D" ~successor:"O";
  insert order "E";
  connect order ~predecessor:"E" ~successor:"O";
  insert order "K1";
  connect order ~predecessor:"K1" ~successor:"B";
  connect order ~predecessor:"K1" ~successor:"A";
  connect order ~predecessor:"K1" ~successor:"C";
  insert order "K2";
  connect order ~predecessor:"K2" ~successor:"E";
  connect order ~predecessor:"K2" ~successor:"D";
  connect order ~predecessor:"K2" ~successor:"B";
  insert order "K3";
  connect order ~predecessor:"K3" ~successor:"D";
  connect order ~predecessor:"K3" ~successor:"A";
  insert order "Z";
  connect order ~predecessor:"Z" ~successor:"K2";
  connect order ~predecessor:"Z" ~successor:"K3";
  connect order ~predecessor:"Z" ~successor:"K1";
  assert_method_resolution_order
    (handler order)
    "Z"
    ["Z"; "K1"; "C"; "K3"; "A"; "K2"; "B"; "D"; "E"; "O"]


(* Butterfly:
 *
 * (top / superclasses)
 *
 *  B - D 
 *    X
 *  A - C
 * 
 * (bottom / subclasses)
 *)
let butterfly, butterfly_class_names =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  connect order ~predecessor:"A" ~successor:"C";
  connect order ~predecessor:"A" ~successor:"D";
  connect order ~predecessor:"B" ~successor:"C";
  connect order ~predecessor:"B" ~successor:"D";
  handler order, Hash_set.to_list order.all_class_names


(*          A -- D
 *          |       \
 *    bottom --------- B     top
 *          |  \    /
 *          E -- C *)
let complex_order, complex_order_class_names =
  let bottom = "bottom" in
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order bottom;
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  insert order "E";
  insert order "top";
  connect order ~predecessor:"A" ~successor:"D";
  connect order ~predecessor:"B" ~successor:"D";
  connect order ~predecessor:"E" ~successor:"C";
  connect order ~predecessor:bottom ~successor:"A";
  connect order ~predecessor:bottom ~successor:"B";
  connect order ~predecessor:bottom ~successor:"C";
  connect order ~predecessor:bottom ~successor:"E";
  handler order, Hash_set.to_list order.all_class_names


let diamond_order =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  connect order ~predecessor:"D" ~successor:"B";
  connect order ~predecessor:"D" ~successor:"C";
  connect order ~predecessor:"B" ~successor:"A";
  connect order ~predecessor:"C" ~successor:"A";
  handler order


(*
 *   TOP
 *    |
 *    A
 *   /|
 *  B |
 *   \|
 *    C
 *    |
 * BOTTOM
 *)
let triangle_order =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "A";
  insert order "B";
  insert order "C";
  connect order ~predecessor:"B" ~successor:"A";
  (* NOTE: The C->A edge needs to be inserted before the C->B edge. If the order gets reversed,
     we'll get a inconsistent MRO issue. *)
  connect order ~predecessor:"C" ~successor:"A";
  connect order ~predecessor:"C" ~successor:"B";
  handler order


let test_method_resolution_order_linearize _ =
  assert_method_resolution_order butterfly "D" ["D"];
  assert_method_resolution_order butterfly "A" ["A"; "D"; "C"];
  assert_method_resolution_order diamond_order "D" ["D"; "C"; "B"; "A"];

  (* The subclass gets chosen first even if after the superclass when both are inherited. *)
  assert_method_resolution_order triangle_order "C" ["C"; "B"; "A"]


let test_immediate_parents _ =
  assert_equal (immediate_parents butterfly "D") [];
  assert_equal (immediate_parents butterfly "A") ["D"; "C"];

  assert_equal (immediate_parents complex_order "D") [];
  assert_equal (immediate_parents complex_order "A") ["D"];
  assert_equal (immediate_parents complex_order "bottom") ["E"; "C"; "B"; "A"];
  ()


let test_check_integrity _ =
  let assert_ok ~class_names order =
    match check_integrity ~class_names order with
    | Result.Ok _ -> ()
    | Result.Error _ -> assert_failure "unexpected failure"
  in
  let assert_cyclic ~class_names order =
    match check_integrity ~class_names order with
    | Result.Error (ClassHierarchy.CheckIntegrityError.Cyclic _) -> ()
    | _ -> assert_failure "expected a cyclic error but did not get one"
  in
  let assert_incomplete ~class_names order =
    match check_integrity ~class_names order with
    | Result.Error (ClassHierarchy.CheckIntegrityError.Incomplete _) -> ()
    | _ -> assert_failure "expected a cyclic error but did not get one"
  in

  assert_ok complex_order ~class_names:complex_order_class_names;
  assert_ok butterfly ~class_names:butterfly_class_names;

  (* A <-> B *)
  let order, class_names =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "A";
    insert order "B";
    connect order ~predecessor:"A" ~successor:"B";
    connect order ~predecessor:"B" ~successor:"A";
    handler order, Hash_set.to_list order.all_class_names
  in
  assert_cyclic order ~class_names;

  (* A -> B
   * ^    |
   *  \   v
   * .  - C -> D *)
  let order, class_names =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "A";
    insert order "B";
    insert order "C";
    insert order "D";
    connect order ~predecessor:"A" ~successor:"B";
    connect order ~predecessor:"B" ~successor:"C";
    connect order ~predecessor:"C" ~successor:"A";
    connect order ~predecessor:"C" ~successor:"D";
    handler order, Hash_set.to_list order.all_class_names
  in
  assert_cyclic order ~class_names;

  let order, class_names =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "A";
    handler order, ["B"]
  in
  assert_incomplete order ~class_names;
  ()


let test_to_dot _ =
  let order, keys =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "A";
    insert order "B";
    insert order "C";
    insert order "object";
    connect order ~predecessor:"A" ~successor:"C";
    connect order ~predecessor:"A" ~successor:"B" ~arguments:![Type.string];

    handler order, Hash_set.to_list order.all_class_names
  in
  assert_equal
    ~printer:Fn.id
    ({|
      digraph {
        A[label="A"]
        B[label="B"]
        C[label="C"]
        object[label="object"]
        A -> B[label="(str)"]
        A -> C
      }
    |}
    |> Test.trim_extra_indentation)
    ("\n" ^ to_dot order ~class_names:keys)


let test_generic_parameters_as_variables _ =
  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
  let order =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    insert order "Tensor";
    connect order ~arguments:![Type.variable "T"] ~predecessor:"A" ~successor:"typing.Generic";
    connect
      order
      ~arguments:[Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)]
      ~predecessor:"Tensor"
      ~successor:"typing.Generic";
    handler order
  in
  let assert_generic_parameters_as_variables ~expected source =
    assert_equal expected (generic_parameters_as_variables order source)
  in
  assert_generic_parameters_as_variables ~expected:None "B";
  assert_generic_parameters_as_variables
    ~expected:(Some [TypeVarVariable (Type.Variable.TypeVar.create "T")])
    "A";

  assert_generic_parameters_as_variables ~expected:None "Nonexistent";
  assert_generic_parameters_as_variables
    ~expected:(Some [TypeVarTupleVariable (Type.Variable.TypeVarTuple.create "Ts")])
    "Tensor";
  ()


let test_is_instantiated _ =
  let order =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    handler order
  in
  assert_true (is_instantiated order (Type.Primitive "A"));
  assert_true (is_instantiated order (Type.Primitive "B"));
  assert_false (is_instantiated order (Type.Primitive "C"));
  assert_true (is_instantiated order (Type.parametric "A" ![Type.Primitive "B"]));
  assert_true (is_instantiated order (Type.parametric "A" ![Type.Primitive "A"]));
  assert_true
    (is_instantiated order (Type.parametric "A" ![Type.Primitive "A"; Type.Primitive "B"]));
  assert_false
    (is_instantiated order (Type.parametric "A" ![Type.Primitive "C"; Type.Primitive "B"]));
  assert_false
    (is_instantiated order (Type.parametric "C" ![Type.Primitive "A"; Type.Primitive "B"]))


let parametric_order_base =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  let variable = Type.variable "_T" in
  let other_variable = Type.variable "_T2" in
  let variable_covariant = Type.variable "_T_co" in
  insert order "typing.Generic";
  insert order "int";
  insert order "str";
  insert order "bool";
  insert order "list";
  connect order ~predecessor:"list" ~successor:"typing.Generic" ~arguments:![variable];

  insert order "typing.Iterator";
  connect order ~predecessor:"list" ~successor:"typing.Iterator" ~arguments:![variable];
  connect_with_variance
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Generic"
    ~arguments_with_variances:
      [Type.Argument.Single variable_covariant, Type.Record.PreInferenceVariance.P_Covariant];

  insert order "typing.Iterable";
  connect_with_variance
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Iterable"
    ~arguments_with_variances:
      [Type.Argument.Single variable_covariant, Type.Record.PreInferenceVariance.P_Covariant];
  connect_with_variance
    order
    ~predecessor:"typing.Iterable"
    ~successor:"typing.Generic"
    ~arguments_with_variances:
      [Type.Argument.Single variable_covariant, Type.Record.PreInferenceVariance.P_Covariant];
  connect order ~predecessor:"list" ~successor:"typing.Iterable" ~arguments:![variable];
  insert order "tuple";
  connect order ~predecessor:"tuple" ~successor:"typing.Iterator" ~arguments:![variable];
  connect order ~predecessor:"tuple" ~successor:"typing.Generic" ~arguments:![variable];
  insert order "str";
  connect order ~predecessor:"str" ~successor:"typing.Iterable" ~arguments:![Type.Primitive "str"];
  insert order "AnyIterable";
  connect order ~predecessor:"AnyIterable" ~successor:"typing.Iterable";
  insert order "dict";
  connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Generic"
    ~arguments:![variable; other_variable];
  connect order ~predecessor:"dict" ~successor:"typing.Iterator" ~arguments:![variable];
  insert order "PartiallySpecifiedDict";
  connect
    order
    ~predecessor:"PartiallySpecifiedDict"
    ~successor:"dict"
    ~arguments:![Type.Primitive "int"];
  insert order "OverSpecifiedDict";
  connect
    order
    ~predecessor:"OverSpecifiedDict"
    ~successor:"dict"
    ~arguments:![Type.Primitive "int"; Primitive "int"; Primitive "str"];
  insert order "GenericContainer";
  connect
    order
    ~predecessor:"GenericContainer"
    ~successor:"typing.Generic"
    ~arguments:![variable; other_variable];

  insert order "NonGenericContainerChild";
  connect
    order
    ~predecessor:"NonGenericContainerChild"
    ~successor:"GenericContainer"
    ~arguments:![Type.Primitive "int"; Primitive "str"];
  order


let parametric_order = MockClassHierarchyHandler.handler parametric_order_base

let variadic_order =
  let order = parametric_order_base in
  let open MockClassHierarchyHandler in
  insert order "ClassParametricOnParamSpec";
  connect
    order
    ~predecessor:"ClassParametricOnParamSpec"
    ~successor:"typing.Generic"
    ~arguments:
      [
        CallableParameters
          (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
      ];
  insert order "ChildClassParametricOnParamSpec";
  connect
    order
    ~predecessor:"ChildClassParametricOnParamSpec"
    ~successor:"ClassParametricOnParamSpec"
    ~arguments:
      [
        CallableParameters
          (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
      ];
  connect
    order
    ~predecessor:"ChildClassParametricOnParamSpec"
    ~successor:"typing.Generic"
    ~arguments:
      [
        CallableParameters
          (Type.Variable.ParamSpec.self_reference (Type.Variable.ParamSpec.create "TParams"));
      ];
  insert order "ConcreteChildClassParametricOnParamSpec";
  connect
    order
    ~predecessor:"ConcreteChildClassParametricOnParamSpec"
    ~successor:"ClassParametricOnParamSpec"
    ~arguments:[Single Type.integer];

  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
  let variadic_parameter =
    Type.Argument.Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)
  in
  insert order "Base";
  connect order ~predecessor:"Base" ~successor:"typing.Generic" ~arguments:[variadic_parameter];

  insert order "Child";
  connect order ~predecessor:"Child" ~successor:"typing.Generic" ~arguments:[variadic_parameter];
  connect order ~predecessor:"Child" ~successor:"Base" ~arguments:[variadic_parameter];

  insert order "DTypedTensor";
  connect
    order
    ~predecessor:"DTypedTensor"
    ~successor:"typing.Generic"
    ~arguments:[Single (Type.Variable (Type.Variable.TypeVar.create "DType")); variadic_parameter];
  insert order "IntTensor";
  connect order ~predecessor:"IntTensor" ~successor:"typing.Generic" ~arguments:[variadic_parameter];
  connect
    order
    ~predecessor:"IntTensor"
    ~successor:"DTypedTensor"
    ~arguments:[Single Type.integer; variadic_parameter];
  handler order


let test_instantiate_successors_parameters _ =
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:(Type.list Type.string)
       ~target:"typing.Iterator")
    (Some ![Type.string]);
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:(Type.dictionary ~key:Type.integer ~value:Type.string)
       ~target:"typing.Iterator")
    (Some ![Type.integer]);
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:Type.string
       ~target:"typing.Iterable")
    (Some ![Type.string]);
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:(Type.tuple [Type.integer; Type.string])
       ~target:"typing.Iterable")
    (Some ![Type.union [Type.integer; Type.string]]);
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:(Type.tuple [Type.literal_integer 1])
       ~target:"typing.Iterable")
    (Some ![Type.integer]);
  let ( !! ) name = Type.Primitive name in
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:!!"AnyIterable"
       ~target:"typing.Iterable")
    (Some ![Type.Any]);

  (* If you're not completely specified, fill all with anys *)
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:!!"PartiallySpecifiedDict"
       ~target:"dict")
    (Some ![Type.Any; Type.Any]);

  (* If you're over-specified, fill all with anys *)
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:!!"OverSpecifiedDict"
       ~target:"dict")
    (Some ![Type.Any; Type.Any]);

  (* Don't do a search when starting from bottom *)
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:!!"NonGenericContainerChild"
       ~target:"GenericContainer")
    (Some ![Type.integer; Type.string]);
  assert_equal
    (instantiate_successors_parameters
       parametric_order
       ~source:Type.Bottom
       ~target:"GenericContainer")
    (Some ![Type.Any; Type.Any]);

  let assert_equal actual expected =
    assert_equal expected actual ~printer:[%show: Type.Argument.t list option]
  in
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:
         (Type.parametric
            "ChildClassParametricOnParamSpec"
            [
              CallableParameters
                (Defined [Named { name = "p"; annotation = Type.integer; default = false }]);
            ])
       ~target:"ClassParametricOnParamSpec")
    (Some
       [
         CallableParameters
           (Defined [Named { name = "p"; annotation = Type.integer; default = false }]);
       ]);
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:(Primitive "ConcreteChildClassParametricOnParamSpec")
       ~target:"ClassParametricOnParamSpec")
    (Some
       [
         CallableParameters
           (Defined [PositionalOnly { index = 0; annotation = Type.integer; default = false }]);
       ]);
  assert_equal
    (instantiate_successors_parameters variadic_order ~source:Type.Bottom ~target:"Base")
    (Some [Unpacked (Type.OrderedTypes.Concatenation.create_unbounded_unpackable Type.Any)]);
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "Child" ![Type.integer; Type.string; Type.bool])
       ~target:"Base")
    (Some ![Type.integer; Type.string; Type.bool]);
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "IntTensor" ![Type.literal_integer 1; Type.literal_integer 2])
       ~target:"DTypedTensor")
    (Some ![Type.integer; Type.literal_integer 1; Type.literal_integer 2]);
  let variadic = Type.Variable.TypeVarTuple.create "Ts" in
  let variadic_parameter =
    Type.Argument.Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)
  in
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:
         (Type.parametric
            "IntTensor"
            [Single (Type.literal_integer 1); variadic_parameter; Single (Type.literal_integer 2)])
       ~target:"DTypedTensor")
    (Some
       [
         Single Type.integer;
         Single (Type.literal_integer 1);
         variadic_parameter;
         Single (Type.literal_integer 2);
       ]);
  assert_equal
    (instantiate_successors_parameters
       variadic_order
       ~source:
         (Type.Tuple
            (Concatenation (Type.OrderedTypes.Concatenation.create ~prefix:[Type.integer] variadic)))
       ~target:"typing.Iterable")
    (Some [Single Type.object_primitive]);
  ()


let () =
  "order"
  >::: [
         "wikipedia_example" >:: test_wikipedia_example;
         "check_integrity" >:: test_check_integrity;
         "is_instantiated" >:: test_is_instantiated;
         "immediate_parents" >:: test_immediate_parents;
         "to_dot" >:: test_to_dot;
         "generic_parameters_as_variables" >:: test_generic_parameters_as_variables;
         "instantiate_successors_parameters" >:: test_instantiate_successors_parameters;
         "method_resolution_order_linearize" >:: test_method_resolution_order_linearize;
       ]
  |> Test.run
