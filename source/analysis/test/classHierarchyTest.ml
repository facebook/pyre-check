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

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

(* Butterfly:
 *  0 - 2
 *    X
 *  1 - 3 *)
let butterfly, butterfly_indices =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "0";
  insert order "1";
  insert order "2";
  insert order "3";
  connect order ~predecessor:"0" ~successor:"2";
  connect order ~predecessor:"0" ~successor:"3";
  connect order ~predecessor:"1" ~successor:"2";
  connect order ~predecessor:"1" ~successor:"3";
  handler order, Hash_set.to_list order.all_indices


(*          0 - 3
 *          |   |   \
 *          BOTTOM  - b - 1      TOP
 *          |  \       /
 *          4 -- 2 --- *)
let order, order_indices =
  let bottom = "bottom" in
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order bottom;
  insert order "0";
  insert order "1";
  insert order "2";
  insert order "3";
  insert order "4";
  insert order "5";
  connect order ~predecessor:"0" ~successor:"3";
  connect order ~predecessor:"1" ~successor:"3";
  connect order ~predecessor:"4" ~successor:"2";
  connect order ~predecessor:bottom ~successor:"0";
  connect order ~predecessor:bottom ~successor:"1";
  connect order ~predecessor:bottom ~successor:"2";
  connect order ~predecessor:bottom ~successor:"4";
  handler order, Hash_set.to_list order.all_indices


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
  connect order ~predecessor:"C" ~successor:"B";
  connect order ~predecessor:"C" ~successor:"A";
  handler order


let test_method_resolution_order_linearize _ =
  let assert_method_resolution_order (module Handler : Handler) annotation expected =
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      expected
      (method_resolution_order_linearize annotation ~get_successors:Handler.edges)
  in
  assert_method_resolution_order butterfly "3" ["3"];
  assert_method_resolution_order butterfly "0" ["0"; "3"; "2"];
  assert_method_resolution_order diamond_order "D" ["D"; "C"; "B"; "A"];

  (* The subclass gets chosen first even if after the superclass when both are inherited. *)
  assert_method_resolution_order triangle_order "C" ["C"; "B"; "A"]


let test_successors _ =
  (* Butterfly:
   *  0 - 2
   *    X
   *  1 - 3 *)
  assert_equal (successors butterfly "3") [];
  assert_equal (successors butterfly "0") ["3"; "2"];

  (*          0 - 3
   *          /   /   \
   *          BOTTOM - 1      TOP
   *          |  \       /
   *          4 -- 2 --- *)
  assert_equal (successors order "3") [];
  assert_equal (successors order "0") ["3"];
  assert_equal (successors order "bottom") ["4"; "2"; "1"; "0"; "3"]


let test_immediate_parents _ =
  assert_equal (immediate_parents butterfly "3") [];
  assert_equal (immediate_parents butterfly "0") ["3"; "2"];

  assert_equal (immediate_parents order "3") [];
  assert_equal (immediate_parents order "0") ["3"];
  assert_equal (immediate_parents order "bottom") ["4"; "2"; "1"; "0"];
  ()


let test_is_transitive_successor _ =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  let predecessor = "predecessor" in
  let successor = "successor" in
  insert order predecessor;
  insert order successor;
  connect order ~predecessor ~successor;

  let no_placeholder_subclasses_handler order =
    (module struct
      let edges = Hashtbl.find order.edges

      let extends_placeholder_stub _ = false

      let contains annotation = Hash_set.mem order.all_indices (IndexTracker.index annotation)
    end : ClassHierarchy.Handler)
  in
  let all_placeholder_subclasses_handler order =
    (module struct
      let edges = Hashtbl.find order.edges

      let extends_placeholder_stub _ = true

      let contains annotation = Hash_set.mem order.all_indices (IndexTracker.index annotation)
    end : ClassHierarchy.Handler)
  in
  assert_true
    (is_transitive_successor
       (no_placeholder_subclasses_handler order)
       ~source:predecessor
       ~target:successor);
  assert_false
    (is_transitive_successor
       (no_placeholder_subclasses_handler order)
       ~source:successor
       ~target:predecessor);
  assert_true
    (is_transitive_successor
       (all_placeholder_subclasses_handler order)
       ~source:successor
       ~target:predecessor);
  (* The flag disables the special-casing of placeholder stub subclasses. *)
  assert_false
    (is_transitive_successor
       ~placeholder_subclass_extends_all:false
       (all_placeholder_subclasses_handler order)
       ~source:successor
       ~target:predecessor);
  ()


let test_least_upper_bound _ =
  assert_equal (least_upper_bound order "3" "1") ["3"];
  assert_equal (least_upper_bound order "4" "bottom") ["4"];
  assert_equal (least_upper_bound order "0" "1") ["3"];
  assert_equal (least_upper_bound order "0" "2") [];
  assert_equal (least_upper_bound order "0" "2") [];
  assert_equal (least_upper_bound butterfly "0" "1") ["2"; "3"]


let test_check_integrity _ =
  let assert_raises_cyclic nodes expression =
    (* `assert_raises` doesn't work because it uses the polymorphic equality.
     * We implement our own equality check here. *)
    try
      let _ = expression () in
      assert_failure "expression did not raise an exception"
    with
    | Cyclic visited -> assert_equal ~cmp:Hash_set.equal visited (String.Hash_set.of_list nodes)
    | _ -> assert_failure "expression raised an unexpected exception"
  in

  check_integrity order ~indices:order_indices;
  check_integrity butterfly ~indices:butterfly_indices;

  (*(* 0 <-> 1 *)*)
  let order, indices =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "0";
    insert order "1";
    connect order ~predecessor:"0" ~successor:"1";
    connect order ~predecessor:"1" ~successor:"0";
    handler order, Hash_set.to_list order.all_indices
  in
  assert_raises_cyclic ["0"; "1"] (fun _ -> check_integrity order ~indices);
  assert_raises_cyclic ["0"; "1"] (fun _ ->
      let (module Handler : Handler) = order in
      method_resolution_order_linearize "1" ~get_successors:Handler.edges);

  (* 0 -> 1
   * ^    |
   *  \   v
   * .  - 2 -> 3 *)
  let order, indices =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "3";
    connect order ~predecessor:"0" ~successor:"1";
    connect order ~predecessor:"1" ~successor:"2";
    connect order ~predecessor:"2" ~successor:"0";
    connect order ~predecessor:"2" ~successor:"3";
    handler order, Hash_set.to_list order.all_indices
  in
  assert_raises_cyclic ["0"; "1"; "2"] (fun _ -> check_integrity order ~indices);
  assert_raises_cyclic ["0"; "1"; "2"] (fun _ ->
      let (module Handler : Handler) = order in
      method_resolution_order_linearize "2" ~get_successors:Handler.edges)


let test_to_dot _ =
  let order, keys =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "object";
    connect order ~predecessor:"0" ~successor:"2";
    connect order ~predecessor:"0" ~successor:"1" ~parameters:![Type.string];

    (*connect_annotations_to_object order ["0"; "1"; "2"; "object"];*)
    handler order, Hash_set.to_list order.all_indices
  in
  assert_equal
    ~printer:ident
    ({|
      digraph {
        343776663[label="object"]
        453441034[label="1"]
        564400327[label="2"]
        680650890[label="0"]
        680650890 -> 453441034[label="(str)"]
        680650890 -> 564400327
      }
    |}
    |> Test.trim_extra_indentation)
    ("\n" ^ to_dot order ~indices:keys)


let test_variables _ =
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let order =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    insert order "Tensor";
    connect order ~parameters:![Type.variable "T"] ~predecessor:"A" ~successor:"typing.Generic";
    connect
      order
      ~parameters:[Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)]
      ~predecessor:"Tensor"
      ~successor:"typing.Generic";
    handler order
  in
  let assert_variables ~expected source = assert_equal expected (variables order source) in
  assert_variables ~expected:None "B";
  assert_variables ~expected:(Some [Unary (Type.Variable.Unary.create "T")]) "A";

  assert_variables ~expected:None "Nonexistent";
  assert_variables
    ~expected:(Some [TupleVariadic (Type.Variable.Variadic.Tuple.create "Ts")])
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
  let variable_covariant = Type.variable "_T_co" ~variance:Covariant in
  insert order "typing.Generic";
  insert order "int";
  insert order "str";
  insert order "bool";
  insert order "list";
  connect order ~predecessor:"list" ~successor:"typing.Generic" ~parameters:![variable];

  insert order "typing.Iterator";
  connect order ~predecessor:"list" ~successor:"typing.Iterator" ~parameters:![variable];
  connect
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Generic"
    ~parameters:![variable_covariant];
  insert order "typing.Iterable";
  connect
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Iterable"
    ~parameters:![variable_covariant];
  connect
    order
    ~predecessor:"typing.Iterable"
    ~successor:"typing.Generic"
    ~parameters:![variable_covariant];
  connect order ~predecessor:"list" ~successor:"typing.Iterable" ~parameters:![variable];
  insert order "tuple";
  connect order ~predecessor:"tuple" ~successor:"typing.Iterator" ~parameters:![variable];
  connect order ~predecessor:"tuple" ~successor:"typing.Generic" ~parameters:![variable];
  insert order "str";
  connect order ~predecessor:"str" ~successor:"typing.Iterable" ~parameters:![Type.Primitive "str"];
  insert order "AnyIterable";
  connect order ~predecessor:"AnyIterable" ~successor:"typing.Iterable";
  insert order "dict";
  connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Generic"
    ~parameters:![variable; other_variable];
  connect order ~predecessor:"dict" ~successor:"typing.Iterator" ~parameters:![variable];
  insert order "PartiallySpecifiedDict";
  connect
    order
    ~predecessor:"PartiallySpecifiedDict"
    ~successor:"dict"
    ~parameters:![Type.Primitive "int"];
  insert order "OverSpecifiedDict";
  connect
    order
    ~predecessor:"OverSpecifiedDict"
    ~successor:"dict"
    ~parameters:![Type.Primitive "int"; Primitive "int"; Primitive "str"];
  insert order "GenericContainer";
  connect
    order
    ~predecessor:"GenericContainer"
    ~successor:"typing.Generic"
    ~parameters:![variable; other_variable];

  insert order "NonGenericContainerChild";
  connect
    order
    ~predecessor:"NonGenericContainerChild"
    ~successor:"GenericContainer"
    ~parameters:![Type.Primitive "int"; Primitive "str"];
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
    ~parameters:
      [
        CallableParameters
          (Type.Variable.Variadic.Parameters.self_reference
             (Type.Variable.Variadic.Parameters.create "TParams"));
      ];
  insert order "ChildClassParametricOnParamSpec";
  connect
    order
    ~predecessor:"ChildClassParametricOnParamSpec"
    ~successor:"ClassParametricOnParamSpec"
    ~parameters:
      [
        CallableParameters
          (Type.Variable.Variadic.Parameters.self_reference
             (Type.Variable.Variadic.Parameters.create "TParams"));
      ];
  connect
    order
    ~predecessor:"ChildClassParametricOnParamSpec"
    ~successor:"typing.Generic"
    ~parameters:
      [
        CallableParameters
          (Type.Variable.Variadic.Parameters.self_reference
             (Type.Variable.Variadic.Parameters.create "TParams"));
      ];
  insert order "ConcreteChildClassParametricOnParamSpec";
  connect
    order
    ~predecessor:"ConcreteChildClassParametricOnParamSpec"
    ~successor:"ClassParametricOnParamSpec"
    ~parameters:[Single Type.integer];

  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic_parameter =
    Type.Parameter.Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)
  in
  insert order "Base";
  connect order ~predecessor:"Base" ~successor:"typing.Generic" ~parameters:[variadic_parameter];

  insert order "Child";
  connect order ~predecessor:"Child" ~successor:"typing.Generic" ~parameters:[variadic_parameter];
  connect order ~predecessor:"Child" ~successor:"Base" ~parameters:[variadic_parameter];

  insert order "DTypedTensor";
  connect
    order
    ~predecessor:"DTypedTensor"
    ~successor:"typing.Generic"
    ~parameters:[Single (Type.Variable (Type.Variable.Unary.create "DType")); variadic_parameter];
  insert order "IntTensor";
  connect
    order
    ~predecessor:"IntTensor"
    ~successor:"typing.Generic"
    ~parameters:[variadic_parameter];
  connect
    order
    ~predecessor:"IntTensor"
    ~successor:"DTypedTensor"
    ~parameters:[Single Type.integer; variadic_parameter];
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
    assert_equal expected actual ~printer:[%show: Type.Parameter.t list option]
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
  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  let variadic_parameter =
    Type.Parameter.Unpacked (Type.OrderedTypes.Concatenation.create_unpackable variadic)
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
         "check_integrity" >:: test_check_integrity;
         "is_instantiated" >:: test_is_instantiated;
         "least_upper_bound" >:: test_least_upper_bound;
         "successors" >:: test_successors;
         "immediate_parents" >:: test_immediate_parents;
         "is_transitive_successor" >:: test_is_transitive_successor;
         "to_dot" >:: test_to_dot;
         "variables" >:: test_variables;
         "instantiate_successors_parameters" >:: test_instantiate_successors_parameters;
         "method_resolution_order_linearize" >:: test_method_resolution_order_linearize;
       ]
  |> Test.run
