open Core
open Pyre
open OUnit2
open Analysis
open Test
open ClassHierarchy

let connect ?(parameters = Type.OrderedTypes.Concrete []) handler ~predecessor ~successor =
  MockClassHierarchyHandler.connect ~parameters handler ~predecessor ~successor


let ( ! ) concretes = Type.OrderedTypes.Concrete concretes

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
  handler order, Hashtbl.keys order.annotations


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
  handler order, Hashtbl.keys order.annotations


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
  let assert_method_resolution_order ((module Handler : Handler) as order) annotation expected =
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      expected
      (method_resolution_order_linearize order annotation ~get_successors:Handler.edges)
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


let test_least_upper_bound _ =
  assert_equal (least_upper_bound order "3" "1") ["3"];
  assert_equal (least_upper_bound order "4" "bottom") ["4"];
  assert_equal (least_upper_bound order "0" "1") ["3"];
  assert_equal (least_upper_bound order "0" "2") [];
  assert_equal (least_upper_bound order "0" "2") [];
  assert_equal (least_upper_bound butterfly "0" "1") ["2"; "3"]


let test_greatest_lower_bound _ =
  let assert_greatest_lower_bound ~order type1 type2 expected =
    let actual = greatest_lower_bound order type1 type2 |> List.sort ~compare:String.compare in
    assert_equal ~printer:(List.to_string ~f:Fn.id) actual expected
  in
  let (module H : ClassHierarchy.Handler) = diamond_order in
  assert_greatest_lower_bound ~order:diamond_order "A" "C" ["C"];
  assert_greatest_lower_bound ~order:diamond_order "A" "B" ["B"];
  assert_greatest_lower_bound ~order:diamond_order "A" "D" ["D"];
  assert_greatest_lower_bound ~order:diamond_order "B" "D" ["D"];
  assert_greatest_lower_bound ~order:diamond_order "B" "C" ["D"];
  assert_greatest_lower_bound ~order:butterfly "2" "3" ["0"; "1"]


let test_check_integrity _ =
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
    handler order, Hashtbl.keys order.annotations
  in
  assert_raises Cyclic (fun _ -> check_integrity order ~indices);

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
    handler order, Hashtbl.keys order.annotations
  in
  assert_raises Cyclic (fun _ -> check_integrity order ~indices)


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
    handler order, Hashtbl.keys order.annotations
  in
  let (module Handler) = order in
  assert_equal
    ~printer:ident
    ( {|
      digraph {
        343776663[label="object"]
        453441034[label="1"]
        564400327[label="2"]
        680650890[label="0"]
        680650890 -> 453441034[label="(str)"]
        680650890 -> 564400327
      }
    |}
    |> Test.trim_extra_indentation )
    ("\n" ^ to_dot order ~indices:keys)


let test_variables _ =
  let order =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    connect order ~parameters:![Type.variable "T"] ~predecessor:"A" ~successor:"typing.Generic";
    handler order
  in
  let assert_variables ~expected source = assert_equal expected (variables order source) in
  assert_variables ~expected:None "B";
  assert_variables ~expected:(Some (Unaries [Type.Variable.Unary.create "T"])) "A";

  assert_variables ~expected:None "Nonexistent"


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
  let variadic = Type.Variable.Variadic.List.create "Ts" in
  let simple_variadic =
    Type.OrderedTypes.Concatenation
      (Type.OrderedTypes.Concatenation.create
         (Type.OrderedTypes.Concatenation.Middle.create_bare variadic))
  in
  let order = parametric_order_base in
  let open MockClassHierarchyHandler in
  insert order "UserTuple";
  connect order ~predecessor:"UserTuple" ~successor:"typing.Generic" ~parameters:simple_variadic;

  (* Contrived example *)
  connect
    order
    ~predecessor:"UserTuple"
    ~successor:"list"
    ~parameters:
      (Concrete
         [ Type.Tuple
             (Bounded
                (Concatenation
                   (Type.OrderedTypes.Concatenation.create
                      (Type.OrderedTypes.Concatenation.Middle.create_bare variadic)))) ]);
  insert order "SimpleTupleChild";
  connect
    order
    ~predecessor:"SimpleTupleChild"
    ~successor:"typing.Generic"
    ~parameters:simple_variadic;
  connect order ~predecessor:"SimpleTupleChild" ~successor:"UserTuple" ~parameters:simple_variadic;
  insert order "TupleOfLists";
  connect order ~predecessor:"TupleOfLists" ~successor:"typing.Generic" ~parameters:simple_variadic;
  connect
    order
    ~predecessor:"TupleOfLists"
    ~successor:"UserTuple"
    ~parameters:
      (Concatenation
         (Type.OrderedTypes.Concatenation.create
            (Type.OrderedTypes.Concatenation.Middle.create ~mappers:["list"] ~variable:variadic)));
  insert order "DTypedTensor";
  connect
    order
    ~predecessor:"DTypedTensor"
    ~successor:"typing.Generic"
    ~parameters:
      (Concatenation
         (Type.OrderedTypes.Concatenation.create
            ~head:[Type.Variable (Type.Variable.Unary.create "DType")]
            (Type.OrderedTypes.Concatenation.Middle.create_bare variadic)));
  insert order "IntTensor";
  connect
    order
    ~predecessor:"IntTensor"
    ~successor:"typing.Generic"
    ~parameters:
      (Concatenation
         (Type.OrderedTypes.Concatenation.create
            (Type.OrderedTypes.Concatenation.Middle.create_bare variadic)));
  connect
    order
    ~predecessor:"IntTensor"
    ~successor:"DTypedTensor"
    ~parameters:
      (Concatenation
         (Type.OrderedTypes.Concatenation.create
            ~head:[Type.integer]
            (Type.OrderedTypes.Concatenation.Middle.create_bare variadic)));
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
       ~source:(Type.tuple [Type.integer; Type.integer])
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

  let printer optional_ordered_types =
    optional_ordered_types
    >>| Format.asprintf "%a" Type.OrderedTypes.pp_concise
    |> Option.value ~default:"None"
  in
  assert_equal
    (instantiate_successors_parameters variadic_order ~source:Type.Bottom ~target:"UserTuple")
    (Some Any);
  assert_equal
    ~printer
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "SimpleTupleChild" ![Type.integer; Type.string; Type.bool])
       ~target:"UserTuple")
    (Some ![Type.integer; Type.string; Type.bool]);
  assert_equal
    ~printer
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "SimpleTupleChild" ![Type.integer; Type.string; Type.bool])
       ~target:"list")
    (Some ![Type.tuple [Type.integer; Type.string; Type.bool]]);
  assert_equal
    ~printer
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "TupleOfLists" ![Type.integer; Type.string; Type.bool])
       ~target:"UserTuple")
    (Some ![Type.list Type.integer; Type.list Type.string; Type.list Type.bool]);

  (* Concatenation *)
  assert_equal
    ~printer
    (instantiate_successors_parameters
       variadic_order
       ~source:(Type.parametric "IntTensor" ![Type.literal_integer 4; Type.literal_integer 2])
       ~target:"DTypedTensor")
    (Some ![Type.integer; Type.literal_integer 4; Type.literal_integer 2]);
  let list_variadic = Type.Variable.Variadic.List.create "Ts" in
  (* Should be Concatenate[int, Ts, Literal[2]], but concatenation v concatenation work is not done
     yet *)
  assert_equal
    ~printer
    (instantiate_successors_parameters
       variadic_order
       ~source:
         (Type.parametric
            "IntTensor"
            (Type.OrderedTypes.Concatenation
               (Type.OrderedTypes.Concatenation.create
                  ~tail:[Type.literal_integer 2]
                  (Type.OrderedTypes.Concatenation.Middle.create_bare list_variadic))))
       ~target:"DTypedTensor")
    (Some Any);
  ()


let test_instantiate_predecessors_parameters _ =
  let assert_instantiates_to ~source ~target expected =
    let handler = variadic_order in
    let order =
      {
        TypeOrder.handler;
        constructor = (fun _ ~protocol_assumptions:_ -> None);
        attributes = (fun _ ~protocol_assumptions:_ -> None);
        is_protocol = (fun _ ~protocol_assumptions:_ -> false);
        any_is_bottom = false;
        protocol_assumptions = TypeOrder.ProtocolAssumptions.empty;
      }
    in
    let step ~predecessor_variables ~parameters =
      TypeOrder.solve_ordered_types_less_or_equal
        order
        ~constraints:TypeConstraints.empty
        ~left:predecessor_variables
        ~right:parameters
      |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order)
      |> List.hd
    in
    let printer optional_ordered_types =
      optional_ordered_types
      >>| Format.asprintf "%a" Type.OrderedTypes.pp_concise
      |> Option.value ~default:"None"
    in
    assert_equal
      ~printer
      expected
      (ClassHierarchy.instantiate_predecessors_parameters handler ~source ~target ~step)
  in
  assert_instantiates_to
    ~source:(Type.parametric "GenericContainer" ![Type.Primitive "int"; Type.Primitive "str"])
    ~target:"NonGenericContainerChild"
    (Some (Concrete []));
  assert_instantiates_to
    ~source:(Type.parametric "GenericContainer" ![Type.Primitive "int"; Type.Primitive "int"])
    ~target:"NonGenericContainerChild"
    None;
  assert_instantiates_to
    ~source:(Type.list (Type.tuple [Type.integer; Type.string; Type.bool]))
    ~target:"UserTuple"
    (Some (Concrete [Type.integer; Type.string; Type.bool]));
  assert_instantiates_to
    ~source:
      (Type.parametric
         "UserTuple"
         ![Type.list Type.integer; Type.list Type.string; Type.list Type.bool])
    ~target:"SimpleTupleChild"
    (Some (Concrete [Type.list Type.integer; Type.list Type.string; Type.list Type.bool]));
  assert_instantiates_to
    ~source:
      (Type.parametric
         "UserTuple"
         ![Type.list Type.integer; Type.list Type.string; Type.list Type.bool])
    ~target:"TupleOfLists"
    (Some (Concrete [Type.integer; Type.string; Type.bool]));
  ()


let () =
  "order"
  >::: [ "check_integrity" >:: test_check_integrity;
         "greatest_lower_bound" >:: test_greatest_lower_bound;
         "is_instantiated" >:: test_is_instantiated;
         "least_upper_bound" >:: test_least_upper_bound;
         "successors" >:: test_successors;
         "to_dot" >:: test_to_dot;
         "variables" >:: test_variables;
         "instantiate_successors_parameters" >:: test_instantiate_successors_parameters;
         "instantiate_predecessors_parameters" >:: test_instantiate_predecessors_parameters ]
  |> Test.run
