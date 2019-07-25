open Core
open Pyre
open OUnit2
open Analysis
open Test
open ClassHierarchy

let connect ?(parameters = Type.OrderedTypes.Concrete []) handler ~predecessor ~successor =
  connect ~parameters handler ~predecessor ~successor


let ( ! ) concretes = Type.OrderedTypes.Concrete concretes

(* Butterfly:
 *  0 - 2
 *    X
 *  1 - 3 *)
let butterfly =
  let order = Builder.create () |> handler in
  insert order "0";
  insert order "1";
  insert order "2";
  insert order "3";
  connect order ~predecessor:"0" ~successor:"2";
  connect order ~predecessor:"0" ~successor:"3";
  connect order ~predecessor:"1" ~successor:"2";
  connect order ~predecessor:"1" ~successor:"3";
  order


(*          0 - 3
 *          |   |   \
 *          BOTTOM  - b - 1      TOP
 *          |  \       /
 *          4 -- 2 --- *)
let order =
  let bottom = "bottom" in
  let order = Builder.create () |> handler in
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
  order


let diamond_order =
  let order = Builder.create () |> handler in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  connect order ~predecessor:"D" ~successor:"B";
  connect order ~predecessor:"D" ~successor:"C";
  connect order ~predecessor:"B" ~successor:"A";
  connect order ~predecessor:"C" ~successor:"A";
  order


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
  let order = Builder.create () |> handler in
  insert order "A";
  insert order "B";
  insert order "C";
  connect order ~predecessor:"B" ~successor:"A";
  connect order ~predecessor:"C" ~successor:"B";
  connect order ~predecessor:"C" ~successor:"A";
  order


let test_method_resolution_order_linearize _ =
  let assert_method_resolution_order ((module Handler : Handler) as order) annotation expected =
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.Primitive.show next ^ " "))
      expected
      (method_resolution_order_linearize
         order
         annotation
         ~get_successors:(Handler.find (Handler.edges ())))
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
  assert_greatest_lower_bound ~order:diamond_order "A" "C" ["C"];
  assert_greatest_lower_bound ~order:diamond_order "A" "B" ["B"];
  assert_greatest_lower_bound ~order:diamond_order "A" "D" ["D"];
  assert_greatest_lower_bound ~order:diamond_order "B" "D" ["D"];
  assert_greatest_lower_bound ~order:diamond_order "B" "C" ["D"];
  assert_greatest_lower_bound ~order:butterfly "2" "3" ["0"; "1"]


let test_deduplicate _ =
  let (module Handler : Handler) =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    connect order ~parameters:![Type.Top; Type.Top] ~predecessor:"0" ~successor:"1";
    connect order ~parameters:![Type.Top] ~predecessor:"0" ~successor:"1";
    deduplicate order ~annotations:["0"; "1"];
    order
  in
  let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
  let module TargetAsserter (ListOrSet : Target.ListOrSet) = struct
    let assert_targets edges from target parameters create =
      assert_equal
        ~cmp:ListOrSet.equal
        ~printer:(ListOrSet.to_string ~f:Target.show)
        (Handler.find_unsafe edges (index_of from))
        (create { Target.target = index_of target; parameters })
  end
  in
  let module ForwardAsserter = TargetAsserter (Target.List) in
  let module BackwardsAsserter = TargetAsserter (Target.Set) in
  ForwardAsserter.assert_targets (Handler.edges ()) "0" "1" ![Type.Top] (fun target -> [target]);
  BackwardsAsserter.assert_targets
    (Handler.backedges ())
    "1"
    "0"
    ![Type.Top]
    (fun target -> Target.Set.of_list [target])


let test_remove_extra_edges_to_object _ =
  (* 0 -> 1 -> 2 -> object
   *  |----^         ^
   *  |--------------^
   *)
  let (module Handler : Handler) =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "object";
    connect order ~predecessor:"0" ~successor:"1";
    connect order ~predecessor:"0" ~successor:"object";
    connect order ~predecessor:"1" ~successor:"2";
    connect order ~predecessor:"2" ~successor:"object";
    remove_extra_edges_to_object order ["0"; "1"; "2"; "object"];
    order
  in
  let zero_index = Handler.find_unsafe (Handler.indices ()) "0" in
  let one_index = Handler.find_unsafe (Handler.indices ()) "1" in
  let two_index = Handler.find_unsafe (Handler.indices ()) "2" in
  let object_index = Handler.find_unsafe (Handler.indices ()) "object" in
  assert_equal
    (Handler.find_unsafe (Handler.edges ()) zero_index)
    [{ Target.target = one_index; parameters = ![] }];
  assert_equal
    ~cmp:Target.Set.equal
    (Handler.find_unsafe (Handler.backedges ()) object_index)
    (Target.Set.of_list [{ Target.target = two_index; parameters = ![] }])


let test_connect_annotations_to_top _ =
  (* Partial partial order:
   *  0 - 2
   *  |
   *  1   object *)
  let order =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "object";
    connect order ~predecessor:"0" ~successor:"2";
    connect order ~predecessor:"0" ~successor:"1";
    connect_annotations_to_object order ["0"; "1"; "2"; "object"];
    order
  in
  assert_equal (least_upper_bound order "1" "2") ["object"];

  (* Ensure that the backedge gets added as well *)
  assert_equal (greatest_lower_bound order "1" "object") ["1"]


let test_check_integrity _ =
  check_integrity order;
  check_integrity butterfly;

  (* 0 <-> 1 *)
  let order =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    connect order ~predecessor:"0" ~successor:"1";
    connect order ~predecessor:"1" ~successor:"0";
    order
  in
  assert_raises Cyclic (fun _ -> check_integrity order);

  (* 0 -> 1
   * ^    |
   *  \   v
   * .  - 2 -> 3 *)
  let order =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "3";
    connect order ~predecessor:"0" ~successor:"1";
    connect order ~predecessor:"1" ~successor:"2";
    connect order ~predecessor:"2" ~successor:"0";
    connect order ~predecessor:"2" ~successor:"3";
    order
  in
  assert_raises Cyclic (fun _ -> check_integrity order)


let test_to_dot _ =
  let order =
    let order = Builder.create () |> handler in
    insert order "0";
    insert order "1";
    insert order "2";
    insert order "object";
    connect order ~predecessor:"0" ~successor:"2";
    connect order ~predecessor:"0" ~successor:"1" ~parameters:![Type.string];
    connect_annotations_to_object order ["0"; "1"; "2"; "object"];
    order
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
        453441034 -> 343776663
        564400327 -> 343776663
        680650890 -> 453441034[label="(str)"]
        680650890 -> 564400327
      }
    |}
    |> Test.trim_extra_indentation )
    ("\n" ^ to_dot order)


let test_variables _ =
  let order =
    let order = Builder.create () |> handler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    connect order ~parameters:![Type.variable "T"] ~predecessor:"A" ~successor:"typing.Generic";
    order
  in
  let assert_variables ~expected source = assert_equal expected (variables order source) in
  assert_variables ~expected:None "B";
  assert_variables ~expected:(Some (Unaries [Type.Variable.Unary.create "T"])) "A";

  assert_variables ~expected:None "Nonexistent"


let test_is_instantiated _ =
  let order =
    let order = Builder.create () |> handler in
    insert order "typing.Generic";
    insert order "A";
    insert order "B";
    order
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


let test_disconnect_successors _ =
  let order () =
    let order = Builder.create () |> handler in
    insert order "a";
    insert order "b";
    insert order "1";
    insert order "2";
    connect order ~predecessor:"a" ~successor:"1";
    connect order ~predecessor:"b" ~successor:"1";
    connect order ~predecessor:"1" ~successor:"2";
    order
  in
  let assert_backedges_equal wrapped_left unwrapped_right =
    assert_equal ~cmp:Target.Set.equal wrapped_left (Target.Set.of_list unwrapped_right)
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) key in
    disconnect_successors (module Handler) ["1"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "1")) [];
    assert_backedges_equal (Handler.find_unsafe (Handler.backedges ()) (index "2")) [];
    assert_equal
      (Handler.find_unsafe (Handler.edges ()) (index "a"))
      [{ Target.target = index "1"; parameters = ![] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) key in
    disconnect_successors (module Handler) ["a"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "a")) [];
    assert_backedges_equal
      (Handler.find_unsafe (Handler.backedges ()) (index "1"))
      [{ Target.target = index "b"; parameters = ![] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) key in
    disconnect_successors (module Handler) ["b"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "b")) [];
    assert_backedges_equal
      (Handler.find_unsafe (Handler.backedges ()) (index "1"))
      [{ Target.target = index "a"; parameters = ![] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) key in
    disconnect_successors (module Handler) ["a"; "b"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "a")) [];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "b")) [];
    assert_backedges_equal (Handler.find_unsafe (Handler.backedges ()) (index "1")) []
  in
  ()


let parametric_order =
  let order = Builder.default () |> handler in
  let variable = Type.variable "_T" in
  let other_variable = Type.variable "_T2" in
  let variable_covariant = Type.variable "_T_co" ~variance:Covariant in
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


let variadic_order =
  let variadic = Type.Variable.Variadic.List.create "Ts" in
  let order = parametric_order in
  insert order "UserTuple";
  connect
    order
    ~predecessor:"UserTuple"
    ~successor:"typing.Generic"
    ~parameters:(Variable variadic);

  (* Contrived example *)
  connect
    order
    ~predecessor:"UserTuple"
    ~successor:"list"
    ~parameters:(Concrete [Type.Tuple (Bounded (Variable variadic))]);
  insert order "SimpleTupleChild";
  connect
    order
    ~predecessor:"SimpleTupleChild"
    ~successor:"typing.Generic"
    ~parameters:(Variable variadic);
  connect
    order
    ~predecessor:"SimpleTupleChild"
    ~successor:"UserTuple"
    ~parameters:(Variable variadic);
  insert order "TupleOfLists";
  connect
    order
    ~predecessor:"TupleOfLists"
    ~successor:"typing.Generic"
    ~parameters:(Variable variadic);
  connect
    order
    ~predecessor:"TupleOfLists"
    ~successor:"UserTuple"
    ~parameters:(Map (Type.OrderedTypes.Map.create ~mappers:["list"] ~variable:variadic));
  order


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
         "connect_annotations_to_top" >:: test_connect_annotations_to_top;
         "deduplicate" >:: test_deduplicate;
         "disconnect_successors" >:: test_disconnect_successors;
         "greatest_lower_bound" >:: test_greatest_lower_bound;
         "is_instantiated" >:: test_is_instantiated;
         "least_upper_bound" >:: test_least_upper_bound;
         "remove_extra_edges" >:: test_remove_extra_edges_to_object;
         "successors" >:: test_successors;
         "to_dot" >:: test_to_dot;
         "variables" >:: test_variables;
         "instantiate_successors_parameters" >:: test_instantiate_successors_parameters;
         "instantiate_predecessors_parameters" >:: test_instantiate_predecessors_parameters ]
  |> Test.run
