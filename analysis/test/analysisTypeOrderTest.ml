(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Test
open TypeOrder


let (!) name =
  Type.Primitive ~~name


let configuration = Configuration.create ()


let connect ?(parameters = []) order ~predecessor ~successor =
  connect ~configuration ~parameters ~add_backedge:true order ~predecessor ~successor


(* Butterfly:
    0 - 2
      X
    1 - 3 *)
let butterfly =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"0";
  insert order !"1";
  insert order !"2";
  insert order !"3";
  connect order ~predecessor:!"2" ~successor:Type.Top;
  connect order ~predecessor:!"3" ~successor:Type.Top;
  connect order ~predecessor:!"0" ~successor:!"2";
  connect order ~predecessor:!"0" ~successor:!"3";
  connect order ~predecessor:!"1" ~successor:!"2";
  connect order ~predecessor:!"1" ~successor:!"3";
  connect order ~predecessor:Type.Bottom ~successor:!"0";
  connect order ~predecessor:Type.Bottom ~successor:!"1";
  order


(*          0 - 3
            |   |   \
            BOTTOM  - b - 1      TOP
            |  \       /
            4 -- 2 ---           *)
let order =
  let bottom = !"bottom" in
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order bottom;
  insert order Type.Top;
  insert order !"0";
  insert order !"1";
  insert order !"2";
  insert order !"3";
  insert order !"4";
  insert order !"5";
  connect order ~predecessor:!"0" ~successor:!"3";
  connect order ~predecessor:!"1" ~successor:!"3";
  connect order ~predecessor:!"4" ~successor:!"2";
  connect order ~predecessor:!"3" ~successor:Type.Top;
  connect order ~predecessor:!"2" ~successor:Type.Top;
  connect order ~predecessor:Type.Bottom ~successor:bottom;
  connect order ~predecessor:bottom ~successor:!"0";
  connect order ~predecessor:bottom ~successor:!"1";
  connect order ~predecessor:bottom ~successor:!"2";
  connect order ~predecessor:bottom ~successor:!"4";
  order


(*
   TOP
    |
    A
   / \
  B   C
   \ /
    D
    |
 BOTTOM
*)
let diamond_order =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"A";
  insert order !"B";
  insert order !"C";
  insert order !"D";
  connect order ~predecessor:Type.Bottom ~successor:!"D";
  connect order ~predecessor:!"D" ~successor:!"B";
  connect order ~predecessor:!"D" ~successor:!"C";
  connect order ~predecessor:!"B" ~successor:!"A";
  connect order ~predecessor:!"C" ~successor:!"A";
  connect order ~predecessor:!"A" ~successor:Type.Top;
  order


let disconnected_order =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"A";
  insert order !"B";
  order


let default =
  let order = Builder.default ~configuration () |> TypeOrder.handler in
  let variable = Type.Variable { Type.variable = ~~"_T"; constraints = [] } in
  insert order variable;
  connect order ~predecessor:Type.Bottom ~successor:variable;
  connect order ~predecessor:variable ~successor:Type.Top;
  let other_variable = Type.Variable { Type.variable = ~~"_T2"; constraints = [] } in
  insert order other_variable;
  connect order ~predecessor:Type.Bottom ~successor:other_variable;
  connect order ~predecessor:other_variable ~successor:Type.Top;

  insert order !"list";
  insert order !"typing.Sized";
  connect order ~predecessor:Type.Bottom ~successor:!"list";
  connect order ~predecessor:!"list" ~successor:!"typing.Sized";
  connect order ~predecessor:!"list" ~successor:!"typing.Generic" ~parameters:[variable];
  connect order ~predecessor:!"typing.Sized" ~successor:Type.Object;

  insert order !"set";
  connect order ~predecessor:Type.Bottom ~successor:!"set";
  connect order ~predecessor:!"set" ~successor:!"typing.Sized";
  connect order ~predecessor:!"set" ~successor:!"typing.Generic" ~parameters:[variable];

  insert order !"typing.Iterator";
  connect order ~predecessor:!"list" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect order ~predecessor:!"typing.Iterator" ~successor:Type.Top;

  insert order !"tuple";
  connect order ~predecessor:Type.Bottom ~successor:!"tuple";
  connect order ~predecessor:!"tuple" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect order ~predecessor:!"tuple" ~successor:!"typing.Generic" ~parameters:[variable];

  insert order !"str";
  connect order ~predecessor:Type.Bottom ~successor:!"str";
  connect order ~predecessor:!"str" ~successor:!"typing.Iterator" ~parameters:[!"str"];
  connect order ~predecessor:!"str" ~successor:!"typing.Generic" ~parameters:[!"str"];

  insert order !"dict";
  connect order ~predecessor:Type.Bottom ~successor:!"dict";
  connect order ~predecessor:!"dict" ~successor:Type.Object ~parameters:[variable; other_variable];
  connect
    order
    ~predecessor:!"dict"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  connect order ~predecessor:!"dict" ~successor:!"typing.Iterator" ~parameters:[variable];

  order


let test_default _ =
  let order = Builder.default ~configuration () |> TypeOrder.handler in
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);

  (* Test special forms. *)
  let assert_has_special_form primitive_name =
    assert_true (TypeOrder.contains order (Type.Primitive (Identifier.create primitive_name)))
  in
  assert_has_special_form "typing.Tuple";
  assert_has_special_form "typing.Generic";
  assert_has_special_form "typing.Protocol";
  assert_has_special_form "typing.Callable";
  assert_has_special_form "typing.Type";
  assert_has_special_form "typing.ClassVar";

  (* Numerical types. *)
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.integer);
  assert_false (less_or_equal order ~left:Type.float ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.float ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.float);

  (* Test join. *)
  assert_equal (join order Type.integer Type.integer) Type.integer;
  assert_equal (join order Type.float Type.integer) Type.float;
  assert_equal (join order Type.integer Type.float) Type.float;
  assert_equal (join order Type.integer Type.complex) Type.complex;
  assert_equal (join order Type.float Type.complex) Type.complex;

  (* Test meet. *)
  assert_equal (meet order Type.integer Type.integer) Type.integer;
  assert_equal (meet order Type.float Type.integer) Type.integer;
  assert_equal (meet order Type.integer Type.float) Type.integer;
  assert_equal (meet order Type.integer Type.complex) Type.integer;
  assert_equal (meet order Type.float Type.complex) Type.float


let test_successors_fold _ =
  let collect sofar annotation = annotation :: sofar in
  assert_equal (successors_fold ~initial:[] ~f:collect butterfly !"3") [Type.Top];
  assert_equal (successors_fold ~initial:[] ~f:collect butterfly !"0") [Type.Top; !"2"; !"3"]


let test_successors _ =
  (* Butterfly:
      0 - 2
        X
      1 - 3 *)
  assert_equal (successors butterfly !"3") [Type.Top];
  assert_equal (successors butterfly !"0") [!"3"; !"2"; Type.Top];

  (*          0 - 3
              /   /   \
              BOTTOM - 1      TOP
              |  \       /
              4 -- 2 ---           *)
  assert_equal (successors order !"3") [Type.Top];
  assert_equal (successors order !"0") [!"3"; Type.Top];
  assert_equal
    (successors order !"bottom")
    [
      !"4";
      !"2";
      !"1";
      !"0";
      Type.Top;
      !"3";
    ];

  (*  BOTTOM - Iterator[_T] - Iterable[_T] - Generic[_T] - Object
                    \                            /
                      --------------------------                    *)
  let order =
    let variable name = Type.Variable { Type.variable = name; constraints = [] } in
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    insert order !"typing.Iterator";
    insert order !"typing.Iterable";
    insert order !"typing.Generic";
    connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterable";
    connect
      order
      ~predecessor:!"typing.Iterator"
      ~successor:!"typing.Iterable"
      ~parameters:[variable ~~"_T"];
    connect
      order
      ~predecessor:!"typing.Iterator"
      ~successor:!"typing.Generic"
      ~parameters:[variable ~~"_T"];
    connect
      order
      ~predecessor:!"typing.Iterable"
      ~successor:!"typing.Generic"
      ~parameters:[variable ~~"_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    order in

  assert_equal
    (successors
       order
       (Type.Parametric {
           Type.name = ~~"typing.Iterable";
           parameters = [Type.integer];
         }))
    [
      Type.Parametric {
        Type.name = ~~"typing.Generic";
        parameters = [Type.integer];
      };
      Type.Object;
    ];

  assert_equal
    (successors
       order
       (Type.Parametric {
           Type.name = ~~"typing.Iterator";
           parameters = [Type.integer];
         }))
    [
      Type.Parametric {
        Type.name = ~~"typing.Generic";
        parameters = [Type.integer];
      };
      Type.Parametric {
        Type.name = ~~"typing.Iterable";
        parameters = [Type.integer];
      };
      Type.Object;
    ]


let test_predecessors _ =
  (* Butterfly:
      0 - 2
        X
      1 - 3 *)
  assert_equal (predecessors butterfly !"0") [Type.Bottom];
  assert_equal (predecessors butterfly !"3") [!"1"; !"0"; Type.Bottom];

  (*          0 - 3
              /   /   \
              BOTTOM - 1      TOP
              |  \       /
              4 -- 2 ---           *)
  assert_equal (predecessors order !"0") [!"bottom"; Type.Bottom];
  assert_equal (predecessors order !"3") [!"1"; !"0"; !"bottom"; Type.Bottom]


let test_greatest _ =
  let smaller_than value = function
    | Type.Primitive name ->
        begin
          try
            (Identifier.show name |> Int.of_string) < value
          with _ ->
            false
        end
    | _ -> false
  in
  assert_equal (greatest butterfly ~matches:(smaller_than 3)) [!"2"];
  assert_equal (greatest butterfly ~matches:(smaller_than 2)) [!"0"; !"1"]


let test_less_or_equal _ =
  (* Primitive types. *)
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);

  assert_true (less_or_equal order ~left:!"0" ~right:!"0");

  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"0");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"1");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"2");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"3");

  assert_false (less_or_equal order ~left:!"3" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"2" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"1" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"0" ~right:Type.Bottom);

  assert_true (less_or_equal order ~left:!"0" ~right:!"3");
  assert_true (less_or_equal order ~left:!"1" ~right:!"3");
  assert_false (less_or_equal order ~left:!"2" ~right:!"3");

  assert_true (less_or_equal default ~left:!"list" ~right:!"typing.Sized");
  assert_true
    (less_or_equal default ~left:(Type.list Type.integer) ~right:!"typing.Sized");

  (* Parametric types. *)
  assert_true
    (less_or_equal default ~left:(Type.list Type.integer) ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal default ~left:(Type.list Type.float) ~right:(Type.iterator Type.integer));

  (* Mixed primitive and parametric types. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.string)
       ~right:(Type.iterator Type.string));

  (* Mixed tuple and parametric types. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.iterator Type.integer));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.iterator Type.float));

  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Unbounded Type.integer))
       ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Unbounded Type.float))
       ~right:(Type.iterator Type.integer));

  assert_true
    (less_or_equal
       default
       ~left:(Type.Primitive ~~"tuple")
       ~right:(Type.Tuple (Type.Unbounded Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Bounded [Type.integer; Type.integer]))
       ~right:(Type.Parametric { Type.name = ~~"tuple"; parameters = [Type.integer] }));

  (* Union types *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.Optional Type.string)
       ~right:(Type.Union [Type.integer; Type.Optional Type.string]));

  let order =
    let variable name = Type.Variable { Type.variable = name; constraints = [] } in
    let order = Builder.create () |> TypeOrder.handler in
    let add_simple annotation =
      insert order annotation;
      connect order ~predecessor:Type.Bottom ~successor:annotation;
      connect order ~predecessor:annotation ~successor:Type.Top
    in

    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    add_simple (variable ~~"_1");
    add_simple (variable ~~"_2");
    add_simple (variable ~~"_T");
    add_simple (Type.string);
    add_simple (Type.integer);
    add_simple (Type.float);
    add_simple !"tuple";
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"typing.Generic";
    connect order ~predecessor:Type.Bottom ~successor:!"A";

    connect
      order
      ~predecessor:!"A"
      ~successor:!"B"
      ~parameters:[Type.tuple [variable ~~"_1"; variable ~~"_2"]];
    connect
      order
      ~predecessor:!"A"
      ~successor:!"typing.Generic"
      ~parameters:[variable ~~"_1"; variable ~~"_2"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"typing.Generic"
      ~parameters:[variable ~~"_T"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"C"
      ~parameters:[Type.union [variable ~~"_T"; Type.float]];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    order
  in
  assert_true
    (less_or_equal
       order
       ~left:(Type.Parametric {
           Type.name = Identifier.create "A";
           parameters = [Type.integer; Type.string]
         })
       ~right:(Type.Parametric {
           Type.name = Identifier.create "B";
           parameters = [Type.tuple [Type.integer; Type.string]]
         }));

  assert_false
    (less_or_equal
       order
       ~left:(Type.Parametric {
           Type.name = Identifier.create "A";
           parameters = [Type.integer; Type.string]
         })
       ~right:(Type.tuple [Type.integer; Type.string]));

  assert_true
    (less_or_equal
       order
       ~left:(Type.Parametric {
           Type.name = Identifier.create "A";
           parameters = [Type.integer; Type.string]
         })
       ~right:(Type.Parametric {
           Type.name = Identifier.create "C";
           parameters = [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]
         }));

  assert_false
    (less_or_equal
       order
       ~left:(Type.Parametric {
           Type.name = Identifier.create "A";
           parameters = [Type.string; Type.integer]
         })
       ~right:(Type.Parametric {
           Type.name = Identifier.create "C";
           parameters = [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]
         }))


let test_join _ =
  let assert_join ?(order = default) ?(aliases = (fun _ -> None)) left right expected =
    let parse_annotation source =
      let integer = try Int.of_string source |> ignore; true with _ -> false in
      if integer then
        Type.Primitive ~~source
      else
        parse_single_expression source
        |> Type.create ~aliases
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (parse_annotation expected)
      (join order (parse_annotation left) (parse_annotation right))
  in


  (* Primitive types. *)
  assert_join "list" "typing.Sized" "typing.Sized";
  assert_join "typing.Sized" "list" "typing.Sized";
  assert_join "typing.List[int]" "typing.Sized" "typing.Sized";
  assert_join "int" "str" "typing.Union[int, str]";

  (* Parametric types. *)
  assert_join "typing.List[int]" "typing.Iterator[int]" "typing.Iterator[int]";
  assert_join "typing.Iterator[int]" "typing.List[int]" "typing.Iterator[int]";
  assert_join "typing.List[float]" "typing.Iterator[int]" "typing.Iterator[float]";
  assert_join "typing.List[float]" "float[int]" "typing.Any";

  (* Optionals. *)
  assert_join "str" "typing.Optional[str]" "typing.Optional[str]";
  assert_join "str" "typing.Optional[$bottom]" "typing.Optional[str]";
  assert_join "typing.Optional[$bottom]" "str" "typing.Optional[str]";

  (* Handles `[] or optional_list`. *)
  assert_join "typing.List[$bottom]" "typing.Optional[typing.List[int]]" "typing.List[int]";
  assert_join "typing.Optional[typing.List[int]]" "typing.List[$bottom]" "typing.List[int]";
  assert_join "typing.Optional[typing.Set[int]]" "typing.Set[$bottom]" "typing.Set[int]";

  (* Union types. *)
  assert_join
    "typing.Optional[bool]"
    "typing.Union[int, typing.Optional[bool]]"
    "typing.Union[int, typing.Optional[bool]]";
  assert_join "typing.Union[int, str]" "typing.Union[int, bytes]" "typing.Union[int, str, bytes]";

  assert_join
    "typing.Dict[str, str]"
    "typing.Dict[str, typing.List[str]]"
    "typing.Dict[str, typing.Any]";

  assert_join "typing.Union[typing.List[int], typing.Set[int]]" "typing.Sized" "typing.Sized";

  assert_join
    "typing.Optional[float]"
    "typing.Union[float, int]"
    "typing.Optional[typing.Union[float, int]]";

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    let add_simple annotation =
      insert order annotation;
      connect order ~predecessor:Type.Bottom ~successor:annotation;
      connect order ~predecessor:annotation ~successor:Type.Top
    in

    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    add_simple (Type.variable "_1");
    add_simple (Type.variable "_2");
    add_simple (Type.variable "_T");
    add_simple (Type.string);
    add_simple (Type.integer);
    add_simple (Type.float);
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"typing.Generic";
    connect order ~predecessor:Type.Bottom ~successor:!"A";

    connect
      order
      ~predecessor:!"A"
      ~successor:!"B"
      ~parameters:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    connect
      order
      ~predecessor:!"A"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_1"; Type.variable "_2"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    order
  in
  let aliases =
    Type.Table.of_alist_exn [
      Type.primitive "_1", Type.variable "_1";
      Type.primitive "_2", Type.variable "_2";
      Type.primitive "_T", Type.variable "_T";
    ]
    |> Type.Table.find
  in

  assert_join
    ~order
    ~aliases
    "A[int, str]"
    "C[$bottom]"
    "C[typing.Union[float, typing.Tuple[int, str]]]";

  assert_join ~order:disconnected_order "A" "B" "typing.Any";

  (* Callables. *)
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., str]"
    "typing.Callable[..., typing.Union[int, str]]";
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., $bottom]"
    "typing.Callable[..., int]";

  assert_join
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]";

  (* Do not join with overloads. *)
  assert_join "typing.Callable[..., int][..., str]" "typing.Callable[..., int]" "typing.Any";

  assert_join
    "typing.Callable[[Named(a, int), Named(b, str)], int]"
    "typing.Callable[[Named(a, int), Named(b, str)], int]"
    "typing.Callable[[Named(a, int), Named(b, str)], int]";
  assert_join
    "typing.Callable[[Named(a, int)], int]"
    "typing.Callable[[int], int]"
    "typing.Any";

  (* Behavioral subtyping is preserved. *)
  assert_join
    "typing.Callable[[Named(a, str)], int]"
    "typing.Callable[[Named(a, int)], int]"
    "typing.Callable[[Named(a, $bottom)], int]";
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., $bottom]"
    "typing.Callable[..., int]"


let test_meet _ =
  (* Primitive types. *)
  assert_equal (meet default !"list" !"typing.Sized") !"list";
  assert_equal (meet default !"typing.Sized" !"list") !"list";
  assert_equal
    (meet default (Type.list Type.integer) !"typing.Sized")
    (Type.list Type.integer);

  let compare_with_union left right =
    match left, right with
    | Type.Union left, Type.Union right ->
        Type.Set.equal (Type.Set.of_list left) (Type.Set.of_list right)
    | _ -> Type.equal left right
  in
  assert_equal
    ~cmp:compare_with_union
    (meet
       default
       (Type.Union [Type.integer; Type.string])
       (Type.Union [Type.integer; Type.bytes]))
    Type.integer;
  assert_equal
    ~cmp:compare_with_union
    (meet
       default
       (Type.Union [Type.integer; Type.string])
       (Type.Union [Type.string; Type.integer]))
    (Type.Union [Type.integer; Type.string]);
  assert_equal
    ~cmp:compare_with_union
    ~printer:Type.show
    (meet
       default
       (Type.Union [Type.integer; Type.string])
       (Type.Union [Type.integer; Type.Optional Type.string]))
    (Type.integer);
  assert_equal
    ~cmp:compare_with_union
    ~printer:Type.show
    (meet
       default
       (Type.Union [Type.integer; Type.Optional Type.string])
       (Type.Optional Type.string))
    (Type.Optional Type.string);

  (* Parametric types. *)
  assert_equal
    (meet default (Type.list Type.integer) (Type.iterator Type.integer))
    (Type.list Type.integer);
  assert_equal
    (meet default (Type.list Type.float) (Type.iterator Type.integer))
    (Type.list Type.integer);
  assert_equal
    ~printer:Type.show
    (meet default (Type.list Type.float) (Type.parametric "float" [Type.integer]))
    Type.Bottom;

  assert_equal
    (meet disconnected_order !"A" !"B")
    Type.Bottom;

  assert_equal
    (meet
       default
       (Type.dictionary ~key:Type.string ~value:Type.string)
       (Type.dictionary ~key:Type.string ~value:(Type.list Type.string)))
    (Type.dictionary ~key:Type.string ~value:Type.Bottom)


let test_least_upper_bound _ =
  assert_equal (least_upper_bound order Type.Bottom Type.Bottom) [Type.Bottom];

  assert_equal (least_upper_bound order Type.Bottom !"0") [!"0"];
  assert_equal (least_upper_bound order Type.Bottom !"1") [!"1"];
  assert_equal (least_upper_bound order !"3" !"1") [!"3"];
  assert_equal (least_upper_bound order !"4" !"bottom") [!"4"];

  assert_equal (least_upper_bound order !"0" !"1") [!"3"];
  assert_equal (least_upper_bound order !"0" !"2") [Type.Top];
  assert_equal (least_upper_bound order !"0" !"2") [Type.Top];

  assert_equal (least_upper_bound order Type.Top Type.Top) [Type.Top];

  assert_equal (least_upper_bound butterfly !"0" !"1") [!"2"; !"3"]


let test_greatest_lower_bound _ =
  assert_equal (greatest_lower_bound diamond_order Type.Bottom Type.Bottom) [Type.Bottom];

  assert_equal (greatest_lower_bound diamond_order Type.Bottom !"D") [Type.Bottom];
  assert_equal (greatest_lower_bound diamond_order Type.Bottom !"A") [Type.Bottom];
  assert_equal (greatest_lower_bound diamond_order !"A" !"C") [!"C"];
  assert_equal (greatest_lower_bound diamond_order !"A" !"B") [!"B"];
  assert_equal (greatest_lower_bound diamond_order !"A" !"D") [!"D"];
  assert_equal (greatest_lower_bound diamond_order !"B" !"D") [!"D"];
  assert_equal (greatest_lower_bound diamond_order !"B" !"C") [!"D"];

  assert_equal (greatest_lower_bound diamond_order Type.Top !"B") [!"B"];

  assert_equal (greatest_lower_bound diamond_order Type.Top Type.Top) [Type.Top];

  assert_equal (greatest_lower_bound butterfly !"2" !"3") [!"0"; !"1"]


let test_instantiate_parameters _ =
  assert_equal
    (instantiate_parameters default ~source:(Type.list Type.string) ~target:(!"typing.Iterator"))
    (Some [Type.string]);

  assert_equal
    (instantiate_parameters
       default
       ~source:(Type.dictionary ~key:Type.integer ~value:Type.string)
       ~target:(!"typing.Iterator"))
    (Some [Type.integer])


let test_remove_extra_edges _ =
  (* 0 -> 1 -> 2 -> 3
     |----^         ^
     |--------------^
  *)
  let (module Handler: TypeOrder.Handler) =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"0" ~successor:!"3";
    connect order ~predecessor:!"1" ~successor:!"2";
    connect order ~predecessor:!"2" ~successor:!"3";
    remove_extra_edges order ~bottom:!"0" ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order
  in
  let zero_index = Handler.find_unsafe (Handler.indices ()) !"0" in
  let one_index = Handler.find_unsafe (Handler.indices ()) !"1" in
  let two_index = Handler.find_unsafe (Handler.indices ()) !"2" in
  let three_index = Handler.find_unsafe (Handler.indices ()) !"3" in
  assert_equal
    (Handler.find_unsafe (Handler.edges ()) zero_index)
    [{ Target.target = one_index; parameters = []}];
  assert_equal
    (Handler.find_unsafe (Handler.backedges ()) three_index)
    [{ Target.target = two_index; parameters = []}]


let test_connect_annotations_to_top _ =
  (* Partial partial order:
      0 - 2
      |
      1   3 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"2";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect_annotations_to_top order ~configuration ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order in

  assert_equal
    (least_upper_bound order !"1" !"2")
    [!"3"];

  (* Ensure that the backedge gets added as well *)
  assert_equal
    (greatest_lower_bound order !"1" !"3")
    [!"1"]


let test_add_backedges _ =
  (*
     TOP
      |
      A
     / \
    B   C
     \ /
      D
      |
   BOTTOM
  *)
  let (module Handler: TypeOrder.Handler) =
    (* Don't add backedges when connecting *)
    let connect = TypeOrder.connect ~configuration in
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"D";
    connect order ~add_backedge:false ~predecessor:Type.Bottom ~successor:!"D";
    connect order ~add_backedge:false ~predecessor:!"D" ~successor:!"B";
    connect order ~add_backedge:false ~predecessor:!"D" ~successor:!"C";
    connect order ~add_backedge:false ~predecessor:!"B" ~successor:!"A";
    connect order ~add_backedge:false ~predecessor:!"C" ~successor:!"A";
    connect order ~add_backedge:false ~predecessor:!"A" ~successor:Type.Top;
    order
  in
  let assert_backedges annotation number_of_backedges =
    let index = Handler.find_unsafe (Handler.indices ()) annotation in
    match Handler.find (Handler.backedges ()) index with
    | None ->
        assert_equal number_of_backedges 0
    | Some backedges ->
        assert_equal number_of_backedges (List.length backedges)
  in
  assert_backedges !"A" 0;
  assert_backedges !"B" 0;
  assert_backedges !"C" 0;
  assert_backedges !"D" 0;

  TypeOrder.add_backedges (module Handler) ~bottom:Type.Bottom;
  assert_backedges !"A" 2;
  assert_backedges !"B" 1;
  assert_backedges !"C" 1;
  assert_backedges !"D" 1;

  (* Ensure that backedges only get added once by re-adding. *)
  TypeOrder.add_backedges (module Handler) ~bottom:Type.Bottom;
  assert_backedges !"A" 2;
  assert_backedges !"B" 1


let test_check_integrity _ =
  check_integrity order;
  check_integrity butterfly;

  (* 0 <-> 1 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"1" ~successor:!"0";
    order in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);

  (* 0 -> 1
     ^    |
      \   v
        - 2 -> 3 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"1" ~successor:!"2";
    connect order ~predecessor:!"2" ~successor:!"0";
    connect order ~predecessor:!"2" ~successor:!"3";
    order in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order !"0";
    order in
  assert_raises TypeOrder.Incomplete (fun _ -> check_integrity order);

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Top;
    insert order !"0";
    order in
  assert_raises TypeOrder.Incomplete (fun _ -> check_integrity order)


let test_to_dot _ =
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    insert order Type.Bottom;
    insert order Type.Top;
    connect order ~predecessor:!"0" ~successor:!"2";
    connect order ~predecessor:!"0" ~successor:!"1" ~parameters:[Type.string];
    connect_annotations_to_top order ~configuration ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order in
  let (module Handler) = order in
  assert_equal
    ~printer:ident
    ({|
       digraph {
         0[label="`0`"]
         1[label="`1`"]
         2[label="`2`"]
         3[label="`3`"]
         4[label="`typing.Any`"]
         5[label="`unknown`"]
         0 -> 1[label="(`str`)"]
         0 -> 2
         1 -> 3
         2 -> 3
       }
     |}
     |> Test.trim_extra_indentation)
    ("\n" ^ TypeOrder.to_dot order)


let () =
  "builder">:::[
    "default">::test_default;
  ]
  |> run_test_tt_main;
  "order">:::[
    "successors_fold">::test_successors_fold;
    "successors">::test_successors;
    "predecessors">::test_predecessors;
    "greatest">::test_greatest;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "meet">::test_meet;
    "least_upper_bound">::test_least_upper_bound;
    "greatest_lower_bound">::test_greatest_lower_bound;
    "instantiate_parameters">::test_instantiate_parameters;
    "add_backedges">::test_add_backedges;
    "remove_extra_edges">::test_remove_extra_edges;
    "connect_annotations_to_top">::test_connect_annotations_to_top;
    "check_integrity">::test_check_integrity;
    "to_dot">::test_to_dot;
  ]
  |> run_test_tt_main
