(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open Statement
open TypeCheck


open Test


let assert_type_errors =
  assert_errors ~check:TypeCheck.check


let resolution = Test.resolution ()


let create
    ?(bottom = false)
    ?(define = Test.mock_define)
    ?(expected_return = Type.Top)
    ?(resolution = Test.resolution ())
    ?(immutables = [])
    annotations =
  let resolution =
    let annotations =
      let immutables = String.Map.of_alist_exn immutables in
      let annotify (name, annotation) =
        let annotation =
          let create annotation =
            match Map.find immutables name with
            | Some global -> Annotation.create_immutable ~global annotation
            | _ -> Annotation.create annotation
          in
          create annotation
        in
        Access.create name, annotation
      in
      List.map annotations ~f:annotify
      |> Access.Map.of_alist_exn
    in
    Resolution.with_annotations resolution ~annotations
  in
  let define =
    +{
      define with
      Define.return_annotation = Some (Type.expression expected_return);
    }
  in
  State.create ~bottom ~resolution ~define ()


let assert_state_equal =
  assert_equal
    ~cmp:State.equal
    ~printer:(Format.asprintf "%a" State.pp)
    ~pp_diff:(diff ~print:State.pp)


let test_initial _ =
  let assert_initial
      ~parameters
      ?parent
      ?return_annotation
      ?(decorators = [])
      ?(initial = (fun resolution define ->
          State.initial ~resolution define))
      expected =
    let define = {
      Define.name = Access.create "foo";
      parameters = List.map parameters ~f:(~+);
      body = [];
      decorators;
      docstring = None;
      return_annotation;
      async = false;
      parent = parent >>| Access.create;
    }
    in
    assert_state_equal
      expected
      (initial resolution (+define))
  in

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = Some (+Float 1.0);
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = Some (+Float 1.0);
        annotation = None;
      }
    ]
    (create ["x", Type.float]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    ~return_annotation:!"int"
    (create ~immutables:["x", false] ~expected_return:Type.integer ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = None;
        annotation = Some (Type.expression Type.float);
      };
      {
        Parameter.name = "y";
        value = None;
        annotation = Some (Type.expression Type.string)
      };
    ]
    (create ~immutables:["x", false; "y", false] ["x", Type.float; "y", Type.string]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "x";
        value = None;
        annotation = None;
      };
    ]
    (create ["x", Type.Bottom]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = "self";
        value = None;
        annotation = None;
      };
    ]
    ~parent:"Foo"
    (create ["self", Type.Primitive "Foo"]);
  assert_initial
    ~parameters:[
      {
        Parameter.name = "a";
        value = None;
        annotation = None;
      };
    ]
    ~decorators:[!"staticmethod"]
    ~parent:"Foo"
    (create ["a", Type.Bottom])


let test_less_or_equal _ =
  (* <= *)
  assert_true (State.less_or_equal ~left:(create []) ~right:(create []));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.integer]));
  assert_true (State.less_or_equal ~left:(create []) ~right:(create ["x", Type.Top]));
  assert_true
    (State.less_or_equal
       ~left:(create ["x", Type.integer])
       ~right:(create ["x", Type.integer; "y", Type.integer]));

  (* > *)
  assert_false (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create []));
  assert_false (State.less_or_equal ~left:(create ["x", Type.Top]) ~right:(create []));

  (* partial order *)
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["x", Type.string]));
  assert_false
    (State.less_or_equal ~left:(create ["x", Type.integer]) ~right:(create ["y", Type.integer]))


let test_join _ =
  (* <= *)
  assert_state_equal (State.join (create []) (create [])) (create []);
  assert_state_equal
    (State.join (create []) (create ["x", Type.integer]))
    (create ["x", Type.Top]);
  assert_state_equal (State.join (create []) (create ["x", Type.Top])) (create ["x", Type.Top]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.integer; "y", Type.integer]))
    (create ["x", Type.integer; "y", Type.Top]);

  (* > *)
  assert_state_equal
    (State.join (create ["x", Type.integer]) (create []))
    (create ["x", Type.Top]);
  assert_state_equal
    (State.join (create ["x", Type.Top]) (create []))
    (create ["x", Type.Top]);

  (* partial order *)
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["x", Type.string]))
    (create ["x", Type.union [Type.string; Type.integer]]);
  assert_state_equal
    (State.join
       (create ["x", Type.integer])
       (create ["y", Type.integer]))
    (create
       ["x", Type.Top; "y", Type.Top])


let test_widen _ =
  let widening_threshold = 10 in
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:0)
    (create ["x", Type.union [Type.integer; Type.string]]);
  assert_state_equal
    (State.widen
       ~previous:(create ["x", Type.string])
       ~next:(create ["x", Type.integer])
       ~iteration:(widening_threshold + 1))
    (create ["x", Type.Top])


let test_check_annotation _ =
  let assert_check_annotation source expression descriptions =
    let resolution = Test.resolution ~sources:(parse source :: Test.typeshed_stubs) () in
    let state = create ~resolution [] in
    let { State.errors; _ }, _ = State.parse_and_check_annotation ~state !expression in
    let errors = List.map ~f:(Error.description ~detailed:false) (Map.data errors) in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      descriptions
      errors
  in
  assert_check_annotation
    ""
    "x"
    ["Undefined type [11]: Type `x` is not defined."];
  assert_check_annotation
    "x: int = 1"
    "x"
    ["Invalid type [31]: Expression `x` is not a valid type."];
  assert_check_annotation
    "x: typing.Type[int] = int"
    "x"
    []


let test_forward_expression _ =
  let assert_forward
      ?(precondition = [])
      ?(postcondition = [])
      ?(errors = `Undefined 0)
      expression
      annotation =
    let expression =
      parse expression
      |> Preprocessing.expand_format_string
      |> function
      | { Source.statements = [{ Node.value = Statement.Expression expression; _ }]; _ } ->
          expression
      | { Source.statements = [{ Node.value = Statement.Yield expression; _ }]; _ } ->
          expression
      | _ ->
          failwith "Unable to extract expression"
    in
    let { State.state = forwarded; resolved } =
      State.forward_expression
        ~state:(create precondition)
        ~expression
    in
    assert_equal ~cmp:Type.equal ~printer:Type.show annotation resolved;
    assert_state_equal (create postcondition) forwarded;
    let errors =
      match errors with
      | `Specific errors ->
          errors
      | `Undefined count ->
          let rec errors sofar count =
            let error = "Undefined name [18]: Global name `undefined` is undefined." in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(Error.description ~detailed:false))
  in

  (* Access. *)
  assert_forward
    ~precondition:["x", Type.integer]
    ~postcondition:["x", Type.integer]
    "x"
    Type.integer;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    "x.add_key(1)"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.integer]
    "x.add_value(1)"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.string]
    "x.add_both(1, 'string')"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.integer ~value:Type.Bottom]
    ~errors:(`Specific [
        "Incompatible parameter type [6]: "^
        "Expected `int` for 1st anonymous parameter to call `dict.add_key` but got `str`.";
      ])
    "x.add_key('string')"
    Type.none;
  assert_forward
    ~precondition:["x", Type.dictionary ~key:Type.Bottom ~value:Type.Bottom]
    ~postcondition:["x", Type.dictionary ~key:Type.Top ~value:Type.Bottom]
    ~errors:(`Undefined 1)
    "x.add_key(undefined)"
    Type.none;

  (* Await. *)
  assert_forward "await awaitable_int()" Type.integer;
  assert_forward
    ~errors:(`Specific [
        "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
        "Undefined name [18]: Global name `undefined` is undefined.";
      ])
    "await undefined"
    Type.Top;

  (* Boolean operator. *)
  assert_forward "1 or 'string'" (Type.union [Type.integer; Type.string]);
  assert_forward "1 and 'string'" (Type.union [Type.integer; Type.string]);
  assert_forward ~errors:(`Undefined 1) "undefined or 1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "1 or undefined" Type.Top;
  assert_forward ~errors:(`Undefined 2) "undefined and undefined" Type.Top;

  let assert_optional_forward ?(postcondition = ["x", Type.optional Type.integer]) =
    assert_forward ~precondition:["x", Type.optional Type.integer] ~postcondition
  in
  assert_optional_forward "x or 1" Type.integer;
  assert_optional_forward "1 or x" (Type.optional Type.integer);
  assert_optional_forward "x or x" (Type.optional Type.integer);

  assert_optional_forward ~postcondition:["x", Type.integer] "x and 1" (Type.optional Type.integer);
  assert_optional_forward "1 and x" (Type.optional Type.integer);
  assert_optional_forward ~postcondition:["x", Type.integer] "x and x" (Type.optional Type.integer);

  (* Comparison operator. *)
  assert_forward "1 < 2" Type.bool;
  assert_forward "1 < 2 < 3" Type.bool;
  assert_forward "1 is 2" Type.bool;
  assert_forward
    ~precondition:["container", Type.list Type.integer]
    ~postcondition:["container", Type.list Type.integer]
    "1 in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.list Type.integer]
    ~postcondition:["container", Type.list Type.integer]
    "1 not in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.iterator Type.integer]
    ~postcondition:["container", Type.iterator Type.integer]
    "1 in container"
    Type.bool;
  assert_forward
    ~precondition:["container", Type.iterator Type.integer]
    ~postcondition:["container", Type.iterator Type.integer]
    "1 not in container"
    Type.bool;
  assert_forward ~errors:(`Undefined 1) "undefined < 1" Type.Top;
  assert_forward ~errors:(`Undefined 2) "undefined == undefined" Type.Top;

  (* Complex literal. *)
  assert_forward "1j" Type.complex;
  assert_forward "1" Type.integer;
  assert_forward "\"\"" Type.string;
  assert_forward "b\"\"" Type.bytes;

  (* Dictionaries. *)
  assert_forward "{1: 1}" (Type.dictionary ~key:Type.integer ~value:Type.integer);
  assert_forward "{1: 'string'}" (Type.dictionary ~key:Type.integer ~value:Type.string);
  assert_forward "{b'': ''}" (Type.dictionary ~key:Type.bytes ~value:Type.string);
  assert_forward
    "{1: 1, 'string': 1}"
    (Type.dictionary ~key:(Type.union [Type.integer; Type.string]) ~value:Type.integer);
  assert_forward
    "{1: 1, 1: 'string'}"
    (Type.dictionary ~key:Type.integer ~value:(Type.union [Type.integer; Type.string]));
  assert_forward "{**{1: 1}}" (Type.dictionary ~key:Type.integer ~value:Type.integer);
  assert_forward
    "{**{1: 1}, **{'a': 'b'}}"
    (Type.dictionary ~key:Type.Object ~value:Type.Object);
  assert_forward
    ~errors:(`Undefined 1)
    "{1: 'string', **{undefined: 1}}"
    (Type.dictionary ~key:Type.Top ~value:Type.Object);
  assert_forward
    ~errors:(`Undefined 1)
    "{undefined: 1}"
    (Type.dictionary ~key:Type.Top ~value:Type.integer);
  assert_forward
    ~errors:(`Undefined 1)
    "{1: undefined}"
    (Type.dictionary ~key:Type.integer ~value:Type.Top);
  assert_forward
    ~errors:(`Undefined 3)
    "{1: undefined, undefined: undefined}"
    (Type.dictionary ~key:Type.Top ~value:Type.Top);
  assert_forward
    "{key: value for key in [1] for value in ['string']}"
    (Type.dictionary ~key:Type.integer ~value:Type.string);

  (* Ellipses. *)
  assert_forward "..." Type.ellipses;

  (* False literal. *)
  assert_forward "False" Type.bool;

  (* Float literal. *)
  assert_forward "1.0" Type.float;

  (* Generators. *)
  assert_forward "(element for element in [1])" (Type.generator Type.integer);
  assert_forward
    "((element, independent) for element in [1] for independent in ['string'])"
    (Type.generator (Type.tuple [Type.integer; Type.string]));
  assert_forward
    "(nested for element in [[1]] for nested in element)"
    (Type.generator Type.integer);
  assert_forward
    ~errors:(`Undefined 1)
    "(undefined for element in [1])"
    (Type.generator Type.Top);
  assert_forward
    ~errors:(`Undefined 1)
    "(element for element in undefined)"
    (Type.generator Type.Top);

  (* Lambda. *)
  let callable ~parameters ~annotation =
    let parameters =
      let open Type.Callable in
      let to_parameter name =
        Parameter.Named {
          Parameter.name = Access.create name;
          annotation = Type.Object;
          default = false;
        }
      in
      Defined (List.map parameters ~f:to_parameter)
    in
    Type.callable ~parameters ~annotation ()
  in
  assert_forward "lambda: 1" (callable ~parameters:[] ~annotation:Type.integer);
  assert_forward
    "lambda parameter: parameter"
    (callable
       ~parameters:["parameter"]
       ~annotation:Type.Object);
  assert_forward
    ~errors:(`Undefined 1)
    "lambda: undefined"
    (callable ~parameters:[] ~annotation:Type.Top);

  (* Lists. *)
  assert_forward "[]" (Type.list Type.Bottom);
  assert_forward "[1]" (Type.list Type.integer);
  assert_forward "[1, 'string']" (Type.list (Type.union [Type.integer; Type.string]));
  assert_forward ~errors:(`Undefined 1) "[undefined]" (Type.list Type.Top);
  assert_forward ~errors:(`Undefined 2) "[undefined, undefined]" (Type.list Type.Top);
  assert_forward "[element for element in [1]]" (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "[*x]"
    (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "[1, *x]"
    (Type.list Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "['', *x]"
    (Type.list (Type.union [Type.string; Type.integer]));

  (* Sets. *)
  assert_forward "{1}" (Type.set Type.integer);
  assert_forward "{1, 'string'}" (Type.set (Type.union [Type.integer; Type.string]));
  assert_forward ~errors:(`Undefined 1) "{undefined}" (Type.set Type.Top);
  assert_forward ~errors:(`Undefined 2) "{undefined, undefined}" (Type.set Type.Top);
  assert_forward "{element for element in [1]}" (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "{*x}"
    (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.list Type.integer]
    ~postcondition:["x", Type.list Type.integer]
    "{1, *x}"
    (Type.set Type.integer);
  assert_forward
    ~precondition:["x", Type.set Type.integer]
    ~postcondition:["x", Type.set Type.integer]
    "{'', *x}"
    (Type.set (Type.union [Type.string; Type.integer]));

  (* Starred expressions. *)
  assert_forward "*1" Type.Top;
  assert_forward "**1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "*undefined" Type.Top;

  (* String literals. *)
  assert_forward "'string'" Type.string;
  assert_forward "f'string'" Type.string;
  assert_forward "f'string{1}'" Type.string;
  assert_forward ~errors:(`Undefined 1) "f'string{undefined}'" Type.string;

  (* Ternaries. *)
  assert_forward "3 if True else 1" Type.integer;
  assert_forward "1.0 if True else 1" Type.float;
  assert_forward "1 if True else 1.0" Type.float;
  assert_forward ~errors:(`Undefined 1) "undefined if True else 1" Type.Top;
  assert_forward ~errors:(`Undefined 1) "1 if undefined else 1" Type.integer;
  assert_forward ~errors:(`Undefined 1) "1 if True else undefined" Type.Top;
  assert_forward ~errors:(`Undefined 3) "undefined if undefined else undefined" Type.Top;

  (* True literal. *)
  assert_forward "True" Type.bool;

  (* Tuples. *)
  assert_forward "1," (Type.tuple [Type.integer]);
  assert_forward "1, 'string'" (Type.tuple [Type.integer; Type.string]);
  assert_forward ~errors:(`Undefined 1) "undefined," (Type.tuple [Type.Top]);
  assert_forward ~errors:(`Undefined 2) "undefined, undefined" (Type.tuple [Type.Top; Type.Top]);

  (* Unary expressions. *)
  assert_forward "not 1" Type.bool;
  assert_forward ~errors:(`Undefined 1) "not undefined" Type.bool;
  assert_forward "-1" Type.integer;
  assert_forward "+1" Type.integer;
  assert_forward "~1" Type.integer;
  assert_forward ~errors:(`Undefined 1) "-undefined" Type.Top;

  (* Yield. *)
  assert_forward "yield 1" (Type.generator Type.integer);
  assert_forward ~errors:(`Undefined 1) "yield undefined" (Type.generator Type.Top);
  assert_forward "yield" (Type.generator Type.none)


let test_forward_statement _ =
  let assert_forward
      ?(precondition_immutables = [])
      ?(postcondition_immutables = [])
      ?expected_return
      ?(errors = `Undefined 0)
      ?(bottom = false)
      precondition
      statement
      postcondition =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement::rest; _ } -> statement::rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward_statement ~state ~statement)
        ~init:(create ?expected_return ~immutables:precondition_immutables precondition)
        parsed
    in
    assert_state_equal
      (create ~bottom ~immutables:postcondition_immutables postcondition)
      forwarded;
    let errors =
      match errors with
      | `Specific errors ->
          errors
      | `Undefined count ->
          let rec errors sofar count =
            let error = "Undefined name [18]: Global name `undefined` is undefined." in
            match count with
            | 0 -> sofar
            | count -> errors (error :: sofar) (count - 1)
          in
          errors [] count
    in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:"\n")
      errors
      (State.errors forwarded |> List.map ~f:(Error.description ~detailed:false))
  in

  (* Assignments. *)
  assert_forward ["y", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = z"
    ["x", Type.Top; "y", Type.integer; "z", Type.Top];
  assert_forward ["x", Type.integer] "x += 1" ["x", Type.integer];

  assert_forward
    ["z", Type.integer]
    "x = y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.integer];

  (* Assignments with tuples. *)
  assert_forward
    ["c", Type.integer; "d", Type.Top]
    "a, b = c, d"
    ["a", Type.integer; "b", Type.Top; "c", Type.integer; "d", Type.Top];
  assert_forward
    ~errors:
      (`Specific ["Unable to unpack [23]: Unable to unpack `int` into 2 values."])
    ["z", Type.integer]
    "x, y = z"
    ["x", Type.Top; "y", Type.Top; "z", Type.integer];

  assert_forward
    ~errors:
      (`Specific ["Unable to unpack [23]: Unable to unpack 3 values, 2 were expected."])
    ["z", Type.tuple [Type.integer; Type.string; Type.string]]
    "x, y = z"
    ["x", Type.Top; "y", Type.Top; "z", Type.tuple [Type.integer; Type.string; Type.string]];

  assert_forward
    ["y", Type.integer; "z", Type.Top]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.Top]; "y", Type.integer; "z", Type.Top];
  assert_forward
    ["z", Type.tuple [Type.integer; Type.string]]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_forward
    ["z", Type.Tuple (Type.Unbounded Type.integer)]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.Tuple (Type.Unbounded Type.integer)];
  assert_forward
    ~errors:
      (`Specific [
          "Unable to unpack [23]: Unable to unpack `unknown` into 2 values.";
        ])
    []
    "(x, y), z = 1"
    ["x", Type.Top; "y", Type.Top; "z", Type.Top];
  assert_forward
    ["z", Type.list Type.integer]
    "x, y = z"
    ["x", Type.integer; "y", Type.integer; "z", Type.list Type.integer];
  assert_forward
    []
    "x, y = return_tuple()"
    ["x", Type.integer; "y", Type.integer;];
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Bounded [])];

  (* Assignments with list. *)
  assert_forward
    ["x", Type.list Type.integer]
    "[a, b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.integer];
  assert_forward
    ["x", Type.list Type.integer]
    "[a, *b] = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.list Type.integer];
  assert_forward
    ["x", Type.list Type.integer]
    "a, *b = x"
    ["x", Type.list Type.integer; "a", Type.integer; "b", Type.list Type.integer];

  (* Assignments with uniform sequences. *)
  assert_forward
    ["x", Type.iterable Type.integer]
    "[a, b] = x"
    ["x", Type.iterable Type.integer; "a", Type.integer; "b", Type.integer];
  assert_forward
    ["c", Type.Tuple (Type.Unbounded Type.integer)]
    "a, b = c"
    ["a", Type.integer; "b", Type.integer; "c", Type.Tuple (Type.Unbounded Type.integer)];

  (* Assignments with non-uniform sequences. *)
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "*a, b = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.list (Type.union [Type.integer; Type.string]);
      "b", Type.float;
    ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.float]]
    "a, *b = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.float]);
    ];
  assert_forward
    ["x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float]]
    "a, *b, c = x"
    [
      "x", Type.tuple [Type.integer; Type.string; Type.integer; Type.float];
      "a", Type.integer;
      "b", Type.list (Type.union [Type.string; Type.integer]);
      "c", Type.float;
    ];

  (* Assignments with immutables. *)
  assert_forward ~postcondition_immutables:["x", true] [] "global x" ["x", Type.Top];
  assert_forward ~postcondition_immutables:["y", false] [] "y: int" ["y", Type.integer];
  assert_forward
    ~errors:(`Specific [ "Undefined name [18]: Global name `x` is undefined."])
    ~postcondition_immutables:["y", false]
    []
    "y: int = x"
    ["y", Type.integer];
  assert_forward
    ~precondition_immutables:["y", false]
    ~postcondition_immutables:["y", false]
    ["x", Type.Top; "y", Type.Top]
    "y = x"
    ["x", Type.Top; "y", Type.Top];
  assert_forward
    ~precondition_immutables:["y", false]
    ~postcondition_immutables:["y", false]
    ["y", Type.string]
    "y: int"
    ["y", Type.integer];

  (* Assert. *)
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x"
    ["x", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.integer]
    "assert y"
    ["x", Type.optional Type.integer; "y", Type.integer];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is not None"
    ["x", Type.integer];

  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x and y"
    ["x", Type.integer; "y", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float; "z", Type.optional Type.float]
    "assert x and (y and z)"
    ["x", Type.integer; "y", Type.float; "z", Type.float];
  assert_forward
    ["x", Type.optional Type.integer; "y", Type.optional Type.float]
    "assert x or y"
    ["x", Type.optional Type.integer; "y", Type.optional Type.float];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert x is None"
    ["x", Type.optional Type.Bottom];
  assert_forward
    ["x", Type.optional Type.integer]
    "assert (not x) or 1"
    ["x", Type.optional Type.integer];

  (* Isinstance. *)
  assert_forward ["x", Type.Object] "assert isinstance(x, int)" ["x", Type.integer];
  assert_forward
    ["x", Type.Object; "y", Type.Top]
    "assert isinstance(y, str)"
    ["x", Type.Object; "y", Type.string];
  assert_forward
    ["x", Type.Object]
    "assert isinstance(x, (int, str))"
    ["x", Type.union [Type.integer; Type.string]];
  assert_forward
    ["x", Type.integer]
    "assert isinstance(x, (int, str))"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ["x", Type.integer]
    "assert isinstance(x, str)"
    ["x", Type.string];
  assert_forward
    ~bottom:false
    ["x", Type.Bottom]
    "assert isinstance(x, str)"
    ["x", Type.string];
  assert_forward
    ~bottom:false
    ["x", Type.float]
    "assert isinstance(x, int)"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ~errors:
      (`Specific
         ["Incompatible parameter type [6]: " ^
          "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
          "but got `int`."])
    ["x", Type.integer]
    "assert isinstance(x, 1)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x` has type `int`, checking if `x` not " ^
          "isinstance `int` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, int)"
    ["x", Type.integer];
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x` has type `int`, checking if `x` not " ^
          "isinstance `float` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x, float)"
    ["x", Type.integer];
  assert_forward
    ~bottom:false
    ["x", Type.float]
    "assert not isinstance(x, int)"
    ["x", Type.float];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, int)"
    ["x", Type.optional Type.string];
  assert_forward
    ["x", Type.optional (Type.union [Type.integer; Type.string])]
    "assert not isinstance(x, type(None))"
    [
      "$type", Type.meta Type.none;
      "x", Type.union [Type.integer; Type.string];
    ];

  (* Works for general expressions. *)
  assert_forward
    ~errors:
      (`Specific
         ["Impossible isinstance check [25]: `x.__add__(1)` has type `int`, checking if " ^
          "`x.__add__(1)` not isinstance `int` will always fail."])
    ~bottom:true
    ["x", Type.integer]
    "assert not isinstance(x + 1, int)"
    ["x", Type.integer];

  assert_forward
    ~bottom:false
    ["x", Type.Bottom]
    "assert not isinstance(x, int)"
    ["x", Type.Bottom];

  assert_forward
    ~bottom:true
    []
    "assert False"
    [];
  assert_forward
    ~bottom:false
    []
    "assert (not True)"
    [];

  (* Raise. *)
  assert_forward [] "raise 1" [];
  assert_forward ~errors:(`Undefined 1) [] "raise undefined" [];
  assert_forward [] "raise" [];

  (* Return. *)
  assert_forward
    ~errors:
      (`Specific
         ["Missing return annotation [3]: Returning `int` but no return type is specified."])
    []
    "return 1"
    [];
  assert_forward ~expected_return:Type.integer [] "return 1" [];
  assert_forward
    ~expected_return:Type.string
    ~errors:(`Specific ["Incompatible return type [7]: Expected `str` but got `int`."])
    []
    "return 1"
    [];

  (* Pass. *)
  assert_forward ["y", Type.integer] "pass" ["y", Type.integer]


let test_forward _ =
  let assert_forward
      ?(precondition_bottom = false)
      ?(postcondition_bottom = false)
      precondition
      statement
      postcondition =
    let forwarded =
      let parsed =
        parse statement
        |> function
        | { Source.statements = statement::rest; _ } -> statement::rest
        | _ -> failwith "unable to parse test"
      in
      List.fold
        ~f:(fun state statement -> State.forward ~statement state)
        ~init:(create ~bottom:precondition_bottom precondition)
        parsed
    in
    assert_state_equal (create ~bottom:postcondition_bottom postcondition) forwarded;
  in

  assert_forward [] "x = 1" ["x", Type.integer];
  assert_forward ~precondition_bottom:true ~postcondition_bottom:true [] "x = 1" [];

  assert_forward ~postcondition_bottom:true [] "sys.exit(1)" []


let test_show_error_traces _ =
  assert_type_errors ~show_error_traces:true
    "def foo() -> int: return 1.0"
    [
      "Incompatible return type [7]: Expected `int` but got `float`. Type `int` expected on line " ^
      "1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> str: return"
    [
      "Incompatible return type [7]: Expected `str` but got `None`. " ^
      "Type `str` expected on line 1, specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    "def foo() -> typing.List[str]: return 1"
    [
      "Incompatible return type [7]: Expected `typing.List[str]` but got `int`. Type " ^
      "`typing.List[str]` expected on line 1, specified on line 1.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [];

  assert_type_errors ~show_error_traces:true
    "def foo(): pass"
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 1, return type should be specified on line 1."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo():
        return None
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified. " ^
      "Type `None` was returned on line 3, return type should be specified on line 2.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`. Attribute `attribute` declared on line 3, incorrectly " ^
      "used on line 5.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant: int
      def foo() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as " ^
      "type `str`. Redeclare `constant` on line 5 if you wish to override the previously " ^
      "declared type.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        a = 1
        b = 2
        reveal_type(a + b)
    |}
    [
      "Revealed type [-1]: Revealed type for `a.__add__.(...)` is `int`.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized. Attribute `attribute` is declared on " ^
      "line 3, never initialized and therefore must be `typing.Optional[int]`.";
    ];
  assert_type_errors ~show_error_traces:true
    {|
      class Foo:
        attribute = x
      class Bar:
        def bar(self) -> str:
          foo = Foo()
          foo.attribute = 'string'
          return foo.attribute
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `str` " ^
      "but no type is specified. Attribute `attribute` declared on line 3, type `str` deduced " ^
      "from test.py:7:4.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified. Global variable `constant` declared on line 2, type `int` deduced " ^
      "from test.py:5:2.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      constant = x
      def foo() -> None:
        global constant
        constant = "hi"
        constant = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `constant` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `constant` declared " ^
      "on line 2, type `typing.Union[int, str]` deduced from test.py:5:2, test.py:6:2.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      class Other():
        attribute = x
        def foo(self) -> None:
          self.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Other` has type " ^
      "`int` but no type is specified. Attribute `attribute` declared on line 3, " ^
      "type `int` deduced from test.py:5:4.";
      "Undefined name [18]: Global name `x` is undefined.";
    ];

  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> None:
        global x
        x = 5
      def bar() -> None:
        global x
        x = "str"
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `x` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `x` " ^
      "declared on line 4, type `typing.Union[int, str]` deduced from test.py:4:2, " ^
      "test.py:7:2.";
      "Missing global annotation [5]: Globally accessible variable `x` has type " ^
      "`typing.Union[int, str]` but no type is specified. Global variable `x` " ^
      "declared on line 7, type `typing.Union[int, str]` deduced from test.py:4:2, " ^
      "test.py:7:2."
    ];

  assert_type_errors ~show_error_traces:true
    {|
      a: typing.List[float] = [1]
      b: typing.List[int] = [2]
      a = b
    |}
    [
      "Incompatible variable type [9]: a is declared to have type `typing.List[float]` but is \
       used as type `typing.List[int]`. Redeclare `a` on line 4 if you wish to override the \
       previously declared type.  See https://pyre-check.org/docs/error-types.html#list-and-\
       dictionary-mismatches-with-subclassing for mutable container errors.";
    ];
  assert_type_errors ~show_error_traces:true
    {|
      def foo() -> typing.List[float]:
        l = [1]
        return l
    |}
    [
      "Incompatible return type [7]: Expected `typing.List[float]` but got `typing.List[int]`. \
       Type `typing.List[float]` expected on line 4, specified on line 2.  \
       See https://pyre-check.org/docs/error-types.html#list-and-dictionary-mismatches-with-\
       subclassing for mutable container errors.";
    ]


let test_check_imports _ =
  assert_type_errors
    {|
      import durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      import typing
    |}
    [];
  assert_type_errors
    {|
      import typing, durp
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      from typing import durp
    |}
    [];
  assert_type_errors
    {|
      from durp import typing
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  (* Ensure we don't double-error. *)
  assert_type_errors
    {|
      a = durp.x
    |}
    ["Undefined name [18]: Global name `durp` is undefined."];
  assert_type_errors
    {|
      import durp
      a = durp.x
    |}
    ["Undefined import [21]: Could not find a module corresponding to import `durp`."];
  assert_type_errors
    {|
      from typing import Optional
      def foo() -> None: return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."]


let test_reveal_type _ =
  assert_type_errors
    {|
      def foo(x: str) -> None:
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
    ];

  assert_type_errors
    ~debug:false
    {|
      def foo(x) -> None:
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `undefined`.";
    ];
  assert_type_errors
    {|
      def foo(x: int, y: int) -> None:
        reveal_type(x + y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x.__add__.(...)` is `int`.";
    ];
  assert_type_errors
    {|
      def foo(x: int) -> None:

        reveal_type(int_to_str(x))
    |}
    [
      "Revealed type [-1]: Revealed type for `int_to_str.(...)` is `str`.";
    ]


let test_coverage _ =
  let assert_coverage source expected =
    let coverage =
      let environment = Test.environment () in
      let handle = "coverage_test.py" in
      TypeCheck.check
        ~configuration:Test.mock_configuration
        ~environment
        ~source:(parse ~handle source)
      |> ignore;
      Coverage.get ~handle:(File.Handle.create handle)
      |> (fun coverage -> Option.value_exn coverage)
    in
    assert_equal ~printer:Coverage.show expected coverage
  in
  assert_coverage
    {| def foo(): pass |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 0 };
  assert_coverage
    {|
      def foo(y: int):
        if condition():
          x = y
        else:
          x = z
    |}
    { Coverage.full = 1; partial = 0; untyped = 1; ignore = 0; crashes = 0 };
  assert_coverage
    {|
      def foo(y: asdf):
        if condition():
          x = y
        else:
          x = 1
    |}
    { Coverage.full = 0; partial = 0; untyped = 0; ignore = 0; crashes = 1 };

  assert_coverage
    {|
      def foo(y) -> int:
        x = returns_undefined()
        return x
    |}
    { Coverage.full = 1; partial = 0; untyped = 1; ignore = 0; crashes = 0 }


let test_check _ =
  assert_type_errors
    {|
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> int:
        return x[0]
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x[0:1]
    |}
    [];

  assert_type_errors
    ~debug:false
    {|
      def f(x) -> int:
        class Stub:
          ...
        class Actual:
          def f() -> int:
            return 0
        if isinstance(x, Stub):
          return -1
        elif isinstance(x, Actual):
          return 0
        else:
          return 1
    |}
    [];

  assert_type_errors
    {|
      isinstance(1, NonexistentClass)
    |}
    ["Undefined name [18]: Global name `NonexistentClass` is undefined."];

  assert_type_errors "isinstance(1, (int, str))" [];
  assert_type_errors "isinstance(1, (int, (int, str)))" [];
  assert_type_errors
    "isinstance(str, '')"
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
     "but got `str`."];
  assert_type_errors
    "isinstance(1, (int, ('', str)))"
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
     "but got `str`."];

  assert_type_errors
    {|
      _T = typing.TypeVar("_T")
      def meta(x: typing.Type[_T]) -> None: ...
      meta(typing.Dict)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Type[Variable[_T]]` for 1st anonymous parameter to call `meta` but got " ^
     "`typing.TypeAlias`."];

  assert_type_errors
    {|
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];

  assert_type_errors
    {|
      class other(): pass
      def foo() -> other:
        result = 0
        if True:
          result = not_annotated()
        return result
    |}
    ["Incompatible return type [7]: Expected `other` but got `unknown`."];

  assert_type_errors
    {|
      def derp()->int:
          a, b = return_tuple()
          return a+b
    |}
    [];

  assert_type_errors
    {|
      @typing.overload
      def overloaded()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def overloaded()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      class Derp:
        @property
        async def get_int(self) -> int:
          return 5

        def test(self) -> int:
          x = await self.get_int
          return x
    |}
    [];

  assert_type_errors
    {|
      def f(x: int) -> None:
        x: str = int_to_str(x)
    |}
    [];

  assert_type_errors
    {|
      def derp()->typing.Union[str, None]:
          return None
    |}
    [];

  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> str:
          return ''

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)
    |}
    [];

  assert_type_errors
    {|
      class WithClass():
        def __enter__(self) -> int:
          return 5

      def expect_string(x: str) -> None:
        pass

      def test() -> None:
        with WithClass() as x:
          expect_string(x)

    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expect_string` but got `int`."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(1,2)
    |}
    ["Too many arguments [19]: Call `foo` expects 1 argument, 2 were provided."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        a = foo(y=4)
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `y` to call `foo`."];

  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f()
    |}
    ["Missing argument [20]: Call `C.f` expects argument `x`."];

  assert_type_errors
    {|
      class C:
        def f(self, x: str) -> None:
          ...
      def f(c: C) -> None:
        a = c.f("", "")
    |}
    ["Too many arguments [19]: Call `C.f` expects 1 argument, 2 were provided."];

  assert_type_errors
    {|
      def foo(x: int, y: str) -> str:
        return ""
      def f() -> None:
        a = foo()
    |}
    ["Missing argument [20]: Call `foo` expects argument `x`."];

  assert_type_errors
    {|
      def foo() -> str:
        return ""
      def f() -> None:
        a = foo(1,2,3,4)
    |}
    ["Too many arguments [19]: Call `foo` expects 0 arguments, 4 were provided."];

  assert_type_errors
    {|
      class C:
        def __init__(self, x: int) -> None:
          self.a = x
        def a(self) -> int:
          return self.a
    |}
    [];

  assert_type_errors
    {|
      def identity(x: int) -> int:
        return x
      class C:
        def __init__(self, x: int) -> None:
          self.a = identity(x)
        def a(self) -> int:
          return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a`" ^
      " of class `C` has type `int` but no type is specified.";
      "Incompatible return type [7]: Expected `int` but got `unknown`."
    ];

  assert_type_errors
    {|
      def foo( *args: int) -> typing.Iterable[str]:
        return args
    |}
    [
      "Incompatible return type [7]: Expected `typing.Iterable[str]` but " ^
      "got `typing.Tuple[int, ...]`.";
    ];

  assert_type_errors
    {|
      def foo( **kwargs: int) -> None:
        return kwargs
    |}
    ["Incompatible return type [7]: Expected `None` but got `typing.Dict[str, int]`."];

  assert_type_errors
    {|
      def f( *args: int) -> None:
       pass
      def g( *args: int) -> None:
        return f( *args)
    |}
    [];
  assert_type_errors
    {|
      def f(a: int, b: int) -> None:
       pass
      def g(collection: typing.Collection[str]) -> None:
        return f( *collection)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `f` but got `str`."];
  assert_type_errors
    {|
      class C(typing.Iterable[int]):
        ...
      def f(a: int, b: int) -> None:
       pass
      def g(c: C) -> None:
        return f( *c)
    |}
    [];

  assert_type_errors
    {|
      def f() -> str:
        return __name__
      def g() -> str:
        return __file__
      def h() -> str:
        return typing.__name__
      def i() -> str:
        return ...
    |}
    ["Incompatible return type [7]: Expected `str` but got `ellipses`."];

  assert_type_errors
    {|
      def f() -> int:
        return builtins.__name__
    |}
    ["Incompatible return type [7]: Expected `int` but got `unknown`."];

  assert_type_errors
    {|
      def f(meta: typing.Type[int]) -> type[int]:
        return meta
    |}
    [];

  assert_type_errors
    {|
      def f(meta: type[int]) -> typing.Type[int]:
        return meta
    |}
    [];

  assert_type_errors
    {|
      def f(meta: type) -> typing.Type[int]:
        return meta
    |}
    [
      "Incompatible return type [7]: Expected `typing.Type[int]` but got " ^
      "`typing.Type[typing.Any]`.";
    ];

  assert_type_errors
    {|
      def expect_type_float(meta: typing.Type[float]) -> None:
        pass
      expect_type_float(int)
    |}
    [];

  (* object methods are picked up for optionals. *)
  assert_type_errors
    {|
      def f() -> int:
        return None.__sizeof__()
    |}
    [];

  assert_type_errors
    {|
      def f(x: typing.List[int]) -> typing.Set[str]:
        return {1, *x}
    |}
    ["Incompatible return type [7]: Expected `typing.Set[str]` but got `typing.Set[int]`."];

  assert_type_errors
    {|
      def foo() -> int:
        bar, baz = list(range(2))
        reveal_type(bar)
        return bar
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];

  assert_type_errors
    {|
      def foo(s: typing.Sequence[float]) -> list[float]:
        l = list(s)
        bar, baz = l
        reveal_type(bar)
        return l
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `float`."];

  assert_type_errors
    {|
      def foo() -> dict[str, int]:
        d = dict(a = 1, b = 2)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];

  assert_type_errors
    {|
      def foo(map: typing.Mapping[str, int]) -> dict[str, int]:
        d = dict(map)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];

  assert_type_errors
    {|
      def foo(t: typing.Iterable[typing.Tuple[str, int]]) -> dict[str, int]:
        d = dict(t)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."]


let test_check_in _ =
  assert_type_errors
    {|
      class WeirdContains:
        def __contains__(self, x: int) -> int:
          ...
      reveal_type(1 in WeirdContains())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdContains()` is `int`."];

  assert_type_errors
    {|
      class WeirdIterator:
        def __eq__(self, other) -> str:
          ...
        def __iter__(self) -> typing.Iterator[WeirdIterator]:
          ...
      reveal_type(1 in WeirdIterator())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdIterator()` is `str`."];
  assert_type_errors
    {|
      class WeirdEqual:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class WeirdGetItem:
        def __getitem__(self, x: int) -> WeirdEqual:
          ...
      reveal_type(1 in WeirdGetItem())
    |}
    ["Revealed type [-1]: Revealed type for `1 in WeirdGetItem()` is `typing.List[int]`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __iter__(self, x: int) -> typing.Iterator[Equal]:
          ...
        def __contains__(self, a: object) -> bool:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in Multiple()` is `bool`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> str:
          ...
      class Multiple:
        def __getitem__(self, x: int) -> Equal:
          ...
        def __contains__(self, a: object) -> int:
          ...
      reveal_type(1 in Multiple())
    |}
    ["Revealed type [-1]: Revealed type for `1 in Multiple()` is `int`."];
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItemA:
        def __getitem__(self, x: int) -> Equal:
          ...
      class GetItemB:
        def __getitem__(self, x: int) -> Equal:
          ...
      def foo(a: typing.Union[GetItemA, GetItemB]) -> None:
        5 in a
    |}
    []


let test_check_assign _ =
  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x = 'string'  # Reassignment is okay.
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        x: int = 1
        x = 'string'
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type `str`."
    ];

  assert_type_errors
    {|
      def foo(input: typing.Tuple[int, str]) -> None:
        x = input
    |}
    [];

  assert_type_errors
    {|
      def foo(input: int) -> None:
        x, y = input
    |}
    ["Unable to unpack [23]: Unable to unpack `int` into 2 values."];


  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        x += 'asdf'
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."]


let test_check_coverage _ =
  let preprocess source =
    trim_extra_indentation source
    |> String.lstrip
    |> String.split ~on:'\n'
    |> List.map ~f:(fun line -> "    " ^ line)
    |> String.concat ~sep:"\n"
    |> String.substr_replace_all ~pattern:"ERROR" ~with_:"a.undefined"
    |> Format.asprintf "def foo(a: A) -> None:\n%s\n"
  in
  let assert_covered ?(additional_errors = []) source =
    assert_type_errors
      (preprocess source)
      (additional_errors @ ["Undefined attribute [16]: `A` has no attribute `undefined`."])
  in
  let assert_not_covered ?(additional_errors = []) source =
    assert_type_errors (preprocess source) additional_errors
  in

  (* Return statement. *)
  assert_covered
    ~additional_errors:["Incompatible return type [7]: Expected `None` but got `unknown`."]
    "return ERROR";

  (* Assignment. *)
  assert_covered "b = ERROR";

  (* Assertion. *)
  assert_covered "assert ERROR";

  (* Nested definitions. *)
  assert_covered "class B: ERROR";
  assert_covered
    ~additional_errors:[]
    "def nested() -> None: ERROR";

  (* Expressions. *)
  assert_covered "ERROR";

  (* Yield. *)
  assert_covered
    ~additional_errors:[
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Generator[unknown, None, None]`.";
    ]
    "yield ERROR";
  assert_not_covered
    ~additional_errors:[
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Generator[unknown, None, None]`.";
    ]
    "yield from ERROR";

  (* Control statements. *)
  assert_covered
    "for i in ERROR: pass";
  assert_covered "while ERROR: pass";
  assert_covered "if ERROR: pass";

  (* Raise. *)
  assert_covered "raise ERROR";

  assert_covered
    {|
      try:
        pass
      except ERROR:
        pass
    |};

  assert_covered
    {|
      with ERROR:
        pass
    |};
  assert_covered
    {|
      with ERROR as derp:
        pass
    |};

  (* Await. *)
  assert_covered
    ~additional_errors:[
      "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
    ]
    "await ERROR";

  (* Binary operator. *)
  assert_covered "ERROR | 1";
  assert_covered "1 % ERROR";

  (* Boolean operator. *)
  assert_covered "ERROR or False";
  (* True or UNDEFINED evaluates to True in python. *)
  assert_not_covered "True or ERROR";
  assert_covered "ERROR and False";
  assert_covered "True and ERROR";

  (* Comparison operator. *)
  assert_covered "1 == ERROR";
  assert_covered "ERROR < 1";

  (* Dictionaries. *)
  assert_covered "{ ERROR: 1 }";
  assert_covered "{ 1: ERROR }";
  assert_covered "{ ERROR: i for i in dict() }";
  assert_covered "{ i: ERROR for i in dict() }";
  assert_covered "{ i: 1 for i in ERROR }";

  (* Format string. *)
  assert_covered {|f"format{ERROR}"|};

  (* Generator. *)
  assert_covered "(ERROR for i in list())";

  (* Lambdas. *)
  assert_covered "lambda x: ERROR";

  (* Lists. *)
  assert_covered "[1, ERROR]";
  assert_covered "[ERROR for i in list()]";
  assert_covered "[i for i in ERROR]";

  (* Sets. *)
  assert_covered "{1, ERROR}";
  assert_covered "{ERROR for i in list()}";
  assert_covered "{i for i in ERROR}";

  (* Starred. *)
  assert_covered "*ERROR";
  assert_covered "**ERROR";

  (* Ternary. *)
  assert_covered "ERROR if True else 1";
  assert_covered "True if ERROR else 1";
  assert_covered "True if True else ERROR";

  (* Tuples. *)
  assert_covered "ERROR, True";
  assert_covered "True, ERROR";

  (* Unary operators. *)
  assert_covered "not ERROR";
  assert_covered "-ERROR"


let test_check_comprehensions _ =
  assert_type_errors
    {|
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a for a in input]
    |}
    [];

  assert_type_errors
    {|
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a for a in input if len(a) < 5]
    |}
    [];

  assert_type_errors
    {|
      def foo(input: str) -> typing.List[int]:
        return [a for a in input]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];

  assert_type_errors
    {|
      def foo() -> typing.List[str]:
        return [x for x in [4,3,None, 1] if x]
    |}
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `typing.List[int]`."];

  assert_type_errors
    {|
      def foo(l: typing.List[int]) -> None:
        a = [x > 0 and x < 0 for x in l]
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.Dict[int, str]) -> None:
        [x if y else 0 for x, y in l.items()]
    |}
    [
    ];

  assert_type_errors
    {|
      def foo() -> typing.Dict[int, int]:
        return { 0: x for x in [4,3,2] }
    |}
    [];

  assert_type_errors
    {|
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[int, str]` but got " ^
      "`typing.Dict[]`.";
      "Unable to unpack [23]: Unable to unpack `int` into 2 values.";
    ];

  assert_type_errors
    {|
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d.items() }
    |}
    [];

  assert_type_errors
    {|
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a.lower() for a in input]
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.List[str]) -> typing.List[int]:
        return [str_to_int(a) for a in input]
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Set[str]) -> typing.Set[str]:
        return {a for a in input}
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Set[str]) -> typing.Set[str]:
        return {a.lower() for a in input}
    |}
    [];
  assert_type_errors
    {|
      def foo(a: typing.List[str], b: typing.List[str]) -> int:
        return {x + y for x in a for y in b}
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Set[str]`."];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.List[int]:
        return [x for x in a]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];

  assert_type_errors
    {|
      def f() -> int:
          x = {
              "a": [k[0] for x in {}] for k in []
          }
          return 0
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, str]:
        return { x:x for x, y in a.items() }
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, int]:
        return { y:x for x, y in a.items() }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got " ^
      "`typing.Dict[int, str]`.";
    ];

  assert_type_errors
    {|
      def f() -> None:
        l = lambda y: y
        l(1)
        lambda *y: y
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() if y }
    |}
    [];
  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() }
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got \
       `typing.Dict[str, typing.Optional[int]]`.";
    ];

  assert_type_errors
    {|
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: int_to_int(y) for (x, y) in a.items() if y }
    |}
    [];

  assert_type_errors
    {|
      def foo(d: typing.Dict[str, int]) -> None:
        { k: v for k, v in d }
    |}
    []


let test_check_optional _ =
  assert_type_errors
    "def foo() -> str: return None"
    ["Incompatible return type [7]: Expected `str` but got `None`."];

  assert_type_errors
    "def foo() -> typing.Optional[str]: return None"
    [];

  assert_type_errors
    "def foo() -> typing.Optional[int]: return 1"
    [];

  assert_type_errors
    {|
      def foo(flag: bool) -> typing.Optional[float]:
          a = 1.0
          if flag:
            a = None
          return a
    |}
    [];

  assert_type_errors
    "def foo() -> typing.Optional[int]: return 1.0"
    ["Incompatible return type [7]: Expected `typing.Optional[int]` but got `float`."];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> int:
          if optional:
            return optional
          else:
            return -1
    |}
    []


let test_check_generators _ =
  assert_type_errors
    {|
      x: typing.Generator[int, int, int]
      def foo() -> typing.Generator:
        return x
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[int])->typing.Generator[int, None, None]:
        return (x for x in l)
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[int, None, None]:
        return (x for x in l if x)
    |}
    [];

  assert_type_errors
    {|
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[str, None, None]:
        return (x for x in l if x is not None)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[str, None, None]` " ^
      "but got `typing.Generator[int, None, None]`.";
    ];

  assert_type_errors
    {|
      def foo(l: typing.Iterable[typing.Any])->typing.Generator[typing.Any, None, None]:
        return (x for x in l)
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1
    |}
    [];

  assert_type_errors
    {|
      def foo(i: int) -> typing.Iterable[int]:
        if i > 2:
          return
        else:
          yield i
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield 1.0
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` " ^
      "but got `typing.Generator[float, None, None]`.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield from [1]
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[int, None, None]:
        yield from [""]
    |}
    [
      "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` " ^
      "but got `typing.Generator[str, None, None]`."
    ];


  assert_type_errors
    {|
      def generator() -> typing.Generator[int, None, None]:
        yield 1
      def wrapper() -> typing.Generator[int, None, None]:
        yield from generator()
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Iterable[None]:
        yield
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];

  assert_type_errors
    {|
      def foo(flag: bool) -> typing.Generator[int, None, None]:
        if flag:
          return
        yield 1
    |}
    [];
  assert_type_errors
    {|
      async def foo(flag: bool) -> typing.AsyncGenerator[int, None]:
        if flag:
          return
        yield 1
    |}
    [];

  assert_type_errors
    {|
      @asyncio.coroutines.coroutine
      def get() -> typing.Generator[typing.Any, None, int]: ...
      async def foo() -> int:
        awaited = await get()
        return awaited
    |}
    [];
  assert_type_errors
    {|
      async def foo() -> typing.AsyncGenerator[int, None]:
        yield 1
    |}
    [];

  assert_type_errors
    {|
      def takes_int(x: int) -> int:
        return x
      async def loop(g: typing.AsyncGenerator[str, None]) -> typing.AsyncGenerator[int, None]:
        async for item in g:
          yield takes_int(item)
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `takes_int` but got `str`.";
    ]


let test_check_ternary _ =
  assert_type_errors
    {|
      def foo() -> int:
        x: typing.Optional[int]
        y: int
        z = x if x else y
        return z
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        return y if y else 5
    |}
    [];
  assert_type_errors
    {|
      def foo(x: int) -> int:
        if x > 10:
          y = None
        else:
          y = 5
        y = y if y else 0
        return y
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        y: typing.Optional[int]
        x: int
        return y if x else 5
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
          int_to_int(x) if x else 0
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
          return int_to_int(x if x is not None else 1)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        a, b = ("hi", int_to_int(x) if x is not None else 1)
        return b
    |}
    [];
  assert_type_errors
    {|
      def f(s: str) -> None:
        pass

      def pick_alternative3(s: typing.Optional[str]) -> None:
        x = "foo" if s is None else s
        f(x)

      def pick_target(s: typing.Optional[str]) -> None:
        f(s if s is not None else "foo")

      def pick_target2(s: typing.Optional[str]) -> None:
        f(s if s else "foo")

      def pick_target3(s: typing.Optional[str]) -> None:
        x = s if s is not None else "foo"
        f(x)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[bytes]) -> None: ...
      a: typing.Union[int, bytes]
      foo(x=a if isinstance(a, bytes) else None)
    |}
    []


let test_check_nested _ =
  assert_type_errors
    {|
      def foo() -> None:
        def nested() -> None:
          int_to_int(1.0)
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> None:
        def g() -> None:
          return
        a = g()
    |}
    [];

  assert_type_errors
    {|
      class Derp:
          Word = collections.namedtuple("word", ("verb", "noun"))
      def foo() -> Derp.Word: pass
    |}
    ["Incompatible return type [7]: Expected `Derp.Word` but got implicit return value of `None`."];

  (* Nesting behaves differently for the toplevel function. *)
  assert_type_errors
    ~qualifier:(Access.create "shadowing")
    {|
      def shadowing(i: int) -> None: ...
      shadowing('asdf')  # `shadowing` is not replaced with a dummy entry in the globals map.
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `shadowing.shadowing` but got `str`."];

  assert_type_errors
    {|
      def can_fail() -> None:
        try:
          x = 3
        except:
          pass
        always_declared = 4
        def bar() -> int:
          return always_declared
  |}
    []


let test_check_refinement _ =
  assert_type_errors
    {|
      def takes_int(a: int) -> None: pass
      def foo() -> None:
        x: float
        x = 1
        takes_int(x)
        x = 1.0
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[typing.Any] = []
        l = [1]
        l.append('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `list.append` but got `str`."];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = []
        l.append('a')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `list.append` but got `str`."];

  assert_type_errors
    {|
      def foo() -> None:
        l: typing.List[int] = None
        l.append('a')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `list.append` but got `str`."];

  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if not x:
          y = x
        return x
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`."];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def foo(self) -> None:
              if self.a is None:
                  self.a = 5
    |}
    [];
  assert_type_errors
    {|
      class A:
          a: typing.Optional[int] = None
          def bar(self) -> int:
              if self.a is not None:
                  return self.a
              else:
                  return 1
    |}
    [];
  assert_type_errors
    {|
      def bar(x: typing.Optional[int]) -> None:
          if x and int_to_int(x) < 0:
              y = 1
    |}
    [];
  assert_type_errors
    {|
      def bar(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
          if not input:
            input = set()
          return input
    |}
    [];

  assert_type_errors
    {|
      def bar(input: typing.Optional[int]) -> int:
          if not input:
            input = not_annotated()
          return input
    |}
    [
      "Incompatible variable type [9]: input is declared to have type `typing.Optional[int]` " ^
      "but is used as type `unknown`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ]


let test_check_toplevel _ =
  assert_type_errors
    "int_to_int(1.0)"
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`."];

  assert_type_errors
    {|
      a: int = None
      def foobar() -> None:
          b: int = None
    |}
    []


let test_check_tuple _ =
  assert_type_errors
    {|
      def foo(a: typing.Tuple[int, int]) -> None:
        a.tuple_method(1.0)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `tuple.tuple_method` but got `float`."];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, 2, 3)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, str]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return (1, "string", 3)
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[int, str, int]`.";
    ];
  assert_type_errors
    {|
      def foo()-> typing.Tuple[int, ...]:
        return tuple([1,2,3])
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return tuple([""])
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[int, ...]` but got " ^
      "`typing.Tuple[str, ...]`.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return tuple([1])
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1, 2)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2.0)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[float, ...]:
        return (1.0, 2)
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Tuple[int, int, str]) -> typing.Tuple[str, int]:
        a, *b = x
        return b
    |}
    [
      "Incompatible return type [7]: Expected `typing.Tuple[str, int]` but got " ^
      "`typing.List[typing.Union[int, str]]`.";
    ];

  assert_type_errors
    {|
      def derp() -> int:
        a, b = [1,2,3]
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo() -> int:
        (x, y), z = 0
        return x + y + z
    |}
    [
      "Unable to unpack [23]: Unable to unpack `unknown` into 2 values.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self, coord: typing.Tuple[int, int]) -> None:
            self.xxx, self.yyy = coord
    |}
    [
      "Missing attribute annotation [4]: Attribute `xxx` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Missing attribute annotation [4]: Attribute `yyy` of class `Foo` " ^
      "has type `int` but no type is specified.";
    ];

  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1,)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Sized:
        return (1, "")
    |}
    [];

  assert_type_errors
    {|
      def foo() -> typing.Tuple:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[()]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, str]:
        return ()
    |}
    ["Incompatible return type [7]: Expected `typing.Tuple[int, str]` but got `typing.Tuple[]`."];
  assert_type_errors
    {|
      def foo() -> typing.Tuple[int, ...]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Iterable[int]:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def bar(z: typing.Optional[int]) -> typing.Tuple[int, typing.Optional[int]]:
          return 1, int_to_int(z) if z is not None else None
    |}
    [];

  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def b(d: T) -> None:
        a = d.a + d.d
    |}
    [
      "Undefined attribute [16]: `typing.Any` has no attribute `__add__`.";
      "Undefined attribute [16]: `T` has no attribute `d`.";
    ];

  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a b c')
      def foo(t: T) -> None:
        x, y = t
        x, y, z = t
        x, y, z, other = t
    |}
    [];
  assert_type_errors
    {|
      T = collections.namedtuple('T', 'a')
      T(a=1)
      def foo() -> None:
        T(a=2)
    |}
    []


let test_check_redundant_cast _ =
  assert_type_errors
    {|
      def foo(x: int) -> None:
        typing.cast(int, x)
    |}
    ["Redundant cast [22]: The value being cast is already of type `int`."];
  assert_type_errors
    {|
      def foo(x: str) -> None:
        typing.cast(int, x)
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: typing.Any) -> None:
        typing.cast(int, x)
    |}
    [];
  assert_type_errors
    {|
      def foo(x: dict[int, int]) -> None:
        typing.cast(dict[int, int], x)
    |}
    ["Redundant cast [22]: The value being cast is already of type `typing.Dict[int, int]`."];
  assert_type_errors
    {|
      def foo(x: dict[int, int]) -> None:
        typing.cast(dict[int, str], x)
    |}
    []


let test_check_excepts _ =
  assert_type_errors
    {|
      class Exception: pass
      def takes_exception(e: Exception) -> None: pass
      def foo() -> None:
        try:
          pass
        except Exception as e:
          takes_exception(e)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Optional[int]:
        try:
          x = 1
        except:
          return None
        else:
          return x
    |}
    [];
  assert_type_errors
    {|
      def use(i: int) -> None: pass
      def foo(x: bool) -> None:
        try:
          pass
        finally:
          if x:
            use("error")
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call `use` " ^
      "but got `str`."
    ]


let test_check_async _ =
  assert_type_errors
    {|
      async def foo() -> int: return 1
      def bar() -> None:
        await foo()
    |}
    [];
  assert_type_errors
    {|
      def bar(a: typing.Awaitable[int]) -> int:
        return await a
    |}
    [];
  assert_type_errors
    {|
      def bar(a: IsAwaitable) -> int:
        await a
        return 0
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar("T")
      class C(typing.Awaitable[T]): ...

      def foo(c: C) -> int:
        return (await c)
    |}
    [
      "Missing type parameters [24]: Generic type `C` expects 1 type parameter.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      def bar(a: IsAwaitable) -> int:
        return (await a)
    |}
    [];

  assert_type_errors
    {|
      def bar(a: int) -> None:
        await a
    |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `int`."];
  assert_type_errors
    ~debug:false
    {|
      def bar(a: typing.Any) -> None:
        await a
    |}
    [];

  assert_type_errors
    {|
      async def read(file: typing.AsyncIterable[str]) -> typing.List[str]:
        return [data async for data in file]
    |}
    []


let test_check_behavioral_subtyping _ =
  (* Strengthened postcondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo() -> float: return 1.0
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Returned type `float` is not a subtype of the overridden return `int`."
    ];

  assert_type_errors
    {|
      class Foo():
        def foo() -> float: ...
      class Bar(Foo):
        def foo() -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo() -> None: pass
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Returned type `None` is not a subtype of the overridden return `int`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def foo() -> _T: ...
      class Bar(Foo[float]):
        def foo() -> str: return ""
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Returned type `str` is not a subtype of the overridden return `float`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def foo() -> _T: ...
      class Bar(Foo[float]):
        def foo() -> int: return 1
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def foo() -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo() -> str: return ""
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Returned type `str` is not a subtype of the overridden return `float`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def foo() -> _T: ...
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def foo() -> int: return 1
    |}
    [];

  (* Missing annotations. *)
  assert_type_errors
    ~strict:false
    ~debug:false
    {|
      class Foo():
        def foo() -> int: ...
      class Bar(Foo):
        def foo(): pass
    |}
    [
      "Inconsistent override [15]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "The overriding method is not annotated but should return a subtype of `int`.";
    ];

  (* Starred arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, *args: int) -> None: ...
      class D(C):
        def f(self, *args: int) -> None: ...
    |}
    [];

  (* Keyword arguments. *)
  assert_type_errors
    {|
      class C:
        def f(self, **kwargs: str) -> None: ...
      class D(C):
        def f(self, **kwargs: str) -> None: ...
    |}
    [];

  (* TODO(T29679691): We should also warn when parameter annotations are missing. *)
  assert_type_errors
    ~strict:false
    {|
      class Foo():
        def foo(input: int) -> int: ...
      class Bar(Foo):
        def foo(input) -> int: ...
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar("T", bound=int)
      class Foo():
        def foo(self, x: T) -> str:
          return ""
      class Bar(Foo[str]):
        def foo(self, x: str) -> str:
          return x
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `str` is not a supertype of the overridden parameter " ^
      "`Variable[T (bound to int)]`.";
    ];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class Foo(typing.Generic[T]):
        def foo(self) -> T:
          return ""
      class Bar(Foo[int]):
        def foo(self) -> int:
          return 1
      class BarTwo(Foo[None]):
        def foo(self) -> None:
          pass
    |}
    [];

  assert_type_errors ~show_error_traces:true
    {|
      class Foo():
        def bar(self, x: int) -> int:
          return 1
      class Bar(Foo):
        def bar(self, x: int) -> typing.Union[str, int]:
          return 1
    |}
    [
      "Inconsistent override [15]: `Bar.bar` overrides method defined in `Foo` " ^
      "inconsistently. Returned type `typing.Union[int, str]` is not a subtype " ^
      "of the overridden return `int`."
    ];

  (* Decorators are applied. *)
  assert_type_errors
    {|
      class Foo():
        @contextlib.contextmanager
        def foo() -> typing.Generator[int, None, None]: ...
      class Bar():
        @contextlib.contextmanager
        def foo() -> typing.Generator[int, None, None]: ...
    |}
    [];

  (* Weakened precondition. *)
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: float) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `int` is not a supertype of the overridden parameter `float`."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a) -> None: ...
      class Bar(Foo):
        def foo(self, ) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, ) -> None: ...
      class Bar(Foo):
        def foo(self, a) -> None: pass
    |}
    ["Missing parameter annotation [2]: Parameter `a` has no type specified."];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a) -> None: ...
      class Bar(Foo):
        def foo(self, a: int) -> None: pass
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: pass
      class Bar(Foo):
        def foo(self, b: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.foo` overrides method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature."
    ];
  assert_type_errors
    {|
      class Foo():
        def foo(self, a: int) -> None: pass
      class Bar(Foo):
        def foo(self, _a: int) -> None: pass
    |}
    [];
  assert_type_errors ~show_error_traces:true
    {|
      class Foo():
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
      class Bar(Foo):
        def bar(self, x: int) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` " ^
      "inconsistently. Parameter of type `int` is not a " ^
      "supertype of the overridden parameter `typing.Union[int, str]`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `typing.Union[int, str]` is not a supertype " ^
      "of the overridden parameter `typing.Union[float, str]`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Bar(Foo[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[float]):
        def bar(self, x: typing.Union[str, int]) -> None:
          pass
    |}
    [
      "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `typing.Union[int, str]` is not a supertype " ^
      "of the overridden parameter `typing.Union[float, str]`."
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Foo(Generic[_T]):
        def bar(self, x: typing.Union[str, _T]) -> None:
          pass
      class Passthrough(Foo[_T]): ...
      class Bar(Passthrough[int]):
        def bar(self, x: typing.Union[str, float]) -> None:
          pass
    |}
    [];

  (* A leading underscore indicates parameters are unused; they should still be recognized *)
  assert_type_errors
    {|
      class Foo:
          def bar(self, _x: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, _x: int) -> str:
              return ""
      class Baz(Foo):
          def bar(self, _x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, x: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, _x: int) -> str:
              return ""
    |}
    [];
  assert_type_errors
    {|
      class Foo:
          def bar(self, _y: int) -> str:
              return ""
      class Bar(Foo):
          def bar(self, x: int) -> str:
              return ""
    |}
    [
      "Inconsistent override [14]: `Bar.bar` overrides method defined in `Foo` " ^
      "inconsistently. Could not find parameter `y` in overriding signature."
    ];

  (* Don't warn on constructors or class methods. *)
  assert_type_errors
    {|
      class Foo():
        def __init__(self, a: float) -> None: ...
      class Bar(Foo):
        def __init__(self, a: int) -> None: pass
    |}
    [];
  assert_type_errors
    {|
      class Foo():
        @classmethod
        def foo(cls, a: float) -> None: ...
      class Bar(Foo):
        @classmethod
        def foo(cls, a: int) -> None: pass
    |}
    [];

  (* Don't warn on dunder methods. *)
  assert_type_errors
    {|
      class Foo():
        def __dunder__(self, a: float) -> None: ...
      class Bar(Foo):
        def __dunder__(self, a: int) -> None: pass
    |}
    [];

  (* Dunder methods must end with dunder. *)
  assert_type_errors
    {|
      class Foo():
        def __f(self, a: float) -> None: ...
      class Bar(Foo):
        def __f(self, a: int) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.__f` overrides method defined in `Foo` inconsistently. " ^
      "Parameter of type `int` is not a supertype of the overridden parameter `float`.";
    ];

  (* Weakening of object precondition is not possible. *)
  assert_type_errors
    {|
      class Foo():
        def __eq__(self, o: object) -> bool: ...
      class Bar(Foo):
        def __eq__(self, other) -> bool: ...
    |}
    [];

  (* Ensure that our preprocessing doesn't clobber starred argument names. *)
  assert_type_errors
    {|
      class Foo():
        def foo( **kwargs) -> int: ...
      class Bar(Foo):
        def foo( **kwargs) -> int: ...
    |}
    [];

  (* Ignore anything involving `Any`. *)
  assert_type_errors
    ~debug:false
    {|
      class Foo():
        def __eq__(self, o: typing.Any) -> typing.Any: ...
      class Bar(Foo):
        def __eq__(self, o: int) -> int: pass
    |}
    [];

  (* Overrides when both *args and **kwargs exist are not inconsistent. *)
  assert_type_errors
    ~debug:false
    {|
      class Foo():
        def f(self, a: float) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.f` overrides method defined in `Foo` inconsistently. " ^
      "Could not find parameter `a` in overriding signature.";
    ];
  assert_type_errors
    ~debug:false
    {|
      class Foo():
        def f(self, b: int) -> None: ...
      class Bar(Foo):
        def f(self, **kwargs: typing.Any) -> None: pass
    |}
    [
      "Inconsistent override [14]: `Bar.f` overrides method defined in `Foo` inconsistently. " ^
      "Could not find parameter `b` in overriding signature.";
    ];
  assert_type_errors
    ~debug:false
    {|
      class Foo():
        def f(self, c: str) -> None: ...
      class Bar(Foo):
        def f(self, *args: typing.Any, **kwargs: typing.Any) -> None: pass
    |}
    []


let test_check_unbound_variables _ =
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = 1
        else:
          other = 1
        return result
    |}
    [
      "Incompatible return type [7]: Expected `int` but got " ^
      "`typing.Union[int, typing.Undeclared]`.";
      "Undefined name [18]: Global name `result` is undefined.";
    ];
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = narnia()
        return result
    |}
    [
      "Undefined name [18]: Global name `narnia` is undefined.";
      "Incompatible return type [7]: Expected `int` but got " ^
      "`typing.Union[typing.Undeclared, unknown]`.";
      "Undefined name [18]: Global name `result` is undefined.";
    ];
  assert_type_errors
    {|
      def foo(flag: bool) -> int:
        if flag:
          result = narnia()
        else:
          other = 1
        return result
    |}
    [
      "Undefined name [18]: Global name `narnia` is undefined.";
      "Incompatible return type [7]: Expected `int` but got " ^
      "`typing.Union[typing.Undeclared, unknown]`.";
      "Undefined name [18]: Global name `result` is undefined.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        assert unknown is None or 1
        return unknown
    |}
    [
      "Undefined name [18]: Global name `unknown` is undefined.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: bool = False
        def foo(self) -> int:
          if not self.attribute:
            self.attribute = True
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `bool`."]


let test_check_contextmanager _ =
  assert_type_errors
    {|
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->int:
        with f() as number:
          return number
    |}
    [];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->str:
        with f() as number:
          return number
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f() -> typing.Iterable[int]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [
      (* TODO(T27138096): Iterable should have attribute `__enter__`. *)
      "Undefined attribute [16]: `typing.Iterable[typing.Any]` has no attribute `__enter__`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f() -> typing.Generator[int, None, None]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [];

  assert_type_errors
    {|
      class C:
        @contextlib.contextmanager
        def f(self) -> typing.Iterator[int]:
          yield 1
      def foo(c: C) -> str:
        with c.f() as manager:
          return manager
        return ""
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]


let test_environment _ =
  (* Type aliases in signatures are resolved. *)
  assert_type_errors
    "hashlib.md5(1.0)"
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Union[int, str]` for 1st anonymous parameter to call `hashlib.md5` " ^
     "but got `float`."];

  (* Type aliases in the class hierarchy are resolved. I.e. we follow the conditional `Collection`
     indirection in typeshed. *)
  assert_type_errors
    {|
      def foo(input: typing.Sequence[int]) -> typing.Iterable[int]:
        return input
    |}
    []


let test_scheduling _ =
  (* Top-level is scheduled. *)
  assert_type_errors
    "'string' + 1"
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  (* Functions are scheduled. *)
  assert_type_errors
    {|
      def bar() -> None: ...
      def foo() -> None:
        'string' + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  assert_type_errors
    {|
      def bar() -> None:
        def foo() -> None:
          'string' + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  (* Class bodies are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        'string' + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  (* Methods are scheduled. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> None:
          'string' + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  (* Entry states are propagated. *)
  assert_type_errors
    {|
      variable = 1
      def foo() -> int:
        return variable
      def bar() -> str:
        return variable

      variable = 'asdf'
      def bar() -> str:
        return variable
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Missing global annotation [5]: Globally accessible variable `variable` has type " ^
      "`typing.Union[int, str]` but no type is specified.";
    ];

  (* Functions defined after try/except blocks are typechecked. *)
  assert_type_errors
    {|
      class Exception: pass
      try:
        pass
      except Exception:
        pass

      def expect_string(a: str) -> None:
        pass
      def foo() -> None:
        expect_string(1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expect_string` but got `int`."];
  assert_type_errors
    {|
      try:
        pass
      finally:
        pass

      def expect_string(a: str) -> None:
        pass
      def foo() -> None:
        expect_string(1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expect_string` but got `int`."]


let () =
  "type">:::[
    "initial">::test_initial;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "widen">::test_widen;
    "check_annotation">::test_check_annotation;
    "forward_expression">::test_forward_expression;
    "forward_statement">::test_forward_statement;
    "forward">::test_forward;
    "reveal_type">::test_reveal_type;
    "check_error_traces">::test_show_error_traces;
    "coverage">::test_coverage;
    "check">::test_check;
    "check_assign">::test_check_assign;
    "check_coverage">::test_check_coverage;
    "check_comprehensions">::test_check_comprehensions;
    "check_optional">::test_check_optional;
    "check_imports">::test_check_imports;
    "check_yield">::test_check_generators;
    "check_ternary">::test_check_ternary;
    "check_nested">::test_check_nested;
    "check_refinement">::test_check_refinement;
    "check_toplevel">::test_check_toplevel;
    "check_tuple">::test_check_tuple;
    "check_redundant_cast">::test_check_redundant_cast;
    "check_excepts">::test_check_excepts;
    "check_async">::test_check_async;
    "check_behavioral_subtyping">::test_check_behavioral_subtyping;
    "check_unbound_variables">::test_check_unbound_variables;
    "check_contextmanager">::test_check_contextmanager;
    "environment">::test_environment;
    "scheduling">::test_scheduling;
  ]
  |> Test.run
