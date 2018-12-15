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
  let check ~configuration ~environment ~source =
    let { TypeCheck.Result.errors; _ } =
      TypeCheck.check
        ~configuration
        ~environment
        ~source
    in
    errors
  in
  assert_errors ~check


let resolution = Test.resolution ()


let create
    ?(bottom = false)
    ?(define = Test.mock_define)
    ?(expected_return = Type.Top)
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
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = Some (+Float 1.0);
        annotation = Some (Type.expression Type.integer);
      }
    ]
    (create ~immutables:["x", false] ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = Some (+Float 1.0);
        annotation = None;
      }
    ]
    (create ["x", Type.float]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.integer);
      }
    ]
    ~return_annotation:!"int"
    (create ~immutables:["x", false] ~expected_return:Type.integer ["x", Type.integer]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = Some (Type.expression Type.float);
      };
      {
        Parameter.name = ~~"y";
        value = None;
        annotation = Some (Type.expression Type.string)
      };
    ]
    (create ~immutables:["x", false; "y", false] ["x", Type.float; "y", Type.string]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"x";
        value = None;
        annotation = None;
      };
    ]
    (create ["x", Type.Bottom]);

  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"self";
        value = None;
        annotation = None;
      };
    ]
    ~parent:"Foo"
    (create ["self", Type.Primitive ~~"Foo"]);
  assert_initial
    ~parameters:[
      {
        Parameter.name = ~~"a";
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
    (Type.dictionary ~key:Type.Object ~value:Type.Object);
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
  assert_forward [] "x = ()" ["x", Type.Tuple (Type.Unbounded Type.Object)];

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


let test_check_with_qualification _ =
  assert_type_errors
    {|
      x: int = 1
      def foo(x: str) -> str:
        return x
    |}
    [];

  assert_type_errors
    {|
      x: int = 1
      def foo(y: str) -> str:
        return x
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      l: typing.List[int] = [1]
      def hello() -> int:
        for i in l:
          return i
        return -1
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123

      def duh(global_number: str) -> int:
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def nonglobal_inner_access() -> int:
              return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 123
      def wut(global_number: str) -> None:
          def wut_inner_global() -> int:
              global global_number
              return global_number

    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      global_number: int = 123
      def rly() -> int:
          def rly_inner(global_number: str) -> None:
              pass
          return global_number
      def len(s: str) -> int:
          return 1
      def assign() -> int:
          global_number="a" # type: str
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def len(s: str) -> int:
        return 1
      def assign_outer() -> None:
          global_number="a" # type: str
          def assign_inner_access() -> int:
              return len(global_number)
          def assign_inner_global() -> int:
              global global_number
              return global_number
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      global_number: int = 1
      def derp() -> int:
          def derp_inner() -> None:
              global_number="a" # type: str
              pass
          return global_number
    |}
    [];

  assert_type_errors
    {|
      def access_side_effect(global_number: str) -> int:
          side_effect=global_number
          return len(global_number)
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def access_side_effect_2() -> int:
          side_effect=global_number
          return global_number
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def pure_sideffect() -> None:
          side_effect=global_number
          def pure_side_effect_inner() -> int:
              return global_number
    |}
    [];


  assert_type_errors
    {|
      global_number: int = 1
      def access_transitive() -> int:
          transitive=global_number
          return transitive
    |}
    [];

  assert_type_errors
    {|
      global_number: int = 1
      def assign_transitive() -> None:
          another=global_number
          # TODO(T27001301): uncomment next two lines when nested scopes will work
          #def out_of_ideas_3() -> int:
          #    return another
      def assign_transitive_2() -> int:
          transitive=global_number
          def assign_transitive_inner() -> None:
              global_number="a"
          return transitive
    |}
    []


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
    let { Result.coverage; _ } =
      let environment = Test.environment () in
      Analysis.TypeCheck.check
        ~configuration:Test.mock_configuration
        ~environment
        ~source:(parse source)
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
    "def foo() -> None: pass"
    [];

  assert_type_errors
    "def foo() -> None: return"
    [];

  assert_type_errors
    "def foo() -> float: return 1.0"
    [];

  assert_type_errors
    "def foo() -> float: return 1"
    [];

  assert_type_errors
    "def foo() -> int: return 1.0"
    ["Incompatible return type [7]: Expected `int` but got `float`."];

  assert_type_errors
    "def foo() -> str: return 1.0"
    ["Incompatible return type [7]: Expected `str` but got `float`."];

  assert_type_errors
    "def foo() -> str: return"
    ["Incompatible return type [7]: Expected `str` but got `None`."];

  assert_type_errors
    "def foo() -> typing.List[str]: return 1"
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `int`."];

  assert_type_errors
    ~debug:false
    "def foo() -> int: return"
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors
    "def foo() -> typing.List[str]: return []"
    [];

  assert_type_errors
    "def foo() -> typing.Dict[str, int]: return {}"
    [];

  assert_type_errors
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any, typing.Any]: return f()
    |}
    [];

  assert_type_errors
    {|
      def f() -> dict: return {}
      def foo() -> typing.Dict[typing.Any]: return f()
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[typing.Any]` but got " ^
      "`typing.Dict[typing.Any, typing.Any]`."
    ];

  assert_type_errors
    {|
      x: typing.Type
      def foo() -> typing.Type[typing.Any]:
        return x
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> list:
        return x
    |}
    [];

  assert_type_errors
    ~debug:false
    {|
      def f(d: typing.Dict[int, int], x) -> None:
        d.update({ 1: x })
    |}
    [];

  assert_type_errors
    ~debug:false
    {|
      def f(d: typing.Dict[int, str], x) -> str:
        return d.get(x, "")
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        a = {"key": set()}
        b = a.get("key", set())
    |}
    [];

  assert_type_errors
    {|
      def f(l: typing.List[int]) -> int:
        [a, b] = l
        return a + b
    |}
    [];

  assert_type_errors
    {|
      def foo() -> int:
        for x in [1,2,3]:
          if x > 0:
            return x
          x = 15
        return 0
    |}
    [];

  assert_type_errors
    {|
      x: str
      def foo() -> str:
        return x.__getitem__(0)
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> int:
        return x.__getitem__(0)
    |}
    [];

  assert_type_errors
    {|
      x: typing.List[int]
      def foo() -> typing.List[int]:
        return x.__getitem__(slice(0, 1, None))
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
      x: typing.Generator[int, int, int]
      def foo() -> typing.Generator:
        return x
    |}
    [];

  assert_type_errors
    {|
      def foo(a:typing.Optional[int])->str:
        return int_to_str(a) if a else ""
    |}
    [];

  assert_type_errors
    "def foo() -> str: return 1.0\ndef bar() -> int: return ''"
    [
      "Incompatible return type [7]: Expected `str` but got `float`.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
    ];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return A()"
    [];

  assert_type_errors
    "class A: pass\ndef foo() -> A: return 1"
    ["Incompatible return type [7]: Expected `A` but got `int`."];

  assert_type_errors
    "def bar() -> str: return ''\ndef foo() -> str: return bar()"
    [];

  assert_type_errors
    "def foo() -> str: return not_annotated()"
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];

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
      @abstractmethod
      def abstract()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      @abc.abstractproperty
      def abstract()->int:
        pass
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
      def x()->int:
        return None
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `None`."
    ];

  assert_type_errors
    {|
      def x() -> int:
        if unknown_condition:
          return 1
    |}
    [
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
      "Undefined name [18]: Global name `unknown_condition` is undefined.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        if unknown_condition:
          return 1
        else:
          x = 1
    |}
    [
      "Incompatible return type [7]: Expected `int` but got implicit return value of `None`.";
      "Undefined name [18]: Global name `unknown_condition` is undefined.";
    ];

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
      def derp()->typing.Set[int]:
        return {1}
    |}
    [];

  assert_type_errors
    {|
      def derp()->typing.Set[int]:
        return {""}
    |}
    [
      "Incompatible return type [7]: Expected `typing.Set[int]` but got `typing.Set[str]`."
    ];

  assert_type_errors
    {|
      def foo() -> str:
        if condition():
          return 1
        else:
          return 2
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ];

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
      def foo(t: typing.Iterable[Tuple[str, int]]) -> dict[str, int]:
        d = dict(t)
        bar = d['a']
        reveal_type(bar)
        return d
    |}
    ["Revealed type [-1]: Revealed type for `bar` is `int`."];

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
    [];
  (* We require that all elements in a union have the same method for `in`. *)
  assert_type_errors
    {|
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItem:
        def __getitem__(self, x: int) -> Equal:
          ...
      class Contains:
        def __contains__(self, a: object) -> bool:
          ...
      def foo(a: typing.Union[GetItem, Contains]) -> None:
        5 in a
    |}
    ["Undefined attribute [16]: `Contains` has no attribute `__getitem__`."];
  (* Don't crash when filtering on returning a bad type *)
  assert_type_errors
    ~debug:false
    {|
      def foo(a: gurbage) -> None:
        return a
    |}
    [
      "Undefined type [11]: Type `gurbage` is not defined.";
      "Incompatible return type [7]: Expected `None` but got `gurbage`.";
    ]


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
      def foo() -> None:
        x = 1
        x += 'asdf'
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."];

  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x["foo"] = "bar"
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`."];

  assert_type_errors
    {|
      class A:
        pass
      def foo(x: typing.Dict[str, int], y: A) -> None:
        x["foo"] = y["bar"] = "baz"
    |}
    [
      "Undefined attribute [16]: `A` has no attribute `__setitem__`.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Dict[str, typing.Dict[str, int]]) -> None:
        x["foo"]["bar"] = "baz"
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `dict.__setitem__` but got `str`."];

  assert_type_errors
    {|
      def foo(x: typing.Dict[str, int]) -> None:
        x[7] = 7
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `dict.__setitem__` but got `int`."]


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
    [];

  assert_type_errors
    {|
      def foo() -> typing.Any:
        if condition():
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but " ^
      "type `Any` is specified."
    ];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional or int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[bool, int]` " ^
      "but type `Any` is specified.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_bool` but got " ^
      "`typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional and int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[bool]` but " ^
      "type `Any` is specified.";
    ]


let test_check_function_overloads _ =
  assert_type_errors
    {|
      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        @overload
        def derp(self, x: str) -> str:
          pass
        def derp(self, x: typing.Union[int, str]) -> typing.Union[int, str]:
          if isinstance(x, int):
            return 0
          else:
            return ""

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];

  (* Technically invalid; all @overload stubs must be followed by implementation *)
  assert_type_errors
    {|
      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        @overload
        def derp(self, x: str) -> str:
          pass

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];

  (* Technically invalid; @overload stubs must comprehensively cover implementation *)
  assert_type_errors
    {|
      class Foo:
        @overload
        def derp(self, x: int) -> int:
          pass
        def derp(self, x: typing.Union[int, str]) -> typing.Union[int, str]:
          if isinstance(x, int):
            return 0
          else:
            return ""

      def herp(x: Foo) -> int:
        return x.derp(5)
    |}
    [];

  assert_type_errors
    {|
      @overload
      def derp(x: int) -> int: ...
      @overload
      def derp(x: str) -> str: ...
      def derp(x: int) -> int: ...
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Revealed type [-1]: Revealed type for `derp` is " ^
      "`typing.Callable(derp)[[Named(x, str)], str][[[Named(x, int)], int][[Named(x, str)], str]]`."
    ];

  assert_type_errors
    {|
      @overload
      def derp(x: int) -> int: ...
      @overload
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Revealed type [-1]: Revealed type for `derp` is " ^
      "`typing.Callable(derp)[..., unknown][[[Named(x, int)], int][[Named(x, str)], str]]`."
    ];

  (* The overloaded stub will override the implementation *)
  assert_type_errors
    {|
      @overload
      def derp(x: int) -> int: ...
      def derp(x: str) -> str: ...
      @overload
      def derp(x: str) -> str: ...

      reveal_type(derp)
    |}
    [
      "Revealed type [-1]: Revealed type for `derp` is " ^
      "`typing.Callable(derp)[..., unknown][[[Named(x, int)], int][[Named(x, str)], str]]`."
    ];

  assert_type_errors
    {|
      @overload
      def derp(x: int) -> int: ...
      def derp(x: str) -> str: ...
      def derp(): ...

      reveal_type(derp)
    |}
    [
      "Revealed type [-1]: Revealed type for `derp` is " ^
      "`typing.Callable(derp)[[], unknown][[[Named(x, int)], int]]`."
    ]


let test_check_function_parameters _ =
  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1)
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
    ];
  assert_type_errors
    {|
      def preprocessed($renamed_i: str) -> None:
        pass
      def foo() -> None:
        preprocessed(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 1st anonymous parameter to call `preprocessed` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        return int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo(i) -> None:
        int_to_int(i)
    |}
    ["Missing parameter annotation [2]: Parameter `i` has no type specified."];

  assert_type_errors
    {|
      def foo(i: int, *, j: int) -> None:
        pass
    |}
    ["Missing parameter annotation [2]: Parameter `*` has no type specified."];

  assert_type_errors
    {|
      def foo( *args, **kwargs) -> None:
        pass
    |}
    [
      "Missing parameter annotation [2]: Parameter `*args` has no type specified.";
      "Missing parameter annotation [2]: Parameter `**kwargs` has no type specified.";
    ];

  assert_type_errors
    {|
      class A:
        def foo(self) -> None:
          int_to_int(self.attribute)
    |}
    ["Undefined attribute [16]: `A` has no attribute `attribute`."];
  assert_type_errors
    {|
      class C:
       attribute: int = 1
      try:
        x = C()
      except:
        pass
      x.attribute
    |}
    ["Undefined name [18]: Global name `x` is undefined."];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, None]) -> None: pass
      foo(None)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, None, typing.Tuple[int, str]]) -> None:
        pass
      foo(None)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a and int_to_str(a))
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a or int_to_str(a))
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_str` but got " ^
      "`typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(a: int) -> int:
        return a
      x: typing.Optional[int]
      foo(x if x else 1)
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x is not None else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.int_attribute
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ];

  assert_type_errors
    {|
      def foo(x: typing.Union[OtherAttributes, Attributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Attributes` has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      def foo(x) -> None:
        takes_iterable(x)
    |}
    ["Missing parameter annotation [2]: Parameter `x` has no type specified."];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Optional[int]` for 1st anonymous parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      def foo(a):
        # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Optional[int]` for 1st anonymous parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, "hello")
    |}
    [];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, 1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 2nd anonymous parameter to call `foo` but got `int`."]


let test_check_function_parameters_with_backups _ =
  assert_type_errors "(1).__add__(1)" [];
  assert_type_errors "(1).__add__(1j)" [];
  assert_type_errors "(1).__add__(1.0)" []


let test_check_function_parameter_errors _ =
  assert_type_errors
    {|
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.attribute, input.undefined)
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `undefined`."];
  assert_type_errors
    {|
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.undefined, input.undefined)
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.attribute)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 1st anonymous parameter to call `optional_str_to_int` " ^
      "but got `typing.Optional[int]`.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.undefined)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 1st anonymous parameter to call `optional_str_to_int` " ^
      "but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];
  assert_type_errors
    {|
      class attribute:
        ...
      class other:
        attribute: int = 1
      def foo(o: other) -> str:
        return o.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]


let test_check_function_redirects _ =
  assert_type_errors
    {|
      def foo(a: float) -> float:
        return abs(a)
    |}
    []


let test_check_variable_arguments _ =
  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b) -> str:
        return foo ( *b )
    |}
    [
      "Missing parameter annotation [2]: Parameter `b` has no type specified.";
      "Incompatible return type [7]: Expected `str` but got `int`.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `foo` but got `unknown`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.Any) -> int:
        return foo ( *b )
    |}
    [
      "Missing parameter annotation [2]: Parameter `b` must have a type other than `Any`.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `foo` but got `unknown`.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo ( *b )
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `foo` but got `str`.";];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo('asdf', *b)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `foo` but got `str`.";];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 'asdf' )
    |}
    [
      "Too many arguments [19]: Call `foo` expects 2 arguments, 3 were provided.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[str]) -> None:
        foo ( *b, 1, 'asdf' )
    |}
    ["Too many arguments [19]: Call `foo` expects 2 arguments, 4 were provided."];

  assert_type_errors
    {|
      def foo(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        foo ( *b, 'asdf' )
    |}
    ["Too many arguments [19]: Call `foo` expects 2 arguments, 3 were provided."];

  assert_type_errors
    {|
      def durp(a: int, b: str) -> int:
        return 1
      def bar(b: typing.List[int]) -> None:
        durp( *b, 1.0 )
    |}
    [
      "Too many arguments [19]: Call `durp` expects 2 arguments, 3 were provided.";
    ];

  assert_type_errors
    {|
      def foo(a: int, b: int) -> int:
        return 1
      def bar(b: typing.List[str]) -> int:
        return foo('asdf', *b)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 2nd anonymous parameter to call `foo` but got `str`.";]


let test_check_method_returns _ =
  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> int:
          return input.lower().upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      def foo() -> int:
          return ''.upper()
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."]


let test_check_method_parameters _ =
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr(1)
    |}
    [];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(a: str, b: str) -> None:
        pass
      def bar() -> None:
        foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 2nd anonymous parameter to call `foo` but got `int`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.substr('asdf').substr('asdf')
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `str.substr` but got `str`.";
    ];

  assert_type_errors
    {|
      def foo(input: str) -> None:
        input + 1
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got `str`."];

  assert_type_errors
    {|
      def foo(input: str) -> str:
        return input.__sizeof__()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Foo:
        def bar(self) -> None:
          def baz(x: int) -> int:
            return x
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def bar(x: int) -> int:
          return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as type `Foo`.";
      "Incompatible return type [7]: Expected `int` but got `Foo`.";
    ]


let test_check_method_resolution _ =
  assert_type_errors
    {|
      def foo() -> None:
        bar().baz()
    |}
    ["Undefined name [18]: Global name `bar` is undefined."];
  assert_type_errors
    {|
      def foo(input: str) -> None:
        input.lower()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def derp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def herp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Bar` has no attribute `derp`.";
    ]


let test_check_self _ =
  (* Self parameter is typed. *)
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return 1
        def bar(self) -> str:
          return self.foo()
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Other:
          pass

      class Some:
          def one(self) -> None:
              self.two()

          def two(self: Other) -> None:
              pass
    |}
    [
      "Incompatible variable type [9]: self is declared to have type `Other` but is used as type \
       `Some`.";
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T, x: int) -> T:
          return self
      class Subclass(C):
        pass
      def f() -> C:
        a = Subclass()
        b = a.f
        return b(1)
    |}
    []


let test_check_static _ =
  (* No self parameter in static method. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def bar(input: str) -> str:
          return input.lower()

      class Bar:
        @classmethod
        def bar(cls, input: str) -> str:
          return input.lower()

        def baz(self) -> None:
          self.bar("")
    |}
    [];

  (* Static method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def foo(input: int) -> int:
          return input

        def bar(self) -> None:
          self.foo('asdf')

    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.foo` but got `str`."];

  (* Class method calls are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls, input: int) -> int:
          return input

      def foo() -> None:
        Foo.foo('asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Foo.foo` but got `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        @classmethod
        def foo(cls) -> typing.Type[Foo]:
          return cls
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.classmethod('1234')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.classmethod` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        @staticmethod
        def staticmethod(i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.staticmethod('1234')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.staticmethod` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        def instancemethod(self, i: int) -> None:
          pass
        @classmethod
        def classmethod(cls, i: int) -> None:
          cls.instancemethod(Foo(), '1234')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.instancemethod` but got `str`.";
    ];

  (* Special classmethods are treated properly without a decorator. *)
  assert_type_errors
    {|
      class Foo:
        def __init_subclass__(cls) -> typing.Type[Foo]:
          return cls
        def __new__(cls) -> typing.Type[Foo]:
          return cls
        def __class_getitem__(cls, key: int) -> typing.Type[Foo]:
          return cls
    |}
    []


let test_check_init _ =
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: int
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(renamed_self) -> None:
          renamed_self.attribute = 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def __init__(renamed_self) -> None:
          renamed_self.attribute = 0
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but \
       no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: bool = False
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __init__(self) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        attribute_two: str
        def __init__(self) -> None:
          pass
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
      "Uninitialized attribute [13]: Attribute `attribute_two` is declared in class `Foo` to " ^
      "have non-optional type `str` but is never initialized.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __enter__(self) -> None:
          self.attribute = 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = 0 if True else 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if condition():
            self.attribute = 0
          else:
            self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if False:
            return None
          self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          if condition():
            raise
          self.attribute = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = unknown if condition() else unknown2
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` " ^
      "has type `int` but is used as type `unknown`.";
      "Undefined name [18]: Global name `unknown` is undefined.";
      "Undefined name [18]: Global name `unknown2` is undefined.";
    ];

  (* No need to initialize properties. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(sefl) -> None:
          pass
        @property
        def foo() -> str:
          return "asdf"
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          attribute = 0
    |}
    [
      "Uninitialized attribute [13]: Attribute `attribute` is declared in class `Foo` to have " ^
      "non-optional type `int` but is never initialized.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 0
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: typing.Optional[int]
        def __init__(self) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        attribute: int
        def __init__(self) -> None:
          self.attribute = ""
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self, x:int) -> None:
          pass
      a = Foo("")
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `a` has type `Foo` " ^
      "but no type is specified.";
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`.";
    ];

  assert_type_errors
    {|
       alias = int
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def __new__(cls, x: int) -> None:
          pass
      a: Foo = Foo("")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.__new__` but got `str`."];

  (* Prefer init over new if both exist. *)
  assert_type_errors
    {|
      class Foo:
        def __new__(cls, x: int) -> None:
          pass
        def __init__(self, x: str) -> None:
          pass
      a: Foo = Foo("")
    |}
    [];

  assert_type_errors
    {|
      class Super:
        def __new__(cls, x: int) -> None: ...

      class C(Super):
        pass
      c: C = C("")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__new__` but got `str`."];

  (* We look at both __init__ and __new__ in the inheritance structure. *)
  assert_type_errors
    {|
      class SuperSuper:
        def __init__(self, x: str) -> None: ...
      class Super(SuperSuper):
        def __new__(cls, x: int) -> None: ...
      class C(Super):
        pass
      c: C = C("")
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Super.__new__` but got `str`."];
  assert_type_errors
    {|
      class SuperSuper:
        def __new__(self, x: str) -> None: ...
      class Super(SuperSuper):
        def __init__(cls, x: int) -> None: ...
      class C(Super):
        pass
      c: C = C("")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`."]

let test_check_attributes _ =
  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          return self.bar
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];
  assert_type_errors
    {|
      class Foo:
        bar: int = 1
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo(Bar):
        bar: typing.Optional[int] = None
        def foo(self) -> typing.Optional[int]:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo(Bar):
        def foo(self) -> int:
          return self.bar
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        bar = 1 # type: int
        def foo(self) -> int:
          return self.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.bar: int = None
        def f(self) -> str:
            return self.bar
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];


  assert_type_errors
    ~debug:true
    ~strict:true
    {|
      class Bar:
        def bar() -> None:
          pass
      class Foo:
        bar: typing.Optional[Bar] = None
        def foo(self) -> None:
          self.bar.bar()
    |}
    ["Undefined attribute [16]: Optional type has no attribute `bar`."];

  assert_type_errors
    ~strict:true
    {|
      a = str
      b = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `b` has type `int` but no " ^
      "type is specified.";
    ];

  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo() -> int:
        return Bar.bar
    |}
    [];

  assert_type_errors
    ~show_error_traces:true
    {|
      class Bar:
        bar: int = 1
      def foo() -> int:
        x = Bar()
        return x.baz
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`. Type `int` expected on " ^
      "line 6, specified on line 4.";
      "Undefined attribute [16]: `Bar` has no attribute `baz`."
    ];

  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo:
        def foo(self, other: Bar) -> int:
          return other.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but no " ^
      "type is specified.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Any
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but type " ^
      "`Any` is specified.";
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have " ^
      "non-optional type `typing.Any` but is never initialized.";
      "Incompatible return type [7]: Expected `int` but got `str`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int = 1
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
          class Foo:
            a: str = ""
          Foo.a = 1
    |}
    [
      "Incompatible attribute type [8]: Attribute `a` declared in class `Foo` has type `str` " ^
      "but is used as type `int`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      def foo(param: Foo) -> int:
        param.bar = 'foo'
        return param.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      bar: int = 1
      def foo() -> int:
        bar = 'foo'
        return bar
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      class Foo:
        def foo(self) -> int:
          self.bar = 'foo'
          return self.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` has type `str` but no " ^
      "type is specified.";
      "Incompatible return type [7]: Expected `int` but got `str`.";
      "Undefined attribute [16]: `Foo` has no attribute `bar`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = "foo"
        return foo_obj.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar: int = 1
      class Bar(Foo):
        def foo(self) -> int:
          self.bar = "foo"
          return self.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
            self.a = 3
        def foo(self) -> str:
            return self.a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified.";
      "Incompatible return type [7]: Expected `str` but got `int`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a and a.bar:
          return a.bar
        return 0
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Optional[int]
      def foo(a: typing.Optional[Foo]) -> int:
        if a.bar and a:
          return a.bar
        return 0
    |}
    [
      "Undefined attribute [16]: Optional type has no attribute `bar`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar, baz = 1, 2
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Undefined error [1]: Problem with analysis.";
    ];

  assert_type_errors
    {|
      class Foo:
        bar, baz = list(range(2))
      def foo() -> int:
        foo_obj = Foo()
        foo_obj.bar = 1
        return foo_obj.bar
    |}
    [
      "Missing attribute annotation [4]: Attribute `bar` of class `Foo` " ^
      "has type `int` but no type is specified.";
      "Undefined error [1]: Problem with analysis.";
    ];

  assert_type_errors
    {|
      class Foo:
        def foo(self, bar: typing.Optional[int]) -> int:
          self.baz = bar
          if self.baz is None:
            self.baz = 5
          return self.baz
    |}
    [
      "Missing attribute annotation [4]: Attribute `baz` of class `Foo` has type " ^
      "`typing.Optional[int]` but no type is specified.";
      (* TODO(T24330702): we should only report this once. *)
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
      "Undefined attribute [16]: `Foo` has no attribute `baz`.";
    ];

  (* TODO(T25072735): support attribute tests for: class variables, generic annotations *)
  assert_type_errors
    {|
      class Foo:
        bar: typing.ClassVar[int] = 1
      def foo() -> int:
        Foo.bar = "foo"
        return Foo.bar
    |}
    [
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type `int` " ^
      "but is used as type `str`."
    ];

  assert_type_errors
    {|
      class Foo:
        bar: typing.Generic[_T]
        def foo(self) -> int:
          self.bar = 0
          return self.bar
    |}
    [
      "Uninitialized attribute [13]: Attribute `bar` is declared in class `Foo` to have " ^
      "non-optional type `typing.Generic[Variable[_T]]` but is never initialized.";
      "Incompatible attribute type [8]: Attribute `bar` declared in class `Foo` has type " ^
      "`typing.Generic[Variable[_T]]` but is used as type `int`.";
      "Incompatible return type [7]: Expected `int` but got `typing.Generic[Variable[_T]]`.";
    ];

  (* Static attributes are properly resolved. *)
  assert_type_errors
    {|
      class Foo:
        attribute: typing.ClassVar[int] = 1

      def foo() -> str:
        return Foo.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Foo:
        class Bar:
          attribute: int = 1

      def foo() -> str:
        return Foo.Bar().attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      class Foo:
        DERP: typing.ClassVar[str] = "test"

        @staticmethod
        def derp() -> str:
          return Foo.DERP
    |}
    [];

  (* Attributes defined in constructor. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
    ];
  assert_type_errors
    {|
      class unittest.TestCase: ...
      class Foo(unittest.TestCase):
        def setUp(self) -> None:
          self.attribute: int = 1
        def foo(self) -> str:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.attribute: int = 1
        def foo(self) -> int:
          return self.attribute
    |}
    [];

  (* Class implements `__getattr__`. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def __getattr__(self, attribute) -> str: ...
        def foo(self) -> int:
          return self.undefined
        def bar(self) -> int:
          return self.attribute
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  (* Attributes of other classes are properly resolved. *)
  assert_type_errors
    {|
      class Bar:
        bar: int = 1
      class Foo:
        def foo(self, bar: Bar) -> int:
          return bar.bar
      def foo(bar: Bar) -> int:
        return bar.bar
    |}
    [];

  (* Any has all attributes in default mode, but not strict mode. *)
  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    [
      "Missing parameter annotation [2]: Parameter `any` must have a type other than `Any`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(any: typing.Any) -> int:
        return any.attribute
    |}
    [];

  (* We allow instance attributes to be accessed via class objects. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      Foo.attribute
    |}
    [];

  (* Check attribute type propagation. *)
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
        def foo(self) -> None:
          self.attribute = not_annotated()
          a = self.attribute.something
    |}
    [
      "Incompatible attribute type [8]: Attribute `attribute` declared in class `Foo` has type " ^
      "`int` but is used as type `unknown`.";
    ];

  (* Check attribute type variable resolution. *)
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      def bar(wrapper: Wrapper[int]) -> int:
        return wrapper.value
    |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have " ^
      "non-optional type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
      _VALUE = typing.TypeVar('_VALUE')
      class Wrapper(typing.Generic[_VALUE]):
        value: _VALUE

      class WrapperSubclass(Wrapper[int]):
        pass

      def bar(wrapper: WrapperSubclass) -> int:
        return wrapper.value
    |}
    [
      "Uninitialized attribute [13]: Attribute `value` is declared in class `Wrapper` to have " ^
      "non-optional type `Variable[_VALUE]` but is never initialized.";
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class ReturnSelf(typing.Generic[_T]):
        def f(self) -> ReturnSelf[_T]:
          return self
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class ReturnClass(typing.Generic[_T]):
        @classmethod
        def f(cls) -> ReturnClass[_T]:
          return cls
    |}
    [
      "Incompatible return type [7]: Expected `ReturnClass[Variable[_T]]` but got \
       `typing.Type[ReturnClass[Variable[_T]]]`.";
    ];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Class:
        @property
        def property(self: _T) -> typing.Sequence[_T]: ...
      def foo(c: Class) -> typing.Sequence[Class]:
        return c.property
    |}
    [];
  assert_type_errors
    {|
      _T = typing.TypeVar('_T')
      class Class(typing.Generic[_T]):
        @property
        def property(self) -> _T: ...
      def foo(c: Class[int]) -> int:
        return c.property
    |}
    [];

  (* Do not resolve optional attributes to the optional type. *)
  assert_type_errors
    {|
      class Foo:
        debug: int = 1
      def foo(f: typing.Optional[Foo]) -> int:
        return f.debug
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Optional type has no attribute `debug`.";
    ];

  (* Attributes defined with property decorators. *)
  assert_type_errors
    {|
      class Foo:
        @property
        def prop(self) -> int: ...
      def foo() -> str:
        return Foo().prop
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  (* Attributes defined with getters and setters. *)
  assert_type_errors
    {|
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: int) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo`" ^
      " has type `int` but is used as type `str`.";
    ];

  assert_type_errors
    {|
      x: typing.Optional[int]
      class Foo:
        @property
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> None:
        foo = Foo()
        foo.x = 1
        foo.x = None
        foo.x = "string"
    |}
    [
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` but \
       is used as type `None`.";
      "Incompatible attribute type [8]: Attribute `x` declared in class `Foo` has type `int` but \
       is used as type `str`.";
    ];

  assert_type_errors
    {|
      __property__: typing.Any = ...
      x: typing.Optional[int]
      class Foo:
        @__property__
        def x(self) -> int: ...
        @x.setter
        def x(self, value: typing.Optional[int]) -> None: ...
        @$local_file$__property__
        def y(self) -> int: ...
      def bar() -> int:
        foo = Foo()
        return foo.x
      def baz() -> int:
        foo = Foo()
        return foo.y
    |}
    [];

  (* We infer basic constructors. *)
  assert_type_errors
    {|
      class C:
        pass
      class D:
        def __init__(self) -> None:
          self.x = C()
        def foo(self) -> int:
          return self.x
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `D` has type `C` but no type is" ^
      " specified.";
      "Incompatible return type [7]: Expected `int` but got `C`.";
    ];

  assert_type_errors
    {|
      class C:
        pass
      class D:
        def __init__(self) -> None:
          # We trust the callee blindly without examining the arguments for inference.
          self.x = C(1,2,3,4)
        def foo(self) -> int:
          return self.x
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `D` has type `C` but no type is" ^
      " specified.";
      "Too many arguments [19]: Call `object.__init__` expects 0 arguments, 4 were" ^
      " provided.";
      "Incompatible return type [7]: Expected `int` but got `C`."]


let test_check_globals _ =
  assert_type_errors
    {|
      constant: int = 1
      def foo() -> str:
        return constant
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];

  assert_type_errors
    {|
      nasty_global = foo()
      def foo() -> int:
        a = nasty_global
        return 0
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `nasty_global` " ^
      "has type `int` but no type is specified.";
    ]


let test_check_immutables _ =
  assert_type_errors
    {|
    constant: int
    def foo() -> None:
      global constant
      constant = "hi"
    |}
    [
      "Incompatible variable type [9]: constant is declared to have type `int` but is used as " ^
      "type `str`.";
    ];

  assert_type_errors
    ~debug:false
    {|
      def expects_str(x: str) -> None:
        pass

      def foo(x: int, y: typing.Any) -> None:
        x = y
        expects_str(x)
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as " ^
      "type `typing.Any`.";
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 1st anonymous parameter to call `expects_str` but got `int`."
    ];

  assert_type_errors
    {|
      def foo(x: str = 1) -> str:
        return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `str` but is used as " ^
      "type `int`."
    ];

  assert_type_errors
    {|
      def bar() -> typing.Any:
        ...
      def foo(x: str = bar()) -> str:
        return x
    |}
    [];

  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        constant = "hi"
    |}
    [];

  assert_type_errors
    {|
      constant: int
      def foo() -> None:
        global constant
        constant: str
        constant = "hi"
    |}
    [];

  assert_type_errors
    {|
      constant: typing.Union[int, str]
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];

  assert_type_errors
    {|
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    [];

  assert_type_errors
    {|
      constant: typing.Optional[str]
      def foo() -> int:
        if constant is not None:
          return constant
        return 0
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];

  assert_type_errors
    {|
      constant: typing.Optional[int]
      def foo() -> int:
        if constant is not None:
          return 0
        return constant
    |}
    ["Incompatible return type [7]: Expected `int` but got `None`."];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
      constant: typing.Any
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];

  assert_type_errors
    {|
      constant
      def foo() -> int:
        global constant
        constant = 1
        return constant
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      constant: int
      def foo(x: int) -> str:
        if x > 10:
          global constant
          constant: str
        return constant
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `typing.Union[int, str]`."
    ];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        x = "hi"
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `int` but is used as " ^
      "type `str`."
    ];

  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> None:
        x = 1
    |}
    [];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        x: str
        x = "hi"
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        x = 1
        y: str
        y = x
        x = y
    |}
    [
      "Incompatible variable type [9]: y is declared to have type `str` but is used as " ^
      "type `int`."
    ];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        if x > 10:
          y: int
        else:
          y: str

        y = "hi"
    |}
    [];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        if x > 10:
          y: int
        else:
          y: str
        y = 1
    |}
    [];

  assert_type_errors
    {|
      class Foo():
        attribute
      def bar() -> None:
        foo = Foo()
        foo.attribute = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `attribute` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Undefined name [18]: Global name `attribute` is undefined.";
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `int` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      def foo() -> None:
        x: typing.Dict[str, typing.Any] = {}
        x = { 'a': 'b' }
    |}
    [];

  assert_type_errors
    ~debug:false
    {|
      constant = 1
      def foo() -> None:
        global constant
        constant = 1
    |}
    [];

  (* TODO(T25072735): error on typing.Any (incompatible usage) rather than suggest it *)
  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Union[int, str]` but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Union[int, str]` but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = None
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Optional[int]` but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `typing." ^
      "Optional[int]` but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = 1
      def bar() -> None:
        global constant
        constant = 1.0
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `float` " ^
      "but no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `float` " ^
      "but no type is specified."
    ];

  assert_type_errors
    {|
      constant
      def foo() -> None:
        global constant
        constant = A()
      def bar() -> None:
        global constant
        constant = B()
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `A` but " ^
      "no type is specified.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `A` but " ^
      "no type is specified."
    ];

  assert_type_errors
    {|
      constant
      class Foo():
        constant
      def foo() -> None:
        foo = Foo()
        foo.constant = 1
      def bar() -> None:
        global constant
        constant = "hi"
    |}
    [
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing attribute annotation [4]: Attribute `constant` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Undefined name [18]: Global name `constant` is undefined.";
      "Missing global annotation [5]: Globally accessible variable `constant` has type `str` but " ^
      "no type is specified.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `unknown`.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name', 'attribute']
        def foo(self) -> str:
          return self.name + self.attribute + self.constant
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `constant`.";
    ];

  assert_type_errors
    {|
      class Foo():
        __slots__: typing.List[str] = ['name']
        def foo(self) -> str:
          return self.name
        def __init__(self) -> None:
          self.name: int = 1
    |}
    [
      "Incompatible return type [7]: Expected `str` but got `int`.";
    ]


let test_check_named_arguments _ =
  assert_type_errors
    {|
      def bar()->int:
        return str_float_to_int(i="",f=2.0) + str_float_to_int(f=1.0,i="bar")
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int): ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      class Bar:
        @classmethod
        def bar(cls, a: str, b: int = 10): ...

      Bar.bar("asdf", 10)
    |}
    [];
  assert_type_errors
    {|
      def bar()->int:
        return str_float_to_int(i="")
    |}
    ["Missing argument [20]: Call `str_float_to_int` expects argument `f`."];
  assert_type_errors
    {|
      def bar()->int:
        return 1 + str_float_to_int(i=2.0,f=1)
      def foo()->int:
        return str_float_to_int(f="No",i="Hi")
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 1st parameter `i` to call `str_float_to_int` but got `float`.";
      "Incompatible parameter type [6]: " ^
      "Expected `float` for 1st parameter `f` to call `str_float_to_int` but got `str`.";
    ]


let test_check_missing_parameter _ =
  assert_type_errors
    ~debug:false
    ~strict:false
    {|
      def foo(x):
        return 1
    |}
    [];
  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def foo(x) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` has no type specified."];
  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def foo(x = 1) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];
  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def foo(x: typing.Any) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
  assert_type_errors
    ~debug:false
    ~strict:true
    {|
      def foo(x: typing.Any = unknown) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."]


let test_check_missing_return _ =
  assert_type_errors
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but no return type is specified."];

  assert_type_errors
    {|
      def foo() -> typing.Any:
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but type `Any` is specified."];

  assert_type_errors
    {|
      def foo() -> None:
        return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."];

  assert_type_errors
    {|
      def foo():
        return
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];

  assert_type_errors
    {|
      def foo():
        return None
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified.";];

  assert_type_errors
    {|
      def foo() -> None:
        return None
    |}
    [];

  assert_type_errors
    {|
      def foo(a: int):
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but no return type is " ^
      "specified."
    ];

  assert_type_errors
    {|
      def foo() -> None:
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Undefined name [18]: Global name `a` is undefined.";
      "Incompatible return type [7]: Expected `None` but got `int`."
    ];

  assert_type_errors
    {|
      def foo(x) -> typing.Any:
        return x
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
    ];

  assert_type_errors
    {|
      def foo(x):
        return x
    |}
    [
      "Missing return annotation [3]: Return type is not specified.";
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
    ];

  assert_type_errors
    {|
      def unknown_call():
        pass
      def foo():
        x = unknown_call()
        return x
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified.";
      "Missing return annotation [3]: Return type is not specified.";
    ];

  assert_type_errors
    {|
       def foo(x: typing.Any) -> typing.Any:
         return x
     |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."
    ];

  (* Don't report in non-debug mode. *)
  assert_type_errors
    ~debug:false
    {|
      def foo():
        return 1
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      def foo():
        pass
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      def foo(x):
        return x
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      1 + 'asdf'  # report in top-level function
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."]


let test_check_missing_attribute _ =
  assert_type_errors
    {|
      class Foo:
        a = unknown
      Foo.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified.";
      "Undefined name [18]: Global name `unknown` is undefined.";
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but no type is specified."
    ];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, a) -> None:
          self.a = a
    |}
    [
      "Missing parameter annotation [2]: Parameter `a` has no type specified.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        a = unknown
    |}
    [
      "Undefined name [18]: Global name `unknown` is undefined.";
    ];

  assert_type_errors
    {|
        class Foo:
          a: typing.Any
        Foo.a = 1
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but type `Any` is specified.";
      "Uninitialized attribute [13]: Attribute `a` is declared in class `Foo` to have " ^
      "non-optional type `typing.Any` but is never initialized."
    ];

  assert_type_errors
    {|
        class Foo:
          def __init__(self, a: typing.Any) -> None:
            self.a = a
    |}
    [
      "Missing parameter annotation [2]: Parameter `a` must have a type other than `Any`.";
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has no type specified.";
    ];

  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          self.a: typing.Any
      Foo().a = 1
      def foo() -> bool:
        return Foo().a
    |}
    [
      "Missing attribute annotation [4]: Attribute `a` of class `Foo` has type `int` " ^
      "but type `Any` is specified.";
      "Incompatible return type [7]: Expected `bool` but got `int`."
    ];

  (* Don't report in non-debug mode. *)
  assert_type_errors
    ~debug:false
    {|
      class Foo:
        def __init__(self) -> None:
          self.a = 1
    |}
    [];
  assert_type_errors
    ~debug:false
    {|
      class Foo:
        a: typing.Any
      Foo.a = 1
    |}
    []


let test_check_yield _ =
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
      def foo() -> typing.Any:
        yield
    |}
    [
      "Missing return annotation [3]: Returning `typing.Generator[None, None, None]` " ^
      "but type `Any` is specified."
    ];

  assert_type_errors
    {|
      def foo():
        yield
    |}
    [
      "Missing return annotation [3]: Returning `typing.Generator[None, None, None]` " ^
      "but no return type is specified."
    ];

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
    []


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


let test_check_union _ =
  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `typing.Union[int, str]` but got `float`."];

  assert_type_errors
    {|
      def foo() -> typing.Union[str, int]:
        if condition():
          return 1
        else:
          return 'foo'
    |}
    [];

  assert_type_errors
    {|
      def takes_int(a: int) -> None: ...
      def takes_str(a: str) -> None: ...

      def foo(a: typing.Union[str, int]) -> None:
        if isinstance(a, str):
          takes_str(a)
        else:
          takes_int(a)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, int, float]) -> int:
        if isinstance(a, int):
          return a
        else:
          return a
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `typing.Union[float, str]`."
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    [
      "Incompatible return type [7]: Expected `float` but got `typing.Union[int, str]`."
    ];

  assert_type_errors
    {|
      variable: typing.Union[typing.Optional[int], typing.Optional[str]] = None
      def ret_opt_int() -> typing.Optional[int]:
          return None
      variable = ret_opt_int()
    |}
    [];

  assert_type_errors
    {|
      def foo(x: typing.Union[int, Undefined]) -> None:
        pass
      foo(1)
    |}
    [
      "Undefined type [11]: Type `Undefined` is not defined.";
      "Incompatible parameter type [6]: Expected `typing.Union[Undefined, int]` " ^
      "for 1st anonymous parameter to call `foo` but got `int`.";
    ]


let test_check_return_joining _ =
  assert_type_errors
    {|
      def foo():
        if condition():
          return 1
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is " ^
      "specified."
    ];
  assert_type_errors
    {|
      def foo():
        if condition():
          return 1
        else:
          return 2.0
    |}
    ["Missing return annotation [3]: Returning `float` but no return type is specified."];
  assert_type_errors
    {|
      def foo():
        if condition():
          return None
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[str]` but no return type is " ^
      "specified."
    ];
  assert_type_errors
    {|
      def foo():
        if condition():
          return A()
        else:
          return B()
    |}
    ["Missing return annotation [3]: Returning `A` but no return type is specified."]


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


let test_check_invalid_constructor _ =
  assert_type_errors
    {|
      class C:
        def __init__(self) -> None:
          return
    |}
    [];

  assert_type_errors
    {|
      class C:
        def __init__(self) -> int:
          return 0
    |}
    [
      "Incompatible constructor annotation [17]: `__init__` is annotated as " ^
      "returning `int`, but it should return `None`.";
    ]


let test_check_unbounded_variables _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def expects_any(input) -> None: ...
      def expects_string(inut: str) -> None: ...
      def foo(input: T) -> None:
        expects_any(input)
        expects_string(input)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expects_string` but got `Variable[T]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> typing.Any:
        return input
    |}
    ["Missing return annotation [3]: Returning `Variable[T]` but type `Any` is specified."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> int:
        return input
    |}
    ["Incompatible return type [7]: Expected `int` but got `Variable[T]`."]


let test_check_variable_restrictions _ =
  assert_type_errors
    {|
       def f(x: str) -> int:
         return variable_restricted_identity(x)
    |}
    ["Incompatible return type [7]: Expected `int` but got `str`."];
  assert_type_errors
    {|
       def f(x: str) -> str:
         return variable_restricted_identity(x)
    |}
    [];
  assert_type_errors
    {|
       def f(x: float) -> str:
         return variable_restricted_identity(x)
    |}
    ["Incompatible return type [7]: Expected `str` but got `unknown`."];

  assert_type_errors
    {|
      T = typing.TypeVar('T', int, str)
      def foo(t: T) -> None: ...
      def bar(t: T) -> None:
        foo(t)
    |}
    []


let test_check_variable_bindings _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        str_to_int(t)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `str_to_int` but got " ^
     "`Variable[T (bound to int)]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo() -> T:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `Variable[T (bound to int)]` but got `float`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        int_to_str(t)
      def bar(x: str) -> None:
        foo(x)
    |}
    ["Incompatible parameter type [6]: Expected `Variable[T (bound to int)]` for 1st anonymous " ^
     "parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      class C():
        def baz(self) -> int:
          return 7
      T = typing.TypeVar('T', bound=C)
      def foo(t: T) -> int:
        return t.baz()
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
      def foo(x: typing.Union[int, str]) -> int:
        if isinstance(x, str):
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          return 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          raise
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          x = 1
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[int]) -> int:
        if x is None:
          continue
        return x
    |}
    [];
  assert_type_errors
    {|
      def foo(x: typing.Optional[float]) -> typing.Optional[int]:
        if x is not None:
          return int(x)
        return x
    |}
    [];
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
      def foo()-> typing.Tuple:
        return ()
    |}
    [];
  assert_type_errors
    {|
      def foo()-> typing.Tuple:
        return (1, 2)
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


let test_check_meta _ =
  assert_type_errors
    {|
      def foo(input: str) -> typing.List[int]:
        return typing.cast(typing.List[float], input)
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[float]`."];
  assert_type_errors
    {|
      def foo(input) -> typing.List[int]:
        return typing.cast(typing.List[unknown], input)
    |}
    [
      "Missing parameter annotation [2]: Parameter `input` has no type specified.";
      "Undefined type [11]: Type `unknown` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      T = typing.TypeVar('T')
      S = typing.TypeVar('S')
      class C(typing.Generic[T]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(C[int], input)
      class D(typing.Generic[T, S]): pass
      def foo(input: typing.Any) -> None:
        typing.cast(D[int, float], input)
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> C:
        return C.__construct__()
      def boo() -> Subclass:
        return Subclass.__construct__()
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo() -> C:
        return Subclass.__construct__()
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        @classmethod
        def __construct__(cls: typing.Type[T]) -> T:
          ...
      class Subclass(C):
        ...
      def foo()-> Subclass:
        return C.__construct__()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(s: Subclass) -> Subclass:
        to_call = s.f
        return to_call()
    |}
    [];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      class C:
        def f(self: T) -> T:
          ...
      class Subclass(C):
        ...
      def foo(c: C)-> Subclass:
        to_call = c.f
        return to_call()
    |}
    ["Incompatible return type [7]: Expected `Subclass` but got `C`."];


  assert_type_errors
    {|
      class Foo:
        def foo(self) -> typing.Type[Foo]:
          return type(self)
        def bar(self) -> typing.Type[int]:
          return type(1)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        ATTRIBUTE: typing.ClassVar[int] = 1
        def foo(self) -> int:
          return type(self).ATTRIBUTE
    |}
    [];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(t: T) -> str:
        return type(t).__name__
    |}
    [];

  assert_type_errors
    {|
      def foo(x: int) -> str:
        return type(x).__name__
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


let test_check_assert _ =
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional or len(optional) > 0:
          pass
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Sized` for 1st anonymous parameter to call `len` but got " ^
     "`typing.Optional[str]`."];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional is None or len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      def foo(optional: typing.Optional[str]) -> None:
        if optional and len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False, "unreachable, surely"
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert not True
        return int_to_int(x)
    |}
    ["Undefined name [18]: Global name `x` is undefined."];
  assert_type_errors
    {|
      def foo() -> int:
        if True:
          return 0
        else:
          return int_to_int("monkey news")
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
    []


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
      "Missing type parameters [24]: Generic type `C` expects 1 type parameters.";
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
    ];

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


let test_check_collections _ =
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.List[int]]) -> typing.List[int]:
        return input or []
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.Set[int]]) -> typing.Set[int]:
        return input or set()
    |}
    [];
  assert_type_errors
    {|
      def foo(input: typing.Optional[typing.Dict[int, str]]) -> typing.Dict[int, str]:
        return input or {}
    |}
    []


let test_check_constructors _ =
  assert_type_errors
    {|
      class Foo:
        def __init__(self) -> None:
          pass
      def foo() -> Foo:
        return Foo()
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo(10)
    |}
    [];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
      def foo() -> Foo:
        return Foo('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`."];
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int, s: typing.Optional[str] = None) -> None:
          pass
      def foo() -> None:
        Foo('asdf')
        Foo(1, 2)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`.";
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 2nd anonymous parameter to call `Foo.__init__` " ^
      "but got `int`.";
    ];

  (* Explicit call. *)
  assert_type_errors
    {|
      class Foo:
        def __init__(self, i: int) -> None:
          pass
        def foo(self) -> None:
          Foo.__init__(self, 'asdf')
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 2nd anonymous parameter to call \
       `Foo.__init__` but got `str`.";
    ];

  (* Super calls. *)
  assert_type_errors
    {|
      class Super:
        def foo(self, i: int) -> None:
          pass
      class Foo(Super):
        def foo(self, i: int) -> None:
          super().foo('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.foo` but got `str`."];
  assert_type_errors
    {|
      class Super:
        def __init__(self, i: int) -> None:
          pass
      class Foo(Super):
        def __init__(self, i: int) -> None:
          super().__init__('asdf')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Super.__init__` but got `str`."];

  (* The MRO of inheriting both a class and its direct parent will result in super() evaluating
     to the subclass, regardless of order. *)
  assert_type_errors
    {|
      class Subclass(A, B):
        def foo(self)->A:
          return super()
        def wrong(self)->B:
          return super()
    |}
    [];

  (* Overloaded constructors. *)
  assert_type_errors
    {|
      class Class:
        @overload
        def __init__(self, i: int) -> None: ...
        @overload
        def __init__(self, s: str) -> None: ...
      def construct() -> None:
        Class(1)
        Class('asdf')
    |}
    [];

  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None: ...
      def foo(x: typing.Type[Class]) -> Class:
        return x(7)
    |}
    [];

  assert_type_errors
    {|
      class Class:
        def __init__(self, i: int) -> None: ...
      def foo(x: typing.Type[Clss]) -> Class:
        return x(7)
    |}
    [
      "Undefined type [11]: Type `Clss` is not defined.";
      "Incompatible return type [7]: Expected `Class` but got `unknown`.";
      "Call error [29]: `typing.Type[Clss]` is not a function.";
    ]


let test_check_explicit_method_call _ =
  assert_type_errors
    {|
      class Class:
        def method(self, i: int) -> None:
          pass
      Class.method(object(), 1)
    |}
    []


let test_check_meta_annotations _ =
  assert_type_errors
    {|
      class Class:
        pass
      def foo() -> typing.Type[Class]:
        return Class
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


let test_check_noreturn _ =
  assert_type_errors
    {|
      def no_return() -> typing.NoReturn:
        return 0
    |}
    ["Incompatible return type [7]: Expected `typing.NoReturn` but got `int`."];
  assert_type_errors
    {|
      def no_return() -> typing.NoReturn:
        # We implicitly return None, so have to accept this.
        return None
    |}
    [];

  assert_type_errors
    {|
      def no_return(input: typing.Optional[int]) -> int:
        if input is None:
          sys.exit(-1)
        return input
    |}
    [];

  assert_type_errors
    {|
      def no_return() -> str:
        sys.exit(0)  # once control flow terminates, we know input won't be returned.
    |}
    [];

  assert_type_errors
    {|
      def may_not_return() -> str:
        if condition():
          sys.exit(0)
        else:
          return ""
    |}
    [];

  assert_type_errors
    {|
      def no_return() -> int:
        if condition():
          return 1
        else:
          sys.exit(0)
    |}
    []


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


let test_check_callables _ =
  (* Objects with a `__call__` method are callables. *)
  assert_type_errors
    {|
      class Call:
        def __call__(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [];

  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(call: Call) -> int:
        return call()
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Call error [29]: `Call` is not a function.";
    ];

  (* Test for terminating fixpoint *)
  assert_type_errors
    {|
      class Call:
        def not_call(self) -> int: ...
      def foo(x: int, call: Call) -> int:
        for x in range(0, 7):
          call()
        return 7
    |}
    [
      "Call error [29]: `Call` is not a function.";
    ];

  assert_type_errors
    {|
      class patch:
        def __call__(self) -> int: ...

      unittest.mock.patch: patch = ...

      def foo() -> None:
        unittest.mock.patch()
        unittest.mock.patch()  # subequent calls should not modify annotation map
    |}
    [];

  assert_type_errors
    {|
      class Foo:
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Callable(Foo.bar)[[Named(self, unknown), Named(x, int)], str]`.";
    ];

  assert_type_errors
    {|
      class Foo:
        @classmethod
        def bar(self, x: int) -> str:
          return ""

      def bar() -> None:
        return Foo.bar
    |}
    [
      "Incompatible return type [7]: Expected `None` but got " ^
      "`typing.Callable(Foo.bar)[[Named(x, int)], str]`.";
    ];

  assert_type_errors
    {|
      class Call:
        def __call__(self, x: int) -> int: ...
      def foo(call: Call) -> int:
        return call("")
    |}
    [
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `Call.__call__` but got `str`.";
    ];

  (* Callable parameter checks. *)
  assert_type_errors
    {|
      def foo(callable: typing.Callable[[str], None]) -> None:
        callable(1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to anoynmous call but got `int`."];

  (* Type variables & callables. *)
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(x: str) -> int:
        return 0
      def takes_parameter(f: typing.Callable[[T], int]) -> T:
        ...
      def takes_return(f: typing.Callable[[str], T]) -> T:
        ...
      def f() -> str:
        return takes_parameter(foo)
      def g() -> int:
        return takes_return(foo)
    |}
    [];

  assert_type_errors
    {|
      def foo(f: typing.Callable[..., int]) -> None:
        ...
      def i2i(x: int) -> int:
        return x
      foo(i2i)
    |}
    [];
  assert_type_errors
    {|
      def foo(f: typing.Callable[..., int]) -> None:
        ...
      def i2s(x: int) -> str:
        return ""
      foo(i2s)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Callable[..., int]` for 1st anonymous parameter to call `foo` but got " ^
      "`typing.Callable(i2s)[[Named(x, int)], str]`.";
    ];

  (* Classes with __call__ are callables. *)
  assert_type_errors
    {|
      class CallMe:
        def __call__(self, x:int) -> str:
          ...
      class CallMeToo(CallMe):
        pass

      def map(f: typing.Callable[[int], str], l: typing.List[int]) -> typing.List[str]:
        ...
      def apply(x: CallMe, y: CallMeToo) -> None:
        map(x, [])
        map(y, [])
    |}
    [];
  assert_type_errors
    {|
      class CallMe:
        def __call__(self, x: str) -> str:
          ...
      class CallMeToo(CallMe):
        pass

      def map(f: typing.Callable[[int], str], l: typing.List[int]) -> typing.List[str]:
        ...
      def apply(x: CallMe, y: CallMeToo) -> None:
        map(x, [])
        map(y, [])
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Callable[[int], str]` for 1st anonymous parameter to call `map` but got " ^
      "`CallMe`.";
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Callable[[int], str]` for 1st anonymous parameter to call `map` but got " ^
      "`CallMeToo`.";
    ];

  (* Sanity check: Callables do not subclass classes. *)
  assert_type_errors
    {|
      class CallMe:
        def __call__(self, x: int) -> str:
          ...
      def map(callable_object: CallMe, x: int) -> None:
         callable_object(x)
      def apply(f: typing.Callable[[int], str]) -> None:
        map(f, 1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `CallMe` for 1st anonymous parameter to call `map` but got " ^
     "`typing.Callable[[int], str]`."];

  (* The annotation for callable gets expanded automatically. *)
  assert_type_errors
    {|
      def i2i(x: int) -> int:
        return 0
      def hof(c: typing.Callable) -> None:
        return
      hof(i2i)
      hof(1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Callable[..., unknown]` for 1st anonymous parameter to call `hof` but got " ^
     "`int`."];

  (* Lambdas. *)
  assert_type_errors
    {|
      def takes_callable(f: typing.Callable[[Named(x, typing.Any)], int]) -> int:
        return 0
      takes_callable(lambda y: 0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Callable[[Named(x, typing.Any)], int]` for 1st anonymous parameter " ^
      "to call `takes_callable` but got `typing.Callable[[Named(y, typing.Any)], int]`.";
    ];
  assert_type_errors
    {|
      def takes_callable(f: typing.Callable[[Named(x, typing.Any)], int]) -> int:
        return 0
      takes_callable(lambda y: "")
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Callable[[Named(x, typing.Any)], int]` for 1st anonymous parameter " ^
      "to call `takes_callable` but got `typing.Callable[[Named(y, typing.Any)], str]`.";
    ];

  assert_type_errors
    ~debug:false
    {|
      def exec(f: typing.Callable[[], int]) -> int:
        return f()
      def with_default(x: int = 0) -> int:
        return x
      def with_kwargs( **kwargs: int) -> int:
        return 0
      def with_varargs( *varargs: int) -> int:
        return 0
      def with_everything( *varargs: int, **kwargs: int) -> int:
        return 0
      exec(with_default)
      exec(with_kwargs)
      exec(with_varargs)
      exec(with_everything)
    |}
    []


let test_check_assert_functions _ =
  assert_type_errors
    ~debug:false
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    ~debug:false
    ~qualifier:(Access.create "foo")
    {|
      class One:
          a: int

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    {|
      class One:
          a: int = 1

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    []


let test_check_undefined_type _ =
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp) -> Herp:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp, y: Herp) -> None:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: int) -> Herp:
        return x
    |}
    ["Undefined type [11]: Type `Herp` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: typing.Union[Derp, Herp]) -> typing.List[Herp]:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp[int]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Optional) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Optional` is not defined."];

  assert_type_errors
    ~debug:false
    {|
      def foo() -> None:
        x: undefined = 1
        return
    |}
    ["Undefined type [11]: Type `undefined` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp) -> None:
        y: undefined = 1
        return
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `undefined` is not defined.";
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(x: T) -> typing.Union[str, T]:
        return x
    |}
    [];

  (* Ensure other errors are not missed when undefined type is thrown. *)
  assert_type_errors
    {|
      class Bar:
          async def undefined(self, x: Derp) -> Derp:
              return x
      class Foo(Bar):
          def error(self) -> int:
              return None
          async def undefined(self, x: Herp) -> Herp:
              return x
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined.";
      "Incompatible return type [7]: Expected `int` but got `None`.";
      "Undefined type [11]: Type `Herp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ]


let test_check_analysis_failure _ =
  assert_type_errors
    {|
      def foo() -> Derp:
        pass

      def bar(x: int = foo()) -> int:
        return x
    |}
    ["Analysis failure [30]: Terminating analysis because type `Derp` is not defined."]


let test_check_missing_type_parameters _ =
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: C) -> None:
        return None
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameters."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: typing.List[C]) -> None:
        return None
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameters."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameters."]


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


let test_format_string _ =
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1}'
    |}
    [];
  assert_type_errors
    {|
      def foo() -> None:
        f'foo{1 + "x"}'
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + "x"}'
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."];
  assert_type_errors
    {|
      global_number: int = 1
      def foo() -> None:
        f'foo{global_number + 2}'
    |}
    [];
  assert_type_errors
    {|
      def boo() -> int:
        return 1

      def foo() -> None:
        f'{boo() + "x"}'
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__add__` but got `str`."]


let test_check_data_class _ =
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo('a')
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `Foo.__init__` but got `str`."];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    ["Too many arguments [19]: Call `Foo.__init__` expects 1 argument, " ^
     "2 were provided."];
  assert_type_errors
    {|
      @dataclasses.dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo(4,5)
    |}
    ["Too many arguments [19]: Call `Foo.__init__` expects 1 argument, " ^
     "2 were provided."];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x = 1
      def boo() -> None:
          b = Foo(2)
    |}
    [
      "Missing attribute annotation [4]: Attribute `x` of class `Foo` has type `int` but " ^
      "no type is specified.";
      "Too many arguments [19]: Call `Foo.__init__` expects 0 arguments, 1 was" ^
      " provided.";
    ];
  assert_type_errors
    {|
      @dataclass
      class Foo():
        x: int = 1
      def boo() -> None:
          b = Foo()
    |}
    []


let test_check_typed_dictionaries _ =
  let assert_test_typed_dictionary source =
    let typing_stub =
      {
        qualifier = Access.create "typing";
        handle = "typing.pyi";
        source =
          {|
            Any = object()
            class _SpecialForm:
                def __getitem__(self, typeargs: Any) -> Any: ...
          |};
      }
    in
    let mypy_extensions_stub =
      {
        qualifier = Access.create "mypy_extensions";
        handle = "mypy_extensions.pyi";
        source =
          "def TypedDict(typename: str, fields: Dict[str, Type[_T]], total: bool = ...) -> \
           Type[dict]: ..."
      }
    in
    let typed_dictionary_for_import =
      {
        qualifier = Access.create "foo.bar.baz";
        handle = "foo/bar/baz.py";
        source =
          {|
            from mypy_extensions import TypedDict
            class ClassBasedTypedDictGreekLetters(TypedDict):
              alpha: int
              beta: str
              gamma: bool
          |}
      }
    in
    assert_type_errors
      ~update_environment_with:[
        typing_stub;
        mypy_extensions_stub;
        typed_dictionary_for_import;
      ]
      source
  in

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['year'])
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['name'])
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `foo` but got `str`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        a = foo(movie['yar'])
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `yar`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: Movie
        key = "year"
        a = foo(movie[key])
    |}
    [
      "TypedDict accessed with a non-literal [26]: TypedDict key must be a string literal; " ^
      "expected one of ('name', 'year')."
    ];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Film = mypy_extensions.TypedDict('Film', {'name': str, 'year': 'int', 'director': str})
      def foo(movie: Movie) -> str:
        return movie["name"]
      def f() -> None:
        movie: Film
        a = foo(movie)
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      Actor = mypy_extensions.TypedDict('Actor', {'name': str, 'birthyear': 'int'})
      def foo(movie: Movie) -> str:
        return movie["name"]
      def f() -> None:
        actor: Actor
        a = foo(actor)
    |}
    [
      "Incompatible parameter type [6]: Expected `TypedDict `Movie` with " ^
      "fields (name: str, year: int)` for 1st anonymous parameter to call `foo` " ^
      "but got `TypedDict `Actor` with fields (name: str, birthyear: int)`."
    ];

  assert_test_typed_dictionary
    {|
      from mypy_extensions import TypedDict
      Movie = TypedDict('Movie', {'name': str, 'year': int})
      Cat = TypedDict('Cat', {'name': str, 'breed': str})
      Named = TypedDict('Named', {'name': str})

      def foo(x: int, a: Movie, b: Cat) -> Named:
        if x == 7:
            q = a
        else:
            q = b
        return q
    |}
    [];

  assert_test_typed_dictionary
    {|
      from mypy_extensions import TypedDict
      Movie = TypedDict('Movie', {'name': str, 'year': int})
      Cat = TypedDict('Cat', {'name': str, 'breed': str})

      def foo(x: int, a: Movie, b: Cat) -> int:
          if x == 7:
              q = a
          else:
              q = b
          return q["year"]
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "TypedDict accessed with a missing key [27]: TypedDict has no key `year`.";
    ];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': str})
      def foo(dictionary: Mapping[str, typing.Any]) -> None:
        pass
      def f() -> None:
        baz: Baz
        a = foo(baz)
    |}
    [];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      class A:
        pass
      class B(A):
        pass
      Baz = mypy_extensions.TypedDict('Baz', {'foo': A, 'bar': B})
      def foo(dictionary: Mapping[str, A]) -> A:
        return dictionary["foo"]
      def f() -> None:
        baz: Baz
        a = foo(baz)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `Mapping[str, A]` for 1st anonymous parameter to call `foo` but got " ^
      "`TypedDict `Baz` with fields (foo: A, bar: B)`."
    ];

  assert_test_typed_dictionary
    {|
      from typing import Mapping
      class A:
        pass
      class B(A):
        pass
      class C(A):
        pass
      Baz = mypy_extensions.TypedDict('Baz', {'foo': A, 'bar': B})
      def foo(x: int, a: Baz, b: Mapping[str, C]) -> Mapping[str, A]:
        if x == 7:
            q = a
        else:
            q = b
        return q
    |}
    [
      "Incompatible return type [7]: Expected `Mapping[str, A]` but got `Mapping[str, typing.Any]`."
    ];

  assert_test_typed_dictionary
    {|
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': int})
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            q = a["fou"]
        else:
            q = a["bar"]
        return q
    |}
    [
      "TypedDict accessed with a missing key [27]: TypedDict `Baz` has no key `fou`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_test_typed_dictionary
    {|
      Baz = mypy_extensions.TypedDict('Baz', {'foo': int, 'bar': int})
      def foo(x: int, a: Baz) -> int:
        if x == 7:
            k = "foo"
            q = a[k]
        else:
            q = a["bar"]
        return q
    |}
    [
      "TypedDict accessed with a non-literal [26]: " ^
      "TypedDict key must be a string literal; expected one of ('foo', 'bar').";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982)
        return movie['year']
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(year=1982, name='Blade Runner')
        return movie['year']
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name=1982, year='Blade Runner')
        return movie['year']
    |}
    ["Incompatible parameter type [6]: Expected `int` for 2nd parameter `year` to call `__init__` but got `str`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie('Blade Runner', 1982)
        return movie['year']
    |}
    ["Too many arguments [19]: Call `__init__` expects 3 arguments, 5 were provided."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
      def foo() -> int:
        movie = Movie(name='Blade Runner', year=1982, extra=42)
        return movie['year']
    |}
    ["Unexpected keyword [28]: Unexpected keyword argument `extra` to call `__init__`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 'new name'
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] = 7
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got " ^
     "`int`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['nme'] = 'new name'
    |}
    ["TypedDict accessed with a missing key [27]: TypedDict `Movie` has no key `nme`."];

  assert_test_typed_dictionary
    {|
      class A():
        pass
      class B(A):
        pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something': A})
      def f() -> None:
        movie: Movie
        movie['something'] = B()
    |}
    [];

  assert_test_typed_dictionary
    {|
      class A():
        pass
      class B(A):
        pass
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'something': B})
      def f() -> None:
        movie: Movie
        movie['something'] = A()
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `B` for 2nd anonymous parameter to call `TypedDictionary.__setitem__` but got `A`."];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['year'] += 7
    |}
    [];

  assert_test_typed_dictionary
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': 'int'})
      def f() -> None:
        movie: Movie
        movie['name'] += 7
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `int` for 1st anonymous parameter to call `int.__radd__` but got " ^
     "`unknown`."];

  assert_test_typed_dictionary
    {|
      from foo.bar.baz import ClassBasedTypedDictGreekLetters
      def f() -> int:
        baz = ClassBasedTypedDictGreekLetters(alpha = 7, beta = "a", gamma = True)
        return baz['alpha']
    |}
    [];

  (* TODO T37629490 Better error messages for typeddict declaration errors *)
  assert_test_typed_dictionary
    {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
      def foo(x: int) -> str:
        return ""
      def f() -> None:
        movie: NamelessTypedDict
        a = foo(movie['year'])
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `NamelessTypedDict` has type " ^
      "`Type[typing.Dict[typing.Any, typing.Any]]` but no type is specified.";
      "Missing argument [20]: Call `mypy_extensions.TypedDict` expects argument `fields`.";
      "Undefined type [11]: Type `NamelessTypedDict` is not defined.";
      "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call `foo` " ^
      "but got `unknown`.";
    ]


let test_check_getattr _ =
  let assert_test_getattr source =
    let getattr_stub =
      {
        qualifier = Access.create "has_getattr";
        handle = "has_getattr.pyi";
        source =
          {|
            from typing import Any
            def __getattr__(name: str) -> Any: ...
          |};
      }
    in
    let getattr_stub_str =
      {
        qualifier = Access.create "has_getattr_str";
        handle = "has_getattr_str.pyi";
        source =
          {|
            def __getattr__(name: str) -> str: ...
          |};
      }
    in
    let getattr_stub_untyped =
      {
        qualifier = Access.create "has_getattr_untyped";
        handle = "has_getattr_untyped.pyi";
        source =
          {|
            def __getattr__(name): ...
          |};
      }
    in
    let getattr_stub_invalid_arity =
      {
        qualifier = Access.create "has_getattr_invalid_arity";
        handle = "has_getattr_invalid_arity.pyi";
        source =
          {|
            def __getattr__(x: int, y: str) -> str: ...
          |};
      }
    in
    let getattr_stub_not_callable =
      {
        qualifier = Access.create "has_getattr_not_callable";
        handle = "has_getattr_not_callable.pyi";
        source =
          {|
            __getattr__ = 3
          |};
      }
    in
    assert_type_errors
      ~update_environment_with:[
        getattr_stub;
        getattr_stub_str;
        getattr_stub_untyped;
        getattr_stub_invalid_arity;
        getattr_stub_not_callable;
      ]
      source
  in
  assert_test_getattr
    {|
      import has_getattr
      def foo() -> None:
        has_getattr.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      import has_getattr_str
      def foo() -> str:
        return has_getattr_str.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      import has_getattr_untyped
      def foo() -> None:
        has_getattr_untyped.any_attribute
    |}
    [];
  assert_test_getattr
    {|
      from has_getattr import any_attribute
      def foo() -> None:
        any_attribute
    |}
    [];
  assert_test_getattr
    {|
      from has_getattr_str import any_attribute
      def foo() -> str:
        return any_attribute
    |}
    [];
  assert_test_getattr
    {|
      import has_getattr_invalid_arity
      def foo() -> None:
         has_getattr_invalid_arity.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_invalid_arity` " ^
      "has no attribute `any_attribute`."
    ];
  assert_test_getattr
    {|
      import has_getattr_not_callable
      def foo() -> None:
         has_getattr_not_callable.any_attribute
    |}
    [
      "Undefined attribute [16]: Module `has_getattr_not_callable` " ^
      "has no attribute `any_attribute`."
    ]


let test_check_literal_variance _ =
  (* We special case literal lists and dicts for convenience, as they can never escape scope. *)
  assert_type_errors
    {|
      x: typing.List[float] = []
      x = [1]
    |}
    [];
  assert_type_errors
    {|
      x: typing.List[float] = []
      x = [y for y in [1,2,3,4]]
    |}
    [];
  (* Mutable default arguments may escape scope, and we shouldn't allow subtyping. *)
  assert_type_errors
    {|
      def foo(x: typing.List[float] = [1]) -> typing.List[float]:
        return x
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.List[float]` but is " ^
      "used as type `typing.List[int]`.";
    ];
  assert_type_errors
    {|
      x: typing.List[float] = []
      y: typing.List[int] = [1]
      x = y
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.List[float]` but is " ^
      "used as type `typing.List[int]`.";
    ];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = {}
      x = { "s": 1 }
    |}
    [];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = {}
      x = { "s": value for value in [1,2,3] }
    |}
    [];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = {}
      x = { "s": "" }
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Dict[str, float]` but " ^
      "is used as type `typing.Dict[str, str]`.";
    ];
  assert_type_errors
    {|
      x: typing.Dict[str, float] = { "s": 1 }
      y: typing.Dict[str, int] = { "s": 1 }
      x = y
    |}
    [
      "Incompatible variable type [9]: x is declared to have type `typing.Dict[str, float]` but " ^
      "is used as type `typing.Dict[str, int]`.";
    ];

  (* Returns. *)
  assert_type_errors
    {|
      def foo() -> typing.List[float]:
        return [1]
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.List[float]:
        a = [1]
        return a
    |}
    ["Incompatible return type [7]: Expected `typing.List[float]` but got `typing.List[int]`."];
  assert_type_errors
    {|
      def foo() -> typing.Dict[float, float]:
        return {1: 1}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Dict[float, float]:
        a = {1: 1}
        return a
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[float, float]` but got \
       `typing.Dict[int, int]`.";
    ];
  assert_type_errors
    {|
      def foo() -> typing.Set[float]:
        return {1}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Set[float]:
        return {x for x in [1,2,3]}
    |}
    [];
  assert_type_errors
    {|
      def foo() -> typing.Set[float]:
        a = {1}
        return a
    |}
    ["Incompatible return type [7]: Expected `typing.Set[float]` but got `typing.Set[int]`."]



let () =
  "type">:::[
    "initial">::test_initial;
    "less_or_equal">::test_less_or_equal;
    "join">::test_join;
    "widen">::test_widen;
    "forward_expression">::test_forward_expression;
    "forward_statement">::test_forward_statement;
    "forward">::test_forward;
    "reveal_type">::test_reveal_type;
    "check_error_traces">::test_show_error_traces;
    "check_with_qualification">::test_check_with_qualification;
    "coverage">::test_coverage;
    "check">::test_check;
    "check_assign">::test_check_assign;
    "check_coverage">::test_check_coverage;
    "check_comprehensions">::test_check_comprehensions;
    "check_optional">::test_check_optional;
    "check_function_overloads">::test_check_function_overloads;
    "check_function_parameters">::test_check_function_parameters;
    "check_function_parameters_with_backups">::test_check_function_parameters_with_backups;
    "check_function_parameter_errors">::test_check_function_parameter_errors;
    "check_function_redirects">::test_check_function_redirects;
    "check_invalid_constructor">::test_check_invalid_constructor;
    "check_variable_arguments">::test_check_variable_arguments;
    "check_method_returns">::test_check_method_returns;
    "check_method_parameters">::test_check_method_parameters;
    "check_method_resolution">::test_check_method_resolution;
    "check_self">::test_check_self;
    "check_static">::test_check_static;
    "check_init">::test_check_init;
    "check_attributes">::test_check_attributes;
    "check_globals">::test_check_globals;
    "check_immutables">::test_check_immutables;
    "check_imports">::test_check_imports;
    "check_named_arguments">::test_check_named_arguments;
    "check_missing_parameter">::test_check_missing_parameter;
    "check_missing_return">::test_check_missing_return;
    "check_missing_attribute">::test_check_missing_attribute;
    "check_yield">::test_check_yield;
    "check_ternary">::test_check_ternary;
    "check_union">::test_check_union;
    "check_return_joining">::test_check_return_joining;
    "check_nested">::test_check_nested;
    "check_unbounded_variables">::test_check_unbounded_variables;
    "check_variable_restrictions">::test_check_variable_restrictions;
    "check_variable_bindings">::test_check_variable_bindings;
    "check_refinement">::test_check_refinement;
    "check_toplevel">::test_check_toplevel;
    "check_tuple">::test_check_tuple;
    "check_meta">::test_check_meta;
    "check_redundant_cast">::test_check_redundant_cast;
    "check_assert">::test_check_assert;
    "check_excepts">::test_check_excepts;
    "check_async">::test_check_async;
    "check_behavioral_subtyping">::test_check_behavioral_subtyping;
    "check_collections">::test_check_collections;
    "check_constructors">::test_check_constructors;
    "check_explicit_method_call">::test_check_explicit_method_call;
    "check_meta_annotations">::test_check_meta_annotations;
    "check_unbound_variables">::test_check_unbound_variables;
    "check_noreturn">::test_check_noreturn;
    "check_contextmanager">::test_check_contextmanager;
    "check_callables">::test_check_callables;
    "check_assert_functions">::test_check_assert_functions;
    "check_undefined_type">::test_check_undefined_type;
    "check_analysis_failure">::test_check_analysis_failure;
    "check_missing_type_parameters">::test_check_missing_type_parameters;
    "environment">::test_environment;
    "scheduling">::test_scheduling;
    "check_format_string">::test_format_string;
    "check_dataclass">::test_check_data_class;
    "check_getattr">::test_check_getattr;
    "check_typed_dictionarys">::test_check_typed_dictionaries;
    "check_literal_variance">::test_check_literal_variance;
  ]
  |> Test.run
