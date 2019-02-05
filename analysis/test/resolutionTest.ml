(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Statement
open Test


let test_set_local _ =
  let assert_local ~resolution ~access ~expected =
    assert_equal
      ~cmp:(Option.equal Type.equal)
      (expected >>| parse_single_expression >>| Type.create ~aliases:(fun _ -> None))
      (Resolution.get_local resolution ~access:(Access.create access) >>| Annotation.annotation)
  in

  let resolution = Test.resolution ~sources:[] () in
  assert_local ~resolution ~access:"local" ~expected:None;

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.integer)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "int");

  let resolution =
    Resolution.set_local
      resolution
      ~access:(Access.create "local")
      ~annotation:(Annotation.create Type.float)
  in
  assert_local ~resolution ~access:"local" ~expected:(Some "float")


let test_parse_annotation _ =
  let assert_parse_annotation ?(allow_untracked=false) ~resolution ~expected expression =
    assert_equal
      ~cmp:Type.equal
      ~printer:Type.show
      (parse_single_expression expected |> Type.create ~aliases:(fun _ -> None))
      (parse_single_expression expression
      |> Resolution.parse_annotation ~allow_untracked resolution)
  in

  let resolution =
    Test.resolution
      ~sources:([
        parse ~qualifier:(Access.create "empty") ~handle:"empty.pyi" "class Empty: ...";
        parse
          ~qualifier:(Access.create "empty.stub")
          ~local_mode:Source.PlaceholderStub
          ~handle:"empty/stub.pyi"
          "";
      ] @ (Test.typeshed_stubs ()))
      ()
  in
  assert_parse_annotation ~resolution ~expected:"int" "int";
  assert_parse_annotation
    ~allow_untracked:true
    ~resolution
    ~expected:"qualifier.int"
    "$local_qualifier$int";
  assert_parse_annotation ~resolution ~expected:"typing.Any" "empty.stub.Annotation";
  assert_parse_annotation
    ~resolution
    ~expected:"typing.Dict[str, typing.Any]"
    "typing.Dict[str, empty.stub.Annotation]"


let make_resolution source =
  let configuration = Configuration.Analysis.create () in
  let populate source =
    let environment =
      let environment = Environment.Builder.create () in
      Service.Environment.populate
        ~configuration
        (Environment.handler environment)
        (parse source :: typeshed_stubs ());
      environment
    in
    Environment.handler environment
  in
  populate source
  |> fun environment -> TypeCheck.resolution environment ()


let test_resolve_literal _ =
  let resolution =
    make_resolution
      {|
      class C:
        def __init__(self) -> None:
          pass
      def foo()->int:
        ...
      i = 1
      j = foo()
      s = 'asdf'
      t = 1, 1.0
      none = None
      awaitable: typing.Awaitable[int]
    |}
  in
  let assert_resolve_literal source expected =
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    assert_equal ~printer:Type.show expected (Resolution.resolve_literal resolution expression)
  in
  assert_resolve_literal "i" Type.Top;
  assert_resolve_literal "await i" Type.Top;
  assert_resolve_literal "await awaitable" Type.Top;
  assert_resolve_literal "\"\"" Type.string;
  assert_resolve_literal "1" Type.integer;
  assert_resolve_literal "1+1" Type.Object;
  assert_resolve_literal "j" Type.Top;
  assert_resolve_literal "foo()" Type.Top;
  assert_resolve_literal "C()" (Type.Primitive "C");
  assert_resolve_literal "C" (Type.meta (Type.Primitive "C"));
  assert_resolve_literal "none" Type.none;

  (* Dictionary *)
  assert_resolve_literal "{'a': 1}" (Type.dictionary ~key:Type.string ~value:Type.integer);
  assert_resolve_literal "{'a': i}" (Type.Object);
  assert_resolve_literal "{**foo}" Type.Object;
  assert_resolve_literal "{'a': 1, **foo}" Type.Object;

  (* Boolean Operator *)
  assert_resolve_literal "1 or 2" (Type.integer);
  assert_resolve_literal "True or 1" (Type.union [Type.bool; Type.integer]);
  assert_resolve_literal "True or i" (Type.Object);

  (* List *)
  assert_resolve_literal "[1]" (Type.list Type.integer);
  assert_resolve_literal "[1, 'string']" (Type.list (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "[1, i]" (Type.Object);

  (* Set *)
  assert_resolve_literal "{1}" (Type.set Type.integer);
  assert_resolve_literal "{1, 'string'}" (Type.set (Type.Union [Type.integer; Type.string]));
  assert_resolve_literal "{1, i}" (Type.Object);

  (* Ternary *)
  assert_resolve_literal "1 if x else 2" (Type.integer);
  assert_resolve_literal "'hi' if x else 1" (Type.union [Type.string; Type.integer]);
  assert_resolve_literal "1 if i else i" (Type.Object)


let test_resolve_mutable_literals _ =
  let resolution =
    make_resolution
      {|
      class C: ...
      class D(C): ...
      class Q: ...
    |}
  in
  let assert_resolve_mutable_literals ~source ~against expected_output =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
    in
    let expression =
      match parse_single_statement source with
      | { Node.value = Statement.Expression expression; _ } -> expression
      | _ -> failwith "No Assign to parse"
    in
    let resolved = Resolution.resolve resolution expression in
    let expression = Some expression in
    let expected = parse_annotation against in
    assert_equal
      ~printer:Type.show
      (parse_annotation expected_output)
      (Resolution.resolve_mutable_literals resolution ~expression ~resolved ~expected)
  in
  assert_resolve_mutable_literals
    ~source:"[D()]"
    ~against:"typing.List[C]"
    "typing.List[C]";
  assert_resolve_mutable_literals
    ~source:"[Q()]"
    ~against:"typing.List[C]"
    "typing.List[Q]";

  assert_resolve_mutable_literals
    ~source:"[y for y in [D()]]"
    ~against:"typing.List[C]"
    "typing.List[C]";
  assert_resolve_mutable_literals
    ~source:"[y for y in [Q()]]"
    ~against:"typing.List[C]"
    "typing.List[Q]";

  assert_resolve_mutable_literals
    ~source:"{ 's': D() }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': Q() }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, Q]";

  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [D()] }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, C]";
  assert_resolve_mutable_literals
    ~source:"{ 's': y for y in [Q()] }"
    ~against:"typing.Dict[str, C]"
    "typing.Dict[str, Q]";

  assert_resolve_mutable_literals
    ~source:"{ D() }"
    ~against:"typing.Set[C]"
    "typing.Set[C]";
  assert_resolve_mutable_literals
    ~source:"{ Q() }"
    ~against:"typing.Set[C]"
    "typing.Set[Q]";

  assert_resolve_mutable_literals  "{ y for y in [D()] }"
    ~source:"typing.Set[C]"
    ~against:"typing.Set[C]";
  assert_resolve_mutable_literals  "{ y for y in [Q()] }"
    ~source:"typing.Set[C]"
    ~against:"typing.Set[Q]"


let test_function_definitions _ =
  let assert_functions sources access count =
    let sources =
      let source (path, content) = path, trim_extra_indentation content in
      List.map sources ~f:source
    in

    let files =
      let file (path, content) = File.create ~content (mock_path path) in
      List.map sources ~f:file
    in
    let { Service.Parser.syntax_error; system_error; _ } =
      Service.Parser.parse_sources
        ~configuration:mock_configuration
        ~scheduler:(Scheduler.mock ())
        ~preprocessing_state:None
        ~files
    in
    assert_true (List.is_empty syntax_error);
    assert_true (List.is_empty system_error);

    let resolution =
      let sources =
        let source (path, content) =
          parse
            ~qualifier:(Access.create (String.chop_suffix_exn path ~suffix:".py"))
            ~handle:path
            content
          |> Preprocessing.qualify
        in
        List.map sources ~f:source
      in
      resolution ~sources ()
    in
    let functions =
      Resolution.function_definitions resolution (Access.create access)
      >>| List.length
      |> Option.value ~default:0
    in
    assert_equal functions count
  in
  assert_functions ["foo.py", "def foo(): pass\n"] "foo.foo" 1;
  assert_functions
    [
      "bar.py",
      {|
        @overload
        def bar(a: int) -> str: ...
        def bar(a: str) -> int: ...
      |};
    ]
    "bar.bar"
    2;
  assert_functions
    []
    "undefined.undefined"
    0;
  assert_functions
    [
      "foo.py",
      {|
        def foo():
          def nested(): pass
      |};
    ]
    "foo.foo.nested"
    1;
  ()


let test_solve_constraints _ =
  let resolution =
    make_resolution
      {|
      class C: ...
      class D(C): ...
      class Q: ...
      T_Unconstrained = typing.TypeVar('T_Unconstrained')
      T_Bound_C = typing.TypeVar('T_Bound_C', bound=C)
      T_Bound_D = typing.TypeVar('T_Bound_D', bound=D)
      T_Bound_Union = typing.TypeVar('T_Bound_Union', bound=typing.Union[int, str])
      T_Bound_Union_C_Q = typing.TypeVar('T_Bound_Union_C_Q', bound=typing.Union[C, Q])
      T_C_Q = typing.TypeVar('T_C_Q', C, Q)
      T_D_Q = typing.TypeVar('T_D_Q', D, Q)
      T_C_Q_int = typing.TypeVar('T_C_Q_int', C, Q, int)

      T = typing.TypeVar('T')
      class G_invariant(typing.Generic[T]):
        pass
      T_Covariant = typing.TypeVar('T_Cov', covariant=True)
      class G_covariant(typing.Generic[T_Covariant]):
        pass

      class Constructable:
        def __init__(self, x: int) -> None:
          pass
    |}
  in
  let assert_solve ~source ~target ?constraints expected =
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation resolution
    in
    let source = parse_annotation source in
    let target = parse_annotation target in
    let parse_map map =
      map
      >>| List.map ~f:(fun (key, value) -> (parse_annotation key, parse_annotation value))
      >>| Type.Map.of_alist_exn
    in
    let expected = parse_map expected in
    let constraints =
      parse_map constraints
      |> Option.value ~default:(Type.Map.empty)
    in
    let optional_map_compare left right =
      match left, right with
      | Some left, Some right -> Type.Map.equal Type.equal left right
      | None, None -> true
      | _ , _ -> false
    in
    let optional_map_print map =
      let show_line ~key ~data accumulator =
        Format.sprintf "%s \n %s -> %s" accumulator (Type.show key) (Type.show data)
      in
      map
      >>| Map.fold ~init:"" ~f:show_line
      |> Option.value ~default:"None"
    in
    assert_equal
      ~cmp:optional_map_compare
      ~printer:optional_map_print
      expected
      (Resolution.solve_constraints resolution ~constraints ~source ~target)
  in
  assert_solve ~source:"C" ~target:"T_Unconstrained" (Some ["T_Unconstrained", "C"]);
  assert_solve ~source:"D" ~target:"T_Unconstrained" (Some ["T_Unconstrained", "D"]);
  assert_solve ~source:"Q" ~target:"T_Unconstrained" (Some ["T_Unconstrained", "Q"]);

  assert_solve ~source:"C" ~target:"T_Bound_C" (Some ["T_Bound_C", "C"]);
  assert_solve ~source:"D" ~target:"T_Bound_C" (Some ["T_Bound_C", "D"]);
  assert_solve ~source:"Q" ~target:"T_Bound_C" (None);
  assert_solve ~source:"C" ~target:"T_Bound_D" (None);

  assert_solve ~source:"C" ~target:"T_C_Q" (Some ["T_C_Q", "C"]);
  (* An explicit type variable can only be bound to its constraints *)
  assert_solve ~source:"D" ~target:"T_C_Q" (Some ["T_C_Q", "C"]);
  assert_solve ~source:"C" ~target:"T_D_Q" (None);

  assert_solve ~source:"$bottom" ~target:"T_Unconstrained" (Some []);


  assert_solve
    ~source:"typing.Union[int, G_invariant[str], str]"
    ~target:"T_Unconstrained"
    (Some ["T_Unconstrained", "typing.Union[int, G_invariant[str], str]"]);
  assert_solve ~source:"typing.Union[D, C]" ~target:"T_Bound_C" (Some ["T_Bound_C", "C"]);

  assert_solve
    ~constraints:["T_Unconstrained", "Q"]
    ~source:"G_invariant[C]"
    ~target:"G_invariant[T_Unconstrained]"
    None;
  assert_solve
    ~constraints:["T_Unconstrained", "Q"]
    ~source:"G_covariant[C]"
    ~target:"G_covariant[T_Unconstrained]"
    (Some ["T_Unconstrained", "typing.Union[Q, C]"]);

  assert_solve
    ~source:"typing.Optional[C]"
    ~target:"typing.Optional[T_Unconstrained]"
    (Some ["T_Unconstrained", "C"]);
  assert_solve
    ~source:"C"
    ~target:"typing.Optional[T_Unconstrained]"
    (Some ["T_Unconstrained", "C"]);

  assert_solve
    ~source:"typing.Tuple[C, ...]"
    ~target:"typing.Tuple[T_Unconstrained, ...]"
    (Some ["T_Unconstrained", "C"]);
  assert_solve
    ~source:"typing.Tuple[C, Q, D]"
    ~target:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    (Some ["T_Unconstrained", "typing.Union[C, Q]"]);
  assert_solve
    ~source:"typing.Tuple[D, ...]"
    ~target:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    (Some ["T_Unconstrained", "D"]);
  assert_solve
    ~source:"typing.Tuple[C, Q, D]"
    ~target:"typing.Tuple[T_Unconstrained, ...]"
    (Some ["T_Unconstrained", "typing.Union[C, Q]"]);

  assert_solve
    ~source:"G_covariant[C]"
    ~target:"typing.Union[G_covariant[T_Unconstrained], int]"
    (Some ["T_Unconstrained", "C"]);

  assert_solve
    ~source:"typing.Type[int]"
    ~target:"typing.Callable[[], T_Unconstrained]"
    (Some ["T_Unconstrained", "int"]);

  assert_solve
    ~source:"typing.Optional[typing.Tuple[C, Q, typing.Callable[[D, int], C]]]"
    ~target:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    (Some ["T", "typing.Union[C, Q, int]"]);

  (* Bound => Bound *)
  assert_solve
    ~source:"T_Bound_D"
    ~target:"T_Bound_C"
    (Some ["T_Bound_C", "T_Bound_D"]);
  assert_solve
    ~source:"T_Bound_C"
    ~target:"T_Bound_D"
    None;

  (* Bound => Explicit *)
  assert_solve
    ~source:"T_Bound_D"
    ~target:"T_C_Q"
    (Some ["T_C_Q", "C"]);
  assert_solve
    ~source:"T_Bound_C"
    ~target:"T_D_Q"
    None;

  (* Explicit => Bound *)
  assert_solve
    ~source:"T_D_Q"
    ~target:"T_Bound_Union_C_Q"
    (Some ["T_Bound_Union_C_Q", "T_D_Q"]);
  assert_solve
    ~source:"T_D_Q"
    ~target:"T_Bound_D"
    None;

  (* Explicit => Explicit *)
  assert_solve
    ~source:"T_C_Q"
    ~target:"T_C_Q_int"
    (Some ["T_C_Q_int", "T_C_Q"]);
  (* This one is theoretically solvable, but only if we're willing to introduce dependent variables
     as the only sound solution here would be
     T_C_Q_int => T_new <: C if T_D_Q is D, Q if T_D_Q is Q *)
  assert_solve
    ~source:"T_D_Q"
    ~target:"T_C_Q_int"
    None;
  ()


let () =
  "resolution">:::[
    "set_local">::test_set_local;
    "parse_annotation">::test_parse_annotation;
    "resolve_literal">::test_resolve_literal;
    "resolve_mutable_literals">::test_resolve_mutable_literals;
    "function_definitions">::test_function_definitions;
    "solve_constraints">::test_solve_constraints;
  ]
  |> Test.run
