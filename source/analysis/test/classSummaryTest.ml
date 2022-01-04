(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open OUnit2
open Analysis
open Test
open ClassSummary
open Ast
open Statement

let test_attributes _ =
  let create_attribute
      ~annotation
      ?(frozen = false)
      ?(implicit = true)
      ?(location = Location.any)
      ~name
      ?(primitive = false)
      ?(toplevel = true)
      ~value
      ~origin
      ()
    =
    let values = value >>| (fun value -> { Attribute.value; origin }) |> Option.to_list in
    {
      Attribute.kind =
        Simple { annotation; frozen; implicit; primitive; toplevel; values; nested_class = false };
      name;
    }
    |> Node.create ~location
  in
  (* Test define field assigns. *)
  let assert_assigned_by_define source expected =
    let expected =
      let attribute (name, annotation, value, toplevel) =
        create_attribute
          ~name
          ~annotation:(annotation >>| Type.expression)
          ~value:(value >>| parse_single_expression)
          ~toplevel
          ~primitive:true
          ~origin:Implicit
          ()
      in
      List.map expected ~f:attribute
    in
    let definition =
      { Class.name = + !&""; bases = []; body = []; decorators = []; top_level_unbound_names = [] }
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> Attribute.location_insensitive_compare left right = 0))
      ~printer:(fun attributes -> List.map ~f:Attribute.show attributes |> String.concat ~sep:"\n")
      expected
      (parse_single_define source
      |> ClassAttributes.Private.assigned_by_define ~definition
      |> Identifier.SerializableMap.bindings
      |> List.map ~f:snd)
  in
  assert_assigned_by_define "def foo(): pass" [];
  assert_assigned_by_define
    {|
      def foo():
        self.attribute = value
        self.attribute: int = value
    |}
    ["attribute", Some Type.integer, Some "value", true];
  assert_assigned_by_define
    {|
      def foo():
        self.attribute: int = value
        self.attribute = value
    |}
    ["attribute", Some Type.integer, Some "value", true];
  assert_assigned_by_define
    {|
      def foo():
        self.attribute: int = value
        self.attribute: str = value
    |}
    ["attribute", Some (Type.Union [Type.string; Type.integer]), Some "value", true];
  assert_assigned_by_define
    {|
      def foo():
        self.attribute, self.other = derp()
    |}
    ["attribute", None, Some "derp()", true; "other", None, Some "derp()", true];
  assert_assigned_by_define
    {|
      def foo(self, derp: int):
        self.attribute, self.other = derp
    |}
    ["attribute", None, Some "derp", true; "other", None, Some "derp", true];
  assert_assigned_by_define
    {|
      def foo(self, argument: str):
        self.attribute = argument
    |}
    ["attribute", Some Type.string, Some "argument", true];
  assert_assigned_by_define
    {|
      def foo(self, attribute: str):
        self.attribute = attribute
    |}
    ["attribute", Some Type.string, Some "attribute", true];
  assert_assigned_by_define
    {|
      def foo(self, attribute: MyType):
        self.attribute = attribute
    |}
    ["attribute", Some (Type.Primitive "MyType"), Some "attribute", true];
  assert_assigned_by_define
    {|
      def foo(self, attribute: str, test: bool):
        if test:
          self.attribute = attribute
    |}
    ["attribute", None, Some "attribute", false];
  assert_assigned_by_define
    {|
      def foo(self, argument: str):
        self.argument = unknown
    |}
    ["argument", None, Some "unknown", true];

  (* Implicit arguments in branches. *)
  assert_assigned_by_define
    {|
      def foo():
        a = 1
        self.attribute = value
        if True:
          self.other = value
          if False:
            self.nested = value
    |}
    [
      "attribute", None, Some "value", true;
      "nested", None, Some "value", false;
      "other", None, Some "value", false;
    ];

  (* `self` isn't special cased if a self parameter is passed into the function. *)
  assert_assigned_by_define
    {|
    def foo(renamed_self):
      renamed_self.attribute: int = value
  |}
    ["attribute", Some Type.integer, Some "value", true];

  (* Test class attributes. *)
  let assert_attributes ?(in_test = false) ?(include_generated_attributes = true) source expected =
    let expected =
      let attribute
          ( name,
            location,
            annotation,
            values,
            setter,
            number_of_defines,
            property,
            class_property,
            nested_class )
        =
        let location =
          match location with
          | None -> None
          | Some ((start_line, start_column), (stop_line, stop_column)) ->
              Some
                {
                  Location.start = { Location.line = start_line; column = start_column };
                  stop = { Location.line = stop_line; column = stop_column };
                }
        in
        let kind =
          if number_of_defines > 0 then
            let define =
              {
                Define.Signature.name = + !&"foo";
                parameters = [];
                decorators = [];
                return_annotation = Some !"int";
                async = false;
                generator = false;
                parent = None;
                nesting_define = None;
              }
            in
            let signatures = List.init ~f:(fun _ -> define) number_of_defines in
            Attribute.Method { signatures; static = false; final = false }
          else if property then
            let kind =
              if setter then
                Attribute.ReadWrite
                  {
                    getter =
                      { self = None; return = List.hd values >>| fst >>| parse_single_expression };
                    setter = { self = None; value = annotation >>| Type.expression };
                  }
              else
                Attribute.ReadOnly
                  { getter = { self = None; return = annotation >>| Type.expression } }
            in
            Attribute.Property { kind; async = false; class_property }
          else
            Attribute.Simple
              {
                annotation = annotation >>| Type.expression;
                primitive = true;
                values =
                  List.map values ~f:(fun (value, origin) ->
                      { Attribute.value = parse_single_expression value; origin });
                frozen = false;
                toplevel = true;
                implicit = false;
                nested_class;
              }
        in
        { Attribute.kind; name }
        |> Node.create ~location:(Option.value location ~default:Location.any)
      in
      List.map expected ~f:attribute
    in
    let printer attributes = List.map attributes ~f:Attribute.show |> String.concat ~sep:"\n\n" in
    let equal
        { Node.value = { Attribute.kind = left; name = left_name }; location = left_location }
        { Node.value = { Attribute.kind = right; name = right_name }; location = right_location }
      =
      let open Attribute in
      let equal_kind =
        match left, right with
        | Simple left, Simple right ->
            let expression_equal left right =
              Expression.location_insensitive_compare left right = 0
            in
            let origin_and_value_equal left right =
              equal_origin left.origin right.origin && expression_equal left.value right.value
            in
            Option.equal expression_equal left.annotation right.annotation
            && List.equal origin_and_value_equal left.values right.values
            && Bool.equal left.nested_class right.nested_class
        | Method left, Method right ->
            Int.equal (left.signatures |> List.length) (right.signatures |> List.length)
        | _ -> Attribute.location_insensitive_compare_kind left right = 0
      in
      equal_kind
      && String.equal left_name right_name
      && (Location.equal left_location Location.any || Location.equal left_location right_location)
    in
    assert_equal
      ~cmp:(List.equal equal)
      ~printer
      expected
      (parse_single_class source
      |> ClassAttributes.create
      |> ClassAttributes.attributes ~in_test ~include_generated_attributes
      |> Identifier.SerializableMap.bindings
      |> List.map ~f:snd)
  in
  let attribute
      ~name
      ?location
      ?annotation
      ?(values = [])
      ?(number_of_defines = 0)
      ?(property = false)
      ?(setter = false)
      ?(class_property = false)
      ?(nested_class = false)
      ()
    =
    ( name,
      location,
      annotation,
      values,
      setter,
      number_of_defines,
      property,
      class_property,
      nested_class )
  in
  assert_attributes
    {|
      class Foo:
        Foo.attribute: int
        def __init__(self) -> None:
          self.attribute = value
    |}
    [
      attribute
        ~name:"attribute"
        ~annotation:Type.integer
        ~values:["...", Attribute.Explicit; "value", Attribute.Implicit]
        ();
    ];
  assert_attributes
    {|
      class Foo:
        Foo.attribute: int = value
    |}
    [attribute ~name:"attribute" ~annotation:Type.integer ~values:["value", Attribute.Explicit] ()];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.implicit = implicit
          self.whatever()['asdf'] = 5
          if True:
            self.ignored = ignored
        Foo.attribute: int = value
        whatever()['asdf'] = 5
    |}
    [
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute ~name:"attribute" ~annotation:Type.integer ~values:["value", Attribute.Explicit] ();
      attribute ~name:"ignored" ~values:["ignored", Attribute.Implicit] ();
      attribute ~name:"implicit" ~values:["implicit", Attribute.Implicit] ();
    ];
  assert_attributes
    {|
      class Foo:
        @overload
        def Foo.f(self, x: int) -> int: ...
        def Foo.f(self, x: str) -> str: ...
    |}
    [attribute ~name:"f" ~number_of_defines:2 ()];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.attribute = value  # Prioritize explicit declaration
        Foo.attribute: int = value
    |}
    [
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute
        ~name:"attribute"
        ~annotation:Type.integer
        ~values:["value", Attribute.Explicit; "value", Implicit]
        ();
    ];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.init()
        def Foo.init(self):
          self.attribute: int = value
        def Foo.not_inlined(self):
          self.other: int = 1
    |}
    [
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute ~name:"attribute" ~annotation:Type.integer ~values:["value", Attribute.Implicit] ();
      attribute ~name:"init" ~number_of_defines:1 ();
      attribute ~name:"not_inlined" ~number_of_defines:1 ();
    ];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int:
          pass
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ()];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int: ...
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ()];
  assert_attributes
    {|
      class Foo:
        class Foo.Bar:  # no preprocessing in tests
          ...
    |}
    [
      attribute
        ~name:"Bar"
        ~annotation:(Type.class_variable (Type.meta (Type.Primitive "Foo.Bar")))
        ~nested_class:true
        ();
    ];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.x(self) -> int: ...
        @x.setter
        def Foo.x(self, value:str) -> None: ...
    |}
    [
      attribute
        ~name:"x"
        ~annotation:Type.string
        ~values:["int", Attribute.Implicit]
        ~property:true
        ~setter:true
        ();
    ];

  assert_attributes
    {|
      class Foo:
        @pyre_extensions.classproperty
        def Foo.property(self) -> int: ...
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ~class_property:true ()];

  (* Simultaneous assignment *)
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = 1, 2
     |}
    [
      "a", None, None, ["1", Attribute.Explicit], false, 0, false, false, false;
      "b", None, None, ["2", Attribute.Explicit], false, 0, false, false, false;
    ];
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = list(range(2))
    |}
    [
      "a", None, None, ["list(range(2))[0]", Attribute.Explicit], false, 0, false, false, false;
      "b", None, None, ["list(range(2))[1]", Attribute.Explicit], false, 0, false, false, false;
    ];

  (* Implicit attributes in tests. *)
  assert_attributes
    ~in_test:true
    {|
      class Test:
        def Test.setUp(self):
          self.attribute = value
    |}
    [
      attribute ~name:"attribute" ~values:["value", Attribute.Implicit] ();
      attribute ~name:"setUp" ~number_of_defines:1 ();
    ];
  assert_attributes
    ~in_test:true
    {|
      class Test:
        def Test.setUp(self):
          self.attribute = value
        def Test.with_context(self):
          self.context = value
    |}
    [
      attribute ~name:"attribute" ~values:["value", Attribute.Implicit] ();
      attribute ~name:"context" ~values:["value", Attribute.Implicit] ();
      attribute ~name:"setUp" ~number_of_defines:1 ();
      attribute ~name:"with_context" ~number_of_defines:1 ();
    ];
  assert_attributes
    ~in_test:true
    {|
      class Test:
        def Test.setUpClass(self):
          self.attribute = value
    |}
    [
      attribute ~name:"attribute" ~values:["value", Attribute.Implicit] ();
      attribute ~name:"setUpClass" ~number_of_defines:1 ();
    ];

  (* __slot__ attributes *)
  assert_attributes
    {|
      class Foo:
        __slots__ = ['attribute']
    |}
    [attribute ~name:"attribute" ()];
  assert_attributes
    {|
      class Foo:
        __slots__ = ['name', 'identifier']
    |}
    [attribute ~name:"identifier" (); attribute ~name:"name" ()];
  assert_attributes
    {|
      class Foo:
        Foo.x, Foo.y = 1, 2
    |}
    [
      attribute ~location:((3, 2), (3, 7)) ~name:"x" ~values:["1", Attribute.Explicit] ();
      attribute ~location:((3, 9), (3, 14)) ~name:"y" ~values:["2", Attribute.Explicit] ();
    ]


let test_is_final _ =
  let decorator ~name =
    { Decorator.name = Node.create_with_default_location (Reference.create name); arguments = None }
  in
  let class_summary ~decorators =
    {
      name = Reference.create "foo";
      qualifier = Reference.create "bar";
      bases = [];
      decorators;
      attribute_components = ClassAttributes.empty ();
    }
  in
  assert_false (ClassSummary.is_final (class_summary ~decorators:[]));
  assert_true (ClassSummary.is_final (class_summary ~decorators:[decorator ~name:"typing.final"]));
  assert_true
    (ClassSummary.is_final (class_summary ~decorators:[decorator ~name:"typing_extensions.final"]));
  ()


let () =
  "classSummary" >::: ["attributes" >:: test_attributes; "is_final" >:: test_is_final] |> Test.run
