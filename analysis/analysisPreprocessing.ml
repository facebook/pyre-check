(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser
open Statement


exception PreprocessingError


let rename_shadowed_variables source =
  let module Transform = Transform.Make(struct
      type t = Identifier.Set.t

      let expression shadowing_imports { Node.location; value } =
        let value =
          match value with
          | Access ((Access.Identifier identifier) :: tail)
            when Set.mem shadowing_imports identifier ->
              let renamed =
                let renamed =
                  "$renamed_" ^ (Identifier.show identifier)
                  |> Identifier.create
                in
                Access.Identifier renamed
              in
              Access (renamed :: tail)
          | _ ->
              value
        in
        shadowing_imports, { Node.location; value }

      let statement shadowing_imports ({ Node.value; _ } as statement) =
        let shadowing_imports, value =
          match value with
          | Import { Import.from = Some ((Access.Identifier identifier) :: _); _ }
            when Identifier.show identifier <> "." ->
              Set.add shadowing_imports identifier, value
          | Define ({ Define.parameters; _ } as define) ->
              let rename_parameter
                  ({ Node.value = ({ Parameter.name; _ } as parameter); _ } as node) =
                let renamed =
                  if Set.mem shadowing_imports name then
                    "$renamed_" ^ (Identifier.show name)
                    |> Identifier.create
                  else
                    name
                in
                { node with Node.value = { parameter with Parameter.name = renamed } }
              in
              shadowing_imports,
              (Define { define with Define.parameters = List.map ~f:rename_parameter parameters })
          | _ ->
              shadowing_imports, value
        in
        shadowing_imports, [{ statement with Node.value }]
    end)
  in
  Transform.transform Identifier.Set.empty source
  |> snd


let expand_string_annotations source =
  let module Transform = Transform.Make(struct
      type t = unit

      let expression _ expression =
        (), expression

      let statement _ ({ Node.location; value } as statement) =
        let transform ({ Define.parameters; return_annotation; _ } as define) =
          let access = function
            | { Node.location; value = String string } ->
                let parsed =
                  try
                    let buffer = Lexing.from_string (string ^ "\n") in
                    let state = Lexer.State.initial () in
                    match ParserGenerator.parse (Lexer.read state) buffer with
                    | [{ Node.value = Expression { Node.value = Access access; _ } ; _ }] ->
                        access
                    | _ ->
                        raise PreprocessingError
                  with ParserGenerator.Error ->
                    raise PreprocessingError
                in
                { Node.location; value = Access parsed }
            | expression ->
                expression
          in
          let parameter ({ Node.value = ({ Parameter.annotation; _ } as parameter); _ } as node) =
            {
              node with
              Node.value = { parameter with Parameter.annotation = annotation >>| access };
            }
          in
          {
            define with
            Define.parameters = List.map ~f:parameter parameters;
            return_annotation = return_annotation >>| access;
          }
        in
        let statement =
          match value with
          | Define define ->
              Define (transform define)
              |> Node.create ~location
          | Stub (Stub.Define define) ->
              Stub (Stub.Define (transform define))
              |> Node.create ~location
          | _ ->
              statement
        in
        (), [statement]
    end)
  in
  Transform.transform () source |> snd


let qualify source =
  let qualifier = source.Source.qualifier in

  let module OrderIndependent = Transform.Make(struct
      type t = Access.t Access.Map.t

      let expression map expression =
        map, expression

      let statement map { Node.location; value } =
        let rec qualify_class qualifier ({ Class.name; body; _ } as definition) =
          let qualified_name = qualifier @ name in
          let parent = Some qualified_name in
          let qualify_in_class node =
            match node.Node.value with
            | Assign assign ->
                { node with Node.value = Assign { assign with Assign.parent }}
            | Define define ->
                { node with Node.value = Define (qualify_define ~parent [] define) }
            | Stub (Stub.Define define) ->
                { node with Node.value = Stub (Stub.Define (qualify_define ~parent [] define)) }
            | Class define ->
                { node with Node.value = Class (qualify_class qualified_name define) }
            | Stub (Stub.Class define) ->
                { node with Node.value = Stub (Stub.Class (qualify_class qualified_name define)) }
            | _ -> node
          in
          {
            definition with
            Class.name = qualified_name;
            body = List.map ~f:qualify_in_class body;
          }

        and qualify_define
            ?(parent=None)
            qualifier
            ({ Define.name; body; _ } as definition) =
          let qualified_name = qualifier @ name in
          let rec qualify_in_define node =
            match node.Node.value with
            | Define define ->
                { node with Node.value = Define (qualify_define ~parent qualified_name define) }
            | Stub (Stub.Define define) ->
                let define = qualify_define ~parent qualified_name define in
                { node with Node.value = Stub (Stub.Define define) }
            | Class define ->
                { node with Node.value = Class (qualify_class qualified_name define) }
            | Stub (Stub.Class define) ->
                { node with Node.value = Stub (Stub.Class (qualify_class qualified_name define)) }
            | For ({ For.body; For.orelse; _ } as loop) ->
                {
                  node with
                  Node.value =
                    For {
                      loop with
                      For.body = List.map ~f:qualify_in_define body;
                      For.orelse = List.map ~f:qualify_in_define orelse;
                    }
                }
            | While ({ While.body; While.orelse; _ } as loop) ->
                {
                  node with
                  Node.value =
                    While {
                      loop with
                      While.body = List.map ~f:qualify_in_define body;
                      While.orelse = List.map ~f:qualify_in_define orelse;
                    }
                }
            | If ({ If.body; If.orelse; _ } as statement) ->
                {
                  node with
                  Node.value =
                    If {
                      statement with
                      If.body = List.map ~f:qualify_in_define body;
                      If.orelse = List.map ~f:qualify_in_define orelse;
                    }
                }
            | With ({ With.body; _ } as statement) ->
                {
                  node with
                  Node.value =
                    With {
                      statement with
                      With.body = List.map ~f:qualify_in_define body;
                    }
                }
            | Try { Try.body; Try.orelse; Try.finally; Try.handlers } ->
                {
                  node with
                  Node.value =
                    Try {
                      Try.body = List.map ~f:qualify_in_define body;
                      Try.orelse = List.map ~f:qualify_in_define orelse;
                      Try.finally = List.map ~f:qualify_in_define finally;
                      Try.handlers = List.map
                          ~f:(fun handler ->
                              let handler_body =
                                List.map
                                  ~f:qualify_in_define
                                  handler.Try.handler_body
                              in
                              { handler with Try.handler_body })
                          handlers;
                    }
                }
            | _ -> node
          in
          {
            definition with
            Define.name = qualified_name;
            body = List.map ~f:qualify_in_define body;
            parent;
          }
        in

        let rec qualify_toplevel_statement map ({ Node.location; value } as statement) =
          let qualify_statements map statements =
            let add_statement (map, statements) statement =
              let map, qualified = qualify_toplevel_statement map statement in
              map, qualified :: statements
            in
            let map, reversed = List.fold ~init:(map, []) ~f:add_statement statements in
            map, List.rev reversed
          in
          match value with
          (* Add `name -> qualifier.name` for classes. *)
          | Class definition ->
              let qualified = qualify_class qualifier definition in
              Map.set map ~key:definition.Class.name ~data:qualified.Class.name,
              { Node.location; value = Class qualified }
          | Stub (Stub.Class definition) ->
              let qualified = qualify_class qualifier definition in
              Map.set map ~key:definition.Class.name ~data:qualified.Class.name,
              { Node.location; value = Stub (Stub.Class qualified) }

          (* Add `name -> qualifier.name` for functions, not methods. *)
          | Define definition when not (Define.is_method definition) ->
              let qualified = qualify_define qualifier definition in
              Map.set map ~key:definition.Define.name ~data:qualified.Define.name,
              { Node.location; value = Define qualified }
          | Stub (Stub.Define definition) when not (Define.is_method definition) ->
              let qualified = qualify_define qualifier definition in
              Map.set map ~key:definition.Define.name ~data:qualified.Define.name,
              { Node.location; value = Stub (Stub.Define qualified) }
          | If { If.test; body; orelse } ->
              let map, body = qualify_statements map body in
              let map, orelse = qualify_statements map orelse in
              map, { Node.location; value = If { If.test; body; orelse }  }
          | _ ->
              map, statement
        in
        let map, statement = qualify_toplevel_statement map { Node.location; value } in
        map, [statement]

    end)
  in

  let module OrderDependent = Transform.Make(struct
      (* Keeps track of transformations we need to make. E.g. `import a as b` will
         result in a transformation rule from `b` to `a`. *)
      type t = Access.t * Access.t Access.Map.t

      let expression (qualifier, map) expression =
        let rebased =
          match expression with
          | {
            Node.location;
            value = Access
                (Access.Call {
                    Node.value = ({
                        Call.name = { Node.value = Access access; _; };
                        _;
                      } as call);
                    _;
                  } :: tail)
          } ->
              begin
                match List.rev access with
                | name :: qualifier ->
                    let call_access =
                      Access.Call {
                        Node.location;
                        value = {
                          call with
                          Call.name = { Node.location; value = Access [name] };
                        };
                      }
                    in
                    {
                      Node.location;
                      value = Access ((List.rev qualifier) @ [call_access] @ tail);
                    }
                | _ ->
                    expression
              end
          | { Node.location; value = Access (head :: tail) } ->
              begin
                match Map.find map [head] with
                | Some replacement ->
                    {
                      Node.location;
                      value = Access (replacement @ tail);
                    }
                | None ->
                    expression
              end
          | { Node.location; value = String value } ->
              begin
                match Map.find map (Access.create value) with
                | Some replacement ->
                    {
                      Node.location;
                      value = String (Access.show replacement);
                    }
                | None ->
                    expression
              end
          | _ ->
              expression
        in
        (qualifier, map), rebased

      let statement (qualifier, map) ({ Node.location; value } as statement) =
        match value with
        | Import { Import.from = None; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `alias -> name`. *)
                  Map.set map ~key:alias ~data:name
              | None ->
                  map
            in
            (qualifier, List.fold_left imports ~f:add_import ~init:map),
            [statement]
        | Import ({ Import.from = Some from; imports } as import) ->
            (* Expand relative imports according to PEP 328 *)
            let from =
              let dots, postfix =
                let is_dot = function
                  | Access.Identifier identifier when Identifier.show identifier = "" -> true
                  | _ -> false
                in
                List.split_while ~f:is_dot from
              in
              let prefix =
                if not (List.is_empty dots) then
                  let drop =
                    if List.length dots = 2 && List.length postfix = 0 then
                      (* Special case for single `.` in from clause. *)
                      1
                    else
                      List.length dots
                  in
                  List.rev qualifier
                  |> (fun reversed -> List.drop reversed drop)
                  |> List.rev
                else
                  []
              in
              prefix @ postfix
            in
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `alias -> from.name`. *)
                  Map.set map ~key:alias ~data:(from @ name)
              | None ->
                  (* Add `name -> from.name`. *)
                  Map.set map ~key:name ~data:(from @ name)
            in
            (qualifier, List.fold_left imports ~f:add_import ~init:map),
            [{ Node.location; value = Import { import with Import.from = Some from }}]
        | _ ->
            (qualifier, map), [statement]
    end)
  in


  let map =
    let collect_globals sofar = function
      | { Node.value = Assign { Assign.target; _ }; _ }
      | { Node.value = Stub (Stub.Assign { Assign.target; _ }); _ } ->
          begin
            match target with
            | { Node.value = Access access; _ } ->
                Map.set ~key:access ~data:(qualifier @ access) sofar
            | _ ->
                sofar
          end
      | _ ->
          sofar
    in
    List.fold
      ~f:collect_globals
      ~init:Access.Map.empty
      source.Source.statements
  in

  let map, source = OrderIndependent.transform ~shallow:true map source in
  OrderDependent.transform (qualifier, map) source |> snd


let cleanup source =
  let module Cleanup = Transform.Make(struct
      type t = unit

      let expression _ expression =
        (), expression

      let statement _ ({ Node.location; value } as statement) =
        let statement =
          match value with
          | Class ({ Class.body; _ } as definition) ->
              let dequalify_attribute ({ Node.value; _ } as statement) =
                let value =
                  match value with
                  | Assign ({
                      Assign.target = ({ Node.value = Access access; _ } as target);
                      _;
                    } as assign)
                    when List.length access > 0 ->
                      let access =
                        let last =
                          List.rev access
                          |> List.hd_exn
                        in
                        match last with
                        | Access.Identifier name ->
                            let renamed =
                              Str.global_replace (Str.regexp "\\$.*_") "" (Identifier.show name)
                              |> Identifier.create
                            in
                            Access.Identifier renamed
                        | last ->
                            last
                      in
                      Assign {
                        assign with Assign.target = { target with Node.value = Access [access]};
                      }
                  | _ ->
                      value
                in
                { statement with Node.value }
              in
              {
                Node.location;
                value = Class { definition with Class.body = List.map ~f:dequalify_attribute body };
              }
          | _ ->
              statement
        in
        (), [statement]
    end)
  in
  Cleanup.transform () source |> snd


let replace_version_specific_code source =
  let module Transform = Transform.Make(struct
      type t = unit

      let expression _ expression =
        (), expression

      let statement _ ({ Node.location; value } as statement) =
        match value with
        | If { If.test; body; orelse } ->
            (* Normalizes a comparison of a < b, a <= b, b >= a or b > a to Some (a, b). *)
            let extract_single_comparison { Node.value; _ } =
              match value with
              | Expression.ComparisonOperator {
                  Expression.ComparisonOperator.left;
                  right = [
                    operator,
                    right
                  ];
                } ->
                  begin
                    match operator with
                    | Expression.ComparisonOperator.LessThan
                    | Expression.ComparisonOperator.LessThanOrEquals ->
                        Some (left, right)

                    | Expression.ComparisonOperator.GreaterThan
                    | Expression.ComparisonOperator.GreaterThanOrEquals ->
                        Some (right, left)

                    | _ ->
                        None
                  end
              | _ -> None
            in
            begin
              match extract_single_comparison test with
              | Some (left, { Node.value = Expression.Tuple ({ Node.value = major; _ } :: _); _ })
                when Expression.show left = "sys.version_info" && major = Expression.Integer 3 ->
                  (),
                  if List.is_empty orelse then
                    [Node.create ~location Statement.Pass]
                  else
                    orelse
              | Some ({ Node.value = Expression.Tuple ({ Node.value = major; _ } :: _); _ }, right)
                when Expression.show right = "sys.version_info" && major = Expression.Integer 3 ->
                  (),
                  if List.is_empty body then
                    [Node.create ~location Statement.Pass]
                  else
                    body
              | _ ->
                  (), [statement]
            end
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source |> snd


(* TODO(T22862979) Our parser currently parses {""} as Dictionary(kwarg = "").
   The real solution is to fix parsing of singleton dictionaries. *)
let fix_singleton_sets source =
  let module Transform = Transform.Make(struct
      type t = unit

      let expression _ ({ Node.location; value } as expression) =
        match value with
        | Dictionary {
            Dictionary.entries = [];
            keywords = Some { Node.location = keyword_location; value = keyword }
          }
          ->
            begin
              match keyword with
              | Starred _ ->
                  (), expression
              | _ ->
                  (), {
                    Node.location;
                    value = Set [{ Node.location = keyword_location; value = keyword}];
                  }
            end

        | _ ->
            (), expression

      let statement _ statement = (), [statement]
    end)
  in
  Transform.transform () source |> snd


(* TODO(T22866412) Find a more general way of dealing with this problem.
   This hack ensures that assertions from if tests get propagated even if
   there is no explicit else: in the code. *)
let expand_optional_assigns source =
  let module Transform = Transform.Make(struct
      type t = unit

      let expression _ expression = (), expression

      let statement _ { Node.location; value } =
        match value with
        | If { If.test; body; orelse = [] } ->
            (), [{
                Node.location;
                value = If { If.test; body; orelse = [{ Node.location; value = Pass }] };
              }]
        | _ ->
            (), [{ Node.location; value }]
    end)
  in
  Transform.transform () source |> snd


let expand_operators source =
  let module Transform = Transform.Make(struct
      type t = unit

      let expression _ { Node.location; value } =
        let value =
          match value with
          | BinaryOperator operator ->
              BinaryOperator.override operator
              |> Node.value
          | _ -> value
        in
        (), { Node.location; value }

      let statement _ statement =
        (), [statement]
    end)
  in
  Transform.transform () source |> snd


let return_access = Access.create "$return"


let expand_returns source =
  let module ExpandingTransform = Transform.Make(struct
      type t = unit

      let expression state expression =
        state, expression

      let statement state statement =
        match statement with
        (* Expand returns to make them more amenable for analyses. E.g:
           `return x` -> `$return = x; return $return` *)
        | { Node.location; value = Return (Some value) } ->
            let target = { Node.location; value = Access return_access } in
            state,
            [
              {
                Node.location;
                value = Assign {
                    Assign.target;
                    annotation = None;
                    value = Some value;
                    compound = None;
                    parent = None;
                  };
              };
              { Node.location; value = Return (Some target) };
            ]

        (* Insert implicit return statements at the end of function bodies. *)
        | { Node.location; value = Define define } ->
            let define =
              let has_yield =
                let module Visit = Visit.Make(struct
                    type t = bool

                    let expression sofar _ =
                      sofar

                    let statement sofar = function
                      | { Node.value = Statement.Yield _; _ } -> true
                      | { Node.value = Statement.YieldFrom _; _ } -> true
                      | _ -> sofar
                  end)
                in
                Visit.visit false (Source.create define.Define.body)
              in
              let has_return_in_finally =
                match List.last define.Define.body with
                | Some { Node.value = Try { Try.finally; _ }; _ } ->
                    begin
                      match List.last finally with
                      | Some { Node.value = Return _; _ } ->
                          true
                      | _ ->
                          false
                    end
                | _ ->
                    false
              in
              let loops_forever =
                match List.last define.Define.body with
                | Some { Node.value = While { While.test = { Node.value = True; _ }; _ }; _ } ->
                    true
                | _ ->
                    false
              in
              if has_yield || has_return_in_finally || loops_forever then
                define
              else
                match List.last define.Define.body with
                | Some { Node.value = Return _; _ } ->
                    define
                | Some statement ->
                    {
                      define with
                      Define.body = define.Define.body @ [{
                          Node.location = statement.Node.location;
                          value = Return None;
                        }];
                    }
                | _ ->
                    define
            in
            state,
            [{ Node.location; value = Define define }]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_yield_from source =
  let module NormalizingTransform = Transform.Make(struct
      type t = unit

      let expression state expression =
        state, expression

      let statement state statement =
        match statement with
        | { Node.location;
            value = YieldFrom ({
                location = expression_location;
                Node.value = Expression.Yield (Some yield);
              })
          } ->
            let call name =
              Access.Call
                (Node.create_with_default_location
                   {
                     Call.name = Node.create_with_default_location (Access (Access.create name));
                     arguments = [];
                   })
            in
            let add_call = [Access.Expression yield; call "__iter__"] in
            state,
            [{ Node.location;
               value = YieldFrom {
                   Node.location = expression_location;
                   value =
                     Expression.Yield (Some (Node.create_with_default_location (Access add_call)))
                 }
             }]

        | _ ->
            state, [statement]
    end)
  in
  NormalizingTransform.transform () source
  |> snd

let expand_for_loop source =
  let module ExpandingTransform = Transform.Make(struct
      type t = unit

      let expression state expression =
        state, expression

      let statement state statement =
        match statement with
        | {
          Node.location;
          value = For ({
              For.target;
              iterator = { Node.value; _ };
              body;
              async;
              _;
            } as loop);
        } ->
            let body =
              let assignment =
                let value =
                  let next =
                    let call name =
                      Access.Call
                        (Node.create_with_default_location
                           {
                             Call.name =
                               Node.create_with_default_location (Access (Access.create name));
                             arguments = [];
                           })
                    in
                    if async then
                      [call "__aiter__"; call "__anext__"]
                    else
                      [call "__iter__"; call "__next__"]
                  in
                  begin
                    match value with
                    | Access access ->
                        access @ next
                    | expression ->
                        [Access.Expression (Node.create_with_default_location expression)] @ next
                  end
                in
                {
                  Node.location;
                  value = Assign {
                      Assign.target;
                      annotation = None;
                      value = Some {
                          Node.location;
                          value = Access value;
                        };
                      compound = None;
                      parent = None;
                    }
                }
              in
              assignment :: body
            in
            state, [{ Node.location; value = For { loop with For.body } }]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_excepts source =
  let module ExpandingTransform = Transform.Make(struct
      type t = unit

      let expression state expression =
        state, expression

      let statement state statement =
        match statement with
        | {
          Node.location;
          value = Try ({ Try.handlers; _ } as except)
        } ->
            let handlers =
              let transform_handler ({ Try.kind; name; handler_body } as handler) =
                let name = name >>| fun name -> Access.create (Identifier.show name) in
                let assume ~target ~annotation =
                  {
                    Node.location;
                    value = Statement.Assign {
                        Assign.target;
                        annotation = Some annotation;
                        value = None;
                        compound = None;
                        parent = None;
                      }
                  }
                in
                match kind, name with
                | Some ({ Node.location; value = Access _; _ } as annotation), Some name ->
                    let assume =
                      assume ~target:{ Node.location; value = Access name } ~annotation
                    in
                    { handler with Try.handler_body = assume :: handler_body }
                | Some { Node.location; value = Tuple values; _ }, Some name ->
                    let assume =
                      let annotation: Expression.t =
                        let subscript =
                          let index value = Access.Index value in
                          [Access.Subscript (List.map ~f:index values)]
                        in
                        {
                          Node.location;
                          value = Access ((Access.create "typing.Union") @ subscript);
                        }
                      in
                      assume ~target:{ Node.location; value = Access name } ~annotation
                    in
                    { handler with Try.handler_body = assume :: handler_body }
                | _ ->
                    let assume =
                      kind
                      >>| (fun kind -> [Node.create ~location (Statement.Expression kind)])
                      |> Option.value ~default:[]
                    in
                    { handler with Try.handler_body = assume @ handler_body }
              in
              List.map ~f:transform_handler handlers
            in
            state, [{ Node.location; value = Try { except with Try.handlers }}]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_ternary_assign source =
  let module ExpandingTransform = Transform.Make(struct
      type t = unit

      let expression state expression =
        state, expression

      let statement state statement =
        match statement with
        | {
          Node.location;
          value = Assign ({
              Assign.value =
                Some { Node.value = Ternary { Ternary.target; test; alternative }; _ };
              _;
            } as assign)
        } ->
            state,
            [
              {
                Node.location;
                value = If {
                    If.test;
                    body = [
                      {
                        Node.location;
                        value = Assign { assign with Assign.value = Some target };
                      }
                    ];
                    orelse = [
                      {
                        Node.location;
                        value = Assign { assign with Assign.value = Some alternative };
                      }
                    ];
                  };
              };
            ]
        | _ ->
            state, [statement]
    end)
  in
  ExpandingTransform.transform () source
  |> snd


let expand_named_tuples ({ Source.statements; _ } as source) =
  let expand_named_tuples = function
    | {
      Node.location;
      value = Assign {
          Assign.target = {
            Node.value = Access name;
            _;
          };
          value = Some ({
              Node.value =
                Access [
                  Access.Identifier typing;
                  Access.Call {
                    Node.value = {
                      Call.name = {
                        Node.value = Access [Access.Identifier named_tuple];
                        _;
                      };
                      _;
                    };
                    _;
                  }
                ];
              _;
            } as tuple);
          _;
        };
    } when Identifier.show typing = "typing" && Identifier.show named_tuple = "NamedTuple" ->
        let definition =
          {
            Class.name;
            bases = [{ Argument.name = None; value = tuple }];
            body = [Node.create_with_default_location Pass];
            decorators = [];
            docstring = None;
          }
        in
        { Node.location; value = Class definition }
    | statement ->
        statement
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }


let simplify_access_chains source =
  let module SimplifyAccessChains = Transform.Make(struct
      type t = unit

      let count = ref 0

      let expression state expression =
        state, expression

      let statement state statement =
        (* simplify_access_chain breaks down a chain of calls in an access of the form a().b().c to
           [$3 = $2.c; $2 = $1.b(); $1 = a()]. This function creates temporary variables in order to
           build later assignments. The statements are returned in reverse order to make
           postprocessing the last assignment easy.
        *)
        let simplify_access_chain location access =
          let assign_to_temporary ~previous access =
            let target =
              count := !count + 1;
              Expression.Access.Identifier (Identifier.create (Format.sprintf "$%d" !count))
            in
            let access =
              match previous with
              | None ->
                  access
              | Some target ->
                  target :: access
            in
            target,
            {
              Node.location;
              value = Assign {
                  Assign.value = Some (Node.create ~location (Expression.Access access));
                  target = Node.create ~location (Expression.Access [target]);
                  annotation = None;
                  compound = None;
                  parent = None;
                }
            }
          in
          let fold (previous, statements, current_call) last =
            match last with
            | Expression.Access.Call _ ->
                let next_target, call =
                  assign_to_temporary ~previous (List.rev (last :: current_call))
                in
                Some next_target, call :: statements, []
            | _ ->
                previous, statements, (last :: current_call)
          in
          match List.fold ~init:(None, [], []) ~f:fold access with
          | _, statements, [] ->
              statements
          | previous, statements, incomplete ->
              let _, assign = assign_to_temporary ~previous (List.rev incomplete) in
              (assign :: statements)
        in
        match statement with
        | {
          Node.location;
          value = Assign (
              { Assign.value = Some { Node.value = Access access; _ }; target; _ } as assign);
        } ->
            begin
              match simplify_access_chain location access with
              | [] | [_] ->
                  state, [statement]
              | { Node.location; value = Assign { Assign.value = assign_value; _ } }
                :: assignments ->
                  (* Postprocess the last element to assign to the initial target. *)
                  state, List.rev (
                    {
                      Node.location;
                      value = Assign { assign with Assign.target; value = assign_value };
                    }
                    :: assignments)
              | _ ->
                  raise PreprocessingError
            end
        | {
          Node.location;
          value = Expression { Node.value = Expression.Access access; _ };
        } ->
            begin match simplify_access_chain location access with
              | [] | [_] ->
                  state, [statement]
              | { Node.location; value = Assign { Assign.value = Some call; _ } } :: assignments ->
                  state, List.rev ({ Node.location; value = Expression call } :: assignments)
              | _ ->
                  raise PreprocessingError
            end
        | _ ->
            state, [statement]
    end)
  in
  SimplifyAccessChains.transform () source
  |> snd


let defines ({ Source.statements; _ } as source) =
  let toplevel =
    Node.create_with_default_location (Statement.Define.create_toplevel statements)
  in
  let module Collector = Visit.StatementCollector(struct
      type t = Define.t Node.t
      let predicate = function
        | { Node.location; value = Define define } ->
            Some ({ Node.location; Node.value = define })
        | _ ->
            None
    end)
  in
  toplevel :: (Collector.collect source)


let classes source =
  let module Collector = Visit.StatementCollector(struct
      type t = Statement.Class.t Node.t
      let predicate = function
        | { Node.location; value = Class class_define } ->
            Some ({ Node.location; Node.value = class_define })
        | _ ->
            None
    end)
  in
  Collector.collect source


let dequalify_map source =
  let module ImportDequalifier = Transform.Make(struct
      type t = Access.t Access.Map.t

      let expression map expression =
        map, expression

      let statement map ({ Node.value; _ } as statement) =
        match value with
        | Import { Import.from = None; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `name -> alias`. *)
                  Map.set map ~key:(List.rev name) ~data:alias
              | None ->
                  map
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | Import { Import.from = Some from; imports } ->
            let add_import map { Import.name; alias } =
              match alias with
              | Some alias ->
                  (* Add `alias -> from.name`. *)
                  Map.set map ~key:(List.rev (from @ name)) ~data:alias
              | None ->
                  (* Add `name -> from.name`. *)
                  Map.set map ~key:(List.rev (from @ name)) ~data:name
            in
            List.fold_left imports ~f:add_import ~init:map,
            [statement]
        | _ ->
            map, [statement]
    end)
  in
  (* Note that map keys are reversed accesses because it makes life much easier in dequalify *)
  let map =
    Map.set ~key:(List.rev source.Source.qualifier) ~data:[] Access.Map.empty
  in
  ImportDequalifier.transform map source |> fst


let preprocess source =
  source
  |> expand_string_annotations
  |> replace_version_specific_code
  |> rename_shadowed_variables
  |> qualify
  |> cleanup
  |> fix_singleton_sets
  |> expand_optional_assigns
  |> expand_operators
  |> expand_returns
  |> expand_for_loop
  |> expand_yield_from
  |> simplify_access_chains
  |> expand_ternary_assign
  |> expand_named_tuples
  |> expand_excepts
