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


let expand_string_annotations source =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.location; value } as statement) =
        let transform ({ Define.parameters; return_annotation; _ } as define) =
          let access = function
            | { Node.location; value = String string } ->
                let parsed =
                  try
                    let buffer = Lexing.from_string (string ^ "\n") in
                    let state = Lexer.State.initial () in
                    match ParserGenerator.parse (Lexer.read state) buffer with
                    | [{ Node.value = Expression { Node.value = Access access; _ } ; _ }] ->
                        Some access
                    | _ ->
                        raise ParserGenerator.Error
                  with
                  | ParserGenerator.Error
                  | Failure _ ->
                      begin
                        Log.debug
                          "Invalid string annotation `%s` at %a"
                          string
                          Location.pp
                          location;
                        None
                      end
                in
                parsed >>| (fun parsed -> { Node.location; value = Access parsed })
            | expression ->
                Some expression
          in
          let parameter ({ Node.value = ({ Parameter.annotation; _ } as parameter); _ } as node) =
            {
              node with
              Node.value = { parameter with Parameter.annotation = annotation >>= access };
            }
          in
          {
            define with
            Define.parameters = List.map ~f:parameter parameters;
            return_annotation = return_annotation >>= access;
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


type global_entities = {
  variables: Access.t Access.Map.t;
  methods: Access.t Access.Map.t;
}


type define_qualifier_state = {
  local_qualifier: Access.t;
  current_scope: Access.t Access.Map.t;
  globals: Access.t Access.Map.t;
}


let qualify source =
  let global_qualifier = source.Source.qualifier in
  let rename_named_arguments access =
    let rename = function
      | Access.Call { Node.value = { Call.arguments; name }; location } ->
          let rename_argument { Argument.name; value } =
            {
              Argument.name = name >>| Identifier.add_prefix ~prefix:"$renamed_";
              value;
            }
          in
          Access.Call
            (Node.create ~location { Call.arguments = List.map ~f:rename_argument arguments; name })
      | access -> access
    in
    List.map ~f:rename access
  in

  let module QualifyToplevelStatements = Transform.Make(struct
      include Transform.Identity
      type t = global_entities

      let expression_postorder ({ variables; _ } as state) expression =
        let expression =
          match expression with
          | { Node.location; value = Access access } ->
              let access = rename_named_arguments access in
              Map.find variables access
              >>| (fun replacement -> { Node.location; value = Access replacement })
              |> Option.value ~default:(Node.create ~location (Access access))
          | _ ->
              expression
        in
        state, expression

      let statement_keep_recursing _ { Node.value; _ } =
        (* We are qualifying only top-level statements only, hence do not recurse into children
           nodes. Top-level expressions are the exception, variables therein need to be fully
           qualified. *)
        match value with
        | Expression _ -> Transform.Recurse
        | _ -> Transform.Stop

      let statement_postorder ({ variables; _ } as state) ({ Node.value; _ } as statement) =
        let rec qualify_class qualifier ({ Class.name; bases; body; _ } as definition) =
          let qualified_name = qualifier @ name in
          let parent = Some qualified_name in
          let qualify_bases
              ({ Argument.value = ({ Node.value; _ } as expression); _ } as argument) =
            let expression =
              match value with
              | Access access ->
                  let access =
                    Map.find variables access
                    |> Option.value ~default:access
                  in
                  { expression with Node.value = Access access }
              | _ ->
                  expression
            in
            { argument with Argument.value = expression }
          in
          let qualify_in_class ({ Node.value; _ } as statement) =
            let value =
              match value with
              | Assign assign ->
                  Assign { assign with Assign.parent }
              | Define define ->
                  Define (qualify_define ~parent [] define)
              | Stub (Stub.Define define) ->
                  Stub (Stub.Define (qualify_define ~parent [] define))
              | Class define ->
                  Class (qualify_class qualified_name define)
              | Stub (Stub.Class define) ->
                  Stub (Stub.Class (qualify_class qualified_name define))
              | _ ->
                  value
            in
            { statement with Node.value }
          in
          {
            definition with
            Class.name = qualified_name;
            bases = List.map ~f:qualify_bases bases;
            body = List.map ~f:qualify_in_class body;
          }

        and qualify_define
            ?(parent=None)
            qualifier
            ({ Define.name; body; _ } as define) =
          let qualified_name = qualifier @ name in
          let rec qualify_in_define ({ Node.value; _ } as statement) =
            let qualify_statements = List.map ~f:qualify_in_define in
            let value =
              match value with
              | Define define ->
                  Define (qualify_define ~parent qualified_name define)
              | Stub (Stub.Define define) ->
                  Stub (Stub.Define (qualify_define ~parent qualified_name define))
              | Class define ->
                  Class (qualify_class qualified_name define)
              | Stub (Stub.Class define) ->
                  Stub (Stub.Class (qualify_class qualified_name define))
              | For ({ For.body; orelse; _ } as loop) ->
                  For {
                    loop with
                    For.body = qualify_statements body;
                    orelse = qualify_statements orelse;
                  }
              | While ({ While.body; While.orelse; _ } as loop) ->
                  While {
                    loop with
                    While.body = qualify_statements body;
                    orelse = qualify_statements orelse;
                  }
              | If ({ If.body; If.orelse; _ } as statement) ->
                  If {
                    statement with
                    If.body = qualify_statements body;
                    orelse = qualify_statements orelse;
                  }
              | With ({ With.body; _ } as statement) ->
                  With {
                    statement with
                    With.body = qualify_statements body;
                  }
              | Try { Try.body; orelse; finally; handlers } ->
                  let qualify_handler ({ Try.handler_body; _ } as handler) =
                    { handler with Try.handler_body = qualify_statements handler_body }
                  in
                  Try {
                    Try.body = qualify_statements body;
                    orelse = qualify_statements orelse;
                    finally = qualify_statements finally;
                    handlers = List.map ~f:qualify_handler handlers;
                  }
              | _ ->
                  value
            in
            { statement with Node.value }
          in
          {
            define with
            Define.name = qualified_name;
            body = List.map ~f:qualify_in_define body;
            parent;
          }
        in

        let rec qualify_toplevel_statement
            ({ variables; methods } as state)
            ({ Node.value; _ } as statement) =
          let qualify_statements { variables; methods } statements =
            let add_statement (variables, methods, statements) statement =
              let { variables; methods }, qualified =
                qualify_toplevel_statement
                  { variables; methods }
                  statement
              in
              variables, methods, qualified :: statements
            in
            let variables, methods, reversed =
              List.fold
                ~init:(variables, methods, [])
                ~f:add_statement
                statements
            in
            { variables; methods }, List.rev reversed
          in
          let state, value =
            match value with
            (* Add `name -> qualifier.name` for classes. *)
            | Class definition ->
                let qualified = qualify_class global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Class.name ~data:qualified.Class.name;
                },
                Class qualified
            | Stub (Stub.Class definition) ->
                let qualified = qualify_class global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Class.name ~data:qualified.Class.name;
                },
                Stub (Stub.Class qualified)

            (* Add `name -> qualifier.name` for functions, not methods. *)
            | Define definition when not (Define.is_method definition) ->
                let qualified = qualify_define global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Define.name ~data:qualified.Define.name;
                },
                Define qualified
            | Stub (Stub.Define definition) when not (Define.is_method definition) ->
                let qualified = qualify_define global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Define.name ~data:qualified.Define.name;
                },
                Stub (Stub.Define qualified)
            | If { If.test; body; orelse } ->
                let state, body = qualify_statements state body in
                let state, orelse = qualify_statements state orelse in
                state, If { If.test; body; orelse }

            (* Qualify globals *)
            | Assign (
                {
                  Assign.target = ({ Node.value = Access access; _ } as access_node);
                  value;
                  _;
                } as assign) ->
                let qualified = global_qualifier @ access in
                let qualified_access = { access_node with Node.value = Access qualified } in
                (* All assigned targets are variables by default,
                   unless we have already classified their values as methods. *)
                let state =
                  match value with
                  | Some { Node.value = Access value_access; _ }
                    when Map.mem methods value_access ->
                      { state with methods = Map.set methods ~key:access ~data:qualified }
                  | _ ->
                      { state with variables = Map.set variables ~key:access ~data:qualified }
                in
                state,
                Assign { assign with Assign.target = qualified_access }
            | Stub (
                Stub.Assign ({
                    Assign.target = ({ Node.value = Access access; _ } as access_node);
                    _;
                  } as assign)) ->
                let qualified = global_qualifier @ access in
                let qualified_access = { access_node with Node.value = Access qualified } in
                { state with variables = Map.set variables ~key:access ~data:qualified },
                Stub (Stub.Assign { assign with Assign.target = qualified_access })

            | _ ->
                state, value
          in
          state,
          { statement with Node.value }
        in
        let state, statement = qualify_toplevel_statement state { statement with Node.value } in
        state, [statement]

    end)
  in

  let module OrderDependent = Transform.Make(struct
      (* Keeps track of transformations we need to make. E.g. `import a as b` will result in a
         transformation rule from `b` to `a`. *)
      include Transform.Identity
      type t = Access.t * Access.t Access.Map.t

      let expression_postorder (qualifier, map) expression =
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

      let statement_postorder (qualifier, map) ({ Node.location; value } as statement) =
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

  let module QualifyDefines = Transform.Make(struct
      include Transform.Identity
      type t = Access.t Access.Map.t


      let statement_preorder map ({ Node.value; _ } as statement) =
        let prepend_function_prefix access =
          if Access.starts_with ~prefix:"*" access then
            access
          else
            Access.add_prefix ~prefix:"$renamed_" access
        in
        (* Handle qualification of variables in one specific method. *)
        let rec qualify_define
            ~parent_scope
            ~globals
            ~parent_qualifier
            ({ Define.name; parameters; body; _ } as define) =
          let module DefineQualifier = Transform.Make(struct
              include Transform.Identity
              type t = define_qualifier_state

              let expression_postorder
                  ({ current_scope; globals; _ } as state)
                  ({ Node.value; _ } as expression) =
                let value =
                  match value with
                  | Access access ->
                      Access (rename_named_arguments access)
                  | _ ->
                      value
                in
                match value with
                (* Weak globals. *)
                | Access (head :: tail) ->
                    begin
                      match Map.find current_scope [head] with
                      | Some rewrite ->
                          state, { expression with Node.value = Access (rewrite @ tail) }
                      | None ->
                          match Map.find globals [head] with
                          | Some global_access ->
                              (* This is a valid global access. *)
                              {
                                state with
                                current_scope = Map.set
                                    current_scope
                                    ~key:[head]
                                    ~data:global_access;
                              },
                              { expression with Node.value = Access (global_access @ tail) }
                          | None ->
                              (* Invalid global access. *)
                              state, { expression with Node.value }
                    end
                | _ ->
                    state, { expression with Node.value }

              let statement_preorder
                  ({ local_qualifier; current_scope; globals } as state)
                  ({ Node.value; _ } as statement) =
                let qualify_assign state assign =
                  match assign with
                  | {
                    Assign.target = { Node.value = Access [head]; _ } as access_node;
                    _;
                  } as assign ->
                      begin
                        match Map.find current_scope [head] with
                        | Some replacement ->
                            state,
                            {
                              assign with
                              Assign.target = { access_node with Node.value = Access replacement };
                            }
                        | None ->
                            let replacement = prepend_function_prefix [head] in
                            {
                              state with
                              current_scope = Map.set current_scope ~key:[head] ~data:replacement;
                            },
                            {
                              assign with
                              Assign.target = { access_node with Node.value = Access replacement };
                            }
                      end
                  | _ ->
                      state,
                      assign
                in
                match value with
                (* Locals. *)
                | Assign assign ->
                    let state, assign = qualify_assign state assign in
                    state, { statement with Node.value = Assign assign }
                | Stub (Stub.Assign assign) ->
                    let state, assign = qualify_assign state assign in
                    state, { statement with Node.value = Stub (Stub.Assign assign) }

                (* Strong globals. *)
                | Global identifiers ->
                    let access = Access.create_from_identifiers identifiers in
                    let qualified_access = global_qualifier @ access in
                    {
                      state with
                      current_scope = Map.set current_scope ~key:access ~data:qualified_access;
                    },
                    statement

                | Define define ->
                    (* Open a new recursion tree with its own state, that will not be propagated
                       further. *)
                    let define =
                      qualify_define
                        ~parent_scope:current_scope
                        ~globals
                        ~parent_qualifier:local_qualifier
                        define
                    in
                    state, { statement with Node.value = Define define }
                | _ ->
                    state, statement

              let statement_keep_recursing _ { Node.value; _ } =
                match value with
                | Define _ -> Transform.Stop
                | _ -> Transform.Recurse
            end)
          in

          (* Initialize local scope with the parameters. *)
          let add_parameter map { Node.value = { Parameter.name; _ }; _ } =
            let access = Access.create_from_identifiers [name] in
            Map.set ~key:access ~data:(prepend_function_prefix access) map
          in
          let current_scope = List.fold ~init:parent_scope ~f:add_parameter parameters in
          let local_qualifier = parent_qualifier @ name in

          let source =
            Source.create body
            |> DefineQualifier.transform {local_qualifier; current_scope; globals}
            |> snd  (* Discard the generated state, it is not useful. *)
          in
          let parameters =
            let rewrite ({ Node.value = { Parameter.name; _ } as parameter; location }) =
              let name =
                Access.create_from_identifiers [name]
                |> prepend_function_prefix
                |> Access.show
                |> Identifier.create
              in
              Node.create ~location { parameter with Parameter.name }
            in
            List.map ~f:rewrite parameters
          in
          { define with Define.body = Source.statements source; parameters }
        in

        match value with
        | Define define ->
            let define =
              qualify_define
                ~parent_scope:Access.Map.empty
                ~globals:map
                ~parent_qualifier:global_qualifier
                define
            in
            map, { statement with Node.value = Define define }

        | Stub (Stub.Define define) ->
            let define =
              qualify_define
                ~parent_scope:Access.Map.empty
                ~globals:map
                ~parent_qualifier:global_qualifier
                define
            in
            map, { statement with Node.value = Stub (Stub.Define define) }

        | _ ->
            map, statement

      let statement_keep_recursing _ { Node.value; _ } =
        match value with
        | Define _ -> Transform.Stop
        | _ -> Transform.Recurse
    end)
  in


  let { variables = global_variables; methods = global_methods }, source =
    QualifyToplevelStatements.transform
      { variables = Access.Map.empty; methods = Access.Map.empty }
      source
  in
  QualifyDefines.transform global_variables source
  |> snd  (* Discard the generated state, it is not useful. *)
  |> OrderDependent.transform (global_qualifier, global_methods)
  |> snd


let cleanup source =
  let module Cleanup = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.location; value } as statement) =
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
                            Access.Identifier (Identifier.remove_prefix ~prefix:"$renamed_" name)
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
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.location; value } as statement) =
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
      include Transform.Identity
      type t = unit

      let expression_postorder _ ({ Node.location; value } as expression) =
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
    end)
  in
  Transform.transform () source |> snd


(* TODO(T22866412) Find a more general way of dealing with this problem.
   This hack ensures that assertions from if tests get propagated even if
   there is no explicit else: in the code. *)
let expand_optional_assigns source =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ { Node.location; value } =
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
      include Transform.Identity
      type t = unit

      let expression_postorder _ { Node.location; value } =
        let value =
          match value with
          | BinaryOperator operator ->
              BinaryOperator.override operator
              |> Node.value
          | _ -> value
        in
        (), { Node.location; value }
    end)
  in
  Transform.transform () source |> snd


let return_access = Access.create "$return"


let expand_returns source =
  let module ExpandingTransform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
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
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
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
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
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
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
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
      include Transform.Identity
      type t = unit

      let statement_postorder state statement =
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
          value = Some {
              Node.value =
                Access [
                  Access.Identifier module_name;
                  Access.Call {
                    Node.value = {
                      Call.name = {
                        Node.value = Access [Access.Identifier named_tuple];
                        location = name_location;
                      };
                      arguments;
                    };
                    location = call_location;
                  }
                ];
              location = tuple_location;
            };
          _;
        };
    } when
        Identifier.show module_name = "typing" && Identifier.show named_tuple = "NamedTuple" ||
        Identifier.show module_name = "collections" && Identifier.show named_tuple = "namedtuple"
      ->
        let tuple =
          let call =
            {
              Call.name =
                Access [Access.Identifier (Identifier.create "NamedTuple")]
                |> Node.create ~location:name_location;
              arguments;
            }
            |> fun call -> Access.Call (Node.create ~location:call_location call)
          in
          Access [Access.Identifier (Identifier.create "typing"); call]
          |> Node.create ~location:tuple_location
        in
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
      include Transform.Identity
      type t = unit

      let count = ref 0

      let statement_postorder state statement =
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
      include Transform.Identity
      type t = Access.t Access.Map.t

      let statement_postorder map ({ Node.value; _ } as statement) =
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
