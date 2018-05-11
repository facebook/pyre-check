(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser
open Statement


let expand_string_annotations source =
  let module Transform = Transform.MakeStatementTransformer(struct
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
  Transform.transform () source
  |> snd


type global_entities = {
  variables: Access.t Access.Map.t;
  methods: Access.t Access.Map.t;
}


type define_qualifier_state = {
  local_qualifier: Access.t;
  current_scope: Access.t Access.Map.t;
  globals: Access.t Access.Map.t;
}


let qualify ({ Source.qualifier = global_qualifier; path; _ } as source) =
  let rename_named_arguments access =
    let rename = function
      | Access.Call { Node.location; value = arguments } ->
          let rename_argument { Argument.name; value } =
            {
              Argument.name = name >>| (fun ({ Node.value; _ } as node) ->
                  { node with Node.value = Identifier.add_prefix ~prefix:"$renamed_" value });
              value;
            }
          in
          Access.Call { Node.value = (List.map ~f:rename_argument arguments); location }
      | access ->
          access
    in
    List.map ~f:rename access
  in

  let module QualifyToplevelStatements = Transform.Make(struct
      include Transform.Identity
      type t = global_entities

      let expression_postorder { variables; _ } expression =
        match expression with
        | { Node.location; value = Access access } ->
            let access = rename_named_arguments access in
            Map.find variables access
            >>| (fun replacement -> { Node.location; value = Access replacement })
            |> Option.value ~default:(Node.create ~location (Access access))
        | _ ->
            expression

      let statement_keep_recursing _ { Node.value; _ } =
        (* We are qualifying only top-level statements only, hence do not recurse into children
           nodes. Top-level expressions are the exception, variables therein need to be fully
           qualified. *)
        match value with
        | Expression _ -> Transform.Recurse
        | If _ -> Transform.Recurse
        | _ -> Transform.Stop

      let rec qualify_expression ({ variables; _ } as state) ({ Node.value; _ } as expression) =
        let value =
          match value with
          | Access access ->
              let qualify_access access =
                let qualify_subaccess = function
                  | Access.Subscript elements ->
                      let qualify_subscript = function
                        | Access.Index index ->
                            Access.Index (qualify_expression state index)
                        | Access.Slice { Access.lower; upper; step } ->
                            Access.Slice {
                              Access.lower = lower >>| qualify_expression state;
                              upper = upper >>| qualify_expression state;
                              step = step >>| qualify_expression state;
                            }
                      in
                      Access.Subscript (List.map ~f:qualify_subscript elements)
                  | access ->
                      (* Not sure if we want to qualify other accesses as well... *)
                      access
                in
                let lead, tail =
                  List.partition_tf
                    ~f:(function | Access.Identifier _ -> true | _ -> false)
                    access
                in
                let lead =
                  Map.find variables lead
                  |> Option.value ~default:lead
                in
                lead @ (List.map ~f:qualify_subaccess tail)
              in
              Access (qualify_access access)
          | _ ->
              value
        in
        { expression with Node.value }

      let statement_postorder state ({ Node.value; _ } as statement) =
        let rec qualify_class qualifier ({ Class.name; bases; body; _ } as definition) =
          let qualified_name = qualifier @ name in
          let parent = Some qualified_name in
          let qualify_bases ({ Argument.value; _ } as argument) =
            { argument with Argument.value = qualify_expression state value }
          in
          let qualify_statement ({ Node.value; _ } as statement) =
            let value =
              match value with
              | Assign assign ->
                  Assign { assign with Assign.parent }
              | Define define ->
                  Define (qualify_define state ~parent [] define)
              | Stub (Stub.Define define) ->
                  Stub (Stub.Define (qualify_define state ~parent [] define))
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
            body = List.map ~f:qualify_statement body;
          }

        and qualify_define
            state
            ?(parent=None)
            qualifier
            ({ Define.name; body; parameters; return_annotation; _ } as define) =
          let qualified_name = qualifier @ name in
          let rec qualify_in_define ({ Node.value; _ } as statement) =
            let qualify_statements = List.map ~f:qualify_in_define in
            let value =
              match value with
              | Define define ->
                  Define (qualify_define state ~parent qualified_name define)
              | Stub (Stub.Define define) ->
                  Stub (Stub.Define (qualify_define state ~parent qualified_name define))
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
          let qualify_parameter
              ({ Node.value = { Parameter.name; value; annotation }; _ } as parameter) =
            {
              parameter with
              Node.value = {
                Parameter.name;
                value = value >>| qualify_expression state;
                annotation = annotation >>| qualify_expression state;
              }
            }
          in
          {
            define with
            Define.name = qualified_name;
            body = List.map ~f:qualify_in_define body;
            parent;
            parameters = List.map ~f:qualify_parameter parameters;
            return_annotation = return_annotation >>| qualify_expression state
          }
        in

        let rec qualify_toplevel_statement
            ({ variables; methods } as state)
            ({ Node.value; _ } as statement) =
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
                let qualified = qualify_define state global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Define.name ~data:qualified.Define.name;
                },
                Define qualified
            | Stub (Stub.Define definition) when not (Define.is_method definition) ->
                let qualified = qualify_define state global_qualifier definition in
                {
                  state with
                  methods = Map.set methods ~key:definition.Define.name ~data:qualified.Define.name;
                },
                Stub (Stub.Define qualified)

            (* Qualify globals *)
            | Assign (
                {
                  Assign.target = ({ Node.value = Access access; _ } as access_node);
                  value;
                  annotation;
                  _;
                } as assign) ->
                let qualified = global_qualifier @ access in
                let qualified_access = { access_node with Node.value = Access qualified } in
                (* All assigned targets are variables by default,
                   unless we have already classified their values as methods. *)
                let annotation =
                  match annotation with
                  | Some { Node.value = Access access; location } when List.length access = 1 ->
                      begin
                        match Map.find variables access with
                        | Some replacement ->
                            Some { Node.value = Access replacement; location }
                        | _ ->
                            annotation
                      end
                  | _ ->
                      annotation
                in
                let state =
                  match value with
                  | Some { Node.value = Access value_access; _ }
                    when Map.mem methods value_access ->
                      { state with methods = Map.set methods ~key:access ~data:qualified }
                  | _ ->
                      { state with variables = Map.set variables ~key:access ~data:qualified }
                in
                state,
                Assign {
                  assign with
                  Assign.target = qualified_access;
                  value = value >>| qualify_expression state;
                  annotation;
                }
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
      type t = Access.t * (Access.t Access.Map.t) * (Access.t Access.Map.t)

      let expression_postorder (qualifier, variables, methods) expression =
        match expression with
        | { Node.value = Access (head :: tail); _ }
          when Access.equal qualifier [head] && Map.mem variables tail ->
            (* Duct tape fix: don't double qualify global constants. *)
            expression
        | { Node.location; value = Access (head :: tail) } ->
            begin
              match Map.find methods [head] with
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
              match Map.find methods (Access.create value) with
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

      let statement_postorder
          ((qualifier, variables, map) as state) ({ Node.location; value } as statement) =
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
            (qualifier, variables, List.fold_left imports ~f:add_import ~init:map),
            [statement]
        | Import ({ Import.from = Some from; imports } as import)
          when Access.show from <> "builtins" ->
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
                    let drop =
                      if List.length dots = 2 && List.length postfix = 0 then
                        (* Special case for single `.` in from clause. *)
                        1
                      else
                        List.length dots
                    in
                    let is_initializer_module =
                      String.is_suffix path ~suffix:"/__init__.py" ||
                      String.is_suffix path ~suffix:"/__init__.pyi"
                    in
                    if is_initializer_module then
                      drop - 1
                    else
                      drop
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
            (qualifier, variables, List.fold_left imports ~f:add_import ~init:map),
            [{ Node.location; value = Import { import with Import.from = Some from }}]
        | _ ->
            state, [statement]
    end)
  in

  let module QualifyDefines = Transform.Make(struct
      include Transform.Identity
      type t = Access.t Access.Map.t


      let statement_preorder map ({ Node.value; _ } as statement) =
        let prepend_function_prefix access =
          if Access.starts_with access ~prefix:"**" then
            Access.remove_prefix access ~prefix:"**"
            |> Access.add_prefix ~prefix:"**$renamed_"
          else if Access.starts_with access ~prefix:"*" then
            Access.remove_prefix access ~prefix:"*"
            |> Access.add_prefix ~prefix:"*$renamed_"
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

              let expression_preorder ({ current_scope; _ } as state) { Node.location; value } =
                let current_scope, value =
                  match value with
                  | Lambda { Lambda.parameters; body } ->
                      let add_parameter
                          (current_scope, parameters)
                          { Node.location; value = { Parameter.name; _ } as parameter } =
                        let access = Access.create_from_identifiers [name] in
                        let renamed = Access.add_prefix ~prefix:"$renamed_" access in
                        Map.set current_scope ~key:access ~data:renamed,
                        {
                          Node.location;
                          value = {
                            parameter with
                            Parameter.name = Identifier.create (Access.show renamed);
                          };
                        } :: parameters
                      in
                      let (current_scope, reversed_parameters) =
                        List.fold ~f:add_parameter ~init:(current_scope, []) parameters
                      in
                      current_scope,
                      (Lambda { Lambda.parameters = List.rev reversed_parameters; body })
                  | _ ->
                      current_scope, value
                in
                { state with current_scope }, { Node.location; value }

              let expression_postorder
                  { current_scope; globals; _ }
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
                          { expression with Node.value = Access (rewrite @ tail) }
                      | None ->
                          match Map.find globals [head] with
                          | Some global_access ->
                              (* This is a valid global access. *)
                              { expression with Node.value = Access (global_access @ tail) }
                          | None ->
                              (* Invalid global access. *)
                              { expression with Node.value }
                    end
                | _ ->
                    { expression with Node.value }

              let statement_preorder
                  ({ local_qualifier; current_scope; globals } as state)
                  ({ Node.value; _ } as statement) =
                let search_in_scope { current_scope; globals; _ } access =
                  match Map.find current_scope access with
                  | Some replacement ->
                      Some replacement
                  | None ->
                      Map.find globals access
                in
                let rec qualify_target
                    ({ current_scope; _ } as state)
                    ({ Node.value; _ } as target) =
                  let state, value =
                    match value with
                    | Access access when List.length access = 1 ->
                        begin
                          match Map.find current_scope access with
                          | Some replacement ->
                              state,
                              Access replacement
                          | None ->
                              let replacement = prepend_function_prefix access in
                              {
                                state with
                                current_scope =
                                  Map.set current_scope ~key:access ~data:replacement;
                              },
                              Access replacement
                        end
                    | Tuple elements ->
                        let state, elements =
                          let element (state, reversed_elements) element =
                            let state, element = qualify_target state element in
                            state, element :: reversed_elements
                          in
                          List.fold ~init:(state, []) ~f:element elements
                          |> fun (state, reversed_elements) -> state, List.rev reversed_elements
                        in
                        state,
                        Tuple elements
                    | _ ->
                        state,
                        value
                  in
                  state, { target with Node.value }
                in
                let qualify_assign state ({ Assign.target; annotation; _ } as assign) =
                  let annotation =
                    match annotation with
                    | Some { Node.value = Access access; location } when List.length access = 1 ->
                        begin
                          match search_in_scope state access with
                          | Some replacement ->
                              Some { Node.value = Access replacement; location }
                          | _ ->
                              annotation
                        end
                    | _ ->
                        annotation
                  in
                  let state, target = qualify_target state target in
                  state, { assign with Assign.target; annotation }
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

                | For ({ For.target; _ } as for_loop) ->
                    let state, target = qualify_target state target in
                    state, { statement with Node.value = For { for_loop with For.target } }

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
            let remove_stars access =
              if Access.starts_with access ~prefix:"**" then
                Access.remove_prefix access ~prefix:"**"
              else if Access.starts_with access ~prefix:"*" then
                Access.remove_prefix access ~prefix:"*"
              else
                access
            in
            let access =
              Access.create_from_identifiers [name]
              |> remove_stars
            in
            let replacement = prepend_function_prefix access in
            Map.set ~key:access ~data:replacement map
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
  |> OrderDependent.transform (global_qualifier, global_variables, global_methods)
  |> snd


let replace_version_specific_code source =
  let module Transform = Transform.MakeStatementTransformer(struct
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
  Transform.transform () source
  |> snd


let expand_type_checking_imports source =
  let module Transform = Transform.MakeStatementTransformer(struct
      include Transform.Identity
      type t = unit

      let statement_postorder _ ({ Node.value; _ } as statement) =
        let is_type_checking { Node.value; _ } =
          match value with
          | Access [Access.Identifier typing; Access.Identifier type_checking]
            when Identifier.show typing = "typing" &&
                 Identifier.show type_checking = "TYPE_CHECKING" ->
              true
          | Access [Access.Identifier type_checking]
            when Identifier.show type_checking = "TYPE_CHECKING" ->
              true
          | _ ->
              false
        in
        match value with
        | If { If.test; body; orelse = [] } when is_type_checking test ->
            (), body
        | _ ->
            (), [statement]
    end)
  in
  Transform.transform () source
  |> snd

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
                  expression
              | _ ->
                  {
                    Node.location;
                    value = Set [{ Node.location = keyword_location; value = keyword}];
                  }
            end

        | _ ->
            expression
    end)
  in
  Transform.transform () source
  |> snd


(* TODO(T22866412) Find a more general way of dealing with this problem.
   This hack ensures that assertions from if tests get propagated even if
   there is no explicit else: in the code. *)
let expand_optional_assigns source =
  let module Transform = Transform.MakeStatementTransformer(struct
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
  Transform.transform () source
  |> snd


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
        { Node.location; value }
    end)
  in
  Transform.transform () source
  |> snd


let expand_subscripts source =
  let module Transform = Transform.Make(struct
      include Transform.Identity
      type t = unit

      let expression_postorder _ { Node.location; value } =
        let rec substitute_subscripts access_list =
          let subscripts_to_argument subscripts =
            let to_argument subscript =
              let create_slice_call { Access.lower; upper; step } =
                let get_slice_argument slice_index =
                  match slice_index with
                  | Some index -> index
                  | None ->  { Node.value = Access (Access.create "None"); location }
                in
                Access [
                  Access.Identifier (Identifier.create "slice");
                  Access.Call {
                    Node.value = [
                      { Argument.name = None; value = (get_slice_argument lower) };
                      { Argument.name = None; value = (get_slice_argument upper) };
                      { Argument.name = None; value = (get_slice_argument step) };
                    ];
                    location;
                  };
                ]
              in
              match subscript with
              | Access.Index subscript ->
                  subscript
              | Access.Slice subscript ->
                  { Node.value = create_slice_call subscript; location }
            in
            match List.map ~f:to_argument subscripts with
            | [argument] ->
                argument
            | ({ Node.location; _ } as argument) :: arguments ->
                Node.create ~location (Expression.Tuple (argument :: arguments))
            | _ ->
                Node.create ~location (Expression.Tuple [])
          in
          match access_list with
          | Access.Subscript subscript_list :: accesses ->
              Access.Identifier (Identifier.create "__getitem__")
              :: Access.Call {
                Node.value = [{
                    Argument.name = None;
                    value = subscripts_to_argument subscript_list;
                  }];
                location;
              }
              :: (substitute_subscripts accesses)
          | access :: accesses ->
              access
              :: (substitute_subscripts accesses)
          | _ ->
              access_list
        in
        let value =
          match value with
          | Access access -> Access (substitute_subscripts access)
          | _ -> value
        in
        { Node.location; value }
    end)
  in
  Transform.transform () source
  |> snd


let return_access = Access.create "$return"


let expand_returns source =
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
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
  let module NormalizingTransform = Transform.MakeStatementTransformer(struct
      type t = unit

      let statement_postorder state statement =
        match statement with
        | { Node.location;
            value = YieldFrom ({
                location = expression_location;
                Node.value = Expression.Yield (Some yield);
              })
          } ->
            let add_call =
              Access.Expression yield
              :: (Access.call ~name:"__iter__" ~location:expression_location ())
            in
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
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
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
                    if async then
                      (Access.call ~name:"__aiter__" ~location ()) @
                      (Access.call ~name: "__anext__" ~location ())
                    else
                      (Access.call ~name:"__iter__" ~location ()) @
                      (Access.call ~name: "__next__" ~location ())
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
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
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
  let module ExpandingTransform = Transform.MakeStatementTransformer(struct
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
  let tuple_attributes ~parent ~expression =
    match expression with
    | {
      Node.location;
      value =
        Access [
          Access.Identifier module_name;
          Access.Identifier named_tuple;
          Access.Call { Node.value = arguments; _ };
        ];
    } when (Identifier.show module_name = "typing" &&
            Identifier.show named_tuple = "NamedTuple") ||
           (Identifier.show module_name = "collections" &&
            Identifier.show named_tuple = "namedtuple") ->
        let attributes =
          match arguments with
          | [_; { Argument.value = { Node.location; value = String serialized }; _ }] ->
              let attribute name =
                Access (Access.create name)
                |> Node.create ~location
              in
              String.split serialized ~on:' '
              |> List.map ~f:attribute
          | [_; { Argument.value = { Node.value = List arguments; _ }; _ }] ->
              let rec accessify ({ Node.value; _ } as expression) =
                let value =
                  match value with
                  | String name -> Access (Access.create name)
                  | Tuple [name; annotation] -> Tuple [accessify name; annotation]
                  | _ -> value
                in
                { expression with Node.value }
              in
              List.map arguments ~f:accessify
          | _ ->
              []
        in
        let attribute ({ Node.location; value } as expression) =
          let target, annotation =
            match value with
            | Tuple [target; annotation] ->
                target, annotation
            | _ ->
                expression, { Node.location; value = Access (Access.create "typing.Any")}
          in
          Assign {
            Assign.target;
            annotation = Some annotation;
            value = None;
            compound = None;
            parent = Some parent;
          }
          |> Node.create ~location
        in
        let attributes = List.map attributes ~f:attribute in
        if List.is_empty attributes then
          Some [Node.create ~location Pass]
        else
          Some attributes
    | _ ->
        None
  in
  let tuple_base ~location =
    {
      Argument.name = None;
      value = Node.create ~location (Access (Access.create "typing.NamedTuple"));
    }
  in
  let expand_named_tuples ({ Node.location; value } as statement) =
    let value =
      match value with
      | Assign {
          Assign.target = {
            Node.value = Access name;
            _;
          };
          value = Some expression;
          _;
        } ->
          tuple_attributes ~parent:name ~expression
          >>| (fun body ->
              Class {
                Class.name;
                bases = [tuple_base ~location];
                body;
                decorators = [];
                docstring = None;
              })
          |> Option.value ~default:value
      | Class ({ Class.name; bases; body; _; } as original) ->
          let extract_named_tuples (bases, attributes_sofar) ({ Argument.value; _ } as base) =
            tuple_attributes ~parent:name ~expression:value
            >>| (fun attributes -> (tuple_base ~location) :: bases, attributes_sofar @ attributes)
            |> Option.value ~default:(base :: bases, attributes_sofar)
          in
          let reversed_bases, attributes = List.fold bases ~init:([], []) ~f:extract_named_tuples in
          Class {
            original with
            Class.bases = List.rev reversed_bases;
            body = attributes @ body;
          }
      | _ ->
          value
    in
    { statement with Node.value }
  in
  { source with Source.statements = List.map ~f:expand_named_tuples statements }


let defines ?(include_stubs = false) ({ Source.statements; _ } as source) =
  let toplevel =
    Node.create_with_default_location (Statement.Define.create_toplevel statements)
  in
  let module Collector = Visit.StatementCollector(struct
      type t = Define.t Node.t
      let keep_recursing =
        function
        | { Node.value = Define _; _ }
        | { Node.value = Stub (Stub.Define _); _ }  ->
            Transform.Stop
        | _ ->
            Transform.Recurse


      let predicate = function
        | { Node.location; value = Define define } ->
            Some ({ Node.location; Node.value = define })
        | { Node.location; value = Stub (Stub.Define define) } when include_stubs ->
            Some ({ Node.location; Node.value = define })
        | _ ->
            None
    end)
  in
  toplevel :: (Collector.collect source)


let classes source =
  let module Collector = Visit.StatementCollector(struct
      type t = Statement.Class.t Node.t
      let keep_recursing _ = Transform.Recurse

      let predicate = function
        | { Node.location; value = Class class_define } ->
            Some ({ Node.location; Node.value = class_define })
        | _ ->
            None
    end)
  in
  Collector.collect source


let dequalify_map source =
  let module ImportDequalifier = Transform.MakeStatementTransformer(struct
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
  let map = Map.set ~key:(List.rev source.Source.qualifier) ~data:[] Access.Map.empty in
  ImportDequalifier.transform map source
  |> fst


let preprocess source =
  source
  |> expand_string_annotations
  |> replace_version_specific_code
  |> qualify
  |> fix_singleton_sets
  |> expand_optional_assigns
  |> expand_operators
  |> expand_subscripts
  |> expand_returns
  |> expand_for_loop
  |> expand_yield_from
  |> expand_ternary_assign
  |> expand_named_tuples
  |> expand_excepts
