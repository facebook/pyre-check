(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

module Binding = struct
  module Kind = struct
    module Star = struct
      type t =
        | Once
        | Twice
      [@@deriving sexp, compare, hash]
    end

    type t =
      | AssignTarget of Expression.t option
      | ClassName
      | ComprehensionTarget
      | DefineName of Statement.Define.Signature.t
      | ExceptTarget of Expression.t option
      | ForTarget
      | ImportName
      | ParameterName of {
          index: int;
          annotation: Expression.t option;
          star: Star.t option;
        }
      | WithTarget
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    name: Identifier.t;
    location: Location.t;
  }
  [@@deriving sexp, compare, hash]

  let rec of_unannotated_target ~kind sofar { Node.value = target; location } =
    let open Expression in
    match target with
    | Expression.Name (Name.Identifier name) -> { name; kind; location } :: sofar
    | Expression.Starred (Starred.Once element | Starred.Twice element) ->
        of_unannotated_target ~kind sofar element
    | Tuple elements
    | List elements ->
        (* Tuple or list cannot be annotated. *)
        List.fold elements ~init:sofar ~f:(of_unannotated_target ~kind)
    | _ -> sofar


  let rec of_statement sofar { Node.value = statement; location } =
    (* TODO (T53600647): Support walrus operator. *)
    let open Statement in
    match statement with
    | Statement.Assign
        {
          Assign.target =
            { Node.value = Expression.Expression.Name (Expression.Name.Identifier name); location };
          annotation = Some annotation;
          _;
        } ->
        { name; kind = Kind.AssignTarget (Some annotation); location } :: sofar
    | Statement.Assign { Assign.target; _ } ->
        of_unannotated_target ~kind:(Kind.AssignTarget None) sofar target
    | Statement.Class { Class.name = { Node.value = name; _ }; _ } ->
        { kind = Kind.ClassName; name = Reference.show name; location } :: sofar
    | Statement.Define { Define.signature = { name = { Node.value = name; _ }; _ } as signature; _ }
      ->
        { kind = Kind.DefineName signature; name = Reference.show name; location } :: sofar
    | Statement.For { For.target; body; orelse; _ } ->
        let sofar = of_unannotated_target ~kind:Kind.ForTarget sofar target in
        let sofar = of_statements sofar body in
        of_statements sofar orelse
    | Statement.If { If.body; orelse; _ } ->
        let sofar = of_statements sofar body in
        of_statements sofar orelse
    | Statement.Import { Import.imports; from } ->
        let binding_of_import sofar { Import.alias; name = { Node.value = name; location } } =
          match alias with
          | Some { Node.value = alias; location } ->
              { kind = Kind.ImportName; name = Reference.show alias; location } :: sofar
          | None -> (
              match from with
              | Some _ ->
                  (* `name` must be a simple name *)
                  { kind = Kind.ImportName; name = Reference.show name; location } :: sofar
              | None ->
                  (* `import a.b` actually binds name a *)
                  let name = Reference.as_list name |> List.hd_exn in
                  { kind = Kind.ImportName; name; location } :: sofar )
        in
        List.fold imports ~init:sofar ~f:binding_of_import
    | Statement.Try { Try.body; handlers; orelse; finally } ->
        let bindings_of_handler sofar { Try.Handler.name; kind; body } =
          let sofar =
            match name with
            | None -> sofar
            | Some name ->
                (* TODO: Track the location of handler name. *)
                { kind = Kind.ExceptTarget kind; name; location } :: sofar
          in
          of_statements sofar body
        in
        let sofar = of_statements sofar body in
        let sofar = List.fold handlers ~init:sofar ~f:bindings_of_handler in
        let sofar = of_statements sofar orelse in
        of_statements sofar finally
    | Statement.While { While.body; orelse; _ } ->
        let sofar = of_statements sofar body in
        of_statements sofar orelse
    | Statement.With { With.items; body; _ } ->
        let bindings_of_item sofar (_, alias) =
          match alias with
          | None -> sofar
          | Some target -> of_unannotated_target ~kind:Kind.WithTarget sofar target
        in
        let sofar = List.fold items ~init:sofar ~f:bindings_of_item in
        of_statements sofar body
    | Assert _
    | Break
    | Continue
    | Delete _
    | Expression _
    | Global _
    | Nonlocal _
    | Pass
    | Raise _
    | Return _
    | Yield _
    | YieldFrom _ ->
        sofar


  and of_statements sofar statements = List.fold statements ~init:sofar ~f:of_statement

  let of_parameters sofar parameters =
    let of_parameter
        index
        sofar
        { Node.value = { Expression.Parameter.name; annotation; _ }; location }
      =
      let star, name =
        let star, name = Identifier.split_star name in
        let star =
          match star with
          | "**" -> Some Kind.Star.Twice
          | "*" -> Some Kind.Star.Once
          | _ -> None
        in
        star, name
      in
      { kind = Kind.ParameterName { index; star; annotation }; name; location } :: sofar
    in
    List.foldi parameters ~init:sofar ~f:of_parameter


  let of_generators sofar generators =
    let of_generator sofar { Expression.Comprehension.Generator.target; _ } =
      of_unannotated_target ~kind:Kind.ComprehensionTarget sofar target
    in
    List.fold generators ~init:sofar ~f:of_generator
end

let rec globals_of_statement sofar { Node.value; _ } =
  let open Statement in
  match value with
  | Statement.Global globals -> List.rev_append globals sofar
  | For { For.body; orelse; _ }
  | If { If.body; orelse; _ }
  | While { While.body; orelse; _ } ->
      let sofar = globals_of_statements sofar body in
      globals_of_statements sofar orelse
  | Try { Try.body; handlers; orelse; finally } ->
      let sofar = globals_of_statements sofar body in
      let sofar =
        List.fold handlers ~init:sofar ~f:(fun sofar { Try.Handler.body; _ } ->
            globals_of_statements sofar body)
      in
      let sofar = globals_of_statements sofar orelse in
      globals_of_statements sofar finally
  | With { With.body; _ } -> globals_of_statements sofar body
  | Assign _
  | Assert _
  | Break
  | Class _
  | Continue
  | Define _
  | Delete _
  | Expression _
  | Import _
  | Nonlocal _
  | Pass
  | Raise _
  | Return _
  | Yield _
  | YieldFrom _ ->
      sofar


and globals_of_statements sofar statements =
  List.fold statements ~init:sofar ~f:globals_of_statement


let rec nonlocals_of_statement sofar { Node.value; _ } =
  let open Statement in
  match value with
  | Statement.Nonlocal nonlocals -> List.rev_append nonlocals sofar
  | For { For.body; orelse; _ }
  | If { If.body; orelse; _ }
  | While { While.body; orelse; _ } ->
      let sofar = nonlocals_of_statements sofar body in
      nonlocals_of_statements sofar orelse
  | Try { Try.body; handlers; orelse; finally } ->
      let sofar = nonlocals_of_statements sofar body in
      let sofar =
        List.fold handlers ~init:sofar ~f:(fun sofar { Try.Handler.body; _ } ->
            nonlocals_of_statements sofar body)
      in
      let sofar = nonlocals_of_statements sofar orelse in
      nonlocals_of_statements sofar finally
  | With { With.body; _ } -> nonlocals_of_statements sofar body
  | Assign _
  | Assert _
  | Break
  | Class _
  | Continue
  | Define _
  | Delete _
  | Expression _
  | Global _
  | Import _
  | Pass
  | Raise _
  | Return _
  | Yield _
  | YieldFrom _ ->
      sofar


and nonlocals_of_statements sofar statements =
  List.fold statements ~init:sofar ~f:nonlocals_of_statement


module Scope = struct
  module Kind = struct
    type t =
      | Module
      | Define of Statement.Define.Signature.t
      | Lambda
      | Comprehension
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    globals: Identifier.Set.t;
    nonlocals: Identifier.Set.t;
    bindings: Binding.t Identifier.Map.t;
  }
  [@@deriving sexp, compare]

  let create_map ~globals ~nonlocals =
    List.fold ~init:Identifier.Map.empty ~f:(fun sofar ({ Binding.name; _ } as binding) ->
        match Identifier.Set.mem globals name || Identifier.Set.mem nonlocals name with
        | true ->
            (* Global and nonlocal declarations take priority. *)
            sofar
        | false ->
            (* First binding (i.e. last item in the list) wins. *)
            Map.set sofar ~key:name ~data:binding)


  let of_define { Statement.Define.signature; body; _ } =
    let open Statement in
    if Define.Signature.is_toplevel signature then
      None
    else
      let globals = globals_of_statements [] body |> Identifier.Set.of_list in
      let nonlocals = nonlocals_of_statements [] body |> Identifier.Set.of_list in
      let bindings =
        let parameter_bindings =
          let { Define.Signature.parameters; _ } = signature in
          Binding.of_parameters [] parameters
        in
        Binding.of_statements parameter_bindings body
      in
      Some
        {
          kind = Kind.Define signature;
          globals;
          nonlocals;
          bindings = create_map ~globals ~nonlocals bindings;
        }


  let of_define_exn define =
    match of_define define with
    | None ->
        let message =
          Format.asprintf
            "Cannot create a scope for define %a"
            Reference.pp
            (Node.value (Statement.Define.name define))
        in
        failwith message
    | Some result -> result


  let of_expression { Node.value; _ } =
    let open Expression in
    (* There's no way to declare globals or nonlocals in expression scope. *)
    let globals = Identifier.Set.empty in
    let nonlocals = Identifier.Set.empty in
    match value with
    | Expression.Lambda { Lambda.parameters; _ } ->
        Some
          {
            kind = Kind.Lambda;
            globals;
            nonlocals;
            bindings = Binding.of_parameters [] parameters |> create_map ~globals ~nonlocals;
          }
    | DictionaryComprehension { Comprehension.generators; _ }
    | Generator { Comprehension.generators; _ }
    | ListComprehension { Comprehension.generators; _ }
    | SetComprehension { Comprehension.generators; _ } ->
        Some
          {
            kind = Kind.Comprehension;
            globals;
            nonlocals;
            bindings = Binding.of_generators [] generators |> create_map ~globals ~nonlocals;
          }
    | _ -> None


  let of_expression_exn expression =
    match of_expression expression with
    | None ->
        let message =
          Format.asprintf "Cannot create a scope for expression %a" Expression.pp expression
        in
        failwith message
    | Some result -> result


  let of_source { Source.statements; _ } =
    let globals =
      (* Global statement has no effect at the module level. *)
      Identifier.Set.empty
    in
    let nonlocals =
      (* Nonlocal statement is illegal at the module level. *)
      Identifier.Set.empty
    in
    {
      kind = Kind.Module;
      globals;
      nonlocals;
      bindings = Binding.of_statements [] statements |> create_map ~globals ~nonlocals;
    }


  let lookup_bindings { bindings; _ } = Map.find bindings

  module Lookup = struct
    type t =
      | Binding of Binding.t
      | Global
      | Nonlocal
    [@@deriving sexp, compare, hash]
  end

  let lookup { globals; nonlocals; bindings; _ } name =
    match Set.mem globals name with
    | true -> Some Lookup.Global
    | false -> (
        match Set.mem nonlocals name with
        | true -> Some Lookup.Nonlocal
        | false -> Map.find bindings name >>| fun binding -> Lookup.Binding binding )
end

module Access = struct
  module Locality = struct
    type t =
      | Local
      | Nonlocal
      | Global
    [@@deriving sexp, compare, hash]
  end

  module Kind = struct
    type t =
      | CurrentScope
      | OuterScope of Locality.t
    [@@deriving sexp, compare, hash]
  end

  type t = {
    kind: Kind.t;
    binding: Binding.t;
    scope: Scope.t;
  }
  [@@deriving sexp, compare]
end

module ScopeStack = struct
  type t = {
    global: Scope.t;
    locals: Scope.t list;
  }

  let create source = { global = Scope.of_source source; locals = [] }

  let global_scope { global; _ } = global

  let current_scope { locals; global } =
    match locals with
    | [] -> global
    | scope :: _ -> scope


  let lookup { locals; global } name =
    let rec lookup_outer_scopes ~exclude_global = function
      | [] ->
          if exclude_global then
            None
          else
            Scope.lookup_bindings global name >>| fun binding -> global, binding
      | scope :: rest -> (
          match Scope.lookup scope name with
          | Some (Scope.Lookup.Binding binding) -> Some (scope, binding)
          | Some Scope.Lookup.Global -> lookup_outer_scopes ~exclude_global []
          | _ -> lookup_outer_scopes ~exclude_global rest )
    in
    match locals with
    | [] ->
        (* No need to deal with nested scopes if there's none *)
        Scope.lookup_bindings global name
        >>| fun binding -> { Access.binding; scope = global; kind = Access.Kind.CurrentScope }
    | current_scope :: rest -> (
        match Scope.lookup current_scope name with
        | Some (Scope.Lookup.Binding binding) ->
            (* Binding found in the current scope *)
            Some { Access.binding; scope = current_scope; kind = Access.Kind.CurrentScope }
        | Some Scope.Lookup.Global ->
            Scope.lookup_bindings global name
            >>| fun binding ->
            { Access.binding; scope = global; kind = Access.(Kind.OuterScope Locality.Global) }
        | Some Scope.Lookup.Nonlocal ->
            lookup_outer_scopes ~exclude_global:true rest
            >>| fun (scope, binding) ->
            { Access.binding; scope; kind = Access.(Kind.OuterScope Locality.Nonlocal) }
        | None ->
            lookup_outer_scopes ~exclude_global:false rest
            >>| fun (scope, binding) ->
            { Access.binding; scope; kind = Access.(Kind.OuterScope Locality.Local) } )


  let extend ~with_ { global; locals } = { global; locals = with_ :: locals }
end

module Builtins = struct
  let mem = function
    (* Builtins recognized by Pyre itself *)
    | "BoundMethod"
    | "pyre_dump"
    | "reveal_type"
    (* Builtins recognized by Python *)
    | "__build_class__"
    | "__builtins__"
    | "__debug__"
    | "__dict__"
    | "__doc__"
    | "__file__"
    | "__import__"
    | "__loader__"
    | "__name__"
    | "__package__"
    | "__spec__"
    | "ArithmeticError"
    | "AssertionError"
    | "AttributeError"
    | "BaseException"
    | "BlockingIOError"
    | "BrokenPipeError"
    | "BufferError"
    | "BytesWarning"
    | "ChildProcessError"
    | "ConnectionAbortedError"
    | "ConnectionError"
    | "ConnectionRefusedError"
    | "ConnectionResetError"
    | "DeprecationWarning"
    | "EOFError"
    | "Ellipsis"
    | "EnvironmentError"
    | "Exception"
    | "False"
    | "FileExistsError"
    | "FileNotFoundError"
    | "FloatingPointError"
    | "FutureWarning"
    | "GeneratorExit"
    | "IOError"
    | "ImportError"
    | "ImportWarning"
    | "IndentationError"
    | "IndexError"
    | "InterruptedError"
    | "IsADirectoryError"
    | "KeyError"
    | "KeyboardInterrupt"
    | "LookupError"
    | "MemoryError"
    | "ModuleNotFoundError"
    | "NameError"
    | "None"
    | "NotADirectoryError"
    | "NotImplemented"
    | "NotImplementedError"
    | "OSError"
    | "OverflowError"
    | "PendingDeprecationWarning"
    | "PermissionError"
    | "ProcessLookupError"
    | "RecursionError"
    | "ReferenceError"
    | "ResourceWarning"
    | "RuntimeError"
    | "RuntimeWarning"
    | "StopAsyncIteration"
    | "StopIteration"
    | "SyntaxError"
    | "SyntaxWarning"
    | "SystemError"
    | "SystemExit"
    | "TabError"
    | "TimeoutError"
    | "True"
    | "TypeError"
    | "UnboundLocalError"
    | "UnicodeDecodeError"
    | "UnicodeEncodeError"
    | "UnicodeError"
    | "UnicodeTranslateError"
    | "UnicodeWarning"
    | "UserWarning"
    | "ValueError"
    | "Warning"
    | "ZeroDivisionError"
    | "abs"
    | "all"
    | "any"
    | "ascii"
    | "bin"
    | "bool"
    | "breakpoint"
    | "bytearray"
    | "bytes"
    | "callable"
    | "chr"
    | "classmethod"
    | "compile"
    | "complex"
    | "copyright"
    | "credits"
    | "delattr"
    | "dict"
    | "dir"
    | "divmod"
    | "enumerate"
    | "eval"
    | "exec"
    | "exit"
    | "filter"
    | "float"
    | "format"
    | "frozenset"
    | "getattr"
    | "globals"
    | "hasattr"
    | "hash"
    | "help"
    | "hex"
    | "id"
    | "input"
    | "int"
    | "isinstance"
    | "issubclass"
    | "iter"
    | "len"
    | "license"
    | "list"
    | "locals"
    | "map"
    | "max"
    | "memoryview"
    | "min"
    | "next"
    | "object"
    | "oct"
    | "open"
    | "ord"
    | "pow"
    | "print"
    | "property"
    | "quit"
    | "range"
    | "repr"
    | "reversed"
    | "round"
    | "set"
    | "setattr"
    | "slice"
    | "sorted"
    | "staticmethod"
    | "str"
    | "sum"
    | "super"
    | "tuple"
    | "type"
    | "vars"
    | "zip" ->
        true
    | _ -> false
end
