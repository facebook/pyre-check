(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module ExpressionTypes : sig
  type coverage_data = {
    expression: Expression.t option;
    type_: Type.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_data_lookup

  val create_of_module : TypeEnvironment.ReadOnly.t -> Reference.t -> coverage_data_lookup
end

module LocationWithSymbol : sig
  type symbol_with_definition =
    | Expression of Expression.t
    | TypeAnnotation of Expression.t
  [@@deriving compare, show]

  type cfg_data = {
    define_name: Reference.t;
    node_id: int;
    statement_index: int;
  }
  [@@deriving compare, show]

  type symbol_and_cfg_data = {
    symbol_with_definition: symbol_with_definition;
    cfg_data: cfg_data;
    use_postcondition_info: bool;
  }
  [@@deriving compare, show]

  val location_insensitive_compare_symbol_and_cfg_data
    :  symbol_and_cfg_data ->
    symbol_and_cfg_data ->
    int
end

module ExpressionLevelCoverage : sig
  type typeisany =
    | ParameterIsAny
    | OtherExpressionIsAny
  [@@deriving compare, sexp, show, hash, to_yojson]

  type reason =
    | TypeIsAny of typeisany
    | ContainerParameterIsAny
    | CallableParameterIsUnknownOrAny
    | CallableReturnIsAny
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_gap = {
    coverage_data: ExpressionTypes.coverage_data;
    reason: reason;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_gap_by_location = {
    location: Location.t;
    function_name: string option;
    type_: Type.t;
    reason: string list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type coverage_for_path = {
    total_expressions: int;
    coverage_gaps: coverage_gap_by_location list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  val parameter_is_any_message : string list

  val expression_is_any_message : string list

  val container_parameter_is_any_message : string list

  val callable_parameter_is_unknown_or_any_message : string list

  val callable_return_is_any_message : string list

  val get_all_nodes_and_coverage_data
    :  ExpressionTypes.coverage_data_lookup ->
    (Location.t * ExpressionTypes.coverage_data) list

  val classify_coverage_data : ExpressionTypes.coverage_data -> coverage_gap option

  val coverage_gaps_in_module : ExpressionTypes.coverage_data list -> coverage_gap list

  val get_expression_level_coverage : ExpressionTypes.coverage_data_lookup -> coverage_for_path
end

module SymbolSelection : sig
  type resolution_error =
    | ResolvedTop
    | ResolvedUnbound
    | UntrackedType of string
  [@@deriving sexp, show, compare, yojson { strict = false }]

  type attribute_lookup_error =
    | ReferenceNotFoundAndBaseUnresolved of resolution_error
    | AttributeUnresolved
    | ClassSummaryNotFound
    | ClassSummaryAttributeNotFound
  [@@deriving sexp, show, compare, yojson { strict = false }]

  type lookup_error =
    | SymbolNotFound
    | IdentifierDefinitionNotFound of Ast.Reference.t
    | AttributeDefinitionNotFound of string option * attribute_lookup_error
    | UnsupportedExpression of string
  [@@deriving sexp, show, compare, yojson { strict = false }]

  val location_contains_position : Location.t -> Location.position -> bool

  val get_coverage_data
    :  ExpressionTypes.coverage_data_lookup ->
    position:Location.position ->
    (Location.t * ExpressionTypes.coverage_data) option

  val covers_position : position:Location.position -> Statement.t -> bool

  val narrowest_match
    :  LocationWithSymbol.symbol_and_cfg_data list ->
    LocationWithSymbol.symbol_and_cfg_data option

  val find_narrowest_spanning_symbol
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    Location.position ->
    (LocationWithSymbol.symbol_and_cfg_data, lookup_error) result
end

module SingleSymbolQueries : sig
  type hover_info = {
    value: string option;
    docstring: string option;
  }
  [@@deriving sexp, show, compare, yojson { strict = false }]

  module AttributeDetail : sig
    type kind =
      | Simple
      | Variable
      | Property
      | Method
    [@@deriving show, compare, sexp]

    type t = {
      kind: kind;
      name: string;
      detail: string;
    }
    [@@deriving show, compare, sexp]
  end

  module DocumentSymbolItem : sig
    (** A type a document symbol response *)
    module SymbolKind : sig
      type t =
        | File
        | Module
        | Namespace
        | Package
        | Class
        | Method
        | Property
        | Field
        | Constructor
        | Enum
        | Interface
        | Function
        | Variable
        | Constant
        | String
        | Number
        | Boolean
        | Array
        | Object
        | Key
        | Null
        | EnumMember
        | Struct
        | Event
        | Operator
        | TypeParameter
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    type t = {
      name: string;
      detail: string;
      kind: SymbolKind.t;
      range: Ast.Location.t;
      selectionRange: Ast.Location.t;
      children: t list; (* recursive type to represent a list of document symbols *)
    }
    [@@deriving sexp, compare, yojson { strict = false }]
  end

  val resolve_definition_for_symbol
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    LocationWithSymbol.symbol_and_cfg_data ->
    (Ast.Location.WithModule.t, SymbolSelection.lookup_error) result

  val location_of_definition
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    Location.position ->
    (Ast.Location.WithModule.t, SymbolSelection.lookup_error) result

  val resolve_completions_for_symbol
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    LocationWithSymbol.symbol_and_cfg_data ->
    AttributeDetail.t list

  val completion_info_for_position
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    Location.position ->
    AttributeDetail.t list

  val resolve_type_for_symbol
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    LocationWithSymbol.symbol_and_cfg_data ->
    Type.t option

  val type_at_location
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    Location.t ->
    Type.t option

  val hover_info_for_position
    :  type_environment:TypeEnvironment.ReadOnly.t ->
    module_reference:Reference.t ->
    Location.position ->
    hover_info

  val document_symbol_info : source:Ast.Source.t -> DocumentSymbolItem.t list
end
