// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

#![allow(dead_code)]

/*!
 The structs below are used to wrap the CST nodes output by tree-sitter
 into something that is a bit easier to reason about...

 Tree sitter seems to have a string representation of tokens with
 numerical ids that are known at runtime...

 What we do here is to convert a sitter node to an internal
 representation and use that to `match` in Rust for later processing
 into an AST by the cst_to_ast.rs lowerer.

 This wrapping could be eliminated at some point or made cheaper.
*/

use crate::node_wrapper::Node;

///
/// Token supertype after extraction from Tree-sitter CST
#[derive(Debug)]
pub enum NodeType<'a> {
    Error,
    Keyword(Keyword),
    BinaryOperator(BinaryOperator),
    AugAssignOperator(AugAssignOperator),
    ComparisonOperator(ComparisonOperator),
    Delimiter(Delimiter),
    Production(Production<'a>),
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Keyword {
    AND,
    AS,
    ASSERT,
    ASYNC,
    AWAIT,
    BREAK,
    CONTINUE,
    CLASS,
    DEF,
    ELIF,
    ELSE,
    EXCEPT,
    DEL,
    FALSE,
    FINALLY,
    FOR,
    FROM,
    GLOBAL,
    IF,
    IMPORT,
    IN,
    IS,
    IS_NOT,
    LAMBDA,
    NONE,
    NONLOCAL,
    NOT,
    NOT_IN,
    OR,
    PASS,
    RAISE,
    RETURN,
    TRUE,
    TRY,
    YIELD,
    WHILE,
    WITH,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum BinaryOperator {
    ASSIGN_EXPR,
    AT,
    BITWISE_AND,
    BITWISE_OR,
    BITWISE_XOR,
    DOUBLE_SLASH,
    DOUBLE_STAR,
    LEFT_SHIFT,
    MINUS,
    PERCENT,
    PLUS,
    RIGHT_SHIFT,
    SLASH,
    STAR,
    TILDE,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum AugAssignOperator {
    AT_EQUAL,
    BITWISE_AND_EQUAL,
    BITWISE_OR_EQUAL,
    DOUBLE_SLASH_EQUAL,
    DOUBLE_STAR_EQUAL,
    LEFT_SHIFT_EQUAL,
    MINUS_EQUAL,
    PERCENT_EQUAL,
    PLUS_EQUAL,
    RIGHT_SHIFT_EQUAL,
    SLASH_EQUAL,
    STAR_EQUAL,
    BITWISE_XOR_EQUAL,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum ComparisonOperator {
    LESS_THAN,
    LESS_THAN_EQUAL,
    EQUAL,
    NOT_EQUAL,
    GREATER_THAN_EQUAL,
    GREATER_THAN,
    IN,
    NOT,
    IS,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Delimiter {
    ARROW,
    ASSIGN,
    BACK_SLASH,
    COLON,
    COMMA,
    DUBLE_QUOTE,
    HASH,
    LEFT_BRACKET,
    LEFT_CURLY,
    LEFT_PAREN,
    PERIOD,
    RIGHT_BRACKET,
    RIGHT_CURLY,
    RIGHT_PAREN,
    SEMICOLON,
    SINGLE_QUOTE,
}

///
/// A production is a node to traverse and it carries a
/// reference to the sitter node
#[derive(Debug)]
pub struct Production<'a> {
    pub production_kind: ProductionKind,
    pub node: &'a Node<'a>,
}

///
/// Various forms of ProductionKind
#[derive(Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum ProductionKind {
    // statements
    FUTURE_IMPORT_STATEMENT,
    IMPORT_STATEMENT,
    IMPORT_FROM_STATEMENT,
    PRINT_STATEMENT,
    ASSERT_STATEMENT,
    EXPRESSION_STATEMENT,
    RETURN_STATEMENT,
    DELETE_STATEMENT,
    RAISE_STATEMENT,
    PASS_STATEMENT,
    BREAK_STATEMENT,
    CONTINUE_STATEMENT,
    GLOBAL_STATEMENT,
    NONLOCAL_STATEMENT,
    EXEC_STATEMENT,
    IF_STATEMENT,
    FOR_STATEMENT,
    WHILE_STATEMENT,
    TRY_STATEMENT,
    WITH_STATEMENT,
    FUNCTION_DEFINITION,
    CLASS_DEFINITION,
    DECORATED_DEFINITION,
    MATCH_STATEMENT,

    // `expression_statement`
    ASSIGNMENT,
    AUGMENTED_ASSIGNMENT,
    YIELD,

    // `expression`
    COMPARISON_OPERATOR,
    NOT_OPERATOR,
    BOOLEAN_OPERATOR,
    AWAIT,
    LAMBDA,
    CONDITIONAL_EXPRESSION,
    NAMED_EXPRESSION,
    AS_PATTERN,
    EXPRESSION_LIST,
    LIST_SPLAT_OR_EXPRESSIONS,

    // `primary_expression`
    BINARY_OPERATOR,
    IDENTIFIER,
    KEYWORD_IDENTIFIER,
    STRING,
    CONCATENATED_STRING,
    INTEGER,
    FLOAT,
    TRUE,
    FALSE,
    NONE,
    UNARY_OPERATOR,
    ATTRIBUTE,
    SUBSCRIPT,
    CALL,
    LIST,
    LIST_COMPREHENSION,
    DICTIONARY,
    DICTIONARY_COMPREHENSION,
    SET,
    SET_COMPREHENSION,
    TUPLE,
    PARENTHESIZED_EXPRESSION,
    GENERATOR_EXPRESSION,
    ELLIPSIS,

    FOR_IN_CLAUSE,
    IF_CLAUSE,

    TYPED_PARAMETER,
    KEYWORD_ARGUMENT,
    DEFAULT_PARAMETER,
    TYPED_DEFAULT_PARAMETER,
    LIST_SPLAT_PATTERN,
    TUPLE_PATTERN,
    LIST_PATTERN,
    KEYWORD_SEPARATOR,
    POSITIONAL_SEPARATOR,
    DICTIONARY_SPLAT_PATTERN,
    PATTERN_LIST,

    COMMENT,
    MODULE,
    PARAMETERS,
    TYPE,
    BLOCK,

    // scaffolding
    PAIR,
    ARGUMENT_LIST,
    DECORATOR,

    // special function arguments
    LIST_SPLAT,
    DICTIONARY_SPLAT,
}

impl<'a> Production<'a> {
    pub fn new(kind: ProductionKind, node: &'a Node<'a>) -> Self {
        Production {
            production_kind: kind,
            node,
        }
    }
}

///
/// Wrap a sitter `Node` into its structured wrapper (`NodeType`)
pub fn get_node_type<'a>(node: &'a Node<'a>) -> NodeType<'a> {
    match node.kind() {
        // keywords
        "as" => NodeType::Keyword(Keyword::AS),
        "assert" => NodeType::Keyword(Keyword::ASSERT),
        "async" => NodeType::Keyword(Keyword::ASYNC),
        // "await" => NodeType::Keyword(Keyword::AWAIT),
        "break" => NodeType::Keyword(Keyword::BREAK),
        "class" => NodeType::Keyword(Keyword::CLASS),
        "continue" => NodeType::Keyword(Keyword::CONTINUE),
        "def" => NodeType::Keyword(Keyword::DEF),
        "del" => NodeType::Keyword(Keyword::DEL),
        "elif" => NodeType::Keyword(Keyword::ELIF),
        "else" => NodeType::Keyword(Keyword::ELSE),
        "except" => NodeType::Keyword(Keyword::EXCEPT),
        "finally" => NodeType::Keyword(Keyword::FINALLY),
        "for" => NodeType::Keyword(Keyword::FOR),
        "from" => NodeType::Keyword(Keyword::FROM),
        "global" => NodeType::Keyword(Keyword::GLOBAL),
        "if" => NodeType::Keyword(Keyword::IF),
        "import" => NodeType::Keyword(Keyword::IMPORT),
        "in" => NodeType::Keyword(Keyword::IN),
        "is" => NodeType::Keyword(Keyword::IS),
        "is not" => NodeType::Keyword(Keyword::IS_NOT),
        "not in" => NodeType::Keyword(Keyword::NOT_IN),
        // "lambda" => NodeType::Keyword(Keyword::LAMBDA),
        "None" => NodeType::Keyword(Keyword::NONE),
        "nonlocal" => NodeType::Keyword(Keyword::NONLOCAL),
        "not" => NodeType::Keyword(Keyword::NOT),
        "pass" => NodeType::Keyword(Keyword::PASS),
        "raise" => NodeType::Keyword(Keyword::RAISE),
        "return" => NodeType::Keyword(Keyword::RETURN),
        "try" => NodeType::Keyword(Keyword::TRY),
        "while" => NodeType::Keyword(Keyword::WHILE),
        "with" => NodeType::Keyword(Keyword::WITH),

        // boolean operator
        "and" => NodeType::Keyword(Keyword::AND),
        "or" => NodeType::Keyword(Keyword::OR),

        // operator
        "==" => NodeType::ComparisonOperator(ComparisonOperator::EQUAL),
        ">" => NodeType::ComparisonOperator(ComparisonOperator::GREATER_THAN),
        ">=" => NodeType::ComparisonOperator(ComparisonOperator::GREATER_THAN_EQUAL),
        "<" => NodeType::ComparisonOperator(ComparisonOperator::LESS_THAN),
        "<=" => NodeType::ComparisonOperator(ComparisonOperator::LESS_THAN_EQUAL),
        "!=" => NodeType::ComparisonOperator(ComparisonOperator::NOT_EQUAL),

        ":=" => NodeType::BinaryOperator(BinaryOperator::ASSIGN_EXPR),
        "@" => NodeType::BinaryOperator(BinaryOperator::AT),
        "&" => NodeType::BinaryOperator(BinaryOperator::BITWISE_AND),
        "|" => NodeType::BinaryOperator(BinaryOperator::BITWISE_OR),
        "^" => NodeType::BinaryOperator(BinaryOperator::BITWISE_XOR),
        "//" => NodeType::BinaryOperator(BinaryOperator::DOUBLE_SLASH),
        "**" => NodeType::BinaryOperator(BinaryOperator::DOUBLE_STAR),
        "<<" => NodeType::BinaryOperator(BinaryOperator::LEFT_SHIFT),
        "-" => NodeType::BinaryOperator(BinaryOperator::MINUS),
        "%" => NodeType::BinaryOperator(BinaryOperator::PERCENT),
        "+" => NodeType::BinaryOperator(BinaryOperator::PLUS),
        ">>" => NodeType::BinaryOperator(BinaryOperator::RIGHT_SHIFT),
        "/" => NodeType::BinaryOperator(BinaryOperator::SLASH),
        "*" => NodeType::BinaryOperator(BinaryOperator::STAR),
        "~" => NodeType::BinaryOperator(BinaryOperator::TILDE),

        //aug assignment operators
        "@=" => NodeType::AugAssignOperator(AugAssignOperator::AT_EQUAL),
        "&=" => NodeType::AugAssignOperator(AugAssignOperator::BITWISE_AND_EQUAL),
        "|=" => NodeType::AugAssignOperator(AugAssignOperator::BITWISE_OR_EQUAL),
        "//=" => NodeType::AugAssignOperator(AugAssignOperator::DOUBLE_SLASH_EQUAL),
        "**=" => NodeType::AugAssignOperator(AugAssignOperator::DOUBLE_STAR_EQUAL),
        "<<=" => NodeType::AugAssignOperator(AugAssignOperator::LEFT_SHIFT_EQUAL),
        "-=" => NodeType::AugAssignOperator(AugAssignOperator::MINUS_EQUAL),
        "%=" => NodeType::AugAssignOperator(AugAssignOperator::PERCENT_EQUAL),
        "+=" => NodeType::AugAssignOperator(AugAssignOperator::PLUS_EQUAL),
        ">>=" => NodeType::AugAssignOperator(AugAssignOperator::RIGHT_SHIFT_EQUAL),
        "/=" => NodeType::AugAssignOperator(AugAssignOperator::SLASH_EQUAL),
        "*=" => NodeType::AugAssignOperator(AugAssignOperator::STAR_EQUAL),
        "^=" => NodeType::AugAssignOperator(AugAssignOperator::BITWISE_XOR_EQUAL),

        // delimiter
        "->" => NodeType::Delimiter(Delimiter::ARROW),
        "=" => NodeType::Delimiter(Delimiter::ASSIGN),
        "\\" => NodeType::Delimiter(Delimiter::BACK_SLASH),
        ":" => NodeType::Delimiter(Delimiter::COLON),
        "," => NodeType::Delimiter(Delimiter::COMMA),
        "\"" => NodeType::Delimiter(Delimiter::DUBLE_QUOTE),
        "#" => NodeType::Delimiter(Delimiter::HASH),
        "[" => NodeType::Delimiter(Delimiter::LEFT_BRACKET),
        "{" => NodeType::Delimiter(Delimiter::LEFT_CURLY),
        "(" => NodeType::Delimiter(Delimiter::LEFT_PAREN),
        "." => NodeType::Delimiter(Delimiter::PERIOD),
        "]" => NodeType::Delimiter(Delimiter::RIGHT_BRACKET),
        "}" => NodeType::Delimiter(Delimiter::RIGHT_CURLY),
        ")" => NodeType::Delimiter(Delimiter::RIGHT_PAREN),
        ";" => NodeType::Delimiter(Delimiter::SEMICOLON),
        "'" => NodeType::Delimiter(Delimiter::SINGLE_QUOTE),

        // grammar productions
        "comment" => NodeType::Production(Production::new(ProductionKind::COMMENT, node)),
        "block" => NodeType::Production(Production::new(ProductionKind::BLOCK, node)),

        // `_simple_statement`
        "future_import_statement" => NodeType::Production(Production::new(
            ProductionKind::FUTURE_IMPORT_STATEMENT,
            node,
        )),
        "import_statement" => {
            NodeType::Production(Production::new(ProductionKind::IMPORT_STATEMENT, node))
        }
        "import_from_statement" => {
            NodeType::Production(Production::new(ProductionKind::IMPORT_FROM_STATEMENT, node))
        }
        "print_statement" => {
            NodeType::Production(Production::new(ProductionKind::PRINT_STATEMENT, node))
        }
        "assert_statement" => {
            NodeType::Production(Production::new(ProductionKind::ASSERT_STATEMENT, node))
        }
        "expression_statement" => {
            NodeType::Production(Production::new(ProductionKind::EXPRESSION_STATEMENT, node))
        }
        "return_statement" => {
            NodeType::Production(Production::new(ProductionKind::RETURN_STATEMENT, node))
        }
        "delete_statement" => {
            NodeType::Production(Production::new(ProductionKind::DELETE_STATEMENT, node))
        }
        "raise_statement" => {
            NodeType::Production(Production::new(ProductionKind::RAISE_STATEMENT, node))
        }
        "pass_statement" => {
            NodeType::Production(Production::new(ProductionKind::PASS_STATEMENT, node))
        }
        "break_statement" => {
            NodeType::Production(Production::new(ProductionKind::BREAK_STATEMENT, node))
        }
        "continue_statement" => {
            NodeType::Production(Production::new(ProductionKind::CONTINUE_STATEMENT, node))
        }
        "global_statement" => {
            NodeType::Production(Production::new(ProductionKind::GLOBAL_STATEMENT, node))
        }
        "nonlocal_statement" => {
            NodeType::Production(Production::new(ProductionKind::NONLOCAL_STATEMENT, node))
        }
        "exec_statement" => {
            NodeType::Production(Production::new(ProductionKind::EXEC_STATEMENT, node))
        }
        "if_statement" => NodeType::Production(Production::new(ProductionKind::IF_STATEMENT, node)),
        "for_statement" => {
            NodeType::Production(Production::new(ProductionKind::FOR_STATEMENT, node))
        }
        "while_statement" => {
            NodeType::Production(Production::new(ProductionKind::WHILE_STATEMENT, node))
        }
        "try_statement" => {
            NodeType::Production(Production::new(ProductionKind::TRY_STATEMENT, node))
        }
        "with_statement" => {
            NodeType::Production(Production::new(ProductionKind::WITH_STATEMENT, node))
        }
        "function_definition" => {
            NodeType::Production(Production::new(ProductionKind::FUNCTION_DEFINITION, node))
        }
        "class_definition" => {
            NodeType::Production(Production::new(ProductionKind::CLASS_DEFINITION, node))
        }
        "decorated_definition" => {
            NodeType::Production(Production::new(ProductionKind::DECORATED_DEFINITION, node))
        }
        "match_statement" => {
            NodeType::Production(Production::new(ProductionKind::MATCH_STATEMENT, node))
        }

        // `expression_statement`
        "assignment" => NodeType::Production(Production::new(ProductionKind::ASSIGNMENT, node)),
        "augmented_assignment" => {
            NodeType::Production(Production::new(ProductionKind::AUGMENTED_ASSIGNMENT, node))
        }
        "yield" => NodeType::Production(Production::new(ProductionKind::YIELD, node)),

        // `expression`
        "comparison_operator" => {
            NodeType::Production(Production::new(ProductionKind::COMPARISON_OPERATOR, node))
        }
        "not_operator" => NodeType::Production(Production::new(ProductionKind::NOT_OPERATOR, node)),
        "boolean_operator" => {
            NodeType::Production(Production::new(ProductionKind::BOOLEAN_OPERATOR, node))
        }
        "await" => NodeType::Production(Production::new(ProductionKind::AWAIT, node)),
        "lambda" => NodeType::Production(Production::new(ProductionKind::LAMBDA, node)),
        "conditional_expression" => NodeType::Production(Production::new(
            ProductionKind::CONDITIONAL_EXPRESSION,
            node,
        )),
        "named_expression" => {
            NodeType::Production(Production::new(ProductionKind::NAMED_EXPRESSION, node))
        }
        "as_pattern" => NodeType::Production(Production::new(ProductionKind::AS_PATTERN, node)),

        // `primary_expression`
        "binary_operator" => {
            NodeType::Production(Production::new(ProductionKind::BINARY_OPERATOR, node))
        }
        "identifier" => NodeType::Production(Production::new(ProductionKind::IDENTIFIER, node)),
        "keyword_identifier" => {
            NodeType::Production(Production::new(ProductionKind::KEYWORD_IDENTIFIER, node))
        }
        "string" => NodeType::Production(Production::new(ProductionKind::STRING, node)),
        "concatenated_string" => {
            NodeType::Production(Production::new(ProductionKind::CONCATENATED_STRING, node))
        }
        "integer" => NodeType::Production(Production::new(ProductionKind::INTEGER, node)),
        "float" => NodeType::Production(Production::new(ProductionKind::FLOAT, node)),
        "true" => NodeType::Production(Production::new(ProductionKind::TRUE, node)),
        "false" => NodeType::Production(Production::new(ProductionKind::FALSE, node)),
        "none" => NodeType::Production(Production::new(ProductionKind::NONE, node)),
        "unary_operator" => {
            NodeType::Production(Production::new(ProductionKind::UNARY_OPERATOR, node))
        }
        "attribute" => NodeType::Production(Production::new(ProductionKind::ATTRIBUTE, node)),
        "subscript" => NodeType::Production(Production::new(ProductionKind::SUBSCRIPT, node)),
        "call" => NodeType::Production(Production::new(ProductionKind::CALL, node)),
        "list" => NodeType::Production(Production::new(ProductionKind::LIST, node)),
        "list_comprehension" => {
            NodeType::Production(Production::new(ProductionKind::LIST_COMPREHENSION, node))
        }
        "dictionary" => NodeType::Production(Production::new(ProductionKind::DICTIONARY, node)),
        "dictionary_comprehension" => NodeType::Production(Production::new(
            ProductionKind::DICTIONARY_COMPREHENSION,
            node,
        )),
        "set" => NodeType::Production(Production::new(ProductionKind::SET, node)),
        "set_comprehension" => {
            NodeType::Production(Production::new(ProductionKind::SET_COMPREHENSION, node))
        }

        "for_in_clause" => {
            NodeType::Production(Production::new(ProductionKind::FOR_IN_CLAUSE, node))
        }
        "if_clause" => NodeType::Production(Production::new(ProductionKind::IF_CLAUSE, node)),

        "tuple" => NodeType::Production(Production::new(ProductionKind::TUPLE, node)),
        "parenthesized_expression" => NodeType::Production(Production::new(
            ProductionKind::PARENTHESIZED_EXPRESSION,
            node,
        )),
        "generator_expression" => {
            NodeType::Production(Production::new(ProductionKind::GENERATOR_EXPRESSION, node))
        }
        "ellipsis" => NodeType::Production(Production::new(ProductionKind::ELLIPSIS, node)),

        "argument_list" => {
            NodeType::Production(Production::new(ProductionKind::ARGUMENT_LIST, node))
        }
        "parameters" => NodeType::Production(Production::new(ProductionKind::PARAMETERS, node)),

        "pair" => NodeType::Production(Production::new(ProductionKind::PAIR, node)),

        // special function arguments
        "list_splat" => NodeType::Production(Production::new(ProductionKind::LIST_SPLAT, node)),
        "dictionary_splat" => {
            NodeType::Production(Production::new(ProductionKind::DICTIONARY_SPLAT, node))
        }

        // `parameter`
        "typed_parameter" => {
            NodeType::Production(Production::new(ProductionKind::TYPED_PARAMETER, node))
        }
        "keyword_argument" => {
            NodeType::Production(Production::new(ProductionKind::KEYWORD_ARGUMENT, node))
        }
        "default_parameter" => {
            NodeType::Production(Production::new(ProductionKind::DEFAULT_PARAMETER, node))
        }
        "typed_default_parameter" => NodeType::Production(Production::new(
            ProductionKind::TYPED_DEFAULT_PARAMETER,
            node,
        )),
        "list_splat_pattern" => {
            NodeType::Production(Production::new(ProductionKind::LIST_SPLAT_PATTERN, node))
        }
        "tuple_pattern" => {
            NodeType::Production(Production::new(ProductionKind::TUPLE_PATTERN, node))
        }
        "list_pattern" => NodeType::Production(Production::new(ProductionKind::LIST_PATTERN, node)),
        "pattern_list" => NodeType::Production(Production::new(ProductionKind::PATTERN_LIST, node)),
        "keyword_separator" => {
            NodeType::Production(Production::new(ProductionKind::KEYWORD_SEPARATOR, node))
        }
        "positional_separator" => {
            NodeType::Production(Production::new(ProductionKind::POSITIONAL_SEPARATOR, node))
        }
        "expression_list" => {
            NodeType::Production(Production::new(ProductionKind::EXPRESSION_LIST, node))
        }
        "list_splat_or_expressions" => NodeType::Production(Production::new(
            ProductionKind::LIST_SPLAT_OR_EXPRESSIONS,
            node,
        )),
        "dictionary_splat_pattern" => NodeType::Production(Production::new(
            ProductionKind::DICTIONARY_SPLAT_PATTERN,
            node,
        )),

        // place production below properly
        "module" => NodeType::Production(Production::new(ProductionKind::MODULE, node)),
        "type" => NodeType::Production(Production::new(ProductionKind::TYPE, node)),

        "decorator" => NodeType::Production(Production::new(ProductionKind::DECORATOR, node)),

        "ERROR" => NodeType::Error,

        _ => panic!(
            "unmatched keyword node type: {}[{}]",
            node.kind(),
            node.is_named()
        ),
    }
}
