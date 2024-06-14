/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

///
/// Tests the process_trailing_dots method of the `parser_post_process` module.

#[cfg(test)]
mod process_trailing_dots_tests {
    use std::collections::HashSet;

    use parser_post_process::ParserPostprocessor;
    use parser_post_process::AUTOCOMPLETE_TOKEN;

    fn test_harness(input_code: &str, expected: &str, error_segments: Option<HashSet<usize>>) {
        let parser_post_processor = ParserPostprocessor::new();
        // Use specified error_segments or default to all lines
        let err_lines = error_segments.unwrap_or(HashSet::from_iter(0..input_code.lines().count()));
        let actual = parser_post_processor.postprocess(input_code, err_lines);
        println!("INPUT:\n{}", input_code.to_string().replace('\n', "\\n"));
        println!("EXPECTED:\n{}", expected.to_string().replace('\n', "\\n"));
        println!("ACTUAL:\n{}\n\n", actual.replace('\n', "\\n"));
        assert_eq!(expected.to_string(), actual);
    }

    #[test]
    fn test_simple() {
        test_harness("x = a.\n", &format!("x = a.{}\n", AUTOCOMPLETE_TOKEN), None);
        test_harness("x = a\n", "x = a\n", None);
    }

    #[test]
    fn test_dots_complex() {
        test_harness(
            "x = a.b.\n",
            &format!("x = a.b.{}\n", AUTOCOMPLETE_TOKEN),
            None,
        );
        test_harness("x =    .\n", "x =    .\n", None);
        test_harness("x =    .     ", "x =    .", None);
        test_harness("x '\n.'", "x '\n.'", None);
    }

    #[test]
    fn test_lambda_ellipsis() {
        test_harness(
            "lambda x,y: ...\nx = lambda x,y: ...",
            "lambda x,y: ...\nx = lambda x,y: ...",
            None,
        );
    }

    #[test]
    fn test_long_floats() {
        test_harness(
            "timestamp3 = 9223372036854775810.\n",
            "timestamp3 = 9223372036854775810.\n",
            None,
        );
        test_harness(
            "not_scientific = 1111111111111111.\n",
            "not_scientific = 1111111111111111.\n",
            None,
        );
    }

    #[test]
    fn test_alphanumeral_vars() {
        test_harness("a123.\n", &format!("a123.{}\n", AUTOCOMPLETE_TOKEN), None);
    }

    #[test]
    fn test_internal_trailing_dot() {
        test_harness(
            "x. = 123\n",
            &format!("x.{}= 123\n", AUTOCOMPLETE_TOKEN),
            None,
        );
    }
}
