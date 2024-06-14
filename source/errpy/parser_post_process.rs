/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;

use regex::Regex;

// TODO: Refactor this into the constants.rs file once necessary updates are made to the TARGETS file
pub static AUTOCOMPLETE_TOKEN: &str = "AUTOCOMPLETE";

#[derive(Debug)]
pub struct ParserPostprocessor {}

#[derive(Debug)]
pub struct ErrorSegment {
    pub start_lineno: isize, // inclusive
    pub end_lineno: isize,   // exclusive
}

impl ParserPostprocessor {
    pub fn new() -> Self {
        ParserPostprocessor {}
    }

    ///
    /// Returns if the series of characters form a valid Python identifier or
    /// a valid combinations that can precede a "."
    ///
    /// (e.g.,  "foo.bar" is valid, "'foo'.split" is also valid, but
    ///         "!.bar" is not).
    ///
    /// A valid identifier has the following properties:
    /// * It contains only ASCII alphanumeric characters and underscores.
    /// * It begins with a letter or an underscore.
    /// * It is not followed by any punctuation.
    /// * It is not a reserved keyword.
    fn valid_tokens_preceding_dot(&self, line: &str) -> bool {
        let preceding_tokens = match line.rsplit_once([' ', '.', ')', '\'', '}', ']', '\"']) {
            Some((_, identifier_and_trailing_dot)) => identifier_and_trailing_dot,
            None => line,
        };

        !preceding_tokens.is_empty()
        && preceding_tokens
            .chars()
            .next()
            .map_or(false, |c| c.is_ascii_alphabetic() || c == '_') // First char must be letter or an underscore.
        && preceding_tokens
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_') // Only ASCII alphanumeric characters and underscores.
    }

    ///
    /// Injects a `AUTOCOMPLETE_TOKEN` to the input code if a valid identifier
    /// followed by a trailing dot is found.
    fn process_trailing_dots(&self, line: &str) -> String {
        let re = Regex::new(r"\.\s").expect("Invalid Regex");
        let mut result = Vec::new();
        let mut curr = 0;

        let segments = re.split(line).collect::<Vec<_>>();
        let num_segments = segments.len();

        for substring in segments {
            if curr < num_segments - 1 {
                if self.valid_tokens_preceding_dot(substring) {
                    result.push(format!("{}.{}", substring, AUTOCOMPLETE_TOKEN));
                } else {
                    result.push(format!("{}.", substring));
                }
                curr += 1;
            } else {
                // Skip last segment
                result.push(substring.trim_end().to_string());
                if line.ends_with('\n') {
                    result.push("\n".to_string());
                }
                break;
            }
        }
        result.join("")
    }

    pub fn postprocess(self, input_code: &str, error_lines: HashSet<usize>) -> String {
        input_code
            .split_inclusive('\n')
            .enumerate()
            .map(|(line_number, line)| {
                if error_lines.contains(&line_number) {
                    self.process_trailing_dots(line)
                } else {
                    String::from(line)
                }
            })
            .collect()
    }
}
