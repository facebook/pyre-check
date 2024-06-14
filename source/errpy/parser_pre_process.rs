// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

#[derive(PartialEq)]
enum StringCommentState {
    OutsideString,
    InsideSingleQuotedString,
    InsideDoubleQuotedString,
    InsideSingleQuotedStringEscape,
    InsideDoubleQuotedStringEscape,
    InsideSingleQuotedMultilineString,
    InsideDoubleQuotedMultilineString,
    InsideComment,
}

// handles a single (') or double quote (") and returns new parser state
fn handle_quote_character(
    input_code: &str,
    state: StringCommentState,
    idx: &usize,
    multiline_quote: &str,
    inside_string: StringCommentState,
    inside_string_escaped: StringCommentState,
    inside_multiline_string: StringCommentState,
) -> StringCommentState {
    use StringCommentState::*;

    if state == OutsideString {
        if input_code[..idx + 1].ends_with(multiline_quote) {
            // triple quote => start new multiline string
            inside_multiline_string
        } else {
            // single quote => we are now inside a string
            inside_string
        }
    } else if state == inside_string_escaped {
        // escaped quote => we are still inside the string
        inside_string
    } else if state == inside_string {
        // quote ends string => we are now outside
        OutsideString
    } else if state == inside_multiline_string {
        if input_code[..idx + 1].ends_with(multiline_quote)
            && (input_code.len() == idx + 1 || !input_code[..idx + 2].ends_with(multiline_quote))
        {
            // triple quote ends the multiline string => we are now outside
            OutsideString
        } else {
            // a single quote won't do anything in a multiline string
            // => we are still inside the multiline string
            inside_multiline_string
        }
    } else {
        // any other case (e.g. single quote in double quote string)
        // => don't change the parser state
        state
    }
}

///
/// This function will remove comment's from input_code.
/// Comments are replaced with newlines so as to preserve line
/// and column information
///
/// TODO: This code will need refactoring in the future if/when we wish to
///  * Handle `# pyre-fixme:`'s better by integrating them into the AST.
///  * Outut a CST code will need refactoring.
///
/// For now this function is only used on in the context of AST production
/// so it's fit for purpose.
pub fn remove_comments(input_code: String) -> String {
    use StringCommentState::*;

    let without_comments = &mut String::new();

    // start of current slice to copy to result string
    let mut start = 0;
    // line end index (either ends at comment start or at newline)
    let mut line_end: Option<usize> = None;
    let mut state = OutsideString;
    for (idx, char) in input_code.char_indices() {
        match char {
            '\\' => match state {
                InsideSingleQuotedString => state = InsideSingleQuotedStringEscape,
                InsideDoubleQuotedString => state = InsideDoubleQuotedStringEscape,
                _ => {}
            },
            '\'' => {
                state = handle_quote_character(
                    &input_code,
                    state,
                    &idx,
                    "'''",
                    InsideSingleQuotedString,
                    InsideSingleQuotedStringEscape,
                    InsideSingleQuotedMultilineString,
                )
            }
            '\"' => {
                state = handle_quote_character(
                    &input_code,
                    state,
                    &idx,
                    "\"\"\"",
                    InsideDoubleQuotedString,
                    InsideDoubleQuotedStringEscape,
                    InsideDoubleQuotedMultilineString,
                )
            }
            '#' => match state {
                OutsideString => {
                    // we are not inside a string and are at the start of a comment
                    // => line ends already _before_ the hash character
                    line_end = Some(idx);
                    state = InsideComment;
                }
                _ => {}
            },
            '\n' => {
                if state == InsideSingleQuotedStringEscape
                    || state == InsideDoubleQuotedStringEscape
                {
                    // newline was escaped within string
                    continue;
                }
                if line_end.is_none() {
                    // no line comment => end line here (at newline)
                    line_end = Some(idx);
                }
                // note that this slice does not contain the newline character as
                // the newline character will be contained in the next slice (`start` points to the newline char)
                without_comments.push_str(&input_code[start..line_end.unwrap()]);
                start = idx;
                line_end = None;
                match state {
                    InsideSingleQuotedMultilineString | InsideDoubleQuotedMultilineString => {}
                    _ => state = OutsideString,
                }
            }
            _ => {
                if state == InsideSingleQuotedStringEscape {
                    state = InsideSingleQuotedString;
                } else if state == InsideDoubleQuotedStringEscape {
                    state = InsideDoubleQuotedString;
                }
            }
        }
    }
    if line_end.is_none() {
        // no line comment at the end
        // => copy all remaining code into the result
        line_end = Some(input_code.len());
    }
    without_comments.push_str(&input_code[start..line_end.unwrap()]);
    // this extra newline is actually not needed, but kept for backwards-compatibility
    without_comments.push('\n');
    without_comments.to_string()
}
