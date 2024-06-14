/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use parser_pre_process::remove_comments;

fn test_harness(input_code: &str, expected: &str) {
    let actual = remove_comments(input_code.to_string());
    assert_eq!(expected.to_string(), actual);
}

#[test]
fn test_simple() {
    test_harness("a = 8", "a = 8\n");
    test_harness("a = 8 # a comment", "a = 8 \n");
}

#[test]
fn test_strings_simple() {
    test_harness("'#a'", "'#a'\n");
    test_harness("\"#a\"", "\"#a\"\n");
    test_harness("\"a\" #a", "\"a\" \n");
    test_harness("'a' #a", "'a' \n");
}

#[test]
fn test_strings_complex() {
    test_harness("'#a' #b", "'#a' \n");
    test_harness("'''\n#a\n'''", "'''\n#a\n'''\n");
    test_harness("\"\"\"\n#a\n\"\"\"", "\"\"\"\n#a\n\"\"\"\n");
}

#[test]
fn test_strings_escaping() {
    test_harness("'#a\\' #b'", "'#a\\' #b'\n");
    test_harness("\"#a\\\" #b\"", "\"#a\\\" #b\"\n");
    test_harness("'#a\\\n#b'", "'#a\\\n#b'\n");
    test_harness("\"#a\\\n#b\"", "\"#a\\\n#b\"\n");
}

#[test]
fn test_pre_comments() {
    // we expect just newlines in place of comments
    // to preserve line and column information
    test_harness(
        "# comment before
# the main body of code
a = 8",
        "\n\na = 8\n",
    );
}

#[test]
fn test_remove_comments_middle_of_def() {
    // comments can appear in weird places, like this which are stripped out...
    // such as in a tuple definition
    test_harness(
        "a = (# comment in strange place
1 , 2, 3)",
        "a = (\n1 , 2, 3)\n",
    );

    // or in a function definition
    test_harness(
        "def foo(
    a=34,
    # a comment here
    skip_loads=True,
):
    pass
",
        "def foo(\n    a=34,\n    \n    skip_loads=True,\n):\n    pass\n\n",
    );
}

#[test]
fn test_remove_post_plus() {
    test_harness(
        "verification_report += '\\n #' + '#'
if not self.partition_fbig_col_wipeout_map:
    pass",
        "verification_report += '\\n #' + '#'\nif not self.partition_fbig_col_wipeout_map:\n    pass\n",
    );
}
