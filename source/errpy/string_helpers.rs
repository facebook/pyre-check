/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Default)]
pub struct StringCategory {
    pub is_byte: bool,
    pub is_raw: bool,
    pub is_format: bool,
    pub is_unicode: bool,
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct InvalidStringCategoryError {
    pub invalid_prefix: String,
}

/// Return a string containing all characters prior to the first ' or "
pub fn string_prefix(string_contents: &str) -> String {
    let mut prefix: String = String::from("");
    for ch in string_contents.chars() {
        if ch == '\'' || ch == '"' {
            break;
        }
        prefix.push(ch);
    }
    prefix
}

/// categorize_string will return a StringCategory
/// containing the type of string which has been passed to the function
/// i.e. a byte, format, unicode or raw string as signified by the prefix
/// of the string, e.g. `b"a byte string"` is a byte string etc.
/// If an invalid combination of prefix is provided then this is returned
/// as a InvalidStringCategoryError for processing by the caller
pub fn categorize_string(
    to_categorize: &str,
) -> Result<StringCategory, InvalidStringCategoryError> {
    // Only valid prefixes are:
    // "r" | "u" | "f" | "b"
    // "fr" | "rf"
    // "br"| "rb"
    let mut prefix = string_prefix(to_categorize);

    prefix = prefix.to_lowercase();
    match prefix.len() {
        1 => match prefix.as_str() {
            "r" => Ok(StringCategory {
                is_raw: true,
                ..Default::default()
            }),
            "u" => Ok(StringCategory {
                is_unicode: true,
                ..Default::default()
            }),
            "f" => Ok(StringCategory {
                is_format: true,
                ..Default::default()
            }),
            "b" => Ok(StringCategory {
                is_byte: true,
                ..Default::default()
            }),
            _ => Err(InvalidStringCategoryError {
                invalid_prefix: prefix,
            }),
        },
        2 => match prefix.as_str() {
            "fr" | "rf" => Ok(StringCategory {
                is_format: true,
                is_raw: true,
                ..Default::default()
            }),
            "br" | "rb" => Ok(StringCategory {
                is_byte: true,
                is_raw: true,
                ..Default::default()
            }),
            _ => Err(InvalidStringCategoryError {
                invalid_prefix: prefix,
            }),
        },
        0 => Ok(StringCategory {
            ..Default::default()
        }),
        _ => Err(InvalidStringCategoryError {
            invalid_prefix: prefix,
        }),
    }
}
