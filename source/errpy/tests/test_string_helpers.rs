/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use string_helpers::categorize_string;
use string_helpers::InvalidStringCategoryError;
use string_helpers::StringCategory;

fn test_categorize_string(
    input_string: &str,
    expected_categorization: StringCategory,
) -> Result<(), InvalidStringCategoryError> {
    let got_categorization = categorize_string(input_string)?;

    assert_eq!(expected_categorization, got_categorization);
    Ok(())
}
fn test_invalid_categorize_string(
    input_string: &str,
    expected_categorization: InvalidStringCategoryError,
) -> Result<(), InvalidStringCategoryError> {
    let got_categorization = categorize_string(input_string);

    assert_eq!(Err(expected_categorization), got_categorization);
    Ok(())
}

#[test]
fn test_simple_1_char_prefix() -> Result<(), InvalidStringCategoryError> {
    test_categorize_string(
        "r'astring'",
        StringCategory {
            is_raw: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "u'astring'",
        StringCategory {
            is_unicode: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "f'astring'",
        StringCategory {
            is_format: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "b'astring'",
        StringCategory {
            is_byte: true,
            ..Default::default()
        },
    )?;
    Ok(())
}

#[test]
fn test_2_char_prefix() -> Result<(), InvalidStringCategoryError> {
    test_categorize_string(
        "fr'astring'",
        StringCategory {
            is_raw: true,
            is_format: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "rf'astring'",
        StringCategory {
            is_raw: true,
            is_format: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "br'astring'",
        StringCategory {
            is_byte: true,
            is_raw: true,
            ..Default::default()
        },
    )?;
    test_categorize_string(
        "rb'astring'",
        StringCategory {
            is_raw: true,
            is_byte: true,
            ..Default::default()
        },
    )?;
    Ok(())
}

#[test]
fn test_capitalized_prefix() -> Result<(), InvalidStringCategoryError> {
    test_categorize_string(
        "B'astring'",
        StringCategory {
            is_byte: true,
            ..Default::default()
        },
    )?;
    Ok(())
}

#[test]
fn test_nothing_strage_with_double_quote() -> Result<(), InvalidStringCategoryError> {
    test_categorize_string(
        "B\"astring\"",
        StringCategory {
            is_byte: true,
            ..Default::default()
        },
    )?;
    Ok(())
}

#[test]
fn test_no_prefix() -> Result<(), InvalidStringCategoryError> {
    test_categorize_string(
        "'astring'",
        StringCategory {
            ..Default::default()
        },
    )?;
    Ok(())
}

#[test]
fn test_obvious_inavlid_prefix() -> Result<(), InvalidStringCategoryError> {
    test_invalid_categorize_string(
        "s'astring'",
        InvalidStringCategoryError {
            invalid_prefix: "s".to_string(),
        },
    )?;
    test_invalid_categorize_string(
        "sasdAsd'astring'",
        InvalidStringCategoryError {
            invalid_prefix: "sasdasd".to_string(),
        },
    )?;
    Ok(())
}

#[test]
fn test_inavlid_prefix_combination() -> Result<(), InvalidStringCategoryError> {
    // 'raw' 'unicode' string doesnt exist in Python
    test_invalid_categorize_string(
        "ur'astring'",
        InvalidStringCategoryError {
            invalid_prefix: "ur".to_string(),
        },
    )?;
    Ok(())
}
