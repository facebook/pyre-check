/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::error::error::Error;

#[derive(Default, Clone, Debug)]
pub struct Expectation {
    error: Vec<(usize, String)>,
}

impl Expectation {
    fn parse_line(&mut self, line_no: usize, mut s: &str) {
        while let Some((prefix, err)) = s.trim().rsplit_once("# E: ") {
            self.error.push((line_no, err.trim().to_owned()));
            s = prefix.trim_end();
        }
    }

    pub fn parse(s: &str) -> Self {
        let mut res = Self::default();
        for (line_no, line) in s.lines().enumerate() {
            res.parse_line(line_no + 1, line)
        }
        res
    }

    pub fn check(&self, errors: &[Error]) -> anyhow::Result<()> {
        if self.error.len() != errors.len() {
            Err(anyhow::anyhow!(
                "Expected {} errors, but got {}",
                self.error.len(),
                errors.len(),
            ))
        } else {
            for (line_no, msg) in &self.error {
                if !errors
                    .iter()
                    .any(|e| e.msg().contains(msg) && e.source_range().start.row.get() == *line_no)
                {
                    return Err(anyhow::anyhow!("Can't find error (line {line_no}): {msg}"));
                }
            }
            Ok(())
        }
    }
}
