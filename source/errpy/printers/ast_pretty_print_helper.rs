/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::min;

pub struct PrintHelper<'a> {
    pub pprint_output: &'a mut String,
    ident_amount: usize,
    ident_current: usize,
    ident_str: String,
    ignore_next_n_chars: usize,
}

impl PrintHelper<'_> {
    pub fn new(pprint_output: &'_ mut String, ident_amount: usize) -> PrintHelper {
        PrintHelper {
            pprint_output,
            ident_amount,
            ident_current: 0,
            ident_str: "".to_string(),
            ignore_next_n_chars: 0,
        }
    }

    pub fn ignore_next_n_chars(&mut self, ignore_next_n_chars: usize) {
        self.ignore_next_n_chars += ignore_next_n_chars;
    }

    pub fn push_str(&mut self, to_push: &str) {
        if self.ignore_next_n_chars > 0 {
            // cut up to ignore_next_n_chars chars from input, if not enough to remove in one go
            // cut remainder from next call to push_str
            let to_cut = min(to_push.len(), self.ignore_next_n_chars);
            self.pprint_output.push_str(&to_push[to_cut..]);
            self.ignore_next_n_chars -= to_cut;
        } else {
            self.pprint_output.push_str(to_push);
        }
    }

    pub fn push_ident(&mut self) {
        if self.ident_current > 0 {
            self.pprint_output.push_str(&self.ident_str);
        }
    }

    pub fn pop(&mut self) {
        self.pprint_output.pop();
    }

    pub fn pop_many(&mut self, topop: usize) {
        match topop {
            1 => {
                self.pprint_output.pop();
            }
            _ => {
                self.pprint_output
                    .truncate(self.pprint_output.len() - topop);
            }
        };
    }

    pub fn inc_ident(&mut self) {
        self.ident_current += self.ident_amount;
        self.inc_calc();
    }

    pub fn dec_ident(&mut self) {
        self.ident_current -= self.ident_amount;
        self.inc_calc();
    }

    fn inc_calc(&mut self) {
        self.ident_str = " ".repeat(self.ident_current);
    }

    pub fn current_length(&mut self) -> usize {
        self.pprint_output.len()
    }

    pub fn insert_at(&mut self, at_point: usize, what: &str) {
        self.pprint_output.insert_str(at_point, what);
    }

    pub fn substring_contains(&mut self, at_point: usize, what: &str) -> bool {
        self.pprint_output[at_point..].contains(what)
    }

    pub fn strip_all_but_one_trailing_newline(&mut self) {
        let mut offset_from_end = 0;
        for last_char in self.pprint_output.chars().rev() {
            if last_char != '\n' {
                break;
            }

            offset_from_end += 1;
        }

        if offset_from_end > 1 {
            self.pprint_output
                .truncate(self.pprint_output.len() - offset_from_end + 1);
        }
    }
}
