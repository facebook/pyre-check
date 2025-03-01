/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyre2::playground::LanguageServiceState;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct State(LanguageServiceState);

#[wasm_bindgen]
impl State {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self(LanguageServiceState::default())
    }

    #[wasm_bindgen(js_name=updateSource)]
    pub fn update_source(&mut self, source: String) {
        self.0.update_source(source)
    }

    #[wasm_bindgen(js_name=getErrors)]
    pub fn get_errors(&self) -> JsValue {
        serde_wasm_bindgen::to_value(&self.0.get_errors()).unwrap_or(JsValue::NULL)
    }

    #[wasm_bindgen(js_name=queryType)]
    pub fn query_type(&mut self, line: i32, column: i32) -> JsValue {
        self.0
            .query_type(line, column)
            .map(|result| serde_wasm_bindgen::to_value(&result).unwrap())
            .unwrap_or(JsValue::NULL)
    }

    #[wasm_bindgen(js_name=gotoDefinition)]
    pub fn goto_definition(&mut self, line: i32, column: i32) -> JsValue {
        self.0
            .goto_definition(line, column)
            .map(|result| serde_wasm_bindgen::to_value(&result).unwrap())
            .unwrap_or(JsValue::NULL)
    }

    #[wasm_bindgen(js_name=autoComplete)]
    pub fn autocomplete(&mut self, line: i32, column: i32) -> JsValue {
        serde_wasm_bindgen::to_value(&self.0.autocomplete(line, column)).unwrap()
    }
}
