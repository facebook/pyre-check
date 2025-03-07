/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as monaco from 'monaco-editor';
import {loader} from '@monaco-editor/react';
import flowLanguageConfiguration from './flow-configuration.json';

type Position = {lineNumber: number, column: number};

const defaultAutoCompleteFunctionForMonaco = (line: number, column: number): any => {
  throw 'not implemented';
};
const autoCompleteFunctionsForMonaco = new Map<any, typeof defaultAutoCompleteFunctionForMonaco>();

function setAutoCompleteFunction(model: any, f: (_l: number, _c: number) => any): void {
  autoCompleteFunctionsForMonaco.set(model, f);
}

const defaultGetDefFunctionForMonaco = (_l: number, _c: number): any => null;
const getDefFunctionsForMonaco = new Map<any, typeof defaultGetDefFunctionForMonaco>();

function setGetDefFunction(model: any, f: (_l: number, _c: number) => any): void {
  getDefFunctionsForMonaco.set(model, f);
}

let defaultHoverFunctionForMonaco = (_l: number, _c: number): any => null;
const hoverFunctionsForMonaco = new Map<any, typeof defaultHoverFunctionForMonaco>();

function setHoverFunctionForMonaco(model: any, f: (_l: number, _c: number) => any): void {
  hoverFunctionsForMonaco.set(model, f);
}

const defaultInlayHintFunctionForMonaco = (): any => [];
const inlayHintFunctionsForMonaco = new Map<any, typeof defaultInlayHintFunctionForMonaco>();

function setInlayHintFunctionForMonaco(model: any, f: () => any): void {
  inlayHintFunctionsForMonaco.set(model, f);
}

monaco.languages.register({
  id: 'python',
  extensions: ['.py'],
  aliases: ['Python'],
});
monaco.languages.setLanguageConfiguration('python', flowLanguageConfiguration);
const languageId = monaco.languages.getEncodedLanguageId('python');

monaco.languages.registerCompletionItemProvider('python', {
  triggerCharacters: [
    '.',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '[',
    '"',
    "'",
  ],

  provideCompletionItems(model, position) {
    try {
      const f = autoCompleteFunctionsForMonaco.get(model) ?? defaultAutoCompleteFunctionForMonaco;
      const result = f(
        position.lineNumber,
        position.column,
      );
      console.log('completion', position, result);
      return {suggestions: result.map(r => ({...r, insertText: r.label}))};
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
monaco.languages.registerDefinitionProvider('python', {
  provideDefinition(model, position) {
    try {
      const f = getDefFunctionsForMonaco.get(model) ?? defaultGetDefFunctionForMonaco;
      const range = f(
        position.lineNumber,
        position.column,
      );
      return range != null ? {uri: model.uri, range} : null;
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
monaco.languages.registerHoverProvider('python', {
  provideHover(model, position) {
    const f = hoverFunctionsForMonaco.get(model) ?? defaultHoverFunctionForMonaco;
    const result = f(position.lineNumber, position.column);
    return result;
  },
});
monaco.languages.registerInlayHintsProvider('python', {
  provideInlayHints(model) {
    const f = inlayHintFunctionsForMonaco.get(model) ?? defaultInlayHintFunctionForMonaco;
    const hints = f();
    return {hints};
  },
});
loader.config({monaco});
export {
  monaco,
  setAutoCompleteFunction,
  setGetDefFunction,
  setHoverFunctionForMonaco,
  setInlayHintFunctionForMonaco,
};
