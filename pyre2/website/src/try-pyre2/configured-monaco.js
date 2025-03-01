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
import type FlowJsServices from './flow-services';
import flowLanguageConfiguration from './flow-configuration.json';

type Position = {lineNumber: number, column: number};

let autoCompleteFunctionForMonaco = (line: number, column: number,
): any => {
  throw 'not implemented';
};

function setAutoCompleteFunction(f: (_l: number, _c: number) => any): void {
  autoCompleteFunctionForMonaco = f;
}

let getDefFunctionForMonaco = (_l: number, _c: number): any => null;

function setGetDefFunction(f: (_l: number, _c: number) => any): void {
  getDefFunctionForMonaco = (l, c) => f(l, c)
}

let hoverFunctionForMonaco = (_l: number, _c: number): any => null;

function setHoverFunctionForMonaco(f: (_l: number, _c: number) => any): void {
  hoverFunctionForMonaco = f
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
      const result = autoCompleteFunctionForMonaco(position.lineNumber, position.column);
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
      const range = getDefFunctionForMonaco(position.lineNumber, position.column);
      return range != null ? { uri: model.uri, range } : null;
    } catch (e) {
      console.error(e);
      return null;
    }
  },
});
monaco.languages.registerHoverProvider('python', {
  provideHover(model, position) {
    const result = hoverFunctionForMonaco(position.lineNumber, position.column);
    return result;
  },
});
loader.config({monaco});

export {
  monaco,
  setAutoCompleteFunction,
  setGetDefFunction,
  setHoverFunctionForMonaco,
};
