/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as monaco from 'monaco-editor';
import { default as MonacoEditor, loader } from '@monaco-editor/react';
import flowLanguageConfiguration from './flow-configuration.json';
import type { PyreflyErrorMessage } from './TryPyre2Results';

type CompletionItem = monaco.languages.CompletionItem;
type Range = monaco.IRange;
type Hover = monaco.languages.Hover;
type InlayHint = monaco.languages.InlayHint;

type AutoCompleteFunction = (line: number, column: number) => CompletionItem[];
type GetDefFunction = (line: number, column: number) => Range | null;
type HoverFunction = (line: number, column: number) => Hover | null;
type InlayHintFunction = () => InlayHint[];

const defaultAutoCompleteFunctionForMonaco: AutoCompleteFunction = (
    _line: number,
    _column: number,
): CompletionItem[] => {
    throw 'not implemented';
};

const autoCompleteFunctionsForMonaco = new Map<
    monaco.editor.ITextModel,
    AutoCompleteFunction
>();

function setAutoCompleteFunction(
    model: monaco.editor.ITextModel,
    f: AutoCompleteFunction,
): void {
    autoCompleteFunctionsForMonaco.set(model, f);
}

const defaultGetDefFunctionForMonaco: GetDefFunction = (_l: number, _c: number): Range | null => null;
const getDefFunctionsForMonaco = new Map<
    monaco.editor.ITextModel,
    GetDefFunction
>();

function setGetDefFunction(
    model: monaco.editor.ITextModel,
    f: GetDefFunction,
): void {
    getDefFunctionsForMonaco.set(model, f);
}

const defaultHoverFunctionForMonaco: HoverFunction = (_l: number, _c: number): Hover | null => null;
const hoverFunctionsForMonaco = new Map<
    monaco.editor.ITextModel,
    HoverFunction
>();

function setHoverFunctionForMonaco(
    model: monaco.editor.ITextModel,
    f: HoverFunction,
): void {
    hoverFunctionsForMonaco.set(model, f);
}

const defaultInlayHintFunctionForMonaco: InlayHintFunction = (): InlayHint[] => [];
const inlayHintFunctionsForMonaco = new Map<
    monaco.editor.ITextModel,
    InlayHintFunction
>();

function setInlayHintFunctionForMonaco(
    model: monaco.editor.ITextModel,
    f: InlayHintFunction
): void {
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
            const f =
                autoCompleteFunctionsForMonaco.get(model) ??
                defaultAutoCompleteFunctionForMonaco;
            const result = f(position.lineNumber, position.column);
            return {
                suggestions: result.map((r) => ({ ...r, insertText: r.label.toString() }))
            };
        } catch (e) {
            console.error(e);
            return null;
        }
    },
});

monaco.languages.registerDefinitionProvider('python', {
    provideDefinition(model, position) {
        try {
            const f =
                getDefFunctionsForMonaco.get(model) ?? defaultGetDefFunctionForMonaco;
            const range = f(position.lineNumber, position.column);
            return range != null ? { uri: model.uri, range } : null;
        } catch (e) {
            console.error(e);
            return null;
        }
    },
});

monaco.languages.registerHoverProvider('python', {
    provideHover(model, position) {
        const f =
            hoverFunctionsForMonaco.get(model) ?? defaultHoverFunctionForMonaco;
        const result = f(position.lineNumber, position.column);
        return result;
    },
});

monaco.languages.registerInlayHintsProvider('python', {
    provideInlayHints(model) {
        const f =
            inlayHintFunctionsForMonaco.get(model) ??
            defaultInlayHintFunctionForMonaco;
        const hints = f();
        return { hints, dispose: () => { } };
    },
});

// Monaco editor types are now properly handled
loader.config({ monaco });

export {
    monaco,
    setAutoCompleteFunction,
    setGetDefFunction,
    setHoverFunctionForMonaco,
    setInlayHintFunctionForMonaco,
};
