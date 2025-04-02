/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, { useState, useEffect, useRef } from 'react';
import clsx from 'clsx';
import Editor from '@monaco-editor/react';
import * as LZString from 'lz-string';
import styles from './TryPyre2.module.css';
import TryPyre2Results from './TryPyre2Results';
import {
    monaco,
    setAutoCompleteFunction,
    setGetDefFunction,
    setHoverFunctionForMonaco,
    setInlayHintFunctionForMonaco,
} from './configured-monaco';
import type { editor } from 'monaco-editor';
import type { PyreflyErrorMessage } from './TryPyre2Results';

const DEFAULT_PYTHON_PROGRAM = `
from typing import *

def test(x: int):
  return f"{x}"

# reveal_type will produce a type error that tells you the type Pyrefly has
# computed for the return (in this case, str)
reveal_type(test(42))
`.trimStart();

// Import type for Pyre2 State
interface Pyre2State {
    updateSource: (source: string) => void;
    getErrors: () => ReadonlyArray<PyreflyErrorMessage>;
    autoComplete: (line: number, column: number) => any;
    gotoDefinition: (line: number, column: number) => any;
    queryType: (line: number, column: number) => any;
    inlayHint: () => any;
}

const pyre2WasmUninitializedPromise =
    typeof window !== 'undefined'
        ? import('./pyrefly_wasm')
        : new Promise<any>(_resolve => { });

const pyre2WasmInitializedPromise = pyre2WasmUninitializedPromise
    .then(async mod => {
        await mod.default();
        return mod;
    })
    .catch(e => console.log(e));

interface TryPyre2Props {
    sampleFilename: string;
    isCodeSnippet?: boolean;
    codeSample?: string;
    showErrorPanel?: boolean;
}

export default function TryPyre2({
    sampleFilename,
    isCodeSnippet = false,
    codeSample = DEFAULT_PYTHON_PROGRAM,
    showErrorPanel = true,
}: TryPyre2Props): React.ReactElement {
    const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
    const [errors, setErrors] = useState<ReadonlyArray<PyreflyErrorMessage> | null>(
        []
    );
    const [internalError, setInternalError] = useState('');
    const [loading, setLoading] = useState(true);
    const [pyreService, setPyreService] = useState<Pyre2State | null>(null);
    const [editorHeightforCodeSnippet, setEditorHeightforCodeSnippet] = useState<
        number | null
    >(null);
    const [model, setModel] = useState<editor.ITextModel | null>(null);
    const [isCopied, setIsCopied] = useState(false);

    // Only run for initial render, and not on subsequent updates
    useEffect(() => {
        setLoading(true);
        pyre2WasmInitializedPromise
            .then(pyre2 => {
                setPyreService(new pyre2.State());
                setLoading(false);
                setInternalError('');
            })
            .catch(e => {
                setLoading(false);
                setInternalError(JSON.stringify(e));
            });
    }, []);

    // Need to add createModel handler in case monaco model was not created at mount time
    monaco.editor.onDidCreateModel(model => {
        const curModel = fetchCurMonacoModelAndTriggerUpdate(sampleFilename);
        setModel(curModel);
        forceRecheck();
    });

    // Recheck when pyre service or model changes
    useEffect(() => {
        forceRecheck();
    }, [pyreService, model]);

    function forceRecheck() {
        if (model == null || pyreService == null) return;
        const value = model.getValue();

        setAutoCompleteFunction(model, (l: number, c: number) => pyreService.autoComplete(l, c));
        setGetDefFunction(model, (l: number, c: number) => pyreService.gotoDefinition(l, c));
        setHoverFunctionForMonaco(model, (l: number, c: number) => pyreService.queryType(l, c));
        setInlayHintFunctionForMonaco(model, () => pyreService.inlayHint());

        // typecheck on edit
        try {
            pyreService.updateSource(model.getValue());
            const errors = pyreService.getErrors() as ReadonlyArray<PyreflyErrorMessage>;
            monaco.editor.setModelMarkers(model, 'default', mapPyreflyErrorsToMarkerData(errors));
            setInternalError('');
            setErrors(errors);
        } catch (e) {
            console.error(e);
            setInternalError(JSON.stringify(e));
            setErrors([]);
        }
    }

    function onEditorMount(editor: editor.IStandaloneCodeEditor) {
        const model = fetchCurMonacoModelAndTriggerUpdate(sampleFilename);
        setModel(model);

        if (isCodeSnippet) {
            setEditorHeightforCodeSnippet(Math.max(50, editor.getContentHeight()));
        }
        editorRef.current = editor;
    }

    const handleGoToDefFromErrors = (
        startLineNumber: number,
        startColumn: number,
        endLineNumber: number,
        endColumn: number,
    ) => {
        const editor = editorRef.current;
        if (editor === null) {
            return;
        }
        const range = { startLineNumber, startColumn, endLineNumber, endColumn };

        editor.revealRange(range);
        editor.setSelection(range);
    };

    return (
        <div className={styles.tryEditor}>
            <div className={styles.codeEditorContainer}>
                {getPyre2Editor(
                    isCodeSnippet,
                    sampleFilename,
                    codeSample,
                    forceRecheck,
                    onEditorMount,
                    editorHeightforCodeSnippet,
                )}
                {!isCodeSnippet && (
                    <button
                        className={clsx(
                            styles.shareButton,
                            isCopied && styles.shareButtonCopied,
                        )}
                        onClick={() => copyToClipboard(setIsCopied)}
                        aria-label="share URL button">
                        <span className={styles.shareButtonText}>
                            {isCopied ? '✓ URL Copied!' : '📋 Share URL'}
                        </span>
                    </button>
                )}
            </div>
            {showErrorPanel && (
                <TryPyre2Results
                    loading={loading}
                    goToDef={handleGoToDefFromErrors}
                    errors={errors}
                    internalError={internalError}
                />
            )}
        </div>
    );
}

function updateURL(code: string): void {
    const compressed = LZString.compressToEncodedURIComponent(code);
    const newURL = `${window.location.pathname}?code=${compressed}`;
    window.history.replaceState({}, '', newURL);
}

function copyToClipboard(setIsCopied: (copied: boolean) => void): void {
    const currentURL = window.location.href;
    navigator.clipboard.writeText(currentURL).then(() => {
        setIsCopied(true);
        setTimeout(() => setIsCopied(false), 2000);
    });
}

function getCodeFromURL(): string | null {
    if (typeof window === 'undefined') return null;
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');
    return code ? LZString.decompressFromEncodedURIComponent(code) : null;
}

function fetchCurMonacoModelAndTriggerUpdate(fileName: string): editor.ITextModel | null {
    const model = monaco.editor
        .getModels()
        .filter(model => model?.uri?.path === `/${fileName}`)[0];

    if (model == null) {
        return null;
    }

    const codeFromUrl = getCodeFromURL();
    if (codeFromUrl != null && model != null) {
        model.setValue(codeFromUrl);
    }

    // Force update to trigger initial inlay hint
    model.setValue(model.getValue());

    // Ensure tab size is correctly set
    model.updateOptions({ tabSize: 4, insertSpaces: true });

    return model;
}

function isMobile(): boolean {
    return /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
}

function getPyre2Editor(
    isCodeSnippet: boolean,
    fileName: string,
    codeSample: string,
    forceRecheck: () => void,
    onEditorMount: (editor: editor.IStandaloneCodeEditor) => void,
    editorHeightforCodeSnippet: number | null,
): React.ReactElement {
    if (isCodeSnippet) {
        return (
            <Editor
                defaultPath={fileName}
                defaultValue={codeSample}
                defaultLanguage="python"
                theme="vs-light"
                onChange={forceRecheck}
                onMount={onEditorMount}
                keepCurrentModel={true}
                height={editorHeightforCodeSnippet}
                options={{
                    readOnly: isMobile(),
                    minimap: { enabled: false },
                    hover: { enabled: true, above: false },
                    scrollBeyondLastLine: false,
                    overviewRulerBorder: false,
                }}
            />
        );
    } else {
        // TODO (T217559369): Instead of manually calculating the sandbox height, we should
        // use flexbox behavior to make the sandbox height to be 75% of the screen
        // This doesn't seem to work with the monaco editor currently.
        const screenHeight = window.innerHeight;
        const navbarElement = document.querySelector('.navbar') as HTMLElement | null; // Replace with your navbar selector
        const navbarHeight = navbarElement?.offsetHeight || 0;

        const sandboxHeight = ((screenHeight - navbarHeight) * 75) / 100;

        return (
            <Editor
                defaultPath={fileName}
                defaultValue={codeSample}
                defaultLanguage="python"
                theme="vs-light"
                onChange={value => {
                    forceRecheck();
                    if (typeof value === 'string') {
                        updateURL(value);
                    }
                }}
                onMount={onEditorMount}
                keepCurrentModel={true}
                height={sandboxHeight}
                options={{
                    minimap: { enabled: false },
                    hover: { enabled: true, above: false },
                    scrollBeyondLastLine: false,
                    overviewRulerBorder: false,
                }}
            />
        );
    }
}


/**
 * Maps PyreflyErrorMessage array to Monaco editor IMarkerData array
 */
function mapPyreflyErrorsToMarkerData(errors: ReadonlyArray<PyreflyErrorMessage>): editor.IMarkerData[] {
    return errors.map(error => ({
        startLineNumber: error.startLineNumber,
        startColumn: error.startColumn,
        endLineNumber: error.endLineNumber,
        endColumn: error.endColumn,
        message: error.message,
        severity: error.severity
    }));
}
