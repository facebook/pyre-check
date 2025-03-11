/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState, useEffect, useRef, type MixedElement} from 'react';
import {useBaseUrlUtils} from '@docusaurus/useBaseUrl';
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
import BrowserOnly from '@docusaurus/BrowserOnly';
import type {PyreflyErrorMessage} from './TryPyre2Results';

const DEFAULT_PYTHON_PROGRAM = `
from typing import *

def test(x: int):
  return f"{x}"

# reveal_type will produce a type error that tells you the type Pyrefly has
# computed for the return (in this case, str)
reveal_type(test(42))
`.trimStart();

const pyre2WasmUninitializedPromise =
  // $FlowIgnore[cannot-resolve-name]
  typeof window !== 'undefined'
    ? // $FlowIgnore[cannot-resolve-module]
      import('./pyre2_wasm')
    : new Promise(_resolve => {});

const pyre2WasmInitializedPromise = pyre2WasmUninitializedPromise
  .then(async mod => {
    // $FlowIgnore[prop-missing]
    await mod.default();
    return mod;
  })
  .catch(e => console.log(e));

function isMobile(): boolean {
  return /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
}

export default component TryPyre2(
  sampleFilename: string,
  isCodeSnippet: boolean = false,
  codeSample: string = DEFAULT_PYTHON_PROGRAM,
  showErrorPanel: boolean = true,
) {
  const editorRef = useRef(null);
  const [errors, setErrors] = useState<?$ReadOnlyArray<PyreflyErrorMessage>>(
    [],
  );
  const [internalError, setInternalError] = useState('');
  const [loading, setLoading] = useState(true);
  const [pyreService, setPyreService] = useState<any>(null);
  const [editorHeightforCodeSnippet, setEditorHeightforCodeSnippet] = useState<
    number | null,
  >(null);
  const [model, setModel] = useState(null);

  useEffect(() => {
    setLoading(true);
    pyre2WasmInitializedPromise
      .then(pyre2 => {
        // $FlowIgnore[incompatible-use]
        // $FlowIgnore[invalid-constructor]
        // $FlowIgnore[prop-missing]
        setPyreService(new pyre2.State());
        setLoading(false);
        setInternalError('');
      })
      .catch(e => {
        setLoading(false);
        setInternalError(JSON.stringify(e));
      });
  }, []);

  function fetchCurMonacoModelAndTriggerUpdate() {
    const model = monaco.editor
      .getModels()
      .filter(model => model?.uri?.path === `/${sampleFilename}`)[0];

    if (model != null) {
      // Force update to trigger initial inlay hint
      model.setValue(model.getValue());
    }

    return model;
  }

  // Need to add createModel handler in case monaco model was not created at mount time
  monaco.editor.onDidCreateModel(model => {
    const curModel = fetchCurMonacoModelAndTriggerUpdate();
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

    setAutoCompleteFunction(model, (l, c) => pyreService.autoComplete(l, c));
    setGetDefFunction(model, (l, c) => pyreService.gotoDefinition(l, c));
    setHoverFunctionForMonaco(model, (l, c) => pyreService.queryType(l, c));
    setInlayHintFunctionForMonaco(model, () => pyreService.inlayHint());

    // typecheck on edit
    try {
      pyreService.updateSource(model.getValue());
      const errors =
        pyreService.getErrors() as ?$ReadOnlyArray<PyreflyErrorMessage>;
      monaco.editor.setModelMarkers(model, 'default', pyreService.getErrors());
      setInternalError('');
      setErrors(errors);
    } catch (e) {
      console.error(e);
      setInternalError(JSON.stringify(e));
      setErrors([]);
    }
  }

  function onEditorMount(editor: any) {
    const model = fetchCurMonacoModelAndTriggerUpdate();
    setModel(model);

    if (isCodeSnippet) {
      setEditorHeightforCodeSnippet(Math.max(50, editor.getContentHeight()));
    }
    editorRef.current = editor;
  }

  let editor = null;
  if (isCodeSnippet) {
    editor = (
      <Editor
        defaultPath={sampleFilename}
        defaultValue={codeSample}
        defaultLanguage="python"
        theme="vs-light"
        onChange={forceRecheck}
        onMount={onEditorMount}
        height={editorHeightforCodeSnippet}
        options={{
          readOnly: isMobile(),
          minimap: {enabled: false},
          hover: {enabled: true, above: false},
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
    const sandboxHeight = (screenHeight * 75) / 100;

    editor = (
      <Editor
        defaultPath={sampleFilename}
        defaultValue={codeSample}
        defaultLanguage="python"
        theme="vs-light"
        onChange={forceRecheck}
        onMount={onEditorMount}
        height={sandboxHeight}
        options={{
          minimap: {enabled: false},
          hover: {enabled: true, above: false},
          scrollBeyondLastLine: false,
          overviewRulerBorder: false,
        }}
      />
    );
  }
  return (
    <div className={styles.tryEditor}>
      <div className={styles.codeEditorContainer}>
        <div className={styles.codeEditor}>{editor}</div>
      </div>
      {showErrorPanel && (
        <div className={styles.resultsContainer}>
          <TryPyre2Results
            loading={loading}
            errors={errors}
            internalError={internalError}
          />
        </div>
      )}
    </div>
  );
}
