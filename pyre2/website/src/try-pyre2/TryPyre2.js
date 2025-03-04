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
import TryPyre2ConfigEditor from './TryPyre2ConfigEditor';
import TryPyre2Results from './TryPyre2Results';
import {
  monaco,
  setAutoCompleteFunction,
  setGetDefFunction,
  setHoverFunctionForMonaco,
} from './configured-monaco';
import FlowJsServices from './flow-services';
import flowLanguageConfiguration from './flow-configuration.json';

const TRY_FLOW_LAST_CONTENT_STORAGE_KEY = 'TryPyre2LastContent';
const DEFAULT_PYTHON_PROGRAM = `
# Pyre is being run in gradual typing mode: https://pyre-check.org/docs/types-in-python/#gradual-typing
# Use the \`# pyre-strict\` header to run in strict mode, which requires annotations.

from typing import *

# reveal_type will produce a type error that tells you the type Pyre has
# computed for the argument (in this case, int)
def test(x: int) -> str:
  return f"{x}"

reveal_type(test(42))

`;

const pyre2WasmUninitializedPromise =
  typeof window !== 'undefined'
    ? import('./pyre2_wasm')
    : new Promise(_resolve => {});

const pyre2WasmInitializedPromise = pyre2WasmUninitializedPromise
  .then(async mod => {
    await mod.default();
    return mod;
  })
  .catch(e => console.log(e));

export default component TryPyre2(
  defaultFlowVersion: string,
  flowVersions: $ReadOnlyArray<string>,
  editorHeight: number,
  codeSample: string,
) {
  const {withBaseUrl} = useBaseUrlUtils();
  const editorRef = useRef(null);
  const [internalError, setInternalError] = useState('');
  const [loading, setLoading] = useState(true);
  const [pyreService, setPyreService] = useState<any>(null);

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

  useEffect(() => {
    forceRecheck();
  }, [pyreService]);

  function forceRecheck() {
    const model = monaco.editor.getModels()[0];
    if (model == null || pyreService == null) return;
    const value = model.getValue();

    setAutoCompleteFunction((l, c) => pyreService.autoComplete(l, c));
    setGetDefFunction((l, c) => pyreService.gotoDefinition(l, c));
    setHoverFunctionForMonaco((l, c) => pyreService.queryType(l, c));

    // typecheck on edit
    try {
      pyreService.updateSource(model.getValue());
      monaco.editor.setModelMarkers(model, 'default', pyreService.getErrors());
      setInternalError('');
    } catch (e) {
      console.error(e);
      setInternalError(JSON.stringify(e));
    }
  }

  function onMount(editor: any) {
    forceRecheck();

    editorRef.current = editor;
  }

  const height = editorHeight || '600px';

  return (
    <div className={styles.tryEditor}>
      <div className={styles.code}>
        <div className={styles.editorContainer}>
          <Editor
            defaultValue={codeSample || DEFAULT_PYTHON_PROGRAM}
            defaultLanguage="python"
            theme="vs-light"
            height={height}
            onChange={forceRecheck}
            onMount={onMount}
            options={{
              minimap: {enabled: false},
              hover: {enabled: true, above: false},
              scrollBeyondLastLine: false,
              overviewRulerBorder: false,
            }}
          />
        </div>
      </div>
    </div>
  );
}
