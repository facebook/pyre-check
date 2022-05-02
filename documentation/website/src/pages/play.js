/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useEffect, useState} from 'react';
import Layout from '@theme/Layout';
import classnames from 'classnames';
import Link from '@docusaurus/Link';
import ExecutionEnvironment from '@docusaurus/ExecutionEnvironment';

import {Controlled as CodeMirror} from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import styles from './styles.module.css';

if (ExecutionEnvironment.canUseDOM) {
  require('codemirror/mode/python/python');
}


const DEFAULT_INITIAL_CODE = `# Pyre is being run in gradual typing mode: https://pyre-check.org/docs/types-in-python/#gradual-typing
# Use the \`# pyre-strict\` header to run in strict mode, which requires annotations.

from typing import *

# reveal_type will produce a type error that tells you the type Pyre has
# computed for the argument (in this case, int)
reveal_type(1)
`


function Code(props) {
  return (
    <div style={{border: '1px solid lightgrey'}}>
      <CodeMirror
        value={props.code}
        options={{
          mode: 'python',
          lineNumbers: true,
          readOnly: props.busy ? 'nocursor' : false,
          indentUnit: 4,
          indentWithTabs: false,
        }}
        editorDidMount={(editor, _) => {
          props.setEditor(editor);
          editor.setOption("extraKeys", {
            Tab: function(codeMirror) {
              var spaces = Array(codeMirror.getOption("indentUnit") + 1).join(" ");
              codeMirror.replaceSelection(spaces);
            }
          });
        }}
        onBeforeChange={(editor, data, value) => {
          props.setCode(value);
        }}
      />
    </div>
  );
}

function CheckButton(props) {
  const style = props.busy ? {color: 'lightgrey', cursor: 'default'} : null;

  return (
    <div style={{textAlign: 'right'}}>
      <div
        className={classnames(
          'button button--outline button--secondary',
          styles.getStarted,
        )}
        style={style}
        onClick={props.check}>
        {props.busy ? <div className={styles.spinner} /> : null}
        Check (Ctrl-Enter)
      </div>
    </div>
  );
}

function CopyUrlButton(props) {
  const style = props.busy ? {color: 'lightgrey', cursor: 'default'} : null;

  const copyCurrentUrl = () => {
    navigator.clipboard.writeText(window.location.href);
  }

  return (
    <div style={{textAlign: 'right'}}>
      <div
        className={classnames(
          'button button--outline button--secondary',
          styles.getStarted,
        )}
        style={style}
        onClick={copyCurrentUrl}>
        Copy Url
      </div>
    </div>
  );
}


function Results(props) {
  const results = props.results;
  if (results == null) {
    return null;
  }

  let errors = results.data.errors;
  if (errors.length !== 0) {
    let errorDivs = errors
        .map(error => {
          let message = `${error.line}:${error.column}: ${error.description}`;
          return <div key={message}> <pre> {message} </pre> </div>
        });
    return <div>{errorDivs}</div>;
  } else {
    return <div>No Errors!</div>;
  }
}

/**
 * Get a URL parameter, in a way that works in old versions of node where the
 * URLSearchParams global may not exist.
 */
function getCodeFromURL() {
  if (ExecutionEnvironment.canUseDOM) {
      const urlParams = new URLSearchParams(window.location.search);
      return urlParams.get('input');
  }
  return null;
}

function getInitialCode() {
  let codeFromUrl = getCodeFromURL();
  return codeFromUrl == null ? DEFAULT_INITIAL_CODE : codeFromUrl;
}

function encodeCodeAndSetURL(code) {
  const encodedCode = encodeURIComponent(code);
  window.history.pushState(code, 'unused', `/play?input=${encodedCode}`);
  return encodedCode;
}

function markErrors(editor, results) {
  editor.getAllMarks().map(mark => mark.clear());

  if (results == null) {
    return
  }
  results.data.errors.map(error =>
    editor.markText(
      {line: error.line - 1, ch: error.column},
      {line: error.stop_line - 1, ch: error.stop_column},
      {className: 'pyre-type-error'},
    )
  );
}

function Playground() {

  const [results, setResults] = useState(null);
  const [modifierKey, setModifierKey] = useState(false);
  const [code, setCode] = useState(getInitialCode());
  const [busy, setBusy] = useState(false);
  const [editor, setEditor] = useState(null);

  const check = () => {
    setBusy(true);
    const encodedCode = encodeCodeAndSetURL(code);
    fetch(`https://play.pyre-check.org/check?input=${encodedCode}`, {
      method: 'GET',
      mode: 'cors',
      headers: {'Content-Type': 'application/json'},
    })
      .then(response => response.json())
      .then(results => {
        setResults(results);
        setBusy(false);
        markErrors(editor, results);
      })
      .catch(error => console.error(error));
  };

  const checkOnCtrlEnter = (event) => {
    if (event.key === 'Enter' && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      event.stopPropagation();
      check();
    }
  }

  useEffect(() => {
    window.addEventListener('keydown', checkOnCtrlEnter);
    return () => {
      window.removeEventListener('keydown', checkOnCtrlEnter);
    };
  }, [code]);

  return (
    <Layout title="Playground">
      <main className={styles.main}>
        <h1 className={styles.heading}>Playground</h1>
        <Code code={code} results={results} setCode={setCode} setEditor={setEditor} busy={busy} />
        <br />
        <div className={`${styles.buttons} check`}>
          <CheckButton check={check} busy={busy} />
          <CopyUrlButton busy={busy} />
        </div>
        <br />
        <br />
        <Results results={results} />
      </main>
    </Layout>
  );
}

export default Playground;
