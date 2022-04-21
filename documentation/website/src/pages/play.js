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


const DEFAULT_INITIAL_CODE = `# Pyre is being run in strict mode: https://www.internalfb.com/intern/staticdocs/pyre/docs/types-in-python#strict-mode
# Use the pyre-unsafe header to run in unsafe mode.

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
          return <div style = {{fontFamily: 'monospace'}} key={message}> {message}</div>
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


function Playground() {

  const [results, setResults] = useState(null);
  const [modifierKey, setModifierKey] = useState(false);
  const [code, setCode] = useState(getInitialCode());
  const [busy, setBusy] = useState(false);

  const check = () => {
    setBusy(true);
    const encodedCode = encodeCodeAndSetURL(code);
    fetch(`https://play.pyre-check.org/check?input=${encodedCode}`, {
      method: 'GET',
      mode: 'cors',
      headers: {'Content-Type': 'application/json'},
    })
      .then(response => response.json())
      .then(data => {
        setResults(data);
        setBusy(false);
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
        <Code code={code} setCode={setCode} busy={busy} />
        <br />
        <CheckButton check={check} busy={busy} />
        <br />
        <Results results={results} />
      </main>
    </Layout>
  );
}

export default Playground;
