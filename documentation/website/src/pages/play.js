/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useState} from 'react';
import Layout from '@theme/Layout';
import classnames from 'classnames';
import Link from '@docusaurus/Link';

import {Controlled as CodeMirror} from 'react-codemirror2';
import 'codemirror/lib/codemirror.css';
import styles from './styles.module.css';

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
        Check
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
  let errorDivs = errors
      .map(error => {
        let message = `${error.line}:${error.column}: ${error.description}`;
        return <div style = {{fontFamily: 'monospace'}} key={message}> {message}</div>
      });

  return <div>{errorDivs}</div>;
}

function Playground() {
  const [results, setResults] = useState(null);
  const [code, setCode] = useState('reveal_type(1)');
  const [busy, setBusy] = useState(false);

  const check = () => {
    setBusy(true);
    const encodedCode = encodeURIComponent(code)
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
