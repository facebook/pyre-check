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

  var content = '';
  if (results.errors !== undefined) {
    content = results.errors.join('\n');
  } else {
    content = results.data.errors
      .map(error => `${error.line}:${error.column}: ${error.description}`)
      .join('\n');
  }

  return <div style={{fontFamily: 'monospace'}}>{content}</div>;
}

function Sandbox() {
  const [results, setResults] = useState(null);
  const [code, setCode] = useState('reveal_type(1)');
  const [busy, setBusy] = useState(false);

  const check = () => {
    setBusy(true);
    fetch('http://ec2-3-17-23-247.us-east-2.compute.amazonaws.com/check', {
      method: 'POST',
      mode: 'cors',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({input: code}),
    })
      .then(response => response.json())
      .then(data => {
        setResults(data);
        setBusy(false);
      })
      .catch(error => console.error(error));
  };

  return (
    <Layout title="Sandbox">
      <main className={styles.main}>
        <h1 className={styles.heading}>Sandbox</h1>
        <Code code={code} setCode={setCode} busy={busy} />
        <br />
        <CheckButton check={check} busy={busy} />
        <br />
        <Results results={results} />
      </main>
    </Layout>
  );
}

export default Sandbox;
