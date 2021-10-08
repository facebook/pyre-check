/**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  require('codemirror/mode/python/python.js');
}

const default_code = `import subprocess
from flask import Flask

app = Flask(__name__)

@app.route("/rce/<string:payload>")
def definite_rce(payload: str) -> None:
    subprocess.run(payload, shell=True)`;
const default_model = "#Define your custom models here";

function Code(props) {
   return (
    <div className={styles.card}>
      <h4>Code</h4>
      <CheckButton check={props.check} busy={props.busy} />
      <CodeMirror
        value={props.code}
        options={{
          mode: 'text/x-python',
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

function getStorageValue(key, defaultValue) {
    let saved = null;
    if (typeof window !== 'undefined') {
      saved = localStorage.getItem(key);
    }
    return saved || defaultValue;
}

function CheckButton(props) {
  const style = props.busy ? {color: 'lightgrey', cursor: 'default'} : null;

  return (
    <div style={{textAlign: 'right'}}>
      <div
        className={classnames(
          'button button--outline button--secondary',
          styles.getStarted,
          styles.runPysa,
        )}
        style={style}
        onClick={props.check}>
        {props.busy ? <div className={styles.spinner} /> : null}
        Run Pysa
      </div>
    </div>
  );
}

function Results(props) {
  const results = props.results;
  var content = results;
  if (results) {
    if (results.errors !== undefined) {
      content = results.errors.join('\n');
    } else {
      content = (results.data.errors
        .map(error => `${error.line}:${error.column}: ${error.description}`)
        .join('\n')) || "No issues were detected by Pysa.";
    }
  }

  return (
    <div className={classnames(styles.card,styles.resultsCard)}>
      <h4>Results</h4>
      <div style={{fontFamily: 'monospace'}}>
        {content || "Run Pysa to get results"}
      </div>
    </div>
  );
}

function Models(props) {
  return (
    <div className={styles.card}>
      <h4>Models</h4>
      <CodeMirror
        value={props.model}
        options={{
          mode: 'text/x-python',
          lineNumbers: true,
          theme: 'default height100',
        }}
        onBeforeChange={(editor, data, value) => {
          props.setModel(value);
        }}
      />
      <input
        type="checkbox"
        defaultChecked={props.useOSModels}
        onChange={props.setUseOSModels}
      />
      <label>Use open source models</label>
    </div>
  );
}

function Sandbox() {
  const [results, setResults] = useState(null);
  const [code, setCode] = useState(getStorageValue("code", default_code));
  const [busy, setBusy] = useState(false);
  const [model, setModel] = useState(getStorageValue("model", default_model));
  const [useOSModels, setUseOSModels] = useState(true);

  useEffect(() => {
    if (typeof window !== 'undefined') {
      localStorage.setItem("code", code);
      localStorage.setItem("model", model);
    }
  })

  const check = () => {
    setBusy(true);
    setTimeout(() => {
      if (results === null) {
        setBusy(false);
        setResults({errors: ["Cannot reach server - request timed out"]})
      }
    }, 2000);
    fetch('http://localhost:5000/analyze', {
      method: 'POST',
      mode: 'cors',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({
        input: code,
        model: model,
        use_builtin_pysa_models: useOSModels,
      }),
    })
      .then(response => response.json())
      .then(data => {
        setResults(data);
        setBusy(false);
      })
      .catch(error => console.error(error));
  };

  return (
    <Layout title="Pysa Playground">
      <main className={styles.playgroundMain}>
        <Models
          model={model}
          setModel={setModel}
          useOSModels={useOSModels}
          setUseOSModels={setUseOSModels}/>
        <Results results={results} />
        <Code
          code={code}
          setCode={setCode}
          busy={busy}
          check={check}
        />
      </main>
    </Layout>
  );
}

export default Sandbox;
