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
import { ToastContainer, toast } from 'react-toastify';

import 'codemirror/lib/codemirror.css';
import 'react-toastify/dist/ReactToastify.css';
import styles from './styles.module.css';

let socketIOClient = () => {};

if (ExecutionEnvironment.canUseDOM) {
  require('codemirror/mode/python/python.js');
  socketIOClient = require('socket.io-client')
}

let query_params = {get: () => {}};
if (typeof window !== 'undefined') {
  query_params = new URLSearchParams(window.location.search);
}
const code_in_uri = decodeURI(query_params.get("code"));
const models_in_uri = decodeURI(query_params.get("models"));
const use_os_models_in_uri = decodeURI(query_params.get("use_os_models"));

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
  let result = (saved === null || saved === '')? defaultValue : saved;
  return result;
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

function CopyButton(props) {
  return (
    <div style={{textAlign: 'right'}}>
      <div
        className={classnames(
          'button button--outline button--secondary',
          styles.getStarted,
          styles.runPysa,
        )}
        onClick={props.click}>
        Copy Shareable Link
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
      content = results.data.join('\n');
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

function Menu(props) {
  const copy = () => {
    if(typeof window !== 'undefined') {
      const uri = window.location.href + "?code=" + encodeURI(props.code) + "&models=" + encodeURI(props.models) + "&use_os_models=" + encodeURI(props.useOSModels);
      navigator.clipboard.writeText(uri).then(() => {
        toast("Link copied to clipboard");
      }, () => {
        toast("Failed to copy to clipboard");
      });
    }
  };
  return (
    <CopyButton click={copy}/>
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
        onChange={(e) => {props.setUseOSModels(e.target.checked)}}
      />
      <label>Use open source models</label>
    </div>
  );
}

function Sandbox() {
  const [results, setResults] = useState(null);
  const [code, setCode] = useState(
    (code_in_uri === "null")? getStorageValue("code", default_code) : code_in_uri
  );
  const [busy, setBusy] = useState(false);
  const [model, setModel] = useState(
    (models_in_uri === "null")? getStorageValue("model", default_model) : models_in_uri
  );
  const [useOSModels, setUseOSModels] = useState(
    ((use_os_models_in_uri === "null")? getStorageValue("use_os_models", "true") : use_os_models_in_uri) === "true"
  );

  const socket = socketIOClient("ws://127.0.0.1:5000/analyze", {reconnection: false});
  let synchronous_results = [];

  useEffect(() => {
    socket.on("pysa_results_channel", data => {
      if (data["type"] === undefined) {
        setResults({errors: ["Invalid data received from the server."]});
        setBusy(false);
      } else if(data["type"] === "finished") {
        setBusy(false);
      } else if(data["type"] === "output") {
        if(data["line"] === undefined){
          setResults({errors: ["Invalid data received from the server."]});
          setBusy(false);
        } else {
          synchronous_results.push(data["line"]);
        }
        setResults({data: synchronous_results});
      }
    });
    socket.on("connect_error", () => {
      setBusy(false);
      setResults({errors: ["Error establishing a connection to the server."]})
    });
  }, [results]);

  useEffect(() => {
    if (typeof window !== 'undefined') {
      localStorage.setItem("code", code);
      localStorage.setItem("model", model);
      localStorage.setItem("use_os_models", useOSModels);
    }
  });

  const check = () => {
    setBusy(true);
    synchronous_results = [];
    socket.emit("analyze", {
      input: code,
      model: model,
      use_builtin_pysa_models: useOSModels,
    });
  };

  return (
    <Layout title="Pysa Playground">
      <main className={styles.playgroundMain}>
        <Models
          model={model}
          setModel={setModel}
          useOSModels={useOSModels}
          setUseOSModels={setUseOSModels}/>
        <Menu code={code} models={model} useOSModels={useOSModels}/>
        <Results results={results} />
        <Code
          code={code}
          setCode={setCode}
          busy={busy}
          check={check}
        />
        <ToastContainer
          position="bottom-center"
          autoClose={5000}
          hideProgressBar
          newestOnTop={false}
          closeOnClick
          rtl={false}
          pauseOnFocusLoss={false}
          draggable={false}
          pauseOnHover={false}
        />
      </main>
    </Layout>
  );
}

export default Sandbox;
