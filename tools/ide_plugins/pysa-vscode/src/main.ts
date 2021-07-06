/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * @file Entry point for Pysa's VSCode extension.
 */

import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  DidChangeConfigurationNotification,
} from "vscode-languageclient";

import * as childProcess from "child_process";

// let exec = childProcess.exec;
let exec = require('child_process').exec;
let languageClient: LanguageClient;

namespace Configuration {
  let configurationListener: vscode.Disposable;

  export function initialize() {
    configurationListener = vscode.workspace.onDidChangeConfiguration(() => {
      languageClient.sendNotification(DidChangeConfigurationNotification.type, {
        settings: null,
      });
    });
  }

  export function dispose() {
    if (configurationListener) {
      configurationListener.dispose();
    }
  }
}

export async function activate(_: vscode.ExtensionContext) {
  let serverOptions = {
    command: "pyre",
    args: ["pysa-language-server"],
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "pysa_model" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contain in the workspace
      fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
    },
  };

  const languageClient = new LanguageClient(
    "pyre",
    "Pyre Language Client",
    serverOptions,
    clientOptions
  );

  languageClient.registerProposedFeatures();
  languageClient.onReady().then(() => {
    Configuration.initialize();
  });
  languageClient.start();

  // regsistering the command to generate model
  const commandHandler = (path: string) => {
    console.log(`Generating model from ${path}...`);
    copyModel(path);
    vscode.window.showInformationMessage(
      "Generated model copied to clipboard!"
    );
  };
  _.subscriptions.push(
    vscode.commands.registerCommand("pysa.generateModel", commandHandler)
  );
}

export function deactivate() {
  Configuration.dispose();
}

async function copyModel(path: string) {
  try {
    let cp = exec(
      "pyre query \"types('./test.py')\"",
      {
        cwd: path,
      },
      (error, stdout, stderr) => {
        if (error) {
          console.log(`${error.name}: ${error.message}`);
          console.log(`[STACK] ${error.stack}`);
        }
        console.log(stdout);
        console.log(stderr);
      }
    );
    cp.stdout.on("data", (data) => console.log(data));

    // const terminal = vscode.window.createTerminal(
    //   `Ext Terminal #${NEXT_TERM_ID++}`
    // );
    // terminal.sendText("echo 'testing'");

    // vscode.window.onDidWriteTerminalData((e) => {})
    // alternatively, parse the JSON and check the start and end positions
    //  let line = 1;
    //  let char = 1;
    //  let functionRange = new vscode.Range (vscode.Position(line, char), vscode.Position(line, char));
    //  if (functionRange.contains(active)){
    //     // copy the Pysa model
    //  }

    var active = await vscode.window.activeTextEditor.selection.active;
    await vscode.env.clipboard.writeText(
      `Selected function at ${active.line}:${active.character}`
    );
  } catch (error) {
    vscode.window.showErrorMessage(
      `Generating model failed. Error: ${JSON.stringify(error)}`
    );
  }
}
