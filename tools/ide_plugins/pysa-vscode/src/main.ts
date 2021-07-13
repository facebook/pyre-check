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
} from "vscode-languageclient/node";

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

  languageClient = new LanguageClient(
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
    var active = vscode.window.activeTextEditor.selection.active;
    languageClient
      .sendRequest("copyModel", {
        path: path,
        position: { line: active.line, character: active.character },
      })
      .then((response: any) => {
        vscode.env.clipboard.writeText(response);
        vscode.window.showInformationMessage(
          "Generated model copied to clipboard!"
        );
        console.log(response);
      });
  } catch (error) {
    vscode.window.showErrorMessage(
      `Generating model failed. Error: ${JSON.stringify(error)}`
    );
    console.error(error);
  }
}
