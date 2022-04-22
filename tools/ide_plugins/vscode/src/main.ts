/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * @file Entry point for Pyre's VSCode extension.
 */

import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, DidChangeConfigurationNotification } from 'vscode-languageclient';

let languageClient: LanguageClient;

namespace Configuration {

	let configurationListener: vscode.Disposable;

	export function initialize() {
		configurationListener = vscode.workspace.onDidChangeConfiguration(() => {
			languageClient.sendNotification(DidChangeConfigurationNotification.type, { settings: null });
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
		args: ["persistent"]
	};

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: 'python'}],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contain in the workspace
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc'),
		}
	};

	const languageClient = new LanguageClient(
		'pyre',
		'Pyre Language Client',
		serverOptions,
		clientOptions,
	)

	languageClient.registerProposedFeatures();
	languageClient.onReady().then(() => {
		Configuration.initialize();
	});
	languageClient.start();
}

export function deactivate() {
	Configuration.dispose();
}
