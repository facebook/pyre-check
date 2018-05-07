/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2016-present, Facebook, Inc.
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *). All rights reserved.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import { workspace, ExtensionContext, WorkspaceConfiguration, Disposable } from 'vscode';
import {
	LanguageClient, LanguageClientOptions, ServerOptions, CancellationToken, Middleware,
	DidChangeConfigurationNotification, Proposed, ProposedFeatures
} from 'vscode-languageclient';

let client: LanguageClient;

namespace Configuration {

	let configurationListener: Disposable;

	export function initialize() {
		configurationListener = workspace.onDidChangeConfiguration(() => {
			client.sendNotification(DidChangeConfigurationNotification.type, { settings: null });
		});
	}

	export function dispose() {
		if (configurationListener) {
			configurationListener.dispose();
		}
	}
}


export function activate(_: ExtensionContext) {

	let serverOptions: ServerOptions = {
	    command: "pyre",
	    args: ["persistent"]
	}

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: 'python'}],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contain in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
		}
	}

	// Create the language client and start the client.
	client = new LanguageClient('pyre', 'Pyre Language Client', serverOptions, clientOptions);
	// Register new proposed protocol if available.
	client.registerProposedFeatures();
	client.onReady().then(() => {
		Configuration.initialize();
	});

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> {
	if (!client) {
		return undefined;
	}
	Configuration.dispose();
	return client.stop();
}
