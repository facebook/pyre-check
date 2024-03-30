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
import { EnvironmentPath, PVSC_EXTENSION_ID, PythonExtension } from '@vscode/python-extension'
import { dirname, join } from 'path';
import { existsSync, statSync } from 'fs';

type LanguageClientState = {
	languageClient: LanguageClient,
	configListener: Promise<vscode.Disposable>
}

// Extension state
let state : LanguageClientState | undefined;
let envListener: vscode.Disposable | undefined;

let outputChannel = vscode.window.createOutputChannel("pyre");

export async function activate(_: vscode.ExtensionContext) {

	const pythonExtension = vscode.extensions.getExtension<PythonExtension>(PVSC_EXTENSION_ID);

	if (!pythonExtension) {
		outputChannel.appendLine("Python extension not found. Will use the default console environment.");
		state = createLanguageClient('pyre');
		outputChannel.appendLine("Done");
		return;
	}

	const activatedEnvPath = pythonExtension.exports.environments.getActiveEnvironmentPath();
	const pyrePath = inferPyreCommand(activatedEnvPath);

	if (pyrePath) {
		state = createLanguageClient(pyrePath);
	}

	envListener = pythonExtension.exports.environments.onDidChangeActiveEnvironmentPath(async (e) => {
		state?.languageClient?.stop();
		state?.configListener.then((listener) => listener.dispose());
		state = undefined;

		const pyrePath = inferPyreCommand(e);
		if (pyrePath) {
			state = createLanguageClient(pyrePath);
		}
	});
}

function createLanguageClient(pyrePath: string) : LanguageClientState {

	const serverOptions = {
		command: pyrePath,
		args: ["persistent"]
	};

	const clientOptions: LanguageClientOptions = {
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

	const configListener = languageClient.onReady().then(() => {
		return vscode.workspace.onDidChangeConfiguration(() => {
			languageClient.sendNotification(DidChangeConfigurationNotification.type, { settings: null });
		});
	});

	languageClient.start();

	return {languageClient, configListener};
}

function inferPyreCommand(envPath: EnvironmentPath) : string | undefined {

	if (envPath.id === 'DEFAULT_PYTHON') {
		outputChannel.appendLine(`Using the default python environment`);
		return 'pyre';
	}

	const path = envPath.path;
	const stat = statSync(path)

	const pyrePath = stat.isFile()
		? join(dirname(envPath.path), 'pyre')
		: stat.isDirectory()
			? join(path, 'bin', 'pyre')
			: undefined;

	if (pyrePath && existsSync(pyrePath) && statSync(pyrePath).isFile()) {
		outputChannel.appendLine(`Using pyre path: ${pyrePath} from python environment: ${envPath.id} at ${envPath.path}`);
		return pyrePath;
	} else if (pyrePath) {
		outputChannel.appendLine(`Environment: ${envPath.id} at ${envPath.path} does not contain a pyre executable at ${pyrePath}`);
		return undefined;
	} else {
		outputChannel.appendLine(`Could not infer pyre path from python environment: ${envPath.id} at ${envPath.path}`);
		return undefined
	}
}

export function deactivate() {
	state?.languageClient.stop();
	state?.configListener.then((listener) => listener.dispose());
	envListener?.dispose();
}
