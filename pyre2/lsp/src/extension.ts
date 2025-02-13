/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { ExtensionContext } from 'vscode';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

/// Get a setting at the path, or throw an error if it's not set.
function requireSetting<T>(path: string): T {
    const ret: T = vscode.workspace.getConfiguration().get(path);
    if (ret == undefined) {
        throw new Error(`Setting "${path}" was not configured`)
    }
    return ret;
}

export function activate(context: ExtensionContext) {
    const path: string = requireSetting("pyre2.lspPath");
    const args: [string] = requireSetting("pyre2.lspArguments");

    // Otherwise to spawn the server
    let serverOptions: ServerOptions = { command: path, args: args };
    let rawInitialisationOptions = vscode.workspace.getConfiguration("pyre2");

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        initializationOptions: rawInitialisationOptions,
        // Register the server for Starlark documents
        documentSelector: [{ scheme: 'file', language: 'python' }],
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'pyre2',
        'Pyre2 language server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();

    context.subscriptions.push(
        vscode.commands.registerCommand('pyre2.restartClient', async () => {
            await client.stop();
            client = new LanguageClient(
                'pyre2',
                'Pyre2 language server',
                serverOptions,
                clientOptions
            );
            await client.start();
        }),
    );
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
