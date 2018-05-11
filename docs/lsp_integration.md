---
id: lsp-integration
title: See Pyre Errors In Your Editor
sidebar_label: Editor Integration
---

Pyre is integrated with VSCode and Nuclide which allows you to catch your errors in real-time.

## VSCode

![VSCode Integration](img/vscode-screenshot.png)

After running `pyre init` on the root of your repository, open the root in VSCode. Install the pyre-check extension from the extension marketplace, and pyre should start up automatically.

The extension's available [here](https://marketplace.visualstudio.com/items?itemName=fb-pyre-check.pyre-vscode).

## Nuclide

![Nuclide Integration](img/nuclide-screenshot.png)

You can `apm install atom-ide-ui && apm install ide-pyre` in order to install pyre's Atom integration.

The pyre Atom package is available [here](https://atom.io/packages/ide-pyre).
