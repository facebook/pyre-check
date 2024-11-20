# Pyre2 VS Code LSP extension

A VSCode LSP extension that talks over stdin/stdout to a binary.

If using another binary, the settings to be aware of are `pyre2.lspPath` (the
binary path) and `pyre2.lspArguments` (the arguments to that binary). These
are available in the VSCode extension settings UI.

Based on a combination of:

- Tutorial at
  https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
- Code for the tutorial at
  https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample

## Pre-requisites

You need to have npm v7+ installed. Afterwards, run `npm install` in this folder
and in `client`.

## Debugging

- Follow steps in Pre-requisites section.
- Open VS Code on this folder.
- Press Ctrl+Shift+B to compile the client and server.
- Switch to the Debug viewlet.
- Select `Launch Client` from the drop down.
- Run the launch config.

## Installing

- Follow steps in Pre-requisites section.
- Run `npm install vsce`
- Run `npm exec vsce package`
- In VS Code, go to Extensions, click on the "..." button in the Extensions bar,
  select "Install from VSIX" and then select the `pyre2-1.0.0.vsix` file that
  was produced.
- Build the minipyre binary with `buck2 build minipyre @fbcode//mode/opt --show-output`
  and either:
  1. Put it on your `$PATH`.
  2. Add the `pyre2.lspPath` configuration key to point at it.

## Updating

Every few months security advisories will arrive about pinned versions of
packages.

- `npm audit` to see which packages have security updates.
- `npm audit fix` to fix those issues.
- Try `npm audit`, if it still has issues run `npm update`.
- `npm exec vsce package` to confirm everything still works.
