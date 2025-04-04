# Pyrefly VS Code LSP extension

:warning: **Extension is very work in progress**, please use at your own risk!

A VSCode LSP extension that talks over stdin/stdout to a binary.

If using another binary, the settings to be aware of are `pyre2.lspPath` (the
binary path) and `pyre2.lspArguments` (the arguments to that binary). These are
available in the VSCode extension settings UI.

Based on a combination of:

- Tutorial at
  https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
- Code for the tutorial at
  https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample

## Pre-requisites

- You need to have npm v7+ installed. Afterwards, run `npm install` in this
  folder and in `client`.

## Debugging (Recommended Development Workflow)

- Follow steps in Pre-requisites section.
- Ensure `cargo` is installed
- Open VS Code on the pyre2 folder.
- Switch to the Debug viewlet.
- Select `Run Installed Extension (pyre2)` from the drop down.
- Run the launch config.
- By default, stderr of the language server will appear in the output pane of
  VSCode under "Pyrefly language server".
- Add `"pyre2.trace.server": "verbose"` to the VSCode config. Then all the LSP
  JSON requests and responses will be logged together with stderr of language
  server in the output pane.

## Installing

- Follow steps in Pre-requisites section.
- Build the Pyrefly binary with
  `buck2 build pyre2 @fbcode//mode/opt --show-output` or `cargo build` and
  either:

1. Place binary at `lsp/bin/release/pyrefly(.exe)`
2. Add the `pyre2.lspPath` configuration key to point at it after extension
   startup.

- Run `npm install vsce`
- Run `npm exec vsce package`
- In VS Code, go to Extensions, click on the "..." button in the Extensions bar,
  select "Install from VSIX" and then select the `pyre2-1.0.0.vsix` file that
  was produced.

## Building for all Platforms

- Run
  [build_extension](https://github.com/facebook/pyrefly/actions/workflows/build_extension.yml)
  github workflow on your branch.

## Updating

Every few months security advisories will arrive about pinned versions of
packages.

- `npm audit` to see which packages have security updates.
- `npm audit fix` to fix those issues.
- Try `npm audit`, if it still has issues run `npm update`.
- `npm exec vsce package` to confirm everything still works.
