# Pyre Visual Studio Code Client

This extension allows you to view pyre errors for any workspace containing a .pyre_configuration.

## Requirements

pyre must be installed for the extension to work. You can do so by running `pip install pyre-check`.

## Issues

You can file bugs, feature requests, etc. for pyre-check at the [pyre-check Github](https://github.com/facebook/pyre-check).

## Building

To build the extension, you must have the command line tools for Visual Studio Code Extensions (VSCE). To install this,
run `npm install -g @vscode/vsce`. Then, `cd tools/ide_plugins/vscode` and run `npm install && vsce package`. That will
build a .vsix file allowing you to install the extension locally in VSCode.
