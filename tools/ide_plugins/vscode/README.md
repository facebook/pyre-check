# Pyre Visual Studio Code Client

This extension allows you to view pyre errors for any workspace containing a .pyre_configuration.

## Requirements

`pyre` and `watchman` must be installed and configured for the extension to work:

- Install pyre e.g. running `pip install pyre-check`.
- Install [watchman (https://facebook.github.io/watchman/docs/install) e.g. on Debian/Ubuntu `apt-get install -y watchman`.
- Create configuration for pyre and watchman by running `pyre init` 

## Issues

You can file bugs, feature requests, etc. for pyre-check at the [pyre-check Github](https://github.com/facebook/pyre-check).

## Building

To build the extension, you must have the command line tools for Visual Studio Code Extensions (VSCE). To install this,
run `npm install -g @vscode/vsce`. Then, `cd tools/ide_plugins/vscode` and run `npm install && vsce package`. That will
build a .vsix file allowing you to install the extension locally in VSCode.
