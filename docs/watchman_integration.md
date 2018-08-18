---
id: watchman-integration
title: Watchman Integration
sidebar_label: Watchman Integration
---

In order to support incremental typechecking using terminal editors such as vim or emacs, pyre supports listening on file changes via Watchman. Having watchman installed and watching your repository is recommended if you use command-line editing tools.


# Installing Watchman
[Installation instructions for watchman can be found here](https://facebook.github.io/watchman/docs/install.html).

# Watching your repository
If you get a warning stating that watchman isn't watching your repository root, you can run the following:

```
$ watchman watch .
$ pyre restart
```
