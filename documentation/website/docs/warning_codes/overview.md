---
id: overview-public
title: Overview
sidebar_label: Overview
---

Pysa tracks data flows from a set of sources to sinks. Each set of sources we track to sinks is assigned a unique code. When Pysa finds a flow, it emits issues associated with the unique code. This section of the website contains documentation for the warning codes we emit.

The source of truth for these warning codes is the `taint_config.json` files used in a given run.
