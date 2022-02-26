---
id: pysa-shipping-rules-models
title: Shipping Pysa Models and Rules
sidebar_label: Shipping Pysa Models and Rules
---

import useBaseUrl from '@docusaurus/useBaseUrl';

We welcome contributions and coverage improvements for Pysa! This page walks you through the basics of adding new models or rules, and testing your changes.

## Prerequisites
This document assumes you:

1. Have followed [these instructions](pysa_quickstart.md) to set up Pysa and SAPP.
2. Understand the [basic concepts of sources, sinks, sanitizers, and
   rules](pysa_basics.md) as well as [features](pysa_features.md).
3. Have completed a [successful run of Pysa](pysa_running.md)
4. Know the sources and sinks you'd like to detect.

Ideally, you will also have completed the Pysa tutorial ([code](https://github.com/facebook/pyre-check/tree/main/documentation/pysa_tutorial),
[video](https://www.youtube.com/watch?v=8I3zlvtpOww)).

## Where to put your models / `taint.config` changes

Common privacy/security-related rules and models should be placed under [`stubs/taint/core_privacy_security`](https://github.com/facebook/pyre-check/tree/main/stubs/taint/core_privacy_security). These should only reference entities that exist in the Python standard library, or the [type stubs](https://github.com/facebook/pyre-check/tree/main/stubs) that are shipped with Pyre.

Models for third-party libraries should be placed under [`stubs/third_party_taint`](https://github.com/facebook/pyre-check/tree/main/stubs/third_party_taint).


## Writing your models / `taint.config` changes

Refer to the docs on [sources, sinks, sanitizers, rules](pysa_basics.md), and
[features](pysa_features.md) when you need help. You can usually find
pre-existing examples of
[sources](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/flask_sources_sinks.pysa),
[sinks](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/filesystem_sinks.pysa),
[sanitizers](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/sanitizers.pysa),
and
[features](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/django_sources_sinks.pysa)
to copy as a starting point if you get stuck.

When adding models for a pre-existing source/sink/feature type (e.g.
[`UserControlled`](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/taint.config#L21),
you won't need to modify `taint.config`. Make sure to check for pre-existing
source, sink, and feature declarations before declaring a new one; most types of
source and sinks are already declared, and it can be more sustainable to add an
additional model to an existing category, rather than creating a whole new one.

When adding a new rule, you will need to update `taint.config`. As with the
previous paragraph, try to put existing sources and sinks to use. For example,
if you're writing a sensitive data logging rule, using the
[`Logging`](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/stubs/taint/core_privacy_security/taint.config#L115)
sink will allow you to detect your chosen data flowing into many different types of loggers that we model.

## Testing

Providing a quick explanation of the issue you intended to catch and evidence that the issue was caught in your local run is usually sufficient. This can be in the form of a screenshot of the issue in SAPP, a sample or paste of the `taint-output.json` produced by Pysa, etc.

### Catching Known Issues

To test, you need to have an issue that you want to find. The best option is to
have a known vulnerability from [a past CVE](https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=python)
or issue. You can manually identify the flow of data that caused the issue, make
sure you have the correct sources and sinks, and then verify that your new
rule/source/sink catches the issue.

Follow [these instructions](pysa_quickstart.md) to run Pysa and import the
results into the SAPP UI.

Once your run completes, you should be able to see the issue you intended to
catch in the UI. If there are a lot of issues showing up, you may need to use
[filters](https://pyre-check.org/docs/pysa-quickstart#run-sapp) to find the
particular issue you were looking for. If you're not finding your
issue, read through the [development tips](pysa_tips.md) for help debugging.

### Integration Test

If you don't have an existing project to test on, you can also use the integration
test environment provided with the pyre-check repo. This is a minimal, deliberately
vulnerable Flask web app. You can find all the details and instructions on how to set
it up [here](https://github.com/facebook/pyre-check/tree/main/documentation/deliberately_vulnerable_flask_app).

It should already be set up to use the taint models in the `pyre-check/stubs` folder,
so you can easily make your changes to existing Pysa models and observe the effects.

For example, suppose we wanted to add a new rule for some builtin Python functions.
We would add our new source, sink and rule in `stubs/taint/core_privacy_security/taint.config`:
```json
{
    ...
    "sources": [
        ...,
        {
            "name": "Foo",
            "comment": "used to annotate a foo source"
        },
    ],
    "sinks": [
        ...,
        {
            "name": "Bar",
            "comment": "used to annotate a bar sink"
        },
    ],
    "rules": [
        ...,
        {
            "name": "Foo to Bar",
            "code": 9000,
            "sources": [
                "Foo"
            ],
            "sinks": [
                "Bar"
            ],
            "message_format": "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
        },
    ],
}
```
Make sure your new rule has a unique number and new source and sink names are also unique!

We can now add our models in a `.pysa` file under `stubs/taint/core_privacy_security/`:
```python
def input() -> TaintSource[Foo]: ...
def ascii(__source: TaintSink[Bar]): ...
```

Then, we can open a source file in the vulnerable Flask app
(e.g. [`app.py`](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/documentation/deliberately_vulnerable_flask_app/app.py)),
and inject an issue of the type we want to catch:
```python
def alarm1() -> None:
    x = input()
    ascii(x)
```

Finally, we can run the integration test using the [`run_integration_tests.sh`](https://github.com/facebook/pyre-check/blob/d82759a1f1ce5467aa2250708b226790c046f207/documentation/deliberately_vulnerable_flask_app/run_integration_tests.sh) script (or just run `pyre analyze`)
and verify that the issue we expect to be caught is indeed caught:
```bash
ERROR ----BEGIN PYSA INTEGRATION TEST ERROR----
ERROR Output differs from expected:
...
@@ -46,5 +46,11 @@
    ...
+  },
+  {
+    "code": 9000,
+    "define": "app.alarm1",
+    "description": "Foo to Bar [9000]: Data from [Foo] source(s) may reach [Bar] sink(s)",
+    "path": "app.py"
   }
 ]
ERROR ----END PYSA INTEGRATION TEST ERROR----
```

## Contributing Coverage Improvements to Pysa

When you've proven that you can catch issues with your changes, send a PR
to the [pyre-check](https://github.com/facebook/pyre-check) Github repository.
Please make sure you include a test plan in your PR that follows the
[testing](#testing) guidelines mentioned above!

If you used the [deliberately_vulnerable_flask_app](https://github.com/facebook/pyre-check/tree/main/documentation/deliberately_vulnerable_flask_app) for testing, free to also
include your integration test changes with your injected issue in your PR to
help us expand our open source integration tests! Note this will require you to update the
[`full_result.json`](https://github.com/facebook/pyre-check/blob/main/documentation/deliberately_vulnerable_flask_app/full_result.json) file in the same folder. When you run the integration test script, a
`full_result.actual` file will be produced when the output does not match
the existing expected output. Simply copy the contents of that file to
`full_result.json`, e.g. in the example above, we would add the following lines to `full_result.json`:
```json
    ...
    {
        "code": 9000,
        "column": 10,
        "define": "app.alarm1",
        "description": "Foo to Bar [9000]: Data from [Foo] source(s) may reach [Bar] sink(s)",
        "line": 64,
        "name": "Foo to Bar",
        "path": "app.py",
        "stop_column": 11,
        "stop_line": 64
    }
]
```
