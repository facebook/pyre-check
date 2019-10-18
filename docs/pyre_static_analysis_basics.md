---
id: pyre-static-analysis-basics
title: Basics
sidebar_label: Basics
---

## Overview

Pyre has applications beyond type checking python code; it can also run static
analysis to identify potential security issues. These security issues are
identified with what is called a **Taint Analysis**.

Note that references to "Pyre" throughout the Static Analysis section of the
documentation will usually be specifically referring to the Pyre Static
Analyzer.

## Taint Analysis

**Tainted data** is data that must be treated carefully. The Pyre Static
Analyzer works by tracking flows of data from where they originate (sources) to
where they terminate in a dangerous location (sinks). For example, we might use
it to track flows where user-controllable request data flows into an `eval`
call, leading to a remote code execution vulnerability. This analysis is
made possible by user-created stubs which provide annotations on source code, as
well as Rules that define which sources are dangerous for which sinks. Pyre
comes with many pre-written stubs and rules for builtin and common python
libraries.

Pyre propagates taint as operations are performed on tainted data. For example,
if we start with a tainted integer and perform a number of operations on it, the
end results will still be tainted:

```python
x = some_function_that_returns_a_tainted_value() # 'x' is marked as tainted
y = x + 10
s = str(x)
f = f"Value = {s}" # 'f' is marked with the same taint 'x' had
```

Pyre will only analyze the code in the repo that it runs on, as well as code in
directories listed in the `search_path` of your
[`.pyre_configuration`](configuration.md) file. It does not see the source of
your dependencies. **Just because** ***you*** **can see code in your editor, it
does not mean Pyre has access to that code during analysis.** Because of this
limitation, Pyre makes some simplifying assumptions during static analysis. If
taint flows into a function Pyre doesn't have the source for, it will assume
that the return type of that function has the same taint. This helps prevents
false negatives, but can also lead to false positives.

When an object is tainted, that means that all attributes of that object are
also tainted. Note that this can lead to false positives, such as taint flows
that include `some_obj.__class__`. This means that Pyre will detect all the
following flows:

```python
x = some_source() # 'x' is marked as tainted

some_sink(x) # This is detected
some_sink(x.some_attribute) # This is also detected
some_sink(x.__class__) # This is (unfortunately) also detected
```

## Configuration

Pyre uses two types of files for configuration: a single `taint.config` file,
and an unlimited number of files with a `.pysa` extension. The `taint.config`
file is a JSON document which stores definitions for Sources, Sinks, Features,
and Rules (discussed below). The `.pysa` files are stub files (elaborated on
below) which annotate your code with the Sources, Sinks, and Features defined in
your `taint.config` file. Examples of these files can be found in the [pyre
repository](https://github.com/facebook/pyre-check/tree/master/stubs/taint).

These files live in the directory configured by `taint_models_path` in your
`.pyre_configuration` file. Any `.pysa` file found in this folder will be parsed
by Pyre and the stubs will be used during the analysis.


## Sources

Sources are where tainted data originates. They are declared in your
`taint.config` file like this:

```python
sources: [
    {
        name: "Cookies",
        comment: "used to annotate cookie sources"
    },
]
```

Stubs that indicate what is a source are then defined in `.pysa` files. Sources
are declared in the same places that [types are declared in Python
3](https://docs.python.org/3/library/typing.html). Function return types,
class/model attributes, and even entire classes can be declared as sources by
adding `TaintSource[SOURCE_NAME]` wherever you would add a python type:

```python
# Function return source
def django.http.request.HttpRequest.get_signed_cookie(
    self,
    key,
    default=...,
    salt=...,
    max_age=...
) -> TaintSource[Cookies]: ...

# Class attribute source:
django.http.request.HttpRequest.COOKIES: TaintSource[Cookies] = ...
```

When tainting an entire class, any return from a method or access of an
attribute of the class will count as a returning tainted data. The specifics of
these stub files are discussed further in the Stubs section.

```python
# Class source:
class BaseException(TaintSource[Exception]): ...
```

## Sinks

Sinks are where tainted data terminates. They are declared in your
`taint.config` file like this:

```python
sinks: [
  {
    name: "SQL",
    comment: "use to annotate places of SQL injection risk"
  }
]
```

Stubs that indicate what is a sink are then defined in `.pysa` files. Sinks can
be added to the same files as sources. Sinks are declared in the same places
that [types are declared in python
3](https://docs.python.org/3/library/typing.html). Function parameters and even
whole classes can be declared as sinks by adding `TaintSink[SINK_NAME]` where
you would add a python type:

```python
# Function parameter sink
def sqlite3.Cursor.execute(self, sql: TaintSink[SQL], parameters): ...
```

When tainting an entire class, any flow into a method or attribute of the class
will count as a flow to a taint sink. The specifics of these stub files are
discussed further in the Stubs section.

```python
# Entire class sink
class BaseException(TaintSink[Logging]): ...
```

## Rules

Rules declare which flows from source to sink we are concerned about. They are
declared in your `taint.config` file like this:

```python
rules: [
  {
    name: "SQL injection.",
    code: 1,
    sources: [ "UserControlled" ],
    sinks: [ "SQL" ],
    message_format: "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
  }
]
```

Each rule needs a brief `name` that explains its purpose and a *unique* `code`.
The rule must define a list of one or more `sources`, which we are concerned
about flowing into one or more `sinks`. `message_format` can further explain the
issue. When a flow is detected the `{$sources}` and `{$sinks}` variables will be
replaced with the name of the specific source(s) and sink(s) that were involved
in the detected flow.

## Sanitizers

Sanitizers break a taint flow by removing taint from data. Stubs that indicate
sanitizing functions are defined in `.pysa` files. Sanitizers can be added to
the same files as sources and sinks. Functions are declared as sanitizers by
marking their return type as `Sanitize`:

```python
# Sanitizer function
def django.utils.html.escape(text) -> Sanitize: ...
```

This annotation is useful in the case of explicit sanitizers such as `escape`,
which helps prevent cross site scripting (XSS) by escaping HTML characters. The
annotation is also useful, however, in cases where a function is not intended to
sanitize inputs, but is known to always return safe data despite touching
tainted data. One such example could be `hmac.digest(key, msg, digest)`, which
returns sufficiently unpredictable data that the data should no longer be
considered attacker-controlled after passing through.

Note that sanitizers are currently universal, meaning that they remove all taint
and can't be restricted to a specific rule or individual source to sink flows.
This means you need to ensure you aren't potentially affecting other flows when
you add a sanitizer for a flow you care about. For this reason, the above
sanitizer examples might not be a good idea to use. If you are trying to track
flows where SQL injection occurs, the `escape` sanitizer would would prevent you
from seeing any flows where data going into your SQL query happened to be html
escaped.

## Taint Propagation

Sometimes the features discussed in the Taint Analysis section are not enough to
detect all taint flows. In particular, Pyre relies on additional annotations to
help it understand when an object is tainted via a function call or when a
function call on a tainted object returns tainted data. Taint propagation is
defined by adding `TaintInTaintOut` annotations to stubs in `.pysa` files.

When a function call taints an object, such as when you update a dictionary with
a tainted value, Pyre needs a `TaintInTaintOut` annotation that indicates
`Updates[self]`:

```python
def dict.update(self, __m: TaintInTaintOut[Updates[self]]): ...
```

When a function call on a tainted object returns taint, such as when you
retrieve a value from a dictionary, Pyre needs a `TaintInTaintOut` annotation
that indicates `LocalReturn`:

```python
def dict.get(self: TaintInTaintOut[LocalReturn], key, default = ...): ...
```

## Features

Features annotations are also placed in your `taint.config` and `.pysa` files.
This is a larger topic and will be covered in detail on [its own page](pyre_static_analysis_features.md).

## Stub files

### Usage

By default, Pyre computes an inferred model for each function and combines it
with any declared models in `.pysa` files (of which there can be more than one).
The union of these models and their annotations will be used. For example,
cookies are both user controlled and potentially sensitive to log, and Pyre
allows us apply two different annotations to them:

```python
django.http.request.HttpRequest.COOKIES: TaintSource[UserControlled] = ...
django.http.request.HttpRequest.COOKIES: TaintSource[Cookies] = ...
```

There are other stub files with the `.pyi` extension which can also exist in
your codebase. These `.pyi` stubs are similar and use [the same
syntax](https://www.python.org/dev/peps/pep-0484/#stub-files) as the `.pysa`
stubs, but are not the stubs that are referred to in this document (though they
are relavent to static analysis). See the "Stubs" section of the [Gradual Typing
page](gradual_typing.md) for more info.

### Requirements and Features

#### Fully qualified names

Any declarations in `.pysa` files must use the fully qualified name for the
function/attribute they are attempting to annotate. You can usually find the
fully qualified name for a type by looking at how it is imported, however, it's
important to note that fully qualified names correspond to where something is
declared, not necessarily where it is imported from. For example, you can import
`HttpRequest` directly from the `django.http` module, even though it is defined in
`django.http.request`. If you wanted to taint an attribute of `HttpRequest`,
you would need to use the module in which it was defined:

```python
django.http.request.HttpRequest.GET: TaintSource[UserControlled] = ...
```

#### Exact Signatures

The signatures of any stub functions need to exactly match the the signature of
the function definition. This means that all parameters, including optional
parameters, `*args`, and `**kwargs` must be present. The default value of
optional parameters, however, can be elided (see below). Additionally, if a
function includes an asterisk that indicates [keyword only
arguments](https://www.python.org/dev/peps/pep-3102/), then that should be
present too. So for example, `urllib.request.urlopen` has the following
signature:

```python
def urlopen(url, data=None, timeout=socket._GLOBAL_DEFAULT_TIMEOUT, *, cafile=None,
            capath=None, cadefault=False, context=None):
```

That function would be annotated like this:

```python
def urllib.request.urlopen(url: TaintSink[RequestSend], data = ...,
                           timeout = ..., *, cafile = ..., capath = ...,
                           cadefault = ..., context = ...): ...
```
#### Eliding

As you can see from the above examples, defaulted values and function bodies can
both be elided with `...`. Additionally, type annotations *must* be entirely
omitted (not replaced with `...`), even when present on the declaration of the
function. This is done to make parsing taint annotations unambiguous.
