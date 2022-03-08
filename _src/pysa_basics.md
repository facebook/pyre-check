---
id: pysa-basics
title: Overview
sidebar_label: Overview
---

import Internal from './fb/pysa_basics_internal.md';
import YouTube from 'react-youtube';

Pyre has applications beyond type checking python code: it can also run static
analysis, more specifically called **Taint Analysis**, to identify potential security issues.
The Python Static Analyzer feature of Pyre is usually abbreviated to Pysa
(pronounced like the Leaning Tower of Pisa).

<YouTube videoId="LDxAczqkBiY" />

<Internal />

## Taint Analysis

**Tainted data** is data that must be treated carefully. Pysa works by tracking
flows of data from where they originate (sources) to where they terminate in a
dangerous location (sinks). For example, we might use it to track flows where
user-controllable request data flows into an `eval` call, leading to a remote
code execution vulnerability. This analysis is made possible by user-created
models which provide annotations on source code, as well as rules that define
which sources are dangerous for which sinks. Pysa comes with many pre-written
models and rules for builtin and common python libraries.

Pysa propagates taint as operations are performed on tainted data. For example,
if we start with a tainted integer and perform a number of operations on it, the
end results will still be tainted:

```python
x = some_function_that_returns_a_tainted_value() # 'x' is marked as tainted
y = x + 10
s = str(x)
f = f"Value = {s}" # 'f' is marked with the same taint 'x' had
```

Pysa will only analyze the code in the repo that it runs on, as well as code in
directories listed in the `search_path` of your
[`.pyre_configuration`](configuration.md) file. It does not see the source of
your dependencies. **Just because** ***you*** **can see code in your editor
does not mean Pysa has access to that code during analysis.** Because of this
limitation, Pysa makes some simplifying assumptions. If taint flows into a
function Pysa doesn't have the source for, it will assume that the return type
of that function has the same taint. This helps prevents false negatives, but can
also lead to false positives.

When an object is tainted, that means that all attributes of that object are
also tainted. Note that this is another source of potential false positives,
such as taint flows that include `some_obj.__class__`. This means that Pysa
will detect all of the following flows:

```python
x = some_source() # 'x' is marked as tainted

some_sink(x) # This is detected
some_sink(x.some_attribute) # This is also detected
some_sink(x.__class__) # This is (unfortunately) also detected
```

## Configuration

Pysa uses two types of files for configuration: a single `taint.config` file,
and an unlimited number of files with a `.pysa` extension. The `taint.config`
file is a JSON document which stores definitions for *sources*, *sinks*, *features*,
and *rules* (discussed below). The `.pysa` files are model files (also discussed
below) which annotate your code with the *sources*, *sinks*, and *features* defined in
your `taint.config` file. Examples of these files can be found in the [Pyre
repository](https://github.com/facebook/pyre-check/tree/main/stubs/taint).

These files live in the directory configured by `taint_models_path` in your
`.pyre_configuration` file. Any `.pysa` file found in this folder will be parsed
by Pysa and the models will be used during the analysis.


## Sources

Sources are where tainted data originates. They are declared in your
`taint.config` file like this:

```json
"sources": [
    {
        "name": "Cookies",
        "comment": "used to annotate cookie sources"
    }
]
```

Models that indicate what is a source are then defined in `.pysa`
files. Sources are declared with the same syntax as [type annotations in Python
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
django.http.request.HttpRequest.COOKIES: TaintSource[Cookies]
```

When tainting an entire class, any return from a method or access of an
attribute of the class will count as a returning tainted data. The specifics of
these model files are discussed further in the Models section.

```python
# Class source:
class BaseException(TaintSource[Exception]): ...
```

When tainting indexable return types such as `Dict`s, `List`s, and `Tuple`s, the
`AppliesTo` syntax can be used to only mark a portion of the return type as
tainted:

```python
def applies_to_index.only_applies_to_nested() -> AppliesTo[0, AppliesTo[1, TaintSource[Test]]]: ...
def applies_to_index.only_applies_to_a_key() -> AppliesTo["a", TaintSource[Test]]: ...
```

Note that `AppliesTo` syntax can also be applied to fields of classes and globals,
which can be particularly helpful when annotating dictionaries.

```python
# Source file: a.py
class C:
    dictionary_field = {"text": "will_be_tainted"}

# Model file: models.pysa
a.C.dictionary_field: AppliesTo["text", TaintSource[Test]]
```

## Sinks

Sinks are where tainted data terminates. They are declared in your
`taint.config` file like this:

```json
"sinks": [
  {
    "name": "SQL",
    "comment": "use to annotate places of SQL injection risk"
  }
]
```

Models that indicate what is a sink are then defined in `.pysa` files. Sinks can
be added to the same files as sources. Like sources, sinks are declared with the
same syntax as [type annotations in Python
3](https://docs.python.org/3/library/typing.html). Function parameters, class
attributes, and even whole classes can be declared as sinks by adding
`TaintSink[SINK_NAME]` where you would add a python type:

```python
# Function parameter sink
def sqlite3.dbapi2.Cursor.execute(self, sql: TaintSink[SQL], parameters): ...

# Attribute sink
file_name.ClassName.attribute_name: TaintSink[RemoteCodeExecution]
```

When tainting an entire class, any flow into a method or attribute of the class
will count as a flow to a taint sink. The specifics of these model files are
discussed further in the Models section.

```python
# Entire class sink
class BaseException(TaintSink[Logging]): ...
```

### Implicit Sinks

Implicit sinks are program expressions that we want to act as sinks, but that
cannot be specified via taint signatures in `.pysa` files.  Currently, only
conditional tests are supported as implicit sinks. This allows writing rules
that track whether a particular source is used in a conditional test
expression.

```json
"implicit_sinks": {
  "conditional_test": [ <your kind> ]
}
```

## Rules

Rules declare which flows from source to sink we are concerned about. They are
declared in your `taint.config` file like this:

```json
"rules": [
  {
    "name": "SQL injection.",
    "code": 1,
    "sources": [ "UserControlled" ],
    "sinks": [ "SQL" ],
    "message_format": "Data from [{$sources}] source(s) may reach [{$sinks}] sink(s)"
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

Sanitizers break a taint flow by removing taint from data. Models that indicate
sanitizing functions are defined in `.pysa` files. Sanitizers can be added to
the same files as sources and sinks. Functions are declared as sanitizers by
adding a special decorator:

```python
# This will remove any taint passing through a function, regardless of whether
# it is a taint source returned by this function, taint reaching sinks within
# the function via 'text', or taint propagateing through 'text' to the
# return value.
@Sanitize
def django.utils.html.escape(text): ...
```

This annotation is useful in the case of explicit sanitizers such as `escape`,
which helps prevent cross site scripting (XSS) by escaping HTML characters. The
annotation is also useful, however, in cases where a function is not intended to
sanitize inputs, but is known to always return safe data despite touching
tainted data. One such example could be `hmac.digest(key, msg, digest)`, which
returns sufficiently unpredictable data that the output should no longer be
considered attacker-controlled after passing through.

Sanitizers can also be scoped to only remove taint returned by a function,
passing through a specific argument, or passing through all arguments.

```python
# This will remove any taint returned by this function, but allow taint
# to be passed in to the function via 'argument'. It also prevents taint
# from propagating from any argument to the return value.
def module.sanitize_return(argument) -> Sanitize: ...

# This prevents any taint which passes through 'argument' from reaching a sink within
# the function, but allows taint which originates within the function to be returned.
def module.sanitize_parameter(argument: Sanitize): ...

# This prevents any taint which passes through any parameter from entering the function,
# but allows taint which originates within the function to be returned. It also prevents
# taint from propagating from any argument to the return value.
@Sanitize(Parameters)
def module.sanitize_all_parameters(): ...

# This will remove any taint which propagates through any argument to the return
# value, but allow taint sources to be returned from the function as well as
# allow taint to reach sinks within the function via any argument.
@Sanitize(TaintInTaintOut)
def module.sanitize_tito(a, b, c): ...

# Same as before, but only for parameter 'b'
def module.sanitize_tito_b(a, b: Sanitize[TaintInTaintOut], c): ...
```

Pysa also supports only sanitizing specific sources or sinks to ensure that the
sanitizers used for a rule don't have adverse effects on other rules. The syntax
used is identical to how taint sources and sinks are specified normally:

```python
# Sanitizes only the `UserControlled` source kind.
def module.return_not_user_controlled() -> Sanitize[TaintSource[UserControlled]]: ...

# Sanitizes both the `SQL` and `Logging` sinks.
def module.sanitizes_sql_and_logging_sinks(
  flows_to_sql: Sanitize[TaintSink[SQL]],
  logged_parameter: Sanitize[TaintSink[Logging]],
): ...
```

For taint-in-taint-out (TITO) sanitizers, Pysa supports only sanitizing specific
sources and sinks through TITO:

```python
# With this annotation, whenever `escape(data)` is called, the UserControlled taint of `data`
# will be sanitized, whereas other taint that might be present on `data` will be preserved.
@Sanitize(TaintInTaintOut[TaintSource[UserControlled]])
def django.utils.html.escape(text): ...

@Sanitize(TaintInTaintOut[TaintSink[SQL, Logging]])
def module.sanitize_for_logging_and_sql(): ...
```

Note that you can use any combination of annotations, i.e sanitizing specific
sources or specific sinks, on the return value, a specific parameter or all parameters:

```python file=source/interprocedural_analyses/taint/test/integration/sanitize.py.pysa start=DOCUMENTATION_RETURN_SANITIZERS_START end=DOCUMENTATION_RETURN_SANITIZERS_END

```

```python file=source/interprocedural_analyses/taint/test/integration/sanitize.py.pysa start=DOCUMENTATION_PARAMETER_SPECIFIC_SANITIZERS_START end=DOCUMENTATION_PARAMETER_SPECIFIC_SANITIZERS_END

```

```python file=source/interprocedural_analyses/taint/test/integration/sanitize.py.pysa start=DOCUMENTATION_PARAMETERS_SANITIZERS_START end=DOCUMENTATION_PARAMETERS_SANITIZERS_END

```

Attributes can also be marked as sanitizers to remove all taint passing through
them:

```python
django.http.request.HttpRequest.GET: Sanitize
```

Sanitizing specific sources and sinks can also be used with attributes:

```python
def module.Node.id: Sanitize[TaintSource[UserSecrets]] = ...
def module.Node.id: Sanitize[TaintSink[Logging]] = ...
```

Note that sanitizers come with the risk of losing legitimate taint flows. They
remove all taint and aren't restricted to a specific rule or individual source
to sink flows. This means you need to ensure you aren't potentially affecting
other flows when you add a sanitizer for a flow you care about. For this reason,
some of the above sanitizer examples might not be a good idea to use. For example,
if you are trying to track flows where SQL injection occurs, the `escape` sanitizer
removing all taint kinds would prevent you from seeing any flows where data going
into your SQL query happened to be HTML escaped. The best practice with sanitizers,
then, is to make them as specific as possible. It's recommended to sanitize
specific sources and sinks over using the general `@Sanitize`, `-> Sanitize` or
`: Sanitize` annotations.

## Taint Propagation

Sometimes, Pysa is unable to infer that tainted data provided as an argument to a function will be returned by that function. In such cases, Pysa models can be annotated with `TaintInTaintOut[LocalReturn]` to encode this information for using during the analysis. This annotation can be applied to any parameter, including `self`, and is useful in scenarios such as when retrieving a value from a collection containting tainted data:

```python
# This tells Pysa that if a 'dict' contains tainted data, the result
# of calling 'get' on that dict will also contain tainted data
def dict.get(self: TaintInTaintOut[LocalReturn], key, default): ...
```

Note that `TaintInTaintOut` (ie. without square brackets) is also accepted and can be used as a short hand for `TaintInTaintOut[LocalReturn]`. `LocalReturn` is ony ever *required* when using the `Updates` syntax below and wanting to preserve the `LocalReturn` behaviour.

For performance reasons, Pysa does not keep track of when functions place taint into their parameters, such as when a function adds a tainted entry to a list it received (with some notable exceptions for taint assigned to `self` in a constructor or property). The `TaintInTaintOut[Updates[PARAMETER]]` annotation can be used to work around Pysa's limitations by telling Pysa that taint will flow the the annotated parameter into the parameter named `PARAMETER`:

```python
# This tells Pysa that if 'dict.update' is called with tainted data,
# then the 'self' object (ie. the dictionary itself) should then be
# considered tainted.
def dict.update(self, __m: TaintInTaintOut[Updates[self]]): ...
```

Note that [feature annotations](pysa_features.md) may also be placed inside the `[]` blocks of `TaintInTaintOut[...]` annotations.

## Features

Feature annotations are also placed in your `taint.config` and `.pysa` files.
This is a larger topic and will be covered in detail on [its own page](pysa_features.md).

## Model files

### Usage

By default, Pysa computes an inferred model for each function and combines it
with any declared models in `.pysa` files (of which there can be more than one).
The union of these models and their annotations will be used. For example,
cookies are both user controlled and potentially sensitive to log, and Pysa
allows us apply two different annotations to them:

```python
django.http.request.HttpRequest.COOKIES: TaintSource[UserControlled]
django.http.request.HttpRequest.COOKIES: TaintSource[Cookies]
```

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
django.http.request.HttpRequest.GET: TaintSource[UserControlled]
```

#### Matching signatures

The signature of any modeled function needs to match the signature of the
function, as seen by Pyre. Note that Pyre doesn't always see the definition of
the functions directly. If [`.pyi` stub
files](https://www.python.org/dev/peps/pep-0484/#stub-files) are present, Pyre
will use the signatures from those files, rather than the actual signature from
the function definition in your or your dependencies' code. See the [Gradual
Typing page](gradual_typing.md) for more info about these `.pyi` stubs.

This matching signature requirement means that all parameters being modelled must
be named identically to the parameters in the corresponding code or `.pyi` file.
Unmodelled parameters, `*args`, and `**kwargs` may be included, but
are not required. When copying parameters to your model, all type information
must be removed, and all default values must be elided (see below).

If a function includes an `*` that indicates [keyword only
parameters](https://www.python.org/dev/peps/pep-3102/), or a `/` that indicates
[positional-only parameters](https://www.python.org/dev/peps/pep-0570/), then
that may be included in your model. Note that unlike when modeling named parameters,
you need to include all positional only parameters the model so that Pysa knows what
position is being tainted.

For example, `urllib.request.urlopen` has the following signature:

```python
def urlopen(url, data=None, timeout=socket._GLOBAL_DEFAULT_TIMEOUT, *, cafile=None,
            capath=None, cadefault=False, context=None):
```

Given that signature, either of the following models are acceptable:

```python
def urllib.request.urlopen(url: TaintSink[RequestSend], data,
                           timeout, *, cafile, capath,
                           cadefault, context): ...
def urllib.request.urlopen(url: TaintSink[RequestSend]): ...
```

Pysa will complain if the signature of your model doesn't match the
implementation. When working with functions defined outside your project, where
you don't directly see the source, you can use [`pyre query`](querying_pyre.md)
with the `signature` argument to have Pysa dump it's internal model of a
function, so you know exactly how to write your model.

#### Eliding

As you can see from the above examples, unmodelled parameters and function bodies can
both be elided with `...`. Additionally, type annotations *must* be entirely
omitted (not replaced with `...`), even when present on the declaration of the
function. This is done to make parsing taint annotations unambiguous.

<FbInternalOnly>

## Next Steps

Ready to start writing some models? Check out our docs on the
[end-to-end process of shipping pysa models.](fb/pysa_shipping_rules_models_internal.md)

</FbInternalOnly>
