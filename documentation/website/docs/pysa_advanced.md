---
id: pysa-advanced
title: Advanced Topics
sidebar_label: Advanced Topics
---

This page documents less straightforward bits of Pysa.

## Obscure models

When Pysa does not have enough information about a function or method, it will
make basic assumptions about its behavior. This is referred to as an **obscure
model**. Most notably, it assumes that the function or method propagates the
taint from its arguments to its return value.

This usually happens when Pysa doesn't know about the callee of a function call:

```python
def foo(f: Any):
    x = input()
    y = f(x) # no information about `f`, y will be considered tainted.
    eval(y)
```

Functions and methods defined in type stubs or in a different language (for
instance, in C or C++ binding) will also be treated as obscure models.

To prevent a function or method to be marked as obscure, one can use the
`@SkipObscure` taint annotation in a `.pysa` file:

```python
@SkipObscure
def module.foo(): ...
```

## Tainting Specific `kwargs`

Sometimes, a function can have potential sinks mixed together with benign
parameters in the keyword arguments (`kwargs`) that it accepts. In these cases,
tainting the whole `kwargs` variable will result in false positives when tainted
data flows into a benign `kwarg`. Instead, for a function like this:

```python
def eval_and_log(**kwargs):
    eval(kwargs["eval"])
    logging.debug(kwargs["log"])
```

We can lie a bit in our `.pysa` file, and break out the dangerous argument for
tainting:

```python
def eval_and_log(*, eval: TaintSink[RemoteCodeExecution], **kwargs): ...
```

This allows us to catch flows only into the `eval` keyword argument.

## Instance attributes versus class attributes

Models can specify sources and sinks on attributes, following the type annotation
syntax:

```python
django.http.request.HttpRequest.GET: TaintSource[UserControlled]
```

Any access to `request.GET` will be tainted when `request` is an instance of
`HttpRequest` or any of its children. However, note that the access to the class
attribute (i.e, `HttpRequest.GET`) won't be considered tainted.

To specify sources and sinks on class attributes, use the `__class__` prefix:

```python
django.http.request.HttpRequest.__class__.GET: TaintSource[UserControlled]
```

To specify a source on both the class attribute and instance attribute, simply
use both lines.

## Literal String Sources And Sinks

Some security vulnerabilities are best captured by modeling strings of a given
form flowing to dangerous functions, or format strings that match a pattern getting
tainted data passed in.

To mark all literal strings matching a pattern as sources, you first need to add a
regular expression corresponding to the pattern to your `taint.config`:

```json
{
  "sources": [
    {
      "name": "IPAddress"
    }
  ],
  "implicit_sources": {
     "literal_strings": [
       {
         "regexp": "\\d{1,3}(\\.\\d{1,3})+",
         "kind": "IPAddress",
         "description": "String that looks like an IP address."
       }
     ]
  }
}
```

With this regex in place, whenever Pysa sees a string such as `123.456.789.123`, it will flag it
as a taint source with the kind `IPAddress`.

```python
def test() -> None:
    ip_address = "123.456.789.123"
    dont_pass_an_ip_address(ip_address) # Pysa will now flag this.
```

The converse of supporting literal strings as sinks is also supported, for data flowing into a tainted string. The
syntax allows you to model data being used to format strings, like f-strings, manual string formatting, the string `format()` method, and printf-style string formatting with `%`.

Template strings and and manual string formatting with more than two subexpressions are not yet supported.

To add a literal sink, first add the literal_sink to your configuration

```json
{
  "sinks": [
    { "name": "MayBeRendered" },
    { "name": "MayBeSQL" }
  ],
  "implicit_sinks": {
     "literal_strings": [
       {
         "regexp": "^<.*>$",
         "kind": "MayBeRendered",
         "description": "Indicates a string whose contents may be rendered."
       },
       {
         "regexp": "^SELECT *.",
         "kind": "MayBeSQL",
         "description": "Indicates a string whose contents may be a SQL query."
       }

     ]
  }
```

Now, Pysa will treat any values flowing into a each of the following as a regular sink:

```python
def may_render(parameter: str) -> None:
    result = f"<content={parameter}>"
    result = "<content={}>".format(parameter)
    result = "<content%s>" % (parameter,)
```
As well as values flowing into each of these as a regular sink:
```python
def build_sql_query(columns: str) -> None:
    result = f"SELECT {columns} FROM users;"
    result = "SELECT {} FROM users;".format(columns)
    result = "SELECT %s FROM users" % (columns,)
    result = "SELECT " + columns + " FROM users;"
```


## Combined Source Rules

Some security vulnerabilities are better modeled as *multiple* sources reaching
a sink. For example, leaking credentials via `requests.get` could be modeled as
user controlled data flowing into the `url` parameter and credentials flowing
into the `params` parameter. These flows can be modeled by *combined source
rules*.

Sources for combined source rules are declared as normal in `taint.config`.
Sinks, however, need to be unique to the combined source rule and are declared inside
the rule definition. The rule itself is declared in the `combined_source_rules`
top level entry. The rule lists all the same things as a regular rule, but also ties
labels to its sources:

```json
{
  "sources": [
    { "name": "UserControlled" },
    { "name": "Credentials" }
  ],
  "combined_source_rules": [
    {
       "name": "Credentials leaked through requests",
       "sources": { "url": "UserControlled", "creds": "Credentials" },
       "partial_sink": "UserControlledRequestWithCreds",
       "code": 1,
       "message_format": "Credentials leaked through requests"
    }
  ]
}
```

Sources are declared as normal in `.pysa` files. Instead of specifying sinks
with a `TaintSink` annotation, however, `PartialSink` annotations are used to
specify where each source needs to flow for the combined source rule. These
`PartialSink` must reference the labels that were declared in
`multi_sink_labels`:

```python
def requests.api.get(
  url: PartialSink[UserControlledRequestWithCreds[url]],
  params: PartialSink[UserControlledRequestWithCreds[creds]],
  **kwargs
): ...
```

With the above configuration, Pysa can detect cases where `UserControlled` flows
into `url` and `Credentials` flow into `params` *at the same time*.

## Prevent Inferring Models with `SkipAnalysis`

In addition to the models defined in `.pysa` files, Pysa will infer models for
functions based what sources, sinks, etc. they call in their body. The
`SkipAnalysis` annotation can be used to prevent Pysa from inferring models, and
instead force it to use only the user defined models for determining taint flow:

```python
@SkipAnalysis
def qualifier.dont_generate_models(argument): ...
```

`SkipAnalysis` can be applied at the class level as a shorthand to prevent pysa
from infering models for all functions in a class:

```python
class skip_analysis.SkipMe(SkipAnalysis): ...
```

## Ignoring overrides

When a method is called on a base class, Pysa has to assume that that call could
actually invoke any subclass methods that override the base class's method. For
heavily overriden methods, this can lead to both performance impacts and false
positives. When running Pysa, you may see messages such as this in the output:

```
2020-09-02 09:25:50,677 WARNING `object.__init__` has 106 overrides, this might slow down the analysis considerably.
```

The above message indicates that 106 subclasses of `object` have overridden
`__init__`. If Pysa sees taint flowing into `object.__init__`, then it will
treat all 106 overrides of `object.__init__` as also receiving that taint.

The `@SkipOverrides` decorator can be applied to deal with false positives or
performance issues from having too many overrides on a given function:

```python
@SkipOverrides
def object.__init__(self): ...
```

This annotation will cause Pysa not to propagate taint into to and from
overridden methods on subclasses, when analyzing functions that call the
overriden method on the base class.

`maximum_overrides_to_analyze` can be added the the `options` block of
`taint.config` to limit the number of overrides that Pysa will analyze:

```json
{
  "sources": [],
  "sinks": [],
  "features": [],
  "rules": [],
  "options": {
    "maximum_overrides_to_analyze": 60
  }
}
```

This can speed up the analysis, but it will lead to false negatives, because
Pysa will only propagate taint to or from 60 (in the case of the above example)
overriden methods on subclasses. The remaining overriding methods will be
ignored and treated as if they weren't actually overriding the base class
method.

By default, Pysa skips overrides on some functions that are typically
problematic. You can find the full list of default-skipped functions in
[`stubs/taint/skipped_overrides.pysa`](https://github.com/facebook/pyre-check/blob/main/stubs/taint/skipped_overrides.pysa)

## Limit the trace length for better signal and performance

By default, Pysa will find all flows from sources to sinks matching a rule.
This can lead to very long traces which are hard to understand and tend to be
false positives. This also brings down the performance a lot.

Pysa provides a `--maximum-trace-length <integer>` command line argument which
limits the length of traces that it finds. In general, this will also make Pysa
faster.

This option can also be added in the `taint.config` as follows:

```json
{
  "sources": [],
  "sinks": [],
  "features": [],
  "rules": [],
  "options": {
    "maximum_trace_length": 20
  }
}
```

Note that this is not a silver bullet and that this might hide security
vulnerabilities. Use it with caution.

## Limit the tito depth for better signal and performance

Pysa infers automatically when a function propagate the taint from one argument
to its return value. This is called tito, for Taint In Taint Out. In practice,
infering it can be very expensive since the taint can go through an arbitrary
number of hops (i.e, depth).

For instance:

```python
def foo(x):
  return x
def bar(x):
  return foo(x)
def baz(x):
  return bar(x)
```

In this example, `baz` propagates the taint on its argument to the return value
using 3 hops.

Pysa provides a `--maximum-tito-depth <integer>` command line argument which
limints the depth of inferred propagations. In combination with the trace length
limit, this usually makes Pysa faster.

This option can also be added in the `taint.config` as follows:

```json
{
  "sources": [],
  "sinks": [],
  "features": [],
  "rules": [],
  "options": {
    "maximum_tito_depth": 20
  }
}
```

# Inlining Decorators during Analysis

By default, Pysa ignores issues that arise in the bodies of decorators. For example, it misses issues like decorators logging data. In the code below, Pysa will not catch the flow from `loggable_string` to the sink within the decorator `with_logging`:

```python
def with_logging(f: Callable[[str], None]) -> Callable[[str], None]:

  def inner(y: str) -> None:
    log_to_my_sink(y)
    f(y)

  return inner

@with_logging
def foo(z: str) -> None:
  print(z)

foo(loggable_string)
```

However, Pysa has the ability to inline decorators within functions before analyzing them so that it can catch such flows. This is currently an experimental feature hidden behind the `--inline-decorators` flag.

## Prevent Inlining Decorators with `SkipDecoratorWhenInlining`

Decorator inlining comes at the cost of increasing the analysis time and also increasing the lengths of traces. If you would like to prevent certain decorators from being inlined, you can mark them in your `.pysa` file using `@SkipDecoratorWhenInlining`:

```python
# foo.pysa
@SkipDecoratorWhenInlining
def foo.decorator_to_be_skipped(f): ...
```

```python
# foo.py
@decorator_to_be_skipped
def bar(x: int) -> None:
  pass
```

This will prevent the decorator from being inlined when analyzing `bar`. Note that we use `@SkipDecoratorWhenInlining` on the decorator that is to be skipped, not the function on which the decorator is applied.

## Single trace sanitizers with `@SanitizeSingleTrace`

Sanitizers, as described in the [Overview](pysa_basics.md), are applied in both
the forward (i.e source) trace and backward (i.e sink) trace.

For instance, with the given `.pysa` file:

```python
@Sanitize(TaintInTaintOut[TaintSink[RemoteCodeExecution]])
def shlex.quote(x): ...
```

And the following Python code:

```python
import subprocess
from shlex import quote

def quoted_input():
  x = input() # source 'UserControlled'
  y = quote(x)
  return y

def echo(argument):
  subprocess.run(f'/bin/echo {argument}', shell=True) # sink 'RemoteCodeExecution'

def issue():
  x = quoted_input() # source trace: input -> quoted_input -> issue
  echo(x) # sink trace: issue -> echo -> subprocess.run
```

Pysa will NOT find an issue here, as expected.
This is because during the propagation of the 'UserControlled' source in the
forward trace, pysa remembers that it was sanitized for the sink 'RemoteCodeExecution'.

However, Pysa provides a simpler version of sanitizers, which only sanitizes in the
forward trace or the backward trace:

```python
@SanitizeSingleTrace(TaintSource)
def f(): ...

@SanitizeSingleTrace(TaintSource[UserControlled])
def g(): ...

@SanitizeSingleTrace(TaintSink)
def h(): ...

@SanitizeSingleTrace(TaintSink[RemoteCodeExecution])
def i(): ...
```

These sanitizers are a lot cheaper and could save analysis time. However, these
might introduce false positives, so we recommend to use the default sanitizers.

## Taint In Taint Out Transforms

Taint in taint out transforms can be used to capture more precise flows.

As an example:
```python
def read_file(path):
  with open(path, "r") as f:
    content = f.read()
  return content
```
Without taint in taint transforms we can write a rule that captures a `UserControlled` path is `read`. Such a rule can be made much higher signal if we can detect that `content` is also `ReturnedToUser`. We can use taint in taint out transforms to stitch the two flows together. We mark `read` with a taint in taint out transform `FileRead`, and the rule becomes `UserControlled -> FileRead -> ReturnedToUser`.

To contrast with feature annotations, there are two differences:
* The filtering is done during analysis itself, and limits the issues generated (as opposed to a post-processing step by the user)
* Taint in taint out transforms can be used to reason about the order of events

### Syntax
In `taint.config`, one can specify `transforms` to define new transforms. Each transform is defined by following fields:
* `name`: name of the transform, this is used when defining rules, as well as writing models
* `comment`: description of the transform

```
{
  ...
  "transforms": [
    {
      "name": "MyTransform",
      "comment": "This is my transform"
    },
    ...
  ],
  ...
}
```

Then, one may use these transforms in `rules` as follows:
```
 {
  ...
  "rules": [
    {
      "name": ...,
      "code": ...,
      "sources": ["SourceA"],
      "transforms": ["MyTransform1", "MyTransform2"],
      "sinks": ["SinkB"],
      "message_format": "[{$sources}] transformed by [${transforms}] may reach [${sinks}]"
    },
    ...
  ],
  ...
}
```
Intuitively, one can think of the rule above as `SourceA -> MyTransform1 -> MyTransform2 -> SinkB`. The order is important.

Finally, in `.pysa` model files a taint transform can be specified using a `TaintInTaintOut[Transform[...]]` annotation, where the parameter is the name of the transform.
```
def my_function(arg: TaintInTaintOut[Transform[MyTransform]]): ...
```

### Semantics
```
  y = my_function(x)
```
If `x` has source taint `SourceA`, the taint of `y` is `MyTransform:SourceA`. This will correspond to matching `SourceA -> MyTransform` in a rule. Likewise, if `y` has sink taint `SinkB`, then the taint of `x` is `MyTransorm:SinkB`. This will correspond to matching `MyTransform -> SinkB` in a rule.

Note that a transform modifies the taint itself. Hence, if a flow passes through a transform, it will no longer match rules which do not contain the transform.
```
RuleX: SourceA -> SinkB
RuleY: SourceA -> MyTransform -> SinkB
Flow1: SourceA -> SinkB
Flow2: SourceA -> MyTransform -> SinkB
```
`Flow1` matches `RuleX` but not `RuleY`. `Flow2` matches `RuleY` but not `RuleX`.

Consider the scenario where we have an additional rule:
```
RuleZ: SourceC -> SinkD
```
If transform `MyTransform` is applied to taint `SourceC`, there is no possible rule it can possibly match. As an optimization, we check for this continuously in our analysis and filter out eagerly.

Also note that the existing TaintInTaintOut annotation semantics of TITO being assumed (instead of inferred) on the argument are unchanged.
