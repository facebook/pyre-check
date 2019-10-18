---
id: pyre-static-analysis-features
title: Feature Annotations
sidebar_label: Feature Annotations
---

## Overview

Features (sometimes called breadcrumbs) are additional metadata that are
associated with taint flows. They can be useful for helping to filter out false
positives, or for zeroing in on high-signal subsets of a rule. Some are
automatically added during the analysis process, and there is a rich system for
manually specifying additional features.

## `via` Feature

The `via` feature indicates that a flow passed through a point in the code, such
as a function parameter, that was annotated with the specified feature name. For
example, `via:getattr` might indicate that the flow passed through a call to
`getattr`

Feature names are declared in your `taint.config` file (the same file as
sources/sinks/rules) like this:

```python
features: [
    {
        name: "getattr",
        comment: "via getattr first parameter"
    },
    {
        name: "request_files",
        comment: "via django request.FILES"
    }
]
```

The `via` feature can be appended to `TaintSource`, `TaintSink` and
`TaintInTaintOut` annotations, to add extra metadata to any flow that goes
through that annotated function/parameter/attribute. This is done by adding
`Via[FEATURE_NAME]` within square brackets after the `TaintXXXX` annotation in a
stubs file:

```python
# Augmenting TaintSource
django.http.request.HttpRequest.FILES: TaintSource[UserControlled, Via[request_files]] = ...

# Augmenting TaintInTaintOut
def getattr(
    o: TaintInTaintOut[Via[getattr]],
    name: TaintSink[GetAttr],
    default: TaintInTaintOut[LocalReturn],
): ...
```

Pyre also supports attaching features to inferred flows, which allows you to
filter flows passing through a function without having to annotate the taint
yourself explicitly, and having the feature attached to all taint flowing
through the function. This is done by adding the `AttachToSource` and
`AttachToSink` annotations in a stubs file:

```python
# Attaching taint to sources.
def get_signed_cookie() -> AttachToSource[Via[signed]]: ...

# Attaching taint to sinks.
def HttpResponseRedirect.__init__(self, redirect_to: AttachToSink[Via[redirect]], *args, **kwargs): ...
```

Note that **Pyre automatically adds some `via` features with special meaning**
such as `via:obscure`, `via:format-string`, and `via:tito`. `via:obscure` means
that the flow passed through code that Pyre does not have access to analyze, and
thus some taint flow assumptions were made. This can be a useful to filter out
flows that may be more noisy. `via:format-string` means that a flow passed
through a [python f-string](https://www.python.org/dev/peps/pep-0498/)
(`f"Variable: {variable_name}"`). Tito stands for taint-in-taint-out which
refers to taint flows that enter a function via a parameter and then exit it in
some form via the return value. The `via:tito` feature is attached automatically
to all such flows.

## `via-value` Feature

The `via-value` feature is similar to the `via` feature, however, it captures
*the value of the specified argument, rather than a feature name*. Note that
this only works for string literals and enums. For example,
`via-value:Access-Control-Allow-Origin` might indicate that the string literal
`Access-Control-Allow-Origin` was used to set a header in a Django response.

The `via-value` feature can be added anywhere that the `via` feature can be
added. It is added by specifying `ViaValueOf[PARAMETER_NAME]`, where
`PARAMETER_NAME` is the name of the function parameter for which you would like
to capture the argument value. To continue the above example, this is how you
would capture the name of a header being set on a Django `HttpResponse`:

```python
def django.http.response.HttpResponse.__setitem__(
    self,
    header: TaintSink[ResponseHeaderName],
    value: TaintSink[ResponseHeaderValue, ViaValueOf[header]]
): ...
```


## `type` Feature

The `type` feature is an automatically added feature which indicates that the
flow passes through a conversion to the specified type. This feature currently
only tracks conversion to numeric values (ie. `type:scalar`). This can be useful
for filtering out flows when numeric values are highly unlikely to result in an
exploitable flow, such as SQL injection or RCE.

## `first-field` Feature

The `first-field` feature is automatically added to flows for the first field
access on the flow. E.g., if `request` is a source, and the flow starts with
`request.f`, then `first-field:f` should be attached to the flow.

## `first-index` Feature

The `first-index` feature is an automatically added feature which indicates that
a flow starts with a dictionary access using the specified constant as the key.
This is useful in cases such as Django's `GET`/`POST`/`META` dictionaries on the
`HttpRequest` object. A flow that started with as access of the `HTTP_REFERER`
header from the `META` object would result in the `first-index:HTTP_REFERER`
feature being added.

## `has` Feature

The `has` features is a summary feature for `first-field` and `first-index`.
Thus, `has:first-index` simply indicates that there is at least one
`first-index:<name>` feature present, and similarly for `has:first-field`.


## `always-` Modifier on Features

The `always-` modifier will automatically be added to any of the above features,
when every single flow within an issue has the feature. For example, if an issue
captures flows from three different sources of user input into a SQL sink, the
`always-type:scalar` modifier would be added if all three of those flows pass
through a conversion to `int` before reaching the sink. Note that **the
`always-` version of a feature is _exclusive_ with the non-`always-` version**;
if `always-type:scalar` is present, `type:scalar` will not be present.
