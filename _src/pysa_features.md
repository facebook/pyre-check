---
id: pysa-features
title: Feature Annotations
sidebar_label: Feature Annotations
---

Features (sometimes called breadcrumbs) are additional metadata that are
associated with taint flows. They can be useful for helping to filter out false
positives, or for zeroing in on high-signal subsets of a rule. Some are
automatically added during the analysis process, and there is a rich system for
manually specifying additional features.

## Manually Added Features

### `via` Feature Using `Via[]`

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
model file:

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

Pysa also supports attaching features to inferred flows, which allows you to
filter flows passing through a function without having to annotate the taint
yourself explicitly, and having the feature attached to all taint flowing
through the function. This is done by adding the `AttachToSource`,
`AttachToSink`, and `AttachToTito` annotations in a model file:

```python
# Attaching taint to sources.
def get_signed_cookie() -> AttachToSource[Via[signed]]: ...

# Attaching taint to sinks.
def HttpResponseRedirect.__init__(self, redirect_to: AttachToSink[Via[redirect]], *args, **kwargs): ...

# Attaching taint to taint-in-taint-out models.
def attach_features.tito_and_sink(arg: AttachToTito[Via[some_feature_name]]): ...
```

Pysa additionally supports attaching features to flows irrespective of sources,
sinks, and TITO, using the `AddFeatureToArgument` annotation:

```python
def add_feature_to_argument.add_feature_to_first(
  first: AddFeatureToArgument[Via[string_concat_lhs]],
  second
): ...
```

Note that **Pysa automatically adds some `via` features with special meaning**.
See the Automatic Features section for details.

### `via-value` Feature Using `ViaValueOf[]`

The `via-value` feature is similar to the `via` feature, however, it captures
*the value of the specified argument, rather than a feature name*. Note that
this only works for string literals, boolean literals, numeric literals, and enums.
For example, `via-value:Access-Control-Allow-Origin` might indicate that the string
literal `Access-Control-Allow-Origin` was used to set a header in a Django response.

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

In cases where the argument is not a constant, the feature will appear as
`via-value:<unknown:ARGUMENT_TYPE>`, where `ARGUMENT_TYPE` indicates how the
argument value is provided at the callsite. For a model such as this:

```python
def f (first, second, third) -> TaintSource[Test, ViaValueOf[second]]:...
```

The following function invocations will produce the features shown
in the comments:

```
f(*args)            # Generates via-value:<unknown:args>
f(**kwargs)         # Generates via-value:<unknown:kwargs>
f(second=foo)       # Generates via-value:<unknown:named>
f(foo, bar)         # Generates via-value:<unknown:positional>
f(*args, **kwargs)  # Generates via-value:<unknown:args_or_kwargs>
```

If the argument is not provided at the call site (e.g,
using the default value), the feature will appear as `via-value:<missing>`.

You can also associate a tag with a `via-value` feature to ensure that different
`via-value` annotations don't interfere with each other. Here's how you can retain
the information that the name of the header was being set:

```python
def django.http.response.HttpResponse.__setitem__(
    self,
    header: TaintSink[ResponseHeaderName],
    value: TaintSink[ResponseHeaderValue, ViaValueOf[header, WithTag["set-header"]]
): ...
```

The feature would now appear as `via-set-header-value:Access-Control-Allow-Origin`.

### `via-type` Feature Using `ViaTypeOf[]`

The `via-type` feature is nearly identical to the `via-value` feature, however,
it captures *the type of the specified argument, rather than it's value*. Pysa
will retrieve the type information for the argument from Pyre, and add a feature
such as `"via-type": "str"`, `"via-type": "typing.List[str]"`, or `"via-type":
"typing.Any"` (in the case Pyre doesn't have type information).

`ViaTypeOf` is useful for sinks such as `subprocess.run`, which accepts
`Union[bytes, str, Sequence]` for it's `arg` parameter. The `via-type` feature
can help identify which type the argument to `arg` actually had. Knowing the
type of the argument can help assess the severity of a given issue (user
controlled input in a `str` passed to `arg` is much easier to exploit for RCE
than user controlled input in one element of a `Sequence` passed to `arg`).

The `via-value` feature can be added anywhere that the `via` feature can be
added. It is added by specifying `ViaTypeOf[PARAMETER_NAME]`, where
`PARAMETER_NAME` is the name of the function parameter for which you would like
to capture the argument value:

```python
def subprocess.run(
    args: TaintSink[RemoteCodeExecution, ViaTypeOf[args]],
): ...
```

The `via-type` feature also supports adding tags, using the same syntax as the `via-value`
feature:

```python
def subprocess.run(
    args: TaintSink[RemoteCodeExecution, ViaTypeOf[args, WithTag["subprocess-arg"]]]
): ...
```

`ViaTypeOf` can also be used on attribute or global models, although tags are not supported. For example:
```python
my_module.MyClass.source: TaintSource[Test, ViaTypeOf] = ...
my_module.MyClass.sink: TaintSource[Test, ViaTypeOf] = ...
```

A standalone `ViaTypeOf` is also supported in this case, and is shorthand for `TaintInTaintOut[ViaTypeOf]`:
```python
my_module.MyClass.my_attribute: ViaTypeOf = ...
```

Note that `ViaTypeOf` on `Annotated` types will not include the annotations after the first type specified.
This is because Pyre does not store annotations as part of the type information. Consider the following code:
```python
from typing import Annotated

class Foo:
  x: Annotated[int, "foo"]
```

If there is a `ViaTypeOf` on `Foo.x` here, the feature shown on traces will be `via-type-of:typing.Annotated[int]`,
**not** `via-type-of:typing.Annotated[int, "foo"]`.

### Supporting Features Dynamically Using `ViaDynamicFeature[]`

In general, Pysa requires you to specify the list of features that are allowed. This encourages features
to be documented, and help avoid typos when writing features so that the features propagating in the analysis are
consistent with filters you might have on issues.

However, there might be very specific cases where you want to dynamically generate features, depending on artifacts
of the code. Most cases here can be handled by `via-type` and `via-value` features, however, you might be dealing with
dynamic code or metadata that the system can't detect. In these cases, Pysa allows skipping validation on features
by the use of `ViaDynamicFeature`. This syntax has identical behavior to `Via[]` except the lack of validation. Here's an example:

```python
def subprocess.run(
  args: TaintSink[RemoteCodeExecution, ViaDynamicFeature[subprocess_run_execution]]
): ...
```

## Automatic Features

### `via` Feature

In addition to the manually specified `via` features, Pysa automatically adds
some `via` features with special meaning such as `via:obscure:model`, `via:obscure:unknown-callee`,
`via:format-string`, and `via:tito`. `via:obscure:model` means that the flow passed
through code that Pysa does not have access to analyze, and thus some taint flow
assumptions were made. This can be a useful feature to filter out flows that may be more
noisy. `via:obscure:unknown-callee` means that a call cannot be resolved as the callee is
unknown (most likely because of missing type information). `via:format-string` means that
a flow passed through a [python f-string](https://www.python.org/dev/peps/pep-0498/) (`f"Variable:
{variable_name}"`) or a `str.format`. Tito stands for taint-in-taint-out which refers to taint
flows that enter a function via a parameter and then exit it in some form via
the return value. The `via:tito` feature is attached automatically to all such
flows.

### `type` Feature

The `type` feature is an automatically added feature which indicates that the
flow passes through a conversion to the specified type. This feature currently
only tracks conversion to numeric values (ie. `type:scalar`). This can be useful
for filtering out flows when numeric values are highly unlikely to result in an
exploitable flow, such as SQL injection or RCE.

### `first-field` Feature

The `first-field` feature is automatically added to flows for the first field
access on the flow. E.g., if `request` is a source, and the flow starts with
`request.f`, then `first-field:f` should be attached to the flow.

### `first-index` Feature

The `first-index` feature is an automatically added feature which indicates that
a flow starts with a dictionary access using the specified constant as the key.
This is useful in cases such as Django's `GET`/`POST`/`META` dictionaries on the
`HttpRequest` object. A flow that started with as access of the `HTTP_REFERER`
header from the `META` object would result in the `first-index:HTTP_REFERER`
feature being added.

### `has` Feature

The `has` features is a summary feature for `first-field` and `first-index`.
Thus, `has:first-index` simply indicates that there is at least one
`first-index:<name>` feature present, and similarly for `has:first-field`.


### `always-` Modifier on Features

The `always-` modifier will automatically be added to any of the above features,
when every single flow within an issue has the feature. For example, if an issue
captures flows from three different sources of user input into a SQL sink, the
`always-type:scalar` modifier would be added if all three of those flows pass
through a conversion to `int` before reaching the sink. Note that **the
`always-` version of a feature is _exclusive_ with the non-`always-` version**;
if `always-type:scalar` is present, `type:scalar` will not be present.

### `broadening` Feature

The `broadening` feature is automatically added whenever Pysa makes an
approximation about a taint flow. This is also referred as "taint collapsing"
because the taint is internally represented as a tree structure where edges are
attributes or indexes. Collapsing usually leads to tainting a whole object
instead of a single attribute of that object.

Pysa also provides more fine grained features for all scenario where we make
approximations.

#### `widen-broadening` Feature

The `widen-broadening` feature is added when number of tainted attributes of an
object reaches a certain threshold. For scalability reasons, Pysa cannot track
an infinite amount of attributes or indexes, and thus makes the approximation
that the whole object is tainted.

```python
d = {}
if condition:
  d["a"] = source()
  d["b"] = source()
  # c, d, e, etc.
else:
  d["1"] = source()
  d["2"] = source()
  # etc.
sink(d) # too many indexes, the whole `d` variable becomes tainted.
```

#### `tito-broadening` Feature

The `tito-broadening` feature is added when an object with tainted attributes is
propagated through a function to its return value. For correctness reasons, Pysa
makes the approximation that the whole returned object is tainted. This is
referred as "Taint-In-Taint-Out collapsing".

```python
def identity(x):
  return x

def foo():
  x = {"a": source(), "b": "foo"} # only `x["a"]` is tainted.
  y = identity(x)
  sink(y) # the whole `y` variable is tainted because of tito collapsing.
  sink(y['b']) # since `y` is tainted, any access to `y` is also tainted.
```

#### `issue-broadening` Feature

The `issue-broadening` feature is added when an object with a tainted attribute
reaches a taint sink. Pysa will consider the flow as valid even if the whole
object is not tainted.

```python
d = {"a": source(), "b": "foo"}
sink(d) # `d` itself is not tainted, but `d["a"]` is, thus we emit an issue.
```
