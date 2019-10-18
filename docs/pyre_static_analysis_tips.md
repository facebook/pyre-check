---
id: pyre-static-analysis-tips
title: Development Tips
sidebar_label: Development Tips
---

## Features/Limitations

### Inheritance

Pyre is aware of inheritance, so you can add taint annotations to a base class,
and Pyre will detect when the tainted attribute or function is accessed via a
child class. For example, this flow will be detected during static analysis:

```python
class Parent:
    def some_source(self): # Annotated as a source
        pass

class Child(Parent):
    pass

child = Child()
some_sink(child.some_source()) # Detected as a tainted flow
```

Additionally, Pyre is aware that child classes can be used anywhere a parent
classes's type is present. If you access a method on a parent class and the
implementation on any child class returns taint, Pyre will detect that and
treat the return from the parent class as tainted. For example, this will be
detected as a tainted flow during static analysis:

```python
class Parent:
    def some_fn(self):
        """Benign function with no annotations"""
        pass

class Child(Parent):
    def some_fn(self):
        """Function returning a tainted value"""
        return get_some_tainted_value()

def fn(obj: Parent):
    some_sink(obj.some_fn()) # Detected as a tainted flow
```

**A huge caveat here is that Pyre needs to be aware of these inheritance
relationships and function definitions for it to work.** Code that lives
outside the repo under analysis might not be visible to Pyre, so these
inheritances/implementations may be missed. See the Stubs section below for
more details.

### Stubs

The concept of stubs is covered in general _[here](pyre_static_analysis_basics.md)_, but this
section in particular will cover specific issues you may encounter with the
`.pyi` kind of stubs. These stubs can be used to prevent pyre errors for types
that live outside the codebase you are running Pyre on. The simplest stubs are
just empty files in the root of the `stubs` directory (assuming you have a
`stubs` directory specified in the `search_path` list in your
`.pyre_configuration` file). An empty stub basically prevents all type checking
errors within the namespace of that stub. So for `uwsgi.pyi`, in the `stubs`
directory, the following code would not raise pyre errors (though it would
obviously fail to run):

```python
import uwsgi
from uwsgi import asdf, ZXCV
uwsgi.qwer()
variable = ZXCV()
variable.hjkl()
```

If you want to be able to create `.pysa` stubs (i.e. annotate sources, sinks,
etc.) for something that is outside your codebase, such as Django's
`django.http.request.HttpRequest` object, you need more than just an empty stubs
file. You need a directory structure and `.pyi` file that matches your import,
such as `stubs/django/http/request.pyi`. Within that `.pyi` file, you
then need a stub of the class:

```python
class HttpRequest(BinaryIO):
    def __init__(self) -> None: ...
    COOKIES: Any = ...
    GET: QueryDict = ...
    # And a bunch more stuff...
```

Only at this point can you add `.pysa` files with annotations such as these:

```
django.http.request.HttpRequest.COOKIES: TaintSource[UserControlled] = ...
django.http.request.HttpRequest.GET: TaintSource[UserControlled] = ...
```

**There is a huge gotcha here**: If you had both an empty `stubs/django.pyi`
file, and the `stubs/django/http/request.pyi` file shown above, pyre will see
the `django.pyi` file first and ignore the `request.pyi` file. This would mean
that your stub of `HttpRequest` would be missed, and your `HttpRequest.COOKIES`
and `HttpRequest.GET` annotations would cause errors when running Pyre. The fix
is simply to delete the `django.pyi` file. When deleting that file, you may all
of a sudden see new typing errors for other types within Django, for which
you'll need to add new .`pyi` files at the appropriate locations.

### Missing types cause missed flows

Due to optimizations to allow parallelization, Pyre can be blind in some
scenarios that might be obvious to a human. Pyre needs to know the type of an
object that is a source/sink *at the point at which it is accessed*, in order
for it to detect tainted flows. For example, if you have a function that returns
a wrapper around a source, flows from that source will not be found unless the
return type of the function is specified. See below how one of the flows in the
`run` function is missed, simply because the return type on
`get_wrapper_untyped` is missing:

```python
from django.http import HttpRequest
class RequestWrapper:
    request: HttpRequest
    def __init__(self, request: HttpRequest):
        self.request = request

    @property
    def get_request_data(self):
        return self.request.GET["data"]

def get_wrapper_untyped(request: HttpRequest):
    return RequestWrapper(request)

def get_wrapper_typed(request: HttpRequest) -> RequestWrapper:
    return RequestWrapper(request)

def run(request: HttpRequest):
    # This flow WILL NOT be found
    wrapper = get_wrapper_untyped(request)
    eval(wrapper.get_request_data)
    # This flow WILL be found
    wrapper = get_wrapper_typed(request)
    eval(wrapper.get_request_data)
```

This illustrates how important typing is for ensuring all flows are caught by
during static analysis.

## Helpful Python knowledge

Pretty much all python operators are reduced down to double underbar functions.
For example, constructing an object results in a call to `__init__(self, ...)`
and an asterisk operator results in a call to `__mul__(a, b)`. A full list of
these operators can be found
[here](https://docs.python.org/3.7/library/operator.html). This is useful to
know when you need to add annotations to the usage of operators, such as the use
of square brackets to access a dictionary.

## Preprocessor

Some annotations are tractable to add and keep up to date, such as tracking
where user input can come out of the `HttpRequest` object. Others are more
difficult, such as annotating every endpoint in a large Django API that receives
user input which has been [extracted from a URL
parameter](https://docs.djangoproject.com/en/2.2/topics/http/urls/#example).
These functions are constantly being added, removed, and changed. It would be
intractable to keep creating `.pysa` stubs for them.

For use cases where we need to generate a large number of stubs in a dynamic
fashion, we have preprocessors that dynamically generate `.pysa` stubs. The code
lives in
[`tools/generate_taint_models`](https://github.com/facebook/pyre-check/tree/master/tools/generate_taint_models),
and can easily be extended to support additional preprocessing use cases.

## Debugging Tools

### `pyre_dump()`

You can insert a call to the (non-existent) `pyre_dump()` function in your code
to trigger to pyre to output a ton of metadata about it's current state when it
parses the that function call. This can be useful as a starting point to figure
out why something is/isn't happening. This will produce *very* verbose output.

### `reveal_type(info)`

If you only want to check what pyre knows about the types of variables, you can
call `reveal_type(YOUR_VARIABLE)` on the variable and then run pyre on the code
to get much less verbose output than `pyre_dump()`.

### Detecting Taint

There is no direct way to know if Pyre has detected a variable as tainted or
not. One simple way to do this is inject a call to a known sink for your source,
such as `eval`. You would pass the object that you want to know about into the
sink, run Pyre Static Analyzer, and see if the flow is detected.

Another strategy for getting a bit more metadata is adding a function into your
code, which simply constructs and returns the type you want to examine. You can
then run Pyre Static Analyzer, and grep for the function's name in the
`results.json` file located wherever you pointed `--save-results-to=` to when
running Pyre. You should then be able to see if that function is detected as
returning taint, plus a bit more metadata about it.
