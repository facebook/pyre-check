---
id: pysa-tips
title: General Tips
sidebar_label: General Tips
---

## Features/Limitations

### Inheritance

Pysa is aware of inheritance, so you can add taint annotations to a base class,
and Pysa will detect when the tainted attribute or function is accessed via a
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

Additionally, Pysa is aware that child classes can be used anywhere a parent
classes's type is present. If you access a method on a parent class and the
implementation on any child class returns taint, Pysa will detect that and
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

**A huge caveat here is that Pysa needs to be aware of these inheritance
relationships and function definitions for it to work.** Code that lives
outside the repo under analysis might not be visible to Pysa, so these
inheritances/implementations may be missed. See the Stubs section below for
more details.

### Stubs

The concept of stubs is covered in general _[here](pysa_basics.md)_, but this
section in particular will cover specific issues you may encounter with
`.pyi` stubs. These stubs can be used to prevent pyre errors for types
that live outside the codebase you are running Pysa on. The simplest stubs are
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

If you want to be able to create `.pysa` models (i.e. annotate sources, sinks,
etc.) for something that is outside your codebase, such as Django's
`django.http.request.HttpRequest` object, you need more than just an empty stub
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
django.http.request.HttpRequest.COOKIES: TaintSource[UserControlled]
django.http.request.HttpRequest.GET: TaintSource[UserControlled]
```

**There is a huge gotcha here**: If you had both an empty `stubs/django.pyi`
file, and the `stubs/django/http/request.pyi` file shown above, pyre will see
the `django.pyi` file first and ignore the `request.pyi` file (following
[PEP 484](https://www.python.org/dev/peps/pep-0484/#stub-files)). This would
mean that your stub of `HttpRequest` would be missed, and your
`HttpRequest.COOKIES` and `HttpRequest.GET` annotations would cause errors when
running Pysa. The fix is simply to delete the `django.pyi` file. When deleting
that file, you may all of a sudden see new typing errors for other types within
Django, for which you'll need to add new .`pyi` files at the appropriate
locations.

Since definitions in type stubs don't have bodies, all functions and methods
will be treated as [obscure models](pysa_advanced.md#obscure-models). If this
leads to false positives, you will want to write a model for it.

## Helpful Python knowledge

Pretty much all python operators are reduced down to double underbar functions.
For example, constructing an object results in a call to `__init__(self, ...)`
and an asterisk operator results in a call to `__mul__(a, b)`. A full list of
these operators can be found
[here](https://docs.python.org/3.7/library/operator.html). This is useful to
know when you need to add annotations to the usage of operators, such as the use
of square brackets to access a dictionary.

## Debugging Tools

### `pyre_dump()`

You can insert a call to the (non-existent) `pyre_dump()` function in your code
to enable verbose logging of the forward and backward analysis of the current
function or method. This can be useful as a starting point to figure out why
something is/isn't happening. This will produce *very* verbose output.

### `reveal_type(YOUR_VARIABLE)`

If you only want to check what pyre knows about the types of variables, inject a
call to `reveal_type(YOUR_VARIABLE)` (no import needed) in your code. Running
Pyre on your code will then give you compact output indicating what Pyre thinks
the type of your variable is.

### `reveal_taint(YOUR_VARIABLE)`

Similarly to `reveal_type`, if you only want to check what pyre knows about the
taint on variables, inject a call to `reveal_taint(YOUR_VARIABLE)` (no import
needed) in your code. Running Pysa on your code will then give you compact
output indicating what taint Pysa has discovered. Note that each time Pysa
analyzes the function (which could be many times) it will update it's
understanding of the taint flowing into the function and output the current
state. The final output will be the most complete.

### `pyre_dump_perf()`

You can insert a call to `pyre_dump_perf` (no import needed) in a function or
method to profile the current analysis on that function or method, and dump
the results on stdout.

### `results.json`

Another strategy for getting a bit more metadata is adding a function into your
code, which simply constructs and returns the type you want to examine. You can
then run Pysa, and grep for the function's name in the
`results.json` file located wherever you pointed `--save-results-to=` to when
running Pysa. You should then be able to see if that function is detected as
returning taint, plus a bit more metadata about it.

### `sapp`

The [Static Analysis Post Processor (SAPP)](static_analysis_post_processor.md)
has access to the same information as `results.json`. While SAPP doesn't display
all the information `results.json` contains, it can display the information in a
more user-friendly gdb-style way. It's especially useful for exploring flows
which pass through many frames.


## Developer Quality-of-Life

### Iterating quickly with Pysa

On large projects, Pysa can take a long time to run; it takes about an hour to
run on Instagram, which contains millions of lines of Python code. A few tricks
to iterate more quickly with Pysa are:

1. **Run in a sample project or test environment.** Pysa runs much more quickly
   on smaller projects, so if you need to test something that isn't specific to
   your environment (eg. a model that corresponds to code in typeshed) then do
   your testing in a smaller codebase. Even if you are iterating on something
   specific to your codebase, it can sometimes be worthwhile to port the code
   snippet you're working on into a test project.
   1. The stub integration tests will validate any stubs in `tools/pyre/taint`,
      and this can be a fast shortcut for validating new stubs you want to
      write. These tests reside in `stubs/integration_test` and can be invoked
      by running `make stubs_integration_test` in the root of the repo.
   1. The interprocedural analysis tests dump information about models, issues,
      the call graph, and overrides. It can be very helpful to test code in this
      environment if you need a detailed understanding of Pysa's internal state
      to debug a false positive or negative. Note that these tests do not have
      access to typeshed or any other type stubs. These tests reside in
      `interprocedural_analyses/taint/test/integration` and can be invoked by
      running `make test` in the root of the repo.
1. **Skip analysis entirely if you only need to validate taint models**. `pyre
   validate-models` can be used to validate taint models without having to run
   the entire analysis.
1. **Filter runs with `--rule ####`.** This option will cause Pysa to ignore
   sources and sinks that are not involved in the given rule, saving on analysis
   time. Eg. `pyre analyze --rule 5000`
1. **Parallelize across machines.** If working in a could hosted environment,
   reserving a second machine and working on two projects in parallel can be
   effective. As Pysa is running on one machine, you can switch to the other,
   make changes there, kick off a run, and then switch back to the first to look
   at results.
1. **Put in all debug statements up front.** When using the debugging tools
   outlined above, put in way more debug statments than you think you need,
   dumping type info and taint for anything remotely related to the flow you're
   looking at. This will reduce the odds that you need to do a second run to
   figure out what's going wrong.
1. **Enable the `--use-cache` flag.** All Pysa runs require some information
   from Pyre, such as the typechecking environment, dependencies, etc.
   Computing this information can be time-consuming on larger projects.
   However, if you're only editing taint models and not the project source,
   this information isn't expected to change between Pysa runs. By enabling
   this flag, you can tell Pysa to save this information to cache files
   (located in .pyre/.pysa_cache) and load from cache in subsequent runs,
   rather than computing it from scratch each time. The cache will be
   invalidated if any of the project source files change, in which case
   Pysa will fall back to doing a clean run and then saving the computed
   artifacts in new cache files.


### File Types

`taint.config` is a JSON file and `.pysa` files use Python syntax. If you update
your editor to recognize those files as JSON and Python respectively, it'll make
development easier.

## Usage Examples

Not all Pysa features will be covered in these docs, and provided examples won't
always be complete. Every feature, however, _will_ be covered in the tests
located
[here](https://github.com/facebook/pyre-check/tree/master/interprocedural_analyses/taint/test/integration).
These tests can be a useful resource to discover how to use Pysa features.
