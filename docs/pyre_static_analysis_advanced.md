---
id: pyre-static-analysis-advanced
title: Advanced Topics
sidebar_label: Advanced Topics
---

## Overview

This page documents less straightforward bits of Pyre Static Analyzer.

## Annotating `dataclass` Models

In Pyre, [`dataclass`](https://docs.python.org/3/library/dataclasses.html?)es
are defined via attributes, which are converted to properties under the hood. If
you want to taint the attributes of a `dataclass`, you might try to do the
following:

```python
# tainted.py
@dataclass(frozen=True)
class MyDataClass:
    attribute: str = ""
```


```python
# stubs/taint/tainted.py.pysa
# This won't work
tainted.MyDataClass.attribute: TaintSource[SensitiveData]
```

This doesn't work, because during analysis Pyre's understanding of the data
class is of how the class looks after the property is expanded; that is:

```python
# Pyre's view of tainted.py
class MyDataClass:
  @property
  def attribute(self) -> str: ...
  @attribute.setter
  def attribute(self, value) -> None: ...
```

Therefore, to annotate a `dataclass` attribute, you can use the `@property`
annotations:

```python
# stubs/taint/tainted.py.pysa
@property
def tainted.MyDataClass.attribute(self) -> TaintSource[SensitiveData]: ...
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
