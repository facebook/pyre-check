---
id: pysa-model-dsl
title: Model Domain Specific Language (DSL)
sidebar_label: Model DSL
custom_edit_url: https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/tools/pyre/documentation/website/docs/pysa_model_dsl.md
---

We have started developing a model Domain Specific Language (DSL) that can be
used to solve many of the same problems as [model
generators](pysa_precollectors.md), while still keeping model information in
`.pysa` files. The DSL aims to provide a compact way to generate models for all
code that matches a given query. This allows users to avoid writing hundereds or
thousand of models.

## Basics

The most basic form of querying Pysa's DSL is by generating models based on function names. To
do so, add a `ModelQuery` to your `.pysa` file:

```python
ModelQuery(
  # Indicates that this query is looking for functions
  find = "functions",
  # Indicates those functions should be called 'foo'
  where = [name.matches("foo")],
  # Indicates that matched function should be modeled as returning 'Test' taint
  model = [
    Returns(TaintSource[Test]),
  ]
)
```

Things to note in this example:

1. The `find` clause lets you pick what kinds of callables  you're looking to model.
1. The `where` clause is how you filter down the callables you're modeling - in this example, we're filtering functionos by names.
1. The `model` clause is a list of models to attach to the functions. Here, the syntax means that we model `foo` as returning `TaintSource[Test]`.

When invoking Pysa, if you add the `--dump-model-query-results` flag to your invocation, the generated models will be written to a file in JSON format.

```
$ pyre analyze --dump-model-query-results
...
> Emitting the model query results to `/my/home/dir/.pyre/model_query_results.pysa`
```

You can then view this file to see the generated models.

## Find clauses

The `find` clause currently supports `"functions"` and `"methods"`. `"functions"` indicates that you're querying for free functions, whereas `"methods"` indicates that you're only querying methods.

## Where clauses

Where clauses are a list of predicates, all of which must match for a function to be modelled.

### `name.matches`

The most basic query predicate is a name match - the name you're searching for is compiled as a regex, and the function names are compared against this.

Example:

```python
ModelQuery(
  find = ...,
  where = [
    name.matches("foo.*")
  ],
  model = ...
)
```

### `return_annotation` clauses

Model queries allow for querying based on the return annotation of a function. Pysa currently only allows querying whether a function type is `typing.Annotated`.

Example:
```python
ModelQuery(
  find = ...,
  where = [
    return_annotation.is_annotated_type(),
  ],
  model = ...
)
```

### `any_parameter` clauses

Model queries allow matching callables where any parameter matches a given clause. For now, the only clauses we support for parameters is type- based ones.

Example:
```python
ModelQuery(
  find = "functions",
  where = [
    any_parameter.annotation.is_annotated_type()
  ],
  model = ...
)
```

This model query will taint all functions which have one parameter with type `typing.Annotated`.

### `AnyOf` clauses

There are cases when we want to model functions which match any of a set of clauses. The `AnyOf` clause represents exactly this case.

Example:

```python
ModelQuery(
  find = "methods",
  where = [
    AnyOf(
      any_parameter.annotation.is_annotated_type(),
      return_annotation.is_annotated_type(),
    )
  ],
  model = ...
)
```

## Generated models

The last bit of model queries is actually generating models for all callables that match the provided where clauses. We support generating models for parameters by name or position, as well as generating models for all paramaters. Additionally, we support generating models for the return annotation.


### Returned taint

Returned taint takes the form of `Returns(TaintSpecification)`, where `TaintSpecification` is either a taint annotation or a list of taint annotations.

```python
ModelQuery(
  find = "methods",
  where = ...,
  model = [
    Returns(TaintSource[Test, Via[foo]])
  ]
)
```

### Parameter taint

Parameter taint can be specified by name or by position.

Named parameter taint takes the form of `NamedParameter(name=..., taint = TaintSpecification)`, and positional parameter taint takes the form of `PositionalParameter(index=..., taint = TaintSpecification)`:

```python
ModelQuery(
  find = "methods",
  where = ...,
  model = [
    NamedParameter(name="x", taint = TaintSource[Test, Via[foo]]),
    PositionalParameter(index=0, taint = TaintSink[Test, Via[bar]]),
  ]
)
```

### Tainting all parameters

One final convenience we provide is the ability to taint all parameters of a callable. The syntax is `AlllParameters(TaintSpecification)`.

```python
ModelQuery(
  find = "functions",
  where = ...,
  model = [
    AllParameters(TaintSource[Test])
  ]
)
```
