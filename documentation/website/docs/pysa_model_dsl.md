---
id: pysa-model-dsl
title: Model Domain Specific Language (DSL)
sidebar_label: Model DSL
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

1. The `find` clause lets you pick whether you want to model functions, methods or attributes.
1. The `where` clause is how you refine your criteria for when a model should be generated - in this example, we're filtering for functions where the name matches `"foo"`.
1. The `model` clause is a list of models to generate. Here, the syntax means that the functions matching the where clause should be modelled as returning `TaintSource[Test]`.

When invoking Pysa, if you add the `--dump-model-query-results` flag to your invocation, the generated models will be written to a file in JSON format.

```
$ pyre analyze --dump-model-query-results
...
> Emitting the model query results to `/my/home/dir/.pyre/model_query_results.pysa`
```

You can then view this file to see the generated models.

## Find clauses

The `find` clause specifies what entities to model, and currently supports `"functions"`, `"methods"` and `"attributes"`. `"functions"` indicates that you're querying for free functions, `"methods"` indicates that you're only querying class methods, and `"attributes"` indicates that you're querying for attributes on classes.

Note that `"attributes"` also includes constructor-initialized attributes, such as `C.y` in the following case:
```python
class C:
  x = ...

  def __init__(self):
    self.y = ...
```

## Where clauses

Where clauses are a list of predicates, all of which must match for an entity to be modelled. Note that certain predicates are only compatible with specific find clause kinds.

### `name.matches`

The most basic query predicate is a name match - the name you're searching for is compiled as a regex, and the entity's fully qualified name is compared against it. A fully qualified name includes the module and class - for example, for a method `foo` in class `C` which is part of module `bar`, the fully qualified name is `bar.C.foo`.

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

There are cases when we want to model entities which match any of a set of clauses. The `AnyOf` clause represents exactly this case.

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

### `any_decorator` clauses

`any_decorator` clauses are used to find callables decorated with decorators that match a pattern.

Pysa currently only supports matching on the name of any decorator. For example, if you wanted to find all functions which are decorated by `@app.route()`, you can write:


```python
ModelQuery(
  find = "functions",
  where = any_decorator.name.matches("app.route"),
  ...
)
```

### `parent.equals` clause

You may use the `parent` clause to specify predicates on the parent class. This predicate can only be used when the find clause specifies methods or attributes.

The `parent.equals` clause is used to model entities when the parent's fully qualified name is an exact match for the specified string.

Example:

```python
ModelQuery(
  find = "methods",
  where = parent.equals("foo.Bar"),
  ...
)
```

### `parent.matches` clause

The `parent.matches` clause is used to model entities when the parent's fully qualified name matches the provided regex.

Example:

```python
ModelQuery(
  find = "methods",
  where = parent.matches(".*Foo.*"),
  ...
)
```

### `parent.extends` clause

The `parent.extends` clause is used to model entities when the parent's class is a subclass of the provided class name.

Example:

```python
ModelQuery(
  find = "attributes",
  where = parent.extends("C"),
  ...
)
```

The default behavior is that it will only match if the parent class is a direct subclass of the specified class. For example, with classes:
```python
class C:
  x = ...

class D(C):
  y = ...

class E(D):
  z = ...
```

the above query will only model the attribute `D.y`, since `D` is a direct subclass of `C`, but not `C` itself, or `E`, which is a sub-subclass of `C`.

If you would like to model a class and all subclasses transitively, you can use the `is_transitive` flag to get this behavior.

Example:

```python
ModelQuery(
  find = "attributes",
  where = parent.extends("C", is_transitive=True),
  ...
)
```

This query will model `C.x`, `D.y` and `E.z`.

### `Not` clauses

The `Not` clause negates any existing clause that is valid for the entity being modelled.

Example:

```python
ModelQuery(
  find = "methods",
  where = [
    Not(
      name.matches("foo.*"),
      parent.matches("testing.unittest.UnitTest"),
    )
  ],
  model = ...
)
```


## Generated models (Model clauses)

The last bit of model queries is actually generating models for all entities that match the provided where clauses. For callables, we support generating models for parameters by name or position, as well as generating models for all paramaters. Additionally, we support generating models for the return annotation.


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

You can choose to exclude a single parameter or a list of parameters in order to avoid overtainting.

```python
ModelQuery(
  find = "functions",
  where = ...,
  model = [
    AllParameters(TaintSource[Test], exclude="self")
  ]
)

ModelQuery(
  find = "functions",
  where = ...,
  model = [
    AllParameters(TaintSource[Test], exclude=["self", "other"])
  ]
)
```

### Models for attributes

Taint for attribute models requires a `AttributeModel` model clause, which can only be used when the find clause specifies attributes.

Example:

```python
ModelQuery(
  find = "attributes",
  where = ...,
  model = [
    AttributeModel(TaintSource[Test], TaintSink[Test])
  ]
)
```
