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

### `name.equals`

This clause will match when the entity's fully qualified name is exactly the same as the specified string.

Example:

```python
ModelQuery(
  find = ...,
  where = [
    name.equals("bar.C.foo")
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

### `Decorator` clauses

`Decorator` clauses are used to find callables decorated with decorators that match a pattern. The syntax for using this clause is `Decorator(<name clause>, [<arguments clause>])`.

The first argument to `Decorator` should be a name clause, which is used to match the name of a decorator. The supported name clauses are the same as the ones discussed above for model query constraints, i.e. `name.matches("pattern")`, which will match when the decorator matches the regex pattern specified as a string, and `name.equals("foo.bar.d1")` which will match when the fully-qualified name of the decorator equals the specified string exactly.

For example, if you wanted to find all functions which are decorated by `@app.route()`, a decorator imported from `my_module`, you can write:

```python
ModelQuery(
  find = "functions",
  where = Decorator(name.matches("app.route")),
  ...
)
```
or
```python
ModelQuery(
  find = "functions",
  where = Decorator(name.equals("my_module.app.route")),
  ...
)
```


The second argument to `Decorator` is an optional arguments clause, which is used to match on the arguments provided to the decorator. The supported arguments clauses are `arguments.contains(...)`, which will match when the arguments specified are a subset of the decorator's arguments, and `arguments.equals(...)`, which will match when the decorator has the specified arguments exactly.

`arguments.contains()` supports both positional and keyword arguments. For positional arguments, the list of positonal arguments supplied to the `arguments.contains()` clause must be a prefix of the list of positional arguments on the actual decorator, i.e. the value of the argument at each position should be the same. For example, with the following Python code:
```python
@d1(a, 2)
def match1():
  ...

@d1(a, 2, 3, 4)
def match2():
  ...

@d1(2, a):
def nomatch():
  ...
```

This query will match both `match1()` and `match2()`, but not `nomatch()`, since the values of the positional arguments don't match up.
```python
ModelQuery(
  find = "functions",
  where = Decorator(
    name.matches("d1"),
    arguments.contains(a, 2)
  ),
  ...
)
```

For keyword arguments in `arguments.contains()`, the specified keyword arguments must be a subset of the decorator's keyword arguments, but can be specified in any order. For example, with the following Python code:
```python
@d1(a, 2, foo="Bar")
def match1():
  ...

@d1(baz="Boo", foo="Bar")
def match2():
  ...
```

This query will match both `match1()` and `match2()`:
```python
ModelQuery(
  find = "functions",
  where = Decorator(
    name.matches("d1"),
    arguments.contains(foo="Bar")
  ),
  ...
)
```

`arguments.equals()` operates similarly, but will only match if the specified arguments match the decorator's arguments exactly. This means that for positional arguments, all arguments in each position must match by value exactly. Keyword arguments can be specified in a different order, but the set of specified keyword arguments and the set of the decorator's actual keyword arguments must be the same. For example, with the following Python code:
```python
@d1(a, 2, foo="Bar", baz="Boo")
def match1():
  ...

@d1(a, 2, baz="Boo", foo="Bar")
def match2():
  ...

@d1(2, a, baz="Boo", foo="Bar")
def nomatch1():
  ...

@d1(a, 2, 3, baz="Boo", foo="Bar")
def nomatch2():
  ...
```

This query will match both `match1()` and `match2()`, but not `nomatch1()` or `nomatch2()`:
```python
ModelQuery(
  find = "functions",
  where = Decorator(
    name.matches("d1"),
    arguments.equals(a, 2, foo="bar", baz="Boo")
  ),
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

The default behavior is that it will only match if the parent class is an instance of, or a direct subclass of the specified class. For example, with classes:
```python
class C:
  x = ...

class D(C):
  y = ...

class E(D):
  z = ...
```

the above query will only model the attributes `C.z` and `D.y`, since `C` is considered to extend itself, and `D` is a direct subclass of `C`. However, it will not model `E.z`, since `E` is a sub-subclass of `C`.

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
