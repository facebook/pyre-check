---
id: pysa-model-dsl
title: Model Domain Specific Language (DSL)
sidebar_label: Model DSL
---

We have started developing a model Domain Specific Language (DSL) that can be
used to solve many of the same problems as [model
generators](pysa_precollectors.md), while still keeping model information in
`.pysa` files. The DSL aims to provide a compact way to generate models for all
code that matches a given query. This allows users to avoid writing hundreds or
thousands of models.

## Basics

The most basic form of querying Pysa's DSL is by generating models based on function names. To
do so, add a `ModelQuery` to your `.pysa` file:

```python
ModelQuery(
  # Indicates the name of the query
  name = "get_foo_sources",
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

1. The `name` clause is the name of your query.
1. The `find` clause lets you pick whether you want to model functions, methods or attributes.
1. The `where` clause is how you refine your criteria for when a model should be generated - in this example, we're filtering for functions where the name matches `"foo"`.
1. The `model` clause is a list of models to generate. Here, the syntax means that the functions matching the where clause should be modelled as returning `TaintSource[Test]`.

When invoking Pysa, if you add the `--dump-model-query-results /path/to/output/file` flag to your invocation, the generated models will be written to a file in JSON format.

```
$ pyre analyze --dump-model-query-results /path/to/output/file.txt
...
> Emitting the model query results to `/my/home/dir/.pyre/model_query_results.pysa`
```

You can then view this file to see the generated models.

## Name clauses
The `name` clause describes what the query is meant to find.  Normally it follows the format of `get_` + [what the query matches with in the `where` clause] + [`_sinks`, `_source` and/or `_tito`]. This clause should be unique for every ModelQuery within a file.

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

`where` clauses are a list of predicates, all of which must match for an entity to be modelled. Note that certain predicates are only compatible with specific find clause kinds.

### `name.matches`

The most basic query predicate is a name match - the name you're searching for is compiled as a regex, and the entity's fully qualified name is compared against it. A fully qualified name includes the module and class - for example, for a method `foo` in class `C` which is part of module `bar`, the fully qualified name is `bar.C.foo`.

Example:

```python
ModelQuery(
  name = "get_foo",
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
  name = "get_bar_C_foo",
  find = ...,
  where = [
    name.equals("bar.C.foo")
  ],
  model = ...
)
```

### `return_annotation` clauses

Model queries allow for querying based on the return annotation of a callable. Note that this `where` clause does not work when the `find` clause specifies `"attributes"`.

#### `return_annotation.equals`

The clause will match when the fully-qualified name of the callable's return type matches the specified value exactly.

```python
ModelQuery(
  name = "get_return_HttpRequest_sources",
  find = "functions",
  where = [
    return_annotation.equals("django.http.HttpRequest"),
  ],
  model = Returns(TaintSource[UserControlled, Via[http_request]])
)
```

#### `return_annotation.matches`

This is similar to the previous clause, but will match when the fully-qualified name of the callable's return type matches the specified pattern.

```python
ModelQuery(
  name = "get_return_Request_sources",
  find = "methods",
  where = [
    return_annotation.matches(".*Request"),
  ],
  model = Returns(TaintSource[UserControlled, Via[http_request]])
)
```

#### `return_annotation.is_annotated_type`

This will match when a callable's return type is annotated with [`typing.Annotated`](https://docs.python.org/3/library/typing.html#typing.Annotated). This is a type used to decorate existing types with context-specific metadata, e.g.
```python
from typing import Annotated

def bad() -> Annotated[str, "SQL"]:
  ...
```

Example:

```python
ModelQuery(
  name = "get_return_annotated_sources",
  find = functions,
  where = [
    return_annotation.is_annotated_type(),
  ],
  model = Returns(TaintSource[SQL])
)
```

This query would match on functions like the one shown above.

### `any_parameter` clauses

Model queries allow matching callables where any parameter matches a given clause. For now, the only clauses we support for parameters is specifying conditions on the type annotation of a callable's parameters. These can be used in conjunction with the `Parameters` model clause (see [`type_annotation`](#type_annotation-clause)) to taint specific parameters. Note that this `where` clause does not work when the `find` clause specifies `"attributes"`.

#### `any_parameter.annotation.equals`

This clause will match all callables which have at least one parameter where the fully-qualified name of the parameter type matches the specified value exactly.

Example:
```python
ModelQuery(
  name = "get_parameter_HttpRequest_sources",
  find = "functions",
  where = [
    any_parameter.annotation.equals("django.http.HttpRequest")
  ],
  model =
    Parameters(
      TaintSource[UserControlled],
      where=[
        name.equals("request"),
        name.matches("data$")
      ]
    )
)
```

#### `any_parameter.annotation.matches`

This clause will match all callables which have at least one parameter where the fully-qualified name of the parameter type matches the specified pattern.

Example:
```python
ModelQuery(
  name = "get_parameter_Request_sources",
  find = "methods",
  where = [
    any_parameter.annotation.matches(".*Request")
  ],
  model =
    Parameters(
      TaintSource[UserControlled],
      where=[
        type_annotation.matches(".*Request"),
      ]
    )
)
```

#### `any_parameter.annotation.is_annotated_type`

This clause will match all callables which have at least one parameter with type `typing.Annotated`.

Example:
```python
ModelQuery(
  name = "get_parameter_annotated_sources",
  find = "functions",
  where = [
    any_parameter.annotation.is_annotated_type()
  ],
  model =
    Parameters(
      TaintSource[Test],
      where=[
        type_annotation.is_annotated_type(),
      ]
    )
)
```

### `AnyOf` clauses

There are cases when we want to model entities which match any of a set of clauses. The `AnyOf` clause represents exactly this case.

Example:

```python
ModelQuery(
  name = "get_AnyOf_example",
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

### `AllOf` clauses

There are cases when we want to model entities which match all of a set of clauses. The `AllOf` clause may be used in this case.

Example:

```python
ModelQuery(
  name = "get_AllOf_example",
  find = "methods",
  where = [
    AnyOf(
      AllOf(
        parent.extends("a.b"),
        parent.matches("Foo"),
      ),
      AllOf(
        parent.extends("c.d"),
        parent.matches("Bar")
      )
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
  name = "get_app_route_decorator",
  find = "functions",
  where = Decorator(name.matches("app.route")),
  ...
)
```
or
```python
ModelQuery(
  name = "get_my_module_app_route_decorator",
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
  name = "get_d1_decorator",
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
  name = "get_d1_decorator",
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
  name = "get_d1_decorator",
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
  name = "get_childOf_foo_Bar",
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
  name = "get_childOf_Foo",
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
  name = "get_subclassOf_C",
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
  name = "get_transitive_subclassOf_C",
  find = "attributes",
  where = parent.extends("C", is_transitive=True),
  ...
)
```

This query will model `C.x`, `D.y` and `E.z`.

### `parent.decorator` clause

The `parent.decorator` clause is used to specify constraints on a class decorator, so you can choose to model entities on classes only if the class it is part of has the specified decorator.

The arguments for this clause are identical to the non-class constraint `Decorator`, for more information, please see the [`Decorator` clauses](#decorator-clauses) section.

Example:

```python
ModelQuery(
  name = "get_childOf_d1_decorator_sources",
  find = "methods",
  where = [
    parent.decorator(
      name.matches("d1"),
      arguments.contains(2)
    ),
    name.matches("\.__init__$)
  ],
  model = [
    Parameters(TaintSource[Test], where=[
        Not(name.equals("self")),
        Not(name.equals("a"))
    ])
  ]
)
```

For example, the above query when run on the following code:
```python
@d1(2)
class Foo:
  def __init__(self, a, b):
     ...

@d1()
class Bar:
  def __init(self, a, b):
    ...

@d2(2)
class Baz:
  def __init(self, a, b):
    ...
```
will result in a model for `def Foo.__init__(b: TaintSource[Test])`.

### `Not` clauses

The `Not` clause negates any existing clause that is valid for the entity being modelled.

Example:

```python
ModelQuery(
  name = "get_Not_example",
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
  name = "get_Returns_sources",
  find = "methods",
  where = ...,
  model = [
    Returns(TaintSource[Test, Via[foo]])
  ]
)
```

### Parameter taint

Parameters can be tainted using the `Parameters()` clause. By default, all parameters will be tained with the supplied taint specification. If you would like to only taint specific parameters matching certain conditions, an optional `where` clause can be specified to accomplish this, allowing for constraints on parameter names, the annotation type of the parameter, or parameter position. For example:

```python
ModelQuery(
  name = "get_Parameters_sources",
  find = "methods",
  where = ...,
  model = [
    Parameters(TaintSource[A]), # will taint all parameters by default
    Parameters(
      TaintSource[B],
      where=[
        Not(index.equals(0))   # will only taint parameters that are not the first parameter
      ]
    ),
  ]
)
```

#### `name` clauses

To specify a constraint on parameter name, the `name.equals()` or `name.matches()` clauses can be used. As in the main `where` clause of the model query, `equals()` searches for an exact match on the specified string, while `matches()` allows a regex to be supplied as a pattern to match against.

Example:

```python
ModelQuery(
  name = "get_request_data_sources",
  find = "methods",
  where = ...,
  model = [
    Parameters(
      TaintSource[Test],
      where=[
        name.equals("request"),
        name.matches("data$")
      ]
    )
  ]
)
```

#### `index` clause

To specify a constraint on parameter position, the `index.equals()` clause can be used. It takes a single integer denoting the position of the parameter.

Example:

```python
ModelQuery(
  name = "get_index_sources",
  find = "methods",
  where = ...,
  model = [
    Parameters(
      TaintSource[Test],
      where=[
        index.equals(1)
      ]
    )
  ]
)
```

#### `type_annotation` clause

This clause is used to specify a constraint on parameter type annotation. Currently the clauses supported are: `type_annotation.equals()`, which takes the fully-qualified name of a Python type or class and matches when there is an exact match, `type_annotation.matches()`, which takes a regex pattern to match type annotations against, and `type_annotation.is_annotated_type()`, which will match parameters of type [`typing.Annotated`](https://docs.python.org/3/library/typing.html#typing.Annotated).

Example:

```python
ModelQuery(
  name = "get_annotated_parameters_sources",
  find = "methods",
  where = ...,
  model = [
    Parameters(
      TaintSource[Test],
      where=[
        type_annotation.equals("foo.bar.C"),  # exact match
        type_annotation.matches("^List\["),   # regex match
        type_annotation.is_annotated_type(),  # matches Annotated[T, x]
      ]
    )
  ]
)
```

To match on the annotation portion of `Annotated` types, consider the following example. Suppose this code was in `test.py`:
```python
from enum import Enum
from typing import Annotated, Option

class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3

class Foo:
  x: Annotated[Optional[int], Color.RED]
  y: Annotated[Optional[int], Color.BLUE]
  z: Annotated[int, "z"]
```

Note that the type name that should be matched against is its fully qualified name, which also includes the fully qualified name of any other types referenced (for example, `typing.Optional` rather than just `Optional`). When multiple arguments are provided to the type they are implicitly treated as being in a tuple.

Here are some examples of `where` clauses that can be used to specify models for the annotated attributes in this case:
```python
ModelQuery(
  name = "get_annotated_attributes_sources",
  find = "attributes",
  where = [
    AnyOf(
      type_annotation.equals("typing.Annotated[(typing.Optional[int], test.Color.RED)]"),
      type_annotation.equals("typing.Annotated[(int, z)]"),
      type_annotation.matches(".*Annotated\[.*Optional[int].*Color\..*\]")
      type_annotation.is_annotated_type()
    )
  ],
  model = [
    AttributeModel(TaintSource[Test]),
  ]
)
```

This query should generate the following models:
```
test.Foo.x: TaintSource[Test]
test.Foo.y: TaintSource[Test]
test.Foo.z: TaintSource[Test]
```

#### `Not`, `AllOf` and `AnyOf` clauses

The `Not`, `AllOf` and `AnyOf` clauses can be used in the same way as they are in the main `where` clause of the model query. `Not` can be used to negate any existing clause, `AllOf` to match when all of several supplied clauses match, and `AnyOf` can be used to match when any one of several supplied clauses match.

Example:

```python
ModelQuery(
  name = "get_Not_AnyOf_AllOf_example_sources",
  find = "methods",
  where = ...,
  model = [
    Parameters(
      TaintSource[Test],
      where=[
        Not(
          AnyOf(
            AllOf(
              parent.extends("a.b"),
              parent.matches("Foo"),
            ),
            AllOf(
              parent.extends("c.d"),
              parent.matches("Bar")
            )
          )
        )
      ]
    )
  ]
)
```

#### Using `ViaTypeOf` with the `Parameters` clause

Usually when specifying a `ViaTypeOf` the argument that you want to capture the value or type of should be specified. However, when writing model queries and trying to find all parameters that match certain conditions, we may not know the exact name of the parameters that will be modelled. For example:
```python
def f1(bad_1, good_1, good_2):
  pass

def f2(good_3, bad_2, good_4):
  pass
```

Suppose we wanted to model all parameters with the prefix `bad_` here and attach a `ViaTypeOf` to them. In this case it is still possible to attach these features to the parameter model, by using a standalone `ViaTypeOf` as follows:
```python
ModelQuery(
  name = "get_f_sinks",
  find = "functions",
  where = name.matches("f"),
  model = [
    Parameters(
      TaintSink[Test, ViaTypeOf],
      where=[
        name.matches("bad_")
      ]
    )
  ]
)
```

This would produce models equivalent to the following:
```python
def f1(bad_1: TaintSink[Test, ViaTypeOf[bad_1]]): ...
def f2(bad_2: TaintSink[Test, ViaTypeOf[bad_2]]): ...
```


### Models for attributes

Taint for attribute models requires a `AttributeModel` model clause, which can only be used when the find clause specifies attributes.

Example:

```python
ModelQuery(
  name = "get_attribute_sources_sinks",
  find = "attributes",
  where = ...,
  model = [
    AttributeModel(TaintSource[Test], TaintSink[Test])
  ]
)
```
