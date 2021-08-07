---
id: pysa-model-generators
title: Dynamically Generating Models
sidebar_label: Dynamically Generating Models
---

import InternalModelGenerators from './fb/pysa_model_generators_internal.md';

Some sources and sinks may be too numerous or too rapidly changing for defining
them statically to be practical. For these scenarios, Pysa has the concept of
model generators, which can generate taint models by reading the project's source code before static analysis is
started. The current set of model generators is stored in
[`tools/generate_taint_models`](https://github.com/facebook/pyre-check/tree/main/tools/generate_taint_models)
within the pyre-check repository.

Pysa now has the concept of a [Model DSL](pysa_model_dsl.md), which supports
some model generation usecases which could previously only be done with model
generators. You should prefer the Model DSL if it supports your usecase.

## Running Model Generators

The majority of model generators require access to a running environment. For
example, the `RESTApiSourceGenerator` needs to be able to access `urlpatterns`
configured for Django, meaning it has to import (and implicitly run) the file
you use to configure routing. The recommended way to run model generators is to set
up a small script within your repository that can run within the virtual
environment for your project. **[This tutorial
exercise](https://github.com/facebook/pyre-check/tree/main/documentation/pysa_tutorial/exercise5)
provides an example of how to setup and use model generators.**

## Example Model Generators

The set of model generators is always changing, but below are some examples of
model generators which are currently provided out of the box with Pysa.

### [`RESTApiSourceGenerator`](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/get_REST_api_sources.py)

This model generator is intended to taint all arguments to [Django view
functions](https://docs.djangoproject.com/en/2.2/topics/http/views/) as
`UserControlled`. This is useful when you have views that receive
user-controlled data as arguments separate from the `HttpRequest` parameter,
such as when [capturing values from the request
path](https://docs.djangoproject.com/en/2.2/topics/http/urls/#example).

### [`ExitNodeGenerator`](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/get_exit_nodes.py)

This generator is intended to taint all data returned from [Django view
functions](https://docs.djangoproject.com/en/2.2/topics/http/views/) as
`ReturnedToUser`. This is useful when you have decorators which allow your view
functions to return raw python types, rather than `HttpResponse` objects. Note
that you do not need this generator if you always construct `HttpResponse`
objects, because they are already annotated as `ReturnedToUser` sinks.

### [`GraphQLSourceGenerator`](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/get_graphql_sources.py)

This model generator is similar to the `RESTApiSourceGenerator` and
`ExitNodeGenerator` discussed above, but it is intended to generate models with
`UserControlled` and `ReturnedToUser` annotations for graphene-style GraphQL
`resolver` functions.

### [`AnnotatedFreeFunctionWithDecoratorGenerator`](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/get_annotated_free_functions_with_decorator.py)

This model generator provides general purpose functionality to annotate all free
functions which have a given decorator. The annotations can be used to mark any
of the function's arguments or return types as sources, sinks, features, etc.
This is useful whenever you have a function which modifies taint analysis
expectations. For example, if you had a decorator which applies rate limiting to
functions, you could use this model generator to add a feature to all flow passing
through rate limited functions, to enable you to filter them out from a given
rule.

## Writing Model Generators

All model generator code lives in
[`tools/generate_taint_models`](https://github.com/facebook/pyre-check/tree/main/tools/generate_taint_models)
within the pyre-check repository.


### Adding a new model generator

[This commit](https://github.com/facebook/pyre-check/commit/ea900c5e77d4c6d951e9c42b7310613f7f6edf08#diff-9ef72470683730531933e74a50ea98a1)
provides an example of how to add a new model generator.

The basic workflow is:

1. Create a new file under `generate_taint_models` of the form `get_<pattern of model>`.
1. Write a class that inherits from [ModelGenerator](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/model_generator.py).
1. Collect all the callables you're interested in modeling via `gather_functions_to_model`.
1. Convert the callables you've collected into models. The [CallableModel](https://github.com/facebook/pyre-check/blob/main/tools/generate_taint_models/model.py) class is a convenience that pretty prints things in the right way - you just need to specify what kind of taint the parameters and return value should have, specify the callable to model, and call generate().
1. Write unit tests ([example](https://github.com/facebook/pyre-check/blob/922410239404aa436691754402b0c3db68c5a46f/tools/generate_taint_models/tests/get_annotated_free_functions_with_decorator_test.py)).
1. Import your new class in the `__init__` file ([example](https://github.com/facebook/pyre-check/blob/922410239404aa436691754402b0c3db68c5a46f/tools/generate_taint_models/__init__.py#L7)).

<InternalModelGenerators/>
