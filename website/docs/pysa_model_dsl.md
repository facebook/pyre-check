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

Currently, the DSL only supports generating models based on function names. To
do so, add a `ModelQuery` to your `.pysa` file:

```python
ModelQuery(
  # Indicates that this query is looking for functions
  find = "functions",
  # Indicates those fuctions should be called 'foo'
  where = [name.matches("foo")],
  # Indicates that matched function should be modeled as returning 'Test' taint
  model = [
    Returns(TaintSource[Test]),
  ]
)
```
