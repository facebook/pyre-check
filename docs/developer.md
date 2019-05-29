---
id: developer
title: Developing on Pyre
sidebar_label: Developing on Pyre
---


# Writing Your Own Static Analyses
The architecture of Pyre makes it a suitable base for developers interested in writing their own static analysis tools for Python. In the next two subsections, we will talk about how to extend Pyre with two kinds of static analysis tools:
1. those orthogonal to the typechecker
2. those which strengthen our existing typechecker

We will present an example of each to describe the process of developing such tools.


## Extensions to The Typechecker: Numpy ndarrays
Consider a static analysis which checks the validity of tensor operations using [numpy n-dimensional arrays](https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.html). A simple example is given below.
```python
from numpy import array

a = array([1, 2, 3])
b = array([1, 2])

c = a + a   # valid
d = a + 1   # valid due to broadcasting
e = a + b   # invalid
```

### Getting the Type Information
### Inferring List Length
### Checking Validity of Tensor Operations
### Tying Our Analysis to a Pyre Command


## Orthogonal to The Typechecker: Taint Analysis
**TODO**: Fill in documentation on how the taint analysis tool was built, with emphasis on how this approach can be generalized.