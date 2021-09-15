---
id: features
title: Features
sidebar_label: Features
---

Pyre has custom support for Python idioms that would otherwise not be supported by the usual type annotations.

## Registering attributes using PyTorch's register_buffer

PyTorch allows subclasses of [`nn.Module`](https://pytorch.org/docs/stable/generated/torch.nn.Module.html?highlight=nn%20module#torch.nn.Module) to [register](https://pytorch.org/docs/stable/generated/torch.nn.Module.html?highlight=register_buffer#torch.nn.Module.register_buffer) a buffer in an object using `self.register_buffer("foo", initial_value)`. Pyre supports this pattern when used within the constructor. It simply treats the buffer as a Tensor attribute of the class:

```python
import torch
import torch.nn as nn

class Foo(nn.Module):
    def __init__(self) -> None:
        super(Foo, self).__init__()
        self.register_buffer("foo", torch.zeros(10, 20))
        self.register_buffer("foo_persistent", torch.zeros(10, 20), persistent=False)

    def bar(self) -> None:
        reveal_type(self.foo) # => torch.Tensor
        reveal_type(self.foo_persistent) # => torch.Tensor

def baz() -> None:
    y = Foo().foo
    reveal_type(y) # => torch.Tensor
```

Note that Pyre will not recognize buffers registered in methods other than the constructor (just like it doesn't recognize [attributes](errors.md#16-missing-attributes) defined in methods other than the constructor).

It will also not recognize buffers that are initialized with `None` since it cannot infer the exact type of the buffer. In such a case, you can tell Pyre about the attribute's type by explicitly defining it in the class:

```python
import torch
import torch.nn as nn
from typing import Optional

class Foo(nn.Module):
    my_none_buffer: Optional[torch.Tensor]

    def __init__(self) -> None:
        super(Foo, self).__init__()
        self.register_buffer("my_none_buffer", None)

    def bar(self) -> None:
        reveal_type(self.my_none_buffer) # => Optional[torch.Tensor]
```
