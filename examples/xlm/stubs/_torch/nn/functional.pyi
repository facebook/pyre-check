from _torch import Tensor
from pyre_extensions import ListVariadic

Shape = ListVariadic("Shape")

def softmax(t: Tensor[Shape], dim: int) -> Tensor[Shape]: ...
def dropout(t: Tensor[Shape], p, training) -> Tensor[Shape]: ...
