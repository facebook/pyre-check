import typing

class MultiValueDict(typing.Dict[typing.Any, typing.Any]):
    def copy(self) -> MultiValueDict: ...
