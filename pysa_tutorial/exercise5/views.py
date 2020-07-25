from typing import Callable


def api_wrapper(func: Callable):
    def inner(request):
        func(request, **request.GET)

    return inner


def operate_on_twos(request, operator: str):
    result = eval(f"2 {operator} 2")  # noqa: P204

    return result


@api_wrapper
def operate_on_threes(request, operator: str):

    exec(f"result = 3 {operator} 3")

    return result  # noqa: F821
