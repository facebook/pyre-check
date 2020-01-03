from django.http import HttpRequest, HttpResponse


def operate_on_twos(request: HttpRequest) -> HttpResponse:
    operator = request.GET["operator"]

    result = eval(f"2 {operator} 2")  # noqa: P204

    return result
