# @nolint


def view_entry_field(request):
    eval(request.payload)


def view_entry_index(request):
    eval(request.GET["payload"])


def first_index():
    x = __test_source()
    return x["access_token"]


def first_index_numeric():
    x = __test_source()
    return x[0]


def first_index_unknown():
    x = __test_source()
    unknown = "some text"
    return x[unknown]


def view_entry_get(request: django.http.Request):
    eval(request.GET.get("payload", "foo bar"))


def return_is_RCE(request: django.http.Request) -> None:
    return request.GET.get("payload", "foo bar")
