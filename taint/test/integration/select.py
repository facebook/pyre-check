# @nolint


def view_entry_field(request):
    eval(request.payload)


def view_entry_index(request):
    eval(request.GET['payload'])


def first_index():
    x = __testSource()
    return x['access_token']


def first_index_numeric():
    x = __testSource()
    return x[0]


def first_index_unknown():
    x = __testSource()
    unknown = 'some text';
    return x[unknown]


def view_entry_get(request: django.http.Request):
    eval(request.GET.get('payload', 'foo bar'))
