# @nolint


def view_entry_field(request):
    eval(request.payload)

def view_entry_index(request):
    eval(request.GET['payload'])
