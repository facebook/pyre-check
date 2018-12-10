# @nolint

class Data:
  def __init__(self, a, b):
      self.a = a
      self.b = b



def get_data(x):
    return { 'name': x, 'id': x }


def product_data(x):
    data = get_data(x)

    if x:
        parent = product_data(x.parent)
    else:
        parent = None

    is_blocked = some_service(data.id)
    report_tuple = DataRecord(
        id=data.id,
        username=data.name,
        isBlocked=is_blocked,
    )
    return {
        'report': _unpack(report_tuple),
        'id': data.id,
        'parent_data': parent,
        'name': data.name,
    }


def product_data_wrapper(x):
    return product_data(x)

def test_tito():
    return product_data_wrapper(__testSource())
