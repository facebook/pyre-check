# @nolint

def test_from_1_to_0():
  x = 0
  change_arg0(x, __test_source())
  return x

def test_from_0_to_1():
  y = 0
  change_arg1(__test_source(), y)
  return y

def test_from_1_to_0_nested():
  x = {}
  change_arg0(x.foo, __test_source())
  return x.foo

def test_from_1_to_0_nested_distinct():
  x = {}
  change_arg0(x.foo, __test_source())
  return x.bar

def test_list_append():
  l = MyList()
  l.append(__test_source())
  return l

# Mocks that have models

def change_arg0(x, y): ...
def change_arg1(x, y): ...

class MyList:
  def append(self, data):
    pass
