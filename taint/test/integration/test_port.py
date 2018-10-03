# @nolint


def source_field():
    result = {}
    result.a = __testSource()
    return result

def sink_field(arg):
    __testSink(arg.a)

def match_flows():
    x = source_field()
    sink_field(x)
