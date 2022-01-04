# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def source():
    pass


def sinkA(x):
    pass


def sinkB(x):
    pass


def sinkC(x):
    pass


def sinkD(x):
    pass


def split(x):
    y = x._params
    sinkB(y)
    sinkC(y)
    sinkD(y)
    return x


def wrapper(x):
    y = split(x)
    sinkA(y)


def issue():
    x = source()
    wrapper(x)


def splitwrapper(x):
    return split(x)


class QueryBase:
    def send(self):
        pass


class Query(QueryBase):
    _params = None

    def send(self):
        return splitwrapper(self)

    def params(self, data):
        self._params = data
        return self


def log_call(params, response):
    sinkA(params)
    sinkA(response)


def wrapper2(x: Query):
    params = x._params
    response = None
    try:
        response = x.send()
    except Exception as ex:
        raise ex
    log_call(params, response)


def issue2():
    taint = source()
    query = Query().params(taint)
    wrapper2(query)
