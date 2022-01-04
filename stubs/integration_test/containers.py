# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from django.http import HttpRequest


def dictionary_update(request: HttpRequest):
    result = {}
    source = {"source": request.GET["bad"]}
    result.update(source)
    eval(result)


def list_append(request: HttpRequest):
    l = []
    l.append(request.GET["bad"])
    eval(l[0])


def list_update(request: HttpRequest):
    l = []
    tainted_list = [request.GET["bad"]]
    l.extend(tainted_list)
    eval(l[0])


def list_insert(request: HttpRequest):
    l = [1] * 10
    l.insert(5, request.GET["bad"])
    eval(l[5])


def set_add(request: HttpRequest):
    s = {1}
    s.add(request.GET["bad"])
    for element in s:
        eval(element)


def set_intersection_update(request: HttpRequest):
    s = {1}
    s.intersection_update({request.GET["bad"]})
    for element in s:
        eval(element)


def set_update(request: HttpRequest):
    s = {1}
    s.update({request.GET["bad"]})
    for element in s:
        eval(element)
