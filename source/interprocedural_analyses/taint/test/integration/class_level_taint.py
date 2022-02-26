# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class ClassSink:
    async def sink(self, argument):
        pass


class ClassSource:
    async def source(self):
        pass


def test(class_sink: ClassSink, class_source: ClassSource):
    class_sink.sink(class_source.source())
