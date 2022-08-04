# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source, _test_sink
from typing import Dict, Any


def async_render_to_response(
    request: str,
    context: Dict[str, Any],
):
    _test_sink(context)


def async_distillery_render(request, **kwargs: Any):
    # Any source in dict `kwargs` should not reach the sink
    # in async_render_to_response
    kwargs['request'] = _test_source()
    kwargs['context'] = _test_source()
    kwargs.pop('context')
    async_render_to_response(**kwargs)
    # Only the value of key 'request' in dict `kwargs` is a source
    return kwargs


def args_sink(*args):
    # Only the first element of tuple `args` is a sink
    _test_sink(args[1])
