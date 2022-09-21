# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Simple wrapper around the application code that defines an `application`
module-level global, which is what the wsgi web server gateway framework
expects.
"""
# pyre-ignore-all-errors

from application import get_server

application, _ = get_server()
