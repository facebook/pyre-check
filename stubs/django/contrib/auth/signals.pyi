# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from django.dispatch import Signal

user_logged_out: Signal = ...
user_login_failed: Signal = ...
user_logged_in: Signal = ...
