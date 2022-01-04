# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from django.db import models

class BaseUserManager(models.Manager):
    pass

class AbstractBaseUser(models.Model):
    pass
