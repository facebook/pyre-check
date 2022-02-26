# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from django.db.models import CharField, EmailField, TextField

class CIText:
    def get_internal_type(self) -> str: ...
    def db_type(self, connection) -> str: ...

CICharField = CharField
CIEmailField = EmailField
CITextField = TextField
