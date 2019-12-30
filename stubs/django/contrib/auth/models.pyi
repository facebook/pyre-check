# pyre-unsafe

from typing import Any, Optional

from django.db import models

class AnonymousUser(object):
    id: Optional[int]
    is_active: bool
    def is_anonymous(self) -> bool: ...
    def is_authenticated(self) -> bool: ...

class AbstractUser(models.Model):
    pass

class User(AbstractUser):
    pass

def _user_has_module_perms(user: Any, app_label: Any) -> bool: ...
