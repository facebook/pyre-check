# pyre-unsafe

from email.mime.base import MIMEBase as MIMEBase
from typing import Any, Iterable, Optional, Tuple, Union

from django.core.mail.message import (
    EmailMessage as EmailMessage,
    SafeMIMEMultipart as SafeMIMEMultipart,
    SafeMIMEText as SafeMIMEText,
)

def send_mail(
    subject: Any,
    message: Any,
    from_email: Any,
    recipient_list: Any,
    fail_silently: bool = ...,
    auth_user: Any = ...,
    auth_password: Any = ...,
    connection: Any = ...,
    html_message: Any = ...,
) -> None: ...
