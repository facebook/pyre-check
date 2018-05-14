from typing import Any

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
) -> None:
    ...
