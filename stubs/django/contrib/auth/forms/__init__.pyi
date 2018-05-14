from django.forms import Form


class AuthenticationForm(Form):
    user_cache: Any = ...


class PasswordChangeForm(Form): ...


class SetPasswordForm(Form): ...
