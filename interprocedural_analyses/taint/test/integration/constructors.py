# flake8: noqa


class SomeAPI:
    HOST = "api.some.com/1.1"
    AUTHENTICATE_URL = f"https://{HOST}/some.json"

    def __init__(self, oauth_token: str, oauth_token_secret: str) -> None:
        self.oauth_token = oauth_token
        self.oauth_token_secret = oauth_token_secret

    @classmethod
    def from_default_keys(cls, oauth_token: str, oauth_token_secret: str) -> "SomeAPI":
        return cls(oauth_token, oauth_token_secret)

    def async_get_authenticated_user(self):
        eval(self.AUTHENTICATE_URL)


def test_construction(request: HttpRequest):
    data = request.POST
    instance = SomeAPI.from_default_keys(data["1"], data["2"])
    instance.async_get_authenticated_user()
    return instance
