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


class SourceInConstructor:
    def __init__(self):
        self.x = __test_source()
        self.y = 0


def test_source_in_constructor():
    c = SourceInConstructor()
    __test_sink(c.x)
    __test_sink(c.y)


class ParentWithInit:
    def __init__(self):
        pass


class ChildWithNew(ParentWithInit):
    def __new__(cls, input):
        __test_sink(input)
        return object.__new__(cls)


def test_new_thing():
    c = ChildWithNew(__test_source())
