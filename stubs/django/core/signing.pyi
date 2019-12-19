from datetime import timedelta
from typing import Optional, Union

class BadSignature(Exception):
    pass

class SignatureExpired(Exception):
    pass

class Signer:
    def signature(self, value: str) -> str: ...
    def sign(self, value: str) -> str: ...
    def unsign(self, signed_value: str) -> str: ...

class TimestampSigner(Signer):
    # pyre-ignore [14]: original django source violates LSP here
    def unsign(
        self, value: str, max_age: Optional[Union[str, timedelta]] = None
    ) -> str: ...

def get_cookie_signer(salt: str) -> TimestampSigner: ...
