from dataclasses import dataclass

from views import operate_on_twos


@dataclass
class UrlPattern:
    path: str
    callback: str


urlpatterns = [UrlPattern(r"^operate_on_twos/(.*)", operate_on_twos)]
