from typing import Dict

from django.http import HttpRequest

def csrf(request: HttpRequest) -> Dict[str, str]: ...
