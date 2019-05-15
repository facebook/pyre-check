# @nolint

from util.tasks import igtask
from django.http import HttpRequest


# Integration test illustrating that we properly propagate taint through `@igtask`s.


@igtask()
def eval_igtask(self, into_eval):
    eval(into_eval)


def whoops(request: HttpRequest) -> None:
    user_controlled = request.GET['user_controlled']
    eval_igtask(user_controlled)
