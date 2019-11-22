# flake8: noqa


def only_applies_to_first():
    return 1, 0


def only_applies_to_second():
    return 0, 1


def only_applies_to_nested():
    return ((0, 1), (0, 0))


def issue_only_with_first():
    issue, no_issue = only_applies_to_first()
    __test_sink(issue)
    __test_sink(no_issue)


def issue_only_with_second():
    no_issue, issue = only_applies_to_second()
    __test_sink(no_issue)
    __test_sink(issue)


def issue_only_with_nested_first():
    first, second = only_applies_to_nested()
    a, issue = first
    c, d = second
    __test_sink(issue)
    __test_sink(a)
    __test_sink(c)
    __test_sink(d)
    return only_applies_to_nested()
