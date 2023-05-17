---
id: pyre-faq
title: FAQ & Troubleshooting
sidebar_label: FAQ
---
import {OssOnly, FbInternalOnly} from 'docusaurus-plugin-internaldocs-fb/internal';

<FbInternalOnly>

<!-- TODO(T132521708) comb through Q&A summaries for questions that users frequently ask, especially ones in our backlog (https://www.internalfb.com/intern/qa?topic=Pyre%20Q%26A&query=%7B%22key%22%3A%22AND%22%2C%22children%22%3A[%7B%22key%22%3A%22EQUALS_ANY_OF_FBIDS%22%2C%22field%22%3A%22INTERN_QA_QUESTION_TOPIC%22%2C%22value%22%3A[%7B%22title%22%3A%22Pyre%20Q%26A%22%2C%22fbid%22%3A%22381019102517653%22%7D]%7D]%7D) marked as "documentation". -->

WIP

## Thrift Stubs

<!-- TODO(T132521708) Internal specific docs -->

</FbInternalOnly>

<OssOnly>

<!-- TODO(T132521708) OSS-specific docs -->

WIP

</OssOnly>

<!-- TODO(T132521708) OSS/Internal agnostic docs. ie., https://mypy.readthedocs.io/en/stable/faq.html  -->

<!-- TODO(T132521708) Common issues (ideally with info on how to contribute to this page via PR): ie., https://mypy.readthedocs.io/en/stable/common_issues.html  -->

## Pyre Gotchas
### Why isn't pyre checking functions that don't have a return signature?
Pyre [follows PEP 484](gradual_typing.md) which specifies gradual typing in Python. As such, Pyre only reports errors on functions that have an explicit return or parameter type annotation.

## Pyre Rage

<!-- TODO(T132521708) -->

## Typeshed & Stubs

<!-- TODO(T132521708) -->
