We do half yearly planning at Facebook and want to share this roadmap with the community. This document lists goals we want to achieve at the end of each half.

# H2 2019

## Coverage
We want to empower teams to increase type coverage in their codebase.

- Surface coverage data through LSP extensions
- Automatically suggest more type annotations through LSP code actions
- Make Pyre's inference mode easy to use

## Performance & Reliability
Consistency of incremental checks is difficult to get right. We have strict metrics on bigger projects internally for this.

- The Pyre server remains reliable, and doesn't terminate due to exceptions or memory issues
- Incremental checks are always consistent with full checks
- Our diagnostic tooling should make it easy to reproduce inconsistencies reported by users

## Static Analysis
We've built a platform to do static analysis for Python and want to extend its usage.

- Provide accurate type information for external tooling
- Implement analyses to find unawaited awaitables, dead stores, and immutable collections
- We continue to build out our security analysis and make it easier to use

## Open Source
We want to make Pyre easier to use outside of Facebook.

- Run on typeshed CI
- Robust documentation with sandbox and examples

## Numerical Stack
We have partially built the basic variadic extensions we suspect are necessary to type check frameworks like numpy or PyTorch and will continue that work.

- Extend the work we've done on variadics to include classes
- Support more operators on list variadics (Index, Concatenate)
- Experiment with stubs for PyTorch ignoring broadcasting
- Experiment with different approaches to deal with broadcasting

## User Experience
We want to make our experience better.

- Make errors easier to understand by surfacing trace information
