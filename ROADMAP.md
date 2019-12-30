We do half yearly planning at Facebook and want to share this roadmap with the community. This document lists goals we want to achieve at the end of each half.

# H1 2020

## User Experience
After focusing on foundational architectural changes to make Pyre reliable and performant, we want our users to reap the benefits of that work. To start we will focus on measuring %sad (distribution of interactions categorized as good vs. bad), cycle time, and sentiment, and driving down %sad.

- Design metrics capturing the user experience through the command line as well as IDEs
- Build robust infrastructure for collecting, tracking and visualizing that helps us identify and prioritize issues
- Substantially move %sad metric baselines

## Coverage
We want to build infrastructure that makes it easy for people to adopt type checking and drive coverage in their codebase.

- Surface coverage data in editors, code review tools, and in internal code quality dashboards
- Make type inference codemods easy
- Support strongly typed Django API endpoints

## Performance & Reliability
Do not regress on reliability wins from last half, and continue to invest in making Pyre more performant.

- No regression on Pyre cold start time
- Make incremental checks twice as fast
- No reliability regression: provably consistent full check vs. incremental check results

## Open Source
Ensure external users also have a good experience onboarding with and using Pyre.

- Onboard more than one Facebook open source project
- Reproducibly reliable onboarding user experience
- Regular cadence for external version releases, with automated CI testing
- Robust documentation including pyre infer, how to run on Github CI, and virtualenv workflows
- More responsive to open source issues

## Type System Usability
We care about investing in type system improvements and features to better support diverse applications of the language.

- Fully support high value dynamic libraries (e.g. click, SQLAlchemy)
- Improve usability of optional types with attributes; i.e. lazy attributes, improved refinement semantics
- Fully support all Python 3.8 features
- Investigate the frequency/severity of shape mismatch errors in production ML pipelines.

## Taint Analysis
We plan on improving the coverage and expressiveness of Pysa, our taint analysis, and make it a generally usable tool for open source use cases.

- Build a web-based UI available for open source users, similar to the Facebook-internal UI that we use for the consumption of results.
- Ensure that all Pysa features are documented and that a tutorial is available.
- Invest in the performance of the taint analysis.

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
