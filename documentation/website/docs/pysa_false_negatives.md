---
id: pysa-false-negatives
title: Debugging False Negatives
sidebar_label: Debugging False Negatives
---

False Negatives occur when there is a legitimate flow of tainted data from a
source to a sink, but Pysa fails to catch it.

## Common Causes of False Negatives

### Missing Type Information

Pysa relies on type information from Pyre to identify sources and sinks, and to
build the call graph needed to follow the propagation of taint between the two.
Just because type information is available *somewhere* in the code, does not
mean Pyre will know the type of an object in the exact place where Pysa needs
it. See the documentation on [Coverage Increasing
Strategies](pysa_increasing_coverage.md) for tips on how to increase type
coverage. The following examples demonstrate how lost type information leads to
lost flows.

#### Missing Sources/Sinks

`HttpRequest.GET` is a common source of `UserControlled` data in Django. If the
`request` objects are not explicitly typed as `HttpRequest`, however, Pysa will
fail to detect obvious issues:

```python
from django.http import HttpRequest

def this_is_missed(request):
    # This flow WILL NOT be found, because Pysa does not know the type of
    # 'request' at this point and thus does not know 'request.GET' is a source
    # (even though the type is known in 'run')
    eval(request.GET["command"])

def this_is_caught(request: HttpRequest):
    # This flow WILL be found.
    eval(request.GET["command"])

def run(request: HttpRequest):
    this_is_missed(request)
    this_is_caught(request)
```

#### Incomplete Call Graph

Pysa relies on type information in order to build a call graph that accurately
tracks a method call of `foo.bar(x)` to the `def bar(self, x)` implementation.
Without type information on `bar`, Pysa will be unable to figure out how to
dispatch the call and the flow will be lost:

```python

from django.http import HttpRequest

class Runner:
    def run(self, command: str) -> None:
        eval(command)

def this_is_missed(request: HttpRequest, runner):
    # This flow WILL NOT be found, because Pysa does not know the type of
    # 'runner', and thus does not know where the 'run' call dispatches to
    runner.run(request.GET["command"])

def this_is_caught(request: HttpRequest, runner: Runner):
    # This flow WILL be found.
    runner.run(request.GET["command"])
```

### Globals

To allow for parallel processing, Pysa is limited in it's ability to track taint
flows through global variables. For example, Pysa will not detect an issue in
the following code:

```python
user_controlled_data = ""

def load_data(request: HttpRequest) -> None:
    user_controlled_data = request.GET["data"]

def run_command(request: HttpRequest) -> None:
    load_data(request)
    eval(user_controlled_data)
```

The best workaround is to avoid using globals in your code. If a refactor isn't
possible, but you do know what globals should be considered tainted, you can
explicitly declare the global tainted in your `.pysa` files.

## Methodology for Debugging False Negatives

1. Identify the flow you expect to see
   1. Source
   1. Sink
   1. Every function call/return that propagates the tainted data from the
      source to the sink
   1. Every variable that the tainted data passes through, within the identified
      functions. This usually includes the parameter which initially received
      the taint, and then 0 or more local variables that hold the tainted data
      as it is transformed in some way.
1. Add a [`reveal_taint`](pysa_tips.md#reveal_taintyour_variable) and
   [`reveal_type`](pysa_tips.md#reveal_typeyour_variable) statement to each of
   the variables identified in the previous step
1. Run Pysa using the same command you used when the false negative manifested,
   but also include the `--noninteractive` flag (eg. `pyre --noninteractive
   analyze`)
1. Start following the flow from source to sink in your code, and find the
   corresponding output for each `reveal_taint` statement.
   - Note that each time Pysa analyzes a function (could be many times), it will
     dump the latest taint information, so **the last instance of `reveal_taint`
     output for a given line will be the most accurate and is the one you should
     look at.**
   - `reveal_taint` output exposes some of the [implementation
     details](pysa_implementation_details.md) of Pysa, by giving you `Revealed
     forward taint` and `Revealed backward taint` messages. Without going into
     those details, you should expect to see *either* the source name (eg.
     `UserControlled`) you care about appearing in the `Revealed forward taint`
     output, or the sink name (eg. `RemoteCodeExecution`) you care about in the
     `Revealed backward taint` output.
   1. For each `reveal_taint`, following the flow of tainted data from source to
      sink, locate the output in the logs that reveals the taint (eg.
      `integration_test.reveal_taint:20:4-20:16: Revealed forward taint for
      ``command``:`).
   1. If you see your source or sink name in the output, then go back to 1) and
      carry on with the next `reveal_taint` statement. If you *do not* see the
      source or sink name, then that means the cause of the false negative is
      likely between your previous `reveal_taint` and the one you're currently
      looking at. Refer to the "Commom Causes of False Negatives" section above
      for ideas on the cause, and how to fix it.

### Example

Pysa will not be able to detect a vulnerability in the following code:
```python
from django.http import HttpRequest, HttpResponse

class Runner:
    def run(self, command: str) -> None:
        eval(command)

def get_command(request: HttpRequest) -> str:
    command = request.GET["command"]
    return command

def execute_command(runner: Runner, command):
    runner.run(command)

def start(request: HttpRequest):
    command = get_command(request)
    runner = Runner()
    execute_command(runner, command)
```

Folling the above debugging steps we identify the flow of data from beginning to
end, and add debugging statements:

```python

from django.http import HttpRequest, HttpResponse

class Runner:
    def run(self, command: str) -> None:
        reveal_type(command)
        reveal_taint(command)
        eval(command) # 5. User controlled data reaches the sink in here

def get_command(request: HttpRequest) -> str:
    command = request.GET["command"] # 1. User controlled data originates here
    reveal_type(command)
    reveal_taint(command)
    return command

def execute_command(runner: Runner, command):
    reveal_type(command)
    reveal_taint(command)
    reveal_type(runner)
    reveal_taint(runner)
    runner.run(command) # 4. User controlled data is passed in here

def start(request: HttpRequest):
    command = get_command(request) # 2. User controlled data is returned here
    reveal_type(command)
    reveal_taint(command)
    runner = Runner()
    execute_command(runner, command) # 3. User controlled data is passed in here
```
See the appendix for the full output of running `pyre --noninteractive analyze` on this example.

Starting at 1), we see this in the output:
```
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:16: Revealed forward taint for `command`: @integration_test.reveal_taint:11:14-11:25 -> UserControlled -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,486 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING UserControlled_Payload -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,486 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:25: Revealed backward taint for `command`: declaration -> LocalReturn -> SimpleFeature: [], ComplexFeature: [(Features.Complex.ReturnAccessPath [])], TraceLength: 4611686018427387903, FirstIndex: [], FirstField: []
```
Removing the timestamps and other noise gives us:
```
integration_test.reveal_taint:13:4-13:16:
  Revealed forward taint for `command`:
    @integration_test.reveal_taint:11:14-11:25 -> UserControlled ->
    SimpleFeature: [
      Features.Simple.LeafName {
        leaf = "Obj{django.http.request.HttpRequest.GET}"; port = None
      }
    ],
    ComplexFeature: [],
    TraceLength: 0,
    FirstIndex: ["command"],
    FirstField: []

    UserControlled_Payload -> SimpleFeature: [
      Features.Simple.LeafName {
        leaf = "Obj{django.http.request.HttpRequest.GET}"; port = None
      }
    ],
    ComplexFeature: [],
    TraceLength: 0,
    FirstIndex: ["command"],
    FirstField: []

integration_test.reveal_taint:13:4-13:25:
  Revealed backward taint for `command`:
    declaration -> LocalReturn ->
    SimpleFeature: [],
    ComplexFeature: [(Features.Complex.ReturnAccessPath [])],
    TraceLength: 4611686018427387903,
    FirstIndex: [],
    FirstField: []
```
For debugging false negatives, the only portion we care about is:
```
  Revealed forward taint for `command`:
    @integration_test.reveal_taint:11:14-11:25 -> UserControlled
```
This confirms that on line 11 (characters 14-25), we did indeed detect that
`command` was tainted as `UserControlled`.

Moving on to 2, the `forward taint` output again tells us that we have
`UserControlled` taint on `command` at line 26 (characters 4-16).

Starting with 4, we notice that we no longer see `UserControlled` or
`RemoteCodeExecution` in our revealed forward or backwards taint:
```
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:18:4-18:16: Revealed forward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:18:4-18:25: Revealed backward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
```
This has helped us narrow down the problem to the `execute_command` function. In
the end, the problem was that we did not have type information on `runner`, so
Pysa did not know where the definition of `runner.run` was. Without knowing
where the definition was, Pysa couldn't know that `run` containted a sink and
thus couldn't know that `command` eventually reached that sink.

## Appendix
Subset of the output from running `pyre --noninteractive analyze` on the
example:
```
2020-12-28 13:02:31,719 [PID 3382063] PERFORMANCE Overrides recorded: 2.408138s
2020-12-28 13:02:31,719 [PID 3382063] INFO Building call graph...
2020-12-28 13:02:34,166 [PID 3382063] PERFORMANCE Call graph built: 2.447174s
2020-12-28 13:02:34,166 [PID 3382063] INFO Call graph edges: 100
2020-12-28 13:02:34,166 [PID 3382063] INFO Computing overrides...
2020-12-28 13:02:34,311 [PID 3382063] PERFORMANCE Computed overrides: 0.144886s
2020-12-28 13:02:34,311 [PID 3382063] PERFORMANCE Pre-fixpoint computation for static analysis: 7.664068s
2020-12-28 13:02:34,311 [PID 3382063] INFO Analysis fixpoint started for 3075 overrides 68 functions...
2020-12-28 13:02:34,311 [PID 3382063] INFO Iteration #0. 3143 Callables [...]
2020-12-28 13:02:35,471 [PID 3382063] WARNING integration_test.reveal_taint:6:8-6:19: Revealed type for command: str
2020-12-28 13:02:35,471 [PID 3382063] WARNING integration_test.reveal_taint:7:8-7:20: Revealed forward taint for `command`:
2020-12-28 13:02:35,471 [PID 3382063] WARNING
2020-12-28 13:02:35,471 [PID 3382063] WARNING integration_test.reveal_taint:7:8-7:29: Revealed backward taint for `command`: @integration_test.reveal_taint:8:13-8:20 -> RemoteCodeExecution -> SimpleFeature: [Features.Simple.LeafName {leaf = "eval"; port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: [], FirstField: []
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:17:4-17:15: Revealed type for command: typing.Any
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:18:4-18:16: Revealed forward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:19:4-19:15: Revealed type for command: typing.Any
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:20:4-20:16: Revealed forward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:20:4-20:25: Revealed backward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,472 [PID 3382063] WARNING integration_test.reveal_taint:18:4-18:25: Revealed backward taint for `command`:
2020-12-28 13:02:35,472 [PID 3382063] WARNING
2020-12-28 13:02:35,473 [PID 3382063] WARNING integration_test.reveal_taint:12:4-12:15: Revealed type for command: str
2020-12-28 13:02:35,473 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:16: Revealed forward taint for `command`: @integration_test.reveal_taint:11:14-11:25 -> UserControlled -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:35,473 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:35,473 [PID 3382063] WARNING UserControlled_Payload -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:35,473 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:35,473 [PID 3382063] WARNING
2020-12-28 13:02:35,473 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:25: Revealed backward taint for `command`: declaration -> LocalReturn -> SimpleFeature: [], ComplexFeature: [(Features.Complex.ReturnAccessPath [])], TraceLength: 4611686018427387903, FirstIndex: [], FirstField: []
2020-12-28 13:02:35,473 [PID 3382063] WARNING
2020-12-28 13:02:35,480 [PID 3382063] WARNING integration_test.reveal_taint:25:4-25:15: Revealed type for command: str
2020-12-28 13:02:35,480 [PID 3382063] WARNING integration_test.reveal_taint:26:4-26:16: Revealed forward taint for `command`: via call@integration_test.reveal_taint:24:14-24:34[integration_test.reveal_taint.get_command][{ root = LocalResult; path = [] }] -> UserControlled -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:35,480 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 1, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:35,480 [PID 3382063] WARNING UserControlled_Payload -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:35,480 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 1, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:35,480 [PID 3382063] WARNING
2020-12-28 13:02:35,481 [PID 3382063] WARNING integration_test.reveal_taint:26:4-26:25: Revealed backward taint for `command`:
2020-12-28 13:02:35,481 [PID 3382063] WARNING
2020-12-28 13:02:35,546 [PID 3382063] PERFORMANCE Expensive callables for iteration 0:
2020-12-28 13:02:35,577 [PID 3382063] INFO Iteration #0, 3143 callables, heap size 46105024 took 1.266790s
2020-12-28 13:02:35,578 [PID 3382063] INFO Iteration #1. 3038 Callables [...]
2020-12-28 13:02:36,482 [PID 3382063] WARNING integration_test.reveal_taint:6:8-6:19: Revealed type for command: str
2020-12-28 13:02:36,482 [PID 3382063] WARNING integration_test.reveal_taint:7:8-7:20: Revealed forward taint for `command`:
2020-12-28 13:02:36,483 [PID 3382063] WARNING
2020-12-28 13:02:36,483 [PID 3382063] WARNING integration_test.reveal_taint:7:8-7:29: Revealed backward taint for `command`: @integration_test.reveal_taint:8:13-8:20 -> RemoteCodeExecution -> SimpleFeature: [Features.Simple.LeafName {leaf = "eval"; port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: [], FirstField: []
2020-12-28 13:02:36,483 [PID 3382063] WARNING
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:12:4-12:15: Revealed type for command: str
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:16: Revealed forward taint for `command`: @integration_test.reveal_taint:11:14-11:25 -> UserControlled -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,486 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING UserControlled_Payload -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,486 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 0, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:13:4-13:25: Revealed backward taint for `command`: declaration -> LocalReturn -> SimpleFeature: [], ComplexFeature: [(Features.Complex.ReturnAccessPath [])], TraceLength: 4611686018427387903, FirstIndex: [], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:25:4-25:15: Revealed type for command: str
2020-12-28 13:02:36,486 [PID 3382063] WARNING integration_test.reveal_taint:26:4-26:16: Revealed forward taint for `command`: via call@integration_test.reveal_taint:24:14-24:34[integration_test.reveal_taint.get_command][{ root = LocalResult; path = [] }] -> UserControlled -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,486 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 1, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,486 [PID 3382063] WARNING UserControlled_Payload -> SimpleFeature: [Features.Simple.LeafName {leaf = "Obj{django.http.request.HttpRequest.GET}";
2020-12-28 13:02:36,487 [PID 3382063] WARNING   port = None}], ComplexFeature: [], TraceLength: 1, FirstIndex: ["command"], FirstField: []
2020-12-28 13:02:36,487 [PID 3382063] WARNING
2020-12-28 13:02:36,492 [PID 3382063] WARNING integration_test.reveal_taint:26:4-26:25: Revealed backward taint for `command`:
2020-12-28 13:02:36,492 [PID 3382063] WARNING
2020-12-28 13:02:36,552 [PID 3382063] PERFORMANCE Expensive callables for iteration 1:
2020-12-28 13:02:36,585 [PID 3382063] INFO Iteration #1, 3038 callables, heap size 46521728 took 1.007461s
2020-12-28 13:02:36,585 [PID 3382063] INFO Iteration #2. 23 Callables [...]
2020-12-28 13:02:37,018 [PID 3382063] PERFORMANCE Expensive callables for iteration 2:
2020-12-28 13:02:37,018 [PID 3382063] INFO Iteration #2, 23 callables, heap size 46530432 took 0.432597s
2020-12-28 13:02:37,018 [PID 3382063] INFO Iteration #3. 2 Callables [integration_test.string_concatenation.bad_1 (fun), integration_test.string_concatenation.bad_2 (fun)]
2020-12-28 13:02:37,130 [PID 3382063] PERFORMANCE Expensive callables for iteration 3:
2020-12-28 13:02:37,131 [PID 3382063] INFO Iteration #3, 2 callables, heap size 46532352 took 0.113038s
2020-12-28 13:02:37,131 [PID 3382063] INFO Iteration #4. 0 Callables []
2020-12-28 13:02:37,131 [PID 3382063] INFO Fixpoint iterations: 4
2020-12-28 13:02:37,348 [PID 3382063] PERFORMANCE Analysis fixpoint complete: 3.037628s
2020-12-28 13:02:37,369 [PID 3382063] PERFORMANCE Analyze: 18.289002s
```
