# Pysa Tutorial

This tutorial will walk you through practical exercises highlighting essential
features of the Python Static Analyzer (Pysa) that comes with Pyre. The tutorial
is meant to be consumed along with an accompanying presentation. It is possible
to go through this tutorial without the presentation, but you will want to read
the [public Pysa docs](https://pyre-check.org/docs/pysa-basics.html), and refer
to them throughout the exercises.

The tutorial contains 5 exercises that cover the following topics: running and
observing results, adding _sinks_ and _rules_, using _sanitizers_ to filter
results, adding _features_ to and browsing results with SAPP, and the model
generation concept. The tutorial assumes we are ramping up to analyze a Django
project.

## Setup Instructions

1. Install the correct environment. From this directory, run:

   ```
   python3 -m venv tutorial
   source tutorial/bin/activate
   pip3 install pyre-check
   pip3 install click click-log ipython==7.6.1 munch pygments SQLAlchemy ujson~=1.35 xxhash~=1.3.0 prompt-toolkit~=2.0.9
   ```

1. If doing this tutorial with an accompanying lecture, wait for the
   presentation to resume. If going through on your own, go to the first
   exercise:

   ```
   cd exercise1
   cat README.md
   ```
