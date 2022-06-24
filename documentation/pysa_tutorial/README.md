# Pysa Tutorial

This tutorial will walk you through practical exercises highlighting essential
features of the Python Static Analyzer (Pysa) that comes with Pyre. The tutorial
is meant to be consumed along with an accompanying presentation. You can see a
recording of the accompanying presentation from DEF CON 28
[here](https://www.youtube.com/watch?v=8I3zlvtpOww).

It is possible to go through this tutorial without the presentation, but you
will want to read the [public Pysa
docs](https://pyre-check.org/docs/pysa-basics.html), and refer to them
throughout the exercises.

The tutorial contains 5 exercises that cover the following topics: running and
observing results, adding _sinks_ and _rules_, using _sanitizers_ to filter
results, adding _features_ to and browsing results with SAPP, and the model
generation concept. The tutorial assumes we are ramping up to analyze a Django
project.

## Setup Instructions

1. If you aren't already looking at this README on your local machine (ie. you're
   reading this on GitHub), clone this repo and navigate to the root of the folder:

   ```
   git clone https://github.com/facebook/pyre-check.git
   cd pyre-check
   ```

2. Install the correct environment. We currently support Ubuntu 18.04 and OSX at
   the moment with plans to extend compatibility in the future.

      *For supported OS* -
      ```
      cd documentation/pysa_tutorial

      # STOP! Make sure you're in pyre-check/documentation/pysa_tutorial

      python3 -m venv tutorial
      source tutorial/bin/activate
      pip3 install pyre-check fb-sapp
      ```

      *Others* -

      You can use our Docker container. First install Docker by following the steps
      on https://docs.docker.com/engine/install/ and then enter the following in
      your terminal.

      NOTE: You will want to make sure you allow Docker at least 6GB of memory.
      This can be configured in Docker > Preferences > Resources > Memory.

      ```
      docker build -f documentation/pysa_tutorial/Dockerfile -t pyre-check-docker .
      docker run --rm --name pyre-check -it pyre-check-docker /bin/bash

      # You will now be inside the docker container you just created
      cd documentation/pysa_tutorial
      ```

3. If doing this tutorial with an accompanying lecture, wait for the
   presentation to resume. If going through on your own, go to the first
   exercise:

   ```
   cd exercise1
   cat README.md
   ```
