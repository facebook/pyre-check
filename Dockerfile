FROM ocaml/opam:debian-10-ocaml-4.11

LABEL maintainer="gracewgao"

USER root

# installs system dependencies
RUN apt update && apt upgrade -y \
    && apt install python3 python3-pip pkg-config libsqlite3-dev build-essential -y \
    && opam switch create 4.10.2

# creates symlink for Python 3
RUN ln -s /usr/bin/python3 /usr/bin/python

# updates PATH variable for opam
ENV PATH="/home/opam/.opam/4.10.2/bin:$PATH"

# adds then installs Python dependencies
ADD --chown=1000:1000 requirements.txt /home/opam/pyre-check/
WORKDIR /home/opam/pyre-check
RUN pip3 install -r requirements.txt

# adds then unzips typeshed
ADD --chown=1000:1000 stubs/typeshed /home/opam/pyre-check/stubs/typeshed
WORKDIR /home/opam/pyre-check/stubs/typeshed
RUN unzip typeshed.zip && chown -R 1000:1000 typeshed-master

# adds then builds pyre-check
ADD --chown=1000:1000 . /home/opam/pyre-check
WORKDIR /home/opam/pyre-check
RUN ./scripts/setup.sh --local
WORKDIR /home/opam/pyre-check/source
RUN make && make test

# workaround to enable running local changes to pyre commands
RUN echo '#!/bin/bash\n \
    python -m client.pyre "$@" \n' >> /home/opam/pyre-check/scripts/pyre \
    && chmod +x /home/opam/pyre-check/scripts/pyre

# runs python tests
WORKDIR /home/opam/pyre-check
RUN ./scripts/run-python-tests.sh

# updates environment variables to use pyre-check
ENV PYRE_BINARY="/home/opam/pyre-check/source/_build/default/main.exe"
ENV PYRE_TYPESHED="/home/opam/pyre-check/stubs/typeshed/typeshed-master"
ENV PYTHONPATH="/home/opam/pyre-check:$PYTHONPATH"
ENV PATH="/home/opam/pyre-check/scripts:$PATH"

# switches to home directory and non-root user on launch
USER 1000
WORKDIR /home/opam/

CMD /bin/bash