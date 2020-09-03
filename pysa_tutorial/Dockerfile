FROM ubuntu:18.04

RUN apt-get update

RUN apt-get install -y build-essential python3.7 python3.7-dev python3-pip python3.7-venv libssl-dev libffi-dev
RUN apt-get install -y git

# update pip
RUN python3.7 -m pip install pip --upgrade
RUN python3.7 -m pip install wheel

RUN mkdir /pyre-check
WORKDIR /pyre-check
COPY . .

RUN pip3 install psutil
RUN pip3 install pyre-check
RUN pip3 install click click-log ipython==7.6.1 munch pygments SQLAlchemy ujson~=1.35 xxhash~=1.3.0 prompt-toolkit~=2.0.9 flask flask_cors flask_graphql graphene_sqlalchemy
