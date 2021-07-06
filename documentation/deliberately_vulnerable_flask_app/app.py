# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess

from flask import Flask


app = Flask(__name__)


@app.route("/rce/<string:payload>")
def definite_rce(payload: str) -> None:
    subprocess.run(payload, shell=True)


@app.route("/rce/<string:payload>")
def potential_rce_1(payload: str) -> None:
    subprocess.run(["echo", payload])


@app.route("/rce/<int:payload>")
def potential_rce_2(payload: int) -> None:
    subprocess.run(f"echo {payload}", shell=True)
