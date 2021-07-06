# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import sqlite3
from flask import Flask
import request
from lxml import etree

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


@app.route("/pt/<string:payload>")
def definite_pt(payload: str) -> str:
    f = open(payload, "r")
    text = f.read()
    return text


@app.route("/xss/<string:payload>")
def definite_xss(payload: str) -> None:
    flask.Markup(payload)


@app.route("/sql/<string:payload>")
def definite_sql(payload: str) -> None:
    con = sqlite3.connect()
    cur = con.cursor()
    cur.execute(payload)


@app.route("/ssrf/<string:payload>")
def definite_ssrf(payload: str) -> None:
    requests.get(payload)


@app.route("/xxe/<string:payload>")
def definite_xxe(payload: str) -> None:
    etree.fromstring(payload)
