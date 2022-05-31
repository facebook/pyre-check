# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sqlite3
import subprocess

import flask
import requests
from flask import Flask, render_template
from lxml import etree
import sh.sh as sh

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
    content = flask.Markup(payload)
    return render_template(content)


@app.route("/sql/<string:payload>")
def definite_sql(payload: str) -> None:
    con = sqlite3.connect()
    cur = con.cursor()
    cur.execute(f"SELECT info FROM users WHERE name={payload}")


@app.route("/ssrf/<string:payload>")
def definite_ssrf(payload: str) -> None:
    requests.get(payload)


@app.route("/xxe/<string:payload>")
def definite_xxe(payload: str) -> None:
    etree.fromstring(payload)


@app.route("/rce/<string:payload>") # picked
def potential_rce_3(payload: str) -> None:
    sh.Command(path=payload, search_paths=[payload])


@app.route("/rce/<string:payload>") # picked
def potential_rce_4(payload: str) -> None:
    sh.RunningCommand(payload, call_args=[payload])


@app.route("/rce/<string:payload>") # picked
def potential_rce_5(payload: str) -> None:
    sh.OProc(command=payload, cmd=[payload])


@app.route("/rce/<string:payload>") # picked
def potential_rce_6(payload: str) -> None:
    sh.which(payload, paths=[payload])


@app.route("/rce/<string:payload>")
def potential_rce_7(payload: str) -> None:
    sh.ls(payload, payload)