# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sqlite3
import subprocess
import smtplib
from pathlib import Path

import flask
import jsonpickle
import requests

from flask import Flask, render_template
from lxml import etree
from werkzeug.utils import redirect

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

@app.route("/getattr_tp/<string:payload>")
def user_data_to_getattr_tp(payload: str) -> None:
    jsonpickle.unpickler.loadclass(payload)

@app.route("/getattr_tn/<string:payload>")
def user_data_to_getattr_tn(payload: str) -> None:
    payload = "payload"
    jsonpickle.unpickler.loadclass(payload)

@app.route("/user_data_to_filesystem_read_write_tp/<path:payload>")
def user_data_to_filesystem_read_write_tp(payload: Path) -> None:
    with open(payload) as f:
        data = f.read()
    return data

@app.route("/open_redirect_tp/<string:payload>")
def open_redirect_tp(payload: str) -> None:
    redirect(payload)

@app.route("/open_redirect_tn/<int:payload>")
def open_redirect_tn(payload: int) -> None:
    redirect("test", payload)

@app.route("/user_controlled_data_to_email_send_to_users_tp/<string:payload>")
def user_controlled_data_to_email_send_to_users_tp(payload: str) -> None:
    smtp_obj = smtplib.SMTP("test", 123, "test", 3000)
    smtp_obj.sendmail("test", "test", payload)

@app.route("/user_controlled_data_to_email_send_to_users_tn/<string:payload>")
def user_controlled_data_to_email_send_to_users_tn(payload: str) -> None:
    payload = "test"
    smtp_obj = smtplib.SMTP("test", 123, "test", 3000)
    smtp_obj.sendmail("test", "test", payload)

@app.route("/user_controlled_data_flows_into_url_like_string_tp/<string:payload>")
def user_controlled_data_flows_into_url_like_string_tp(payload: str) -> None:
    url = "https://test/" + payload # noqa

@app.route("/user_controlled_data_flows_into_url_like_string_tn/<string:payload>")
def user_controlled_data_flows_into_url_like_string_tn(payload: str) -> None:
    url = "test/" + payload # noqa
