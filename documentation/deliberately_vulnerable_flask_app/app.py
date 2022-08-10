# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import sqlite3
import subprocess
from pathlib import Path
from smtplib.SMTP import sendmail
from wsgiref.headers.Headers import setdefault

import flask
import jsonpickle
import requests

from flask import Flask, render_template
from jinja2.environment import Template
from lxml import etree
from mypy_boto3_acm.client.ACMClient import export_certificate
from werkzeug.utils import redirect

app = Flask(__name__)
parser = argparse.ArgumentParser(description="test arguments - do not use")
parser.add_argument('payload', metavar='P', type=str)
args = parser.parse_args()

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
    flask.__get_attr__(payload)


@app.route("/getattr_tn/<string:payload>")
def user_data_to_getattr_tn(payload: str) -> None:
    payload = "payload"
    flask.__get_attr__(payload)

@app.route("/user_data_to_filesystem_read_write_tp/<path:payload>")
def user_data_to_filesystem_read_write_tp(payload: Path) -> None:
    with open(payload) as f:
        data = f.read()
    return data

@app.route("/user_data_to_filesystem_read_write_tn/<path:payload>")
def user_data_to_filesystem_read_write_tn(payload: Path) -> None:
    return flask.send_from_directory("test", payload)

@app.route("/open_redirect_tp/<string:payload>")
def open_redirect_tp(payload: str) -> None:
    redirect(payload)

@app.route("/open_redirect_tn/<int:payload>")
def open_redirect_tn(payload: int) -> None:
    redirect("test", payload)

@app.route("/server_secrets_reach_exit_tp/")
def server_secrets_reach_exit_tp() -> None:
    return export_certificate()

@app.route("/server_secrets_reach_exit_tn/")
def server_secrets_reach_exit_tn() -> None:
    test = requests.get(export_certificate())
    return test

@app.route("/user_controlled_response_headers_tp/<string:payload>")
def user_controlled_response_headers_tp(payload: str) -> None:
    setdefault(payload)

@app.route("/user_controlled_response_headers_tn/<string:payload>")
def user_controlled_response_headers_tn(payload: str) -> None:
    payload = "test"
    setdefault(payload)

@app.route("/user_controlled_data_to_email_send_to_users_tp/<string:payload>")
def user_controlled_data_to_email_send_to_users_tp(payload: str) -> None:
    sendmail("test", "test", payload)

@app.route("/user_controlled_data_to_email_send_to_users_tn/<string:payload>")
def user_controlled_data_to_email_send_to_users_tn(payload: str) -> None:
    payload = "test"
    sendmail("test", "test", payload)

@app.route("/possible_shell_injecton_via_command_line_arguments_tp/")
def possible_shell_injecton_via_command_line_arguments_tp() -> None:
    eval(args.payload)

@app.route("/possible_shell_injecton_via_command_line_arguments_tn/")
def possible_shell_injecton_via_command_line_arguments_tn() -> None:
    eval("1")

@app.route("/potential_ssti_tp")
def potential_ssti_tp() -> None:
    response = requests.get("http://test/test.json")
    template = Template(response) # noqa

@app.route("/potential_ssti_tn")
def potential_ssti_tn() -> None:
    response = "test"
    template = Template(response) # noqa

@app.route("/unsafe_deserialisation_may_result_in_rce_tp/")
def unsafe_deserialisation_may_result_in_rce_tp() -> None:
    response = requests.get("http://test/test.json")
    jsonpickle.decode(response)

@app.route("/unsafe_deserialisation_may_result_in_rce_tn/")
def unsafe_deserialisation_may_result_in_rce_tn() -> None:
    response = "test"
    jsonpickle.decode(response)

@app.route("/commandline_arguments_injection_may_result_in_rce_tp/")
def commandline_arguments_injection_may_result_in_rce_tp() -> None:
    response = requests.get("http://test/test.json")
    subprocess.run(response)

@app.route("/commandline_arguments_injection_may_result_in_rce_tn/")
def commandline_arguments_injection_may_result_in_rce_tn() -> None:
    response = requests.get("http://test/test.json")
    response = "echo 'test'"
    subprocess.run(response)

@app.route("/environment_variable_or_import_injection_may_result_in_rce_tp/")
def environment_variable_or_import_injection_may_result_in_rce_tp() -> None:
    response = requests.get("http://test/test.json")
    __import__(response)

@app.route("/environment_variable_or_import_injection_may_result_in_rce_tn/")
def environment_variable_or_import_injection_may_result_in_rce_tn() -> None:
    response = requests.get("http://test/test.json")
    response = "test"
    __import__(response)

@app.route("/user_controlled_data_flows_into_url_like_string_tp/<string:payload>")
def user_controlled_data_flows_into_url_like_string_tp(payload: str) -> None:
    url = "https://test/" + payload # noqa

@app.route("/user_controlled_data_flows_into_url_like_string_tn/<string:payload>")
def user_controlled_data_flows_into_url_like_string_tn(payload: str) -> None:
    url = "test/" + payload # noqa

@app.route("/exception_returned_to_user_tp")
def exception_returned_to_user_tp() -> Exception:
    return Exception("test")

@app.route("/exception_returned_to_user_tn")
def exception_returned_to_user_tn() -> str:
    return "test"
