# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import argparse
import os

from flask import Flask, send_from_directory
from flask_cors import CORS
from flask_graphql import GraphQLView

from .models import session
from .schema import schema


application = Flask(
    __name__,
    static_folder=os.path.join(os.path.dirname(__file__), "..", "frontend", "build"),
)
# TODO(T58070180): we should remove the cross origin policy.
CORS(application)
application.debug = True


@application.teardown_appcontext
def shutdown_session(exception=None):
    session.remove()


@application.route("/", defaults={"path": ""})
@application.route("/<path:path>")
def serve(path):
    if path != "" and os.path.exists(application.static_folder + "/" + path):
        return send_from_directory(application.static_folder, path)
    else:
        print(application.static_folder)
        return send_from_directory(application.static_folder, "index.html")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--interactive-graphql", action="store_true")
    arguments = parser.parse_args()

    application.add_url_rule(
        "/graphql",
        view_func=GraphQLView.as_view(
            "graphql", schema=schema, graphiql=arguments.interactive_graphql
        ),
    )

    application.run()
