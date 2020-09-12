# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
from typing import Optional

import sqlalchemy
from flask import Flask, send_from_directory
from flask_graphql import GraphQLView
from sqlalchemy.orm import Session, scoped_session, sessionmaker

from .db import DB
from .models import Base
from .schema import schema


logging.basicConfig(
    format="%(asctime)s [%(levelname)s] %(message)s", level=logging.DEBUG
)
LOG = logging.getLogger(__name__)


application = Flask(
    __name__, static_folder=os.path.join(os.path.dirname(__file__), "frontend", "build")
)

session: Optional[Session] = None


@application.teardown_appcontext
def shutdown_session(exception=None):

    if session is not None:
        session.remove()


@application.route("/", defaults={"path": ""})
@application.route("/<path:path>")
def serve(path):
    LOG.info(f"Serving `{path}`...")
    if path != "" and os.path.exists(application.static_folder + "/" + path):
        LOG.info("Found static resource.")
        return send_from_directory(application.static_folder, path)
    else:
        LOG.info("Resource not found. Falling back to `index.html`")
        return send_from_directory(application.static_folder, "index.html")


def start_server(database: DB, debug: bool, static_resources: Optional[str]) -> None:
    engine = sqlalchemy.create_engine(
        sqlalchemy.engine.url.URL("sqlite", database=database.dbname),
        echo=False,
        poolclass=None,
    )
    session = scoped_session(sessionmaker(bind=engine))
    # pyre-ignore
    Base.query = session.query_property()

    application.add_url_rule(
        "/graphql",
        view_func=GraphQLView.as_view(
            "graphql",
            schema=schema,
            graphiql=True,
            get_context=lambda: {"session": session},
        ),
    )
    if static_resources:
        application.static_folder = static_resources
    application.run(debug=debug)
