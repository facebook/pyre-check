# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import logging
import os
from typing import Optional

import sqlalchemy
from flask import Flask, send_from_directory
from flask_cors import CORS
from flask_graphql import GraphQLView
from sqlalchemy.orm import Session, scoped_session, sessionmaker

from .models import Base
from .schema import schema


application = Flask(
    __name__,
    static_folder=os.path.join(os.path.dirname(__file__), "..", "frontend", "build"),
)
# TODO(T58070180): we should remove the cross origin policy.
CORS(application)
application.debug = True

session: Optional[Session] = None


@application.teardown_appcontext
def shutdown_session(exception=None):
    if session is not None:
        session.remove()


@application.route("/", defaults={"path": ""})
@application.route("/<path:path>")
def serve(path):
    if path != "" and os.path.exists(application.static_folder + "/" + path):
        return send_from_directory(application.static_folder, path)
    else:
        logging.error(application.static_folder)
        return send_from_directory(application.static_folder, "index.html")


def start_app(database):
    engine = sqlalchemy.create_engine(
        sqlalchemy.engine.url.URL("sqlite", database=database.dbname),
        echo=False,
        poolclass=None,
    )
    session = scoped_session(sessionmaker(bind=engine))
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

    application.run()
