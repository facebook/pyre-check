# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import os


if __name__ == "__main__":
    try:
        os.remove("database.sqlite3")
    except IOError:
        pass

    from models import engine, session, Base, Run, Issue

    Base.metadata.create_all(bind=engine)

    session.add(Run(run_id=0))
    session.add(Run(run_id=1))

    session.add(
        Issue(issue_id=0, source="UserControlled", sink="RemoteCodeExecution", run=1)
    )
    session.add(Issue(issue_id=1, source="UserControlled", sink="Logging", run=1))
    session.add(Issue(issue_id=2, source="UserControlled", sink="SqlInjection", run=1))
    session.add(Issue(issue_id=3, source="Filesystem", sink="ReturnedToUser", run=0))

    session.commit()
