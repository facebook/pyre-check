# Instructions

Currently this is split into front- and backend. The backend is a flask server in python and the
frontend is react running in NodeJS.

I'm assuming all commands start from the `sapp_ui` directory in these instructions.

## Backend
```
$ python3 -m venv venv
$ . venv/bin/activate
$ pip install -r requirements.txt
$ python backend/generate_data.py
$ python -m backend.application [--interactive-graphql]
```

Query for all runs:
```
{
  allRuns {
    edges {
      node {
        runId
      }
    }
  }
}
```

Query for all issues of a specific run:
```
{
  allIssues(run:1) {
    edges {
      node {
        id
        run
      }
    }
  }
}
```

## Frontend
You have to install NodeJS with `brew install nodejs`.

```
$ npm install
$ npm start
```
