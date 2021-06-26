# Pysa Github Action

Run pysa on the repository in Github Actions and create a SARIF File.

## Dependencies

The action requires a python setup with the python version > 3.5

You may create a python setup with the latest python version by using the
following code. Do this only if you haven't setup python in your workflow.

```yml
- name: Setup python environment
  uses: actions/setup-python@v2

```
## Usage

Include the action in your workflow by including the following piece of code:

```yml
- name: Runs pysa
  uses: facebook/pyre-check@master
```

Pysa tries to find a `.pyre_configuration` file containing the configuration,
if there is no such file, it generates a new file by executing `pyre init`.

Optionally, you may pass the build-type argument to use the nightly version
instead of the stable version of pysa as:

```yml
- name: Runs pysa
  uses: facebook/pyre-check@master
  with:
    build-type: nightly
```

When the parameter is not specified, the stable version of pysa is used.

## SARIF File

After you run the pysa action,the action creates a `sarif.json` file which
contains the results of the analysis performed by pysa.

You may expose the SARIF File to Github to get it report on the Security tab or on a pull
request by including the following steps as well in your workflow:

```yml
- name: Expose SARIF Results
  uses: actions/upload-artifact@v2
  with:
    name: SARIF Results
    path: sarif.json

- name: Upload SARIF Results
  uses: github/codeql-action/upload-sarif@v1
  with:
    sarif_file: sarif.json
```

## Action

The action does the following:
- Installs pysa by installing pyre-check
- Creates a `.pyre_configuration` file if no such file exists in the current working directory
- Runs pysa
- Convert the results into SARIF format and save it to `sarif.json``
