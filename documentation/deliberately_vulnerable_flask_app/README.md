# Overview
This is a trivially vulnerable Flask application, meant as a playground for
testing Pysa

# Setup
1. Setup a virtual environment (if you haven't already):
```bash
python -m venv ../env
source ../env/bin/activate
```

2. Install pyre-check
```bash
pip install pyre-check
```

3. Install dependencies of the flask app
```bash
pip install -r requirements.txt
```

4. Create the .pyre_configuration file:
```bash
pyre init
```

5. Run the analysis:
```bash
pyre analyze --no-verify
```

Instead of steps 3 and 4, you may run `. setup.sh` which does the same but
generates a slightly different `.pyre_configuration` and copies the
integration test scripts to the folder.
