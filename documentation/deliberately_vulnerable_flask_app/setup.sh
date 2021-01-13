# Source this file to run it:
# . ./setup.sh

# This file should ONLY include standard setup steps which would be used by a
# real Flask project. Do not add anything Pysa/Pyre specific.

python3 -m venv venv
. venv/bin/activate
pip install Flask
rm ../.pyre_configuration
