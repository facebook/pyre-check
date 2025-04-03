# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import subprocess
import venv
import os
import shutil
import tempfile
from pathlib import Path
from ...api.connection import PyreConnection
from ...api.query import get_invalid_taint_models

cur_dir = Path(__file__).parent
envpath = cur_dir / ".env"
requirements_path = cur_dir / "verify_models" / "requirements.txt"
activate = envpath / "bin" / "activate"
command = f"pip install -r {requirements_path}"

if os.path.exists(envpath):
  shutil.rmtree(envpath)
os.mkdir(envpath)
venv.create(envpath, with_pip=True)


subprocess.check_call(["bash", "-c", f"source {activate}; {command}"])

pyre_connection = PyreConnection(cur_dir / "verify_models")

temp_file = tempfile.TemporaryFile(prefix=".pyre_configuration", suffix=".local", dir=cur_dir)
temp_file.write(b'{"source_directories": ["verify_models"]}')

print(get_invalid_taint_models(pyre_connection))
shutil.rmtree(envpath)
