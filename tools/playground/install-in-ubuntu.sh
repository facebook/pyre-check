#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Script to install pyre playground on a fresh ubuntu server.

set -e
set -x

sudo apt-get update

sudo apt-get install -y \
  build-essential \
  python3.8 \
  python3.8-venv \
  python3-dev \
  watchman \
  git \
  nginx

# Grab the code and link it to ~/playground
git clone https://github.com/facebook/pyre-check.git
ln -s pyre-check/tools/playground/ ~/playground
cd ~/playground

# Set up the environment. You can verify the python app works with:
#   python application.py --debug
python3 -m venv ~/play-env
source ~/play-env/bin/activate
pip install -r requirements.txt

# Start the service. You can verify it is up with:
#   curl "http://0.0.0.0:5000/check?input=reveal_type(1)"
sudo cp system/playground.service /etc/systemd/system/
sudo systemctl start playground
sudo systemctl enable playground

# Set up nginx proxy. We need to remove the default config for
# it to take effect. You can verify it is up with:
#    curl "http://0.0.0.0:80/check?input=reveal_type(1)"
sudo cp system/playground.nginx /etc/nginx/sites-available/playground
sudo ln -s /etc/nginx/sites-{available,enabled}/playground
sudo rm /etc/nginx/sites-enabled/default
sudo systemctl restart nginx

# verify that it is up
sleep 5
curl "http://0.0.0.0:80/check?input=reveal_type(1)"
echo "Server is up!"
