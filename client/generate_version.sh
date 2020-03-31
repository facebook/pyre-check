#!/bin/sh

sed -e "s/internal-dev/fb-dotslash $(hg whereami)/" version.py > "$INSTALL_DIR"/version.py
