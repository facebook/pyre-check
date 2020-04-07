#!/bin/sh

VERSION=$(cd "$SRCDIR" && hg whereami)
sed -e "s/internal-dev/fb-dotslash $VERSION/" version.py > "$INSTALL_DIR"/version.py
