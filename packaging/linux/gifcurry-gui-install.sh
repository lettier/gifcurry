#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

GIFCURRY_VERSION="2.2.0.1"
GIFCURRY_RELEASES_DOWNLOAD="https://github.com/lettier/gifcurry/releases/download/$GIFCURRY_VERSION"
GIFCURRY_PACKAGING_LINUX="https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux"
GIFCURRY_APP_IMAGE="gifcurry-$GIFCURRY_VERSION-x86_64.AppImage"

echo -e "Installing the Gifcurry GUI...\n"
cd "$HOME"
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"
cd "$HOME/.local/bin"
wget "$GIFCURRY_RELEASES_DOWNLOAD/$GIFCURRY_APP_IMAGE" -O "$GIFCURRY_APP_IMAGE"
chmod a+x "$GIFCURRY_APP_IMAGE"
cd "$HOME/.icons"
wget "$GIFCURRY_PACKAGING_LINUX/gifcurry.png" -O "gifcurry.png"
cd "$HOME/.local/share/applications"
wget "$GIFCURRY_PACKAGING_LINUX/gifcurry.desktop" -O "gifcurry.desktop"
echo "Exec=$HOME/.local/bin/$GIFCURRY_APP_IMAGE" >> "gifcurry.desktop"
cd "$HOME"
touch ".profile"
echo -e "\nexport PATH=\"\$PATH:\$HOME/.local/bin\"" >> ".profile"
echo -e "\nGifcurry GUI installed."
