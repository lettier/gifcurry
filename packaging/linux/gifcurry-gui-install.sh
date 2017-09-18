#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

GIFCURRY_VERSION="2.2.0.0"
GIFCURRY_PACKAGING_LINUX="https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux"

echo "Installing the Gifcurry GUI...\n"
cd "$HOME"
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"
cd "$HOME/.local/bin"
wget "https://github.com/lettier/gifcurry/releases/download/$GIFCURRY_VERSION/gifcurry-linux-app-image-$GIFCURRY_VERSION"
chmod a+x "gifcurry-linux-app-image-$GIFCURRY_VERSION"
cd "$HOME/.icons"
wget "$GIFCURRY_PACKAGING_LINUX/gifcurry.png" -O "gifcurry.png"
cd "$HOME/.local/share/applications"
wget "$GIFCURRY_PACKAGING_LINUX/gifcurry.desktop" -O "gifcurry.desktop"
echo "Exec=$HOME/.local/bin/gifcurry-linux-app-image-$GIFCURRY_VERSION" >> "gifcurry.desktop"
cd "$HOME"
touch ".profile"
echo "\nexport PATH=\"\$PATH:\$HOME/.local/bin\"" >> ".profile"
echo "\nGifcurry GUI installed."
