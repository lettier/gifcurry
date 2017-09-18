#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

GIFCURRY_VERSION="2.2.0.0"

echo "Installing the Gifcurry GUI..."
cd "$HOME"
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"
cd "$HOME/.local/bin"
wget "https://github.com/lettier/gifcurry/releases/download/$GIFCURRY_VERSION/gifcurry-linux-app-image-$GIFCURRY_VERSION"
chmod a+x "gifcurry-linux-app-image-$GIFCURRY_VERSION"
cd "$HOME/.local/icons"
wget https://github.com/lettier/gifcurry/blob/master/packaging/linux/gifcurry.png
cd "$HOME/.local/share/applications"
wget https://github.com/lettier/gifcurry/blob/master/packaging/linux/gifcurry.desktop
echo "Exec=$HOME/.local/bin/gifcurry-linux-app-image-$GIFCURRY_VERSION" >> "gifcurry.desktop"
cd "$HOME"
touch ".profile"
echo "export PATH=\"$PATH:$HOME/.local/bin/\"" >> ".profile"
echo "Gifcurry GUI installed."
