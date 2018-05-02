#!/usr/bin/env bash

# (C) 2017 David Lettier
# lettier.com

GIFCURRY_VERSION="3.0.0.2"
GIFCURRY_RELEASES_DOWNLOAD="https://github.com/lettier/gifcurry/releases/download/$GIFCURRY_VERSION"
GIFCURRY_PACKAGING_LINUX_COMMON="https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/common"
GIFCURRY_APP_IMAGE="gifcurry-$GIFCURRY_VERSION-x86_64.AppImage"

echo -e "Installing the Gifcurry AppImage...\n"
cd "$HOME"
mkdir -p "$HOME/.local/bin" "$HOME/.local/share/applications" "$HOME/.icons"
cd "$HOME/.local/bin"
wget "$GIFCURRY_RELEASES_DOWNLOAD/$GIFCURRY_APP_IMAGE" -O "$GIFCURRY_APP_IMAGE"
chmod a+x "$GIFCURRY_APP_IMAGE"
cd "$HOME/.icons"
wget "$GIFCURRY_PACKAGING_LINUX_COMMON/com.lettier.gifcurry.svg" -O "com.lettier.gifcurry.svg"
cd "$HOME/.local/share/applications"
wget "$GIFCURRY_PACKAGING_LINUX_COMMON/com.lettier.gifcurry.desktop" -O "com.lettier.gifcurry.desktop"
echo -e "`sed '$ d' com.lettier.gifcurry.desktop`\nExec=$HOME/.local/bin/$GIFCURRY_APP_IMAGE" > "com.lettier.gifcurry.desktop"
cd "$HOME"
touch ".profile"
echo -e "\nexport PATH=\"\$PATH:\$HOME/.local/bin\"" >> ".profile"
echo -e "\nGifcurry AppImage installed."
