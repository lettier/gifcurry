# (C) 2016 David Lettier
# lettier.com

.RECIPEPREFIX != ps

_GIFCURRY_VERSION="6.0.1.0"
_STACK=stack --allow-different-user
_GHC_VERSION=`$(_STACK) ghc -- --version | sed 's|The Glorious Glasgow Haskell Compilation System, version ||g'`
_STACK_PATH_LOCAL_BIN=`$(_STACK) path --local-bin`
_STACK_GHC_EXE=`$(_STACK) path --compiler-exe`
_STACK_GHC_BIN=`$(_STACK) path --compiler-bin`
_STACK_PATHS=$(_STACK_PATH_LOCAL_BIN):$(_STACK_GHC_BIN)
_CABAL=env PATH=$(_STACK_PATHS):$(PATH) $(_STACK_PATH_LOCAL_BIN)/cabal
_CABAL_SANDBOX_DIR=".cabal-sandbox"
_METAINFO_DIR="$(_CABAL_SANDBOX_DIR)/share/metainfo"
_APPLICATIONS_DESKTOP_DIR="$(_CABAL_SANDBOX_DIR)/share/applications"
_ICONS_HICOLOR_SCALABLE_APPS_DIR="$(_CABAL_SANDBOX_DIR)/share/icons/hicolor/scalable/apps"
_PACKAGING_LINUX_COMMON_DIR="./packaging/linux/common"
_GIFCURRY_LINUX_PACKAGE_DIR="gifcurry-linux-$(_GIFCURRY_VERSION)"

export PATH := $(PATH):$(_STACK_PATH_LOCAL_BIN)

all: setup update sandbox_clean alex happy haskell_gi gtk2hs_buildtools install_dependencies configure build cabal_install

none:

setup: none
  $(_STACK) setup && $(_STACK) update && \
  $(_STACK) install Cabal && \
  $(_STACK) install cabal-install

alex: setup
  $(_STACK) install alex

happy: setup
  $(_STACK) install happy

haskell_gi: setup
  $(_STACK) install haskell-gi

gtk2hs_buildtools: setup
  $(_STACK) install gtk2hs-buildtools

sandbox: setup
  $(_CABAL) sandbox init

clean: setup
  $(_CABAL) clean

check: setup
  $(_CABAL) check

sandbox_clean: setup
  $(_CABAL) sandbox init && $(_CABAL) sandbox delete && $(_CABAL) sandbox init

update: sandbox
  $(_CABAL) --require-sandbox update

install_dependencies: sandbox
  $(_CABAL) --require-sandbox install -j -w $(_STACK_GHC_EXE) --only-dependencies

configure: sandbox
  $(_CABAL) --require-sandbox configure -w $(_STACK_GHC_EXE)

appdata_xml: sandbox
  mkdir -p $(_METAINFO_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.gifcurry.appdata.xml $(_METAINFO_DIR)/

applications_desktop: appdata_xml
  mkdir -p $(_APPLICATIONS_DESKTOP_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.gifcurry.desktop $(_APPLICATIONS_DESKTOP_DIR)/

icons_hicolor_scalable_apps: applications_desktop
  mkdir -p $(_ICONS_HICOLOR_SCALABLE_APPS_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/com.lettier.gifcurry.svg $(_ICONS_HICOLOR_SCALABLE_APPS_DIR)/

build: configure
  $(_CABAL) --require-sandbox build -j

cabal_install: appdata_xml applications_desktop icons_hicolor_scalable_apps build
  $(_CABAL) --require-sandbox install -j -w $(_STACK_GHC_EXE) --enable-relocatable

package_cabal_sandbox_for_linux: cabal_install
  rm -rf "._gifcurry_trash_" && \
  mkdir -p "._gifcurry_trash_" && \
  mkdir -p $(_GIFCURRY_LINUX_PACKAGE_DIR) && \
  touch "$(_GIFCURRY_LINUX_PACKAGE_DIR).tar.gz" && \
  mv "$(_GIFCURRY_LINUX_PACKAGE_DIR).tar.gz" "._gifcurry_trash_/" && \
  mv $(_GIFCURRY_LINUX_PACKAGE_DIR) "._gifcurry_trash_" && \
  mkdir -p $(_GIFCURRY_LINUX_PACKAGE_DIR) && \
  cp -R "$(_CABAL_SANDBOX_DIR)/." $(_GIFCURRY_LINUX_PACKAGE_DIR) && \
  find "$(_GIFCURRY_LINUX_PACKAGE_DIR)/share/x86_64-linux-ghc-$(_GHC_VERSION)/" -mindepth 1 -maxdepth 1 -type d \
    -not -path '*Gifcurry*' -exec mv {} "._gifcurry_trash_/" \; && \
  find "$(_GIFCURRY_LINUX_PACKAGE_DIR)/lib/x86_64-linux-ghc-$(_GHC_VERSION)/" -mindepth 1 -maxdepth 1 -type d \
    -exec mv {} "._gifcurry_trash_/" \; && \
  find "$(_GIFCURRY_LINUX_PACKAGE_DIR)/bin/" -type f -not -name '*gifcurry*' -exec mv {} "._gifcurry_trash_/" \; && \
  find "$(_GIFCURRY_LINUX_PACKAGE_DIR)/" -mindepth 1 -maxdepth 1 -type d -not -path '*bin*' -not -path '*lib*' -not -path '*share*' \
    -exec mv {} "._gifcurry_trash_/" \; && \
  find "$(_GIFCURRY_LINUX_PACKAGE_DIR)/" -mindepth 1 -maxdepth 1 -type f -not -path '*bin*' -not -path '*lib*' -not -path '*share*' \
    -exec mv {} "._gifcurry_trash_/" \; && \
  tar -zcvf "$(_GIFCURRY_LINUX_PACKAGE_DIR).tar.gz" $(_GIFCURRY_LINUX_PACKAGE_DIR)

release: check build
  $(_CABAL) sdist

run_gui: cabal_install
  ./.cabal-sandbox/bin/gifcurry_gui

run_cli: cabal_install
  ./.cabal-sandbox/bin/gifcurry_cli $(CLI_ARGS)

build_docs: setup
  $(_CABAL) haddock --hyperlink-source \
  --html-location='http://hackage.haskell.org/package/Gifcurry/docs' \
  --contents-location='http://hackage.haskell.org/package/Gifcurry' && \
  mkdir -p ./haddock && \
  cp -R ./dist/doc/html/Gifcurry/ ./haddock/Gifcurry-$(_GIFCURRY_VERSION)-docs && \
  cd ./haddock && \
  tar --format=ustar -cvf ./Gifcurry-$(_GIFCURRY_VERSION)-docs.tar Gifcurry-$(_GIFCURRY_VERSION)-docs
