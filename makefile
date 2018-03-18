# (C) 2016 David Lettier
# lettier.com

.RECIPEPREFIX != ps

STACK=stack --allow-different-user
STACK_PATH_LOCAL_BIN=`$(STACK) path --local-bin`
STACK_GHC_EXE=`$(STACK) path --compiler-exe`
STACK_GHC_BIN=`$(STACK) path --compiler-bin`
STACK_PATHS=$(STACK_PATH_LOCAL_BIN):$(STACK_GHC_BIN)
CABAL=env PATH=$(PATH):$(STACK_PATHS) $(STACK_PATH_LOCAL_BIN)/cabal
CABAL_SANDBOX_DIR=".cabal-sandbox"
_APPLICATIONS_DESKTOP_DIR="$(CABAL_SANDBOX_DIR)/share/applications"
_ICONS_HICOLOR_SCALABLE_APPS_DIR="$(CABAL_SANDBOX_DIR)/share/icons/hicolor/scalable/apps"
_PACKAGING_LINUX_COMMON_DIR="./packaging/linux/common"
VERSION='3.0.0.1'

export PATH := $(PATH):$(STACK_PATH_LOCAL_BIN)

all: setup update sandbox_clean clean alex happy haskell_gi gtk2hs_buildtools install_dependencies configure build cabal_install

setup:
  $(STACK) setup && $(STACK) update && \
  $(STACK) install Cabal && \
  $(STACK) install cabal-install

alex: setup
  $(STACK) install alex

happy: setup
  $(STACK) install happy

haskell_gi: setup
  $(STACK) install haskell-gi

gtk2hs_buildtools: setup
  $(STACK) install gtk2hs-buildtools

sandbox: setup
  $(CABAL) sandbox init

clean: setup
  $(CABAL) clean

check: setup
  $(CABAL) check

sandbox_clean: setup
  $(CABAL) sandbox init && $(CABAL) sandbox delete && $(CABAL) sandbox init

update: sandbox
  $(CABAL) --require-sandbox update

install_dependencies: sandbox
  $(CABAL) --require-sandbox install -j -w $(STACK_GHC_EXE) --only-dependencies

configure: sandbox
  $(CABAL) --require-sandbox configure -w $(STACK_GHC_EXE)

applications_desktop: sandbox
  mkdir -p $(_APPLICATIONS_DESKTOP_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/gifcurry.desktop $(_APPLICATIONS_DESKTOP_DIR)/

icons_hicolor_scalable_apps: applications_desktop
  mkdir -p $(_ICONS_HICOLOR_SCALABLE_APPS_DIR) && \
  cp $(_PACKAGING_LINUX_COMMON_DIR)/gifcurry-icon.svg $(_ICONS_HICOLOR_SCALABLE_APPS_DIR)/

build: configure
  $(CABAL) --require-sandbox build -j

cabal_install: applications_desktop icons_hicolor_scalable_apps build
  $(CABAL) --require-sandbox install -j -w $(STACK_GHC_EXE) --enable-relocatable

release: check build
  $(CABAL) sdist

run_gui: cabal_install
  ./.cabal-sandbox/bin/gifcurry_gui

run_cli: cabal_install
  ./.cabal-sandbox/bin/gifcurry_cli $(CLI_ARGS)

build_docs: setup
  $(CABAL) haddock --hyperlink-source \
  --html-location='http://hackage.haskell.org/package/Gifcurry/docs' \
  --contents-location='http://hackage.haskell.org/package/Gifcurry' && \
  mkdir -p ./haddock && \
  cp -R ./dist/doc/html/Gifcurry/ ./haddock/Gifcurry-$(VERSION)-docs && \
  cd ./haddock && \
  tar --format=ustar -cvf ./Gifcurry-$(VERSION)-docs.tar Gifcurry-$(VERSION)-docs
