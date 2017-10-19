# (C) 2016 David Lettier
# lettier.com

.RECIPEPREFIX != ps

STACK=stack --allow-different-user
STACK_PATH_LOCAL_BIN=`$(STACK) path --local-bin`
STACK_GHC_EXE=`$(STACK) path --compiler-exe`
STACK_GHC_BIN=`$(STACK) path --compiler-bin`
STACK_PATHS=$(STACK_PATH_LOCAL_BIN):$(STACK_GHC_BIN)
CABAL=env PATH=$(PATH):$(STACK_PATHS) $(STACK_PATH_LOCAL_BIN)/cabal
VERSION='2.3.0.0'

export PATH := $(PATH):$(STACK_PATH_LOCAL_BIN)

all: setup update sandbox_clean clean alex happy install_dependencies configure build install

setup:
  $(STACK) setup && $(STACK) update && $(STACK) install Cabal && $(STACK) install cabal-install

alex: setup
  $(STACK) install alex

happy: setup
  $(STACK) install happy

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

build: configure
  $(CABAL) --require-sandbox build -j

install: build
  $(CABAL) --require-sandbox install -j -w $(STACK_GHC_EXE) --enable-relocatable

release: check build
  $(CABAL) sdist

run_gui: install
  ./.cabal-sandbox/bin/gifcurry_gui

run_cli: install
  ./.cabal-sandbox/bin/gifcurry_cli $(CLI_ARGS)

build_docs: setup
  $(CABAL) haddock --hyperlink-source \
  --html-location='http://hackage.haskell.org/package/Gifcurry/docs' \
  --contents-location='http://hackage.haskell.org/package/Gifcurry' && \
  cp -R ./dist/doc/html/Gifcurry/ ./docs/Gifcurry-$(VERSION)-docs && \
  cd ./docs && \
  tar --format=ustar -cvf ./Gifcurry-$(VERSION)-docs.tar Gifcurry-$(VERSION)-docs

# Begin Arch Linux Specific
arch_os_build_gifcurry: setup update clean sandbox_clean alex happy arch_os_install_dependencies arch_os_configure arch_os_build

arch_os_install_dependencies: sandbox
  $(CABAL) --require-sandbox install -j -w $(STACK_GHC_EXE) --force-reinstalls --reinstall --only-dependencies

arch_os_configure: sandbox
  $(CABAL) --require-sandbox configure -w $(STACK_GHC_EXE) --prefix=$(PREFIX)

arch_os_build: arch_os_configure
  $(CABAL) --require-sandbox build -j

arch_os_install_gifcurry: arch_os_build
  $(CABAL) --require-sandbox copy --destdir=$(DESTDIR)
# End Arch Linux Specific
