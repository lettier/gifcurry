# David Lettier (C) 2016
# http://www.lettier.com/

.RECIPEPREFIX != ps
VERSION='2.1.1.0'

all: update alex_happy gtk2hs sandbox clean install_dependencies configure build install

update:
  cabal update

clean:
  cabal clean

deep_clean:
  cabal sandbox delete && cabal clean

sandbox:
  cabal sandbox init

configure:
  cabal configure

alex_happy: update
  cabal install alex happy -j

gtk2hs: update
  cabal install gtk2hs-buildtools -j

install_dependencies: update sandbox
  cabal install -j --dependencies-only

build: sandbox clean configure
  cabal build -j

install: build
  cabal install -j --enable-relocatable

run_gui:
  ./.cabal-sandbox/bin/gifcurry_gui

run_cli:
  ./.cabal-sandbox/bin/gifcurry_cli $(CLI_ARGS)

check:
  cabal check

release: check build
  cabal sdist

build_docs:
  cabal haddock --hyperlink-source \
  --html-location='http://hackage.haskell.org/package/Gifcurry/docs' \
  --contents-location='http://hackage.haskell.org/package/Gifcurry' && \
  cp -R ./dist/doc/html/Gifcurry/ Gifcurry-$(VERSION)-docs && \
  tar --format=ustar -zcvf 'Gifcurry-$(VERSION)-docs.tar.gz' 'Gifcurry-$(VERSION)-docs'
