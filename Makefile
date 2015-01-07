SHELL := /bin/bash
now   := $(shell date +%Y%m%d%H%M)
cabv  := $(shell grep '^Version' makeci.cabal | awk -F ' ' '{print $$2}')

version := $(cabv).$(now)

all:
	cabal build

clean:
	rm -rf dist/

depends:
	postgresql-9.3

build-depends:
	ghc-7.8.4
	cabal-install-1.22
	happy-1.19.4
	alex-3.1.3
	zlib1g-dev

apt-sources-build:
	ppa:hvr/ghc

setup-build-env:
	cp /opt/ghc/7.8.4/bin/* /usr/bin/
	cp /opt/alex/3.1.3/bin/* /usr/bin/
	cp /opt/happy/1.19.4/bin/* /usr/bin/
	cp /opt/cabal/1.22/bin/* /usr/bin/
	cabal update
	cabal install --force-reinstalls -j --only-dependencies

install:
	cp dist/build/pkgmake/pkgmake /usr/bin/
