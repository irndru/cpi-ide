#!/bin/bash
set -e
apt-get update
apt-get -y install ghc libncurses5-dev happy alex libcairo2-dev libpango1.0-dev gtk+-3.0 glib-2.0 gsl-bin gsl-ref-html libgsl0-dev liblapack-dev libgtksourceview-3.0 libgtksourceview-dev-3.0 cabal-install 
cabal update
cabal install gtk2hs-buildtools --global
cabal install text hledger gtk3 MissingH hmatrix data-accessor colour Chart Chart-cairo hmatrix-gsl gtksourceview3
