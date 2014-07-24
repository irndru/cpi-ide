#!/bin/bash
set -e
sudo apt-get update
sudo apt-get install ghc libncurses5-dev happy alex libcairo2-dev libpango1.0-dev gtk+-3.0 glib-2.0 gsl-bin gsl-ref-html libgsl0-dev liblapack-dev libgtksourceview-3.0 libgtksourceview-3.0-dev cabal-install
cabal update
sudo cabal install --global gtk2hs-buildtools text hledger gtk3 MissingH hmatrix data-accessor colour Chart Chart-cairo hmatrix-gsl gtksourceview3
