#!/bin/bash
apt-get update
apt-get install ghc libncurses5-dev happy alex libcairo2-dev libpango1.0-dev gtk+-2.0 glib-2.0 gsl-bin gsl-ref-html libgsl0-dev liblapack -dev libghc-gtksourceview2-dev cabal-install 
cabal update
cabal install gtk2hs-buildtools --global
cabal install text hledger gtk MissingH hmatrix data-accessor colour 
cabal install gtksourceview2-0.12.3.1 --reinstall
ghc-pkg unregister Chart
cabal install chart-0.14 
