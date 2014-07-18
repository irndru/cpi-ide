#!/bin/bash
apt-get update
apt-get -y install ghc 
apt-get -y install libncurses5-dev 
apt-get -y install happy 
apt-get -y install alex 
apt-get -y install libcairo2-dev 
apt-get -y install libpango1.0-dev 
apt-get -y install gtk+-2.0 
apt-get -y install glib-2.0 
apt-get -y install gsl-bin 
apt-get -y install gsl-ref-html 
apt-get -y install libgsl0-dev 
apt-get -y install liblapack-dev 
apt-get -y install libghc-gtksourceview2-dev 
apt-get -y install cabal-install 
cabal update
cabal install gtk2hs-buildtools --global
cabal install text hledger gtk MissingH hmatrix data-accessor colour 
cabal install gtksourceview2-0.12.3.1 --reinstall
ghc-pkg unregister Chart
cabal install chart-0.14 
