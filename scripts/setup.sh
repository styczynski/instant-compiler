#!/bin/bash

DOWNLOADS_DIR="$HOME/Downloads"

GHC_VERSION="8.0.1"  
ARCHITECTURE="x86_64"  
# for 32 bit ARCHITECTURE="i386"      
PLATFORM="deb8-linux"  
GHC_DIST_FILENAME="ghc-$GHC_VERSION-$ARCHITECTURE-$PLATFORM.tar.xz"

CABAL_VERSION="1.24.0.0"
CABAL_DIST_FILENAME="Cabal-$CABAL_VERSION.tar.gz"

CABAL_INSTALL_VERSION="1.24.0.0"
CABAL_INSTALL_DIST_FILENAME="cabal-install-$CABAL_INSTALL_VERSION.tar.gz"


# get distr  
cd $DOWNLOADS_DIR
GHC_DIST_URL="https://www.haskell.org/ghc/dist/$GHC_VERSION/$GHC_DIST_FILENAME"
curl -L -O $GHC_DIST_URL  
tar xvfJ $GHC_DIST_FILENAME  
cd ghc-$GHC_VERSION  

# install to  
mkdir $HOME/Development/bin/ghc-$GHC_VERSION  
# or choose another path

./configure --prefix=$HOME/Development/bin/ghc-$GHC_VERSION  

make install

# symbol links  
cd $HOME/Development/bin
rm -f ghc
ln -s `pwd`/ghc-$GHC_VERSION ghc  

# add $HOME/Development/bin/ghc to $PATH  
# add this line to ~/.profile  
export GHC_HOME=$HOME/Development/bin/ghc  
export PATH=$GHC_HOME/bin:${PATH}

# to use updated path without log off
source ~/.profile

# remove temporary files  
cd $DOWNLOADS_DIR  
rm -rfv ghc-$GHC_VERSION*
