#!/bin/bash

DOWNLOADS_DIR=$HOME/Downloads

STACK_VERSION="2.5.1"  
STACK_ARCHITECTURE="x86_64"  
STACK_PLATFORM="linux"  
STACK_DIST_FILENAME="stack-$STACK_VERSION-$STACK_PLATFORM-$STACK_ARCHITECTURE.tar.gz"  
STACK_DIST_UNZIPPED_DIR="stack-$STACK_VERSION-$STACK_PLATFORM-$STACK_ARCHITECTURE"
STACK_DIST_URL="https://www.stackage.org/stack/$STACK_PLATFORM-$STACK_ARCHITECTURE"
STACK_INSTALL_DIR="$HOME/Development/bin"
STACK_TARGET_DIR="stack-$STACK_VERSION"


cd $DOWNLOADS_DIR

echo "Download Stack"
curl -L -o $STACK_DIST_FILENAME $STACK_DIST_URL  

echo "Unpack tar Stack release"
tar xvfz $STACK_DIST_FILENAME

# in case if error like this: 
#curl: (77) error setting certificate verify locations: CAfile: 
# /etc/pki/tls/certs/ca-bundle.crt CApath: 
# ...
# create ~/.curlrc file
# and put this lines to it
# capath=/etc/ssl/certs/
# cacert=/etc/ssl/certs/ca-certificates.crt

# move to home development dir
echo "Move Stack to home development dir: rm $STACK_INSTALL_DIR/$STACK_TARGET_DIR"  
rm -rf $STACK_INSTALL_DIR/$STACK_TARGET_DIR  

echo "Move Stack to home development dir: mv $STACK_DIST_UNZIPPED_DIR -> $STACK_INSTALL_DIR/$STACK_TARGET_DIR"
mv $STACK_DIST_UNZIPPED_DIR $STACK_INSTALL_DIR/$STACK_TARGET_DIR

echo "Move Stack to home development dir: cd $STACK_INSTALL_DIR"
cd $STACK_INSTALL_DIR  

# sym link
echo "Sym link Stack: rm stack"
rm -rvf stack  
echo "Sym link Stack: ln stack"
ln -s `pwd`/$STACK_TARGET_DIR stack  

# add to PATH environment  
echo "Add to path environmental variable"
STACK_HOME=$HOME/Development/bin/stack  
PATH=$STACK_HOME:$PATH

# clean up
echo "Clean up"
cd $DOWNLOADS_DIR
rm -rf stack-$STACK_VERSION*
