#!/bin/bash

if [ -x "$(command -v git)" ]; then
  exit 0
fi

# Stack installation script, adapted from:
# https://github.com/yantonov/install-ghc/blob/af0b968b9e8423efb152ccec4224821e29317710/ubuntu/install-ghc-ubuntu.md

DOWNLOADS_DIR=$HOME/Downloads
STACK_INSTALL_DIR="$HOME/Development/bin"
STACK_VERSION="2.1.3"  
STACK_ARCHITECTURE="x86_64"  
STACK_PLATFORM="linux"  

mkdir -p "$DOWNLOADS_DIR" > /dev/null 2> /dev/null
mkdir -p "$STACK_INSTALL_DIR" > /dev/null 2> /dev/null

# Check that libgmp is installed. This is the main critical system-level
# dependency of the Haskell environment that may not be present.

function check_lib()
{
    echo "int main(){}" | gcc -o /dev/null -lgmp -x c -
    return $?
}

GMP_OK=false
if (ldconfig -p | grep -q "libgmp.so.10"); then
    GMP_VERSION_POSTFIX=""
    if (check_lib -lgmp); then GMP_OK=true; fi
elif (ldconfig -p | grep -q "libgmpxx.so.4"); then
    GMP_VERSION_POSTFIX="-gmp4"
    if (check_lib -lgmp); then GMP_OK=true; fi
fi


if [ $GMP_OK = false ]; then
    echo >&2 "Haskell requires the GNU multi-precision library (with headers)"
    echo >&2 "in version 4 or 10, but neither can be found. Try"
    echo >&2
    echo >&2 "$ sudo apt-get install libgmp-dev"
    echo >&2
    exit 1
fi

STACK_DIST_FILENAME="stack-$STACK_VERSION-$STACK_PLATFORM-$STACK_ARCHITECTURE.tar.gz"  
STACK_DIST_UNZIPPED_DIR="stack-$STACK_VERSION-$STACK_PLATFORM-$STACK_ARCHITECTURE"
STACK_DIST_URL="https://www.stackage.org/stack/$STACK_PLATFORM-$STACK_ARCHITECTURE"
STACK_TARGET_DIR="stack-$STACK_VERSION"

cd $DOWNLOADS_DIR

curl -L -o $STACK_DIST_FILENAME $STACK_DIST_URL  
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
rm -rfd $STACK_INSTALL_DIR/$STACK_TARGET_DIR  
echo mv $STACK_DIST_UNZIPPED_DIR $STACK_INSTALL_DIR/$STACK_TARGET_DIR
mv $STACK_DIST_UNZIPPED_DIR $STACK_INSTALL_DIR/$STACK_TARGET_DIR

cd $STACK_INSTALL_DIR  

# sym link
rm -rfd stack  
ln -s `pwd`/$STACK_TARGET_DIR stack  

# add to PATH environment  
STACK_HOME=$HOME/Development/bin/stack  
PATH=$STACK_HOME:$PATH

# clean up
cd $DOWNLOADS_DIR
rm -rfd stack-$STACK_VERSION*

# install ghc
d=$(pwd)
echo "Now call stack at ${d}"
./stack setup --reinstall || true

# link stack
ln -s "$HOME/Development/bin/stack/stack" ./stack 2> /dev/null > /dev/null
