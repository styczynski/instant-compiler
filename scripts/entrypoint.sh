#!/bin/bash

case "$1" in
  "jvm")
    ./insc_jvm ${@:2}
    ;;
  "llvm")
    ./insc_llvm ${@:2}
    ;;
  *)
    echo "You have failed to specify what to do correctly: Please use jvm <options> or llvm <options>."
    exit 1
    ;;
esac
