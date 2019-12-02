#!/bin/bash
if ! [ -x "$(command -v git)" ]; then
  echo "$HOME/Development/bin/stack/stack"
else
  echo "$(which stack)"
fi