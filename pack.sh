#!/bin/bash

git add . && git reset --hard
git clean -fX
rm -rfd *.tar.gz
#tar -czvf ps386038.tar.gz *