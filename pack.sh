#!/bin/bash

git add . && git reset --hard
git clean -fx
rm -rfd *.tar.gz
tar -czvf ps386038.tar.gz *