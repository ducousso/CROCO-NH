#!/bin/bash

# GIT code update
#-----------------
SOURCE_ROMS=$HOME/GIT/croco/OCEAN
echo
echo "SOURCE_ROMS="$SOURCE_ROMS
echo 
echo "PROCESS GIT UPDATE"
cd  $SOURCE_ROMS/..
/usr/bin/git checkout master
cd -

# Get revision of sources
#-------------------------
./gitinfo.sh $SOURCE_ROMS > gitinfos
