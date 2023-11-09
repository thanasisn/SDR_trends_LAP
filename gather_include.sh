#!/bin/bash
## created on 2023-11-09

#### enter description here

## Repo must only one level deep no rucursive folders

## Source file with paths in REPO
SOURCE="$1"
## Repository of include files
REPO="$2"
## Location to copy include files
TARGET="$3"

[ ! -f "$SOURCE" ] && echo && echo "NOT A FILE $SOURCE"   && exit 1
[ ! -d "$REPO"   ] && echo && echo "NOT A FOLDER $REPO"   && exit 1
# [ ! -d "$TARGET" ] && echo && echo "NOT A FOLDER $TARGET" && exit 1

mkdir -p "$TARGET/$REPO"

## get included paths
sed -e :a -re 's/<!--.*?-->//g;/<!--/N;//ba' "$SOURCE" |
    sed '/^[[:blank:]]*#/d;s/#.*//' |
    grep "$REPO/"                   |
    sed 's/.*\('"$REPO"'\)/\1/'     |
    sed 's/\..*//'                  |
    sed 's/}.*//'                   |
    sort -u                         | 
    while read line; do 
        echo "Processing: $line"
        # ls "$line"*
        cp -v "$line"* "$TARGET/$REPO"
    done

cp -vu  "$SOURCE"                 "$TARGET"
cp -vu  "./manualreferences.bib"  "$TARGET"
cp -vur "./Definitions"           "$TARGET"

exit 0 
