#!/usr/bin/env bash
git pull --quiet
LAST_TAGGED_BUILD_NUMBER=$(git tag | grep "$1\-" | sort --reverse | head -n 1 | cut -d"-" -f 2)
LAST_BUILD_NUMBER=${LAST_TAGGED_BUILD_NUMBER:-0}
NEXT_BUILD_NUMBER=$(($LAST_BUILD_NUMBER+1))
echo $NEXT_BUILD_NUMBER
