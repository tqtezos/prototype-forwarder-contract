#!/usr/bin/env bash

sha256() {
    shasum -a 256 $1 | cut -d " " -f 1
}

for tarball in $(ls $1/*.tar.gz)
do
    sha256 ${tarball} > ${tarball}.sha256
done
