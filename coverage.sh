#!/bin/bash
bin="LanguageASN1Testsuite"
./rebuild.sh
rm -f ./$bin.tix
./$bin "$@"
hpc report $bin
hpc markup $bin
