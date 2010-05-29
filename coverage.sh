#!/bin/bash
bin="LanguageASN1Testsuite"
set -e
./rebuild.sh
rm -f ./$bin.tix
./$bin
hpc report $bin
hpc markup $bin
