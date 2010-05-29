#!/bin/sh
set -e

runhaskell ./Setup.hs configure --user --bindir=`pwd` --libdir=`pwd`/lib --docdir=`pwd` -ftest
runhaskell ./Setup.hs build
runhaskell ./Setup.hs copy
