#!/bin/sh
for f in samples/*.asn* ; do
  ./dump-asn1-ast $f > samples/output/$(basename $f)
done
