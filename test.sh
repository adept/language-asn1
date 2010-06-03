#!/bin/sh
./rebuild.sh
rm TestASN1Parser.tix
for f in samples/*.asn* ; do
  ./TestASN1Parser $f > samples/output/$(basename $f)
done
hpc report TestASN1Parser
