GHCOPTS=+RTS -M256m -RTS -fglasgow-exts

all: asn1tests

clean:
	rm -f *.o *.hi asn1tests

depend:
	ghc -M $(GHC_OPTS) *.lhs *.hs

asn1tests: ASN1Tests.hs ASN1Parser.hs
	ghc --make -o asn1tests $(GHCOPTS) ASN1Tests.hs
