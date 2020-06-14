ASN.1 definitions parser for Haskell.
====================

This parser covers pretty large subset of standards ITU-T Rec. X.680 -- X.683 and ISO/IEC 8824-1 -- 8824-4

This is unfinished work, so beware - there might be dragons!

Supplied binary `dump-asn1-ast` could be used to test parser code:
```
dump-asn1-ast LDAP.asn1
```

Use `stack test` or `stack test --coverage` to run comprehensive test suite 

Implementation status
=====================

- X.680-0207: DONE, medium test coverage. Restricted character strings
            are done sloppily

- X.681-0207: DONE, poor test coverage. "WITH SYNTAX" is not implemented.

- X.682-0207: partially done, completeness not evaluated

- X.683-0207: partially done, completeness not evaluated

- Tests: partially done, see comments in Testsuite.hs for status

Test code coverage:
```
 88% expressions used (4682/5279)
 60% boolean coverage (3/5)
      60% guards (3/5), 2 always True
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
 42% alternatives used (29/68)
 33% local declarations used (32/96)
 28% top-level declarations used (271/958)
```

Parser uses information about types when it is immediately available.
For example, when parsing value assignments ("var SomeType ::= val"),
parser will accept only valid values of type "SomeType". Type information
is also used when parsing "DEFAULT value" constructs and constraints.

Other than that, parser performs no validation/semantical analysis. In particular:
* Values of enumerated/choice/sequences are not checked against
  respective types. 
* References to types are not followed to find out the real type
* Associates sequence types of REAL, EMBEDDED-PDV and others are not
  taken into account
* Type constraints are not taken into account when parsing values

When no typing context is available, some values could not be
unambigously attributed to particular type. In this case, parser will
produce so-called ambiguous values:
  * `SomeNumber Double` -- for values that could be either `Integer` or `Real`
  * `SomeNamedValueList ComponentValueList` -- for `SequenceRealValue`,
    `SequenceValue`, `SequenceOfValue` (named values), `SetValue`,
    `SetOfValue` (named values) 
  * `SomeValueList [Value]` -- for `IdentifierListBitString`,
    `SequenceOfValue` (values only), `SetOfValue` (values only)
  * `SomeIdentifiedValue Identifier` -- for `Integer` or `Enumerated`
  * `SomeOIDLikeValue OID` -- for `OID` or `RELATIVE-OID`

Even when module does not import external types, those values are not
resolved to their more definitive forms. In order to do so, one would
probably have to do a multi-pass parsing.

Implementation notes
====================
There are 24 ASN.1 types (`BuiltinType` + `ReferencedType` in X.680
terminology), represented by the `BuiltinType` Haskell datatype.

Some of the ASN1 types have several distinct variants, which is why
`BuiltinType` has more than 24 constructors.

Parsing of the ASN1 type declaration is handled by the parser
`theType`.

For each type there is a single value parser, used in contexts when
type information is easily available -- for example, when parsing
right side of `ValueAssignment` one should use parser for values of the
type of the left-hand side (see `valueOfType`).

X.68[0-4] have several places that go like this: "here is non-terminal
with several alternatives. However, alternatives 1 and 4 are only
applicable when <condition>, and alternatives 2 and 3 - when <other condition>, otherwise,
alternatives 5 and 6 are used". All those places have to be re-checked
with care because I am not 100% sure that I have not missed one of
the conditions.

Somethimes, I decided to postpone implementation of these complex
rules. Those and other major TODOs are marked in source code with `TODO`.
