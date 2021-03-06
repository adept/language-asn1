import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit

import Data.List

import Language.ASN1.Parser as P

main = defaultMain tests

tests = 
  [ 
    -------------
    -- X.681-0207
    -------------
    testGroup "X.680-0207, Abstract Syntax Notation One (ASN.1): Specification of basic notation"
    [ testGroup "X.680-0207, clause 11, Lexical tokens" lexicalTests
    , testGroup "X.680-0207, clause 12, Modules" moduleTests
      -- 13, Referencing type and value definitions
      -- 14 is SKIPPED
      -- 15, Assigning types and values
      -- 16, Definition of types and values
    , testGroup "X.680-0207, clause 17, BOOLEAN" booleanTests
    , testGroup "X.680-0207, clause 18, INTEGER" integerTests    
    , testGroup "X.680-0207, clause 19, ENUMERATED" enumeratedTests
    , testGroup "X.680-0207, clause 20, REAL" realTests
    , testGroup "X.680-0207, clause 21, BIT STRING" bitStringTests
    , testGroup "X.680-0207, clause 22, OCTET STRING" octetStringTests
    , testGroup "X.680-0207, clause 23, NULL" nullTests
    , testGroup "X.680-0207, clause 24, SEQUENCE" sequenceTests
    , testGroup "X.680-0207, clause 25, SEQUENCE-OF" sequenceOfTests
    , testGroup "X.680-0207, clause 26, SET" setTests
    , testGroup "X.680-0207, clause 27, SET-OF" setOfTests
    , testGroup "X.680-0207, clause 28, CHOICE" choiceTests
    , testGroup "X.680-0207, clause 29, CHOICE" selectionTests
      -- 30, Notation for tagged types
    , testGroup "X.680-0207, clause 31, OID" oidTests
    , testGroup "X.680-0207, clause 32, RELATIVE-OID" relativeOIDTests
      -- 33, Notation for the embedded-pdv type
      -- 34, Notation for the external type
    -- TODO: re-enable after charstring implementation ceases to be so primitive
    -- , testGroup "X.680-0207, clause 35, Character string (restricted and unrestricted)" characterStringTests
      -- 36-41 are SKIPPED
      -- 42, Generalized time
      -- 43, Universal time
      -- 44, The object descriptor type
      -- 45, Constrained types
      -- 46, Element set specification
      -- 47, Subtype elements
      -- 48 is SKIPPED
      -- 48, The exception identifier
    ]
    
    -------------
    -- X.681-0207
    -------------
    , testGroup "X.681-0207, Abstract Syntax Notation One (ASN.1): Information object specification"
      [ testGroup "X.681-0207, clause 9, Information object class definition and assignment" classTests
      ]
  ]

-- Helpers
testType val expected = testCase ("Type "++val) $ parseASN1 theType val @?= expected
  
testAmbiguousValue val parser expected expected_amb = 
  testGroup ("Value " ++ val)
    [ tsValue val parser expected
    , gValue val expected_amb
    ]
  where
    tsValue val parser expected = testCase "Type-specific parser" $ parseASN1 parser val @?= expected
    gValue val expected         = testCase "Generic value parser" $ parseASN1 value  val @?= expected
    
testValue val parser expected = testAmbiguousValue val parser expected expected

testAssignment val expected = testCase ("Assignment "++val) $ parseASN1 assignment val @?= expected

-- Expected values for type, value and assignment tests that does not produce anything useful yet.
noType = Just (Type {type_id = Boolean, subtype = Nothing})
noValue = Just (BooleanValue False)
noAssignment = Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = Boolean, subtype = Nothing}, assigned_value = BooleanValue True})
                     
--
-- TESTS                     
--
-- X.680-0207, clause 11, lexical tokens
lexicalTests =
  [ testTypeRef "SomeType" $ Just (TypeReference "SomeType")
  , testTypeRef "Some-Type" $ Just (TypeReference "Some-Type")
  , testTypeRef "SomeType -- Very\n-- important" $ Just (TypeReference "SomeType")
  , testTypeRef "Some-Type /* Even more important */" $ Just (TypeReference "Some-Type")
  , testTypeRef "-- Leading comment --Some-Type -- Trailing comment" $ Just (TypeReference "Some-Type")
  , testInvTypeRef "Some--Type" -- double dash
  , testInvTypeRef "Some-Type-" -- trailing dash
  , testInvTypeRef "-Some-Type" -- leading dash
  , testInvTypeRef "someType"   -- lower case
  , testInvTypeRef "UTCTime"    -- reserved word
  , testIdent "someType" $ Just (Identifier "someType")
  , testIdent "some-Type" $ Just (Identifier "some-Type")
  , testInvIdent "some--Type"   -- double dash
  , testInvIdent "some-Type-"   -- trailing dash
  , testInvIdent "-some-Type"   -- leading dash
  , testInvIdent "SomeType"     -- upper case
  , testReal "0" $ Just 0
  , testReal "10" $ Just 10.0
  , testReal "-10" $ Just (-10.0)
  , testReal "10.1" $ Just 10.1
  , testReal "-10.1" $ Just (-10.1)
  , testReal "-1.5e2" $ Just (-150.0)
  , testBString "''B" $ Just (BinString 'B' "")
  , testBString "'01'B" $ Just (BinString 'B' "01")
  , testBString "'01 11'B" $ Just (BinString 'B' "0111")
  , testBString "-- Invalid -- '01 11' B" $ Nothing
  , testBString "-- Invalid -- '0123'B" $ Nothing
  , testHString "''H" $ Just (BinString 'H' "")
  , testHString "'0A'H" $ Just (BinString 'H' "0A")
  , testHString "'0A B C'H" $ Just (BinString 'H' "0ABC")
  , testHString "-- Invalid -- '0A' H" $ Nothing
  , testHString "-- Invalid -- '08abc'H" $ Nothing
  , testCString "\"\"" $ Just (CString "")  
  , testCString "\"abc\"" $ Just (CString "abc")
  , testCString "\"abc\ndef\"" $ Just (CString "abcdef")
  , testCString "\"abc\"\"def\"" $ Just (CString "abc\"def")
  ]
  where
    testTypeRef val expected = testCase ("Type reference " ++ val) $ parseASN1 typereference val @?= expected
    testIdent  val expected = testCase ("Identifier " ++ val) $ parseASN1 identifier val @?= expected
    testInvTypeRef val = testCase ("Invalid type reference " ++ val) $ parseASN1 typereference val @?= Nothing
    testInvIdent   val = testCase ("Invalid identifier " ++ val) $ parseASN1 identifier val @?= Nothing
    testReal val expected = testCase ("realnumber " ++ val) $ parseASN1 realnumber val @?= expected
    testBString val expected = testCase ("bstring " ++ val) $ parseASN1 bstring val @?= expected
    testHString val expected = testCase ("hstring " ++ val) $ parseASN1 hstring val @?= expected
    testCString val expected = testCase ("cstring " ++ val) $ parseASN1 cstring val @?= expected

-- X.680-0207, clause 12, modules
moduleTests =
  [ testModule "A DEFINITIONS ::= BEGIN END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Nothing})
  , testModule "A DEFINITIONS EXPLICIT TAGS ::= BEGIN END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Nothing})
  , testModule "A DEFINITIONS IMPLICIT TAGS ::= BEGIN A ::= NULL END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ImplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = ExportsAll, module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testModule "A DEFINITIONS IMPLICIT TAGS ::= BEGIN EXPORTS A; A ::= NULL END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ImplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = Exports [ExportedSymbol (TypeReferenceSymbol (TypeReference "A"))], module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testModule "MMS-Object-Module-1 { iso standard 9506 part(1) mms-object-model-version1(2) } DEFINITIONS ::= BEGIN END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "MMS-Object-Module-1") (Just [DefinitiveOIDName (Identifier "iso"),DefinitiveOIDName (Identifier "standard"),DefinitiveOIDNumber 9506,DefinitiveOIDNamedNumber (Identifier "part") 1,DefinitiveOIDNamedNumber (Identifier "mms-object-model-version1") 2]), default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Nothing})
  , testModule "A {iso 4 16 ccitt} DEFINITIONS AUTOMATIC TAGS EXTENSIBILITY IMPLIED ::= BEGIN END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") (Just [DefinitiveOIDName (Identifier "iso"),DefinitiveOIDNumber 4,DefinitiveOIDNumber 16,DefinitiveOIDName (Identifier "ccitt")]), default_tag_type = AutomaticTags, extensibility_implied = True, module_body = Nothing})
  , testInvModule "A DEFINITIONS ::= BEGIN IMPORTS EXPORTS A ::= NULL END"
  , testInvModule "A DEFINITIONS ::= BEGIN IMPORTS; EXPORTS; A ::= NULL END"
  , testModule "A DEFINITIONS ::= BEGIN EXPORTS; A ::= NULL END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = Exports [], module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testModule "A DEFINITIONS ::= BEGIN EXPORTS ALL; A ::= NULL END" $ Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = ExportsAll, module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testImports "IMPORTS Bi1, Bi2 FROM B Ci1 FROM C;" $ Just (Imports [SymbolsFromModule [TypeReferenceSymbol (TypeReference "Bi1"),TypeReferenceSymbol (TypeReference "Bi2")] (GlobalModuleReference (ModuleReference "B") Nothing),SymbolsFromModule [TypeReferenceSymbol (TypeReference "Ci1")] (GlobalModuleReference (ModuleReference "C") Nothing)])
  , testImports "IMPORTS\nProbableCause FROM Attribute-ASN1Module {joint-iso-itu-t ms(9) smi(3) part2(2) asn1Module(2) 1}\nTimePeriod FROM MetricModule\nsomeLocalValue Foo FROM BAR External.value;" $ Just (Imports [SymbolsFromModule [TypeReferenceSymbol (TypeReference "ProbableCause")] (GlobalModuleReference (ModuleReference "Attribute-ASN1Module") (Just (AssignedIdentifierOID [ObjIdName (Identifier "joint-iso-itu-t"),ObjIdNamedNumber (NamedNumber (Identifier "ms") 9),ObjIdNamedNumber (NamedNumber (Identifier "smi") 3),ObjIdNamedNumber (NamedNumber (Identifier "part2") 2),ObjIdNamedNumber (NamedNumber (Identifier "asn1Module") 2),ObjIdNumber 1]))),SymbolsFromModule [TypeReferenceSymbol (TypeReference "TimePeriod")] (GlobalModuleReference (ModuleReference "MetricModule") (Just (AssignedIdentifierDefinedValue (LocalValueReference (ValueReference "someLocalValue"))))),SymbolsFromModule [TypeReferenceSymbol (TypeReference "Foo")] (GlobalModuleReference (ModuleReference "BAR") (Just (AssignedIdentifierDefinedValue (ExternalValueReference (ModuleReference "External") (ValueReference "value")))))])
 , testDefinitiveIdentifier "{ iso(1) dod(6) }" $ Just (Just [DefinitiveOIDNamedNumber (Identifier "iso") 1,DefinitiveOIDNamedNumber (Identifier "dod") 6])
 , testDefinitiveIdentifier "{ iso standard 9506 part(2) mms-environment-version1(4)}" $ Just (Just [DefinitiveOIDName (Identifier "iso"),DefinitiveOIDName (Identifier "standard"),DefinitiveOIDNumber 9506,DefinitiveOIDNamedNumber (Identifier "part") 2,DefinitiveOIDNamedNumber (Identifier "mms-environment-version1") 4])
 ]
  where
    testModule  val expected = testCase ("Module definition " ++ val) $ parseASN1 moduleDefinition val @?= expected
    testInvModule val = testCase ("Invalid module definition " ++ val) $ parseASN1 moduleDefinition val @?= Nothing
    testImports val expected = testCase ("Imports " ++ val) $ parseASN1 imports val @?= expected
    testDefinitiveIdentifier val expected = testCase ("Definitive identifier " ++ val) $ parseASN1 definitiveIdentifier val @?= expected

-- Clause 17
booleanTests =
  [ testType "BOOLEAN" $ Just (Type {type_id = Boolean, subtype = Nothing})
  , testValue "TRUE"  booleanValue $ Just (BooleanValue True)
  , testValue "FALSE" booleanValue $ Just (BooleanValue False)
  , testAssignment "a BOOLEAN ::= TRUE" $ Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = Boolean, subtype = Nothing}, assigned_value = BooleanValue True})
  ]
    
-- Clause 18
integerTests =
  [ testType "INTEGER" $ Just (Type {type_id = TheInteger [], subtype = Nothing})
  , testType "INTEGER { a(3), b(a) }" $ Just (Type {type_id = TheInteger [NamedNumber (Identifier "a") 3,NamedDefinedValue (Identifier "b") (LocalValueReference (ValueReference "a"))], subtype = Nothing})
  , testAmbiguousValue "10" integerValue  (Just (SignedNumber 10)) $ Just (SomeNumber 10)
  , testAmbiguousValue "-10" integerValue (Just (SignedNumber (-10))) $ Just (SomeNumber (-10))
  , testAmbiguousValue "a" integerValue (Just (IdentifiedNumber (Identifier "a"))) $ Just (SomeIdentifiedValue (Identifier "a"))
  , testAssignment "a INTEGER ::= 1" $ Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = TheInteger [], subtype = Nothing}, assigned_value = SignedNumber 1})
  , testAssignment "a INTEGER {a(3), b(a)} ::= b" $ Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = TheInteger [NamedNumber (Identifier "a") 3,NamedDefinedValue (Identifier "b") (LocalValueReference (ValueReference "a"))], subtype = Nothing}, assigned_value = IdentifiedNumber (Identifier "b")})
  , testType "INTEGER {first(1), last(31)} (first | last)" $ Just (Type {type_id = TheInteger [NamedNumber (Identifier "first") 1,NamedNumber (Identifier "last") 31], subtype = Just (Constraint (ClosedSet False (Union [[Intersection (Subtype (SingleValue (IdentifiedNumber (Identifier "first")))) Nothing],[Intersection (Subtype (SingleValue (IdentifiedNumber (Identifier "last")))) Nothing]])) Nothing)})
  , testType "INTEGER {first(1), last(31)} (first .. last)" $ Just (Type {type_id = TheInteger [NamedNumber (Identifier "first") 1,NamedNumber (Identifier "last") 31], subtype = Just (Constraint (ClosedSet False (Singleton (Subtype (ValueRange (Closed (Value (IdentifiedNumber (Identifier "first")))) (Closed (Value (IdentifiedNumber (Identifier "last")))))))) Nothing)})
  ]

-- X.680-0207, clause 19, "ENUMERATED"
enumeratedTests = 
  [ testType "ENUMERATED {a(1),b(2)}" $ Just (Type {type_id = SimpleEnumeration [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)], subtype = Nothing})
  , testType "ENUMERATED {a(1),b(2),...}" $ Just (Type {type_id = EnumerationWithException [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing, subtype = Nothing})
  , testType "ENUMERATED {a(1),b(2),...,someIdent}" $ Just (Type {type_id = EnumerationWithExceptionAndAddition [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing [EnumerationItemIdentifier (Identifier "someIdent")], subtype = Nothing})
  , testAmbiguousValue "a" enumeratedValue (Just (EnumeratedValue (Identifier "a")))  $ Just (SomeIdentifiedValue (Identifier "a"))
  , testAssignment "x ENUMERATED {a, b(3), ..., c(1)} ::= c" $ Just (ValueAssignment {value_ref = ValueReference "x", value_ref_type = Type {type_id = EnumerationWithExceptionAndAddition [EnumerationItemIdentifier (Identifier "a"),EnumerationItemNumber (NamedNumber (Identifier "b") 3)] Nothing [EnumerationItemNumber (NamedNumber (Identifier "c") 1)], subtype = Nothing}, assigned_value = EnumeratedValue (Identifier "c")})
  ]
  
-- Clause 20
realTests =
  [ testType "REAL" $ Just (Type {type_id = Real, subtype = Nothing})
  , testType "REAL (WITH COMPONENTS {mantissa (-16777215..16777215),base (2),exponent (-125..128) } )" $ Just (Type {type_id = Real, subtype = Just (Constraint (ClosedSet False (Singleton (Subtype (MultipleTypeConstaints [NamedConstraint (Identifier "mantissa") (ComponentConstraint (Just (Constraint (ClosedSet False (Singleton (Subtype (ValueRange (Closed (Value (RealValue (-1.6777215e7)))) (Closed (Value (RealValue 1.6777215e7))))))) Nothing)) Nothing),NamedConstraint (Identifier "base") (ComponentConstraint (Just (Constraint (ClosedSet False (Singleton (Subtype (SingleValue (RealValue 2.0))))) Nothing)) Nothing),NamedConstraint (Identifier "exponent") (ComponentConstraint (Just (Constraint (ClosedSet False (Singleton (Subtype (ValueRange (Closed (Value (RealValue (-125.0)))) (Closed (Value (RealValue 128.0))))))) Nothing)) Nothing)])))) Nothing)}) -- TODO: components are really integer. Need to introduce associated types throughout the parser
  , testAmbiguousValue "10.0" realValue  (Just (RealValue 10.0)) $ Just (SomeNumber (10.0))
  , testAmbiguousValue "-10.0" realValue (Just (RealValue (-10.0))) $ Just (SomeNumber (-10.0))
  , testAmbiguousValue "10" realValue  (Just (RealValue 10.0)) $ Just (SomeNumber 10)
  , testAmbiguousValue "-10" realValue (Just (RealValue (-10.0))) $ Just (SomeNumber (-10))
  , testValue "PLUS-INFINITY" realValue  $ Just (PlusInfinity)
  , testValue "MINUS-INFINITY" realValue $ Just (MinusInfinity)
  , testAmbiguousValue "{}" realValue (Just (SequenceRealValue [])) $ Just (SomeNamedValueList [])
  , testAmbiguousValue "{mantissa 1, base 10, exponent 10}" realValue (Just (SequenceRealValue [NamedValue (Identifier "mantissa") (SomeNumber 1.0),NamedValue (Identifier "base") (SomeNumber 10.0),NamedValue (Identifier "exponent") (SomeNumber 10.0)])) $ Just (SomeNamedValueList [NamedValue (Identifier "mantissa") (SomeNumber 1.0),NamedValue (Identifier "base") (SomeNumber 10.0),NamedValue (Identifier "exponent") (SomeNumber 10.0)])
  , testAssignment "a REAL ::= PLUS-INFINITY" $ Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = Real, subtype = Nothing}, assigned_value = PlusInfinity})
  , testAssignment "a REAL ::= -10e5" $ Just (ValueAssignment {value_ref = ValueReference "a", value_ref_type = Type {type_id = Real, subtype = Nothing}, assigned_value = RealValue (-1000000.0)})
  ]

-- Clause 21
bitStringTests =
  [ testType "BIT STRING (SIZE (12))" $ Just (Type {type_id = BitString [], subtype = Just (Constraint (ClosedSet False (Singleton (Subtype (SizeConstraint (Constraint (ClosedSet False (Singleton (Subtype (SingleValue (SignedNumber 12))))) Nothing))))) Nothing)})
  , testType "BIT STRING {sunday(0), monday (1), tuesday(2),wednesday(3), thursday(4), friday(5),saturday(6) } (SIZE (0..7))" $ Just (Type {type_id = BitString [NamedNumber (Identifier "sunday") 0,NamedNumber (Identifier "monday") 1,NamedNumber (Identifier "tuesday") 2,NamedNumber (Identifier "wednesday") 3,NamedNumber (Identifier "thursday") 4,NamedNumber (Identifier "friday") 5,NamedNumber (Identifier "saturday") 6], subtype = Just (Constraint (ClosedSet False (Singleton (Subtype (SizeConstraint (Constraint (ClosedSet False (Singleton (Subtype (ValueRange (Closed (Value (SignedNumber 0))) (Closed (Value (SignedNumber 7))))))) Nothing))))) Nothing)})
  , testValue "'100110100100001110110'B" bitStringValue $ Just (BinaryString (BinString 'B' "100110100100001110110"))
  , testValue "'0123456789ABCDEF'H" bitStringValue $ Just (HexString (BinString 'H' "0123456789ABCDEF"))
  , testValue "'0000 0001 0010'B" bitStringValue $ Just (BinaryString (BinString 'B' "000000010010"))
  , testAmbiguousValue "{sunday, monday, wednesday}" bitStringValue (Just (IdentifierListBitString [Identifier "sunday",Identifier "monday",Identifier "wednesday"])) $ Just (SomeValueList [SomeIdentifiedValue (Identifier "sunday"),SomeIdentifiedValue (Identifier "monday"),SomeIdentifiedValue (Identifier "wednesday")])
  , testValue "CONTAINING NULL" bitStringValue $ Just (Containing NullValue)
  , testAssignment "image BIT STRING ::= '1001'B" $ Just (ValueAssignment {value_ref = ValueReference "image", value_ref_type = Type {type_id = BitString [], subtype = Nothing}, assigned_value = BinaryString (BinString 'B' "1001")})
  ]

-- Clause 22
octetStringTests =
  [ testType "OCTET STRING (SIZE (12))" $ Just (Type {type_id = OctetString, subtype = Just (Constraint (ClosedSet False (Singleton (Subtype (SizeConstraint (Constraint (ClosedSet False (Singleton (Subtype (SingleValue (SignedNumber 12))))) Nothing))))) Nothing)})
  , testValue "'100110100100001110110'B" bitStringValue $ Just (BinaryString (BinString 'B' "100110100100001110110"))
  , testValue "'3FE2EBAD471005'H" bitStringValue $ Just (HexString (BinString 'H' "3FE2EBAD471005"))
  , testValue "CONTAINING 10.0" bitStringValue $ Just (Containing (SomeNumber 10.0))
  , testAssignment "image OCTET STRING ::= '1001'B" $ Just (ValueAssignment {value_ref = ValueReference "image", value_ref_type = Type {type_id = OctetString, subtype = Nothing}, assigned_value = BinaryString (BinString 'B' "1001")})
  ]

-- Clause 23
nullTests =
  [ testType "NULL" $ Just (Type {type_id = Null, subtype = Nothing})
  , testValue "NULL" nullValue $ Just NullValue
  , testAssignment "foo NULL ::= NULL" $ Just (ValueAssignment {value_ref = ValueReference "foo", value_ref_type = Type {type_id = Null, subtype = Nothing}, assigned_value = NullValue})
  ]

-- Clause 24
sequenceTests = 
  [ testType "SEQUENCE {}" $ Just (Type {type_id = Sequence Empty, subtype = Nothing})
  , testType "SEQUENCE {...}" $ Just (Type {type_id = Sequence (JustException Nothing), subtype = Nothing})
  , testType "SEQUENCE {...!BOOLEAN : FALSE}" $ Just (Type {type_id = Sequence (JustException (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) (BooleanValue False)))), subtype = Nothing})
  , testType "SEQUENCE {...!BOOLEAN : FALSE, a A}" $ Just (Type {type_id = Sequence (JustExtensions (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) (BooleanValue False))) (Just [ExtensionAdditionType (NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing})])), subtype = Nothing})
  , testType "SEQUENCE {a A,...,b B}" $ Just (Type {type_id = Sequence (ExtensionsAtEnd [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing}] Nothing (Just [ExtensionAdditionType (NamedTypeComponent {element_type = NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "B"), subtype = Nothing}), element_presence = Nothing})])), subtype = Nothing})
  , testType "SEQUENCE {a A,...,[[b B, c C]]}" $ Just (Type {type_id = Sequence (ExtensionsAtEnd [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing}] Nothing (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "B"), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]])), subtype = Nothing})
  , testType "SEQUENCE {...!BOOLEAN : FALSE, [[ d D, e E ]] , ..., c C }" $ Just (Type {type_id = Sequence (ExtensionsAtStart (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) (BooleanValue False))) (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "d") (Type {type_id = LocalTypeReference (TypeReference "D"), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "e") (Type {type_id = LocalTypeReference (TypeReference "E"), subtype = Nothing}), element_presence = Nothing}]]) [NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]), subtype = Nothing})
  , testType "SEQUENCE {a A OPTIONAL,...!BOOLEAN : FALSE, [[ d D DEFAULT 5, e E ]] , ..., c C }" $ Just (Type {type_id = Sequence (ExtensionsInTheMiddle [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Just OptionalValue}] (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) (BooleanValue False))) (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "d") (Type {type_id = LocalTypeReference (TypeReference "D"), subtype = Nothing}), element_presence = Just (DefaultValue (SomeNumber 5.0))},NamedTypeComponent {element_type = NamedType (Identifier "e") (Type {type_id = LocalTypeReference (TypeReference "E"), subtype = Nothing}), element_presence = Nothing}]]) [NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]), subtype = Nothing})
  , testAmbiguousValue "{}" sequenceValue (Just (SequenceValue [])) $ Just (SomeNamedValueList [])
  , testAmbiguousValue "{a 1, b 2, c 3}" sequenceValue (Just (SequenceValue [NamedValue (Identifier "a") (SomeNumber 1.0),NamedValue (Identifier "b") (SomeNumber 2.0),NamedValue (Identifier "c") (SomeNumber 3.0)])) $ Just (SomeNamedValueList [NamedValue (Identifier "a") (SomeNumber 1.0),NamedValue (Identifier "b") (SomeNumber 2.0),NamedValue (Identifier "c") (SomeNumber 3.0)])
  ]

-- Clause 25
sequenceOfTests = 
  [ testType "SEQUENCE OF BOOLEAN" $ Just (Type {type_id = SequenceOf Nothing (Left (Type {type_id = Boolean, subtype = Nothing})), subtype = Nothing})
  , testType "SEQUENCE OF foo BAR" $ Just (Type {type_id = SequenceOf Nothing (Right (NamedType (Identifier "foo") (Type {type_id = LocalTypeReference (TypeReference "BAR"), subtype = Nothing}))), subtype = Nothing})
  , testType "SEQUENCE OF foo SEQUENCE {...!BOOLEAN : FALSE}" $ Just (Type {type_id = SequenceOf Nothing (Right (NamedType (Identifier "foo") (Type {type_id = Sequence (JustException (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) (BooleanValue False)))), subtype = Nothing}))), subtype = Nothing})
  , testAmbiguousValue "{}" sequenceOfValue (Just (SequenceOfValue (Right []))) $ Just (SomeNamedValueList [])
  , testAmbiguousValue "{FALSE, FALSE, TRUE}" sequenceOfValue (Just (SequenceOfValue (Left [BooleanValue False,BooleanValue False,BooleanValue True]))) $ Just (SomeValueList [BooleanValue False,BooleanValue False,BooleanValue True])
  , testAmbiguousValue "{a 1, b 2, c 3}" sequenceOfValue (Just (SequenceOfValue (Right [NamedValue (Identifier "a") (SomeNumber 1.0),NamedValue (Identifier "b") (SomeNumber 2.0),NamedValue (Identifier "c") (SomeNumber 3.0)]))) $ Just (SomeNamedValueList [NamedValue (Identifier "a") (SomeNumber 1.0),NamedValue (Identifier "b") (SomeNumber 2.0),NamedValue (Identifier "c") (SomeNumber 3.0)])
  ]

-- Clause 26
setTests = 
  [ testType "SET {}" $ Just (Type {type_id = Set Empty, subtype = Nothing})
  , testType "SET {personalName [0] VisibleString, organizationName [1] VisibleString, countryName [2] VisibleString}" $ Just (Type {type_id = Set (ComponentTypeList [NamedTypeComponent {element_type = NamedType (Identifier "personalName") (Type {type_id = Tagged (Tag Nothing (ClassNumber 0)) Nothing (Type {type_id = VisibleString, subtype = Nothing}), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "organizationName") (Type {type_id = Tagged (Tag Nothing (ClassNumber 1)) Nothing (Type {type_id = VisibleString, subtype = Nothing}), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "countryName") (Type {type_id = Tagged (Tag Nothing (ClassNumber 2)) Nothing (Type {type_id = VisibleString, subtype = Nothing}), subtype = Nothing}), element_presence = Nothing}]), subtype = Nothing})
  , testAmbiguousValue "{}" setValue (Just (SetValue [])) $ Just (SomeNamedValueList [])
  , testAmbiguousValue "{a 1, b 2, c 3}" setValue (Just (SetValue [NamedValue (Identifier "a") (SomeNumber 1),NamedValue (Identifier "b") (SomeNumber 2),NamedValue (Identifier "c") (SomeNumber 3)])) $ Just (SomeNamedValueList [NamedValue (Identifier "a") (SomeNumber 1.0),NamedValue (Identifier "b") (SomeNumber 2.0),NamedValue (Identifier "c") (SomeNumber 3.0)])
  , testAssignment "someASN1Keywords SET {aaa BOOLEAN, bbb NULL OPTIONAL} ::= {bbb NULL, aaa FALSE}" $ Just (ValueAssignment {value_ref = ValueReference "someASN1Keywords", value_ref_type = Type {type_id = Set (ComponentTypeList [NamedTypeComponent {element_type = NamedType (Identifier "aaa") (Type {type_id = Boolean, subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "bbb") (Type {type_id = Null, subtype = Nothing}), element_presence = Just OptionalValue}]), subtype = Nothing}, assigned_value = SetValue [NamedValue (Identifier "bbb") NullValue,NamedValue (Identifier "aaa") (BooleanValue False)]})
  ]

-- Clause 27
setOfTests =
  [ testType "SET OF keyword VisibleString" $ Just (Type {type_id = SetOf Nothing (Right (NamedType (Identifier "keyword") (Type {type_id = VisibleString, subtype = Nothing}))), subtype = Nothing})
  , testAssignment "someASN1Keywords2 SET OF keyword VisibleString ::= {keyword \"INTEGER\", keyword \"BOOLEAN\", keyword \"REAL\"}" $ Just (ValueAssignment {value_ref = ValueReference "someASN1Keywords2", value_ref_type = Type {type_id = SetOf Nothing (Right (NamedType (Identifier "keyword") (Type {type_id = VisibleString, subtype = Nothing}))), subtype = Nothing}, assigned_value = SetOfValue (Right [NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "INTEGER")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "BOOLEAN")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "REAL")])])})
  , testAmbiguousValue "{keyword \"INTEGER\", keyword \"BOOLEAN\", keyword \"REAL\"}" setOfValue (Just (SetOfValue (Right [NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "INTEGER")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "BOOLEAN")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "REAL")])]))) $ Just (SomeNamedValueList [NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "INTEGER")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "BOOLEAN")]),NamedValue (Identifier "keyword") (RestrictedCharacterStringValue [CharsCString (CString "REAL")])])
  ]

-- Clause 28, CHOICE
choiceTests =
  [ testType "CHOICE { a Ta, b Tb, c Tc }" $ Just (Type {type_id = Choice (SimpleAlternativeTypeList [NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "Ta"), subtype = Nothing}),NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "Tb"), subtype = Nothing}),NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "Tc"), subtype = Nothing})]), subtype = Nothing})
  , testType "CHOICE { a Ta, b Tb,..., c Tc }" $ Just (Type {type_id = Choice (AlternativeTypeListWithExtension [NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "Ta"), subtype = Nothing}),NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "Tb"), subtype = Nothing})] Nothing (Just [ExtensionAdditionAlternativesType (NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "Tc"), subtype = Nothing}))])), subtype = Nothing})
  , testType "CHOICE { a Ta, b Tb,..., c Tc, ... }" $ Just (Type {type_id = Choice (AlternativeTypeListWithExtension [NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "Ta"), subtype = Nothing}),NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "Tb"), subtype = Nothing})] Nothing (Just [ExtensionAdditionAlternativesType (NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "Tc"), subtype = Nothing}))])), subtype = Nothing})
  , testType "CHOICE {d [0] NULL,e [1] NULL}" $ Just (Type {type_id = Choice (SimpleAlternativeTypeList [NamedType (Identifier "d") (Type {type_id = Tagged (Tag Nothing (ClassNumber 0)) Nothing (Type {type_id = Null, subtype = Nothing}), subtype = Nothing}),NamedType (Identifier "e") (Type {type_id = Tagged (Tag Nothing (ClassNumber 1)) Nothing (Type {type_id = Null, subtype = Nothing}), subtype = Nothing})]), subtype = Nothing})
  , testValue "vmd-specific: basicVMD-specific: \"M_DAYTIME\"" choiceValue $ Just (ChoiceValue (Identifier "vmd-specific") (ChoiceValue (Identifier "basicVMD-specific") (RestrictedCharacterStringValue [CharsCString (CString "M_DAYTIME")])))
  , testValue "foo: bar: baz: a: b: c: NULL" choiceValue $ Just (ChoiceValue (Identifier "foo") (ChoiceValue (Identifier "bar") (ChoiceValue (Identifier "baz") (ChoiceValue (Identifier "a") (ChoiceValue (Identifier "b") (ChoiceValue (Identifier "c") NullValue))))))
  , testValue "foo: TRUE" choiceValue $ Just (ChoiceValue (Identifier "foo") (BooleanValue True))
  , testValue "foo: \":\"" choiceValue $ Just (ChoiceValue (Identifier "foo") (RestrictedCharacterStringValue [CharsCString (CString ":")]))
  ]

-- Clause 29, selection
selectionTests =
  [ testType "date-last-used < FileAttribute" $ Just (Type {type_id = Selection (Identifier "date-last-used") (Type {type_id = LocalTypeReference (TypeReference "FileAttribute"), subtype = Nothing}), subtype = Nothing})
  , testType "SEQUENCE {first-attribute date-last-used < FileAttribute, second-attribute file-name < FileAttribute }" $ Just (Type {type_id = Sequence (ComponentTypeList [NamedTypeComponent {element_type = NamedType (Identifier "first-attribute") (Type {type_id = Selection (Identifier "date-last-used") (Type {type_id = LocalTypeReference (TypeReference "FileAttribute"), subtype = Nothing}), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "second-attribute") (Type {type_id = Selection (Identifier "file-name") (Type {type_id = LocalTypeReference (TypeReference "FileAttribute"), subtype = Nothing}), subtype = Nothing}), element_presence = Nothing}]), subtype = Nothing})
  ]

-- Clause 31, OID
oidTests = 
  [ testType "OBJECT IDENTIFIER" $ Just (Type {type_id = ObjectIdentifier, subtype = Nothing})
  , testAmbiguousValue "{ 1 0 8571 1 }" objectIdentifierValue (Just (OID [ObjIdNumber 1,ObjIdNumber 0,ObjIdNumber 8571,ObjIdNumber 1])) $ Just (SomeOIDLikeValue [ObjIdNumber 1,ObjIdNumber 0,ObjIdNumber 8571,ObjIdNumber 1])
  , testAmbiguousValue "{ iso standard 8571 pci (1) }" objectIdentifierValue (Just (OID [ObjIdName (Identifier "iso"),ObjIdName (Identifier "standard"),ObjIdNumber 8571,ObjIdNamedNumber (NamedNumber (Identifier "pci") 1)])) $ Just (SomeOIDLikeValue [ObjIdName (Identifier "iso"),ObjIdName (Identifier "standard"),ObjIdNumber 8571,ObjIdNamedNumber (NamedNumber (Identifier "pci") 1)])
  , testAssignment "foo OBJECT IDENTIFIER ::= { iso standard 8571 pci (1) }" $ Just (ValueAssignment {value_ref = ValueReference "foo", value_ref_type = Type {type_id = ObjectIdentifier, subtype = Nothing}, assigned_value = OID [ObjIdName (Identifier "iso"),ObjIdName (Identifier "standard"),ObjIdNumber 8571,ObjIdNamedNumber (NamedNumber (Identifier "pci") 1)]})
  ]

-- Clause 32, RELATIVE-OID
relativeOIDTests = 
  [ testType "RELATIVE-OID" $ Just (Type {type_id = RelativeOID, subtype = Nothing})
  , testAmbiguousValue "{science-fac(4) maths-dept(3)}" relativeOIDValue (Just (RelativeOIDValue [RelativeOIDNamedNumber (NamedNumber (Identifier "science-fac") 4),RelativeOIDNamedNumber (NamedNumber (Identifier "maths-dept") 3)])) $ Just (SomeOIDLikeValue [ObjIdNamedNumber (NamedNumber (Identifier "science-fac") 4),ObjIdNamedNumber (NamedNumber (Identifier "maths-dept") 3)])
  , testAssignment "firstgroup RELATIVE-OID ::= {science-fac(4) maths-dept(3)}" $ Just (ValueAssignment {value_ref = ValueReference "firstgroup", value_ref_type = Type {type_id = RelativeOID, subtype = Nothing}, assigned_value = RelativeOIDValue [RelativeOIDNamedNumber (NamedNumber (Identifier "science-fac") 4),RelativeOIDNamedNumber (NamedNumber (Identifier "maths-dept") 3)]})
  ]

-- Clause 35
characterStringTests =
  [ testType "Cyrillic (Level1)" $ noType
  , testType "BasicArabic (SIZE (1..100) ^ Level2)" $ noType
  , testType "UniversalString (FROM (Katakana | BasicLatin))" $ noType
  , testType "BMPString (FROM (ALL EXCEPT CombiningCharactersType-2))" $ noType
  , testType "CHARACTER STRING (WITH COMPONENTS {identification (WITH COMPONENTS {fixed PRESENT })" $ noType
  , testAssignment "greekCapitalLetterSigma BMPString ::= {0, 0, 3, 163}" $ noAssignment
  , testAssignment "property UTF8String ::= {\"f \", rightwardsArrow, \" \", infinity}" $ noAssignment
  , testAssignment "mystring MyAlphabet ::= \"HOPE\"" $ noAssignment
  ]

classTests =
  [ testField "&TypeField" $ Just (TypeField (TypeFieldReference "TypeField") Nothing)
  , testField "&object FIELD" $ Just (ObjectField (ObjectFieldReference "object") (LocalObjectClassReference (ObjectClassReference "FIELD")) Nothing)
  , testField "&Object SET-FIELD" $ Just (ObjectSetField (ObjectSetFieldReference "Object") (LocalObjectClassReference (ObjectClassReference "SET-FIELD")) Nothing)
  , testField "&fixedType ValueField" $ Just (FixedTypeValueField (ValueFieldReference "fixedType") (Type {type_id = LocalTypeReference (TypeReference "ValueField"), subtype = Nothing}) False Nothing)
  , testField "&variable-Type &Value-Field" $ Just (VariableTypeValueField (ValueFieldReference "variable-Type") [PrimTFR (TypeFieldReference "Value-Field")] Nothing)
  , testField "&Fixed-Type ValueSetField" $ Just (FixedTypeValueSetField (ValueSetFieldReference "Fixed-Type") (Type {type_id = LocalTypeReference (TypeReference "ValueSetField"), subtype = Nothing}) Nothing)
  , testField "&Variable-type &Value-Set-Field" $ Just (VariableTypeValueSetField (ValueSetFieldReference "Variable-type") [PrimTFR (TypeFieldReference "Value-Set-Field")] Nothing)
  , testField "&ArgumentType OPTIONAL" $ Just (TypeField (TypeFieldReference "ArgumentType") (Just OptionalType))
  , testField "&Errors ERROR OPTIONAL" $ Just (ObjectSetField (ObjectSetFieldReference "Errors") (LocalObjectClassReference (ObjectClassReference "ERROR")) (Just OptionalObjectSet))
  , testField "&resultReturned BOOLEAN DEFAULT TRUE" $ Just (FixedTypeValueField (ValueFieldReference "resultReturned") (Type {type_id = Boolean, subtype = Nothing}) False (Just (DefaultValue (BooleanValue True))))
  , testField "&code INTEGER UNIQUE" $ Just (FixedTypeValueField (ValueFieldReference "code") (Type {type_id = TheInteger [], subtype = Nothing}) True Nothing)
  ]
  where
    testField val exp = testCase ("Field definition " ++ val) $ parseASN1 field val @?= exp
