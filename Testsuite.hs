import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit

import Data.List

import Language.ASN1.Parser as P

main = defaultMain tests

tests = 
  [ testGroup "Modules" moduleTests
  , testGroup "Enumerated type" enumeratedTests
  , testGroup "SEQUENCE" sequenceTests
  ]

-- X.680-0207, clause 12, modules
moduleTests =
  [ testCase "Empty module" $ parseASN1 moduleDefinition "A DEFINITIONS ::= BEGIN END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Nothing})
  , testCase "EXPLICIT TAGS" $ parseASN1 moduleDefinition "A DEFINITIONS EXPLICIT TAGS ::= BEGIN END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Nothing})
  , testCase "IMPLICIT TAGS" $ parseASN1 moduleDefinition "A DEFINITIONS IMPLICIT TAGS ::= BEGIN END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ImplicitTags, extensibility_implied = False, module_body = Nothing})
  , testCase "Empty module, automatic tags, extensibility implied" $ parseASN1 moduleDefinition "A {iso(2) 4 16 ccitt} DEFINITIONS AUTOMATIC TAGS EXTENSIBILITY IMPLIED ::= BEGIN END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") (Just [DefinitiveOIDNamedNumber (Identifier "iso") 2,DefinitiveOIDNumber 4,DefinitiveOIDNumber 16,DefinitiveOIDName (Identifier "ccitt")]), default_tag_type = AutomaticTags, extensibility_implied = True, module_body = Nothing})
  , testCase "Unterminated IMPORTS and EXPORTS" $ parseASN1 moduleDefinition "A DEFINITIONS ::= BEGIN IMPORTS EXPORTS A ::= NULL END" @?= Nothing
  , testCase "Empty IMPORTS" $ parseASN1 moduleDefinition "A DEFINITIONS ::= BEGIN IMPORTS; EXPORTS; A ::= NULL END" @?= Nothing
  , testCase "No IMPORTS, empty EXPORTS" $ parseASN1 moduleDefinition "A DEFINITIONS ::= BEGIN EXPORTS; A ::= NULL END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = Exports [], module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testCase "EXPORTS ALL" $ parseASN1 moduleDefinition "A DEFINITIONS ::= BEGIN EXPORTS ALL; A ::= NULL END" @?= Just (Module {module_id = ModuleIdentifier (ModuleReference "A") Nothing, default_tag_type = ExplicitTags, extensibility_implied = False, module_body = Just (ModuleBody {module_exports = ExportsAll, module_imports = ImportsNone, module_assignments = [TypeAssignment (TypeReference "A") (Type {type_id = Null, subtype = Nothing})]})})
  , testCase "IMPORTS from several modules without assigned identifiers" $ parseASN1 imports "IMPORTS Bi1, Bi2 FROM B Ci1 FROM C;" @?= Just (Imports [SymbolsFromModule [TypeReferenceSymbol (TypeReference "Bi1"),TypeReferenceSymbol (TypeReference "Bi2")] (GlobalModuleReference (ModuleReference "B") Nothing),SymbolsFromModule [TypeReferenceSymbol (TypeReference "Ci1")] (GlobalModuleReference (ModuleReference "C") Nothing)])
  , testCase "IMPORTS from modules with assigned OID, local and external defined values" $ parseASN1 imports "IMPORTS ProbableCause FROM Attribute-ASN1Module {joint-iso-itu-t ms(9) smi(3) part2(2) asn1Module(2) 1} TimePeriod FROM MetricModule someLocalValue Foo FROM BAR External.value;" @?= Just (Imports [SymbolsFromModule [TypeReferenceSymbol (TypeReference "ProbableCause")] (GlobalModuleReference (ModuleReference "Attribute-ASN1Module") (Just (AssignedIdentifierOID [ObjIdDefinedValue (LocalValueReference (ValueReference "joint-iso-itu-t")),ObjIdNamedNumber (NamedNumber (Identifier "ms") 9),ObjIdNamedNumber (NamedNumber (Identifier "smi") 3),ObjIdNamedNumber (NamedNumber (Identifier "part2") 2),ObjIdNamedNumber (NamedNumber (Identifier "asn1Module") 2),ObjIdNumber 1]))),SymbolsFromModule [TypeReferenceSymbol (TypeReference "TimePeriod")] (GlobalModuleReference (ModuleReference "MetricModule") (Just (AssignedIdentifierDefinedValue (LocalValueReference (ValueReference "someLocalValue"))))),SymbolsFromModule [TypeReferenceSymbol (TypeReference "Foo")] (GlobalModuleReference (ModuleReference "BAR") (Just (AssignedIdentifierDefinedValue (ExternalValueReference (ModuleReference "External") (ValueReference "value")))))])
 ]

-- X.680-0207, clause 19, "ENUMERATED"
enumeratedTests = 
  [ testCase "Simple enumeration" $
    parseASN1 enumeratedType "ENUMERATED {a(1),b(2)}" @?= Just (SimpleEnumeration [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)])
  , testCase "Enumeration with extension at the end" $
    parseASN1 enumeratedType "ENUMERATED {a(1),b(2),...}" @?= Just (EnumerationWithException [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing)
  , testCase "Enumeration with extension in the middle" $
    parseASN1 enumeratedType "ENUMERATED {a(1),b(2),...,someIdent}" @?= Just (EnumerationWithExceptionAndAddition [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing [EnumerationItemIdentifier (Identifier "someIdent")])
  ]
  
-- Section 12.2, SEQUENCE
sequenceTests = 
  [ testCase "Empty SEQUENCE" $ parseASN1 sequenceType "SEQUENCE {}" @?= Just EmptySequence
  , testCase "Empty extendable SEQUENCE" $ parseASN1 sequenceType "SEQUENCE {...}" @?= Just (EmptyExtendableSequence Nothing)
  , testCase "Empty extendable SEQUENCE with exception" $ parseASN1 sequenceType "SEQUENCE {...!BOOLEAN : NULL}" @?= Just (EmptyExtendableSequence (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) NullValue)))
  , testCase "Empty extendable SEQUENCE with exception and extensions" $ parseASN1 sequenceType "SEQUENCE {...!BOOLEAN : NULL, a A}" @?= Just (Sequence (JustExtensions (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) NullValue)) (Just [ExtensionAdditionType (NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing})])))
  , testCase "SEQUENCE with extensions at end" $ parseASN1 sequenceType "SEQUENCE {a A,...,b B}" @?= Just (Sequence (ExtensionsAtEnd [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing}] Nothing (Just [ExtensionAdditionType (NamedTypeComponent {element_type = NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "B"), subtype = Nothing}), element_presence = Nothing})])))
  , testCase "SEQUENCE with grouped extensions at end" $ parseASN1 sequenceType "SEQUENCE {a A,...,[[b B, c C]]}" @?= Just (Sequence (ExtensionsAtEnd [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Nothing}] Nothing (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "b") (Type {type_id = LocalTypeReference (TypeReference "B"), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]])))
  , testCase "SEQUENCE with grouped extensions at start and exception" $ parseASN1 sequenceType "SEQUENCE {...!BOOLEAN : NULL, [[ d D, e E ]] , ..., c C }" @?= Just (Sequence (ExtensionsAtStart (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) NullValue)) (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "d") (Type {type_id = LocalTypeReference (TypeReference "D"), subtype = Nothing}), element_presence = Nothing},NamedTypeComponent {element_type = NamedType (Identifier "e") (Type {type_id = LocalTypeReference (TypeReference "E"), subtype = Nothing}), element_presence = Nothing}]]) [NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]))
  , testCase "SEQUENCE with extensions in the middle, exception, optional and default values" $ parseASN1 sequenceType "SEQUENCE {a A OPTIONAL,...!BOOLEAN : NULL, [[ d D DEFAULT 5, e E ]] , ..., c C }" @?= Just (Sequence (ExtensionsInTheMiddle [NamedTypeComponent {element_type = NamedType (Identifier "a") (Type {type_id = LocalTypeReference (TypeReference "A"), subtype = Nothing}), element_presence = Just OptionalValue}] (Just (ExceptionTypeAndValue (Type {type_id = Boolean, subtype = Nothing}) NullValue)) (Just [ExtensionAdditionGroup Nothing [NamedTypeComponent {element_type = NamedType (Identifier "d") (Type {type_id = LocalTypeReference (TypeReference "D"), subtype = Nothing}), element_presence = Just (DefaultValue (SignedNumber 5))},NamedTypeComponent {element_type = NamedType (Identifier "e") (Type {type_id = LocalTypeReference (TypeReference "E"), subtype = Nothing}), element_presence = Nothing}]]) [NamedTypeComponent {element_type = NamedType (Identifier "c") (Type {type_id = LocalTypeReference (TypeReference "C"), subtype = Nothing}), element_presence = Nothing}]))
  ]