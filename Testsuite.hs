import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit

import Data.List

import Language.ASN1.Parser as P

main = defaultMain tests

tests = 
  [ testGroup "Enumerated type" 
    [ testCase "Simple enumeration" $
      parseASN1 enumeratedType "ENUMERATED {a(1),b(2)}" @?= Just (SimpleEnumeration [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)])
    , testCase "Enumeration with extension at the end" $
      parseASN1 enumeratedType "ENUMERATED {a(1),b(2),...}" @?= Just (EnumerationWithException [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing)
    , testCase "Enumeration with extension in the middle" $
      parseASN1 enumeratedType "ENUMERATED {a(1),b(2),...,someIdent}" @?= Just (EnumerationWithExceptionAndAddition [EnumerationItemNumber (NamedNumber (Identifier "a") 1),EnumerationItemNumber (NamedNumber (Identifier "b") 2)] Nothing [EnumerationItemIdentifier (Identifier "someIdent")])
    ]
  , testGroup "SEQUENCE" sequenceTests
  ]
  -- parseASN1 sequenceType "SEQUENCE { a A,b B,..., [[ d D, e E ]] ,..., c C }"

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