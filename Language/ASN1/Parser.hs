{-# LANGUAGE DeriveDataTypeable #-}
module Language.ASN1.Parser {-(
  parseASN1FromFileOrDie
  , parseASN1FromFile
  , parseASN1
  , Module(..)
  , Type(..)
  , Assignment(..)
    
  , TypeReference(..)
  )-} where
{-
 ASN.1 Parser for Haskell (C) Dmitry Astapov 2003-2010

 This software is distibuted under the terms of BSD license
 See LICENSE for more information

 The early versions of this parser were based on the ASN.1 grammar for JavaCC:
/*
 *
 *  ASN.1 grammar  for JavaCC
 *
 *  Contributed by Helena Sarin (hsarin@lucent.com)
 *
 *  Derived in part from the following work: snacc - a freeware ASN.1 to C or C++ compiler, v 1.3,
 *  yacc/lex source code ( parse-asn1.y, lex-asn1.l), 
 *  the free software, which is covered by GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or later
 *
 */

 Current version is written from scratch following X.680-X.683 specification texts
 and "ASN.1, Communication between Heterogeneous Systems" book by Olivier Dubuisson.

 This is still work in progress, so there could be bug lurking. However, most of the ASN1 files
 found in the wild should be parsable.
-}

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import System.Exit (exitFailure)
import Data.Generics
import Control.Applicative ((<$>),(<*),(*>),(<*>),(<$))
import Control.Monad (when)
import Data.Char (isUpper, isAlpha)
import Data.List (isInfixOf)

-- {{ Top-level interface
parseASN1FromFileOrDie :: String -> IO ([Module])
parseASN1FromFileOrDie fname =
  do result <- parseASN1FromFile fname
     case result of
       Left err -> do { putStr "parse error at: "
                      ; print err
                      ; exitFailure
                      }
       Right x  -> return x


parseASN1FromFile :: String -> IO (Either ParseError [Module])
parseASN1FromFile fname = 
  parseFromFile asn1Input fname

-- This parser is intended for use in tests
parseASN1 p source = 
  case parse p' "" source of
       Left err -> Nothing
       Right x  -> Just x
  where
    p' = fixupComments *> whiteSpace *> p <* eof
-- }}

-- {{ Top-level parser
asn1Input = do
  fixupComments
  whiteSpace
  modules <- many1 moduleDefinition
  eof
  return modules
  <?> "asn1Input"

-- Parsec machinery (Token parser) is incapable of handling complex commenting 
-- conditions like "comment ends on next '--' or on newline". Which is why all
-- line comments are turned into block comments and Token parser is instructed
-- to handle only block comments.
fixupComments = do
  inp <- getInput
  setInput $ unlines $ map fixup $ lines inp
  where 
    fixup l
      | "--" `isInfixOf` l && unterminated = l ++ " --"
      | otherwise = l
      where
        unterminated = checkUnterm False l
        checkUnterm p []             = p
        checkUnterm p ('-':'-':rest) = checkUnterm (not p) rest
        checkUnterm p (_:rest)       = checkUnterm p rest
-- }}
        
-- {{ X.680-0207,  Clause 11, "ASN.1 lexical items"
        -- TODO
-- }} end of clause 11
        
-- {{ X.680-0207, Clause 12, "Module definition"
data Module = Module { module_id::ModuleIdentifier
                     , default_tag_type::TagDefault
                     , extensibility_implied :: Bool
                     , module_body::Maybe ModuleBody
                     } deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
moduleDefinition = 
  Module <$> moduleIdentifier <*> (reserved "DEFINITIONS" *> tagDefault) <*> extensibility 
         <*> (reservedOp "::=" *> reserved "BEGIN" *> moduleBody) <* reserved "END"  
         <?> "ModuleDefinition"
 where
   -- Checked, X.680-0207
   extensibility = option False $ True <$ (reserved "EXTENSIBILITY" >> reserved "IMPLIED")
   
data ModuleIdentifier = ModuleIdentifier ModuleReference (Maybe DefinitiveOID) deriving (Eq,Ord,Show, Typeable, Data)

-- Checked, X.680-0207
moduleIdentifier = ModuleIdentifier <$> modulereference <*> definitiveIdentifier
                   <?> "ModuleIdentifier"

type DefinitiveOID = [DefinitiveOIDComponent]

-- Checked, X.680-0207
definitiveIdentifier =
  optionMaybe (braces (many1 definitiveOIDComponent))
  <?> "DefinitiveIdentifier"

data DefinitiveOIDComponent = DefinitiveOIDNumber Integer 
                            | DefinitiveOIDNamedNumber Identifier Integer
                            | DefinitiveOIDName Identifier deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
definitiveOIDComponent =
  choice [ DefinitiveOIDNumber <$> number
         , try $ DefinitiveOIDNamedNumber <$> identifier <*> parens number
         , DefinitiveOIDName . Identifier <$> reservedOIDIdentifier
         ]
  <?> "DefinitiveObjectIdComponent"

-- Checked, X.680-0207
-- If not set, defaults to ExplicitTags per X.680-0207, 12.2
tagDefault = option ExplicitTags td <?> "tagDefault"
  where 
    td = choice [ ExplicitTags <$ reserved "EXPLICIT"
                , ImplicitTags <$ reserved "IMPLICIT"
                , AutomaticTags <$ reserved "AUTOMATIC"
                ]
         <* reserved "TAGS"  


data Exports = ExportsAll | Exports [ExportedSymbol] deriving (Eq,Ord,Show, Typeable, Data)
data Imports = ImportsNone | Imports [SymbolsFromModule] deriving (Eq,Ord,Show, Typeable, Data)
data ModuleBody = ModuleBody { module_exports::Exports
                             , module_imports::Imports
                             , module_assignments::[Assignment]
                             } deriving (Eq,Ord,Show, Typeable, Data)

-- Checked, X.680-0207
moduleBody = optionMaybe ( ModuleBody <$> exports <*> imports <*> assignmentList )
             <?> "ModuleBody"

newtype ExportedSymbol = ExportedSymbol Symbol deriving (Eq,Ord,Show, Typeable, Data)
exports = 
  option ExportsAll ( 
    choice [ try $ ExportsAll <$ ( reserved "EXPORTS" *> reserved "ALL" *> semi )
           , Exports <$> (reserved "EXPORTS" *> symbolsExported )
           ]) <?> "Exports"
  where symbolsExported = (commaSep $ ExportedSymbol <$> theSymbol) <* semi

-- Checked, X.680-0207
imports = option ImportsNone ( Imports <$> (reserved "IMPORTS" *> symbolsImported) )
          <?> "Imports"
  where symbolsImported = (many1 symbolsFromModule) <* semi

data SymbolsFromModule = SymbolsFromModule [Symbol] GlobalModuleReference deriving (Eq,Ord,Show, Typeable, Data)

-- Checked, X.680-0207
symbolsFromModule = SymbolsFromModule <$> commaSep1 theSymbol <*> (reserved "FROM" *> globalModuleReference)
                    <?> "SymbolsFromModule"

data GlobalModuleReference = GlobalModuleReference ModuleReference (Maybe AssignedIdentifier) deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
globalModuleReference = GlobalModuleReference <$> modulereference <*> assignedIdentifier

data AssignedIdentifier = AssignedIdentifierOID OID | AssignedIdentifierDefinedValue DefinedValue deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
assignedIdentifier = 
  optionMaybe $ 
  choice [ AssignedIdentifierOID <$> try oid
         , try $ AssignedIdentifierDefinedValue <$> definedValue
         ]

data Symbol = TypeReferenceSymbol TypeReference
            -- TODO: | ValueReferenceSymbol ValueReference
            -- it is impossible to distinguish TypeReference and ValueReference syntactically
            | ObjectClassReferenceSymbol ObjectClassReference
            | ObjectReferenceSymbol ObjectReference
            -- TODO: | ObjectSetReferenceSymbol ObjectSetReference
            deriving (Eq,Ord,Show, Typeable, Data)
theSymbol =
 choice ( map try [ TypeReferenceSymbol <$> typereference
                  , ObjectClassReferenceSymbol <$> objectclassreference
                  , ObjectReferenceSymbol <$> objectreference
                  -- TODO: , ObjectSetReferenceSymbol <$> objectsetreference
                  -- TODO: , ValueReferenceSymbol <$> valuereference
                  ] ) <* parametrizedDesignation
 where
   -- Checked, X.683-0207, 9.1
   parametrizedDesignation = optional (lexeme (char '{') >> lexeme (char '}'))

-- Checked, X.680-0207
assignmentList = many1 assignment <?> "assignmentList"


data Assignment = ValueAssignment { value_ref::ValueReference
                                  , value_ref_type::Type
                                  , assigned_value::Value
                                  }
                | TypeAssignment TypeReference Type
                | ValueSetTypeAssignment TypeReference Type ValueSet
                | ObjectClassAssignment ObjectClassReference ObjectClass
                | ObjectAssignment ObjectReference DefinedObjectClass Object
                -- TODO: | ObjectSetAssignment 
                -- TODO: | ParameterizedAssignment
                  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
assignment = 
  choice $ map try [ objectAssignment
                   , valueAssignment
                   , typeAssignment
                   , objectClassAssignment
                   , valueSetTypeAssignment
                   -- TODO: , objectSetAssignment
                   -- TODO: , parameterizedAssignment
                   ]
-- }} end of clause 12
-- {{ X.680-0207, clause 13, "Referencing type and value definitions"

-- definedType is used only in BuiltinType. See BuiltinType for constructors.
-- I also took libery of reusing "simpleDefinedType" for the first two
-- alternatives of definedType.
-- Checked, X.680-0207
definedType = simpleDefinedType --TODO: <|> parametrizedType <|> parametrizedValueSetType

-- ExternalTypeReference is inlined here
-- Checked, X.680-0207
simpleDefinedType = 
  choice [ try $ ExternalTypeReference <$> moduleReferenceAndDot <*> typereference
         , LocalTypeReference <$> typereference
         ] <?> "SimpleDefinedType"

data DefinedValue = ExternalValueReference ModuleReference ValueReference
                  | LocalValueReference ValueReference
                  deriving (Eq,Ord,Show, Typeable, Data)

-- ExternalValueReference is inlined here
-- Checked
definedValue = 
  choice [ try $ ExternalValueReference <$> moduleReferenceAndDot <*> valuereference
         , LocalValueReference <$> valuereference
         -- TODO: , parametrizedValue 
         ] <?> "DefinedValue"
-- }} end of clause 13
-- {{ X.680-0207, clause 14, "Notation to support references to ASN1 components" does not have any useful productions }} --
-- {{ X.680-0207, clause 15, "Assigning types and values"

-- Checked
typeAssignment = TypeAssignment <$> typereference <*> (reservedOp "::=" *> theType)
                 <?> "TypeAssignment"

-- Checked
valueAssignment = do
  ref <- valuereference 
  t <- theType 
  v <- (reservedOp "::=" *> valueOfType t)
  return $ ValueAssignment ref t v

-- Checked
valueSetTypeAssignment = ValueSetTypeAssignment <$> typereference <*> theType <*> (reservedOp "::=" *> valueSet)
  where
    -- This alternative is defined in X.680-0207, clause 15.8
    valueSetOrAlternative = valueSet <|> parens elementSetSpecs
-- }} end of clause 15
-- {{ X.680-0207, clause 16, "Definition of types and values"
-- ConstrainedType (clause 45) is merged in other parsers: "Type Constraint" alternative is encoded here,
-- and TypeWithConstraint in implemented in SetOf/SequenceOf parsers
data Type = Type { type_id::BuiltinType
                 , subtype::Maybe SubtypeSpec
                 }
               deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
theType = Type <$> ( builtinType <|> referencedType ) <*> optionMaybe constraint
          <?> "Type"
{-
Type Clause in the X.680
------------------------
BitStringType 21
BooleanType 17
CharacterStringType 36
ChoiceType 28
EmbeddedPDVType 33
EnumeratedType 19
ExternalType 34
InstanceOfType ITU-T Rec. X.681 | ISO/IEC 8824-2, Annex C
IntegerType 18
NullType 23
ObjectClassFieldType ITU-T Rec. X.681 | ISO/IEC 8824-2, 14.1
ObjectIdentifierType 31
OctetStringType 22
RealType 20
RelativeOIDType 32
SequenceType 24
SequenceOfType 25
SetType 26
SetOfType 27
TaggedType 30

ReferencedTypes:
----------------
DefinedType 13.1
UsefulType 41.1
SelectionType 29
TypeFromObject ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
ValueSetFromObjects ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
-}

data BuiltinType = BitString [NamedNumber]
                 | Boolean
                   -- Fourteen CharacterString variants
                 | CharacterString
                 | BMPString
                 | GeneralString
                 | GraphicString
                 | IA5String
                 | ISO646String
                 | NumericString
                 | PrintableString
                 | TeletexString
                 | T61String
                 | UniversalString
                 | UTF8String
                 | VideotexString
                 | VisibleString
                 | Choice AlternativeTypeLists
                 | EmbeddedPDV
                   -- Three ENUMERATED variants
                 | SimpleEnumeration [EnumerationItem]
                 | EnumerationWithException [EnumerationItem] (Maybe ExceptionIdentification)
                 | EnumerationWithExceptionAndAddition [EnumerationItem] (Maybe ExceptionIdentification) [EnumerationItem]
                 | External
                   -- TODO: InstanceOf
                 | TheInteger [NamedNumber]
                 | Null
                   -- TODO: ObjectClassField
                 | ObjectIdentifier
                 | OctetString 
                 | Real
                 | RelativeOID
                 | Sequence ComponentTypeLists
                 | SequenceOf (Maybe SubtypeSpec) (Either Type NamedType)-- TODO: fix types when constraint is implemented properly
                 | Set ComponentTypeLists
                 | SetOf (Maybe SubtypeSpec) (Either Type NamedType) -- TODO: fix types when constraint is implemented properly
                 | Tagged Tag (Maybe TagType) Type
                   -- Referenced Type constructors:
                   -- Four defined type variants
                 | LocalTypeReference TypeReference
                 | ExternalTypeReference ModuleReference TypeReference
                   -- TODO: | ParameterizedType
                   -- TODO: | ParametrizedValueSetType
                   -- Two UsefulType variants:
                 | GeneralizedTime
                 | UTCTime
                 | Selection Identifier Type
                   -- TODO: TypeFromObject constructors                   
                   -- TODO: ValueSetFromObjects constructors
                   -- Obsolete, for backward compatibility
                 | Any Identifier
                 deriving (Eq,Ord,Show, Typeable, Data)

-- Checked                          
builtinType =
  choice $ map try [ integerType -- clause 18
                   , bitStringType -- clause 21
                   , try $ sequenceType -- clause 24
                   , try $ setType -- clause 26
                   , setOrSequenceOfType -- clauses 25 and 27
                   , choiceType -- clause 28
                   , taggedType -- clause 30
                   , enumeratedType -- clause 19
                   , OctetString <$ (reserved "OCTET" *> reserved "STRING") -- clause 22
                   , ObjectIdentifier <$ (reserved "OBJECT" *> reserved "IDENTIFIER") -- clause 31
                   , RelativeOID <$ reserved "RELATIVE-OID" -- clause 32
                   , Real <$ reserved "REAL" -- clause 20
                   , Boolean <$ reserved "BOOLEAN" -- clause 17
                   , Null <$ reserved "NULL" -- clause 23
                   , External <$ reserved "EXTERNAL" -- clause 34
                   , characterStringType -- clause 36
                   , EmbeddedPDV <$ ( reserved "EMBEDDED" *> reserved "PDV" ) -- clause 33
                     -- TODO: , instanceOfType -- ITU-T Rec. X.681 | ISO/IEC 8824-2, Annex C
                     -- TODO: , objectClassFieldType -- ITU-T Rec. X.681 | ISO/IEC 8824-2, 14.1
                   , anyType
                   ]
-- Checked
referencedType = definedType -- clause 13.1
                 <|> usefulType -- clause 41.1
                 <|> selectionType -- clause 29
                 {- TODO: 
                 <|> typeFromObject -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
                 <|> valueSetFromObjects -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
                 -} 
  <?> "ReferencedType"

data NamedType = NamedType Identifier Type deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
namedType = NamedType <$> identifier <*> theType

data Value = 
  -- Five BitString (and OctetString) values:
    HexString StringConst
  | BinaryString StringConst
  | Containing Value
  | IdentifierListBitString [Identifier]
  | IdentifiedNumber Identifier
    
  | BooleanValue Bool
  | CharString StringConst -- TODO: maybe more variants
  | ChoiceValue Identifier Value
  | EmbeddedPDVValue ComponentValueList  
  | EnumeratedValue Identifier
  | ExternalValue ComponentValueList
    -- TODO: InstanceOf value
  | SignedNumber Integer -- this is integerValue
  | NullValue
  | OID [OIDComponent]
    -- OctetString values are covered by BitString values above
    -- Four Real values
  | RealValue Double
  | SequenceRealValue ComponentValueList
  | PlusInfinity
  | MinusInfinity
  | RelativeOIDValue [RelativeOIDComponent]  
    -- Set/Seq and SetOf/SeqOf:
  | SequenceValue ComponentValueList -- this
  | SetValue ComponentValueList
  | NamedValueList ComponentValueList
  | ValueList [Value]
    -- Tagged value is just Value
    -- ReferencedValue constructors:
  | DefinedV DefinedValue
    -- TODO: | ParameterizedType value
    -- TODO: | ParametrizedValueSetType value
    -- TODO: | GeneralizedTime value
    -- TODO: | UTCTime value
    -- Selection type value is just Value
    -- TODO: TypeFromObject constructors                   
    -- TODO: ValueSetFromObjects constructors
    -- TODO: | ValueFromObject ...
    -- Catch-all for Integer and Enumerated types
  | SomeIdentifiedValue Identifier
    -- TODO: catch-all for OID-like values
  deriving (Eq,Ord,Show, Typeable, Data)

-- TODO: incomplete, check for "undefined"
valueOfType (Type t _) = v t
  where
    v (BitString _) = bitStringValue
    v Boolean = booleanValue
    -- Fourteen CharacterString variants
    v CharacterString = undefined
    v BMPString = undefined
    v GeneralString = undefined
    v GraphicString = undefined
    v IA5String = undefined
    v ISO646String = undefined
    v NumericString = undefined
    v PrintableString = undefined
    v TeletexString = undefined
    v T61String = undefined
    v UniversalString = undefined
    v UTF8String = undefined
    v VideotexString = undefined
    v VisibleString = undefined
    v (Choice _) = choiceValue
    v EmbeddedPDV = embeddedPDVValue
    -- Three ENUMERATED variants
    v (SimpleEnumeration _) = enumeratedValue
    v (EnumerationWithException _ _) = enumeratedValue
    v (EnumerationWithExceptionAndAddition _ _ _) = enumeratedValue
    v External = externalValue
    -- TODO: InstanceOf = undefined
    v (TheInteger namedNumber) = integerValue
    v Null = nullValue
    -- TODO: ObjectClassField = undefined
    v ObjectIdentifier = objectIdentifierValue
    v OctetString  = octetStringValue
    v Real = realValue
    v RelativeOID = relativeOIDValue
    v (Sequence _) = sequenceValue
    v (SequenceOf _ _) = sequenceOfValue
    v (Set _) = setValue
    v (SetOf _ _) = setOfValue
    v (Tagged _ _ innerType) = valueOfType innerType
    -- Referenced Type constructors:
    -- Four defined type variants
    v (LocalTypeReference _) = value
    v (ExternalTypeReference _ _) = value
      -- TODO: v ParameterizedType = undefined
      -- TODO: v ParametrizedValueSetType = undefined
      -- Two UsefulType variants:
    v GeneralizedTime = undefined
    v UTCTime = undefined
    v (Selection _ innerType) = valueOfType innerType
      -- TODO: TypeFromObject constructors                    = undefined
      -- TODO: ValueSetFromObjects constructors     = undefined
    v (Any _) = value

-- TODO: When we dont know the type of value we are parsing, we could not distinguish between some
-- of the alternatives without deep context analysis and/or semantical analysis
-- Checked    
value = builtinValue <|> referencedValue {- TODO: <|> objectClassFieldValue -}
        <?> "Value"

-- TODO: re-check this after implementation of all builtin types
builtinValue =
  choice $ map try [ booleanValue -- ok
                   , nullValue -- ok
                   , SignedNumber <$> integer -- Integer identified by 'identifier' is covered by SomeIdentifiedValue
                   , bitStringValue -- This covers OCTET STRING values as well
                   , CharString <$> characterStringValue
                   , setOrSequenceOfValue -- this covers the plain SET/SEQUENCE as well
                   , choiceValue
                     -- embeddedPDVValue and externalValue clash with sequenceValue
                     -- TODO: instanceOfValue
                     -- TODO: objectClassFieldValue
                   , OID <$> oid -- TODO: clashes with RelativeOID
                     -- relativeOIDValue seems to be covered by OID value
                     -- taggedValue is not here because it is just "value" and would lead to infinie loop
                     --   TODO
                   , realValue -- ok
                     -- Two types could have values denoted by a simple identified. Without semantical analysis
                     -- it is impossible to tell them apart. So they are captured by this single catch-all case below
                   , SomeIdentifiedValue <$> identifier -- ok
                   ]

-- Checked
referencedValue = 
  choice [ DefinedV <$> definedValue 
         -- TODO: , valueFromObject -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
         ]

data NamedValue = NamedValue Identifier Value deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
namedValue = NamedValue <$> identifier <*> value
             <?> "NamedValue"
-- }} end of clause 16
-- {{ X.680-0207, clause 17, "Notation for the boolean type"
-- booleanType parser is inlined into basicType parser
-- Checked
booleanValue =
  BooleanValue <$>
  choice [ True <$ reserved "TRUE"
         , False <$ reserved "FALSE"
         ]
-- }} end of clause 17
-- {{ X.680-0207, clause 18, "Notation for the integer type"
-- Checked
integerType = TheInteger <$> (reserved "INTEGER" *> option [] (braces namedNumberList))
  
-- Checked
namedNumberList = commaSep1 namedNumber

data NamedNumber = NamedNumber Identifier Integer
                 | NamedDefinedValue  Identifier DefinedValue
                 deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
namedNumber = 
  choice [ try $ NamedNumber <$> identifier <*> parens signedNumber
         , NamedDefinedValue <$> identifier <*> parens definedValue
         ]
  <?> "NamedNumber"
  
-- Checked  
signedNumber = integer <?> "SignedNumber"

integerValue = 
  choice [ SignedNumber <$> signedNumber
         , IdentifiedNumber <$> identifier
         ]
-- }} end of clause 18
-- {{ X.680-0207, clause 19, "Notation for the enumerated type"
-- Checked
enumeratedType = reserved "ENUMERATED" *> braces enumerations

-- Commented commas are in the ASN.1, but here they are consumed by the preceding parsers
-- Checked
enumerations = 
  choice [ try $ EnumerationWithExceptionAndAddition <$> enumeration <*> ({- comma *>-} symbol "..." *> exceptionSpec) <*> (comma *> enumeration)
         , try $ EnumerationWithException <$> enumeration <*> ({- comma *> -} symbol "..." *> exceptionSpec)
         , SimpleEnumeration <$> enumeration
         ]

-- Checked
enumeration = commaSepEndBy1 enumerationItem

data EnumerationItem = EnumerationItemNumber NamedNumber
                     | EnumerationItemIdentifier Identifier
                     deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
enumerationItem = 
  choice [ try $ EnumerationItemNumber <$> namedNumber
         , EnumerationItemIdentifier <$> identifier
         ]

-- Value parser is inlined into builtinValue parser
enumeratedValue = EnumeratedValue <$> identifier
-- }} end of clause 19
-- {{ X.680-0207, clause 20, "REAL"
-- The type parser inlined into builtinType parser

realValue = 
  choice [ reserved "PLUS-INFINITY" >> return PlusInfinity
         , reserved "MINUS-INFINITY" >> return MinusInfinity
         , RealValue <$> float
         , SequenceRealValue <$> componentValueList
         ]

-- }} end of clause 20
-- {{ X.680-0207, clause 21, "BITSTRING"
-- NamedBitList is really a list of NamedNumbers. See definition of INTEGER for namedNumberList
-- Checked
bitStringType = BitString <$> ( reserved "BIT" *>  reserved "STRING" *> option [] (braces namedNumberList) )

-- Checked
bitStringValue =
  choice [ try $ BinaryString <$> bstring
         , HexString <$> hstring
         , IdentifierListBitString <$> braces (commaSep identifier)
         , Containing <$> (reserved "CONTAINING" *> value)
         ]
-- }} end of clause 21
-- {{ X.680-0207, clause 22, "OCTET STRING"
-- Type parser is inlined in builtinType parser
-- Value parser is also a subset of BITSTRING value parser (hstring, bstring and CONTAINING clauses) and inlined into bitStringValue
-- Checked
octetStringValue = 
  choice [ try $ BinaryString <$> bstring
         , HexString <$> hstring
         , Containing <$> (reserved "CONTAINING" *> value)
         ]
-- }} end of clause 22
-- {{ X.680-0207, clause 23, "NULL"
-- Type parser is inlined in builtinType parser
nullValue = NullValue <$ reserved "NULL"
-- }} end of clause 23
-- {{ X.680-0207, clause 24, "SEQUENCE"

-- Checked, X.680-0207
sequenceType = 
  Sequence <$>
  choice [ try $ Empty <$ ( reserved "SEQUENCE" >> lexeme (char '{') >> lexeme (char '}') )
         , try $ JustException <$> ( reserved "SEQUENCE" *> braces (extensionAndException <* optionalExtensionMarker) )
         , (reserved "SEQUENCE" *> braces componentTypeLists)
         ]

-- Checked
extensionAndException = symbol "..." >> exceptionSpec
  

-- Checked
optionalExtensionMarker = optional $ extensionEndMarker

data ComponentTypeLists = ComponentTypeList [ComponentType]
                        | Empty
                        | JustException  (Maybe ExceptionIdentification)
                        | JustExtensions (Maybe ExceptionIdentification) (Maybe [ExtensionAddition])
                        | ExtensionsAtStart (Maybe ExceptionIdentification) (Maybe [ExtensionAddition]) [ComponentType]
                        | ExtensionsAtEnd [ComponentType] (Maybe ExceptionIdentification) (Maybe [ExtensionAddition])
                        | ExtensionsInTheMiddle [ComponentType] (Maybe ExceptionIdentification) (Maybe [ExtensionAddition]) [ComponentType]
                        deriving (Eq,Ord,Show, Typeable, Data)

-- Commented commas are in the ASN.1, but here they are consumed by the preceding parsers
-- Checked, X.680-0207
componentTypeLists = 
  choice [ try $ ExtensionsInTheMiddle <$> componentTypeList <*> ({- comma *>-} extensionAndException) <*> extensionsAdditions <*> (extensionEndMarker *> comma *> componentTypeList)
         , try $ ExtensionsAtStart <$> extensionAndException <*> extensionsAdditions <*> (extensionEndMarker *> comma *> componentTypeList)
         , JustExtensions <$> extensionAndException <*> extensionsAdditions <* optionalExtensionMarker
         , try $ ExtensionsAtEnd <$> componentTypeList <*> ({- comma *>-} extensionAndException) <*> extensionsAdditions <* optionalExtensionMarker
         , ComponentTypeList <$> componentTypeList
         ]
  

-- If this marker comes after *TypeList, then trailing comma would be consumed by the *TypeList parser.
-- Hence the (optional comma) and not (comma) as was in ASN.1 spec
-- Checked, X.680-0207
extensionEndMarker = optional comma >> symbol "..."

-- TODO: merge with similar code in "CHOICE" type parser
-- Checked, X.680-0207
extensionsAdditions = optionMaybe (comma >> extensionAdditionList)

-- It is hard to ensure that this parser does not consume the trailing coma (and fail).
-- So we let it do that and make subsequent coma optional at call site
-- Checked, X.680-0207
extensionAdditionList = commaSepEndBy1 extensionAddition

data ExtensionAddition = ExtensionAdditionGroup (Maybe Integer) [ComponentType]
                       | ExtensionAdditionType ComponentType
                       deriving (Eq,Ord,Show, Typeable, Data)

-- Checked, X.680-0207
extensionAddition = 
  choice [ extensionAdditionGroup
         , ExtensionAdditionType <$> componentType
         ]
  where
    -- Checked, X.680-0207
    extensionAdditionGroup = ExtensionAdditionGroup <$> ( symbol "[[" *> versionNumber ) <*> componentTypeList <* symbol "]]"

-- Checked, X.680-0207
versionNumber = optionMaybe $ number <* lexeme (char ':')

-- It is hard to ensure that this parser does not consume the trailing coma (and fail).
-- So we let it do that and make subsequent coma optional at call site
-- Checked, X.680-0207
componentTypeList = commaSepEndBy1 componentType

data ComponentType = NamedTypeComponent { element_type::NamedType
                                        , element_presence::Maybe ValueOptionality
                                        } 
                   | ComponentsOf Type deriving (Eq,Ord,Show, Typeable, Data)

-- Three cases of definition of componentType from X.680 are folded into valueOptionality helper parser
-- Checked, X.680-0207
componentType =
  choice [ try $ ComponentsOf <$> (reserved "COMPONENTS" *> reserved "OF" *> theType)
         , NamedTypeComponent <$> namedType <*> valueOptionality
         ]

data ValueOptionality = OptionalValue | DefaultValue Value  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
valueOptionality = optionMaybe $
  choice [ reserved "OPTIONAL" >> return OptionalValue
         , reserved "DEFAULT" >> value >>= return . DefaultValue
         ] 

-- This is also used as SequenceOf value
-- Checked
type ComponentValueList = [NamedValue]
componentValueList = braces (commaSep namedValue)

sequenceValue = SequenceValue <$> componentValueList
-- Value parsing is covered by setOf/seqOf value parser
-- }} end of clause 24
-- {{ X.680-0207, clause 25, "SEQUENCE OF" and clause 27, "SET OF"
-- 'TypeWithConstraint' is merged with SetOfType and SequenceOfType for brevity
-- Checked
setOrSequenceOfType = do  
  constr <- constructor
  c <- optionMaybe setSeqConstraint
  reserved "OF"
  t <- choice [ try $ Right <$> namedType
              , Left <$> theType
              ]
  return $ constr c t
  where
    constructor = ( SetOf <$ reserved "SET" ) <|> ( SequenceOf <$ reserved "SEQUENCE")
    setSeqConstraint =
      choice [ reserved "SIZE" >> constraint -- TODO: This is SizeConstraint, wrap in appropriate constructor
             , constraint
             ]

-- Checked      
setOrSequenceOfValue = 
  choice [ try $ NamedValueList <$> componentValueList
         , ValueList <$> braces (commaSep value)
         ]

sequenceOfValue = setOrSequenceOfValue
setOfValue = setOrSequenceOfValue
-- }} end of clause 25, end of clause 27
-- {{ X.680-0207, clause 26, "SET"
setType = 
  Set <$>
  choice [ try $ Empty <$ ( reserved "SET" >> lexeme (char '{') >> lexeme (char '}') )
         , try $ JustException <$> ( reserved "SET" *> braces ( extensionAndException <* optionalExtensionMarker ) )
         , (reserved "SET" *> braces componentTypeLists)
         ]
-- value parser is also handled by setOrSequenceOfValue
setValue = SetValue <$> componentValueList
-- }} end of clause 26
-- {{ X.680-0207, clause 28, "CHOICE"
-- Checked
choiceType = Choice <$> ( reserved "CHOICE" *> braces alternativeTypeLists )
             <?> "ChoiceType"

data AlternativeTypeLists = SimpleAlternativeTypeList [NamedType] 
                          | AlternativeTypeListWithExtension [NamedType] (Maybe ExceptionIdentification) (Maybe [ExtensionAdditionAlternative])
                          deriving (Eq,Ord,Show, Typeable, Data)

-- Commented commas are in the ASN.1, but here they are consumed by the preceding parsers
-- Checked
alternativeTypeLists = 
  choice [ try $ AlternativeTypeListWithExtension <$> alternativeTypeList <*> {- comma *> -} extensionAndException <*> extensionAdditionAlternatives <* optionalExtensionMarker
         , SimpleAlternativeTypeList <$> alternativeTypeList 
         ]

-- rootAlternativeTypeList is inlined since it has only one production

-- Checked
extensionAdditionAlternatives = optionMaybe (comma >> extensionAdditionAlternativesList)

-- Checked
extensionAdditionAlternativesList = commaSepEndBy1 extensionAdditionAlternative

-- TODO: merge with definitions from SEQUENCE
data ExtensionAdditionAlternative = ExtensionAdditionAlternativesGroup (Maybe Integer) [NamedType]
                                  | ExtensionAdditionAlternativesType NamedType
                                  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
extensionAdditionAlternative =
  choice [ extensionAdditionGroupAlternatives
         , ExtensionAdditionAlternativesType <$> namedType
         ]
-- Checked
extensionAdditionGroupAlternatives = ExtensionAdditionAlternativesGroup <$> ( symbol "[[" *> versionNumber) <*> alternativeTypeList  <* symbol "]]"

-- Checked
alternativeTypeList = commaSepEndBy1 namedType

choiceValue = ChoiceValue <$> identifier <*> ( colon *> value)
-- }} end of clause 28
-- {{ X.680-0207, clause 29, "Selection Types"
selectionType = Selection <$> identifier <*> (symbol "<" *> theType)
                <?> "SelectionType"

-- SelectionType does not have a special value parser
-- }} end of clause 29
-- {{ X.680-0207, clause 30, "Tagged Types"
-- Checked
taggedType = Tagged <$> tag <*> tagType <*> theType

data Tag = Tag (Maybe Class) ClassNumber deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
tag = squares (Tag <$> optionMaybe theClass <*> classNumber)

data ClassNumber = ClassNumber Integer | ClassNumberAsDefinedValue DefinedValue deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
classNumber =
  choice [ ClassNumber <$> number
         , ClassNumberAsDefinedValue <$> definedValue
         ]

data Class = Universal | Application | Private deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
theClass = choice [ Universal <$ reserved "UNIVERSAL"
                  , Application <$ reserved "APPLICATION"
                  , Private <$ reserved "PRIVATE"
                  ]
-- Tagged type does not have value parser - it is just "value"
-- }} end of clause 30
-- {{ X.680-0207, clause 31, "Object Identifier Type"
-- Type parser is trivial and inlined in builtinType parser

objectIdentifierValue = OID <$> oid
-- ObjectIdentifier is replaced with OID for brevity
type OID = [OIDComponent]
-- Checked
oid = braces (many1 oidComponent) <?> "OID"

data OIDComponent = ObjIdDefinedValue DefinedValue | ObjIdNumber Integer | ObjIdNamedNumber NamedNumber | ObjIdName Identifier deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
oidComponent =
  choice [ ObjIdNamedNumber <$> try namedNumber
         , ObjIdName . Identifier <$> try reservedOIDIdentifier
         , ObjIdNumber <$> number
         , ObjIdDefinedValue <$> definedValue
         ]
  <?> "OIDComponent"

-- Checked
reservedOIDIdentifier = do
  i <- choice $ map (try.symbol) $ [ "itu-t", "ccitt", "iso", "joint-iso-itu-t", "joint-iso-ccitt"
                                   , "recommendation", "question", "administration", "network-operator"
                                   , "identified-organization", "standard", "member-body"] ++ map (:[]) ['a'..'z']
  notFollowedBy $ oneOf $ ['a'..'z']++['0'..'9']++"-."
  return i
-- }} end of clause 31
-- {{ X.680-0207, clause 32, "Relative OID"
-- Type parser is primitive and inlined into builtinType
data RelativeOIDComponent =
  RelativeOIDNumber Integer
  | RelativeOIDNamedNumber NamedNumber
  | RelativeOIDDefinedValue DefinedValue
  deriving (Eq,Ord,Show, Typeable, Data)
               
relativeOIDComponent =
  choice [ RelativeOIDNamedNumber <$> try namedNumber
         , RelativeOIDNumber <$> number
         , RelativeOIDDefinedValue <$> definedValue
         ]

relativeOIDValue = RelativeOIDValue <$> braces (many1 relativeOIDComponent)
-- }} end of clause 32  
-- {{ X.680-0207, clause 33, "Embedded PDV"
-- Type parser is trivial and embedded in builtinType
embeddedPDVValue = do
  (SequenceValue cvl) <- sequenceValue
  return $ EmbeddedPDVValue cvl
-- }} end of clause 33
-- {{ X.680-0207, clause 34, "EXTERNAL"
-- Type parser is trivial and embedded in builtinType
externalValue = do
  (SequenceValue cvl) <- sequenceValue
  return $ ExternalValue cvl
-- }} end of clause 34
-- {{ X.680-0207, clause 35, TODO
-- {{ X.680-0207, clause 36, TODO
-- {{ X.680-0207, clause 37, TODO
-- {{ X.680-0207, clause 38, TODO
-- {{ X.680-0207, clause 39, TODO
-- {{ X.680-0207, clause 40, TODO
-- {{ X.680-0207, clause 41, TODO
-- {{ X.680-0207, clause 42, TODO
-- {{ X.680-0207, clause 43, TODO
-- {{ X.680-0207, clause 44, TODO
-- {{ X.680-0207, clause 45, TODO
-- {{ X.680-0207, clause 46, TODO
-- {{ X.680-0207, clause 47, TODO
-- {{ X.680-0207, clause 48, TODO
-- {{ X.680-0207, clause 49, "The exception identifier"
-- Checked
exceptionSpec = 
  optionMaybe ( lexeme (char '!') >> exceptionIdentification )
                
data ExceptionIdentification = ExceptionNumber Integer
                             | ExceptionValue DefinedValue
                             | ExceptionTypeAndValue Type Value
                             deriving (Eq,Ord,Show, Typeable, Data)
exceptionIdentification =
  choice [ ExceptionNumber <$> signedNumber 
         , ExceptionValue <$> definedValue
         , ExceptionTypeAndValue <$> theType <*> (colon *> value)
         ]
-- }} end of clause 49

data ValueSet = ValueSet TODO deriving (Eq,Ord,Show, Typeable, Data)
valueSet = braces elementSetSpecs
elementSetSpecs = undefined

newtype TypeName = TypeName Identifier deriving (Eq,Ord,Show, Typeable, Data)
type NumberOrDefinedValue = Either Integer DefinedValue
data ElementType = NamedElementType { _element_name::TypeName
                                    , _element_body::Type
                                    , _element_presence::Maybe ValueOptionality
                                    } 
                 | ComponentsOf_ Type deriving (Eq,Ord,Show, Typeable, Data)
newtype ValueName = ValueName Identifier deriving (Eq,Ord,Show, Typeable, Data)
data SizeConstraint = SizeConstraint SubtypeSpec | UndefinedSizeContraint deriving (Eq,Ord,Show, Typeable, Data)


-- { Chapter 8.1, "Lexical tokens in ASN.1"
data StringConst = StringConst (Maybe Char) String deriving (Eq,Ord,Show, Typeable, Data)
stringConst allowedSet marker = 
  do { char '\'' ; body <- many (oneOf allowedSet) ; char '\''; char marker ; return (StringConst (Just marker) body) } 

type BString = StringConst
bstring = stringConst "01" 'B' <?> "bstring"

type HString = StringConst
hstring = stringConst "0123456789ABCDEFabcdef" 'H' <?> "hstring"

cstring = 
  do { char '"'; s <- anyChar `manyTill` (char '"' ); return (StringConst Nothing s) } <?> "cstring"

lcaseFirstIdent = do { i <- parsecIdent
                     ; when (isUpper $ head i) $ unexpected "uppercase first letter"
                     ; return i
                     }

ucaseFirstIdent = do { i <- parsecIdent
                     ; when (not . isUpper $ head i) $ unexpected "lowercase first letter"
                     ; return i
                     }

ucaseIdent = do { i <- parsecIdent
                ; when (not $ all isUpper $ filter isAlpha i) $ unexpected "lowercase letter"
                ; return i
                }
-- }

taggedValue = value

-- UsefulObjectClassReference is inlined in definedObjectClass
data DefinedObjectClass = ExternalObjectClassReference ModuleReference ObjectClassReference
                        | LocalObjectClassReference ObjectClassReference
                        | TypeIdentifier
                        | AbstractSyntax
                        deriving (Eq,Ord,Show, Typeable, Data)
definedObjectClass =
  choice [ try $ ExternalObjectClassReference <$> moduleReferenceAndDot <*> objectclassreference
         , LocalObjectClassReference <$> objectclassreference
         , TypeIdentifier <$ reserved "TYPE-IDENTIFIER"
         , AbstractSyntax <$ reserved "ABSTRACT-SYNTAX"
         ]

data DefinedObject = ExternalObjectReference ModuleReference ObjectReference
                  | LocalObjectReference ObjectReference
                  deriving (Eq,Ord,Show, Typeable, Data)
definedObject = 
  choice [ try $ ExternalObjectReference <$> moduleReferenceAndDot <*> objectreference
         , LocalObjectReference <$> objectreference
         ] <?> "DefinedObject"
  
data DefinedObjectSet = ExternalObjectSetReference ModuleReference ObjectSetReference
                  | LocalObjectSetReference ObjectSetReference
                  deriving (Eq,Ord,Show, Typeable, Data)
definedObjectSet = 
  choice [ try $ ExternalObjectSetReference <$> moduleReferenceAndDot <*> objectsetreference
         , LocalObjectSetReference <$> objectsetreference
         ] <?> "DefinedObjectSet"
-- }} end of section 9.3
-- } end of chapter 9
-- { Chapter 10, "Basic types"
-- {{ Section 10.1, "BOOLEAN type"
-- }} end of section 10.1
-- {{ Section 10.2, "NULL type"
-- parsers for type and value are inlined into builtinType and builtinValue
-- }} end of section 10.2
-- {{ Section 10.3, "INTEGER type"
-- }} end of section 10.3
-- {{ Section 10.4, "The ENUMERATED type"
-- TODO: values
-- }} end of section 10.4
-- {{ Section 12.3, "The constructor SET"
-- TODO: values
-- }} end of section 12.3
-- {{ Section 12.4 and 12.5, "The constructor SEQUENCE OF" and "The constructor SET OF"
-- }}





data TagDefault = ExplicitTags | ImplicitTags | AutomaticTags deriving (Eq,Ord,Show, Typeable, Data)
data TagType = Explicit | Implicit deriving (Eq,Ord,Show, Typeable, Data)
tagType = 
  optionMaybe $
  choice [ Explicit <$ reserved "EXPLICIT"
         , Implicit <$ reserved "IMPLICIT"
         ]

  

newtype TypeReference = TypeReference String deriving (Eq,Ord,Show, Typeable, Data)


embeddedPDVType = undefined
instanceOfType = undefined
objectClassFieldType = undefined
relativeOIDType = undefined
typeFromObject = undefined
valueSetFromObjects = undefined
objectDescriptor = undefined

-- Dubuisson 9.1.2

-- Dubuisson 11.15.2
usefulType = 
  choice [ reserved "GeneralizedTime" >> return GeneralizedTime
         , reserved "UTCTime" >> return UTCTime
         ]
  
moduleReferenceAndDot = 
  do { 
     ; mref <- modulereference 
     ; char '.'
     ; return (mref)
     }

-- Dubuisson 11.13
characterStringType = 
  choice [ reserved "BMPString" >> return BMPString
         , reserved "GeneralString" >> return GeneralString
         , reserved "GraphicString" >> return GraphicString
         , reserved "IA5String" >> return IA5String 
         , reserved "ISO646String" >> return ISO646String
         , reserved "NumericString" >> return NumericString 
         , reserved "PrintableString" >> return PrintableString
         , reserved "TeletexString" >> return TeletexString 
         , reserved "T61String" >> return T61String
         , reserved "UniversalString" >> return UniversalString 
         , reserved "UTF8String" >> return UTF8String
         , reserved "VideotexString" >> return VideotexString 
         , reserved "VisibleString" >> return VisibleString 
         , reserved "CHARACTER" >> reserved "STRING" >> return CharacterString
         ]







-- Dubuisson 12.6.2
-- }}}  

elementTypeList = commaSep1 elementType
                  <?> "ElementTypeList"

elementType =
  choice $ map try 
           [ componentsType >>= return . ComponentsOf_
           , do { id <- option UndefinedIdentifier (try identifier)
                ; t <- theType
                ; presence <- valueOptionality
                ; return (NamedElementType (TypeName id) t presence)
                }
           ]
           
componentsType :: Parser Type
componentsType =
  do {
     ; reserved "COMPONENTS"
     ; reserved "OF"
     ; t <- theType
     ; return t
     }
     <?> "ComponentsType"




           
anyType = Any <$>
  do { reserved "ANY"
     ; option UndefinedIdentifier (  do {reserved "DEFINED"  ;  reserved "BY"  ; identifier  } ) 
     }
     <?> "AnyType"

-- Dubuisson, 9.1.2
objectClassAssignment = do
  ref <- objectclassreference
  reservedOp "::="
  c <- objectClass
  return $ ObjectClassAssignment ref c

-- Dubuisson, 15.2.2
data ObjectClass = ObjectClassDefn [FieldSpec] | DefinedObjectClassDefn DefinedObjectClass deriving (Eq,Ord,Show, Typeable, Data)

-- Dubuisson, 15.2.2
objectClass = 
  choice [ definedObjectClass >>= return . DefinedObjectClassDefn
         , objectClassDefn 
         -- , parametrizedObjectClass
         ]
  <?> "ObjectClass"

parametrizedObjectClass = undefined

-- Dubuisson, 15.2.2
objectClassDefn = do
  reserved "CLASS"
  ObjectClassDefn <$> (braces $ commaSep1 field)
  -- TODO : withSyntaxSpec  
  
-- Dubuisson, 15.2.2, TODO
data FieldSpec = TypeField TypeFieldReference (Maybe TypeOptionality) 
               | FixedTypeValueField ValueFieldReference Type Bool {-unique or not-} (Maybe ValueOptionality)
               | ObjectField ObjectFieldReference DefinedObjectClass (Maybe ObjectOptionality)
               | ObjectSetField ObjectSetFieldReference DefinedObjectClass (Maybe ObjectSetOptionality)
               | FixedTypeValueSetField ValueSetFieldReference Type (Maybe ValueSetOptionality)
               deriving (Eq,Ord,Show, Typeable, Data)
data TypeOptionality = OptionalType | DefaultType Type deriving (Eq,Ord,Show, Typeable, Data)
data ObjectOptionality = OptionalObject | DefaultObject Object deriving (Eq,Ord,Show, Typeable, Data)
data ObjectSetOptionality = OptionalObjectSet | DefaultObjectSet {- TODO: ObjectSet -} deriving (Eq,Ord,Show, Typeable, Data)
data ValueSetOptionality = OptionalValueSet | DefaultValueSet {- TODO: ValueSet -} deriving (Eq,Ord,Show, Typeable, Data)

-- Dubuisson, 15.2.2
{-
Table 15.1 says:
If the field name   and if it is followed by   then the field of the
starts with                                    object contains
--------------------------------------------------------------------
&Upper-case       nothing                     a type

&lower-case       a type or a type reference  a fixed-type value
                  (Upper-case)

&lower-case       a type field (&Upper-case)  a variable-type value

&Upper-case       a type or a type reference  a fixed-type value set
                  (Upper-case)

&Upper-case       a type field (&Upper-case)  a variable-type value set
&lower-case       a class name (UPPER-CASES)  an information object
&Upper-case       a class name (UPPER-CASES)  an information object set
-}

-- Warning: ORDER IS IMPORTAND HERE
field = try objectField
        <|> try objectSetField
        <|> try fixedTypeValueField
        -- TODO: <|> variableTypeValueField
        <|> try fixedTypeValueSetField
        -- TODO: <|> variableTypeValueSetField
        <|> typeField
        <?> "Field"
        
-- Dubuisson, 15.2.2
typeField = do
  ref <- typefieldreference
  optionality <- optionMaybe $ 
                 choice [ reserved "OPTIONAL" >> return OptionalType
                        , reserved "DEFAULT" >> theType >>= return . DefaultType
                        ]
  return $ TypeField ref optionality
  <?> "TypeField"

-- Dubuisson, 15.2.2
fixedTypeValueField = do
  ref <- valuefieldreference
  t <- theType
  u <- option False (reserved "UNIQUE" >> return True)
  vo <- valueOptionality
  return $ FixedTypeValueField ref t u vo
  <?> "FixedTypeValueField"

-- Dubuisson, 15.2.2
fixedTypeValueSetField = do
  ref <- valuesetfieldreference 
  t <- theType
  o <- valueSetOptionality
  return $ FixedTypeValueSetField ref t o
  
valueSetOptionality =
  optionMaybe $
  choice [ reserved "OPTIONAL" >> return OptionalValueSet
         -- , reserved "DEFAULT" >> valueSet >>= return . DefaultValueSet
         ] 
  
-- Dubuisson, 15.2.2
objectField = do
  ref <- objectfieldreference
  c <- definedObjectClass
  oo <- objectOptionality
  return $ ObjectField ref c oo
  
-- Dubuisson, 15.2.2
objectOptionality = optionMaybe $
  choice [ reserved "OPTIONAL" >> return OptionalObject
         , reserved "DEFAULT" >> object >>= return . DefaultObject
         ] 

-- Dubuisson, 15.2.2
objectSetField = do
  ref <- objectsetfieldreference 
  c <- definedObjectClass
  oo <- objectSetOptionality
  return $ ObjectSetField ref c oo

objectSetOptionality = optionMaybe $
  choice [ reserved "OPTIONAL" >> return OptionalObjectSet
         -- TODO: , reserved "DEFAULT" >> objectSet >>= return . DefaultObjectSet
         ] 
  

-- Dubuisson, 9.3.2

-- 
objectAssignment = do
  or <- objectreference 
  doc <- definedObjectClass 
  reservedOp "::=" 
  o <- object
  return $ ObjectAssignment or doc o

-- Dubuisson, 15.2.2, TODO
data Object = ObjectDefn [FieldSetting]
            deriving (Eq,Ord,Show, Typeable, Data)
object =
  choice [ objectDefn
         -- TODO: , definedObject
         -- TODO: , objectFromObject
         -- TODO: , parametrizedObject
         ]
 
objectDefn = 
  choice [ ObjectDefn <$> defaultSyntax 
           -- TODO: , definedSyntax
         ]
  where
    defaultSyntax = braces $ commaSep fieldSetting
    
data FieldSetting = FieldSetting PrimitiveFieldName Setting
                  deriving (Eq,Ord,Show, Typeable, Data)
fieldSetting = do
  pfn <- primitiveFieldName
  s <- setting
  return $ FieldSetting pfn s

data Setting = TypeSetting Type | ValueSetting Value | ObjectSetting Object
             deriving (Eq,Ord,Show, Typeable, Data)
setting = 
  choice [ TypeSetting <$> theType
         , ValueSetting <$> value
         -- TODO: , valueSet
         , ObjectSetting <$> object
         -- TODO: , objectSet
         ]
  
data PrimitiveFieldName = PrimTFR TypeFieldReference | PrimVFR ValueFieldReference | PrimOFR ObjectFieldReference
                        deriving (Eq,Ord,Show, Typeable, Data)
primitiveFieldName =
  choice [ PrimTFR <$> try typefieldreference
         , PrimOFR <$> objectfieldreference           
         , PrimVFR <$> valuefieldreference
         -- TODO: , valuesetfieldreference
         -- TODO: , objectsetfieldreference
         ]

-- unchecked, 15.7.2
type SubtypeSpec = [SubtypeValueSet]

-- unchecked, 15.7.2
constraint :: Parser SubtypeSpec
constraint = parens subtypeValueSetList
              <?> "SubtypeSpec"

subtypeValueSetList = sepBy1 subtypeValueSet (symbol "|") <?> "SubtypeValueSetList"

data ValueRangeElement = MinValue | MaxValue | UndefinedValue | Value Value deriving (Eq,Ord,Show, Typeable, Data)

data SubtypeValueSet = ValueRange { range_from::ValueRangeElement
                                  , range_to::ValueRangeElement
                                  }
                     | ContainedSubType Type
                     | PermittedAlphabet SubtypeSpec
                     | SizeConstr SizeConstraint
                     | SingleTypeConstraint SubtypeSpec 
                     | MultipleTypeConstaint TypeConstraints
                       deriving (Eq,Ord,Show, Typeable, Data)

subtypeValueSet :: Parser SubtypeValueSet
subtypeValueSet =
  do {
      choice $ map try  [ valueRange
                        , containedSubtype >>= return . ContainedSubType
                        , permittedAlphabet >>= return . PermittedAlphabet
                        , sizeConstraint >>= return . SizeConstr
                        , innerTypeConstraints
                        ]
     }
     <?> "SubtypeValueSet"

containedSubtype =
  do { reserved "INCLUDES"  
     ; theType >>= return
     }
     <?> "ContainedSubtype"

singleValue = value <?> "SingleValue"

valueRange =
  do { from <- choice [ value >>= return . Value
                      , do reserved "MIN"
                           return MinValue
                      ]
     ; to <- option UndefinedValue ( do optional (symbol "<")
                                        dot
                                        dot
                                        optional (symbol "<") 
                                        choice [ value >>= return . Value
                                               , do reserved "MAX"
                                                    return MaxValue
                                               ]
                                   )
     ; return (ValueRange from to)
     }
     <?> "ValueRange"

-- Dubuisson 13.5.2
sizeConstraint :: Parser SizeConstraint
sizeConstraint =
  do {
     ; reserved "SIZE" 
     ; spec <- constraint
     ; return (SizeConstraint spec)
     }
     <?> "SizeConstraint"

-- Dubuisson, 13.6.2
permittedAlphabet = do
  reserved "FROM" 
  constraint
  <?> "PermittedAlphabet"

innerTypeConstraints =
  do { reserved "WITH" 
     ; choice [ do { reserved "COMPONENT" 
                   ; singleTypeConstraint >>= return . SingleTypeConstraint
                   }
              , do { reserved "COMPONENTS" 
                   ; multipleTypeConstraints >>= return . MultipleTypeConstaint
                   }
              ]
     }
     <?> "InnerTypeConstraints"

singleTypeConstraint = constraint <?> "SingleTypeConstraint"

multipleTypeConstraints =
  do {
      braces(  do { optional (  reservedOp "..." >> reservedOp "," ) 
                  ; typeConstraints
                  }
            )
     }
     <?> "MultipleTypeConstraints"

type TypeConstraints = [NamedConstraint]
typeConstraints = commaSep1 namedConstraint <?> "TypeConstraints"

data NamedConstraint = NamedConstraint Identifier Constraint  deriving (Eq,Ord,Show, Typeable, Data)
namedConstraint =
  do { id <- option UndefinedIdentifier identifier
     ; c <- otherConstraint
     ; return (NamedConstraint id c)
     }
     <?> "NamedConstraint"

data Constraint = Constraint ValueConstraint PresenceConstraint deriving (Eq,Ord,Show, Typeable, Data)
otherConstraint = 
  do { vc <- option UndefinedVC (valueConstraint)  
     ; pc <- option UndefinedContraint (presenceConstraint)
     ; return (Constraint vc pc)
     }
     <?> "Constraint"

data ValueConstraint = DefinedVC SubtypeSpec | UndefinedVC deriving (Eq,Ord,Show, Typeable, Data)
valueConstraint = constraint >>= return . DefinedVC <?> "ValueConstraint"

data PresenceConstraint = PresentConstraint 
                        | AbsentConstraint 
                        | OptionalConstraint 
                        | UndefinedContraint deriving (Eq,Ord,Show, Typeable, Data)
presenceConstraint =
  choice [ do reserved "PRESENT"; return PresentConstraint
         , do reserved "ABSENT"; return AbsentConstraint
         , do reserved "OPTIONAL"; return OptionalConstraint
         ]
  <?> "PresenceConstraint"






data TODO = TODO deriving (Eq,Ord,Show, Typeable, Data)


binaryString = bstring <?> "BinaryString"
hexString = hstring  <?> "HexString"
characterStringValue = cstring <?> "CharacteStringValue"
number = natural <?> "number"
data Identifier = Identifier String 
                   | UndefinedIdentifier
                     deriving (Eq,Ord,Show, Typeable, Data)
identifier = lcaseFirstIdent >>= return . Identifier <?> "identifier"

data ModuleReference = ModuleReference String deriving (Eq,Ord,Show, Typeable, Data)
modulereference = ucaseFirstIdent >>= return . ModuleReference <?> "modulereference"

typereference = ucaseFirstIdent >>= return . TypeReference <?> "typereference"

newtype TypeFieldReference = TypeFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
typefieldreference = char '&' >> ucaseFirstIdent >>= return . TypeFieldReference <?> "typefieldreference"

newtype ObjectClassReference = ObjectClassReference String deriving (Eq,Ord,Show, Typeable, Data)
objectclassreference = ucaseIdent >>= return . ObjectClassReference

newtype ValueFieldReference = ValueFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
valuefieldreference = char '&' >> lcaseFirstIdent >>= return . ValueFieldReference

newtype ObjectFieldReference = ObjectFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
objectfieldreference = char '&' >> lcaseFirstIdent >>= return . ObjectFieldReference

newtype ObjectReference = ObjectReference String deriving (Eq,Ord,Show, Typeable, Data)
objectreference = lcaseFirstIdent >>= return . ObjectReference

newtype ObjectSetReference = ObjectSetReference String deriving (Eq,Ord,Show, Typeable, Data)
objectsetreference = ucaseFirstIdent >>= return . ObjectSetReference

newtype ObjectSetFieldReference = ObjectSetFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
objectsetfieldreference = char '&' >> ucaseFirstIdent >>= return . ObjectSetFieldReference

newtype ValueSetFieldReference = ValueSetFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
valuesetfieldreference = char '&' >> ucaseFirstIdent >>= return . ValueSetFieldReference

newtype ValueReference = ValueReference String deriving (Eq,Ord,Show, Typeable, Data)
valuereference = lcaseFirstIdent >>= return . ValueReference


data DefinedMacroName = ObjectType | TextualConvention deriving (Eq,Ord,Show, Typeable, Data)
definedMacroName =
  do {
      choice [ reserved "OBJECT-TYPE" >> return ObjectType 
             , reserved "TEXTUAL-CONVENTION" >> return TextualConvention
             ]
     }
     <?> "DefinedMacroName"

-----------------------------------------------------------
-- Tokens
-- Use qualified import to have token parsers on toplevel
-----------------------------------------------------------
asn1Style
  = emptyDef
    { commentStart = "--"
    , commentEnd = "--"
    , nestedComments = False
    , identStart     = letter
    , identLetter = alphaNum <|> oneOf "-"
    , caseSensitive = True
      -- X.680-0207, 11.27
    , reservedNames = [ "ABSENT",  "ENCODED",  "INTEGER",  "RELATIVE-OID", 
                        "ABSTRACT-SYNTAX",  "END",  "INTERSECTION",  "SEQUENCE", 
                        "ALL",  "ENUMERATED",  "ISO646String",  "SET", 
                        "APPLICATION",  "EXCEPT",  "MAX",  "SIZE", 
                        "AUTOMATIC",  "EXPLICIT",  "MIN",  "STRING", 
                        "BEGIN",  "EXPORTS",  "MINUS-INFINITY",  "SYNTAX", 
                        "BIT",  "EXTENSIBILITY",  "NULL",  "T61String", 
                        "BMPString",  "EXTERNAL",  "NumericString",  "TAGS", 
                        "BOOLEAN",  "FALSE",  "OBJECT",  "TeletexString", 
                        "BY",  "FROM",  "ObjectDescriptor",  "TRUE", 
                        "CHARACTER",  "GeneralizedTime",  "OCTET",  "TYPE-IDENTIFIER", 
                        "CHOICE",  "GeneralString",  "OF",  "UNION", 
                        "CLASS",  "GraphicString",  "OPTIONAL",  "UNIQUE", 
                        "COMPONENT",  "IA5String",  "PATTERN",  "UNIVERSAL", 
                        "COMPONENTS",  "IDENTIFIER",  "PDV",  "UniversalString", 
                        "CONSTRAINED",  "IMPLICIT",  "PLUS-INFINITY",  "UTCTime", 
                        "CONTAINING",  "IMPLIED",  "PRESENT",  "UTF8String", 
                        "DEFAULT",  "IMPORTS",  "PrintableString",  "VideotexString", 
                        "DEFINITIONS",  "INCLUDES",  "PRIVATE",  "VisibleString", 
                        "EMBEDDED",  "INSTANCE",  "REAL",  "WITH" ]
                      
    , reservedOpNames = [ "::=", ",", "...", "!" ]
    }

asn1            = P.makeTokenParser asn1Style
            
whiteSpace      = P.whiteSpace asn1
lexeme          = P.lexeme asn1
symbol          = P.symbol asn1
parsecIdent     = P.identifier asn1
reserved        = P.reserved asn1
reservedOp      = P.reservedOp asn1
comma           = P.comma asn1
commaSep        = P.commaSep asn1
commaSep1       = P.commaSep1 asn1
braces          = P.braces asn1
squares         = P.squares asn1
parens          = P.parens asn1
semi            = P.semi asn1
colon           = P.colon asn1
natural         = P.natural asn1
integer         = P.integer asn1
float           = P.float asn1
dot             = P.dot asn1
commaSepEndBy p = p `sepEndBy` comma
commaSepEndBy1 p = p `sepEndBy1` comma

-- Local Variables: 
-- outline-regexp:"-- [{]\+"
-- End: