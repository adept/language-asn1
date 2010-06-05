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
import Data.Char (isUpper, isAlpha, isSpace, ord)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (fromJust)

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
-- Comments between -- and -- are replaced by /* */ comments.
fixupComments = do
  inp <- getInput
  setInput $ fixup inp

fixup = repl 0 False
  where
    -- repl `level of star comments' isInDashComment                                  
    repl 0 True []             = "*/"
    repl starComment inDashComment [] = []        
    repl 0 False (c:'-':'-':rest) | isSpace c  = c:'/':'*':(repl 0 True rest)
                                  | otherwise = c:'-':'-':(repl 0 False rest)
    repl 0 False ('-':'-':rest) = '/':'*':(repl 0 True rest)
    repl 0 True ('-':'-':rest) = '*':'/':(repl 0 False rest)
    repl 0 True (c:rest) | isNewline c = '*':'/':c:(repl 0 False rest)
    repl n False ('/':'*':rest) = '/':'*':(repl (n+1) False rest)
    repl n False ('*':'/':rest) = '*':'/':(repl (n-1) False rest)
    repl n inDashComment (c:rest) = c:(repl n inDashComment rest)
        
isNewline c | ord c >= 10 && ord c <= 13 = True
            | otherwise = False
-- }}
        
-- {{ X.680-0207,  Clause 11, "ASN.1 lexical items"
newtype TypeReference = TypeReference String deriving (Eq,Ord,Show, Typeable, Data)
_typereference = ucaseFirstIdent
typereference = TypeReference <$> _typereference

newtype Identifier = Identifier String deriving (Eq,Ord,Show, Typeable, Data)
_identifier = lcaseFirstIdent
identifier = Identifier <$> _identifier

-- TODO: distinguished from identifier only by context - check this
newtype ValueReference = ValueReference String deriving (Eq,Ord,Show, Typeable, Data)
valuereference = ValueReference <$> _identifier
_valuereference = _identifier

-- TODO: distinguished from typereference only by context - check this
data ModuleReference = ModuleReference String deriving (Eq,Ord,Show, Typeable, Data)
modulereference = ModuleReference <$> _typereference

-- For comment handling see `fixupComments'

number = natural <?> "number"
realnumber = 
  choice [ try $ negate <$> (char '-' *> float)
         , try float
         , fromInteger <$> integer
         ]
  where
    float = P.float asn1

type BString = BinString
bstring = binString " 01" 'B' <?> "bstring"

type HString = BinString
hstring = binString " 0123456789ABCDEF" 'H' <?> "hstring"

data BinString = BinString Char String deriving (Eq,Ord,Show, Typeable, Data)
binString allowedSet marker = BinString marker <$> ( ( symbol "'" *> (filter (not.isSpace) <$> many (oneOf allowedSet) ) ) <* char '\'' <* char marker <* whiteSpace)

newtype CString = CString String deriving (Eq,Ord,Show, Typeable, Data)
cstring = CString <$> (filter (not.isNewline) <$> intercalate "\"" <$> many1 ( char '"' *> anyChar `manyTill` (char '"'))) <* whiteSpace

-----------------------------------------------------------
-- Token parser
-----------------------------------------------------------
lcaseFirstIdent = do 
  i <- parsecIdent
  when (isUpper $ head i) $ unexpected "uppercase first letter"
  when (last i == '-') $ unexpected "trailing hyphen"
  return i

ucaseFirstIdent = do 
  i <- parsecIdent
  when (not . isUpper $ head i) $ unexpected "lowercase first letter"
  when (last i == '-') $ unexpected "trailing hyphen"
  return i

ucaseIdent = do 
  i <- parsecIdent
  when (not $ all isUpper $ filter isAlpha i) $ unexpected "lowercase letter"
  when (last i == '-') $ unexpected "trailing hyphen"
  return i

asn1Style
  = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , nestedComments = True
    , identStart     = letter
    , identLetter = alphaNum <|> ( char '-' <* notFollowedBy (char '-') )
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
    }

asn1            = P.makeTokenParser asn1Style
            
whiteSpace      = P.whiteSpace asn1
symbol          = P.symbol asn1
parsecIdent     = P.identifier asn1
reserved        = P.reserved asn1
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
dot             = P.dot asn1
commaSepEndBy1 p = p `sepEndBy1` comma
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
         <*> (symbol "::=" *> reserved "BEGIN" *> moduleBody) <* reserved "END"  
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
data TagDefault = ExplicitTags | ImplicitTags | AutomaticTags deriving (Eq,Ord,Show, Typeable, Data)
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
            | ValueReferenceSymbol ValueReference
            -- TODO: it is impossible to distinguish TypeReference and ValueReference syntactically
            | ObjectClassReferenceSymbol ObjectClassReference
            | ObjectReferenceSymbol ObjectReference
            | ObjectSetReferenceSymbol ObjectSetReference
            deriving (Eq,Ord,Show, Typeable, Data)
theSymbol =
 choice ( map try [ TypeReferenceSymbol <$> typereference
                  , ObjectClassReferenceSymbol <$> objectclassreference
                  , ObjectReferenceSymbol <$> objectreference
                  , ObjectSetReferenceSymbol <$> objectsetreference
                  , ValueReferenceSymbol <$> valuereference
                  ] ) <* parametrizedDesignation
 where
   -- Checked, X.683-0207, 9.1
   parametrizedDesignation = optional (symbol "{" >> symbol "}")

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
                | ObjectSetAssignment ObjectSetReference DefinedObjectClass ObjectSet
                -- TODO: | ParameterizedAssignment
                  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
assignment = 
  choice $ map try [ objectAssignment
                   , valueAssignment
                   , typeAssignment
                   , objectClassAssignment
                   , valueSetTypeAssignment
                   , objectSetAssignment
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

moduleReferenceAndDot = modulereference <* dot

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
typeAssignment = TypeAssignment <$> typereference <*> (symbol "::=" *> theType)
                 <?> "TypeAssignment"

-- Checked
valueAssignment = do
  ref <- valuereference 
  t <- theType 
  v <- (symbol "::=" *> valueOfType t)
  return $ ValueAssignment ref t v

-- Checked
valueSetTypeAssignment = do
  tr <- typereference
  t <- theType
  ValueSetTypeAssignment tr t <$> (symbol "::=" *> valueSet (Just t))
  where
    -- This alternative is defined in X.680-0207, clause 15.8
    valueSetOrAlternative = valueSet Nothing <|> parens (elementSetSpecs Nothing) -- TODO: type propagation
    
type ValueSet = ElementSets
valueSet t = braces $ elementSetSpecs t
-- }} end of clause 15
-- {{ X.680-0207, clause 16, "Definition of types and values"
-- ConstrainedType (clause 45) is merged in other parsers: "Type Constraint" alternative is encoded here,
-- and TypeWithConstraint in implemented in SetOf/SequenceOf parsers
data Type = Type { type_id::BuiltinType
                 , subtype::Maybe Constraint
                 }
               deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
theType = do
  t <- builtinType <|> referencedType
  Type t <$> optionMaybe (constraint $ Just $ Type t Nothing)
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
                 | InstanceOf DefinedObjectClass -- X.681 annex C
                 | TheInteger [NamedNumber]
                 | Null
                 | ObjectClassField DefinedObjectClass FieldName -- X.681 clause 14
                 | ObjectIdentifier
                 | OctetString 
                 | Real
                 | RelativeOID
                 | Sequence ComponentTypeLists
                 | SequenceOf (Maybe Constraint) (Either Type NamedType)
                 | Set ComponentTypeLists
                 | SetOf (Maybe Constraint) (Either Type NamedType)
                 | Tagged Tag (Maybe TagType) Type
                   -- Referenced Type constructors:
                   -- Four defined type variants
                 | LocalTypeReference TypeReference
                 | ExternalTypeReference ModuleReference TypeReference
                   -- TODO: | ParameterizedType
                   -- TODO: | ParametrizedValueSetType
                   -- Three UsefulType variants:
                 | GeneralizedTime
                 | UTCTime
                 | ObjectDescriptor
                 | Selection Identifier Type
                 | TypeFromObject ReferencedObjects FieldName
                 | ValueSetFromObjects ReferencedObjects FieldName
                   -- Obsolete, for backward compatibility
                 | Any (Maybe Identifier)
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
                   , instanceOfType -- ITU-T Rec. X.681 | ISO/IEC 8824-2, Annex C
                   , objectClassFieldType -- ITU-T Rec. X.681 | ISO/IEC 8824-2, 14.1
                   , anyType
                   ]
-- Checked
referencedType = try definedType -- clause 13.1
                 <|> usefulType -- clause 41.1
                 <|> selectionType -- clause 29
                 <|> typeFromObject -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
                 <|> valueSetFromObjects -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
 
  <?> "ReferencedType"

data NamedType = NamedType Identifier Type deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
namedType = NamedType <$> identifier <*> theType

data Value = 
  -- Five BitString (and OctetString) values:
    HexString BinString
  | BinaryString BinString
  | Containing Value
  | IdentifierListBitString [Identifier]
  | IdentifiedNumber Identifier
    
  | BooleanValue Bool
    -- Two CharacterString values:
  | RestrictedCharacterStringValue [CharsDefn]
  | UnrestrictedCharacterStringValue ComponentValueList
  | ChoiceValue Identifier Value
  | EmbeddedPDVValue ComponentValueList  
  | EnumeratedValue Identifier
  | ExternalValue ComponentValueList
  | InstanceOfValue ComponentValueList
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
  | SequenceOfValue (Either [Value] ComponentValueList)
  | SetOfValue (Either [Value] ComponentValueList)
    -- Tagged value is just Value
    -- ReferencedValue constructors:
  | DefinedV DefinedValue
    -- TODO: | ParameterizedType value
    -- TODO: | ParametrizedValueSetType value
  | GeneralizedTimeValue CString -- TODO: do better, with components
  | UTCTimeValue CString -- TODO: do better, with components
  | ObjectDescriptorValue CString -- TODO: do better, with components
    -- Selection type value is just Value
  -- TODO: | TypeFromObjectValue - see X.681 clause 15
  -- TODO: | ValueSetFromObjectsValue - see X.681 clause 15
  | ValueFromObject ReferencedObjects FieldName
    -- Two ObjectClassFieldValue variants:
  | OpenTypeFieldValue Type Value
  | FixedTypeFieldValue Value
    
    -- Constructors for ambiguous values
  | SomeNumber Double -- Integer or Real
  | SomeNamedValueList ComponentValueList -- SequenceRealValue, SequenceValue, SequenceOfValue (named values), SetValue, SetOfValue (named values)
  | SomeValueList [Value] -- IdentifierListBitString, SequenceOfValue (values only), SetOfValue (values only)
  | SomeIdentifiedValue Identifier -- Integer or Enumerated
  | SomeOIDLikeValue OID -- OID or RELATIVE-OID
    -- TODO: catch-all for OID-like values
  deriving (Eq,Ord,Show, Typeable, Data)

-- Helper for type propagation through parser
maybeValueOfType Nothing = value
maybeValueOfType (Just t) = valueOfType t

-- TODO: incomplete, check for "undefined"
valueOfType (Type t _) = v t
  where
    v (BitString _) = bitStringValue
    v Boolean = booleanValue
    -- Fourteen CharacterString variants
    v CharacterString = unrestrictedCharacterStringValue
    v BMPString = restrictedCharacterStringValue
    v GeneralString = restrictedCharacterStringValue
    v GraphicString = restrictedCharacterStringValue
    v IA5String = restrictedCharacterStringValue
    v ISO646String = restrictedCharacterStringValue
    v NumericString = restrictedCharacterStringValue
    v PrintableString = restrictedCharacterStringValue
    v TeletexString = restrictedCharacterStringValue
    v T61String = restrictedCharacterStringValue
    v UniversalString = restrictedCharacterStringValue
    v UTF8String = restrictedCharacterStringValue
    v VideotexString = restrictedCharacterStringValue
    v VisibleString = restrictedCharacterStringValue
    v (Choice _) = choiceValue
    v EmbeddedPDV = embeddedPDVValue
    -- Three ENUMERATED variants
    v (SimpleEnumeration _) = enumeratedValue
    v (EnumerationWithException _ _) = enumeratedValue
    v (EnumerationWithExceptionAndAddition _ _ _) = enumeratedValue
    v External = externalValue
    v (InstanceOf _) = instanceOfValue
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
    v GeneralizedTime = generalizedTimeValue
    v UTCTime = utcTimeValue
    v ObjectDescriptor = objectDescriptorValue
    v (Selection _ innerType) = valueOfType innerType
      -- TODO: TypeFromObject constructors                    = undefined
      -- TODO: ValueSetFromObjects constructors     = undefined
    v (Any _) = value

-- TODO: When we dont know the type of value we are parsing, we could not distinguish between some
-- of the alternatives without deep context analysis and/or semantical analysis
-- Checked    
value = builtinValue <|> referencedValue <|> objectClassFieldValue
        <?> "Value"

-- TODO: re-check this after implementation of all builtin types
builtinValue =
  choice $ map try [ booleanValue -- unambiguous
                   , nullValue -- unambiguous
                   , SomeNumber <$> realnumber -- comes before integer to parse "10.0" as 10.0 and not 10
                   , PlusInfinity <$ reserved "PLUS-INFINITY"
                   , MinusInfinity <$ reserved "MINUS-INFINITY"
                   , restrictedCharacterStringValue -- TODO: need generic string value
                   , choiceValue
                   , SomeNamedValueList <$> componentValueList
                   , SomeValueList <$> braces (commaSep value)
                   , SomeOIDLikeValue <$> oid -- Either OID or RELATIVE-OID
                     -- taggedValue and instanceOfValue are not here because they are just "value" and would lead to infinie loop
                   , bitStringValue -- This covers OCTET STRING values as well
                   , SomeIdentifiedValue <$> identifier -- Integer or ENUMERATED
                   ]

-- Checked
referencedValue = 
  choice [ DefinedV <$> definedValue 
         , valueFromObject -- ITU-T Rec. X.681 | ISO/IEC 8824-2, clause 15
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
  choice [ PlusInfinity <$ reserved "PLUS-INFINITY"
         , MinusInfinity <$ reserved "MINUS-INFINITY"
         , RealValue <$> realnumber
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
  choice [ try $ Empty <$ ( reserved "SEQUENCE" >> symbol "{" >> symbol "}" )
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
versionNumber = optionMaybe $ number <* colon

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
         , NamedTypeComponent <$> namedType <*> valueOptionality Nothing -- TODO: add type context
         ]

data ValueOptionality = OptionalValue | DefaultValue Value  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked, X.680-0207
valueOptionality t = optionMaybe $
  choice [ OptionalValue <$ reserved "OPTIONAL"
         , DefaultValue <$> ( reserved "DEFAULT" *> maybeValueOfType t)
         ] 

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
      choice [ -- Form of SIZE constraint without enclosing "(" ")" for backward compatibility
               flip Constraint Nothing . ClosedSet False . Singleton . Subtype <$> sizeConstraint
             , constraint Nothing
               -- TODO: how to break dependency loop here and propagate types?
             ]

-- Checked      
setOrSequenceOfValue =
  choice [ try $ Right <$> componentValueList
         , Left <$> braces (commaSep value)
         ]

sequenceOfValue = SequenceOfValue <$> setOrSequenceOfValue
setOfValue = SetOfValue <$> setOrSequenceOfValue
-- }} end of clause 25, end of clause 27
-- {{ X.680-0207, clause 26, "SET"
setType = 
  Set <$>
  choice [ try $ Empty <$ ( reserved "SET" >> symbol "{" >> symbol "}" )
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

data TagType = Explicit | Implicit deriving (Eq,Ord,Show, Typeable, Data)
tagType = 
  optionMaybe $
  choice [ Explicit <$ reserved "EXPLICIT"
         , Implicit <$ reserved "IMPLICIT"
         ]

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
  (Identifier i) <- identifier
  when (not $ i `elem` reservedIds ) $ unexpected ("non-reserved identifier "++i)
  return i
  where
    reservedIds = [ "itu-t", "ccitt", "iso", "joint-iso-itu-t", "joint-iso-ccitt"
                  , "recommendation", "question", "administration", "network-operator"
                  , "identified-organization", "standard", "member-body"] ++ map (:[]) ['a'..'z']
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
embeddedPDVValue = EmbeddedPDVValue <$> componentValueList
-- }} end of clause 33
-- {{ X.680-0207, clause 34, "EXTERNAL"
-- Type parser is trivial and embedded in builtinType
externalValue = ExternalValue <$> componentValueList
-- }} end of clause 34
-- {{ X.680-0207, clause 35-40, "Character string types"
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

restrictedCharacterStringValue =
  RestrictedCharacterStringValue <$>
  choice [ braces (many1 ( try (CharsDefinedValue <$> definedValue) <|> charsDefn ))
         , (:[]) <$> charsDefn 
         ]

data CharsDefn =
  Tuple {table_column::Integer, table_row::Integer}
  | Quadruple Integer Integer Integer Integer
  | CharsDefinedValue DefinedValue
  | CharsCString CString
  deriving (Eq,Ord,Show, Typeable, Data)

charsDefn =
  choice [ try $ braces ( Tuple <$> number <*> (comma *> number) )
         , try $ braces ( Quadruple <$> number <*> (comma *> number) <*> (comma *> number) <*> (comma *> number) )
         , CharsCString <$> cstring
         ]

unrestrictedCharacterStringValue = UnrestrictedCharacterStringValue <$> componentValueList
-- }} end of clause 35-40
-- {{ X.680-0207, clause 41-44, "Useful Types"
usefulType = 
  choice [ GeneralizedTime <$ reserved "GeneralizedTime"
         , UTCTime <$ reserved "UTCTime"
         , ObjectDescriptor <$ reserved "ObjectDescriptor"
         ]
-- TODO: this implementation is sloppy
generalizedTimeValue = GeneralizedTimeValue <$> cstring
utcTimeValue = UTCTimeValue <$> cstring
objectDescriptorValue = ObjectDescriptorValue <$> cstring
-- }} end of clause 41-44
-- {{ X.680-0207, clause 45, "Constraints"
-- constrainedType parser is merged into theType and parsers for SetOf/SequenceOf
data Constraint = Constraint ElementSets (Maybe ExceptionIdentification) deriving (Eq,Ord,Show, Typeable, Data)
constraint t = parens ( Constraint <$> constraintSpec t <*> exceptionSpec )
onlySubtypeConstraint t = parens ( Constraint <$> subtypeConstraint t <*> exceptionSpec )

constraintSpec t = subtypeConstraint t {- TODO: <|> generalConstraint -}

subtypeConstraint t = elementSetSpecs t
-- }} end of clause 45
-- {{ X.680-0207, clause 46, "Element sets"
data ElementSets = 
  ClosedSet Bool ElementSet -- extendable or not
  | ExtendableSet ElementSet
  | SetRange ElementSet ElementSet deriving (Eq,Ord,Show, Typeable, Data)
elementSetSpecs t = 
  choice [ try $ SetRange <$> elementSetSpec t <*> (comma *> symbol "..." *> comma *> elementSetSpec t)
         , try $ ClosedSet True <$> (elementSetSpec t <* comma <* symbol "...")
         , ClosedSet False <$> elementSetSpec t
         ]
  
data ElementSet = AllExcept Exclusions 
                | Union [[Intersection]] 
                | Singleton Elements  -- special form of single-element union for better readability
                deriving (Eq,Ord,Show, Typeable, Data)
elementSetSpec t =
  choice [ AllExcept <$> ( reserved "ALL" *> exclusions t )
         , mkUnion <$> unions t
         ]
  where
    mkUnion ([[Intersection es Nothing]]) = Singleton es
    mkUnion s = Union s

unions t = (intersections t) `sepBy1` unionMark

intersections t = (intersectionElements t) `sepBy1` intersectionMark

data Intersection = Intersection Elements (Maybe Exclusions) deriving (Eq,Ord,Show, Typeable, Data)
intersectionElements t = Intersection <$> elements t <*> optionMaybe (exclusions t)

type Exclusions = Elements
exclusions t = reserved "EXCEPT" *> elements t

unionMark = ( () <$ symbol "|" ) <|> reserved "UNION"
intersectionMark = ( () <$ symbol "^" ) <|> reserved "INTERSECTION"

data Elements = Subset ElementSet | Subtype SubtypeElements | ObjSet ObjectSetElements
  deriving (Eq,Ord,Show, Typeable, Data)
elements t =
  choice [ Subset <$> parens (elementSetSpec t)
         , try $ Subtype <$> (subtypeElements t)
         , ObjSet <$> objectSetElements -- TODO: propagate type here as well?
         ]
-- }} end of clause 46
-- {{ X.680-0207, clause 47, "Subtype elements"
-- TODO: apply table 9 from clause 47.1 ("Applicability of subtype value sets")
data SubtypeElements = 
  SingleValue Value
  | ContainedSubtype Type
  | ValueRange ValueRangeEndpoint ValueRangeEndpoint
  | PermittedAlphabet Constraint
  | SizeConstraint Constraint
    -- TypeConstraint is not implemented because it is not distinguishable from ContainedSubtype without context
    -- two variants of innerTypeConstraint
  | SingleTypeConstraint Constraint
  | MultipleTypeConstaints TypeConstraints
  | PatternConstraint CString
  deriving (Eq,Ord,Show, Typeable, Data)

-- TODO: TypeConstraint and ContainedSubtype without "INCLUDES" are not distinguishable without context!
-- therefore typeConstraint is not implemented
subtypeElements t =
  choice [ try $ valueRange t
         , permittedAlphabet t
         , sizeConstraint
         , innerTypeConstraints t
         , try $ containedSubtype
         , SingleValue <$> maybeValueOfType t
         ]
  
containedSubtype = ContainedSubtype <$> ( optional (reserved "INCLUDES") *>  theType )

data ValueRangeEndpoint = Closed ValueRangeEndValue | Open ValueRangeEndValue deriving (Eq,Ord,Show, Typeable, Data)
data ValueRangeEndValue = MinValue | MaxValue | Value Value deriving (Eq,Ord,Show, Typeable, Data)
valueRange t = ValueRange <$> lowerEndpoint t <*> ( symbol ".." *> upperEndpoint t )
lowerEndpoint t = 
  choice [ try $ Open <$> ( lowerEndValue t <* symbol "<" )
         , Closed <$> lowerEndValue t
         ]
upperEndpoint t =
  choice [ Open <$> ( symbol "<" *> upperEndValue t )
         , Closed <$> upperEndValue t
         ]
lowerEndValue t = (MinValue <$ reserved "MIN") <|> (Value <$> maybeValueOfType t)
upperEndValue t = (MaxValue <$ reserved "MAX") <|> (Value <$> maybeValueOfType t)

sizeConstraint = SizeConstraint <$> ( reserved "SIZE" *> onlySubtypeConstraint sizeConstraintType)
  where sizeConstraintType = parseASN1 theType "INTEGER (0..MAX)"

permittedAlphabet t = PermittedAlphabet <$> ( reserved "FROM" *> onlySubtypeConstraint t )

innerTypeConstraints t = do
  reserved "WITH" 
  choice [ MultipleTypeConstaints <$> ( reserved "COMPONENTS" *> multipleTypeConstraints t )
         , SingleTypeConstraint <$> ( reserved "COMPONENT" *> constraint t )
         ]

multipleTypeConstraints t = braces(  optional (symbol "..." >> symbol ",")  >> typeConstraints t )

type TypeConstraints = [NamedConstraint]
typeConstraints t = commaSep1 (namedConstraint t)

data NamedConstraint = NamedConstraint Identifier ComponentConstraint  deriving (Eq,Ord,Show, Typeable, Data)
namedConstraint t = NamedConstraint <$> identifier <*> componentConstraint t

data ComponentConstraint = ComponentConstraint (Maybe Constraint) (Maybe PresenceConstraint) deriving (Eq,Ord,Show, Typeable, Data)
componentConstraint t = ComponentConstraint <$> optionMaybe (constraint t) <*> optionMaybe presenceConstraint

data PresenceConstraint = Present
                        | Absent
                        | Optional
                        deriving (Eq,Ord,Show, Typeable, Data)
presenceConstraint =
  choice [ Present  <$ reserved "PRESENT"
         , Absent   <$ reserved "ABSENT"
         , Optional <$ reserved "OPTIONAL"
         ]

patternConstraint = PatternConstraint <$> ( reserved "PATTERN" *> cstring )
-- }} end of clause 47
-- {{ X.680-0207, clause 48, "The extension marker", has no useful productions }} --
-- {{ X.680-0207, clause 49, "The exception identifier"
-- Checked
exceptionSpec = 
  optionMaybe ( symbol "!" >> exceptionIdentification )
                
data ExceptionIdentification = ExceptionNumber Integer
                             | ExceptionValue DefinedValue
                             | ExceptionTypeAndValue Type Value
                             deriving (Eq,Ord,Show, Typeable, Data)
exceptionIdentification =
  choice [ ExceptionNumber <$> signedNumber 
         , ExceptionValue <$> definedValue
         , do t <- theType
              colon 
              v <- valueOfType t
              return $ ExceptionTypeAndValue t v
         ]
-- }} end of clause 49
-- {{ X.680-0207, DEPRECATED clause, "The ANY type"
-- Deprecated ANY type could be found in many older specifications
anyType = Any <$> ( reserved "ANY" *> optionMaybe ( reserved "DEFINED" *> reserved "BY" *> identifier ) )
-- TODO: ANY type does not have value parser(?)
-- }} end of DEPRECATED clause

--------------------------------------------------
-- X.681-0207: Information Object Specification --
--------------------------------------------------

-- {{ X.681-0207, clause 7, "Lexical items"
newtype ObjectClassReference = ObjectClassReference String deriving (Eq,Ord,Show, Typeable, Data)
objectclassreference = ObjectClassReference <$> ucaseIdent

newtype ObjectReference = ObjectReference String deriving (Eq,Ord,Show, Typeable, Data)
objectreference = ObjectReference <$> _valuereference
_objectreference = _valuereference

newtype ObjectSetReference = ObjectSetReference String deriving (Eq,Ord,Show, Typeable, Data)
objectsetreference = ObjectSetReference <$> _typereference
_objectsetreference = _typereference

newtype TypeFieldReference = TypeFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
typefieldreference = TypeFieldReference <$> (char '&' *> _typereference)

newtype ValueFieldReference = ValueFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
valuefieldreference = ValueFieldReference <$> ( char '&' *> _valuereference)

newtype ValueSetFieldReference = ValueSetFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
valuesetfieldreference = ValueSetFieldReference <$> ( char '&' *> _typereference )

newtype ObjectFieldReference = ObjectFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
objectfieldreference = ObjectFieldReference <$> ( char '&' *> _objectreference )

newtype ObjectSetFieldReference = ObjectSetFieldReference String deriving (Eq,Ord,Show, Typeable, Data)
objectsetfieldreference = ObjectSetFieldReference <$> ( char '&' *> _objectsetreference )
-- }} end of clause 7
-- {{ X.681-0207, clause 8, "Referencing definitions"

-- UsefulObjectClassReference is inlined in definedObjectClass as TypeIdentifier and AbstractSyntax
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
-- }} end of clause 8
-- {{ X.681-0207, clause 9, "Information object class definition and assignment"

objectClassAssignment = ObjectClassAssignment <$> objectclassreference <*> ( symbol "::=" *> objectClass )

data ObjectClass = ObjectClassDefn [Field] | DefinedObjectClassDefn DefinedObjectClass deriving (Eq,Ord,Show, Typeable, Data)
objectClass = 
  choice [ DefinedObjectClassDefn <$> definedObjectClass
         , objectClassDefn 
         -- TODO: , parametrizedObjectClass -- ITU-T Rec. X.683 | ISO/IEC 8824-4, 9.2.
         ]

objectClassDefn = do
  reserved "CLASS"
  ObjectClassDefn <$> (braces $ commaSep1 field)
  -- TODO : optionMaybe withSyntaxSpec  
  
data Field = TypeField TypeFieldReference (Maybe TypeOptionality) 
           | FixedTypeValueField ValueFieldReference Type Bool {-unique or not-} (Maybe ValueOptionality)
           | VariableTypeValueField ValueFieldReference FieldName (Maybe ValueOptionality)
           | FixedTypeValueSetField ValueSetFieldReference Type (Maybe ValueSetOptionality)
           | VariableTypeValueSetField ValueSetFieldReference FieldName (Maybe ValueSetOptionality)
           | ObjectField ObjectFieldReference DefinedObjectClass (Maybe ObjectOptionality)
           | ObjectSetField ObjectSetFieldReference DefinedObjectClass (Maybe ObjectSetOptionality)
           deriving (Eq,Ord,Show, Typeable, Data)
data TypeOptionality = OptionalType | DefaultType Type deriving (Eq,Ord,Show, Typeable, Data)
data ObjectOptionality = OptionalObject | DefaultObject Object deriving (Eq,Ord,Show, Typeable, Data)
data ObjectSetOptionality = OptionalObjectSet | DefaultObjectSet ObjectSet deriving (Eq,Ord,Show, Typeable, Data)
data ValueSetOptionality = OptionalValueSet | DefaultValueSet ValueSet deriving (Eq,Ord,Show, Typeable, Data)

{-
From Dubuisson:
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
field = try objectField -- &lower ALL-UPPER
        <|> try objectSetField -- &Upper ALL-UPPER
        <|> try fixedTypeValueField -- &lower Upper
        <|> try variableTypeValueField -- &lower &Upper
        <|> try fixedTypeValueSetField -- &Upper Upper
        <|> try variableTypeValueSetField -- &Upper &Upper
        <|> typeField -- &Upper none
        <?> "Field"
        
typeField = TypeField <$> typefieldreference <*> optionality
  where optionality = optionMaybe $ ( OptionalType <$ reserved "OPTIONAL" ) <|> ( DefaultType <$> ( reserved "DEFAULT" *> theType ) )

fixedTypeValueField = do
  vfr <- valuefieldreference
  t <- theType
  FixedTypeValueField vfr t <$> uniqueness <*> valueOptionality (Just t)
  where
    uniqueness = option False (reserved "UNIQUE" >> return True)

variableTypeValueField = VariableTypeValueField <$> valuefieldreference <*> fieldName <*> valueOptionality Nothing -- TODO: can add type context here?

fixedTypeValueSetField = do
  vsfr <- valuesetfieldreference
  t <- theType
  FixedTypeValueSetField vsfr t <$> valueSetOptionality (Just t)
  
valueSetOptionality t =
  optionMaybe $
  choice [ OptionalValueSet <$ reserved "OPTIONAL"
         , DefaultValueSet <$> ( reserved "DEFAULT" *> valueSet t )
         ] 
  
variableTypeValueSetField = VariableTypeValueSetField <$> valuesetfieldreference <*> fieldName <*> valueSetOptionality Nothing -- TODO: type propagation

objectField = ObjectField <$> objectfieldreference <*> definedObjectClass <*> objectOptionality
  
objectOptionality = optionMaybe $
  choice [ OptionalObject <$ reserved "OPTIONAL"
         , DefaultObject <$> ( reserved "DEFAULT" *> object )
         ] 

objectSetField = ObjectSetField <$> objectsetfieldreference <*> definedObjectClass <*> objectSetOptionality

objectSetOptionality = optionMaybe $
  choice [ OptionalObjectSet <$ reserved "OPTIONAL"
         , DefaultObjectSet <$> ( reserved "DEFAULT" *> objectSet )
         ] 
  
data PrimitiveFieldName = PrimTFR TypeFieldReference | PrimVFR ValueFieldReference | PrimOFR ObjectFieldReference
                        | PrimVSFR ValueSetFieldReference | PrimOSFR ObjectSetFieldReference
                        deriving (Eq,Ord,Show, Typeable, Data)
primitiveFieldName =
  choice [ PrimTFR <$> try typefieldreference -- &Upper
         , PrimOFR <$> objectfieldreference -- &lower       
         , PrimVFR <$> valuefieldreference -- &lower
         , PrimVSFR <$> valuesetfieldreference -- &Upper
         , PrimOSFR <$> objectsetfieldreference -- &Upper
         ]

type FieldName = [PrimitiveFieldName]
fieldName = primitiveFieldName `sepBy1` dot
-- }} end of clause 9
-- {{ X.681-0207, clause 10, "Syntax List"
-- TODO
-- }} end of clause 10
-- {{ X.681-0207, clause 11, "Information object definition and assignment"

objectAssignment = ObjectAssignment <$> objectreference <*> definedObjectClass <*> ( symbol "::=" *> object )

data Object = ObjectDefn [FieldSetting]
            | ObjDefinedObject DefinedObject
            | ObjectFromObject ReferencedObjects FieldName
            deriving (Eq,Ord,Show, Typeable, Data)
object =
  choice [ objectDefn
         , ObjDefinedObject <$> definedObject
         , objectFromObject
         -- TODO: , parametrizedObject
         ]
 
objectDefn = 
  choice [ ObjectDefn <$> defaultSyntax 
           -- TODO: , definedSyntax
         ]
  where
    defaultSyntax = braces $ commaSep fieldSetting
    -- definedSyntax = TODO
    
data FieldSetting = 
  TypeFieldSetting TypeFieldReference Type
  | ValueFieldSetting ValueFieldReference Value
  | ValueSetFieldSetting ValueSetFieldReference ValueSet
  | ObjectFieldSetting ObjectFieldReference Object
  | ObjectSetFieldSetting ObjectSetFieldReference ObjectSet
                  deriving (Eq,Ord,Show, Typeable, Data)
-- PrimitingFieldName and Setting non-terminals are inlined here to lessen the number of ambiguities:
-- both primitiveFieldName and setting could produce several possible parses for wide variety of inputs.
-- Combining them in the single parser helps resolve this according to clause 11.7 of X.681-0207
fieldSetting = 
  choice $ map try [ TypeFieldSetting <$> typefieldreference <*> theType
                   , ValueFieldSetting <$> valuefieldreference <*> value
                   , ValueSetFieldSetting <$> valuesetfieldreference <*> valueSet Nothing -- TODO: type propagation
                   , ObjectFieldSetting <$> objectfieldreference <*> object
                   , ObjectSetFieldSetting <$> objectsetfieldreference <*> objectSet
                   ]
-- }} end of clause 11
-- {{ X.681-0207, clause 12, "Information object set definition and assignment"
objectSetAssignment = ObjectSetAssignment <$> objectsetreference <*> definedObjectClass <*> ( symbol "::=" *> objectSet )

objectSet = braces objectSetSpec

data ObjectSet = 
  ObjectSet ElementSet
  | ObjectSetExtendableAtEnd ElementSet
  | EmptyExtendableObjectSet
  | ObjectSetExtendableAtStart ElementSet
  | ObjectSetExtendableInTheMiddle ElementSet ElementSet
  deriving (Eq,Ord,Show, Typeable, Data)
objectSetSpec =
  choice [ try $ ObjectSetExtendableAtStart <$> ( symbol "..." *> comma *> elementSetSpec Nothing ) -- TODO: type propagation
         , EmptyExtendableObjectSet <$ (symbol "...")
         , try $ ObjectSetExtendableInTheMiddle <$> elementSetSpec Nothing <*> ( comma *> symbol "..." *> comma *> elementSetSpec Nothing ) -- TODO: type propagation
         , try $ ObjectSetExtendableAtEnd <$> elementSetSpec Nothing <* comma <* symbol "..." -- TODO: type propagation
         , ObjectSet <$>  elementSetSpec Nothing -- TODO: type propagation
         ]

data ObjectSetElements = 
  ObjectElement Object 
  | DefinedObjectSetElement DefinedObjectSet 
  | ObjectSetFromObjectsElement ReferencedObjects FieldName
  -- TODO: | ParametrizedObjectSetElement ParametrizedObjectSet 
  deriving (Eq,Ord,Show, Typeable, Data)
objectSetElements =
  choice [ ObjectElement <$> object
         , DefinedObjectSetElement <$> definedObjectSet
         , ObjectSetFromObjectsElement <$> referencedObjects <*> ( dot *> fieldName ) -- objectSetFromObjects is inlined here
         -- TODO: , ParametrizedObjectSetElement <$> parameterizedObjectSet
         ]
-- }} end of clause 12
-- {{ X.681-0207, clause 13, "Associated tables" does not have any productions }} --
-- {{ X.681-0207, clause 14, "Notation for the object class field type"
objectClassFieldType = ObjectClassField <$> definedObjectClass <*> ( dot *> fieldName )

objectClassFieldValue = openTypeFieldVal <|> fixedTypeFieldVal

openTypeFieldVal  = do
  t <- theType
  OpenTypeFieldValue t <$> ( colon *> valueOfType t )
  
fixedTypeFieldVal = FixedTypeFieldValue <$> ( builtinValue <|> referencedValue )
  
-- }} end of clause 14
-- {{ X.681-0207, clause 15, "Information from objects"
{- InformationFromObjects ::=
   ValueFromObject
   | ValueSetFromObjects
   | TypeFromObject
   | ObjectFromObject
   | ObjectSetFromObjects -}

valueFromObject = ValueFromObject <$> referencedObjects <*> ( dot *> fieldName )
valueSetFromObjects = ValueSetFromObjects <$> referencedObjects <*> (dot *> fieldName )
typeFromObject = TypeFromObject <$> referencedObjects <*> ( dot *> fieldName )
objectFromObject = ObjectFromObject <$> referencedObjects <*> (dot *> fieldName )
-- objectSetFromObjects is inlined into ObjectSetElements

data ReferencedObjects = 
  ReferencedObject DefinedObject
  | ReferencedObjectSet DefinedObjectSet
  deriving (Eq,Ord,Show, Typeable, Data)
referencedObjects =
  choice [ ReferencedObject <$> definedObject
           -- TODO : , parameterizedObject
         , ReferencedObjectSet <$> definedObjectSet
         -- TODO: , parameterizedObjectSet
         ]
-- }} end of clause 15
-- {{ X.681-0207, annex C, "Instance Of type"
instanceOfType = InstanceOf <$> ( reserved "INSTANCE" *> reserved "OF" *> definedObjectClass )
-- Value of InstaceOf is the value of associated SEQUENCE type (see X.681 annex C.7)
instanceOfValue = InstanceOfValue <$> componentValueList
-- }} end of annex C

-- Local Variables: 
-- outline-regexp:"-- [{]\+"
-- End: