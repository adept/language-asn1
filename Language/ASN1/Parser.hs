{-# LANGUAGE DeriveDataTypeable #-}
module Language.ASN1.Parser (
  parseASN1FromFileOrDie
  , parseASN1FromFile
  , parseASN1
  , Module(..)
  , Type(..)
  , Assignment(..)
    
  , TypeReference(..)
  ) where
{-
 ASN.1 Parser for Haskell (C) Dmitry Astapov 2003-2010

 This software is distibuted under the terms of GPL license
 See LICENSE for more information

 Based on the ASN.1 grammar for JavaCC:
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
 *  I tested it against several ASN.1 files used by CMIP;
 *  Additional work is reguired to fully support SNMP MIBs parsing
 *   
 *  Please let me know if you use this grammar: i'm using it to develop ASN.1/IDL compiler
 *  
 */

 JavaCC parser was semi-automatically converted into Parsec parser, and resulting
 code is being checked against the "ASN.1, Communication between Heterogeneous Systems" 
 by Olivier Dubuisson.

 This is work in progress, so there could be bug lurking. Definitions checked against the book
 are annotated with comments referencing to the appropriate book chapter and/or section.
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

-- { Top-level interface
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

parseASN1 source = 
  parse asn1Input "" source
-- }

-- { Top-level parser
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
-- }
newtype TypeName = TypeName Identifier deriving (Eq,Ord,Show, Typeable, Data)
type NumberOrDefinedValue = Either Integer DefinedValue
data ElementType = NamedElementType { _element_name::TypeName
                                    , _element_body::Type
                                    , _element_presence::Maybe ValueOptionality
                                    } 
                 | ComponentsOf_ Type deriving (Eq,Ord,Show, Typeable, Data)
data NamedValue = NamedValue { value_name::ValueName
                             , named_value::Value
                             } deriving (Eq,Ord,Show, Typeable, Data)
newtype ValueName = ValueName Identifier deriving (Eq,Ord,Show, Typeable, Data)
data SizeConstraint = SizeConstraint SubtypeSpec | UndefinedSizeContraint deriving (Eq,Ord,Show, Typeable, Data)


-- { Chapter 8.1, "Lexical tokens in ASN.1"
data StringConst = StringConst String deriving (Eq,Ord,Show, Typeable, Data)
stringConst allowedSet marker = 
  do { char '\'' ; body <- many (oneOf allowedSet) ; char '\''; char marker ; return (StringConst body) } 

bstring = stringConst "01" 'B' <?> "bstring"

hstring = stringConst "0123456789ABCDEFabcdef" 'H' <?> "hstring"

cstring = 
  do { char '"'; s <- anyChar `manyTill` (char '"' ); return (StringConst s) } <?> "cstring"

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

-- { Chapter 9, "Modules and assignments"
-- {{ Section 9.1, "Assignments"
assignmentList = (assignment `sepBy1` (optional semi)) <?> "assignmentList"


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
assignment = 
  choice $ map try [ objectAssignment
                   , valueAssignment
                   , typeAssignment
                   , objectClassAssignment
                   , valueSetTypeAssignment
                   -- TODO: , objectSetAssignment
                   -- TODO: , parameterizedAssignment
                   ]
  
typeAssignment = TypeAssignment <$> typereference <*> (reservedOp "::=" *> theType)
                 <?> "TypeAssignment"

-- ConstrainedType is merged in other parsers: "Type Constraint" alternative is encoded here,
-- and TypeWithConstraint in implemented in SetOf/SequenceOf parsers
data Type = Type { type_id::BuiltinType
                 , subtype::Maybe SubtypeSpec
                 }
               deriving (Eq,Ord,Show, Typeable, Data)
theType = Type <$> ( builtinType <|> referencedType ) <*> optionMaybe constraint
          <?> "Type"

data BuiltinType = TheInteger [NamedNumber]
                 | BitString [NamedNumber]
                   -- SEQUENCE variants
                 | EmptySequence
                 | EmptyExtendableSequence (Maybe ExceptionIdentification)
                 | Sequence ComponentTypeLists
                 | EmptySet
                 | EmptyExtendableSet (Maybe ExceptionIdentification)
                 | Set ComponentTypeLists
                 | SetOf (Maybe SubtypeSpec) Type -- TODO: fix types when constraint is implemented properly
                 | SequenceOf (Maybe SubtypeSpec) Type -- TODO: fix types when constraint is implemented properly
                 | Choice AlternativeTypeLists
                 | Selection Identifier Type
                 | Tagged Tag (Maybe TagType) Type
                 | Any Identifier
                 | SimpleEnumeration [EnumerationItem]
                 | EnumerationWithException [EnumerationItem] (Maybe ExceptionIdentification)
                 | EnumerationWithExceptionAndAddition [EnumerationItem] (Maybe ExceptionIdentification) [EnumerationItem]
                 | OctetString 
                 | ObjectIdentifier
                 | Real
                 | Boolean
                 | Null
                 | External
                 | LocalTypeReference TypeReference
                 | ExternalTypeReference ModuleReference TypeReference
                 -- TODO: | ParameterizedType
                 -- TODO: | ParametrizedValueSetType
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
                 | CharacterString
                 | GeneralizedTime
                 | UTCTime
                 deriving (Eq,Ord,Show, Typeable, Data)
                          
builtinType =
  choice $ map try [ integerType
                   , BitString <$> bitStringType
                   , try $ sequenceType
                   , try $ setType
                   , setOrSequenceOfType
                   , choiceType
                   , taggedType
                   , Any <$> anyType -- DEPRECATED
                   , enumeratedType
                   , OctetString <$ (reserved "OCTET" *> reserved "STRING")
                   , ObjectIdentifier <$ (reserved "OBJECT" *> reserved "IDENTIFIER")
                   , Real <$ reserved "REAL"
                   , Boolean <$ reserved "BOOLEAN"
                   , Null <$ reserved "NULL"
                   , External <$ reserved "EXTERNAL"
                   , characterStringType
                     -- TODO: , embeddedPDVType
                     -- TODO: , instanceOfType
                     -- TODO: , objectClassFieldType
                     -- TODO: , relativeOIDType
                   ]

referencedType = 
  definedType <|> usefulType <|> selectionType {- TODO: <|> typeFromObject <|> valueSetFromObjects -} 
  <?> "ReferencedType"

valueAssignment = ValueAssignment <$> valuereference <*> theType <*> (reservedOp "::=" *> value)
                  <?> "ValueAssignment"

data Value = BooleanValue Bool
           | NullValue
           | PlusInfinity
           | MinusInfinity
           | SignedNumber Integer
           | HexString StringConst
           | BinaryString StringConst
           | CharString StringConst
           | CompoundValue OID
             -- ReferencedValue constructors:
           | DefinedV DefinedValue
             -- TODO: | ValueFromObject ...
           | IntegerOrEnumeratedIdentifiedValue Identifier
           deriving (Eq,Ord,Show, Typeable, Data)

    
value = builtinValue <|> referencedValue
        <?> "Value"
                      
-- TODO: re-check all this once again
builtinValue =
  choice $ map try [ booleanValue >>= return . BooleanValue -- ok
                   , NullValue <$ reserved "NULL" -- ok
                   , specialRealValue -- is this RealValue?
                     -- Integer identified by 'identifier' is described below
                   , SignedNumber <$> signedNumber -- ok
                   , hexString >>= return . HexString
                   , binaryString >>= return . BinaryString
                   , characterStringValue >>= return . CharString -- ok
                   , compoundValue >>= return . CompoundValue
                     -- TODO: bitStringValue
                     -- TODO: choiceValue
                     -- TODO: embeddedPDVValue
                     -- TODO: enumeratedValue
                     -- TODO: externalValue
                     -- TODO: instanceOfValue
                     -- TODO: objectClassFieldValue
                     -- TODO: objectIdentifierValue
                     -- TODO: octetStringValue
                     -- TODO: relativeOIDValue
                     -- TODO: sequenceValue
                     -- TODO: sequenceOfValue
                     -- TODO: setValue
                     -- TODO: setOfValue
                     -- TODO: taggedValue
                     -- From ReferencedValue:
                     --   TODO
                     -- Two types could have values denoted by a simple identified. Without semantical analysis
                     -- it is impossible to tell them apart. So they are captured by this single catch-all case below
                   , IntegerOrEnumeratedIdentifiedValue <$> identifier -- ok

                   ]

referencedValue = 
  choice [ DefinedV <$> definedValue 
         -- TODO: , valueFromObject
         ]

taggedValue = value

valueSetTypeAssignment = ValueSetTypeAssignment <$> typereference <*> theType <*> (reservedOp "::=" *> valueSet)
-- }} end of section 9.1
-- {{ Section 9.2, "Module structure"
data Module = Module { module_id::ModuleIdentifier
                     , default_tag_type::Maybe TagDefault
                     , extensibility_implied :: Bool
                     , module_body::Maybe ModuleBody
                     } deriving (Eq,Ord,Show, Typeable, Data)
moduleDefinition = 
  Module <$> moduleIdentifier <*> (reserved "DEFINITIONS" *> tagDefault) <*> extensibility 
         <*> (reservedOp "::=" *> reserved "BEGIN" *> moduleBody) <* reserved "END"  
         <?> "ModuleDefinition"
 where
   extensibility = option False $ True <$ (reserved "EXTENSIBILITY" >> reserved "IMPLIED")
   
data ModuleIdentifier = ModuleIdentifier ModuleReference (Maybe DefinitiveOID) deriving (Eq,Ord,Show, Typeable, Data)
moduleIdentifier = ModuleIdentifier <$> modulereference <*> definitiveIdentifier
                   <?> "ModuleIdentifier"

type DefinitiveOID = [DefinitiveOIDComponent]
definitiveIdentifier =
  optionMaybe (braces (many1 definitiveOIDComponent))
  <?> "DefinitiveIdentifier"

data DefinitiveOIDComponent = DefinitiveOIDNumber Integer 
                            | DefinitiveOIDNamedNumber Identifier Integer
                            | DefinitiveOIDName Identifier deriving (Eq,Ord,Show, Typeable, Data)
definitiveOIDComponent =
  choice [ DefinitiveOIDNumber <$> number
         , try $ DefinitiveOIDNamedNumber <$> identifier <*> parens number
         , DefinitiveOIDName . Identifier <$> reservedOIDIdentifier
         ]
  <?> "DefinitiveObjectIdComponent"

type Exports = [ExportedSymbol]
type Imports = [SymbolsFromModule]
data ModuleBody = ModuleBody { module_exports::Maybe [Exports]
                             , module_imports::Maybe [Imports]
                             , module_assignments::[Assignment]
                             } deriving (Eq,Ord,Show, Typeable, Data)
moduleBody = optionMaybe ( ModuleBody <$> exports <*> imports <*> assignmentList )
             <?> "ModuleBody"

newtype ExportedSymbol = ExportedSymbol Symbol deriving (Eq,Ord,Show, Typeable, Data)
exports = optionMaybe ( reserved "EXPORTS" *> symbolsExported)
          <?> "Exports"
  where symbolsExported = (commaSep $ ExportedSymbol <$> theSymbol) `endBy` semi

imports = optionMaybe ( reserved "IMPORTS" *> symbolsImported )
          <?> "Imports"
  where symbolsImported = (many $ try symbolsFromModule) `endBy` semi

data SymbolsFromModule = SymbolsFromModule [Symbol] GlobalModuleReference deriving (Eq,Ord,Show, Typeable, Data)
symbolsFromModule = SymbolsFromModule <$> commaSep1 theSymbol <*> (reserved "FROM" *> globalModuleReference)
                    <?> "SymbolsFromModule"

data GlobalModuleReference = GlobalModuleReference ModuleReference (Maybe AssignedIdentifier) deriving (Eq,Ord,Show, Typeable, Data)
globalModuleReference = GlobalModuleReference <$> modulereference <*> assignedIdentifier

data AssignedIdentifier = AssignedIdentifierOID OID | AssignedIdentifierDefinedValue DefinedValue deriving (Eq,Ord,Show, Typeable, Data)
assignedIdentifier = 
  optionMaybe $ 
  choice [ AssignedIdentifierOID <$> try oid
         , AssignedIdentifierDefinedValue <$> definedValue
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
   parametrizedDesignation = optional (lexeme (char '{') >> lexeme (char '}'))
-- }} end of section 9.2

-- {{ Section 9.3, "Local and external references"   

-- definedType is used only in BuiltinType. See BuiltinType for constructors.
-- I also took libery of reusing simpleDefinedType from chapter 17 for the first two
-- alternatives of definedType.
definedType = simpleDefinedType --TODO: <|> parametrizedType <|> parametrizedValueSetType


data DefinedValue = ExternalValueReference ModuleReference ValueReference
                  | LocalValueReference ValueReference
                  deriving (Eq,Ord,Show, Typeable, Data)
definedValue = 
  choice [ try $ ExternalValueReference <$> moduleReferenceAndDot <*> valuereference
         , LocalValueReference <$> valuereference
         -- TODO: , parametrizedValue 
         ] <?> "DefinedValue"
     
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
-- booleanType parser is inlined into basicType parser
booleanValue =
  choice [ True <$ reserved "TRUE"
         , False <$ reserved "FALSE"
         ]
-- }} end of section 10.1
-- {{ Section 10.2, "NULL type"
-- parsers for type and value are inlined into builinType and builtinValue
-- }} end of section 10.2
-- {{ Section 10.3, "INTEGER type"
integerType = TheInteger <$> (reserved "INTEGER" *> option [] (braces namedNumberList))
  
namedNumberList = commaSep1 namedNumber

data NamedNumber = NamedNumber Identifier Integer
                 | NamedDefinedValue  Identifier DefinedValue
                 deriving (Eq,Ord,Show, Typeable, Data)
namedNumber = 
  choice [ try $ NamedNumber <$> identifier <*> parens signedNumber
         , NamedDefinedValue <$> identifier <*> parens definedValue
         ]
  <?> "NamedNumber"
-- }} end of section 10.3
-- {{ Section 10.4, "The ENUMERATED type"
enumeratedType = reserved "ENUMERATED" *> braces enumerations

enumerations = 
  choice [ try $ EnumerationWithExceptionAndAddition <$> enumeration <*> (comma *> symbol "..." *> exceptionSpec) <*> (comma *> enumeration)
         , try $ EnumerationWithException <$> enumeration <*> (comma *> symbol "..." *> exceptionSpec)
         , SimpleEnumeration <$> enumeration
         ]

-- TODO: merge three almost similar functions in enumeration, sequence and choice parsers
enumeration = enumerationItem `sepBy1` comma'
  where
    comma' = try $ do
      comma
      notFollowedBy (lexeme (char '.'))

data EnumerationItem = EnumerationItemNumber NamedNumber
                     | EnumerationItemIdentifier Identifier
                     deriving (Eq,Ord,Show, Typeable, Data)
enumerationItem = 
  choice [ try $ EnumerationItemNumber <$> namedNumber
         , EnumerationItemIdentifier <$> identifier
         ]

-- TODO: values
-- }} end of section 10.4
-- { X.680-0207, section 24, "Notation for sequence types"

-- Checked
sequenceType = 
  choice [ try $ EmptySequence <$ ( reserved "SEQUENCE" >> lexeme (char '{') >> lexeme (char '}') )
         , try $ EmptyExtendableSequence <$> ( reserved "SEQUENCE" *> braces (extensionAndException <* optionalExtensionMarker) )
         , Sequence <$> (reserved "SEQUENCE" *> braces componentTypeLists)
         ]

data ComponentTypeLists = ComponentTypeList [ComponentType]
                        | JustExtensions (Maybe ExceptionIdentification) (Maybe [ExtensionAddition])
                        | ExtensionsAtStart (Maybe ExceptionIdentification) (Maybe [ExtensionAddition]) [ComponentType]
                        | ExtensionsAtEnd [ComponentType] (Maybe ExceptionIdentification) (Maybe [ExtensionAddition])
                        | ExtensionsInTheMiddle [ComponentType] (Maybe ExceptionIdentification) (Maybe [ExtensionAddition]) [ComponentType]
                        deriving (Eq,Ord,Show, Typeable, Data)

-- Commented commas are in the ASN.1, but here they are consumed by the preceding parsers
-- Checked
componentTypeLists = 
  choice [ try $ ExtensionsInTheMiddle <$> componentTypeList <*> ({- comma *>-} extensionAndException) <*> extensionsAdditions <*> (extensionEndMarker *> comma *> componentTypeList)
         , try $ ExtensionsAtStart <$> extensionAndException <*> extensionsAdditions <*> (extensionEndMarker *> comma *> componentTypeList)
         , JustExtensions <$> extensionAndException <*> extensionsAdditions <* optionalExtensionMarker
         , try $ ExtensionsAtEnd <$> componentTypeList <*> ({- comma *>-} extensionAndException) <*> extensionsAdditions <* optionalExtensionMarker
         , ComponentTypeList <$> componentTypeList
         ]
  
-- If this marker comes after *TypeList, then trailing comma would be consumed by the *TypeList parser.
-- Hence the (optional comma) and not (comma) as was in ASN.1 spec
-- Checked
extensionEndMarker = optional comma >> symbol "..."

-- TODO: merge with similar code in "CHOICE" type parser
-- Checked
extensionsAdditions = optionMaybe (comma >> extensionAdditionList)

-- It is hard to ensure that this parser does not consume the trailing coma (and fail).
-- So we let it do that and make subsequent coma optional at call site
-- Checked
extensionAdditionList = commaSepEndBy1 extensionAddition -- `sepEndBy1` comma'
  where 
    comma' = try $ do
      comma
      notFollowedBy (lexeme (char '.'))

data ExtensionAddition = ExtensionAdditionGroup (Maybe Integer) [ComponentType]
                       | ExtensionAdditionType ComponentType
                       deriving (Eq,Ord,Show, Typeable, Data)

-- Checked
extensionAddition = 
  choice [ extensionAdditionGroup
         , ExtensionAdditionType <$> componentType
         ]
  where
    -- Checked
    extensionAdditionGroup = ExtensionAdditionGroup <$> ( symbol "[[" *> versionNumber ) <*> componentTypeList <* symbol "]]"

-- Checked
versionNumber = optionMaybe $ number <* lexeme (char ':')

-- It is hard to ensure that this parser does not consume the trailing coma (and fail).
-- So we let it do that and make subsequent coma optional at call site
-- Checked
componentTypeList = commaSepEndBy1 componentType -- `sepBy1` comma'
  where
    comma' = try $ do
      comma
      notFollowedBy (lexeme (char '.'))

data ComponentType = NamedTypeComponent { element_type::NamedType
                                        , element_presence::Maybe ValueOptionality
                                        } 
                   | ComponentsOf Type deriving (Eq,Ord,Show, Typeable, Data)

-- Three cases of definition of componentType from X.680 are folded into valueOptionality helper parser
-- Checked
componentType =
  choice [ try $ ComponentsOf <$> (reserved "COMPONENTS" *> reserved "OF" *> theType)
         , NamedTypeComponent <$> namedType <*> valueOptionality
         ]

data ValueOptionality = OptionalValue | DefaultValue Value  deriving (Eq,Ord,Show, Typeable, Data)
-- Checked
valueOptionality = optionMaybe $
  choice [ reserved "OPTIONAL" >> return OptionalValue
         , reserved "DEFAULT" >> value >>= return . DefaultValue
         ] 

-- TODO: values
-- }} end of section 12.2
-- {{ Section 12.3, "The constructor SET"
setType = 
  choice [ try $ EmptySet <$ ( reserved "SET" >> lexeme (char '{') >> lexeme (char '}') )
         , try $ EmptyExtendableSet <$> ( reserved "SET" *> braces ( extensionAndException <* optionalExtensionMarker ) )
         , Set <$> (reserved "SET" *> braces componentTypeLists)
         ]
-- TODO: values
-- }} end of section 12.3
-- {{ Section 12.4 and 12.5, "The constructor SEQUENCE OF" and "The constructor SET OF"
-- 'TypeWithConstraint' is merged with SetOfType and SequenceOfType for brevity
setOrSequenceOfType = do  
  set <- isSet
  c <- optionMaybe setSeqConstraint
  reserved "OF"
  if set 
    then SetOf c <$> theType
    else SequenceOf c <$> theType
  where
    isSet = ( True <$ reserved "SET" ) <|> (False <$ reserved "SEQUENCE")
    setSeqConstraint =
      choice [ reserved "SIZE" >> constraint -- TODO: This is SizeConstraint, wrap in appropriate constructor
             , constraint
             ]
-- TODO: values      
-- }}

data NamedType = NamedType Identifier Type deriving (Eq,Ord,Show, Typeable, Data)
namedType = NamedType <$> identifier <*> theType



simpleDefinedType = 
  choice [ try $ ExternalTypeReference <$> moduleReferenceAndDot <*> typereference
         , LocalTypeReference <$> typereference
         ]
  <?> "SimpleDefinedType"

data TagDefault = ExplicitTags | ImplicitTags | AutomaticTags deriving (Eq,Ord,Show, Typeable, Data)
data TagType = Explicit | Implicit deriving (Eq,Ord,Show, Typeable, Data)
tagType = 
  optionMaybe $
  choice [ Explicit <$ reserved "EXPLICIT"
         , Implicit <$ reserved "IMPLICIT"
         ]

tagDefault = optionMaybe td <?> "tagDefault"
  where td = do 
          t <- choice [ ExplicitTags <$ reserved "EXPLICIT"
                      , ImplicitTags <$ reserved "IMPLICIT"
                      , AutomaticTags <$ reserved "AUTOMATIC"
                      ]
          reserved "TAGS"  
          return t
  

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

bitStringType =
  do { reserved "BIT";  reserved "STRING" 
     ; option [] $ braces namedNumberList 
     }
     <?> "BitStringType"



signedNumber = integer
     <?> "SignedNumber"



-- Dubuisson 12.6.2
choiceType = Choice <$> ( reserved "CHOICE" *> braces alternativeTypeLists )
             <?> "ChoiceType"

data AlternativeTypeLists = SimpleAlternativeTypeList [NamedType] 
                          | AlternativeTypeListWithExtension [NamedType] (Maybe ExceptionIdentification) (Maybe [NamedType])
                          deriving (Eq,Ord,Show, Typeable, Data)
alternativeTypeLists = 
  choice [ try complex
         , SimpleAlternativeTypeList <$> alternativeTypeList 
         ]
  where
    complex = do
      r <- alternativeTypeList
      comma
      ex <- extensionAndException
      add <- extensionAdditionAlternatives
      optionalExtensionMarker
      return $ AlternativeTypeListWithExtension r ex add

-- rootAlternativeTypeList is inlined since it has only one production

extensionAdditionAlternatives = optionMaybe (comma >> extensionAdditionAlternativesList)

extensionAdditionAlternativesList = 
  choice [ extensionAdditionAlternative
         , do l <- extensionAdditionAlternativesList; comma; return l
         , extensionAdditionAlternative
         ]

-- FIXME: should I wrap in additional datatype here?
extensionAdditionAlternative =
  choice [ extensionAdditionGroupAlternatives
         , namedType >>= return . (:[])
         ]

extensionAdditionGroupAlternatives = do
  reserved "[["
  l <- alternativeTypeList 
  reserved "]]"
  return l
  
alternativeTypeList = namedType `sepBy1` comma'
  where
    comma' = try $ do
      comma
      notFollowedBy (lexeme (char '.'))


-- {{{ Dubuisson 12.9.2
extensionAndException = do
  symbol "..."
  exceptionSpec
  
optionalExtensionMarker = optional $ extensionEndMarker


exceptionSpec = 
  optionMaybe ( lexeme (char '!') >> exceptionIdentification )
                
data ExceptionIdentification = ExceptionNumber Integer
                             | ExceptionValue DefinedValue
                             | ExceptionTypeAndValue Type Value
                             deriving (Eq,Ord,Show, Typeable, Data)
exceptionIdentification =
  choice [ ExceptionNumber <$> signedNumber 
         , ExceptionValue <$> definedValue
         , ExceptionTypeAndValue <$> theType <*> (reserved ":" *> value)
         ]
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


selectionType =
  do { id <- identifier;
     ; symbol "<"
     ; t <- theType
     ; return (Selection id t)
     }
     <?> "SelectionType"


taggedType =
  do { t <- tag
     ; tt <- tagType
     ; typ <- theType
     ; return (Tagged t tt typ)
     }
     <?> "TaggedType"

data Tag = Tag Class ClassNumber deriving (Eq,Ord,Show, Typeable, Data)
tag = squares $ do { c <- option UndefinedClass theClass
                   ; cn <- classNumber
                   ; return (Tag c cn)
                   } 

data ClassNumber = ClassNumber Integer | ClassNumberAsDefinedValue DefinedValue deriving (Eq,Ord,Show, Typeable, Data)
classNumber =
  choice [ number >>= return . ClassNumber
         , definedValue >>= return . ClassNumberAsDefinedValue
         ]
  <?> "ClassNumber"

data Class = Universal | Application | Private | UndefinedClass deriving (Eq,Ord,Show, Typeable, Data)
theClass = choice [ reserved "UNIVERSAL" >> return Universal
                  , reserved "APPLICATION" >> return Application
                  , reserved "PRIVATE" >> return Private
                  ]
           
anyType =
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





compoundValue = oid
     <?> "CompoundValue"



specialRealValue =
  choice [ reserved "PLUS-INFINITY" >> return PlusInfinity
         , reserved "MINUS-INFINITY" >> return MinusInfinity
         ]



namedValue =
  do {
     ; id <- option UndefinedIdentifier ( try identifier )
     ; v <- value
     ; return (NamedValue (ValueName id) v)
     }
     <?> "NamedValue"

oid = braces (many1 oidComponent)
     <?> "OID"

type OID = [OIDComponent]
data OIDComponent = ObjIdDefinedValue DefinedValue | ObjIdNumber Integer | ObjIdNamedNumber NamedNumber | ObjIdName Identifier deriving (Eq,Ord,Show, Typeable, Data)
oidComponent =
  choice [ ObjIdNamedNumber <$> try namedNumber
         , ObjIdName . Identifier <$> try reservedOIDIdentifier
         , ObjIdNumber <$> number
         , ObjIdDefinedValue <$> definedValue
         ]
  <?> "OIDComponent"

reservedOIDIdentifier = do
  i <- choice $ map (try.symbol) $ [ "itu-t", "ccitt", "iso", "joint-iso-itu-t", "joint-iso-ccitt"
                                   , "recommendation", "question", "administration", "network-operator"
                                   , "identified-organization", "standard", "member-body"] ++ map (:[]) ['a'..'z']
  notFollowedBy $ oneOf $ ['a'..'z']++['0'..'9']++"-."
  return i

data TODO = TODO deriving (Eq,Ord,Show, Typeable, Data)

data ValueSet = ValueSet TODO deriving (Eq,Ord,Show, Typeable, Data)
valueSet = undefined

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
natural         = P.natural asn1
integer         = P.integer asn1
dot             = P.dot asn1
commaSepEndBy p = p `sepEndBy` comma
commaSepEndBy1 p = p `sepEndBy1` comma

-- Local Variables: 
-- outline-regexp:"-- [{]\+"
-- End: