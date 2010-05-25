{-# LANGUAGE DeriveDataTypeable #-}
module Language.ASN1.Parser where
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
-}

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import System.Exit (exitFailure)
import Data.Generics
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Char (isUpper)
import Data.List (isInfixOf)

data Module = Module { module_id::ModuleIdentifier
                     , module_oid :: Maybe OID
                     , default_tag_type::TagType
                     , module_body::Maybe ModuleBody
                     } deriving (Eq,Ord,Show, Typeable, Data)
data GlobalType = GlobalT Type | GlobalDMT DefinedMacroType  deriving (Eq,Ord,Show, Typeable, Data)
data Type = Type { type_id::BuiltinType
                       , subtype::Maybe SubtypeSpec
                       }
               deriving (Eq,Ord,Show, Typeable, Data)
newtype TypeName = TypeName TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
data NamedNumber = NamedNumber { number_name::TheIdentifier
                               , number_value::NumberOrDefinedValue
                               } deriving (Eq,Ord,Show, Typeable, Data)
type NumberOrDefinedValue = Either Integer DefinedValue
data ElementType = NamedElementType { element_name::TypeName
                                    , element_body::Type
                                    , element_presence::Maybe ValueOptionality
                                    } 
                 | ComponentsOf Type deriving (Eq,Ord,Show, Typeable, Data)
data ValueOptionality = OptionalValue | DefaultValue TheValue  deriving (Eq,Ord,Show, Typeable, Data)
data NamedValue = NamedValue { value_name::ValueName
                             , named_value::TheValue
                             } deriving (Eq,Ord,Show, Typeable, Data)
newtype ValueName = ValueName TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
data SizeConstraint = SizeConstraint SubtypeSpec | UndefinedSizeContraint deriving (Eq,Ord,Show, Typeable, Data)

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


objectIdentifier =
  do { reserved "OBJECT" ; reserved "IDENTIFIER" }

data StringConst = StringConst String deriving (Eq,Ord,Show, Typeable, Data)
stringConst allowedSet marker = 
  do { char '\'' ; body <- many (oneOf allowedSet) ; char '\''; char marker ; return (StringConst body) } 

bstring = stringConst "01" 'B' <?> "bstring"

hstring = stringConst "0123456789ABCDEFabcdef" 'H' <?> "hstring"

cstring = 
  do { char '"'; s <- anyChar `manyTill` (char '"' ); return (StringConst s) } <?> "cstring"

numberERange = natural

lcaseFirstIdent = do { i <- identifier
                     ; when (isUpper $ head i) $ unexpected "uppercase letter"
                     ; return i
                     }

ucaseFirstIdent = do { i <- identifier
                     ; when (not . isUpper $ head i) $ unexpected "lowercase letter"
                     ; return i
                     }

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

moduleDefinition = 
  do { id <- moduleIdentifier   
     ; oid_ <- optMaybe oid
     ; reserved "DEFINITIONS"
     ; td <- option UndefinedTagType tagDefault
     ; reserved "::="
     ; reserved "BEGIN" 
     ; body <- optMaybe moduleBody
     ; reserved "END"  
     ; return (Module id oid_ td body)
     }
     <?> "moduleDefinition"

data TagType = Explicit | Implicit | UndefinedTagType deriving (Eq,Ord,Show, Typeable, Data)
tagType = choice [ reserved "EXPLICIT" >> return Explicit
                 , reserved "IMPLICIT" >> return Implicit
                 ]

tagDefault = 
  do { t <- tagType
     ; reserved "TAGS"  
     ; return t
     }
     <?> "tagDefault"

data ModuleIdentifier = ModuleIdentifier (Maybe ModuleReference) (Maybe AssignedIdentifier) deriving (Eq,Ord,Show, Typeable, Data)
data ModuleReference = ModuleReference String deriving (Eq,Ord,Show, Typeable, Data)
moduleIdentifier = 
  do { ref <- optMaybe moduleReference
     ; id <- optMaybe assignedIdentifier
     ; when ( ref == Nothing && id == Nothing) $ unexpected "Empty module identifier, please provide author with sample file"
     ; return (ModuleIdentifier ref id)
     }
     <?> "moduleIdentifier"

data ModuleBody = ModuleBody { module_exports::[[ExportedSymbol]]
                             , module_imports::[[SymbolsFromModule]]
                             , module_assignments::[Assignment]
                             } deriving (Eq,Ord,Show, Typeable, Data)
moduleBody = 
  do { e <- option [] exports
     ; i <- option [] imports
     ; a <- option [] assignmentList
     ; return (ModuleBody e i a)
     }
     <?> "moduleBody"

newtype ExportedSymbol = ExportedSymbol TheSymbol deriving (Eq,Ord,Show, Typeable, Data)
exports = 
  do { reserved "EXPORTS" 
     ; endBy (commaSep1 (theSymbol >>= return . ExportedSymbol)) semi
     }
     <?> "Exports"

imports = 
  do { reserved "IMPORTS"  
     ; (many1 $ try symbolsFromModule) `endBy` semi
     }
     <?> "Imports"

data SymbolsFromModule = SymbolsFromModule [TheSymbol] ModuleIdentifier deriving (Eq,Ord,Show, Typeable, Data)
symbolsFromModule =
  do { ss <- commaSep1 theSymbol
     ; reserved "FROM" 
     ; id <- moduleIdentifier 
     ; return (SymbolsFromModule ss id)
     }
     <?> "SymbolsFromModule"

data AssignedIdentifier = AssignedIdentifierOID OID | AssignedIdentifierDefinedValue DefinedValue deriving (Eq,Ord,Show, Typeable, Data)
assignedIdentifier = 
  choice [ AssignedIdentifierOID <$> try oid
         , AssignedIdentifierDefinedValue <$> definedValue
         ]

data TypeReference = TypeReference String deriving (Eq,Ord,Show, Typeable, Data)

data TheSymbol = TypeReferenceSymbol TypeReference
               | TheIdentifierSymbol TheIdentifier
               | DefinedMacroNameSymbol DefinedMacroName 
                 deriving (Eq,Ord,Show, Typeable, Data)
theSymbol = 
 choice $ map try [ typereference >>= return . TypeReferenceSymbol
                  , theIdentifier >>= return . TheIdentifierSymbol
                  , definedMacroName >>= return . DefinedMacroNameSymbol
                  ]

assignmentList = 
  do { sepBy1 assignment (optional semi)
     }
     <?> "assignmentList"

data Assignment = MacroDefinition { macro_def_type::MacroDefinitionType
                                  , macro_body::MacroBody
                                  }
                | ValueAssignment { value_ref::TheIdentifier
                                  , value_ref_type::Type
                                  , assigned_value_ref::TheIdentifier
                                  , assigned_value::BuiltinValue
                                  }
                | TypeAssignment  { type_ref::TypeReference
                                  , assigned_type::Type
                                  } 
                  deriving (Eq,Ord,Show, Typeable, Data)
assignment = 
  choice $ map try [ macroDefinition >>= return
                   , valueAssignment >>= return
                   , typeAssignment >>= return
                   ]

data MacroDefinitionType = DefinedMacroNameMDT DefinedMacroName 
                         | TypeReferenceMDT TypeReference
                           deriving (Eq,Ord,Show, Typeable, Data)
macroDefinition =
  do { t <- choice [ definedMacroName >>= return . DefinedMacroNameMDT
                   , typereference >>= return . TypeReferenceMDT
                   ]
     ; reserved "MACRO"
     ; reserved "::="  
     ; reserved "BEGIN"
     ; body <- macroBody
     ; reserved "END"
     ; return (MacroDefinition t body) 
     }
     <?> "macroDefinition"

data MacroBody = MacroBody String deriving (Eq,Ord,Show, Typeable, Data)
macroBody = anyChar `manyTill` (reserved "END") >>= return . MacroBody -- WRONG or not?
{-
JAVACODE
void macroBody {	
  Token tok;
  int nesting = 1;
  while (true) {
    tok = getToken(1);
    if (tok.kind == END_TKN) {
	break;	
    }
    tok = getnextToken;
  }
}
-}

data MacroReference = TypeMacroReference TypeReference
                    | DefinedNameMacroReference DefinedMacroName
                      deriving (Eq,Ord,Show, Typeable, Data)
macroReference = 
  do {
      choice [ typereference >>= return . TypeMacroReference
             , definedMacroName >>= return . DefinedNameMacroReference
             ]
     }
     <?> "MacroReference"

-- Dubuisson 9.1.2
typeAssignment =
  do { t1 <- typereference
     ; reserved "::=" 
     ; t2 <- theType
     ; return (TypeAssignment t1 t2)
     }
     <?> "TypeAssignment"

-- Dubuisson 9.1.2
-- ConstrainedType is merged in other parsers: "Type Constraint" alternative is encoded here,
-- and TypeWithConstraint in implemented in Set/Sequence parsers
theType :: Parser Type
theType = do
  t <- builtinType <|> referencedType
  c <- optionMaybe constraint
  return (Type t c)
  <?> "Type"

data BuiltinType = IntegerT [NamedNumber]
          | BitString [NamedNumber]
          | Set [ElementType]
          | Sequence [ElementType]
          | SetOf SizeConstraint Type
          | SequenceOf SizeConstraint Type
          | Choice [ElementType]
          | Selection TheIdentifier Type
          | Tagged Tag TagType Type
          | Any TheIdentifier
          | Enumerated [NamedNumber]
          | OctetString 
          | ObjectIdentifier
          | Real
          | Boolean
          | Null
          | External
          | Defined (Maybe ModuleReference) TypeReference
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

-- Dubuisson 9.1.2
builtinType =
  do {
      choice $ map try [ integerType >>= return . IntegerT             -- OK
                       , bitStringType >>= return . BitString         -- OK
                       , setOrSequenceType                            -- OK
                       , setOrSequenceOfType                          -- OK
                       , choiceType >>= return . Choice               -- OK
                       , taggedType                                   -- OK
                       , anyType >>= return . Any                     
                       , enumeratedType >>= return . Enumerated       -- OK
                       , octetString >> return OctetString            -- OK
                       , objectIdentifier >> return ObjectIdentifier  -- OK
                       , reserved "REAL" >> return Real         -- OK
                       , reserved "BOOLEAN" >> return Boolean   -- OK
                       , reserved "NULL" >> return Null         -- OK
                       , reserved "EXTERNAL" >> return External -- OK
                       , characterStringType
                       -- TODO: , embeddedPDVType
                       -- TODO: , instanceOfType
                       -- TODO: , objectClassFieldType
                       -- TODO: , relativeOIDType
                       ]
     }
     <?> "BuiltinType"

embeddedPDVType = undefined
instanceOfType = undefined
objectClassFieldType = undefined
relativeOIDType = undefined
typeFromObject = undefined
valueSetFromObjects = undefined
objectDescriptor = undefined

-- Dubuisson 9.1.2
referencedType = 
  definedType <|> usefulType <|> selectionType {- <|> typeFromObject <|> valueSetFromObjects -} 
  <?> "ReferencedType"

-- Dubuisson 11.15.2
usefulType = 
  choice [ reserved "GeneralizedTime" >> return GeneralizedTime
         , reserved "UTCTime" >> return UTCTime
         ]
  
definedType =
  do {
     ; mref <- optMaybe (try moduleReferenceAndDot)
     ; typeref <- typereference
     ; return (Defined mref typeref)
     }
     <?> "DefinedType"

moduleReferenceAndDot = 
  do { 
     ; mref <- moduleReference 
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

octetString = 
  do { reserved "OCTET" ; reserved "STRING" }

enumeratedType =
  do { reserved "ENUMERATED"
     ; braces namedNumberList
     }
     <?> "EnumeratedType"

integerType =
  do { reserved "INTEGER"
     ; option [] $ braces namedNumberList
     }
     <?> "IntegerType"

bitStringType =
  do { reserved "BIT";  reserved "STRING" 
     ; option [] $ braces namedNumberList 
     }
     <?> "BitStringType"

namedNumberList = commaSep1 namedNumber
                  <?> "NamedNumberList"

namedNumber =
  do { id <- theIdentifier
     ; v <- parens numberOrDefinedValue
     ; return (NamedNumber id v)
     }
     <?> "NamedNumber"

numberOrDefinedValue = 
  choice [ Left <$> signedNumber
         , Right <$> definedValue
         ]

signedNumber = integer
     <?> "SignedNumber"

data SetOrSeq = SetT | SequenceT deriving (Eq,Ord,Show, Typeable, Data)
setOrSeq = choice $ map try [ reserved "SET" >> return SetT
                            , reserved "SEQUENCE" >> return SequenceT
                            ]

setOrSequenceType =
  do { t <- setOrSeq
     ; e <- braces elementTypeList
     ; case t of 
       SetT -> return (Set e)
       SequenceT -> return (Sequence e)
     }
     <?> "SetOrSequenceType"

setOrSequenceOfType =
  do { t <- setOrSeq
     ; sc <- option UndefinedSizeContraint ( sizeConstraint <|> parens sizeConstraint )
     ; reserved "OF"
     ; innert <- theType
     ; case t of 
       SetT -> return (SetOf sc innert)
       SequenceT -> return (SequenceOf sc innert)
     }
     <?> "SetOrSequenceOfType"

choiceType =
  do { reserved "CHOICE"
     ; braces elementTypeList
     }
     <?> "ChoiceType"

elementTypeList = commaSep1 elementType
                  <?> "ElementTypeList"

elementType =
  choice $ map try 
           [ componentsType >>= return . ComponentsOf
           , do { id <- option UndefinedIdentifier (try theIdentifier)
                ; t <- theType
                ; presence <- optionMaybe $ valueOptionality
                ; return (NamedElementType (TypeName id) t presence)
                }
           ]
           
valueOptionality = 
  choice [ reserved "OPTIONAL" >> return OptionalValue
         , reserved "DEFAULT" >> theValue >>= return . DefaultValue
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
  do { id <- theIdentifier;
     ; symbol "<"
     ; t <- theType
     ; return (Selection id t)
     }
     <?> "SelectionType"


taggedType =
  do { t <- tag
     ; tt <- option UndefinedTagType tagType
     ; typ <- theType
     ; return (Tagged t tt typ)
     }
     <?> "TaggedType"

data Tag = Tag TheClass ClassNumber deriving (Eq,Ord,Show, Typeable, Data)
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

data TheClass = Universal | Application | Private | UndefinedClass deriving (Eq,Ord,Show, Typeable, Data)
theClass = choice [ reserved "UNIVERSAL" >> return Universal
                  , reserved "APPLICATION" >> return Application
                  , reserved "PRIVATE" >> return Private
                  ]
           
anyType =
  do { reserved "ANY"
     ; option UndefinedIdentifier (  do {reserved "DEFINED"  ;  reserved "BY"  ; theIdentifier  } ) 
     }
     <?> "AnyType"

type SubtypeSpec = [SubtypeValueSet]

-- unchecked, 15.7.2
constraint :: Parser SubtypeSpec
constraint = parens subtypeValueSetList
              <?> "SubtypeSpec"

subtypeValueSetList = sepBy1 subtypeValueSet (symbol "|") <?> "SubtypeValueSetList"

data ValueRangeElement = MinValue | MaxValue | UndefinedValue | Value TheValue deriving (Eq,Ord,Show, Typeable, Data)

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

singleValue =
  do {
      theValue
     }
     <?> "SingleValue"

valueRange =
  do { from <- choice [ theValue >>= return . Value
                      , do reserved "MIN"
                           return MinValue
                      ]
     ; to <- option UndefinedValue ( do optional (symbol "<")
                                        dot
                                        dot
                                        optional (symbol "<") 
                                        choice [ theValue >>= return . Value
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
      braces(  do { optional (  reserved "...," ) 
                  ; typeConstraints
                  }
            )
     }
     <?> "MultipleTypeConstraints"

type TypeConstraints = [NamedConstraint]
typeConstraints = commaSep1 namedConstraint <?> "TypeConstraints"

data NamedConstraint = NamedConstraint TheIdentifier Constraint  deriving (Eq,Ord,Show, Typeable, Data)
namedConstraint =
  do { id <- option UndefinedIdentifier theIdentifier
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


valueAssignment =
  do { id <- theIdentifier
     ; t <- theType 
     ; reserved "::="  
     ; id2 <- option UndefinedIdentifier $ do { i <- theIdentifier
                                              ; optional $ symbol ":" 
                                              ; return i
                                              } 
     ; v <- option UndefinedBuiltinValue builtinValue
     ; return (ValueAssignment id t id2 v)
     }
     <?> "ValueAssignment"

data TheValue = BuiltinV BuiltinValue
              | DefinedV DefinedValue -- TODO: add ValueFromObject here
              | UndefinedV deriving (Eq,Ord,Show, Typeable, Data)
theValue = 
  do {
     ; choice [ builtinValue >>= return . BuiltinV
              , definedValue >>= return . DefinedV
              ]
     }
     <?> "Value"

data DefinedValue = DefinedValue (Maybe ModuleReference) TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
definedValue =
  do {
     ; mref <- optMaybe $ try moduleReferenceAndDot
     ; id <- theIdentifier
     ; return (DefinedValue mref id)
     }
     <?> "DefinedValue"

data BuiltinValue = BooleanValue Bool
                  | NullValue
                  | PlusInfinity
                  | MinusInfinity
                  | SignedNumber Integer
                  | HexString StringConst
                  | BinaryString StringConst
                  | CharString StringConst
                  | CompoundValue OID
                  | UndefinedBuiltinValue deriving (Eq,Ord,Show, Typeable, Data)
                      
builtinValue =
  do {
      choice $ map try [ booleanValue >>= return . BooleanValue
                       , nullValue >> return NullValue
                       , specialRealValue
                       , signedNumber >>= return . SignedNumber
                       , hexString >>= return . HexString
                       , binaryString >>= return . BinaryString
                       , charString >>= return . CharString
                       , compoundValue >>= return . CompoundValue
                       ]
     }
     <?> "BuiltinValue"

compoundValue = oid
     <?> "CompoundValue"


booleanValue =
  choice [ reserved "TRUE" >> return True
         , reserved "FALSE" >> return False
         ]

specialRealValue =
  choice [ reserved "PLUS-INFINITY" >> return PlusInfinity
         , reserved "MINUS-INFINITY" >> return MinusInfinity
         ]

nullValue = reserved "NULL"  

namedValue =
  do {
     ; id <- option UndefinedIdentifier ( try theIdentifier )
     ; v <- theValue
     ; return (NamedValue (ValueName id) v)
     }
     <?> "NamedValue"

oid = braces (many1 oidComponent)
     <?> "OID"

type OID = [OIDComponent]
data OIDComponent = ObjIdDefinedValue DefinedValue | ObjIdNumber Integer | ObjIdNamedNumber NamedNumber | ObjIdName TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
oidComponent =
  choice [ ObjIdNamedNumber <$> try namedNumber
         , ObjIdName . TheIdentifier <$> try reservedOIDIdentifier
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

binaryString = bstring <?> "BinaryString"
hexString = hstring  <?> "HexString"
charString = cstring <?> "CharString"
number = natural <?> "number"
data TheIdentifier = TheIdentifier String 
                   | UndefinedIdentifier
                     deriving (Eq,Ord,Show, Typeable, Data)
theIdentifier = lcaseFirstIdent >>= return . TheIdentifier <?> "identifier"
moduleReference = ucaseFirstIdent >>= return . ModuleReference <?> "modulereference"
typereference = ucaseFirstIdent >>= return . TypeReference <?> "typereference"

data DefinedMacroType = TextualConventionDMT TextualConventionMacroType | SnmpObjectDMT SnmpObjectTypeMacroType deriving (Eq,Ord,Show, Typeable, Data)
definedMacroType =
   do {
      choice [ textualConventionMacroType >>= return . TextualConventionDMT
             , snmpObjectTypeMacroType >>= return . SnmpObjectDMT
             ]
     }
     <?> "DefinedMacroType"

data DefinedMacroName = ObjectType | TextualConvention deriving (Eq,Ord,Show, Typeable, Data)
definedMacroName =
  do {
      choice [ reserved "OBJECT-TYPE" >> return ObjectType 
             , reserved "TEXTUAL-CONVENTION" >> return TextualConvention
             ]
     }
     <?> "DefinedMacroName"

data SnmpObjectTypeMacroType = SnmpObjectTypeMacroType Type SnmpAccess SnmpStatus SnmpDescr SnmpRefer SnmpIndex TheValue
                               deriving (Eq,Ord,Show, Typeable, Data)
snmpObjectTypeMacroType =
  do { reserved "OBJECT-TYPE" 
     ; reserved "SYNTAX" 
     ; t <- theType
     ; reserved "ACCESS"
     ; sa <- snmpAccess
     ; reserved "STATUS"
     ; ss <- snmpStatus 
     ; sd <- option UndefinedSnmpDescr (snmpDescrPart)
     ; sr <- option UndefinedSnmpRefer (snmpReferPart)
     ; si <- option UndefinedSnmpIndex (snmpIndexPart)
     ; sdvp <- option UndefinedV (snmpDefValPart)
     ; return (SnmpObjectTypeMacroType t sa ss sd sr si sdvp)
     }
     <?> "SnmpObjectTypeMacroType"

data SnmpAccess = SnmpAccess TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
snmpAccess = theIdentifier >>= return . SnmpAccess
             <?> "SnmpAccess"

data SnmpStatus = SnmpStatus TheIdentifier deriving (Eq,Ord,Show, Typeable, Data)
snmpStatus = theIdentifier >>= return . SnmpStatus
             <?> "SnmpStatus"

data SnmpDescr = SnmpDescr StringConst
               | UndefinedSnmpDescr 
                 deriving (Eq,Ord,Show, Typeable, Data)
snmpDescrPart =
  do { reserved "DESCRIPTION" 
     ; charString >>= return . SnmpDescr
     }
     <?> "SnmpDescrPart"

data SnmpRefer = SnmpRefer StringConst 
               | UndefinedSnmpRefer
                 deriving (Eq,Ord,Show, Typeable, Data)
snmpReferPart =
  do { reserved "REFERENCE"
     ; charString >>= return . SnmpRefer
     }
     <?> "SnmpReferPart"
         
data SnmpIndex = SnmpIndex [TypeOrValue]
               | UndefinedSnmpIndex
                 deriving (Eq,Ord,Show, Typeable, Data)
snmpIndexPart =
  do { reserved "INDEX"  
     ; braces typeOrValueList >>= return . SnmpIndex
     }
     <?> "SnmpIndexPart"

typeOrValueList =
  do {
      typeOrValue 
     ; many ( do {reserved "," ; typeOrValue} )
     }
     <?> "TypeOrValueList"

data TypeOrValue = T Type | V TheValue deriving (Eq,Ord,Show, Typeable, Data)
typeOrValue =
  do {
      choice $ map try [ theType >>= return . T
                       , theValue >>= return . V
                       ]
     }
     <?> "TypeOrValue"

snmpDefValPart =
  do { reserved "DEFVAL"  
     ; braces theValue
     }
     <?> "SnmpDefValPart"

data TextualConventionMacroType = TextualConventionMacroType DisplayHint SnmpStatus SnmpDescr SnmpRefer Type
                                  deriving (Eq,Ord,Show, Typeable, Data)
data DisplayHint = DisplayHint StringConst 
                 | UndefinedDisplayHint 
                   deriving (Eq,Ord,Show, Typeable, Data)
textualConventionMacroType =
  do { reserved "TEXTUAL-CONVENTION"
     ; dh <- option UndefinedDisplayHint (displayHint)
     ; reserved "STATUS"
     ; ss <- snmpStatus 
     ; sd <- option UndefinedSnmpDescr (snmpDescrPart)
     ; sr <- option UndefinedSnmpRefer (snmpReferPart)
     ; reserved "SYNTAX"
     ; t <- theType
     ; return (TextualConventionMacroType dh ss sd sr t)
     }
     <?> "TextualConventionMacroType"

displayHint =
  do { reserved "DISPLAY-HINT" 
     ; charString >>= return . DisplayHint
     }
     <?> "DisplayHint"

optMaybe p = option Nothing (Just <$> p)

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
    , reservedNames = [ "ABSENT", "ABSTRACT-SYNTAX", "ALL", "APPLICATION", "AUTOMATIC", "BEGIN",
                        "BIT", "BMPString", "BOOLEAN", "BY", "CHARACTER", "CHOICE", "CLASS", "COMPONENT",
                        "COMPONENTS", "CONSTRAINED", "CONTAINING" , "DEFAULT", "DEFINITIONS", "EMBEDDED",
                        "ENCODED" , "END", "ENUMERATED", "EXCEPT", "EXPLICIT", "EXPORTS", "EXTENSIBILITY",
                        "EXTERNAL", "FALSE", "FROM", "GeneralizedTime", "GeneralString", "GraphicString",
                        "IA5String", "IDENTIFIER", "IMPLICIT", "IMPLIED", "IMPORTS", "INCLUDES", "INSTANCE",
                        "INTEGER", "INTERSECTION", "ISO646String", "MAX", "MIN", "MINUS-INFINITY",
                        "NULL", "NumericString", "OBJECT", "ObjectDescriptor", "OCTET", "OF", "OPTIONAL",
                        "PATTERN" , "PDV", "PLUS-INFINITY", "PRESENT", "PrintableString", "PRIVATE",
                        "REAL", "RELATIVE-OID", "SEQUENCE", "SET", "SIZE", "STRING", "SYNTAX", "T61String",
                        "TAGS", "TeletexString", "TRUE", "TYPE-IDENTIFIER", "UNION", "UNIQUE", "UNIVERSAL",
                        "UniversalString", "UTCTime", "UTF8String", "VideotexString", "VisibleString",
                        "WITH" ]
    , reservedOpNames = [ "::=" ]
    }

asn1            = P.makeTokenParser asn1Style
            
whiteSpace      = P.whiteSpace asn1
symbol          = P.symbol asn1
identifier      = P.identifier asn1
reserved        = P.reserved asn1
decimal         = P.decimal asn1
charLiteral     = P.charLiteral asn1
stringLiteral   = P.stringLiteral asn1
comma           = P.comma asn1
colon           = P.colon asn1
commaSep1       = P.commaSep1 asn1
semiSep1        = P.semiSep1 asn1
braces          = P.braces asn1
squares         = P.squares asn1
parens          = P.parens asn1
semi            = P.semi asn1
natural         = P.natural asn1
integer         = P.integer asn1
dot             = P.dot asn1
