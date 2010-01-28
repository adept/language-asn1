module Main where

-- ("-package lang" "-i./GenericLib/" "-fglasgow-exts" "-fallow-overlapping-instances" "-fallow-undecidable-instances")

import ASN1Parser
import IO
import Data.Generics
import List
import System(getArgs)

data A =A { field1::Maybe (Int,String) }

showTerm t = showTerm' 0 $ show t
  where
  showTerm' lvl [] = []
  showTerm' lvl ('{':cs) = "{\n"++(gap (lvl+1))++(showTerm' (lvl+1) cs)
  showTerm' lvl ('}':cs) = '\n':(gap lvl)++"}"++(showTerm' (lvl-1) cs)
  showTerm' lvl (',':cs) = ",\n"++(gap lvl)++(showTerm' lvl (dropWhile (==' ') cs))
  showTerm' lvl (c:cs)   = c:(showTerm' lvl cs)
  gap x = take (x*2) $ repeat ' '

allAssignedTypes = everything (++) ([] `mkQ` assignedTypeName)
assignedTypeName (TypeAssignment (TypeReference tr) _) = [tr]
assignedTypeName _ = []

main = 
  do args <- getArgs
     case args of
          [] -> error "Give me ASN.1 file to parse and type name for which to construct data type"
          (file:typeName:_) -> --testExtT file typeName 
                               testDTGen file typeName

testExtT file typeName = 
  do ms <- parseASN1FromFileOrDie file
     let m = head ms
     case findTypeAssigmentByName typeName m of
          Nothing -> error "No such global type"
          Just t -> do putStrLn $ showTerm t
                       mapM_ putStrLn $ gEncode t

gEncode = everything (++) ([] `mkQ` gEncType `extQ` gEncNamedNumber `extQ` gEncSubType)
gEncSubType :: SubtypeSpec -> [String ]
gEncSubType [] = [""]
gEncSubType _ = ["Dont know how to encode subtype"]    
gEncNamedNumber (NamedNumber (NumberName UndefinedIdentifier) v) = [ "What to do with unnamed number?" ]
gEncNamedNumber (NamedNumber (NumberName (TheIdentifier name)) value) = [ name ]
gEncType (Defined UndefinedModuleReference (TypeReference typeReference)) = [typeReference]
gEncType (Defined (ModuleReference mRef) (TypeReference typeReference)) =  [mRef ++ "." ++ typeReference]
gEncType (Sequence etList) = ["{"]++ everything (++) ([] `mkQ` gEncType) etList  ++["}"]
gEncType _ = [ "Dont know how to encode this particular type" ]

-- Set/Seq is just a enumeration
-- SetOf/SeqOf is a list
-- Named integer is X | Y | Z
-- Optional in Maybe
-- Tags are stripped out -> used only during encode-decode
testDTGen file typeName =
  do ms <- parseASN1FromFileOrDie file
     let m = head ms
     case findTypeAssigmentByName typeName m of
          Nothing -> error "No such global type"
          Just t -> do putStrLn $ showTerm t
                       putStrLn $ "data " ++ typeName ++ " = " ++ typeName ++ encodeGlobalType t m

encodeGlobalType t m = 
  do case t of
          GlobalDMT dmt -> error "Dont know how to encode DMT"
          GlobalT theT -> encodeTheType theT 

encodeTheType theT = encodeType (type_id theT) ++ encodeSubtype (subtype theT)

encodeSubtype [] = ""
encodeSubtype _ = "Dont know how to encode subtype"

encodeType (IntegerT nnList) = "Integer " ++ concatMap encodeNamedNumber nnList
encodeType (BitString nnList) = "BitString " ++ concatMap encodeNamedNumber nnList
encodeType (Set etList) = "{"++ concatMap encodeElementType etList ++"}"
encodeType (Sequence etList) = "{"++ (concat $ intersperse ",\n" $ map encodeElementType etList) ++"}"
encodeType (SetOf sizeConstraint theType) = 
  case sizeConstraint of
       UndefinedSizeContraint -> "["++ encodeTheType theType ++"]"
       _                      -> error "Unable to encode size-constrained SetOf"
encodeType (SequenceOf sizeConstraint theType) = 
  case sizeConstraint of
       UndefinedSizeContraint -> "["++ encodeTheType theType ++"]"
       _                      -> error "Unable to encode size-constrained SequenceOf"
encodeType (Choice etList) = concat $ intersperse " | " $ map encodeElementType etList
encodeType (Selection theIdentifier theType) = "Unable to encode Selection"
encodeType (Tagged tag tagType theType) = encodeTheType theType
encodeType (Any theIdentifier) = "Unable to encode Any"
encodeType (Enumerated nnList) = "Unable to encode Enumerated"
encodeType (OctetString) = "OctetString"
encodeType (ObjectIdentifier) = "ObjectIdentifier"
encodeType (Real) = "Real"
encodeType (Boolean) = "Boolean"
encodeType (Null) = "Null"
encodeType (External) = "External"
encodeType (Defined UndefinedModuleReference (TypeReference typeReference)) = typeReference
encodeType (Defined (ModuleReference mRef) (TypeReference typeReference)) =  mRef ++ "." ++ typeReference

encodeNamedNumber (NamedNumber (NumberName UndefinedIdentifier) v) = error "What to do with unnamed number?"
encodeNamedNumber (NamedNumber (NumberName (TheIdentifier name)) value) = name
  

encodeElementType (NamedElementType (TypeName tIdent) theT ePresence) = 
  let opener = case ePresence of
                    Optional -> "Maybe ("
                    _        -> ""
      closer = case ePresence of
                    Optional -> ")"
                    _        -> ""
      name = case tIdent of
                  TheIdentifier x -> x++"::"
                  UndefinedIdentifier -> ""
  in name ++ opener ++ encodeTheType theT ++ closer
encodeElementType (ComponentsOf theType)                   = error "Unable to encode ComponentsOf"

findTypeAssigmentByName name = something (Nothing `mkQ` (findByTypeName name))
findByTypeName name (TypeAssignment (TypeReference tr) t) | tr == name = Just t
findByTypeName name _ = Nothing

test file =
  do ms <- parseASN1FromFileOrDie file
     let m = head ms
     putStrLn "Parsed module"
     putStrLn $ showTerm m
     putStrLn "Types defined in the module"
     mapM_ (putStrLn.show) $ allAssignedTypes m
