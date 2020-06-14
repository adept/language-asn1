module Main where

import Language.ASN1.Parser
import Data.Generics
import System.Environment(getArgs)

-- Sorry, no fancy prettyprinting combinators :)
showTerm t = showTerm' 0 $ show t
  where
  showTerm' lvl [] = []
  showTerm' lvl ('{':cs) = unwords [ "{\n", gap (lvl+1), showTerm' (lvl+1) cs]
  showTerm' lvl ('}':cs) = unwords [ "\n", gap lvl, "}", showTerm' (lvl-1) cs]
  showTerm' lvl (',':cs) = unwords [",\n", gap lvl, showTerm' lvl (dropWhile (==' ') cs)]
  showTerm' lvl (c:cs)   = c:(showTerm' lvl cs)
  gap x = take (x*2) $ repeat ' '

allAssignedTypes = everything (++) ([] `mkQ` assignedTypeName)
assignedTypeName (TypeAssignment (TypeReference tr) _) = [tr]
assignedTypeName _ = []

main = do
  args <- getArgs
  case args of
    [] -> error "Give me ASN.1 file to parse"
    [file] -> parseAndDumpTypes file

parseAndDumpTypes file =
  do ms <- parseASN1FromFileOrDie file
     let m = head ms
     putStrLn "============="
     putStrLn "First parsed module"
     putStrLn $ showTerm m
     putStrLn "==========================="
     putStrLn "Types defined in the module"
     mapM_ putStrLn $ allAssignedTypes m
