{-
Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus
  
  extended by Thomas Cotter

A Compiler for the MiniTriangle language. Compiles .mt files into .tam programs.
Can run .tam programs in the TAM Virtual Machine

Main executable.
-}

module Main where

import ExpParser
import ExpCompiler
import TAM
import ExecuteTAM

import System.Environment
import Data.Char


data FileType = EXP | TAM | MT
  deriving (Eq,Show)


main :: IO Stack
main = do args <- getArgs
          let inputName = head args
          let (fileName, extension) = fileNE args
                        
          let tamFun :: [TAMInst] -> IO Stack
              tamFun = \tam -> do putStrLn ("Executing TAM code")
                                  execTAM' tam

          case extension of
            TAM -> do src <- readFile (fileName++".tam")
                      let tam = parseTAM src 
                      tamFun tam
            MT -> do src <- readFile (fileName++".mt") 
                     writeFile (fileName++".tam") (compileTAM src) >> putStrLn ("compiled to TAM file: " ++ fileName ++ ".tam")
                     return [5]
            



-- Finding the base name and extension of a file name

baseName :: String -> String
baseName = takeWhile (/='.')

fileExt :: String -> String
fileExt fn = let ext = dropWhile (/='.') fn
             in if ext == "" then ".exp" else ext

extType :: String -> Maybe FileType
extType ".tam" = Just TAM
extType ".mt" = Just MT
extType _ = Nothing

parseFileName :: String -> Maybe (String,FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then let name = baseName arg
             ext  = extType (fileExt arg)
         in case ext of
              Just t -> Just (name,t)
              Nothing -> Nothing
    else Nothing




unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String,FileType)
fileNE = head . unJust . (map parseFileName)

