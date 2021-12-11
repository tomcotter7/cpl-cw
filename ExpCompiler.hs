{-

Compilers Course (COMP3012), 2021
  Venanzio Capretta
  Nicolai Kraus

A Compiler and evaluator for Arithmetic expressions with booleans,
relations, and conditional expressions.
Compiles simple Arith Expr into TAM programs.

-}

module ExpCompiler where

import TAM
import ExpParser
import ExpTAM


-- Compiling Programs to TAM

compProgram :: String -> [TAMInst]
compProgram s = astCode [] (astParse s)

-- reading from a file

compileTAM :: String -> String
compileTAM = writeTAM . compProgram

